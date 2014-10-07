
gcd.vif <- function(long1, lat1, long2, lat2) {
    
    # WGS-84 ellipsoid parameters
    a <- 6378137         # length of major axis of the ellipsoid (radius at equator)
    b <- 6356752.314245  # ength of minor axis of the ellipsoid (radius at the poles)
    f <- 1/298.257223563 # flattening of the ellipsoid
    
    L <- long2-long1 # difference in longitude
    U1 <- atan((1-f) * tan(lat1)) # reduced latitude
    U2 <- atan((1-f) * tan(lat2)) # reduced latitude
    sinU1 <- sin(U1)
    cosU1 <- cos(U1)
    sinU2 <- sin(U2)
    cosU2 <- cos(U2)
    
    cosSqAlpha <- NULL
    sinSigma <- NULL
    cosSigma <- NULL
    cos2SigmaM <- NULL
    sigma <- NULL
    
    lambda <- L
    lambdaP <- 0
    iterLimit <- 200
    while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
        sinLambda <- sin(lambda)
        cosLambda <- cos(lambda)
        sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                              (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
        if (sinSigma==0) return(0)  # Co-incident points
        cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
        sigma <- atan2(sinSigma, cosSigma)
        sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
        cosSqAlpha <- 1 - sinAlpha*sinAlpha
        cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
        if (is.na(cos2SigmaM)) cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
        C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
        lambdaP <- lambda
        lambda <- L + (1-C) * f * sinAlpha *
            (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
        iterLimit <- iterLimit - 1
    }
    if (iterLimit==0) return(NA)  # formula failed to converge
    uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
    A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
    B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
    deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                                                 B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
    s <- b*A*(sigma-deltaSigma) #/ 1000
    
    return(s) # Distance in m
}

gcd.hf <- function(long1, lat1, long2, lat2) {
    R <- 6371 # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d = R * c
    return(d*1000) # Distance in m
}

gcd.slc <- function(long1, lat1, long2, lat2) {
    R <- 6371 # Earth mean radius [km]
    d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
    return(d*1000) # Distance in m
}

Daum2WGS84 <- function(xy){
    xy <- data.frame(x = as.numeric(xy[,1])/2.5,
                     y = as.numeric(xy[,2])/2.5)
    
    coordinates(xy) <- c("x", "y")
    proj4string(xy) <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs")
    xywgs84 <- spTransform(xy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    coordinates(xywgs84)
}


pathvolpoly <- function(res, pathvol, stpathind=NULL, turnpathind=NULL, scl=4){
    
    require(polyclip)
    if(is.null(turnpathind)){
        turnxy <- as.numeric(res$busStops[which(as.logical(res$busStops$turningPoint)), c("x", "y")])
        turnpathind <- which.min((res$path[,1] - turnxy[1])^2 + (res$path[,2] - turnxy[2])^2)
    }
    if(is.null(stpathind)){
        gostop <- res$busStops[1:which(as.logical(res$busStops$turningPoint)), c("x", "y")]
        backstop <- res$busStops[(1+which(as.logical(res$busStops$turningPoint))) :nrow(res$busStops) , c("x", "y")]
        
        gopath <- res$path[1:turnpathind, ]
        backpath <- res$path[(1+turnpathind):nrow(res$path), ]
        
        goind <- rep(1L, nrow(gostop))
        backind <- rep(1L, nrow(backstop))
        for(i in 1:nrow(gostop)){
            goind[i] <- which.min((as.numeric(gostop[i, 1]) - gopath[ ,1])^2 + (as.numeric(gostop[i, 2]) - gopath[, 2])^2)
        }
        for(i in 1:nrow(backstop)){
            backind[i] <- which.min((as.numeric(backstop[i, 1]) - backpath[ ,1])^2 + (as.numeric(backstop[i, 2]) - backpath[, 2])^2) + turnpathind
        }
        stpathind <- c(goind, backind)
    }
    
    
    rmdpath <- res$path
    dupind <- duplicated(paste(rmdpath[,1], rmdpath[,2], sep =","))
    dupind[stpathind] <- TRUE
    rmdpath <- rmdpath[!dupind, ]
    rmdpathvol <- pathvol[!dupind]
    
    turnpathind2 <- turnpathind - sum(dupind[1:turnpathind])
    
    A <- list(list(x=rmdpath[1:turnpathind2, 1], y=rmdpath[1:turnpathind2, 2]))
    nxx1 <- rep(0.0, turnpathind2)
    nyy1 <- rep(0.0, turnpathind2)
    for(i in 1:turnpathind2){
        #width <- i*2
        width <- rmdpathvol[i]*scl
        C <- polylineoffset(A, width, jointype="round", endtype="openround")
        id1 <- which.min((C[[1]]$x - rmdpath[1, 1])^2 + (C[[1]]$y - rmdpath[1, 2])^2)
        id2 <- which.min((C[[1]]$x - rmdpath[turnpathind2, 1])^2 + (C[[1]]$y - rmdpath[turnpathind2, 2])^2)
        if(id1 < id2){
            ind1 <- id1:id2
        } else {
            ind1 <- c(id1:length(C[[1]]$x), 1:id2)
        }
        
        ind2 <- which.min((rmdpath[i, 1] - C[[1]]$x[ind1])^2 + (rmdpath[i, 2] - C[[1]]$y[ind1])^2)
        nxx1[i] <- C[[1]]$x[ind1[ind2]]
        nyy1[i] <- C[[1]]$y[ind1[ind2]]
    }
    
    A <- list(list(x=rmdpath[(turnpathind2+1):nrow(rmdpath), 1], 
                   y=rmdpath[(turnpathind2+1):nrow(rmdpath), 2]))
    
    nxx2 <- rep(0.0, nrow(rmdpath) - turnpathind2)
    nyy2 <- rep(0.0, nrow(rmdpath) - turnpathind2)
    for(i in (turnpathind2+1):nrow(rmdpath)){
        #width <- (nrow(rmdpath)- i + 1)*2
        width <- rmdpathvol[i]*scl
        C <- polylineoffset(A, width, jointype="round", endtype="openround")
        id1 <- which.min((C[[1]]$x - rmdpath[turnpathind2+1, 1])^2 + (C[[1]]$y - rmdpath[turnpathind2+1, 2])^2)
        id2 <- which.min((C[[1]]$x - rmdpath[nrow(rmdpath), 1])^2 + (C[[1]]$y - rmdpath[nrow(rmdpath), 2])^2)
        if(id1 < id2){
            ind1 <- id1:id2
        } else {
            ind1 <- c(id1:length(C[[1]]$x), 1:id2)
        }
        
        ind2 <- which.min((rmdpath[i, 1] - C[[1]]$x[ind1])^2 + (rmdpath[i, 2] - C[[1]]$y[ind1])^2)
        nxx2[i-turnpathind2] <- C[[1]]$x[ind1[ind2]]
        nyy2[i-turnpathind2] <- C[[1]]$y[ind1[ind2]]
    }
    
    #polygo <- data.frame(x = c(rmdpath[1:turnpathind2, 1], rev(nxx1)), 
    #                     y = c(rmdpath[1:turnpathind2, 2], rev(nyy1)) )
    #polyback <- data.frame(x = c(rmdpath[(turnpathind2+1):nrow(rmdpath), 1], rev(nxx2)), 
    #                       y = c(rmdpath[(turnpathind2+1):nrow(rmdpath), 2], rev(nyy2)) )
    
    polygo <- data.frame(x = c(res$path[1:turnpathind, 1], rev(nxx1)), 
                         y = c(res$path[1:turnpathind, 2], rev(nyy1)) )
    polyback <- data.frame(x = c(res$path[(turnpathind+1):nrow(res$path), 1], rev(nxx2)), 
                           y = c(res$path[(turnpathind+1):nrow(res$path), 2], rev(nyy2)) )
    
    return(list(polygo=polygo, polyback=polyback, turnpathind=turnpathind, stoptopathind=stpathind))
}


deg2rad <- function(deg) return(deg*pi/180)
