pathss <-"SeoulSubwayShp/"
nc2_file <- paste(pathss,"지하철_분당선.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=tmerc +lat_0=38 +lon_0=128 +k=0.9999 +x_0=400000 +y_0=600000 +ellps=bessel +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43"))
nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc22, col=alpha("#E0A134",1), lwd=1)

nc2_file <- paste(pathss,"지하철_인천1호선.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=tmerc +lat_0=38 +lon_0=128 +k=0.9999 +x_0=400000 +y_0=600000 +ellps=bessel +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43"))
nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc22, col=alpha("#6E98BB",1), lwd=1)


nc2_file <- paste(pathss,"공항철도.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc2, col=alpha("#0095DA",1), lwd=1)

nc2_file <- paste(pathss,"경의선.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc2, col=alpha("#32C6A6",1), lwd=1)

nc2_file <- paste(pathss,"경춘선.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc2, col=alpha("#32C6A6",1), lwd=1)

nc2_file <- paste(pathss,"중앙선.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc2, col=alpha("#32C6A6",1), lwd=1)

nc2_file <- paste(pathss,"신분당선.shp", sep="")
nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
lines(nc2, col=alpha("#C82127",1), lwd=1)


colss <- alpha(c("#00498B", "#009246", "#F36630", "#00A2D1", "#A064A3", "#9E4510", "#5D6519", "#D6406A", "#AA9872"), 1)
for(i in 1:9){
  nc2_file <- paste(pathss, "지하철_서울", i, "호선2.shp", sep="")
  nc2 <- readShapeLines(nc2_file, proj4string=CRS("+proj=tmerc +lat_0=38 +lon_0=128 +k=0.9999 +x_0=400000 +y_0=600000 +ellps=bessel +units=m +no_defs"))
  nc22 <- spTransform(nc2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  lines(nc22, col=colss[i], lwd=1)
}
