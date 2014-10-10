#####################################################################################################
#####################################################################################################
# R-script for KSS Letter on October 2014
# Since the encoding of this file is UTF-8, 
# If you see the non-readeble characers in this script, re-open this file with UTF-8 encoding.
dir.create("KSSLetter")
setwd("KSSLetter")

gitadd <- "https://github.com/dongikjang/"

# install required packages
reqpkgs <- c("mapdata", "RColorBrewer", "RNetCDF", "colorRamps", "rgl", 
             "ggmap", "jpeg", "png", "plyr", "fields", "geosphere", 
             "XML", "bitops", "jpeg", "ggplot2",
             "scales", "polyclip", "maptools", "rgdal", "data.table", 
             "RgoogleMaps", "sp", "maps", "RCurl")
inspkgind <- !reqpkgs %in% installed.packages()[,1] 
if(any(inspkgind)){
  for(inspkg in reqpkgs[inspkgind]) install.packages(inspkg)
}

update.packages()

# sessionInfo()
#####################################################################################################
#####################################################################################################
# Preliminary Steps for Windows user
# Some codes in the following need to download files with HTTP Secure (not http).
# The 'download.file' function in R does not support HTTP Secure with default 'method'.
# Therefore we should change the method option of the 'download.file' function in to "curl" or "wget"
# But "curl" and "wget" have not been installed basically on the Windows since those are command line tools.
#
# You should install programs with following steps 
# 0. Install Rtools (optional)
#     http://cran.r-project.org/bin/windows/Rtools/
#   On the install Rtool, You should check the "Edit the System Path" and 
#     add the directory which has "R.exe" file something like 
#     C:\Program Files\R\R-3.1.0\bin\i386;
#     for 32-bit OS (if you use 64-bit os then the last folder may be 'x64').
#   After install Rtool, please check the status of system path on the CMD.
#   On the CMD, type 'R' (without quotation mark) then R will be excuted
#     with non-gui mode on the CMD.
# 1. downlaod  nuget Command-Line Utility at
#   	http://nuget.org/nuget.exe
# 2. On the CMD, change directory to download folder of 'nuget.exe' and install 'chocolatey'. 
#   Type the following on the CMD.
#		nuget.exe install chocolatey
# 3. On the CMD (not one the powershell) type the following
# @powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin
# 4. Install curl and wget with following commands.s
#		choco install curl
#		choco install Wget
# Then we can use 'download.file' for HTTP Secure.


#####################################################################################################
#####################################################################################################
# Figure 1
library(mapdata)
library(RColorBrewer)
cols <- brewer.pal(9, "Set1")
# (a)
# cairo_pdf("mapdefault1.pdf", width=4.6, height=4)
par(mar=c(0,0,0,0))
southkor <- map('worldHires', 'South Korea', fill=TRUE, plot=FALSE)
plot(southkor, type="n", asp=1, axes=FALSE, xlab="", ylab="", main="")
polygon(southkor, col = cols[2], border=1)
#dev.off()
# (b)
#cairo_pdf("mapdefault2.pdf", width=8, height=4)
par(mar=c(0,0,0,0), xaxs = "i", yaxs = "i")
map('worldHires', fill=TRUE, plot=T, col=cols)
#dev.off()


#####################################################################################################
#####################################################################################################
# Figure 2
# (a)
download.file(paste(gitadd, "kssletter/raw/master/etopo1.nc", sep=""),
              destfile="etopo1.nc", method="curl", extra=" -L -k ", quiet=TRUE )
library(RNetCDF)
nc <- open.nc("etopo1.nc")
tmp <- read.nc(nc)
names(tmp) <- c("crs", "y", "x", "z")
close.nc(nc)

ocean.pal <- colorRampPalette(
 c("#000000", "#000209", "#000413", "#00061E", "#000728", "#000932", "#002650", 
 "#00426E", "#005E8C", "#007AAA", "#0096C8", "#22A9C2", "#45BCBB", 
 "#67CFB5", "#8AE2AE", "#ACF6A8", "#BCF8B9", "#CBF9CA", "#DBFBDC", 
 "#EBFDED")
)
 
land.pal <- colorRampPalette(
 c("#336600", "#F3CA89", "#D9A627", 
 "#A49019", "#9F7B0D", "#996600", "#B27676", "#C2B0B0", "#E5E5E5", 
 "#FFFFFF")
)
zbreaks <- seq(min(tmp$z), max(tmp$z), by=5)
cols <- c(ocean.pal(sum(zbreaks<=0)), land.pal(sum(zbreaks>0)-1))
#cairo_pdf("surface.pdf", width=15, height=11.5)
par(mar=c(0,0,0,3))
library(fields)
image.plot(tmp, col=cols, asp=1, breaks=zbreaks, 
  useRaster=TRUE, xlab="", ylab="", axes=FALSE,
  legend.width = 2, axis.args=list(cex.axis=1.5),
  legend.shrink=.8, legend.mar=6)
#dev.off()

# (b)
library(colorRamps)
download.file("https://www.dropbox.com/s/6opewjw02wn0fj0/SurfaceWorld.xyz?dl=0",
              destfile="SurfaceWorld.xyz", method="curl", extra=" -L -k ", quiet=TRUE)
xyz <- read.delim("SurfaceWorld.xyz", header=F)
vals <- matrix(xyz[,3], 2161,1081)
long <- pi*matrix(xyz[,1], 2161,1081)/180
lati <- pi*matrix(xyz[,2], 2161,1081)/180
vals <- vals[nrow(vals):1,]
z <- (vals-min(vals))/diff(range(vals)*4) + 1
r <- z
z <- r*sin(lati)
y <- r*cos(lati)*cos(long)
x <- r*cos(lati)*sin(long)
nlevel = 1024
tmp <- round((nlevel-1)*vals/diff(range(vals)) + 1.5)
n1 <- -min(tmp)
n2 <- nlevel -n1 #max(tmp)  
col1 <- blue2red(2*n1)[1:n1]
col2 <- blue2red(2*n2)[(n2+1):(2*n2)]
col <- c(col1, col2)
col <- col[tmp+n1]
library(rgl)
persp3d(x, y, z, col=col,specular="black", axes=FALSE, box=FALSE, xlab="", ylab="", zlab="")


#####################################################################################################
#####################################################################################################
# Figure 3
# (a)
library(RgoogleMaps)
MyMap <- GetMap(center = c(37.5665,126.978), zoom =13, size = c(640, 640), 
	destfile = "seoul1.png", maptype="roadmap")
PlotOnStaticMap(MyMap, size = c(640, 640))

# (a)
MyMap <- GetMap(center = c(37.5665,126.978), zoom =13, size = c(640, 640), 
	destfile = "seoul1.png", maptype="roadmap")
PlotOnStaticMap(MyMap, size = c(640, 640))
# (b)
MyMap <- GetMap(center = c(37.5665,126.978), zoom =13, size = c(640, 640), 
	destfile = "seoul2.png", maptype="satellite")
PlotOnStaticMap(MyMap, size = c(640, 640))
# not included in the letter
MyMap <- GetMap(center = c(37.5665,126.978), zoom =13, size = c(640, 640), 
	destfile = "seoul3.png", maptype="terrain")
PlotOnStaticMap(MyMap, size = c(640, 640))

#####################################################################################################
#####################################################################################################
# Figure 4
library(ggmap)
library(jpeg)
library(png)
library(plyr)
source_https <- function(url, ...) {
  # load package
  require(RCurl)
  require(plyr)
  # parse and evaluate each .R script
  l_ply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, 
                             cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), 
         envir = .GlobalEnv)
  })
}
source_https(paste(gitadd, "GoogleMap/raw/master/StamenWatercolor.R", sep=""))

# (a)
#cairo_pdf("ggmap1.pdf", width=6, height=6)
qmap("seoul", zoom = 11, maptype = 'hybrid')
#dev.off()
# (b)
#cairo_pdf("ggmap2.pdf", width=6, height=6)
qmap("seoul", zoom = 11, source = 'osm')
#dev.off()
# (c)
#cairo_pdf("ggmap3.pdf", width=6, height=6)
qmap("seoul", zoom = 11, maptype = 'toner', source = 'stamen')
#dev.off()
# (d)
#cairo_pdf("ggmap4.pdf", width=6, height=6)
qmap("seoul", zoom = 11, maptype = 'watercolor', source = 'stamen')
#dev.off()


#####################################################################################################
#####################################################################################################
# Figure 5
library(ggmap)
download.file(paste(gitadd, "kssletter/raw/master/cleanair.csv", sep=""),
              destfile="cleanair.csv", method="curl", extra=" -L -k ", quiet=TRUE )
              
if(Encoding("a") == "unknown" &  .Platform$OS.type =="windows"){
  pm10 <- read.csv("cleanair.csv", stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8", encoding = "EUC-KR")
} else{
  pm10 <- read.csv("cleanair.csv", stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8", encoding = "UTF-8")
}


mstodeg <- function(x){
  x <- as.numeric(x)
  x[1] + x[2]/60 + x[3]/3600
}
pm10$경도 <- unlist(lapply(strsplit(pm10$경도, ":"), mstodeg))
pm10$위도 <- unlist(lapply(strsplit(pm10$위도, ":"), mstodeg))

seoulmap <- qmap("seoul", zoom = 11, maptype = 'toner', source = 'stamen')
seoulmap + geom_point(mapping=aes(x = 경도, y = 위도), colour=brewer.pal(9, "Set1")[1], 
                      cex=4, data = pm10)
# location of observation
gm <- get_googlemap(center = "seoul", zoom = 12, filename = "ggmapTemp")
location <- as.numeric(attr(gm, "bb"))[c(2, 1, 4, 3)]
out <- get_stamenmap(bbox = location, zoom = 12, maptype = "toner", 
                     crop = TRUE, messaging = FALSE, urlonly = FALSE, 
                     filename = "ggmapTmp", color = "color")
xmin <- attr(out, "bb")$ll.lon
xmax <- attr(out, "bb")$ur.lon
ymin <- attr(out, "bb")$ll.lat
ymax <- attr(out, "bb")$ur.lat

outraster <- as.raster(out)

library(fields)
fit1 <- Tps(pm10[, c("경도", "위도")], pm10$pm10, lambda=0.000001)
result1 <- predictSurface(fit1, grid.list = NULL, extrap = FALSE, 
                          nx = 200, ny = 200, drop.Z = TRUE)
fit2 <- Tps(pm10[, c("경도", "위도")], pm10$pm2.5)
result2 <- predictSurface(fit2, grid.list = NULL, extrap = FALSE, 
                          nx = 200, ny = 200, drop.Z = TRUE)
fit3 <- Tps(pm10[, c("경도", "위도")], pm10$no2)
result3 <- predictSurface(fit3, grid.list = NULL, extrap = FALSE, 
                          nx = 200, ny = 200, drop.Z = TRUE)
fit4 <- Tps(pm10[, c("경도", "위도")], pm10$o3)
result4 <- predictSurface(fit4, grid.list = NULL, extrap = FALSE, 
                          nx = 200, ny = 200, drop.Z = TRUE)
fit5 <- Tps(pm10[, c("경도", "위도")], pm10$so2)
result5 <- predictSurface(fit5, grid.list = NULL, extrap = FALSE, 
                          nx = 200, ny = 200, drop.Z = TRUE)
fit6 <- Tps(pm10[, c("경도", "위도")], pm10$co)
result6 <- predictSurface(fit6, grid.list = NULL, extrap = FALSE, 
                          nx = 200, ny = 200, drop.Z = TRUE)

library(scales)
par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
plot(c(xmin, xmax), c(ymin, ymax), type = "n", xlab = "", ylab = "")
rasterImage(outraster, xmin, ymin, xmax, ymax, interpolate = TRUE)
image(result1, add=TRUE, col=alpha(tim.colors(64), .7))

par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
plot(c(xmin, xmax), c(ymin, ymax), type = "n", xlab = "", ylab = "")
rasterImage(outraster, xmin, ymin, xmax, ymax, interpolate = TRUE)
image(result2, add=TRUE, col=alpha(tim.colors(64), .7))

for(i in 1:6){
  #cairo_pdf(paste("airseoul", i, ".pdf", sep=""), width=10, height=6.9)
  par(mar = c(0,0,0,2), xaxs = "i", yaxs = "i")
  mat <- matrix(1:2, ncol=2)
  layout(mat, width=c(10,1))
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", xlab = "", ylab = "", asp=1, axes=FALSE)
  #box()
  rasterImage(outraster, xmin, ymin, xmax, ymax, interpolate = TRUE)
  
  assign("result", eval(parse(text= paste("result", i, sep=""))))
  image(result, add=TRUE, col=alpha(tim.colors(64), .7), useRaster=TRUE)
  zrng <- range(result$z, na.rm=TRUE)
  zseq <- seq(zrng[1], zrng[2],, 64)
  par(mar = c(6,0,6,3))
  image(list(x=1, y=zseq, z=matrix(zseq, nrow=1)), useRaster=TRUE, axes=FALSE, col=tim.colors(64))
  axis(4, cex.axis=3, padj=.5)
  #dev.off()
}



#####################################################################################################
#####################################################################################################
# Figure 6
download.file(paste(gitadd, "/kssletter/raw/master/9711.csv", sep=""), 
              destfile="bus9711.csv", method="curl", extra=" -L -k ", quiet=TRUE)
bus9711 <- read.csv("bus9711.csv")
head(bus9711, 6)

library(RColorBrewer)
library(scales)
#cairo_pdf("ggmap5.pdf", width=9, height=9)
seoulmap <- qmap("seoul", zoom = 11, maptype = 'toner', source = 'stamen')
seoulmap + geom_path(mapping=aes(x = x, y = y), colour=brewer.pal(9, "Set1")[1], 
	lwd=2, data = bus9711)
#dev.off()

source_https(paste(gitadd, "DaumMap/raw/master/getDaummap.R", sep=""))
source_https(paste(gitadd, "NaverMap/raw/master/getNavermap.R", sep=""))

lon <- c(126.7405, 127.0398)
lat <- c(37.46889, 37.67667)
nmap <- getNaverMap(lon, lat, zoom=NA,  maproj = "Naver")
lon <- c(126.7405, 127.0398)
lat <- c(37.45889, 37.68667)
dmap <- getDaumMap(lon, lat, zoom=NA,  maproj = "Daum")

# (a)
#cairo_pdf("naver.pdf", width=8, height=7)
par(mar=c(0,0,0,0))
plot(nmap)
lines(WGS842Naver(bus9711), col=brewer.pal(9, "Set1")[1], lwd=4)
#dev.off()
# (b)
library(RColorBrewer)
library(scales)
source_https(paste(gitadd, "kssletter/raw/master/smartcardsource.R", sep=""))
download.file(paste(gitadd, "kssletter/raw/master/9711vol.RData", sep=""),
              destfile="9711vol.RData", method="curl", extra=" -L -k " )
load("9711vol.RData")

out <- pathvolpoly(res, pathvol, scl=1.2)
polygo <- out$polygo
polyback <- out$polyback
colval <- alpha(brewer.pal(9, "Set1"), .6)
#cairo_pdf("daum.pdf", width=8, height=7)
par(mar=c(0,0,0,0))
plot(dmap)
polygon(polygo/2.5, col=colval[1], border="white")
polygon(polyback/2.5, col=colval[2], border="white")
#dev.off()


#####################################################################################################
#####################################################################################################
# Figure 8
library(maptools)
library(rgdal)
library(RColorBrewer)

# (a)
dir.create("2012_1_0")
download.file(paste(gitadd, "kssletter/raw/master/2012_1_0/temp.shp", sep=""),
              destfile="2012_1_0/temp.shp", method="curl", extra=" -L -k " )
download.file(paste(gitadd, "kssletter/raw/master/2012_1_0/temp.dbf", sep=""),
              destfile="2012_1_0/temp.dbf", method="curl", extra=" -L -k " )
download.file(paste(gitadd, "kssletter/raw/master/2012_1_0/temp.shx", sep=""),
              destfile="2012_1_0/temp.shx", method="curl", extra=" -L -k " )

proj4val <- "+proj=tmerc +lat_0=38 +lon_0=127.0028902777778 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel
+units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43"
nc <- readShapePoly("2012_1_0/temp.shp", proj4string=CRS(proj4val))
nc <- spTransform(nc, CRS("+init=epsg:4326")) 
#cairo_pdf("sgis2.pdf", width=8, height=7)
cols <- c(brewer.pal(9, "Set1"), brewer.pal(7,"Accent"))
par(mar=c(0,0,0,0))
plot(nc, col=cols, lwd=.1, border=1)
#dev.off()

# (b)
library(plyr)
val2col2 <- function(z, zlim, col = heat.colors(12), breaks){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    #breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col))) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col))) 
    #breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  colorlevels <- col[((as.vector(z)-breaks[1])/(range(breaks)[2]-range(breaks)[1]))*(length(breaks)-1)+1] # assign colors to heights for each point
  colorlevels[z > zlim[2]] <- col[length(col)]
  colorlevels
}

library(data.table)
download.file(paste(gitadd, "kssletter/raw/master/cencus2010_den_wgs1984.RData", sep=""),
              destfile="cencus2010_den_wgs1984.RData", method="curl", extra=" -L -k " )

load('cencus2010_den_wgs1984.RData')
name <- unlist(lapply(out, function(x) x$name))
ind <- which(substring(name, 1, 5) == 11230)
out2 <- out[ind]

dir.create("2012_2_11230")
download.file(paste(gitadd, "kssletter/raw/master/2012_2_11230/temp.shp", sep=""),
              destfile="2012_2_11230/temp.shp", method="curl", extra=" -L -k " )
download.file(paste(gitadd, "kssletter/raw/master/2012_2_11230/temp.dbf", sep=""),
              destfile="2012_2_11230/temp.dbf", method="curl", extra=" -L -k " )
download.file(paste(gitadd, "kssletter/raw/master/2012_2_11230/temp.shx", sep=""),
              destfile="2012_2_11230/temp.shx", method="curl", extra=" -L -k " )
              
              
#png("census2010_den.png", width=2500, height=2000)
#cairo_pdf("census2010_den.pdf", width=12.5, height=10)
mat <- matrix(1:2, nrow=2)
layout(mat, height=c(9,2))
par(mar=c(1,4,0,2), family="NanumGothic")
proj4val <- "+proj=tmerc +lat_0=38 +lon_0=127.0028902777778 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel
             +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43"
nc <- readShapePoly("2012_2_11230/temp.shp", proj4string=CRS("+init=epsg:2097"))
nc <- spTransform(nc, CRS("+init=epsg:4326")) 
plot(nc, col=gray(.9), border=gray(.9))
zlim <- range(unlist(lapply(out, function(x) x[[3]])), na.rm=TRUE)
zlim[2] <- 60000
cols <- tim.colors(64) # colorRampPalette(c("green", "red"), space="rgb")(64)
l_ply(out2, function(x) plot(x$Geometry, 
                             col=alpha(val2col2(x$popdensity, zlim, col=cols), 1), 
                             add=TRUE, 
                             border="white", 
                             lwd=.1))
par(mar=c(4,8,3.2,8))
image(matrix(seq(0,1,,200)), col=tim.colors(200), axes=FALSE, useRaster=TRUE)
seqval <- seq(floor(zlim[1]), ceiling(zlim[2]))
labval <- seqval[!duplicated(floor(seqval/10000)*10000)]
atval <- (labval - floor(zlim[1]))/(ceiling(zlim[2])-floor(zlim[1]))
labval <- labval[-1]
labval[length(labval)] <- paste(">", ceiling(zlim[2]), sep="")
atval <- atval[-1]
axis(1, at=atval, labels=labval, cex.axis=3, padj=.7)
mtext(expression(group("(", bold(명/km^2), ")")), side=1, line=2.1, at=1.07, cex=2)
#dev.off()



#####################################################################################################
#####################################################################################################
# Figure 9
# Based on Figure 8
download.file(paste(gitadd, "kssletter/raw/master/seoul_subway2.R", sep=""),
              destfile="seoul_subway2.R", method="curl", extra=" -L -k " )
              
if(Encoding("a") == "unknown" & .Platform$OS.type =="windows"){
	download.file(paste(gitadd, "kssletter/raw/master/SeoulSubwayShp2.zip", sep=""),
              	      destfile="SeoulSubwayShp.zip", method="curl", extra=" -L -k " )
} else {
	download.file(paste(gitadd, "kssletter/raw/master/SeoulSubwayShp.zip", sep=""),
              	      destfile="SeoulSubwayShp.zip", method="curl", extra=" -L -k " )
}
unzip("SeoulSubwayShp.zip", exdir="SeoulSubwayShp")
shpfolder <- "SeoulSubwayShp"


library(maptools)
library(rgdal)
proj4val <- "+proj=tmerc +lat_0=38 +lon_0=127.0028902777778 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel
             +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43"
nc <- readShapePoly("2012_1_0/temp.shp", proj4string=CRS(proj4val))
nc <- spTransform(nc, CRS("+init=epsg:4326")) 
nc$name <- as.factor(iconv(as.character(nc$name), "EUC-KR", "UTF-8"))
ncs <- subset(nc, name=="서울특별시")

#cairo_pdf("subway.pdf", width=5, height=5)
par(mar=c(0,0,0,0))
plot(ncs, col=gray(.7))
source("seoul_subway2.R", encoding="UTF-8")
#dev.off()



#####################################################################################################
#####################################################################################################
# Figure 10
library(osmar)
api <- osmsource_api()
bbox  <- corner_bbox(126.9450, 37.445, 126.963, 37.470)
osmmap <- get_osm(bbox, source = api)
# (a)
#cairo_pdf("snu1.pdf", width=7, height=8)
par(mar=c(0,0,0,0))
plot(bbox[c(1,3)], bbox[c(2,4)], asp=1, xlab="", ylab="", type="n", axes=FALSE)
plot_ways(osmmap, add=TRUE)
#dev.off()

# (b)
buildingid <- find(osmmap, way(tags(id ==185936759 | k == "building" | v ==  "농업생명과학대학 (200)")))
ids <- find_down(osmmap, way(buildingid))
building <- subset(osmmap, ids=ids)


road1id <- find(osmmap, way(tags(v=="motorway" | v=="motorway_link")))
ids <- find_down(osmmap, way(road1id))
road1 <- subset(osmmap, ids=ids)

road2id <- find(osmmap, way(tags(k=="highway" & v=="secondary")))
ids <- find_down(osmmap, way(road2id))
road2 <- subset(osmmap, ids=ids)

road3id <- find(osmmap, way(tags(k=="highway" & v=="residential")))
ids <- find_down(osmmap, way(road3id))
road3 <- subset(osmmap, ids=ids)

road4id <- find(osmmap, way(tags(k=="highway" & (v=="tertiary" | v=="tertiary_link"))))
ids <- find_down(osmmap, way(road4id))
road4 <- subset(osmmap, ids=ids)

road5id <- find(osmmap, way(tags(k=="highway" & v=="living_street")))
ids <- find_down(osmmap, way(road5id))
road5 <- subset(osmmap, ids=ids)

parkid <- find(osmmap, way(tags((v=="wood" | v=="park" | v=="grass") & id != 290466559 & id != 206049084)))
ids <- find_down(osmmap, way(parkid))
park <- subset(osmmap, ids=ids)

areaid <- find(osmmap, way(tags(v=="university" & id == 173556800)))
ids <- find_down(osmmap, way(areaid))
area <- subset(osmmap, ids=ids)


library(maps)
inpoly <- function(xx){
  any(maps:::in.polygon(as_sp(area, "polygons")@polygons[[1]]@Polygons[[1]]@coords, 
                        xx@Polygons[[1]]@coords))
}
ids <- unlist(lapply(as_sp(park, "polygons")@polygons, inpoly))
park <- subset(as_sp(park, "polygons"), ids)


#cairo_pdf("snu2.pdf", width=7, height=8)
library(sp)
cols <- brewer.pal(9, "Set1")
par(mar=c(0,0,0,0))
plot(bbox[c(1,3)], bbox[c(2,4)], asp=1, xlab="", ylab="", type="n", axes=FALSE)
plot(as_sp(area, "polygons"), add=TRUE, col="grey")
plot(park, add=TRUE, col=cols[3])
plot_ways(road1, add=T, lwd=2, col=cols[2], lty=3)
plot_ways(road2, add=T, lwd=7, col=cols[2])
plot_ways(road3, add=T, lwd=2, col=cols[2])
plot_ways(road4, add=T, lwd=2, col=cols[2])
plot_ways(road5, add=T, lwd=1, col=cols[2])
plot(as_sp(building, "polygons"), col = cols[4], add=TRUE)
#dev.off()
