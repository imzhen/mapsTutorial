library(maptools)

img_pos=data.frame(
        x=c(103,103,403,403),
        y=c( 75,275, 75,275))
real_pos=data.frame(
        x=c(105,105,115,115),
        y=c( 27, 20, 27, 20))

data_x=data.frame(img=img_pos$x,rel=real_pos$x)
data_y=data.frame(img=img_pos$y,rel=real_pos$y)

lm_x=lm(rel~img, data=data_x)
lm_y=lm(rel~img, data=data_y)

mytrans_x=function(myimg){
     predict(lm_x, newdata=data.frame(img=myimg))} 
mytrans_y=function(myimg){
     predict(lm_y, newdata=data.frame(img=myimg))} 


myfiles=c("Jiana.xls","Kutedan.xls","Miyaluo.xls","Woda.xls","Yada.xls")
mypolys=lapply(myfiles, 
               function(x){
                 tmp=read.table(paste0("data/",x));
                 tmp=rbind(tmp,tmp[1,]);
                 tmp$X=mytrans_x(tmp$X);
                 tmp$Y=mytrans_y(tmp$Y);
                 tmp})

mynames=sub(".xls$","",myfiles)
names(mypolys) = mynames

myPolygons = lapply(mynames, 
                    function(x){
                      tmp=mypolys[[x]];
                      Polygons(list(Polygon(cbind(tmp$X,tmp$Y))),x)})

mySpn = SpatialPolygons(myPolygons)

myCNnames=c("嘉纳","库特丹","米亚洛","沃达","雅达");
myshpdata = SpatialPolygonsDataFrame(mySpn, 
                                     data=data.frame(
                                        Names=mynames,
                                        CNnames=myCNnames,
                                        row.names=row.names(mySpn)))

writePolyShape(x=myshpdata, fn="data/myDIYmap_poly")
