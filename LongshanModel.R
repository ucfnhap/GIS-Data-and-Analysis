####need to install the packages if first time run the model, by uncomment the lines
# install.packages("Rcpp")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("sp")
##########################

library(ggplot2)
library(raster)
library(rgdal)


##
archeologicalsites <- raster("Longshan.tif")
elevation <- raster("DEMOnefit.tif")
slope <- raster("Slope.tif")
aspect <- raster("Aspect.tif")
distoriver <- raster("DistancetoRivers.tif")
distomajorsites <- raster("DistancetoMajorsites.tif")

Raster_vars <- stack (elevation, slope, aspect)
plot (Raster_vars)

##convert to spatial points data frame
archeologicalsitespoints <- as(archeologicalsites, "SpatialPointsDataFrame")

#separate the dataset as truepoints and false poitns, truepoints means identified archeological sites
truepoints = subset(archeologicalsitespoints, archeologicalsitespoints[[1]] == 1)
#sample true points
selectedtruepoints <- sample(1:length(truepoints[[1]]), 0.8 * length(truepoints))
truepointframe= truepoints[selectedtruepoints,]

#falsepoints means identified nonarcheological sites
falsepoints = subset(archeologicalsitespoints, archeologicalsitespoints[[1]] == 0)
#sample false points
selectedfalsepoints <- sample(1:length(falsepoints[[1]]), 0.8 * length(truepoints))
falsepointframe= falsepoints[selectedfalsepoints,]

##true sites list
tarcheologicalsites <- raster::extract(archeologicalsites, truepointframe)
televation <- raster::extract(elevation, truepointframe)
tslope <- raster::extract(slope, truepointframe)
taspect <- raster::extract(aspect, truepointframe)
tdistoriver <- raster::extract(distoriver, truepointframe)
tdistomajorsites <- raster::extract(distomajorsites, truepointframe)

##false sites list
farcheologicalsites <- raster::extract(archeologicalsites, falsepointframe)
felevation <- raster::extract(elevation, falsepointframe)
fslope <- raster::extract(slope, falsepointframe)
faspect <- raster::extract(aspect, falsepointframe)
fdistoriver <- raster::extract(distoriver, falsepointframe)
fdistomajorsites <- raster::extract(distomajorsites, falsepointframe)

##combine the two datasets together
trainarcheologicalsites <- append(tarcheologicalsites,farcheologicalsites)
trainelevation <- append(televation,felevation)
trainslope <- append(tslope,fslope)
trainaspect <- append (taspect, faspect)
traindistoriver <- append (tdistoriver, fdistoriver)
traindistomajorsites <- append (tdistomajorsites, fdistomajorsites)

## put data to data frame
trainingdata <- data.frame(trainarcheologicalsites,trainelevation,trainslope,trainaspect,traindistoriver,traindistomajorsites)

## logistic regression
apredictmodel <- glm(formula = trainarcheologicalsites ~ trainelevation + trainslope + trainaspect + traindistoriver + traindistomajorsites,
                     data=trainingdata, family = binomial)

Summary(apredictmodel)
# 
# ##predict yangshao culture
# pred_raster_yangshao <- archeologicalsites[[1]]
# vdata <- stack(elevation, slope, aspect,distoriver,distomajorsites)
# cc <- calc(vdata,function(x){apredictmodel$coefficients[2]})
# predict_yangshao <- predict(vdata,cc)
# plot(predict_yangshao)



