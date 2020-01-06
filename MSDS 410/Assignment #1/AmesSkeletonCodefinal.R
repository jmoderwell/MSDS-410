
setwd("C:/Users/R/Desktop/MSDS 410")

##################

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")

str(mydata)
head(mydata)
names(mydata)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF
mydata$HouseArea <- mydata$GrLivArea + mydata$TotalBsmtSF
summary(mydata$price_sqft)
hist(mydata$price_sqft)

building_type_table <- table(newdata$BldgType, newdata$Zoning)
building_type_table

require(dplyr)

str(mydata$QualityIndex)
newdata <- mydata %>% dplyr::filter(Zoning %in% c("FV", "RH", "RL", "RM") & BldgType %in% "1Fam")                 
newdata <- newdata[which(newdata$TotalFloorSF < 4000),]
newdata <- subset(newdata, select = -c(SID, PID))
newdata <- subset(newdata, select = c(SalePrice, Neighborhood, QualityIndex, logSalePrice, HouseArea, LotArea, HouseAge, TotalFloorSF, SaleCondition, PavedDrive, GarageArea, GarageFinish, FireplaceQu, KitchenQual, TotRmsAbvGrd, OverallQual, YearBuilt, BsmtQual, TotalBsmtSF, FirstFlrSF, GrLivArea, FullBath, GarageCars, SecondFlrSF, CentralAir, QualityIndex))

newdatatable <- table(lapply(newdataintegers, mean))

newdataintegerssummary <- summary(newdataintegers, na.rm = TRUE)
newdataintegerssummary

newdatafactorssummary <- summary(newdatafactors)
newdatafactorssum

newdataintegers <- newdata[,sapply(newdata, is.integer)]

newdatafactors <- newdata[, sapply(newdata,is.factor)]
newdatafactors <- na.omit(newdatafactors)

summary(newdatafactors)

str(newdataintegers)

newdatafinal <- subset(newdata, select = c(SalePrice, logSalePrice, HouseArea, OverallQual, GrLivArea, TotalFloorSF, HouseAge, TotalBsmtSF, GarageArea, TotRmsAbvGrd))

summary(newdatafinal)

                 

require(dlookr)
dataqualitycheck <- diagnose(newdata)
dataqualitycheck

subdat <- subset(mydata, select= c() )

str(subdat)


subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                  "SalePrice","LotArea","OverallQual","logSalePrice"))
#####################################################################
######################### Assignment 1 ##############################
#####################################################################

#################################################################
################## univariate EDA ##############################
###############################################################
require(ggplot2)
ggplot(subdat) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


ggplot(newdata, aes(x=CentralAir)) + 
 geom_bar(color="blue") +
  labs(title="Bar Plot of Central Air") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)),

ggplot(newdata, aes(x=FireplaceQu)) + 
  geom_bar(color="blue") +
  labs(title="Bar Plot of Fireplace Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)),

ggplot(newdata, aes(x=KitchenQual)) + 
  geom_bar(color="blue") +
  labs(title="Bar Plot of Kitchen Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(newdata, aes(x=BsmtQual)) + 
  geom_bar(color="blue") +
  labs(title="Bar Plot of Basement Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)),

ggplot(newdata, aes(x=GarageFinish)) + 
  geom_bar(color="blue") +
  labs(title="Bar Plot of Garage Finish Level") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)), ncol= 3, nrow = 2)

ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=QualityIndex)) + 
  geom)(color="black", binwidth= 10) +
  labs(title="Distribution of QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)
#######################################################################
########### bivariate EDA ########################################
###################################################################
ggplot(newdata, aes(x=KitchenQual, y=SalePrice)) + 
  geom_boxplot(color="blue", shape=1) +
  ggtitle("Box Plot of KitchenQual vs SalePrice") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(newdata, aes(x=OverallQual, y=SalePrice, fill = OverallQual)) + 
  geom_boxplot(fill=OverallQual) +
  labs(title="Overall Quality vs Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

############################################################
################ model focussed EDA #######################
###########################################################

ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm, se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) + 
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

require(gridExtra)
grid.arrange(saleconditionplot, paveddriveplot, garagefinishplot, fireplacequplot, kitchenqualplot, bsmtqualplot, centralairplot, ncol = 3, nrow = 3)

saleconditionplot <- ggplot(newdata, aes(x= SaleCondition, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Sale Condition vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

paveddriveplot <- ggplot(newdata, aes(x= PavedDrive, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Paved Drive vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

garagefinishplot <- ggplot(newdata, aes(x= GarageFinish, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Garage Finish vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

fireplacequplot <- ggplot(newdata, aes(x= FireplaceQu, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Fireplace Quality vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

kitchenqualplot <- ggplot(newdata, aes(x= KitchenQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Kitchen Quality vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

bsmtqualplot <- ggplot(newdata, aes(x= BsmtQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Basement Quality vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

centralairplot <- ggplot(newdata, aes(x= CentralAir, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Central Air vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))



grid.arrange(lotareaplot, houseageplot, totalfloorsfplot, garageareaplot, totalroomsabovegradeplot, overallqualplot, yearbuiltplot, totalbasementsfplot, firsfloorsfplot, grlivareaplot, fullbathplot, garagecarsplot, secondfloorsfplot, ncol = 4, nrow = 4)

lotareaplot <- ggplot(newdata, aes(x=LotArea, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Lot Area vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 


houseageplot <- ggplot(newdata, aes(x=HouseAge, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="House Age vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 


totalfloorsfplot <- ggplot(newdata, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(fill="red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title="Total Floor Sq. Footage vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 


garageareaplot <- ggplot(newdata, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Garage Area vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

totalroomsabovegradeplot <- ggplot(newdata, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(fill="red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title="Total Rooms above Grade vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

overallqualplot <- ggplot(newdata, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(fill="red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title="Overall Quality vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

overallqualbplot <- ggplot(newdata, aes(x= OverallQual, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot of Overall Quality vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

totalfloorsfbplot <- ggplot(newdata, aes(x= TotalFloorSF, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot of Total Floor Area vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

grlivareabplot <- ggplot(newdata, aes(x= GrLivArea, y=SalePrice, fill = newdata$KitchenQual)) + 
  geom_boxplot(fill="blue") + geom_smooth(method = "lm", se = FALSE) +
  labs(title="Boxplot of Above Grade Living Area vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

firstfloorbplot <- ggplot(newdata, aes(x= FirstFlrSF, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Boxplot of Total First Floor Area vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

grid.arrange(overallqualplot, totalfloorsfplot, grlivareaplot, firsfloorsfplot, ncol = 2, nrow = 2)

yearbuiltplot <- ggplot(newdata, aes(x=YearBuilt, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Year Built vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

totalbasementsfplot <- ggplot(newdata, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Total Basement Sq. Footage vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

firsfloorsfplot <- ggplot(newdata, aes(x=FirstFlrSF, y= log(SalePrice), color = KitchenQual)) + 
  geom_point(fill="red") + 
  labs(title="First Floor Sq. Footage vs log(SP) by Kitchen Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 
firsfloorsfplot

grlivareaplot <- ggplot(newdata, aes(x=GrLivArea, y=log(SalePrice), color = KitchenQual)) + 
  geom_point(fill= "red") + 
  labs(title="Living Area Above Grade vs log(SP) by Kitchen Quality") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

grlivareaplot

fullbathplot <- ggplot(newdata, aes(x=FullBath, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Full Bath vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 


garagecarsplot <- ggplot(newdata, aes(x=GarageCars, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Garage size (# of cars) vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 


secondfloorsfplot <- ggplot(newdata, aes(x=SecondFlrSF, y=SalePrice)) + 
  geom_point(fill="red") +
  labs(title="Second Floor Sq. Footage vs SP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#####################################################################
############# EDA for multiple variables ###########################
##################################################################
require(GGally)
ggpairs(newdatafinal)

require(GGally)
ggally_cor(newdata, mapping = ggplot2::aes_string(x = "KitchenQual", y = "SalePrice"))

require(lattice)
pairs(newdataintegers, pch = 21)

require(corrplot)
mcor <- cor(newdataintegers)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5, diag = TRUE, na.label = "square", na.label.col = "grey",  addCoef.col = "black")

#####################################################################
############# Define the sample data ###########################
##################################################################

subdat2 <- subdat[which(subdat$TotalFloorSF < 4000),]

###################################################################
##################  Assignment 2  ################################
#################################################################

attach(subdat2)

ggplot(subdat2, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

ggplot(subdat2, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###



# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d)
attach(newdatafinal)
s3d <-scatterplot3d(HouseArea,SalePrice,GarageArea,
                    highlight.3d=TRUE,type="h", main="3D Scatterplot")

fit <- lm(SalePrice ~ HouseArea) 
s3d$plane3d(fit)

library(Rcmdr)
attach(subdat2)
scatter3d(SalePrice,TotalFloorSF,QualityIndex)

############## fitting a SLR ###################################

SLR_kitchenqual = lm(SalePrice ~ KitchenQual, data = newdata)
anova(SLR_kitchenqual)
summary(SLR_kitchenqual)
par(mfrow = c(2,2))
plot(SLR_kitchenqual)

res_kq <- residuals(SLR_kitchenquals)

fit_kq <- fitted(SLR_kitchenqual)

require(forecast)

checkresiduals(SLR_kitchenqual)
par(mfrow = c(1,2))
hist(res_kq, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(res_kq, col = "red", main = "Boxplot of Residuals", ylab = "Residual")

library(plyr)
subdat1 <- ddply(newdata, .(KitchenQual), summarise, 
                 Mean_Sale_Price = mean(SalePrice)
subdat1

summary(newdata$KitchenQual)

newdata$KitchenQualExcellent1 <- 
  ifelse(newdata$KitchenQual == "Ex", 1, 0)
newdata$KitchenQualGood1 <- 
  ifelse(newdata$KitchenQual == "Gd", 1, 0)
newdata$KitchenQualTypical1 <- 
  ifelse(newdata$KitchenQual == "TA", 1, 0)
newdata$KitchenQualFair1 <- 
  ifelse(newdata$KitchenQual == "Fa", 1, 0)
newdata$KitchenQualPoor1 <- 
  ifelse(newdata$KitchenQual == "Po", 1, 0)

subdat$NbhdGrp1 <- 
  ifelse(subdat$NbhdGrp == "grp1", 1, 0)
subdat$NbhdGrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$NbhdGrp3 <- 
  ifelse(subdat$NbhdGrp == "grp3", 1, 0)

KitchenQual_Dummy <- lm(SalePrice ~ KitchenQualGood1+KitchenQualTypical1+KitchenQualFair1+KitchenQualPoor1, data = newdata)
anova(KitchenQual_Dummy)
summary(KitchenQual_Dummy)

SLRresult = lm(SalePrice ~ TotalFloorSF, data=subdatnum)#subdat2
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(1,1))  # visualize four graphs at once

simple_model1 <- lm(SalePrice ~ HouseArea, data = newdatafinal)
anova(simple_model1)
summary(simple_model1)

par(mfrow=c(2,2))
plot(simple_model1)

SLR1_Res <- residuals(simple_model1)
summary(SLR1_Res)
SLR1_Res_fit <- fitted(simple_model1)
par(mfrow = c(1,2))
hist(SLR1_Res, col = "blue", main = "Histogram of Residuals", xlab = "Residual")
boxplot(SLR1_Res, col = "blue", main = "Boxplot of Residuals", ylab = "Residual")
par(mfrow = c(1,1))
moments::kurtosis(r)
moments::skewness(r)

SLR2_Res <- residuals(simple_model2)
summary(SLR2_Res)
SLR2_Res_fit <- fitted(simple_model2)
par(mfrow = c(1,2))
hist(SLR2_Res, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(SLR1_Res, col = "red", main = "Boxplot of Residuals", ylab = "Residual")
par(mfrow = c(1,1))

MLR_Res <- residuals(Multiple_model)
summary(MLR_Res)
SLR2_Res_fit <- fitted(Multiple_model)
par(mfrow = c(1,2))
hist(SLR2_Res, col = "green", main = "Histogram of Residuals", xlab = "Residual")
boxplot(SLR1_Res, col = "green", main = "Boxplot of Residuals", ylab = "Residual")
par(mfrow = c(1,1))

SLR_log_res <- residuals(simple_model1_log)
summary(SLR_log_res)
SLR_log_res_fit <- fitted(simple_model1_log)
par(mfrow = c(1,2))
hist(SLR_log_res, col = "purple", main = "Histogram of Residuals", xlab = "Residual")
boxplot(SLR_log_res, col = "purple", main = "Boxplot of Residuals", ylab = "Residual")
par(mfrow = c(1,1))

SLR2_log_res <- residuals(simple_model2_log)
summary(SLR2_log_res)
SLR2_log_res_fit <- fitted(simple_model2_log)
par(mfrow = c(1,2))
hist(SLR2_log_res, col = "yellow", main = "Histogram of Residuals", xlab = "Residual")
boxplot(SLR2_log_res, col = "yellow", main = "Boxplot of Residuals", ylab = "Residual")
par(mfrow = c(1,1))

MLR_log_res <- residuals(multiple_model_log)
summary(MLR_log_res)
MLR_log_res_fit <- fitted(multiple_model_log)
par(mfrow = c(1,2))
hist(MLR_log_res, col = "orange", main = "Histogram of Residuals", xlab = "Residual")
boxplot(MLR_log_res, col = "orange", main = "Boxplot of Residuals", ylab = "Residual")
par(mfrow = c(1,1))

model1_summary <- summary(simple_model1)
model1_coeffs <- model1_summary$coeficients

beta1_estimate <- model1_coeffs["HouseArea", "Estimate"]

std_error1 <- model1_coeffs["HouseArea", "Std. Error"]


simple_prediction1 <- as.data.frame(predict(simple_model1,newdatafinal,interval="prediction"))
str(simple_prediction1)
head(simple_prediction1)
newdatafinal <- cbind(newdatafinal,simple_prediction1)
str(newdatafinal)
head(newdatafinal)
newdatafinal <- subset( newdatafinal, select = -lwr)
newdatafinal <- subset(newdatafinal, select = -upr)
library(reshape)
newdatafinal <- rename(newdatafinal, c(fit="fitSLR"))

simple_model2 <- lm(SalePrice ~ GarageArea, data = newdatafinal)
anova(simple_model2)
summary(simple_model2)

model1_summary <- summary(simplemodel2)

par(mfrow = c(2,2))
plot(simple_model2)



simple_prediction2 <- as.data.frame(predict(simple_model2,newdatafinal,interval="prediction"))
str(simple_prediction2)
head(simple_prediction2)
newdatafinal <- cbind(newdatafinal,simple_prediction2)
str(newdatafinal)
head(newdatafinal)
newdatafinal <- subset(newdatafinal, select = -lwr)
newdatafinal <- subset(newdatafinal, select = -upr)
library(reshape)
newdatafinal <- rename(newdatafinal, c(fit="fitSLR2"))

newdatafinal

plot(newdatafinal$SalePrice, newdatafinal$fitSLR)

require(Metrics)
rmse(newdatafinal$SalePrice, newdatafinal$fitSLR)

newdatafinal
############## fitting a MLR ###################################

summary(newdataintegers)

MLRresult = lm(SalePrice ~ TotalFloorSF+GrLivArea, data=newdataintegers)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

Multiple_model <- lm(SalePrice ~ HouseArea+GarageArea, data = newdatafinal)
anova(Multiple_model)
summary(Multiple_model)
par(mfrow=c(2,2))
plot(Multiple_model)

multiple_pred <- as.data.frame(predict(Multiple_model,newdatafinal,interval="prediction"))
str(multiple_pred)
head(multiple_pred)
newdatafinal <- cbind(newdatafinal,multiple_pred)
newdatafinal <- subset(newdatafinal, select = -lwr)
newdatafinal <- subset(newdatafinal, select = -upr)
str(newdatafinal)
head(newdatafinal)
newdatafinal <- rename(newdatafinal, c(fit="fitMLR"))
newdatafinal$res <- newdatafinal$SalePrice - newdatafinal$fitMLR

head(newdatafinal)

predictions_all <- subset(newdatafinal, select = c(SalePrice, fitSLR, fitSLR2, fitMLR))

predictions_all                          
                          
###################################################################
##################### Assignment 3  ################################
#################################################################

################  MAE calculation ###################################
Model1 = lm(SalePrice ~ TotalFloorSF+QualityIndex, data=newdata)
anova(MLRresult)
summary(MLRresult)

par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

checkresiduals(MLRresult)


pred1 <- as.data.frame(predict(Model1, newdata))

names(pred1)

library(reshape)

pred1 <- rename(pred1, c("predict(Model1, newdata)" = "prd"))

pred1$prd

newdata$pred1 <- pred1$prd

newdata$res1 <- newdata$SalePrice - newdata$pred1

newdata$absres1 <- abs(newdata$res1)

MAE1 <- mean(newdata$absres1)

res.mean <- ddply(newdata, .(Neighborhood), summarize, Res_mean = mean(res1))

require(knitr)
res.meankable<- kable(res.mean, format = "html", caption = "Model 1 Residual Means by Neighborhood")

require(ggplot2)
ggplot(newdata, aes(x=Neighborhood, y=res1, col = Neighborhood)) + 
  geom_boxplot() +
  labs(title="Box Plots of Residuals by Neighborhood") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

newdata1 <- ddply(newdata, .(Neighborhood), summarize, 
                 MAE = mean(absres1))
newdata2 <- ddply(newdata, .(Neighborhood), summarize, 
                 MeanPrice = mean(SalePrice))
newdata3 <- ddply(newdata, .(Neighborhood), summarize, 
                 TotalPrice = sum(SalePrice))
newdata4 <- ddply(newdata, .(Neighborhood), summarize, 
                 TotalSqft = sum(TotalFloorSF))

newdata34 <- cbind(newdata3,newdata4)
newdata34$AvgPr_Sqft <- newdata34$TotalPrice/newdata34$TotalSqft

newdataall <- newdata1
newdataall$MeanPrice <- newdata2$MeanPrice
newdataall$AvgPr_Sqft <- newdata34$AvgPr_Sqft

newdata$AvgPr_Sqft <- newdataall$AvgPr_Sqft

ggplot(newdataall, aes(x=AvgPr_Sqft, y=MAE, col=Neighborhood)) + 
  geom_point(shape=1,size=3) +
  ggtitle("Scatter Plot of MAE vs Avg Price/Sq. Ft") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(newdataall, aes(x=AvgPr_Sqft, y=MeanPrice, col = Neighborhood)) + 
  geom_point(shape=1,size=3) +
  ggtitle("Scatter Plot of Avg Price/ Sq. Ft vs ") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))


newdata$Nghbrhd_subgroups <-
  ifelse(newdata$AvgPr_Sqft<=110, "Group1", 
    ifelse(newdata$AvgPr_Sqft <= 140, "Group2",
      ifelse(newdata$AvgPr_Sqft <= 150, "Group3",
             "Group4"))) 


Model2 <- lm(SalePrice ~ TotalFloorSF+QualityIndex+Nghbrhd_subgroups, data=newdata)
anova(Model2)
summary(Model2)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(Model2)
par(mfrow=c(1,1))

pred2 <- as.data.frame(predict(Model2,newdata))
names(pred2)
library(reshape)
pred2 <- rename(pred2, c("predict(Model2, newdata)" = "prd"))
newdata$pred2 <- pred2$prd
newdata$res2 <- newdata$SalePrice - newdata$pred2
newdata$absres2 <- abs(newdata$res2)

require(stargazer)
stargazer(Model1, Model2,  type="text",report=('vcstp*'),title="Comparison of Model1 & Model2", align=TRUE)

anova(Model1, Model2)

MAE2 <- mean(newdata$absres2)
c(MAE1,MAE2)

summary(newdata)

Model3 <- lm(SalePrice ~ QualityIndex + HouseArea + LotArea + GarageArea + BsmtQual, data = newdata)
anova(Model3)
summary(Model3)
par(mfrow=c(2,2))  
plot(Model3)

pred3 <- as.data.frame(predict(Model3,newdata))
names(pred3)
library(reshape)
pred3 <- rename(pred2, c("predict(Model3, newdata)" = "prd"))
newdata$pred3 <- pred3$prd
newdata$res3 <- newdata$SalePrice - newdata$pred3
newdata$absres3 <- abs(newdata$res3)
MAE3 <- mean(newdata$absres3)

Model4 <- lm(logSalePrice ~ QualityIndex + HouseArea + LotArea + GarageArea + BsmtQual, data = newdata)
anova(Model4)
summary(Model4)
par(mfrow=c(2,2))  
plot(Model4)

pred4 <- as.data.frame(predict(Model4,newdata))
names(pred4)
library(reshape)
pred2 <- rename(pred4, c("predict(Model4, newdata)" = "prd"))
newdata$pred4 <- pred4$prd
newdata$res4 <- newdata$SalePrice - newdata$pred4
newdata$absres4 <- abs(newdata$res4)

MAE4 <- mean(newdata$absres3)

c(MAE3,MAE4)

library(car)
vif(Model4)
par(mfrow=c(1,1))
influencePlot(Model4,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

summary(inflm.MLRLog <- influence.measures(Model4))
dffitslog <- dffits(Model4)
newdata$dffitslog <- cbind(newdata,dffitslog)

newdata$absdf <- abs(newdata$dffitslog)

dfits_thr<-2*sqrt((8+1)/(nrow(newdata)-8-1))
subdatinf <- newdata[which(newdata$absdf < (dfits_thr)),]


multiple_pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

require(ggplot2)
ggplot(subdat, aes(x=OverallQual, y=res)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################################################################
############### Log Transformation #############################
simple_model1_log <- lm(logSalePrice ~ HouseArea, data = newdatafinal)
anova(simple_model1_log)
summary(simple_model1_log)
par(mfrow=c(2,2))
plot(simple_model1_log)

simple_pred_log1 <- as.data.frame(predict(simple_model1_log,newdatafinal,interval="prediction"))
str(simple_pred_log1)
head(simple_pred_log1)
newdatafinal <- cbind(newdatafinal,simple_pred_log1)
newdatafinal  <- subset(newdatafinal, select = -lwr)
newdatafinal  <- subset(newdatafinal, select = -upr)
str(newdatafinal)
head(newdatafinal)
newdatafinal <- rename(newdatafinal, c(fit="fitSLR1Log"))
newdatafinal$reslog <- newdatafinal$logSalePrice - newdatafinal$fitSLR1Log

newdatafinal$reslog

SLR1_log_MAE <- mean(abs(newdatafinal$reslog))
SLR1_log_MAE


simple_model2_log <- lm(logSalePrice ~ GarageArea, data = newdatafinal)
anova(simple_model2_log)
summary(simple_model2_log)
par(mfrow=c(2,2))
plot(simple_model2_log)

multiple_model_log <- lm(logSalePrice ~ HouseArea + GarageArea, data = newdatafinal)
anova(multiple_model_log)
summary(multiple_model_log)
par(mfrow=c(2,2))
plot(multiple_model_log)



MLRLogresult = lm(logSalePrice ~ TotalFloorSF+KitchenQual, data=newdata)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)

pred <- as.data.frame(predict(MLRLogresult,subdatnum,interval="prediction"))
str(pred)
head(pred)
subdatnum <- cbind(subdatnum,pred)
subdatnum <- subset( subdatnum, select = -lwr)
subdatnum <- subset( subdatnum, select = -upr)
str(subdatnum)
head(subdatnum)
subdatnum <- rename(subdatnum, c(fit="fitMLRLog"))
subdatnum$reslog <- subdatnum$logSalePrice - subdatnum$fitMLRLog
MAE <- mean(abs(subdatnum$reslog))
MAE

head(subdatnum)

library(car)
vif(Model4)
par(mfrow=c(1,1))
influencePlot(Model4,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.Model4 <- influence.measures(Model4))
dffitslog <- dffits(Model4)
newdataintegers <- cbind(newdataintegers, dffitslog)
str(subdatnum)

ggplot(subdatnum, aes(x=OverallQual, y=reslog)) + 
  geom_point(color="blue", size=2) +
  ggtitle("Scatter Plot of Residual vs OverallQual") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm,se=FALSE)  ## method=lm, se=FALSE ###

################ influential points removed  #######
newdata$absdf <- abs(newdata$dffitslog)
subdatnuminf <- subdatnum[which(subdatnum$absdf < 0.064),]

MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual, data=subdatnuminf)
anova(MLRLogresult)
summary(MLRLogresult)

############## analyze Neighborhood variable #########

require(ggplot2)
ggplot(newdata, aes(x=Neighborhood, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price in Neighborhoods") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

library(plyr)
subdat1 <- ddply(subdat, .(Neighborhood), summarise, 
                 MAE = mean(absres))
subdat2 <- ddply(subdat, .(Neighborhood), summarise, 
                 MeanPrice = mean(SalePrice))
subdat3 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalPrice = sum(SalePrice))
subdat4 <- ddply(subdat, .(Neighborhood), summarise, 
                 TotalSqft = sum(TotalFloorSF))
subdat34 <- cbind(subdat3,subdat4)
subdat34$AvgPr_Sqft <- subdat34$TotalPrice/subdat34$TotalSqft

subdatall <- subdat1
subdatall$MeanPrice <- subdat2$MeanPrice
subdatall$AvgPr_Sqft <- subdat34$AvgPr_Sqft

require(ggplot2)
ggplot(subdatall, aes(x=AvgPr_Sqft, y=MeanPrice)) + 
  geom_point(color="blue", shape=1,size=3) +
  ggtitle("Scatter Plot") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#### Clean up of the Neighborhood varaible  ########

newdata$Neighborhood_subgroups <-
  ifelse(newdata$price_sqft<=100, "grp1", 
         ifelse(newdata$price_sqft<=120, "grp2",
                ifelse(newdata$price_sqft<=140, "grp3",
                       "grp4"))) 


newdata$
################ include categoriacl variable in the model #######

MLRresult = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp, data=subdat)
anova(MLRresult)
summary(MLRresult)
pred <- as.data.frame(predict(MLRresult,subdat))
names(pred)
library(reshape)
pred <- rename(pred, c("predict(MLRresult, subdat)" = "prd"))
subdat$pred <- pred$prd
subdat$res <- subdat$SalePrice - subdat$pred
subdat$absres <- abs(subdat$res)
MAE <- mean(subdat$absres)
MAE

################# define dummy variables ###################

subdat$NbhdGrp1 <- 
  ifelse(subdat$NbhdGrp == "grp1", 1, 0)
subdat$NbhdGrp2 <- 
  ifelse(subdat$NbhdGrp == "grp2", 1, 0)
subdat$NbhdGrp3 <- 
    ifelse(subdat$NbhdGrp == "grp3", 1, 0)

MLRresult4 = lm(SalePrice ~ TotalFloorSF+OverallQual+NbhdGrp1+NbhdGrp2+NbhdGrp3, 
                data=subdat)
anova(MLRresult4)
summary(MLRresult4)



############################################################
############## assignment 5 #############################
#########################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- subset(newdata, select = c(SalePrice, LotArea, OverallQual, OverallCond, GarageArea, KitchenQual, Neighborhood_subgroups, HouseAge, BsmtQual, GarageCars, PavedDrive, TotalFloorSF, SaleCondition, price_sqft, GarageFinish, FullBath, BsmtFinSF1, BsmtFinSF2, GrLivArea))

my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

my.data$QualityIndex <- my.data$OverallQual*my.data$OverallCond;
my.data$TotalSqftCalc <- my.data$BsmtFinSF1+my.data$BsmtFinSF2+my.data$GrLivArea;

newdata <- mydata %>% dplyr::filter(Zoning %in% c("FV", "RH", "RL", "RM") & BldgType %in% "1Fam")                 
newdata <- newdata[which(newdata$TotalFloorSF < 4000),]
newdata <- subset(newdata, select = -c(SID, PID))
newdata <- subset(newdata, select = c(SalePrice, Neighborhood, QualityIndex, logSalePrice, HouseArea, LotArea, HouseAge, TotalFloorSF, SaleCondition, PavedDrive, GarageArea, GarageFinish, FireplaceQu, KitchenQual, TotRmsAbvGrd, OverallQual, YearBuilt, BsmtQual, TotalBsmtSF, FirstFlrSF, GrLivArea, FullBath, GarageCars, SecondFlrSF, CentralAir, QualityIndex,  OverallQual, OverallCond, BsmtFinSF1, BsmtFinSF2, price_sqft, Neighborhood_subgroups))

newdata$Neighborhood_subgroups <-
  ifelse(newdata$price_sqft<=100, "grp1", 
         ifelse(newdata$price_sqft<=120, "grp2",
                ifelse(newdata$price_sqft<=140, "grp3",
                       "grp4"))) 
require(knitr)
kable(matrix(c(my.data),ncol=3,byrow=F),format="pandoc",caption = "Predictors of Interest")

names(my.data)
summary(my.data)

set.seed(123)
# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);

dim(train.df)

dim(test.df)

names(train.df)

train.clean <- subset(train.df, select=c("SalePrice", "LotArea", "OverallQual", "OverallCond", "KitchenQual", "GarageArea", "Neighborhood_subgroups", "HouseAge", "BsmtQual", "GarageCars", "PavedDrive", "TotalFloorSF", "SaleCondition", "price_sqft", "GarageFinish", "FullBath", "QualityIndex", "TotalSqftCalc"),)

test.clean <- subset(test.df, select=c("SalePrice", "LotArea", "OverallQual", "OverallCond", "KitchenQual", "GarageArea", "Neighborhood_subgroups", "HouseAge", "BsmtQual", "GarageCars", "PavedDrive", "TotalFloorSF", "SaleCondition", "price_sqft", "GarageFinish", "FullBath", "QualityIndex", "TotalSqftCalc"))


train.clean1 <- subset(train.df, select=c("SalePrice", "LotArea", "OverallCond", "KitchenQual", "GarageArea", "Neighborhood_subgroups", "HouseAge", "BsmtQual", "GarageCars", "PavedDrive", "TotalFloorSF", "SaleCondition", "GarageFinish", "FullBath", "QualityIndex", "TotalSqftCalc"),)

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)


# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean1);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean);
summary(lower.lm)
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalFloorSF,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ OverallQual + LotArea, data=train.clean)
summary(junk.lm)

library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

library(broom)
forwardmetrics <- broom::glance(forward.lm)
forwardmodel <- broom::augment(forward.lm)
forwardmetrics$MAE <- mean(abs(forwardmodel$.resid))
forwardmetrics$MSE <- mean(forwardmodel$.resid ^2)

backwardmetrics <- broom::glance(backward.lm)
backwardmodel <- broom::augment(backward.lm)
backwardmetrics$MAE <- mean(abs(backwardmodel$.resid))
backwardmetrics$MSE <- mean(backwardmodel$.resid ^2)


stepwisemetrics <- broom::glance(stepwise.lm)
stepwisemodel <- broom::augment(stepwise.lm)
stepwisemetrics$MAE <- mean(abs(stepwisemodel$.resid))
stepwisemetrics$MSE <- mean(stepwisemodel$.resid ^2)


junkmetrics <- broom::glance(junk.lm)
junkmodel <- broom::augment(junk.lm)
junkmetrics$MAE <- mean(abs(junkmodel$.resid))
junkmetrics$MSE <- mean(junkmodel$.resid ^2)


comparison <- rbind(junkmetrics,forwardmetrics,backwardmetrics,stepwisemetrics)
comparison$Model <- c("Junk", "Forward Selection", "Backward Elimination", "Stepwise")
comparison <- comparison %>% 
  dplyr::select(Model,adj.r.squared,AIC,BIC,MSE, MAE) %>% 
  dplyr::mutate(BIC = round(BIC,2)) %>%
  dplyr::mutate(adj.r.squared = round(adj.r.squared,4))
knitr::kable(comparison,format = "pandoc",caption = "Model Fit Metrics Comparison",longtable = T)
Ranktable <- comparison %>% 
  dplyr::mutate(Rank.adjR = as.numeric(as.factor(-adj.r.squared)), Rank.AIC = as.numeric(as.factor(AIC)), Rank.BIC=as.numeric(as.factor(BIC)), Rank.MSE = as.numeric(as.factor(MSE)), Rank.MAE = as.numeric(as.factor(MAE))) %>% 
  dplyr::select(Model,Rank.adjR, Rank.AIC, Rank.BIC, Rank.MSE, Rank.MAE)
knitr::kable(Ranktable,format = "pandoc",caption = "Model Fit Metrics Ranks",longtable = T)



library(stargazer)
stargazer(forward.lm, backward.lm, stepwise.lm, type="text",report=('vcstp*'),title = "Comparision of models selected through different variable selection methods")

forward.test <- predict(forward.lm,newdata=test.clean);
forward.test

library(Metrics)



backward.test <- predict(backward.lm,newdata=test.clean);
stepwise.test <- predict(stepwise.lm,newdata=test.clean);
junk.test <- predict(junk.lm,newdata=test.clean);

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(junk.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice;
MAPE <- mean(junk.testPCT)
MAPE


Validation<-test.clean
junk.test <- predict(junk.lm,newdata = test.df)
junktest<- data.frame(test.df$SalePrice, junk.test)
junktest<- junktest %>% 
  dplyr::mutate(error = test.df.SalePrice -junk.test)
forward.test <- predict(object = forward.lm, newdata = Validation)
fwdtest<- data.frame(Validation$SalePrice, forward.test)
fwdtest<- fwdtest %>% dplyr::mutate(error = Validation.SalePrice - forward.test)
backward.test <- predict(object = backward.lm, newdata = Validation)
bwdtest<- data.frame(Validation$SalePrice, backward.test)
bwdtest<- bwdtest %>% dplyr::mutate(error = Validation.SalePrice - backward.test)
stepwise.test <- predict(object = stepwise.lm, newdata = Validation)
stptest<- data.frame(Validation$SalePrice, stepwise.test)
stptest<- fwdtest %>% dplyr::mutate(error = Validation.SalePrice - stepwise.test)
PredictCompare <- data.frame(Model = c("Junk", "Forward", "Backward", "Stepwise"), MAE = c(mean(abs(junktest$error)), mean(abs(fwdtest$error)),mean(abs(bwdtest$error)),mean(abs(stptest$error))),MSE = c(mean(junktest$error^2), mean(fwdtest$error^2),mean(bwdtest$error^2),mean(stptest$error^2)))
PredictCompare <- cbind(PredictCompare,comparison$MAE, comparison$MSE)
PredictCompare <- PredictCompare[,c(1,4,2,5,3)]
colnames(PredictCompare) <- c("Model", "MAE.In.Sample", "MAE.Out.of.Sample", "MSE.In.Sample", "MSE.Out.of.Sample")
knitr::kable(PredictCompare, format = "pandoc", caption = "Prediction Metrics Comparision")

# Assign Prediction Grades training data;
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)


# Assign Prediction Grades test data;
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)




library(purrr)
library(scales)
getgrade <- function(x) {
  ifelse(x <= 0.1, 'Grade 1: [0.0, 0.1]',
         ifelse(x <= 0.15, 'Grade 2: [0.10,0.15]',
                ifelse(x <= 0.25, 'Grade 3: [0.15, 0.25]','Grade 4: [0.25 +]')))
}
# in sample prediction grade - forward
stepwise.error.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice
stepwise.predict.grade <- getgrade(stepwise.error.pct)
stepwise.trainTable <- table(stepwise.predict.grade)
stepwise.train.grade <- round(stepwise.trainTable/sum(stepwise.trainTable),2)
# out of sample prediction grade - forward
stepwise.predictError.pct <- abs(stepwisetest$error)/stepwisetest$Validation.SalePrice
stepwise.predictValid.grade <- getgrade(stepwise.predictError.pct)
stepwise.testTable <- table(stepwise.predictValid.grade)
stepwise.test.grade <- round(fstepwise.testTable/sum(stepwise.testTable),2)
# in sample prediction grade - Junk
junk.error.pct <- abs(junk.lm$residuals)/train.df$SalePrice
junk.predict.grade <- getgrade(junk.error.pct)
junk.trainTable <- table(junk.predict.grade)
junk.train.grade <- round(junk.trainTable/sum(junk.trainTable),2)
# out of sample prediction grade - junk
junk.predictError.pct <- abs(junktest$error)/junktest$test.df.SalePrice
junk.predictValid.grade <- getgrade(junk.predictError.pct )
junk.testTable <- table(junk.predictValid.grade)
junk.test.grade <- round(junk.testTable/sum(junk.testTable),2)
OperationalValidation <- as.data.frame(cbind(forward.train.grade,forward.test.grade,junk.train.grade,junk.test.grade))
name <- row.names(OperationalValidation)
OperationalValidation <- map_df(.x = OperationalValidation, .f = percent)
OperationalValidation$Grade <- name
OperationalValidation <- OperationalValidation[,c(5,1,2,3,4)]
knitr::kable(OperationalValidation,format = "pandoc", caption = "Operational Validation")

subdat <- train.clean
Model1<-stepwise.lm

anova(Model1)
summary(Model1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(Model1)
par(mfrow=c(1,1))
library(car)
vif(Model1)
par(mfrow=c(1,1))
influencePlot(Model1,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


summary(inflm.MLRLog <- influence.measures(Model1))
 dffitslog <- dffits(Model1)
 subdat <- cbind(subdat,dffitslog)
# 
 ################ influential points removed  #######
subdat$absdf <- abs(subdat$dffitslog)
head(subdat)
dfits_thr<-2*sqrt((32+1)/(nrow(subdat)-32-1))
subdatinf <- subdat[which(subdat$absdf < (dfits_thr)),]
# 
# 
MLRresult = lm(log(SalePrice) ~ TotalFloorSF + price_sqft + BsmtQual + 
                    Neighborhood_subgroups + KitchenQual + OverallCond + QualityIndex + 
                    LotArea + PavedDrive + TotalSqftCalc + SaleCondition + FullBath, data = subdatinf)
 
# #MLRLogresult = lm((SalePrice) ~ HouseArea + NbhdGrp+TotalSqftCalc+ HouseStyle+QualityIndex, data = train.df)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)
par(mfrow=c(1,1))





######################################################################
sub <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                                         "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                                         "TotalBsmtSF","Style1","Style2"))

MLRresult1 = lm(logSalePrice ~ ., data=sub)

sub2 <- subset(subdat, select=c("TotalFloorSF","HouseAge",
                               "OverallQual","LotArea","logSalePrice","BsmtFinSF1",
                               "TotalBsmtSF"))

MLRresult2 = lm(logSalePrice ~ ., data=sub2)
anova(MLRresult1,MLRresult2)

anova(MLRresult1)
summary(MLRresult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

names(MLRresult)
head(MLRresult$df.residual)

inflm.MLRLog <- influence.measures(MLRresult)
names(inflm.MLRLog)
str(inflm.MLRLog)
inflmetrics <- as.data.frame(inflm.MLRLog$infmat)
dffit_df <- subset(inflmetrics, select= c(dffit))
sub$r1 <- row.names(sub)
dffit_df$r1 <- row.names(dffit_df)
subnew <- merge(sub, dffit_df, all=FALSE)

subnew <- subset(subnew, select= -c(r1))

subnew$absdffit <- abs(subnew$dffit)

subnewinf <- subnew[which(subnew$absdf < 0.064),]



MLRLogresult = lm(logSalePrice ~ TotalFloorSF+OverallQual+HouseAge+
            LotArea+BsmtFinSF1+TotalBsmtSF+Style1+Style2,data=subnewinf)
anova(MLRLogresult)
summary(MLRLogresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRLogresult)








