#LIBRARES
library(fastDummies)
library(pracma)
library(correlation)
library(ggplot2)
library(MASS)
library(scales)
library(reshape2)
library(e1071)
library(car)
library(lmtest)
library(REAT)

#_______________________________________________________________________________
#DATA
amsterdam = read.csv2("Desktop/amsterdam_weekdays.csv",
                      sep = ',', header = TRUE) #Ldasdoading data

amsterdam <- amsterdam[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,19,20)] #Picking informative columns
head(amsterdam) #Looking at the data
 
attach(amsterdam)

amsterdam = as.data.frame(amsterdam) #Making DataFrame from our data
str(amsterdam) #Looking at types of our columns variables

#_______________________________________________________________________________
#DATA TYPE MANIPULATION
amsterdam$realSum <- as.numeric(amsterdam$realSum)
amsterdam$room_shared <- as.logical(amsterdam$room_shared)
amsterdam$room_private <- as.logical(amsterdam$room_private)
amsterdam$person_capacity <- as.numeric(amsterdam$person_capacity)
amsterdam$host_is_superhost <- as.logical(amsterdam$host_is_superhost)
amsterdam$multi <- as.logical(amsterdam$multi)
amsterdam$biz <- as.logical(amsterdam$biz)
amsterdam$cleanliness_rating <- as.numeric(amsterdam$cleanliness_rating)
amsterdam$guest_satisfaction_overall <- as.numeric(amsterdam$guest_satisfaction_overall)
amsterdam$bedrooms <- as.numeric(amsterdam$bedrooms)
amsterdam$dist <- as.numeric(amsterdam$dist)
amsterdam$metro_dist <- as.numeric(amsterdam$metro_dist)
amsterdam$lng <- as.numeric(amsterdam$lng)
amsterdam$lat <- as.numeric(amsterdam$lat)

str(amsterdam) #Checking if all the manipulations goes as planed

#Preparing categorical data to perform dummy variables coding
#Transforming all the character values in Room Type column to specific numbers
amsterdam$room_type <- replace(amsterdam$room_type, 
                               amsterdam$room_type == "Private room", 1)
amsterdam$room_type <- replace(amsterdam$room_type, 
                               amsterdam$room_type == "Entire home/apt", 2)
amsterdam$room_type <- replace(amsterdam$room_type, 
                               amsterdam$room_type == "Shared room", 3)
#str(amsterdam$room_type) <- We get character data so next line switch it to numerical
amsterdam$room_type <- as.numeric(amsterdam$room_type) 

#For columns: Cleanliness rating
#             Quest satisfaction
#             Room type (for some reason function for dummy encoding doesn't liked
#                        column Room Type being character at first place, that's why
#                        earlier transformation was performed)
#             Person capacity
#             Bedrooms
#All the numerical levels we transfered to characterical variables
amsterdam$cleanliness_rating <- cut(amsterdam$cleanliness_rating, 4, 
                                    labels = c('Poor', 'Ok', 
                                               'Good', "Great"))
amsterdam$guest_satisfaction_overall <- cut(amsterdam$guest_satisfaction_overall, 4,
                                            labels = c('Bad', 'Ok', 
                                                       'Good', "Great"))
amsterdam$room_type <- cut(amsterdam$room_type, 3,
                           labels = c("Private room", "Entire home/apt", 
                                      "Shared room"))
amsterdam$person_capacity <- cut(amsterdam$person_capacity, 5,
                           labels = c("comp_2","comp_3","comp_4",
                                      "comp_5","comp_6"))
amsterdam$bedrooms <- cut(amsterdam$bedrooms, 6,
                           labels = c("no_bedrooms", "1_bedrooms", 
                                      "2_bedrooms", "3_bedrooms", 
                                      "4_bedrooms", "5_bedrooms"))

#Data set gives us additional to columns with coordinates of each AirBnB so we 
#decided to use them by perform mathematical equation to get distance of each 
#AirBnB to the most popular Amsterdam beach

#Beach coordinates
amsterdam_sea_lat <- 52.442334
amsterdam_sea_lng <- 4.558035

#Performing the equation and seving the distances in new column named: Disntace_sea
amsterdam$distance_sea <- acos(sin(deg2rad(amsterdam_sea_lat))*sin(deg2rad(amsterdam$lat)) + 
                             cos(deg2rad(amsterdam_sea_lat))*cos(deg2rad(amsterdam$lat))*cos(deg2rad(amsterdam$lng - amsterdam_sea_lng)))*6371

#_______________________________________________________________________________
#OUTLIERS
ggplot(amsterdam, aes(y = realSum)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(color = "black", fill = "darkgreen", alpha = 0.7, outlier.color = "black") +
  labs(title = "RealSum BoxPlot", y = "realSum") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

#We can see that most values are in range from 0 to 2000, that's why we will remove
#outliers by picking only rows where 'RealSum' values is equal or lower than Upper level

quartiles <- quantile(amsterdam$realSum, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(amsterdam$realSum)
Upper <- quartiles[2] + 1.5*IQR

amsterdam <- amsterdam[amsterdam$realSum <= 1165.491, ]

ggplot(amsterdam, aes(y = realSum)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(color = "black", fill = "darkgreen", alpha = 0.7, outlier.color = "black") +
  labs(title = "RealSum BoxPlot", y = "realSum") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

#_______________________________________________________________________________
#DATA VALIDATION
#Final look at our data to make sure that everything is ready for the transformation
str(amsterdam)
summary(amsterdam) #Looking at data

#_______________________________________________________________________________
#DUMMY VARIABLES
#One way of dealing with categorical data in linear regression is to perform dummy
#encoding on categorical data.

#"It's a process of converting categorical variables into a set of binary variables 
#(also known as dummy variables), which can be used in mathematical models."

#Performing dummy encoding on earlier prepared columns and saving the outcome to new variable
amsterdam_dv <- dummy_cols(amsterdam, select_columns = c("cleanliness_rating", 
                                                         "guest_satisfaction_overall", 
                                                         "room_type", 
                                                         "bedrooms", 
                                                         "person_capacity"))
str(amsterdam_dv) #Checking the outcome

amsterdam_dv <- amsterdam_dv[,-c(2,5,9,10,11,14,15)] #Removing columns on which 
                                                     #encoding was performed
#_______________________________________________________________________________
#TRAIN / TEST DATA

sample <- sample(1:nrow(amsterdam_dv), 0.7 * nrow(amsterdam_dv), replace = FALSE)
amsterdam_dv <- amsterdam_dv[sample, ]
amsterdam_dv_test <- amsterdam_dv[-sample, ]

attach(amsterdam_dv)

#_______________________________________________________________________________
#CORRELATION MATRIX
#Computing aperiantly pleasing correlation matrix on out new data set
co=melt(cor(amsterdam_dv,method="s"))
ggplot(co, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + 
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Correlation Plot") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Corr. Coef.") + coord_flip()

#_______________________________________________________________________________
#TRANSFORMATIONS AND MODEL COMPUTING
par(mfrow=c(3,1))
plot(realSum ~ dist, data = amsterdam_dv, main ='realSum ~ dist')
plot(realSum ~ metro_dist, data = amsterdam_dv, main ='realSum ~ metro_dist')
plot(realSum ~ distance_sea, data = amsterdam_dv, main ='realSum ~ distance_sea')

#Transformations
cor.test(amsterdam_dv$realSum, amsterdam_dv$dist)
z1 <- 1/(amsterdam_dv$dist^(0.1))
plot(realSum ~ z1, data = amsterdam_dv)
cor.test(amsterdam_dv$realSum, z1)

cor.test(amsterdam_dv$realSum, amsterdam_dv$metro_dist)
z2 <- 1/(amsterdam_dv$metro_dist^(0.1))
plot(realSum ~ z2, data = amsterdam_dv)
cor.test(amsterdam_dv$realSum, z2)

#MODELS
#Model after step by step selection
model1 <- lm(realSum ~ . -cleanliness_rating_Great-
               guest_satisfaction_overall_Great-
               `room_type_Private room`-
               `room_type_Entire home/apt`-
               `room_type_Shared room`-
               bedrooms_5_bedrooms-
               bedrooms_4_bedrooms-
               person_capacity_comp_6-
               cleanliness_rating_Good-
               guest_satisfaction_overall_Ok-
               guest_satisfaction_overall_Bad-
               person_capacity_comp_5-
               cleanliness_rating_Poor-
               guest_satisfaction_overall_Good-
               host_is_superhost-
               bedrooms_3_bedrooms-
               cleanliness_rating_Ok
             , data = amsterdam_dv)
summary(model1)

#Own model selection
summary(lm(realSum ~ room_shared + room_private + 
               metro_dist + distance_sea + cleanliness_rating_Good +
               cleanliness_rating_Great + guest_satisfaction_overall_Good +
               guest_satisfaction_overall_Great + `room_type_Private room` +
               `room_type_Entire home/apt` + bedrooms_1_bedrooms +
               bedrooms_2_bedrooms + bedrooms_3_bedrooms + person_capacity_comp_2 +
               person_capacity_comp_3 + person_capacity_comp_4, 
           data = amsterdam_dv))
model2 <- lm(realSum ~ room_shared + room_private + dist + 
             metro_dist + distance_sea + cleanliness_rating_Good +
             cleanliness_rating_Great + guest_satisfaction_overall_Great + bedrooms_1_bedrooms +
             bedrooms_2_bedrooms + bedrooms_3_bedrooms + person_capacity_comp_2 +
             person_capacity_comp_3 + person_capacity_comp_4, 
             data = amsterdam_dv)
summary(model2)

#Step function model selection
step(lm(realSum ~ ., data = amsterdam_dv))
model3 <- lm(formula = realSum ~ room_shared + room_private + multi + biz + 
               dist + metro_dist + distance_sea + cleanliness_rating_Ok + 
               bedrooms_no_bedrooms + bedrooms_1_bedrooms + bedrooms_2_bedrooms + 
               person_capacity_comp_2 + person_capacity_comp_3 + person_capacity_comp_4, 
             data = amsterdam_dv)
summary(model3)

#_______________________________________________________________________________
#CORRELATION MATRIX - data used for modeling
#MODEL1
co=melt(cor(model1$model, method="s"))
ggplot(co, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + 
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Correlation Plot - Model 1") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Corr. Coef.")

#MODEL2
co=melt(cor(model2$model, method="s"))
ggplot(co, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + 
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Correlation Plot - Model 2") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Corr. Coef.")

#MODEL3
co=melt(cor(model3$model, method="s"))
ggplot(co, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = co$value, label = round(co$value, 2))) + 
  scale_fill_gradient2(low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Correlation Plot - Model 3") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="Corr. Coef.")

#_______________________________________________________________________________
#PROPERTIES OF RANDOM COMPONENTS
res1 <- model1$residuals
res2 <- model2$residuals
res3 <- model3$residuals

#Mean, skewness and kurtosis
#Model1
mean(res1)
skewness(res1, type = 2)
kurtosis(res1, type = 2)

#Model2
mean(res2)
skewness(res2, type = 2)
kurtosis(res2, type = 2)

#Model3
mean(res3)
skewness(res3, type = 2)
kurtosis(res3, type = 2)

#NORMAL DISTRIBUTION
#David-Hellwig Test
DH <- function(model){
  r <- model$residuals
  r <- sort(r, decreasing = FALSE)
  n <- length(r)
  
  se <- sd(r) 
  
  Fe <- pnorm(r, mean = 0, sd = se)
  
  columns= c("Fe","cell","start","end","assig")
  df = data.frame(matrix(nrow = n, ncol = length(columns))) 
  colnames(df) = columns
  
  df$Fe <- Fe
  df$cell <- c(1:n)
  df$start[1] <- 0
  df$end[1] <- df$start[1] + 1/n
  
  for (i in 2:length(r)){
    df$start[i] <- df$end[i-1]
    df$end[i] <- df$start[i] + 1/n
  }
  
  for (x in 1:length(r)){
    for (y in 1:length(r)){
      if (df$Fe[x] >= df$start[y] & df$Fe[x] <= df$end[y])
      {df$assig[x] <- df$cell[y]}
    }
  }
  #print(df)
  
  cele <- as.data.frame(table(factor(df$assig, levels = 1:n)))
  colnames(cele) <- c('Cela','Liczebność')
  
  #print(cele)
  #print(cele[cele$Liczebność == 0,])
  
  print(paste('Number of empty cells:', nrow(cele[cele$Liczebność == 0,])))
}

DH(model1)
DH(model2)
DH(model3)

#Shapiro Test
shapiro.test(res1) #W = 0.90186, p-value = 2.2e-16 <- so our res1 doesn't have normal distribution
shapiro.test(res2) #W = 0.94910, p-value = 3.66e-15 <- so our res2 doesn't have normal distribution
shapiro.test(res3) #W = 0.95047, p-value = 6.192e-15 <- so our res3 doesn't have normal distribution

#SYMMETRY
#Symmetry test
TS <- function(model){
  r <- model$residuals
  n = length(r)
  m = sum(r > 0)
  
  t = (m/n - 0.5)/
    sqrt(((m/n) * (1 - (m/n)))/(n-1))
  
  print(paste('Statistical value:', t))
  print(paste('Critical value (a=0.05):', qt(0.975,n-1)))
}

TS(model1)
TS(model2)
TS(model3)

#RANDOMNESS
#Series test
LS <- function(model){
  r <- model$residuals
  x <- c()
  for (i in 1:length(r)){
    if (r[i] > 0){x[i] <- '+'}
    else {x[i] <- '-'}
  }
  print(paste('Number of series:',length(rle(x)$lengths)))
  print(paste('MAX length of serie:',max(rle(x)$lengths)))
}

LS(model1)
LS(model2)
LS(model3)

#FIRST ORDER AUTOCORRELATION
#Durbin-Watson test
#Model1
durbinWatsonTest(model1)
dwtest(model1, alternative = 'greater')

#Model2
durbinWatsonTest(model2)
dwtest(model2, alternative = 'greater')

#Model3
durbinWatsonTest(model3)
dwtest(model3, alternative = 'greater')

#HOMOSCEDASTICITY
#Breusch-Pagan test for homoscedasticity
#! bptest(model) we have more categorical and logical variables
#Model1
plot(res1)

gqtest(model1)
hmctest(model1)

#Model2
plot(res2)

gqtest(model2)
hmctest(model2)

#Model1
plot(res3)

gqtest(model3)
hmctest(model3)

#DIAGNOSTIC CHARTS
plot(model1)
plot(model2)
plot(model3)

#LINEARITY
resettest(model1)
resettest(model2)
resettest(model3)

#STRANGE OBSERVATIONS
#Model1
leverage <- hat(model.matrix(model1))
plot(leverage)
index1 <- as.numeric(rownames(amsterdam_dv[leverage > 2*nrow(summary(model1)$coeff)
                                 /nrow(amsterdam_dv),]))

#Model2
leverage <- hat(model.matrix(model2))
plot(leverage)
index2 <- as.numeric(rownames(amsterdam_dv[leverage > 2*nrow(summary(model2)$coeff)
                                 /nrow(amsterdam_dv),]))

#Model3
leverage <- hat(model.matrix(model3))
plot(leverage)
index3 <- as.numeric(rownames(amsterdam_dv[leverage > 2*nrow(summary(model3)$coeff)
                                 /nrow(amsterdam_dv),]))

unique(index1,index2,index3)

#Cook's distance
plot(cooks.distance(model1))
plot(cooks.distance(model2))
plot(cooks.distance(model3))

#VIF
vif(model1)
vif(model2)
vif(model3)

#_______________________________________________________________________________
#EX POST
#Predictions
yp1 <- predict(model1, newdata = amsterdam_dv_test)
yp2 <- predict(model2, newdata = amsterdam_dv_test)
yp3 <- predict(model3, newdata = amsterdam_dv_test)

#Metrics for model1
E1 <- amsterdam_dv_test$realSum - yp1
AE1 <- abs(amsterdam_dv_test$realSum - yp1)
SE1 <- (amsterdam_dv_test$realSum - yp1)^2
R_E1 <- ((amsterdam_dv_test$realSum - yp1)/amsterdam_dv_test$realSum) * 100
R_AE1 <- (abs(amsterdam_dv_test$realSum - yp1)/amsterdam_dv_test$realSum) * 100
R_SE1 <- ((amsterdam_dv_test$realSum - yp1)^2/amsterdam_dv_test$realSum) * 100

M_E1 <- mean(E1)
M_AE1 <- mean(AE1)
M_SE1 <- mean(SE1)
M_R_E1 <- mean(R_E1)
M_R_AE1 <- mean(R_AE1)

RMSE_1 <- sqrt(M_SE1) 
VRMSE_1 <- RMSE_1/mean(amsterdam_dv_test$realSum)

I2_1 <- sum((amsterdam_dv_test$realSum - yp1)^2) / sum(amsterdam_dv_test$realSum^2)
I21_1 <- (mean(amsterdam_dv_test$realSum) - mean(yp1))^2 / mean(amsterdam_dv_test$realSum^2)
st <- sqrt(mean((amsterdam_dv_test$realSum - mean(amsterdam_dv_test$realSum))^2))
st_ <- sqrt(mean((yp1 - mean(yp1))^2))
I22_1 <- (st - st_)^2 / mean(amsterdam_dv_test$realSum^2)
rt <- cor(amsterdam_dv_test$realSum, yp1)
I23_1 <- (2*st*st_*(1-rt)) / mean(amsterdam_dv_test$realSum^2)

I21_1 + I22_1 + I23_1
I2_1

J2_1 <- mean((amsterdam_dv_test$realSum - yp1)^2) / 
  mean((amsterdam_dv$realSum - model1$fitted.values)^2)

#Metrics for model2
E2 <- amsterdam_dv_test$realSum - yp2
AE2 <- abs(amsterdam_dv_test$realSum - yp2)
SE2 <- (amsterdam_dv_test$realSum - yp2)^2
R_E2 <- ((amsterdam_dv_test$realSum - yp2)/amsterdam_dv_test$realSum) * 100
R_AE2 <- (abs(amsterdam_dv_test$realSum - yp2)/amsterdam_dv_test$realSum) * 100
R_SE2 <- ((amsterdam_dv_test$realSum - yp2)^2/amsterdam_dv_test$realSum) * 100

M_E2 <- mean(E2)
M_AE2 <- mean(AE2)
M_SE2 <- mean(SE2)
M_R_E2 <- mean(R_E2)
M_R_AE2 <- mean(R_AE2)

RMSE_2 <- sqrt(M_SE2) 
VRMSE_2 <- RMSE_2/mean(amsterdam_dv_test$realSum)

I2_2 <- sum((amsterdam_dv_test$realSum - yp2)^2) / sum(amsterdam_dv_test$realSum^2)
I21_2 <- (mean(amsterdam_dv_test$realSum) - mean(yp2))^2 / mean(amsterdam_dv_test$realSum^2)
st <- sqrt(mean((amsterdam_dv_test$realSum - mean(amsterdam_dv_test$realSum))^2))
st_ <- sqrt(mean((yp2 - mean(yp2))^2))
I22_2 <- (st - st_)^2 / mean(amsterdam_dv_test$realSum^2)
rt <- cor(amsterdam_dv_test$realSum, yp2)
I23_2 <- (2*st*st_*(1-rt)) / mean(amsterdam_dv_test$realSum^2)

I21_2 + I22_2 + I23_2
I2_2

J2_2 <- mean((amsterdam_dv_test$realSum - yp2)^2) / 
  mean((amsterdam_dv$realSum - model2$fitted.values)^2)

#Metrics for model3
E3 <- amsterdam_dv_test$realSum - yp3
AE3 <- abs(amsterdam_dv_test$realSum - yp3)
SE3 <- (amsterdam_dv_test$realSum - yp3)^2
R_E3 <- ((amsterdam_dv_test$realSum - yp3)/amsterdam_dv_test$realSum) * 100
R_AE3 <- (abs(amsterdam_dv_test$realSum - yp3)/amsterdam_dv_test$realSum) * 100
R_SE3 <- ((amsterdam_dv_test$realSum - yp3)^2/amsterdam_dv_test$realSum) * 100

M_E3 <- mean(E3)
M_AE3 <- mean(AE3)
M_SE3 <- mean(SE3)
M_R_E3 <- mean(R_E3)
M_R_AE3 <- mean(R_AE3)

RMSE_3 <- sqrt(M_SE3) 
VRMSE_3 <- RMSE_3/mean(amsterdam_dv_test$realSum)

I2_3 <- sum((amsterdam_dv_test$realSum - yp3)^2) / sum(amsterdam_dv_test$realSum^2)
I21_3 <- (mean(amsterdam_dv_test$realSum) - mean(yp3))^2 / mean(amsterdam_dv_test$realSum^2)
st <- sqrt(mean((amsterdam_dv_test$realSum - mean(amsterdam_dv_test$realSum))^2))
st_ <- sqrt(mean((yp3 - mean(yp3))^2))
I22_3 <- (st - st_)^2 / mean(amsterdam_dv_test$realSum^2)
rt <- cor(amsterdam_dv_test$realSum, yp3)
I23_3 <- (2*st*st_*(1-rt)) / mean(amsterdam_dv_test$realSum^2)

I21_3 + I22_3 + I23_3
I2_3

J2_3 <- mean((amsterdam_dv_test$realSum - yp3)^2) / 
  mean((amsterdam_dv$realSum - model3$fitted.values)^2)

#Table of Metrics
t1 <- rbind(I2_1, I21_1, I22_1, I23_1, J2_1, M_E1, M_AE1, M_SE1, M_R_E1, M_R_AE1, RMSE_1, VRMSE_1)
rownames(t1) <- c("I2", "I21", "I22", "I23", "J2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "V_RMSE")

t2 <- rbind(I2_2, I21_2, I22_2, I23_2, J2_2, M_E2, M_AE2, M_SE2, M_R_E2, M_R_AE2, RMSE_2, VRMSE_2)
rownames(t2) <- c("I2", "I21", "I22", "I23", "J2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "V_RMSE")

t3 <- rbind(I2_3, I21_2, I22_3, I23_3, J2_3, M_E3, M_AE3, M_SE3, M_R_E3, M_R_AE3, RMSE_3, VRMSE_3)
rownames(t3) <- c("I2", "I21", "I22", "I23", "J2", "ME", "MAE", "MSE", "MPE", "MAPE", "RMSE", "V_RMSE")

table <- cbind(t1, t2, t3)
colnames(table) <- c("Model1", "Model2", "Model3")
round(table, digits = 5)

#EX ANTE
X <- model.matrix(model1)
xt <- model.matrix(model1)[1:10,1:10]
Vt <- sd(model1$residuals)*sqrt((xt^10)%*% ginv(t(X)%*%X) %*% xt + 1)
