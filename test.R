setwd("~/myScripts/LabTest")
#Libraries ----
install.packages("corrplot");library(corrplot) 
install.packages("PerformanceAnalytics");library(PerformanceAnalytics)
install.packages("psych")>library(psych)
install.packages("ggplot2");library(ggplot2)
install.packages("ggfortify");library(ggfortify)
install.packages("factoextra");library(factoextra)
install.packages("dplyr");library(dplyr)
install.packages("MASS");library(MASS)
install.packages("MLmetrics");library(MLmetrics)
install.packages("Metrics");library(Metrics)
install.packages("olsrr");library(olsrr)
install.packages("nortest");library(nortest)
install.packages("car");library(car)
install.packages("lmtest");library(lmtest)
install.packages("PerformanceAnalytics");library(PerformanceAnalytics)
install.packages("lattice"); library(lattice)
install.packages("tseries");library(tseries)
#
#Importing the dataset into a dataframe ----
data <- read.csv("Airbnb_Data.csv", sep = ",", dec = ".")
data$amenities <- NULL
data$id <- NULL
data$description <- NULL
data$first_review <- NULL
data$host_since <- NULL
data$last_review <- NULL
data$name <- NULL
data$thumbnail_url <- NULL
data$neighbourhood <- NULL
data$zipcode <- NULL
data <- na.omit(data)
data <- data[data$host_has_profile_pic != "",]
data$cleaning_fee <- as.factor(data$cleaning_fee)
data$bed_type <- as.factor(data$bed_type)
data$property_type <- as.factor(data$property_type)
data$room_type <- as.factor(data$room_type)
data$cancellation_policy <- as.factor(data$cancellation_policy)
data$city <- as.factor(data$city)
data$host_has_profile_pic <- factor(data$host_has_profile_pic, levels = c("t", "f"), labels = c("TRUE", "FALSE"))
data$host_identity_verified <- factor(data$host_identity_verified, levels = c("t", "f"), labels = c("TRUE", "FALSE"))
data$instant_bookable <- factor(data$instant_bookable, levels = c("t", "f"), labels = c("TRUE", "FALSE"))
#Converting the percentages to numeric with the right proportions
data$host_response_rate <- as.numeric(gsub("%", "", data$host_response_rate))
data$host_response_rate <- data$host_response_rate / 100
#
data <- data[!is.na(data$host_response_rate),]
summary(data)
str(data)
#Creating a data frame with only the numeric variables ----
df <- data[,c(1,4,5,12,14,15,16,17,18,19)]
summary(df)
#
#Descriptive Statistics ----
##Histogram ----
hist(df$log_price, 
     main = "Distribution of Log-Transformed Prices",
     xlab = "Log-Transformed Price",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     xlim = c(min(df$log_price) - 1, max(df$log_price) + 1),
)
hist(df$accommodates, 
     main = "Distribution of Accommodations",
     xlab = "Number of Accommodations",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     breaks = seq(min(df$accommodates) - 0.5, max(df$accommodates) + 0.5, by = 1),
     xlim = c(min(df$accommodates) - 1, max(df$accommodates) + 1),
     ylim = c(0,17000)
)
hist(df$bathrooms, 
     main = "Distribution of Bathrooms",
     xlab = "Number of Bathrooms",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     xlim = c(min(df$bathrooms) - 1, max(df$bathrooms) + 1),
     ylim = c(0,10000)
)
hist(df$host_response_rate, 
     main = "Distribution of Host Response Rate",  
     xlab = "Host Response Rate (as a fraction of 1.0)",  
     ylab = "Frequency",  
     col = "#778b90", 
     border = "white",  
     breaks = 20,  
     xlim = c(0, 1),  
     ylim = c(0, 5000)  
)
hist(df$number_of_reviews, 
     main = "Distribution of Number of Reviews",  
     xlab = "Number of Reviews",  
     ylab = "Frequency",  
     col = "#778b90", 
     border = "white",  
     xlim = c(min(df$number_of_reviews) - 1, max(df$number_of_reviews)),
     ylim = c(0, 10000)
)
hist(df$review_scores_rating, 
     main = "Distribution of Review Scores Rating",
     xlab = "Review Scores Rating",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     breaks = 20,
     xlim = c(0, 100),
     ylim = c(0, 15000)
)
hist(df$bedrooms, 
     main = "Distribution of Number of Bedrooms",
     xlab = "Number of Bedrooms",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     xlim = c(0, 5),
     ylim = c(0, 20000)
)
hist(df$beds, 
     main = "Distribution of Number of Beds",
     xlab = "Number of Beds",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     xlim = c(0, 10),
     ylim = c(0, 15000)
)
#
##Boxplot ----
boxplot(df$log_price, 
        main = "Boxplot of Log Price",
        horizontal = TRUE,
        axes = T,
        col="#778090"
)
boxplot(df$accommodates, 
        main = "Boxplot of Accommodates",
        horizontal = TRUE,
        notch = F,
        col="#778090"
)
boxplot(df$bathrooms, 
        main = "Boxplot of Bathrooms",
        horizontal = TRUE,
        notch = TRUE,
        col="#778090"
)
boxplot(df$host_response_rate, 
        main = "Boxplot of Host Response Rate",
        horizontal = TRUE,
        notch = TRUE,
        col="#778090"
)
boxplot(df$number_of_reviews, 
        main = "Boxplot of Number of Reviews",
        horizontal = TRUE,
        notch = TRUE,
        col="#778090"
)
boxplot(df$review_scores_rating, 
        main = "Boxplot of Review Scores Rating",
        horizontal = TRUE,
        ylim = c(0, 100),
        col="#778090"
)
boxplot(df$bedrooms, 
        main = "Boxplot of Bedrooms",
        horizontal = TRUE,
        col="#778090"
)
boxplot(df$beds, 
        main = "Boxplot of Beds",
        horizontal = TRUE,
        col="#778090"
)
#
##Barplot of Factors ----
plot(data$bed_type, col="#778b90", main="Types of Beds")
plot(data$property_type, col="#778b90", main="Types of Properties")
plot(data$room_type, col="#778b90", main="Types of Rooms")
plot(data$cancellation_policy, col="#778b90", main="Types of Cancellation Policy")
plot(data$cleaning_fee, col="#778b90", main="Cleaning Fee")
plot(data$city, col="#778b90", main="Cities")
plot(data$host_has_profile_pic, col="#778b90", main="Host Profile Picture")
plot(data$host_identity_verified, col="#778b90", main="Host Identity Verified")
plot(data$instant_bookable, col="#778b90", main="Instant Bookable")
#
##Boxplot with Factor ----
plot(data$log_price ~ data$bed_type, 
     main = paste("Plot of Price by Bed Type"),
     xlab = "Bed Type",
     ylab = "Log of Price",
     col="#778090"
)
plot(data$accommodates ~ data$bed_type, 
     main = paste("Plot of Accommodates by Bed Type"),
     xlab = "Bed Type",
     ylab = "Accommodates",
     col="#778090"
)
plot(data$log_price ~ data$city, 
     main = paste("Plot of Price by City"),
     xlab = "City",
     ylab = "Log of Price",
     col="#778090"
)
plot(data$accommodates ~ data$city, 
     main = paste("Plot of Accommodates by City"),
     xlab = "City",
     ylab = "Accommodates",
     col="#778090"
)
plot(data$log_price ~ data$cancellation_policy, 
     main = paste("Plot of Price by Cancellation Policy"),
     xlab = "Cancellation Policy",
     ylab = "Log of Price",
     col="#778090"
)
plot(data$accommodates ~ data$cancellation_policy, 
     main = paste("Plot of Accommodates by Cancellation Policy"),
     xlab = "Cancellation Policy",
     ylab = "Accommodates",
     col="#778090"
)
plot(data$log_price ~ data$room_type, 
     main = paste("Plot of Price by Room Type"),
     xlab = "Room type",
     ylab = "Log of Price",
     col="#778090"
)
plot(data$accommodates ~ data$room_type, 
     main = paste("Plot of Accommodates by Room Type"),
     xlab = "Room Type",
     ylab = "Accommodates",
     col="#778090"
)
plot(data$log_price ~ data$property_type, 
     main = paste("Plot of Price by Property Type"),
     xlab = "Property Type",
     ylab = "Log of Price",
     col="#778090"
)
#
## Corrplot ----
mycor <- cor(df)
corrplot(mycor, type = "full", method= "shade", col = COL1('YlGn'),cl.pos = 'b', tl.col = "black", tl.srt = 45, title = "Correlation Heatmap (Numerical Columns Only)", mar = c(0,0,1,0))
mycor
chart.Correlation(df, histogram = T, pch = 19)
pairs.panels(df)
pairs(df)
#
#Principal Component Analysis (PCA) ----
#Scale the data (standardize)
scaled_data <- scale(df)
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
#Scree plot to show the variance explained by each principal component
fviz_eig(pca_result)
#Biplot of the first two principal components
fviz_pca_biplot(pca_result, 
                geom.ind = "point", # Show points only (not text)
                pointshape = 21, pointsize = 2,
                col.ind = "cos2", # Color by the quality of representation
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE # Avoid text overlapping
)
#Variable plot of the first two principal components
fviz_pca_var(pca_result,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
summary(pca_result)
princomp(x = df, cor = T, scores = T)
#
#Step Method ----
y <- df$log_price
FitAll <- lm(y~., data=df[ ,-1])
FitStart <- lm(y~1, data=df[ ,-1])
#forward
step(FitStart, direction="forward", scope=formula(FitAll))
#backward
step(FitAll, direction="backward", trace=F) 
#bidirectional
step(FitStart, direction="both", scope=formula(FitAll)) 
##
step.model <- stepAIC(FitAll, direction = "both", trace = F)
summary(step.model)
#
#Training and Testing ----
set.seed(123)
trainingRowIndex <- sample(1:nrow(df), 0.7*nrow(df))
training.set <- df[trainingRowIndex, ]  #training data
testing.set  <- df[-trainingRowIndex, ]   #test data
#
#Prediction ----
regressor <- lm(log_price ~ ., data=training.set)
predictor <- predict(regressor, newdata=testing.set)
summary(regressor)
#
#Metrics ----
actual <- testing.set$log_price
actuals_preds <- as.data.frame(cbind(actual, predictor))
MAPE(predictor,actual) #MAPE mean absolute percentage error may give Inf
MAE(predictor, actual) #MAE mean absolute error
MSE(predictor, actual) #MSE mean squared error
RMSE(predictor, actual) #RMSE root mean squared error
Accuracy(predictor, actual) #Accuracy may give 0 when actual = 0 or discrete variable 
mape_2 <- mean(abs((actual - predictor)/ predictor)) #may give Inf
mape_2
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) #may give Inf
min_max_accuracy
cor(predictor, actual) # correlation between values
# if some of actual = 0 then MAPE gives Inf in that case we use :
smape(predictor, actual) #symmetric mean absolute error
percent_bias(predictor, actual) #difference error in percentage
precision(predictor, actual) #precision in percentage
accuracy(predictor, actual) #accuracy in percentage
rae(predictor, actual) #relative absolute error
#
##Actual vs Predicted Plot ----
plot(actual, predictor, 
     main = "Actual vs Predicted Values",
     xlab = "Actual Values", 
     ylab = "Predicted Values",
     col = "#808b90", # Semi-transparent blue
     pch = 19, 
     cex = 0.5) # Smaller points
abline(0, 1, col = "red", lty = 2, lwd = 2)  # Add a reference line
lines(lowess(actual, predictor), col = "darkgreen", lwd = 2) # Add a smooth line
#
#Residuals of Regressor ----
resid.reg <- regressor$residuals
hist(resid.reg, 
     main = "Distribution of the Residuals",
     xlab = "Residuals",
     ylab = "Frequency",
     col = "#778b90",
     border = "white",
     xlim = c(-3,3),
     ylim = c(0, 10000)
)
boxplot(resid.reg, 
        main = "Boxplot of Residuals",
        horizontal = TRUE,
        notch = TRUE,
        col = "#778090"
)
autoplot(regressor)
#
#Creating the regression ----
reg <- lm(df$log_price ~ ., data = df)
resid.reg <- reg$residuals
summary(reg)
autoplot(reg) +
  theme_minimal() +  
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(hjust = 0.5, color = "black")
  ) 
plot(cooks.distance(reg), type = "o", pch = 21, main = "Cook's Distance Plot", xlab = "Observation Number", ylab = "Cook's Distance")
#
#Assumptions ----
##Multicollinearity ----
vif(reg)
#
#PCA can transform latitude and longitude into uncorrelated components
#Standardizing the latitude and longitude to ensure that PCA treats both dimensions equally
df$latitude1 <- scale(df$latitude)
df$longitude1 <- scale(df$longitude)
#Perform PCA on standardized latitude and longitude
pca <- prcomp(df[, c("latitude", "longitude")], scale. = TRUE)
#Add principal components to the data frame
df$pca1 <- pca$x[, 1]
df$pca2 <- pca$x[, 2]
#Drop the original latitude and longitude
df$longitude1 <- NULL
df$latitude1 <- NULL
df$latitude <- NULL
df$longitude <- NULL
#Check VIF to ensure multicollinearity is reduced
reg <- lm(df$log_price ~ ., data = df)
resid.reg <- reg$residuals
vif(reg)
summary(reg)
##Normality of residuals ----
res.student <- rstudent(reg)
res.stdrs <- rstandard(reg)
plot(res.student,pch=20,
     col="#159989",cex= .5,
     xlab = "Observation Number",
     ylab = "Studentized Residuals",
     main = "Studentized Residuals with Outlier Thresholds"
     )
abline(h=c(-2,0,2), lty= c(2,1,2), lwd=2, col="grey30")
plot(res.stdrs,pch=20,
     col="lightpink",cex= .5,
     xlab = "Observation Number",
     ylab = "Standardized Residuals",
     main = "Standardized Residuals with Outlier Thresholds"
)
abline(h=c(-2,0,2), lty= c(2,1,2), lwd=2, col="grey30")
#ols_test_normality(resid.reg)#i have too many elements
###Anderson-Darling normality test ----
ad.test(resid.reg)#if p-value > we have normality of the residuals
summary(resid.reg)
#
###Kolmogorov-Smirnov test ----
ks.test(resid.reg, "pnorm", mean = mean(resid.reg), sd = sd(resid.reg))
#
###Jarque-Bera Test ----
jarque.bera.test(resid.reg)
#
###Plots of Residuals ----
hist(resid.reg, 
     main = "Distribution of the Residuals",
     xlab = "Residuals",
     ylab = "Frequency",
     col = "#778b90",
     breaks = 20,
     border = "white",
     xlim = c(-3,3),
     ylim = c(0, 10000)
)
boxplot(resid.reg, 
        main = "Boxplot of Residuals",
        horizontal = TRUE,
        notch = TRUE,
        col = "#778090"
)

##Homoskedasticity ----
bptest(regressor)#if pvalue < 0.05 we accept the homoskedasticity
#
##Autocorrelation ----
durbinWatsonTest(regressor)# if pvalue> 0.05 there is no autocorrelation
#
#
#Anova ----
##Using Aov ----
###Room Type and Price ----
myaov_room_type <- aov(log_price ~ room_type, data = data)
summary(myaov_room_type)
ad.test(myaov_room_type$residuals)
bptest(myaov_room_type)
durbinWatsonTest(myaov_room_type)
leveneTest(myaov_room_type)#homogeneity
oneway.test(log_price ~ room_type, data = data, var.equal = FALSE)
# check if the residuals follow the normal distribution and homogeneity
res.aov_room_type <- rstudent(myaov_room_type)
mypanel_room_type <- function(...) {
  panel.xyplot(...)
  panel.abline(h=c(-2,0,2), lty=c(3,2,3),...)
  }
trellis.par.set(list(fontsize=list(point=5, text=8)))
xyplot(res.aov_room_type ~ I(1:71)|room_type,  
       data=data, pch=20,
       ylim=c(-6,6), panel=mypanel_room_type,
       ylab="Residuals",xlab="")
#Boxplot of Residuals
boxplot(data$log_price ~ data$room_type, ylab = "log_price", xlab = "room_type", main="Boxplot of the variable", col = "#778090") 
boxplot(myaov_room_type$residuals ~ data$room_type, ylab = "Residuals", xlab = "room_type", main="Boxplot of the residuals", col = "#778090")
###
####Tukey Room Type and Price ----
mytukey_room_type <- TukeyHSD(myaov_room_type)
plot(mytukey_room_type)
mytukey_room_type # to find the strongest level of the factor var
####
###Bed Type and Price ----
myaov_bed_type <- aov(log_price ~ bed_type, data = data)
summary(myaov_bed_type)
ad.test(myaov_bed_type$residuals)
bptest(myaov_bed_type)
durbinWatsonTest(myaov_bed_type)
leveneTest(myaov_bed_type)#homogeneity
oneway.test(log_price ~ bed_type, data = data, var.equal = FALSE)
# check if the residuals follow the normal distribution and homogeneity
res.aov_bed_type <- rstudent(myaov_bed_type)
mypanel_bed_type <- function(...) {
  panel.xyplot(...)
  panel.abline(h=c(-2,0,2), lty=c(3,2,3),...)
  }
trellis.par.set(list(fontsize=list(point=5, text=8)))
xyplot(res.aov_bed_type ~ I(1:71)|bed_type,  
       data=data, pch=20,
       ylim=c(-6,6), panel=mypanel_bed_type,
       ylab="Residuals",xlab="")
#Boxplot of Residuals
boxplot(data$log_price ~ data$bed_type, main="Boxplot of the variable", xlab = "bed_type", ylab = "log_price", col = "#778090") 
boxplot(myaov_bed_type$residuals ~ data$bed_type, main="Boxplot of the residuals", xlab = "bed_type", ylab = "Residuals", col = "#778090")
###
####Tukey Bed Type and Price ----
mytukey_bed_type <- TukeyHSD(myaov_bed_type)
plot(mytukey_bed_type)
mytukey_bed_type # to find the strongest level of the factor var
####
###Cancellation Policy and Price ----
myaov_cancellation_policy <- aov(log_price ~ cancellation_policy, data = data)
summary(myaov_cancellation_policy)
ad.test(myaov_cancellation_policy$residuals)
bptest(myaov_cancellation_policy)
durbinWatsonTest(myaov_cancellation_policy)
leveneTest(myaov_cancellation_policy)#homogeneity
oneway.test(log_price ~ cancellation_policy, data = data, var.equal = FALSE)
# check if the residuals follow the normal distribution and homogeneity
res.aov_cancellation_policy <- rstudent(myaov_cancellation_policy)
mypanel_cancellation_policy <- function(...) {
  panel.xyplot(...)
  panel.abline(h=c(-2,0,2), lty=c(3,2,3),...)
  }
trellis.par.set(list(fontsize=list(point=5, text=8)))
xyplot(res.aov_cancellation_policy ~ I(1:71)|cancellation_policy,  
       data=data, pch=20,
       ylim=c(-6,6), panel=mypanel_cancellation_policy,
       ylab="Residuals",xlab="")
#Boxplot of Residuals
boxplot(data$log_price ~ data$cancellation_policy, main="Boxplot of the variable", xlab = "cancellation_policy", ylab = "log_price", col = "#778090") 
boxplot(myaov_cancellation_policy$residuals ~ data$cancellation_policy, main="Boxplot of the residuals", xlab = "cancellation_policy", ylab = "Residuals", col = "#778090")
###
####Tukey Cancellation Policy and Price ----
mytukey_cancellation_policy <- TukeyHSD(myaov_cancellation_policy)
plot(mytukey_cancellation_policy)
mytukey_cancellation_policy # to find the strongest level of the factor var
####
###City and Price ----
myaov_city <- aov(log_price ~ city, data = data)
summary(myaov_city)
ad.test(myaov_city$residuals)
bptest(myaov_city)
durbinWatsonTest(myaov_city)
leveneTest(myaov_city)#homogeneity
oneway.test(log_price ~ city, data = data, var.equal = FALSE)
# check if the residuals follow the normal distribution and homogeneity
res.aov_city <- rstudent(myaov_city)
mypanel_city <- function(...) {
  panel.xyplot(...)
  panel.abline(h=c(-2,0,2), lty=c(3,2,3),...)
  }
trellis.par.set(list(fontsize=list(point=5, text=8)))
xyplot(res.aov_city ~ I(1:71)|city,  
       data=data, pch=20,
       ylim=c(-6,6), panel=mypanel_city,
       ylab="Residuals",xlab="")
#Boxplot of Residuals
boxplot(data$log_price ~ data$city, main="Boxplot of the variable") 
boxplot(myaov_city$residuals ~ data$city, main="Boxplot of the residuals")
###
####Tukey City and Price ----
mytukey_city <- TukeyHSD(myaov_city)
plot(mytukey_city)
mytukey_city # to find the strongest level of the factor var
####


########TEST########

install.packages("ggplot2");library(ggplot2)
# Create the scatter plot
ggplot(data, aes(x = longitude, y = latitude, color = log_price)) +
  geom_point() +
  labs(title = "Geospatial Visualization", x = "Longitude", y = "Latitude") +
  theme_gray()
##
# Install and load necessary packages
install.packages("ggplot2")
install.packages("maps")
install.packages("ggspatial")
library(ggplot2)
library(maps)
library(ggspatial)
# Create the map
ggplot() +
  borders("state", colour = "grey85", fill = "grey") +
  geom_point(data = data, aes(x = longitude, y = latitude, color = log_price), size = 4) +
  labs(title = "Geospatial Data Points", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red") +
  coord_fixed(1.3) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)

