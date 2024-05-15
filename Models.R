# Set working directory
setwd("~/Documents/NTU STUDY MATERIALS/Year 2 Sem 2/BC2407/Project")

# Read the dataset
df <- read.csv("Final_Data_Clean.csv", stringsAsFactors = TRUE)

#For data cleaning, refer to the python codes

# Data Visualisations
# Heatmap of accidents
library(ggplot2)
library(dplyr)
df$LATITUDE <- round(df$LATITUDE, 2)
df$LONGITUDE <- round(df$LONGITUDE, 2)
# Aggregate data to count number of accidents at each location
accident_counts_1 <- df %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarise(accident_count = n())

ggplot(accident_counts, aes(x = LONGITUDE, y = LATITUDE, fill = accident_count)) +
  geom_tile() +
  scale_fill_gradient(name = "Number of Accidents", low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Accidents by Location", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Weather Condition
avg_injuries_1 <- aggregate(INJURIES_TOTAL ~ WEATHER_CONDITION, data = df, FUN = mean)

ggplot(avg_injuries, aes(x = WEATHER_CONDITION, y = INJURIES_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Number of Injuries by Weather Condition",
       x = "Weather Condition", y = "Average Number of Injuries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lighting condition
avg_injuries_2 <- aggregate(INJURIES_TOTAL ~ LIGHTING_CONDITION, data = df, FUN = mean)

ggplot(avg_injuries, aes(x = LIGHTING_CONDITION, y = INJURIES_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Number of Injuries by Lighting Condition",
       x = "Lighting Condition", y = "Average Number of Injuries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Trafficway type
accident_counts_2 <- df %>%
  group_by(TRAFFICWAY_TYPE) %>%
  summarise(accident_count = n()) 

ggplot(accident_counts, aes(x = TRAFFICWAY_TYPE, y = accident_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Accidents by Trafficway Type",
       x = "Trafficway Type", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Models
# Load required libraries
library(data.table)
library(caTools)
library(car)

# Split data into training and testing sets

train_index <- sample.split(df$INJURIES_TOTAL, SplitRatio = 0.7)
trainset <- df[train_index, ]
testset <- df[!train_index, ]


set.seed(123)
# Fit the linear model
model <- lm(INJURIES_TOTAL ~POSTED_SPEED_LIMIT+WEATHER_CONDITION+
              LIGHTING_CONDITION+LANE_CNT+ALIGNMENT+ROADWAY_SURFACE_COND+
              ROAD_DEFECT+CRASH_TYPE+INTERSECTION_RELATED_I+NOT_RIGHT_OF_WAY_I+HIT_AND_RUN_I+DAMAGE+
              BEAT_OF_OCCURRENCE+NUM_UNITS, data = trainset)
summary(model)

par(mfrow=c(2,2)) 



for (col in names(trainset)) {
  if (is.factor(trainset[[col]])) {
    testset[[col]] <- factor(testset[[col]], levels = levels(trainset[[col]]))
  }
}
for (col in names(testset)) {
  if (is.factor(testset[[col]])) {
    model$xlevels[[col]] <- levels(testset[[col]])
  }
}

# Check for multicollinearity
vif_results <- vif(model1)
print(vif_results)

# Convert categorical variables to factors in both train and test sets
factor_cols <- c("CRASH_DATE", "TRAFFIC_CONTROL_DEVICE", "DEVICE_CONDITION", 
                 "FIRST_CRASH_TYPE", "TRAFFICWAY_TYPE", "STREET_DIRECTION", "LOCATION")

for (col in factor_cols) {
  trainset[[col]] <- factor(trainset[[col]])
  testset[[col]] <- factor(testset[[col]], levels = levels(trainset[[col]]))
}

# Make predictions
testset$predictions <- predict(model, newdata = testset)

# Compute RMSE
rmse <- sqrt(mean((testset$INJURIES_TOTAL - testset$predictions)^2))
print(paste("RMSE:", rmse))

#earth
library(earth)
m.mars1 <- earth(INJURIES_TOTAL ~POSTED_SPEED_LIMIT+WEATHER_CONDITION+
                   LIGHTING_CONDITION+LANE_CNT+ALIGNMENT+ROADWAY_SURFACE_COND+
                   ROAD_DEFECT+CRASH_TYPE+INTERSECTION_RELATED_I+NOT_RIGHT_OF_WAY_I+HIT_AND_RUN_I+DAMAGE+
                   BEAT_OF_OCCURRENCE+NUM_UNITS, degree=4, data=df)
summary(m.mars1)

plot(m.mars1)
m.mars1.yhat <- predict(m.mars1)

RMSE.mars1 <- sqrt(mean((df$INJURIES_TOTAL - m.mars1.yhat)^2))
RMSE.mars1

importance <- evimp(m.mars1)
print(importance)


#CART
library(rpart)
library(rpart.plot)
#Growing the tree
cart_model <- rpart(INJURIES_TOTAL ~POSTED_SPEED_LIMIT+WEATHER_CONDITION+
                      LIGHTING_CONDITION+LANE_CNT+ALIGNMENT+ROADWAY_SURFACE_COND+
                      ROAD_DEFECT+CRASH_TYPE+INTERSECTION_RELATED_I+NOT_RIGHT_OF_WAY_I+HIT_AND_RUN_I+DAMAGE+
                      BEAT_OF_OCCURRENCE+NUM_UNITS, method = "anova",
                    control = rpart.control(minsplit = 2, cp = 0),data = trainset)
rpart.plot(cart_model)
plotcp(cart_model)

#Pruning the tree
printcp(cart_model)
m.opt <- prune(cart_model, cp= 0.032)
rpart.plot(m.opt)
plotcp(m.opt)
printcp(m.opt)
m.opt$variable.importance

#Calculate RMSE
predictions <- predict(cart_model, newdata = testset)
mean_squared_error <- mean((testset$INJURIES_TOTAL - predictions)^2)
rmse <- sqrt(mean_squared_error)
rmse

#Random Forest
library(randomForest)
set.seed(1)  # for Bootstrap sampling & RSF selection.

m.RF.1 <- randomForest(INJURIES_TOTAL ~POSTED_SPEED_LIMIT+WEATHER_CONDITION+
                         LIGHTING_CONDITION+LANE_CNT+ALIGNMENT+ROADWAY_SURFACE_COND+
                         ROAD_DEFECT+CRASH_TYPE+INTERSECTION_RELATED_I+NOT_RIGHT_OF_WAY_I+HIT_AND_RUN_I+DAMAGE+
                         BEAT_OF_OCCURRENCE+NUM_UNITS, data = df,
                       na.action = na.roughfix,
                       importance = T, keep.inbag= T)
summary(m.RF.1)

oob_predictions <- predict(m.RF.1)
oob_residuals <- df$INJURIES_TOTAL - oob_predictions
oob_mse <- mean(oob_residuals^2)
print(oob_mse)
oob_mse <- sqrt(oob_mse)
print(oob_mse)
