rm(list=ls())

#Setting up our environment
#libraries we're going to use

#install.packages('xgboost')
#install.packages('tidyverse')

library(xgboost) # for xgboost
library(tidyverse) # general utility functions

#set a random seed & shuffle data frame
set.seed(1234)
airbnb <- airbnb[sample(1:nrow(airbnb)), ]

#Preparing our data & selecting features
#print the first few rows of our dataframe
head(airbnb)

#Remove information about the target variable from the training data
airbnbtune <- airbnb %>%
  select(host_is_superhost, host_listings_count, host_identity_verified, neighbourhood_cleansed, property_type, room_type, bedrooms, beds, price, availability_90, number_of_reviews_ltm, review_scores_rating) 

head(airbnbtune$neighbourhood_cleansed)
head(airbnbtune$property_type)

# convert categorical factor into one-hot encoding
neighbourhood <- model.matrix(~neighbourhood_cleansed-1, airbnbtune)
property <- model.matrix(~property_type-1, airbnbtune)
room <- model.matrix(~room_type-1, airbnbtune)

# add our one-hot encoded variable and convert the dataframe into a matrix
airbnbmat <- cbind(airbnbtune, neighbourhood, property, room)
airbnbmat2 <- airbnbmat %>%
  select(-neighbourhood_cleansed, -property_type, -room_type)

# Define Target
x <- airbnbmat2
y <- airbnbmat2 %>%
  select(price)

# convert into matrix
x<- as.matrix(x)
y<- as.matrix(y)

#Split dataset into testing and training subsets
#get the numb 70/30 training test split
numberOfTrainingSamples <- round(length(y) * .7)

# training data
train_data <- x[1:numberOfTrainingSamples,]
train_labels <- y[1:numberOfTrainingSamples]

# testing data
test_data <- x[-(1:numberOfTrainingSamples),]
test_labels <- y[-(1:numberOfTrainingSamples)]

#Convert the cleaned dataframe to a dmatrix
# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

#Training our model
model <- xgboost(data = dtrain, # the data   
                 nround = 10, # max number of boosting iterations
                 objective = "reg:linear")  # the objective function

scoring_data <- scoringdata %>%
  select(host_is_superhost, host_listings_count, host_identity_verified, neighbourhood_cleansed, property_type, room_type, bedrooms, beds, availability_90, number_of_reviews_ltm, review_scores_rating) 
)


#test
pred <- predict(model, newdata = dtest)

# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)
