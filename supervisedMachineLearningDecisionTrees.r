#supervised machine learning
#the data being used will be the flag data from the UC Irvine Machine Learning database
data <- read.csv('flag_data.csv', header = T)
#loaded the flag dataset and gave it the name data
data
#tested to make sure it loaded correctly

traindata = data[1:150, ]
#this data will be used to train the machine

testdata = data[151:194, ]
#this data will be used to test/validate the machine learning

#to use the CART modeling algorithm, we must install and load the package in R
install.packages('rpart')
library('rpart')
#loaded the package using the code above, I could have also used the package window to load it
#the rpart package is for recursive packaging, which is the process to which our trees are made

#there is an rpart manual that is available as a reference resource
#key terms
#supervised machine learning: search for patterns in labeled data to produce a model to make predictions on future labeled data
#explanatory variables: predictor variables or independent variables
#response variable: the variable you are predicting or classifying or dependent variable

#we will use the explanatory variables in our dataset to predict the area of a country
#so area is our dependent or response variable

#how many possible explanatory variables are there?
ncol(data)
#30 total columns - 1 (for the response variable), so 29 possible explanatory variables

#classification and regression trees (CART)
#uses rpart -> builds tree models in R

#classification trees are used when you want to classify your data into classes
#this means the response variable is discrete and finite
#when building classification trees with "rpart", the response must be binary (one of two possible categories)

#regression trees are used when response is continuous

#decision trees is a general term for the tree logic used in modeling

#trees are made of interior and terminal nodes
#structured upside down 
#root node is at the top and branches downwards

#terminal nodes are the ending nodes (no nodes beneath them)

#advantages of decision tree models:
#easist to interpret and explain, especially to busines professionals
#require less data prep (work with missing data)
#automatically perform feature selection, essentially finds most important variables for prediction automatically
#make very few assumptions about the data, which makes it easy to use (relatively)

#disadvantages of decision tree models:
#are prone to overfitting the data
#lack of assumptions in decision trees is also a disadvantage
#gives limitations on what you can say

#decision trees have many different types, including regression trees

#if we made religion our response variable, then we could not use classification trees
#classification trees are only valid in 'rpart' when the response is binary (religion is not binary)
#the 'C50' package would instead need to be used (it can handle multivariate classification)

#building a model

#a model to predict area of a country based on its flag symbols and other variables
model = rpart(area ~ language + religion + bars + stripes + colours, data = traindata, method = "anova", control = rpart.control(minsplit = 10))

#'area' is the response (dependent) variable
# '~' is used to separate the response and explanatory variables
# 'language + religion + bars + stripes + colours' are the explanatory (independent) variables
# 'method = 'anova'' since our response variable is a continuous variable
# 'control=rpart.control(minsplit10)' the default minimum split is 20, we lowered it to 10 since we only have 150 observations in the dataset
#NOTE: WE SKIPPED THE IMPORTANT PROCESS OF PERFORMING EXPLORATORY DATA ANALYSIS TO DETERMINE VARIABLES WHICH WOULD BE PREDICTIVE FOR YOUR MODEL
#ADVANTAGE OF BUIDLING DECISION TREE MODELS IS HOW AUTOMATICALLY ORDERS VARIABLES IN TERMS OF PREDICTIVE POWER BY TREE SPLITS

#build a model (quiz_model) has the response variable area 
#explanatory variables of: population, language, and religion
#use the default 'control' argument
quiz_model = rpart(area ~ population + language + religion, data = traindata, method='anova')
help("rpart")
#created the model after some tweaking, after looking at the help(rpart) result

model
#time to explain what the first line meant
#root: defines the split criteria, at root node there is none
#150: is that total number of observations
#346526000: the deviance or error
#607.4733: the predicted response at that split

# a '*' denotes a terminal node

#time to explain what the second line meant:
#coulours>=2.5: an interior node where values with colours>=2.5 are split
#117: remaining observations from the previous split
#157170400: the deviance or error
#463.0171: the predicted area at this split

#now we plot our model to better observe it
plot(model, margin = .1, main = "My First Model")
text(model)

#how the model is built
#since the response is the area of a country (continuous variable), we are building a regression tree model
#this is a walkthrough of how the CART algorithm determines the splits in our tree model

#any given split of a tree is defined by two characteristics:
#the variable that splits the data
#the cutoff value that splits the variable

#the optimality of a split for a varaible and cutoff is defined by the minimum amount of missclassification (error)
#misclassification (error) as calculated in statistical terms as the residual sum of squares (RSS) (more generally known as the deviance)

#step by step of how the RSS is calculated
#let us do this for the first split in our quiz_model

#taking a look at the R output of the quiz_model
quiz_model
#first split is with variable population and a cutoff value of 14.5
  #population is an explanatory variable we are using to predict the response (area of a country)
  #splits into two groups
    #populations greater than 14.5 and populations less than 14.5

#cbind() is a function in R which stands for column bind
  #cbind() is used to combine columns of data 
split_data = data.frame(cbind(traindata$population, traindata$area))
#creates a dataframe were those two columns are combined
colnames(split_data)
#shows that the columns are named X1 and X2
colnames(split_data) = c("population", "area")
#renamed the columns "population" and "area"

#with split_data we split it by the cutoff value of 14.5 on the variable population
group_one = split_data[which(split_data$population < 14.5), ]
group_two = split_data[which(split_data$population >= 14.5), ]

#group_one represents data used to calc sum of G1
#group_two represents data used to calc sum of G2
#i represents each row in group_one and group_two
#Yi represents the response value (area) for each row in both dataframes
#Ybar represents the mean of the response values for both dataframes

#calculate the RSS, first by the two groups

#group_one
g1_area_avg = mean(group_one$area)
g1_error = sum((group_one$area - g1_area_avg)^2)

#group_two
g2_area_avg = mean(group_two$area)
g2_error = sum((group_two$area - g2_area_avg)^2)

#total RSS
g1_error + g2_error
#verify that the individual groups' RSSs were properly calculated
g1_error
  #equal to the second node's RSS (deviance)
g2_error
  #equal to the second node's RSS (deviance)
quiz_model
#THEY WERE

#using language and a cutoff value of 5, calculate the error of the two groups that result from the split
split_data2 = data.frame(cbind(traindata$language, traindata$area))
group1 = split_data2[which(split_data2$religion < 5), ]
group2 = split_data2[which(split_data2$religion >= 5), ]
colnames(split_data2) = (c("religion", "area"))
g_one_area_avg = mean(group1$area)
g_one_error = sum((group1$area - g_one_area_avg)^2)

g_two_area_avg = mean(group2$area)
g_two_error = sum((group2$area - g_two_area_avg)^2)

g_one_error + g_two_error
#what is the deviation if group 1 is language >= 5
  #for me it is actually group2
g_two_error
#the deviance is: 178563443

g_one_error
#the deviance is: 167593457

#the total RSS of the split with variable language and cutoff value 5 is not less than the original RSS we calculated


#cross-validation
#to evaluate model accuracy, we take a subset of our data, apply our tree model and verify accuracy

subset = data[1:10, -4]
train_subset = data[11:150, ]
#i just created a subset of our training data and separated it from the training data
subset_area = data [1:10, 4]
#saved the areas of subset into subset_area

#now build cross-validation on rest of the train_subset
cv_model = rpart(area ~ language + religion + bars + stripes + colours, data = train_subset, method = "anova", control = rpart.control(minsplit = 10))
#had to reinstall rpart (on different computer) and re-run the previous code
install.packages(rpart)
library(rpart)

#next make predictions using this model on the subset data
  #use the predict() function in R
cv_predictions = predict(cv_model, newdata = subset, type ="vector")
cv_predictions = as.vector(cv_predictions)
cv_predictions
#predicted an area of 479.0000 for the second observation

#cross-validation uses data subset of the TRAINING DATA
  #test data is data which we do not have response values, so it would be impossible to cross validate

#overfitting
  #key concept that must be understood
#overfitting is when you find patterns in data that does not generalize new datasets
  #when your learning algorithm continues to develop hypotheses that reduce training set error at the cost of an increased test set error

#possible to build regression tree model which classifies every country area correctly
    #that would be overfitting our model
overfit_model = rpart(area ~ language + religion + bars + stripes + colours, data=traindata, method="anova", control = rpart.control(minsplit = 2, cp = 0))
plot(overfit_model, uniform = T, main = "Overfitting Model")

#overfit_model predict every area in the training dataset correctly
  #but it makes the tree very messy and complex
#overfitting would make the model perform very poorly with new data, because it is too specific to the TRAIN DATA!

#Training data predictions do not always generalize to test data

#conversely, if a model is too simple -> more likely to misclassify
  #there is a trade between complexity and accuracy
    #the trade is taken into account with the complexity parameter (cp)

nrow(traindata)
#the maximum number of splits a tree model would create would be infinite until the model perfectly predicts

#to prevent overfitting, a method called cost complexity pruning is applied to the tree model
  #it defines an equation which balances the model's goal to have high accuracy but also low complexity
#done by creating a penalty for a more complex tree in the value alpha

#by making the complexity parameter (cp) equal to zero we effectively removes the penalty for a more complex tree
#if alpha in cost complexity equation is 0, you would have the largest tree and the smallest error

#complexity parameter

#to prevent overfitting, a method called cost complexity pruning is applied to the tree model
#it defines an equation which balances the model's goal to have high accuracy but also low complexity
#done by creating a penalty for a more complex tree in the value alpha

#by making the complexity parameter (cp) equal to zero we effectively removes the penalty for a more complex tree
#if alpha in cost complexity equation is 0, you would have the largest tree and the smallest error

#complexity parameter
#cp in R, the cp is a pruned tree which minimizes the tradeoff between classification error and tree complexity
#after building tree, select optimal number of branches in your tree which gives the lowest cross-validation error and is the simplest tree
#printcp() in R enables you to pick the optimal complexity parameter and number of tree splits

printcp(model)
#see the cp for the model

plotcp(model)
#plot the cp graphically

#the lowest 'xerror' is 0.82569, however since .82569 + .05504 > 0.82569 we select a simpler tree

#the maximum complexity parameter that is within 1 SE of the dotted line is at 0.041

prune_model = prune (model, cp = .027)
plot(prune_model, main = "Model 1", margin = .1)
text(prune_model_two)

predictions = predict(prune_model, newdata = testdata, type = "vector")
predictions_two = predict(prune_model_two, newdata = testdata, type = "vector")

sum((test_responses - predictions)^2)
sum((test_responses - predictions_two)^2)

#what area does prune_model predict for country singapore?
  #598
data$area
#actual area of Singapore
  #is 1