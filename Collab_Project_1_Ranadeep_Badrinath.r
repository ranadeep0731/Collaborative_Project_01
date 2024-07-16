#######################################################################################################################################################################################################################
#Project Name: NORTH - POINT SOFTWARE PRODUCTION COMPANY
#Student Name: RANADEEP REDDY PINGILI, BADRINATH REDDY KUSUGUNDLA
#######################################################################################################################################################################################################################
#--------------------------------------------REQUIRED LIBRARIES--------------------------------------------#
#install.packages("caret")      # Classification And Regression Training
#install.packages("dplyr")      # Data manipulation and transformation
#install.packages("gains")      # Evaluation of predictive models
#install.packages("ggcorrplot") # Visualization of correlation matrices using ggplot2
#install.packages("ggplot2")    # Data visualization and plotting
#install.packages("gridExtra")  # Combining plots in a single view
#install.packages("MASS")       # Various statistical functions and datasets; for stepwise AIC
#install.packages("rpart")      # Recursive Partitioning and Regression Trees for decision tree analysis
#install.packages("rpart.plot") # Visualization of decision trees produced by rpart package
#install.packages("tidyr")      # Data manipulation and reshaping for tidy data principles
library(caret)
library(dplyr)
library(gains)
library(ggcorrplot)
library(ggplot2)
library(gridExtra)
library(MASS)
library(rpart)
library(rpart.plot)
library(tidyr)

#----------------------------------------------DATA COLLECTION----------------------------------------------#
northpoint.df <- read.csv('North-PointList.csv') #Load data from the csv file

#--------------------------------------DATA EXPLORATION & PRE_PROCESSING------------------------------------#
dim(northpoint.df)  #Check for total number of rows and columns 
northpoint.df[1:5,] #Display first 5 rows
#View(northpoint.df) #Show all the data in a new tab
str(northpoint.df)  #Overview of the data
sum(is.na(northpoint.df)) #Identify the missing values
colSums(northpoint.df == 0) #Check for zero values in each column
boxplot(northpoint.df, outline = TRUE, main = "Boxplot of Each Column") #Box plot to check for outliers

#Visualize distribution of a categorical variable
plot_US <- ggplot(northpoint.df, aes(x = factor(US))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of US", x = "US", y = "Frequency")

plot_source_a <- ggplot(northpoint.df, aes(x = factor(source_a))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_a", x = "source_a", y = "Frequency")

plot_source_c <- ggplot(northpoint.df, aes(x = factor(source_c))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_c", x = "source_c", y = "Frequency")

plot_source_b <- ggplot(northpoint.df, aes(x = factor(source_b))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_b", x = "source_b", y = "Frequency")

plot_source_d <- ggplot(northpoint.df, aes(x = factor(source_d))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_d", x = "source_d", y = "Frequency")

plot_source_e <- ggplot(northpoint.df, aes(x = factor(source_e))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_e", x = "source_e", y = "Frequency")

plot_source_m <- ggplot(northpoint.df, aes(x = factor(source_m))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_m", x = "source_m", y = "Frequency")

plot_source_o <- ggplot(northpoint.df, aes(x = factor(source_o))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_o", x = "source_o", y = "Frequency")

plot_source_h <- ggplot(northpoint.df, aes(x = factor(source_h))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_h", x = "source_h", y = "Frequency")

plot_source_r <- ggplot(northpoint.df, aes(x = factor(source_r))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_r", x = "source_r", y = "Frequency")

plot_source_s <- ggplot(northpoint.df, aes(x = factor(source_s))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_s", x = "source_s", y = "Frequency")

plot_source_t <- ggplot(northpoint.df, aes(x = factor(source_t))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_t", x = "source_t", y = "Frequency")

plot_source_u <- ggplot(northpoint.df, aes(x = factor(source_u))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_u", x = "source_u", y = "Frequency")

plot_source_p <- ggplot(northpoint.df, aes(x = factor(source_p))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_p", x = "source_p", y = "Frequency")

plot_source_x <- ggplot(northpoint.df, aes(x = factor(source_x))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_x", x = "source_x", y = "Frequency")

plot_source_w <- ggplot(northpoint.df, aes(x = factor(source_w))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of source_w", x = "source_w", y = "Frequency")

plot_Web.order <- ggplot(northpoint.df, aes(x = factor(Web.order))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Web.order", x = "Web.order", y = "Frequency")

plot_Gender.male <- ggplot(northpoint.df, aes(x = factor(Gender.male))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Gender.male", x = "Gender.male", y = "Frequency")

plot_Address_is_res <- ggplot(northpoint.df, aes(x = factor(Address_is_res))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Address_is_res", x = "Address_is_res", y = "Frequency")

plot_Purchase <- ggplot(northpoint.df, aes(x = factor(Purchase))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Purchase", x = "Purchase", y = "Frequency")

#Grid view: distribution of a categorical variable
grid.arrange(plot_US, plot_source_a, plot_source_c, plot_source_b, plot_source_d, plot_source_e, plot_source_m, plot_source_o, plot_source_h, plot_source_r, plot_source_s, plot_source_t, plot_source_u, plot_source_p, plot_source_x, plot_source_w, plot_Web.order, plot_Gender.male, plot_Address_is_res, plot_Purchase, nrow=4)

#Visualize distribution of a numeric variable
plot_Freq <- ggplot(northpoint.df, aes(x = Freq)) +
  geom_histogram(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Freq", x = "Freq", y = "Frequency")

plot_last_update_days_ago <- ggplot(northpoint.df, aes(x = last_update_days_ago)) +
  geom_histogram(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of last_update_days_ago", x = "last_update_days_ago", y = "Frequency")

plot_X1st_update_days_ago <- ggplot(northpoint.df, aes(x = X1st_update_days_ago)) +
  geom_histogram(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of X1st_update_days_ago", x = "X1st_update_days_ago", y = "Frequency")

plot_Spending <- ggplot(northpoint.df, aes(x = Spending)) +
  geom_histogram(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Spending", x = "Spending", y = "Frequency")

#Grid view: distribution of a numeric variable
grid.arrange(plot_Freq, plot_last_update_days_ago, plot_X1st_update_days_ago, plot_Spending, nrow=2)

#---------------------------------------PREDICTORS ANALYSIS AND RELEVANCY---------------------------------------#
northpoint.df <- northpoint.df[, -1] #Remove Sequence_Number column as it is not used for analysis
northpoint.df[1:5,]

#Use Logistic model to identify the significant predictors
logistic_model <- glm(Purchase ~. -Spending, data = northpoint.df, family = "binomial")
summary(logistic_model)
#Important predictors identified:source_a, source_e, source_m, source_h, source_r, source_s, source_t, source_u, source_p, source_x, source_w, Freq, Web.order, Address_is_res are given as important predictors

#---------------------------------------------Dimension Reduction---------------------------------------------#
#Considering numeric variables
northpoint_numeric_vars <- northpoint.df[,c("Freq","last_update_days_ago","X1st_update_days_ago")]
head(northpoint_numeric_vars)
summary(northpoint_numeric_vars)
cor(northpoint_numeric_vars) #Correlation for Numeric variables
ggcorrplot(cor(northpoint_numeric_vars)) # Plotting correlation matrix
#NOTE: Correlation is seen between:- "last_update_days_ago" and "X1st_update_days_ago" - Keeping it the same for now and will decide removing one of these post model buildings.

#---------------------------------------------DATA TRANSFORMATIONS---------------------------------------------#
#Change column names
#Freq -> frequency, X1st_update_days_ago -> 1st_update_days_ago, Web.order -> web_order, Gender.male -> gender_male, Address_is_res -> address_is_res.
#northpoint.df <- rename(northpoint.df, frequency = Freq, '1st_update_days_ago' = X1st_update_days_ago, web_order = Web.order, gender_male = Gender.male, address_is_res = Address_is_res)
#head(northpoint.df)

library(plyr)
northpoint.df<-rename(northpoint.df, c("Freq" ="frequency", "X1st_update_days_ago" = "1st_update_days_ago", "Web.order" = "web_order", "Gender.male" = "gender_male", "Address_is_res" = "address_is_res"))
head(northpoint.df)

#---------------------------------------------DATASET PARTITIONING---------------------------------------------#
set.seed(2024) #Random number generator
indices <- sample(1:2000) #Generate random indices
train.df <- northpoint.df[indices[1:800], ] #Training data frame with 800 observations
valid.df <- northpoint.df[indices[801:1500], ] #Validation data frame with 700 observations
holdout.df <- northpoint.df[indices[1501:2000], ] #Holdout data frame with 500 observations
###########################################################################################################################################
#Step 1: Each list booklet costs approximately $2 to mail, which includes printing, postage, and mailing other mailing costs).
#Estimate the gross profit that the firm could expect from remaining 180,000 observations (customers) if it selects randomly from the pool.
###########################################################################################################################################
cost_per_mailing <- 2  # Cost per mailing booklet in dollars
total_remaining_customers <- 180000  # Total remaining customers
total_test_customers <- 20000  # Total customers in the test mailing
total_purchasers <- 1065  # Total purchasers from the test mailing
response_rate <- total_purchasers / total_test_customers # Response rate from the test mailing: 0.05325
avg_purchase_amount <- mean(northpoint.df$Spending[northpoint.df$Purchase==1]) #205.249
expected_revenue_per_mailing <- response_rate * avg_purchase_amount #10.92951

gross_profit <- total_remaining_customers * (expected_revenue_per_mailing - cost_per_mailing)
gross_profit #1607312 

#Considering the response_rate as 0.053 the gross profit is: 
expected_revenue_per_mailing <- 0.053 * avg_purchase_amount
gross_profit <- total_remaining_customers * (expected_revenue_per_mailing - cost_per_mailing)
gross_profit #1598075

#NOTE: 
 #1. gross_profit when considered response rate as 0.053 is: 1598075
 #2. gross_profit when considered response rate as 0.05325 is: 1607312
######################################################################################################################
#Step 2: Develop the best model for classifying a customer as purchaser or non-purchaser, using Logistic regression. 
#a.	Partition the data randomly into training (800 records), validation (700 records), and holdout set (500 records).
#b.	Run logistic regression on training data and evaluate model performance on validation data.
######################################################################################################################
#a.	Partition the data randomly into training (800 records), validation (700 records), and holdout set (500 records).
#Partitions(train, valid, and holdout) were created above in DATASET PARTITIONING section, using the same for model building.

#b.	Run logistic regression on training data and evaluate model performance on validation data.
#--------------------------------------LOGISTIC REGRESSION WITH ALL PREDICTORS--------------------------------------#
head(northpoint.df)
#Logistic Regression model building on train.df set
glm_model_np <- glm(Purchase ~. -Spending, data = train.df, family = "binomial")
summary(glm_model_np)
#Predictions on valid.df set
glm_model_np_predictions <- predict(glm_model_np, newdata = valid.df, type = "response")
#Confusion Matrix for Predictions on valid.df set
glm_model_np_predictions <- ifelse(glm_model_np_predictions > 0.5, 1, 0) #Convert predicted probabilities to binary using cutoff.
conf_matrix_np <- confusionMatrix(as.factor(glm_model_np_predictions), as.factor(valid.df$Purchase), positive = "1")
conf_matrix_np

#----------------------LOGISTIC REGRESSION WITH SIGNIFICANT PREDICTORS USING BACKWARD REJECTION----------------------#
stepAIC(glm_model_np, direction = "forward") #Stepwise using forward selection
stepAIC(glm_model_np, direction = "backward") #Stepwise using backward rejection
#Considering the significant predictors given by backward rejection. 

#Logistic Regression model building on train.df set using significant predictors
np_glm_model_imppred <- glm(Purchase ~ source_a + source_e + source_h + source_r + 
                              source_s + source_t + source_u + source_p + source_x + source_w + 
                              frequency + last_update_days_ago + web_order + address_is_res, 
                            family = "binomial", data = train.df)
summary(np_glm_model_imppred)
#Prediction on valid.df set
glm_model_np_predictions_imppred <- predict(np_glm_model_imppred, newdata = valid.df, type="response")
#Confusion Matrix for Predictions on valid.df set
glm_model_np_predictions_imppred <- ifelse(glm_model_np_predictions_imppred > 0.5, 1, 0) #Convert predicted probabilities to binary using cutoff.
conf_matrix_np_imppred <- confusionMatrix(as.factor(glm_model_np_predictions_imppred), as.factor(valid.df$Purchase), positive = "1")
conf_matrix_np_imppred

#Note: glm model with all predictors have the better accuracy and sensitivity compared to the model built on significant predictors using stepwise backward

#-------------------------------------------------CLASSIFICATION TREE-------------------------------------------------#
#Classification Tree model building on train.df set
np_tree_model <- rpart(Purchase ~. -Spending, data = train.df, method = "class")
rpart.plot(np_tree_model) # Visualize the decision tree
#Prediction on valid.df set
np_tree_model_pred <- predict(np_tree_model, newdata = valid.df, type = "class")
#np_tree_model_pred <- factor(np_tree_model_pred, levels = c("1","0"))
# Confusion Matrix for Predictions on valid.df set
conf_matrix_np_treemodel <- confusionMatrix(as.factor(np_tree_model_pred), as.factor(valid.df$Purchase), positive = "1")
conf_matrix_np_treemodel

#Note: Logistic Reg with all predictors is giving better performance.

#-------------------------------------K-NEAREST NEIGHBOUR MODEL WITH ALL PREDICTORS-------------------------------------#
train.df$Purchase <- factor(train.df$Purchase, levels = c("1", "0"))
#Train k-NN model with k=9
np_knn_model <- train(Purchase ~. -Spending, data=train.df,
                      method="knn",  # specify the model
                      preProcess=c("center", "scale"),  # normalize data
                      tuneGrid=expand.grid(k=9),
                      trControl=trainControl(method="none"))
np_knn_model
#Prediction on valid.df set
np_knn_model_pred <- predict(np_knn_model, newdata = valid.df)
#Confusion Matrix for Predictions on valid.df set
np_knn_conf_matrix <- confusionMatrix(as.factor(np_knn_model_pred), as.factor(valid.df$Purchase), positive = "1")
np_knn_conf_matrix

#----------------------------------K-NEAREST NEIGHBOUR MODEL WITH SIGNIFICANT PREDICTORS----------------------------------#
#Train k-NN model with k=9
np_knn_model_imppred <- train(Purchase ~ source_a + source_e + source_h + source_r + 
                                source_s + source_t + source_u + source_p + 
                                source_x + source_w + frequency + last_update_days_ago + 
                                web_order + address_is_res,
                              data = train.df,
                              method = "knn",  # specify the model
                              preProcess = c("center", "scale"),  # normalize data
                              tuneGrid = expand.grid(k = 9),
                              trControl = trainControl(method = "none"))
#Prediction on valid.df set
np_knn_model_imppred_pred <- predict(np_knn_model_imppred, newdata = valid.df)
#Confusion Matrix for Predictions on. valid.df set
np_knn_conf_matrix_imppred <- confusionMatrix(as.factor(np_knn_model_imppred_pred), as.factor(valid.df$Purchase), positive = "1")
np_knn_conf_matrix_imppred

#NOTE: KNN with significant predictors gave the best accuracy and sensitivity compared to KNN with all predictors.
##################################################################################################################################
# When Purchaser==1; Predict Spending
#Step 3: Develop the best model for predicting spending among the purchasers. Go through the modeling of machine learning process. 
##################################################################################################################################
#a.	Create training and validation datasets for only purchasers based on partitions from step 2.
head(northpoint.df)
nrow(northpoint.df) #2000
nrow(northpoint.df[northpoint.df$Purchase == 1, ]) #1000
northpoint_purchase <- northpoint.df[northpoint.df$Purchase == 1, ] #Choose records who made purchase and have some spending
head(northpoint_purchase)
nrow(northpoint_purchase) #1000

set.seed(2024)
n <- nrow(northpoint_purchase) #Sample size
train_indices <- sample(1:n, 0.4*n)
valid_indices <- sample(setdiff(1:n, train_indices), 0.35*n)
holdout_indices <- setdiff(1:n, c(train_indices, valid_indices))

spending_train.df <- northpoint_purchase[train_indices, ]
spending_valid.df <- northpoint_purchase[valid_indices, ]
spending_holdout.df <- northpoint_purchase[holdout_indices, ]

nrow(spending_train.df) #400
nrow(spending_valid.df) #350
nrow(spending_holdout.df) #250

#b.	Develop a model for predicting spending value, for each purchaser, with stepwise regression.
#------------------------------------------------LINEAR REGRESSION MODEL------------------------------------------------#
#Linear Regression model building on spending_train.df set
lm_model_spending <- lm(Spending ~. -Purchase, data = spending_train.df)
summary(lm_model_spending)
#Prediction on spending_valid.df set
lm_model_spending_pred <- predict(lm_model_spending, newdata = spending_valid.df)
#Performan Evaluation
MAE(spending_valid.df$Spending, lm_model_spending_pred) #MAE: 111.902
RMSE(spending_valid.df$Spending, lm_model_spending_pred) #RMSE: 178.2695
mean(spending_valid.df$Spending - lm_model_spending_pred) #ME: -13.19875

#Stepwise reg: Stepwise regression is like forward selection except that at each step, we consider dropping predictors that are not useful as in backward elimination.
trControl <- caret::trainControl(method="none")
model <- caret::train(Spending ~., data=spending_train.df, trControl=trControl,
                      method="glmStepAIC", direction='both')
coef(model$finalModel)
                                                        ###OR###

#--------------------------------------------------STEPWISE REGRESSION--------------------------------------------------# 
#Building stepwise regression model on spending_train.df
stepwise_model <- step(lm(Spending ~. -Purchase , data = spending_train.df))
summary(stepwise_model)
#Prediction and Performance metrics
rbind(Training=mlba::regressionSummary(predict(stepwise_model, spending_train.df), spending_train.df$Spending),
      Validation=mlba::regressionSummary(predict(stepwise_model, spending_valid.df), spending_valid.df$Spending))
# Training: MAE:94.72419, RMSE:141.1593, ME:-4.259704e-13
# Validation:MAE:109.8421, RMSE: 176.7141, ME:-10.79523

#c.	Develop a regression tree model.
#-------------------------------------------------REGRESSION TREE MODEL-------------------------------------------------# 
#Building regression tree model on spending_train.df
regtree_model_spending <- rpart(Spending ~. -Purchase , data = spending_train.df, method = "anova")
summary(regtree_model_spending)
rpart.plot(regtree_model_spending) #Plot the tree
#Prediction and Performance metrics
regtree_model_spending_pred <- predict(regtree_model_spending, newdata = spending_valid.df)
MAE(spending_valid.df$Spending, regtree_model_spending_pred) #MAE: 100.581
RMSE(spending_valid.df$Spending, regtree_model_spending_pred) #RMSE: 178.4194
mean(spending_valid.df$Spending - regtree_model_spending_pred) #ME: -5.653799

#d.	Choose the best model among your models
#The lower the MAE, the better the model.
#Regression tree model has lesser MAE compared to other models; but as per the project requirement i am choosing linear reg with stepwise model for prediction on holdout
#########################################################################################################################
#Step 4: Considering the original holdout data (the one was built in step 2) then, 
#########################################################################################################################
#a.	Add a column to the data frame with the predicted probability of purchase from your selected machine learning model in step 2.
holdout.df$predicted_probability_purchase <- predict(glm_model_np, newdata = holdout.df, type = "response")
str(holdout.df) #Check for column addition
#View(holdout.df) #View the values in newly added column

#b. Add another column with predicted spending value from the work in step 3.
#-----------------------------------------------------------------------------
#Generate predictions for spending using the stepwise regression
holdout.df$predicted_spending <- predict(stepwise_model, newdata = holdout.df)
str(holdout.df) #check for column addition
#View(holdout.df) #View the values in newly added column

#c. Add a column for “adjusted probability of purchase” to adjust for oversampling the purchaser (see #1). 
#This can be done by multiplying “predicted probability of purchase” by original purchase rate (1065/20000/0.50=0.1065). 
#This is to adjust for over-sampling the purchases. 
#-----------------------------------------------------------------------------------------------------------------------
org_purchase_rate <-0.1065 #given
holdout.df$adjusted_probability_purchase <- org_purchase_rate * holdout.df$predicted_probability_purchase
str(holdout.df) #check for column addition
#View(holdout.df) #View the values in newly added column

#d.	Add another column for expected spending. 
#This should be the adjusted spending – adjusting predicted spending of 4b by adjusted probability of 4c.
#---------------------------------------------------------------------------------------------------------
holdout.df$expected_spending <- (holdout.df$predicted_spending - (holdout.df$predicted_spending * holdout.df$adjusted_probability_purchase))
str(holdout.df) #check for column addition
#View(holdout.df) #View the values in newly added column

#e.	Plot the cumulative gain chart for the expected spending (cumulative expected spending as a function of records targeted).
#-----------------------------------------------------------------------------------------------------------------------------
gain <- gains(holdout.df$Spending, holdout.df$expected_spending) #Cumulative gain
#Cumulative gain chart
gain_np <- data.frame(
  ncases=c(0, gain$cume.obs),
  cum_expected_spending=c(0, gain$cume.pct.of.total * sum(holdout.df$expected_spending))
)
ggplot(gain_np, aes(x=ncases, y=cum_expected_spending)) +
  geom_line() +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout.df)), cum_expected_spending=c(0, sum(holdout.df$expected_spending))),
            color="gray", linetype=2) + # adds baseline
  labs(x="Records Targeted", y="Cumulative Expected Spending", title="Cumulative Gain Chart for Expected Spending") +
  scale_y_continuous(labels = scales::comma)
