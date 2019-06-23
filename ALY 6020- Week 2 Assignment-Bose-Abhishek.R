# Set the working directory.
setwd("/Users/abhishek/Downloads/fashion")

#Import training data and test data
fashion_data_train <- read.csv("fashion_train.csv")
fashion_data_test <- read.csv("fashion_test.csv")

#Labeling each column with relevent name.
col_name <- c("label",sprintf("pixel%02d", seq(1,784))) #initialize a column name as a vector
colnames(fashion_data_train) <- col_name                #Add column name for Training Data
colnames(fashion_data_test) <- col_name                 #Add column name for Test Data

head(fashion_data_train)                                #Display head of training dataset

list_by_label <- list()                                 #Initialize a list to store all the group of 
                                                        #data with similar label

#Running a for loop to create a 10 data frame to store similar label data and add a 
#additional column to tag a classificatio label as 1 since this this will be our true data set
#for all the 10 sample of training dataset.

for (i in c(0:9)) {
  temp <- subset(fashion_data_train, fashion_data_train$label==i)
  temp["binary_label"] <- 1                            #Adding label as 1 for true dataset.
  list_by_label[[i+1]] <- temp
}


#Function that will create negetive sample for each group and add classification label as '0'
create.sample.for.each.label <- function(list_by_label,label,size_for_ith_sample) {
  negetive_data <- data.frame()                       #Initialize a data frame.
  for (i in c(0:9)) {                                 #for loop to collect data from all the label 
                                                      #other that the true label
    if (i != label){                                  #if condition to check if the sample is equal to true label
      temp <-as.data.frame(list_by_label[i+1])        #Initialize temp data frame to load the data of a particular label
      
                                                      #Create a sample based on size of our traing sample model this 
                                                      #will get passed from the main program
      temp<-temp[sample(nrow(as.data.frame(list_by_label[i+1])), size_for_ith_sample), ]
      temp["binary_label"] <- 0                       #Add zero as a negetive classifier
    }
    negetive_data <- rbind(negetive_data,temp)        #combining data of all the iteration
  }
  return(negetive_data)                               #Return Negetive data
}

list_of_train_data=list()                             #Initialize a list to store train dataset 
                                                      #for all the label
for (i in c(0:9)) {                                   #for loop to create training sample for each label
  size_for_ith_sample <- round((20000-nrow(list_by_label[[i+1]]))/9) # Calculate size of negetive data in our sample coming from each label
  negetive_data_itr <- create.sample.for.each.label(list_by_label,i,size_for_ith_sample) #Call Function
  temp <- as.data.frame(list_by_label[1+i])           #Store true data of our sample in a temporary variable
  train_data_frame <- rbind(temp,negetive_data_itr)   #Combining positive and negetive data to from sample. 
  list_of_train_data[[i+1]] <- train_data_frame[sample(1:20000), ] #Randomize the element in the sample
}

col_name_train <- names(train_list[[1]])              #Define Column names since we add a binary classification
x_var <- col_name_train[2:785]                        #Define Predictor variable column name
y_var <- col_name_train[786]                          #Define dependent Variable column name

rm(temp)
list_glm_model_fashion <- list()                      #Define list to store GLM model in form of list 
for (i in c(0:9)) {                                   #For loop to build the model for 10 labels
  temp <- as.data.frame(list_of_train_data[i+1])              #Import training sample for a particular label to a temp var
  temp <- cbind(temp[y_var],temp[x_var])              #Organizing the sample
  temp <- temp[sample(nrow(temp),),]

#Call the glm function to build the model   
  list_glm_model_fashion[[i+1]] <- list(glm(formula = binary_label~.,
                                            family = binomial(link = "logit"), 
                                            data=temp))
}

#Build the Prediction function for our test data.
rm(predict_obs)
df<-fashion_data_test[x_var]                         #Defining Test Data
actual_obs<-fashion_data_test[1]                     #Extract the actual observation
prob_vec <- rep(NA, 10)                              #Initialize a vector to store 10 probability score for each record
predict_obs <- data.frame()                          #Init a data frame to store all the probability score
predict_obs  <- rbind(predict_obs,c(prob_vec))       #Initialize the first row
for (i in c(1:nrow(fashion_data_test))) {            #For loop to traverse all the record of test dataset
  for (j in c(0:9)) {                                #For loop to run prediction model for each data
    glm_model <- as.list(list_glm_model_fashion[[j+1]])   #Load the glm model for each iteration
    predict_test <- unlist(predict(glm_model,df[i,],
                                   type='response')) #Call the prediction Function
    prob_vec[j+1] <- predict_test                    #Vector to stor 10 prob score for a single image
    print(j)
  }
  predict_obs[nrow(predict_obs) + 1,] = c(prob_vec)  #Combining all vectors
  print(i)
}
colnames(predict_obs) <- c(0:9)                      #Labeling column with respective label
predict_obs <- predict_obs[-c(1),]
Predicted <- vector()                                        #Initialize Vector to store max probability for each record or row
for (i in c(1:nrow(fashion_data_test))) {            #Assigning max probabilty for ith data or row
  Predicted[i] <- (which.is.max(as.vector(as.numeric(predict_obs[i,]))))-1
}

actual<-actual_obs[c(1:nrow(fashion_data_test)),]         #Extracting actual observation.
table_test<-table(actual,Predicted)                       #represent using table to find the actual vs prediction
print(table_test)                                         #Print Confusion Matrix
accuracy<-sum(diag(table_test))                           #Accuracy Calculation
print(accuracy/nrow(fashion_data_test))



New_train_sample <- fashion_data_train[1:20000,]
New_train_sample["binary_label"] <- 0                     #Adding label for binary classification.

new_list_of_train_data=list()
for (i in c(0:9)) {                                       #for loop to create training sample for each label
  New_train_sample["binary_label"] <- 0
  New_train_sample["binary_label"] <- as.numeric(ifelse(New_train_sample$label == i, 1, 0))
  new_list_of_train_data[[i+1]] <- New_train_sample   #assign it to the list
}


new_list_glm_model_fashion <- list()                      #Define list to store GLM model in form of list 
for (i in c(0:9)) {                                       #For loop to build the model for 10 labels
  temp <- as.data.frame(new_list_of_train_data[[i+1]])    #Import training sample for a particular label to a temp var
  temp <- cbind(temp[y_var],temp[x_var])                  #Organizing the sample
  
  #Call the glm function to build the model   
  new_list_glm_model_fashion[[i+1]] <- list(glm(formula = binary_label~.,
                                            family = binomial(link = "logit"), 
                                            data=temp))
  print(i)
}


rm(new_predict_obs)
df<-fashion_data_test[x_var]                         #Defining Test Data
actual_obs<-fashion_data_test[1]                     #Extract the actual observation
new_prob_vec <- rep(NA, 10)                              #Initialize a vector to store 10 probability score for each record
new_predict_obs <- data.frame()                          #Init a data frame to store all the probability score
new_predict_obs  <- rbind(new_predict_obs,c(new_prob_vec))       #Initialize the first row
for (i in c(1:2000)) {            #For loop to traverse all the record of test dataset
  for (j in c(0:9)) {                                #For loop to run prediction model for each data
    glm_model <- as.list(new_list_glm_model_fashion[[j+1]])   #Load the glm model for each iteration
    predict_test <- unlist(predict(glm_model,df[i,],
                                   type='response')) #Call the prediction Function
    new_prob_vec[j+1] <- predict_test                    #Vector to stor 10 prob score for a single image
    print(j)
  }
  new_predict_obs[nrow(new_predict_obs) + 1,] = c(new_prob_vec)  #Combining all vectors
  print(i)
}
colnames(new_predict_obs) <- c(0:9)                      #Labeling column with respective label
new_predict_obs <- new_predict_obs[-c(1),]
new_Predicted <- vector()                                        #Initialize Vector to store max probability for each record or row
for (i in c(1:2000)) {            #Assigning max probabilty for ith data or row
  new_Predicted[i] <- (which.is.max(as.vector(as.numeric(new_predict_obs[i,]))))-1
}

actual<-actual_obs[c(1:2000),]         #Extracting actual observation.
new_table_test<-table(actual,new_Predicted)                       #represent using table to find the actual vs prediction
print(table_test)                                         #Print Confusion Matrix
new_accuracy<-sum(diag(new_table_test))                           #Accuracy Calculation
print(accuracy/nrow(fashion_data_test))
