rm(list = ls()) 
dataset <- 
  read.csv("C:/Users/Asus/Desktop/Data Mining Project/global air pollution dataset.csv")

View(dataset)



# Deleting rows with blank values in Country/City column

dataset <- dataset[dataset$Country!="", ] 
dataset <- dataset[dataset$City!="", ]
View(dataset)



# Deleting ("Country", "City", "CO_Category", "Ozone_Category", "NO2_Category"
# and "PM_2.5_Category columns

dataset <- dataset[ , ! colnames(dataset) %in% c("Country", "City", "CO_Category", "Ozone_Category", "NO2_Category", "PM_2.5_Category")]

# Taking 5000 random samples from all the records

dataset<- dataset[sample(nrow(dataset), size=5000), ]




library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = Ozone_Value, y = AQI_Value)) + 
  geom_point(color = "orange", alpha = .7, size = 2)+ #Specifying color, opacity and size geom_smooth( method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs Ozone Value in air") + #Title/heading of the plot
  xlab("Ozone Value") + #Label of x axis
  ylab("AQI Value") #Label of y axis


library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = CO_Value, y = AQI_Value)) + 
  geom_point(color = "green", alpha = 1, size = 2)+ #Specifying color, opacity and size 
  geom_smooth(method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs Carbon-Monoxide Value in air") + #Title/heading of the plot
  xlab("Carbon Monoxide Value") + #Label of x axis
  ylab("AQI Value") #Label of y axis


library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = NO2_Value, y = AQI_Value)) + 
  geom_point(color = "brown", alpha = 1, size = 2)+ #Specifying color, opacity and size 
  geom_smooth(method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs Nitrous Oxide Value in air") + 
  #Title/heading of the plot
  xlab("Nitrous Oxide Value") + #Label of x axis
  ylab("AQI Value") #Label of y axis


library(ggplot2) #Plotting tools belong to this library
ggplot(data = dataset, mapping = aes(x = PM_2.5_Value, y = AQI_Value)) + 
  geom_point(color = "black", alpha = 1, size = 2)+ #Specifying color, opacity and size 
  geom_smooth(method = "lm")+ #Specifying a linear model to be fitted
  ggtitle("Plot of overall Air Quality Index vs value of 2.5 μm particulates in air") + 
  #Title/heading of the plot
  xlab("2.5 μm particulates value") + #Label of x axis
  ylab("AQI Value") #Label of y axis


# Factoring our label from ‘Good’ to ‘Hazardous’
AQI_Category_list <- 
  factor(dataset$AQI_Category, levels=c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Unhealthy', 'Very Unhealthy', 'Hazardous'))

# Making a barplot

barplot(table(AQI_Category_list),
        main= "Number of cities divided by quality of air",
        xlab= "Grade",
        ylab= "Count",
        border= "red",
        col=c( "green" , "yellow" , "orange" , "red" , "brown" ,  "black" ))




# Splitter function
splitter = function(dataset, training_ratio)
{ # This function will split the dataset...
  # ...into training and testing sets and then return them in a list
  
  if(training_ratio>=1 | training_ratio<=0)
  {return("Training ratio has to be a fraction value i range: 0<x<1")
    
  }else
  {training_row_count<-round(training_ratio*nrow(dataset)) # No. Of rows
  testing_row_count<-round((1-training_ratio)*nrow(dataset))
  training_dataset<-head(dataset, training_row_count) # Splitting by head()
  testing_dataset<-tail(dataset, testing_row_count)  # Splitting by tail()
  return_vales <- list(training_dataset, testing_dataset) # Binding in a list
  return(return_vales)  # Returning the list
  }
  
}










# Manhattan distance function
manhattan_distance=function(training_row, testing_row, label_name)
{ # This function will return manhattan distance
  distance=0
  
  for(attribute in colnames(testing_row))
  { if(attribute %in% c(label_name)){next}   
    # Target label is irrelevant to calculating distance
    # Sometimes row count ‘X’ is also imported; which is also to be discarded
    distance=distance+ abs(testing_row[attribute] - training_row[attribute])
  }
  
  distance<-as.numeric(distance)
  return(distance)
}

# Euclidean distance function
euclidean_distance=function(training_row, testing_row, label_name)
{ # This function will return euclidean distance
  distance=0 
  for(attribute in colnames(testing_row))
  { if(attribute %in% c(label_name)){next} 
    # Target label is irrelevant to calculating distance
    # Sometimes row count ‘X’ is also imported; which is also to be discarded 
    distance=distance+ (testing_row[attribute] - training_row[attribute])^2
  }  
  distance<-as.numeric(distance)
  distance<-sqrt(distance)
  return(distance)}


# Function to work as KNN model
evaluate_KNN<-function(training_dataset, testing_dataset, k_value, label_name, distance_type ){
  # This function will predict target label using KNN and show accuracy rates
  match_count=0
  accuracy=0
  for(x in 1:nrow(testing_dataset)){
    testing_row=testing_dataset[x,]
    #creating distance matrix with 0 rows and 3 columns
    distance_matrix <- data.frame(matrix(ncol = 3, nrow = 0))
    #providing column names
    colnames(distance_matrix) <- c('distance', 'predicted_label', 'actual_label')
    
    for(i in 1:nrow(training_dataset))
    { # Training
      training_row=training_dataset[i,]
      # Specifying type of distance
      if(distance_type=="Euclidean")
      {distance=euclidean_distance(training_row, testing_row, label_name)
      }
      else{ distance=manhattan_distance(training_row, testing_row, label_name)}
      
      predicted_label= training_row[label_name]
      
      actual_label = testing_row[label_name]
      
      distance_matrix[nrow(distance_matrix) + 1,] <- c(distance,predicted_label,actual_label)
      
    }
    ordered_distance_matrix <- distance_matrix[order(distance_matrix$distance),]
    distance_matrix<-ordered_distance_matrix
    distance_matrix<-head(distance_matrix, k_value) # Only considering k rows
    unique_values=(unique(distance_matrix$predicted_label))
    
    tabulated_values=tabulate(match(distance_matrix$predicted_label, unique_values))
    
    predicted_label=unique_values[tabulated_values==max(tabulated_values)] 
    # Choosing the label which occurs the most among k records
    
    # Testing
    
    if(predicted_label==actual_label)
    {  
      match_count=match_count+1
      
    } 
  }
  accuracy=round(match_count/nrow(testing_dataset), 2)*100 # Calculating Accuracy
  
  return(accuracy)}










# Dropping AQI_Value column along with the X column, which is sometimes encountered 
dataset= subset(dataset, select = -c(AQI_Value) )

# Making our dataset smaller in-case the model takes too long to execute, this part is optional 
dataset<- dataset[sample(nrow(dataset), size=500), ]    
# Splitting our dataset into 80% - 20% format
train_set<-data.frame(splitter(dataset,0.8)[1]) 
test_set<-data.frame(splitter(dataset,0.8)[2]) 

# Evaluating through KNN and visualizing the results (Euclidean distancing)
k_value <- c(1, 3, 5, 7, 9) #  Show accuracy for these k values
accuracy_rate <- c(evaluate_KNN(train_set, test_set, 1, 'AQI_Category', 'Euclidean'),
                   evaluate_KNN(train_set, test_set, 3, 'AQI_Category', 'Euclidean'),
                   evaluate_KNN(train_set, test_set, 5, 'AQI_Category', 'Euclidean'),
                   evaluate_KNN(train_set, test_set, 7, 'AQI_Category', 'Euclidean'),
                   evaluate_KNN(train_set, test_set, 9, 'AQI_Category', 'Euclidean'))
#  Distances will be calculated in Euclidean Method
#  'AQI_Category' is the target label
plotting_df_euclidean <- data.frame(k_value, accuracy_rate)
print (plotting_df_euclidean)
plotting_df_euclidean$k_value<-as.character(plotting_df_euclidean$k_value)

library(ggplot2)
ggplot(plotting_df_euclidean, aes(x = k_value, y = accuracy_rate )) +
  geom_bar(stat = "identity",color="black",  fill=c("purple", "pink", "red", "cyan", "violet"))+
  ggtitle("Accuracy of KNN model with Euclidean distance and different K values") + 
  #Title/heading of the plot
  xlab("Values of K nearest neighbours") + #Label of x axis
  ylab("Accuracy rate") #Label of y axis
# Evaluating through KNN and visualizing the results (Manhattan distancing)
k_value <- c(1, 3, 5, 7, 9) #  Show accuracy for these k values
accuracy_rate <- c(evaluate_KNN(train_set, test_set, 1, 'AQI_Category', 'Manhattan'),
                   evaluate_KNN(train_set, test_set, 3, 'AQI_Category', 'Manhattan'),
                   evaluate_KNN(train_set, test_set, 5, 'AQI_Category', 'Manhattan'),
                   evaluate_KNN(train_set, test_set, 7, 'AQI_Category', 'Manhattan'),
                   evaluate_KNN(train_set, test_set, 9, 'AQI_Category', 'Manhattan'))
#  Distances will be calculated in Manhattan Method
#  'AQI_Category' is the target label

plotting_df_manhattan <- data.frame(k_value, accuracy_rate)

print (plotting_df_manhattan)

plotting_df_manhattan$k_value<-as.character(plotting_df_manhattan$k_value)

library(ggplot2)
ggplot(plotting_df_manhattan, aes(x = k_value, y = accuracy_rate )) +
  geom_bar(stat = "identity" ,color="black",  fill=c("orange", "yellow", "green", "brown", "salmon"))+ # Setting colors
  ggtitle("Accuracy of KNN model with Manhattan distance and different K values") + 
  # Title/heading of the plot
  xlab("Values of K nearest neighbours") + # Label of x axis
  ylab("Accuracy rate") # Label of y axis


