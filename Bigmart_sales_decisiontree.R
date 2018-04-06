#Setting up the directory
getwd()
setwd("C:/Users/Ganesh/Documents/Bigmart")

#Load the CSV files 
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

# We have find 1463 missing variables in train set
colSums(is.na(train))
colSums(is.na(test))

# All the 1463 misssing variables are under the column "Item_Weight".
summary(train)
dim(train)

#Graphical representation of the variables
#install.packages("ggplot2")
library(ggplot2)

ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + 
  geom_point(size = 2.5, color="navy") +
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") + 
  ggtitle("Item Visibility vs Item Outlet Sales")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
  geom_bar(stat = "identity", color = "purple") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + 
  geom_bar( stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) +
  xlab("Item Type") + 
  ylab("Item Outlet Sales")+
  ggtitle("Item Type vs Sales")

ggplot(train, aes(Item_Type, Item_MRP)) +
  geom_boxplot() +
  ggtitle("Box Plot") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + 
  xlab("Item Type") + 
  ylab("Item MRP") + 
  ggtitle("Item Type vs Item MRP")
hist(train$Item_Weight)

#Need to combine the train and test column to fill the missing values
#Before that need to add the column in test data
test$Item_Outlet_Sales<-1
combi<-rbind(train,test)
colSums(is.na(combi))

#to fill na in the Item_weight column
combi$Item_Weight[is.na(combi$Item_Weight)]=median(combi$Item_Weight, na.rm = T)
table(is.na(combi$Item_Weight))

#Also i could see that item_visiblity is o values, because there is no '0' values for the item_Visibility
combi$Item_Visibility=ifelse(combi$Item_Visibility==0, median(combi$Item_Visibility),combi$Item_Visibility)

# Also i could see the outlet_size values are null 
levels(combi$Outlet_Size)[1]="Other"
summary(combi)

#In these data we could see that the Item _fat_Content values are duplicated. We need to revalued
#install.packages("dplyr")
#install.packages("plyr")
library(plyr)
library(dplyr)
combi$Item_Fat_Content=revalue(combi$Item_Fat_Content,c("LF"="Low Fat","reg"="Regular"))
combi$Item_Fat_Content=revalue(combi$Item_Fat_Content,c("low fat"="Low Fat"))
table(combi$Item_Fat_Content)     
#combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)
combi$Year <- 2013 - combi$Outlet_Establishment_Year

combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))
View(combi)
#Divide the data set
newtrain<-combi[1:nrow(train),]
dim(newtrain)
dim(train)
newtest<-combi[-(1:nrow(train)),]

##loading required libraries
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)

#setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree
tree_model <- train(Item_Outlet_Sales ~ ., 
                    data = newtrain, 
                    method = "rpart", 
                    trControl = fitControl, 
                    tuneGrid = cartGrid)
print(tree_model)

main_tree <- rpart(Item_Outlet_Sales ~ ., 
                   data = newtrain, 
                   control = rpart.control(cp=0.01))
prp(main_tree)
#Calculating RMSE after converting the result into the vector
library(Metrics)
pre_score <- predict(main_tree, type = "vector")
rmse(newtrain$Item_Outlet_Sales, pre_score)

#Exporting the result
main_predict <- predict(main_tree, newdata = newtest, type = "vector")
sub_file <- data.frame(Item_Identifier = test$Item_Identifier,
                       Outlet_Identifier = test$Outlet_Identifier, 
                       Item_Outlet_Sales = main_predict)
write.csv(sub_file, 'Decision_tree_sales.csv')

