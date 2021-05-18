# Load CompleteResponses and SurveyIncomplete data
library(readr)
Data_Complete<-read.csv('CompleteResponses.csv')
Survey_Incomplete<-read.csv('SurveyIncomplete.csv')

# EDA of the raw data

library(summarytools)
view(dfSummary(Data_Complete),
     file="Data_Complete_RAW.html")

view(dfSummary(Survey_Incomplete),
     file="Survey_Incomplete_RAW.html")

library(explore)
explore(Data_Complete)

library(dlookr)
plot_normality(Data_Complete)
plot_correlate(Data_Complete)

eda_report(Data_Complete,target=brand,output_format="html",
           output_file="EDA_dlookr_report.html")

library(DataExplorer)

#plot_str(Data_Complete)
plot_missing(Data_Complete)
plot_histogram(Data_Complete)
plot_correlation(Data_Complete)
plot_bar(Data_Complete)
plot_boxplot(Data_Complete,by="car")

# Data processing

str(Data_Complete)
Data_Complete$brand<-as.factor(Data_Complete$brand)
# now for brand 0 becomes 1 and 1 becomes 2,
# therefore, now 1 means Acer & 2 means Sony?

# Data partitioning
library(caret)
set.seed(123)
idx_training<-createDataPartition(Data_Complete$brand,
                                  p=0.75,list=FALSE)
training<-Data_Complete[idx_training,]
testing<-Data_Complete[-idx_training,]

#10 fold cross validation

fitControl<-trainControl(method="repeatedcv",number=10,
                         repeats=1)

# train Stochastic Gradient Boost GBM with automatic grid
model_gbm<-train(brand~.,data=training,method='gbm',
                 preProcess=c('scale','center'),
                 trControl=fitControl)

model_gbm #this displays model summary in console

library(gbm)
model_gbm_imp<-varImp(model_gbm)

model_gbm_imp


# train Random Forest with 10-fold xval
# and manually tune 5 different mtry values

#dataframe for manual tuning of mtry
rfGrid<-expand.grid(mtry=c(1,2,3,4,5))

model_RF<-train(brand~.,data=training,method='rf',
                preProcess=c('scale','center'),
                trControl=fitControl,
                tuneGrid=rfGrid)

model_RF
model_RF_imp<-varImp(model_RF)
model_RF_imp

# Make and assess predictions on the testing set

pred_gbm<-predict(model_gbm,testing)
pred_RF<-predict(model_RF,testing)

cm_gbm<-confusionMatrix(testing$brand,pred_gbm)
cm_RF<-confusionMatrix(testing$brand,pred_RF)

#library(dplyr)

#library(tidyr)

#### Creating the draw_confusion_matrix_function
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Acer', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Sony', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Acer', cex=1.2, srt=90)
  text(140, 335, 'Sony', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(cm_gbm)
draw_confusion_matrix(cm_RF)

pR_gbm<-postResample(testing$brand,pred_gbm)
pR_RF<-postResample(testing$brand,pred_RF)



# MODEL SELECTED: GBM

# Data processing

str(Survey_Incomplete)
Survey_Incomplete$brand<-as.factor(Survey_Incomplete$brand)

# now, apply model to incomplete survey

Survey_corrected=Survey_Incomplete

Survey_corrected$brand<-predict(model_gbm,Survey_corrected)

summary(Survey_Incomplete)
summary(Survey_corrected)
# histograms to compare before and after 
# synthetic brand

b<-c("Acer","Sony")

library(ggplot2)

#ggplot(data.frame(Survey_corrected$brand), 
       #aes(x=b)) +  geom_bar()

ggplot(data.frame(Survey_Incomplete$brand), 
       aes(x=b)) +  geom_bar()

view(dfSummary(Survey_Incomplete),
     file="Survey_Incomplete.html")

view(dfSummary(Survey_corrected),
     file="Survey_corrected.html")

save.image()

