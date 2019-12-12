# Module 2 - Task 3: Multiple Regression in R
# Ariadna Fernández

#### ---- Function for saving PostResample Results and Predictions ----
ModelsMetrics<-list()
MyMetrics<-function(model,index){
  predictionmodel<-predict(model,TestingSet)
  metric<-postResample(predictionmodel,TestingSet$Volume)
  ModelsMetrics[[index]]<-metric
  ModelsMetrics
}


ModelsPredictions<-data.frame(matrix(NA, nrow=22, ncol=0))
MyPredictions<-function(model,index){
  predictionmodel<-predict(model,TestingSet)
  ModelsPredictions<-predictionmodel
  predictionmodel
  ModelsPredictions
}


#### ------ Libraries ------
library("readr")
library("ggplot2")
library("reshape2") #For the correlation matrix as a heatmap
library("ggcorrplot") #For the correlation matrix as a heatmap
library("corrplot")  
library("GGally") #It is like an extension of ggplot2
library("caret") #To dummify variables
library("dplyr") #Pipelines
library("ggpubr") #For one way anove test
library("tidyverse")  
library("corrr") #Extra work with correlation matrix
library("reshape") #Reorder things, like the correlation matrix
library("caret") #For the models (Random Forest)
library("party") #For ctree function (Decision Tree)
library("Hmisc") #To reorder the correlation matrix (Pairwise)
library("plotly") #Nice Violin Plots!


####------ Load the data and take a look -------
  ExistingProductsData <- read.csv(file = "existingproductattributes2017.csv", header = TRUE)
  colnames(ExistingProductsData)[11] <- "RecommendProduct" #To change name for a single column (I did not like the spelling)
  attach(ExistingProductsData)
  summary(ExistingProductsData) #BestSellersRank has 15 NA values
  str(ExistingProductsData)
  levels(ProductType) #To see the different products
  table(ExistingProductsData$ProductType)


#### ------- Plotting the Data ------
 #Volume distribution to find outliers
    boxplot(Volume, main="Volume Sales Distribution")
 #Frequency distribution for ProductType and Volume of Sales
    ggplot(data.frame(ProductType), aes(x=ProductType)) + geom_bar(fill="blue") + ggpubr::rotate_x_text() + ggtitle("Product Type Counts") + ylab("Frequency")+ theme(plot.title = element_text(hjust = 0.5))
    ggplot(data.frame(Volume), aes(x=Volume)) + geom_bar(fill="blue") + ggpubr::rotate_x_text() + ggtitle("VOlume of Sales Counts") + ylab("Frequency")+ theme(plot.title = element_text(hjust = 0.5))#+ xlim (0,2500)
 #ScatterPlot for Volume
    ggplot(ExistingProductsData, aes(Volume,Volume,color = ProductType)) + geom_jitter(aes(colour = ProductType)) #It is redundant to color by Product class 2 times, right?
  #Boxplot showing ProductType and Volume
    ggplot(ExistingProductsData, aes(x=ProductType, y=Volume, color=ProductType)) + ggpubr::rotate_x_text() +  geom_boxplot()
  # ScatterPlot Volume with the other attributes (coloured per ProductType)
    ggplot(ExistingProductsData, aes(Volume, x5StarReviews, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, x4StarReviews, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, x3StarReviews, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, x1StarReviews, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, PositiveServiceReview, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, NegativeServiceReview, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, RecommendProduct, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, ShippingWeight, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, ProfitMargin, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    ggplot(ExistingProductsData, aes(Volume, ExistingProductsData$ProductDimension, color = ProductType)) + geom_jitter() + ggpubr::rotate_x_text()
    
    #Violin Plot for Volume Distribution
    violinplot <- ExistingProductsData %>% plot_ly(y = ~Volume, type = 'violin', box = list(visible = T),
         meanline = list(visible = T), x0 = 'Volume Distribution') %>% 
         layout(yaxis = list(title = "", zeroline = F, title = "Volume Distribution"))    
    
    violinplot
    
#### ------- Feature Engineering ------
    
#Product Height, width and length is going to be transformed in ProductDimension:
  ExistingProductsData$ProductDimension <- (ExistingProductsData$ProductDepth)*(ExistingProductsData$ProductWidth)*(ExistingProductsData$ProductHeight)
  ExistingProductsData$ProductDepth <- NULL
  ExistingProductsData$ProductHeight <- NULL
  ExistingProductsData$ProductWidth <- NULL
  # To remove a certain product type rows:
    #ExProdData <- ExProdData[ExProdData$ProductType != "ExtendedWarranty", ]
#### ------- Preprocessing ------

  #Exclusion of Product Number
   ExistingProductsData$ProductNum <- NULL
  #### ------ Missing values ------
    sum(is.na(ExistingProductsData))
    ExistingProductsData$BestSellersRank <- NULL #15 of 80 observations does not have this attribute. WE could either remove the 15 rows or remove the column. As the column attribute is not very relevant, we remove it  
    sum(is.na(ExistingProductsData))
  #### ------ Outliers -------
  #Volume Outliers needs to be removed
      boxplot1<-boxplot(Volume)
      outliers1 <- boxplot(Volume, plot=FALSE)$out
      found_outliers1<-ExistingProductsData[which(Volume %in% outliers1),]
      ExistingProductsData <- anti_join(ExistingProductsData,found_outliers1)
      boxplot(ExistingProductsData$Volume)
    #Outliers for volume as function of ProductType
      #outliers2 <- boxplot(Volume~ProductType, plot=FALSE)$out
      #found_outliers2<-ExistingProductsData[which(Volume %in% outliers2 & c(ProductType=="Accessories" | ProductType=="ExtendedWarranty" | ProductType=="Printer") ),]
      #Existing_WithoutOutliers2 <- anti_join(ExistingProductsData,found_outliers2)
      #boxplot(Existing_WithoutOutliers2$Volume~Existing_WithoutOutliers2$ProductType)
      
  #### ------ Dummy Data ------  
    newDataFrame <- dummyVars(" ~ .", data = ExistingProductsData)
    summary(newDataFrame)
    DummyData <- data.frame(predict(newDataFrame, newdata = ExistingProductsData))

#### ------ Correlations ------  
  ### ---- Normal Correlation Matrix ----
    corrData <- cor(DummyData)
    corrData 
    summary(corrData)
    names(DummyData)
    ggcorrplot(corrData, lab=FALSE, outline.col = "white", title="Correlation Matrix")  #method="circle" when possible is better the visualization
  #Volume with ProductType
    ggcorrplot(corrData[c(1:19),c(1:19)], lab=TRUE, outline.col = "white", title="Correlation Matrix of ProductType and Reviews", type="lower") + theme(plot.title = element_text(hjust = 0.5))  
  #Volume with the rest of the Variables
    ggcorrplot(corrData[c(13:25),c(13:25)], lab=TRUE, outline.col = "white", title="Correlation Matrix without ProductType") + theme(plot.title = element_text(hjust = 0.5))
  #To see only the volume correlations, filtered between 0.3 and 0.9
    volumecorrelation<-corrData
    volumecorrelation<-volumecorrelation[,c(24)]
    volumecorrelation[abs(volumecorrelation)<0.4] <-NA
    volumecorrelation[abs(volumecorrelation)>0.9] <-NA
    volumecorrelation<-sort(volumecorrelation, decreasing = TRUE)
    volumecorrelation
    sink("volumecorrelation.txt")
    print(volumecorrelation)
    sink()
  #To order the correlation matrix to see highest correlations (PairWise table)
    list_correlations<-corrData
    #list_correlations[list_correlations == 1] <- NA #drop 1 values. In this case no, I want to crosscheck x5stars review
    list_correlations[abs(list_correlations) < 0.5] <- NA # drop less than abs(0.5)
    list_correlations <- na.omit(melt(list_correlations)) # melt! 
    list_correlations[order(-abs(list_correlations$value)),] # sort
  #Find the correlation between our independent variables: x4star, x3star,x2star, positive and negative review
    filteredlist<-list_correlations %>% filter(X1=="x4StarReviews" | X1=="PositiveServiceReview"| X1=="x3StarReviews" | X1=="NegativeServiceReview" | X1=="x2StarReviews")
    filteredlist<-list_correlations %>% filter(X2=="x4StarReviews" | X2=="PositiveServiceReview"| X2=="x3StarReviews" | X2=="NegativeServiceReview" | X2=="x2StarReviews")
    filteredlist<-filteredlist %>% arrange(desc(value))
    filteredlist
    sink("independentcorrelations.txt")
    print(filteredlist)
    sink()
  
  ### ---- Correlation with Corrr Package ----
  #All the infi: https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/
    #CorrrPackage_Data <- correlate(DummyData, method = "pearson", diagonal = 1)
    #CorrrPackage_Data
    #fashion(CorrrPackage_Data)
    #rplot(CorrrPackage_Data)
    #CorrrPackage_Data %>% rplot(shape = 15, colours = c("blue", "red"), legend = TRUE)
    
  #Select Sections above 0.7:
      #Corr_above70 <- function(CorrrPackage_Data) any(CorrrPackage_Data > .6, na.rm = TRUE)
      #test<-CorrrPackage_Data %>% focus_if(Corr_above70, mirror = TRUE)
      #CorrrPackage_Data %>% rplot

####------ MODELING ------- 
  #We decided to keep only the following attributes: x4star, x3star,x2star, positive and negative review
    names(DummyData)  
    DummyData_v2<-DummyData %>% select(x4StarReviews,x3StarReviews,x2StarReviews,PositiveServiceReview,NegativeServiceReview,Volume)
    names(DummyData_v2)
  ####------ Training and Test Sets -------
    set.seed(123)
    training_indices <- createDataPartition(DummyData_v2$Volume, p=.70, list = FALSE)
    TrainingSet <- DummyData_v2[training_indices,]
    TestingSet <- DummyData_v2[-training_indices,]
    
    hist(TrainingSet$Volume)
    hist(TestingSet$Volume)

  #### ----- Linear Model----- 
    #4xStart Reviews and Positive Review Service
      LM_4xstar_positive <- lm(Volume~x4StarReviews+PositiveServiceReview, DummyData_v2)
      summary(LM_4xstar_positive)
      sink("LM_4xstar_positive.txt")
      print(summary(LM_4xstar_positive))
      sink()
      ModelsMetrics<-MyMetrics(LM_4xstar_positive, "LinearModel_2var")
      ModelsPredictions$"LinearModel_2var"<-MyPredictions(LM_4xstar_positive, "LinearModel_2var")
     
    #4xStart Reviews, 3xStartReview, Positive Review Service
      LM_4xstar_3xstar_positive <- lm(Volume~x4StarReviews+PositiveServiceReview+x3StarReviews, DummyData_v2)
      summary(LM_4xstar_3xstar_positive)
      sink("LM_4xstar_3xstar_positive.txt")
      print(summary(LM_4xstar_3xstar_positive))
      sink()
      ModelsMetrics<-MyMetrics(LM_4xstar_3xstar_positive, "LinearModel_3var")
      ModelsPredictions$"LinearModel_3var"<-MyPredictions(LM_4xstar_3xstar_positive, "LinearModel_3var")
    
      #All the attributes
      LM_all <- lm(Volume~., DummyData_v2)
      summary(LM_all)
      sink("LM_all.txt")
      print(summary(LM_all))
      sink()
      ModelsMetrics<-MyMetrics(LM_all, "LM_all")
      ModelsPredictions$"LinearModel_all"<-MyPredictions(LM_4xstar_3xstar_positive, "LM_all")

  #### ------ Decission Tree -----
    #10 fold cross validation
    set.seed(654)
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
    decisiontree <- ctree(Volume~.,data = TrainingSet, controls = ctree_control(maxdepth = 4))
    #If problems with visualization: https://stackoverflow.com/questions/13751962/how-to-plot-a-large-ctree-to-avoid-overlapping-nodes
    plot(decisiontree)
    
  #### ----- Random Forest (Auto Grid) -----
    set.seed(456)
    fitControl_RF <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
    #4xStart Reviews and Positive Review Service  
      RF_4xstar_positive <- train(Volume~x4StarReviews+PositiveServiceReview, data = TrainingSet, method = "rf", trControl=fitControl_RF, tuneLength = 2, importance=TRUE) #ntree = 50, do.trace = 10 #Train the model with a tuneLenght = 2 (trains with 2 different mtry values for RandomForest)
      RF_4xstar_positive
      varImp(RF_4xstar_positive)
      summary(RF_4xstar_positive)
      sink("RF_4xstar_positive.txt")
      print(summary(RF_4xstar_positive))
      sink()
      ModelsMetrics<-MyMetrics(RF_4xstar_positive, "RandomForest_2var")
      ModelsPredictions$"RandomForest_2var"<-MyPredictions(RF_4xstar_positive, "RandomForest_2var")
    
    #4xStart Reviews,x3Start Review andPositive Review Service  
      RF_4xstar_3xstar_positive <- train(Volume~x4StarReviews+PositiveServiceReview+x3StarReviews, data = TrainingSet, method = "rf", trControl=fitControl_RF, tuneLength = 4, importance=TRUE) #ntree = 50, do.trace = 10 #Train the model with a tuneLenght = 2 (trains with 2 different mtry values for RandomForest)
      RF_4xstar_3xstar_positive
      varImp(RF_4xstar_3xstar_positive)
      summary(RF_4xstar_3xstar_positive)
      sink("RF_4xstar_3xstart_positive.txt")
      print(summary(RF_4xstar_3xstar_positive))
      sink()
      ModelsMetrics<-MyMetrics(RF_4xstar_3xstar_positive, "RandomForest_3var")
      ModelsPredictions$"RandomForest_3var"<-MyPredictions(RF_4xstar_3xstar_positive, "RandomForest_2var")
      
    #All variables 
      RF_all <- train(Volume~., data = TrainingSet, method = "rf", trControl=fitControl_RF, tuneLength = 4, importance=TRUE) #ntree = 50, do.trace = 10 #Train the model with a tuneLenght = 2 (trains with 2 different mtry values for RandomForest)
      RF_all
      varImp(RF_all)
      summary(RF_all)
      sink("RF_all.txt")
      print(summary(RF_all))
      sink()
      ModelsMetrics<-MyMetrics(RF_all, "RandomForest_all")
      ModelsPredictions$"RandomForest_all"<-MyPredictions(RF_all, "RandomForest_all")
      
      
  #### ------ KNN (K-Nearest-Neighbor)-------
      #Details: https://rpubs.com/njvijay/16444
      #kNN requires variables to be normalized or scaled. caret provides facility to preprocess data. I am going to choose centring and scaling
      trainX <- TrainingSet[,names(TrainingSet) != "Volume"] #Takes all the variables excepcting the Volume
      preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
      preProcValues
      summary(preProcValues)
     
      set.seed(356)
      fitControl_kNN <- trainControl(method="repeatedcv", search = "random", repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
      #4xStarsReviews and Positive Reviews
       kNN_4xstar_positive <- train(Volume ~ x4StarReviews + PositiveServiceReview, data = TrainingSet, method = "knn", trControl = fitControl_kNN, preProcess = c("center","scale"), tuneLength = 20, importance=TRUE)
       kNN_4xstar_positive
       varImp(kNN_4xstar_positive)
       summary(kNN_4xstar_positive)
       sink("kNN_4xstar_positive.txt")
       print(summary(kNN_4xstar_positive))
       sink()
       ModelsMetrics<-MyMetrics(kNN_4xstar_positive, "kNN_2var")
       ModelsPredictions$"kNN_2var"<-MyPredictions(kNN_4xstar_positive, "kNN_2var")
       
       #4xStarsReviews, x3StarReview and Positive Reviews
       kNN_4xstar_3star_positive <- train(Volume ~ x4StarReviews + PositiveServiceReview + x3StarReviews, data = TrainingSet, method = "knn", trControl = fitControl_kNN, preProcess = c("center","scale"), tuneLength = 20, importance=TRUE)
       kNN_4xstar_3star_positive
       varImp(kNN_4xstar_3star_positive)
       summary(kNN_4xstar_3star_positive)
       sink("kNN_4xstar_3star_positive.txt")
       print(summary(kNN_4xstar_3star_positive))
       sink()
       ModelsMetrics<-MyMetrics(kNN_4xstar_3star_positive, "kNN_3var")
       ModelsPredictions$"kNN_3var"<-MyPredictions(kNN_4xstar_3star_positive, "kNN_3var")
       
       #All Attributes
       kNN_all <- train(Volume ~., data = TrainingSet, method = "knn", trControl = fitControl_kNN, preProcess = c("center","scale"), tuneLength = 20, importance=TRUE)
       kNN_all
       varImp(kNN_all)
       summary(kNN_all)
       sink("kNN_all.txt")
       print(summary(kNN_all))
       sink()
       ModelsMetrics<-MyMetrics(kNN_all, "kNN_all")
       ModelsPredictions$"kNN_all"<-MyPredictions(kNN_all, "kNN_all")
       
  #### ------ SVM (Support Vector Machine) -------  
       
       #4xStarsReviews and Positive Reviews
       
      set.seed(299)
      fitControl_SVM <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      SVMGrid1 <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
      
      SVM_x4star_positive <- train(Volume ~x4StarReviews+PositiveServiceReview, data = TrainingSet, method = "svmLinear", 
                         trControl = fitControl_SVM, preProcess = c("center", "scale"),
                         tuneGrid = SVMGrid1, tuneLength = 10, importance=TRUE)
      SVM_x4star_positive
      varImp(SVM_x4star_positive)
      sink("SVM_x4star_positive.txt")
      print(summary(SVM_x4star_positive))
      sink()
      ModelsMetrics<-MyMetrics(SVM_x4star_positive, "SVM_x4star_positive")
      ModelsPredictions$"SVM_x4star_positive"<-MyPredictions(SVM_x4star_positive, "SVM_x4star_positive")
     
      #4xStarsReviews,3xStard and Positive Reviews

      SVM_x4star_x3star_positive <- train(Volume ~x3StarReviews+x4StarReviews+PositiveServiceReview, data = TrainingSet, method = "svmLinear", 
                                   trControl = fitControl_SVM, preProcess = c("center", "scale"),
                                   tuneGrid = SVMGrid1, tuneLength = 10, importance=TRUE)
      SVM_x4star_x3star_positive
      varImp(SVM_x4star_x3star_positive)
      sink("SVM_x4star_x3star_positive.txt")
      print(summary(SVM_x4star_x3star_positive))
      sink()
      ModelsMetrics<-MyMetrics(SVM_x4star_x3star_positive, "SVM_x4star_x3star_positive")
      ModelsPredictions$"SVM_x4star_x3star_positive"<-MyPredictions(SVM_x4star_x3star_positive, "SVM_x4star_x3star_positive")
      
      
      #All attributes
      SVM_all <- train(Volume ~., data = TrainingSet, method = "svmLinear", 
                                          trControl = fitControl_SVM, preProcess = c("center", "scale"),
                                          tuneGrid = SVMGrid1, tuneLength = 10, importance=TRUE)
      SVM_all
      varImp(SVM_all)
      sink("SVM_all.txt")
      print(summary(SVM_all))
      sink()
      ModelsMetrics<-MyMetrics(SVM_all, "SVM_all")
      ModelsPredictions$"SVM_all"<-MyPredictions(SVM_all, "SVM_all")

  #### ----- Write the metrics and the predicitions ----
      write.csv(ModelsMetrics,"metrics.csv")
  #### ------ Visualizing errors in predictions between models ------
    #We create a dataframe with our testing set an all our predicted values.
      PredictedData<-cbind(ModelsPredictions,TestingSet$Volume)
      names(PredictedData)
      colnames(PredictedData)[13] <- "Testing.Volume"
      names(PredictedData)
      ggplot()+ 
        geom_smooth(PredictedData, mapping = aes(x = c(1:22), y =PredictedData$RandomForest_2var, color = "purple"),size=1.1, se=F)+
        geom_smooth(PredictedData, mapping = aes(x = c(1:22), y =PredictedData$RandomForest_3var, color = "black"),size=1.1, se=F)+
        geom_smooth(PredictedData, mapping = aes(x = c(1:22), y =PredictedData$kNN_2var, color = "green"),size=1.1, se=F)+
        geom_smooth(PredictedData, mapping = aes(x = c(1:22), y =PredictedData$SVM_x4star_positive, color = "blue"),size=1.1, se=F)+
        geom_smooth(PredictedData, mapping = aes(x = c(1:22), y =PredictedData$Testing.Volume, color = "red"), size=1.5, se=F)+
        scale_color_discrete(name = "Predictions", labels = c("RF_2variables","RF_3variables","kNN_2variables","SVM_2variables","Original Volume"))+xlab("Observations") + ylab("Volume of Sales") + ggtitle("Comparison of real&predicted Volumes of Sales")
     
     ggplot()+ 
       geom_smooth(PredictedData, mapping = aes(x =PredictedData$Testing.Volume , y =PredictedData$RandomForest_2var, color = "purple"),size=1, se=F)+
       geom_smooth(PredictedData, mapping = aes(x = PredictedData$Testing.Volume, y =PredictedData$RandomForest_3var, color = "black"),size=1.1, se=F)+
       geom_smooth(PredictedData, mapping = aes(x = PredictedData$Testing.Volume, y =PredictedData$kNN_2var, color = "green"),size=1.1, se=F)+
       geom_smooth(PredictedData, mapping = aes(x = PredictedData$Testing.Volume, y =PredictedData$SVM_x4star_positive, color = "blue"),size=1.1, se=F)+
       geom_smooth(PredictedData, mapping = aes(x = PredictedData$Testing.Volume, y =PredictedData$Testing.Volume, color = "red"), size=1.5, se=F)+
       scale_color_discrete(name = "Predictions", labels = c("RF_2variables","RF_3variables","kNN_2variables","SVM_2variables","Original Volume")) + xlab("Volume of Sales") + ylab("Volume of Sales") + ggtitle("Comparison of real&predicted Volumes of Sales")
     
     
#### ------- Prediction for new products ------
  #Be careful, because the new data needs to have AT LEAST the same attributes that the ones we used for the model
     NewProductAttributes_v1 <- read.csv("newproductattributes2017.csv", header = TRUE)
     newproducts_prediction1 <- predict(RF_4xstar_positive,NewProductAttributes_v1)
     newproducts_prediction1
  #Store data into dataframe
     NewProductAttributes_v1["PredictedVolume"] <- newproducts_prediction1
  #Save the final dataframe with the predicted values:
     write.csv(NewProductAttributes_v1, file="newproductspredictions_v1.csv", row.names = TRUE)
  
     newproducts_prediction2 <- predict(RF_4xstar_positive,TestingSet, interval="prediction")
     newproducts_prediction2
     str(newproducts_prediction2)
     
     
     
##### ------ FINAL VISUALIZATION TOUCH -------
     
  #Observed&Predicted Volume of Sales per Product Type 
    ggplot(new_and_existing_final,aes(x=ProductType,y=Volume, color = NewProduct)) + 
       geom_point(size=5)+ geom_jitter(size = 3, position = position_jitter(width = 0.1, height = 0.1)) + 
       labs(title = "Volume of Sales per category", x = "Product Type", y= "Volume Of Sales") +  theme(plot.title = element_text(hjust = 0.5))+ ggpubr::rotate_x_text()+
       scale_color_discrete(name = "realVSprediction", labels = c("KnownSales","PredictedSales"))
  
  #Predicted Volume of Sales per Product Type
     ggplot(newproductspredictions_v1,aes(x=ProductType,y=PredictedVolume))+
     geom_bar(stat = "identity", fill="steelblue")+
     theme(plot.title = element_text(hjust = 0.5))+ ggpubr::rotate_x_text()+
     labs(title = "Predicted Volume of Sales per Product Type", y="Predicted Volume of Sales")
  #Predicted&Exising Volume of Sales as a function of their most important predictors     
     
     ggplot(new_and_existing_final,aes(x=x4StarReviews,y=Volume, color = NewProduct)) + 
       geom_point(size=5)+ geom_jitter(size = 3, position = position_jitter(width = 0.1, height = 0.1)) + 
       labs(title = "Volume of Sales per x4 Stars Review", x = "x4 Stars Reviews", y= "Volume Of Sales") +  theme(plot.title = element_text(hjust = 0.5))+ ggpubr::rotate_x_text()+
       scale_color_discrete(name = "realVSprediction", labels = c("KnownSales","PredictedSales"))
     
     ggplot(new_and_existing_final,aes(x=PositiveServiceReview,y=Volume, color = NewProduct)) + 
       geom_point(size=5)+ geom_jitter(size = 3, position = position_jitter(width = 0.1, height = 0.1)) + 
       labs(title = "Volume of Sales per Positive Review", x = "Positive Reviews", y= "Volume Of Sales") +  theme(plot.title = element_text(hjust = 0.5))+ ggpubr::rotate_x_text()+
       scale_color_discrete(name = "realVSprediction", labels = c("KnownSales","PredictedSales"))
     
  #TOP5 Sellers
     newproductspredictions_v1<-newproductspredictions_v1 %>% arrange(desc(PredictedVolume))
     writenewproductspredictions_v1
     
     #Average Volumes per ProductType
      #ExistingProductsData %>% group_by(ProductType) %>% summarise(x=mean(Volume), sd=sd(Volume))
      #newproductspredictions_v1 %>% group_by(ProductType) %>% summarise(x=mean(PredictedVolume), sd=sd(PredictedVolume))
     
    #Median Volumes per ProductType
      #ExistingProductsData %>% group_by(ProductType) %>% summarise(x=median(Volume))
      #newproductspredictions_v1 %>% group_by(ProductType) %>% summarise(x=median(PredictedVolume))
      
#### ------ One Way Anova Test for Impact of Reviews by ProductType------
     
        #AnovaTest for Volume by ProductType
        ExistingProductsData %>% group_by(ProductType) %>% summarise(count = n(), mean = mean(PositiveServiceReview, na.rm = TRUE), sd = sd(PositiveServiceReview, na.rm = TRUE))
        ExistingProductsData %>% group_by(ProductType) %>% summarise(count = n(), mean = mean(NegativeServiceReview, na.rm = TRUE), sd = sd(NegativeServiceReview, na.rm = TRUE))   
        ggboxplot(ExistingProductsData, x = "ProductType", y = "Volume", color = "ProductType",ylab = "Volume", xlab = "Product")
        anova1 <- aov(PositiveServiceReview ~ ProductType, data = ExistingProductsData)
        summary(anova1)
        plot(TukeyHSD(anova1)) #Performing multiple pairwise-comparison between the means of groups.
      
        anova_1 <- aov(formula = Data$Volume ~ Data$ProductType)
        plot(TukeyHSD(res.aov))
        
      #AnovaTest for Volume by PositiveServiceReview
        ExistingProductsData %>% group_by(ProductType) %>% summarise(count = n(), mean = mean(PositiveServiceReview, na.rm = TRUE), sd = sd(PositiveServiceReview, na.rm = TRUE))
        ggboxplot(ExistingProductsData, x = "ProductType", y = "PositiveServiceReview", color = "ProductType",ylab = "Volume", xlab = "Product")
        anova2 <- aov(PositiveServiceReview ~ ProductType, data = ExistingProductsData)
        summary(anova2)
        plot(TukeyHSD(anova2)) #Performing multiple pairwise-comparison between the means of groups.
        
