# function to classify the german credit data using different ML algorithm, like decison tree, logistic regression,
#, svm, Neural network, KNN etc
#,
# @paraam train.data A data_frame containing train set on which model is prepared
# @param test.data A data_frame containing test ste on which model is prepared
#,
# @return result.frame A data_frame containing different metric like TP, FP, Sensitivity, Specificity AUC etc of different model
classifier <- function(train.data, test.data)
{
        result.frame  <- data.frame(model.name = as.character(), TP=numeric(), FN=numeric(), FP=numeric(),
                                    TN= numeric(),SENSITIVITY=numeric(),SPECITICITY=numeric(),
                                    ACCURACY=numeric(),AUC=numeric())
        
        tree.model <- rpart(Creditability~., data = train.data, method = "class")
        result.frame <- rbind(result.frame, predict.auc("decison.tree", tree.model, test.data))
        
        random.model <- randomForest(as.factor(Creditability)~., data = train.data, ntree = 500, mtry = 4,importance = TRUE, 
                                     replace = T)
        result.frame <- rbind(result.frame, predict.auc("random.forest", random.model, test.data))
        
        logit.model <- glm(Creditability~., data = train.data, family = binomial(link = "logit"))
        result.frame <- rbind(result.frame, predict.auc("logistic.regrr", logit.model, test.data, 1))
        
        neural.model <- neuralNetwork(train.data, test.data)
        result.frame <- rbind(result.frame, neural.model)
        
        svm.model <- ksvm(as.factor(Creditability)~., data = train.data, scaled = TRUE, prob.model = T)
        result.frame <- rbind(result.frame, predict.auc("svm", svm.model, test.data, 3))
        
        knn.model <- knn(train.data, test.data, as.factor(train.data$Creditability), k = 5)
        result.frame <- rbind(result.frame, predict.auc("knn.model", knn.model, test.data, 4))
        
        return(result.frame)
}
#############################################################################################################################
# function to classify the german credit data using different ML algorithm, like decison tree, logistic regression
#, svm, Neural network, KNN etc using 10-fold validation
#,
# @param total.data A data_frame containing data set on which model preparation and prediction to be done
#,
# @return result.frame A data_frame containing different metric like TP, FP, Sensitivity, Specificity AUC etc of different 
#, model using 10-fold validation techniques and result of all metric shows as average of all fold
kFold_VAl_Calssifier <-  function(total.data)
{
        total.data[["Creditability"]] <- as.factor(total.data[["Creditability"]])
        pr <- partition.cv.strat(data=total.data, strat = "Creditability", nfold = 10, repetition=1)
        
        result.frame  <- data.frame(model.name = as.character(), TP=numeric(), FN=numeric(), FP=numeric(),
                                    TN= numeric(),SENSITIVITY=numeric(),SPECITICITY=numeric(),
                                    ACCURACY=numeric(),AUC=numeric())
        
        for (k in 1:10) 
        {
              print(paste("iteration.no = ", k))
              train.Indx <- pr[["1"]][[k]]$train
              test.Indx  <- pr[["1"]][[k]]$test
              
              train.data <- total.data[train.Indx, ]
              test.data  <- total.data[test.Indx,  ]
              
              result.frame <- rbind(result.frame, classifier(train.data, test.data))
        }
        result.frame <- result.frame %>% group_by(model.name) %>%
                                          summarise(TP = mean(TP),
                                                    FN = mean(FN),
                                                    FP = mean(FP),
                                                    TN = mean(TN),
                                                    sensitivity = mean(SENSITIVITY),
                                                    specificity = mean(SPECITICITY),
                                                    accuracy = mean(ACCURACY),
                                                    auc = mean(AUC))
        return(result.frame)
}
#############################################################################################################################
# Function to predict the value of test data
# @param model.name A String indicate the ML algorithm name
# @param model A ML model 
# @param test.data A data_frame on which prediction to be done
# @param algo.flag A flag indicate which prediction of which ML algo to be done, 0-decison tree, random Forest, 1-logistic reg
#, 2-Neural network, 3-svm, 4-knn, 
#,
# @return result A data_frame conating prediction.metric of test data
predict.auc <- function(model.name, model, test.data, algo.flag = 0)
{
        target.attr <- ncol(test.data) -1
        if(algo.flag == 1)
        {
          pred  <- predict(model, test.data, type = "response")
          pred <- as.vector(ifelse(pred > 0.5, 1, 0))
          
        }else if(algo.flag == 2)
        {
          pred <- compute(model, test.data[,1:target.attr])
          pred <- as.factor(round(pred$net.result))
          
        }else if(algo.flag == 3)
        {
          pred  <- predict(model, test.data)
        }else if(algo.flag == 4)
        {
            pred <- model
        }else
        {
          pred  <- predict(model, test.data, type = "class")
        }
       print(model.name)
        confMatrix <- confusionMatrix(test.data$Creditability, pred)
    
        TP <- confMatrix$table[[4]]
        FN <- confMatrix$table[[3]]
        TN <- confMatrix$table[[1]]
        FP <- confMatrix$table[[2]]
        
        SENSITIVITY <- confMatrix$byClass[[1]]
        SPECITICITY <- confMatrix$byClass[[2]]
        ACCURACY    <- confMatrix$overall[[1]]
        AUC         <- (SENSITIVITY + SPECITICITY)/2
        
        result <- data.frame(model.name, TP, FN, FP, TN, SENSITIVITY, SPECITICITY, ACCURACY, AUC)
        return(result)
}
############################################################################################################################
# Separate Function of Neural network, where special preprocessing respective to NN to be done like scaling, then prepare a 
#, NN model over that and finally prediction on test data to be done
#,
# @param train.data A data_frame containing train set
# @param test.data A data_frame containing test set
#,
# @return  neural.metric A data_frame containg Prediction.metric on test data
neuralNetwork  <- function(train.data, test.data)
{
     total.data <- rbind(train.data, test.data)
     
     maxs <- apply(total.data, 2, max)
     mins <- apply(total.data, 2, min)
     scaled <- as.data.frame(scale(total.data, center = mins, scale = maxs - mins))
     
     train.data <- split_data(total.data, 1)
     test.data  <- split_data(total.data, 0)
     
     cName <- names(train.data)
     formula <- as.formula(paste("Creditability~", paste(cName[!cName %in% "Creditability"], collapse = " + ")))
     
     neural.model <- neuralnet(formula, data = train.data, hidden = c(5, 3), linear.output = FALSE, stepmax = 100000000)
     neural.metric <- predict.auc("neural.network",  neural.model, test.data, 2)
     
     return(neural.metric)
}
  