########################Function to divide data into train and test #################################
#,
# @param in.data A data_frame containing whole data set
# @param type_data A binary flag 1 indicate train type, 0 indicate test_type
#,
# @return train.data/test.data depending type_data = 1/0 respectively
split_data <- function(in.data, type_data)
{
      in.data    <- in.data[sample(nrow(in.data)), ]
      data.index <- createDataPartition(in.data$Creditability, p = 0.8, list = FALSE, times = 1)
      
      train.data <- in.data[data.index, ]
      test.data  <- in.data[-data.index, ]
      
      if(type_data == 1)
      {
            return(train.data)
      }else
      {
            return(test.data)
      }
}

############################# remove correlated variable #########################################
#,
# @param in.data A Data_frame containing data 
# @param target.variable A target column.no
#,
# @return out.data A data frame in which all correlated column removed
cor.data <- function(in.data, target.variable)
{
      response.data  <- in.data[, -c(target.variable)]
      target.data   <- as.data.frame(in.data[, c(target.variable)])
      
      cor.data      <- cor(response.data)
      cor.var       <- findCorrelation(cor.data, cutoff = 0.75)
      
      if(length(cor.var) > 0)
      {
            response.data <- response.data[, -c(cor.var)]
            print(paste("no of varible remove from original data= ", cor.var))
      }
      
      out.data      <- cbind(response.data, target.data)
      return(in.data)
}
############################ Missing value replacement with mean ###################################
#,
# @param total.data A data_frame containing whole data set
# @param threshold A numeric constant define above which any attribute contains missing value % remove
# @param target.variable A column define the target attributes
#,
# @return out.data A data_frame after treating missing value in original data set
missing.val.treatment <- function(total.data, threshold, target.variable)
{
      in.data            <- total.data[, -c(target.variable)]
      missing.val.cal    <- data.frame(missing.value.per = apply(in.data, 2, function(col.data){(sum(is.na(col.data))/length(col.data))*100}),
                                    row.names = NULL)
      
      attribute.miss.per <- cbind(attribute.name = colnames(in.data), missing.val.cal)
      remove.attribute   <- as.character(subset(attribute.miss.per, missing.val.cal >= threshold, select = c(attribute.name)))
      
      if(length(remove.attribute) > 1)
      {
          in.data <- in.data[, -c(remove.attribute)]
      }
      
      in.data <- as.data.frame(lapply(in.data, function(col.data)ifelse(is.na(col.data), mean(col.data, na.rm = TRUE), col.data)))
      in.data <- cbind(in.data, data.frame(Creditability = total.data$Creditability))
      
      out.data <- in.data
      return(out.data)
}