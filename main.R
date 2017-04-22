source("LoadLibrary.R")
source("Preprocessing.R")
source("Algorithm.R")
load.library()
set.seed(42)

############################################################################
#                           Load Data                                      #
############################################################################
german.data        <- read.csv("Data/german_credit.csv", header = T, sep = ",", na.strings = c("", " "))
target.variable.no <- 1

###########################################################################
#                    Features engineering                                 #
###########################################################################
german.data   <- missing.val.treatment(german.data, 0.5, target.variable.no)
german.data   <- cor.data(german.data, target.variable.no)

train.data  <- split_data(german.data, 1)
test.data   <- split_data(german.data, 0)

print(table(train.data$Creditability)) ## shows train.data is balance
###########################################################################
#                  Machine Learning Algorithm                             #
###########################################################################
model.class <- classifier(train.data, test.data)
write.csv(model.class, "output/error_metric.csv", row.names = F)

###########################################################################
#                  10-fold cross validation                               #
###########################################################################
k.fold.validation <- kFold_VAl_Calssifier(german.data)
write.csv(k.fold.validation, "output/k_fold_error_metric.csv", row.names = F)