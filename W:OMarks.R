library(corrplot)
library(keras)
library(dplyr)
library(rlist)

#Model ANN without Marks columns
modelANNnoMarks <- function(data, i)
{
  #Marks features are removed for NoMarksModel
  data <- ShuffleData(data)
  data <- select(data, -(user_id))
  data <- select(data, -(average_score))
  data <- select(data, -(total_score))
  data <- select(data, -(max_score))
  data <- select(data, -(min_score))
  #data <- select(data,-(competency_code))
  data <-select(data,-(total_num_attempts))
  data <-
    select(data,
           status,
           competency_code,
           collection_type,
           everything())
  
  #Status and Collection_Type are converted to numbers to feed into model.
  data[, 1] <- as.numeric(data[, 1]) -1
  data[, 2] <- as.numeric(factor(data[, 2]))
  data[, 3] <- as.numeric(data[, 3]) -1
  print (data)
  #Correlation between features is plotted
 # plotCor(data)
  #dimnames(data) <- NULL
  
  data_original <- data
  
  #Data is normalized before feeding to model
  data <- normalize(as.matrix(data[, 1:8]))
  
  #Data is sampled randomly into training and test data
  set.seed(12345)
  ind <- sample(2,
                nrow(data),
                replace = TRUE,
                prob = c(0.8, 0.2))
  data.training <- data[ind == 1, 2:8]
  data.test <- data[ind == 2, 2:8]
  data.trainingtarget <- data_original[ind == 1, 1]
  data.testtarget <- data_original[ind == 2, 1]
  #print (data.testtarget)
  #One Hot Encoding of Testing and Training target values
  data.trainLabels <- to_categorical(data.trainingtarget)
  data.testLabels <- to_categorical(data.testtarget)
  #print (data.testLabels)

  #Initializing and compiling keras model
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 20,
                activation = 'relu',
                input_shape = c(7)) %>%
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dropout(rate = 0.1) %>%
    # layer_dense(units = 10, activation = 'relu') %>%
    # layer_dropout(rate = 0.1) %>%
    layer_dense(units = 2, activation = 'softmax')

  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(lr = 0.0001 , decay = 1e-6),
    metrics = 'accuracy'
  )
  summary(model)
  get_config(model)
  get_layer(model, index = 1)

  model$layers
  model$inputs
  model$outputs

  #Loading model and weights
  model <-
    load_model_hdf5(
      paste(getwd(), "/Models/",i,"_Model_NoMarks_NoTotalAttempts.h5", sep = ""),
      custom_objects = NULL,
      compile = TRUE
    )
  load_model_weights_hdf5(
    model,
    paste(getwd(), "/Models/",i,"_ModelWeights_NoMarks_NoTotalAttempts.h5", sep = ""),
    by_name = FALSE,
    skip_mismatch = FALSE,
    reshape = FALSE
  )

  #Fitting model to train data
  # history <- model %>% fit(
  #   as.matrix(data.training),
  #   data.trainLabels,
  #   epochs = 30,
  #   batch_size = 16,
  #   callbacks = callback_tensorboard("logs/run_b"),
  #   validation_split = 0.1,
  #   verbose = 2,
  #   shuffle = TRUE
  # )
  # save_model_hdf5(
  #   model,
  #   paste(getwd(), "/Models/", i, "_Model_NoMarks_NoAvgTime.h5", sep = ""),
  #   overwrite = TRUE,
  #   include_optimizer = TRUE
  # )
  # save_model_weights_hdf5(
  #   model,
  #   paste(getwd(), "/Models/", i, "_ModelWeights_NoMarks_NoAvgTime.h5", sep = ""),
  #   overwrite = TRUE
  # )
  #Model training is visualized on Tensorboard
  #plotTraining(history)
  
  #Model is tested with test data
  classes <-
    model %>% predict_classes(data.test, batch_size = 16, verbose = 1)
  table(data.testtarget, classes)
  #print ("DATA.TEST")
  #print (data.test)
  #print ("DATA.TESTLABELS")
  #print (data.testLabels)
  score <- model %>% evaluate(data.test,
                              data.testLabels,
                              batch_size = 16,
                              verbose = 1)
  
  #Status prediction accuracy
  print(score)
  
  
  #Separating correct and wrong predictions
  # classes <- data.frame(classes)
  # colnames(classes) <- "status_predicted"
  # classes <- cbind(classes,data_original[ind ==2,])
  # classes$match <- xor(classes$status,classes$status_predicted)
  # right <- classes[classes$match == FALSE,2:10]
  # wrong <- classes[classes$match == TRUE,2:10]
  # write.csv(right,file=paste("right",toString(i),".csv",sep=""))
  # write.csv(wrong,file=paste("wrong",toString(i),".csv",sep=""))
  #print (right)
  return(score)
  
}

#Plot Corelation
plotCor <- function(data)
{
  
  #Correlation of all features
  M <- stats::cor(as.matrix(data))
  corrplot::corrplot(M, method = "circle")
}


#Shuffle Dataset
ShuffleData <- function(all_data_f)
{
  set.seed(12345)
  all_data_f <- all_data_f[sample(nrow(all_data_f)),]
  return(all_data_f)
}

path <- paste(getwd(), "/Data/", sep = "")
data_all <- read.csv(paste(path, "fixed_data_comp.csv", sep = ""))

#Read Subject-wise Data
MA <- read.csv(paste(path, "Subjects/MA.csv", sep = ""))
SS <- read.csv(paste(path, "Subjects/SS.csv", sep = ""))
SC <- read.csv(paste(path, "Subjects/SC.csv", sep = ""))
ELA <- read.csv(paste(path, "Subjects/ELA.csv", sep = ""))

#Read Domain-wise data
ELA_RL <- read.csv(paste(path, "Domains/ELA_RL.csv", sep = ""))
MA_CED <- read.csv(paste(path, "Domains/MA_CED.csv", sep = ""))
SS_CPI <- read.csv(paste(path, "Domains/SS_CPI.csv", sep = ""))
MA_OAT <- read.csv(paste(path, "Domains/MA_OAT.csv", sep = ""))
SS_HCC <- read.csv(paste(path, "Domains/SS_HCC.csv", sep = ""))



data_all_right<- read.csv("right0.csv")
data_all_wrong <- read.csv("wrong0.csv")
MA_right<- read.csv("right1.csv")
MA_wrong <- read.csv("wrong1.csv")
SS_right<- read.csv("right2.csv")
SS_wrong <- read.csv("wrong2.csv")
SC_right<- read.csv("right3.csv")
SC_wrong <- read.csv("wrong3.csv")
ELA_right<- read.csv("right4.csv")
ELA_wrong <- read.csv("wrong4.csv")
#data_list <- list(data_all, MA, SS, SC,ELA)
#dat <-c("data_all","MA", "SS","SC","ELA")

data_list<- list(data_all)
 dat<-c("data_all")


# data_list <- list(data_all_right,data_all_wrong, MA_right,MA_wrong, SS_right,SS_wrong, SC_right,SC_wrong,ELA_right,ELA_wrong)
# dat <-c("data_all_right","data_all_wrong","MA_right","MA_wrong", "SS_right","SS_wrong","SC_right","SC_wrong","ELA_right","ELA_wrong")




#acc_marks = c()
acc_Nomarks = c()
#loss_marks = c()
loss_Nomarks = c()

i = 0
for (dataw in data_list)
{
  data <- dataw
  s <- modelANNnoMarks(data, i)
  acc_Nomarks <- append(acc_Nomarks, s$acc)
  loss_Nomarks <- append(loss_Nomarks, s$loss)
 # i = i + 1
}

performance <-
  data.frame(dat, acc_Nomarks, loss_Nomarks)
View(performance)
