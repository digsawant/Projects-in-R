##
# Code that predicts the probablity of customer who is going to churn based on their 
# orders, complaints, recency, frequency etc and numerous other variables.
##


churnR <- function(start_ind, end_ind, start_dep, end_dep, evaluate){
  main_time <- Sys.time()
  
  f <- "%Y-%m-%d"
  t1 <- as.Date(start_ind, f)
  t2 <- as.Date(end_ind, f)
  t3 <- as.Date(start_dep, f)
  t4 <- as.Date(end_dep, f) #dump date
  
 
  
  readAndPrepareData <- function(train = TRUE, ...){
    data_prep_time <- Sys.time()
    classes <- c(rep("character",4), rep("factor", 3))
    main_complaints <- fread("http://F://data//complaints.txt", header = TRUE, sep = ";", colClasses = classes)
    main_complaints$ComplaintDate <- as.Date(main_complaints$ComplaintDate, format = "%d/%m/%Y")
    
    classes <- c("character", "character","factor","character","factor","numeric","numeric")
    main_credit <- fread("http://F://data//credit.txt", header = TRUE, sep = ";", colClasses = classes)
    main_credit$ProcessingDate <- as.Date(main_credit$ProcessingDate, format = "%d/%m/%Y")
    
    classes <- c("character","factor","character","factor","character","character")
    customers <- fread("http://F://data//customers.txt", header = TRUE, sep = ";", colClasses = classes)
    customers$DOB <- as.Date(customers$DOB, format = "%d/%m/%Y")
    
    classes <- c("character","character","factor","factor","factor","character","character")
    main_delivery <- fread("http://F://data//delivery.txt", header = TRUE, sep = ";", colClasses = classes)
    main_delivery$StartDate <- as.Date(main_delivery$StartDate, format = "%d/%m/%Y")
    main_delivery$EndDate <- as.Date(main_delivery$EndDate, format = "%d/%m/%Y")
    
    classes <- c("character","character","factor","numeric")
    formula <- fread("http://F://data//formula.txt", header = TRUE, sep = ";", colClasses = classes)
    
    classes <- c(rep("character", 3),"factor",rep("character", 2),rep("numeric", 2),"character",rep("factor", 2),rep("character", 2),rep("numeric", 8))
    subscriptions <- fread("http://F://data//subscriptions.txt", header = TRUE, sep = ";", colClasses = classes)
    subscriptions$StartDate <- as.Date(subscriptions$StartDate, format = "%d/%m/%Y")
    subscriptions$EndDate <- as.Date(subscriptions$EndDate, format = "%d/%m/%Y")
    subscriptions$RenewalDate <- as.Date(subscriptions$RenewalDate, format = "%d/%m/%Y")
    subscriptions$PaymentDate <- as.Date(subscriptions$PaymentDate, format = "%d/%m/%Y")
    
    
    
    if(train == TRUE){
      #Selecting your customers
      #Getting your final people you wanna work with
      #Those who have a start_date < t2 and end_date > t3. Only those guys are included
      cust_date_list <- by(data = subscriptions, INDICES = list(subscriptions$CustomerID), function(x) x[which(x$StartDate < t2 & x$EndDate > t3), ])
    }else{
      cust_date_list <- by(data = subscriptions, INDICES = list(subscriptions$CustomerID), function(x) x[which(x$StartDate >= t1 & x$EndDate <= t4), ])
    }  
    
    
    unique_cust_id <- do.call(rbind, cust_date_list)
    final_subscriptions <- subscriptions[which(subscriptions$CustomerID %in% unique_cust_id$CustomerID),]
      
      #####Finding out churned guys
      #Case 1: t4 - final_endDate > 90
      churn_1 <- by(data = final_subscriptions, INDICES = list(final_subscriptions$CustomerID), function(x) x[which.max(x$EndDate), ])
      churn_1 <- do.call(rbind, churn_1)
      churn_1 <- churn_1[which((t4 - churn_1$EndDate) > 90), ]
      
      #Case 2: 
      #If the start_date of next subscription - end_date of current subscription is greater than 90
      #NextStartDate - CurrentEndDate > 90
      final_subscriptions$start_end_diff <- rep(0, nrow(final_subscriptions))
      final_subscriptions <- final_subscriptions[with(final_subscriptions, order(CustomerID, ProductID, StartDate)), ]
      
      for(i in 1:(nrow(final_subscriptions) - 1)){
        if((final_subscriptions$CustomerID[i] == final_subscriptions$CustomerID[i + 1]) & (final_subscriptions$ProductID[i] == final_subscriptions$ProductID[i + 1])){
          final_subscriptions$start_end_diff[i] <- final_subscriptions$StartDate[i + 1] - final_subscriptions$EndDate[i]
        }
      }
      
      churn_2 <- final_subscriptions[which(final_subscriptions$start_end_diff > 90 & is.na(final_subscriptions$RenewalDate)), ]
      
      #Final list of churned people
      final_churn <- append(unique(churn_1$CustomerID), unique(churn_2$CustomerID))
      
      #Adding churned/not churned to customers table
      #Also adding an indicator called is_deleted which tell us whether to include that guy in the analysis or not
      customers$churn <- rep(0, nrow(customers))
      customers$is_deleted <- rep(1, nrow(customers))
      if(train == FALSE){
        customers$is_deleted <- rep(0, nrow(customers))
      }
      
      for(i in 1:nrow(customers)){
        if(customers$CustomerID[i] %in% final_churn){
          customers$churn[i] <- 1
        }
        
        if(customers$CustomerID[i] %in% final_subscriptions$CustomerID){
          customers$is_deleted[i] <- 0
        }
      }
      
      
      #############################################Getting model variables from complaints
      complaints <- main_complaints[(main_complaints$ComplaintDate >= t1 & main_complaints$ComplaintDate <= t2), ]
      if(train == FALSE){
        complaints <- main_complaints[(main_complaints$ComplaintDate >= t1), ]
      }
      
      #Var 1 - number of complaints
      complaints$count <- rep(1, nrow(complaints))
      cust_comp_agg_total <- aggregate(complaints$count, by = list(CustomerID = complaints$CustomerID), sum)
      colnames(cust_comp_agg_total)[2] <- "Nbr_complaints"
      
      #Var 2 : Diff in min(subscription_start_date) and min(Complaint_date)
      cust_min_sub_date <- by(data = final_subscriptions, INDICES = list(final_subscri7ptions$CustomerID), function(x) x[which.min(x$StartDate), ])
      cust_min_sub_date <- do.call(rbind, cust_min_sub_date)
      cust_min_sub_date <- by(data = final_subscriptions, INDICES = list(final_subscriptions$CustomerID), function(x) x[which.min(x$StartDate), ])
      cust_min_sub_date <- do.call(rbind, cust_min_sub_date)
      
      cust_min_comp_date <- by(data = complaints, INDICES = list(complaints$CustomerID), function(x) x[which.min(x$ComplaintDate), ])
      cust_min_comp_date <- do.call(rbind, cust_min_comp_date)
      
      min_start_comp_date <- merge(cust_min_sub_date, cust_min_comp_date, by = 'CustomerID') 
      min_start_comp_date <- min_start_comp_date[, c('CustomerID', 'StartDate', 'ComplaintDate')]
      min_start_comp_date$diff_start_comp_date <- min_start_comp_date$ComplaintDate - min_start_comp_date$StartDate
      min_start_comp_date <- min_start_comp_date[, c("CustomerID", "diff_start_comp_date")]
      colnames(min_start_comp_date)[2] <- "min_start_comp_date"
      
      #Var 3 : Diff in t2 and max(Complaint_date)
      cust_max_comp_date <- by(data = complaints, INDICES = list(complaints$CustomerID), function(x) x[which.max(x$ComplaintDate), ])
      cust_max_comp_date <- do.call(rbind, cust_max_comp_date)
      max_end_comp_date <- cust_max_comp_date[, c('CustomerID', 'ComplaintDate')]
      max_end_comp_date$diff_start_comp_date <- t2 - max_end_comp_date$ComplaintDate
      max_end_comp_date <- max_end_comp_date[, c('CustomerID', 'diff_start_comp_date')]
      colnames(max_end_comp_date)[2] <- "max_end_comp_date" 
      
      #Var 4 : Dummy for Complaints Type, solution Type, FeedbackType
      complaints$ProductID <- as.factor(complaints$ProductID)
      dummy_comp_type <- complaints[,c("ProductID", "ComplaintType", "SolutionType", "FeedbackType")]
      dummy_comp_type <- dummy(dummy_comp_type)
      merge_comp_dummies <- cbind(complaints, dummy_comp_type)
      merge_comp_dummies <- merge_comp_dummies[, c(2, c(9:ncol(merge_comp_dummies))), with = FALSE]
      merge_comp_dummies <- as.data.frame(merge_comp_dummies)
      merge_comp_dummies[] <- lapply(merge_comp_dummies, function(x) type.convert(as.character(x)))
      merge_comp_dummies <- aggregate(. ~ CustomerID, merge_comp_dummies, sum)
      
      #Final variables from complaints
      datalist <- list(cust_comp_agg_total, min_start_comp_date, max_end_comp_date, merge_comp_dummies)
      final_complaint_variables <- Reduce(function(x, y) merge(x, y, by='CustomerID'),datalist)
      final_complaint_variables$min_start_comp_date <- as.numeric(final_complaint_variables$min_start_comp_date)
      final_complaint_variables$max_end_comp_date <- as.numeric(final_complaint_variables$max_end_comp_date)
      
      
      #######################################################Getting the model variables from Credit
      #Getting rows in between T1 and T2
      credit <- main_credit[which(main_credit$ProcessingDate >= t1 & main_credit$ProcessingDate <= t2), ]
      if(train == FALSE){
        credit <- main_credit
      }
      
      #Var 1: Amount/Number of credits that guy has on complaint
      credit_comp <- credit[which(credit$CreditSource == 'COM'), ]
      sub_credit_comp_amt <- merge(final_subscriptions, credit_comp, by = 'SubscriptionID', all.x = TRUE)
      sub_credit_comp_amt$Amount[is.na(sub_credit_comp_amt$Amount)] <- 0
      sub_credit_comp_amt <- aggregate(sub_credit_comp_amt$Amount, by = list(CustomerID = sub_credit_comp_amt$CustomerID), sum)
      colnames(sub_credit_comp_amt)[2] <- "Complaint_Credits"
      
      #Var 2: Amount of credits that guy has in total
      sub_credit_amt_tot <- merge(final_subscriptions, credit_comp, by = 'SubscriptionID', all.x = TRUE)
      sub_credit_amt_tot$Amount[is.na(sub_credit_amt_tot$Amount)] <- 0
      sub_credit_amt_tot <- aggregate(sub_credit_amt_tot$Amount, by = list(CustomerID = sub_credit_amt_tot$CustomerID), sum)
      colnames(sub_credit_amt_tot)[2] <- "Total_Credits"
      
      #Var 3: NbrNewspapers on credit
      NbrNewsPap_aggregate <- aggregate(credit$NbrNewspapers, by = list(SubscriptionID = credit$SubscriptionID), sum)
      colnames(NbrNewsPap_aggregate)[2] <- "NbrNewspapers_credit"
      NbrNewsPap_Subscriptions_Merge <- merge(final_subscriptions, NbrNewsPap_aggregate, by = 'SubscriptionID')
      NbrNewsPap_Subscriptions_Merge <- NbrNewsPap_Subscriptions_Merge[, c('CustomerID', 'NbrNewspapers_credit')]
      Cust_NbrNewsPapCredit <- aggregate(NbrNewsPap_Subscriptions_Merge$NbrNewspapers_credit, by = list(CustomerID = NbrNewsPap_Subscriptions_Merge$CustomerID), sum)
      colnames(Cust_NbrNewsPapCredit)[2] <- "NbrNewsPapCredit"
      
      #Final variables from complaints
      datalist <- list(sub_credit_comp_amt, sub_credit_amt_tot, Cust_NbrNewsPapCredit)
      final_credit_variables <- Reduce(function(x, y) merge(x, y, by='CustomerID'),datalist)
      
      ###################################################Getting variables from Delivery table
      #Var 1 : length of delivery
      delivery <- main_delivery[(main_delivery$StartDate >= t1 & main_delivery$StartDate <= t2),]
      if(train == FALSE){
        delivery <- main_delivery
      }
      
      delivery$length_delivery <- as.numeric(t2 - delivery$StartDate)
      length_dev_subscrip_agg <- aggregate(delivery$length_delivery, by = list(SubscriptionID = delivery$SubscriptionID), sum)
      colnames(length_dev_subscrip_agg)[2] <- "Length_delivery"
      length_dev_subscrip_agg <- merge(final_subscriptions, length_dev_subscrip_agg, by = 'SubscriptionID')
      length_dev_subscrip_agg <- length_dev_subscrip_agg[, c('CustomerID', 'Length_delivery')]
      final_delivery_variables <- aggregate(length_dev_subscrip_agg$Length_delivery,  by = list(CustomerID = length_dev_subscrip_agg$CustomerID), sum)
      colnames(final_delivery_variables)[2] <- "Length_delivery"
      
      
      ###################################################Getting variables from Customer table
      #Dummy. Just 2 varibles. Gender and District
      dummy_customers <- dummy(customers[,c("Gender", "District")])
      final_customer_variables <- cbind(customers[,-c(2, 4)], dummy_customers)
      
      
      ###################################################Getting variables from Subscriptions table
      indep_sub <- final_subscriptions[(final_subscriptions$StartDate >= t1 & final_subscriptions$StartDate <= t2),]
      if(train == FALSE){
        indep_sub <- final_subscriptions
      }
      
      dummy_subscriptions <- dummy(indep_sub[,c(4)])
      indep_sub <- cbind(indep_sub[,-c(4)], dummy_subscriptions)
      
      # Number of subscriptions 
      NBR_subscriptions <- aggregate(indep_sub[,"CustomerID",drop=FALSE],by=list(CustomerID=indep_sub$CustomerID),length)
      colnames(NBR_subscriptions)[2] <- "NBR_subscriptions"
      
      #recency of latest subscription start
      MAX_subscription_date <-  aggregate(indep_sub[,"StartDate",drop=FALSE],by=list(CustomerID=indep_sub$CustomerID),max)
      colnames(MAX_subscription_date)[2] <- "MAX_subscription_date"
      
      REC_subscriptions <-data.frame(CustomerID= MAX_subscription_date$CustomerID,
                                     REC_subscriptions = as.numeric(as.numeric(t2) - as.numeric(MAX_subscription_date$MAX_subscription_date)),
                                     stringsAsFactors=FALSE)
      
      # elapsed time since first subscription
      MIN_subscription_date <-  aggregate(indep_sub[,"StartDate",drop=FALSE], by=list(CustomerID=indep_sub$CustomerID),min)
      colnames(MIN_subscription_date)[2] <- "MIN_subscription_date"
      
      DUR_subscriptions <- data.frame(CustomerID=MIN_subscription_date$CustomerID, DUR_subscriptions= as.numeric(t2) -
                                        as.numeric(MIN_subscription_date$MIN_subscription_date),stringsAsFactors=FALSE)
      
      #aggregate averages in subscriptions table
      sub_aggregate <- aggregate(list(indep_sub$NbrNewspapers, indep_sub$NbrStart,
                                      indep_sub$GrossFormulaPrice, indep_sub$NetFormulaPrice,
                                      indep_sub$NetNewspaperPrice, indep_sub$ProductDiscount,
                                      indep_sub$FormulaDiscount, indep_sub$TotalDiscount,
                                      indep_sub$TotalPrice, indep_sub$TotalCredit), 
                                 by = list(indep_sub$CustomerID), mean)
      
      colnames(sub_aggregate) <- c("CustomerID","NbrNewspapers", "NbrStart", "GrossFormulaPrice", "NetFormulaPrice",
                                   "NetNewspaperPrice","ProductDiscount","FormulaDiscount", "TotalDiscount",
                                   "TotalPrice", "TotalCredit")
      
      #merge into aggregated subscriptions table
      sub_aggregate <- Reduce(function(...)
        merge(..., all = TRUE, by = "CustomerID"),
        list(sub_aggregate, DUR_subscriptions, REC_subscriptions, NBR_subscriptions))
      
      #aggregate sums in subscriptions table
      sub_aggregate_2 <- aggregate(list(as.numeric(as.character(indep_sub$Pattern_0000010)),
                                        as.numeric(as.character(indep_sub$Pattern_0000100)),
                                        as.numeric(as.character(indep_sub$Pattern_0001000)),
                                        as.numeric(as.character(indep_sub$Pattern_0010000)),
                                        as.numeric(as.character(indep_sub$Pattern_0100000)),
                                        as.numeric(as.character(indep_sub$Pattern_0100010)),
                                        as.numeric(as.character(indep_sub$Pattern_0101010)),
                                        as.numeric(as.character(indep_sub$Pattern_1000000)),
                                        as.numeric(as.character(indep_sub$Pattern_1000010)),
                                        as.numeric(as.character(indep_sub$Pattern_1001010)),
                                        as.numeric(as.character(indep_sub$Pattern_1111110))),
                                   by = list(indep_sub$CustomerID), sum)
      
      colnames(sub_aggregate_2) <- c("CustomerID", "Pattern_0000010", "Pattern_0000100", "Pattern_0001000", "Pattern_0010000",
                                     "Pattern_0100000", "Pattern_0100010", "Pattern_0101010", "Pattern_1000000",
                                     "Pattern_1000010", "Pattern_1001010", "Pattern_1111110")
      
      #merge averages and sums into one final aggregated subscriptions table
      final_subscription_variables <- merge(sub_aggregate,sub_aggregate_2, by = "CustomerID")
      
      ##Getting all the variables together to put in a model
      final_customers <- customers[which(customers$is_deleted == 0), c(1,7)]
      final_customers <- merge(final_customers, final_complaint_variables, all.x = TRUE)
      final_customers <- merge(final_customers, final_credit_variables, all.x = TRUE)
      final_customers <- merge(final_customers, final_delivery_variables, all.x = TRUE)
      final_customers <- merge(final_customers, final_customer_variables, all.x = TRUE)
      final_customers <- merge(final_customers, final_subscription_variables, all.x = TRUE)
      colnames(final_customers)[2] <- "Churn"
      final_customers$churn.y <- NULL
      final_customers$ZIP <- NULL
      final_customers$StreetID <- NULL
      final_customers$is_deleted <- NULL
      final_customers$DOB <- NULL
      final_customers[is.na(final_customers)] <- 0
      final_customers[, c(39:50)] <- lapply(final_customers[,c(39:50)], function(x) type.convert(as.character(x)))
      print(paste("Data Prepared in:", as.character(Sys.time() - data_prep_time)))
      return(final_customers)
  }
  
  final_customers <- readAndPrepareData()
  
  if(evaluate == TRUE){
      total_time <- 0
      print("Evaluating Model...")
      time <- Sys.time()
      ######## Building the model
      allind <- sample(x=1:nrow(final_customers),size=nrow(final_customers))
      #split in three parts
      trainind <- allind[1:round(length(allind)*(2/3))]
      testind <- allind[round(length(trainind)+1):length(allind)]
      
      BasetableTRAINbig <- final_customers[trainind,]
      BasetableTEST <- final_customers[testind,]
      
      BasetableTRAINbig_cust_ids <- BasetableTRAINbig$CustomerID
      BasetableTRAINbig$CustomerID <- NULL
      
      BasetableTEST_cust_ids <- BasetableTEST$CustomerID
      BasetableTEST$CustomerID <- NULL
      
      yTrainBig <- as.factor(BasetableTRAINbig$Churn)
      BasetableTRAINbig$Churn <- NULL
      
      yTest <- as.factor(BasetableTEST$Churn)
      BasetableTEST$Churn <- NULL 
      
      total_time <- total_time + (Sys.time() - time )
      print(paste("Done Building Data and Ready to Model in:", total_time ))
      
      print("Begin evaluating....")
      #######################################Trying Boosting   Accuracy: 90 - 93%
      print("Trying Boosting:")
      time <- Sys.time()
    
      ABmodel <- ada(yTrainBig ~ . , BasetableTRAINbig, iter=100)
      predAB <- as.numeric(predict(ABmodel, BasetableTEST, type="probs")[,2])
      boost_auc <- AUC::auc(roc(predAB,yTest))
      boost_time <- Sys.time() - time
      
      ########################################Try rotation forest   Accuracy:87-92%
      print("Trying Rotation Forest:")
      time <- Sys.time()
      
      RoF <- rotationForest(x=BasetableTRAINbig, y=yTrainBig, L=20)
      predRoF <- predict(RoF, BasetableTEST)
      rof_auc <- AUC::auc(roc(predRoF,yTest))
      rof_time <- Sys.time() - time
      
      ######################################## Trying Ensemble methods 90 - 91%
      print("Trying Ensemble")
      time <- Sys.time()
      
      ensemblesize <- 100
      ensembleoftrees <- vector(mode='list',length=ensemblesize)
      for (i in 1:ensemblesize){
        bootstrapsampleindicators <- sample.int(n=nrow(BasetableTRAINbig),
                                                size=nrow(BasetableTRAINbig),
                                                replace=TRUE)
        ensembleoftrees[[i]] <- rpart(yTrainBig[bootstrapsampleindicators] ~ .,
                                      BasetableTRAINbig[bootstrapsampleindicators,])
      }
      baggedpredictions <- data.frame(matrix(NA,ncol=ensemblesize,
                                             nrow=nrow(BasetableTEST)))
      for (i in 1:ensemblesize){
        baggedpredictions[,i] <- as.numeric(predict(ensembleoftrees[[i]],
                                                    BasetableTEST)[,2])
      }
      finalprediction <- rowMeans(baggedpredictions)
      ensemble_auc <- AUC::auc(roc(finalprediction,yTest))
      ensemble_time <- Sys.time() - time
      
      print("Printing all the AUCs...")
      print(paste("Boosting AUC:",boost_auc, "   Boosting Time Taken:", boost_time))
      print(paste("ROF AUC:",rof_auc, "   RoF Time Taken:", rof_time))
      print(paste("Ensemble AUC:",ensemble_auc,  "   Ensemble Time Taken:", ensemble_time))
      
      l <- list(ABmodel, readAndPrepareData)
      print(paste("Data Prepared and evaluated in:", as.character(Sys.time() - main_time)))
      return(l)
  }else{
    cust_ids <- final_customers$CustomerID
    yTrain <- as.factor(final_customers$Churn)
    final_customers$CustomerID <- NULL
    final_customers$Churn <- NULL
    
    model <- ada(yTrain ~ . , final_customers, iter=100) 
    l <- list(model, readAndPrepareData)
    print(paste("Data Prepared but not evaluated in:", as.character(Sys.time() - main_time)))
    return(l)
  }
}


churnModel <- churnR("2006-01-02", "2009-12-01", "2009-12-02", "2011-03-02", FALSE)

predictChurn <- function(object, dumpDate){
  main_time <- Sys.time()
  #Load all required packages
  for (i in c("AUC","lift","randomForest", "dummy","randomForest", "glmnet", "ada", "rotationForest", "rpart", "data.table")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i,
                       repos='http://cran.rstudio.com',
                       quiet=TRUE)
      require(i,
              character.only=TRUE,
              quietly=TRUE)
    }
  }
  
  #Reading and preparind data with train = FALSE parameter  
  readAndPrepareData <- object[[2]]
  basetable <- readAndPrepareData(FALSE)
  
  #Building final database of predictions of churn
  cust_preds <- data.frame(basetable$CustomerID)
  basetable$CustomerID <- NULL
  basetable$Churn <- NULL
  
  
  model <- object[[1]]
  
  preds <- predict(model, basetable, type="probs")[,2]
  
  cust_preds$Predictions <- preds
  cust_preds <- cust_preds[with(cust_preds, order(-cust_preds$Predictions)), ]
  colnames(cust_preds)[1] <- "CustomerID"
  rownames(cust_preds) <- c()
  print(head(cust_preds))
  print(paste("Predict function runs in:", as.character(Sys.time() - main_time)))
}

pred <- predictChurn(object=churnModel, dumpDate="2011-03-02")