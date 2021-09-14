library("mice")
library('VIM')
library('stringr')
library('dplyr')
library('zoo')
library('outliers')
working_folder <- '~/rstudio/scripts/ActiveIron/' # edit this folder to match your working folder
source(paste(working_folder,'functions.R',sep=''))
set.seed(500)

##### FUNCTIONS #####
get_t_test_data <- function(n, var1, var2) {
  test <- t.test(var1, var2, paired=TRUE)
  # variable name
  var_name <- paste(n,'_',toupper(substr(deparse(substitute(var1)),1,2)),'-',toupper(substr(deparse(substitute(var2)),1,2)),sep='')
  # t test results
  t_test_significant <- ifelse(test$p.value<0.025, 'significant', '')
  t_test_p_value <- round(test$p.value,4)
  t_test_test_statistic <- round(test$statistic[[1]],4)
  t_test_confidence_interval_lower <- round(test$conf.int[1],4)
  t_test_confidence_interval_upper <- round(test$conf.int[2],4)
  t_test_standard_error <- round(test$stderr,4)
  # Shapiro Wilks normality test
  diff <- var1 - var2
  shap.test <- shapiro.test(diff)
  normality <- ifelse(shap.test$p.value<0.05, 'Normal', '')
  shapiro_p.value <- round(shap.test$p.value,4)
  outliers = length(boxplot(diff)$out)
  # add values to vector
  test <- c(var_name, t_test_significant, t_test_p_value, t_test_test_statistic, t_test_confidence_interval_lower, t_test_confidence_interval_upper, t_test_standard_error, normality, shapiro_p.value, outliers)
  return(test)
}

get_t_test_report <- function(data) {
  # subset V2, V3, V4 column name list s
  v2 <- data[ , grepl('V2' , names(data) ) ]
  v3 <- data[ , grepl('V3' , names(data) ) ]
  v4 <- data[ , grepl('V4' , names(data) ) ]
  # remove V2, V3, V4 from column names
  names(v2) <- sort(gsub(x = names(v2), pattern = 'V2', replacement = ''))
  names(v3) <- sort(gsub(x = names(v3), pattern = 'V3', replacement = ''))
  names(v4) <- sort(gsub(x = names(v4), pattern = 'V4', replacement = ''))
  # create list of V2, V3, V4 lists values
  vx <- NULL
  var_all <- NULL
  for (n in names(v2)) {
    vx[[n]] <- list(v2[,n],v3[,n],v4[,n])
    var_all <- c(var_all,vx[n])  
  }
  # run t test for all variables and save to dataframe
  test_data <- data.frame()
  for (n in names(v2)) {
    v2_var <- unlist(var_all[n][[1]][1])
    v3_var <- unlist(var_all[n][[1]][2])
    v4_var <- unlist(var_all[n][[1]][3])
    # test differences between V2, V3
    test <- get_t_test_data(n, v2_var, v3_var)
    test_data <- rbind(test_data, as.data.frame(t(test)))
    # test differences between V3, V4
    test <- get_t_test_data(n, v3_var, v4_var)
    test_data <- rbind(test_data, as.data.frame(t(test)))
    # test differences between V2, V4
    test <- get_t_test_data(n, v2_var, v4_var)
    test_data <- rbind(test_data, as.data.frame(t(test)))
  }
  names(test_data)<-c('variable', 't test', 'p.value', 'statistic', 'conf.int1', 'conf.int2', 'stderr','Shapiro','p.value','outliers')
  paste(working_folder,'functions.R',sep='')
  write.csv(test_data, paste(working_folder,'/output/t.test_data.csv',sep=''), row.names=FALSE)
  return (test_data)
}
##### FUNCTIONS #####

##### MAIN #####
# select input file - full dataset
data <- read.csv(paste(working_folder,'/input/ActiveIronClean.csv',sep=''),na.strings=c('','NA'), stringsAsFactors=FALSE)
length(data)
str(data, list.len=ncol(data))
# missing values by %
NA_count <- sum(is.na(data))
NA_not_count <- sum(!is.na(data))
NA_percent <- round(NA_count/(NA_count+NA_not_count)*100,1)
print(paste('NAs found ::', NA_count,'out of',NA_count+NA_not_count,'-', NA_percent,'% of dataset'))
# missing values by row
ini <- mice(data, maxit = 0)
table(ini$nmis)

# pattern of missing data
md.pat <- md.pattern(data, plot=TRUE, rotate.names=TRUE)

# run MICE process on data
imp <- mice(data, m=10, maxit=10, meth='pmm')
completedData <- complete(imp,1)
write.csv(completedData, paste(working_folder,'/output/complete1.csv',sep=''), row.names=FALSE)
mice(methods)
summary(imp)
attributes(imp)
imp$data
length(imp$imp)
proc.time() - ptm # Stop the clock
completedData <- complete(imp,1)
write.csv(completedData, paste(working_folder,'/output/PARABLE_tempData.csv',sep=''), row.names=FALSE)
head(completedData)

data <- completedData
# drop missing columns 
drops <- names(data[ , grepl('Compliance|DaysSince|SelfRepHMP' , names(data) ) ])
data <- data[ , !(names(data) %in% drops)]
# get t test report
test_data <- get_t_test_report_active_iron(data)
test_data

