library("mice")
library("readxl")
library('ppcor')
library('ggplot2')
library('lares')
library('magrittr')
working_folder <- '~/rstudio/scripts/Solvitron/' # edit this folder to match your working folder
source(paste(working_folder,'functions.R',sep=''))

##### ETL #####

# variable selection/grouping
analysis_vars <- get_analysis_vars() # list from xsheet with various edits
# import and transform data
data <- data.frame(read_xlsx(paste(working_folder,'/input/WorkingFileWomenForMICE_26.5.21.xlsx',sep='')))
data_test <- data
dim(data_test) # rows x cols
data_test <- data_ai_subset(data_test, analysis_vars) # clean data, various edits

##### EXPLORE #####

# inspect dataframe
str(data_test, list.len=ncol(data_test)) # data stucture - object (eg dataframe), dimensions, variable types (eg factor, numeric, character, etc)
md.pat <- md.pattern(data_test, plot=TRUE, rotate.names=TRUE)
md.pat <- md.pattern(data_test[34:104], plot=TRUE, rotate.names=TRUE)
# count NAs
sapply(data_test, function(x) sum(is.na(x)))
# influx/outflux
fx <- flux(data_test, names(data_test))
plot(fx$influx, fx$outflux, xlim=c(0,1), ylim=c(0,1), abline(1,-1))
outlist1 <- row.names(fx)[fx$outflux < 0.5] # predictive power limited - these variables could be removed if they are not of scientific interest
outlist1


##### IMPUTATIONS #####
source(paste(working_folder,'functions.R',sep=''))
analysis_vars <- get_analysis_vars()
results <- get_imp_results(data_test, m=50, maxit=10)
imp$loggedEvents # warnings with Wgt and BMI
