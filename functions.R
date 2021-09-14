library('stringr')
library('dplyr')
library('zoo')
#### FUNCTIONS #####

covariates <- c('TREATMENT_ALLOCATION', 'AGE_BASELINE', 'GENDER', 'BL_TG', 'BL_LDLC', 'BL_SBP', 'BL_DBP', 'BL_PULSE', 'MACECOUNT_POST', 'CAD_BL', 'ANGINA_BL', 'MI_BL', 'IHD_BL', 'HPT_BL', 'DYSLIPIDEMIA_BL', 'DIABETES_BL', 'STROKE_BL', 'TIA_BL', 'ANTIARRTHYMIC_BL', 'ALPHABLOCKER_BL', 'BETABLOCKER_BL', 'CCB_BL', 'ALDOSTERONE_ANTAG_BL', 'STATIN_BL', 'OTHER_DYSLIPID_BL', 'THIAZIDE_BL', 'LOOP_BL', 'OTHER_DIURETIC_BL', 'ANTIPLATELET_EXCL_ASP_BL', 'ASPIRIN_BL', 'ANTICOAG_EXCL_WARFARIN_BL', 'WARFARIN_BL', 'INSULIN_BL', 'METANTIDIABETIC_BL', 'SUREAANTIDIABETIC_BL', 'DPP4ANTIDIABETIC_BL', 'GLPANTIDIABETIC_BL', 'SGLT2ANTIDIABETIC_BL', 'BMI_BL')
baseline <- c('BL_NTPROBNP','BL_BNP', 'BL_ABPM_SYSTOLIC_24H', 'BL_ABPM_DIASTOLIC_24H', 'BL_ABPM_PULSEPRESSURE_24H', 'BL_ABPM_HEARTRATE_24H', 'ECHO_LAVI_BL', 'JDMRI_LAVI_MAX_BL', 'ECHO_E_Ep_AVG_BL')

subset_LAVI_and_BNP_data <- function(data) {
        print(paste('Dataset dimensions (rows x cols):', nrow(data),'x ',ncol(data)))
        print('Reducing full dataset to covariates and NTPROBNP & LAVI endpoints and measurements...')
        all_variables <- c('TREATMENT_ALLOCATION', 'AGE_BASELINE', 'GENDER', 'BL_TG', 'BL_LDLC', 'MACECOUNT_POST', 'CAD_BL', 'ANGINA_BL', 'MI_BL', 'IHD_BL', 'HPT_BL', 'DYSLIPIDEMIA_BL', 'DIABETES_BL', 'STROKE_BL', 'TIA_BL', 'ANTIARRTHYMIC_BL', 'ALPHABLOCKER_BL', 'BETABLOCKER_BL', 'CCB_BL', 'ALDOSTERONE_ANTAG_BL', 'STATIN_BL', 'OTHER_DYSLIPID_BL', 'THIAZIDE_BL', 'LOOP_BL', 'OTHER_DIURETIC_BL', 'ANTIPLATELET_EXCL_ASP_BL', 'ASPIRIN_BL', 'ANTICOAG_EXCL_WARFARIN_BL', 'WARFARIN_BL', 'INSULIN_BL', 'METANTIDIABETIC_BL', 'SUREAANTIDIABETIC_BL', 'DPP4ANTIDIABETIC_BL', 'GLPANTIDIABETIC_BL', 'SGLT2ANTIDIABETIC_BL', 'BMI_BL', 'BL_SBP', 'BL_DBP', 'BL_PULSE', 'JDMRI_LAVI_MAX_BL', 'JDMRI_LAVImax_18M', 'BL_NTPROBNP', 'X3M_NTPROBNP', 'X6M_NTPROBNP', 'X9M_NTPROBNP', 'X12M_NTPROBNP', 'X15M_NTPROBNP', 'X18M_NTPROBNP', 'BL_BNP', 'X3M_BNP', 'X6M_BNP', 'X9M_BNP', 'X12M_BNP', 'X15M_BNP', 'X18M_BNP', 'BNPchg9M', 'BNPchg18M', 'BL_ABPM_SYSTOLIC_24H', 'BL_ABPM_DIASTOLIC_24H', 'BL_ABPM_PULSEPRESSURE_24H', 'BL_ABPM_HEARTRATE_24H', 'X9M_ABPM_SYSTOLIC_24H', 'X9M_ABPM_DIASTOLIC_24H', 'X9M_ABPM_PULSEPRESSURE_24H', 'X9M_ABPM_HEARTRATE_24H', 'X18M_ABPM_SYSTOLIC_24H', 'X18M_ABPM_DIASTOLIC_24H', 'X18M_ABPM_PULSEPRESSURE_24H', 'X18M_ABPM_HEARTRATE_24H', 'ECHO_LAVI_BL', 'ECHO_E_LAT_Ep_BL', 'ECHO_LAVI_9M', 'ECHO_E_Ep_AVG_9M', 'ECHO_LAVI_18M', 'ECHO_E_Ep_AVG_18M', 'CHGLVEF18M', 'CHGLVMI18M', 'ChangeJDMRI_LA_EF_18M', 'CHGLASVI')
         data <- data[,names(data) %in% all_variables] 
        print(paste('Dataset dimensions (rows x cols):', nrow(data),'x',ncol(data)))
 return(data)
}

clean_data <- function(data) {
 print('Cleaning data values...')
 # convert text to lower
 data <- data.frame(lapply(data, tolower))
 # remove leading or trailing white space
 data <- data.frame(apply(data, 2, function (x) gsub('^\\s+|\\s+$','',x)))
 # replace unknown labels with'NA'
 replacements <- c('not done','#value!','unk','not measured','none','-999','na','NA')
 for (r in replacements) {data <- data.frame(lapply(data, function(x) gsub(r, NA, x)))}
 # clean'N/A' values
 data <- data.frame(lapply(data, function(x) gsub('^n/a$', 0, x)))
 # AGAIN remove leading or trailing white space
 data <- data.frame(apply(data, 2, function (x) gsub('^\\s+|\\s+$','',x)))
 # convert text to numeric - no free text fields, saved as factor
 data <- as.data.frame(lapply(data, function(x) if(any(grepl('^[[:digit:]]+\\.*[[:digit:]]*$',x))) as.numeric(as.character(x)) else x))
 # convert NA text to true NA 
 make.true.NA <- function(x) {is.na(x) <- x=='NA'; x}
 data <- data.frame(lapply(data, make.true.NA))
 print(paste('Dataset dimensions (rows x cols):', nrow(data),'x',ncol(data)))
 return (data) 
}

drop_excess_NA_features <- function(data, col_threshold, row_threshold, keep_endpoints) {
 print('Removing features with excess NAs...')
 endpoints_LAVI <- names(data[,grep('JDMRI_LAVI_MAX',names(data))])
 endpoints_NTPROBNP <- names(data[,grep('NTPROBNP',names(data))])
 endpoints <- c(endpoints_LAVI, endpoints_NTPROBNP)
 # remove cols with > X% missing 
 col_missing <- unlist(sapply(data, function(x) (sum(is.na(x))/length(x)*100)))
 all_cols <- ncol(data)
 drop_cols <- names(data[col_missing>col_threshold])
 drop_cols_some <- drop_cols[!drop_cols %in% endpoints]
 if (keep_endpoints==TRUE) {
 data <- data[!names(data) %in% drop_cols_some]
 sub_cols <- ncol(data)} 
 else {
 data <- data[!names(data) %in% drop_cols]
 sub_cols <- ncol(data)}
 cols_removed <- all_cols - sub_cols
 print(paste('Dropped', cols_removed,'columns with >',col_threshold,'% missing values :'))
 print(cols_removed)
 # remove rows with > X% missing 
 row_missing <- apply(data, 1, function(x) round((sum(is.na(x))/(sum(is.na(x))+sum(!is.na(x))))*100,0))
 all_rows <- nrow(data)
 rows_removed <- as.numeric(rownames(data[row_missing > row_threshold,]))
 data <- data[row_missing < row_threshold,]
 sub_rows <- nrow(data)
 print(paste('Dropped', all_rows-sub_rows,'observations with >',row_threshold,'% missing values :'))
 print(rows_removed)
 print(paste('Dataset dimensions (rows x cols):', nrow(data),'x',ncol(data)))
 return(data)
}

drop_implied_features <- function(data) {
 print('Removing features with implied values* :')
 implied_features <- c('BNPchg9M', 'BNPchg18M', 'CHGLASVI', 'CHGLVEF18M', 'CHGLVMI18M', 'ChangeJDMRI_LA_EF_18M')
 print(implied_features)
 print('*Tested BNPchg9M = X9M_BNP - BL_BNP (note 13/250 discrepancies)')
 data <- data[,!names(data) %in% implied_features]
 print(paste('Dataset dimensions (rows x cols):', nrow(data),'x',ncol(data)))
 return(data)
}

rename_features <- function(data) {
 # rename columns with'.' to'_'
 print('Renaming features with . in column name : . to _')
 names(data) <- gsub(x = names(data), pattern = '\\.', replacement = '_')
 # rename features with time suffix - standardise naming convention
 print('Renaming features with time suffix - standardise naming convention')
 names(data)[names(data) == 'ECHO_LAVI_BL'] <- 'BL_ECHO_LAVI'
 names(data)[names(data) == 'ECHO_E_Ep_AVG_BL'] <- 'BL_ECHO_E_Ep_AVG'
 names(data)[names(data) == 'ECHO_LAVI_9M'] <- 'X9M_ECHO_LAVI'
 names(data)[names(data) == 'ECHO_E_Ep_AVG_9M'] <- 'X9M_ECHO_E_Ep_AVG'
 names(data)[names(data) == 'ECHO_LAVI_18M'] <- 'X18M_ECHO_LAVI'
 names(data)[names(data) == 'ECHO_E_Ep_AVG_18M'] <- 'X18M_ECHO_E_Ep_AVG'
 names(data)[names(data) == 'JDMRI_LAVI_MAX_BL'] <- 'BL_JDMRI_LAVI_MAX'
 names(data)[names(data) == 'JDMRI_LAVImax_18M'] <- 'X18M_JDMRI_LAVI_MAX' # JDMRI_LAVImax_18M has lowercase aswell as suffix
 print(paste('Dataset dimensions (rows x cols):', nrow(data),'x',ncol(data)))
 return(data)
}

get_endpoints_LAVI <- function(cols) {
 endpoints_LAVI <- sort(c(cols[grep('JDMRI_LAVI_MAX',cols)]))
 #print(paste('Endpoints LAVI_MAX x',length(endpoints_LAVI),':',toString(endpoints_LAVI)))
 return(endpoints_LAVI)
}
get_endpoints_NTPROBNP <- function(cols) {
 endpoints_NTPROBNP <- sort(c(cols[grep('NTPROBNP',cols)]))
 #print(paste('Endpoints NTPROBNP x',length(endpoints_NTPROBNP),':',toString(endpoints_NTPROBNP)))
 return(endpoints_NTPROBNP)
}
get_final_endpoints <- function(cols) {
 final_endpoints <- sort(c(endpoints[grep('18M',endpoints)]))
 #print(paste('Endpoints - Final x',length(final_endpoints),':',toString(final_endpoints)))
 return(final_endpoints)
 }
get_measures_LAVI <- function(cols) {
 measures_LAVI <- sort(c(cols[grep('ECHO',cols)]))
 #print(paste('Measures LAVI_MAX x',length(measures_LAVI),':',toString(measures_LAVI)))
 return(measures_LAVI)
 }
get_measures_NTPROBNP <- function(cols) {
 measures_NTPROBNP <- sort(c(cols[grep('_BNP|_ABPM',cols)]))
 #print(paste('Measures BNP, ABPM x',length(measures_NTPROBNP),':',toString(measures_NTPROBNP)))
 return(measures_NTPROBNP)
 }
get_endpoints_and_measures <- function(cols) {
 endpoints_and_measures <- sort(c(get_endpoints_LAVI(cols), get_endpoints_NTPROBNP(cols), get_measures_LAVI(cols), get_measures_NTPROBNP(cols)))
 #print(paste('Endpoints and Measures x',length(endpoints_and_measures),':',toString(endpoints_and_measures)))
 return(endpoints_and_measures)
 }
get_covariates <- function(cols) {
 covariates <- sort(c(cols[!cols %in% endpoints_and_measures]))
 #print(paste('Covariates x',length(covariates),':',toString(covariates)))
 return(covariates)
 }

IsOutlier <- function(data_tmp) {
 lowerq = quantile(data_tmp, na.rm = TRUE)[2]
 upperq = quantile(data_tmp, na.rm = TRUE)[4]
 iqr = upperq - lowerq 
 threshold_upper = (iqr * 1.5 * 2) + upperq
 threshold_lower = lowerq - (iqr * 1.5 * 2)
 data_tmp > threshold_upper | data_tmp < threshold_lower 
}

remove_outliers <- function(data_tmp, threshold) {
 data_factors <- cbind(data_tmp[1],data_tmp[3])
 data_tmp <- cbind(data_tmp[2],data_tmp[4:length(data_tmp)])
 row_outliers <- as.numeric(rownames(data_tmp[rowSums(sapply(data_tmp, IsOutlier), na.rm = TRUE) > threshold,]))
 data_tmp <- cbind(data_factors,data_tmp)
 data_tmp <- data_tmp[-row_outliers, ]
 return(data_tmp)
} 

get_t_test_data <- function(n, var1, var2) {
 test <- t.test(var1, var2, paired=TRUE)
 # variable name
 var_name <- paste(n,'_',toupper(substr(deparse(substitute(var1)),1,2)),'-',toupper(substr(deparse(substitute(var2)),1,2)),sep='')
 # t test results
 t_test_significant <- ifelse(test$p.value<0.025,'significant','')
 t_test_p_value <- round(test$p.value,4)
 t_test_test_statistic <- round(test$statistic[[1]],4)
 t_test_confidence_interval_lower <- round(test$conf.int[1],4)
 t_test_confidence_interval_upper <- round(test$conf.int[2],4)
 t_test_standard_error <- round(test$stderr,4)
 # Shapiro Wilks normality test
 diff <- var1 - var2
 shap.test <- shapiro.test(diff)
 normality <- ifelse(shap.test$p.value<0.05,'Normal','')
 shapiro_p.value <- round(shap.test$p.value,4)
 outliers = length(boxplot(diff)$out)
 # add values to vector
 test <- c(var_name, t_test_significant, t_test_p_value, t_test_test_statistic, t_test_confidence_interval_lower, t_test_confidence_interval_upper, t_test_standard_error, normality, shapiro_p.value, outliers)
 return(test)
}

get_t_test_report_active_iron <- function(data) {
 # subset V2, V3, V4 column name list s
 v2 <- data[ , grepl('V2' , names(data) ) ]
 v3 <- data[ , grepl('V3' , names(data) ) ]
 v4 <- data[ , grepl('V4' , names(data) ) ]
 # remove V2, V3, V4 from column names
 names(v2) <- sort(gsub(x = names(v2), pattern ='V2', replacement =''))
 names(v3) <- sort(gsub(x = names(v3), pattern ='V3', replacement =''))
 names(v4) <- sort(gsub(x = names(v4), pattern ='V4', replacement =''))
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
 names(test_data)<-c('variable','t test','p.value','statistic','conf.int1','conf.int2','stderr','Shapiro','p.value','outliers')
 write.csv(completedData, paste(working_folder,'/output/t.test_data.csv',sep=''), row.names=FALSE)
 return (test_data)
}

locf_update_data <- function(data) {
 # Last observation carried forward (LOCF)
 # create new dataframe with measures
 print('Creating new subset dataframe with measures and endpoints only')
 all_cols <- names(data)
 endpoints <- c(get_endpoints_LAVI(all_cols),get_endpoints_NTPROBNP(all_cols))
 measures <- c(get_measures_LAVI(all_cols),get_measures_NTPROBNP(all_cols))
 data_locf <- data[, c(measures, endpoints)] # subset time repeating group columns
 # count NAs before LOCF
 before <- sum(is.na(data_locf))
 # create a copy to compare results
 write.csv(data_locf,'~/rstudio/scripts/HFVC/output/data_locf.before.csv', row.names=FALSE)
 # create list of distinct variables measured
 print('Found following groups')
 groups <- c(unique(str_split(names(data_locf),'_', n=2, simplify=T)[,2]))
 print(toString(groups))
 # remove groups with only 1 element - na.locf function will fail
 for (group in groups[1:length(groups)]) {
         if (length(grep(group, names(data_locf))) < 2) {groups = groups[!(groups %in% group)]}
 }
 # loop through each group in groups list, apply na.locf function to each column in data_locf
 data_locf.new <- data.frame(matrix(0, ncol = 0, nrow = 250)) # create empty df
 print(paste('Processing groups...'))
 for (group in groups[1:length(groups)]) {
         # subset the data_locf frame by indexing on the cols_locf columns (3rd column) matching the group and save to temp dataframe
         group_cols <- grep(group, names(data_locf))
         print(paste(group,':', toString(names(data_locf[,group_cols]))))
         data_locf.tmp <- data_locf[, group_cols]
         # apply the na.locf function to temp dataframe
         data_locf.tmp <- data.frame(t(apply(data_locf.tmp, 1, function(x) na.locf(x, fromLast = F, na.rm = F))))
         data_locf.new <- cbind(data_locf.new, data_locf.tmp)
         }
 # count NAs after LOCF
 after <- sum(is.na(data_locf.new))
 # calculate % difference
 reduction <- round((before-after)/before*100,1)
 # display result
 print(paste('NAs:: Before:',before,'After:',after,'Reduction(%):',reduction))
 write.csv(data_locf.new,'~/rstudio/scripts/HFVC/output/data.new.locf.csv', row.names=FALSE)
 return(data_locf.new)
}

find_regressors <- function(imp, x, y) {
        done=FALSE
        print(paste('Running step wise regression on',y,'...'))
        while (done==FALSE) {
                fit <- with(imp1, lm(as.formula(paste(y,'~',paste(x,collapse =' +')))))
                pool_fit <- pool(fit) # pool coefficients and standard errors across all regression models
                summary <- summary(pool(fit))
                if (summary[1,][1] =='(Intercept)'){intercept <- summary[1,]}
                max_p_line <- summary[summary$p.value == max(summary$p.value), ]
                max_p_var <- toString(max_p_line[,'term'])
                if (max_p_var=='TREATMENT_ALLOCATIONb') max_p_var='TREATMENT_ALLOCATION' # factor error handling
                if (max_p_var=='GENDERmale') max_p_var='GENDER' # factor error handling
                max_p_val <- toString(max_p_line[,'p.value'])
                if (max_p_val > 0.05) {x <- x[!x %in% max_p_var]}
                else {done=TRUE}
        }
        return(summary)
}

get_features <- function(reg_data, data) {
        test_features_df <- data.frame(reg_data[1])
        test_features_df <- data.frame(lapply(test_features_df, as.character), stringsAsFactors=FALSE) # save model features
        names(test_features_df)[1] <- 'features'
        coefficients <- reg_data[2] # save model coefficients
        test_features_df <- cbind(test_features_df, coefficients)
        cols <- c(as.character(unlist(test_features_df[1])))
        means <- colMeans(data[,cols],na.rm=TRUE)
        test_features_df <- cbind(test_features_df, means)
        names(test_features_df)[3] <- 'mean'
        rownames(test_features_df)<- c()
        test_features_df <- cbind(test_features_df, test_features_df[,2]*test_features_df[,3])
        names(test_features_df)[4] <- 'product'
        test_features_df <- cbind(test_features_df, rank(abs(test_features_df[,4])))
        names(test_features_df)[5] <- 'rank'
        test_features_df <- test_features_df[order(test_features_df[,5]),]
        return(test_features_df)
}

apply_regression <- function(test_features, coefficients, feature, data, pop_feat) {
        if (pop_feat == TRUE & length(test_features)>2) {
                print(paste('Popped',head(test_features,1)))
                test_features <- tail(test_features,-1)
                print(head(test_features,-1))}
        test_features <- c(test_features, feature)
        print(test_features)
        temp_df <- data[test_features] # subset master dataset by features
        if ('TREATMENT_ALLOCATION' %in% names(temp_df)) { # convert factor to numeric if exists
                temp_df[,'TREATMENT_ALLOCATION'] <- as.numeric(temp_df[,'TREATMENT_ALLOCATION'])} 
        if ('GENDER' %in% names(temp_df)) { # convert factor to numeric if exists
                temp_df[,'GENDER'] <- as.numeric(temp_df[,'GENDER'])} 
        before <- sum(is.na(temp_df[,ncol(temp_df)])) # count NAs before imputation value replacement
        
        # subset rows with NA in column of interest & no NA in all others
        NA_found <- is.na(temp_df[,ncol(temp_df)]) & !is.na(rowSums(temp_df[, -ncol(temp_df)]))
        
        
        if (dim(temp_df[which(NA_found),])[1]!=0) {
                # temp_df[which(NA_found),][ncol(temp_df)] <- coefficients[1] # save 1st coefficient (intercept) value
                # loop through all features (other features, not last feature)
                for (i in 1:(length(test_features)-1)){
                        # save product (feature coefficient x feature value) to test df subset
                        x <- coefficients[i]*temp_df[which(NA_found),][,i]
                        if (any(NA_found) == TRUE) {temp_df[which(NA_found),][ncol(temp_df)] <- temp_df[which(NA_found),][ncol(temp_df)] + x
                        }}}

        
        # count NAs after imputated value replacement
        after <- sum(is.na(temp_df[,ncol(temp_df)])) 
        
        if (dim(temp_df[which(NA_found),])[1]!=0) {
                print(paste('Inner :', feature,'updated - replaced',before-after,'values'))
                data[,feature] <- temp_df[,feature] } # update NAs with imputations by replacing feature column
        else {print(paste('Inner :', feature,'not updated - No NAs found'))}
        
        return(data)
}

load_clean_data <- function(data){
        data <- subset_LAVI_and_BNP_data(data)
        data <- clean_data(data)
        data <- drop_implied_features(data)
        data <- rename_features(data)
        features_NA <- names(data[,sapply(data, function(x) sum(is.na(x))>0)])
        mice_rejected <- c('OTHER_DIURETIC_BL', 'X9M_ABPM_SYSTOLIC_24H')
        features_NA <- sort(features_NA[!features_NA %in% mice_rejected])
        return(data)
}

get_analysis_vars <- function(){
        # feature variables - xsheet columns in red text manually copied to create vector
        analysis_vars <- c('DoseGroupNo','Age','Ethnicity','SmokingCurrent','AlcoholUnitsPerWeek','IronSc','UIBCSc','TIBCSc','TSATSc','FerritinSc','WCCSc','RCCSc','HbSc','HCTSc','MCVSc','MCHSc','MCHCSc','RDWSc','PlatSc','NeutSc','LymphSc','MonoSc','EosinoSc','BasoSc','WgtSc','HgtSc','BMISc','TempSc','SBPSc','DBPSc','HRSc','GSRSScoreSc','DaysSinceSc','TempV2','SBPV2','DBPV2','HRV2','WgtV2','BMIV2','IronV2','UIBCV2','TIBCV2','TSATV2','FerritinV2','WCCV2','RCCV2','HbV2','HCTV2','MCVV2','MCHV2','MCHCV2','RDWV2','PlatV2','NeutV2','LymphV2','MonoV2','EosinoV2','BasoV2','9SFFulloflifeV2','9SFEnergyV2','9SFWornOutV2','9SFTiredV2','GSRSOtherProds','GSRSScoreV2','SelfRepHMPV2','HMPLength','HMPsoakthrough','HMPdoubleprotection','HMPLargeClots','HMPInterfereReglifestyle','HMPPeriodPain','HMPEasyBruising','HMPMedications','DaysSinceV2','TempV3','SBPV3','DBPV3','HRV3','WgtV3','BMIV3','IronV3','UIBCV3','TIBCV3','TSATV3','FerritinV3','WCCV3','RCCV3','HbV3','HCTV3','MCVV3','MCHV3','MCHCV3','RDWV3','PlatV3','NeutV3','LymphV3','MonoV3','EosinoV3','BasoV3','9SFFulloflifeV3','9SFEnergyV3','9SFWornOutV3','9SFTiredV3','GSRSOtherProdsCheck','GSRSScoreV3','ComplianceV3','DaysSinceV3','TempV4','SBPV4','DBPV4','HRV4','WgtV4','BMIV4','IronV4','UIBCV4','TIBCV4','TSATV4','FerritinV4','WCCV4','RCCV4','HbV4','HCTV4','MCVV4','MCHV4','MCHCV4','RDWV4','PlatV4','NeutV4','LymphV4','MonoV4','EosinoV4','BasoV4','9SFFulloflifeV4','9SFEnergyV4','9SFWornOutV4','9SFTiredV4','GSRSScoreV4','ComplianceV4')
        
        # Sc does not have X9SFEnergy, X9SFFulloflife, X9SFTired, X9SFWornOut
        placeholders <- c('9SFEnergySc','9SFFulloflifeSc','9SFTiredSc','9SFWornOutSc')
        analysis_vars <- c(placeholders, analysis_vars)
        analysis_vars <- sort(analysis_vars)
        
        # remove SC endpoints
        #endpoints_Sc <- analysis_vars[grep('Sc$',analysis_vars)]
        #analysis_vars <- analysis_vars[!analysis_vars %in% endpoints_Sc]
        # remove 'SelfRepHMPV2' - missing 'SelfRepHMPV3', 'SelfRepHMPV4'
        # remove 'ComplianceV3','ComplianceV4' - missing 'ComplianceV2'
        # remove 'DaysSinceV2','DaysSinceV3' - missing 'DaysSinceV4'
        # remove 'Ethnicity' - no variance
        dump_vars <- c('SelfRepHMPV2','ComplianceV3','ComplianceV4','DaysSinceSc','DaysSinceV2','DaysSinceV3','Ethnicity','HgtSc')
        analysis_vars <- analysis_vars[!analysis_vars %in% dump_vars]
        # column with digit first character imported with X prefix - replace column names with digit prefix with X
        analysis_vars <- gsub('9SF', 'X9SF', analysis_vars)
        
        return(analysis_vars)
}

data_ai_subset <- function(data_test, analysis_vars){
        # duplicate column heading SmokingCurrent in xsheet imported as SmokingCurrent...21 and SmokingCurrent...22
        # change SmokingCurrent...21 to SmokingCurrent
        names(data_test)[grep('Smoking',names(data_test))][1] <- 'SmokingCurrent'
        # subset only analysis_vars
        data_test <- data_test[names(data_test) %in% analysis_vars]
        # remove last two rows - 'DoseGroupNo' at H62 --> adds two empty rows 
        data_test <- data_test[1:(nrow(data_test)-2),] # basic text cleaning
        # transform data
        data_test <- clean_data(data_test) # basic text cleaning
        
        return(data_test)
}

create_df_endpoints <- function(endpoints){
        # view endpoints by time series V2,V3,V4
        df_end <- data.frame()
        df_end <- rbind(df_end, data.frame(endpoints[grep('V2$',endpoints)]))
        df_end <- gsub('V2$','',df_end[,1])
        df_end <- cbind(df_end, data.frame(endpoints[grep('V2$',endpoints)]))
        df_end <- cbind(df_end, data.frame(endpoints[grep('V3$',endpoints)]))
        df_end <- cbind(df_end, data.frame(endpoints[grep('V4$',endpoints)]))
        df_end <- cbind(df_end, data.frame(endpoints[grep('Sc$',endpoints)]))
        names(df_end) <- c('variable','V2','V3','V4','Sc')
        
        return(df_end)
}

get_imp_results <- function(data_test, m, maxit){
        treatment <- 'DoseGroupNo'
        analysis_vars <- sort(get_analysis_vars())
        endpoints <- analysis_vars[grep('Sc$|V2$|V3$|V4$',analysis_vars)]
        placeholders <- c('X9SFEnergy','X9SFFulloflife','X9SFTired','X9SFWornOut')
        covariates <- setdiff(analysis_vars, c(endpoints,treatment))
        df_end <- create_df_endpoints(endpoints)
        
        # create imputations for each measure for each V3-V2, V4-V3, V4-V2 visit time difference
        vars <- df_end[,1] # variable names
        times_df <- data.frame(); var_df <- data.frame(); results <- data.frame() # output dfs to record statistics from pooled regressions
        times <- c('V3 - V2','V4 - V3','V4 - V2')
        plots <- list(); j <- 1
        for (i in 1:3) {
                for (var in vars) {
                        # select all Vx variables
                        vars_Vx <- as.character(unlist(df_end[df_end$variable == var,][2:5]))
                        if (var %in% placeholders) {vars_Vx <- vars_Vx[-4]}
                        # select all imp variables
                        imp_vars <- c(vars_Vx, treatment, covariates)
                        # subset data using imp vars
                        data_test_imp <- data_test[imp_vars]
                        # execute MICE - create imputations
                        imp <- mice(data_test_imp, seed=500, m=m, maxit=maxit, meth='pmm')
                        # pool coefficients and standard errors across all regression models
                        x <- c(vars_Vx[1], treatment, covariates)
                        if (!var %in% placeholders) {x <- c(x, vars_Vx[4])}
                        # select y vars for time series 'V3 - V2','V4 - V3','V4 - V2', eg WgtV3 and WgtV2
                        if (i == 1) {y <- vars_Vx[c(2,1)]} else if (i == 2) {y <- vars_Vx[c(3,2)]} else {y <- vars_Vx[c(3,1)]}
                        # fit regression
                        fit <- with(imp, lm(as.formula(paste(paste(y,collapse =' -'),'~',paste(x,collapse =' +')))))
                        # pool results
                        pool <- pool(fit)
                        # generate CIs
                        conf <- summary(pool, conf.int = TRUE, conf.level=0.95) 
                        # save time difference 'V3 - V2' x n times to match number of cols in conf statistical report df
                        times_df <- rbind(times_df, data.frame(rep(times[i],nrow(conf))))
                        # save variable x n times to match number of cols in conf statistical report df
                        var_df <- rbind(var_df, data.frame(rep(var,nrow(conf))))
                        # save conf statistical report df
                        results <- rbind(results, data.frame(conf))
                        # save plot
                        p <- stripplot(imp, as.formula(paste(vars_Vx[i],'~.imp',sep='')), pch=20)
                        file_name = paste(working_folder,'/output/',vars_Vx[i],'_plot_', j, '.png', sep='')
                        png(file_name)
                        print(p)
                        dev.off()
                        j <- j + 1
                }}
        # combine time difference 'V3 - V2' x n times to variable x n times
        var_df <- cbind(times_df, var_df)
        # combine both to conf statistical report df
        results <- cbind(var_df, results)
        # rename variables
        names(results)[c(1,2,9,10)] <- c('time diff.','variable', 'ci 2.5%', 'ci 97.5%')
        write.csv(results, paste(working_folder,'/output/Results.xlsx',sep=''), row.names=FALSE)
        
        return(results)
}

