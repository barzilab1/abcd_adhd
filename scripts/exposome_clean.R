library(readr)

source("config.R")


###School Risk and Protective Factors
srpf01 = read.csv(file = paste0(exposome_files_path,"srpf01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
srpf01 = srpf01[-1,]

#rename visit as eventname
srpf01$eventname = srpf01$visit

#remove columns introduced by NDA and alienation items 
srpf01 = srpf01[srpf01$eventname == "baseline_year_1_arm_1",  !(colnames(srpf01) %in% c("srpf01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey","study_cohort_name", "dataset_id","visit","school_15_y", "school_17_y"))]

srpf01 = droplevels(srpf01)
summary(srpf01)


#school_environment_sum
srpf01$school_environment_sum = apply(srpf01[,grepl("(_2|4|5|6|7|10)", colnames(srpf01))], 1, function(r) sum(as.numeric(as.character(r))))

#positive_school_involvement_sum
srpf01$positive_school_involvement_sum = apply(srpf01[,grepl("(3|8|9|12)", colnames(srpf01))], 1, function(r) sum(as.numeric(as.character(r))))

#school_protective_factors
srpf01$school_protective_factors = srpf01$positive_school_involvement_sum + srpf01$school_environment_sum

summary(srpf01)



###Family Environment Scale: Family Conflict Subscale
sscey01 = read.csv(file = paste0(exposome_files_path,"abcd_sscey01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
sscey01 = sscey01[-1,]

sscey01 = sscey01[sscey01$eventname == "baseline_year_1_arm_1", grepl("(src|interview|event|sex)|(fes_y_ss_fc)$", colnames(sscey01))]

sscey01 = droplevels(sscey01)
table(sscey01$fes_y_ss_fc)



###Parental Monitoring Survey
pmq01 = read.csv(file = paste0(exposome_files_path,"pmq01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
pmq01 = pmq01[-1,]

#remove columns introduced by NDA
pmq01 = pmq01[,!(names(pmq01) %in% c("pmq01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]

pmq01 = droplevels(pmq01)
summary(pmq01)

#mean the 5 questions
pmq01$parent_monitor_mean = apply(pmq01[,grepl("_y$", colnames(pmq01))], 1, function(r) mean(as.numeric(as.character(r))))



###Screen Time Youth
ssmty = read.csv(file = paste0(exposome_files_path,"abcd_ssmty01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
ssmty = ssmty[-1,]

ssmty = ssmty[ssmty$eventname == "baseline_year_1_arm_1", grepl("(src|interview|event|sex)|(stq_y_ss_weekend)$", colnames(ssmty))]

ssmty = droplevels(ssmty)
table(ssmty$stq_y_ss_weekend)



###family relationship section
acspsw03 = read.csv(file = paste0(exposome_files_path,"acspsw03.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
acspsw03 = acspsw03[-1,]

acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1", grepl("^(src|interview|event|sex)|(rel_family_id)$", colnames(acspsw03))]

acspsw03 = droplevels(acspsw03)
summary(acspsw03)



### merge all tables

exposome_set = merge(srpf01,sscey01)
exposome_set = merge(exposome_set,pmq01)
exposome_set = merge(exposome_set,ssmty)
exposome_set = merge(exposome_set,acspsw03)

write.csv(file = "outputs/exposome_set.csv",x = exposome_set, row.names = F, na = "")




