############## Import data and load packages ##############  

set.seed(112)

# clear workspace
rm(list = ls())

# set workspace directory 
setwd("C:/Users/StephanieCampbell/Dropbox/ShockGameAnalysis")


# install packages
library(data.table)
library(foreign)
library(lme4)
library(ggplot2)
library(doBy)
library(cowplot)
library(gridExtra)
library("ggExtra")
library("psych")
library("plyr")
library('corrplot')
library(lmerTest)
library(lsmeans)
library(multcomp)
library(ggsignif)
library(optimx)
library(HH)
library(gmodels)
library(effsize)
library(sjPlot)
library(sjmisc)
library(memisc)
library("PerformanceAnalytics")
library(RODBC)
library(robustlmm)
library(Hmisc)
library(WRS2)
library(robust)
library(ordinal)
library(ggpubr)

# import data from csv file  
data <- utils::read.csv("shock_game_data.csv", header = TRUE, na.strings = c("","NA"))
subinfo <- utils::read.csv("shock_game_subinfo.csv", header = TRUE, na.strings = c("","NA"))

############## Prepare data ##############  

# rename MTurk IDs that have been incorrectly entered
data$subid <- base::trimws(data$subid, which = "both")
subinfo$subid <- base::trimws(subinfo$subid, which = "both")

# fix MTurk IDs entered in strange format
subinfo$subid <- plyr::mapvalues(subinfo$subid, from = "\\t\\t\\n A1MTRZDRKNB9GV", to = "A1MTRZDRKNB9GV")
data$subid <- plyr::mapvalues(data$subid, from = "\\t\\t\\n A1MTRZDRKNB9GV", to = "A1MTRZDRKNB9GV")

# convert from dataframe foramt to datatable format
data <- data.table::setDT(data)
subinfo <- data.table::setDT(subinfo)

# convert class of subids to character 
data$subid <- as.character(data$subid)
subinfo$subid <- as.character(subinfo$subid)

# remove character strings that are not MTurkIDs
data <- data[startsWith(subid, "A")]
subinfo <- subinfo[startsWith(subid, "A")]

# convert class of subids back to original factor
data$subid <- as.factor(data$subid)
subinfo$subid <- as.factor(subinfo$subid)

# fix MTurk IDs entered in strange format
data$subid <- plyr::mapvalues(data$subid, from = c("A14T2FRYXEUPAO\\n", "AGFWV1U4CJXP5\\n\\n\\n\\n", "A3PPDYVZ2LQZ1E\\n", "A2OWFD7J4MRGKM\\n", "A3QDZ935V5U31J\\n", "AQ4UDCIBEFO01\\n", "A1KJE75LL545EI\\n","A20W01MAJ3M9AC\\nA20W01MAJ3M9AC\\nA20W01MAJ3M9AC", "A38TZ2TQW9TVBUÃ¯Æ'â???¦"), to = c("A14T2FRYXEUPAO", "AGFWV1U4CJXP5", "A3PPDYVZ2LQZ1E", "A2OWFD7J4MRGKM", "A3QDZ935V5U31J", "AQ4UDCIBEFO01", "A1KJE75LL545EI", "A20W01MAJ3M9AC","A38TZ2TQW9TVBU"))
 
subinfo$subid <- plyr::mapvalues(subinfo$subid, from = c("A14T2FRYXEUPAO\\n", "AGFWV1U4CJXP5\\n\\n\\n\\n", "A3PPDYVZ2LQZ1E\\n", "A2OWFD7J4MRGKM\\n", "A3QDZ935V5U31J\\n", "AQ4UDCIBEFO01\\n", "A1KJE75LL545EI\\n","A20W01MAJ3M9AC\\nA20W01MAJ3M9AC\\nA20W01MAJ3M9AC", "A38TZ2TQW9TVBUÃ¯Æ'â???¦"), to = c("A14T2FRYXEUPAO", "AGFWV1U4CJXP5", "A3PPDYVZ2LQZ1E", "A2OWFD7J4MRGKM", "A3QDZ935V5U31J", "AQ4UDCIBEFO01", "A1KJE75LL545EI", "A20W01MAJ3M9AC","A38TZ2TQW9TVBU"))

# leave out practice trials
data <- subset(data, data$practice == 0)

# rename variables and levels to be consistent 

# convert gender info to lowercase
subinfo$sex <- as.character(subinfo$sex) 
subinfo$sex <- base::tolower(subinfo$sex) 
subinfo$sex <- as.factor(subinfo$sex) 
subinfo <- plyr::rename(subinfo, c("sex" = "gender"))
subinfo$gender <- plyr::mapvalues(subinfo$gender, from = c("female ", "m", "male ", "male  ", "nonbinary", "woman"), to = c("female", "male", "male", "male", "other", "female"))

# check # of trials completed by each participant
# remove the participants who did not complete all 125 trials (trial_index = [0,124])
n_trials_df <- data %>%
  dplyr::group_by(subid) %>%
  dplyr::summarise(no_trials = n_distinct(trial_index)) %>%
  dplyr::arrange(subid, no_trials) 
data <- merge(data, n_trials_df, by = "subid") # merge these two dataframes
data <- subset(data, data$no_trials == 125) # remove subjects with less than 125 trials
data$no_trials <- NULL # remove this unnecessary column

# check for duplicated MTurk IDs
count_df <- subinfo %>% 
  dplyr::group_by(subid) %>%
  dplyr::summarise(no_rows = length(subid)) # create a counting variable
subinfo <- base::merge(subinfo, count_df, by = "subid") 
subinfo <- subset(subinfo, subinfo$no_rows == "1") # leave out duplicated workers
subinfo$no_rows <- NULL 

count_df <- data %>% 
  dplyr::group_by(subid) %>%
  dplyr::summarise(no_rows = length(subid)) 
data <- base::merge(data, count_df, by = "subid") 
data <- subset(data, data$no_rows == "125") 
data$no_rows <- NULL 

# remove unnecessary raw files
# base::remove(n_trials_df)

# create a vector of all subjects with complete data
sub_vec <- data[, c(1,3,4)]
sub_vec <- sub_vec[!duplicated(sub_vec), ]
sub_vec <- sub_vec[ order(sub_vec$subid), ] 

# merge subinfo with sub_vec
subinfo <- dplyr::right_join(subinfo, sub_vec, by = "subid")
#subinfo <- na.omit(subinfo)

# rearrange both data and subinfo to see if everything looks okay
subinfo <- subinfo[ order(subinfo$subid), ]  
data <- data[ order(data$subid), ] 

# check
utils::head(data)
utils::str(data)
base::summary(data)
dplyr::glimpse(data)

utils::head(subinfo)
utils::str(subinfo)
base::summary(subinfo)
dplyr::glimpse(subinfo)

# write final datasets in csv format
utils::write.csv(data, "data.csv", row.names = FALSE)
utils::write.csv(subinfo, "subinfo.csv", row.names = FALSE)
