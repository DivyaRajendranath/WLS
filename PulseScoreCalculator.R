#install.packages("log4r")
library('dplyr')
library('tidyverse')
library('lubridate')
library(ggplot2)
library(log4r)

#--------------------------------------------
my_logfile = "C:/Divya/NUS Course Materials/FYP/SampleCode/PulseScoreLogs.txt"
#my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE)
my_logger <- log4r::logger(threshold = "INFO", appenders= list(my_file_appender))
info(my_logger, paste("Here is the log message "))
#--------------------------------------------


dump = read.csv("C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/user_assessments.csv")
dump %>%
  group(country, question_id) %>%
  summarise(n=n())

# TOTAL 1596 questions
#TOTAL 295 tags
str(dump)
dim(dump)
india_sample = dump %>%
  filter(country == "IN")
dim(india_sample)
#  1295290       9
india_sample = india_sample[, c("question_id","question_tags","masked_user_id","no_of_trials","points_earned","submission_utc_ts")]
india_sample = india_sample[!duplicated(india_sample),]
dim(india_sample)
# 1290148       9
india_sample$answer_datetime = ymd_hms(india_sample$submission_utc_ts)
india_sample$answer_date = as.Date(india_sample$answer_datetime)
str(india_sample)
dim(india_sample)
#  1290148       8

# ------------------- Check the QM counts ------------------
qm = india_sample %>%
  group_by(question_id, question_tags) %>%
  filter(n() == 1) %>%
  select(question_id, question_tags)
# india questions 519

qm %>%
  group_by(question_tags) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
# ------------------- Check the QM counts ------------------


final_india_sample = india_sample %>%
  group_by(masked_user_id, answer_date, question_id,question_tags) %>%
  summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
            points_earned = points_earned[which.max(answer_datetime)])
dim(final_india_sample)
#   944137      6
str(final_india_sample)
final_india_sample$no_of_trials
length(unique(final_india_sample$question_id))
#921
length(unique(final_india_sample$question_tags))
#245
length(unique(final_india_sample$masked_user_id))
#8620

# total number of questions answered by users
final_india_sample %>%
  group_by(masked_user_id, question_tags)%>%
  summarise(n=n()) %>%
  arrange(desc(n))
# 1eff6bf0        5530
# 9ca4acb0        3809
# 4e234be9        3707
#masked_user_id question_tags     n
# 1eff6bf0       tag-e442f041    850
# 4e234be9       tag-e442f041    817
# 4e234be9       tag-e5809a76    489
# 1eff6bf0       tag-e0cf8cf3    434
# 1eff6bf0       tag-e5809a76    371

str(final_india_sample)
#---------------------------------
# Question Master
#---------------------------------
india_qm = final_india_sample %>%
  group_by(question_id, question_tags) %>%
  summarise(n = n()) %>%
  select(question_id, question_tags) 

#---------------------------------
# Difficulty level
#---------------------------------
diff_level = final_india_sample %>%
  group_by(question_id, question_tags) %>%
  summarise(correct_count = sum(points_earned),
            attempt_count = (sum(no_of_trials)*10),
            diff = (1 - correct_count/attempt_count))
  
#-----------------------------------
# Tag level paramters - Qcnt, Qlvl, K
#-----------------------------------
# find how many questions are there in each question tag
india_qtag_qcnt = india_qm %>%
  group_by(question_tags) %>%
  summarise(qcnt = n())

partitions = c(0,10,20,30,40,50,60,70,80,90,100)
bands = c(10,20,30,40,50,60,70,80,90,100)
india_qtag_qcnt$qLvl <- cut(india_qtag_qcnt$qcnt, breaks = partitions, labels = bands)

kValues = NULL
for (x in 1:length(bands)) {
  print(bands[x])
  kValues[x] = round((log(0.05/0.95))/(-(bands[x]-1)), 4)
}
kSet=NULL
kSet = data.frame(bands, kValues)

india_qtag_qcnt$k = sapply(india_qtag_qcnt$qLvl, function(y) kSet[kSet$bands == y, 2])

#-----------------------------------
# Penalty factor
#-----------------------------------
plist = list()
scaleValue = list()
for(r in 1:nrow(kSet)) {
  qLvl = kSet[r,"bands"]  
  k = kSet[r,"kValues"]
  #print(qLvl)
  #print(k)
  penalty_set = array(0.5, dim=c(qLvl, 1, 1))
  for(i in 2:length(penalty_set)) {
    penalty_set[i] = round(1/(1 + exp((-k)* (i-1))),4)
  }
  scaleValue = append(scaleValue, round(sum(penalty_set)/qLvl,4))
  plist = append(plist, list(penalty_set))
}
str(kSet)
kSet$penalty = plist
kSet$scale = scaleValue
india_qtag_qcnt$penaltylist = sapply(india_qtag_qcnt$qLvl, function(y) kSet[kSet$bands == y, 3])
india_qtag_qcnt$scale = sapply(india_qtag_qcnt$qLvl, function(y) kSet[kSet$bands == y, 4])

#-------------------------------------------------------------
# Penalised Pulse Score
#-------------------------------------------------------------
head(india_qtag_qcnt)
head(kSet)

#-------------------------full india user set for pulse score ---------------------
correct_ans_india = final_india_sample[final_india_sample$points_earned == 10,]
dim(correct_ans_india)

users_uniqques = correct_ans_india %>%
  group_by(masked_user_id, question_id,question_tags) %>%
  summarise(no_of_trials = no_of_trials[which.max(answer_date)], 
            points_earned = points_earned[which.max(answer_date)],
            answer_date = max(answer_date)) %>%
  arrange(answer_date)

# find unique tags
users_uniqtag = users_uniqques %>%
  group_by(masked_user_id, question_tags) %>%
  summarise(n=n())

#users_uniqques = users_uniqques[users_uniqques$masked_user_id %in% c("0007bfe1","11776ae8","f2c11944","027fa47b","1df6e005"),]



#-------------------------full india user set for pulse score ---------------------
master=NULL
cnames = c("userid", "qtag","pulsescore")
master = data.frame(matrix(ncol=3))
#master = data.frame()
names(master) = cnames

#india_qtag_qcnt$scale = sapply(india_qtag_qcnt$qLvl, function(y) kSet[kSet$bands == y, 4])
# how many tags have qCnts=1, so that i can use sapply function
qTag_WithSingleQCnt = users_uniqtag %>%
  filter(n==1)

qId_WithSingleQCnt  = inner_join(users_uniqques, qTag_WithSingleQCnt, by = c("question_tags", "masked_user_id")) %>%
  select("masked_user_id", "question_id", "question_tags")

qId_WithSingleQCnt$pulsescore = sapply(qId_WithSingleQCnt, function(x,y,z){
  diff_level[diff_level$question_id == y & diff_level$question_tags == z,5]%>%pull()
  #penScale = india_qtag_qcnt[india_qtag_qcnt$question_tags == qId_WithSingleCnt$question_tags, 6] %>% pull()
  #difLvl = diff_level[diff_level$question_id == qId & diff_level$question_tags == qtag, 5] %>% pull()
  
})
                                       
for(i in 1:nrow(users_uniqtag)) {
  info(my_logger, paste("------------------- For loop --------------"))
  u = users_uniqtag[i,"masked_user_id"] %>% pull()
  u = as.character(u)
  info(my_logger, paste("User : ", u))
  qtag = users_uniqtag[i,"question_tags"] %>% pull()
  qtag = as.character(qtag)
  qCnt = users_uniqtag[i,"n"] %>% pull()
  userSet = users_uniqques[users_uniqques$masked_user_id == u & users_uniqques$question_tags == qtag,]
  if (qCnt==1) {
    info(my_logger, paste("Question count: 1 : tag = ", qtag))
    qId = userSet[, 2] %>% pull()
    #n = users_uniqques[users_uniqques$masked_user_id == u & users_uniqques$question_id == qId, 4] %>% pull()
    #p = users_uniqques[users_uniqques$masked_user_id == u & users_uniqques$question_id == qId, 5]  %>% pull()
    pf = india_qtag_qcnt[india_qtag_qcnt$question_tags == qtag, 5]  %>% pull()
    ps = india_qtag_qcnt[india_qtag_qcnt$question_tags == qtag, 6] %>% pull()
    d = diff_level[diff_level$question_id == qId & diff_level$question_tags == qtag, 5] %>% pull()
    pps = (as.numeric(pf[[1]][1]) * as.numeric(d))/as.numeric(ps)
    info(my_logger, paste("Question count: 1 : id =  ", qId))
    master = rbind(master, c(u, qtag, round(pps,4)))
  } else {
    qIdArr = userSet[, 2] %>% pull()
    qnum = length(qIdArr)
    info(my_logger, paste("= * = * = "))
    info(my_logger, paste("Question count: MORE THAN 1 : tag = ", qtag))
    info(my_logger, paste("Question count: MORE THAN 1 : qCount  = ", qnum))
    pf = india_qtag_qcnt[india_qtag_qcnt$question_tags == qtag, 5]  %>% pull()
    pfVal=0
    ppsVal = 0
    for (n in 1:qnum) {
      pfVal = as.numeric(pf[[1]][n])
      d = diff_level[diff_level$question_id == qIdArr[n] & diff_level$question_tags == qtag, 5] %>% pull()
      info(my_logger, paste("Question Iteration = ",n))
      info(my_logger, paste("Question Id = ",qIdArr[n]))
      info(my_logger, paste("Penalty factor = ",pfVal))
      info(my_logger, paste("Diff level = ",d))
      ppsVal = ppsVal + (as.numeric(d) * pfVal)
      print(ppsVal)
      info(my_logger, paste("= * = * = "))
    }
    ps = india_qtag_qcnt[india_qtag_qcnt$question_tags == qtag, 6] %>% pull()
    pps = (ppsVal/qnum)/(as.numeric(ps))
    info(my_logger, paste("Question count: MORE THAN 1 PPS :",pps))
    master = rbind(master, c(u, qtag, round(pps,4)))
    info(my_logger, paste("= * = * = "))
  }
}

write.csv(master, "C:/Divya/NUS Course Materials/FYP/SampleCode/master.csv")
#--------------------------
t = length(unique(india_qtag_qcnt$question_tags))
m = length(unique(final_india_sample$masked_user_id))

tagNames = array(unique(india_qtag_qcnt$question_tags))
users = array(unique(final_india_sample$masked_user_id))

test = data.frame(matrix(rep(0,m*t), nrow=m, ncol=t))
names(test) = tagNames
row.names(test) = users

#--------------------------

cq_user_ecf5aad3 = user_ecf5aad3 %>%
  filter(points_earned == 10) %>%
  group_by(question_id, question_tags) %>%
  filter(n() == 1)
  
  summarise(answer_date = answer_date[which.max(answer_date)])
  
  group_by(masked_user_id, answer_datet, question_id,question_tags) %>%
  summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
            points_earned = points_earned[which.max(answer_datetime)])


user_3316e33a = final_india_sample[final_india_sample$masked_user_id == "3316e33a",]

user_1c6fd02b = final_india_sample[final_india_sample$masked_user_id == "1c6fd02b",]

user_2f42c4fe = final_india_sample[final_india_sample$masked_user_id == "2f42c4fe",]

# ecf5aad3   32
# 3316e33a   72
# 2f42c4fe  200
# 1c6fd02b  502

