library('data.table')
library('dplyr')
library('tidyverse')
library('lubridate')
library('recommenderlab')
library('stringr')
#-----------------------------------------------
# 1. Read the Synthetic tag data file for assessment data
#-----------------------------------------------
test = load(file = "C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/assessments_with_tags.RData")
df = melt(question_data6, id.vars = c("question_id","country","org_id","role_id","submission_utc_ts","no_of_trials","points_earned","masked_user_id"))
names(df)[10] = "question_tags"
df = within(df, rm("variable"))
# original KUSHAL question activity data set
# dt_dump = fread("C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/user_assessments.csv")
# dt = as.data.frame(dt_dump)

# consider on Great Britain
gb = df %>%
  filter(country == 'GB') 
dim(gb)
#585144

# use data tables
dt_dump = as.data.table(gb)
dt_dump = na.omit(dt_dump)
dt_dump[, eval("answer_datetime"):=ymd_hms(dt_dump$submission_utc_ts)]
dt_dump[, eval("answer_date"):=as_date(dt_dump$answer_datetime)]
dt_dump[,.N, country][order(-N)]
# total question activity records in GB: 353108
#-----------------------------------------------
# Question Tag Master
#-- ---------------------------------------------
dt_questions_fullset = unique(dt_dump[,.(question_id, question_tags)]) #2630 unique questions & tag combinations
dt_questions_fullset[,qFreq:=.N, by=question_id]
dt_questions_fullset[,tFreq:=.N, by=question_tags]
#3891 question id, with qFreq=4, tFreq=2,24,62
dt_questions_fullset[,diff_level:=ifelse(round((1/qFreq) * (1/tFreq),2) <= 0, 
                                         0.01,
                                         round((1/qFreq) * (1/tFreq),2)), by=question_tags]
dt_tag_fullset = dt_questions_fullset[,.N, by=question_tags] #379 unique tags
df_tag_fullset = as.data.frame(dt_tag_fullset)

partitions = seq(0,2400, 10)
bands = seq(10,2400, 10)
df_tag_fullset$qLvl <- cut(df_tag_fullset$N, breaks = partitions, labels = bands)

kValues = NULL
for (x in 1:length(bands)) {
  kValues[x] = round((log(0.05/0.95))/(-(bands[x]-1)), 4)
}
kSet=NULL
kSet = data.frame(bands, kValues)
df_tag_fullset$k = sapply(df_tag_fullset$qLvl, function(y) kSet[kSet$bands == y, 2])
#-----------------------------------
# Penalty factor
#-----------------------------------
df_tag_penalty_set = data.frame()
for(z in 1:nrow(kSet)) {
  lvl = kSet[z, "bands"]
  k = kSet[z, "kValues"]
  penalty_set = array(0.5, dim=c(lvl, 1, 1))
  for(i in 2:length(penalty_set)) {
    penalty_set[i] = round(1/(1 + exp((-k)* (i-1))),4)
  }
  scaleValue = round(sum(penalty_set)/lvl,4)
  penalty_map = data.frame(bands=rep(lvl,lvl),sid=1:lvl, penalty=penalty_set, qPoolScale=rep(scaleValue,lvl))
  df_tag_penalty_set=rbind(df_tag_penalty_set,penalty_map)
}
df_expanded_tag_set = data.frame()
for(y in 1:nrow(df_tag_fullset)) {
  tag = df_tag_fullset[y, "question_tags"]
  lvl = as.numeric(as.character(df_tag_fullset[y,"qLvl"]))
  tag_set = cbind(question_tags=rep(tag,lvl), df_tag_penalty_set[df_tag_penalty_set$bands==lvl,])
  df_expanded_tag_set=rbind(df_expanded_tag_set, tag_set)
}

#---------------------------------------------------------
# Country specific changes
#---------------------------------------------------------
country_list = unique(dt_dump[,country])
country_list = country_list[!is.na(country_list)]
country_list = country_list[country_list != ""]
file_path = "C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/output/pulse_score.csv"
for (c in country_list) {
  c = trimws(c, which = c("both"))
  print(paste("Start of Processing: " , c, "...."))
  
  # 2. Consider only US data
  dt_cntry_full_data = unique(dt_dump[country == c])
  cntDataSet = nrow(dt_cntry_full_data) #185269
  print(paste("Number of unique records in ", c, ": ", cntDataSet))
  # 3. Remove repeated occurrence of same question id and question tags within the same date for every user
  dt_cntry_full_data = dt_cntry_full_data[, .SD[which.max(answer_datetime)], 
                                        by=c("masked_user_id", "answer_date", "question_id","question_tags")]
  #nrow(dt_cntry_full_data)
  # 138113
  #unique(dt_cntry_full_data[,.N, c("no_of_trials","points_earned")])[order(no_of_trials)]
  #unique(dt_cntry_full_data[(points_earned == 10 | points_earned == 5),.N, c("points_earned","no_of_trials")])

  # 4. Consider '10' points_earned only
  dt_correct_ans = dt_cntry_full_data[points_earned == 10 | points_earned == 5,]
  #nrow(dt_correct_ans) #105777 #117364
  
  # 5. Remove repeated questions answered by users in the past.
  #    Consider only the most recently answered correct questions, 
  #    with no repetition of same question id = tag for same user
  dt_final_user_ques_set = dt_correct_ans[,
                                        .SD[which.max(answer_date)],
                                        by=c("masked_user_id","question_id","question_tags")][order(masked_user_id, answer_date)]
  #dim(dt_final_user_ques_set) #87311 #96514
  
  #dt_final_user_ques_set[,.N, c("masked_user_id","question_id","question_tags")][order(-N)]
  dt_final_user_ques_set = dt_final_user_ques_set[,sid:=seq_along(answer_date),by=c("masked_user_id","question_tags")]
  #dim(dt_final_user_ques_set) #87311 #96514
  #unique(dt_final_user_ques_set[,no_of_trials])
  
  dt_final_user_ques_set[,c("submission_utc_ts","answer_datetime"):=NULL]
  #dt_final_user_ques_set[,.N,by=c("masked_user_id")][order(-N)]
  
  df_ps_comp_set = merge(dt_final_user_ques_set,df_expanded_tag_set, by=c("question_tags","sid"), all = TRUE)
  df_ps_comp_set=na.omit(df_ps_comp_set)
  df_ps_comp_set[,penalty:= ifelse(points_earned<10, round(penalty/2,4), penalty)]
  
  #---------------------------------
  # Difficulty level
  #---------------------------------
  #dt_question_data = dt_final_user_ques_set[,.(question_id, question_tags, points_earned, no_of_trials)]
  #df_question_data = as.data.frame(dt_question_data)
  #df_question_diff_level =  df_question_data %>%
  #  group_by(question_id, question_tags) %>%
  #  summarise(correct_count = sum(points_earned),
  #            attempt_count = (sum(no_of_trials)*10),
  #            diff = round(1 - correct_count/attempt_count,3))
  df_question_diff_level = as.data.frame(dt_questions_fullset)
  df_question_diff_level$qFreq = NULL
  df_question_diff_level$tFreq = NULL
  df_ps_comp_set = merge(df_ps_comp_set,df_question_diff_level, by=c("question_tags","question_id"), all = TRUE)
  df_ps_comp_set=na.omit(df_ps_comp_set)
  #df_ps_comp_set[,c("correct_count","attempt_count","sid"):=NULL]
  df_ps_comp_set[,.N,by=c("masked_user_id","question_tags")]
  #32868 #34274
  df_ps_comp_set[,score:=round((sum(penalty * diff_level)/.N)/qPoolScale,4),by=c("masked_user_id","question_tags")]
  df_final_ps_score_set = unique(df_ps_comp_set[,list(masked_user_id,question_tags,score)])
  
  write.table(df_final_ps_score_set,file_path,append = TRUE, col.names = TRUE, sep = ",", quote = FALSE) 
  print(paste("Pulse score output file: ", file_path))
  print(paste("End of Processing: ", c))
  print(paste("----------------------------------------------"))
}

#-----------------------------------------------------------------
# UPSKILL RECOMMENDER
#-----------------------------------------------------------------

df_final_ps_score_set %>%
  filter(score<=0) %>%
  count()

min(df_final_ps_score_set$score)
max(df_final_ps_score_set$score)

v_users = unique(df_final_ps_score_set$masked_user_id)
length(v_users)
#1396 users
df_users = data.frame(v_users, c(1:length(v_users)))
colnames(df_users) = c("masked_user_id","user_id")
v_tags = unique(df_final_ps_score_set$question_tags)
length(v_tags)
#379 tags
df_items = data.frame(v_tags, c(1:length(v_tags)))
colnames(df_items) = c("question_tags","item_id")

total_users_cnt = length(unique(df_final_ps_score_set$masked_user_id))
test_users_cnt = round(0.1 * total_users_cnt,0)
train_users_cnt = total_users_cnt - test_users_cnt
v_test_users = tail(v_users,test_users_cnt)
v_train_users = tail(v_users,train_users_cnt)

df_train_set = df_final_ps_score_set[df_final_ps_score_set$masked_user_id %in% v_train_users,]
df_train_set = merge(df_train_set, df_users, by="masked_user_id", all = TRUE)
df_train_set = merge(df_train_set, df_items, by="question_tags", all = TRUE)
df_train_set = na.omit(df_train_set)
dim(df_train_set)
# 65725
df_test_set = df_final_ps_score_set[df_final_ps_score_set$masked_user_id %in% v_test_users,]
df_test_set = merge(df_test_set, df_users, by="masked_user_id", all = TRUE)
df_test_set = merge(df_test_set, df_items, by="question_tags", all = TRUE)
df_test_set = na.omit(df_test_set)
dim(df_test_set)
#2537

#-----------------------------------------------------------------
# Recosystem - Matrix Factorization
#-----------------------------------------------------------------
library(recosystem)
set.seed(123)

train_data = data_memory(df_train_set$user_id, df_train_set$item_id, df_train_set$score)
test_data = data_memory(df_test_set$user_id, df_test_set$item_id)
r = Reco()
model = r$tune(train_data = train_data, opts = list(dim = c(10,20,30),
                                                    lrate = c(0.1,0.2),
                                                    costp_l1 = 0,
                                                    costq_l1 = 0,
                                                    nthread = 1,
                                                    niter = 10))
model
r$train(train_data, opts = c(model$min, nthread = 1, niter = 10))
pred_file = tempfile()
pred_score = r$predict(test_data = test_data, out_memory())

df_eval_test_set = cbind(df_test_set, pred_score)
#diff between original and predicted
df_eval_test_set$sq_err = (df_eval_test_set$score - df_eval_test_set$pred_score)**2
total_error = sqrt(sum(df_eval_test_set$sq_err))
# error = 0.6111

recomm_tags = df_eval_test_set %>%
  arrange(desc(pred_score)) %>%
  group_by(masked_user_id) %>%
  slice(1:5)

write.csv(recomm_tags, "C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/output/upskill_recommendations.csv")
