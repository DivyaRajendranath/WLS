#-------------------------------------------------------
#
# Source code file name: PulseScoreCalculator.R
#
# The code computes the Pulse Score and generates
# question recommendations using Recosystem. It 
# runs the pulse score for the whole data set
# and test runs for every activity day in MAy-20.
#
# 
# Author: Divya Rajendranath
#
#-------------------------------------------------------
library('data.table')
library('dplyr')
library('tidyverse')
library('lubridate')
library('recommenderlab')
library('stringr') 
library(recosystem)
set.seed(123)

#---- Constant variables and vectors----
partitions = seq(0,2400, 10)
bands = seq(10,2400, 10)
#---- Constant variables and vectors----

# Read the question activity data file
read_question_activity_file = function() {
  print(paste0("Inside file read function"))
  #file path
  activity_data = load(file = "C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/assessments_with_tags.RData")
  # change wide format to long format
  df = melt(question_data6, id.vars = c("question_id","country","org_id","role_id","submission_utc_ts","no_of_trials","points_earned","masked_user_id"))
  names(df)[10] = "question_tags"
  FULL_question_data <<- within(df, rm("variable"))
  # original KUSHAL question activity data set
  # dt_dump = fread("C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/user_assessments.csv")
  print(paste0("DF size = ",nrow(df)))
}
############################# PART-1 ###############################

filter_question_data = function(gb, start_date, end_date,mode) {
  print(paste0("Inside filter question data function ", start_date, " ", end_date, " mode = ", mode))
  # use data tables
  dt_dump = as.data.table(gb)
  print(paste0("~~~~~ size of dt_dump before removal = ", nrow(dt_dump)))
  if(mode == "synthetic") {
    if(cnt>1) {
      for(x in 1:cnt) {
        print(paste0("~~~~", cnt, " inner dates = ", may_dates[x]))
        dt_dump = dt_dump[!((substr(dt_dump$submission_utc_ts, 1, 10) == may_dates[x]) & (masked_user_id %in% u_test)),]
        print(paste0("~~~~~ size of dt_dump after ", x ,"  removal = ", nrow(dt_dump)))
      }
    } else{
      print(paste0("~~~~", cnt, " dates = ", may_dates[cnt]))
      dt_dump = dt_dump[!((substr(dt_dump$submission_utc_ts, 1, 10) == may_dates[cnt]) & (masked_user_id %in% u_test)),]
      print(paste0("~~~~~ size of dt_dump after ", cnt ,"  removal = ", nrow(dt_dump)))
    }
    dt_dump = rbind(dt_dump, SYNTHETIC_activity_data)
    print(paste0("After adding synthetic rows, total size ", nrow(dt_dump)))
  }
  # remove NA rows
  dt_dump = na.omit(dt_dump)
  # add answer_date and datetime features
  dt_dump[, eval("answer_datetime"):=ymd_hms(dt_dump$submission_utc_ts)]
  dt_dump[, eval("answer_date"):=as_date(dt_dump$answer_datetime)]
  dt_dump = dt_dump[answer_date >= start_date & answer_date <= end_date]
  

  print(paste0("DT DUMP size = ",nrow(dt_dump)))
  return(dt_dump)
}

set_tag_parameters = function(df_tag_fullset) {
  print(paste0("Inside tag density kvalue function"))
  # level the tag density, based on steps of 10
  df_tag_fullset$qLvl <- cut(df_tag_fullset$N, breaks = partitions, labels = bands)
  kValues = NULL
  # find k-value for each stepped tag density
  for (x in 1:length(bands)) {
    kValues[x] = round((log(0.05/0.95))/(-(bands[x]-1)), 4)
  }
  kSet=NULL
  kSet = data.frame(bands, kValues)
  df_tag_fullset$k = sapply(df_tag_fullset$qLvl, function(y) kSet[kSet$bands == y, 2])
  print(paste0("KSET size = ",nrow(kSet)))
  #return(kSet,df_tag_fullset)

  print(paste0("Inside penalty factor function"))
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
  print(paste0("PENALTYSET size = ",nrow(df_tag_penalty_set)))

  print(paste0("Inside qPool scaled value function"))
  df_expanded_tag_set = data.frame()
  # scaled denominator for each tag
  for(y in 1:nrow(df_tag_fullset)) {
    tag = df_tag_fullset[y, "question_tags"]
    lvl = as.numeric(as.character(df_tag_fullset[y,"qLvl"]))
    #print(paste0("tag = ", tag, " level = ", lvl))
    tag_set = cbind(question_tags=rep(tag,lvl), df_tag_penalty_set[df_tag_penalty_set$bands==lvl,])
    df_expanded_tag_set=rbind(df_expanded_tag_set, tag_set)
  }
  print(paste0("EXPANDED TAG SET size = ",nrow(df_expanded_tag_set)))
  return(df_expanded_tag_set)
}

get_ordered_question_data = function(dt_cntry_full_data) {
  print(paste0("Inside ordered question function"))
  cntDataSet = nrow(dt_cntry_full_data) #185269
  print(paste("Number of unique records in GB ",cntDataSet))
  # Remove repeated occurrence of same question id and question tags within the same date for every user
  dt_cntry_full_data = dt_cntry_full_data[, .SD[which.max(answer_datetime)], 
                                          by=c("masked_user_id", "answer_date", "question_id","question_tags")]
   # Consider '10' points_earned only
  dt_correct_ans = dt_cntry_full_data[points_earned == 10 | points_earned == 5,]
  #nrow(dt_correct_ans) #105777 #117364
  
  # Remove repeated questions answered by users in the past.
  # Consider only the most recently answered correct questions, 
  # with no repetition of same question id = tag for same user
  dt_final_user_ques_set = dt_correct_ans[,
                                          .SD[which.max(answer_date)],
                                          by=c("masked_user_id","question_id","question_tags")][order(masked_user_id, answer_date)]
  dt_final_user_ques_set = dt_final_user_ques_set[,sid:=seq_along(answer_date),by=c("masked_user_id","question_tags")]
  dt_final_user_ques_set[,c("submission_utc_ts","answer_datetime"):=NULL]
  print(paste0("DF FINAL USER QUES SET size = ",nrow(dt_final_user_ques_set)))
  return(dt_final_user_ques_set)
}

get_question_difficulty_levels = function(dt_final_user_ques_set) {
  print(paste0("Inside  question difficulty  function"))
  dt_question_data = dt_final_user_ques_set[,.(question_id, question_tags, points_earned, no_of_trials)]
  df_question_data = as.data.frame(dt_question_data)
  df_question_diff_level =  df_question_data %>%
    group_by(question_id, question_tags) %>%
    summarise(correct_count = sum(points_earned),
              attempt_count = (sum(no_of_trials)*10),
              easy = round(correct_count/attempt_count,3)) %>%
    mutate(diff = ifelse(easy >= 0.75, 0.3, ifelse(easy <= 0.25, 0.1, 0.2)))
  print(paste0("QUESTION DIFF LEVEL size = ", nrow(df_question_diff_level)))
  return(df_question_diff_level)
}

calculate_pulse_score_using_diff_level = function(df_question_diff_level,
                                                  df_expanded_tag_set,
                                                  dt_final_user_ques_set,
                                                  mode) {
  print(paste0("Inside pulse score calculation function"))
  df_ps_comp_set = merge(dt_final_user_ques_set,df_expanded_tag_set, by=c("question_tags","sid"), all = TRUE)
  df_ps_comp_set=na.omit(df_ps_comp_set)
  df_ps_comp_set[,penalty:= ifelse(points_earned<10, round(penalty/2,4), penalty)]
  print(paste0("PS_COMP_SET size = ", nrow(df_ps_comp_set)))
  
  df_ps_comp_set = merge(df_ps_comp_set,df_question_diff_level, by=c("question_tags","question_id"), all = TRUE)
  df_ps_comp_set=na.omit(df_ps_comp_set)
  #df_ps_comp_set[,c("correct_count","attempt_count","sid"):=NULL]
  df_ps_comp_set[,.N,by=c("masked_user_id","question_tags")]
  #32868 #34274
  df_ps_comp_set[,score:=round((sum(penalty - (diff * penalty))/.N)/qPoolScale,4),by=c("masked_user_id","question_tags")]
  #df_ps_comp_set[,score:=round((sum(penalty)/.N)/qPoolScale,4),by=c("masked_user_id","question_tags")]
  df_final_ps_score_set = unique(df_ps_comp_set[,list(masked_user_id,question_tags,score)])
  
  if(mode == "full") {
    GB_df_ps_comp_set <<- df_ps_comp_set
    GB_df_final_ps_score_set <<- df_final_ps_score_set
    print(paste0("FULL PS SET size = ", nrow(GB_df_final_ps_score_set)))
  } else if(mode == "subset") {
    SUB_df_ps_comp_set <<- df_ps_comp_set
    SUB_df_final_ps_score_set <<- df_final_ps_score_set
    print(paste0("SUB PS SET size = ", nrow(SUB_df_final_ps_score_set)))
  } else {
    SYN_df_ps_comp_set <<- df_ps_comp_set
    SYN_df_final_ps_score_set <<- df_final_ps_score_set
    print(paste0("SYN PS SET size = ", nrow(SYN_df_final_ps_score_set)))
  }

  #if(mode == "full") {
  #  file_path = "C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/output/GB_pulse_score.txt"
   # write.table(df_final_ps_score_set,file_path,append = TRUE, col.names = TRUE, sep = ",", quote = FALSE) 
  #}
}

calculate_pulse_score = function(start_date, end_date, mode) {
  
  print(paste0("Inside MAIN PULSE SCORE CALCULATION function"))

  dt_dump = data.table()
  dt_dump = filter_question_data(GB, start_date, end_date, mode)
  dt_questions_fullset = unique(dt_dump[,.(question_id, question_tags)]) #2630 unique questions & tag combinations  
  dt_tag_fullset = dt_questions_fullset[,.N, by=question_tags] #379 unique tags
  df_tag_fullset = as.data.frame(dt_tag_fullset)
  df_expanded_tag_set = set_tag_parameters(df_tag_fullset)
  if (mode == "full") {
    GB_dt_final_user_ques_set <<- get_ordered_question_data(dt_dump)
    GB_df_question_diff_level <<- get_question_difficulty_levels(GB_dt_final_user_ques_set)
    calculate_pulse_score_using_diff_level(GB_df_question_diff_level,
                                           df_expanded_tag_set,
                                           GB_dt_final_user_ques_set,
                                           mode)
    print(paste0("EOF (FULL) MAIN PULSE SCORE CALCULATION = ", nrow(GB_df_final_ps_score_set)))
  } else if(mode == "subset") {
    SUB_dt_final_user_ques_set <<- get_ordered_question_data(dt_dump)
    SUB_df_question_diff_level <<- get_question_difficulty_levels(SUB_dt_final_user_ques_set)
    calculate_pulse_score_using_diff_level(SUB_df_question_diff_level,
                                           df_expanded_tag_set,
                                           SUB_dt_final_user_ques_set,
                                           mode)
    print(paste0("EOF (SUBSET) MAIN PULSE SCORE CALCULATION = ", nrow(SUB_df_final_ps_score_set)))
  } else {
    SYN_dt_final_user_ques_set <<- get_ordered_question_data(dt_dump)
    SYN_df_question_diff_level <<- get_question_difficulty_levels(SYN_dt_final_user_ques_set)
    calculate_pulse_score_using_diff_level(SYN_df_question_diff_level,
                                           df_expanded_tag_set,
                                           SYN_dt_final_user_ques_set,
                                           mode)
    print(paste0("EOF (SYNTHETIC) MAIN PULSE SCORE CALCULATION = ", nrow(SYN_df_final_ps_score_set)))
  }
}

############################# PART-2 ###############################
get_train_data = function(ps_data, smp_size) {
  print(paste0("Inside train data function "))
  train_indexes <- sample(1: nrow(ps_data), size = smp_size)
  df_train_set <- ps_data[train_indexes, ]; dim(df_train_set)
  df_train_set = merge(df_train_set, GB_df_users, by="masked_user_id", all = TRUE)
  df_train_set = merge(df_train_set, GB_df_items, by="question_tags", all = TRUE)
  df_train_set = na.omit(df_train_set)
  df_ver_set = merge(ps_data, Verification_users, by="masked_user_id", all = TRUE)
  df_ver_set = merge(df_ver_set, GB_df_items, by="question_tags", all = TRUE)
  df_train_set = rbind(df_train_set, df_ver_set)
  df_train_set = na.omit(df_train_set)
  print(paste0("Train data set size = ", nrow(df_train_set)))
  return(df_train_set)
}


get_test_data = function(ps_data, smp_size, df_users, df_items){
  print(paste0("Inside test data function ",length(u_test)))
  train_indexes <- sample(1: nrow(ps_data), size = smp_size)
  df_test_set  <- ps_data[-train_indexes, ]; dim(df_test_set)  
  df_test_set = merge(df_test_set, GB_df_users, by="masked_user_id", all = TRUE)
  df_test_set = merge(df_test_set, GB_df_items, by="question_tags", all = TRUE)
  df_test_set = na.omit(df_test_set)
  df_ver_set = merge(ps_data, Verification_users, by="masked_user_id", all = TRUE)
  df_ver_set = merge(df_ver_set, GB_df_items, by="question_tags", all = TRUE)
  print(paste0(nrow(df_ver_set)))
  df_test_set = rbind(df_test_set, df_ver_set)
  df_test_set = na.omit(df_test_set)
  print(paste0("Test data set size = ", nrow(df_test_set)))
  return(df_test_set)
}

run_recommender = function(df_train_set, df_test_set) {
  print(paste0("Inside run recommender function "))
  train_data = data_memory(df_train_set$user_id, df_train_set$item_id, df_train_set$score)
  test_data = data_memory(df_test_set$user_id, df_test_set$item_id)
  r = Reco()
  model = r$tune(train_data = train_data, opts = list(dim = c(10,20,30,40),
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
  total_error  
  print(paste0("DF EVAL TEST SET size = ", nrow(df_eval_test_set)))
  return(df_eval_test_set)
}

generate_questions_from_recomm_tags = function(df_eval_test_set, mode){
  
  print(paste0("Inside genrate question from recomm tags function "))
  
  # Take top 100 tags with highest pulse score 
  # means those tags with past activity
  recomm_tags = df_eval_test_set %>%
    filter(masked_user_id %in% u_test) %>%
    group_by(masked_user_id) %>%
    arrange(desc(pred_score)) %>%
    slice(1:100)
  
  rank_ordered  = data.frame()
  attempted_full_set = data.frame()
  tobe_recomm_questions = data.frame()
  top5_recomm_questions = data.frame()
  
  if(mode == "subset") {
    rank_ordered = SUB_df_question_diff_level %>%
      arrange(question_tags, diff) %>%
      select(question_tags, question_id, diff)
    attempted_full_set = merge(SUB_df_ps_comp_set, recomm_tags, by=c("masked_user_id","question_tags"), all = TRUE)
    recomm_tags_with_history_tags = attempted_full_set %>% 
      filter(pred_score > 0) %>%
      select(masked_user_id, question_tags, question_id, score.x) 
    tobe_recomm_questions = SUB_df_question_diff_level %>%
      left_join(recomm_tags_with_history_tags, by = ("question_tags" = "question_tags")) %>%
      filter(question_id.x != question_id.y) %>%
      arrange(masked_user_id, desc(diff)) %>%
      select(masked_user_id, question_tags, question_id.x)
    # from the question master, take out the unanswered questions with the lowest diff level
    
    tobe_recomm_questions = unique(tobe_recomm_questions)
    
    # for each user, and every recommended tag, take the top 5 recommended questions
    top5_recomm_questions = tobe_recomm_questions %>%
      group_by(masked_user_id, question_tags) %>%
      top_n(3)
    colnames(top5_recomm_questions)[colnames(top5_recomm_questions) == 'question_id.x'] <- 'question_id'
    top5_recomm_questions = top5_recomm_questions %>%
      top_n(50)
    
  } else {
    # previous activity of test users
    prev_activity_test_users  = SYN_df_ps_comp_set %>%
      filter(masked_user_id %in% u_test) %>%
      select(masked_user_id, question_id, question_tags, score)

    # pick tags from recommender with previous activity
    attempted_full_set = merge(prev_activity_test_users, recomm_tags, by=c("masked_user_id","question_tags"), all = TRUE)
 
    # pick recomm tags and its past questions
    recomm_tags_with_history_tags = attempted_full_set %>% 
      filter(pred_score > 0) %>%
      select(masked_user_id, question_tags, question_id, score.x) 
    
    #top5_recomm_questions = data.frame()
    q_recomm = data.frame()
    for (u in u_test) {
      recomm_tags_per_user = recomm_tags %>%
        filter(masked_user_id == u)
      question_pool_per_user = SYN_df_question_diff_level %>%
        filter(question_tags %in% recomm_tags_per_user$question_tags) %>%
        select(question_tags, question_id, diff)
      recomm_tags_with_history_per_user = recomm_tags_with_history_tags %>%
        filter(masked_user_id == u)
      delta_questions_per_user = merge(question_pool_per_user, recomm_tags_with_history_per_user, 
                                       by=c("question_tags","question_id"), all.x=TRUE)
      q_recomm = delta_questions_per_user %>%
        filter(is.na(score.x)) %>%
        group_by(question_tags) %>%
        arrange(desc(diff)) %>%
        select(question_tags, question_id) %>%
        top_n(3)
      q_recomm = q_recomm %>%
        top_n(50)
      
      q_recomm = cbind(u, q_recomm )
      colnames(q_recomm) = c("masked_user_id","question_tags","question_id")
      top5_recomm_questions = rbind(top5_recomm_questions, q_recomm)
    }
  }
  print(paste0("DF top 50 recomm question  = ", nrow(top5_recomm_questions)))
  return(top5_recomm_questions)
}

create_activity_data_from_recomm_questions = function(top5_recomm_questions, syn_date) {
  print(paste0("~~~~~~~~~~~~ Inside create activity data from recomm questions function ", syn_date))
  recomm_size = nrow(top5_recomm_questions)
  print(paste0("Recomm questions size = ", recomm_size))
  print(paste0("Col names = ", colnames(top5_recomm_questions)))
  country = rep("GB",recomm_size)
  org_id = rep(28, recomm_size)
  role_id =  rep(1,recomm_size)
  dval = paste0(syn_date," 00:00:00")
  datetime_val = rep(dval,recomm_size)
  trials = rep(1,recomm_size)
  points = rep(10, recomm_size)
  #right_answers = round(recomm_size - (recomm_size/10))
  #points = sample(c(rep(10,right_answers),rep(5,round(recomm_size - right_answers))))
  
  
  top5_recomm_questions["org_id"] = org_id
  top5_recomm_questions["role_id"] = role_id
  top5_recomm_questions["country"]=country
  top5_recomm_questions["points_earned"] = points
  top5_recomm_questions["no_of_trials"] = trials
  top5_recomm_questions["submission_utc_ts"] = datetime_val
  print(paste0("~~~~~~~~~~~~SYNTHETIC activity data set size = ",nrow(top5_recomm_questions)))
  return(top5_recomm_questions)
}

recommend_tags_using_pulse_score = function(ps_data, smp_size, syn_date, mode) {
  print(paste0("~~~~~~~~~~ Recommendation main method = ", syn_date , " with mode = ", mode))
  df_train_set = get_train_data(ps_data, smp_size)
  df_test_set = get_test_data(ps_data, smp_size)
  df_eval_test_set = run_recommender(df_train_set, df_test_set)
  top5_recomm_questions = generate_questions_from_recomm_tags(df_eval_test_set,mode)
  recomm_activity_data = create_activity_data_from_recomm_questions(top5_recomm_questions, syn_date)
  print(paste0("~~~~~~~~~~ recomm activity data size = ", nrow(recomm_activity_data), " for date = ", syn_date))
  SYNTHETIC_activity_data <<- rbind(SYNTHETIC_activity_data, recomm_activity_data)
  print(paste0("~~~~~~~~~~ total synthetic activity data size = ", nrow(SYNTHETIC_activity_data)))
}

############################# PART-3 ###############################
#-----------------------------------------------------------------
# 1. Read question activity data
#-----------------------------------------------------------------
SYNTHETIC_activity_data <<- data.frame()
may_dates <<- c("2020-05-03","2020-05-04","2020-05-09", "2020-05-10",
                "2020-05-11","2020-05-14","2020-05-17","2020-05-25",
                "2020-05-26")


read_question_activity_file()
#consider only country=GB
GB <<- FULL_question_data %>%
  filter(country == 'GB') 
dim(GB) #585144
remove(FULL_question_data)

# Universe of users
v_users = unique(GB$masked_user_id)
GB_df_users <<- data.frame(v_users, c(1:length(v_users)))
colnames(GB_df_users) = c("masked_user_id","user_id")

# Selective test users who have answered question before APRIL-2020
u_test <<- c("035f412b","108ae76d","22408aad","2a41059e","4ffee38a","560d7304","8100aef3","8f7b79fd","fe5e359d")
#u_test <<- c("fe5e359d","108ae76d")
Verification_users <<-GB_df_users[GB_df_users$masked_user_id %in% u_test,]
GB_df_users = GB_df_users[!(GB_df_users$masked_user_id %in% Verification_users$masked_user_id),]

# Universe of tags
v_tags = unique(GB$question_tags)
GB_df_items <<- data.frame(v_tags, c(1:length(v_tags)))
colnames(GB_df_items) = c("question_tags","item_id")

#-----------------------------------------------------------------
# 2. Pulse score generation for Complete GB data
#-----------------------------------------------------------------
calculate_pulse_score("2019-05-12","2020-05-26", "full")
GB_df_final_ps_score_set %>%
  filter(score<=0) %>%
  count()

#-----------------------------------------------------------------
# 3. Pulse score generation for SubSet GB data
#    Go back to End of APRIL and create recommendations
#    to synthesize the data for MAY
#-----------------------------------------------------------------
calculate_pulse_score("2019-05-12", "2020-04-30", "subset")
 
#-----------------------------------------------------------------
# 4. USING UPSKILL RECOMMENDER get the questions to be recommended
#-----------------------------------------------------------------
final_ps_report = data.frame()
cnt<<-0
for(i in may_dates) {
  cnt <<- cnt +1
  iter <<- i
  print(paste0("~~~In iterations = ",iter, " recommendation date = ", iter))
  
  if (cnt ==1) {
    smp_size <- floor(0.9 * nrow(SUB_df_final_ps_score_set))
    recommend_tags_using_pulse_score(SUB_df_final_ps_score_set, smp_size, iter, "subset")
  } else {
    smp_size <- floor(0.9 * nrow(SYN_df_final_ps_score_set))
    recommend_tags_using_pulse_score(SYN_df_final_ps_score_set, smp_size, iter, "synthetic")
  }
  #-----------------------------------------------------------------
  # 5. Run Pulse score on Synthetic data and compare the increase in pulse scores
  #-----------------------------------------------------------------
  print(paste0("~~~~~~~~~~ total synthetic activity data size = ", nrow(SYNTHETIC_activity_data)))
  calculate_pulse_score("2019-05-12", iter, "synthetic")

  daily_ps = SYN_df_final_ps_score_set %>%
    filter(masked_user_id %in% u_test)
  daily_ps = cbind(daily_ps, iter)
  final_ps_report = rbind(final_ps_report, daily_ps)  
}
 