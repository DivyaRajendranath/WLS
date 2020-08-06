install.packages('synthpop')

library(dplyr)

suppressPackageStartupMessages(library(synthpop))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sampling))
suppressPackageStartupMessages(library(partykit))
library('tidyverse')
library(lubridate)
#---------------------------------------------
# Teamstreamz initial data
#---------------------------------------------
dump = read.csv("C:/Divya/NUS Course Materials/FYP/views_model.csv")
str(dump)
unique(dump$country)

dump %>%
  filter(action == "STREAM_COMPLETE") %>%
  select(deck_id)

dump %>%
  filter(action %in% c("STREAM_COMPLETE", "STREAM_RECIEVED")) %>%
  select(deck_id)

dump %>%
  filter(deck_id == "96d06370") %>%
  select(user_id, deck_id, action)

country_list = dump %>% 
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

country_dump = dump %>%
  filter(country == "IN") %>%
  select(user_id, deck_id)
dim(country_dump)

country_dump %>%
  group_by(user_id) %>%
  summarise(count = n())  %>%
  arrange(desc(count)) %>%
  filter(count > 50 & count < 55)

str(dump)


two_users = dump %>%
  filter(user_id == "0cc79b85" | user_id == "0c3c13e3") %>%
  select(user_id, deck_id, user_action_timestamp)

head(two_users)

two_users %>%
  group_by(deck_id) %>%
  summarise(c = n()) %>%
  arrange(desc(c)) %>%
  filter(c>1)



country_lang_list = dump %>% 
  group_by(country, lang_code) %>%
  summarise(count = n()) %>%
  arrange(country, lang_code, count)

country_city_list = dump %>% 
  group_by(country, city) %>%
  summarise(count = n()) %>%
  arrange(country,city,count)

country_role_list = dump %>% 
  group_by(country, role_id) %>%
  summarise(count = n()) %>%
  arrange(country,role_id,count)

country_deck_list = dump %>% 
  select(country, deck_id) %>%
  group_by(country) %>%
  summarise(deck_count = n_distinct(deck_id))

country_user_list = dump %>% 
  select(country, user_id) %>%
  group_by(country) %>%
  summarise(user_count = n_distinct(user_id))


            
str(SD2011)
original.df = SD2011 %>% dplyr::select(sex, age, edu, marital, income, ls, wkabint)

dim(original.df)
head(original.df)
myseed = 17914709

# Default synthesized dataset1
syn1.df = syn(original.df, seed = myseed)
typeof(syn1.df)
syn1.df

# i dont want to synthesize var5 and var7
syn2.df = syn(original.df, seed = myseed, visit.sequence = c(1,2,6,4,3))
syn2.df

# Parametic methods
syn3.df = syn(original.df, seed = myseed, method = "parametric")
syn3.df

# visit.sequence -> indices of col in an order desired by user to be synthesized.
# if some col need not be synthesized, do not include in the visit.sequence
# i dont want to synthesize var5 and var7 & dont want it to be part of synthesized dataset
syn4.df = syn(original.df, seed = myseed, visit.sequence = c(1,2,6,4,3), drop.not.used = TRUE)
syn4.df

#sex, age, edu, marital, income, ls, wkabint
# marital should not be synthesized from ls, instead use polyreg - polytomous log reg instead of ctree
# do not synthesize income. 
# ls to be synthesized from income.
# edu to be synthesized from income.
# marital to be synthesized from income.
visit.seq = c(1,2,5,6,4,3)
method.set = c("sample","ctree","ctree","polyreg","","ctree","")
syn5.df = syn(data = original.df, seed = myseed, visit.sequence = visit.seq,
              method = method.set, m=0, drop.not.used = FALSE)
syn5.df
# to achieve "marital should not be synthesized from ls"
pred.matrix.corrrected = syn5.df$predictor.matrix
pred.matrix.corrrected["marital","ls"]=0
pred.matrix.corrrected
syn5.corrected.df = syn(data = original.df, seed = myseed, visit.sequence = visit.seq,
                        method = method.set, predictor.matrix = pred.matrix.corrrected)
syn5.corrected.df

# Fill up NA/missing values

library(lubridate)

#=======================================================================
# testing the package
#=======================================================================
employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)
#startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'), origin="1970-01-01")
employee.df <- data.frame(employee, salary)
str(employee.df)

e.syn.df = syn(data=employee.df, seed = 100, m=3)

e.syn.df = syn(data=employee.df, seed = 100, k=20)

e.syn.df$syn

?syn


#=======================================================================
# RECOSYSTEM
#=======================================================================
#https://www.rdocumentation.org/packages/recosystem/versions/0.3#:~:text=recosystem%20is%20an%20R%20wrapper,the%20vignette%20of%20this%20package.
library(recosystem)
set.seed(123)
trainset = system.file("dat","smalltrain.txt",package="recosystem")
testset = system.file("dat", "smalltest.txt", package = "recosystem")

print(trainset)
print(testset)

r=Reco()
opts = r$tune(trainset, opts = list(dim=c(10,20,30), lrate=c(0.05,0.1,0.2),
                                    nthread=1, niter=10))
opts
r$train(trainset, opts = c(opts$min, ntherad = 1, niter = 10))
outfile = tempfile()
r$predict(testset, outfile)
print(read.table(testset, header = FALSE, sep = " ", nrows = 10)$V3)
print(scan(outfile, n=10))

#https://www.kaggle.com/michelblaauw/basic-matrix-factorization-recommender
library(data.table)
library(dplyr)
library(recosystem)

books = fread("C:/Divya/NUS Course Materials/FYP/SampleCode/books.csv")
ratings = fread("C:/Divya/NUS Course Materials/FYP/SampleCode/ratings.csv")

str(books)
head(books)
head(ratings)

m = ratings %>%
  dcast(formula = book_id ~ user_id) %>%
  as.matrix()
head(m)

train_set = data_memory(ratings$user_id, ratings$book_id, rating = ratings$rating)

r = Reco()

# tuning the hyper parameters
opts_1 = r$tune(train_set, opts = list(dim = c(10,20,30), lrate = c(0.1,0.2),
                                       costp_l1 = 0, costq_l1=0,
                                       nthread = 1, niter = 10))

# Now train it
r$train(train_set, opts = opts)

# produce recommendation for userid
userId1 = 100
userId2 = 200
NoOfRecomm = 5

test_set = data_memory(rep(c(userId1),nrow(books)),books$book_id, rating = NULL)
class(test_set)

data.frame(userId = userId1,
           bookId = books$title,
           rating = r$predict(test_set, out_memory())) %>%
  setorder(-rating) %>%
  head(NoOfRecomm)

test_set = data_memory(rep(c(userId2),nrow(books)),books$book_id, rating = NULL)


data.frame(userId = userId2, 
           bookId = books$title, 
           rating = r$predict(test_set, out_memory())) %>%
  setorder(-rating) %>%
  head(NoOfRecomm)

#----------------------------------------------------------------
# question MF
#----------------------------------------------------------------
library(data.table)
library(dplyr)
library(recosystem)
score = fread("C:/Divya/NUS Course Materials/FYP/SampleCode/question.txt")
class(score)
trainset = data_memory(score$V1, score$V2, rating = score$V3)
 model = Reco()
# tuning the hyper parameters
opts_2 = model$tune(trainset, opts = list(dim = c(10,20,30), lrate = c(0.1,0.2),
                                       costp_l1 = 0, costq_l1=0,
                                       nthread = 1, niter = 10))
model$train(trainset, opts = opts_2)
test = data_file("C:/Divya/NUS Course Materials/FYP/SampleCode/test.txt", index1 = TRUE)
outfile = tempfile(tmpdir = "C:/Divya/NUS Course Materials/FYP/SampleCode",
                   pattern = "output", fileext = "txt")
  model$predict(test, out_pred = outfile)
  print(outfile)
  
  print(read.table(outfile, header = FALSE, sep = " ", nrows = 10)$V3)
  print(scan(outfile, n=10))
  
  #----------------------------------------------------------------------------
  library('dplyr')
  library('tidyverse')
  library('lubridate')
  library(ggplot2)
  
  dump = read.csv("C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/user_assessments.csv")
  
  dim(dump)
  # 5878968       9
  str(dump)
  unique(dump$org_id)
  unique(dump$country)
  
  dump %>%
    group_by(country) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  #65 countries
  # TOP 3
  # PK      1413969
  # IN      1295290
  # IR       656436
  
  # PAKISTAN DATA
  pak_sample = dump %>%
    filter(country == "PK") 
  pak_sample = pak_sample[!duplicated(pak_sample),]
  str(pak_sample)
  # 1388250       9
  pak_sample$answer_datetime = ymd_hms(pak_sample$submission_utc_ts)
  pak_sample$answer_date = as.Date(pak_sample$answer_datetime)
  str(pak_sample)
  pak_sample = pak_sample[,-c(1,2,3)]
  str(pak_sample)
  length(unique(pak_sample$masked_user_id))
  #972
  length(unique(pak_sample$question_id))
  #520
  length(unique(pak_sample$question_tags))
  #160
  # find unique question id mapped to tags
  pak_uniq_ques_tag = pak_sample %>%
    group_by(question_id, question_tags) %>%
    summarise(n=n())
  # 752   tag-02540159    50509
  # 752   tag-a37890de    50509
  # 752   tag-f7664328    50509
  
  # how many questions are assigned to 160 unique tags
  pak_qtags_noof_qids = pak_uniq_ques_tag %>%
    group_by(question_tags) %>%
    summarise(n=n())
  # tag-13b3f31a  44
  # tag-2e6be148  26
  # tag-dee1033d  26
  
  ggplot(pak_qtags_noof_qids, aes(x=question_tags, y=n)) + geom_bar(stat = "identity")
  
  # find how many tags to linked to 520 questions
  pak_qids_noof_tags = pak_uniq_ques_tag %>%
    group_by(question_id) %>%
    summarise(n=n())
  # 3     4
  # 186   4
  # 188   4
  
  pak_qids_noof_tags %>%
    filter(n>1) %>%
    ggplot(aes(x=question_id, y=n)) + geom_bar(stat = "identity")
  
  # Find how many times each user has attempted the questions under specific tags
  pak_user_ques_dtl= pak_sample %>%
    group_by(masked_user_id, question_id, question_tags) %>%
    summarise(n=n())
  # 08b0a063   752   tag-02540159    50257
  # 08b0a063   752   tag-a37890de    50257
  # 08b0a063   752   tag-f7664328    50257
  
  # find top users with maximum questions
  top_pak_users  = pak_user_ques_dtl %>%
    group_by(masked_user_id) %>%
    summarise(u_s = sum(n)) 
  # 08b0a063    1307725
  # 53d2140d    3416
  # 7ebac409    3197
  # b5ade8ba    1958
  
  
  top_pak_users %>%
    filter(u_s > 300) %>%
    filter(masked_user_id != "08b0a063") %>%
    ggplot(aes(x=masked_user_id, y=u_s)) + geom_bar(stat = "identity")
  
  # find top question tags with maximum users
  top_pak_qtags = pak_user_ques_dtl %>%
    group_by(question_tags) %>%
    summarise(q_t = sum(n))
  
  top_pak_qids = pak_user_ques_dtl %>%
    group_by(question_id) %>%
    summarise(q_i = sum(n))
  
  # USER LEVEL Analysis
  user_08b0a063 = pak_sample %>%
    filter(masked_user_id == "08b0a063")
  dim(user_08b0a063)
  # user 08b0a063 has 1307725 records in the whole pak dataset
  
  
  user_08b0a063_monthly =  user_08b0a063 %>%
    group_by(monthYear) %>%
    summarise(n=n())
  # September has highest number of usage - 694016
  
  # from 694016 activity records, unique activity on each question is reduced 
  # sep month actual activity data is 700
  user_08b0a063_sep_data = user_08b0a063 %>%
    filter(monthYear == "09-2019" ) %>%
    group_by(answer_date, question_id,question_tags) %>%
    summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
              points_earned = points_earned[which.max(answer_datetime)])
 
  # each day in september he has attempted 26 questions
  # per day usage
  user_08b0a063_sep_daily_data  = user_08b0a063_sep_data %>%
    group_by(answer_date) %>%
    summarise(n=n())
  #-----------------------------------------------------------------
  # FINAL CLEANUP - PAK
  #-----------------------------------------------------------------
  dim(pak_sample)
  # 1388250       9
  final_pak_sample = pak_sample %>%
    group_by(masked_user_id, answer_date, question_id,question_tags) %>%
    summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
              points_earned = points_earned[which.max(answer_datetime)])
  dim(final_pak_sample)
  #  30444     5
  length(unique(final_pak_sample$question_id))
  #520
  length(unique(final_pak_sample$question_tags))
  #160
  length(unique(final_pak_sample$masked_user_id))
  #972
  #-----------------------------------------------------------------
  # FINAL CLEANUP - INDIA
  #-----------------------------------------------------------------
  india_sample = dump %>%
    filter(country == "IN")
  dim(india_sample)
  #  1295290       9
  india_sample = india_sample[!duplicated(india_sample),]
  dim(india_sample)
  # 1290148       9
  india_sample$answer_datetime = ymd_hms(india_sample$submission_utc_ts)
  india_sample$answer_date = as.Date(india_sample$answer_datetime)
  str(india_sample)
  india_sample = india_sample[,-c(1,2,3)]
  str(india_sample)
  dim(india_sample)
  #  1290148       8
  final_india_sample = india_sample %>%
    group_by(masked_user_id, answer_date, question_id,question_tags) %>%
    summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
              points_earned = points_earned[which.max(answer_datetime)])
  dim(final_india_sample)
  #   944137      6
  length(unique(final_india_sample$question_id))
  #921
  length(unique(final_india_sample$question_tags))
  #245
  length(unique(final_india_sample$masked_user_id))
  #8620
  #-----------------------------------------------------------------
  #-----------------------------------------------------------------------------------
  stream_views = read.csv("C:/Divya/NUS Course Materials/FYP/SampleCode/SecondDataSet/views_model.csv")
  dim(stream_views)
  #3253429      13
  
  str(stream_views)
  stream_views %>%
    group_by(country) %>%
    summarise(cnt = n()) %>%
    arrange(desc(cnt))
india_streams = stream_views %>%
  filter(country == "IN")
  
india_streams = india_streams[!duplicated(india_streams),]
str(india_streams)  
india_streams$view_datetime = ymd_hms(india_streams$user_action_timestamp)
india_streams$view_date = as.Date(india_streams$view_datetime)
  
final_india_streams = india_streams %>%
  group_by(masked_user_id, view_date, deck_id,card_id) %>%
  summarise(view_datetime = view_datetime[which.max(view_datetime)])
dim(final_india_streams)
  
  length(unique(final_india_streams$deck_id))
  #744
  length(unique(final_india_streams$masked_user_id))
  #28576
  
  length(unique(final_india_sample$question_id))
  #921
  length(unique(final_india_sample$question_tags))
  #245
  length(unique(final_india_sample$masked_user_id))
  #8620
  
 
  q_india_users = sapply(final_india_sample[,"masked_user_id"], as.character)
  q_india_users = as.data.frame(unique(q_india_users))
  dim(q_india_users)
  s_india_users = sapply(final_india_streams[,"masked_user_id"], as.character)
  s_india_users = as.data.frame(unique(s_india_users))
  dim(s_india_users)
  # common users
  com_users = q_india_users %>% 
    inner_join(s_india_users, by = "masked_user_id")  
  dim(com_users)
  final_india_streams
  
  # if the common users in the stream data set, have deck_ids that are used by other users
  c_user_vector = as.vector(com_users$masked_user_id)
  comm_decks = final_india_streams[final_india_streams$masked_user_id %in% c_user_vector, "deck_id"]
  length(unique(comm_decks$deck_id))
  length(unique(final_india_streams$deck_id))
  
  #---------------------------------------------------------------------
  # Pulse score
  #---------------------------------------------------------------------
  q_summ = final_india_sample %>%
    group_by(masked_user_id, answer_date) %>%
    summarise(n=n())
  
  q_u = final_india_sample %>%
    group_by(masked_user_id, question_id) %>%
    summarise(n=n())
  
    max_q_u = q_u %>% 
      group_by(masked_user_id) %>%
      summarise(s = sum(n))
  
    
    str(final_india_sample)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 1. MONTH-YEAR 
  pak_sample$monthYear = format(as.Date(pak_sample$answer_date), "%m-%Y")
  levels(pak_sample$monthYear) = c("05-2019","06-2019","07-2019","08-2019","09-2019","10-2019","11-2019","12-2019","01-2020","02-2020","03-2020","04-2020","05-2020","06-2020","07-2020")
  str(pak_sample)
  # 1.1. monthly stats
  pak_monthly_stats = pak_sample %>%
    group_by(monthYear) %>%
    summarise(cnt =n()) 
  ggplot(pak_monthly_stats, aes(x=monthYear, y=cnt)) + geom_bar(stat = "identity")
  dim(pak_sample)
  # 1388250       9
  pak_sample = pak_sample[!duplicated(pak_sample),]
  dim(pak_sample)
  #SAME
  # 1.2. User usage
  pak_sep_19_user = pak_sample %>%
    filter(monthYear == "09-2019") 
  
  ggplot(pak_sep_19_user, aes(answer_date)) +
    geom_histogram()
  
  may_19_user_qcnt = pak_sep_19_user %>%
    group_by(answer_date, masked_user_id) %>%
    summarise(qcnt = n())
  
  may_19_user_qcnt %>%
    arrange(desc(qcnt)) %>%
    top_n(20)
  
  #user=897c4b8a has 1157 entries for MAY-2019
  user_897c4b8a_may19 = may_19_user %>%
    filter(masked_user_id == "897c4b8a") %>%
    group_by(answer_date,question_id) %>%
    summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
              points_earned = points_earned[which.max(answer_datetime)])
  
  may_19_user %>%
    filter(masked_user_id == "897c4b8a" & question_id == 11944 & answer_date == "2019-05-12")
  
  may_19_user %>%
    filter(masked_user_id == "897c4b8a" & question_id == 6338 & answer_date == "2019-05-31")
  
  max(may_19_user$no_of_trials)
  may_19_user %>%
    filter(no_of_trials == 19)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  dump %>%
    filter(country == "IN") %>%
    count(distinct(question_tags))
  #245
  
  india_sample = dump %>%
    filter(country == "IN") 
  
india_sample$answer_datetime = ymd_hms(india_sample$submission_utc_ts)
india_sample$answer_date = as.Date(india_sample$answer_datetime)
india_sample = india_sample[,-c(1:3)]
india_sample = india_sample[,-6]
india_sample = india_sample[,-2]

str(india_sample)
dim(india_sample)
india_sample = india_sample[!duplicated(india_sample),]
dim(india_sample)
write.csv(india_sample, "C:/Divya/NUS Course Materials/FYP/india.csv")


length(unique(india_sample$masked_user_id))
#8620
length(unique(india_sample$question_id))
#921


str(india_sample)
# 1. MONTH-YEAR 
india_sample$monthYear = format(as.Date(india_sample$answer_date), "%m-%Y")
monthly_stats = india_sample %>%
  group_by(monthYear) %>%
  summarise(cnt =n()) 

library(ggplot2)
ggplot(monthly_stats, aes(x=monthYear, y=cnt)) + geom_bar(stat = "identity")

#. User usage
may_19_user = india_sample %>%
  filter(monthYear == "05-2019") 

ggplot(may_19_user, aes(answer_date)) +
  geom_histogram()

may_19_user_qcnt = may_19_user %>%
  group_by(answer_date, masked_user_id) %>%
  summarise(qcnt = n())

may_19_user_qcnt %>%
  arrange(desc(qcnt)) %>%
  top_n(20)

#user=897c4b8a has 1157 entries for MAY-2019
user_897c4b8a_may19 = may_19_user %>%
  filter(masked_user_id == "897c4b8a") %>%
  group_by(answer_date,question_id) %>%
  summarise(no_of_trials = no_of_trials[which.max(answer_datetime)], 
            points_earned = points_earned[which.max(answer_datetime)])

may_19_user %>%
  filter(masked_user_id == "897c4b8a" & question_id == 11944 & answer_date == "2019-05-12")

may_19_user %>%
  filter(masked_user_id == "897c4b8a" & question_id == 6338 & answer_date == "2019-05-31")

max(may_19_user$no_of_trials)
may_19_user %>%
  filter(no_of_trials == 19)

may_19_user %>%
  filter(masked_user_id == "47b5f2c4" & answer_date == "2019-05-29")

max(dump$no_of_trials)

unique(india_sample$org_id)
str(india_sample)
#----------------------------------------
sum(is.na(india_sample))
#873499
sum(is.na(dump$masked_user_id))
# user ids are not NAN
sum(is.na(dump$question_id))
# question ids are not NAN
sum(is.na(dump$no_of_trials))
# number of trials are not NAN
sum(is.na(dump$points_earned))
# points earned are not NAN
sum(is.na(dump$role_id))
# 1395383 NULLS
sum(is.na(dump$org_id))
# org id are not NAN
#----------------------------------------
length(unique(india_sample$org_id))
# unique companys 6
length(unique(india_sample$role_id))
# unique role 10
length(unique(india_sample$question_id))
# unique #question ids = 921
length(unique(india_sample$masked_user_id))
# unique users 8620
length(unique(india_sample$no_of_trials))
# number of trials = 20
length(unique(india_sample$points_earned))
# points earned = 3
length(unique(india_sample$question_tags))
# question tags 245
#----------------------------------------
str(india_sample)


library(lubridate)
q_cnt = india_sample %>%
  group_by(question_id) %>%
  summarise(count = n()) %>%
  arrange(question_id, desc(count))
india_sample$answer_datetime = ymd_hms(india_sample$submission_utc_ts)
#ymd_hms("2010-12-13 15:30:30")
dim(india_sample)

install.packages('xlsx')
library('xlsx')
write.xlsx(india_sample, "C:/Divya/NUS Course Materials/FYP/india.xlsx", sheetName = "india", col.names = TRUE, row.names = TRUE, append = FALSE)

  min(india_sample$answer_datetime)
#"2019-05-12 00:08:17 UTC"
max(india_sample$answer_datetime)
#"2020-07-20 18:51:34 UTC"

sample = india_sample %>%
  filter(question_id == 11502) %>%
  select(masked_user_id, question_id, no_of_trials, points_earned, answer_datetime) %>%
  arrange(answer_datetime)

x =sample %>%
  group_by(masked_user_id) %>%
  summarise(c = n()) %>%
  arrange(masked_user_id, desc(c))

i = sample %>%
  filter(masked_user_id == "ea1fe289") %>%
  select(masked_user_id, question_id, no_of_trials, points_earned, answer_datetime) %>%
  arrange(answer_datetime)
dim(i)

d = i %>%
  distinct(masked_user_id, question_id, no_of_trials, points_earned, answer_datetime)


india_sample %>%
  filter(no_of_trials == 9) %>%
  select(masked_user_id, question_id, no_of_trials, points_earned, answer_datetime)


unique(india_sample$no_of_trials)
# 1  2  4  3  5  8  9  6  7  0 10 11 18 12 15 16 19 14 13 28

india_sample %>%
  filter(no_of_trials == 28)%>%
  select(masked_user_id, question_id, no_of_trials, points_earned, answer_datetime)

z =india_sample %>%
  filter(question_id == 18778) %>%
  select(masked_user_id, question_id, no_of_trials, points_earned, answer_datetime) %>%
  arrange(answer_datetime)


