# Load packages
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)
library(readxl)


# p(1) = 0.5, p(50) = 0.95, k = 0.0588
# p(1) = 0.5, p(30) = 0.95, k = 0.1015
# p(100) = 0.5, k=0.029
Q=5000
k=0.0005

# Populate penalty scores for the total 100 questions in question pool
penalty_set = array(0.5, dim=c(Q, 1, 1))
for(i in 2:length(penalty_set)) {
  penalty_set[i] = 1/(1 + exp((-k)* (i-1)))
}
for(i in 2:length(penalty_set)) {
  print(i)
  print(penalty_set[i])
}

log(0.1)
(-2.3)/(-49)

#p(100) = 0.95
# 0.95 = 1 / (1 + e^-99k)
# 0.95 +  0.95e^-99k = 1
# 0.95e^-99k = 0.05
# e^-99k = 0.05/0.95
# 0.05/0.95 = 0.0526
# -99k = log(0.0526)
# log(0.0526)
2.9450/99



## Cumulative penalty score for question pool
Q_penalty_score = sum(penalty_set)/Q

data = read_excel("C:/Divya/NUS Course Materials/FYP/FYP_Scribble_Sheet.xlsx", sheet = "DiffEasyComp")
s_data = as.data.frame(data)
#get list of all users
users = unique(s_data$USER_ID)
print(users)

class(s_data)
head(s_data)
str(s_data)


daily_pulse_score = data.frame(date = as.Date(character()), userid = character(), pulseScore = double())
names(daily_pulse_score) = c("date", "userid", "pulseScore")

for(i in (1:length(users))) {
  current_user = as.data.frame(filter(s_data, USER_ID == users[i]))
  dates = sort(unique(as.Date(current_user$DATE)))
  for (d in (1:length(dates))) {
    count = 1
    score=0
    m = as.data.frame(filter(current_user, as.Date(DATE) <= as.Date(dates[d])))
    #print(m)
    for (row in 1:nrow(m)) {
      score = score + (m[row,"CORRECT"] * m[row,"DIFF_LVL"] * penalty_set[count])
      #print(count)
      #print(score)
      count = count + 1
    }
    print("---------------------------------")
    
    ps = (score/(count-1))/Q_penalty_score
    print(ps)
    print(users[i])
    print(dates[d])
    i_score = data.frame(date=as.Date(dates[d]), userid=users[i], pulseScore= ps)
    daily_pulse_score = rbind(daily_pulse_score, i_score)
    print("---------------------------------")
  }
  
}


# Have to compute Pulse Score every day at User level
# FORMULA = [SUM {I(n) * d(n) * p(n) * a(n)}/ N]/[SUM {p(n)} / Q]
# Q = 200, a(n)=1 (assessment mode), a(n)=0.5 (learning mode)
# (1) Compute the p(n) for 1 to 200 questions, store in penaltySet

print(exp(1))
print(log(1))
print(log(2.718282))
print(log(0.0526315)) #-2.94444
print(log(0.0526315)/(-50))
print(-2.94444/-50)



#tqdata = t(qdata)
#dim(tqdata)

#test = tqdata[-1,]
#print(tqdata["Q11","ES"])

#tqdata = tqdata[-1,]

#print(floor(runif(50, min=1, max=50)))
print(sample(1:50, 50, replace=F))
print(sample(51:100, 50, replace=F))

##print(tqdata["Q2",])
#print(ncol(tqdata))

#c = tqdata["Q1",]
#c1 = colnames(tqdata["Q1",])

#dim(tqdata)
#k = arrayInd(1.00000000, dim(tqdata))
#rownames(tqdata)[k[,1]]
#colnames(tqdata)[k[,1]]

#for(i in 1:nrow(tqdata)) {
#  mat[]
#  for(j in 1:ncol(tqdata)) {


#qdf = as.data.frame(tqdata)
#print(nrow(qdf))
#print(rownames(qdf))
#print(colnames(qdf))

# for (i in 1:nrow(qdf)) {
#   print(rownames(qdf)[i])
# }
# 
# str(qdf)
# qdf$T1 = as.numeric(as.character(qdf$T1))
# qdf$T2 = as.numeric(as.character(qdf$T2))
# qdf$T3 = as.numeric(as.character(qdf$T3))
# qdf$T4 = as.numeric(as.character(qdf$T4))
# qdf$T5 = as.numeric(as.character(qdf$T5))
# qdf$T6 = as.numeric(as.character(qdf$T6))
# qdf$T7 = as.numeric(as.character(qdf$T7))
# qdf$T8 = as.numeric(as.character(qdf$T8))
# qdf$T9 = as.numeric(as.character(qdf$T9))
# qdf$T10 = as.numeric(as.character(qdf$T10))
# qdf$T11 = as.numeric(as.character(qdf$T11))
# qdf$T12 = as.numeric(as.character(qdf$T12))
# qdf$T13 = as.numeric(as.character(qdf$T13))
# qdf$T14 = as.numeric(as.character(qdf$T14))
# qdf$T15 = as.numeric(as.character(qdf$T15))
# qdf$Total = as.numeric(as.character(qdf$Total))
# qdf$DS = as.numeric(as.character(qdf$DS))
# qdf$ES = as.numeric(as.character(qdf$ES))
# 
# # question -> list of tags
# tags = c()
# qlist = list()
# print(tags)
# print(qlist)
# for (i in 49:nrow(qdf)) {
#   print(rownames(qdf)[i])
#   for (j in 1:ncol(qdf[i,])) {
#     if(qdf[i,j] != 0) {
#       print(colnames(qdf[i,])[j]) 
#       tags = c(tags, colnames(qdf[i,])[j])
#     }
#     qlist = list(rownames(qdf)[i], tags)
#   }
# }
# 
# print(qlist)
# mylist = list(question="Q1", tags = c("T1","T10","T11"), num=3, DS=.6, ES=.4)
# print(mylist$tags)
