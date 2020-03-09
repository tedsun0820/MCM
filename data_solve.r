library(readr)
library(ggplot2)
library(randomForest)
library(lubridate)
library(grid)    
library(MASS)
library(neuralnet)
library(dplyr)
library(nnet)
microwave = read_csv("newmicrowave.csv")
microwave$type = "microwave"
hair_dryer = read_csv("newhair_dryer.csv")
hair_dryer$type = "hair_dryer"
pacifier = read_csv("pacifiernew.csv")
pacifier$type = "pacifier"
#日期转化
microwave$review_date = as.Date(microwave$review_date,'%m/%d/%Y')
hair_dryer$review_date = as.Date(hair_dryer$review_date,'%m/%d/%Y',tz = "NZ")
pacifier$review_date = as.Date(pacifier$review_date,'%m/%d/%Y')
#微波炉的时间序列
start = min(microwave$review_date)
truestart1 = as.Date("2004-05-01")
end = max(microwave$review_date)
micro_time = data.frame('period' = rep(1,135))
for (i in 1:135) {
  truestart1 = truestart1 %m+% months(1)
  truestart2 = truestart1 %m+% months(1) -1
  micro_time$period[i] = as.character(truestart1)
  zanding = microwave[which(microwave$review_date>=truestart1 & microwave$review_date<=truestart2),]
  micro_time$comments[i] = nrow(zanding)
  micro_time$star[i] = sum(zanding$star_rating)/nrow(zanding)
}
micro_time$type = "microwave"
plot(as.Date(micro_time$period),micro_time$comments)
#吹风机的时间序列
start = min(hair_dryer$review_date)
truestart1 = as.Date("2002-02-01")
end = max((hair_dryer$review_date))
hair_dryer_time = data.frame('period' = rep(1,162))
for (i in 1:162) {
  truestart1 = truestart1 %m+% months(1)
  truestart2 = truestart1 %m+% months(1) - 1
  hair_dryer_time$period[i] = as.character(truestart1)
  zanding = hair_dryer[which(hair_dryer$review_date>=truestart1 & hair_dryer$review_date<=truestart2),]
  hair_dryer_time$comments[i] = nrow(zanding)
  hair_dryer_time$star[i] = sum(zanding$star_rating)/nrow(zanding)
}
hair_dryer_time$type = 'hair_dryer'
#奶嘴的时间序列
start = min(pacifier$review_date)
truestart1 = as.Date("2003-03-01")
end = max((pacifier$review_date))
pacifier_time = data.frame('period' = rep(1,149))
for (i in 1:149) {
  truestart1 = truestart1 %m+% months(1)
  truestart2 = truestart1 %m+% months(1) - 1
  pacifier_time$period[i] = as.character(truestart1)
  zanding = pacifier[which(pacifier$review_date>=truestart1 & pacifier$review_date<=truestart2),]
  pacifier_time$comments[i] = nrow(zanding)
  pacifier_time$star[i] = sum(zanding$star_rating)/nrow(zanding)
}
pacifier_time$type = "pacifier"
#时间序列总图
total = rbind(micro_time,hair_dryer_time,pacifier_time)
ggplot(data = total,aes(x =as.Date(period),y = comments,color = type)) +
  geom_point() + xlab("Time") + ylab("Number of Comments")+theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))
#产品总数及均分
#微波炉
product = cbind(unique(microwave$product_parent))
n = nrow(product)
microwave_comments = data.frame("product" = product)
for (i in 1:n) {
  name = microwave_comments$product[i]
  microwave_comments$comments[i] = 
    nrow(microwave[which(microwave$product_parent==name),])
  microwave_comments$star[i] = 
    sum(microwave[which(microwave$product_parent==name),8])/microwave_comments$comments[i]
}
microwave_comments$type = "microwave"
ggplot(data =microwave_comments, aes(star,comments)) + geom_point()
ggplot(data =microwave_comments, aes(comments)) + geom_bar()
#吹风机
product = cbind(unique(hair_dryer$product_parent))
n = nrow(product)
hair_dryer_comments = data.frame("product" = product)
for (i in 1:n) {
  name = hair_dryer_comments$product[i]
  hair_dryer_comments$comments[i] = 
    nrow(hair_dryer[which(hair_dryer$product_parent== name),])
  hair_dryer_comments$star[i] = 
    sum(hair_dryer[which(hair_dryer$product_parent==name),8])/hair_dryer_comments$comments[i]
}
hair_dryer_comments$type = "hair_dryer"
ggplot(data = hair_dryer_comments, aes(star,comments)) + geom_point()
ggplot(data = hair_dryer_comments, aes(comments)) + geom_bar()

#奶嘴
product = cbind(unique(pacifier$product_parent))
n = nrow(product)
pacifier_comments = data.frame("product" = product)
for (i in 1:n) {
  name = pacifier_comments$product[i]
  pacifier_comments$comments[i] = 
    nrow(pacifier[which(pacifier$product_parent== name),])
  pacifier_comments$star[i] = 
    sum(pacifier[which(pacifier$product_parent == name),8])/pacifier_comments$comments[i]
}
pacifier_comments$type = "pacifier"
ggplot(data = pacifier_comments, aes(star,comments)) + geom_point()

#总图
total = rbind(hair_dryer_comments,microwave_comments,pacifier_comments)
ggplot(data = total, aes(star,comments,color = type),color = type) + geom_point() + scale_fill_manual(values=alpha(c("#6495ED","#FFA500","#FF4500"), 0.5))+
theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))+xlab("Star") + ylab("Comments")
cor(total$star,total$comments) 

#评分的分布图
total = c(hair_dryer$star_rating,microwave$star_rating,pacifier$star_rating)
type = c(hair_dryer$type,microwave$type,pacifier$type)
total = data.frame("star_rating"=total,"type"=type)
ggplot(data =total,aes(star_rating,fill =type),color = type) + 
  geom_bar(stat='bin',position="dodge",bins = 5)
ggplot(data =total,aes(factor(type),fill =factor(star_rating))) + 
  geom_bar(stat = 'count', position = 'dodge')+ xlab("Product_type")+ylab("Count")+
  theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))

  #随机森林(神经网络)处理评分与评论的关系

hair_dryer = as.data.frame(hair_dryer)
microwave = as.data.frame(microwave)
pacifier = as.data.frame(pacifier)
hair_dryer$product_category = 1
microwave$product_category = 2
pacifier$product_category = 3
total = rbind(hair_dryer,microwave,pacifier)

lo = glm(as.factor(star_rating)~as.factor(product_category) + helpful_votes +total_votes+
           vine + verifie_purchaes + polarity + subjectivity,data = total,family = binomial("logit"))
lo = multinom(as.factor(star_rating)~as.factor(product_category) + helpful_votes +total_votes+
                  vine + verifie_purchaes + polarity + subjectivity,data = total)
summary(lo)
lo <- step(lo)
summary(lo)
#评论与时间的关系
timeline = function(hair_dryer){
  zanding = data.frame("id" = unique(hair_dryer$product_id))
  n = nrow(zanding)
  #选出评论较多的
  for (i in 1:n) {
    name = as.character(zanding$id[i])
    zanding$num[i] = nrow(hair_dryer[which(hair_dryer$product_id == name),])
  }
  xuanchu = zanding[which(zanding$num>50),]
  n = nrow(xuanchu)
  out = list()
  truename = c()
  for (i in 1:n) {
    name =  as.character(xuanchu$id[i])
    a = hair_dryer[which(hair_dryer$product_id == as.character(xuanchu$id[i])),]
    a = data.frame(a$product_id,a$star_rating,a$helpful_votes,a$total_votes,a$verifie_purchaes,a$vine,a$type,a$review_date,a$polarity,a$subjectivity)
    truename = c(truename,name)
    assign(name,a)
    }
  out = list(lapply(truename, get))
  return(out)
}
hair_dryer_timeline = timeline(hair_dryer)
timeline = function(microwave){
  zanding = data.frame("id" = unique(microwave$product_id))
  n = nrow(zanding)
  #选出评论较多的
  for (i in 1:n) {
    name = as.character(zanding$id[i])
    zanding$num[i] = nrow(microwave[which(microwave$product_id == name),])
  }
  xuanchu = zanding[which(zanding$num>100),]
  n = nrow(xuanchu)
  out = list()
  truename = c()
  for (i in 1:n) {
    name =  as.character(xuanchu$id[i])
    a = microwave[which(microwave$product_id == as.character(xuanchu$id[i])),]
    a = data.frame(a$product_id,a$star_rating,a$helpful_votes,a$total_votes,a$verifie_purchaes,a$vine,a$type,a$review_date)
    truename = c(truename,name)
    assign(name,a)
  }
  out = list(lapply(truename, get))
  return(out)
}



microwave_timeline = timeline(microwave)
timeline = function(pacifier){
  zanding = data.frame("id" = unique(pacifier$product_id))
  n = nrow(zanding)
  #选出评论较多的
  for (i in 1:n) {
    name = as.character(zanding$id[i])
    zanding$num[i] = nrow(pacifier[which(pacifier$product_id == name),])
  }
  xuanchu = zanding[which(zanding$num>100),]
  n = nrow(xuanchu)
  out = list()
  truename = c()
  for (i in 1:n) {
    name =  as.character(xuanchu$id[i])
    a = pacifier[which(pacifier$product_id == as.character(xuanchu$id[i])),]
    a = data.frame(a$product_id,a$star_rating,a$helpful_votes,a$total_votes,a$verifie_purchaes,a$vine,a$type,a$review_date)
    assign(name,a)
    truename = c(truename,name)
  }
  out = list(lapply(truename, get))
    return(out)
}


#时序图 
xuanze = B00132ZG3U
start = min(xuanze$a.review_date)
truestart1 = as.Date("2008-10-01")
end = max((xuanze$a.review_date))
pacifier_time = data.frame('period' = rep(1,82))
need = data.frame("comments" = 0,"positive" = rep(0,82),"negative" = rep(0,82),"goodstar" =rep(0,82),"badstar" =rep(0,82),"period" = rep(0,82),"total" = 0)
for (i in 1:82) {
  truestart1 = truestart1 %m+% months(1)
  truestart2 = truestart1 %m+% months(1) - 1
  need$period[i] = as.character(truestart1)
  zanding = xuanze[which(xuanze$a.review_date>=truestart1 & xuanze$a.review_date<=truestart2),]
  need$goodstar[i] = nrow(zanding[which(zanding$a.star_rating>=4),])
  need$badstar[i] = nrow(zanding[which(zanding$a.star_rating<=2),])
  need$positive[i] = sum(zanding[which(zanding$a.polarity>0),]$a.polarity)*10
  need$negative[i] = sum(zanding[which(zanding$a.polarity<0),]$a.polarity)*10
  need$total[i] = need$goodstar[i] - need$badstar[i]
  need$comments[i] = nrow(zanding)
}
plot()

TLCC = function(x){
  n = nrow(x)
  cor = data.frame("goodstar comments" = rep(0,10),"bad comment" = rep(0,10),
                   "total comments" = rep(0,10),"good posit" = rep(0,10),
                   "bad posit" = rep(0,10),"bad neg" = 0,lag = 0)
  for (i in 1:10) {
    cor$goodstar.comments[i] =  cor(x$goodstar[1:(n-i)],x$comments[(1+i):n])
    cor$bad.comment[i] = cor(x$badstar[1:(n -i)],x$comments[(1+i):n])
    cor$total.comments[i] = cor(x$total[1:(n -i)],x$comments[(1+i):n])
    cor$good.posit[i] = cor(x$goodstar[1:(n -i)],x$positive[(1+i):n])
    cor$bad.posit[i] = cor(x$badstar[1:(n -i)],x$positive[(1+i):n])
    cor$bad.neg[i] = cor(x$badstar[1:(n -i)],x$negative[(1+i):n])
    cor$lag[i] = i
  }
}
ggplot(data = need,aes(x = period,group = 1,)) + 
    geom_line(aes(y = total),color = "cyan",size =1.5)+
  geom_line(aes(y = goodstar),color = "gray",size = 1.5)+
  geom_line(aes(y = positive),color= "pink",size = 1.5)+theme(axis.text.x = element_blank())+
  geom_line(aes(y = comments),color = "green",size = 1.5) + ylab("") + xlab("Time")+guides()+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))
  
plot(cor$goodstar.comments,type = "h",xlab = "Lag",
     ylab = "Cor(High_star, Total_review)",cex.lab = 1.5)
plot(cor$good.posit,type = "h",xlab = "Lag",
     ylab = " Cor(High_star, Positive_review)",cex.lab = 1.5)
plot(cor$bad.neg,type = "h",xlab = "lag",cex.lab = 1.5,
     ylab = "Cor(Low_star, Negative_review)")
##胡扯
n = nrow(need)
u  = 1
k = 10
w = 0.4
p = 0.5
need$guoqu = 0
need$pois = 0
need$influence = (need$positive - need$negative)/10
for (i in 2:n) {
  lamda = p*u
  for (j in 1:(i-1)) {
    lamda = lamda + (1-p)*k*w*exp(-w*(i-j))*need$influence[j]
    print(lamda)
  }
  need$pois[i] = rpois(1,lamda)
  need$guoqu[i] = lamda
}
ggplot(data = need,aes(x = period,group = 1,))+
  geom_line(aes(y = comments),color = "#00BFC4",size = 1.5)+
  geom_point(aes(y = comments),color= "cyan")+
  geom_line(aes(y = pois),color="#F8766D",size = 1.5 )+
  geom_point(aes(y = pois),color = "pink")+ ylab("") + xlab("Time")+
  theme(axis.text.x = element_blank())+ theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))
all = read.csv("all.csv")
lm = lm(d~polarity+subjectivity	+ total_votes	+ verifie_purchaes +	vine,data = all)
rm = randomForest(d~polarity+subjectivity	+ total_votes	+ verifie_purchaes +	vine,
                  data = all,
               ntree =500,
               mtry=3,
               importance=TRUE ,
               proximity=TRUE)
