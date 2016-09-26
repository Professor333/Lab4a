#For dataset 1, assignment
setwd("C:/Users/Ryan/Development/R/BIO 614/Lab4a/Lab4a")
a=read.csv("lab4data1.csv", header = T)
attach(a)
names(a)


as.factor(size1)
size12<-factor(size1,levels=c("10","20","30","40",">50"))

library(car)
library(ggplot2)
library(pwr)

sample.size.a= aggregate(.~size1, data=a, length)
sample.size.a=sample.size.a[order(sample.size.a$size1),]

boxplot(rich1~size12,xlab="Sizes",ylab="Species")

out11=aov(rich1~size12)
summary(out11)

qqPlot(out11$resid,ylab="Z value")
shapiro.test(out11$resid)

par(mfrow = c(2, 2))  
plot(out11)

bartlett.test(rich1~size12)

anova(out11)

square.between= function(x) (mean(x) - grand.mean.a)^2
group.mean.a<- aggregate(.~size1, data=a, mean)
as.factor(group.mean.a$size1)
group.mean.a$size1<-factor(group.mean.a$size1,levels=c("10","20","30","40",">50"))
group.mean.a[order(group.mean.a$size1), ]

grand.mean.a<- sum(group.mean.a[,2]*sample.size.a[,2])/sum(sample.size.a[,2])



ss.between=sum(sample.size.a[,2]*aggregate(.~size1,data=a,square.between)[,2])

square.within= function(x) sum((x-mean(x))^2)
ss.within= sum(aggregate(.~size1, data=a, square.within)[,2])
ss.total= ss.between + ss.within

N=sum(sample.size.a[,2])
k=5

cohen.f= sqrt(ss.between/N/ (ss.within/(N-k))) #Standardized by degrees of freedom N-k


#SS from anova
output1=lm(rich1~size12)
SSG=anova(output1)[1,2]
SSE=anova(output1)[2,2]
cohen.f=SSG/SSE

cohen.f2=sqrt((SSG/N) / (SSE/(N-k))) #Equivalent to by hand




#For dataset 2
b=read.csv("lab4data2.csv", header = T)
attach(b)
names(b)


as.factor(size2)
size22<-factor(size2,levels=c("10","20","30","40",">50"))

sample.size.b= aggregate(.~size2, data=b, length)
sample.size.b=sample.size.b[order(sample.size.b$size2),]

boxplot(rich2~size22,xlab="Sizes",ylab="Species")

out22=aov(rich2~size22)
summary(out22)

qqPlot(out22$resid,ylab="Z value")
shapiro.test(out22$resid)

par(mfrow = c(2, 2))  
plot(out22)

bartlett.test(rich2~size22)

anova(out22)

square.between= function(x) (mean(x) - grand.mean.b)^2
group.mean.b<- aggregate(.~size2, data=b, mean)
as.factor(group.mean.b$size2)
group.mean.b$size2<-factor(group.mean.b$size2,levels=c("10","20","30","40",">50"))
group.mean.b[order(group.mean.b$size2), ]

grand.mean.b<- sum(group.mean.b[,2]*sample.size.b[,2])/sum(sample.size.b[,2])



ss.between=sum(sample.size.b[,2]*aggregate(.~size2,data=b,square.between)[,2])

square.within= function(x) sum((x-mean(x))^2)
ss.within= sum(aggregate(.~size2, data=b, square.within)[,2])
ss.total= ss.between + ss.within

N=sum(sample.size.b[,2])
k=5

cohen.f= sqrt(ss.between/N/ (ss.within/(N-k))) #Standardized by degrees of freedom N-k


#SS from anova
output1=lm(rich2~size22)
SSG=anova(output1)[1,2]
SSE=anova(output1)[2,2]
cohen.f=SSG/SSE

cohen.f2=sqrt((SSG/N) / (SSE/(N-k))) #Equivalent to by hand


