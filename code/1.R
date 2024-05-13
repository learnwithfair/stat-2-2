#Problem-01: Point estimation and Interval estimation
#Where 30 students quiz test marks, total marks 50
#Find out the point estimate of population mean and
#interval estimate of population mean,, Population size N=30 and Sample size n=10.
data<-c(2,4,3,23,25,27,28,13,15,16,20,14,35,33,32,21,35,40,42,22,33,13,17,20,25,29,27,40,38,31)
length(data)
#for cheak normality
qqnorm(data)
qqline(data)
set.seed(125)
x<-sample(data,10,replace=TRUE)
x
y<-mean(x)
y #point estimate for mean = 21.8
sigma= sd(data)
sigma
#Interval estimate
qnorm(0.025,0,1) # -1.96
#lower class interval
l= y-(((1.96)*(sigma))/sqrt(10))
l
#Upper class interval
u= y+(((1.96)*(sigma))/sqrt(10))
u
#95% confidance interval for population mean is (14.97, 28.63)
#sample size determination
# There are two ways
# 1). We know that the range is four times of standerd deviation(signam).
# so we will get the sigma value if the range divided by 4.
# 2).
n= ((2*1.96*sigma)/2)^2

n # Probable Sample size will be 465.8536 ~ 466
pnorm(-1.96,0,1) #0.025
#SAMPLING DISTIBUTION FOR MEAN
choose(30,10) # 30045015
set.seed(125)
a<-rep(0,3004)
for(j in 1:3004){
a[j]<-mean(sample(data,10,replace=TRUE))}
mean(a) #Expected value E(x ber)= 24.12693
mean(data) #population mean
bais=mean(a)-mean(data)
bais #bias is 0.02693076 that is almost zero, so Sampling mean or (x ber) is an unbiased estimator of
population mean µ.
hist(a)
qqnorm(a)
qqline(a)

######### Efficiency Check ########
# sampling distribution for median
b<-rep(0,3004)
for(i in 1:3004){
b[i]<-median(sample(data,10,replace=TRUE))}
bias = median(b)-median(data)


v1 <- sum((a-mean(data))^2)/length(a)
v1
v2 <-sum((b0median(data))^2)/lenght(b)
v2

#Comment: Sampling mean is an unbiased estimator of population mean.
#####################################################################################
###### The extra part which is no needed for this code ############
#####################################################################################
curve(dnorm(x), xlim=c(-3.5, 3.5), ylab="density", main="Standard Normal Distribution")
dnorm(x=0)
curve(pnorm(x), xlim=c(-3.5, 3.5), ylab="probability", main="Standard Normal Cumulative Distribution")
ami<-function(x){
(1/(sqrt(2*pi)))*exp(-0.5*x^2)
}
ami(-1.96) #dnorm(-1.96)=ami(-1.96)
integrate(ami, lower=-Inf, upper=3)
################Chi-square distrubution#########
curve(dchisq(x,df=3), xlim=c(0,10), ylim=c(0,1), col="red", main="PDF of chi-square distribution")
###### t distribution#####
curve(dt(x,df=3), xlim=c(-4,4), col="red", main="PDF of t distribution")
b<-rt(100,3)
b
hist(b)