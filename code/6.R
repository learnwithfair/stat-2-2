# Problem-6: Test the hypothesis that the mean systolic blod pressure
of healdy subject(status-0) and subject with hypertension(status-1)
are equal, have do= 0. The dataset contains n1= 25 subject with
status-0 nad n2= 30 with status-1.
(120, 115, 94, 118, 111, 102, 102, 131, 104, 107, 115, 139, 115, 113, 114,
105, 115, 134, 109, 109, 93, 118, 109, 106, 125)
(150, 142, 119, 127, 141, 149, 144, 142, 149, 161, 143, 140 ,
148, 149, 141, 146, 159, 152, 135, 134, 161, 130, 125, 141, 148 ,153,
145, 137, 147, 169)


############## Solution #############

Ho: mue1=mue2
H1: mue1 not equal mue2
data1<- c(120, 115, 94, 118, 111, 102, 102, 131, 104, 107, 115, 139, 115, 113, 114,
105, 115, 134, 109, 109, 93, 118, 109, 106, 125)
n1<- length(data1)
n1
s1<- sd(data1)
s1
x_ber1<- mean(data1)
x_ber1
data2<- c(150, 142, 119, 127, 141, 149, 144, 142, 149, 161, 143, 140 ,
148, 149, 141, 146, 159, 152, 135, 134, 161, 130, 125, 141, 148 ,153,
145, 137, 147, 169)
n2<- length(data2)
n2
s2<- sd(data2)
s2
x_ber2<- mean(data2)
x_ber2
##########Checking the normality ########
par(mfrow=c(1,2))
qqnorm(data1,main="QQ plot of status-0")
qqline(data1)
qqnorm(data2,main="QQ plot of status-1")
qqline(data2)
###########Checking the variance equal or not ###########
boxplot(list(status_0=data1, status_1=data2), col="red")
ratio_sd<- s1/s2
ratio_sd # 1.018009 is close to 1, that's why we can say that they have equal variances.
alpha<- 0.05
t_tab1<- qt(alpha/2, n1+n2-2)
t_tab1 # -2.005746
t_tab2<- qt(1-alpha/2, n1+n2-2)
t_tab2 #2.005746
sp<- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
sp
t_cal<- (x_ber1 - x_ber2)/sqrt(sp^2*((1/n1)+(1/n2)))
t_cal #-10.46787
#Comments: t_cal= -10.46787is not fall in between the Critical value -2.005746 to 2.005746,
so the Null hypothsis(Ho) is rejected.
###########using p-value##############
p_value<- 2*pt(t_cal, n1+n2-2)
p_value #2.793985e-15 that is < 0.05, so Ho is rejected.
########## using t.test function ############
t.test(data1, data2, alternative="two.sided", mu=0, paired= FALSE, conf.level=0.95)
      
