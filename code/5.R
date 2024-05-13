###problem-05: In order to find out whether children with
#choronic diarrhea have the same average hemoglobin
#level(Hb) that is normally seen in healthy children in the same area
#, a random sample of 10 children with chonic diarrhea are selected and there
# Hb levels <g/dl) are obtained as follows:
# 12.3, 11.4, 14,2, 15.3, 14.8, 13.8, 11.1,15.1,15.8,13.2
#another random sample of 12 children with chonic diarrhea are
# 11.1, 17.2, 13.4, 15.2, 14.1, 13.0, 12.5, 11.5, 12.7, 14.5, 15.3, 14.0
# is there any differance between in mean Hb label between the two group of children???
##############Solution##############
#Ho: mue1=mue2
#H1: mue not equal mue2
level<- 0.05
alpha<- 0.05/2
alpha #0.025
data1<- c(12.3, 11.4, 14.2, 15.3, 14.8, 13.8, 11.1,15.1,15.8,13.2)
n1<- length(data1)
n1
s1<- sd(data1)
s1
x_ber1<- mean(data1)
x_ber1
data2<- c(11.1, 17.2, 13.4, 15.2, 14.1, 13.0, 12.5, 11.5, 12.7, 14.5, 15.3, 14.0)
n2<- length(data2)
n2
s2<- sd(data2)
s2
x_ber2<- mean(data2)
x_ber2
t_tab1<- qt(alpha, n1+n2-2)
t_tab1 #-2.085
t_tab2<- qt(1-alpha, n1+n2-2)
t_tab2 #2.085
####### Checking the variance equal or not ##########
boxplot(list(sample_1=data1, sample_2=data2), col="red")
#If the middle line of both boxplot are very colse then is indicate equal variances.
ratio_sd<- s1/s2
ratio_sd # 0.961985 is close to 1, that's why we can say that they have equal variances.
sp<- sqrt((((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2))
sp
t_cal<- (x_ber1 - x_ber2)/sqrt(sp^2*((1/n1)+(1/n2)))
t_cal # -0.01150547
#Comments: Ho is accepted, since t_cal= -0.0137 fall between the t_tab value of -2.085 to 2.085
######using p-value###
p_value<- 2*pt(t_cal, n1+n2-2)
p_value # 0.9891
#Comments: since p_value= 0.9891> 0.05, so Ho is accepted.
######### 95% Confidance Interval #########
CI<-c((x_ber1-x_ber2)+(t_tab1*sp*sqrt((1/n1)+(1/n2))), (x_ber1-x_ber2)+t_tab2*sp*sqrt((1/n1)+(1/n2)))
CI # -1.519183 1.502516
# -1.519183 Cofidance interval 95% lower
# 1.502516 Cofidance interval 95% upper
######### Using t.test function ##########
?t.test
t.test(data1, data2, alternative="two.sided", mu=0, paired= FALSE, conf.level=0.95)
######################## For Another Alternative hypothesis #############
# Ho: mue1=mue2
#H1: mue1>mue2
t_cal # -0.0137
?qt
t_tabx<- qt(0.05, n1+n2-2, lower.tail=FALSE)
t_tabx #1.724718
#Comments: Ho is accepted, since t_cal<t_cal.
########### Using P-value ##########
p_valuex<- pt(t_cal, n1+n2-2)
p_valuex # 0.4945988
#Comments: Ho is accepted, since p-valuex= 0.4945988> 0.05