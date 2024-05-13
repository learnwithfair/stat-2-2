########################## Problem-04 ######################
# Hb levels <g/dl) are obtained as follows:
# 12.3, 11.4, 14.2, 15.3, 14.8, 13.8, 11.1,15.1,15.8,13.2#less than of the normal value of 14.6 (g/dl)?
# Test at 0.01 level of significance.
# Draw a boxplot and normal plot for this data and comments.
# Ho: mue = 14.6
# H1: mue < 14.6

data<-c(12.3,11.4,14.2,15.3,14.8,13.8,11.1,15.1,15.8,13.2)
n<-length(data)
n
x_ber<- mean(data)
x_ber
sample_sd<- sd(data)
sample_sd
mue<- 14.6
t_tab<- qt(0.01, n-1)
t_tab #-2.821
t_cal<- ((x_ber-mue)/(sample_sd/sqrt(n)))
t_cal #-1.71

# Comments: since t_cal>t_tab, so Ho is accepted.

########### Using p-value ###############
p_value<- pt(t_cal, n-1)
p_value #0.059

# Comments: since p_value=0.059>0.01, so Ho is accepted.

#################### Using function ###################
t.test(data, mu=14.6, conf.level=0.99, alternative="less")
boxplot(data,ylab="Hb lebel", col="green"   )
qqnorm(data, main="Normal Q-Q plot of Hb lebel")
qqline(data)