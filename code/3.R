##Problem-03: A herd of 1500 steer was feed a speacial high protein gain for month.
# A random sample of 29 was weighted and had gain an average of 6.7 pounds. If the
# sd of weight gain for the entire herd is 7.1. Test the hypothsis at 5% level of
# significance that the average weight gain per steer fop the month was more than
# 5 pounds. Also comments on the test using p-value.
#Ho: mue equal 5
#H1: mue greater than 5
x_ber <- 6.7
mue <- 5
sd <- 7.1
n <- 29
alpha= 0.05
     
z_tab <- qnorm(0.05,lower.tail=FALSE)
z_tab #1.644  
z_cal <- (x_ber-mue)/(sd/sqrt(n)) 
z_cal #1.289
#Comments: Ho is accepted, Since z.cal<z.tab
####Using p-value ####
p_value <- pnorm(z_cal,lower.tail=FALSE)
p_value # 0.098
#Comments: Since p-value= 0.098 > 0.05, so Ho is accepted
#This p-value also indicate that if we test the hipothesis with maximum 9.8%
#level of signicance, Ho was also accepted. But if we test above 9.8% then
#Ho was rejected.

####If it was two tail test ####
# i.e. Ho equal 5
#and H1: mue not equal 5
alpha= 0.05 
z_tab1<- qnorm(alpha/2)#.025 = lower limit
z_tab1 #-1.96
z_tab2<- qnorm(1-alpha/2)#.975 = upper limit
z_tab2 #1.96
#Also Ho is accepted, since z_cal=1.289 fall between
# -1.96 to 1.96
#Using p-value##
p_value2<- 2*pnorm(z_cal, lower.tail=FALSE)
p_value2 # 0.19
#Here, p_value=0.19>0.05, so Ho is accepted
#######Now make the confidance interval#####
# 95% confidance interval for mue(Population mean)
CI<-c(x_ber+z_tab1*sd/sqrt(n), x_ber+z_tab2*sd/sqrt(n))
CI # 4.1159 to 9.2840
#we are 95% sure, we have confidance that the average
# weight gain is between 4.1159 to 9.2840 due to applying high protien.