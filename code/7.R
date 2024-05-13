# Problem-7: 

################## Solution ##############
Ho: There is no association with smoking and heart disses.
H1: There is a association with smoking and heart disses.
m<- matrix(c(55, 16, 23, 32), ncol=2, byrow=TRUE, dimnames=list(c("yes", "no"), c("disses",
"not_disses")))
m
c1<- sum(m[,1])
c1
c2<- sum(m[,2])
c2
r1<- sum(m[1,])
r1
r2<- sum(m[2,])
r2
n<- sum(m)
n
E11<- (c1*r1)/n
E11
E21<- (c1*r2)/n
E21
E12<- (c2*r1)/n
E12
E22<- (c2*r2)/n
E22
chi_cal<- (((abs(m[1]-E11)-0.5)^2)/E11) + (((abs(m[2]-E21)-0.5)^2)/E21) + ((abs(m[3]-E12)-0.5)^2/E12)+ ((abs(m[4]-E22)-0.5)^2/E22)
chi_cal #16.69906
#df=(r-1)(c-1) , this is the fourmula to find the df.
chi_tab<- qchisq(0.05, df=1, lower.tail=FALSE)
chi_tab # 3.841459
#Comments: chi_cal= 16.69906 > chi_tab= 3.841459 , so Ho is rejected.
# i.e smoking is associated with heart disses.
###### P-value ##########
p_value<- pchisq(chi_cal, df=1, lower.tail=FALSE)
p_value # 4.38026e-05
#Coments: p-value< 0.05, so Ho is rejected.
############################## Using chisq.test function #############
chisq.test(m)
# Here p-value = 9.56e-05 < 0.05, so Ho is rejected.
#i.e smoking is associated with heart disses.