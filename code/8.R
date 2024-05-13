#Probelem-08: There are two COVID-19 testing booths, we test some people
and their recorded data is below, where the numbers of people of booth-1 is 11 and
the numbers of people of booth-2 is 10:
Booth-1: positive,positive, negative,positive, negative, negative,positive,
positive,positive, negative, positive
Both-2: negative, negative, negative, positive, positive,negative, positive,
negative, negative, negative
is there any relation between two both???????
################### Solution #############
Ho: There is no relation between booth-1 and booth-2.
H1: There is relation between booth-1 and booth-2.
booth_1<- c("positive","positive", "negative","positive", "negative", "negative",
"positive","positive","positive", "negative", "positive")
#booth_1<- c(1, 1, 0, 1,0, 0, 1, 1, 1, 0, 1)
#table(booth_1)
booth_2<- c("negative", "negative", "negative", "positive", "positive",
"negative", "positive", "negative", "negative", "negative")
#booth_2<- c(0, 0, 0, 1, 1, 0, 1, 0, 0, 0)
#table(booth_2)
x_table1<- table(booth_1)
x_table1
x_table2<- table(booth_2)
x_table2
m<- matrix(c(4, 7, 7, 3), ncol=2, byrow=TRUE, dimnames=list(c("Booth-1", "Booth-2"), c("negative",
"positive")))
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
E22 # 4.761905 < 5, so we need to do "Yates" continuity correction.
chi_yates<- (((abs(m[1]-E11)-0.5)^2)/E11) + (((abs(m[2]-E21)-0.5)^2)/E21) + ((abs(m[3]-E12)-0.5)^2/E12)+ ((abs(m[4]-E22)-0.5)^2/E22)
chi_yates #1.218781

#df=(r-1)(c-1) , this is the fourmula to find the df.
chi_tab<- qchisq(0.05, df=1, lower.tail=FALSE)
chi_tab # 3.841459
#Comments: chi_yates = 1.218781 < chi_tab= 3.841459 , so Ho is accepted.
#i.e. There is no relation between booth-1 and booth-2.
j
###### P-value ##########
p_value<- pchisq(chi_yates, df=1, lower.tail=FALSE)
p_value # 0.2696
#Coments: p-value = 0.2696 > 0.05, so Ho is accepted.
############################## Using chisq.test function #############
chisq.test(m)
# Here p-value = 0.2696 > 0.05, so Ho is accepted.
#i.e There is no relation between booth-1 and booth-2.