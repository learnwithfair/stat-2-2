# Problem-9
############ varience Test ###########
x<- c(120, 115, 94, 118, 111, 102, 102, 131, 104, 107, 115, 139, 115, 113, 114,
105, 115, 134, 109, 109, 93, 118, 109, 106, 125)

sigma = 400
mu = 130
df = length(x)
chisquare<-sum(x-mu)^2/sigma #chi_cal = 455.82
chisquare
p.value <-2*min(pchisq(chisquare,df),1-pchisq(chisquare,df))
p.value # pvalue = 0
p.value2<- 1-pchisq(chisquare,df) #pvalue = 0
p.value2
p.value3<- pchisq(chisquare,df) #pvalue = 1
p.value3
#Comment : pvalue<chi_cal Hence Rejected