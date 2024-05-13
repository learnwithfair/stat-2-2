x<- c(122,145,120,45,98,67,109,100,107,106,93,125,130,90,34,108,80,48,65,56)
H0:median = 110
md = 110
y<-sum(x>md)
n<-sum(x!=md)
p.value<-1-pbinom(y-1,n,0.5)
alpha = 0.05

######## If Wilcoxon SIgned Rank Test #######
wilcox.test(x,mu=110,exact=FALSE,correct=TRUE,alternative="two.sided")