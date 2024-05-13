#Problem-02: Two dice rolled, S is the sum of both face, Find the E(s) and V(s)
#Vector of outcomes
s<- 2:12
#vector of probabilites
ps<- c(1:6, 5:1) / 36
#Expectation of s
es<- sum(s* ps)
es
# Variance of s
esq<- sum((s^2) * ps)
vs<- esq-es^2
vs
# Divide the plotting area into one row with two columns
par(mfrow = c(1,2))
#plot the distribution of s
barplot(ps,
ylim=c(0, 0.2),
xlab= "S",
ylab="Probabilites",
col ="red",
space= 0,
main= "Sum of two dice rolls")
#plot the distribution of D
probability <- rep(1/6, 6)
names(probability) <- 1:6
barplot(probability,
ylim=c(0, 0.2),
xlab= "D",
ylab="Probabilites",
col ="green",
space= 0,
main= "Outcomes of a sngle dice rolls")
################# Home Task ##############
#Two dice rolled, S is the sum of both face that is gratter than 2.
# i.e sum of both faces > 2.
#Find the E(s) and V(s)
#Vector of outcomes for (sum of both face)>2.
s1<- 3: 12
s1
#vector of probabilites
ps1<- c(2:6, 5:1) / 36
ps1
sum(ps1) # total probability
#Expectation of s
es1<- sum(ps1* s1)
es1
# Variance of s
esq1<- sum((s1^2) * ps1)
vs1<- esq1-es1^2
vs1
# Divide the plotting area into one row with two columns
par(mfrow = c(1,2))
#plot the distribution of s
barplot(ps1,
ylim=c(0, 0.2),
xlab= "S",
ylab="Probabilites",
col ="yellow",
space= 0,
main= "Sum of two dice rolls")
#plot the distribution of D
probability <- rep(1/6, 6)
names(probability) <- 1:6
barplot(probability,
ylim=c(0, 0.2),
xlab= "D",
ylab="Probabilites",
col ="green",
space= 0,
main= "Outcomes of a single dice rolls")