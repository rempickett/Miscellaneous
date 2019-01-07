#This is code for a blog post I wrote looking at regression from a potential outcomes framework
#The idea is to compare test scores by school type (public or catholic), and see what a regression will get you for various selection on treatment
#You can find the blogpost here: www.robertempickett.com/post/regression-and-potential-outcomes/

#First we need to set up the parameters
#There are two groups, public school and catholic school
#The mean score for public school is 100, for catholic school it is 110
mu0 <- 100
mu1 <- 110

#Each student has some random deviation from their group means, v0 and v1
v0 <- rnorm(1000, mean=0, sd=10)
v1 <- rnorm(1000, mean=0, sd=10)



#If we could observe each student's potential outcomes, we would have both y0 (outcome under public school) and y1 (outcome under catholic school) for each student
#The score is simply the sum of the group mean and the student's random variation
y1 <- mu1 + v1
y0 <- mu0 + v0

#We can then plot those potential outcomes to get each student's individual treatment effect for catholic school
plot(c(y0, y1)~c(rep(1,1000),rep(2,1000)), ylab="Test Score", xaxt="n", xlab="School Type")
axis(side=1, at=c(1,2), labels=c("Public School", "Catholic School"))
for(i in 1:1000){
  lines(x = c(1,2), y=c(y0[i],y1[i]))
}



#We could also assume a more realistic scenario where each student is either in public school or catholic school and not both
#We can assign each of the 1000 students catholic school treatment, d, at random
#If d is equal to 1 the student is in catholic school, if 0 public school
d <- rbinom(1000,1,.5)
#Each student's test score, then, would be their potential outcome above, selected for whichever treatment status they got
y <- mu0 + (mu1 - mu0)*d + v0+d*(v1-v0)


#We could then look at the mean scores for each school type
mean(y[d==1])
mean(y[d==0])
#Or run a regression of test score on school type
summary(lm(y~d))
#and get unbiased results because treatment is at random

#Because treatment is at random, we can confirm that there is no relationship between treatment and a student's difference from the group means, or in the difference between the student's hypothetical differences.
cor(d, v1)
cor(d, v0)
cor(d, (v1-v0))



#We can create a plot that shows the observed and potential outcomes for each student
#The red lines are potential and observed for students who ended up in catholic school
#The blue lines are potential and observed for students who ended up in public school
plot(c(y0, y1)~c(rep(1,1000),rep(2,1000)), ylab="Test Score", xaxt="n", xlab="School Type")
axis(side=1, at=c(1,2), labels=c("Public School", "Catholic School"))
for(i in 1:1000){
  lines(x = c(1,2), y=c(y0[i],y1[i]), col=ifelse(d[i]==1, 'red','blue'))
}
#The random distribution of lines reflects the random selection into treatment, and results in unbiased regression estimates


#We could also asign treatment based on each student's potential outcomes
#We could send all the people who don't do well in public school to catholic school
d <- v0< -5
#And get the observed outcomes
y <- mu0 + (mu1 - mu0)*d + v0+d*(v1-v0)

#We then get unbiased outcomes for catholic school, but we've exagerated the scores in public school (by selecting out the poor performers)
#Note, we've assumed no relationship between the student's potential heterogeneity in public school (v0) and their potential heterogeneity in catholic school (v1)
mean(y[d==1])
mean(y[d==0])
summary(lm(y~d))
#We can see that treatment is still assigned randomly with respect to student performance in catholic school, but is strongly related to performance in public school and in the difference between catholic and public school performance
cor(d, v1)
cor(d, v0)
cor(d, (v1-v0))

#Again We can create a plot that shows the observed and potential outcomes for each student
plot(c(y0, y1)~c(rep(1,1000),rep(2,1000)), ylab="Test Score", xaxt="n", xlab="School Type")
axis(side=1, at=c(1,2), labels=c("Public School", "Catholic School"))
for(i in 1:1000){
  lines(x = c(1,2), y=c(y0[i],y1[i]), col=ifelse(d[i]==1, 'red','blue'))
}
#The cluster of red lines with low scores in public school represents the selection on poor performing public school students





#We could also select students who do particularly well in catholic school
d <- v1 > 5
y <- mu0 + (mu1 - mu0)*d + v0+d*(v1-v0)
#Again, we get biased results where we've exaggerated observed catholic school test scores
mean(y[d==1])
mean(y[d==0])
summary(lm(y~d))
cor(d, v1)
cor(d, v0)
cor(d, (v1-v0))

#Again We can create a plot that shows the observed and potential outcomes for each student
plot(c(y0, y1)~c(rep(1,1000),rep(2,1000)), ylab="Test Score", xaxt="n", xlab="School Type")
axis(side=1, at=c(1,2), labels=c("Public School", "Catholic School"))
for(i in 1:1000){
  lines(x = c(1,2), y=c(y0[i],y1[i]), col=ifelse(d[i]==1, 'red','blue'))
}
#The cluster of red lines with high scores in catholic school represents the selection on high performing catholic school students



#Lastly we could select on students who are likely to do well regardless
d <- v0 > 10 & v1 > 5
y <- mu0 + (mu1 - mu0)*d + v0+d*(v1-v0)
#We've again exaggerated the effect of catholic school, and slightly biased public school performance down
mean(y[d==1])
mean(y[d==0])
summary(lm(y~d))
cor(d, v1)
cor(d, v0)
cor(d, (v1-v0))

#and again the plot
plot(c(y0, y1)~c(rep(1,1000),rep(2,1000)), ylab="Test Score", xaxt="n", xlab="School Type")
axis(side=1, at=c(1,2), labels=c("Public School", "Catholic School"))
for(i in 1:1000){
  lines(x = c(1,2), y=c(y0[i],y1[i]), col=ifelse(d[i]==1, 'red','blue'))
}

