##Lucas Humphrey STAT462 Homework 2

##Part 1
###Compute the sample mean and the standard deviation of the box weight.
weight=read.table("spaghetti.txt",header=TRUE)
head(weight)

summary(weight$spaghetti)
mean(weight$spaghetti)
sd(weight$spaghetti)
n=20

###Perform a suitable test to verify if the mean of the box weight is equal to the nominal one (16 oz) or not.
####Compute the test statistic.
se=sd(weight$spaghetti)/sqrt(20)
se
t_stat=(mean(weight$spaghetti)-16)/se
t_stat

####Compute the test p-value.
pvalue=2*(1-pt(abs(t_stat),n-1))
pvalue

###Compute the 99% two-sided confidence interval for the mean of the box weight.
mean(weight$spaghetti)+(2.58*sd(weight$spaghetti))
mean(weight$spaghetti)-(2.58*sd(weight$spaghetti))

###Perform the test for the consumers' association.
####Compute the test p-value.
pvalue2=(1-pt(abs(t_stat),n-1))
pvalue2


##Part 2
###Suppose that X~N(0; 1), Y = 3 -2X and Z = 2 -X.
####Draw 50 independent realizations of Y. Using those values, create 50 realizations of Z.
set.seed(123)
y=rnorm(n=50,mean=3,sd=2)
plot(y)
z=rnorm(n=50,mean=2,sd=1)
plot(z)

####Create a scatterplot of Z vs Y.
plot(y,z)

####Compute the correlation coefficient of Y and Z.
cor(y,z)
