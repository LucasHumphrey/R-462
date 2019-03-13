##Lucas Humphrey STAT462 Homework 1

##Load the dataset record.txtin R, using the function read.table
record=read.table("record.txt",header=TRUE)
head(record)

##Plot the scatterplot matrix of six variables
plot(record)

##Produce summaries of the variable m400
summary(record$m400)
hist(record$m400)
boxplot(record$m400)

##Produce summaries of the variable m1500
summary(record$m1500)
hist(record$m1500)
boxplot(record$m1500)

##Produce a 95% confidence interval for the mean of the variable m400
mean(record$m400)
sd(record$m400)
1.96*sd(record$m400)
mean(record$m400)+(1.96*sd(record$m400))
mean(record$m400)-(1.96*sd(record$m400))

##Produce a 95% confidence interval for the mean of the variable m1500
mean(record$m1500)+(1.96*sd(record$m1500))
mean(record$m1500)-(1.96*sd(record$m1500))
