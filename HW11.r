##Lucas Humphrey STAT 462 Homework 11

##Part 1
greenhouse=read.table("greenhouse.txt",header=TRUE)
attach(greenhouse)
head(greenhouse)
summary(method)
x1=factor(method,levels=c("bed","aeroponic","hydroponic","pot"))
lmbed=lm(weight~x1)
summary(lmbed)

weight_hydroponic=weight[method=='hydroponic']
mean(weight_hydroponic)

lmbed_null=lm(weight~1)
lmbed_alt=lm(weight~x1)
anova(lmbed_null,lmbed_alt)

summary(variety)
x2=factor(variety,levels=c("Costanera","Mariva","Unica"))
lmvar=lm(weight~x1+x2)
summary(lmvar)

weight_costanera=weight[variety=='Costanera'&method=='hydroponic']
mean(weight_costanera)
weight_unica=weight[variety=='Unica'&method=='hydroponic']
mean(weight_unica)

anova(lmbed,lmvar)

lmint=lm(weight~x1+x2+x1:x2)
summary(lmint)
mean(weight_costanera)
mean(weight_unica)

anova(lmvar,lmint)

##Part 2
data(uswages, package="faraway")
attach(uswages)
summary(uswages)
region=rep(NA,2000)
for(i in 1:2000){
  if(ne[i]==1){region[i]="NE"}
  if(mw[i]==1){region[i]="MW"}
  if(we[i]==1){region[i]="WE"}
  if(so[i]==1){region[i]="SO"}
}

region2=factor(region,levels=c("NE","MW","WE","SO"))
lmne=lm(wage~region2)
summary(lmne)
lm_null=lm(wage~1)
anova(lm_null,lmne)

region3=factor(region,levels=c("SO","NE","MW","WE"))
lmso=lm(wage~region3)
summary(lmso)
anova(lm_null,lmso)
X=model.matrix(lmso)
X

lmeduc=lm(wage~region3+educ)
anova(lm_null,lmeduc)

lmeduc2=lm(wage~region3+educ+region3:educ)
summary(lmeduc2)
anova(lm_null,lmeduc2)

plot(educ,wage,type="n")
points(educ[region3=='NE'],wage[region3=='NE'])
points(educ[region3=='MW'],wage[region3=='MW'],col="red")
points(educ[region3=='WE'],wage[region3=='WE'],col="blue")
int = coef(lmeduc2)[1]; slp = coef(lmeduc2)[6]
abline(int,slp,col="black")
int = coef(lmeduc2)[1]; slp = coef(lmeduc2)[7]
abline(int,slp,col="red")
int = coef(lmeduc2)[1]; slp = coef(lmeduc2)[8]
abline(int,slp,col="blue")
legend('topleft',leg=c('NE','MW','WE'),col=c('black','red','blue'),lty=1)
