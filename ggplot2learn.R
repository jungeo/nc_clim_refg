############################
#ggplot2 package learning
library(ggplot2)
qplot(carat,price,data = diamonds)
qplot(log(carat),log(price),data = diamonds)
qplot(carat,x*y*z,data = diamonds)
dsmall = diamonds[sample(nrow(diamonds),100),]
qplot(carat,price,data = dsmall,colour = color)

qplot(carat,price,data = dsmall,shape = cut)

qplot(carat,price,data = diamonds,alpha = I(1/10))
qplot(carat,price,data = diamonds,alpha = I(1/100))
qplot(carat,price,data = diamonds,alpha = I(1/200))

qplot(carat,price,data = dsmall,geom = c("point","smooth"))
qplot(carat,price,data = dsmall,geom = c("smooth"))

qplot(carat,price,data = diamonds,geom = c("point","smooth"),alpha = I(1/100))
#qplot(carat,price,data = dsmall,geom = c("point","smooth"),span = 0.3)
library(mgcv)
#qplot(carat,price,data = dsmall,geom = c("point","smooth"),method="gam",formula=y~a(x))
library(MASS)
#
qplot(color,price/carat,data = diamonds,geom = "boxplot",alpha = I(1/80))
qplot(color,price/carat,data = diamonds,geom = "jitter",alpha = I(1/80))
qplot(factor(diamonds$color),price/carat,data = diamonds,geom = c("boxplot","jitter"),colour=c(color),alpha = I(1/80))
#
qplot(carat,data = diamonds,geom="histogram")
qplot(carat,data = diamonds,geom="histogram",binwidth = .1,xlim = c(0,3))
qplot(carat,data = diamonds,geom="histogram",binwidth = .01,xlim = c(0,3.5))

qplot(carat,data = diamonds,geom="density")

qplot(carat,data = diamonds,geom = "density",colour = color)
qplot(carat,data = diamonds,geom = "histogram",fill = color,xlim = c(0,3.5))
#
qplot(color,data = diamonds,geom = "bar")
#qplot(color,data = diamonds,geom = "bar",weitht=diamonds$carat)
#
qplot(date,unemploy/pop,data = economics,geom="line")
qplot(date,uempmed,data = economics,geom="line")
#
year = function(x)
  as.POSIXlt(x)$year + 1900

qplot(unemploy/pop,uempmed,data = economics,geom = c("point","path"))
qplot(unemploy/pop,uempmed,data = economics,geom = "path",colour=year(date))

qplot(unemploy/pop,uempmed,data = economics,geom = "path",colour=year(date))
qplot(carat,data = diamonds,facets = color~.,geom = "histogram",binwidth = 0.1,xlim = c(0,3))
qplot(carat,..density..,data = diamonds,facets = color~.,geom = "histogram",binwidth = 0.1,xlim = c(0,3))

######################
#
qplot(displ,hwy,data = mpg,colour=factor(cyl))
qplot(displ,hwy,data = mpg,facets =.~year)+geom_smooth()
#
p = qplot(displ,hwy,data = mpg,colour=factor(cyl))
summary(p)
getwd()
setwd("e:/RTEST")
save(p,file = "plot.rdata")
load("plot.rdata")
ggsave("plot.png",width = 5,height = 5)

####################
p = ggplot(diamonds,aes(carat,price,colour=cut))
p=p+layer(geom = "point",stat = "identity", position = "identity")
p
##
p=ggplot(diamonds,aes(x=carat))
p=p+layer(geom = "bar",stat = "bin", position = "identity",params = list(fill = "steelblue",binwidth=0.1))
p
#
p+geom_histogram(binwidth = 0.1,fill="steelblue")
#
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()
qplot(sleep_rem/sleep_total,awake,data = msleep)
#
qplot(sleep_rem/sleep_total,awake,data = msleep)+geom_smooth()
qplot(sleep_rem/sleep_total,awake,data = msleep,geom = c("point","smooth"))
ggplot(msleep,aes(sleep_rem/sleep_total,awake))+geom_point()+geom_smooth()
#
############
library(scales)
bestfit = geom_smooth(method = "lm",se=F,colour = alpha("steelblue",0.5),size=2)
bestfit
qplot(sleep_rem,sleep_total,data = msleep)+bestfit
qplot(awake,brainwt,data = msleep,log="y")+bestfit
qplot(bodywt,brainwt,data=msleep,log="xy")+bestfit
#
#############################
#2@22222
p = ggplot(mtcars)
summary(p)
p = p + aes(wt,hp)
summary(p)
#####
p = ggplot(mtcars,aes(x=mpg,y=wt))
p+geom_point()

p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y = disp))


