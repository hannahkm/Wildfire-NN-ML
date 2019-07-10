#install.packages("PRROC")
library("PRROC")

fg <- rnorm(300)
bg <- rnorm(500, -2)

roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)

#scores.class0 = classification scores of all data points OR data points in positive class
#scores.class1 = either same as scores.class0 OR scores for negative data poitnts

plot(roc)
plot(pr)

x <- c(fg, bg)
lab <- c(rep(1,length(fg)),rep(0,length(bg)))

roc1 <- roc.curve(scores.class0 = x, weights.class0 = lab, curve = TRUE)
pr1 <- pr.curve(scores.class0 = x, weights.class0 = lab, curve = TRUE)

wfg <- c(runif(300,min=0.5,max=1),runif(500,min=0,max=0.5))
wroc <- roc.curve(scores.class0=x,weights.class0=wfg,curve=TRUE)
wpr <- pr.curve(scores.class0=x,weights.class0=wfg,curve=TRUE)

wroc1 <- roc.curve(scores.class0=x,scores.class1=x,weights.class0=wfg,
                   weights.class1=1-wfg,curve=TRUE)
wpr1 <- pr.curve(scores.class0=x,scores.class1=x,weights.class0=wfg,
                weights.class1=1-wfg,curve=TRUE)

wroc2 <- roc.curve(scores.class0=x,weights.class0=wfg,curve=TRUE,
                   max.compute=T,min.compute=T,rand.compute=T)
wpr2 <- pr.curve(scores.class0=x,weights.class0=wfg,curve=TRUE,
                 max.compute=T,min.compute=T,rand.compute=T)

plot(wpr2,max.plot=TRUE,min.plot=TRUE,rand.plot=TRUE,fill.area=TRUE)

y<-c(rnorm(300,sd=2),rnorm(500,-5,sd=2))
wroc3 <- roc.curve(scores.class0=y,weights.class0=wfg,curve=TRUE,
                   max.compute=TRUE,min.compute=TRUE,rand.compute=TRUE)
wpr3 <- pr.curve(scores.class0=y,weights.class0=wfg,curve=TRUE,
                 max.compute=TRUE,min.compute=TRUE,rand.compute=TRUE)

plot(wpr2,max.plot=TRUE,min.plot=TRUE,rand.plot=TRUE,fill.area=T,
     color=2,auc.main=FALSE)
plot(wpr3,add=TRUE,color=3)

#just playing with the plot at this point tbh
plot(wpr2,scale.color=heat.colors(100))
plot(wpr2,color=6,lty="dotted")
plot(wpr2,legend=1) #1=bottom, 2=left, 3=top, 4=right

#================ DMwR and ROCR

pred <- prediction(predict_elm,y_test)
perf <- performance(pred,"prec","rec")
PRcurve(predict_elm,y_test)


#================ verification
roc.plot(y_test,as.data.frame(predict_elm))

