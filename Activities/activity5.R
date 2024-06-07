rm(list=ls())
example(lm)

summary(lm.D90)


newLM<-unclass(lm.D90)

#1
class(lm.D90)

class(newLM)

names(newLM$model) <-c("var1","var2")

newLM$model[2]

newLM$model[2][sample(1:18),]
newLM<-unclass(lm.D90)

newerLM <- list("model"<-lm.D90$coefficients, "newLM" <- newLM$coefficients)
newerLM


m1<-cbind(vap, tv) 
list.a<-list(m1, vap, 3) 
vector1<-c(1,2,3)
gospels<-c("matthew","mark","luke", "john")
my.matrix<-matrix(c(1:20), nrow=4)
m2<-rbind(vap, tv)
dimnames(m1)[[2]][1]<-"Pigglywiggly"
list.a<-list(m1, vap, 3) 
my.data<-data.frame(cbind(vap, tv))
my.crazy.list<-list(vector1, gospels, my.matrix, TRUE, list.a)
names(my.crazy.list)<-c("OneTwoThree", "Gospels", 
                        "SmallMat", "OneLogical", "AnotherList")
