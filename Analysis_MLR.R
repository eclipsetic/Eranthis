# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(vegan, pvclust, factoextra, readxl)
setwd("E:/Eranthis")

#////////////////////////////////////////////////////случаное реплецирование
data <- read_excel("E:/Eranthis/Tulips.xls")

data<-data[,-1]

data[is.na(data)]<--9

data_v<-data[,c(2:ncol(data))]

sl<-list(data_v[data_v[,1]!=-9,1])
for(i in 1:ncol(data_v)) sl<-c(sl,list(data_v[data_v[,i]!=-9,i]))

for(i in 1:ncol(data_v)){
  data_v[data_v[,i]==-9,i]<-sample(sl[[i]], length(data_v[data_v[,i]==-9,i]), replace=T)
}

data_rep<-cbind(pop=data[,1], data_v)

data<-data_rep

row_n<-data.frame(point=data[,1])

data<-data[,-1]



for(i in 1:ncol(data)) data[,i]<-(data[,i]-mean(data[,i]))/sd(data[,i])

fit<-prcomp(data)
fviz_pca_biplot(fit, habillage=row_n[,1], addEllipses=T, pointsize = 6)

#////////////////////////////////////////////////////множественная линейная регрессия
mising_value <- function(data_p, nx){
  pr_names<-c("y")
  for(i in 2:ncol(data_p)) pr_names<-c(pr_names, paste("x", i-1, sep="")) 
  colnames(data_p)<-pr_names
  f<-paste(pr_names[1], "~", sep="")
  for(i in 2:length(pr_names)) f<-paste(f, "+", pr_names[i], sep="")  
  f<-as.formula(f)
  fit <- lm(f, data=data_p)
  newdata=data.frame(x=nx[1])
  for(i in 2:length(nx)) newdata<-cbind(newdata, c(nx[i]))
  colnames(newdata)<-pr_names[2:length(pr_names)]
  predict(fit, newdata)[[1]]
}

data_T <- read_excel("E:/Eranthis/Tulips.xls")
pop<-data[,2]
data<-data[,-c(1:2)]
data_rep<-data


for (i in 1:ncol(data)) {
  for (j in 1:nrow(data)) {
    if (is.na(data[j, i])) {
      yd <- data[, i]
      nx <- as.numeric(data[j, ])
      nx <- nx[!is.na(nx)]
      na_index <- which(!is.na(data[j, , drop = FALSE]))
      data_p <- data[, na_index]
      data_p <- cbind(y = yd, data_p)
      data_p <- data_p[rowSums(is.na(data_p)) == 0, ]
      data_rep[j,i]<-mising_value(data_p, nx)
    }
  }
}


data_rep<-cbind(pop=pop, data_rep)

data<-data_rep

row_n<-data.frame(point=data[,1])

data<-data[,-1]

data_am<-data.frame(dep=data[,12], point=row_n)

fit <- aov(dep ~ point, data=data_am)

summary(fit)

a<-3.019

1-1/a

boxplot(dep ~ point, data=data_am)

for(i in 2:ncol(data)){
  data[,i]<-(data[,i]-mean(data[,i]))/sd(data[,i])
}

fit<-prcomp(data[,2:12])
fviz_pca_biplot(fit, habillage=row_n[,1], addEllipses=T, pointsize = 6)

#////////////////////////////////////////////////////множественная полиномиальная регрессия


mising_value <- function(data_p, nx, degree)
{
  pr_names<-c("y")
  for(k in 2:ncol(data_p)) pr_names<-c(pr_names, paste("x", k-1, sep="")) 
  colnames(data_p)<-pr_names
  f<-paste(pr_names[1], "~ x1", sep="")
  for(k in 3:length(pr_names)) f<-paste(f, "+", pr_names[k], sep="")  
  if(degree>1)
  {
    for(l in 2:degree)
    {
      for(k in 2:length(pr_names)) f<-paste(f, "+I(", pr_names[k], "^", l, ")", sep="")
    } 
  }
  f<-as.formula(f)
  fit <- lm(f, data=data_p)
  newdata=data.frame(x=nx[1])
  for(k in 2:length(nx)) newdata<-cbind(newdata, c(nx[k]))
  colnames(newdata)<-pr_names[2:length(pr_names)]
  predict(fit, newdata)[[1]]
  
}

rep_data <- function(data, degree)
{
  data_rep<-data
  for(i in 1:ncol(data))
    for(j in 1:nrow(data))
    {
      if(is.na(data[j,i]))
      {
        yd<-data[,i]
        nx<-as.numeric(data[j,])
        nx<-nx[!is.na(nx)]
        data_p<-data[,!is.na(data[j,])]
        data_p<-cbind(y=yd,data_p)
        data_p<-data_p[rowSums(is.na(data_p))==0,]
        data_rep[j,i]<-mising_value(data_p, nx, degree=degree)
      }
    }
  return(data_rep)
}

data<-read.table("data.txt",header=T,sep="\t")


data<-read.table("data.txt",header=T,sep="\t")
pop<-data[,2]
data<-data[,-c(1:2)]

data_rep<-rep_data(data, degree=5)

data_rep<-cbind(pop=pop, data_rep)

data<-data_rep

row_n<-data.frame(point=data[,1])

data<-data[,-1]

for(i in 1:ncol(data)) data[,i]<-(data[,i]-mean(data[,i]))/sd(data[,i])

fit<-prcomp(data)
fviz_pca_biplot(fit, habillage=row_n[,1], addEllipses=T, pointsize = 6)



#///////////////////////////////////////////////BIC

data<-read.table("data.txt",header=T,sep="\t")
pop<-data[,2]
data<-data[,-c(1:2)]

i=7
j=100
degree=5

yd<-data[,i]
nx<-as.numeric(data[j,])
nx<-nx[!is.na(nx)]
data_p<-data[,!is.na(data[j,])]
data_p<-cbind(y=yd,data_p)
data_p<-data_p[rowSums(is.na(data_p))==0,]

pr_names<-c("y")
for(k in 2:ncol(data_p)) pr_names<-c(pr_names, paste("x", k-1, sep="")) 
colnames(data_p)<-pr_names
f<-paste(pr_names[1], "~ x1", sep="")
for(k in 3:length(pr_names)) f<-paste(f, "+", pr_names[k], sep="")  
if(degree>1)
{
  for(l in 2:degree)
  {
    for(k in 2:length(pr_names)) f<-paste(f, "+I(", pr_names[k], "^", l, ")", sep="")
  } 
}
f<-as.formula(f)
fit <- lm(f, data=data_p)
newdata=data.frame(x=nx[1])
for(k in 2:length(nx)) newdata<-cbind(newdata, c(nx[k]))
colnames(newdata)<-pr_names[2:length(pr_names)]
predict(fit, newdata)[[1]]
BIC(fit)


#//////////////////////////////////////////////////
#////////////////////////////////////////////////////множественная полиномиальная регрессия BIC


mising_value <- function(data_p, nx, degree)
{
  rez<-c(1,2)
  pr_names<-c("y")
  for(k in 2:ncol(data_p)) pr_names<-c(pr_names, paste("x", k-1, sep="")) 
  colnames(data_p)<-pr_names
  f<-paste(pr_names[1], "~ x1", sep="")
  for(k in 3:length(pr_names)) f<-paste(f, "+", pr_names[k], sep="")  
  if(degree>1)
  {
    for(l in 2:degree)
    {
      for(k in 2:length(pr_names)) f<-paste(f, "+I(", pr_names[k], "^", l, ")", sep="")
    } 
  }
  f<-as.formula(f)
  fit <- lm(f, data=data_p)
  newdata=data.frame(x=nx[1])
  for(k in 2:length(nx)) newdata<-cbind(newdata, c(nx[k]))
  colnames(newdata)<-pr_names[2:length(pr_names)]
  rez[1]<-predict(fit, newdata)[[1]]
  rez[2]<-BIC(fit)
  return(rez)     
}

rep_data <- function(data, degree)
{
  data_rep<-data
  data_BIC<-data
  data_BIC[,]<-0
  for(i in 1:ncol(data))
    for(j in 1:nrow(data))
    {
      if(is.na(data[j,i]))
      {
        yd<-data[,i]
        nx<-as.numeric(data[j,])
        nx<-nx[!is.na(nx)]
        data_p<-data[,!is.na(data[j,])]
        data_p<-cbind(y=yd,data_p)
        data_p<-data_p[rowSums(is.na(data_p))==0,]
        rez<-mising_value(data_p, nx, degree=degree)
        data_rep[j,i]<-rez[1]
        data_BIC[j,i]<-rez[2]
      }
    }
  fit<-list(rep=data_rep, BIC=data_BIC)
  return(fit)
}

rep_BIC<-function(data, max_degree)
{
  rep<-list()
  BIC<-list
  
  for(i in 1:max_degree)
  {
    fit<-rep_data(data, degree=i)
    rep<-c(rep, list(fit$rep))
    BIC<-c(BIC, list(fit$BIC))
  }
  
  
  
  return(BIC)
}

data<-read.table("data.txt",header=T,sep="\t")


data<-read.table("data.txt",header=T,sep="\t")
pop<-data[,2]
data<-data[,-c(1:2)]

rez<-rep_BIC(data, max_degree=2)



fit<-rep_data(data, degree=2)






data_rep<-cbind(pop=pop, data_rep)

data<-data_rep

row_n<-data.frame(point=data[,1])

data<-data[,-1]

for(i in 1:ncol(data)) data[,i]<-(data[,i]-mean(data[,i]))/sd(data[,i])

fit<-prcomp(data)
fviz_pca_biplot(fit, habillage=row_n[,1], addEllipses=T, pointsize = 6)







