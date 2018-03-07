#x1 <- read.csv("E:/My papers/paper 9  GAA/data/normal.csv", header=T)
#x2 <- read.csv("E:/My papers/paper 9  GAA/data/testing1.csv",header=T)


x1=nsl.attack
x2=unsw.test

f_no = 8 #  the selected number of features

# to select the features without the labels
lb_tr=matrix(0,nrow = nrow(x1),1)
lb_ts=matrix( 1,nrow = nrow(x2),3)

lb_tr=x1[,(ncol(x1)-2)]
lb_ts=x2[,(ncol(x2)-2):ncol(x2)]


x1=x1[,1:(ncol(x1)-3)]
x2=x2[,1:(ncol(x2)-3)]

# function of applying the PCA on the selected Features

FUN <- function(x) as.numeric(ifelse(x=="NA", NA, x))
x1 <- data.frame(apply(x1, 2, FUN))
x1 <- x1[complete.cases(x1),]

weigh <- function(x)
{
  d= princomp(x1)
  #d= princomp(formula = ~., data = x, cor = TRUE, na.action=na.exclude)
  y= d$sdev
  y=colMeans(cov(x))
  dim(y)<-c(ncol(x),1)
  return (y)
}

tr_w1=weigh(x1)
cor_r <- function (tr_w)
{
  cor_rk= matrix(,nrow(tr_w),2)
  cor_rk[,1]= seq(1,nrow(tr_w),1)
  for(y in 1:nrow(tr_w))
    cor_rk[y,2]=tr_w[y,1]
  cor_rk1= data.frame(cor_rk)
  cor_rk1=cor_rk1[with(cor_rk1, order(cor_rk1[,2],decreasing = F)), ]
  return (cor_rk1)
}
f_set <- function(x1,cor_rk1, f_no)
{
  x1_1= matrix(,nrow(x1),f_no)
  for(g in 1:nrow(x1))
  {
    for(d in 1: f_no)
    {
      r= cor_rk1[d,1]
      x1_1[g,d]=x1[g,r]
    }  
  }
  return (x1_1)
}

cor_f <- cor_r(tr_w1)
tr_w= matrix(, f_no, 1)
tr_w[]= cor_f[1:f_no,2 ]
tr_f= f_set(x1,cor_f,f_no)
ts_f= f_set(x2,cor_f,f_no)

#tr1= princomp(tr_f)
#tr_f=tr1$scores
# 
#ts1=princomp(ts_f)
#ts_f=ts1$scores

#tr_f= scale(tr_f, center = TRUE, scale = TRUE) # zscore
#ts_f= scale(ts_f, center = TRUE, scale = TRUE) # zscore
rm(tr_w1)
rm(tr_w)
rm(cor_f)
rm(ts1)
rm(tr1)
