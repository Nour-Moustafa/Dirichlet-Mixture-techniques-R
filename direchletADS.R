library(sirt)
library(entropy)
library(DirichletReg)

tr_f=unsw.normal[1:20,1:8]
p= dirichlet.mle(tr_f, weights=NULL , eps = 10^(-5), convcrit = 1e-15 , maxit=1000,  oldfac = .3 , progress=FALSE)
tr= as.numeric(unlist(tr_f))
dim (tr)=c(nrow(tr_f),ncol(tr_f))  # important matrix
p1=p$xsi      # parameters for training and testing

# training denisty 
tr_density= ddirichlet(tr,p1, log = T) 
tr_density[which(!is.finite(tr_density))] <- 0
dim (tr_density)=c(nrow(tr_f),1)  # important matrix

# testing density 

ts_density= ddirichlet(ts_f,p1, log = T)
ts_density[which(!is.finite(tr_density))] <- 0
dim (ts_density)=c(nrow(ts_f),1)  # important matrix

d <- density(tr_density)
#plot(d, main="Density of UNSW-NB15 Normal")

par(mfrow=c(1,2)) 
hist(tr_density,freq=FALSE,main="Density of NSL-KDD Abnormal",xlab = "Values")
lines(density(x),na.rm=TRUE)
polygon(d, col=2, border=,lty=2) # attack
polygon(d, col="green", border=,lty = 1) # Normal


# plot normal distribution 
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

plot(x, hx, type="l", lty=1, xlab="x values",col=4,lwd=2,
     ylab="Density", main="")

