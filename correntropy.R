
library(matrixStats)

correntopy<-function (input)
{
  
  data=matrix(0,nrow(input),ncol(input))
  col_means=colMeans(input)
  dim(col_means) <- c(1, ncol(input))
  col_stds= colSds(input)
  dim(col_stds) <- c(1, ncol(input))
  for(y in 1:ncol(input))
  {
    data[,y] = (1/(sqrt(2*3.14*(col_stds[,y]))) * exp(-(col_means[,y]-input[,y])^2)/(2*col_stds[,y]^2))
  }
  data[is.infinite(data)] <- 0 
  r= rowMeans(data,na.rm = TRUE)
  dim(r) <- c(1, nrow(data))
  r= as.data.frame(t(r))
  r= scale(r, center = TRUE, scale = FALSE)
  return (r)
}
par(mfrow=c(1,2)) 

input1= as.matrix(normal.dns)
normal_dns=correntopy(input1)
input2= as.matrix(attack.dns)
attack_dns=correntopy(input2)
plot(normal_dns,xlab="Feature vectors",type = c("b"),main="Correntropy of DNS instances",ylab="Values",pch=1,col=3,lwd=2)
points(attack_dns,pch=2,col=2,lwd=2,type = c("b"))
legend("bottomleft", legend = c("Normal DNS ","Attack DNS"),lwd=2, pch=c(1,2),col=c(3,2))


input3= as.matrix(normal.http)
normal_http=correntopy(input3)
input4= as.matrix(attack.http)
attack_http=correntopy(input4)
plot(normal_http,xlab="Feature vectors",type = c("b"),main="Correntropy of HTTP instances",ylab="Values",pch=1,col=3,lwd=2)
points(attack_http,pch=2,col=2,lwd=2,type = c("b"))
legend("bottomright", legend = c("Normal http ","Attack http"),lwd=2, pch=c(1,2),col=c(3,2))
