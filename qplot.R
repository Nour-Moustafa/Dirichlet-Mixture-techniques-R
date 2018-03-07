
# q-q plot of UNSW-NB15 dataset
x1=unsw.normal
x1=x1[,1:(ncol(x1)-3)]

par(mfrow=c(3,3)) 

n1 = length(x1$ct_dst_sport_ltm)
probabilities1 = (1:n1)/(n1+1)
normal.quantiles1 = qnorm(probabilities1, mean(x1$ct_dst_sport_ltm, na.rm = T), sd(x1$ct_dst_sport_ltm, na.rm = T))
plot(sort(normal.quantiles1), sort(x1$ct_dst_sport_ltm) , xlab = '',
     ylab = 'Quantiles of ct_dst_sport_ltm', main = '',col=4)
abline(0,1,col=2)

n2 = length(x1$tcprtt)
probabilities2 = (1:n2)/(n2+1)
normal.quantiles2 = qnorm(probabilities2, mean(x1$tcprtt, na.rm = T), sd(x1$tcprtt, na.rm = T))
plot(sort(normal.quantiles2), sort(x1$tcprtt) , xlab = '',
     ylab = 'Quantiles of tcprtt', main = 'Normal Q-Q plot for UNSW-NB15 Features',col=4)
abline(0,1,col=2)

n3 = length(x1$dwin)
probabilities3 = (1:n3)/(n3+1)
normal.quantiles3 = qnorm(probabilities3, mean(x1$dwin, na.rm = T), sd(x1$dwin, na.rm = T))
plot(sort(normal.quantiles3), sort(x1$dwin) , xlab = '',
     ylab = 'Quantiles of dwin', main = '',col=4)
abline(0,1,col=2)

n4 = length(x1$ct_src_dport_ltm)
probabilities4 = (1:n4)/(n4+1)
normal.quantiles4 = qnorm(probabilities4, mean(x1$ct_src_dport_ltm, na.rm = T), sd(x1$ct_src_dport_ltm, na.rm = T))
plot(sort(normal.quantiles4), sort(x1$ct_src_dport_ltm) , xlab = '',
     ylab = 'Quantiles of ct_src_dport_ltm', main = '',col=4)
abline(0,1,col=2)

n5 = length(x1$ct_dst_src_ltm)
probabilities5 = (1:n5)/(n5+1)
normal.quantiles5 = qnorm(probabilities5, mean(x1$ct_dst_src_ltm, na.rm = T), sd(x1$ct_dst_src_ltm, na.rm = T))
plot(sort(normal.quantiles5), sort(x1$ct_dst_src_ltm) , xlab = '',
     ylab = 'Quantiles of ct_dst_src_ltm ', main = '',col=4)
abline(0,1,col=2)

n6 = length(x1$ct_dst_ltm)
probabilities6 = (1:n6)/(n6+1)
normal.quantiles6 = qnorm(probabilities6, mean(x1$ct_dst_ltm, na.rm = T), sd(x1$ct_dst_ltm, na.rm = T))
plot(sort(normal.quantiles6), sort(x1$ct_dst_ltm) , xlab = '',
     ylab = 'Quantiles of ct_dst_ltm', main = '',col=4)
abline(0,1,col=2)

n7 = length(x1$smean)
probabilities7 = (1:n7)/(n7+1)
normal.quantiles7 = qnorm(probabilities7, mean(x1$smean, na.rm = T), sd(x1$smean, na.rm = T))
plot(sort(normal.quantiles7), sort(x1$smean) , xlab = '',
     ylab = 'Quantiles of smean', main = '',col=4)
abline(0,1,col=2)

n8 = length(x1$service_pro)
probabilities8 = (1:n8)/(n8+1)
normal.quantiles8 = qnorm(probabilities8, mean(x1$service_pro, na.rm = T), sd(x1$service_pro, na.rm = T))
plot(sort(normal.quantiles8), sort(x1$smean) , xlab = 'Theoretical Quantiles from Normal Distribution',
     ylab = 'Quantiles of service', main = '',col=4)
abline(0,1,col=2)

##################################################################################
# q-q plot of NSLKDD dataset
x1=nsl.train
x1=x1[,1:(ncol(x1)-3)]

par(mfrow=c(3,3)) 

n1 = length(x1$srv_count)
probabilities1 = (1:n1)/(n1+1)
normal.quantiles1 = qnorm(probabilities1, mean(x1$srv_count, na.rm = T), sd(x1$srv_count, na.rm = T))
plot(sort(normal.quantiles1), sort(x1$srv_count) , xlab = '',
     ylab = 'Quantiles of srv_count', main = '',col=4)
abline(0,1,col=2)

n2 = length(x1$dst_host_srv_count)
probabilities2 = (1:n2)/(n2+1)
normal.quantiles2 = qnorm(probabilities2, mean(x1$dst_host_srv_count, na.rm = T), sd(x1$dst_host_srv_count, na.rm = T))
plot(sort(normal.quantiles2), sort(x1$dst_host_srv_count) , xlab = '',
     ylab = 'Quantiles of dst_host_srv_count', main = 'Normal Q-Q plot for NSL-KDD Features',col=4)
abline(0,1,col=2)

n3 = length(x1$count )
probabilities3 = (1:n3)/(n3+1)
normal.quantiles3 = qnorm(probabilities3, mean(x1$count , na.rm = T), sd(x1$count, na.rm = T))
plot(sort(normal.quantiles3), sort(x1$count ) , xlab = '',
     ylab = 'Quantiles of count', main = '',col=4)
abline(0,1,col=2)

n4 = length(x1$dst_host_same_srv_rate)
probabilities4 = (1:n4)/(n4+1)
normal.quantiles4 = qnorm(probabilities4, mean(x1$dst_host_same_srv_rate, na.rm = T), sd(x1$dst_host_same_srv_rate, na.rm = T))
plot(sort(normal.quantiles4), sort(x1$dst_host_same_srv_rate) , xlab = '',
     ylab = 'Quantiles of dst_host_same_srv_rate', main = '',col=4)
abline(0,1,col=2)

n5 = length(x1$dst_host_count)
probabilities5 = (1:n5)/(n5+1)
normal.quantiles5 = qnorm(probabilities5, mean(x1$dst_host_count, na.rm = T), sd(x1$dst_host_count, na.rm = T))
plot(sort(normal.quantiles5), sort(x1$dst_host_count) , xlab = '',
     ylab = 'Quantiles of dst_host_count', main = '',col=4)
abline(0,1,col=2)

n6 = length(x1$hot)
probabilities6 = (1:n6)/(n6+1)
normal.quantiles6 = qnorm(probabilities6, mean(x1$hot, na.rm = T), sd(x1$hot, na.rm = T))
plot(sort(normal.quantiles6), sort(x1$hot) , xlab = '',
     ylab = 'Quantiles of hot', main = '',col=4)
abline(0,1,col=2)

n7 = length(x1$srv_diff_host_rate)
probabilities7 = (1:n7)/(n7+1)
normal.quantiles7 = qnorm(probabilities7, mean(x1$srv_diff_host_rate, na.rm = T), sd(x1$srv_diff_host_rate, na.rm = T))
plot(sort(normal.quantiles7), sort(x1$srv_diff_host_rate) , xlab = '',
     ylab = 'Quantiles of srv_diff_host_rate', main = '',col=4)
abline(0,1,col=2)

n8 = length(x1$rerror_rate)
probabilities8 = (1:n8)/(n8+1)
normal.quantiles8 = qnorm(probabilities8, mean(x1$rerror_rate, na.rm = T), sd(x1$rerror_rate, na.rm = T))
plot(sort(normal.quantiles8), sort(x1$rerror_rate) , xlab = 'Theoretical Quantiles from Normal Distribution',
     ylab = 'Quantiles of rerror_rate', main = '',col=4)
abline(0,1,col=2)


