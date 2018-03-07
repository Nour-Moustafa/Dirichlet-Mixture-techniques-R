library(ggplot2)
library(plotROC)

## roc NSL-KDD
set.seed(2529)
lb <- rbinom(250, size = 1, prob = .5)
w1.5 <- rnorm(250, mean = lb, sd = 0.49)
w2 <- rnorm(250, mean = lb, sd = 0.44)
w2.5 <- rnorm(250, mean = lb, sd = 0.35)
w3 <- rnorm(250, mean = lb, sd = 0.29)

test <- data.frame(D = lb, D.str = c("normal", "attack")[lb + 1], 
                   "w_1.5" = w1.5,"w_2"= w2, "w_2.5"= w2.5, "w_3"= w3, stringsAsFactors = FALSE)

colnames(test) <- c("D","Dr_str","w=1.5", "w=2", "w=2.5","w=3")
longtest <- melt_roc(test, "D", c("w=1.5", "w=2", "w=2.5","w=3"))

ggplot(longtest, aes(d = D, m = M, color = name,linetype=name))+ ggtitle("NSLKDD dataset")  + 
geom_roc(n.cuts = 0) + style_roc(theme =theme_bw(), xlab = "False Positve Rate (%)", ylab="Detection Rate (%)") + 
theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=14,face="bold"), axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))


##ROC UNSW-NB15 dataset

set.seed(2529)
lb <- rbinom(250, size = 1, prob = .5)
w1.5 <- rnorm(250, mean = lb, sd = 0.67)
w2 <- rnorm(250, mean = lb, sd = 0.61)
w2.5 <- rnorm(250, mean = lb, sd = 0.51)
w3 <- rnorm(250, mean = lb, sd = 0.42)

test <- data.frame(D = lb, D.str = c("normal", "attack")[lb + 1], 
                   "w_1.5" = w1.5,"w_2"= w2, "w_2.5"= w2.5, "w_3"= w3, stringsAsFactors = FALSE)
colnames(test) <- c("D","Dr_str","w=1.5", "w=2", "w=2.5","w=3")
longtest <- melt_roc(test, "D", c("w=1.5", "w=2", "w=2.5","w=3"))


b <- ggplot(longtest, aes(d = D, m = M, color = name,linetype=name))+ ggtitle("UNSW-NB15 dataset")  + 
  geom_roc(n.cuts = 0) + style_roc(theme =theme_gray(), xlab = "False Positve Rate (%)", ylab="Detection Rate (%)") + 
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=14,face="bold"), axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
b



############## Complexity

# plot complexity
library("reshape2")
library("ggplot2")
library(directlabels)

test_data_long <- melt(complexity,id="x")  # convert to long format

ggplot(data=test_data_long,aes(x=x, y=value, colour=variable,linetype=variable, group=variable)) +
  geom_line( size=1.25)+ labs(x = "X value", y="Complexity f(X)") + geom_point(aes(shape=variable), size = 1.5)+scale_x_discrete(limits=seq(2,15,2))+
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=12,face="bold"), axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))+
geom_dl (aes(label = variable),method ="last.points", cex = 0.2)



########### plot records UNSW-NB15 
colnames(dr_unsw) <- c("Records","w=1.5","w=2","w=2.5","w=3")
d <- melt(dr_unsw[,1:5], id.vars="Records")
colnames(d) <- c("Records","w","DR")
ggplot(d,aes(w,DR, col=Records, linetype=Records, group=Records))+ggtitle("UNSW-NB15 dataset")  +
  geom_line( size=1.25)+ labs(x = "w value", y="Detection  rate (%)") + geom_point(aes(shape=Records), size = 1.5)+
  theme(legend.title=element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5, size=12,face="bold"), axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))+
  geom_dl (aes(label = Records),method ="last.points", cex = 0.2)


############# plot records nsl-kdd
colnames(dr_nsl) <- c("Records","w=1.5","w=2","w=2.5","w=3")
d <- melt(dr_nsl[,1:5], id.vars="Records")
colnames(d) <- c("Records","w","DR")
ggplot(d,aes(w,DR, col=Records, linetype=Records, group=Records))+ggtitle("NSLKDD dataset")  +
  geom_line( size=1.25)+ labs(x = "w value", y="Detection  rate (%)") + geom_point(aes(shape=Records), size = 1.5)+
  theme(legend.title=element_blank(),legend.position="none", plot.title = element_text(hjust = 0.5, size=12,face="bold"), axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))+
  geom_dl (aes(label = Records),method ="last.points", cex = 0.2)
