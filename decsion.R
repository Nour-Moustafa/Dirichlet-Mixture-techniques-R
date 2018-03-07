
lowerq = quantile(tr_density)[2]
upperq = quantile(tr_density)[4]
iqr = upperq - lowerq
upper_base = (iqr * 2) + upperq
lower_base = lowerq - (iqr * 2)

out<- matrix(,nrow=nrow(ts_density),ncol=1) 

  for(e in 1:nrow(ts))
  {
    if( ts_density[e] >= lower_base & ts_density[e] <= upper_base  )
    {
      out[e]= 0
    }
    
    else   
      out[e]=1
  } 
fe=cbind(out ,lb_ts$label)
tp= matrix(0, nrow(fe),1)
tn=matrix(0, nrow(fe),1)
fp=matrix(0, nrow(fe),1)
fn=matrix(0, nrow(fe),1)
wr_tr=matrix(0, nrow(fe),1)
wr_ts=matrix(0, nrow(fe),1)

for(h in 1:nrow(fe))
{
  if(fe[h,1] == fe[h,2] & fe[h,2] ==0)
  {
    tn[h] = 1
    
  }
  else if(fe[h,1] == fe[h,2] & fe[h,2] ==1)
  {
    tp[h,1] = 1
  }
  else if(fe[h,1] != fe[h,2] & fe[h,2] ==0)
  {
    fn[h]= 1
    
  }
  else if(fe[h,1] != fe[h,2] & fe[h,2] ==1)
  {
    fp[h]= 1
  }
}

# the evaluation measures 
tp1=sum(tp) 
tn1=sum(tn)
fp1=sum(fp) 
fn1=sum(fn)

accuracy= (tp1+tn1)/ (tp1+tn1+fp1+fn1)
fpr=fp1/(fp1+tn1) 
fnr=fn1/(fn1+tp1) 
far=(fp1+fn1)/ (tp1+tn1+fp1+fn1)  
DR=tp1/(tp1+fn1)
