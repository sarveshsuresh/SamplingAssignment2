


N=c(5,15,30,50,100,500,1000)
ns=N
sizes=c()
times=c()
exp_m=c()
exp_v=c()
for(k in ns){
  n=k


  for (j in N){
    means=c()
    vrs=c()
    
  
    for(i in 1:j){
      
    
      sample=rnorm(n,mean=5,sd=2)
      
      
      
      mn=mean(sample)
      vr=(n*var(sample))/(n-1)
      
      means=append(means,mn)
      vrs=append(vrs,vr)
    }
    
    sizes=append(sizes,n)
    times=append(times,j)
    exp_m=append(exp_m,mean(means))
    exp_v=append(exp_v,mean(vrs))
    if(n==5){
      if (j==5){
        hist(means,density=T,type='l')
        
        
      }else{
        hist(means,density = T,add=T,type='l')
      }
      
    }
    
    
    
  }
  
  
  
}


dfr=data.frame(sizes,times,exp_m,exp_v)
colnames(dfr)=c('Sample Size','Times','Expected Mean','Expected Variance')
write.csv(dfr,'D:/dfr.csv')
