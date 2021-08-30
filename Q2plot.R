N=c(5,15,30,50,100,500,1000)
ns=N
sizes=c()
times=c()
exp_m=c()
exp_v=c()
means=c()
vrs=()
for(k in ns){
  n=k
  
  
  for (j in N){
   
    
    
    for(i in 1:j){
      
      
      sample=rnorm(n,mean=5,sd=2)
      
      
      
      mn=mean(sample)
      vr=(n*var(sample))/(n-1)
      
      means=append(means,mn)

      vrs=append(vrs,vr)
      sizes=append(sizes,n)
      times=append(times,j)
    }
    
    
    exp_m=append(exp_m,mean(means))
    exp_v=append(exp_v,mean(vrs))
    
    
    
    
  }
  
  
  
}


dfra=data.frame(sizes,times,means)
densityplot(~ means, group = times, data = dfra[dfra$sizes==5,], auto.key = TRUE)
