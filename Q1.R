sam_size=c()
N=10
bm=c()
bv=c()
pm=c()
pv=c()
nm=c()
nv=c()
bn1m=c()
bn1v=c()
bn2m=c()
bn2v=c()

for(j in 1:100){
  n=j*N
  print(j)
  
  




  for(i in 1:100){
    sam_size=append(sam_size,n)
    
    
    
    
  
    binom_sample=rbinom(n,12,0.25)
    #binom_sample
    
    binom_mean=mean(binom_sample)
    binom_variance=(n*var(binom_sample))/(n-1)
    bm=append(bm,binom_mean)
    bv=append(bv,binom_variance)
    
    poi_sample=rpois(n,5.7)
    poi_sample
    poi_mean=mean(poi_sample)
    poi_variance=(n*var(poi_sample))/(n-1)
    pm=append(pm,poi_mean)
    pv=append(pv,poi_variance)
    
    
    
    
    norm_sample=rnorm(n,mean=3.5,sd=sqrt(5.75))
    norm_sample
    norm_mean=mean(norm_sample)
    norm_variance=(n*var(norm_sample))/(n-1)
    nm=append(nm,norm_mean)
    nv=append(nv,norm_variance)
    
    
    mu=c(15.7,42.8)
    s1=9.2
    s2=11.7
    rho=0.79
    sigma=matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
    
    biv_norm_sample=as.data.frame(mvrnorm(n, mu = mu, Sigma = sigma ))
    colnames(biv_norm_sample)=c('BN1','BN2')
    biv_norm_sample
    bn1_mean=mean(biv_norm_sample$BN1)
    bn1_variance=(n*var(biv_norm_sample$BN1))/(n-1)
    bn1m=append(bn1m,bn1_mean)
    bn1v=append(bn1v,bn1_variance)
    
    bn2_mean=mean(biv_norm_sample$BN2)
    bn2_variance=(n*var(biv_norm_sample$BN2))/(n-1)
    bn2m=append(bn2m,bn2_mean)
    bn2v=append(bn2v,bn2_variance)
  
  }
}

sum_df=data.frame(
sam_size,
bm,
bv,
pm,
pv,
nm,
nv,
bn1m,
bn1v,
bn2m,bn2v)



colnames(sum_df)=c('Sample_Size','Binomial Mean','Binomial Variance','Poisson Mean','Poisson Variance','Normal mean','Normal variance',
                   'BivNorm1mean','BivNorm1variance','BivNorm2mean','BivNorm2Variance')

  


summary_df=sum_df%>%group_by(Sample_Size)%>%summarise_all(mean)
write.csv(sum_df,'D:/sum_df.csv')
write.csv(summary_df,'D:/summary_df.csv')
