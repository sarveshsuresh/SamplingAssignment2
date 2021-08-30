areas=c(98,  270, 79, 273, 130,	158, 116, 194,  41, 33, 78, 56, 58, 19, 64, 81, 141, 58, 29, 46, 93, 127,
        114, 88, 108, 58, 47, 69, 44, 56, 102, 102, 187, 161, 179, 76, 137, 179, 76, 137, 127,
        104, 117, 170, 210, 101, 222, 223, 96, 114, 318, 272, 155, 292, 240, 201, 261, 189 )
N=length(areas)
n=8
##WOR
sample_wor=sample(areas,n,replace=FALSE)
mu_est_wor=mean(sample_wor)
mu_est_wor
ss=var(sample_wor)*n/(n-1)
var_xbar_est=((N-n)/(N*n))*ss
var_xbar_est
t.test(sample_wor,conf.level = 0.95)

#WR
sample_wr=sample(areas,n,replace=TRUE)
mu_est_wr=mean(sample_wr)
mu_est_wr
ss=var(sample_wr)*n/(n-1)
var_xbar_est=ss/n
var_xbar_est


t.test(sample_wr,conf.level = 0.95)
