library(ggplot2)
lambda=0.2
n=40
theoretical_mean=1/lambda
theoretical_sd=(1/lambda)/sqrt(n)
nsim=1000
set.seed(820)

exp_means=NULL
for(i in 1:nsim) exp_means=c(exp_means,mean(rexp(n,lambda)))

#distribution of sample means
hist(exp_means,col = "blue",breaks=20)
actual_mean<-mean(exp_means)
abline(v=actual_mean, col="red")

print(theoretical_mean)

sample_sd<-sd(exp_means)
print(sample_sd)

sample_variance<-sample_sd^2
print(sample_variance)

print(theoretical_sd)

theoretical_variance<-theoretical_sd^2
print(theoretical_variance)

#exp_means<-as.numeric()
hist(exp_means, prob=TRUE, col="green", main="mean distribution for rexp()", breaks=20)
lines(density(exp_means), lwd=3, col="blue")

#Coverage for confidence interval of 1/lambda
l<-seq(4,6, by=0.01)
coverage <- function(l){
  means<-rowMeans(matrix(rexp(n*nsim, rate = 0.2),nsim,n))
  ll<-means - qnorm(0.975)*sqrt(1/lambda**2/n)
  ul<-means + qnorm(0.975)*sqrt(1/lambda**2/n)
  mean(ll<l & ul>l)
}
l_coverages<-sapply(l,coverage)
qplot(l, l_coverages) + geom_hline(yintercept=0.95)


















