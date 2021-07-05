# Victor Lhoste
# Mahalanobis outlier detection 
# July 2021



rm(list = ls())

library(rrcov)
library(MASS)  # only for simulating the data


mahalanobis_plot=function(var1,var2,level=(1-pnorm(6))*2){     # default level is equivalent to being 6 sd away from the mean for normal distribution
  
  data=as.data.frame(cbind(var1,var2))
  b=covMcd(data,alpha=0.95)    # robust estimate of the covariance   
  
  m_dist <- mahalanobis(data, b$center,b$cov)
  data$m_dist <-  m_dist 
  
  data$outlier <- "Not detected"
  data[which(data$m_dist > qchisq(1-level, 2)),'outlier'] <- "detected"
  
  d2<-data[which(data$outlier=="detected"),]
  
  p<-ggplot(data,aes(x=var1,y=var2)) + 
    geom_bin2d(bins = 130) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()+
    geom_point(data=d2,aes(x=var1,y=var2,color=outlier), shape=21, size = 3)  +
    scale_color_manual(name="", values = c("red","red"))
  
  return(list(p, row.names(data[which(data$outlier=="detected"),])))
}





