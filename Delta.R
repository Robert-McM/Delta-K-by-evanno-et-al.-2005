
##This function calculates optimal K using the method described by evanno et al. 2005
#Requires a log probability file with two columns, V1 being the value of K, 
#and V2 being the LnProb given by STRUCTURE for that value of K
## Not run: Delta_K(x)

library(dplyr)
library(ggplot2)

Delta_K<- function(x) {
  T2<- x %>% group_by(V1) %>% summarise(St_dev= sd(V2), mean=mean(V2))
  T3<- T2 %>% filter(V1!=1) %>% select(mean)
  T3<- as.data.frame(c(0,T2$mean[-1]))
  T2<- T2[-3]
  T2<- cbind(T2,T3)
  names(T2)[3]<- "mean"
  T2<- T2 %>% mutate(LK1=NA, LK2=NA)
  
  for (i in 2:length(T2$V1)){
    T2$LK1[i]<- T2$mean[i]-T2$mean[i-1]
  }
  
  for (i in 2:length(T2$V1)){
    T2$LK2[i]<- T2$LK1[i+1]-T2$LK1[i]
  }
  
  T2<- T2 %>% mutate(delta_k=abs(LK2)/St_dev)
  
  g<- ggplot(T2, aes(x=V1, y=delta_k))+
    geom_bar(aes(y=delta_k), stat = "identity", fill="#E3FF47", alpha=0.85)+
    geom_point(col="#6347FF")+
    geom_line(col="#6347FF")+
    theme_bw()+
    labs(title = "Optimal Number of Clusters", 
         subtitle = expression(paste(Delta,"K=m(","|","L''K","|",")","/",sigma,"L(K)]")), 
         y= expression(paste(Delta, "K")), 
         x="K")+
    scale_x_continuous(breaks = seq(1,length(T2$delta_k)))
  return(g)
}
