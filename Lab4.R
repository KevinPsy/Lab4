#Q3

library(tidyverse)


h0x<-seq(-4,4,by=0.01)
h0<-dnorm(h0x,sd = 1)
h01 <- dnorm(x1)
h1x<-seq(-1,7,by=0.01)
h1<-dnorm(h1x,mean=3,sd=1)


ggplot()+
  geom_line(aes(x=h1x,y=h1),color ="red",lwd=1.5)+
  geom_line(aes(x=h0x,y=h0),color ="blue",lwd=1.5)+
  geom_vline(aes(xintercept=qnorm(0.95,0,1)),
             linetype="dashed", size=1.2)+
  geom_polygon(data = NULL,
               aes(x = c(qnorm(0.95,0,1),seq(qnorm(0.95,0,1),4, by=0.01),4),
                   y = c(0,dnorm(seq(qnorm(0.95,0,1),4, by=0.01)), 0),
                   fill = "Type I error"), col = "black",alpha = 0.5)+
  geom_polygon(data = NULL,
               aes(x = c(-1,seq(-1,qnorm(0.95,0,1), by=0.01),qnorm(0.95,0,1)),
                   y = c(0,dnorm(seq(-1,qnorm(0.95,0,1), by=0.01),3,1), 0),
                   fill = "Type II error"), col = "black",alpha = 0.5)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 12, angle = 90),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_rect(color = 'black',fill=NA),
        legend.box.margin = margin(c(1,1,1,1)),
        legend.position = c(0,1),
        legend.text = element_text(size = 14,margin = margin(l=8)),
        plot.margin = unit(c(1.5,0.5,2,0.5),"cm"),
        legend.justification = c(0,1))+
  scale_x_continuous(breaks=c(0,3),labels=c(expression(theta[0]),expression(theta[a])))+
  scale_fill_manual(values=c("blue","red"),label = c("Type I error","Type II error"))+
  geom_text(aes(x=1.2,y=0.03),label=expression(beta),size = 7)+
  geom_text(aes(x=2,y=0.03),label=expression(alpha),size =7,color="white")


#Q4

prime <- function(x){
  if(x>2){
    p = 0
    for (i in 2:(x-1)) {
      if(x%%i==0){
        p = 1
        break
      }
    }
   if(p==1){  
   print(paste(x,"is not a prime number"))
   }
   else{
   print(paste(x,"is a prime number."))
   }
  }
  if(x==2){
    print(paste(x,"is a prime number"))
  }
  if(x<2){
    print(paste(x,"is not a prime number."))
  }
}

for (i in 1:15) {
  prime(i)
}





