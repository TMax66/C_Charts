library(shiny)
library(datasets)
library(qicharts2)
library(ggplot2)
library(plotly)
library(googlesheets)
library(dplyr)
library(tidyverse)
library(lubridate)

########################
rm(list=ls())
ccsiero <-gs_title("ccsierologia")
ccsiero<-gs_title("ccsierologiaSO")
#ccmicro <-gs_title("ccmicrobiologia")


df <- gs_read(ccsiero, ws = "BVD C+")


df<-as_tibble(df)
df$data<-dmy(df$data)
df<-mutate(df,anno=year(data))
df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
df$anno<-substring(as.factor(df$anno),1,4)
df$anno<-as.numeric(as.character(df$anno))



dfx<-df %>% 
  filter(anno==2019) %>% 
  rowwise() %>% 
  mutate(X=mean(c(ct1,ct2))) %>% 
  data.frame() %>% 
  mutate(R = abs(X-lag(X)))


meanx<-mean(dfx$X,na.rm=T)
xul<-mean(dfx$X, na.rm = T)+2.66*mean(dfx$R, na.rm=T)
xil<-mean(dfx$X, na.rm = T)-2.66*mean(dfx$R, na.rm=T)
xends<-max(dfx$piastra, na.rm=TRUE)

ggplotly(ggplot(dfx, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
           
           geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
           geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
           geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
           geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
           geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
           geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))

############################################

ccmicro <-gs_title("ccmicrobiologia")


df <- gs_read(ccmicro, ws = "Salmonella")

df$data<-dmy(df$data)
df<-mutate(df,anno=year(data))
df<-na.omit(df)
df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
df$anno<-substring(as.factor(df$anno),1,4)
dfx<-df %>% 
  mutate(R = abs(X-lag(X)))


meanx<-mean(dfx$X,na.rm=T)
xul<-mean(dfx$X, na.rm = T)+2.66*mean(dfx$R, na.rm=T)
xil<-mean(dfx$X, na.rm = T)-2.66*mean(dfx$R, na.rm=T)
xends<-max(dfx$piastra, na.rm=TRUE)

ggplotly(ggplot(dfx, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
           
           geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
           geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
           geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
           geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
           geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
           geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))





