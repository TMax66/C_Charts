library("shiny")
library("datasets")
library("tidyverse")
library("plotly")
library("googledrive")
library("googlesheets4")
library("lubridate")
library("shinycssloaders")
library("shinyjs")



options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
drive_auth()
gs4_auth(token = drive_token())
mydrive <- drive_find(type = "spreadsheet")
sid <- filter(mydrive, mydrive$name == "bgccsierologia")
mid <- filter(mydrive, mydrive$name == "ccmicrobiologia")

micro <- read_sheet(mid$id, col_types = "cddc",  sheet = "Salmonella")
 
dfm <-micro %>%   
      mutate(data = dmy(data),
             anno = year(data),
             R = abs(X-lag(X))) 


 
  meanx<-mean(dfm $X,na.rm=T)
  xul<-mean(dfm$X, na.rm = T)+2.66*mean(dfm$R, na.rm=T)
  xil<-mean(dfm$X, na.rm = T)-2.66*mean(dfm$R, na.rm=T)
  xends<-max(dfm$piastra, na.rm=TRUE)
  
  ggplotly(ggplot(dfm, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
  
 

    meanx<-mean(dfm$R,na.rm=T)
    xul<-3.26*mean(dfm$R, na.rm=T)
    xil<-0
    xends<-max(dfm$piastra, na.rm=TRUE)
    
    ggplotly(ggplot(dfm, aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
               geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
               geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
               geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
 
  

                 