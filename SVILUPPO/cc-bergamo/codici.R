options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
drive_auth()
gs4_auth(token = drive_token())
mydrive <- drive_find(type = "spreadsheet")
sid <- filter(mydrive, mydrive$name == "bgccsierologia")
 


dati <- read_sheet(sid$id, col_types = "cdccddc",  sheet = "Neospora")



df <-  dati  %>% 
                 mutate(data = dmy(data), 
                        anno = year(data))
      rowwise() %>% 
  mutate( X=mean(c(ct1,ct2)),
                        R = abs(X-lag(X)))


 
  meanx<-mean(df()$X,na.rm=T)
  xul<-mean(df()$X, na.rm = T)+2.66*mean(df()$R, na.rm=T)
  xil<-mean(df()$X, na.rm = T)-2.66*mean(df()$R, na.rm=T)
  xends<-max(df()$piastra, na.rm=TRUE)
  
  ggplotly(ggplot(df(), aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
  
})



