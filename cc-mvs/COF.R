dati<- read_sheet(id$id, col_types = "cidddd", sheet = 1) 

df <- dati
df$data<-dmy(df$data)
df<-mutate(df,anno=year(data))
df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
df$anno<-substring(as.factor(df$anno),1,4)
dfx<-df %>% 
  mutate(X = DO_Cn) %>% 
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



datiMVS<- read_sheet(id$id, col_types = "cidddd", sheet = "MVS")

df <-  datiMVS %>% 
                 select(data, piastra, X = "DO_Cn") %>% 
                 mutate(data = dmy(data), 
                        anno = substring(as.factor(as.Date(year(data),"-01","-01",sep="")), 1,4),
                        R = abs(X-lag(X)))


  meanx<-mean(df$X,na.rm=T)
  xul<-mean(df$X, na.rm = T)+2.66*mean(df$R, na.rm=T)
  xil<-mean(df$X, na.rm = T)-2.66*mean(df$R, na.rm=T)
  xends<-max(df$piastra, na.rm=TRUE)
  
   
  ggplotly(ggplot(df, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
 

