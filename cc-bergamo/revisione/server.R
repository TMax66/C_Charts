shinyServer(function(input, output, session) {
  
# ccsiero <-gs_title("ccsierologia")
# ccmicro <-gs_title("ccmicrobiologia")
 
  worksheet <- reactive({
    input$ws
  })
  
  worksheetmicro<- reactive({
    input$wsm
  })
 
  output$tsiero <- renderTable({
    Sys.sleep(3)
    
    dati <- gs_read(ccsiero, ws = worksheet())#,locale = readr::locale(decimal_mark = ","))
    dati %>% 
      arrange(desc(piastra)) %>% 
      select(data,piastra,X,R) %>% 
      head()
  
    
  })
  
  
  output$tmicro <- renderTable({
    Sys.sleep(3)

    dati <- gs_read(ccmicro, ws = worksheetmicro())#,locale = readr::locale(decimal_mark = ","))
    dati %>%
      arrange(desc(piastra)) %>%
      select(data,piastra,X,R) %>%
      head()


  })

  
  output$validazione<-renderTable({
    
    valid<-read.csv("validazione.csv", header=T, sep=";", dec=',')
    valid
    
    
  })
  
  output$MyPlot <- renderPlotly({
    Sys.sleep(3)
    df <- gs_read(ccsiero, ws = worksheet())
    df<-as_tibble(df)
    df$data<-dmy(df$data)
    df<-mutate(df,anno=year(data))
    df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
    df$anno<-substring(as.factor(df$anno),1,4)
    dfx<-df %>% 
      filter(anno==input$anno) %>% 
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
    
     })
  
  output$MyPlot2 <- renderPlotly({
    Sys.sleep(3)
    df <- gs_read(ccsiero, ws = worksheet())#,locale = readr::locale(decimal_mark = ","))
    df<-as_tibble(df)
    df$data<-dmy(df$data)
    df<-mutate(df,anno=year(data))
    df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
    df$anno<-substring(as.factor(df$anno),1,4)
    dfx<-df %>% 
      filter(anno==input$anno) %>% 
      rowwise() %>% 
      mutate(X=mean(c(ct1,ct2))) %>% 
      data.frame() %>% 
      mutate(R = abs(X-lag(X)))

    meanx<-mean(dfx$R,na.rm=T)
    xul<-3.26*mean(dfx$R, na.rm=T)
    xil<-0
    xends<-max(dfx$piastra, na.rm=TRUE)
    
        ggplotly(ggplot(dfx, aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
                
                   geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
                   geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
                   geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
                   geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
                   geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
                   geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
  
  
})
  

  output$MyPlotmicro <- renderPlotly({
    Sys.sleep(3)
    df <- gs_read(ccmicro, ws = worksheetmicro())
    
    df$data<-dmy(df$data)
    df<-mutate(df,anno=year(data))
    df<-na.omit(df)
    df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
    df$anno<-substring(as.factor(df$anno),1,4)
    dfx<-df %>% 
      filter(anno==input$anno2) %>% 
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
    
  })
  
  output$MyPlot2micro <- renderPlotly({
    Sys.sleep(3)
    df <- gs_read(ccmicro, ws = worksheetmicro())#,locale = readr::locale(decimal_mark = ","))
    df$data<-dmy(df$data)
    df<-mutate(df,anno=year(data))
    df<-na.omit(df)
    df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
    df$anno<-substring(as.factor(df$anno),1,4)
    dfx<-df %>% 
      filter(anno==input$anno2) %>% 
      mutate(R = abs(X-lag(X)))
    
    meanx<-mean(dfx$R,na.rm=T)
    xul<-3.26*mean(dfx$R, na.rm=T)
    xil<-0
    xends<-max(dfx$piastra, na.rm=TRUE)
    ggplotly(ggplot(dfx, aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
               
               geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
               geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
               geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
    
    
  })
  
  
  output$validR <- renderPlotly({

    df <- read.csv("validazione.csv", header=T, sep=";", dec=',')
    
    meanx<-mean(df$R,na.rm=T)
    xul<-3.26*mean(df$R, na.rm=T)
    xil<-0
    xends<-max(df$piastra, na.rm=TRUE)
    ggplotly(ggplot(df, aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
               
               geom_segment(aes(x=0,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
               geom_segment(aes(x=0,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
               geom_segment(aes(x=0,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
    
    
  })
  
  
  output$validX <- renderPlotly({
    
    df <- read.csv("validazione.csv", header=T, sep=";", dec=',')
    
    meanx<-mean(df$X,na.rm=T)
    xul<-mean(df$X, na.rm = T)+2.66*mean(df$R, na.rm=T)
    xil<-mean(df$X, na.rm = T)-2.66*mean(df$R, na.rm=T)
    xends<-max(df$piastra, na.rm=TRUE)
    
    ggplotly(ggplot(df, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
               
               geom_segment(aes(x=0,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
               geom_segment(aes(x=0,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
               geom_segment(aes(x=0,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
               geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
    
  })
  
  
  
  
})
  

