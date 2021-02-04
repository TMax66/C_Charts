
credentials <- list("CCbergamo" = ".#.Cc20")

shinyServer(function(input, output) {
USER <- reactiveValues(Logged = FALSE)

observeEvent(input$.login, {
  if (isTRUE(credentials[[input$.username]]==input$.password)){
    USER$Logged <- TRUE
  } else {
    show("message")
    output$message = renderText("Invalid user name or password")
    delay(2000, hide("message", anim = TRUE, animType = "fade"))
  }
})

#CODICE UI####
output$app = renderUI(
  if (!isTRUE(USER$Logged)) {
    fluidRow(column(width=4, offset = 4,
                    wellPanel(id = "login",
                              textInput(".username", "Username:"),
                              passwordInput(".password", "Password:"),
                              div(actionButton(".login", "Log in"), style="text-align: center;")
                    ),
                    textOutput("message")
    ))
  } else {
    navbarPage("Carte di controllo",
               
               tabPanel("Sierologia",
                        fluidPage(
                          sidebarPanel(
                            selectInput("ws", "",
                                        choices = c("Brucellosi", 
                                        "Neospora", 
                                        "BVD_C+", 
                                        "ScreenPTBC_C+ ", 
                                        "ConfPTBC_C+",
                                        "IBR_latte",
                                        "IBR_Siero", 
                                        "Mycoplasma_agal", 
                                        "LEB ", 
                                        "FEBBRE_Q")),
                            
                            #tableOutput("tsiero"),
                            br(),
                            
                            sliderInput("anno","anno",min=2015, max=2022,value="2021")
                            
                          ),#chiude il panello laterale
                          
                          mainPanel(
                            
                            # conditionalPanel(
                            #   condition = "input.ws == 'Brucellosi'",
                            plotlyOutput("MyPlot") %>% 
                              withSpinner(color="blue", type=8),
                            
                            plotlyOutput("MyPlot2") %>%
                              withSpinner(color="blue", type=8)))),
               
               
               tabPanel("Microbiologia Alimenti",
                        fluidPage(
                          sidebarPanel(
                            # selectInput("wsm", "",
                            #             choices = ccmicro$ws$ws_title),
                            
                            #tableOutput("tmicro"),
                            br(),
                            
                            sliderInput("anno2","anno",min=2015, max=2022,value="2019")
                            
                          ),#chiude il panello laterale
                          
                          mainPanel(
                            
                            
                            plotlyOutput("MyPlotmicro")%>%
                              withSpinner(color="blue", type=8),
                            plotlyOutput("MyPlot2micro")%>%
                              withSpinner(color="blue", type=8) ))),
               
               tabPanel("Validazione",
                        fluidPage(
                          fluidRow( 
                            column(12, div( 
                              includeHTML('validazione.html')
                            ))
                          ),
                          
                          hr(),
                          
                          fluidRow(
                            column(4, div(
                              tableOutput("validazione")
                            )),
                            
                            column(8, div( 
                              plotlyOutput("validX"),
                              plotlyOutput("validR")
                            ))
                          )
                          
                          
                        ))
               
               
    )
    
  }    
    
)    

# ccsiero <-gs_title("ccsierologia")
# ccmicro <-gs_title("ccmicrobiologia")
 
  # worksheet <- reactive({
  #   input$ws
  # })
  # 
  # worksheetmicro<- reactive({
  #   input$wsm
  # })


#CODICE SERVER####
##Dati per grafici sierologia####

dati <-reactive ({read_sheet(sid$id, col_types = "cdccddc",  sheet = input$ws)  })
  

###Tabella sierologia####
  output$tsiero <-  renderTable({
    # Sys.sleep(3)
    dati() %>%
      arrange(desc(piastra)) %>%
      head(10)
  })
  
###Grafici sierologia####
df <-  reactive(dati()  %>%  rowwise() %>% 
  mutate(X = mean(c(ct1,ct2)), 
         data = dmy(data), 
         anno = year(data)) %>% 
  data.frame() %>% 
  mutate(R = abs(X-lag(X))) %>% 
    filter(anno==input$anno))

  
####Grafico X####
output$MyPlot <- renderPlotly({
  Sys.sleep(2)
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


####Grafico R####
output$MyPlot2 <- renderPlotly({
  Sys.sleep(2)
  meanx<-mean(df()$R,na.rm=T)
  xul<-3.26*mean(df()$R, na.rm=T)
  xil<-0
  xends<-max(df()$piastra, na.rm=TRUE)

  ggplotly(ggplot(df(), aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
             geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
             geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
})




 
##Tabella microbiologia#####
  # output$tmicro <- renderTable({
  #   Sys.sleep(3)
  # 
  #   dati <- gs_read(ccmicro, ws = worksheetmicro())#,locale = readr::locale(decimal_mark = ","))
  #   dati %>%
  #     arrange(desc(piastra)) %>%
  #     select(data,piastra) %>% #,X,R) %>%
  #     head()
  # 
  # 
  # })


##Tabella validazione####
output$validazione<-renderTable({
    
    valid<-read.csv("validazione.csv", header=T, sep=";", dec=',')
    valid
    
    
  })
  
  # output$MyPlot <- renderPlotly({
  #   Sys.sleep(3)
  #   df <- gs_read(ccsiero, ws = worksheet())
  #   df<-as_tibble(df)
  #   df$data<-dmy(df$data)
  #   df<-mutate(df,anno=year(data))
  #   df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
  #   df$anno<-substring(as.factor(df$anno),1,4)
  #   dfx<-df %>% 
  #     filter(anno==input$anno) %>% 
  #     rowwise() %>% 
  #     mutate(X=mean(c(ct1,ct2))) %>% 
  #     data.frame() %>% 
  #     mutate(R = abs(X-lag(X)))
  #   
  #   
  #   meanx<-mean(dfx$X,na.rm=T)
  #   xul<-mean(dfx$X, na.rm = T)+2.66*mean(dfx$R, na.rm=T)
  #   xil<-mean(dfx$X, na.rm = T)-2.66*mean(dfx$R, na.rm=T)
  #   xends<-max(dfx$piastra, na.rm=TRUE)
  #   
  #   ggplotly(ggplot(dfx, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
  #              
  #              geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
  #              geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
  #              geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
  #              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
  #              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
  #              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
  #   
  #    })
  # 
#   output$MyPlot2 <- renderPlotly({
#     Sys.sleep(3)
#     df <- gs_read(ccsiero, ws = worksheet())#,locale = readr::locale(decimal_mark = ","))
#     df<-as_tibble(df)
#     df$data<-dmy(df$data)
#     df<-mutate(df,anno=year(data))
#     df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
#     df$anno<-substring(as.factor(df$anno),1,4)
#     dfx<-df %>% 
#       filter(anno==input$anno) %>% 
#       rowwise() %>% 
#       mutate(X=mean(c(ct1,ct2))) %>% 
#       data.frame() %>% 
#       mutate(R = abs(X-lag(X)))
# 
#     meanx<-mean(dfx$R,na.rm=T)
#     xul<-3.26*mean(dfx$R, na.rm=T)
#     xil<-0
#     xends<-max(dfx$piastra, na.rm=TRUE)
#     
#         ggplotly(ggplot(dfx, aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
#                 
#                    geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
#                    geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
#                    geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
#                    geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
#                    geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
#                    geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
#   
#   
# })
#   
# 
#   output$MyPlotmicro <- renderPlotly({
#     Sys.sleep(3)
#     df <- gs_read(ccmicro, ws = worksheetmicro())
#     df<-as_tibble(df)
#     df$data<-dmy(df$data)
#     df<-mutate(df,anno=year(data))
#     #df<-na.omit(df)
#     df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
#     df$anno<-substring(as.factor(df$anno),1,4)
#     dfx<-df %>% 
#       filter(anno==input$anno2) %>% 
#       mutate(R = abs(X-lag(X)))
#     
#     
#     meanx<-mean(dfx$X,na.rm=T)
#     xul<-mean(dfx$X, na.rm = T)+2.66*mean(dfx$R, na.rm=T)
#     xil<-mean(dfx$X, na.rm = T)-2.66*mean(dfx$R, na.rm=T)
#     xends<-max(dfx$piastra, na.rm=TRUE)
#     
#     ggplotly(ggplot(dfx, aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
#                
#                geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
#                geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
#                geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
#                geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
#                geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
#                geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
#     
#   })
#   
#   output$MyPlot2micro <- renderPlotly({
#     Sys.sleep(3)
#     df <- gs_read(ccmicro, ws = worksheetmicro())#,locale = readr::locale(decimal_mark = ","))
#     df<-as_tibble(df)
#     df$data<-dmy(df$data)
#     df<-mutate(df,anno=year(data))
#     #df<-na.omit(df)
#     df$anno<-as.Date((paste(df$anno,"-01","-01",sep="")))
#     df$anno<-substring(as.factor(df$anno),1,4)
#     dfx<-df %>% 
#       filter(anno==input$anno2) %>% 
#       mutate(R = abs(X-lag(X)))
#     
#     meanx<-mean(dfx$R,na.rm=T)
#     xul<-3.26*mean(dfx$R, na.rm=T)
#     xil<-0
#     xends<-max(dfx$piastra, na.rm=TRUE)
#     ggplotly(ggplot(dfx, aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
#                
#                geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
#                geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
#                geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
#                geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
#                geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
#                geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
#     
#     
#   })
#   
  
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
  

