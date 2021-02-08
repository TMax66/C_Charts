
#credentials <- list("CCbergamo" = ".#.Cc20")

credentials <- list("cc" = "cc")

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
                                        "FEBBRE_Q"), "IBR_Siero"),
                            
                            #tableOutput("tsiero"),
                            br(),
                            sliderInput("anno","anno",min=2015, max=2022,value="2021"),
                            hr(),
                            tableOutput("tsiero"),
                            hr(),
                            a(actionButton("Ins", "Inserimento nuovi dati",
                                           class = "btn-primary",
                                           icon("flask")),
                              href="https://docs.google.com/spreadsheets/d/1dsfJJy_bxJi-Fid3mPyncVvjpMfi87mqxtRQtM_7H5M/edit?usp=sharing")

                            
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
                             selectInput("wsm", "", choices = "Salmonella")))),
                            #             choices = c("Salmonella",
                            #                         "Salmonella_ct_estrazione",
                            #                         "Listeria_monocytocenes",
                            #                         "Listeria_ct_estrazione",
                            #                         "Campylobacter",
                            #                         "Campylobacter_ct_estrazione",
                            #                         ),"Salmonella")))),
               # 
               #              #tableOutput("tmicro"),
               #              br(),
               # 
               #              sliderInput("anno2","anno",min=2015, max=2022,value="2021"),
               #              hr(),
               #              tableOutput("tmicro"),
               #              hr(),
               #              a(actionButton("Ins", "Inserimento nuovi dati",
               #                             class = "btn-primary",
               #                             icon("flask")),
               #                href="https://docs.google.com/spreadsheets/d/1tmeb3a_l3YCTXkn8yNqXSDJqbx0LVTdYIKd85bcg77I/edit?usp=sharing"),
               # 
               # 
               #            ),#chiude il panello laterale
               # 
               #            mainPanel(
               # 
               # 
               #              plotlyOutput("MyPlotmicro")%>%
               #                withSpinner(color="blue", type=8),
               #              plotlyOutput("MyPlot2micro")%>%
               #                withSpinner(color="blue", type=8) ))),

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

siero <-reactive ({read_sheet(sid$id, col_types = "cdccddc",  sheet = input$ws)  })

df <-  reactive(siero()  %>%  rowwise() %>% 
  mutate(X = mean(c(ct1,ct2)), 
         data = dmy(data), 
         anno = year(data)) %>% 
  data.frame() %>% 
  mutate(R = abs(X-lag(X))) %>% 
    filter(anno==input$anno))

###Tabella sierologia####
output$tsiero <-  renderTable({
  # Sys.sleep(3)
  df() %>% tibble() %>% 
    select(anno,piastra, ct1, ct2, X, R) %>% 
  arrange(desc(piastra)) %>%
    head(10)
})
   
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


# ##Dati per grafici microbiologia####
# 
# micro <-reactive ({read_sheet(mid$id, col_types = "cddc",  sheet = input$wsm)  })
# 
# dfm <-  reactive(micro() %>%   
#                    mutate(data = dmy(data),
#                           anno = year(data),
#                           R = abs(X-lag(X))) %>% 
#                   filter(anno==input$anno))
# 
# 
# ###Tabella microbiologia####
# output$tmicro <-  renderTable({
#   # Sys.sleep(3)
#   dfm() %>% tibble() %>% 
#     select(data,piastra,  X, R) %>% 
#     arrange(desc(piastra)) %>%
#     head(10)
# })
# 
# 
# ####Grafico X####
# output$MyPlotmicro <- renderPlotly({
#   Sys.sleep(2)
#   meanx<-mean(dfm()$X,na.rm=T)
#   xul<-mean(dfm()$X, na.rm = T)+2.66*mean(dfm()$R, na.rm=T)
#   xil<-mean(dfm()$X, na.rm = T)-2.66*mean(dfm()$R, na.rm=T)
#   xends<-max(dfm()$piastra, na.rm=TRUE)
#   
#   ggplotly(ggplot(dfm(), aes(x = piastra, y = X)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
#              geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
#              geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
#              geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
#              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
#              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
#              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
#   
# })
# 
# 
# 
# ####Grafico R####
# 
# output$MyPlot2micro <- renderPlotly({
#   Sys.sleep(2)
#   meanx<-mean(dfmicro()$R,na.rm=T)
#   xul<-3.26*mean(dfmicro()$R, na.rm=T)
#   xil<-0
#   xends<-max(dfmicro()$piastra, na.rm=TRUE)
#   
#   ggplotly(ggplot(dfmicro(), aes(x = piastra, y = R)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
#              geom_segment(aes(x=piastra,xend=xends,y=meanx,yend=meanx), color='blue', linetype=1,size=0.2)+
#              geom_segment(aes(x=piastra,xend=xends,y=xul,yend=xul), color='blue', linetype=1, size=0.2)+
#              geom_segment(aes(x=piastra,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
#              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = meanx, label = paste("LC=", round(meanx,3))),size=3)+
#              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xul, label = paste("LSup=", round(xul,3))), size=3)+
#              geom_text(aes(x = max(piastra, na.rm=TRUE)+10, y = xil, label = paste("LInf=", round(xil,3))),size=3))
# })
# 
# 


##Tabella validazione####
output$validazione<-renderTable({
    
    valid<-read.csv("validazione.csv", header=T, sep=";", dec=',')
    valid
    
    
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
  

