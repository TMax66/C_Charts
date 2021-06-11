
credentials <- list("ccforli" = "ccforli")

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

###UI###########################################################################

###controllo password####
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

###UI___________________________________________________
navbarPage("S.T. di Forlì - Carte di controllo (ver 0 del 11/06/2021)",
               ### Pannello Sierologia####
               tabPanel("Sierologia",
                        fluidPage(
                          sidebarPanel(
                            selectInput("par", "",
                                        choices = c("Influenza")),
                            br(),
                            
                            tableOutput("dati"), 
                            
                            sliderInput("anno","anno",min=2021, max=2021,value="2021"), 
                            
                            a(actionButton("Ins", "Inserimento nuovi dati",
                                           class = "btn-primary",
                                           icon("flask")),
                              href="https://docs.google.com/spreadsheets/d/1KWzjYlimEZkbCZS0uJ5W9kk16yN9ihvC4UaC90T2Hqw/edit?usp=sharing"),
                            
                            
                            
                            
                          ),#chiude il panello laterale
                          
                          mainPanel(
                            
                            plotlyOutput("MyPlot") %>% 
                              withSpinner(color="blue", type=8),
                            
                            plotlyOutput("MyPlot2") %>%
                              withSpinner(color="blue", type=8)))),

              ### Pannello Validazione####
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

###Server#####################################################################################################
 
datiforli<-reactive ({read_sheet(id$id, col_types = "cdcdc", sheet = input$par)  })

  
  output$dati <- renderTable({
    # Sys.sleep(3)
    datiforli() %>%
      select(data, piastra, ct1) %>% 
      arrange(desc(piastra)) %>%
      head(10)
  })
  
   
  

 

df <- reactive({datiforli() %>% 
    rowwise() %>% 
    mutate(X = ct1, 
           data = dmy(data), 
           anno = year(data)) %>% 
    data.frame() %>% 
    mutate(R = abs(X-lag(X))) %>% 
    filter(anno==input$anno)
  })

  
  output$MyPlot <- renderPlotly({
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
  
  output$MyPlot2 <- renderPlotly({

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



 
  
  
  

#### Validazione####
  

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
  

