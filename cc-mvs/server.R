
credentials <- list("MVS" = "mvs")

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
navbarPage("Carte di controllo",
               
               tabPanel("CC",
                        fluidPage(
                          sidebarPanel(
                            selectInput("ws", "Seleziona la prova",
                                        choices = c("mvs", "sierotipoA")),
                            selectInput("par", "Seleziona il parametro", 
                                        choices = c("DO_Cn", "ICn7.5", "Cp202", "Cp7.5")), 
                            br(),
                            
                            tableOutput("dati"), 
                            
                            #sliderInput("anno","anno",min=2015, max=2022,value="2019")
                            
                            a(actionButton("Ins", "Inserimento nuovi dati",
                                           class = "btn-primary",
                                           icon("flask")),
                              href="https://docs.google.com/spreadsheets/d/1QMIQGZGB4oqIj6QNJdg6YaGZ1jc5PTmJz4-NP-YtLYs/edit?usp=sharing"),
                            
                            
                            
                            
                          ),#chiude il panello laterale
                          
                          mainPanel(
                            
                            plotlyOutput("MyPlot") %>% 
                              withSpinner(color="blue", type=8),
                            
                            plotlyOutput("MyPlot2") %>%
                              withSpinner(color="blue", type=8)))),
               
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
 

dati<-reactive ({read_sheet(id$id, col_types = "cidddd", sheet = input$ws)  })
  
  
  output$dati <- renderTable({
    # Sys.sleep(3)
    dati() %>%
      arrange(desc(piastra)) %>%
      head(10)
  })
  
   
  
output$validazione<-renderTable({
    
    valid<-read.csv("validazione.csv", header=T, sep=";", dec=',')
    valid
    
    
  })
###########################################################################################

df <- reactive(dati() %>% 
  select(data, piastra, X = input$par) %>% 
    mutate(data = dmy(data), 
           anno = substring(as.factor(as.Date(year(data),"-01","-01",sep="")), 1,4),
           R = abs(X-lag(X))))

  
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
  

