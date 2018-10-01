ui <- navbarPage("Carte di controllo",
              
tabPanel("Sierologia",
   fluidPage(
     sidebarPanel(
       selectInput("ws", "",
                   choices = ccsiero$ws$ws_title),
     
       tableOutput("tsiero")
       
     ),#chiude il panello laterale
     
     mainPanel(
     
         # conditionalPanel(
         #   condition = "input.ws == 'Brucellosi'",
           plotlyOutput("MyPlot"),
           plotlyOutput("MyPlot2") ))),
    
 
# tabPanel("Microbiologia Alimenti",
#          fluidPage(
#            sidebarPanel(
#              selectInput("wsm", "",
#                          choices = ccmicro$ws$ws_title),
#              
#              tableOutput("tmicro")
#              
#            ),#chiude il panello laterale
#            
#            mainPanel(
#              
# 
#              plotlyOutput("MyPlotmicro"),
#              plotlyOutput("MyPlot2micro") ))),

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


  
  
