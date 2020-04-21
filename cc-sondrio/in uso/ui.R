ui <- navbarPage("Carte di controllo",
              
tabPanel("Sierologia",
   fluidPage(
     sidebarPanel(
       selectInput("ws", "",
                   choices = ccsiero$ws$ws_title),
     
       #tableOutput("tsiero"),
       br(),
       
      sliderInput("anno","anno",min=2015, max=2022,value="2019")
       
     ),#chiude il panello laterale
     
     mainPanel(
     
         # conditionalPanel(
         #   condition = "input.ws == 'Brucellosi'",
           plotlyOutput("MyPlot") %>% 
             withSpinner(color="blue", type=8),
           
           plotlyOutput("MyPlot2") %>%
             withSpinner(color="blue", type=8)))),
#     
#  
# tabPanel("Microbiologia Alimenti",
#          fluidPage(
#            sidebarPanel(
#              selectInput("wsm", "",
#                          choices = ccmicro$ws$ws_title),
#              
#              #tableOutput("tmicro"),
#              br(),
#              
#              sliderInput("anno2","anno",min=2015, max=2022,value="2019")
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


  
  
