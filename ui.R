#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)

navbarPage("KIST-EUROPE/EINOCLE",
           tabPanel("MIXTOX",
                      sidebarPanel(width=4, 
                        h2("Import data and adjust parameter"),
                        h3("Compounds"),
                        fixedRow(
                          column(6, fileInput("input1", NULL, multiple=TRUE)),
                          
                          column(3, downloadButton("downloadText", "Sample Input"))
                        ),
                        # h3("Mixture"),
                        # fileInput("input2", NULL),
                        fixedRow(
                          column(4, "Function"),
                          column(3, "EC ratio")
                          #column(5, "Molecular weight")
                        ),
                        
                        fixedRow(
                          column(4,selectInput("func", NULL, choices=list("Logit", "Logit_three", "Logit_four", "Hill", "Hill_two", "Hill_three", "Hill_four", "Weibull", "Weibull_three", "Weibull_four"), selected="Logit")),
                          column(3,numericInput("EC", NULL, value=0.05, min=0, max=1, step=0.005))
                          #column(5,textInput("MW", NULL))
                          
                        ),
                        fixedRow(
                          
                          column(7, "Volume of the mixture (ml)"),
                          column(7,numericInput("ml", NULL, value=100, min=0, max=1, step=0.5))
                          
                        ),
                        fixedRow(
                          column(4,actionButton("action_button","Submit")),
                          column(4,downloadButton("download_button", "Make report"))
                        )
                      ),
      mainPanel(width=5,
        
        
          
          
          tabsetPanel(type = "tabs",
                      tabPanel("CA and IA prediction plot", plotOutput("plot", height="400px", width="500px"),uiOutput("auth_output")),
                      
                      tabPanel("Table", column(6, strong(uiOutput("out_vol"),tableOutput("table1"))),
                               column(6,strong(uiOutput("out_vol_2"),tableOutput("table2"))),
                               column(7,strong(uiOutput("out_vol_3"),tableOutput("table3")))),
                      tabPanel("Validation plot for mixture data", plotOutput("plot_mixture", height="400px", width="500px"),
                               uiOutput("auth_output_mixture"))
          )
                      
                      
          
          
          
        
      ),
      sidebarPanel(width=3,
        h2("Validation with experimental mixture toxicity data"),
        h3("Upload Mixture Data"),
        fileInput("input2", NULL),
        fixedRow(
          column(4,actionButton("action_button_mixture","Submit")),
          column(4,downloadButton("download_button_mixture", "Make report"))
          
        )
        
      )
    
                    
           
  ),
  tabPanel("Bayesian",
    sidebarLayout(
      sidebarPanel(
        h1("Import data and select comparison"),
        fileInput("bay_input1", NULL),
        downloadButton("bay_pdf")
      ),
      mainPanel(
        h1("There is no information.")
      )
    )
 )
)