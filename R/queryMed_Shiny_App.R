################################################
#' Shiny App for drugs interaction vizualisation
#' 
#' @details This app proposes a graph, radarplot or heatmap representation of potential drug-drug interaction as listed in DIKB data base.
#' 
#' @param shiny.object object drug(s) fill in by the user
#' @param shiny.level.precipitant interaction level between choosen object and precipitants (from largest ATC level to precipitant drugs name)
#' 
#' @import shiny
#' @import shinywidgets
#' @import shinydashboard
#' 
#' @author S. Tessier
#' 
#' @seealso [DIKB] dataset 
#' data(DIKB)

ui <- bootstrapPage(useShinyjs(),
                    useSweetAlert(),
                    dashboardPage(skin = 'black',
                                  title = "",
                                  dashboardHeader(
                                    title = "DRUGS INTERACTIONS",
                                    titleWidth = 350
                                  ),
                                  dashboardSidebar(width = 0),
                                  dashboardBody(
                                    fluidRow(
                                      box(width = 2,
                                          title = "Inputs",
                                          solidHeader = TRUE, status = "primary",
                                          dropdownButton(
                                            
                                            tags$h4("List of inputs"),
                                            
                                            radioButtons("graph.type","Type of graph",
                                                         choiceNames = c("HeatMap","Graph","RadarPlot"),
                                                         choiceValues = c("H","graph","radar"),
                                                         selected = NULL),
                                            
                                            selectizeInput("shiny.object",
                                                           "Object drug",
                                                           DIKB2$object,
                                                           selected = NULL,
                                                           multiple = TRUE,
                                                           options = NULL),
                                            
                                            selectInput("shiny.level.precipitant",
                                                        "ATC level for precipitant",
                                                        choices = c("ATC 1 : anatomical main group" = 1,
                                                                    "ATC 2 : therapeutic subgroup" = 2, 
                                                                    "ATC 3 : therapeutic/pharmacological subgroup" = 3, 
                                                                    "ATC 4 : chemical/therapeutic/pharmacological subgroup" = 4, 
                                                                    "ATC 5 : chemical substance" = 5)),
                                            
                                            conditionalPanel(condition = "input['graph.type'] != 'H'",
                                                             sliderInput("nbsources",
                                                                         "Minimum number of sources",
                                                                         min = 1,max = 5,value = 1)
                                                             # ,
                                                             # textInput("palette",
                                                             #           "Change the nodes color"),
                                                             # numericInput("weight",
                                                             #              "Threshold graph nodes",
                                                             #              min = 0,
                                                             #              max = 5,
                                                             #              value = NULL)
                                                             ),

                                            actionButton("update","Plot"), 
                                            
                                            circle = TRUE, status = "primary",margin = "10px",
                                            icon = icon("angle-down"),
                                            tooltip = tooltipOptions(title = "Click to see inputs !")
                                            
                                          )
                                          ),
                                      box(width = 10,
                                          title = "Graph",
                                          solidHeader = TRUE, status = "primary",
                                          conditionalPanel(
                                            condition = "input['graph.type'] == 'H'",
                                            plotlyOutput("HeatMap") 
                                          ),
                                          conditionalPanel(
                                            condition = "input['graph.type'] != 'H'",
                                            renderPlot("GR") 
                                          )
                                      )
                                    )
                                  )
                                  
                    )
)

server <- function(input, output, session) {
  
  new.object <- eventReactive(input$update, {
    
    input$shiny.object
    
  })
  
  new.level.precipitant <- eventReactive(input$update, {
    
    input$shiny.level.precipitant
    
  })
  
  new.nbsources <- eventReactive(input$update, {
    
    input$nbsources
    
  })
  
  new.palette <- eventReactive(input$update, {
    
    input$palette
    
  })
  
  new.weight <- eventReactive(input$update, {
    
    input$weight
    
  })
  
  new.graph.type <- eventReactive(input$update, {
    
    input$graph.type
    
  })
  
  observeEvent(input$update, {
    
    if(is.null(new.object())){
      
      output$HeatMap <- renderPlotly({})
      
      output$GR <- renderPlot({})
      
      sendSweetAlert(
        session = session,
        title = "Warning!",
        text = "Please fill in a drug",
        type = "warning"
      ) 
      
    }else{
      
      output$HeatMap <- renderPlotly({
        
        pddi_heatmap(data = DIKB, object = new.object(), 
                         level.precipitant = new.level.precipitant())
        
      }) 
      
      output$GR <- renderPlot(
        
        pddi_plot(drug = new.object(), nbsources = new.nbsources(), 
                  level = new.level.precipitant(), plot = new.graph.type()
                  # ,
                  # mypalette = new.palette(), weight = new.weight()
                  )
        
      )
      
    }
    
  })
  
}


shinyApp(ui = ui, server = server)