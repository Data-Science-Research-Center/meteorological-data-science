################################################################################
# Module solution area
#
# Author: Cristian Pazmiño
# Created: 2020-04-20 16:43:05
################################################################################

# Solution area UI -----
solution_area_ui<-function(id){
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      tabsetPanel(
        type = "tabs",
        tabPanel( 
          "Data", # Section 1
          fluidRow(
            column( # Menu
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt;",
                fluidRow(
                  column(
                    width = 12,
                    h4("Titulo"),
                    p("descripcion"),
                    fileInput(
                      inputId = ns("file_input_csv"),
                      width = "100%",
                      label = NULL,
                      buttonLabel = span(class="ti-file", style = "font-size:22pt"),
                      accept = c("text/csv","text/comma-separated-values",".csv")
                    ),
                    radioButtons(
                      ns("sep"), 
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";"),
                      selected = ","
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    p("If you need a guide to the data schema, download it here"), 
                  )
                )
              )
            ),
            column( # Result
              width = 9,
              material_card(
                style = "background:#ffffff; height:500px; color:#272829",
                div(
                  uiOutput(ns("data_title"))
                ),
                div(
                  style = "font-size: 8.5pt",
                  DT::DTOutput(ns("data_from"))
                )
              )
            )
          )
        ),
        tabPanel(
          "Temporary Data", # Section 2 
          fluidRow(
            column( # Menu
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:300px",
                div(
                  h4("Temporary Data"),
                  p("descripcion"),
                ),
                div(
                  actionBttn(
                    inputId = ns("btt_load_td"),
                    label = "Graph",
                    size = "xs"
                  )
                )
              )
            ),
            column( # Result
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  uiOutput(ns("td_tittle")),
                ),
                div(
                  plotOutput(ns("td_plot"))
                )
              )
            )
          )
        ),
        tabPanel(
          "Summary", # Section 3
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:300px",
                div(
                  h4("Titulo"),
                  p("descripcion"),
                ),
                div(
                  
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  uiOutput(ns("fgfgfgfgfg")),
                  h1("hola")
                ),
                div(
                  
                )
              )
            )
          )
        )
        
      )
    )
  )
}

# Solution area SERVER -----
solution_area_server<-function(input, output,session){
  ns <- session$ns
  
  # Load CSV area
  fun_load_csv(input,output,session)
  
  # Reload CSV data values
  proxyData = dataTableProxy('data_from')
  observeEvent(input$data_from_cell_edit,{
    infor = input$data_from_cell_edit
    i = infor$row
    j = infor$col + 1
    v = infor$value
    session$userData$DATA_CSV[i, j] <<- DT::coerceValue(v, session$userData$DATA_CSV[i, j])
    replaceData(proxyData, session$userData$DATA_CSV, resetPaging = FALSE, rownames = FALSE)
  })
  
  # Load Temporary Data
  fun_td(input,output,session)
  # Temporary Data Plot
  # output$td_plot <- renderPlot({
  # plot(x = session$userData$DATA_CSV$Humedad.Relativa.Aire, y = session$userData$DATA_CSV$Temperatura.Aire)
  # cat(as.character( names(session$userData$DATA_CSV)))
  # })
  #     
  #   
  # 
  
}

# Function Load CSV area
fun_load_csv <- function(input,output,session){
  output$data_from <- DT::renderDT({
    if(is.null(input$file_input_csv)){
      
      return((NULL))
      
    }else{
      data_ext <- tools::file_ext(toString(input$file_input_csv))
      
      if(data_ext != "csv"){
        
        shinyjs::hide("data_config")
        
        output$data_title <- renderUI({
          h4("incorrecto", style = "color:red")
        })
        
        return((NULL))
        
      }else{
        shinyjs::show("data_config")
        
        tryCatch(
          {
            session$userData$DATA_CSV <- read.csv(input$file_input_csv$datapath, sep = input$sep, fileEncoding = "UTF-8-BOM")
          },
          error = function(e) {
            stop(safeError(e))
          }
        )
        
        output$data_title <- renderUI({
          h4("texto")
        })
        
        session$userData$DATA_CSV %>%
          DT::datatable(
            extensions = "Scroller",
            editable = "cell",
            options = list(responsive = TRUE, scrollY = 360, scrollX =TRUE, scroller = TRUE, searching = FALSE, dom = "ftip"),
            selection = list(mode = "single"),
            class = "display compact",
            rownames = FALSE
          )
        
      }
    }
  })
}

# Fucntion Load Temporary Data
fun_td <- function(input,output,session){
  
  var_td <- reactive({
    names(session$userData$DATA_CSV)
  })
  
  observeEvent(input$btt_load_td,{
    output$td_plot <- renderPlot({
      plot(x = session$userData$DATA_CSV$Humedad.Relativa.Aire, y = session$userData$DATA_CSV$Temperatura.Aire)
      })
  })
}

