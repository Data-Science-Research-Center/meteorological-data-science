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
          "Data",
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                fluidRow(
                  column(
                    width = 12,
                    fileInput(
                      inputId = ns("file_input_csv"),
                      width = "100%",
                      label = "From CSV file",
                      buttonLabel = span(class="ti-file", style = "font-size:22pt"),
                      accept = c("text/csv","text/comma-separated-values","text/plain",".csv")
                    ),
                    radioButtons(
                      "sep_csv_file", 
                      "Separator",
                      choices = c(
                        Semicolon = ";",
                        Comma = ","),
                      selected = ";")
                  )
                ),
                # hr(),
                fluidRow(
                  column(
                    width = 12,
                    p("From projects"),
                    div(
                      # style = "margin-top:13px",
                      actionBttn(
                        ns("load_project"), 
                        label = "Load", 
                        size = "xs",
                      )
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
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; height:520px; color:#272829",
                div(
                  style = "margin-bottom: 10px",
                  fluidRow(
                    column(
                      width = 1,
                      uiOutput(ns("data_config")),
                    ),
                    column(
                      width = 11,
                      style = "margin-top:5px",
                      uiOutput(ns("data_title"))
                    )
                  )
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
          "Summary"
        ),
        tabPanel(
          "Table"
        )
      )
    )
  )
}

# Solution area SERVER -----
solution_area_server<-function(input, output,session){
  ns <- session$ns
  
  load_from_projects(input, output, session)
 
  load_from_scv(input, output, session)
}

# Load data from projects -----
load_from_projects <- function(input, output, session){
  
  
  
  observeEvent(input$load_project,{
    
    data_select_path <- reactive({
      path_json <- data_specific(session$userData$ID_GLOBAL_PROJECT) %>%
        select(projectData)
      fromJSON(txt = sprintf("%s",path_json), simplifyDataFrame = TRUE)
    })

    data_select_title <- reactive({
      path_json <- data_specific(session$userData$ID_GLOBAL_PROJECT) %>%
        select(projectName)
    })
    
    if(is.null(session$userData$ID_GLOBAL_PROJECT)){

      output$data_title <- renderUI({
        h4("no datos proyecto", style = "color:red")
      })

      return(NULL)

    }else{

      DATA_GLOBAL_PROJECT <<- data_select_path()
      
      output$data_title <- renderUI({
        req(data_select_title())
        
        tagList(
          h4(data_select_title())
        )
      })
      
      output$data_from<-DT::renderDT({
        DATA_GLOBAL_PROJECT %>%
          DT::datatable(
            extensions = "Scroller",
            editable = "cell",
            options = list(responsive = TRUE, scrollY = 360, scrollX =TRUE, scroller = TRUE, searching = FALSE, dom = "ftip"),
            selection = list(mode = "single"),
            class = "display compact",
            rownames = FALSE
          )
      })
    }
    
    proxyData = dataTableProxy('data_from')
    observeEvent(input$data_from_cell_edit,{
      info = input$data_from_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      DATA_GLOBAL_PROJECT[i, j] <<- DT::coerceValue(v, DATA_GLOBAL_PROJECT[i, j])
      replaceData(proxyData, DATA_GLOBAL_PROJECT, resetPaging = FALSE, rownames = FALSE)
      
      session$userData$DATA_GLOBAL_PROJECT <- DATA_GLOBAL_PROJECT 
    })
    
    
  })
}

# Load dara from file -----
load_from_scv <- function(input, output, session){
  
  
  output$data_from <- DT::renderDT({
    
    req(input$file_input_csv)
    
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
        
        DATA_GLOBAL_CSV <<- read.csv2(input$file_input_csv$datapath)
        
        output$data_title <- renderUI({
          h4("texto")
        })
        
        output$data_config <- renderUI({
          dropdownButton(
            inputId = "config_data_load",
            label = FALSE,
            size = "sm",
            icon = icon("sliders"),
            status = "primary",
            width = "300px",
            circle = TRUE,
            h1("hola")
          )
        })
        
        DATA_GLOBAL_CSV %>%
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
  
  proxyData = dataTableProxy('data_from')
    observeEvent(input$data_from_cell_edit,{
      info = input$data_from_cell_edit
      i = info$row
      j = info$col + 1
      v = info$value
      DATA_GLOBAL_CSV[i, j] <<- DT::coerceValue(v, DATA_GLOBAL_CSV[i, j])
      replaceData(proxyData, DATA_GLOBAL_CSV, resetPaging = FALSE, rownames = FALSE)
      
      session$userData$DATA_GLOBAL_CSV <- DATA_GLOBAL_CSV
    })
}