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
                h4("Load data"),
                fluidRow(
                  column(
                    width = 12,
                    p("From projects"),
                    div(
                      style = "margin-top:15px",
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
                    p("From CSV file"),
                    fileInput(
                      inputId = ns("file_input_csv"),
                      label = NULL,
                      buttonLabel = span(class="ti-file", style = "font-size:22pt"),
                      accept = c("text/csv","text/comma-separated-values","text/plain",".csv")
                    ),
                    actionBttn(
                      ns("load_csv"), 
                      label = "Load", 
                      size = "xs",
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
                style = "background:#ffffff; height:480px; color:#272829",
                div(
                  style = "margin-bottom: 20px",
                  uiOutput(ns("data_title"))
                ),
                div(
                  DT::DTOutput(ns("data_from")),
                  style = "font-size: 8.5pt;"
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
  
  output$data_title <- renderUI({
    tagList(
      h1("bonita caratula")
    )
  })
  
  load_from_projects(input, output, session)
 
  load_from_scv(input, output, session)
}

load_from_projects <- function(input, output, session){
  observeEvent(input$load_project,{
    
    data_select_path <- reactive({
      path_json <- data_specific(ID_GLOBAL_PROJECT) %>%
        select(projectData)
      fromJSON(txt = sprintf("%s",path_json), simplifyDataFrame = TRUE)
    })

    data_select_title <- reactive({
      path_json <- data_specific(ID_GLOBAL_PROJECT) %>%
        select(projectName)
    })
    
    if(ID_GLOBAL_PROJECT == ""){
      sendSweetAlert(
        session = session,
        title = NULL,
        width = 300,
        showCloseButton = TRUE,
        btn_labels = NA,
        text = fluidRow(
          column(
            width = 12,
            br(),
            p("texto no a seleccionado proyecto")
          )
        ),
        html = TRUE
      )
    }else{
      assign("DATA_GLOBAL_PROJECT", data_select_path(), envir = .GlobalEnv )
      
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
            options = list(responsive = TRUE, scrollY = 325, scrollX =TRUE, scroller = TRUE, searching = FALSE, dom = "ftip"),
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
    })
    
  })
}

load_from_scv <- function(input, output, session){
  
  observeEvent(input$load_csv,{
    
    if(is.null(input$file_input_csv)){
      sendSweetAlert(
        session = session,
        title = NULL,
        width = 300,
        showCloseButton = TRUE,
        btn_labels = NA,
        text = fluidRow(
          column(
            width = 12,
            br(),
            p("texto no a seleccionado proyecto")
          )
        ),
        html = TRUE
      )
    }else{
      # data_read <- eventReactive(input$file_input_csv, {
      #   read.csv(input$file_input_csv$datapath)
      # })
      data_read <- read.csv(input$file_input_csv$datapath)
      
      output$data_from<-DT::renderDT({
        # req(data_read())
        
        data_read %>%
          DT::datatable(
            extensions = "Scroller",
            editable = "cell",
            options = list(responsive = TRUE, scrollY = 325, scrollX =TRUE, scroller = TRUE, searching = FALSE, dom = "ftip"),
            selection = list(mode = "single"),
            class = "display compact",
            rownames = FALSE
          )
      }) 
    }
    
   
    
  })
}