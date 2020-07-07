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
          "Data Viewer", # Section 1
          fluidRow(
            column( # Menu
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt;",
                fluidRow(
                  column(
                    width = 12,
                    h4("Data Viewer"),
                    p("descripcion"),
                    br(),
                    radioButtons(
                      ns("sep"), 
                      "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";"),
                      selected = ","
                    ),
                    fileInput(
                      inputId = ns("file_input_csv"),
                      width = "100%",
                      label = NULL,
                      buttonLabel = span(class="ti-file", style = "font-size:22pt"),
                      accept = c("text/csv","text/comma-separated-values",".csv")
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
                  style = "font-size: 8.5pt;",
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
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Temporary Data"),
                  p("descripcion"),
                ),
                div(
                  uiOutput(ns("var1_select_td")),
                  uiOutput(ns("var2_select_td"))
                ),
                hr(),
                div(
                  p("comparison"),
                  uiOutput(ns("var3_select_td"))
                ),
                # div(
                #   colorSelectorInput(
                #     inputId = "mycolor1", label = "Pick a color :",
                #     choices = c("steelblue", "cornflowerblue",
                #                 "firebrick", "palegoldenrod",
                #                 "forestgreen")
                #   ),
                # )
              )
            ),
            column( # Result
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  fluidRow(
                    column(
                      width = 2,
                      offset = 10,
                      style = "text-align:right",
                      dropdownButton(
                        inputId = ns("td_config_drop"),
                        label =  NULL,
                        circle = TRUE,
                        status = "primary",
                        icon =  icon("gear"),
                        width = "30%",
                        size = "sm",
                        h3("hola")
                      )
                    )
                  )
                ),
                div(
                  style = "height:500px",
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
  
  # Data var reactive
  csv_names <- reactiveVal()
  csv_data <- reactiveVal()
  
  # Load CSV area
  output$data_from <- DT::renderDT({
    if(is.null(input$file_input_csv)){
      
      return((NULL))
      
    }else{
      data_ext <- tools::file_ext(toString(input$file_input_csv))
      
      if(data_ext != "csv"){
        
        output$data_title <- renderUI({
          h4("incorrecto", style = "color:red")
        })
        
        return((NULL))
        
      }else{
        
        
        tryCatch(
          {
            session$userData$DATA_CSV <- read.csv(input$file_input_csv$datapath, sep = input$sep, fileEncoding = "UTF-8-BOM")
          },
          error = function(e) {
            stop(safeError(e))
          }
        )
        
        csv_names(names(session$userData$DATA_CSV))
        csv_data(session$userData$DATA_CSV)
        
        output$data_title <- renderUI({
          h4("texto")
        })
        
        session$userData$DATA_CSV %>%
          DT::datatable(
            extensions = "Scroller",
            editable = "cell",
            options = list(responsive = TRUE, scrollY = 390, scrollX =TRUE, scroller = TRUE, searching = FALSE, dom = "ftip"),
            selection = list(mode = "single"),
            class = "display compact",
            rownames = FALSE
          )
        
      }
    }
  })
  
  # Reload CSV data values
  proxyData = dataTableProxy('data_from')
  observeEvent(input$data_from_cell_edit,{
    infor = input$data_from_cell_edit
    i = infor$row
    j = infor$col + 1
    v = infor$value
    session$userData$DATA_CSV[i, j] <<- DT::coerceValue(v, session$userData$DATA_CSV[i, j])
    replaceData(proxyData, session$userData$DATA_CSV, resetPaging = FALSE, rownames = FALSE)
    csv_data(session$userData$DATA_CSV)
  })

  output$var1_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var1_td"),
        label = "A label",
        choices = csv_names()
      )
    )
  })
  
  output$var2_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var2_td"),
        label = "A label",
        choices = csv_names()
      )
    )
  })
  
  output$var3_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var3_td"),
        label = "A label",
        choices = csv_names()
      )
    )
  })
  
  observe({
    output$td_plot <- renderPlot({
      
      if(is.null(input$picker_var1_td)){
        return(NULL)
      }else{
        ggplot2::ggplot(csv_data()) + geom_line(aes_string(x = input$picker_var1_td, y = input$picker_var2_td, group=1), color = "red")
      }
    })
  })

}