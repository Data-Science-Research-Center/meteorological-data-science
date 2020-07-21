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
                      label = "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";"),
                      selected = ","
                    ),
                    fileInput(
                      inputId = ns("file_input_csv"),
                      width = "100%",
                      label = "file",
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
                hr(),
                div(
                  style = "font-size: 8.5pt;",
                  DT::DTOutput(ns("data_from"))
                )
              )
            )
          )
        ),
        tabPanel(
          "Temporary Graphics", # Section 2 
          fluidRow(
            column( # Menu
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Temporary Graphics"),
                  p("descripcion"),
                ),
                div(
                  radioButtons(
                    inputId = ns("radio_geom_type"), 
                    label = "Type of graphic", 
                    choices = c("Dot" = "style_a", "Line" = "style_b", "Dot & Line" = "style_c"),
                    selected = "style_a",
                    inline = TRUE
                  )
                  
                ),
                div(
                  uiOutput(ns("var1_select_td")),
                  uiOutput(ns("var2_select_td"))
                ),
                hr(),
                div(
                  p("description text"),
                  checkboxInput(
                    inputId = ns("check_able_td"),
                    label = "comparison",
                    value = FALSE
                  ),
                  uiOutput(ns("var3_select_td"))
                )
              )
            ),
            column( # Result
              width = 9,
              material_card(
                style = "background:#ffffff; height:500px",
                div(
                  style = "margin-bottom: 10px",
                  dropdownButton(
                    inputId = ns("td_config_drop"),
                    label =  NULL,
                    circle = FALSE,
                    status = "primary",
                    icon =  icon("gear"),
                    width = "35%",
                    size = "sm",
                    div(
                      style = "text-align: justify; color:#272829; font-size:9pt;",
                      h4("Chart settings", style = "text-align:center"),
                      colorSelectorInput(
                        inputId = ns("col_line1"),
                        label = "col 1",
                        choices = c("steelblue", "cornflowerblue",
                                    "firebrick", "palegoldenrod",
                                    "forestgreen"),
                        mode = "radio"
                      ),
                      colorSelectorInput(
                        inputId = ns("col_line2"),
                        label = "col 2",
                        choices = c("red", "cornflowerblue",
                                    "firebrick", "palegoldenrod",
                                    "forestgreen")
                      ),
                      selectInput(
                        inputId = ns("picker_shape_point"), 
                        label = "Dots shape", 
                        width = "25%",
                        choices = c("1" = 16, "2" = 17, "3" = 15, "4" = 4, "5" = 3),
                        selected = "1"
                      ),
                      selectInput(
                        inputId = ns("picker_shape_point2"), 
                        label = "Dots shape", 
                        width = "25%",
                        choices = c("1" = 16, "2" = 17, "3" = 15, "4" = 4, "5" = 3),
                        selected = "1"
                      ),
                      selectInput(
                        inputId = ns("picker_size_line"), 
                        label = "Size", 
                        width = "25%",
                        choices = c("0.5" = 0.5, "1" = 1, "1.5" = 1.5),
                        selected = 0.5
                      )
                    )
                  )
                ),
                hr(),
                div(
                  plotOutput(ns("td_plot"))
                )
              )
            )
          )
        ),
        tabPanel(
          "Descriptive Graphics", # Section 3
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:300px",
                div(
                  h4("Descriptive Graphics"),
                  p("descripcion"),
                ),
                div(
                  uiOutput(ns("var4_select_td")),
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  style = "margin-bottom: 10px",
                  dropdownButton(
                    inputId = ns("td_config_drop1"),
                    label =  NULL,
                    circle = FALSE,
                    status = "primary",
                    icon =  icon("gear"),
                    width = "35%",
                    size = "sm",
                    div(
                      style = "text-align: justify; color:#272829; font-size:9pt;",
                      h4("Chart settings", style = "text-align:center"),
                    )
                  )
                ),
                hr(),
                div(
                  fluidRow(
                    column(
                      width = 4,
                      plotOutput(ns("plot_hist"))
                    ),
                    column(
                      width = 4,
                      style = "border-left: solid silver 1px; border-right: solid silver 1px",
                      plotOutput(ns("plot_box"))
                    ),
                    column(
                      width = 4,
                      plotOutput(ns("plot_vio"))
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "summary", # Section 4
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
                  uiOutput(ns("fgfsdsdgfg")),
                  h1("hola")
                ),
                div(
                  
                )
              )
            )
          )
        ),
        tabPanel(
          "summary", # Section 5
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
                  uiOutput(ns("fdfdfdfdfgfgfgfgfg")),
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
  csv_names_num <- reactiveVal()
  
  # JS events
  shinyjs::hide("td_config_drop")
  shinyjs::hide("td_config_drop1")
  
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
            session$userData$DATA_CSV <- read.csv(input$file_input_csv$datapath, sep = input$sep, dec = ".", fileEncoding = "UTF-8-BOM")
            
            csv_names(names(session$userData$DATA_CSV))
            csv_data(session$userData$DATA_CSV)
            csv_names_num(names(session$userData$DATA_CSV %>% select_if(is.numeric)))
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
  
  # Vareable selector
  output$var1_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var1_td"),
        label = "x",
        choices = csv_names()
      )
    )
  })
  
  output$var2_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var2_td"),
        label = "y",
        choices = csv_names()
      )
    )
  })
  
  output$var3_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var3_td"),
        label = "y2",
        choices = csv_names()
      )
    )
  })
  
  output$var4_select_td <- renderUI({
    tagList(
      pickerInput(
        inputId = ns("picker_var4_td"),
        label = "y1",
        choices = csv_names_num()
      )
    )
  })

  observe({
    
    output$td_plot <- renderPlot({
      
      if(is.null(input$picker_var1_td)){
        
        return(NULL)
        
      }else{
        
        shinyjs::show("td_config_drop")
        
        switch(
          EXPR = input$radio_geom_type,
          "style_a" = {
            if(input$check_able_td == FALSE){
              ggplot(
                data = csv_data(),
                mapping = aes_string(x = input$picker_var1_td, y = input$picker_var2_td)
              ) + 
                geom_point(
                  shape = as.numeric(input$picker_shape_point), 
                  size = 3, 
                  color = input$col_line1
                ) +
                theme_minimal()
            }else{
              ggplot(
                data = csv_data(),
                mapping = aes_string(x = input$picker_var1_td)
              ) + 
                geom_point(
                  mapping = aes_string(y = input$picker_var2_td),
                  shape = as.numeric(input$picker_shape_point), 
                  size = 3, 
                  color = input$col_line1
                ) +
                geom_point(
                  mapping = aes_string(y = input$picker_var3_td),
                  shape = as.numeric(input$picker_shape_point2), 
                  size = 3, 
                  color = input$col_line2
                ) +
                labs(
                  x = input$picker_var1_td, 
                  y =  sprintf("%s - %s", input$picker_var2_td, input$picker_var3_td)
                ) + 
                theme_minimal() 
            }
          },
          "style_b" = {
            if(input$check_able_td == FALSE){
              ggplot(
                data = csv_data(),
                mapping = aes_string(x = input$picker_var1_td, y = input$picker_var2_td, group = 1)
              ) +
                geom_line(
                  color = input$col_line1,
                  size = as.numeric(input$picker_size_line)
                ) +
                theme_minimal()
            }else{
              ggplot(
                data = csv_data(),
                mapping = aes_string(x = input$picker_var1_td)
              ) +
                geom_line(
                  mapping = aes_string(y = input$picker_var2_td, group = 1),
                  color = input$col_line1,
                  size = as.numeric(input$picker_size_line)
                ) +
                geom_line(
                  mapping = aes_string(y = input$picker_var3_td, group = 2),
                  color = input$col_line2,
                  size = as.numeric(input$picker_size_line)
                ) +
                labs(
                  x = input$picker_var1_td, 
                  y =  sprintf("%s - %s", input$picker_var2_td, input$picker_var3_td)
                ) +
                theme_minimal()
            }
          },
          "style_c" = {
            if(input$check_able_td == FALSE){
              ggplot(
                data = csv_data(),
                mapping = aes_string(x = input$picker_var1_td, y = input$picker_var2_td)
              ) +
                geom_line(
                  mapping =aes_string(group = 1),
                  color = input$col_line1,
                  size = as.numeric(input$picker_size_line)
                ) +
                geom_point(
                  shape = as.numeric(input$picker_shape_point), 
                  size = 3, 
                  color = input$col_line1
                ) +
                theme_minimal()
            }else{
              ggplot(
                data = csv_data(),
                mapping = aes_string(x = input$picker_var1_td)
              ) +
                geom_line(
                  mapping =aes_string(y = input$picker_var2_td, group = 1),
                  color = input$col_line1,
                  size = as.numeric(input$picker_size_line)
                ) +
                geom_point(
                  mapping =aes_string(y = input$picker_var2_td),
                  shape = as.numeric(input$picker_shape_point), 
                  size = 3, 
                  color = input$col_line1
                ) + 
                geom_line(
                  mapping =aes_string(y = input$picker_var3_td, group = 2),
                  color = input$col_line2,
                  size = as.numeric(input$picker_size_line)
                ) +
                geom_point(
                  mapping = aes_string(y = input$picker_var3_td),
                  shape = as.numeric(input$picker_shape_point2), 
                  size = 3, 
                  color = input$col_line2
                ) +
                labs(
                  x = input$picker_var1_td, 
                  y =  sprintf("%s - %s", input$picker_var2_td, input$picker_var3_td)
                ) +
                theme_minimal()
            }
            
          }
        )
      }
      
    })
  })
  
  observe({
    
    output$plot_hist <- renderPlot({
      
      if(is.null(input$picker_var4_td)){

        return(NULL)

      }else{
        
        shinyjs::show("td_config_drop1")
        
        ggplot(
          data = csv_data(),
          mapping = aes_string(x = input$picker_var4_td)
        ) +
          geom_histogram(
            color="black", 
            fill="white"
          ) +
          theme_minimal()
      }
    })
    
    output$plot_box <- renderPlot({
      
      if(is.null(input$picker_var4_td)){
        
        return(NULL)
        
      }else{
        
        ggplot(
          data = csv_data(),
          mapping = aes_string(y = input$picker_var4_td)
        ) +
          geom_boxplot() +
          theme_minimal()
      }
    })
    
    # output$plot_vio <- renderPlot({
    #   
    #   if(is.null(input$picker_var4_td)){
    #     
    #     return(NULL)
    #     
    #   }else{
    #     
    #     ggplot(
    #       data = csv_data(),
    #       mapping = aes_string(y = input$picker_var4_td)
    #     ) +
    #       geom_violin() +
    #       theme_minimal()
    #   }
    # })
    
  })

}