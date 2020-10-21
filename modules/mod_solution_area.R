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
        type = "pills",
        tabPanel( # Data Viewe - Section 1
          "Data Viewer", 
          fluidRow(
            column( 
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt;",
                fluidRow(
                  column(
                    width = 12,
                    h4("Data Viewer", style = "text-align:center"),
                    hr(),
                    radioButtons(
                      ns("sep"), 
                      label = "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";"),
                      selected = ",",
                      inline = FALSE
                    ),
                    fileInput(
                      inputId = ns("file_input_csv"),
                      width = "100%",
                      label = "Document",
                      multiple = TRUE,
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
            column( 
              width = 9,
              material_card(
                style = "background:#ffffff; height:500px; color:#272829",
                div(
                  style = "font-size: 8.5pt;",
                  DT::DTOutput(ns("data_from"))
                )
              )
            )
          )
        ),
        tabPanel( # Temporary Charts - Section 2
          "Temporary Charts",  
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Temporary Charts", style = "text-align:center"),
                  hr(),
                  radioButtons(
                    inputId = ns("diagram_chart_simple"),
                    label = "Type of chart",
                    choices = c(
                      "Line chart" = "op1s", 
                      "Dot chart" = "op2s", 
                      "Bar chart" = "op3s"),
                    selected = "op1s",
                    inline = FALSE
                  ),
                  uiOutput(ns("select_tempo_1"))
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  highchartOutput(
                    outputId = ns("ho_plot"),
                    width = "100%",
                    height = "480px"
                  )
                )
              )
            )
          )
        ),
        tabPanel( # Comparative Time Charts - Section 3
          "Comparative Charts", 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Comparative Time Charts", style = "text-align:center"),
                  hr(),
                  radioButtons(
                    inputId = ns("diagram_chart_multiple"),
                    label = "Type of chart",
                    choices = c(
                      "Line chart" = "op1m", 
                      "Dot chart" = "op2m", 
                      "Bar chart" = "op3m"),
                    selected = "op1m",
                    inline = FALSE
                  ),
                  uiOutput(ns("select_tempo_1m"))
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  highchartOutput(
                    outputId = ns("ho_plot_m"),
                    width = "100%",
                    height = "240px"
                  ),
                  highchartOutput(
                    outputId = ns("ho_plot_m2"),
                    width = "100%",
                    height = "240px"
                  )
                )
              )
            )
          )
        ),
        tabPanel( # Descriptive Charts - Section 4
          "Descriptive Charts", 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Descriptive Charts", style = "text-align:center"),
                  hr(),
                  radioButtons(
                    inputId = ns("descriptive_chart"),
                    label = "Type of chart",
                    choices = c(
                      "Boxplot chart" = "op1",
                      "Violinplot chart" = "op2",
                      "Histogram chart" = "op3"),
                    selected = "op1",
                    inline = FALSE
                  ),
                  uiOutput(ns("select_descriptive"))
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  plotOutput(
                    ns("descriptive_graph"),
                    width = "100%",
                    height = "460px"
                  )
                )
              )
            )
          )
        ),
        tabPanel( # Analytical results - Section 5
          "Analytical results", 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Analytical results", style = "text-align:center"),
                  hr(),
                  uiOutput(ns("select_analytic"))
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt;",
                div(
                  fluidRow(
                    column(
                      6,
                      uiOutput(ns("uiout_pear"))
                    ),
                    column(
                      6,
                      uiOutput(ns("uiout_ken"))
                    )
                    
                  ),
                  br(),
                  fluidRow(
                    column(
                      6,
                      uiOutput(ns("uiout_spear"))
                    ),
                    column(
                      6,
                      uiOutput(ns("uiout_v"))
                    )
                    
                  ),
                  br(),
                  fluidRow(
                    column(
                      7,
                      uiOutput(ns("uiout_mr"))
                    )
                  
                  )
                )
              )
            )
          )
        ),
        tabPanel( # Save Project - Section 6
          "Save Project", 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Save Project", style = "text-align:center"),
                  hr()
                  
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt;",
                div(
                  h4("Save Project"),
                  p("dfsdfsfsdf")
                ),
                br(),
                div(
                  fluidRow(
                    column(
                      6,
                      p("Project's information"),
                      textInput(
                        ns("input_name"),
                        label = NULL,
                        placeholder = "Project's name",
                        width = "100%"
                      ),
                      textInput(
                        ns("input_institution"),
                        label = NULL,
                        placeholder = "Institution's name",
                        width = "100%"
                      ),
                      textAreaInput(
                        ns("input_description"),
                        label = NULL,
                        placeholder = "Project description",
                        width = "100%",
                        height = "150px",
                        resize = "none"
                      )
                      
                    ),
                    column(
                      6,
                      p("Project's author"),
                      fluidRow(
                        column(
                          6,
                          textInput(
                            ns("input_a_name"),
                            label = NULL,
                            placeholder = "Name",
                            width = "100%"
                          )
                        ),
                        column(
                          6,
                          textInput(
                            ns("input_a_lastname"),
                            label = NULL,
                            placeholder = "Last name",
                            width = "100%"
                          )
                        )
                      ),
                      passwordInput(
                        ns("input_password"),
                        label = NULL,
                        placeholder = "Password",
                        width = "100%",
                      ),
                      actionBttn(
                        ns("save_project"), 
                        label = "Save", 
                        size = "xs"
                      )
                    )
                  )
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
  
  data_csv <- reactiveVal()
  numeric_names <- reactiveVal()
  numeric_data <- reactiveVal()
  
  
  # Variables to save projects
  
  p_name <- reactiveVal()
  p_institution <- reactiveVal()
  p_description <- reactiveVal()
  p_author <- reactiveVal()
  p_date <- reactiveVal()
  p_password <- reactiveVal()
  dat_ran <- reactiveVal()
  
  # File name
  fname <- reactiveVal()
  
  # Load CSV area
  output$data_from <- DT::renderDT({
    if(is.null(input$file_input_csv)){
      
      return((NULL))
      
    }else{
      
      data_ext <- tools::file_ext(toString(input$file_input_csv))
      fname(path_file(input$file_input_csv))
      
      if(data_ext != "csv"){

        return((NULL))

        
      }else{
        
        tryCatch(
          {
            session$userData$DATA_CSV <- read.csv(input$file_input_csv$datapath, sep = input$sep, dec = ".", fileEncoding = "UTF-8-BOM")
            
            data_csv(session$userData$DATA_CSV) # Reactive variable of the complete database
            
            numeric_names(names(session$userData$DATA_CSV %>% select_if(is.numeric))) # Reactive variable of numeric variable names
            
            numeric_data(session$userData$DATA_CSV %>% select_if(is.numeric)) # Reactive vareable of numeric database
            
          },
          error = function(e) {
            
            stop(safeError(e))
            
          }
        )

        session$userData$DATA_CSV %>%
          DT::datatable(
            extensions = "Scroller",
            editable = "cell",
            options = list(responsive = TRUE, scrollY = 410, scrollX =TRUE, scroller = TRUE, searching = FALSE, dom = "ftip"),
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
    data_csv(session$userData$DATA_CSV)
  })
  
  # Vareable selector for Temporary Graphics
  output$select_tempo_1 <- renderUI({
    
    tagList(
      pickerInput(
        inputId = ns("picker_tempo_1"),
        label = "Variables",
        choices = numeric_names()
      )
    )
    
  })
  
  output$select_tempo_1m <- renderUI({
    
    tagList(
      pickerInput(
        inputId = ns("picker_tempo_1m"),
        label = "Variables",
        choices = numeric_names()
      ),
      pickerInput(
        inputId = ns("picker_tempo_2m"),
        choices = numeric_names()
      )
    )
    
  })
  
  # Create the time series object
  tempo_app_simple <- reactive({

    xts(as.vector(data_csv() %>% select(input$picker_tempo_1)), order.by = as.Date(data_csv()$date, "%Y-%m-%d"))

  })
  
  tempo_app_multi1 <- reactive({
    
    xts(as.vector(data_csv() %>% select(input$picker_tempo_1m)), order.by = as.Date(data_csv()$date, "%Y-%m-%d"))
    
  })
  
  tempo_app_multi2 <- reactive({
    
    xts(as.vector(data_csv() %>% select(input$picker_tempo_2m)), order.by = as.Date(data_csv()$date, "%Y-%m-%d"))
    
  })

  
  # Temporal charts section
  observe({
    
    output$ho_plot <- renderHighchart({
      
      if(is.null(input$picker_tempo_1)){

        return(NULL)

      }else{
      
        switch(
          EXPR = input$diagram_chart_simple,
          # Line chart
          "op1s" = {
            
            tempo_app_simple() %>%
              hchart(
                type = "line"
              )
            
          },
          # Dot chart
          "op2s" = {
            
            tempo_app_simple() %>%
              hchart(
                type = "scatter"
              )
            
          },
          # Bar chart
          "op3s" = {
            
            tempo_app_simple() %>%
              hchart(
                type = "column"
              )
            
          }
        )
      
      }
      
    })
    
  })
  
  observe({

    output$ho_plot_m <- renderHighchart({

      if(is.null(input$picker_tempo_1m)){

        return(NULL)

      }else{

        switch(
          EXPR = input$diagram_chart_multiple,
          # Line chart
          "op1m" = {

            tempo_app_multi1() %>%
              hchart(
                type = "line",
                color = "green"
              )
            
          },
          # Dot chart
          "op2m" = {
            
            tempo_app_multi1() %>%
              hchart(
                type = "scatter",
                 color = "green"
              )

          },
          # Bar chart
          "op3m" = {

            tempo_app_multi1() %>%
              hchart(
                type = "column",
                color = "green"
              )

          }
        )

      }

    })

  })
  
  observe({
    
    output$ho_plot_m2 <- renderHighchart({
      
      if(is.null(input$picker_tempo_2m)){
        
        return(NULL)
        
      }else{
        
        switch(
          EXPR = input$diagram_chart_multiple,
          # Line chart
          "op1m" = {
            
            tempo_app_multi2() %>%
              hchart(
                type = "line"
              )
            
          },
          # Dot chart
          "op2m" = {
            
            tempo_app_multi2() %>%
              hchart(
                type = "scatter"
              )
            
          },
          # Bar chart
          "op3m" = {
            
            tempo_app_multi2() %>%
              hchart(
                type = "column"
              )
            
          }
        )
        
      }
      
    })
    
  })
  
  # Vareable selector for Descriptive Charts
  output$select_descriptive <- renderUI({
    
    tagList(
      pickerInput(
        inputId = ns("picker_descrip1"),
        label = "Variables",
        choices = numeric_names()
      ),
      pickerInput(
        inputId = ns("picker_descrip2"),
        choices = numeric_names()
      )
    )
    
  })
  
  
  # Decriptive charts section
  observe({

    output$descriptive_graph <- renderPlot({
      
      if(is.null(input$picker_descrip1)){
        
        return(NULL)
        
      }else{
        
        switch(
          
          EXPR = input$descriptive_chart,
          "op1" = {
            
            ggplot(
              data = data_csv(),
            ) +
              geom_boxplot(
                mapping = aes_string(
                  y =  input$picker_descrip1,
                  colour = "date")
              ) +
              theme_minimal()
            
          },
          "op2" = {
            
            ggplot(
              data = data_csv(),
            ) +
              geom_violin(
                mapping = aes_string(
                  x =  input$picker_descrip1,
                  y =  input$picker_descrip2,
                  colour = "date")
              ) +
              theme_minimal()
            
          },
          "op3" = {
            
            ggplot(
              data = data_csv(),
            ) +
              geom_histogram(
                mapping = aes_string(
                  x =  input$picker_descrip1,
                  fill = "date")
              ) +
              theme_minimal()
          
          }
          
        )
        
      }
      
    })

  })
  
  # Vareable selector for Analytic Results
  output$select_analytic <- renderUI({
    
    tagList(
      pickerInput(
        inputId = ns("var_analytic"),
        label = "Variables",
        choices = numeric_names()
      ),
      pickerInput(
        inputId = ns("var_analytic2"),
        choices = numeric_names()
      )
    )
    
  })

  
  observe({
    
    if(is.null(input$var_analytic)){
      
      return(NULL)
      
    }else{
      
      output$value_cor_pear <- renderPrint({

        cor(
          x = numeric_data() %>% select(input$var_analytic), 
          y = numeric_data() %>% select(input$var_analytic2),
          method = "pearson"
        )
        
      })
      
      output$uiout_pear <- renderUI({
        
        tagList(
          h6("Pearson's Correlation Coefficient"),
          verbatimTextOutput(ns("value_cor_pear"))
        )
        
      })   
      
      output$value_cor_ken <- renderPrint({
        
        cor(
          x = numeric_data() %>% select(input$var_analytic), 
          y = numeric_data() %>% select(input$var_analytic2),
          method = "kendall"
        )
        
      })
      
      output$uiout_ken <- renderUI({
        
        tagList(
          h6("Pearson's Correlation Coefficient"),
          verbatimTextOutput(ns("value_cor_ken"))
        )
        
      })  
      
      output$value_cor_spear <- renderPrint({
        
        cor(
          x = numeric_data() %>% select(input$var_analytic), 
          y = numeric_data() %>% select(input$var_analytic2),
          method = "spearman"
        )
      })
      
      output$uiout_spear <- renderUI({
        
        tagList(
          h6("Pearson's Correlation Coefficient"),
          verbatimTextOutput(ns("value_cor_spear"))
        )
        
      })
      
    }
    
  })
  
  observe({
    
    if(is.null(input$var_analytic)){
      
      return(NULL)
      
    }else{
      
      output$value_mr <- renderPrint({
        summary(object = numeric_data() %>% select(input$var_analytic,input$var_analytic2) )
      })
      
      output$uiout_mr <- renderUI({
        
        tagList(
          h6("Mean and Ranges"),
          verbatimTextOutput(ns("value_mr"))
        )
        
      })
      
    }
    
  })
  
  observe({
    
    if(is.null(input$var_analytic)){
      
      return(NULL)
      
    }else{
      
      output$value_v <- renderPrint({
        var(
          x = numeric_data() %>% select(input$var_analytic),
          y = numeric_data() %>% select(input$var_analytic2)
        )
      })
      
      output$uiout_v <- renderUI({
        
        tagList(
          h6("Variance"),
          verbatimTextOutput(ns("value_v"))
        )

      })
      
    }
    
  })
  
  
  observeEvent(input$save_project,{
    
    p_name(input$input_name)
    p_institution(input$input_institution)
    p_description(input$input_description)
    p_author(sprintf("%s %s",input$input_a_name,input$input_a_lastname))
    p_date(Sys.Date())
    p_password(input$input_password)
    
    
    dat_ran(sprintf("private/data/%s.json", runif(1, min=3, max=4)))
    
    write_json(data_csv(), dat_ran())
    
    result_sav <- project_save(p_password(), p_name(), p_description(), p_institution(), p_author(), p_date(), dat_ran())
    
    shinyjs::reset("input_name")
    shinyjs::reset("input_institution")
    shinyjs::reset("input_description")
    shinyjs::reset("input_a_name")
    shinyjs::reset("input_a_lastname")
    shinyjs::reset("input_password")
    
    
    
  })

}