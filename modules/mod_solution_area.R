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
      useShinyFeedback(),
      tabsetPanel(
        type = "pills",
        tabPanel( # Data Viewe - Section 1
          h4(style="font-family: 'Kanit', sans-serif; font-weight: normal","Data Viewer"),
          fluidRow(
            column( 
              width = 3,
              material_card(
                style = "background:#f9f9f9; text-align: justify; color:#272829; font-size:9pt;",
                fluidRow(
                  column(
                    width = 12,
                    h4("Data Viewer", style = "text-align:center; font-family: 'Kanit', sans-serif"),
                    br(),
                    radioButtons(
                      ns("sep"), 
                      label = "Separator",
                      choices = c(
                        Comma = ",",
                        Semicolon = ";"),
                      selected = ",",
                      inline = TRUE
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
                div(
                  actionBttn(
                    ns("data_diagnostics"), 
                    label = "Diagnostic", 
                    size = "xs"
                  )
                  
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    p("If you need a guide to the data schema", downloadLink(ns("downloadLink"), "download", style = "color:red"), "it here.")
                    
                    
                  )
                )
              )
            ),
            column( 
              width = 9,
              material_card(
                style = "background:#f9f9f9; color:#272829",
                div(
                  style = "font-size: 8.5pt;",
                  DT::DTOutput(ns("data_from"))
                )
              )
            )
          )
        ),
        tabPanel( # Temporary Charts - Section 2
          h4(style="font-family: 'Kanit', sans-serif; font-weight: normal","Temporary Charts"),  
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#f9f9f9; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Temporary Charts", style = "text-align:center; font-family: 'Kanit', sans-serif"),
                  br(),
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
                style = "background:#f9f9f9; text-align: justify; color:#272829; font-size:9pt",
                div(
                  highchartOutput(
                    outputId = ns("ho_plot"),
                    width = "100%"
                  )
                )
              )
            )
          )
        ),
        tabPanel( # Comparative Time Charts - Section 3
          h4(style="font-family: 'Kanit', sans-serif; font-weight: normal","Comparative Charts"), 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#f9f9f9; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Comparative Time Charts", style = "text-align:center; font-family: 'Kanit', sans-serif"),
                  br(),
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
                style = "background:#f9f9f9; text-align: justify; color:#272829; font-size:9pt;",
                div(
                  highchartOutput(
                    outputId = ns("ho_plot_m"),
                    width = "100%"
                    # height = "240px"
                  )
                )
              ),
              material_card(
                style = "background:#f9f9f9; text-align: justify; color:#272829; font-size:9pt;",
                div(
                  highchartOutput(
                    outputId = ns("ho_plot_m2"),
                    width = "100%"
                    # height = "240px"
                  )
                )
              )
            )
          )
        ),
        tabPanel( # Descriptive Charts - Section 4
          h4(style="font-family: 'Kanit', sans-serif; font-weight: normal","Descriptive Charts"), 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Descriptive Charts", style = "text-align:center; font-family: 'Kanit', sans-serif"),
                  br(),
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
                div(
                  plotOutput(
                    ns("descriptive_graph"),
                    width = "100%"
                  )
                )
            )
          )
        ),
        tabPanel( # Analytical results - Section 5
          h4(style="font-family: 'Kanit', sans-serif; font-weight: normal","Analytical results"), 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Analytical results", style = "text-align:center; font-family: 'Kanit', sans-serif"),
                  br(),
                  uiOutput(ns("select_analytic"))
                )
              )
            ),
            column(
              width = 9,
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
              # )
            )
          )
        ),
        tabPanel( # Save Project - Section 6
          h4(style="font-family: 'Kanit', sans-serif; font-weight: normal","Save Project"), 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Save Project", style = "text-align:center; font-family: 'Kanit', sans-serif"),
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
  
  
  # Document preprocessing
  observeEvent(input$data_diagnostics,{
    
    if(is.null(input$file_input_csv)){
      
      return((NULL))
      
    }else{
      
      sendSweetAlert(
        session = session,
        title = NULL,
        width = 300,
        showCloseButton = TRUE,
        btn_labels = NA,
        text = fluidRow(
          column(
            width = 12,
            style = "font-size: 10pt",
            tagList(
              h4(style="font-family: 'Kanit', sans-serif;","File diagnostic"),
              br(),
              p("General summary of the file to be processed.", style = "text-align:justify"),
              br(),
              div(
                style = "text-align:justify",
                p(sprintf("Variables: %s",ncol(data_csv()))),
                p(sprintf("Records: %s",nrow(data_csv()))),
                p(sprintf("Quantitative: %s",ncol(numeric_data()))),
                p(sprintf("Qualitative: %s",ncol(data_csv())-ncol(numeric_data()))),
              )
            )
          )
        ),
        html = TRUE
      )
      
    }
    
  })

  # Download input
  output$downloadLink <- downloadHandler(
    filename = "info.pdf",
    content = function(file) {
      file.copy("private/config/info.pdf", file)
    },
    contentType = "pdf"
  )
  
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
          h6("kendall's Correlation Coefficient"),
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
          h6("Spearman's Correlation Coefficient"),
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
  
  # Save project in data base
  observeEvent(input$save_project,{
    
    if(is.null(input$file_input_csv)){
      
      
      return(NULL)
      
    }else{
      
      req(input$input_name,input$input_institution,input$input_a_name,input$input_a_lastname,input$input_password)
      
     
      p_name(input$input_name)
      p_institution(input$input_institution)
      p_description(input$input_description)
      p_author(sprintf("%s %s",input$input_a_name,input$input_a_lastname))
      p_date(Sys.Date())
      p_password(input$input_password)
      
      
      
      dat_ran(sprintf("private/data/%s.json", runif(1, min=1, max=4)))
      
      write_json(data_csv(), dat_ran())

      save_result <- project_save(as.character(p_password()), 
                   as.character(p_name()), 
                   as.character(p_description()), 
                   as.character(p_institution()), 
                   as.character(p_author()), 
                   as.character(p_date()), 
                   as.character(dat_ran()))
      
      
      
      if(save_result == "Saved correctly"){
        
        sendSweetAlert(
          session = session,
          title = NULL,
          width = 250,
          showCloseButton = TRUE,
          btn_labels = NA,
          text = fluidRow(
            column(
              width = 12,
              style = "font-size: 10pt",
              br(),
              span(class = "ti-check", style = "text-align:center; color: green; font-size:20pt"),
              br(),
              p(save_result)
            )
          ),
          html = TRUE
        )
        session$reload()
        
      }else{
        sendSweetAlert(
          session = session,
          title = NULL,
          width = 250,
          showCloseButton = TRUE,
          btn_labels = NA,
          text = fluidRow(
            column(
              width = 12,
              style = "font-size: 10pt",
              br(),
              span(class = "ti-close", style = "text-align:center; color: red; font-size:20pt"),
              br(),
              p(save_result)
            )
          ),
          html = TRUE
        )
      }
      
      
    }

  })
  
  observeEvent(input$input_password, {
    req(input$input_password)
    if (nchar(input$input_password) < 5) {
        showFeedbackWarning(
          inputId = "input_password", 
          text = "Very short password",
          icon = span(class = "ti-alerta")
        )
    } else {
      hideFeedback("input_password")
    }
  })
    
  

}