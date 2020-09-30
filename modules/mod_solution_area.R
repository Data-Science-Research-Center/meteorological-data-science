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
        tabPanel( # Comparative Time Charts - Section 4
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
                      "Violinplot chart" = "op2"),
                    selected = "op1m",
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
                  plotOutput(ns("descriptive_graph"))

                )
              )
            )
          )
        ),
        tabPanel( # Comparative Time Charts - Section 5
          "Comparative Charts", 
          fluidRow(
            column(
              width = 3,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt",
                div(
                  h4("Comparative Time Charts", style = "text-align:center"),
                  hr(),
                  # radioButtons(
                  #   inputId = ns("diagram_chart_multiple"),
                  #   label = "Type of chart",
                  #   choices = c(
                  #     "Line chart" = "op1m", 
                  #     "Dot chart" = "op2m", 
                  #     "Bar chart" = "op3m"),
                  #   selected = "op1m",
                  #   inline = FALSE
                  # ),
                  # uiOutput(ns("select_tempo_1m"))
                )
              )
            ),
            column(
              width = 9,
              material_card(
                style = "background:#ffffff; text-align: justify; color:#272829; font-size:9pt; height:500px",
                div(
                  # highchartOutput(
                  #   outputId = ns("ho_plot_m"),
                  #   width = "100%",
                  #   height = "240px"
                  # ),
                  # highchartOutput(
                  #   outputId = ns("ho_plot_m2"),
                  #   width = "100%",
                  #   height = "240px"
                  # )
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

        return((NULL))
        
      }else{
        
        tryCatch(
          {
            session$userData$DATA_CSV <- read.csv(input$file_input_csv$datapath, sep = input$sep, dec = ".", fileEncoding = "UTF-8-BOM")
            
            data_csv(session$userData$DATA_CSV) # Reactive variable of the complete database
            
            numeric_names(names(session$userData$DATA_CSV %>% select_if(is.numeric))) # Reactive variable of numeric variable names
            
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

    xts(as.vector(data_csv() %>% select(input$picker_tempo_1)),as.Date(data_csv()$date))

  })
  
  tempo_app_multi1 <- reactive({
    
    xts(as.vector(data_csv() %>% select(input$picker_tempo_1m)), as.Date(data_csv()$date))
    
  })
  
  tempo_app_multi2 <- reactive({
    
    xts(as.vector(data_csv() %>% select(input$picker_tempo_2m)), as.Date(data_csv()$date))
    
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
                type = "line"
              )
            
          },
          # Dot chart
          "op2m" = {
            
            tempo_app_multi1() %>%
              hchart(
                type = "scatter"
              )

          },
          # Bar chart
          "op3m" = {

            tempo_app_multi1() %>%
              hchart(
                type = "column"
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
  
  
  # observe({
  #   
  #   output$
  #   
  # })
  
  # observe({
  #   
  #   output$td_plot <- renderPlot({
  #     
  #     if(is.null(input$picker_var1_td)){
  #       
  #       return(NULL)
  #       
  #     }else{
  #       
  #       shinyjs::show("td_config_drop")
  #       
  #       switch(
  #         EXPR = input$radio_geom_type,
  #         "style_a" = {
  #           if(input$check_able_td == FALSE){
  #             ggplot(
  #               data = csv_data(),
  #               mapping = aes_string(x = input$picker_var1_td, y = input$picker_var2_td)
  #             ) + 
  #               geom_point(
  #                 shape = as.numeric(input$picker_shape_point), 
  #                 size = 3, 
  #                 color = input$col_line1
  #               ) +
  #               theme_minimal()
  #           }else{
  #             ggplot(
  #               data = csv_data(),
  #               mapping = aes_string(x = input$picker_var1_td)
  #             ) + 
  #               geom_point(
  #                 mapping = aes_string(y = input$picker_var2_td),
  #                 shape = as.numeric(input$picker_shape_point), 
  #                 size = 3, 
  #                 color = input$col_line1
  #               ) +
  #               geom_point(
  #                 mapping = aes_string(y = input$picker_var3_td),
  #                 shape = as.numeric(input$picker_shape_point2), 
  #                 size = 3, 
  #                 color = input$col_line2
  #               ) +
  #               labs(
  #                 x = input$picker_var1_td, 
  #                 y =  sprintf("%s - %s", input$picker_var2_td, input$picker_var3_td)
  #               ) + 
  #               theme_minimal() 
  #           }
  #         },
  #         "style_b" = {
  #           if(input$check_able_td == FALSE){
  #             ggplot(
  #               data = csv_data(),
  #               mapping = aes_string(x = input$picker_var1_td, y = input$picker_var2_td, group = 1)
  #             ) +
  #               geom_line(
  #                 color = input$col_line1,
  #                 size = as.numeric(input$picker_size_line)
  #               ) +
  #               theme_minimal()
  #           }else{
  #             ggplot(
  #               data = csv_data(),
  #               mapping = aes_string(x = input$picker_var1_td)
  #             ) +
  #               geom_line(
  #                 mapping = aes_string(y = input$picker_var2_td, group = 1),
  #                 color = input$col_line1,
  #                 size = as.numeric(input$picker_size_line)
  #               ) +
  #               geom_line(
  #                 mapping = aes_string(y = input$picker_var3_td, group = 2),
  #                 color = input$col_line2,
  #                 size = as.numeric(input$picker_size_line)
  #               ) +
  #               labs(
  #                 x = input$picker_var1_td, 
  #                 y =  sprintf("%s - %s", input$picker_var2_td, input$picker_var3_td)
  #               ) +
  #               theme_minimal()
  #           }
  #         },
  #         "style_c" = {
  #           if(input$check_able_td == FALSE){
  #             ggplot(
  #               data = csv_data(),
  #               mapping = aes_string(x = input$picker_var1_td, y = input$picker_var2_td, colour = "Fecha")
  #               
  #             ) +
  #               geom_point(
  #                 # aes(
  #                 #   
  #                 # )
  #                 # shape = as.numeric(input$picker_shape_point), 
  #                 # size = 3, 
  #                 # color = input$col_line1
  #               ) +
  #               geom_line(
  #                 # aes(
  #                 #   group = "Fecha"
  #                 # )
  #                 # mapping =aes_string(group = 1),
  #                 # color = input$col_line1,
  #                 # size = as.numeric(input$picker_size_line)
  #               ) +
  #               theme_minimal()
  #           }else{
  #             ggplot(
  #               data = csv_data(),
  #               mapping = aes_string(x = input$picker_var1_td)
  #             ) +
  #               geom_line(
  #                 mapping =aes_string(y = input$picker_var2_td, group = 1),
  #                 color = input$col_line1,
  #                 size = as.numeric(input$picker_size_line)
  #               ) +
  #               geom_point(
  #                 mapping =aes_string(y = input$picker_var2_td),
  #                 shape = as.numeric(input$picker_shape_point), 
  #                 size = 3, 
  #                 color = input$col_line1
  #               ) + 
  #               geom_line(
  #                 mapping =aes_string(y = input$picker_var3_td, group = 2),
  #                 color = input$col_line2,
  #                 size = as.numeric(input$picker_size_line)
  #               ) +
  #               geom_point(
  #                 mapping = aes_string(y = input$picker_var3_td),
  #                 shape = as.numeric(input$picker_shape_point2), 
  #                 size = 3, 
  #                 color = input$col_line2
  #               ) +
  #               labs(
  #                 x = input$picker_var1_td, 
  #                 y =  sprintf("%s - %s", input$picker_var2_td, input$picker_var3_td)
  #               ) +
  #               theme_minimal()
  #           }
  #           
  #         }
  #       )
  #     }
  #     
  #   })
  # })
  # 
  # observe({
  #   output$plot_hbv <- renderPlot({ 
  #     
  #     if(is.null(input$picker_var4_td)){
  #       
  #       return(NULL)
  #       
  #     }else{
  #       
  #       shinyjs::show("td_config_drop1")
  #       
  #       switch(
  #         EXPR = input$radio_geom_type2,
  #         "graph_a" = {
  #           ggplot(
  #             data = csv_data(),
  #             mapping = aes_string(x = input$picker_var4_td)
  #           ) +
  #             geom_histogram(
  #               color="black", 
  #               fill="white"
  #             ) +
  #             theme_minimal()
  #         },
  #         "graph_b" = {
  #           ggplot(
  #             data = csv_data(),
  #             mapping = aes_string(x = input$picker_var4_td)
  #           ) +
  #             geom_boxplot() +
  #             theme_minimal()
  #         },
  #         "graph_c" = {
  #           ggplot(
  #             data = csv_data(),
  #             mapping = aes_string(x = input$picker_var6_td, y = input$picker_var4_td )
  #           ) +
  #             geom_violin() +
  #             theme_minimal()
  #         }
  #       )
  #       
  #     }
  #   })
  #   
  #   output$plot_hbv_comp <- renderPlot({ 
  #     
  #     if(is.null(input$picker_var5_td)){
  #       
  #       return(NULL)
  #       
  #     }else{
  #       
  #       shinyjs::show("td_config_drop1")
  #       
  #       switch(
  #         EXPR = input$radio_geom_type2,
  #         "graph_a" = {
  #           ggplot(
  #             data = csv_data(),
  #             mapping = aes_string(x = input$picker_var5_td)
  #           ) +
  #             geom_histogram(
  #               color="black", 
  #               fill="white") +
  #             theme_minimal()
  #         },
  #         "graph_b" = {
  #           ggplot(
  #             data = csv_data(),
  #             mapping = aes_string(x =  input$picker_var5_td)
  #           ) +
  #             geom_boxplot() +
  #             theme_minimal()
  #         },
  #         "graph_c" = {
  #           ggplot(
  #             data = csv_data(),
  #             mapping = aes_string(x = input$picker_var6_td, y = input$picker_var5_td)
  #           ) +
  #             geom_violin() +
  #             theme_minimal()
  #         }
  #       )
  #       
  #     }
  #   })
  # })

}