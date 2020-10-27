################################################################################
# Module explorer area
#
# Author: Cristian Pazmiño
# Created: 2020-04-16 10:23:25
################################################################################

# Explorer area UI -----
explorer_area_ui <- function(id) {
  ns <- NS(id)
  
  material_card(
    style = "background:#ffffff",
    fluidRow(
      column(
      width = 3,
      DT::DTOutput(ns("project_table")),
      style = "font-size: 8pt;"
      ),
      column(
        width = 6,
        fluidRow(
          style = "border-left: solid silver 1px; border-right: solid silver 1px",
          column(
            width = 12,
            DT::DTOutput(ns("project_data_csv")), 
            style = "font-size: 9pt;"
          )
        )
      ),
      column(
        width = 3,
        fluidRow(
          column(
            width = 12,
            uiOutput(ns("project_info")) %>% 
              withSpinner(type = 6, size = 0.3, proxy.height = "200px")
          )
        )
      )
    )
  )
}

# Explorer area SERVER -----
explorer_area_server<- function(input, output, session) {
  ns <- session$ns
  

  data_all_select <- reactive({
    data_all() %>%
      select(projectName, projectDate) %>%
      rename(project = projectName, date = projectDate)
  })
  
  output$project_table<-DT::renderDT({
    data_all_select() %>%
      DT::datatable(
        options = list(pageLength = 15, responsive = TRUE, dom = "ftip", searching = TRUE),
        selection = list(mode = "single"),
        class = "display compact",
        rownames = FALSE
      )
  })

  observe({
    req(data_all(), data_first())

    row_index <- which(data_all()$`_id` == data_first())

    DT::dataTableProxy("project_table") %>%
      DT::selectRows(row_index) %>%
      DT::selectPage(which(input$project_table_rows_all == row_index) %/% 1)
  })
  
  value_clicked <- reactive({
    req(input$project_table_rows_selected)

    data_all() %>%
      slice(input$project_table_rows_selected) %>%
      pull(`_id`)
  })

  selected_project <- reactive({
    req(value_clicked())

    list(
      project_db_selected = data_all() %>% filter(`_id` == value_clicked())
    )
  })
  
  select_data_csv <- reactive({
    req(selected_project())
    
    path_json <- selected_project()$project_db_selected$projectData
    json_data <- fromJSON(txt = sprintf("%s",path_json), simplifyDataFrame = TRUE)
    
  })
  
  output$project_data_csv<-DT::renderDT({
    
    select_data_csv() %>%
      DT::datatable(
        extensions = "Scroller",
        options = list(responsive = TRUE, scrollY = 450, scrollX =TRUE, scroller = TRUE, searching = FALSE),
        selection = list(mode = "single"),
        class = "display compact",
        rownames = FALSE
      )
  })
  
  output$project_info<-renderUI({
    req(selected_project())
    
    project_date <- selected_project()$project_db_selected$projectDate %>% as.Date()
    project_autor <- selected_project()$project_db_selected$projectAutor
    project_description <- selected_project()$project_db_selected$projectDescription

    tagList(
      h4(selected_project()$project_db_selected$projectName, style = "color:#272829"),
      h5(selected_project()$project_db_selected$institutionName, style = "color:#272829"),
      p("Created:", tags$em(project_date), style = "font-size: 9pt; color:#4B4C50"),
      p("Autor:", project_autor, style = "font-size: 9pt; color:#4B4C50"),
      p("Description:", project_description, style = "font-size: 9pt; color:#4B4C50; text-align:justify"),
      div(
        style = "margin-top: 15px;",
        actionBttn(
          ns("edit_info"), 
          label = "Edit", 
          size = "xs",
        ),
        actionBttn(
          ns("delete_project"), 
          label = "Delete", 
          size = "xs"
        ),
        downloadBttn(
          ns("downloadButtonFile"),
          label = "CSV",
          size = "xs"
        )
        
      )
    )
  })
  
  observeEvent(input$edit_info, {
    sendSweetAlert(
      session = session,
      title = NULL,
      width = 500,
      showCloseButton = TRUE,
      btn_labels = NA,
      text = fluidRow(
        column(
          width = 12,
          uiOutput(ns("edit_info_html")),
          style = "font-size: 10pt"
        )
      ),
      html = TRUE
    )
  })
  
  observeEvent(input$delete_project, {
    sendSweetAlert(
      session = session,
      title = NULL,
      width = 300,
      showCloseButton = TRUE,
      btn_labels = NA,
      text = fluidRow(
        column(
          width = 12,
          uiOutput(ns("delete_project_html")),
          style = "font-size: 10pt"
        )
      ),
      html = TRUE
    )
  })
  
  output$edit_info_html <- renderUI({
    req(selected_project())
    tagList(
      br(),
      h4("Update your information"),
      fluidRow(
        style = "text-align:justify",
        column(
          width = 6,
          textInput(
            ns("edit_projectName_pro"),
            label = "Project name",
            width = "100%",
            value = selected_project()$project_db_selected$projectName
          ),
          textInput(
            ns("edit_institution_pro"),
            label = "Institution",
            width = "100%",
            value = selected_project()$project_db_selected$institutionName
          )
        ),
        column(
          width = 6,
          textAreaInput(
            inputId = ns("edit_description_pro"),
            label = "Description", 
            width = "100%",
            height = "100px",
            resize = "vertical",
            value = selected_project()$project_db_selected$projectDescription
          )
        )
      ),
      p("Please type your project ", strong("password")," to confirm.", style = "text-align:justify"),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          passwordInput(
            inputId = ns("password_edit"),
            label = NULL,
            width = "100%"
          )
        )
      ),
      div(
        actionBttn(
          ns("password_edit_acept"), 
          label = "Save", 
          size = "s"
        )
      )
    )
  })
  
  output$delete_project_html <- renderUI({
    req(selected_project())
    tagList(
      br(),
      h4("Are you absolutely sure?"),
      div(
        style = "text-align:justify",
        p("This action", strong("cannot"), "be undone. 
        This will permanently delete the ",strong(selected_project()$project_db_selected$projectName), "project."),
        p("Please type your project ", strong("password")," to confirm.")
      ),
      div(
        passwordInput(
          inputId = ns("password_delete"),
          label = NULL,
          width = "100%"
        )
      ),
      div(
        actionBttn(
          ns("password_delete_acept"), 
          label = "Delete", 
          size = "s",
        )
      )
    )
  })
  
  observeEvent(input$password_delete_acept,{
    req(selected_project())
    
    id_project <- selected_project()$project_db_selected$`_id`
    path_data <- selected_project()$project_db_selected$projectData                                          
    pass_project <- input$password_delete
    
    text_result <- password_delete(id_project,pass_project,path_data)
    
    
    if(text_result == "Successfully removed"){
      data_all_select <- reactive({
        data_all() %>%
          select(projectName,projectDate) %>%
          rename(project = projectName, date = projectDate)
      })

      output$project_table<-DT::renderDT({
        data_all_select() %>%
          DT::datatable(
            options = list(pageLength = 15, responsive = TRUE, dom = "ftip"),
            selection = list(mode = "single"),
            class = "display compact",
            rownames = FALSE
          )
      })
    }
  })
  
  observeEvent(input$password_edit_acept,{
    req(selected_project())
    
    if(input$edit_description_pro == ""){
      description_project <- selected_project()$project_db_selected$projectDescription
      name_project <- selected_project()$project_db_selected$projectName
      name_institution <- selected_project()$project_db_selected$institutionName
    }else{
      description_project <- input$edit_description_pro
      name_project <- input$edit_projectName_pro
      name_institution <- input$edit_institution_pro
    }
    
    
    id_project <- selected_project()$project_db_selected$`_id`
    pass_project <- input$password_edit
    
    text_result <- password_edit(as.character(id_project), as.character(pass_project), as.character(description_project), as.character(name_project), as.character(name_institution))
    
    
    if(text_result == "Successfully edited"){
      data_all_select <- reactive({
        data_all() %>%
          select(projectName,projectDate) %>%
          rename(project = projectName, date = projectDate)
      })
      
      
      output$project_table<-DT::renderDT({
        data_all_select() %>%
          DT::datatable(
            options = list(pageLength = 15, responsive = TRUE, dom = "ftip"),
            selection = list(mode = "single"),
            class = "display compact",
            rownames = FALSE
          )
      })
    }
  })
  
  output$downloadButtonFile <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(select_data_csv(), file, row.names = FALSE)
    },
    contentType = "csv"
  )
    

  
}

