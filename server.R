function(input, output, session) { 
  ns <- session$ns

  callModule(module = header_area_server,"header_area")
  callModule(module = explorer_area_server,"explorer_area")
  callModule(module = solution_area_server,"solution_area")
}