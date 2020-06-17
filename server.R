################################################################################
# Server logic of the app
#
# Author: Cristian Pazmiño
# Created: 2020-04-06 10:05:14
################################################################################

function(input, output, session) { 
  ns <- session$ns
  
  # Header area -----
  callModule(module = header_area_server,"header_area")
  
  # Explorer area -----
  callModule(module = explorer_area_server,"explorer_area")
  
  # Solution area -----
  callModule(module = solution_area_server,"solution_area")
}