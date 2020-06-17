##############################################################################################################
# Module header area
#
# Author: Cristian Pazmiño
# Created: 2020-05-13 12:24:40
##############################################################################################################

# Header area UI -----
header_area_ui<-function(id){
  ns <- NS(id)
  fluidRow(
    h1("Header for edit")
  )
}

# Header area SERVER -----
header_area_server<-function(input, output, session){
  ns <- session$ns
}