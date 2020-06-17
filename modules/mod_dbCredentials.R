################################################################################
# Module data base credentials
#
# Author: Cristian Pazmiño
# Created: 2020-04-09 12:47:08
################################################################################

db_credentials <- reactive({
  
  data_config <- "private/config/config.ini"
  
  readr::read_delim(file = data_config, delim = ";")

})