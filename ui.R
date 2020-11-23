################################################################################
# UI of the app
#
# Author: Cristian Pazmiño
# Created: 2020-04-06 09:54:47
################################################################################

htmlTemplate(
  filename = "www/index.html",
  explorer_area = explorer_area_ui("explorer_area"),
  solution_area = solution_area_ui("solution_area")
)