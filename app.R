# Shiny app####
source("inst/shiny-CCgraphs/myapp/shiny_global.R") #loading local variables and helper functions

#loading the necessary funtions to create the plots
#search dwd station
 source("./R/dwd_search_station.R")

#cum sum plot + table
 source("./R/get_dwd_for_cumsum.R")
 source("./R/plot_cum_sum.R")
 source("./R/table_cum_sum.R")

#anomalies
 source("./R/get_dwd_for_anomalies.R")
 source("./R/anomalies_precip.R")
 source("./R/anomalies_temp.R")

#shiny app
source("./inst/shiny-CCgraphs/myapp/server.R") #server
source("inst/shiny-CCgraphs/myapp/ui.R")#user interface
shinyApp(ui = ui, server = server)
