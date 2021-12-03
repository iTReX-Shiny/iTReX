############################
## iTReX ShinyApp file    ##
## Author: Yannick Berker ##
############################

iTReX:::set_env()
shinyApp(iTReX:::itrex_ui(), iTReX:::itrex_server)
