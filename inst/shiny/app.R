############################
## iTReX ShinyApp file    ##
## Author: Yannick Berker ##
############################

# nolint start: undesirable_operator.
iTReX:::set_env()
shinyApp(iTReX:::itrex_ui(), iTReX:::itrex_server)
# nolint end
