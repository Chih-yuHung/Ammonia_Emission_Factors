app_env <- environment()

sys.source("server.R", envir = app_env)
sys.source("ui.R", envir = app_env)

shinyApp(ui = ui, server = server)
