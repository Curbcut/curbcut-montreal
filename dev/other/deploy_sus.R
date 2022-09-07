deploy_sus <- function(app_name) {

  chain_of_cmd <- c("heroku login", 
                    "heroku container:login",
                    paste0("heroku container:push web -a ", app_name),
                    paste0("heroku container:release web -a ", app_name))
  
  walk(chain_of_cmd, ~{
    termId <- rstudioapi::terminalExecute("heroku login",
                                          workingDir = getwd())
    rstudioapi::terminalKill(termId)
  })
  
}

restart_dyno <-  function(app_name, dyno) {
  
  chain_of_cmd <- c("heroku login", 
                    paste0("heroku restart ", dyno, " -a ", app_name))
  
  walk(chain_of_cmd, ~{
    termId <- rstudioapi::terminalExecute("heroku login",
                                          workingDir = getwd())
    rstudioapi::terminalKill(termId)
  })
  
}