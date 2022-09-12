deploy_sus <- function(app_name) {
  
  if (Sys.info()["sysname"] != "Windows") 
    stop("As of now, this function is only adapted for Windows.")
  
  cancel <- readline(prompt = 
             paste0("Pursuing will open a terminal and terminate this R sessio",
                    "n. Write 'ok' to proceed:   "))
  
  if (cancel != "ok") return(cat("Aborted succesfully."))
  
  cmds <- c(paste0("cd ", getwd()),
                  "heroku login",
                  "heroku container:login",
                  paste0("heroku container:push web -a ", app_name),
                  paste0("heroku container:release web -a ", app_name))
  
  paste0(cmds, collapse = "\n") |> 
    writeLines("dev/other/deploy.ps1")
  
  shell(paste0("start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ", 
               gsub("/", "\\\\", paste0(getwd(), "/dev/other/deploy.ps1"))))
  
  quit(save = "no")
}

restart_dyno <-  function(app_name, dyno) {
  
  cmds <- c(paste0("cd ", getwd()),
            "heroku login", 
            paste0("heroku restart ", dyno, " -a ", app_name))
  
  paste0(cmds, collapse = "\n") |> 
    writeLines("dev/other/restart_dyno.ps1")
  
  shell(paste0("start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ", 
               gsub("/", "\\\\", paste0(getwd(), "/dev/other/restart_dyno.ps1"))))
  
}
