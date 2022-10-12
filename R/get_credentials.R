get_credentials <- function(){
  username <- readline("Enter username: ")
  password <- readline("Enter password: ")
  assign(x = "credentials", value = list(username, password),
         envir = globalenv())
  lockBinding("credentials", globalenv())
}
