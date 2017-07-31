# Updating h2o and other packages
install.packages("sparkylr") 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rparkling")
install.packages("devtools")
install.packages("nycflights13")
install.packages("readr")
install.packages("RPostgreSQL")
install.packages("RMySQL")
install.packages("ggthemes")


detach("package:rsparkling", unload = TRUE)
if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
if (isNamespaceLoaded("h2o")){ unloadNamespace("h2o") }
remove.packages("h2o")
install.packages("h2o", type = "source", repos = "http://h2o-release.s3.amazonaws.com/h2o/rel-tverberg/2/R")

install.packages("sparkylr") 