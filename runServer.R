print(.libPaths())

library(plumber)


plum = plumb('server.R')
plum$run(port=8000,swagger = TRUE,host = "0.0.0.0")
