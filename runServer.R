library(plumber)

plum = plumb('server.R')
plum$run(port=8000)
