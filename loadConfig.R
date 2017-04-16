# Load configuration file and create all variables into de system
#

config<-read.csv("config/config.dat",header = TRUE,sep = ";",stringsAsFactors = FALSE)
myClass<-config$value[[which(config$var=="myClass")]]
