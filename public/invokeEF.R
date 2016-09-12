library(gridExtra)
library(grid)  
source("ElectionForensics.R")

zz <- file("all.Rout", open="wt")
sink(zz, type= c("output", "message"))

args <- commandArgs(TRUE)

print(args)

fname <- args[1]

csv_file <- args[2]
fdata <- read.csv(csv_file)

can <- args[3]
can <- lapply(strsplit(args[3], ','), as.character)[[1]]
can <- strsplit(args[3], ',')[[1]]

lev <- args[4]
reg <- args[5]
vot <- args[6]

met <- args[7]
met <- strsplit(args[7], ',')[[1]]


#print('call EF')
results = ElectionForensics(fname,fdata,can,lev,reg,vot,met)
#print('done with EF')
#print(results)

outputfile <- substr(csv_file,5,nchar(csv_file)-4)


output_csv <- paste(outputfile,".csv",sep="")
output_html <- paste(outputfile,".html",sep="")

write.csv(results[1], output_csv, row.names=FALSE)

#print(results[2])
dframe <- as.data.frame(do.call(cbind,results[2]))
write.table(dframe, output_html, row.names=FALSE, quote=FALSE)

sink(type="message")
sink()


