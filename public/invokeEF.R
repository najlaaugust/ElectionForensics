library(gridExtra)
library(grid)  
source("ElectionForensics.R")

Uploadspath <- "D:/PHP/ElectionForensics/public/Uploads";
Resultspath <- "D:/PHP/ElectionForensics/public/Results";

zz <- file("all.Rout", open="wt")
sink(zz, type= c("output", "message"))

args <- commandArgs(TRUE)

print(args)

fname <- args[1]

csv_file <- args[2]
prevWD <- getwd();
setwd(Uploadspath);
fdata <- read.csv(csv_file)


can <- args[3]
can <- lapply(strsplit(args[3], ','), as.character)[[1]]
can <- strsplit(args[3], ',')[[1]]

lev <- args[4]
reg <- args[5]
vot <- args[6]

met <- args[7]
met <- strsplit(args[7], ',')[[1]]


print('call EF')
setwd(prevWD);
results = ElectionForensics(fname,fdata,can,lev,reg,vot,met)
print('done with EF')

outputfile <- substr(csv_file,5,nchar(csv_file)-4)

output_html <- paste(outputfile,".html",sep="")
output_csv <- paste(outputfile,".csv",sep="")


setwd(Resultspath);
write.csv(results[1], output_csv, row.names=FALSE)

dframe <- as.data.frame(do.call(cbind,results[2]))
write.table(dframe, output_html, row.names=FALSE, quote=FALSE)

sink(type="message")
sink()


