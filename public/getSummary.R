
zz <- file("summary.Rout", open="wt")
sink(zz, type= c("output", "message"))

args <- commandArgs(TRUE)

csv_file <- args[1]
session_id <- args[2]

fdata <- read.csv(csv_file)

#print(the_summary)
the_summary <- summary(fdata)

#outputfile <- substr(csv_file,5,nchar(csv_file)-4)
outputfile <- session_id

output_summary <- paste(outputfile,".txt",sep="")
capture.output(print(the_summary, prmsd=TRUE, digits=1), file=output_summary)


sink(type="message")
sink()


