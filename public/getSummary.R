Uploadspath <- "D:/PHP/ElectionForensics/public/Uploads";
Resultspath <- "D:/PHP/ElectionForensics/public/Results";

#zz <- file("summary.Rout", open="wt")
#sink(zz, type= c("output", "message"))

args <- commandArgs(TRUE)

session_id <- args[1]

#csv file name is data[sessionid].csv and its located in Uploads folder
setwd(Uploadspath);
csv_file <- paste("data", session_id, ".csv",sep="")
fdata <- read.csv(csv_file)

#print(the_summary)
the_summary <- summary(fdata)
setwd(Resultspath);
output_summary <- paste(session_id,".txt",sep="")
capture.output(print(the_summary, prmsd=TRUE, digits=1), file=output_summary)

#sink(type="message")
#sink()


