#AFG_generalU.dbf
#AFG_generalU.prj
#AFG_generalU.shp
#AFG_generalU.shx

library(foreign)
library(ggmap)
library(maptools)
library(grDevices) 
library(geosphere)
library(moments)
library(spdep)
library(diptest)

mkcldf <- function(fnames) {
    name <- as.character(fnames);
    size <- file.size(fnames);
    type <- as.character(mime::guess_type(fnames));
    datapath <- as.character(rep(SHPpath,length(fnames)));
    clusterdataCL <- data.frame(name,size,type,datapath, stringsAsFactors=FALSE);
}

zz <- file("loadmaps.Rout", open="wt")
sink(zz, type= c("output", "message"))

args <- commandArgs(TRUE)

user_uploaded <- args[1]
session_id <- args[2]

if (user_uploaded == "yes") {
    #fnames <- c("Albania2013_communes.dbf", "Albania2013_communes.prj", "Albania2013_communes.shp", "Albania2013_communes.shx", "Albania2013_communes.cpg", "Albania2013_communes.sbn", "Albania2013_communes.sbx", "Albania2013_points_popweighted2.dbf", "Albania2013_points_popweighted2.prj", "Albania2013_points_popweighted2.shp", "Albania2013_points_popweighted2.shx", "Albania2013_points_popweighted2.cpg", "Albania2013_points_popweighted2.sbn", "Albania2013_points_popweighted2.sbx");
    fnames <- scan(paste(session_id,"FileNames.csv",sep=""), what="", sep=",")

    SHPpath <- "D:/PHP/ElectionForensics/public";
    uploadDirectory <- "D:/PHP/ElectionForensics/public";

    prevWD <- getwd();
    setwd(SHPpath);
    
    name <- as.character(fnames);
    size <- file.size(fnames);
    type <- as.character(mime::guess_type(fnames));
    datapath <- as.character(rep(SHPpath,length(fnames)));
    clusterdataCL <- data.frame(name,size,type,datapath, stringsAsFactors=FALSE);

    shpDF <- clusterdataCL;
} else {
    #print ('dropdown selected');
    print (user_uploaded);
    basepath <- "D:/PHP/ElectionForensics/public/Data";

    if (user_uploaded=='Afghanistan_2014_initial') {
        SHPpath <- paste(basepath,"Afghanistan2014/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Afghanistan2014/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(list.files(,pattern="generalU.(shp|dbf|sbn|sbx|shx|prj)$"));
    } else if (user_uploaded=='Afghanistan_2014_runoff') {
        SHPpath <- paste(basepath,"Afghanistan2014/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Afghanistan2014/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(list.files(,pattern="runoffU.(shp|dbf|sbn|sbx|shx|prj)$"));
    } else if (user_uploaded=='Albania_2013') {
        SHPpath <- paste(basepath,"Albania2013/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Albania2013/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(list.files(,pattern="popweighted2.(shp|dbf|sbn|sbx|shx|prj)$"));
    } else if (user_uploaded=='Bangladesh_2001') {
        SHPpath <- paste(basepath,"Bangladesh2001/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Bangladesh2001/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(list.files(,pattern="draft1p.(shp|dbf|sbn|sbx|shx|prj)$"));
    } else if (user_uploaded=='Cambodia_2013') {
        SHPpath <- paste(basepath,"Cambodia2013/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Cambodia2013/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(list.files(,pattern="communes.(shp|dbf|sbn|sbx|shx|prj)$"));
    } else if (user_uploaded=='Kenya_2013') {
        SHPpath <- paste(basepath,"Kenya2013/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Kenya2013/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <-
            mkcldf(c(list.files(,pattern="polygons1.(shp|dbf|sbn|sbx|shx|prj)$"),
            list.files(,pattern="points1.(shp|dbf|sbn|sbx|shx|prj)$")));
    } else if (user_uploaded=='Libya_2014') {
        SHPpath <- paste(basepath,"Libya2014/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Libya2014/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        x <- list.files(,pattern="(shp|dbf|sbn|sbx|shx|prj)$");
        XX <- x[-grep("fem",x)];
        clusterdataCL <- mkcldf(c(XX[grep("general\\.",XX)], XX[grep("popweighted\\.",XX)]));
    } else if (user_uploaded=='Libya_2014_Fem') {
        SHPpath <- paste(basepath,"Libya2014/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Libya2014/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(c(list.files(,pattern="fem.(shp|dbf|sbn|sbx|shx|prj)$"),
            list.files(,pattern="fem_popweighted.(shp|dbf|sbn|sbx|shx|prj)$")));
    } else if (user_uploaded=='South_Africa_2014') {
        SHPpath <- paste(basepath,"SouthAfrica2014/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"SouthAfrica2014/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <- mkcldf(list.files(,pattern="pollingstationsU2p.(shp|dbf|sbn|sbx|shx|prj)$"));
    } else if (user_uploaded=='Uganda_2006') {
        SHPpath <- paste(basepath,"Uganda2006/shapefiles/",sep="/");
        uploadDirectory <- paste(basepath,"Uganda2006/shapefiles/",sep="/");
        prevWD <- getwd();
        setwd(SHPpath);
        clusterdataCL <-
          mkcldf(c(list.files(,pattern="constituencies.(shp|dbf|sbn|sbx|shx|prj)$"),
          list.files(,pattern="constituencies_points.(shp|dbf|sbn|sbx|shx|prj)$")));
    } 

    shpDF <- clusterdataCL;
    for (i in 1:nrow(shpDF)) {
        file.rename(shpDF$datapath[i], shpDF$name[i])
    }
}

shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
shpPath <- paste(uploadDirectory, shpName, sep="/")

#print('shpPath:')
#print(shpPath)


dbfName <- shpDF$name[grep(x=shpDF$name, pattern="*.dbf")]
dbfPath <- paste(uploadDirectory, dbfName, sep="/")

#print('and here')
if (length(shpPath)==2) {
    CLfile1<-readShapeSpatial(shpPath[1])
    CLfile2<-readShapeSpatial(shpPath[2])
    detectType <- ifelse("polygons" %in% slotNames(CLfile1),2,1)
    if (detectType==2) {
        CLFile2<-CLfile1;
        CLFile1<-CLfile2
    }
    else {
        CLFile1<-CLfile1;
        CLFile2<-CLfile2
    }
    DBFfile1 <- read.dbf(gsub(".shp$",".dbf",shpPath[detectType]))
    DBFfile2 <- read.dbf(gsub(".shp$",".dbf",shpPath[-detectType]))
    #  rm(list=c("CLfile1","CLfile2"))
    Return_File<-list(CLFile1=CLFile1,CLFile2=CLFile2,DBFfile1=DBFfile1,DBFfile2=DBFfile2)
}

if (length(shpPath)==1) {
    CLFile1<-readShapeSpatial(shpPath)

    #print('shape file read')

    DBFfile1 <- read.dbf(gsub(".shp$",".dbf",shpPath))
    Return_File<-list(CLFile1=CLFile1,DBFfile1=DBFfile1)
}
#print('DBF read')

#print(Return_File)
df <- Return_File

items=c("No index", names(df$CLFile1@data)) #items=c(names(df$CLFile1@data))

setwd(prevWD);
#print(items)

output_mi <- paste("mi",session_id,sep="")
output_mi_txt <- paste(output_mi,".txt",sep="")
write(items, file = output_mi_txt, append = FALSE, sep = "")

df_c <- Return_File
#if (is.null(df)) return(NULL)    
Candidates<-names(df_c$CLFile1@data)
whichCandidates<-regexpr("[C]\\d",Candidates)>0;
subwhichCandidates<-regexpr("_",Candidates[whichCandidates])<0
listCandidates<-Candidates[whichCandidates][subwhichCandidates]
items_c=c(listCandidates)

#print(items_c)
output_can <- paste("can",session_id,sep="")
output_can_txt <- paste(output_can,".txt",sep="")
write(items_c, file = output_can_txt, append = FALSE, sep = "")

sink(type="message")
sink()