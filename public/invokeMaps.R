library(gridExtra)
library(grid)  
source("ClusterDetector.R")

zz <- file("allMaps.Rout", open="wt")
sink(zz, type= c("output", "message"))

args <- commandArgs(TRUE)

print(args)

user_uploaded <- args[1]
session_id <- args[2]

mi <- args[3]
can <- args[4]
can <- lapply(strsplit(args[4], ','), as.character)[[1]]
can <- strsplit(args[4], ',')[[1]]
met <- args[5]
met <- strsplit(args[5], ',')[[1]]

if (user_uploaded == "yes") {
    #fnames <- c("Albania2013_communes.dbf", "Albania2013_communes.prj", "Albania2013_communes.shp", "Albania2013_communes.shx", "Albania2013_communes.cpg", "Albania2013_communes.sbn", "Albania2013_communes.sbx", "Albania2013_points_popweighted2.dbf", "Albania2013_points_popweighted2.prj", "Albania2013_points_popweighted2.shp", "Albania2013_points_popweighted2.shx", "Albania2013_points_popweighted2.cpg", "Albania2013_points_popweighted2.sbn", "Albania2013_points_popweighted2.sbx");
    fnames <- scan(paste(session_id,"FileNames.csv",sep=""), what="", sep=",")

    SHPpath <- "D:/PHP/ElectionForensics/public";
    uploadDirectory <- "D:/PHP/ElectionForensics/public";
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





#results = ClusterDetector(Return_File,mi,can,met)
#print(results)


v1 <- rep(paste(rep(can,each=length(met)), met, sep="_"), each=2)
cand_label<-unlist(strsplit(v1, "_"))[seq(1,length(unlist(strsplit(v1, "_"))),2)]
meth_label<-unlist(strsplit(v1, "_"))[seq(2,length(unlist(strsplit(v1, "_"))),2)]
methods<-c("First", "2BL", "CLD", "CL05", "PL05")
methods_lab<-methods[as.numeric(meth_label)]
Candidates_methods<-paste(v1,rep(c("M","G"),length(v1)/2), sep="_")   
counter<-rep(1:length(cand_label),each=2)
#    conductanalysisCL()

print('call Maps')
#GMapsResultsCl <<- MapsResultsCl = ClusterDetector(Return_File,mi,can,met)
MapsResultsCl = ClusterDetector(Return_File,mi,can,met)
print('done with Maps')
sink()
sink(type="message")
#pdf("plots.pdf")

#output to pdf
pdf(paste("results",session_id,".pdf",sep=""))
ClData<-seq(2,length(names(MapsResultsCl)),2)
for (i in seq(1,length(cand_label),2)) {
    my_i <- i
    gr1<-MapsResultsCl[ClData[[counter[my_i]]]][[1]]$gr1  
    gr2<-MapsResultsCl[ClData[[counter[my_i]]]][[1]]$gr2  
    plotnameM <- Candidates_methods[my_i]
    plotnameG <- Candidates_methods[my_i+1]
    maptitleM<-paste("Moran's I of", methods_lab[my_i], "for", cand_label[my_i], sep=" ")
    maptitleG<-paste("Getis-Ord of", methods_lab[my_i+1], "for", cand_label[my_i+1], sep=" ") 
    label1<-paste("label",Candidates_methods[my_i], sep="_")
    label2<-paste("label",Candidates_methods[my_i+1], sep="_")

    if ("polygons" %in% slotNames(gr1$poshpZ)) {
        plot(gr1$poshpZ, lwd=.02, col=gr1$pcolorZ, main="")
        title(main = maptitleM)
    }
    else {
        plot(gr1$poshpZ, pch=20, cex=.7, col=gr1$pcolorZ, main="")
        title(main = maptitleM)
    }

    if ("polygons" %in% slotNames(gr2$poshpZ)) {
        plot(gr2$poshpZ, lwd=.02, col=gr2$pcolorZ, main="")
        title(main = maptitleG)
    }
    else {
        plot(gr2$poshpZ, pch=20, cex=.7, col=gr2$pcolorZ, main="")
        title(main = maptitleG)
    }
}

#output to separate pngs

ClData<-seq(2,length(names(MapsResultsCl)),2)
for (i in seq(1,length(cand_label),2)) {

    my_i <- i
    gr1<-MapsResultsCl[ClData[[counter[my_i]]]][[1]]$gr1  
    gr2<-MapsResultsCl[ClData[[counter[my_i]]]][[1]]$gr2  
    plotnameM <- Candidates_methods[my_i]
    plotnameG <- Candidates_methods[my_i+1]
    maptitleM<-paste("Moran's I of", methods_lab[my_i], "for", cand_label[my_i], sep=" ")
    maptitleG<-paste("Getis-Ord of", methods_lab[my_i+1], "for", cand_label[my_i+1], sep=" ") 
    label1<-paste("label",Candidates_methods[my_i], sep="_")
    label2<-paste("label",Candidates_methods[my_i+1], sep="_")
    
    imagefilename <- paste(session_id, my_i, ".png", sep="")
    png(filename=imagefilename)

    if ("polygons" %in% slotNames(gr1$poshpZ)) {
        #output[[plotnameM]] <- renderPlot(plot(gr1$poshpZ, lwd=.02, col=gr1$pcolorZ), width = "auto", height = "auto")        
        plot(gr1$poshpZ, lwd=.02, col=gr1$pcolorZ, main = "")
        title(main = maptitleM)
    }
    else {
        #output[[plotnameM]] <- renderPlot(plot(gr1$poshpZ, pch=20, cex=.7, col=gr1$pcolorZ), width = "auto", height = "auto")
        plot(gr1$poshpZ, pch=20, cex=.7, col=gr1$pcolorZ, main="")
        title(main = maptitleM)
    }

    imagefilename <- paste(session_id, my_i, "_2.png", sep="")
    png(filename=imagefilename)

    if ("polygons" %in% slotNames(gr2$poshpZ)) {
        #output[[plotnameG]] <- renderPlot(plot(gr2$poshpZ, lwd=.02, col=gr2$pcolorZ), width = "auto", height = "auto")
        plot(gr2$poshpZ, lwd=.02, col=gr2$pcolorZ, main="")
        title(main = maptitleG)
    }
    else {
        #output[[plotnameG]] <- renderPlot(plot(gr2$poshpZ, pch=20, cex=.7, col=gr2$pcolorZ), width = "auto", height = "auto")
        plot(gr2$poshpZ, pch=20, cex=.7, col=gr2$pcolorZ, main="")
        title(main = maptitleG)
    }
    #output[[label1]] <- renderPrint(maptitleM);  
    #output[[label2]] <- renderPrint(maptitleG);        
}
dev.off()
#print (output)