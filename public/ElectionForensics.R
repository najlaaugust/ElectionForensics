# Election Forensics Toolkit
# by Kirill Kalinin and Walter R. Mebane, Jr.
# 25sep2015

#

source("ColorSignificance.R")
library(diptest)
library(boot)
require(XML)
#library(digest)

XMLdir <- "EFdata/XML";
XMLbasefile <- "^EF_0.xml$";
XMLnamespace <- "http://electionforensics.org";
XMLnamespaceNode <- 'http://electionforensics.org';
names(XMLnamespaceNode) <- "X";

if (!exists("GlobalWait")) { GlobalWait <- FALSE; }

if (!exists("GlobalDoc")) {
  Xflist <- list.files(XMLdir);
  if (length(Xloc <- grep(XMLbasefile,Xflist))>0) {
    XMLbasepath <- paste(XMLdir,Xflist[Xloc],sep="/");
    GlobalDoc <- xmlParse(XMLbasepath);
    GlobalResults <- xmlRoot(GlobalDoc);
  } else {
    GlobalResults <- newXMLNode("results", namespaceDefinitions=XMLnamespace);
    GlobalDoc <- newXMLDoc(node = GlobalResults);
  }
}

if (!exists("GdataID")) { GdataID <- NULL; }
if (!exists("GvarID")) { GvarID <- NULL; }
if (!exists("GlobalDataArray")) { GlobalDataArray <- NULL; }

SKNames <- c("Skew", "Kurt");
KSimNames <-
  paste("KSim", c("I","E","alpha","turnout","winprop","sigma","stdAtt","theta"),sep="");
KLikNames <-
  paste("KLik", c("I","E","alpha","turnout","winprop","sigma","stdAtt","theta","LL"),sep="");
MethNames<-c("_2BL", "LastC", "P05s", "C05s", SKNames, "DipT", KSimNames, KLikNames,
  "Obs");
MethNamesList <- list("_2BL", "LastC", "P05s", "C05s", as.list(SKNames), "DipT",
  as.list(KSimNames), as.list(KLikNames), "Obs");
MethNamesFull <- c("Benford's 2nd Digit(2BL)",
  "Last Digit(Counts)",
  "Percent(05s)",
  "Counts(05s)",
  SKNames,
  "Multimodality test",
  KSimNames,
  KLikNames,
  "Number of Observations");
MethNums <- 1:8;
MethNumsNK <- 1:6;
MethNums2Boot <- 1:5;
MethodsMarkers <-
  c(1,2,3,4,rep(5,length(SKNames)),6,rep(7,length(KSimNames)),rep(8,length(KLikNames)),0);

#Key Election Forensics Functions 
SecondDigitGetter <- function(num,digit) {
  s <- as.character(num);
  idx <- sapply(s, function(x){ nchar(x) >= digit; });
  dout <- ifelse(idx, NA, "");
  if (any(idx)) {
    dout[idx] <- sapply(s[idx], function(x){ substr(x,digit,digit) })}
  return(dout);
}

LastDigitGetter <- function(num) {
  s <- as.character(num);
  dout <-  as.numeric(substr(num, nchar(num),nchar(num)));
  return(dout)
}

NPAnal <- function(methods,pct=NULL,vot=NULL, mnames=MethNames) {
  # methods order specified in output$Methods (see server.R)
  # list("Benford's 2nd Digit"= 1, "Last Digit (Counts)" = 2, 
  #   "Percent (05s)"= 3, "Counts (05s)"= 4, "Skewness/Kurtosis" = 5,
  #   "Multimodality Test" = 6, "Klimek et al. simulation method" = 7,
  # "finite mixture likelihood method" = 8)

  methods <- methods[methods %in% MethNums2Boot];
  outvec <- rep(NA,length(methods));
  names(outvec) <- mnames[methods];
  if (!is.null(pct)) {
    pct100 <- pct*100;
    for (j in methods) {
      if (j == 1) outvec["_2BL"] <- 
        mean(as.numeric(SecondDigitGetter(vot,2)), na.rm=TRUE); # seconddigitResults
      if (j == 2) outvec["LastC"] <- 
        mean(LastDigitGetter(vot), na.rm=TRUE);                 # candidatecountlastResults
      if (j == 3) outvec["P05s"] <- 
        sum(LastDigitGetter(round(pct100,0))==5 |
            LastDigitGetter(round(pct100,0))==0,
          na.rm=TRUE)/(length(pct100)-sum(is.na(pct100)));      # candidatesignalingResults
      if (j == 4) outvec["C05s"] <- 
        mean(LastDigitGetter(vot)==5|LastDigitGetter(vot)==0,
          na.rm=TRUE);                                          # candidatecountsignalingResults
      if (j == 5) {
        outvec["Skew"] <- skewness(pct,na.rm=TRUE);             # skewnessResults
        outvec["Kurt"] <- kurtosis(pct,na.rm=TRUE);             # kurtosisResults
      }
    }
  }
  else {
    for (j in methods) {
      if (j == 1) outvec["_2BL"] <- 
        mean(as.numeric(SecondDigitGetter(vot,2)), na.rm=TRUE); # seconddigitResults
      if (j == 2) outvec["LastC"] <- 
        mean(LastDigitGetter(vot), na.rm=TRUE);                 # candidatecountlastResults
      if (j == 3) outvec["P05s"] <- NA;                         # candidatesignalingResults
      if (j == 4) outvec["C05s"] <- 
        mean(LastDigitGetter(vot)==5|LastDigitGetter(vot)==0,
          na.rm=TRUE);                                          # candidatecountsignalingResults
      if (j == 5) {
        outvec["Skew"] <- NA;                                   # skewnessResults
        outvec["Kurt"] <- NA;                                   # kurtosisResults
      }
    }
  }
  outvec;
}

bootAnalT <- function(dat,idx, methods=1) {
  dat <- dat[idx,,drop=FALSE];
  eligible<-dat$totalReg
  totalVoters<-dat$totalVotes
  turnoutPercent<-totalVoters/eligible
  #Controlling for over 1 
  turnoutPercent<-ifelse(turnoutPercent>=1,1,turnoutPercent)

  NPAnal(methods,pct=turnoutPercent,vot=totalVoters);
}

bootAnalV <- function(dat,idx, methods=1, cand=NULL) {
  dat <- dat[idx,,drop=FALSE];
  voteCandidate<-dat[,names(dat) %in% cand]
  if (!is.null(dat$totalVotes)) {
    totalVoters<-dat$totalVotes
    percentCandidate<-voteCandidate/totalVoters
    #Controlling for over 1 
    percentCandidate<-ifelse(percentCandidate>=1,1,percentCandidate)
    NPAnal(methods,pct=percentCandidate,vot=voteCandidate);
  }
  else {
    NPAnal(methods,vot=voteCandidate);
  }
}

statsArray2Table <- function(arr) {
#  resultsArray[1:levellength,1:num_candidates,resultsWidth,4]
  nLevel <- dim(arr)[1];
  nCands <- dim(arr)[2];
  nParms <- dim(arr)[3];
  nRow <- nLevel*nCands;
  nCol <- nParms + 3;
  Parms <- dimnames(arr)[3];
  tab <- data.frame(matrix(NA, nRow*2, nCol));
  names(tab) <- c("Level","Candidate",dimnames(arr)[[3]],"Obs");
  tab$Level[2*(1:nRow)-1] <- dimnames(arr)[[1]];
  for (j in 1:nCands) {
    tab$Candidate[(j-1)*2*nLevel + 2*(1:nLevel)-1] <- dimnames(arr)[[2]][j];
    for (k in 1:nParms) {
      tab[(j-1)*2*nLevel + 2*(1:nLevel)-1,2+k] <- round(arr[,j,k,1],3);
      for (L in 1:nLevel) {
        if (all(!is.na(arr[L,j,k,2:3]))) {
          tab[(j-1)*2*nLevel + 2*L,2+k] <- 
            paste("(",paste(round(arr[L,j,k,2:3],3),collapse=", "),
              ")",sep="",collapse="");
        }
        else if (!is.na(arr[L,j,k,2])) {
          tab[(j-1)*2*nLevel + 2*L,2+k] <- 
            paste("(",paste(round(arr[L,j,k,2],3),collapse=", "),
              ")",sep="",collapse="");
        }
      }
      if (regexpr("KSim|KLik",Parms[k])<0 & any(!is.na(arr[,j,k,4]))) {
        tab$Obs[(j-1)*2*nLevel + 2*(1:nLevel)-1] <- arr[,j,k,4];  # "Obs"
      }
    }
  }
  tab;
}

statsArray2XML <- function(dataID, dataNAME, dataDIM, arr, sarr, Gnode=GlobalResults,
  regvar="", totalvar="", leveltype="", version="0.2") {
#  resultsArray[1:levellength,1:num_candidates,resultsWidth,4]
#  nLevel <- dim(arr)[1];
#  nCands <- dim(arr)[2];
#  nParms <- dim(arr)[3];
#  nRow <- nLevel*nCands;
#  nCol <- nParms + 3;
#  Parms <- dimnames(arr)[3];

  while (GlobalWait) {
    Sys.sleep(0.2)
  }
  GlobalWait <<- TRUE;

  timestamp <- date();
  dlist <- getNodeSet(Gnode,
    paste("//X:data[@dataID = '",dataID,"']", sep=""),
    namespaces=XMLnamespaceNode);  # XPath
  if (length(dlist)>0) {
    dnode <- dlist[[1]];
  }
  else {
    dnode <- newXMLNode("data",
      attrs=c(dataID=dataID, dataNAME=dataNAME, dataDIM=dataDIM),
      parent = Gnode);
  }
  W <- sapply(dimnames(arr)[[2]], function(z) {
    ilist <- getNodeSet(Gnode,
      paste("//X:data[@dataID = '",dataID,"']",
        "/X:item[@itemname = '",z,"']", sep=""),
      namespaces=XMLnamespaceNode);  # XPath
    if (length(ilist)>0) {
      inode <- ilist[[1]];
    }
    else {
      inode <- newXMLNode("item", attrs=c(itemname=z, itemID="", itemtype="choice"),
        parent = dnode);
    }
    sapply(dimnames(arr)[[1]], function(y) {
      llist <- getNodeSet(Gnode,
        paste("//X:data[@dataID = '",dataID,"']",
          "/X:item[@itemname = '",z,"']",
          "/X:level[@levelname = '",y,"' and @leveltype = '",leveltype,"']", sep=""),
        namespaces=XMLnamespaceNode);  # XPath
      if (length(llist)>0) {
        lnode <- llist[[1]];
      }
      else {
        lnode <- newXMLNode("level", attrs=c(leveltype=leveltype, levelname=y),
          parent = inode);
      }
      sapply(dimnames(arr)[[3]], function(x) {
        if (sarr[y,z,x,"point"]) {
          plist <- getNodeSet(Gnode,
            paste("//X:data[@dataID = '",dataID,"']",
              "/X:item[@itemname = '",z,"']",
              "/X:level[@levelname = '",y,"' and @leveltype = '",leveltype,"']",
              "/X:parm[@parmname = '",x,"' and @regvar = '",regvar,
                "' and @totalvar = '",totalvar,"']", sep=""),
            namespaces=XMLnamespaceNode);  # XPath
          if (length(plist)>0) {
            pnode <- plist[[1]];
            sapply(dimnames(arr)[[4]], function(w) {
              elist <- getNodeSet(Gnode,
                paste("//X:data[@dataID = '",dataID,"']",
                  "/X:item[@itemname = '",z,"']",
                  "/X:level[@levelname = '",y,"' and @leveltype = '",leveltype,"']",
                  "/X:parm[@parmname = '",x,"' and @regvar = '",regvar,
                    "' and @totalvar = '",totalvar,"']",
                  "/X:estimate[@pname= '",x,"' and @name = '",w,"']",sep=""),
                namespaces=XMLnamespaceNode);  # XPath
              if (length(elist)>0) {
                enode <- elist[[1]];
                xmlAttrs(enode) <- c(timestamp=timestamp, version=version);
                xmlValue(enode) <- arr[y,z,x,w];
              }
              else {
                enode <- newXMLNode("estimate",
                  attrs=c(pname=x, name=w, timestamp=timestamp, version=version),
                  parent = pnode,
                  value=arr[y,z,x,w]);
              }
            });
          }
          else {
            pnode <- newXMLNode("parm",
              attrs=c(parmname=x, regvar=regvar, totalvar=totalvar),
              parent = lnode);
            sapply(dimnames(arr)[[4]], function(w) {
              newXMLNode("estimate",
                attrs=c(pname=x, name=w, timestamp=timestamp, version=version),
                parent = pnode, value=arr[y,z,x,w])
            });
          }
        }
      });
    });
  });
  GlobalWait <<- FALSE;
  W;
}

statsCheckXML <- function(dataID, dataNAME, dataDIM,
  Level, Cand, Parm, Gnode=GlobalResults,
  regvar="", totalvar="", leveltype="", version="0.2") {
#  resultsArray[1:levellength,1:num_candidates,resultsWidth,4]
#  nLevel <- dim(arr)[1];
#  nCands <- dim(arr)[2];
#  nParms <- dim(arr)[3];
#  Parms <- dimnames(arr)[3];

  while (GlobalWait) {
    Sys.sleep(0.2)
  }
  GlobalWait <<- TRUE;

  retval <- NULL;

  # specify XPath search string
  checkstr <- paste(
    "//X:data[@dataID = '",dataID,"']",
    "/X:item[@itemname = '",Cand,"']",
    "/X:level[@levelname = '",Level,"' and @leveltype = '",leveltype,"']",
    "/X:parm[@parmname = '",Parm,"' and @regvar = '",regvar,"' and @totalvar = '",totalvar,"']",
    sep="");
  dlist <- getNodeSet(Gnode, checkstr, namespaces=XMLnamespaceNode);  # XPath
  if (length(dlist)>0) {
    retval <- dlist[[1]];
  }

  GlobalWait <<- FALSE;

  return(retval);
}

ElectionForensics<-function(dataNAME, data=data, Candidates=Candidates, 
                            Level=Level,TotalReg=TotalReg,
                            TotalVotes=TotalVotes,Methods=Methods, nullvar="-", R=1000, cores=30){

#zz <- file("allef.Rout", open="wt")
#sink(zz, type= c("output", "message"))

#print("here")
#print(data)
#data = read.table("allprint.Rout")

  varID <- unlist(lapply(data,function(x){
    digest::digest(as.character(x), algo="sha1", serialize=FALSE)
  }));
  assign("GvarID", varID, envir = .GlobalEnv);    ## assign GvarID in global environment

  dataID <- digest::digest(paste(varID,collapse=" "), algo="sha1", serialize=FALSE);
  assign("GdataID", dataID, envir = .GlobalEnv);  ## assign GdataID in global environment

  dataDIM <- paste(dim(data), collapse=" ");

  #Set variables for analysis
  #browser()
  dataC<-NULL
  sims<-10
  meanExtreme<-0.98  #for extreme precincts, used for Nonparametric only
  sensitivity=0.03   #used for Nonparametric only
  totalMethods<-10 #Total Number of Methods, 12 in total

  #Obtain Variables
  if (TotalReg != nullvar & TotalVotes != nullvar & TotalReg != TotalVotes) {
    totalReg<-data[,which(names(data)%in%TotalReg)]
    totalVotes<-data[,which(names(data)%in%TotalVotes)]                   
    turnoutPercent<-totalVotes/totalReg
    data$Turnout<-turnoutPercent
    Candidates<-c("Turnout",Candidates)  #add turnout measure to Candidates
  }
  else if (TotalVotes != nullvar) {
    totalVotes <- data[,which(names(data)%in%TotalVotes)];
    totalReg <- turnoutPercent <- data$Turnout <- NULL;
  }
  else {
    totalReg <- totalVotes <- turnoutPercent <- data$Turnout <- NULL;
  }
  Candidates[grepl("All Leaders",Candidates)]<-"AllLeaders"

#  browser()

  #browser()

  #Obtain Compact dataset
  data$AllLeaders<-1
  candidates<-data[, names(data)%in%Candidates, drop=FALSE];
  num_candidates<-length(Candidates)
  if (!is.null(totalReg) & !is.null(totalVotes)) {
    dataC<-data.frame(totalReg,totalVotes, candidates)
    colnames(dataC)<-c("totalReg","totalVotes", colnames(candidates))
  }
  else if (!is.null(totalReg)) {
    dataC<-data.frame(totalReg, candidates)
    colnames(dataC)<-c("totalReg", colnames(candidates))
  }
  else if (!is.null(totalVotes)) {
    dataC<-data.frame(totalVotes, candidates)
    colnames(dataC)<-c("totalVotes", colnames(candidates))
  }
  else {
    dataC <- data.frame(candidates);
    colnames(dataC) <- c(colnames(candidates));
  }

  levels<-data[, names(data)%in%Level]; 
  methods<-as.numeric(Methods)
  sampleSize <- dim(data)[1];
  
  #Divide by level  1 for national
  if(length(levels)==0) {
    levels<-rep(1,dim(dataC)[1]);
    data$National<-levels
  }
  dataCC<-split(dataC, levels)
  levellength<-length(names(dataCC))
  if(levellength=='1') {
    names(dataCC)<-"National"
  }
  levelnames<-names(dataCC)
  
  whichCandidates<-regexpr("[CP]\\d",names(data))>0;
  matrixCandidates<- split(data[,whichCandidates,drop=FALSE],levels)

  AllLeaders<-rep(NA, dim(data)[1])
  if(("All Leaders" %in% Candidates) | ("AllLeaders" %in% Candidates)) {
    resElections <- lapply(matrixCandidates, function(x)
      names(data[,whichCandidates,drop=FALSE])[which.max(colSums(x, na.rm=TRUE))])
    for(ii in 1:length(names(resElections))) {
      AllLeaders[levels==names(resElections)[ii]] <- 
        data[levels==names(resElections)[ii],names(data)%in%resElections[ii]]
    }
    dataC$AllLeaders<-AllLeaders
  }

  dataCC<-split(dataC, levels)
  levellength<-length(names(dataCC))
  if(levellength=='1') {
    names(dataCC)<-"National"
  }
  levelnames<-names(dataCC)
  
  #Create holder for results
  resultsLength<-num_candidates*levellength*2
  resultsWidth <- length(unlist(MethNamesList[methods[methods %in% MethNums]]));
  resultsMatrix <- matrix(NA,resultsLength,resultsWidth); #+2 to account for Level and Candiates columns

  #Loops
  resultsArray <-
    array(NA,dim=c(levellength,num_candidates,resultsWidth,4));
#  dimnames(resultsArray) <- list(levelnames, Candidates,
#    MethNames[methods[methods %in% MethNumsNK]], c("point","lo","up","obs"));

  dimnames(resultsArray) <- list(NULL, NULL, NULL, NULL);
  dimnames(resultsArray)[[1]] <- levelnames;
  dimnames(resultsArray)[[2]] <- Candidates;
  dimnames(resultsArray)[[3]] <- unlist(MethNamesList[methods[methods %in% MethNums]]);
  dimnames(resultsArray)[[4]] <- c("point","lo","up","obs");
  sourceArray <- resultsArray;
  sourceArray[,,,] <- TRUE;

  varssArray <- array(NA,dim=c(levellength,num_candidates));

  varReg <- ifelse("totalReg" %in% names(dataCC[[1]]),c("totalReg"),"");
  varVotes <- ifelse("totalVotes" %in% names(dataCC[[1]]),"totalVotes","")

  if (any(MethNumsNK %in% methods)) {
    #withProgress(message = 'Toolkit is working.', value = 0, {
      #  setProgress(message = "Election Forensics Toolkit is working...")
      
      for(candidID in 1:num_candidates){  #Loop over Candidates  
        for(levelID in 1:levellength) {     #Loop over Levels
          #setProgress(value = candidID)     
          #incProgress(1/num_candidates, detail = paste("Analyzing ", Candidates[candidID], ".", sep=""))
    
          dataSubset<-dataCC[[levelID]]
          numberUnits<-dim(dataSubset)[1]
          if (dim(dataSubset)[1]==0) next;
      
          voteCandidateG <- dataSubset[,names(dataSubset)%in%Candidates[candidID]]
          if (!is.null(dataSubset$totalReg)) {
            eligibleG <- dataSubset$totalReg;
          }
          else {
            eligibleG <- NULL;
          }
          if (!is.null(dataSubset$totalVotes)) {
            totalVotersG <- dataSubset$totalVotes;
            percentCandidateG <- na.omit(voteCandidateG/totalVotersG)
          }
          else {
            totalVotersG <- percentCandidateG <- NULL;
          }
          if (!is.null(eligibleG) & !is.null(totalVotersG)) {
            turnoutPercentG <- na.omit(totalVotersG/eligibleG);
          }
          else {
            turnoutPercentG <- NULL;
          }
  
          varIDs <- unlist(lapply(dataSubset,function(x){
            digest::digest(as.character(x), algo="sha1", serialize=FALSE)
          }));

          parmCheck <- function (methods, methodsDrop=vector(mode="numeric")) {
            for (kCheck in methods) {
              kM <- MethNamesList[kCheck];
              if (is.character(kM)) {
                kMethNames <- kM;
              }
              else {
                kMethNames <- unlist(kM);
              }
              for (kkM in kMethNames) {
                kenode <- statsCheckXML(dataID, dataNAME, dataDIM,
                  levelnames[levelID], Candidates[candidID], kkM,
                  regvar=varReg, totalvar=varVotes, leveltype=Level);
                if (!is.null(kenode)) {
                  for (jkk in 1:length(kenode["estimate"])) {
                    if ((!is.na(kpname <- xmlGetAttr(kenode["estimate"][[jkk]], "name", NA))) &&
                      kpname %in% dimnames(resultsArray)[[4]]) {
                      if ("NA" != (valXML <- xmlValue(kenode["estimate"][[jkk]]))) {
                        resultsArray[levelnames[levelID],Candidates[candidID],kkM,kpname] <<-
                          as.numeric(valXML);
                      }
                      sourceArray[levelnames[levelID],Candidates[candidID],kkM,kpname] <<-
                        FALSE;
                    }
                  }
                  methodsDrop <- c(methodsDrop, kCheck);  # parms to nuke from estimating
                }
              }
            }
            methodsDrop;
          }

          #Nonparametric bootstrap
          if (Candidates[candidID] == "Turnout" &&
            !is.null(dataSubset$totalReg) && !is.null(dataSubset$totalVotes)) {
            if (any(methodsidx <- methods %in% MethNums2Boot)) {
              methodsDrop <- parmCheck(methods[methodsidx]);
              methodsUse <- methods[methodsidx][!(methods[methodsidx] %in% methodsDrop)];
              if (length(methodsUse)>0) {
                bootT <- boot(dataSubset, bootAnalT, R, parallel="multicore", ncpus=cores,
                  methods=methodsUse);
                resultsArray[levelID,candidID,names(bootT$t0),1] <- bootT$t0;
                resultsArray[levelID,candidID,names(bootT$t0),4] <- dim(bootT$data)[1];
                for (j in 1:length(bootT$t0)) {
                  if (!is.na(bootT$t0[j])) resultsArray[levelID,candidID,names(bootT$t0)[j],2:3] <-
                    boot.ci(bootT, index=j, type="basic")$basic[4:5];
                }
              }
            }
            if (6 %in% methods) {
              if (length(turnoutPercentG)==0) next;
              methodsDrop <- parmCheck(methods[methods==6]);
              methodsUse <- methods[methods==6][!(methods[methods==6] %in% methodsDrop)];
              if (length(methodsUse)>0 && 6 %in% methodsUse) {
                resultsArray[levelID,candidID,"DipT",1] <- dip.test(turnoutPercentG)$p.value
                resultsArray[levelID,candidID,"DipT",4] <- length(turnoutPercentG);
              }
            }
          }
          else {
            if (any(methodsidx <- methods %in% MethNums2Boot)) {
              methodsDrop <- parmCheck(methods[methodsidx]);
              methodsUse <- methods[methodsidx][!(methods[methodsidx] %in% methodsDrop)];
              if (length(methodsUse)>0) {
                bootV <- boot(dataSubset, bootAnalV, R, parallel="multicore", ncpus=cores,
                  methods=methodsUse, cand=Candidates[candidID]);
                resultsArray[levelID,candidID,names(bootV$t0),1] <- bootV$t0;
                resultsArray[levelID,candidID,names(bootV$t0),4] <- dim(bootV$data)[1];
                for (j in 1:length(bootV$t0)) {
                  if (!is.na(bootV$t0[j])) resultsArray[levelID,candidID,names(bootV$t0)[j],2:3] <-
                    boot.ci(bootV, index=j, type="basic")$basic[4:5];
                }
              }
            }
            if (6 %in% methods) {
              if (length(percentCandidateG)==0) next;
              methodsDrop <- parmCheck(methods[methods==6]);
              methodsUse <- methods[methods==6][!(methods[methods==6] %in% methodsDrop)];
              if (length(methodsUse)>0 && 6 %in% methodsUse) {
                resultsArray[levelID,candidID,"DipT",1] <- dip.test(percentCandidateG)$p.value
                resultsArray[levelID,candidID,"DipT",4] <- length(percentCandidateG);
              }
            }
          }
        }
      }
    
    #})  #with progress
  
    assign("GlobalDataArray", resultsArray, envir = .GlobalEnv);  ## assign GlobalDataArray globally
    mNK <- unlist(MethNamesList[methods[methods %in% MethNumsNK]]);
    if (any(sourceArray[,,mNK,,drop=FALSE])) {
      statsArray2XML(dataID, dataNAME, dataDIM,
        resultsArray[,,mNK,,drop=FALSE], sourceArray[,,mNK,,drop=FALSE],
        regvar=varReg, totalvar=varVotes, leveltype=Level);
    }

    CandidatesNames<-rep(Candidates,each=levellength*2)
    LevelNames<-rep(levelnames,length(Candidates), each=2) 
    SelectedMethods <- statsArray2Table(resultsArray);
    SelectedMethods[,dim(SelectedMethods)[2]][seq(2,dim(SelectedMethods)[1],2)]<-""
    SelectedMethods[,1][seq(2,length(SelectedMethods[,1]),2)]<-""
    SelectedMethods[,2][seq(2,length(SelectedMethods[,2]),2)]<-""
    SelectedMethods[is.na(SelectedMethods)] <- "--";
    colnames(SelectedMethods) <- c("Level", "Candidate's Name",
      MethNames[MethodsMarkers %in% c(methods,0)]);
  }

###Klimek et al. simulation approach
  if (7 %in% methods) {
#    f1r <- seq(from=0.0,to=1.0,by=0.1);       # min=0.0, max=1.0
#    f2r <- seq(from=0.0,to=0.3,by=0.025);     # min=0.0, max=1.0
#    ar <- seq(from=0.5,to=9.5,by=0.5);        # min=0.0
    f1r <- seq(from=0.0,to=1.0,by=0.2);       # min=0.0, max=1.0
    f2r <- seq(from=0.0,to=0.3,by=0.1);     # min=0.0, max=1.0
    ar <- seq(from=0.5,to=1,by=0.5);        # min=0.0
    iter <- 100

    CandidatesKlimek <- Candidates[Candidates!="Turnout"]
    KlimekMatrix<- matrix(NA,resultsLength,2);
    thresSet <- c(5+c(0:9)/10);
    withProgress(message = "Computing Klimek et al. simulation.", value = 0, {
      for (candidID in 1:length(CandidatesKlimek)) {  #Loop over Candidates  
        for (levelID in 1:levellength) {     #Loop over Levels
          #setProgress(value = candidID)     
          #incProgress(1/num_candidates, detail = paste("Analyzing ", CandidatesKlimek[candidID], ".", sep=""))
          parmCheck <- function (methods, methodsDrop=vector(mode="numeric")) {
            for (kCheck in methods) {
              kM <- MethNamesList[kCheck];
              if (is.character(kM)) {
                kMethNames <- kM;
              }
              else {
                kMethNames <- unlist(kM);
              }
              for (kkM in kMethNames) {
                kenode <- statsCheckXML(dataID, dataNAME, dataDIM,
                  levelnames[levelID], CandidatesKlimek[candidID], kkM,
                  regvar=varReg, totalvar=varVotes, leveltype=Level);
                if (!is.null(kenode)) {
                  for (jkk in 1:length(kenode["estimate"])) {
                    if ((!is.na(kpname <- xmlGetAttr(kenode["estimate"][[jkk]], "name", NA))) &&
                      kpname %in% dimnames(resultsArray)[[4]]) {
                      if ("NA" != (valXML <- xmlValue(kenode["estimate"][[jkk]]))) {
                        resultsArray[levelnames[levelID],CandidatesKlimek[candidID],kkM,kpname] <<-
                          as.numeric(valXML);
                      }
                      sourceArray[levelnames[levelID],CandidatesKlimek[candidID],kkM,kpname] <<-
                        FALSE;
                    }
                  }
                  methodsDrop <- c(methodsDrop, kCheck);  # parms to nuke from estimating
                }
              }
            }
            methodsDrop;
          }
          methodsDrop <- parmCheck(methods[methods==7]);
          methodsUse <- methods[methods==7][!(methods[methods==7] %in% methodsDrop)];
          if (length(methodsUse)>0 && 7 %in% methodsUse) {
            dataSubset<-dataCC[[levelID]]
            numberUnits<-dim(dataSubset)[1]
            NVoters <- dataSubset$totalReg
            NValid  <- dataSubset$totalVotes
            Votes   <- dataSubset[,names(dataSubset) %in% CandidatesKlimek[candidID]]
            KlData<-data.frame(NVoters,NValid,Votes)
            KlData<-na.omit(KlData)
            KlData<-KlData[!c(KlData$NVoters==0|KlData$NValid==0|KlData$Votes==0),]
            for (k in 1:length(thresSet)) {
              KlimekResults <-
                try(ElectionFitter_sim(data=KlData, thres=thresSet[k],
                  f1range=f1r, f2range=f2r, arange=ar, iterations=iter,
                  cores=cores));
              if (!is.character(KlimekResults)) break;
              if (k==length(thresSet) && is.character(KlimekResults)) KlimekResults <- NULL;
            }
            if (is.null(KlimekResults)) next;
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KSimNames[1:3],1] <- 
              KlimekResults$FF[1:3,1];
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KSimNames[1:3],2] <- 
              KlimekResults$FF[1:3,2];
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KSimNames[4:8],1] <- 
              c(KlimekResults$First_Estimated[c(1,3,6,4,5)]);
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KSimNames,4] <- 
              KlimekResults$FF[8,1]+9;
          }
        }
      }
    })
    mK <- unlist(MethNamesList[methods[methods == 7]]);
    Cidx <- Candidates!="Turnout";
    sourceArray[,!Cidx,mK,] <- FALSE;
    if (any(sourceArray[,Cidx,mK,,drop=FALSE])) {
      statsArray2XML(dataID, dataNAME, dataDIM,
        resultsArray[,Cidx,mK,,drop=FALSE], sourceArray[,Cidx,mK,,drop=FALSE],
        regvar=varReg, totalvar=varVotes, leveltype=Level);
    }

    CandidatesNames<-rep(Candidates,each=levellength*2)
    LevelNames<-rep(levelnames,length(Candidates), each=2) 
    SelectedMethods <- statsArray2Table(resultsArray);
    SelectedMethods[,dim(SelectedMethods)[2]][seq(2,dim(SelectedMethods)[1],2)]<-""
    SelectedMethods[,1][seq(2,length(SelectedMethods[,1]),2)]<-""
    SelectedMethods[,2][seq(2,length(SelectedMethods[,2]),2)]<-""
    SelectedMethods[is.na(SelectedMethods)] <- "--";
    colnames(SelectedMethods) <- c("Level", "Candidate's Name",
      MethNames[MethodsMarkers %in% c(methods,0)]);
  }

###finite mixture likelihood approach
  if (8 %in% methods) {
    iter <- 25
#    iter <- 2

    CandidatesKlimek <- Candidates[Candidates!="Turnout"]
    KlimekMatrix<- matrix(NA,resultsLength,2);
    withProgress(message = "Computing finite mixture likelihood.", value = 0, {
      for (candidID in 1:length(CandidatesKlimek)) {  #Loop over Candidates  
        for (levelID in 1:levellength) {     #Loop over Levels
          #setProgress(value = candidID)     
          #incProgress(1/num_candidates, detail = paste("Analyzing ", CandidatesKlimek[candidID], ".", sep=""))
          parmCheck <- function (methods, methodsDrop=vector(mode="numeric")) {
            for (kCheck in methods) {
              kM <- MethNamesList[kCheck];
              if (is.character(kM)) {
                kMethNames <- kM;
              }
              else {
                kMethNames <- unlist(kM);
              }
              for (kkM in kMethNames) {
                kenode <- statsCheckXML(dataID, dataNAME, dataDIM,
                  levelnames[levelID], CandidatesKlimek[candidID], kkM,
                  regvar=varReg, totalvar=varVotes, leveltype=Level);
                if (!is.null(kenode)) {
                  for (jkk in 1:length(kenode["estimate"])) {
                    if ((!is.na(kpname <- xmlGetAttr(kenode["estimate"][[jkk]], "name", NA))) &&
                      kpname %in% dimnames(resultsArray)[[4]]) {
                      if ("NA" != (valXML <- xmlValue(kenode["estimate"][[jkk]]))) {
                        resultsArray[levelnames[levelID],CandidatesKlimek[candidID],kkM,kpname] <<-
                          as.numeric(valXML);
                      }
                      sourceArray[levelnames[levelID],CandidatesKlimek[candidID],kkM,kpname] <<-
                        FALSE;
                    }
                  }
                  methodsDrop <- c(methodsDrop, kCheck);  # parms to nuke from estimating
                }
              }
            }
            methodsDrop;
          }
          methodsDrop <- parmCheck(methods[methods==8]);
          methodsUse <- methods[methods==8][!(methods[methods==8] %in% methodsDrop)];
          if (length(methodsUse)>0 && 8 %in% methodsUse) {
            dataSubset<-dataCC[[levelID]]
            numberUnits<-dim(dataSubset)[1]
            results<-matrix(NA,sims,totalMethods)
            NVoters <- dataSubset$totalReg
            NValid  <- dataSubset$totalVotes
            Votes   <- dataSubset[,names(dataSubset) %in% CandidatesKlimek[candidID]]
            KlData<-data.frame(NVoters,NValid,Votes)
            KlData<-na.omit(KlData)
            KlData<-KlData[!c(KlData$NVoters==0|KlData$NValid==0|KlData$Votes==0),]
            KlimekResults <-
              ElectionFitter(data=KlData, iterations=iter, itstartmax=10, pop=1000, cores=cores);
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KLikNames[1:8],1] <- 
              KlimekResults$FF[1:8,1];
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KLikNames[9],1] <- 
              KlimekResults$FF[9,1]-KlimekResults$FF_null[9,1];
            resultsArray[levelnames[levelID],CandidatesKlimek[candidID],KLikNames,4] <- 
              KlimekResults$FF[10,1]+9;
          }
        }
      }
    })
    mK <- unlist(MethNamesList[methods[methods == 8]]);
    Cidx <- Candidates!="Turnout";
    sourceArray[,!Cidx,mK,] <- FALSE;
    if (any(sourceArray[,Cidx,mK,,drop=FALSE])) {
      statsArray2XML(dataID, dataNAME, dataDIM,
        resultsArray[,Cidx,mK,,drop=FALSE], sourceArray[,Cidx,mK,,drop=FALSE],
        regvar=varReg, totalvar=varVotes, leveltype=Level);
    }

    CandidatesNames<-rep(Candidates,each=levellength*2)
    LevelNames<-rep(levelnames,length(Candidates), each=2) 
    SelectedMethods <- statsArray2Table(resultsArray);
  
    SelectedMethods[,dim(SelectedMethods)[2]][seq(2,dim(SelectedMethods)[1],2)]<-""
    SelectedMethods[,1][seq(2,length(SelectedMethods[,1]),2)]<-""
    SelectedMethods[,2][seq(2,length(SelectedMethods[,2]),2)]<-""
    SelectedMethods[is.na(SelectedMethods)] <- "--";
    colnames(SelectedMethods) <- c("Level", "Candidate's Name",
      MethNames[MethodsMarkers %in% c(methods,0)]);
  }

#browser()
# browser()

  if (any(sourceArray)) {
    Xtimestamp <- format(Sys.time(), "%d%b%Y_%H:%M:%S");
    saveXML(GlobalDoc,file=paste(XMLdir,"/EF_",Xtimestamp,".xml",sep=""));
  }

  coloredResults<-ColorSignificance(SelectedMethods)
  Results<-list(SelectedMethods,coloredResults)

#print("here also")

  return(Results)
  #return(SelectedMethods)
}
