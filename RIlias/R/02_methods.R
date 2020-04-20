# TODO: Add comment
#
# Author: Jan Pablo Burgard
###############################################################################
#' @export
setMethod("initialize", "Question",
    function(.Object, Type, Text=NULL, Tab=NULL, Image=NULL, Titel,Question,Base64Image=NULL,ImageCoord=NULL,RData=NULL,Answer,Points) {
      .Object@Type        = Type
      .Object@Text        = Text
      .Object@Tab         = Tab
      .Object@Image       = Image
      .Object@RData       = RData
      .Object@Titel       = Titel
      .Object@Question    = Question
      .Object@Base64Image = Base64Image
      .Object@ImageCoord  = ImageCoord
      .Object@Answer      = Answer
      .Object@Points      = Points
      .Object
    })
#' @export
Question<-function(...) new("Question",...)
#' @export
setGeneric("Write", function(.Object,ID,FolderName,fqpl,fqti,fqpltmp,Author) standardGeneric("Write"))
setMethod ("Write","Question",
    function(.Object,ID,FolderName,fqpl,fqti,fqpltmp,Author){
      fqpl=getConnection(fqpl)
      fqti=getConnection(fqti)
      fqpltmp=getConnection(fqpltmp)
      ### Aller PageContents der qpl-Datei müssen durch ein Tag <PageQbject></PageQbject> umschlossen werden.
      writeLines("<PageObject>", con=fqpl, sep="")
      if(!is.null(.Object@Text))    PageContentText(Text=.Object@Text,fqpl=fqpl)
      if(!is.null(.Object@Tab))     PageContentTable(Tab=.Object@Tab,fqpl=fqpl)
      if(!is.null(.Object@Image))   PageContentImage(Image=.Object@Image,ID=ID,FolderName=FolderName,Title=.Object@Titel,fqpl=fqpl,fqpltmp=fqpltmp)# Dims sind hier noch nicht moeglich...
      if(!is.null(.Object@RData))   PageContentRData(RData=.Object@RData,ID=ID,FolderName=FolderName,Title=.Object@Titel,fqpl=fqpl,fqpltmp=fqpltmp)
    if(.Object@Type=="Matching") {
      ItemMatching(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti=fqti,fqpl=fqpl,Author=Author)
    }
    if(.Object@Type=="MatchingImage") {
      ItemMatchingImage(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti=fqti,fqpl=fqpl,Author=Author)
    }
      if(.Object@Type=="Numeric") {
        ItemNumeric(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      if(.Object@Type=="SingleChoice") {
        ItemSingle(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      if(.Object@Type=="MultipleChoice") {
        ItemMultiple(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      if(.Object@Type=="Ordering") {
        ItemOrdering(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      if(.Object@Type=="ImageMap") {
        ItemImage(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Base64Image=.Object@Base64Image,ImageCoord=.Object@ImageCoord,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      if(.Object@Type=="NumericGaps") {
        ItemNumericGaps(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      if(.Object@Type=="Gaps") {
        ItemGaps(Titel=.Object@Titel,ID=ID,Question=.Object@Question,Answer=.Object@Answer,Points=.Object@Points,fqti,fqpl,Author=Author)
      }
      writeLines("</PageObject>", con=fqpl, sep="")
    }
)

#' @title Create Pdf from Question
#' @details hallo
#' @param QP List of Question-Objects containg the questions of this Question Pool.
#' @author Jan Pablo Burgard
#' @examples 
#' \dontrun{
#' QP <- list()
#' MakeIliasFiles(QP,ds)
#' }
#' 5^2
#' @export
EinsichtPdf <- function(QP, Path, EventName, Vorname, Nachname,debug=FALSE){
  StudentName <- paste(Nachname,Vorname,sep="")
  PathOld<-getwd()    ### Löschen möglicher alter Verzeichnispfade
  
#if(file.exists(EventName)) stop(paste("The Folder",EventName,"already exists!"))
  setwd(Path)
  dir.create(EventName)  ### ein neuer Ordner wird erstellt
  
  unlink(paste0(EventName,"/",EventName,StudentName),recursive=TRUE)  ### löscht Verbindungen zu Dateien oder Pfade -> dies geschieht rekursiv
  dir.create(paste0(EventName,"/",EventName,StudentName))
  setwd(paste0(EventName,"/",EventName,StudentName))
  file.create(paste0(EventName,StudentName,".tex"))     ### Die Ordnernamen müssen stets die Struktur "EventName_qpl" haben
  
  fhandle = file(paste0(EventName,StudentName,".tex"),open="a", encoding="UTF-8")   ### öffnet ein Connection zur qti-Datei -> werden im utf-8 code geschrieben
  
  ### Hier texKopf

  writeLines("\\documentclass[12pt]{article}", con=fhandle, sep="\n")
  writeLines("\\usepackage{amsmath,amsfonts,graphicx}", con=fhandle, sep="\n")
  writeLines("\\usepackage[english,ngerman]{babel}", con=fhandle, sep="\n")
  writeLines("\\usepackage[utf8]{inputenc}",con=fhandle,sep="\n")
  writeLines("\\title{Klausureinsicht}",con=fhandle, sep="\n")
  writeLines("\\author{Lehrstuhl für wirtschaftsstatistik}",con=fhandle, sep="\n")
  
  # et.
  writeLines("\\begin{document}",con=fhandle, sep="\n")  
  writeLines("\\maketitle",con=fhandle, sep="\n")  

  
#   \begin{center}
#   \begin{tabular}{ll}
#   Termin: & Dienstag, 17.02.2015\\
#   Name: &  \\
#   Matrikelnummer: & `r data.neu$mtknr[d]`\\
#   Benutzername: & `r data.neu$Login[d]`\\
#   Passwort: & Das Passwort haben Sie während der Klausur gesetzt.\\
#   \end{tabular}
#   \end{center}
  
  
  ### Fragen
  for(i in 1:length(QP)){
    .Object <- QP[[i]]
    writeLines(paste0("\\section*{",.Object@Titel,"}"),con=fhandle,sep="\n\n")
    if(!is.null(.Object@Text)){
      Text <- gsub("[/tex]","$",gsub("[tex]","$",.Object@Text,fixed = TRUE),fixed = TRUE)
      writeLines(Text, con=fhandle,  sep="\n\n")
    } 
    if(!is.null(.Object@Tab)){
      Tab <- gsub("[/tex]","$",gsub("[tex]","$",.Object@Tab,fixed = TRUE),fixed = TRUE)
      Tab.Text <- character()
      Tab.Text[1]<-paste0("\\begin{tabular}{",paste0(rep("c",ncol(Tab)),collapse = ""),"}")
      Tab.Text[2]<-paste(paste0(Tab[1,],collapse="&"),"\\\\\\hline\\hline")
      for(j in 2:(nrow(Tab)-1)) Tab.Text[j+1]<-paste(paste0(Tab[j,],collapse="&"),"\\\\")
      Tab.Text[nrow(Tab)+1]<-paste(paste0(Tab[nrow(Tab),],collapse="&"),"\\\\")
      Tab.Text[nrow(Tab)+2]<-paste0("\\end{tabular}")
      writeLines(Tab.Text, con=fhandle,  sep="\n")
    } 
    if(!is.null(.Object@Image)){
      Image.Name <- gsub("\\.","",paste0(StudentName,.Object@Titel))
      SaveBase64(.Object@Image,Image.Name,"./")
      Image.Text <- paste0("\\includegraphics[width=\\textwidth]{",Image.Name,".jpg}")
      writeLines(Image.Text, con=fhandle,  sep="\n\n")
    } 
    if(!is.null(.Object@RData)){
      RData <- .Object@RData
      RData.Name <- gsub("\\.","",paste0(StudentName,.Object@Titel))
      save(RData,file=paste0(RData.Name,".RData",sep=""))
      rm(RData)
      Data.Text <- paste0("Appendix: RDatafile ",RData.Name,".RData")
      writeLines(Data.Text, con=fhandle,  sep="\n\n")
    } 
  }
  
  ### Hier texFuß
  writeLines("\\end{document}", con=fhandle, sep="")

  ### Dann werden die aufgebauten Connections zu den beiden generierten Dateien geschlossen
  close(fhandle)
  setwd("../")
  zip(paste0(EventName,StudentName,".zip"),paste0(EventName,StudentName))

  if(PathOld!=Path){
    setwd(PathOld)
  }
}

#' @export
CheckFiles <- function(folderName,Titel) {
  system(paste("sed -n '/NA/p' ",folderName,Titel,"_qpl/",Titel,"_qpl.xml",sep=""))
  system(paste("sed -n '/e-/p' ",folderName,Titel,"_qpl/",Titel,"_qti.xml",sep=""))
  system(paste("sed -n '/e0/p' ",folderName,Titel,"_qpl/",Titel,"_qti.xml",sep=""))
}



#' @title Create Ilias importable Files.
#' @details hallo
#' @param QP List of Question-Objects containg the questions of this Question Pool.
#' @author Jan Pablo Burgard
#' @examples 
#' \dontrun{
#' QP <- list()
#' MakeIliasFiles(QP,ds)
#' }
#' 5^2
#' @export
MakeIliasFiles <- function(QP,Path,FolderName,PoolName,id="lala",Author="Lehrstuhl Wirtschafts- und Sozialstatistik Trier",debug=FALSE){
  FolderName<-paste(FolderName,"_qpl",sep="")
  PathOld<-getwd()    ### Löschen möglicher alter Verzeichnispfade
  
  if(PathOld!=Path){
    setwd(Path)         ### Setzen eines neuen Pfades -> dieser wird dann beim Erzeugen der Dateien individuell als Argument der Funktion definiert
  }
  #if(file.exists(FolderName)) stop(paste("The Folder",FolderName,"already exists!"))
  unlink(FolderName,recursive=TRUE)  ### löscht Verbindungen zu Dateien oder Pfade -> dies geschieht rekursiv
  
  dir.create(FolderName)  ### ein neuer Ordner wird erstellt
  dir.create(paste(FolderName,"/","objects", sep="")) ### erstellt Ordner für Medienobjekte
  
  file.create(paste(FolderName,"/",FolderName,".xml",sep=""))     ### Die Ordnernamen müssen stets die Struktur "Foldername_qpl" haben
  file.create(paste(FolderName,"/",FolderName,".xml.tmp",sep=""))     ### Image Dateien müssen hier reinschreiben. der inhalt dieser Datei wird ans ende angefügt.
  file.create(paste(FolderName,"/",substr(FolderName,1,nchar(FolderName)-3),"qti.xml",sep=""))
  
  fqti=file(paste(FolderName,"/",substr(FolderName,1,nchar(FolderName)-3),"qti.xml",sep=""),open="a", encoding="UTF-8")   ### öffnet ein Connection zur qti-Datei -> werden im utf-8 code geschrieben
  fqpl=file(paste(FolderName,"/",FolderName,".xml",sep=""),open="a", encoding="UTF-8")    ### öffnet ein Connection zur qpl-Datei -> werden im utf-8 code geschrieben
  fqpltmp=file(paste(FolderName,"/",FolderName,".xml.tmp",sep=""),open="r+", encoding="UTF-8")    ### öffnet ein Connection zur qpl.tmp-Datei -> werden im utf-8 code geschrieben
  
  ### Hier werden jeweils in qpl- und qti-Datei Systeminformationen mitgegeben
  
  writeLines("<?xml version=\"1.0\" encoding=\"utf-8\"?>", con=fqpl, sep="")
  writeLines("<!DOCTYPE Test SYSTEM \"http://www.ilias.uni-koeln.de/download/dtd/ilias_co.dtd\">", con=fqpl, sep="")
  
  writeLines("<?xml version=\"1.0\" encoding=\"utf-8\"?>", con=fqti, sep="")
  writeLines("<!DOCTYPE questestinterop SYSTEM \"ims_qtiasiv1p2p1.dtd\">", con=fqti, sep="")
  
  ### Dann wird der jeweilige Fragenpool eingefügt
  writeLines("<ContentObject Type=\"Questionpool_Test\">", con=fqpl, sep="")
  
  ### hier werden Metadaten des Fragenpools definiert
  
  writeLines("<MetaData>", con=fqpl, sep="")
  writeLines("<General Structure=\"Hierarchical\">", con=fqpl, sep="")
  writeLines(paste("<Identifier Entry=\"",FolderName,"\" Catalog=\"ILIAS\"/>",sep=""), con=fqpl, sep="")   ### Name/Bezeichung des Ordners, in welchen die qpl- und qti-Dateien abgespeichert werden sollen
  writeLines(paste("<Title Language=\"de\">",PoolName,"</Title>",sep=""), con=fqpl, sep="")    ### Name/Bezeichung des Fragenpools
  writeLines("<Language Language=\"de\"/>", con=fqpl, sep="")  ### Sprache des Fragenpools
  writeLines("<Description Language=\"de\"/>", con=fqpl, sep="")   ### Sprache der Beschreibung des Fragenpools
  writeLines("<Keyword Language=\"de\"/>", con=fqpl, sep="")
  writeLines("</General>", con=fqpl, sep="")
  writeLines("</MetaData>", con=fqpl, sep="")
  writeLines("<questestinterop>", con=fqti, sep="")  ### ab hier beginnt die Auflistung aller Fragen -> Länge abhängig von Anzahl der Fragen im Fragenpool
  
  for(i in 1:length(QP)){
    do.call("Write",list(c(QP[[i]],ID=paste(id,sprintf("%03d",i),sep=""),FolderName=FolderName,fqpl=fqpl,fqti=fqti,fqpltmp=fqpltmp,Author=Author))[[1]] ) ### Aufruf aller Fragen
    flush(fqpl)
    flush(fqti)
    flush(fqpltmp)
  }
  
  writeLines("</questestinterop>", con=fqti, sep="")
  TMPqpl<-readLines(fqpltmp,warn=FALSE)
  writeLines(TMPqpl, con=fqpl, sep="")
  writeLines("</ContentObject>", con=fqpl, sep="")
  
  ### Dann werden die aufgebauten Connections zu den beiden generierten Dateien geschlossen
  
  close(fqti)
  close(fqpl)
  close(fqpltmp)
  if(file.exists(paste(FolderName,"/",FolderName,".xml.tmp",sep=""))) file.remove(paste(FolderName,"/",FolderName,".xml.tmp",sep=""))
  
  if(file.exists(paste(FolderName,"zip",sep="."))) unlink(paste(FolderName,"zip",sep="."),recursive=TRUE)
  zip(paste(FolderName,"zip",sep="."),FolderName)
  if(!debug) {
    unlink(FolderName,recursive=TRUE)  ### löscht Verbindungen zu Dateien oder Pfade -> dies geschieht rekursiv
  } else {
    print(CheckFiles(FolderName,id))
  }
  
  if(PathOld!=Path){
    setwd(PathOld)
  }
}

