### Generelle Funktionen
### Funktionen zum Dekodieren und Entkodieren von Bildern auf Basis eines BASE64 Codes
### Verwendung bei PageImageContents sowie ImageMapQuestions
#' @export
GetBase64 <-function(Output){
        require(RCurl)
        img = readBin(Output, "raw", file.info(Output)[1, "size"])
        b64 = base64Encode(img, "character")
        #back = base64Decode(b64, "raw")
        #identical(img,back)
        b64[1]
    }
#' @export
SaveBase64 <-function(Output,ImageNametmp,FolderName){
        require(RCurl)
        rda = base64Decode(Output, "raw")
        file = paste(FolderName,"/",ImageNametmp,".jpg", sep="")
        writeBin(rda, file)
    }


### Funktion, welche je nach Vorgabe absolute oder relative Häufigkeiten (SUM = z.B. 100 oder 1) für beliebig viele Klassen (n) in einem bestimmten Intervall (byNUM) zuf�llig generiert
#' @export
getprobs<-function(SUM,n,byNum)
  # SUM : Wie hoch soll die Summer der Zahlen sein
  # n : wie viele Zahlen sollen gezogen werden?
  # byNum : exakte schrittweite der Zahlen
  {
    if(byNum < 0.0001) stop("byNum to low")
    if(abs(n*byNum - SUM)< 0.0001) {
        return(rep(byNum,n))
    }
    if(n*byNum - SUM > 0.0001) stop("Arguments in getprob are not compatible. SUM",SUM," n",n," byNum" , byNum)
    if(n>1){
        x<-sample(seq(from=byNum,to=SUM-(n-1)* byNum,by=byNum),1,FALSE)
        return(c(x,getprobs(SUM-x,n-1,byNum)))
    }else{
        return(SUM)
    }
  }

#' @export
makeLatexFormulaImages <- function(form){
  wd<-getwd()
  name<-character(length(form))
  for(i in 1:length(form)) {
      name[i] <- tempfile(pattern = "file", tmpdir = tempdir())
    con<-file("temp.tex",open="w")
      writeLines(con=con,text=paste("$",form[i],"$",sep=""))
    close(con)
      system("pdflatex latex.tex ")
      system(paste("pdfcrop latex.pdf ",name[i],".pdf",sep=""))
    system(paste("convert -density 600x600 -resize 800x560 -quality 100 ", name[i],".pdf ", name[i],".png",sep=""))
    }
  setwd(wd)
  name<-paste(name,".png",sep="")
  name
}

#' @export
Round <- function(x,digits=4) {
  x1<-sprintf("%-.28f",x)
  x2<-strsplit(x1,"\\.")
  ra<-function(y,digits) {
    if(as.numeric(substr(y[2],digits+1,digits+1))==5) {
      tmp <- paste(substr(y[2],1,digits),6,sep="")
      return(round(as.numeric(paste(y[1],tmp,sep=".")),digits=digits))
    } else {
      return(round(as.numeric(paste(y[1],y[2],sep=".")),digits=digits))
    }
  }
  x3<-sapply(x2,ra,digits=digits)
  if(!is.matrix(x)) return(x3)
  else {
      X <- matrix(x3,nrow=nrow(x),ncol=ncol(x))
      colnames(X) <- colnames(x)
      rownames(X) <- rownames(x)
      return(X)
  }
}
