PageContentText <- function(Text,fqpl) {
    fqpl=getConnection(fqpl)
  if(length(Text)<2) {
      writeLines("<PageContent>", con=fqpl, sep="") # öffnet den 1. Seiteninhalt
      writeLines(paste("<Paragraph Language=\"de\" Characteristic=\"Standard\">",Text,"</Paragraph>", sep=""), con=fqpl)
      writeLines("</PageContent>", con=fqpl, sep="") # schlieöt den 1. Seiteninhalt
  } else{
    writeLines("<PageContent>", con=fqpl, sep="") # öffnet den 1. Seiteninhalt
    writeLines(paste("<Paragraph Language=\"de\" Characteristic=\"Standard\">", sep=""), con=fqpl)
    for(i in 1:length(Text)){
      writeLines(paste("&lt;p&gt;",Text[i],"&lt;/p&gt;", sep=""), con=fqpl)
    }
    writeLines(paste("</Paragraph>", sep=""), con=fqpl)
    writeLines("</PageContent>", con=fqpl, sep="") # schlieöt den 1. Seiteninhalt
  }
}


PageContentTable <- function(Tab,fqpl) {
  fqpl=getConnection(fqpl)
  writeLines("<PageContent>", con=fqpl, sep="") # öffnet den 2. Seiteninhalt
  writeLines("<Table Language=\"de\" Width=\"5\" Border=\"5px\" CellSpacing=\"0px\" CellPadding=\"5px\" HorizontalAlign=\"Left\" HeaderRows=\"1\" Class=\"StandardTable\">", con=fqpl,  sep="")
  DIM<-dim(Tab)
    for(i in 1:DIM[1]){ # hier wird die Anzahl an Zeilen bestimmt
      writeLines("<TableRow>", con=fqpl, sep="")    # öffnet eine neue Zeile
      for(j in 1:DIM[2]){   # hier wird die Anzahl an Spalten bestimmt, indem die Befehlskette (unten) entsprechned der Spaltenanzahl wiederholt wird
        writeLines("<TableData>", con=fqpl, sep="") # hier beginnt die Strukturumgebung eines einzelnen Zelleneintrags
          writeLines("<PageContent>", con=fqpl, sep="")
            writeLines("<Paragraph Language=\"de\" Characteristic=\"TableContent\">",con=fqpl, sep="")
              writeLines(text=as.character(Tab[i,j]),con=fqpl, sep="")     ### hier stehen die eigentlichen Zelleninhalte (Text oder Zahlen)
            writeLines("</Paragraph>",con=fqpl, sep="")
          writeLines("</PageContent>", con=fqpl, sep="")
        writeLines("</TableData>", con=fqpl, sep="")    # hier endet die Strukturumgebung eines einzelnen Zelleneintrags
      }
      writeLines("</TableRow>", con=fqpl, sep="")   # schlieöt eine Zeile ab
    }
    writeLines("</Table>", con=fqpl, sep="")    # schlieöt die Tabelle ab
    writeLines("</PageContent>", con=fqpl, sep="")  # schlieöt den 2. Seiteninhalt
}


PageContentImage <- function(Image,ID,FolderName,Title,fqpl,fqpltmp,DIMS=NULL) {
  cat("beginning Image ...")
  Title <- gsub("[^[:alnum:]]", "", Title)
  cat("ending Image\n")
  fqpl=getConnection(fqpl)
  fqpltmp=getConnection(fqpltmp)
  if(!file.exists(paste(FolderName,"/objects/",ID,sep="")))dir.create(paste(FolderName,"/objects/",ID,sep=""))
  SaveBase64(Image,paste(Title,sep=""),paste(FolderName,"/objects/",ID,sep=""))

  writeLines("<PageContent>", con=fqpl, sep="") # öffnet den 2. Seiteninhalt
    writeLines("<MediaObject>", con=fqpl,  sep="")
      writeLines(paste("<MediaAlias OriginId=\"",ID,"\"/>", sep=""), con=fqpl,  sep="")     ### Name des Ordner, in welchem Medienobjekte gespeichert werden
      if(is.null(DIMS)) writeLines("<MediaAliasItem Purpose=\"Standard\"><Layout HorizontalAlign=\"Left\"/></MediaAliasItem>", con=fqpl, sep="")
      else writeLines(paste("<MediaAliasItem Purpose=\"Standard\"><Layout HorizontalAlign=\"Left\" Width=\"",DIMS$Width,"\" Height=\"", DIMS$Height,"\"/></MediaAliasItem>", sep=""), con=fqpl, sep="")
    writeLines("</MediaObject>", con=fqpl,  sep="")
  writeLines("</PageContent>", con=fqpl, sep="")  # schlieöt den 2. Seiteninhalt

  writeLines("<MediaObject>", con=fqpltmp,  sep="")
      writeLines("<MetaData>", con=fqpltmp,  sep="")
            writeLines("<General Structure=\"Hierarchical\">", con=fqpltmp,  sep="")
                writeLines(paste("<Identifier Catalog=\"ILIAS\" Entry=\"",ID,"\"/>", sep=""), con=fqpltmp, sep="")
                writeLines(paste("<Title Language=\"de\">",Title,"</Title>",sep=""), con=fqpltmp,  sep="")
                writeLines("<Language Language=\"de\"/>", con=fqpltmp,  sep="")
                writeLines("<Description Language=\"de\">image/jpeg</Description>", con=fqpltmp,  sep="")
                writeLines("<Keyword Language=\"de\"/>", con=fqpltmp,  sep="")
            writeLines("</General>", con=fqpltmp,  sep="")
      writeLines("</MetaData>", con=fqpltmp,  sep="")
      writeLines("<MediaItem Purpose=\"Standard\">", con=fqpltmp,  sep="")
            writeLines(paste("<Location Type=\"LocalFile\">",Title,".jpg</Location>", sep=""), con=fqpltmp,  sep="")
            writeLines("<Format>image/jpeg</Format>", con=fqpltmp,  sep="")
            if(is.null(DIMS)) writeLines(paste("<Layout HorizontalAlign=\"Left\" />",sep=""), con=fqpltmp,  sep="")
            else writeLines(paste("<Layout Width=\"",Width,"\" Height=\"",Height,"\" HorizontalAlign=\"Left\" />",sep=""), con=fqpltmp,  sep="")
      writeLines("</MediaItem>", con=fqpltmp,  sep="")
  writeLines("</MediaObject>", con=fqpltmp,  sep="")
}

PageContentRData <- function(RData,ID,FolderName,Title,fqpl,fqpltmp) {
  cat("beginning rdata ...")
  Title <- gsub("[^[:alnum:]]", "", Title)
  cat("ending rdata\n") 
  fqpl=getConnection(fqpl)
  fqpltmp=getConnection(fqpltmp)
  if(!file.exists(paste(FolderName,"/objects/",ID,sep=""))) dir.create(paste(FolderName,"/objects/",ID,sep=""))
  save(RData,file=paste(FolderName,"/objects/",ID,"/",Title,".RData",sep=""))

  writeLines("<PageContent>", con=fqpl,  sep="")
      writeLines("<FileList>", con=fqpl,  sep="")
      writeLines(paste("<Title Language=\"de\">",Title,"</Title>",sep=""), con=fqpl,  sep="")
      #writeLines(paste("<Title Language=\"de\"/>",sep=""), con=fqpl,  sep="")
      writeLines("<FileItem>", con=fqpl,  sep="")
                writeLines(paste("<Identifier Catalog=\"ILIAS\" Entry=\"",ID,"\"/>", sep=""), con=fqpl, sep="")
                writeLines(paste("<Location Type=\"LocalFile\">",Title,".RData</Location>", sep=""), con=fqpl,  sep="")
                writeLines("<Format>application/octet-stream</Format>", con=fqpl,  sep="")
            writeLines("</FileItem>", con=fqpl,  sep="")
      writeLines("</FileList>", con=fqpl,  sep="")
  writeLines("</PageContent>", con=fqpl,  sep="")
  
  writeLines(paste("<PageContent>",sep=""), con=fqpl, sep="")
      writeLines(paste("<Question Qref=\"",ID,"\" />",  sep=""), con=fqpl,  sep="")
  writeLines("</PageContent>", con=fqpl, sep="")
}

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 1) Numerische Fragen (NUMERIC QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemNumeric<-function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author) {
  fqpl=getConnection(fqpl)
  fqti=getConnection(fqti)

  ### Argumente der Funktion:
  # 'Titel': Name des Fragenpools
  # 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
  # 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
  # 'Answer': bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
  # 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
  # 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
  # 'fqti, fqpl': Vgl. oben


  ### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

  ### Wichtige erste Zeile:
  # öffnet Umgebung einer EINZELNEN Frage
  # zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
  # zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>NUMERIC QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

    writeLines(paste("<presentation label=\"",ID,"\">",sep=""), con=fqti, sep="") ### hat keine entscheidenede Funktion (bisher) -> das Label ist nu unmittelbar nach dem Import kurz zu sehen
      writeLines("<flow>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines(paste("<response_num ident=\"",ID,"\" rcardinality=\"Single\" numtype=\"Decimal\">",sep=""), con=fqti, sep="") # Antwort ist eine einzige numerische Dezimalzahl -> hier noch kein Ergebnis
          writeLines("<render_fib fibtype=\"Decimal\" maxchars=\"10\"></render_fib>", con=fqti, sep="") # Zahl darf 'maxchars=Anzahl' Stellen haben
        writeLines("</response_num>", con=fqti, sep="")
      writeLines("</flow>", con=fqti, sep="")
    writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

    writeLines("<resprocessing>", con=fqti, sep="")
      writeLines("<outcomes>", con=fqti, sep="")
        writeLines("<decvar></decvar>", con=fqti, sep="")
      writeLines("</outcomes>", con=fqti, sep="")
      writeLines("<respcondition>", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<vargte respident=\"NUM\">",Answer[1],"</vargte>",sep=""), con=fqti, sep="") # untere Schranke des Ergebnisintervalls
          writeLines(paste("<varlte respident=\"NUM\">",Answer[2],"</varlte>",sep=""), con=fqti, sep="") # obere Schranke des Ergebnisintervalls
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",Points,"</setvar>",sep=""), con=fqti, sep="") # Anzahl der zu erreichenden Punkte der Aufgaben
        writeLines("<displayfeedback feedbacktype=\"Response\" linkrefid=\"Correct\"/>", con=fqti, sep="")
      writeLines("</respcondition>", con=fqti, sep="")
    writeLines("</resprocessing>", con=fqti, sep="")
    writeLines("<itemfeedback ident=\"Correct\" view=\"All\">", con=fqti, sep="")
      writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="") # Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
      writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

   writeLines("<PageContent>", con=fqpl, sep="")
   writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
   writeLines("</PageContent>", con=fqpl, sep="")

}



##################################################################################################################################################################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###
##################################################################################################################################################################


###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 2) Single-Choice-Fragen (SINGLE CHOICE QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemSingle <- function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author){
  fqpl=getConnection(fqpl)
  fqti=getConnection(fqti)

### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Answer': bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>SINGLE CHOICE QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

    writeLines("<presentation label=\"SC\">", con=fqti, sep="") ### hat keine entscheidenede Funktion (bisher) -> das Label ist nu unmittelbar nach dem Import kurz zu sehen
      writeLines("<flow>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines("<response_lid ident=\"MCSR\" rcardinality=\"Single\">", con=fqti, sep="") ### Es handelt sich um ein SingleQuestion Frage mit einer einzigen richtigen Lösung
          writeLines("<render_choice shuffle=\"Yes\">", con=fqti, sep="") ### Möglichkeit die Antwortmöglichkeiten beim Aufruf der Frage zufällig zu mischen: "Yes" oder "No"

    ### Jede Antwort wird mit einem Label gekennzeichnet -> eindeutige Zuordnung von Antworten und Lösungen (später)

        AW<-length(Answer)
        for(i in 1:AW){
         # hier wird die Anzahl möglicher Antworten bestimmt

                        writeLines(paste("<response_label ident=\"",i,"\">", sep=""), con=fqti, sep="") # jede Antwortmögl. erhält ein spezisches Label
              writeLines("<material>", con=fqti, sep="")
                writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer[i],"</mattext>",sep=""), con=fqti, sep="") # hier stehen die Antwortmöglichkeiten [i]
              writeLines("</material>", con=fqti, sep="")
            writeLines("</response_label>", con=fqti, sep="")
        }
          writeLines("</render_choice>", con=fqti, sep="")
        writeLines("</response_lid>", con=fqti, sep="")
      writeLines("</flow>", con=fqti, sep="")
    writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

    writeLines("<resprocessing>", con=fqti, sep="")
      writeLines("<outcomes>", con=fqti, sep="")
        writeLines("<decvar></decvar>", con=fqti, sep="")
      writeLines("</outcomes>", con=fqti, sep="")

        for(i in 1:AW){
            writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<varequal respident=\"MCSR\">",i,"</varequal>", sep=""), con=fqti, sep="")   ### i-te SC-Antwort wird ausgewertet
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",Points[i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte der Aufgaben
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"response_",i,"\"/>", sep=""), con=fqti, sep="")    ### Verbindung zu Feeback für i-te Antwort
      writeLines("</respcondition>", con=fqti, sep="")
    }
    writeLines("</resprocessing>", con=fqti, sep="")

        for(i in 1:AW){
    writeLines(paste("<itemfeedback ident=\"response_",i,"\" view=\"All\">", sep=""), con=fqti, sep="")   ### Feedback für ite Antwort
      writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="")     ### Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
      writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
    }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")

}



##################################################################################################################################################################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###
##################################################################################################################################################################


###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 3) Multiple-Choice-Fragen (MULTIPLE CHOICE QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemMultiple <- function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author){
  fqpl=getConnection(fqpl)
  fqti=getConnection(fqti)

### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Answer': bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>MULTIPLE CHOICE QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

    writeLines("<presentation label=\"MC\">", con=fqti, sep="") ### hat keine entscheidenede Funktion (bisher) -> das Label ist nu unmittelbar nach dem Import kurz zu sehen
      writeLines("<flow>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines("<response_lid ident=\"MCMR\" rcardinality=\"Multiple\">", con=fqti, sep="") ### Es handelt sich um ein MultipleQuestion Frage mit mehreren richtigen Lösungen
          writeLines("<render_choice shuffle=\"Yes\">", con=fqti, sep="") ### Möglichkeit die Antwortmöglichkeiten beim Aufruf der Frage zufällig zu mischen: "Yes" oder "No"

    ### Jede Antwort wird mit einem Label gekennzeichnet -> eindeutige Zuordnung von Antworten und Lösungen (später)

        AW<-length(Answer)
        for(i in 1:AW){
         # hier wird die Anzahl möglicher Antworten bestimmt

                        writeLines(paste("<response_label ident=\"",i,"\">", sep=""), con=fqti, sep="") ### jede Antwortmögl. erhält ein spezisches Label
              writeLines("<material>", con=fqti, sep="")
                writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer[i],"</mattext>",sep=""), con=fqti, sep="") ### hier stehen die Antwortmöglichkeiten [i]
              writeLines("</material>", con=fqti, sep="")
            writeLines("</response_label>", con=fqti, sep="")
        }
          writeLines("</render_choice>", con=fqti, sep="")
        writeLines("</response_lid>", con=fqti, sep="")
      writeLines("</flow>", con=fqti, sep="")
    writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

    writeLines("<resprocessing>", con=fqti, sep="")
      writeLines("<outcomes>", con=fqti, sep="")
        writeLines("<decvar></decvar>", con=fqti, sep="")
      writeLines("</outcomes>", con=fqti, sep="")

    ### die Auswertung anzukreuzender Lösungen ist zweigeteilt -> zunächst werden die richtigen Lösungen mit den jew. Punkten definiert...

        for(i in 1:AW){
            writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<varequal respident=\"MCMR\">",i,"</varequal>", sep=""), con=fqti, sep="")   ### i-te MC-Antwort wird ausgewertet (richtige Antworten)
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",Points[i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte der Aufgaben
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"response_",i,"\"/>", sep=""), con=fqti, sep="")    ### Verbindung zu Feeback für i-te Antwort
      writeLines("</respcondition>", con=fqti, sep="")

    ### ...in den weiteren Antwortbedingungen kann dann die Auswertung für falsch angekreuzte Lösungen definiert werden, z.B. entgegengesetzte (negative) Punkte

      writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
        writeLines("<not>", con=fqti, sep="")   ### das Tag <not></not> beschreibt die Bedingungen für falsche Antworten
          writeLines(paste("<varequal respident=\"MCMR\">",i,"</varequal>", sep=""), con=fqti, sep="") ### i-te MC-Antwort wird ausgewertet (falsche Antworten)
        writeLines("</not>", con=fqti, sep="")
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",-Points[i],"</setvar>", sep=""), con=fqti, sep="")   ### Anzahl negativer Punkte -> gegenteilig zu Punkten bei richtiger Lösung
      writeLines("</respcondition>", con=fqti, sep="")
    }
    writeLines("</resprocessing>", con=fqti, sep="")

        for(i in 1:AW){
    writeLines(paste("<itemfeedback ident=\"response_",i,"\" view=\"All\">", sep=""), con=fqti, sep="") ### individuelles Feeback für die i-te Antwort
      writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="") ### Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
      writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
    }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")

}



##################################################################################################################################################################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###
##################################################################################################################################################################




###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 4) Anordnungsfragen (ORDERING QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemOrdering <- function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author) {
  fqpl=getConnection(fqpl)
  fqti=getConnection(fqti)

### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Answer': bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>ORDERING QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>thumb_geometry</fieldlabel>", con=fqti, sep="")    ### Darstellungsweise der zu ordnenden Felder
          writeLines("<fieldentry>100</fieldentry>", con=fqti, sep="")
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>element_height</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>40</fieldentry>", con=fqti, sep="")        ### Höhe der Boxen mit Text/Antworten
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>points</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Points,"</fieldentry>", sep=""), con=fqti, sep="") ### maximal erreichbare Punktzahl der Aufgabe
        writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

    writeLines("<presentation label=\"OQ\">", con=fqti, sep="") ### hat keine entscheidenede Funktion (bisher) -> das Label ist nu unmittelbar nach dem Import kurz zu sehen
      writeLines("<flow>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines("<response_lid ident=\"OQT\" rcardinality=\"Ordered\">", con=fqti, sep="") ### Es handelt sich um eine OrderingQuestion -> die Antworten müssen in die korrekte Reihenfolge gebracht werden
          writeLines("<render_choice shuffle=\"Yes\">", con=fqti, sep="") ### Möglichkeit die Antwortmöglichkeiten beim Aufruf der Frage zufällig zu mischen: "Yes" oder "No"

    ### Jede Antwort wird mit einem Label gekennzeichnet -> eindeutige Zuordnung von Antworten und Lösungen (später)

        AW<-length(Answer)
        for(i in 1:AW){
         # hier wird die Anzahl möglicher Antworten bestimmt, welche in eine Reihenfolge zu bringen sind -> Reihenfolge erst später definiert

            writeLines(paste("<response_label ident=\"",i,"\">", sep=""), con=fqti, sep="") ### jede Antwortmögl. erhält ein spezisches Label
            writeLines("<material>", con=fqti, sep="")
            writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer[i],"</mattext>",sep=""), con=fqti, sep="") ### hier stehen die Antworten, welche in eine Reihenfolge zu bringen sind
            writeLines("</material>", con=fqti, sep="")
            writeLines("</response_label>", con=fqti, sep="")
        }
          writeLines("</render_choice>", con=fqti, sep="")
        writeLines("</response_lid>", con=fqti, sep="")
      writeLines("</flow>", con=fqti, sep="")
    writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):
### Hier wird die Reihenfolge festgelegt, welche bei der Beantwortung anzugeben ist

    writeLines("<resprocessing>", con=fqti, sep="")
      writeLines("<outcomes>", con=fqti, sep="")
        writeLines("<decvar></decvar>", con=fqti, sep="")
      writeLines("</outcomes>", con=fqti, sep="")

        for(i in 1:AW){
            writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<varequal respident=\"OQT\" index=\"",i,"\">",i,"</varequal>", sep=""), con=fqti, sep="")
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines("<setvar action=\"Add\"/>", con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte der Aufgaben -> bei OQ-Fragen bereits oben in Metadaten definiert
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"link_",i,"\"/>", sep=""), con=fqti, sep="") ### Verbindung zu Feedbackteil für i Antworten
      writeLines("</respcondition>", con=fqti, sep="")
    }
    writeLines("</resprocessing>", con=fqti, sep="")

        for(i in 1:AW){
    writeLines(paste("<itemfeedback ident=\"link_",i,"\" view=\"All\">", sep=""), con=fqti, sep="")   ### Feedback zum Antwort-Link i
      writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="") ### Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
      writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
    }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")

}



##################################################################################################################################################################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###
##################################################################################################################################################################



###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 5) Zuordnungsfragen (MATCHING QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemMatchingImage <- function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author){
if(dim(Answer)[2]!=3){
    ItemMatching(Titel,ID,Question,Answer,Points,fqti,fqpl,Author)
    return()
  }
  Answer1<-Answer[,1]
  Answer2<-Answer[,2]
  Answer3<-Answer[,3]
  fqpl=getConnection(fqpl)
  fqti=getConnection(fqti)

  ### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Answer1': definiert die Gruppe, zu welcher zugeordnet wird
# 'Answer2': definiert die zuzuordnende Gruppe
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


  ### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

  ### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
  writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
  writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
  writeLines("<itemmetadata>", con=fqti, sep="")
  writeLines("<qtimetadata>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
  writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
  writeLines("<fieldentry>MATCHING QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
  writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>shuffle</fieldlabel>", con=fqti, sep="")    ### Wie wird geshuffelt
  writeLines("<fieldentry>1</fieldentry>", con=fqti, sep="") ## 1 both are shuffeld
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>thumb_geometry</fieldlabel>", con=fqti, sep="")    ### Darstellungsweise der zu ordnenden Felder
  writeLines("<fieldentry>100</fieldentry>", con=fqti, sep="")
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>element_height</fieldlabel>", con=fqti, sep="")
  writeLines("<fieldentry>50</fieldentry>", con=fqti, sep="")        ### Höhe der Boxen mit Text/Antworten
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("</qtimetadata>", con=fqti, sep="")
  writeLines("</itemmetadata>", con=fqti, sep="")

  ### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

  writeLines("<presentation label=\"MQ\">", con=fqti, sep="")   ### hat keine entscheidenede Funktion (bisher) -> das Label ist nu unmittelbar nach dem Import kurz zu sehen
  writeLines("<flow>", con=fqti, sep="")
  writeLines("<material>", con=fqti, sep="")
  writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Frage -> Argument 'Question'
  writeLines("</material>", con=fqti, sep="")
  writeLines("<response_grp ident=\"MQ\" rcardinality=\"Multiple\">", con=fqti, sep="") ### Es handelt sich um ein MatchingQuestion
  writeLines("<render_choice shuffle=\"Yes\">", con=fqti, sep="") ### Möglichkeit die Antwortmöglichkeiten beim Aufruf der Frage zufällig zu mischen: "Yes" oder "No"

  ### Jede Antwort wird mit einem Label gekennzeichnet -> eindeutige Zuordnung von Antworten und Lösungen (später)

  MG1<-length(Answer1)
  for(i in 1:MG1){

    # Hier wird die Gruppe der Anworten definiert, zu welcher zugeordnet wird

    writeLines(paste("<response_label ident=\"",i,"\" match_max=\"1\" match_group=\"",paste(101:(101+length(Answer1)-1),  collapse=","),"\">", sep=""), con=fqti,
        sep="") # jede Antwortmögl. erhält ein spezisches Label
    writeLines("<material>", con=fqti, sep="")
    writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer1[i],"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Gruppe der Antworten zu der zugeordnet werden soll
    writeLines("</material>", con=fqti, sep="")
    writeLines("</response_label>", con=fqti, sep="")
  }
  # Hier wird die Gruppe der zuzuordnenden Elemente definiert

  MG2 <- c(101:(101+length(Answer1)-1))
  for(i in 1:length(MG2)){
    writeLines(paste("<response_label ident=\"",i,"\">", sep=""), con=fqti, sep="")
    writeLines("<material>", con=fqti, sep="")
    writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer2[i],"</mattext>", sep=""), con=fqti, sep="")    ## hier stehen die Elemente, welche zugeordnet werden sollen
    writeLines(paste("<matimage imagetype=\"image/png\" label=\"",runif(1,-200,200),".png\" embedded=\"base64\">",Answer3[i],"</matimage>",sep=""), con=fqti, sep="")
    writeLines("</material>", con=fqti, sep="")
    writeLines("</response_label>", con=fqti, sep="")
  }
  writeLines("</render_choice>", con=fqti, sep="")
  writeLines("</response_grp>", con=fqti, sep="")
  writeLines("</flow>", con=fqti, sep="")
  writeLines("</presentation>", con=fqti, sep="")

  ### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

  writeLines("<resprocessing>", con=fqti, sep="")
  writeLines("<outcomes>", con=fqti, sep="")
  writeLines("<decvar></decvar>", con=fqti, sep="")
  writeLines("</outcomes>", con=fqti, sep="")

  ### Hier wird die Zuordnung der beiden Matching Gruppen anhand eindeutiger Zuordnungsindizierungen festgelegt

  for(i in 1:length(MG2)){
    writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
    writeLines("<conditionvar>", con=fqti, sep="")
    writeLines(paste("<varsubset respident=\"MQ\">",MG2[i],",",MG2[i]-100,"</varsubset>", sep=""), con=fqti, sep="")
    writeLines("</conditionvar>", con=fqti, sep="")
    writeLines(paste("<setvar action=\"Add\">",Points[i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte bei korrekt zugeordneten Paaren
    writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"correct_",MG2[i],"_\"/>", sep=""), con=fqti, sep="")   ### Verbindung zu Feeback
    writeLines("</respcondition>", con=fqti, sep="")
  }
  writeLines("</resprocessing>", con=fqti, sep="")

  for(i in 1:length(MG2)){
    writeLines(paste("<itemfeedback ident=\"correct_",MG2[i],"_",MG2[i]-100,"\" view=\"All\">", sep=""), con=fqti, sep="")    ### Feedback für das i-te Matching Paar
    writeLines("<flow_mat>", con=fqti, sep="")
    writeLines("<material><mattext/></material>", con=fqti, sep="") ### Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
    writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
  }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

  ### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")

}
ItemMatching <- function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author){
  Answer1<-Answer[,1]
  Answer2<-Answer[,2]
  fqpl=getConnection(fqpl)
  fqti=getConnection(fqti)

  ### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Answer1': definiert die Gruppe, zu welcher zugeordnet wird
# 'Answer2': definiert die zuzuordnende Gruppe
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


  ### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

  ### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
  writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
  writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
  writeLines("<itemmetadata>", con=fqti, sep="")
  writeLines("<qtimetadata>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
  writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
  writeLines("<fieldentry>MATCHING QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
  writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>shuffle</fieldlabel>", con=fqti, sep="")    ### Wie wird geshuffelt
  writeLines("<fieldentry>1</fieldentry>", con=fqti, sep="") ## 1 both are shuffeld
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>thumb_geometry</fieldlabel>", con=fqti, sep="")    ### Darstellungsweise der zu ordnenden Felder
  writeLines("<fieldentry>100</fieldentry>", con=fqti, sep="")
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("<qtimetadatafield>", con=fqti, sep="")
  writeLines("<fieldlabel>element_height</fieldlabel>", con=fqti, sep="")
  writeLines("<fieldentry>50</fieldentry>", con=fqti, sep="")        ### Höhe der Boxen mit Text/Antworten
  writeLines("</qtimetadatafield>", con=fqti, sep="")
  writeLines("</qtimetadata>", con=fqti, sep="")
  writeLines("</itemmetadata>", con=fqti, sep="")

  ### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

  writeLines("<presentation label=\"MQ\">", con=fqti, sep="")   ### hat keine entscheidenede Funktion (bisher) -> das Label ist nu unmittelbar nach dem Import kurz zu sehen
  writeLines("<flow>", con=fqti, sep="")
  writeLines("<material>", con=fqti, sep="")
  writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Frage -> Argument 'Question'
  writeLines("</material>", con=fqti, sep="")
  writeLines("<response_grp ident=\"MQ\" rcardinality=\"Multiple\">", con=fqti, sep="") ### Es handelt sich um ein MatchingQuestion
  writeLines("<render_choice shuffle=\"Yes\">", con=fqti, sep="") ### Möglichkeit die Antwortmöglichkeiten beim Aufruf der Frage zufällig zu mischen: "Yes" oder "No"

  ### Jede Antwort wird mit einem Label gekennzeichnet -> eindeutige Zuordnung von Antworten und Lösungen (später)

  MG1<-length(Answer1)
  for(i in 1:MG1){

    # Hier wird die Gruppe der Anworten definiert, zu welcher zugeordnet wird

    writeLines(paste("<response_label ident=\"",i,"\" match_max=\"1\" match_group=\"",paste(101:(101+length(Answer1)-1),  collapse=","),"\">", sep=""), con=fqti,
        sep="") # jede Antwortmögl. erhält ein spezisches Label
    writeLines("<material>", con=fqti, sep="")
    writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer1[i],"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Gruppe der Antworten zu der zugeordnet werden soll
    writeLines("</material>", con=fqti, sep="")
    writeLines("</response_label>", con=fqti, sep="")
  }
  # Hier wird die Gruppe der zuzuordnenden Elemente definiert

  MG2 <- c(101:(101+length(Answer1)-1))
  for(i in 1:length(MG2)){
    writeLines(paste("<response_label ident=\"",i,"\">", sep=""), con=fqti, sep="")
    writeLines("<material>", con=fqti, sep="")
    writeLines(paste("<mattext texttype=\"text/xhtml\">",Answer2[i],"</mattext>", sep=""), con=fqti, sep="")    ## hier stehen die Elemente, welche zugeordnet werden sollen
    writeLines("</material>", con=fqti, sep="")
    writeLines("</response_label>", con=fqti, sep="")
  }
  writeLines("</render_choice>", con=fqti, sep="")
  writeLines("</response_grp>", con=fqti, sep="")
  writeLines("</flow>", con=fqti, sep="")
  writeLines("</presentation>", con=fqti, sep="")

  ### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

  writeLines("<resprocessing>", con=fqti, sep="")
  writeLines("<outcomes>", con=fqti, sep="")
  writeLines("<decvar></decvar>", con=fqti, sep="")
  writeLines("</outcomes>", con=fqti, sep="")

  ### Hier wird die Zuordnung der beiden Matching Gruppen anhand eindeutiger Zuordnungsindizierungen festgelegt

  for(i in 1:length(MG2)){
    writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
    writeLines("<conditionvar>", con=fqti, sep="")
    writeLines(paste("<varsubset respident=\"MQ\">",MG2[i],",",MG2[i]-100,"</varsubset>", sep=""), con=fqti, sep="")
    writeLines("</conditionvar>", con=fqti, sep="")
    writeLines(paste("<setvar action=\"Add\">",Points[i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte bei korrekt zugeordneten Paaren
    writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"correct_",MG2[i],"_\"/>", sep=""), con=fqti, sep="")   ### Verbindung zu Feeback
    writeLines("</respcondition>", con=fqti, sep="")
  }
  writeLines("</resprocessing>", con=fqti, sep="")

  for(i in 1:length(MG2)){
    writeLines(paste("<itemfeedback ident=\"correct_",MG2[i],"_",MG2[i]-100,"\" view=\"All\">", sep=""), con=fqti, sep="")    ### Feedback für das i-te Matching Paar
    writeLines("<flow_mat>", con=fqti, sep="")
    writeLines("<material><mattext/></material>", con=fqti, sep="") ### Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
    writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
  }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

  ### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")

}


##################################################################################################################################################################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###
##################################################################################################################################################################



###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 6) Image-Map-Fragen (IMAGE MAP QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemImage <- function(Titel,ID,Question,Base64Image,ImageCoord,Answer,Points,fqti,fqpl,Author){
cat("ItemImage..")
fqpl=getConnection(fqpl)

fqti=getConnection(fqti)
cat("found\n")
### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Base64Image': Bindet ein Image in Base64 Code ein
# 'ImageCoord': legt die Koordinaten der Verlinkungen fest
# 'Answer': bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>IMAGE MAP QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>thumb_geometry</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>100</fieldentry>", con=fqti, sep="")
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>element_height</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry></fieldentry>", con=fqti, sep="")
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>points</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",max(Points),"</fieldentry>", sep=""), con=fqti, sep="")    ### maximal erreichbare Punktzahl der Aufgabe
        writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antowrt (Dezimalanzahl, Anzahl der Stellen der Antwort)):

    writeLines("<presentation label=\"IM\">", con=fqti, sep="") ### Wird nur unmittelbar beim Import als Label angezeigt, später nirgendwo in Metadaten der Frage zu finden -> zweckfrei
      writeLines("<flow>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext texttype=\"text/xhtml\">",Question,"</mattext>",sep=""), con=fqti, sep="") ### hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines("<response_xy ident=\"IM\" rcardinality=\"Single\">", con=fqti, sep="") ### Ein ImageMap mit nur einer richtigen Lösung
          writeLines("<render_hotspot>", con=fqti, sep="")

### Einladen und Dekodierung von Images und Definition der jeweiligen Antwortmöglichkeiten in den hinterlegten Links

            ### Hier werden die gewünschten Images encodiert eingeladen

              writeLines("<material>", con=fqti, sep="")
                writeLines(paste("<matimage imagetype=\"image/jpeg\" label=\"KeineStrasse.jpg\" embedded=\"base64\">",Base64Image,"</matimage>",sep=""), con=fqti, sep="") # hier stehen die Antwortmöglichkeiten
              writeLines("</material>", con=fqti, sep="")

                Link<-length(Answer)
                for(i in 1:Link){

            ### Indizierung aller hinterlegten Verweise in einer Grafik mit eigenem Label sowie deren Positionsübergabe (Form, Koordinaten) und Bezeichnung (Answer[i]) eines EINZELNEN ImageMaps im Pool

### Die Koordinaten müssen eigentlich nur einmal in einer Liste gespeichert werden, weil sie sich nach einmaliger Feststellung nicht mehr ändern
### das Koordinatensystem ist spiegelverkehrt zu einem gewöhnlichen System -> oben links (0,0), unten rechts (Größe Bild, Größe Bild)
### Rectangle = Rechteck -> Koord: (x.ol, y.ol, x.ur, y.ur); Ellipse = Kreis -> Koord: (x,y,Radius/Durchmesser)

                        writeLines(paste("<response_label ident=\"",i,"\" rarea=\"Rectangle\">",paste(ImageCoord[[i]],collapse=","),"<material><mattext>",Answer[i],"</mattext></material>>", sep=""), con=fqti, sep="")
                        ### zu den Koord. gehörige Antwort
            writeLines("</response_label>", con=fqti, sep="")
            }
          writeLines("</render_hotspot>", con=fqti, sep="")
        writeLines("</response_xy>", con=fqti, sep="")
      writeLines("</flow>", con=fqti, sep="")
    writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

    writeLines("<resprocessing>", con=fqti, sep="")
      writeLines("<outcomes>", con=fqti, sep="")
        writeLines("<decvar></decvar>", con=fqti, sep="")
      writeLines("</outcomes>", con=fqti, sep="")

        for(i in 1:Link){
            writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<varinside respident=\"IM\" areatype=\"Rectangle\">",paste(ImageCoord[[i]],collapse=","),"</varinside>", sep=""), con=fqti, sep="")
### erneute Übergabe der Koord. für die Verweise [i]
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",Points[i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte für die verschiedene Antworten [i]
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"response_",i,"\"/>", sep=""), con=fqti, sep="")    ### Verbindung zu Feedbackteil
      writeLines("</respcondition>", con=fqti, sep="")
    }
    writeLines("</resprocessing>", con=fqti, sep="")

        for(i in 1:Link){
    writeLines(paste("<itemfeedback ident=\"response_",i,"\" view=\"All\">", sep=""), con=fqti, sep="")   ### Link zu Feeback für Antwort [i]
      writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="") ### Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
      writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
    }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")

}



##################################################################################################################################################################
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###
##################################################################################################################################################################


###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### 7) Numerische Lücken-Fragen (CLOZE QUESTION) ###
###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###


ItemNumericGaps<-function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author){
fqpl=getConnection(fqpl)
fqti=getConnection(fqti)

### Argumente der Funktion:
# 'Titel': Name des Fragenpools
# 'ID': laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
# 'Question': bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
# 'Answer': bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
# 'Points': reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
# 'Author': Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
# 'fqti, fqpl': Vgl. oben


### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.

### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>CLOZE QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
                writeLines("<qtimetadatafield>", con=fqti, sep="")
                    writeLines("<fieldlabel>textgaprating</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>ci</fieldentry>", con=fqti, sep="") ### identische Bewertung???
                writeLines("</qtimetadatafield>", con=fqti, sep="")
                writeLines("<qtimetadatafield>", con=fqti, sep="")
                    writeLines("<fieldlabel>fixedTextLength</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>20</fieldentry>", con=fqti, sep="") ###
                writeLines("</qtimetadatafield>", con=fqti, sep="")
          writeLines("<qtimetadatafield>", con=fqti, sep="")
            writeLines("<fieldlabel>identicalScoring</fieldlabel>", con=fqti, sep="")
                writeLines("<fieldentry>1</fieldentry>", con=fqti, sep="") ###
                  writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antwort (Dezimalanzahl, Anzahl der Stellen der Antwort)):

    writeLines("<presentation label=\"NUM\">", con=fqti, sep="") ### hat keine entscheidenede Funktion (bisher) -> das Label ist nur unmittelbar nach dem Import kurz zu sehen
      writeLines("<flow>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
        #if(length(Question)>1){quest <- paste(paste("&lt;p&gt;",Question,"&lt;/p&gt;&#13;&#10;",sep=""),collapse="")} else quest<-Question
    if(length(Question)>1){quest <- paste(paste("&lt;p&gt;",Question,"&lt;strong&gt;&lt;em&gt; (Lücke ",1:length(Question),")&lt;/em&gt;&lt;/strong&gt;&lt;/p&gt;&#13;&#10;",sep=""),collapse="")} else quest<-Question
          writeLines(paste("<mattext texttype=\"text/xhtml\">",quest,"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")


        Answer.DIM <- dim(Answer)
        for(Answer.i in 1:Answer.DIM[1]){
                writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext texttype=\"text/xhtml\">&lt;/p&gt;&#13;&#10;&lt;p&gt;Lücke ",Answer.i,"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines(paste("<response_num ident=\"gap_",Answer.i-1,"\" numtype=\"Decimal\" rcardinality=\"Single\">",sep=""), con=fqti, sep="")
                    #writeLines(paste("<render_fib fibtype=\"Decimal\" prompt=\"Box\" columns=\"",nchar(as.character(Answer[Answer.i,3])),"\" minnumber=\"",Answer[Answer.i,1],"\" maxnumber=\"",Answer[Answer.i,3],"\"></render_fib>",sep=""), con=fqti, sep="")
                            randomsize <- sample(0:3,1)
                            writeLines(paste("<render_fib fibtype=\"Decimal\" prompt=\"Box\" columns=\"",nchar(as.character(Answer[Answer.i,3])) + randomsize,"\" maxchars=\"",nchar(as.character(Answer[Answer.i,3])) + randomsize,"\" minnumber=\"",Answer[Answer.i,1],"\" maxnumber=\"",Answer[Answer.i,3],"\"></render_fib>",sep=""), con=fqti, sep="")
        writeLines("</response_num>", con=fqti, sep="")
            }
      writeLines("</flow>", con=fqti, sep="")
    writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

    writeLines("<resprocessing>", con=fqti, sep="")
      writeLines("<outcomes>", con=fqti, sep="")
        writeLines("<decvar></decvar>", con=fqti, sep="")
      writeLines("</outcomes>", con=fqti, sep="")


        for(Answer.i in 1:Answer.DIM[1]){
            writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<varequal respident=\"gap_",Answer.i-1,"\">",Answer[Answer.i,2],"</varequal>", sep=""), con=fqti, sep="")
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",Points[Answer.i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte bei korrekt zugeordneten Paaren
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"",Answer.i-1,"_Response_0\"/>", sep=""), con=fqti, sep="")   ### Verbindung zu Feeback
      writeLines("</respcondition>", con=fqti, sep="")
        }

    writeLines("</resprocessing>", con=fqti, sep="")

        for(Answer.i in 1:Answer.DIM[1]){
    writeLines(paste("<itemfeedback ident=\"",Answer.i-1,"_Response_0\" view=\"All\">",sep=""), con=fqti, sep="")
      writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="") # Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
      writeLines("</flow_mat>", con=fqti, sep="")
    writeLines("</itemfeedback>", con=fqti, sep="")
        }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")
}

### Argumente der Funktion:
#' @title ItemGaps: Workhorse of moste mixed type questions.
#' @param title Name des Fragenpools
#' @param ID laufender Index, welcher die Schnittstelle zwischen qpl- und qti-Datei regelt
#' @param Question bestimmt den eigentlichen Text einer Aufgabenstellung, z.B. "Berechnen sie das arith. Mittel"
#' @param Answer bestimmt die Antwortmöglichkeiten -> z.B. bei num. Aufg. das Ergebnisintervall der ind. Aufgabe -> arith. Mittel zu spez. Tabelleninhalten
#' ist eine Liste mit Listenelementen den entsprechenden Lücken. variablen in Liste siehe Details
#' @details \code{type} = num,drop,text,
#' \code{min,max,true} für grenzen beim Typ num und den wahren Wert
#' \code{labels,true} für Typ text und drop, Vektor mit labels als Antwortmöglichkeiten, true Booleen welche Antwort ist richtig.
#' \code{shuffle} "Yes" oder "No": Ob die Antwortmöglichkeiten zufällig angeordnet werden sollen.
#' @param Points reglet die Punktevergabe der Aufgaben eines Fragenpools -> idR identische Punkteanzahl für dieselbe Aufgabenstellung
#' @param Author Hier kann der jeweilige Name des Erstellers eines Fragenpools mitgegeben werden
#' @param fqti File-Handler
#' @param fqpl File-Handler
### Meta-Daten der Frage -> Art des Fragentyps, Author, ILIAS-Version, Dauer, Name des Fragenpools, usw.
ItemGaps <- function(Titel,ID,Question,Answer,Points,fqti,fqpl,Author){
fqpl=getConnection(fqpl)
fqti=getConnection(fqti)

### Wichtige erste Zeile:
# öffnet Umgebung einer EINZELNEN Frage
# zugleich Schnittstelle ('ID') zwischen qpl- und qti-Datei
# zugleich steht hier der Titel ('Titel') einer Aufgabe, z.B. Normalverteilung, Regressionsanalyse

  writeLines(paste("<item ident=\"",ID,"\" title=\"",Titel,"\">",sep=""), con=fqti, sep="")
    writeLines("<qticomment/>", con=fqti, sep="") ### Beschreibung/Kommentar zum Fragenpool
    writeLines("<duration>P0Y0M0DT0H0M0S</duration>", con=fqti, sep="") ### Zeitdauer für Aufgabenbearbeitung-> am besten auf 1 Stunde fixieren
    writeLines("<itemmetadata>", con=fqti, sep="")
      writeLines("<qtimetadata>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>ILIAS_VERSION</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>4.2.1 2011-11-10</fieldentry>", con=fqti, sep="") ### ILIAS-Version -> immer gleich
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>QUESTIONTYPE</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>CLOZE QUESTION</fieldentry>", con=fqti, sep="") ### Bezeichung des Fragentyps
        writeLines("</qtimetadatafield>", con=fqti, sep="")
        writeLines("<qtimetadatafield>", con=fqti, sep="")
          writeLines("<fieldlabel>AUTHOR</fieldlabel>", con=fqti, sep="")
          writeLines(paste("<fieldentry>",Author,"</fieldentry>",sep=""), con=fqti, sep="") ### Ersteller/Author des Pools
        writeLines("</qtimetadatafield>", con=fqti, sep="")
                writeLines("<qtimetadatafield>", con=fqti, sep="")
                    writeLines("<fieldlabel>textgaprating</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>ci</fieldentry>", con=fqti, sep="") ### identische Bewertung???
                writeLines("</qtimetadatafield>", con=fqti, sep="")
                writeLines("<qtimetadatafield>", con=fqti, sep="")
                    writeLines("<fieldlabel>fixedTextLength</fieldlabel>", con=fqti, sep="")
          writeLines("<fieldentry>20</fieldentry>", con=fqti, sep="") ###
                writeLines("</qtimetadatafield>", con=fqti, sep="")
          writeLines("<qtimetadatafield>", con=fqti, sep="")
            writeLines("<fieldlabel>identicalScoring</fieldlabel>", con=fqti, sep="")
                writeLines("<fieldentry>1</fieldentry>", con=fqti, sep="") ###
                  writeLines("</qtimetadatafield>", con=fqti, sep="")
      writeLines("</qtimetadata>", con=fqti, sep="")
    writeLines("</itemmetadata>", con=fqti, sep="")

### Strukturumgebung der Präsentation der Frage (Fragestellung, Metadaten bzgl der Antwort (Dezimalanzahl, Anzahl der Stellen der Antwort)):

  writeLines("<presentation label=\"NUM\">", con=fqti, sep="") ### hat keine entscheidenede Funktion (bisher) -> das Label ist nur unmittelbar nach dem Import kurz zu sehen
  writeLines("<flow>", con=fqti, sep="")
    Answer.DIM <- length(Answer)
    for(Answer.i in 1:Answer.DIM){
      if(Answer[[Answer.i]]$type=="text") {
        writeLines("<material>", con=fqti, sep="")
        writeLines(paste("<mattext texttype=\"text/xhtml\">&lt;p&gt;",Question[[Answer.i]][1],"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines(paste("<response_str ident=\"gap_",Answer.i-1,"\" rcardinality=\"Single\">",sep=""), con=fqti, sep="")
        writeLines(paste("<render_fib fibtype=\"String\" prompt=\"Box\" columns=\"20\" />",sep=""), con=fqti, sep="")
        writeLines("</response_str>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
        writeLines(paste("<mattext texttype=\"text/xhtml\">",Question[[Answer.i]][2],"&lt;/p&gt;</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
      } else if(Answer[[Answer.i]]$type=="drop") {
        writeLines("<material>", con=fqti, sep="")
        writeLines(paste("<mattext texttype=\"text/xhtml\">&lt;p&gt;",Question[[Answer.i]][1],"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines(paste("<response_str ident=\"gap_",Answer.i-1,"\" rcardinality=\"Single\">",sep=""), con=fqti, sep="")
        writeLines(paste("<render_choice shuffle=\"",ifelse(is.null(Answer[[Answer.i]]$shuffle),"No",Answer[[Answer.i]]$shuffle),"\">",sep=""), con=fqti, sep="")
        for(i in 1:length(Answer[[Answer.i]]$labels)) {
          writeLines(paste("<response_label ident=\"",i-1,"\">",sep=""), con=fqti, sep="")
          writeLines("<material>", con=fqti, sep="")
          writeLines(paste("<mattext>",Answer[[Answer.i]]$labels[i],"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
          writeLines("</material>", con=fqti, sep="")
          writeLines("</response_label>", con=fqti, sep="")
        }
        writeLines("</render_choice>", con=fqti, sep="")
        writeLines("</response_str>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
        writeLines(paste("<mattext texttype=\"text/xhtml\">",Question[[Answer.i]][2],"&lt;/p&gt;</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
      } else if(Answer[[Answer.i]]$type=="num") {
        writeLines("<material>", con=fqti, sep="")
        writeLines(paste("<mattext texttype=\"text/xhtml\">&lt;p&gt;",Question[[Answer.i]][1],"</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
        writeLines(paste("<response_num ident=\"gap_",Answer.i-1,"\" numtype=\"Decimal\" rcardinality=\"Single\">",sep=""), con=fqti, sep="")
        randomsize <- nchar(as.character(Answer[[Answer.i]]$true))+ sample(0:3,1)
        writeLines(paste("<render_fib fibtype=\"Decimal\" prompt=\"Box\" columns=\"", randomsize,"\" maxchars=\"",randomsize,"\" minnumber=\"",Answer[[Answer.i]]$min,"\" maxnumber=\"",Answer[[Answer.i]]$max,"\"></render_fib>",sep=""), con=fqti, sep="")
        writeLines("</response_num>", con=fqti, sep="")
        writeLines("<material>", con=fqti, sep="")
        writeLines(paste("<mattext texttype=\"text/xhtml\">",Question[[Answer.i]][2],"&lt;/p&gt;</mattext>",sep=""), con=fqti, sep="") # hier steht die Frage -> Argument 'Question'
        writeLines("</material>", con=fqti, sep="")
      } else {stop("Answer type is not text, drop, or num")}
      }
  writeLines("</flow>", con=fqti, sep="")
  writeLines("</presentation>", con=fqti, sep="")

### Strukturumgebung für die Verarbeitung und Rückgabe der Frage (Ergebnis, Punktevergabe, ggf. Feedbackkommentar):

    writeLines("<resprocessing>", con=fqti, sep="")
    writeLines("<outcomes>", con=fqti, sep="")
    writeLines("<decvar/>", con=fqti, sep="")
    writeLines("</outcomes>", con=fqti, sep="")

    for(Answer.i in 1:Answer.DIM[1]){
      if(Answer[[Answer.i]]$type=="text") {
        for(i in 1:length(Answer[[Answer.i]]$labels)) {
        writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
        writeLines(paste("<varequal respident=\"gap_",Answer.i-1,"\">",Answer[[Answer.i]]$labels[i],"</varequal>", sep=""), con=fqti, sep="")
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",if(Answer[[Answer.i]]$true[i]) Points[Answer.i] else 0,"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte bei korrekt zugeordneten Paaren
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"",Answer.i-1,"_Response_",i-1,"\"/>", sep=""), con=fqti, sep="")   ### Verbindung zu Feeback
        writeLines("</respcondition>", con=fqti, sep="")
        }
      } else if(Answer[[Answer.i]]$type=="drop") {
        for(i in 1:length(Answer[[Answer.i]]$labels)) {
          writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
          writeLines("<conditionvar>", con=fqti, sep="")
          writeLines(paste("<varequal respident=\"gap_",Answer.i-1,"\">",Answer[[Answer.i]]$labels[i],"</varequal>", sep=""), con=fqti, sep="")
          writeLines("</conditionvar>", con=fqti, sep="")
          writeLines(paste("<setvar action=\"Add\">",if(Answer[[Answer.i]]$true[i]) Points[Answer.i] else 0,"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte bei korrekt zugeordneten Paaren
          writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"",Answer.i-1,"_Response_",i-1,"\"/>", sep=""), con=fqti, sep="")   ### Verbindung zu Feeback
          writeLines("</respcondition>", con=fqti, sep="")
        }
      } else if(Answer[[Answer.i]]$type=="num") {
        writeLines("<respcondition continue=\"Yes\">", con=fqti, sep="")
        writeLines("<conditionvar>", con=fqti, sep="")
        writeLines(paste("<varequal respident=\"gap_",Answer.i-1,"\">",Answer[[Answer.i]]$true,"</varequal>", sep=""), con=fqti, sep="")
        writeLines("</conditionvar>", con=fqti, sep="")
        writeLines(paste("<setvar action=\"Add\">",Points[Answer.i],"</setvar>", sep=""), con=fqti, sep="")    ### Anzahl der zu erreichenden Punkte bei korrekt zugeordneten Paaren
        writeLines(paste("<displayfeedback feedbacktype=\"Response\" linkrefid=\"",Answer.i-1,"_Response_0\"/>", sep=""), con=fqti, sep="")   ### Verbindung zu Feeback
        writeLines("</respcondition>", con=fqti, sep="")
      } else {stop("Answer type is not text, drop, or num")}
    }
    writeLines("</resprocessing>", con=fqti, sep="")

    for(Answer.i in 1:Answer.DIM[1]){
      if(Answer[[Answer.i]]$type=="text") {
        for(i in 1:length(Answer[[Answer.i]]$labels)) {
          writeLines(paste("<itemfeedback ident=\"",Answer.i-1,"_Response_",i-1,"\" view=\"All\">",sep=""), con=fqti, sep="")
          writeLines("<flow_mat>", con=fqti, sep="")
          writeLines("<material><mattext/></material>", con=fqti, sep="") # Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
          writeLines("</flow_mat>", con=fqti, sep="")
          writeLines("</itemfeedback>", con=fqti, sep="")
        }
      } else if(Answer[[Answer.i]]$type=="drop") {
        for(i in 1:length(Answer[[Answer.i]]$labels)) {
          writeLines(paste("<itemfeedback ident=\"",Answer.i-1,"_Response_",i-1,"\" view=\"All\">",sep=""), con=fqti, sep="")
          writeLines("<flow_mat>", con=fqti, sep="")
          writeLines("<material><mattext/></material>", con=fqti, sep="") # Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
          writeLines("</flow_mat>", con=fqti, sep="")
          writeLines("</itemfeedback>", con=fqti, sep="")
        }
      } else if(Answer[[Answer.i]]$type=="num") {
        writeLines(paste("<itemfeedback ident=\"",Answer.i-1,"_Response_0\" view=\"All\">",sep=""), con=fqti, sep="")
        writeLines("<flow_mat>", con=fqti, sep="")
        writeLines("<material><mattext/></material>", con=fqti, sep="") # Möglichkeit eines Feedback, in Abhängigkeit von der Korrektheit der beantworteten Frage
        writeLines("</flow_mat>", con=fqti, sep="")
        writeLines("</itemfeedback>", con=fqti, sep="")
      } else {stop("Answer type is not text, drop, or num")}
    }
  writeLines("</item>", con=fqti, sep="")        ### schließt die Umgebung für eine EINZELNE Frage

### hier wird die Schnittstelle zwischen qpl- und qti-Datei hergestellt -> jeder Frage wird spez. ID zugewiesen

  writeLines("<PageContent>", con=fqpl, sep="")
  writeLines(paste("<Question QRef=\"",ID,"\"/>",sep=""), con=fqpl, sep="")
  writeLines("</PageContent>", con=fqpl, sep="")
}
