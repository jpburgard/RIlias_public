#######################################################################################################################################################
# LIST OF QUESTION TYPES
#
#  Numeric
#  NumericGaps
#  SingleChoice
#  MultipleChoice
#  Matching
#  Ordering
#  ImageMap
#
#######################################################################################################################################################


NumQ_MVSd <- function(Text,Question=NULL,Points,tol,Titel,iter,x,varQ){

  n        <- length(x)
  MultQ    <- c("das arithmetische Mittel","die Varianz","die Standardabweichung")
  Question <- paste("Bitte berechnen Sie",MultQ[varQ],"für die folgenden Werte. Runden Sie, falls nötig, das Ergebnis auf 4 Nachkommastellen. Hinweis: Auch bei den Zwischenrechnungen auf 4 Nachkommastellen runden.")
  answer <- function(x,type){

    mean <- mean(x)
    variance <- var(x) * (n-1)/n
    sd <- sqrt(variance)

    switch(type,
        Q1 = mean,
        Q2 = variance,
        Q3 = sd
    )
  }
  Answer <- answer(x,c("Q1","Q2","Q3")[varQ])
  Tab <- matrix(NA,ncol=n+1,nrow=1)
  Tab[1,] <- c("Beobachtungen",x)

  return(
      Question(Type="Numeric", Text=Text, Tab=Tab, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=sprintf("%g",round(c(Answer*(1-tol),Answer*(1+tol)),4)),Points=Points)
  )
}

## QuestionPool <- sapply(1:100, function(i) NumQ_MVSd(
##                     Text = "Ein Gruppe von Trierer Metereologen misst an 25 aufeinanderfolgenden Tagen die Temperatur in der Negev-Wüste.",
##                     Points = 0.4,
##                     tol = 0.01,
##                     Titel = "DS.K3.NumQ.007.",
##                     iter = i,
##                     x = sample(seq(30,45,by=0.1),25,replace=T),
##                     varQ = 3
##             )
## )

# The Questionpools are made the same way for the other questions.

#######################################################################################################################################################
#
# 		NUMERIC GAPS QUESTIONS
#
#######################################################################################################################################################



### 1) Numerical gaps question: Calculation of the OLS-coefficients of a simple regression model


NumGapsQ_Regression_coefficients <- function(Text,Question,Points,tol,Titel,iter,x,fun){
  y <- fun(x)
  reg <- lm(y~x)
  alpha <- round(coef(reg)[1],4)
  beta <- round(coef(reg)[2],4)
  n<-length(y)

  Answer <- matrix(NA, ncol=3, nrow=2,byrow=FALSE)
  Answer[1,] <- sprintf("%g",round(c(alpha*(1-tol),alpha,alpha*(1+tol)),4))
  Answer[2,] <- sprintf("%g",round(c(beta*(1-tol),beta,beta*(1+tol)),4))

  Tab <- matrix(NA,ncol=n+1,nrow=3)
  Tab[1,] <- c("i",1:n)
  Tab[2,] <- c("X",x)
  Tab[3,] <- c("Y",y)
  return(
    Question(Type="NumericGaps", Text=Text, Tab=Tab, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=Points)
  )
}

#### Example for function call:

#NumGapsQ_Regression_coefficients(

#    Text = "Let X and Y be two metric variables. Y is considered to be dependent on X. The following table provides information on the values of the variables:",   # Description of task or issue of the question
#    Titel = "Question_Reg_coeff_",     # Number or content of the question
#    Quest = "Please caculate the coefficients for the simple regression model [tex]\\hat{y}_i = \\text{Wert}_1 + \\text{Wert}_2\\cdot x_i[/tex] by using the method of least squares.",   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements).
#    Points = c(5,3),     # The achievable number of points must be specified as vectors. The length of these vectors must be the number of match-missing gaps. Each missing value can get the same or different number of points.
#    tol = 0.001,     # Range of tolerance to take into account possible rounding errors (can also be set to cero)
#    iter = x,      # Number of different questions that should be generated
#    x = sample(seq(0,40,1),10,replace=T),     # Vector of values for x variable
#    fun=function(x)round(rnorm(length(x),x*runif(1,-10,10),3),0)    # Function to generate y out of x.
#)

# Further comments:
# -
# -



#######################################################################################################################################################
#
# 		SINGLE CHOICE QUESTIONS
#
#######################################################################################################################################################


### 1) SingleChoice question: Simple single choice question


SCQ_4SimpleAnswers <- function(Text,Question,Points,Titel,iter,CorrectAnswer,WrongAnswers){

  Answer <- c(CorrectAnswer,WrongAnswers) # correct answer must be the first element of all possible answers

  return(
    Question(Type="SingleChoice", Text=Text, Tab=NULL, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=Points)
  )
}

### Example for function call:

#SCQ_4SimpleAnswers(

#    Text = NULL,   # Description of task or issue of the question
#    Titel = "Question_SC4Answers_",     # Number or content of the question
#    Quest = "Which of the following numbers is called the Euler number?",   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements).
#    Points = c(2,0,0,0),     # The achievable number of points must be specified as vectors, where the first element need to be the points for the correct answer.
#    iter = x,      # Number of different questions that should be generated
#    CorrectAnswer = 2.7183,     # Specify excatly one correct answer
#    WrongAnswers = c(1.141,1.732,3.142)     # Specify wrong answers as many as you want, but adjust the points in accordance with answers
#)

# Further comments:
# -
# -





### 2) SingleChoice question: Sample random questions with according answers


SCQ_RandomQuestions <- function(Text,Question,Points,Titel,iter,Answers1,Answers2,Answers3,Answers4){

  variation.question <- sample(1:4,1)
  Question <- paste("Which of the following numbers has",variation.question,"digits?")
  randomAnswer <- c(sample(Answers1,1),sample(Answers2,1),sample(Answers3,1),sample(Answers4,1))
  if(variation.question==1){
        CorrectAnswer <- randomAnswer[1]
        WrongAnswers <- c(randomAnswer[2],randomAnswer[3],randomAnswer[4])
  }
  if(variation.question==2){
        CorrectAnswer <- randomAnswer[2]
        WrongAnswers <- c(randomAnswer[1],randomAnswer[3],randomAnswer[4])
  }
  if(variation.question==3){
        CorrectAnswer <- randomAnswer[3]
        WrongAnswers <- c(randomAnswer[1],randomAnswer[2],randomAnswer[4])
  }
  if(variation.question==4){
        CorrectAnswer <- randomAnswer[4]
        WrongAnswers <- c(randomAnswer[1],randomAnswer[2],randomAnswer[3])
  }
  Answer <- c(CorrectAnswer,WrongAnswers) # correct answer must be the first element of all possible answers

  return(
    Question(Type="SingleChoice", Text=Text, Tab=NULL, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=Points)
  )
}

### Example for function call:

#SCQ_RandomQuestions(

#    Text = NULL,   # Description of task or issue of the question
#    Titel = "Question_SC_RandomQuestions_",     # Number or content of the question
#    Quest = Question,   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements). Question is defined within the function.
#    Points = c(2,0,0,0),     # The achievable number of points must be specified as vectors, where the first element need to be the points for the correct answer.
#    iter = x,      # Number of different questions that should be generated
#    Answers1 = c(1,2,3,4),   # Vector for the first group of answers
#    Answers2 = c(11,22,33,44),   #   Vector for the second group of answers
#    Answers3 = c(111,222,333,444),   #   Vector for the third group of answers
#    Answers4 = c(1111,2222,3333,4444)   #   Vector for the fourth group of answers, etc.
#)

# Further comments:
# -
# -



#######################################################################################################################################################
#
# 		MULTIPLE CHOICE QUESTIONS
#
#######################################################################################################################################################


### 1) MultipleChoice question:


MCQ_RandomAnswers <- function(Text,Question,Points,Titel,iter,NoTotalAnswers,CorrectStatements,WrongStatements){

  NoCorrectAnswers <- sample(c(1:(NoTotalAnswers-1)),1)
  NoWrongAnswers <- NoTotalAnswers - NoCorrectAnswers

  SampleCorrectStatements <- sample(CorrectStatements,NoCorrectAnswers)
  SampleWrongStatements <- sample(WrongStatements,NoWrongAnswers)

  Answer <- c(SampleCorrectStatements,SampleWrongStatements) # correct answer must be the first element of all possible answers

  return(
      Question(Type="MultipleChoice", Text=Text, Tab=NULL, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=c(rep(Points,NoCorrectAnswers),rep(-Points,NoWrongAnswers)))
  )
}

### Example for function call:

#MCQ_RandomAnswers(

#    Text = NULL,   # Description of task or issue of the question
#    Titel = "Question_MC_RandomAnswers_",     # Number or content of the question
#    Quest = "Which of the following statements do you agree with?",   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements). Question is defined within the function.
#    Points = function(A,CA)ifelse(is.element(A,CA),Points <- 1,Points <- -1),     # The achievable number of points must be specified as vectors, where points are depending on randomly drawn answers
#    iter = x,      # Number of different questions that should be generated
#    NoTotalAnswers = 4,  # Number of elements/statements in the multiple choice question
#    CorrectStatements = c("C1","C2","C3","C4","C5","C6","C7"),   # Vector of correct statements; also collection of statements as .txt file possible
#    WrongStatements = c("W1","W2","W3","W4","W5","W6","W7")   #   Vector of wrong statements
#)


# Further comments:
# -
# -






#######################################################################################################################################################
#
# 		MATCHING QUESTIONS
#
#######################################################################################################################################################



### 1) Matching question:



MQ_SimpleMatching <- function(Text,Question,Points,Titel,iter,AttributableGroup,MatchingGroup){

  Answer <- matrix(c(AttributableGroup,MatchingGroup),nrow=length(MatchingGroup),ncol=2)
  Answer1 <- Answer[,1]
  Answer2 <- Answer[,2]

  return(
    Question(Type="Matching", Text=Text, Tab=NULL, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=Points(Answer))
  )
}

### Example for function call:

#MQ_SimpleMatching(

#    Text = NULL,   # Description of task or issue of the question; NULL if no text is needed
#    Titel = "Question_MQ_Simple_",     # Number or content of the question
#    Quest = "Please allocate the given lower case letters to their respective capital letters.",   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements).
#    Points = function(A)rep(1,dim(A)[1]),     # The achievable number of points is depending on the number of possible matchings -> for each matching there must be allocated the same number of points
#    iter = x,      # Number of different questions that should be generated
#    AttributableGroup = c("A","B","C","D"),  # Group of elements/categories, for which a specific MatchingGroup item exist
#    MatchingGroup = c("a","b","c","d")      # Group of elements, which should be allocated to AttributableGroup
#)


# Further comments:
# -
# -



#######################################################################################################################################################
#
# 		ORDERING QUESTIONS
#
#######################################################################################################################################################


### 1) Ordering question: Simple elements



OQ_SimpleElements <- function(Text,Question,Points,Titel,iter,RankingElements){

  Answer <- sort(RankingElements)

  return(
    Question(Type="Ordering", Text=Text, Tab=NULL, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=Points)
  )
}

### Example for function call:

#OQ_SimpleElements(

#    Text = NULL,   # Description of task or issue of the question; NULL if no text is needed
#    Titel = "Question_OQ_SimpleElements_",     # Number or content of the question
#    Quest = "Please rank the given capital letters in the correct way, beginning with the letter that occurs first in the alphabet.",   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements).
#    Points = 2,     # The achievable number of points -> only if all elements are ranked in the only correct way, points can be achieved
#    iter = x,      # Number of different questions that should be generated
#    RankingElements = sample(LETTERS,4)  # Group of elements/categories, which should be ranked in the process of question

#)


# Further comments:
# -
# -



### 2) Ordering question: Random elements with switching question

OQ_RankRandomElements <- function (Text=NULL, Questions, Points, Titel, iter, RankingGroups, ways="asc")
{
  if (length(Questions) > 2) stop("OQ_RankRandomElements works with max 2 Questions")
  Qsam <- sample(1:length(Questions), 1)
  sampled.elements <- sapply(RankingGroups,function(x)sample(x,1))
  if (length(Questions)==2){
      ways <- c("asc","desc")[Qsam]
    }
  if(ways == "asc") {
    Answer <- sampled.elements
  } else{
    Answer <- sampled.elements[length(sampled.elements):1]
  }
  return(Question(Type = "Ordering", Text = Text, Tab = NULL,
          Titel = paste(Titel, sprintf("%05d", iter), sep = ""),
          Question = Questions[Qsam], Answer = Answer, Points = Points))
}

### Example for function call:

#OQ_RankRandomElements(

#    Text = NULL,   # Description of task or issue of the question; NULL if no text is needed
#    Titel = "Question_OQ_RankRandomElements_",     # Number or content of the question
#    Quest = Question,   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements). Question is defined within the function.
#    Points = 2,     # The achievable number of points -> only if all elements are ranked in the only correct way, points can be achieved
#    iter = x,      # Number of different questions that should be generated
#    RankingGroup1 = c("Gender","Colors","Party affiliation")   ,   # Group 1 of elements, which should ranked with regard to to other elements -> one is randomly drawn within the function
#    RankingGroup2 = c("School grades","Clothing sizes","Boxing weight classes","Service grade in the army")  ,    # Group 2 of elements
#    RankingGroup3 = c("Temperature in C","Temperature in F","Year dates","Calendar data")  ,    # Group 3 of elements
#    RankingGroup4 = c("GDP","Net income","Height","Weight","Speed","Temperature in K","Household size")      # Group 4 of elements
#)


# Further comments:
# -
# -








#######################################################################################################################################################
#
# 		IMAGE MAP QUESTIONS
#
#######################################################################################################################################################



# 1) Image map question: Include a simple regression analysis in R


IMQ_Reg <- function(Text,Question,Points,Titel,iter,x,y){

    mod<-(lm(y~x))

    Image <- tempfile(pattern = "file", tmpdir = tempdir())			# path for temporary image file
    jpeg(filename=Image, quality = 100, width=672, height=671)				# parameters of temporary image file

### Extract elements of R-outputs and write them into .jpeg-file

    plot(1:1000,1:1000,col=FALSE, axes=FALSE, xlab=NA, ylab=NA, main="Linear simple regression model")
    Reg.model <- "Regression model:"
    text(20,1000, paste(Reg.model), pos=4)

    # Modell

    Call <- "Call:"
    text(20,900, paste(Call), pos=4)
    formula <- "lm(formula = y ~ x)"
    text(20,850, paste(formula), pos=4)

    # residuals

    Residuals <- "Residuals:"
    text(20,750, paste(Residuals), pos=4)
    min.res <- "Min"
    text(20,700, paste(min.res), pos=4)
    Q1.res <- "1Q"
    text(150,700, paste(Q1.res), pos=4)
    Med.res <- "Median"
    text(280,700, paste(Med.res), pos=4)
    Q3.res <- "3Q"
    text(450,700, paste(Q3.res), pos=4)
    max.res <- "Max"
    text(580,700, paste(max.res), pos=4)

    text(20,650, paste(round(min(mod$res),4)), pos=4)
    text(150,650, paste(round(quantile(mod$res, probs=0.25),4)), pos=4)
    text(280,650, paste(round(quantile(mod$res, probs=0.5),4)), pos=4)
    text(450,650, paste(round(quantile(mod$res, probs=0.75),4)), pos=4)
    text(580,650, paste(round(max(mod$res),4)), pos=4)

    # coefficients

    Coeff <- "Coefficients:"
    text(20,550, paste(Coeff), pos=4)

    Est <- "Estimate"
    text(200,500, paste(Est), pos=4)
    SE <- "Std.Error"
    text(400,500, paste(SE), pos=4)
    tvalue <- "t value"
    text(600,500, paste(tvalue), pos=4)
    pvalue <- "Pr(>|t|)"
    text(800,500, paste(pvalue), pos=4)

    Intercept <- "(Intercept)"
    text(20,450, paste(Intercept), pos=4)
    text(200,450, paste(round(summary(mod)$coeff[1,1],4)), pos=4)
    text(400,450, paste(round(summary(mod)$coeff[1,2],4)), pos=4)
    text(600,450, paste(round(summary(mod)$coeff[1,3],4)), pos=4)

    if(summary(mod)$coeff[1,4] < 0.001) text(800,450, paste(sprintf("%G",summary(mod)$coeff[1,4])), pos=4)
    if(summary(mod)$coeff[1,4] > 0.001) text(800,450,paste(round(summary(mod)$coeff[1,4],5)), pos=4)
    if(summary(mod)$coeff[1,4] < 0.001) text(930,450,paste("***"), pos=4)
    if(0.001 <= summary(mod)$coeff[1,4] & summary(mod)$coeff[1,4] < 0.01) text(930,450,paste("**"), pos=4)
    if(0.01 <= summary(mod)$coeff[1,4] & summary(mod)$coeff[1,4] < 0.05) text(930,450,paste("*"), pos=4)
    if(0.05 <= summary(mod)$coeff[1,4] & summary(mod)$coeff[1,4] < 0.1) text(930,450,paste("."), pos=4)

    x <- "x"
    text(20,400, paste(x), pos=4)
    text(200,400, paste(round(summary(mod)$coeff[2,1],4)), pos=4)
    text(400,400, paste(round(summary(mod)$coeff[2,2],4)), pos=4)
    text(600,400, paste(round(summary(mod)$coeff[2,3],4)), pos=4)

    if(summary(mod)$coeff[2,4] < 0.001) text(800,400, paste(sprintf("%G",summary(mod)$coeff[1,4])), pos=4)
    if(summary(mod)$coeff[2,4] > 0.001) text(800,400,paste(round(summary(mod)$coeff[2,4],5)), pos=4)
    if(summary(mod)$coeff[2,4] < 0.001) text(930,400,paste("***"), pos=4)
    if(0.001 <= summary(mod)$coeff[2,4] & summary(mod)$coeff[2,4] < 0.01) text(930,400,paste("**"), pos=4)
    if(0.01 <= summary(mod)$coeff[2,4] & summary(mod)$coeff[2,4] < 0.05) text(930,400,paste("*"), pos=4)
    if(0.05 <= summary(mod)$coeff[2,4] & summary(mod)$coeff[2,4] < 0.1) text(930,400,paste("."), pos=4)

    # Significance codes:

    striche <- "---"
    text(20,350, paste(striche), pos=4)
    sig.codes <- "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
    text(20,300, paste(sig.codes), pos=4)

    # Residuen.std.err, R.Squared, DF, F-statistic

    res.std.err <- "Residual standard error:"
    on <- "on"
    df.text <- "degrees of freedom"
    text(20,200,paste(c(res.std.err,round(summary(mod)$sigma,4),on,summary(mod)$df[2],df.text), collapse="   "), pos=4)

    M.R.Squared <- "Multiple R-squared:"
    adj.R.Squared <- "Adjusted R-Squared:"
    text(20,150,paste(c(M.R.Squared, round(summary(mod)$r.squared,6)), collapse="   "), pos=4)
    text(450,150,paste(c(adj.R.Squared, round(summary(mod)$adj.r.squared,6)), collapse="   "), pos=4)

    F.stat <- "F-statistic:"
    and <- "and"
    df <- "DF,"
    p.value <- "p-value:"
    text(20,100,paste(c(F.stat,round(summary(mod)$fstatistic[1],5),on,round(summary(mod)$fstatistic[2]),and,round(summary(mod)$fstatistic[3]),df,p.value,round(summary(mod)$coeff[2,4],4)), collapse="   "), pos=4)

    dev.off()    # close temporary image file

    GetBase64 <-function(Output){
        require(RCurl)
        img = readBin(Output, "raw", file.info(Output)[1, "size"])
        b64 = base64Encode(img, "character")
        b64[1]
    }

    Potential.QuestReg <- c("Question 1","Question 2","Question 3","Question 4","Question 5","Question 6","Question 7","Question 8","Question 9","Question 10","Question 11")
    Question <- sample(Potential.QuestReg,1)

    PointsChangingQuests <- function(PotentialQuests,Question,points){
    (PotentialQuests %in% Question ) * points
    }

    Answer <- c(rep("Link",length(Potential.QuestReg)))

  return(
    Question(Type="ImageMap", Text=Text, Tab=NULL, Titel=paste(Titel,sprintf("%05d",iter),sep=""),Question=Question,Answer=Answer,Points=PointsChangingQuests(Potential.QuestReg, Question, Points),Base64Image=GetBase64(Image),ImageCoord=list(c(180,340,255,363),c(180,367,255,390),c(285,340,370,363),c(285,367,370,390),c(395,340,470,363),c(395,367,470,390),c(500,340,620,363),c(500,367,620,390),c(230,465,292,488),c(300,465,350,488),c(200,492,280,515)))
  )
}

### Example for function call:

#IMQ_Reg(

#    Text = "A regression analysis of two variables has the following results:",   # Description of task or issue of the question; NULL if no text is needed
#    Titel = "Question_IM_Reg_",     # Number or content of the question
#    Quest = Question,   # Formulation of a concrete question (Questions and/or Text can contain LaTeX elements). Question is defined within the function.
#    Points = 3 ,     # The achievable number of points is depending on the number of possible matchings -> for each matching there must be allocated the same number of points
#    iter = x,      # Number of different questions that should be generated
#    x = sample(1:25,10),     # vector of values for x variable
#    y = sample(20:80,10)     # vector of values for y variable
#)

# Further comments:
# -
# -


########################## Generate the upload files
?MakeIliasFiles
#MakeIliasFiles(Questionpool, And the other arguments according to personal taste.)
