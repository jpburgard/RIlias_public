#' @export
setClassUnion("numORnull", c("numeric", "NULL"))
#' @export
setClassUnion("charORnull", c("character", "NULL"))
#' @export
setClassUnion("charORlist", c("character","list", "NULL"))
#' @export
setClassUnion("charORmatORnum", c("character", "matrix","numeric"))
#' @export
setClassUnion("charORmatORnumORdfORlist", c("character", "matrix","numeric","data.frame","list","NULL"))
#' @export
setClassUnion("dfORnull", c("data.frame", "NULL"))
#' @export
setClassUnion("matORnull", c("matrix", "NULL"))
#' @export
setClassUnion("listORnull", c("list", "NULL"))
#' @export
setClassUnion("matORnumORnull", c("numeric", "matrix","NULL"))
  
#' @export
setClass ("Question",
  representation ( Type        = "character",
                   Text        = "charORnull",
                   Tab         = "matORnull",
                   Image       = "charORnull",
                   RData       = "charORmatORnumORdfORlist",
                   Titel       = "character",
                   Question    = "charORlist",
                   Base64Image = "charORnull",
                   ImageCoord  = "listORnull",
                   Answer      = "charORmatORnumORdfORlist",
                   Points      = "numeric"
                   ),
  validity = function(object) {
            if(is.na(object@Type)) stop("Type is NA")
            if(is.na(object@Titel)) stop("Titel is NA")
            if(is.na(object@Qobject)) stop("Question is NA")
            if(is.na(object@Answer)) stop("Answer is NA")
            if(is.na(object@Points)) stop("Points is NA")
            if(any(c(!is.null(object@Base64Image),!is.null(object@Image))) & !(!is.null(object@Base64Image)&!is.null(object@Image))) stop("Something is wrong with the images!")
            if(object@Type=="ImageMap" & is.null(object@ImageCoord)) stop("No coordinates for image map question!")
            if(object@Type=="Matching" & !is.matrix(Answer))  stop("Answer is needed as matrix for matching questions!")
            return(TRUE)
         }
)
