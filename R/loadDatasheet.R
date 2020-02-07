#' Allows to edit a datasheet from a given library
#'
#' @description This function wraps saveDatasheet() and datasheet()
#'
#' @export

loadDatasheet <- function (datasheetName, # Name of datasheet
                           ssimObject, # Takes on a object
                           datasheetFolder = getwd(), # Where csvs are saved
                           params = NULL # TODO to review if needed
){
  if (!(is.null(params))) {
    if (is.list(params)) {
      mySheet <- as.data.frame(params)
    }
    else {
      stop("ERROR: parameters not provided as a list")
    }
  }
  else {
    mySheet <- read.csv(paste0(datasheetFolder, datasheetName, ".csv"), 
                        header = T)
  }
  saved_message <- saveDatasheet(ssimObject, mySheet, datasheetName)
  if (saved_message[1] == "saved") {
    print(paste0(datasheetName, " saved in Library."))
  }
  else {
    print(saved_message)
    stop("ERROR: Datasheet could not be loaded. Check path or name, or loading order.")
  }
}
