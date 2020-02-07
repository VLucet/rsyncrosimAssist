#' Allows to edit a datasheet from a given library
#'
#' @description This function wraps saveDatasheet() and datasheet()
#'
#' @export

loadDatasheet <- function (datasheetName, # Name of datasheet
                           tag="default",
                           ssimObject=NULL, # Takes on a object
                           datasheetFolder = getwd() # Where csvs are saved
){
  
  # Match with tag name
  datasheetfilename <- paste0(datasheetName, "_", tag)
  path <- paste0(file.path(paste0(datasheetFolder,datasheetName), paste0(datasheetfilename, ".csv")))
  mySheet <- read.csv(path, header = T)
  
  saved_message <- saveDatasheet(ssimObject, mySheet, datasheetName)
  
  if (saved_message[1] == "saved") {
    print(paste0(datasheetName, " saved in Library."))
  }
  
  else {
    print(saved_message)
    stop("ERROR: Datasheet could not be loaded. Check path or name, or loading order.")
  }
}
