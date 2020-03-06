#' Load a datasheet into library from CSV
#'
#' @description Wraps rsyncrosim::saveDatasheet() to read in a load into library a datasheet directly
#' from a csv file.
#' 
#' @param datasheetName (character) The name of the datasheet (ex: RunControl).
#' @param tag (character) The tag associated with that datasheet, can be NULL, default to "default".
#' @param ssimObject (ssimObject) Object of the class ssimObject from which to extract and 
#' @param datasheetFolder (character) The path to where to save the datasheet if export is TRUE, default to NULL.
#' @param ... Further arguments to be passed onto saveDatasheet()
#' 
#' @export

loadDatasheet <- function (datasheetName,
                           tag="default",
                           ssimObject=NULL,
                           datasheetFolder = NULL,
                           ...=NULL
){
  
  # Match with tag name
  if(is.null(tag)){
    datasheetFileName <- datasheetName
  } else {
    datasheetFileName <- paste0(datasheetName, "_", tag)
  }
  
  # Read the file
  path <- paste0(file.path(paste0(datasheetFolder,datasheetName), paste0(datasheetFileName, ".csv")))
  mySheet <- read.csv(path, header = T)
  
  # Save the sheet
  saved_message <- rsyncrosim::saveDatasheet(ssimObject, mySheet, datasheetName, ...)
  
  # Evaluate success
  if (saved_message[1] == "saved") {
    print(paste0(datasheetName, " saved in Library."))
  } else {
    print(saved_message)
    stop("ERROR: Datasheet could not be loaded. Check path or name, or loading order.")
  }
}
