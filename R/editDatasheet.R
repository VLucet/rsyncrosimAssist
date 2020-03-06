#' Edit a datasheet
#'
#' @description Wraps rsyncrosim::saveDatasheet() and rsyncrosim::datasheet() to edit
#' a datasheet in a syncrosim library, save it, and possibly export it to csv.
#' 
#' @param datasheetName (character) The name of the datasheet (ex: RunControl).
#' @param tag (character) The tag associated with that datasheet, can be NULL (is NULL by default).
#' @param ssimObject (ssimObject) Object of the class ssimObject from which to extract and 
#' save the datasheet.
#' @param argumentList (named list or data.frame) A named list or data.frame that contains the datasheet
#' parameters.
#' @param saveSheet (logical) Whether or not to save the datasheet into the ssimObject, default to TRUE.
#' @param erase (logical) Whether or not to erase the datasheet first before editing, default to FALSE.
#' @param export (logical) Whether or not to export the datasheet to file, default to FALSE.
#' @param datasheetFolder (character) The path to where to save the datasheet if export is TRUE, default to NULL.
#'
#' @importFrom utils read.csv write.csv
#' @export

editDatasheet <- function(datasheetName, 
                          tag=NULL, 
                          ssimObject, 
                          argumentList = NULL,
                          saveSheet = T, 
                          erase = F, 
                          export = F, 
                          datasheetFolder=NULL
){
  
  # Get current datasheet
  datasheetTemp <- rsyncrosim::datasheet(ssimObject, datasheetName, optional = T)
  
  # Erase current contents?
  if (erase){
    datasheetTemp <- datasheetTemp[0,]
  }
  
  # Make a list
  datasheetTemp <- as.list(datasheetTemp)
  
  # Check validity of arguments 
  if (!is.null(argumentList)){
    if (sum((names(argumentList) %in% names(datasheetTemp))) != length(names(argumentList))){
      
      stop("Arguments names not matching datasheet header / ", 
           "Rank of unmatched argument is ", 
           which((names(argumentList) %in% names(datasheetTemp))==F))
    }
    
    # Fill datasheet
    for (argument in names(argumentList)){
      datasheetTemp[argument] <- argumentList[argument]
    }
   
    # Remove empty
    datasheetTemp <- datasheetTemp[c(which(as.vector(lapply(datasheetTemp, FUN=length) != 0)))]
     
  }
  
  # Save datasheet
  if (saveSheet){
    rsyncrosim::saveDatasheet(ssimObject = ssimObject, name = datasheetName, 
                  data = as.data.frame(datasheetTemp))
    print(paste0("Datasheet ", datasheetName, " saved in Library."))
  }
  
  # Save to CSV or not
  if(export){
  
    if(is.null(tag)){
      datasheetFileName <- datasheetName
    } else {
      datasheetFileName <- paste0(datasheetName, "_", tag)
    }

    dir.create(file.path(datasheetFolder, datasheetName), showWarnings = FALSE)
    
    write.csv(as.data.frame(datasheetTemp), 
              file.path(datasheetFolder, datasheetName, paste0(datasheetFileName,".csv")), row.names = FALSE)
    print(paste0("Datasheet ", datasheetFileName, " saved as CSV in ", datasheetFolder))
  }
  
}