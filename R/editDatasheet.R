#' Allows to edit a datasheet from a given library
#'
#' @description This function wraps saveDatasheet() and datasheet()
#'
#' @export

editDatasheet <- function(datasheetName, # Name of datasheet
                          tag=NULL, # the tag for parameterization
                          ssimObject, # Project or library
                          argumentList = NULL, # Columns as a list
                          saveSheet = T, # Saving the sheets or not, default to yes
                          erase = F, # Erase the datasheet prior to filling it
                          export = F, # Save CSV
                          datasheetFolder=NULL # Where datasheets are to be exported 
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
      #print(argument)
      datasheetTemp[argument] <- argumentList[argument]
    }
   
    #print(datasheetTemp)
    # Remove empty
    datasheetTemp <- datasheetTemp[c(which(as.vector(lapply(datasheetTemp, FUN=length) != 0)))]
     
  }
  
  # Save datasheet
  if (saveSheet){
    rsyncrosim::saveDatasheet(ssimObject = ssimObject, name = datasheetName, 
                  data = as.data.frame(datasheetTemp))
    print(paste0("Datasheet ", datasheetName, " saved in Library."))
  }
  
  # Save to CSV
  if(export){
  
    if(is.null(tag)){
      datasheetFileName <- datasheetName
    } else {
      datasheetFileName <- paste0(datasheetName, "_", tag)
    }

    dir.create(file.path(datasheetFolder, datasheetName), showWarnings = FALSE)
    
    write.csv(as.data.frame(datasheetTemp), 
              paste0(datasheetFolder, datasheetName,"/", datasheetFileName,".csv"), row.names = FALSE)
    print(paste0("Datasheet ", datasheetFileName, " saved as CSV in ", datasheetFolder))
  }
  
}