#' Allows to edit a datasheet from a given library
#'
#' @description This function wraps saveDatasheet() and datasheet()
#'
#' @export

editDatasheet <- function(datasheetName, # Name of datasheet
                          ssimObject, # Project or library
                          argumentList, # Columns as a list
                          saveSheet = T, # Saving the sheets or not, default to yes
                          erase = F, # Erase first?
                          export=F, # Save Csv
                          datasheetFolder=NULL # Erase the datasheet prior to filling it
){
  
  # Get current datasheet and turn to list
  datasheetTemp <- as.list(rsyncrosim::datasheet(ssimObject, datasheetName, optional = T))
  
  
  # Check validity of arguments 
  if (sum(!(names(argumentList) %in% names(datasheetTemp))) == 1){
    stop("Arguments names not matching datasheet header / ", 
         "Rank of unmatched argument is ", 
         which((names(argumentList) %in% names(datasheetTemp))==F))
  }
  
  # Erase current contents?
  if (erase){
    datasheetTemp <- datasheetTemp[0,]
  }
  
  # Fill datasheet
  for (argument in names(argumentList)){
    #print(argument)
    datasheetTemp[argument] <- argumentList[argument]
  }
  
  #print(datasheetTemp)
  # Remove empty
  datasheetTemp <- datasheetTemp[c(which(as.vector(lapply(datasheetTemp, FUN=length) != 0)))]
  
  # Save datasheet
  if (saveSheet){
    saveDatasheet(ssimObject = ssimObject, name = datasheetName, 
                  data = as.data.frame(datasheetTemp))
    print(paste0("Datasheet ", datasheetName, " saved in Library."))
  }
  
  if(export){
    write.csv(as.data.frame(datasheetTemp), 
              paste0(datasheetFolder, datasheetName,".csv"), row.names = FALSE)
    print(paste0("Datasheet ", datasheetName, " saved as CSV in ", datasheetFolder))
  }

}