#' Creates Sub-Scenario based on a datasheet
#'
#' @description This function will create a unique sub scenario based on a given datasheet.
#'
#' @export

subScenario <- function(ssimProject,
                        tag = "default", 
                        datasheetNames,
                        datasheetParameters, 
                        savesheet=F, 
                        export=F,
                        datasheetFolder=NULL){
  
  if(length(datasheetNames)==1){
    datasheetParameters <- list(datasheetParameters)
    names(datasheetParameters) <- datasheetNames
  } else{
    datasheetParameters <- as.list(datasheetParameters)
  }
  
  # Make scenario name
  sce_name <- paste(paste(sort(datasheetNames), collapse = "_&_"), tag, "sub", sep = "_")
  
  # Create the scenario
  sce_object <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = sce_name)
  
  # Save datasheet in that scenario
  # Can take more than 1
  for (name in datasheetNames){
    
    editDatasheet(datasheetName = name, tag = tag, 
                  ssimObject = sce_object, argumentList = datasheetParameters[[name]], 
                  saveSheet = savesheet, export = export, datasheetFolder = datasheetFolder)
  }
  
  return(sce_object)
  
}