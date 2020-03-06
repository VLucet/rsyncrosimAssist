#' Create Sub-Scenario based on one or more datasheet(s)
#'
#' @description Wrap around editDatasheet and rsyncrosim::scenario to create a 
#' unique sub scenario based on a given datasheet or set of datasheets.
#' 
#' @param ssimProject (ssimObject) Syncrosim Projectto save to.
#' @param tag (character) The tag associated with that datasheet, can be NULL, default to "default".
#' @param datasheetNames (character or character vector) The name(s) of the datasheet(s).
#' @param datasheetParameters (list or named list) List or named list containing the parameters for the 
#' datasheet(s).
#' @param saveSheet (logical) Whether or not to save the datasheet into the ssimObject, default to TRUE.
#' @param export (logical) Whether or not to export the datasheet to file, default to FALSE.
#' @param datasheetFolder (character) The path to where to save the datasheet if export is TRUE, default to NULL.
#' @param merge (logical) Whether or not to merge dependencies, default to FALSE.
#'
#' @export

subScenario <- function(ssimProject,
                        tag = "default", 
                        datasheetNames,
                        datasheetParameters, 
                        saveSheet=F, 
                        export=F,
                        datasheetFolder=NULL, 
                        merge=F){
  
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
                  saveSheet = saveSheet, export = export, datasheetFolder = datasheetFolder)
  }
  
  
  # If merge
  if(merge){
    mergeDependencies(sce_object)
  }
  
  return(sce_object)
  
}