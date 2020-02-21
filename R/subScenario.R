#' Creates Sub-Scenario based on a datasheet
#'
#' @description This function will create a unique sub scenario based on a given datasheet.
#'
#' @export

subScenario <- function(ssimProject,
                        tag = "default", 
                        datasheetName,
                        datasheetParameters, 
                        savesheet=F, 
                        export=T,
                        datasheetFolder){
  
  # Make scenario name
  sce_name <- paste(datasheetName, tag, "sub", sep = "_")
  
  # Create the scenario
  sce_object <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = sce_name)
  
  # Save datasheet in that scenario
  editDatasheet(datasheetName = datasheetName, tag = tag, 
                ssimObject = sce_object, argumentList = datasheetParameters, 
                saveSheet = savesheet, export = export, datasheetFolder = datasheetFolder)
  
  return(sce_object)
  
}