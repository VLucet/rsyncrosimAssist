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
                        export=F,
                        datasheetFolder=NULL){
  
  # Make scenario name
  sce_name <- paste(datasheetName, tag, "sub", sep = "_")
  
  # Create the scenario
  sce_object <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = sce_name)
  
  # Save datasheet in that scenario
  # Can take more than 1
  for (id_name in length(datasheetName)){
    editDatasheet(datasheetName = datasheetName[[id_name]], tag = tag, 
                  ssimObject = sce_object, argumentList = datasheetParameters[[id_name]], 
                  saveSheet = savesheet, export = export, datasheetFolder = datasheetFolder)
  }
  
  return(sce_object)
  
}