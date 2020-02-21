#' Creates Full Scenario based on a datasheet
#'
#' @description This function will create a unique sub scenario based on a given datasheet.
#'
#' @export

fullScenario <- function(ssimProject, 
                         scenarioName, 
                         tag_list){
  
  # Get all the scenario information
  sce_df <- rsyncrosim::scenario(ssimObject = ssimProject)[,c("scenarioId", "name")]
  
  # Create the full scenario
  sce_object <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = scenarioName)
  
  # Get all the scenarios with proper tag, put them into a list of scenarios
  # TODO think about how to extend this list to the S3 system
  sub_list <- c()
  
  # Check if the tag is the same
  if (length(tag_list) == 1 & is.null(names(tag_list))){
    # TODO complete this
  } else {
    for (subname in names(tag_list)){
      subID <- sce_df[grepl(pattern = subname, sce_df$name) & 
                        grepl(pattern = tag_list[[subname]], sce_df$name),]$scenarioId
      sub <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = subID)
      sub_list <- c(sub_list, sub)
    }
  }
  print(sub_list)
  
  # Then add it as a dependencie
  rsyncrosim::dependency(sce_object, sub_list)
  
  return(sce_object)
  
}