#' Creates Full Scenario based on a datasheet
#'
#' @description This funcstion will create a unique sub scenario based on a given datasheet.
#'
#' @export

fullScenario <- function(ssimProject, 
                         scenarioName, 
                         tag_list){
  
  # Get all the scenario information
  sce_df <- scenario(ssimObject = ssimProject)[,c("scenarioId", "name")]
  
  # Create the full scenario
  sce_object <- scenario(ssimObject = ssimProject, scenario = scenarioName)
  
  # Get all the scenarios with proper tag, put them into a list of scenarios
  # TODO think about how to extend this list to the S3 system
  sub_list <- c()
  
  for (subname in names(tag_list)){
    subID <- sce_df[grepl(pattern = subname, sce_df$name) & 
                      grepl(pattern = tag_list[[subname]], sce_df$name),]$scenarioId
    sub <- scenario(ssimObject = ssimProject, scenario = subID)
    sub_list <- c(sub_list, sub)
  }
  
  print(sub_list)
  
  # Then add it as a dependencie
  dependency(sce_object, sub_list)
  
  return(sce_object)
  
}