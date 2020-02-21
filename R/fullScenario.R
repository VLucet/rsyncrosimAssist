#' Creates Full Scenario based on a datasheet
#'
#' @description This function will create a unique sub scenario based on a given datasheet.
#'
#' @export

fullScenario <- function(ssimProject, 
                         scenarioName, 
                         tag){
  
  # Get all the sub scenario information
  sce_df <- rsyncrosim::scenario(ssimObject = ssimProject)[,c("scenarioId", "name")]
  sub_df <- sce_df[mapply(x=sce_df$name, FUN=grepl, pattern="sub"),] # only get sub scenarios
  
  # Create the full scenario
  sce_object <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = scenarioName)
  
  # Get all the scenarios with proper tag, put them into a list of scenarios
  # TODO think about how to extend this list to the S3 system
  sub_list <- c()
  
  # Matches tag
  # Check if only a string was given
  if (length(tag) == 1 & is.null(names(tag))){
    matches <- mapply(x=sub_df$name, FUN=grepl, pattern=as.character(tag))
    subID <- sub_df[matches,]$scenarioId
    sub_list <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = subID)
  } else {
    # Otherwise loop to match the list of tags
    for (subname in names(tag)){
      matches <- grepl(pattern = subname, sub_df$name) & grepl(pattern = tag[[subname]], sub_df$name)
      subID <- sub_df[matches,]$scenarioId
      sub <- rsyncrosim::scenario(ssimObject = ssimProject, scenario = subID)
      sub_list <- c(sub_list, sub)
    }
  }
  
  # Then add it as a dependencie
  rsyncrosim::dependency(sce_object, sub_list)
  
  return(sce_object)
  
}