# Loads a datasheet from csv and saves it
# wrapper for `savedatasheet` function

loadSheet <- function (sheetname, proj_or_sce, path = getwd(), params = NULL) 
{
  if (!(is.null(params))) {
    if (is.list(params)) {
      mySheet <- as.data.frame(params)
    }
    else {
      stop("ERROR: parameters not provided as a list")
    }
  }
  else {
    mySheet <- read.csv(paste0(path, sheetname, ".csv"), 
                        header = T)
  }
  saved_message <- saveDatasheet(proj_or_sce, mySheet, sheetname)
  if (saved_message[1] == "saved") {
    print(paste0(sheetname, " Saved"))
  }
  else {
    print(saved_message)
    stop("ERROR: Datasheet could not be loaded. Check path or name, or loading order.")
  }
}
