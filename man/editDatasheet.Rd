% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/editDatasheet.R
\name{editDatasheet}
\alias{editDatasheet}
\title{Edit a datasheet}
\usage{
editDatasheet(
  datasheetName,
  tag = NULL,
  ssimObject,
  argumentList = NULL,
  saveSheet = T,
  erase = F,
  export = F,
  datasheetFolder = NULL,
  ... = NULL
)
}
\arguments{
\item{datasheetName}{(character) The name of the datasheet (ex: RunControl).}

\item{tag}{(character) The tag associated with that datasheet, can be NULL (is NULL by default).}

\item{ssimObject}{(ssimObject) Object of the class ssimObject from which to extract and 
save the datasheet.}

\item{argumentList}{(named list or data.frame) A named list or data.frame that contains the datasheet
parameters.}

\item{saveSheet}{(logical) Whether or not to save the datasheet into the ssimObject, default to TRUE.}

\item{erase}{(logical) Whether or not to erase the datasheet first before editing, default to FALSE.}

\item{export}{(logical) Whether or not to export the datasheet to file, default to FALSE.}

\item{datasheetFolder}{(character) The path to where to save the datasheet if export is TRUE, default to NULL.}

\item{...}{Further arguments to be passed onto saveDatasheet()}
}
\description{
Wraps rsyncrosim::saveDatasheet() and rsyncrosim::datasheet() to edit
a datasheet in a syncrosim library, save it, and possibly export it to csv.
}
