% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/species_names_correction.R
\name{species_names_correction}
\alias{species_names_correction}
\title{Species names cleaning}
\usage{
species_names_correction(
  species_vector = NA,
  grouping_vector = NA,
  delimiter = " ",
  convert_to_ascii = FALSE,
  genus_name_extension = FALSE,
  numberHomogenisation = FALSE,
  replace_dots = FALSE,
  replace_question_marks = FALSE,
  erase_descriptor = TRUE
)
}
\arguments{
\item{species_vector}{description}

\item{grouping_vector}{description}

\item{delimiter}{Character wanted between words. Default to ' '.}

\item{convert_to_ascii}{description}

\item{genus_name_extension}{description}

\item{numberHomogenisation}{if \code{TRUE} (the default), the length of numbers will be homogenised
among species names of a single study given in grouping_vector. If in a given study, the largest
number has 3 digits, all numbers will get zeroes concatened before. 1 -> 001.}

\item{replace_dots}{description}

\item{replace_question_marks}{description}

\item{erase_descriptor}{description}
}
\value{
A vector of the same length as species_vector.
}
\description{
Use UTF-8 encoding to read and modify the code of the function
}
\examples{
Write an example.


}
\author{
Alban Sagouis
}
