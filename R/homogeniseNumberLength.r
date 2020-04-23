#' Function transforming species numbers.
#'
#' Change the number of leading zeros to homogenise all numbers found in a vector of species
#' and morphospecies names.
#' The length of numbers will be homogenised among species names of a single study given in grouping_vector. If in a given study, the largest number has 3 digits, all numbers will get zeroes concatened before. 1 -> 001.
#' @param species_vector_by_group description
#'
#' @return A vector of the same length as species_vector_by_group.
#' @author Alban Sagouis
#' @examples Write an example.
#'
#'
#' @export

homogeniseNumberLength <- function(species_vector_by_group) {
   # extracting the part before the number or the whole name if no number
   before_number <- gsub(x=species_vector_by_group, pattern="[[:digit:]]+.*$", replacement="", perl=T)
   # extracting the number
   matches <- gregexpr(pattern='[0-9]+',species_vector_by_group)
   number <- regmatches(species_vector_by_group, matches)
   number[lapply(number, length)==0] <- ""
   number <- unlist(number)

   # transforming the number part by adding extra zeros where needed.
   long_number <- paste0(
      ifelse(max(nchar(number))-nchar(number)>0,0,""),
      ifelse(max(nchar(number))-nchar(number)-1>0,0,""),
      ifelse(max(nchar(number))-nchar(number)-2>0,0,""),
      ifelse(max(nchar(number))-nchar(number)-3>0,0,""),
      number
   )	# ugly code, couldn't get rep() to concatenate 0s as I wanted.

   long_number[nchar(number)==0] <- ""
   # extracting the part after the number and attributing it an empty string if no number.
   after_number <- gsub(x=species_vector_by_group, pattern="^.*[[:digit:]]+", replacement="", perl=T)
   after_number[nchar(number)==0] <- ""

   # pasting parts together and returning.
   return(paste0(before_number, long_number, after_number))
}
