#' Function transforming species numbers.
#'
#' Change the number of leading zeros to homogenise all numbers found in a vector of species
#' and morphospecies names.
#' The length of numbers will be homogenised among species names of a single study given in grouping_vector. If in a given study, the largest number has 3 digits, all numbers will get zeroes concatened before. 1 -> 001.
#' @param species_vector_by_group description
#'
#' @return A vector of the same length as species_vector_by_group.
#'
#' @details If a species name countains several numbers (e.g. Buteo buteoC3F4), they are ignored.
#' @author Alban Sagouis
#' @examples Write an example.
#'
#'
#' @export

homogeniseNumberLength <- function(species_vector_by_group) {
   # Is there any number in the species list?
   speciesWithNumbers <- grepl(pattern='[0-9]+',species_vector_by_group)
   if(any(speciesWithNumbers)) {

      # extracting the part before the number or the whole name if no number
      before_number <- gsub(x=species_vector_by_group, pattern="[[:digit:]]+.*$", replacement="", perl=T)

      # extracting the number
      matches <- gregexpr(pattern='[0-9]+',species_vector_by_group)
      number <- regmatches(species_vector_by_group, matches)
      numberOfNumbers <- lapply(number, length)
      number[numberOfNumbers ==0] <- ""
      number[numberOfNumbers > 1] <- ""
      number <- unlist(number)

      # transforming the number part by adding extra zeros where needed.
      long_number <- paste0(
         sapply(max(nchar(number))-nchar(number), function(x) paste(rep(x = 0, times = x), collapse="")),
         number
      )

      long_number[nchar(number)==0] <- ""
      # extracting the part after the last number or attributing it an empty string if no number.
      after_number <- gsub(x=species_vector_by_group, pattern="^.*[[:digit:]]+", replacement="", perl=F)
      after_number[nchar(number)==0] <- ""

      # pasting parts together and returning.
      species_vector_by_group_homogenised <- ifelse(
         numberOfNumbers == 1,
         paste0(before_number, long_number, after_number),
         species_vector_by_group
      )

   } else {   # if there are no numbers in the string

      species_vector_by_group_homogenised <- species_vector_by_group

   }

   return(species_vector_by_group_homogenised)

}






