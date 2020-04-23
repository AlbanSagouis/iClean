#' Expands abbreviated genus names
#'
#' The function takes a strings of species names of the form \code{Bufo bufo, B. bufo},
#' and writes \code{Bufo bufo, Bufo bufo}.
#'
#' @param species_vector_by_group a character vector containing complete
#' and abbreviated genus names.
#' @return A character vector of species or genus names whom genus name has been expanded.
#' @details The function only analyses texts, there is no call to a known species list.
#' When encontering an abbreviated genus name, the function replaces it with the last
#' known complete name beginning with the same letter.
#' @author Alban Sagouis
#'
#' @examples extend_genus_names(c("Bufo bufo", "B. bufo", "Buteo buteo", "B. buteo", "G. gulo", NA))
#'
#'
#' @export

extend_genus_names <- function(species_vector_by_group) {
	#if(!all(substr(species_vector_by_group, 2,2) == ".")) { 	# if all genus names of a given study are shortened, do nothing. Is this useful or just resource consuming?
		for(i in unique(species_vector_by_group)) {

			if(!grepl(x=i, pattern="^([A-Z])\\..")) {
				previous <- i
				next
			}

			first_letter <- substr(i, 1, 1)
			if(substr(previous,1,1) != first_letter) {
				warning(paste("previous species and i don't match", i))
				next
			}

			# extract first word of previous
			first_word <- gsub(x=previous, pattern="\\ .*", replacement="")

			# replace the first letter of i by first_word
			new_name <- gsub(x=i, pattern="^([:A-Z:])\\.", replacement=first_word)
			species_vector_by_group[species_vector_by_group == i] <- new_name

			previous <- new_name	# In the loop, previous is the previous species name in the list
		}
	#}
	return(species_vector_by_group)
}
