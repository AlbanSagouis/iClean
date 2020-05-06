#' Species names cleaning
#'
#' Use UTF-8 encoding to read and modify the code of the function
#' @param species_vector description
#' @param grouping_vector description
#' @param genus_name_extension description
#' @param numberHomogenisation if \code{TRUE} (the default), the length of numbers will be homogenised
#' among species names of a single study given in grouping_vector. If in a given study, the largest
#' number has 3 digits, all numbers will get zeroes concatened before. 1 -> 001.
#' @param delimiter Character wanted between words. Default to ' '.
#' @param replace_dots description
#' @param replace_question_marks description
#' @param erase_descriptor description
#' @param convert_to_ascii description
#' @return A vector of the same length as species_vector.
#' @author Alban Sagouis
#' @examples Write an example.
#'
#'
#' @export

# function that takes a vector of species names (species_vector) and makes corrections to it.
# http://tnrs.iplantcollaborative.org/TNRSapp.html





species_names_correction <- function(species_vector = NA,
   									grouping_vector = NA,
   									delimiter = " ",
   									convert_to_ascii = FALSE,
   									genus_name_extension = FALSE,
   									numberHomogenisation = FALSE,
   									replace_dots = FALSE,
   									replace_question_marks = FALSE,
   									erase_descriptor = TRUE
									)	{
   if((genus_name_extension || numberHomogenisation) && all(is.na(grouping_vector))) stop("grouping_vector is missing")
   if(class(grouping_vector) != "factor") grouping_vector <- factor(grouping_vector, levels = unique(grouping_vector))

   # Accentuated characters such as Áêãçoàúü are converted to their basic equivalent Aeacoauu
	if(convert_to_ascii) species_vector <- iconv(x=species_vector, from="latin1", to="ASCII//TRANSLIT")

	# species names containing "-"
	# "Ovidia pillo-pillo" should be Ovidia pillopillo? Both seem to be used, authors' choice should be respected.
	# all "-grp." should be turned into nothing. In some of these cases, spaces are missing and sp will be collated to the genus name. So first, "-grp.sp" has to be turned into "-grp. sp"
	species_vector <- gsub(pattern="-grp.sp", replacement=paste0(delimiter, "sp"), x=species_vector)
	species_vector <- gsub(pattern="-grp.", replacement = delimiter, x=species_vector)
	#  all "-aggregate" should be deleted
	species_vector <- gsub(pattern="-aggregate", replacement=delimiter, x=species_vector)
	# there are also cases where aggregate has no hyphen before and they are also deleted
	species_vector <- gsub(pattern="\\ aggregate", replacement=delimiter, x=species_vector)

	# "m-number" species names are used for morphospecies and are treated in the morphospecies part.

	######
	# Unknown, Unidentified, indeterminate
	# "u-number" should be transformed to "Unidentified number".
	species_vector <- gsub(pattern="(u-)([[:digit:]]+)", replacement=paste0("Unidentified", delimiter,"\\2"), x=species_vector)
	# "u4" should be transformed to "Unidentified 4"
	species_vector <- gsub(pattern="\\_(u)([[:digit:]]+)", replacement=paste0(delimiter,"unidentified", delimiter,"\\2"), x=species_vector)
	# "u_u22" should be transformed to "u unidentified 22"
	species_vector <- gsub(pattern="^(u)([[:digit:]]+)", replacement=paste0("Unidentified", delimiter, "\\2"), x=species_vector)
	# Unidentified species 3
	species_vector <- gsub(pattern="^(Unidentified) (species) ([[:digit:]]+)", replacement=paste0("Unidentified", delimiter, "sp\\.", delimiter,"\\3"), x=species_vector)
	# Indet. should be Indeterminate
	species_vector <- gsub(pattern="Indet\\.\\ ", replacement=paste0("Indeterminate", delimiter), x=species_vector)

	# names such as "Homalium_u1" are turned into "Homalium sp. 1"
	# grep(pattern="^([A-Za-z]{2,})(\\_u)([[:digit:]])", x=species_vector, value=T)
	species_vector <- gsub(pattern="^([A-Za-z]{2,})(\\_u)([[:digit:]]+)", replacement=paste0("\\1", delimiter, "sp.", delimiter, "\\3"), x=species_vector)

	# replacing "-" ?
	#species_vector[grep(x=species_vector,  pattern="-"), ]

	# replacing "." by the chosen delimiter
	if(replace_dots) species_vector <- gsub(x=species_vector, pattern="\\.", replacement=delimiter)	# aim? as is, messes with describer names.

	# replacing "text0text" by "text text"
	species_vector <- gsub(x=species_vector, pattern="([a-zA-Z])(0)([a-zA-Z])", replacement=paste0("\\1", delimiter, "\\3"))	# because "sp07" should not be affected

	# species names containing expressions between brackets
	# all expressions between brackets should be deleted but there are exceptions where the brackets should be deleted but the text inside should remain: if there is "sp." between the brackets or any taxonomic name
	# the space before should be deleted too but it can also be deleted later one when searching for all useless spaces at the end of species names.
	#brackets <- unique(species_vector[grep(x=species_vector,  pattern="\\([^}]*\\)")])
	#species_vector <- gsub(x=species_vector, pattern="\\([^}]*\\)", replacement=delimiter)

	# species names containing "?"
	# should be looked at once all expressions between brackets have been deleted.
	if(replace_question_marks) species_vector <- gsub(x=species_vector, pattern="\\?", replacement=delimiter)

	# species names containing ","
	# should be looked at once all expressions between brackets have been deleted.
	species_vector  <- gsub(x=species_vector, pattern=",", replacement=delimiter)

	# species names containing "="
	# should be all transformed into a single space
	species_vector  <- gsub(x=species_vector, pattern="=", replacement=delimiter)
	# species names containing "#"
	species_vector  <- gsub(x=species_vector, pattern="\\#", replacement=delimiter)

	# species names containing "<" or ">"
	species_vector  <- gsub(x=species_vector, pattern="<|>", replacement=delimiter)

	# species names containing "_"
	species_vector  <- gsub(x=species_vector, pattern="_", replacement=delimiter)




	# Species names with more than 2 words

	# look for all forms of "sp." "s p." "s p" to check for typos.
	if(length(grep(x=species_vector, pattern="(s\\ p)([[:digit:]])", value=F, fixed=F, perl=T))>0) warning(paste("Probable typo detected: space between genus name and sp misplaced in ", species_vector[grep(x=species_vector, pattern="(s\\ p)([[:digit:]])", value=F, fixed=F, perl=T)], sep=""))

	# "Pyraustinisp.3" OR "Pyraustinisp3" -> "Pyraustini sp. 3"
	#gsub(x=grep(x=species_vector, pattern="([[:alpha:]])(sp)([[:digit:]]*)[[:>:]]", value=T, perl=T), pattern="([[:alpha:]])(sp.)([[:digit:]]*)[[:>:]]", replacement="\\1 sp. \\3", perl=T)
	species_vector <- gsub(x=species_vector, pattern="([[:alpha:]])(sp\\.)([[:digit:]]*)[[:>:]]|([[:alpha:]])(sp)([[:digit:]]*)[[:>:]]", replacement=paste0("\\1", delimiter,"sp.", delimiter, "\\3"), perl=T)

	# spEOL -> sp.
	species_vector <- gsub(x=species_vector, pattern="sp$|sp\\ $", replacement=paste0(delimiter, "sp\\."))

	# sp .4 -> sp. 4
	#gsub(x=grep(x=species_vector,  pattern="(\\ sp)\\ \\.+([[:alnum:]])", "taxonomic.name", value=T, perl=T),  pattern="(\\ sp)\\ \\.+([[:alnum:]])", "taxonomic.name", replacement="\\1\\.\\ \\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector,  pattern="(\\ sp)\\ \\.+([[:alnum:]])", "taxonomic.name", replacement=paste0("\\1\\.", delimiter, "\\2"), fixed=F, perl=T)

	# sp.4 -> sp. 4
	#gsub(x=grep(x=species_vector,  pattern="(\\ sp\\.)+([[:alnum:]])", value=T, perl=T),  pattern="(\\ sp\\.)+([[:alnum:]])", replacement="\\1\\ \\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector,  pattern="(\\ sp\\.)+([[:alnum:]])", replacement=paste0("\\1", delimiter, "\\2"), fixed=F, perl=T)

	# sp4 -> sp. 4 ( sp04)
	#gsub(x=grep(x=species_vector,  pattern="(\\ sp)+([1-9A-Z])", value=T, perl=T),  pattern="(\\ sp)+([1-9A-Z])", "taxonomic.name", replacement="\\1\\.\\ \\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector,  pattern="(\\ sp)+([0-9A-Z])", replacement=paste0("\\1\\.", delimiter, "\\2"), fixed=F, perl=T)	# not using alnum to avoid cutting words like splendens
	# spbEOL -> sp.b
	species_vector <- gsub(x=species_vector,  pattern="(\\ sp)+([[:alnum:]])$", replacement=paste0("\\1\\.", delimiter, "\\2"), fixed=F, perl=T)

	# sp 4 -> sp. 4
	#gsub(x=grep(x=species_vector,  pattern="(\\ sp)+(\\ [[:alnum:]])", value=T, perl=T),  pattern="(\\ sp)+(\\ [[:alnum:]])", replacement="\\1\\.\\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector,  pattern="(\\ sp)+(\\ )+([[:alnum:]])",  replacement="\\1\\.\\3", fixed=F, perl=T)

	# sp  4 -> sp. 4 (2 spaces)
	#gsub(x=grep(x=species_vector,  pattern="(\\ sp)+(\\ [[:alnum:]])", value=T, perl=T),  pattern="(\\ sp)+(\\ [[:alnum:]])", replacement="\\1\\.\\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector,  pattern="(\\ sp)+(\\ )+(\\ )+([[:alnum:]])",  replacement="\\1\\.\\4", fixed=F, perl=T)

	#  spec. 4 -> sp. 4
	#gsub(x=grep(x=species_vector, pattern="\\ spec\\.", value=T), pattern="\\ spec\\.", replacement="\\ sp\\.")
	species_vector <- gsub(x=species_vector, pattern="\\ spec\\.", replacement=paste0(delimiter, "sp\\."))

	# species 4 -> sp. 4
	# gsub(x=grep(x=species_vector, pattern="(\\ species)+(\\ [[:digit:]])", value=T), pattern="(\\ species)+(\\ [[:digit:]])", replacement="\\ sp\\.\\2")
	species_vector <- gsub(x=species_vector, pattern="(\\ species)+(\\ )+([[:digit:]])", replacement=paste0(delimiter, "sp\\.\\3"))

	# " spp" should be " spp."
	#gsub(x=grep(x=species_vector, pattern="\\ spp[^\\.]|\\ spp$", value=T), pattern="\\ spp[^\\.]|\\ spp$", replacement="\\ spp\\.\\ ")
	species_vector <- gsub(x=species_vector, pattern="\\ spp[^\\.]|\\ spp$", replacement=paste0(delimiter, "spp\\.", delimiter))

	# " aff " should be " aff. "
	#gsub(x=grep(x=species_vector, pattern="\\ aff\\ |\\ aff$", value=T), pattern="\\ aff\\ |\\ aff$", replacement="\\ aff\\.\\ ")
	species_vector <- gsub(x=species_vector, pattern="\\ aff\\ |\\ aff$", replacement=paste0(delimiter, "aff\\.", delimiter))

	# "  var " should be " var. "
	species_vector <- gsub(x=species_vector, pattern="\\ var\\ |\\ var$", replacement=paste0(delimiter, "var\\.", delimiter))

	# "  nr " should be " nt. "
	species_vector <- gsub(x=species_vector, pattern="\\ nr\\ |\\ nr$", replacement=paste0(delimiter, "nr\\.", delimiter))

	# "  cf " should be " cf. "
	species_vector <- gsub(x=species_vector, pattern="\\ cf\\ |\\ cf$", replacement=paste0(delimiter, "cf\\.", delimiter))
	species_vector <- gsub(x=species_vector, pattern="\\ CF\\ |\\ CF$", replacement=paste0(delimiter, "cf\\.", delimiter))
	# "  c.f. " should be " cf. "
	species_vector <- gsub(x=species_vector, pattern="\\ c\\.f\\.\\ |\\ c\\.f\\.$", replacement=paste0(delimiter, "cf\\.", delimiter))

	# " et al" should be " et al."
	species_vector <- gsub(x=species_vector, pattern=" et al$| et al\\ ", replacement=paste0(delimiter,"et", delimiter, "al\\.", delimiter))

	# morphospecies
	# "morphospec 0562" -> "msp. 0562"
	#gsub(x=grep(x=species_vector, pattern="(morphospec)(\\ [[:digit:]])", value=T, perl=T), pattern="(morphospec)(\\ [[:digit:]])", replacement="msp\\.\\ \\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector, pattern="(morphospec)(\\ )([[:digit:]])", replacement=paste0("msp\\.", delimiter, "\\3"), fixed=F, perl=T)
	#gsub(x=grep(x=species_vector, pattern="(m-)([[:digit:]])", value=T, perl=T), pattern="(m-)([[:digit:]])", replacement="msp\\.\\ \\2", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector, pattern="(m-)([[:digit:]])", replacement=paste0("msp\\.", delimiter, "\\2"), fixed=F, perl=T)

	# M3 -> msp. 3
	# gsub(x=grep(x=species_vector, pattern="(.+)(M)([[:digit:]])", value=T, perl=T), pattern="(.+)(M)([[:digit:]])", replacement="\\1\\ msp\\.\\ \\3", fixed=F, perl=T)
	species_vector <- gsub(x=species_vector, pattern="(.+)(M)([[:digit:]])", replacement=paste0("\\1", delimiter, "msp\\.", delimiter, "\\3"), fixed=F, perl=T)



	# Genus1 -> Genus 1
	species_vector <- gsub(x=species_vector, pattern="([[:alpha:]])([[:digit:]])", perl=T, replacement=paste0("\\1", delimiter, "\\2"))

	# species codes such as Micrathena sp. A 1 should be sp. A1. Same for some fragment names.


	# some species names have describer names outside of any brackets
	# when the third word has a capital in the first letter "Onthophagus bidentatus Drapiez" OR "Uroxys pygmaeus Har." but not "Micrathena sp. D" Examples in Andr_2003. Paw paw Tortricid was modified inside the original file (Stireman)
	#grep(species_vector, pattern="^([[:alpha:]])+(\\s)+([[:alpha:]])+(\\s+[[:upper:]]+[[:lower:]]{2,})", value=T)
	#menu(c("yes","no"), title="Did you check that all cases covered by this regular expression are author name?")
	if(erase_descriptor) species_vector <- gsub(species_vector, pattern="^([[:alpha:]]+)+(\\s)+([[:alpha:]]+)+(\\s+[[:upper:]]+[[:lower:]]{2,})", replacement=paste0("\\1", delimiter, "\\3"))


	if(genus_name_extension) 	{
		# when consecutive species are written like that: Quercus robur, Q. coccifera, function replaces Q. by Quercus.
		species_vector <- unlist(tapply(species_vector, grouping_vector, iClean::extend_genus_names))
	}


	if(numberHomogenisation)	species_vector <- unlist(tapply(species_vector, grouping_vector, iClean::homogeniseNumberLength))





	# Deleting final spaces and look for double spaces
	species_vector <- gsub(pattern="\\s{2,}", replacement=delimiter, x=species_vector)	# 2 or more spaces
	species_vector <- trimws(x=species_vector, which="both")	# deleting leading an trailing spaces
   species_vector <- gsub(x=species_vector, pattern="\\s", replacement = delimiter)   # replace remaining spaces with delimiter

	species_vector <- gsub(pattern=paste0(delimiter,"{2,}"), replacement=delimiter, x=species_vector)	# 2 or more spaces

	# Every first letter should be capitalised
	species_vector <- gsub("(^[[:alpha:]])", x=species_vector, replacement="\\U\\1", perl=T)

	return(species_vector)

}
















