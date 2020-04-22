#' Homogenises site names
#' The function takes a strings of heterogeneous site names, corrects misprints and
#' homogenises spacing, capital letters and names.
#'
#' @param site_vector a character vector containing site names to correct
#' @param add_site_category_to_acronyms Should a common name specified in \code{site_category}
#' should be added to sites only described by an acronym. Default to \code{FALSE}.
#' @param site_category Word added to accronyms if \code{add_site_category_to_acronyms = TRUE}.
#' @param convert_to_ascii Change the encoding of the vector to ASCII which replaces all
#' special characters to their closest ASCII equivalent:
#' accentuated characters such as Áêãçoàúü are converted to their basic equivalent Aeacoauu
#' @return A character vector of same length as site_vector
#' @details details
#' @author Alban Sagouis
#'
#' @examples extend_genus_names(c("Bufo bufo", "B. bufo", "Buteo buteo", "B. buteo"))
#' exmpl <- c('s2','1','site.3','forestPrime','forest,border','forest border','(border)')
#' data.frame(exmpl,
#'            corrected = site_names_correction(site_vector = exmpl,
#'                       add_site_category_to_acronyms = TRUE)
#')
#'          exmpl     corrected
#' 1            s2       Site s2
#' 2             1        Site 1
#' 3        site.3        Site.3
#' 4   forestPrime  Forest Prime
#' 5 forest,border Forest,border
#' 6 forest border Forest border
#' 7      (border)      (border)
#'
#' @export

site_names_correction <- function(site_vector=NA,
                                  add_site_category_to_acronyms=FALSE,
                                  site_category="Site",
                                  convert_to_ascii=FALSE) {

	# Accentuated characters such as Áêãçoàúü are converted to their basic equivalent Aeacoauu
	if(convert_to_ascii) site_vector <- iconv(x=site_vector, from="latin1", to="ASCII//TRANSLIT")

	###############################################
	# Homogeneisation needed between "." "_" and " "
	# "letter.letter" -> "letter letter"  (avoiding "4.2ha" and "No. 2")
	site_vector <- gsub(x=site_vector, pattern="([[:alpha:]])(\\.)([[:alpha:]])", replacement="\\1\\ \\3")
	# "_"
	# replacing all "_" with spaces
	#grep(x=site_vector, pattern="\\_", value=T)
	site_vector <- gsub(x=site_vector, pattern="\\_", replacement="\\ ")	# check if it is working in Owen_2008

	# brackets No change, informations may be needed
	#grep(x=site_vector, pattern="\\(", value=TRUE)
	#if(modifications) site_vector <- gsub(x=site_vector, pattern="([[:alnum:]])()([[:alnum:]])", replacement="\\1\\ \\3")
	# "," and ";" -> " " to avoid column delimitation symbols
	#grep(x=site_vector, pattern=", |; ", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern=", |; ", replacement=" ")
	# "'" and """ -> " " to avoid column delimitation symbols
	#grep(x=site_vector, pattern="'", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="'", replacement=" ")

	# When a small letter is followed by a capital letter, a space should be added
	#grep(site_vector, pattern="([a-z]+)([A-Z])", value=T)
	site_vector <- gsub(site_vector, pattern="([a-z]+)([A-Z])", replacement="\\1 \\2")



	# When numbers only, (delete decimal point and decimals +) add "Site " before
	# Deleting decimals in site numbers and adding "Site " at the beginning
	#grep(pattern="^(\\s*)(\\d*)(\\.\\d*)$", x=site_vector, value=T)
	site_vector <- gsub(pattern="^(\\s*)(\\d*)(\\.\\d*)$", x=site_vector, replacement=paste0(site_category,"\\ \\2"))
	# " 13", "13", "   13  " -> "Site 13"
	#grep(x=site_vector, pattern="^(\\s*)([[:digit:]]+)$", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)([[:digit:]]+)$", replacement=paste0(site_category,"\\ \\2"))
	# 4a -> Site 4a but not 60th
	#grep(x=site_vector, pattern="^(\\s*)([[:digit:]]+[[:alpha:]]{1})$", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)([[:digit:]]+[[:alpha:]]{1})$", replacement=paste0(site_category,"\\ \\2"))

	# When only one "f" and a number, replace F by Fragment
	#grep(x=site_vector, pattern="^(f)([[:digit:]]+)$|^(F)([[:digit:]]+)$", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="^(f)([[:digit:]]+)$", replacement="Fragment\\ \\2")
	site_vector <- gsub(x=site_vector, pattern="^(F)([[:digit:]]+)$", replacement="Fragment\\ \\2")	# in 2 lines because replacement \\2 does not work the same way when there is a "|"

	# When only one letter but not a capital letter, add "Site " before
	site_vector <- gsub(x=site_vector, pattern="^([a-z]{1})$", replacement=paste0(site_category,"\\ \\1"))

	# Acronyms, all capital letters, max 4 letters, add "Site" before.
	if(add_site_category_to_acronyms) site_vector <- gsub(x=site_vector, pattern="^([A-Z]{1,4})$", replacement=paste(site_category, "\\ \\1", sep=""))


	# When only one letter (not f) and a number (either lower or upper case), add Site before
	#grep(x=site_vector, pattern="^([[:alpha:]]{1})([[:digit:]]+)$", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="^([[:alpha:]]{1})([[:digit:]]+)$", replacement=paste0(site_category,"\\ \\1\\2"))

	# sites called by an acronym, add Fragment before.
	# the rule works when the fragment is called VBMS but it is not meaningful when the site is called CF2 for Continuous Forest 2
	#grep(x=site_vector, pattern="^([A-Z0-9]{2,4})$", value=TRUE)
	#if(modifications) site_vector <- gsub(x=site_vector, pattern="^([A-Z0-9]{2,4})$", replacement="Fragment\\ \\1")


	# Add spaces between the many "fragment3"
	#grep(x=site_vector, pattern="(\\s*)(fragment)([[:digit:]])", value=T)
	# Fragment24 -> Fragment 24
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(Fragment)([[:digit:]]+)", replacement="Fragment\\ \\3")
	# fragment24 -> Fragment 24
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(fragment)([[:digit:]]+)", replacement="Fragment\\ \\3")
	# "Large.fragment2" -> "Large fragment 2"
	site_vector <- gsub(x=site_vector, pattern="\\.(fragment)([[:digit:]]+)", replacement="\\ fragment\\ \\2")
	# "Large.forest2" -> "Large forest 2"
	site_vector <- gsub(x=site_vector, pattern="\\.(forest)([[:digit:]]+)", replacement="\\ forest\\ \\2")


	# "Site17" -> "Site 17" ?why can't it be done in one line?
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(Site)([[:digit:]]+)", replacement="Site\\ \\3")
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(site)([[:digit:]]+)", replacement="Site\\ \\3")
	#if(modifications) site_vector <- gsub(x=site_vector, pattern="^(\\s*)(site)([[:digit:]]+)", replacement="Site\\ \\3")
	#if(modifications) site_vector <- gsub(x=site_vector, pattern="^(\\s*)(site) ([[:digit:]]+)", replacement="Site\\ \\3")

	# "Island17" "IslandX2" -> "Island_17" (but not Islands)
	#grep(x=site_vector, pattern="^(\\s*)(Island)([[:digit:]]+)$|^(\\s*)(island)([[:digit:]]+)$", value=T)
	site_vector <- gsub(x=site_vector, pattern="(\\s*)(Island)([[:digit:]]+)$", replacement="Island\\ \\3")	#Island23
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(island)([[:digit:]]+)$", replacement="Island\\ \\3")
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(Island)([[:alnum:]]{2,})$", replacement="Island\\ \\3")	#IslandX1
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(island)([[:alnum:]]{2,})$", replacement="Island\\ \\3")
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(Isl)([[:digit:]]+)$", replacement="Island\\ \\3")	#Isl23
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(isl)([[:digit:]]+)$", replacement="Island\\ \\3")	#Isl23
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(Isl) ([[:digit:]]+)$", replacement="Island\\ \\3")	#Isl23
	site_vector <- gsub(x=site_vector, pattern="^(\\s*)(isl) ([[:digit:]]+)$", replacement="Island\\ \\3")	#Isl23

	# "F13" -> "Fragment 13" OR Fragment F13?
#	grep(x=site_vector, pattern="^(F)([[:digit:]]+)$", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="^(F)([[:digit:]]+)$", replacement="Fragment\\ \\2")
	# "S13" ->  "Site 13" OR Fragment S13?
#	grep(x=site_vector, pattern="^(S)([[:digit:]]+)$", value=TRUE)
	site_vector <- gsub(x=site_vector, pattern="^(S)([[:digit:]]+)$", replacement="Site\\ \\2")


	# Always capitalise first letter
	site_vector <- gsub("(^[[:alpha:]])", x=site_vector, replacement="\\U\\1", perl=T)




	# When any word is followed by a number without space in between, add space
#	x<-c("truc 2 mqchin3"," frt6","DU45","machin 2 Truc3","Fragment w106","sl 10 s1","WGPt04 woodland")
#	gsub(x=grep(x=site_vector, pattern="([A-Z]*)([a-z]{2,})([[:digit:]]+)", value=T), pattern="([A-Z]*)([a-z]{2,})([[:digit:]]+)", replacement="\\1\\2 \\3")

	site_vector <- gsub(x=site_vector, pattern="([A-Z]*)([a-z]{2,})([[:digit:]]+)", replacement="\\1\\2 \\3")

	# deleting leading and final spaces, and double spaces
	site_vector <- gsub(pattern="\\s{2,}", replacement=" ", x=site_vector)	# 2 or more spaces
	site_vector <- trimws(x=site_vector, which="both")	# deleting leading an trailing spaces


	# replacing all spaces with "_"
	#grep(x=site_vector, pattern="\\s", value=T)
	#if(modifications) site_vector <- gsub(x=site_vector, pattern="\\s", replacement="\\_")
	#if(modifications) site_vector <- gsub(pattern="\\_{2,}", replacement="_", x=site_vector)	# 2 or more spaces

	return(site_vector)
}
