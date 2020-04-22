#' Coordinate cleaning
#'
#' The function converts coordinates in various formats in a single column to 2 columns,
#' decimal degrees North East.
#' Use UTF-8 encoding to read and modify the code of the function
#' @param coords a character vector containing longitude and latitude together. Either in
#' sexadecimal or decimal degrees, latitude or longitude first, with or without a letter
#' indicateing Northing and Easting.
#' @param id a character of factor vector grouping coordinates belonging to the same
#' study or article ie. coordinates sharing the same style,
#' @param assume_good_order if there is no information, is latitude assumed to come before
#' longitude? Default to \code{FALSE}.
#' @return data.frame with two columns, x and y in decimal degrees North and East,
#' and comments on found errors.
#' @details The function uses regular expressions and parsing to split latitude
#' and longitude values using different cues and prepares the coordinate strings
#' to go through \code{\link[biogeo]{dmsparse}}.
#' @author Alban Sagouis
#' @seealso  \code{\link[biogeo]{dmsabs}}, \code{\link[biogeo]{dmsparse}},
#' \code{\link[biogeo]{quickclean}}, \code{\link[biogeo]{errorcheck}}, \code{CoordinateCleaner}.
#'
#' @examples load("data/coordinates_test_dataset_1")
#' coords <- dat$coord
#' id <- dat$study_ID
#' data.frame(dat, coordinate_cleaning(coords, id))
#'
#'
#' @export


###############
### doSplit = FALSE if the user provides already separated x and y
### See also biogeo::uniqueformats(x)/getformat(x) with biogeo::dmsabs()(for x and y in separated columns)
###############


coordinate_cleaning <- function(coords,
                                id,
                                assume_good_order = FALSE) {


   # converting special characters
   # get and include the full list from https://unicode.org/cldr/utility/confusables.jsp?a=%22&r=None
   #	convert line return to space
   coords <- gsub(x=coords, pattern="\r\n", replacement=" ")
   # " to ”
   coords <- gsub(x=coords, pattern="\"", replacement="”")
   # ʺ to ”
   coords <- gsub(x=coords, pattern="\ʺ", replacement="”")	# does not work yet for encoding reasons. R seems to automatically this character to " in commands but not in the char vector making it hard to reach. Done in Excel for now
   # ' to ’
   coords <- gsub(x=coords, pattern="\'", replacement="’")
   # ʹ to ’
   coords <- gsub(x=coords, pattern="\ʹ", replacement="’")	# does not work yet for encoding reasons. R seems to automatically this character to ' in commands but not in the char vector making it hard to reach. Done in Excel for now
   # ’’ to ”
   coords <- gsub(x=coords, pattern="’’", replacement="”")
   # ʹʹ to ”
   coords <- gsub(x=coords, pattern="\ʹ\ʹ", replacement="”")
   # '' to ’
   coords <- gsub(x=coords, pattern="\'\'", replacement="”")
   # replacing commas used as decimal separator by dots
   coords <- gsub(x=coords, pattern="([0-9])(,)([0-9])", replacement="\\1\\.\\3")
   # e to E, w to W, n to N, s to S
   coords <- toupper(coords)
   # adding spaces to separate easting and northing from numeric values
   coords <- gsub(x=coords, pattern="(.)([A-Z])", replacement="\\1 \\2")
   coords <- gsub(x=coords, pattern="([A-Z])([0-9])", replacement="\\1 \\2")




   # split Latitude and Longitude
   #	the split
   # determining the separator
   comma_number <- stringi::stri_count(str=coords, fixed=",")
   space_number <- stringi::stri_count(str=coords, fixed=" ")
   # scoln_number <- stringi::stri_count(str=coords, fixed=";")


   # If separator is space but there are many spaces in the string
   # is the northing at the beginning or in the middle?
   northing_easting_position <- ifelse(substr(coords, 1, 1) %in% c('N','S','E','W'), "beginning",
                                       ifelse(substr(coords, nchar(coords),nchar(coords)) %in% c('N','S','E','W'), "end", "none"))
   ###### Can be done with biogeo::getletter()?

   # look for the position of the central NE and split before or after depending on northing_easting_position
   northing_easting_rank <- sapply(c('N','S','E','W'), function(pattern) regexec(text=coords, pattern=pattern))
   northing_easting_rank[northing_easting_rank==-1] <- NA
   northing_easting_rank <- apply(northing_easting_rank, 2, as.numeric)
   splitting_rank <- apply(northing_easting_rank, 1 , function(x) min(x[x>1]))
   splitting_rank <- ifelse(
      northing_easting_position=="beginning",
      splitting_rank,
      ifelse(northing_easting_position=="end", splitting_rank+1, NA)
   )
   # Insertion of a comma
   stringi::stri_sub(coords[!is.na(comma_number) & comma_number==0], splitting_rank[!is.na(comma_number) & comma_number==0], (splitting_rank-1)[!is.na(comma_number) & comma_number==0]) <- ", "


   # Updating the separator count
   comma_number <- stringi::stri_count(str=coords, fixed=",")
   space_number <- stringi::stri_count(str=coords, fixed=" ")



   # simple comma split & simple space split & ", " split
   coord_split <- ifelse(comma_number==1 , strsplit(x=coords, split=c(",")),
                         ifelse(comma_number>0 & space_number>0, strsplit(x=coords, split=c(", ")),
                                ifelse(comma_number==0 & space_number==1, strsplit(x=coords, split=c(" ")), rep(NA,2))))
   coord_split <- t(sapply(coord_split, matrix, nrow=1, ncol=2))	# convert to data.table formats
   colnames(coord_split) <- c("lat","long")




   # no minus sign and degree sign in the same string (dmsparse gives a NA but no error message). It seems that a degree symbol without a letter also leads to NA through dmsparse.
   coord_split[, "long"] <- gsub(x=coord_split[, "long"], pattern="(-)(.*°.*)", replacement="W \\2")
   coord_split[, "lat"] <- gsub(x=coord_split[, "lat"], pattern="(-)(.*°.*)", replacement="S \\2")

   coord_split[, "long"] <- gsub(x=coord_split[, "long"], pattern="°", replacement=" ")
   coord_split[, "lat"] <- gsub(x=coord_split[, "lat"], pattern="°", replacement=" ")


   # It seems that dmsparse does not work properly as soon as there is a minus sign and a letter
   # Replacing N with S and S with N when there is a minus symbol.
   coord_split[, "lat"] <- ifelse(grepl(x=coord_split[, "lat"], pattern="-") & grepl(x=coord_split[, "lat"], pattern="N"),
                                  gsub(x=coord_split[, "lat"], pattern="N", replacement="S"),
                                  ifelse(grepl(x=coord_split[, "lat"], pattern="S") & grepl(x=coord_split[, "lat"], pattern="-"),
                                         gsub(x=coord_split[, "lat"], pattern="S", replacement="N"),
                                         coord_split[, "lat"]
                                  )
   )

   # deleting minus symbols when there is a letter
   coord_split[, "lat"] <- ifelse(grepl(x=coord_split[, "lat"], pattern="-") & (grepl(x=coord_split[, "lat"], pattern="N") | grepl(x=coord_split[, "lat"], pattern="S")),
                                  gsub(coord_split[, "lat"], pattern="-", replacement=""),
                                  coord_split[, "lat"]
   )

   # Replacing E with W and W with E when there is a minus symbol.
   coord_split[, "long"] <- ifelse(grepl(x=coord_split[, "long"], pattern="-") & grepl(x=coord_split[, "long"], pattern="E"),
                                   gsub(x=coord_split[, "long"], pattern="E", replacement="W"),
                                   ifelse(grepl(x=coord_split[, "long"], pattern="W") & grepl(x=coord_split[, "long"], pattern="-"),
                                          gsub(x=coord_split[, "long"], pattern="W", replacement="E"),
                                          coord_split[, "long"]
                                   )
   )

   # deleting minus symbols when there is a letter
   coord_split[, "long"] <- ifelse(grepl(x=coord_split[, "long"], pattern="-") & (grepl(x=coord_split[, "long"], pattern="E") | grepl(x=coord_split[, "long"], pattern="W")),
                                   gsub(coord_split[, "long"], pattern="-", replacement=""),
                                   coord_split[, "long"]
   )



   # Making sure that latitude and longitude are given in the good order
   # Are the easting and northing specified?
   northing_easting_info_test <- apply(northing_easting_rank, 1, function(x) sum(!is.na(x)))
   if(any(northing_easting_info_test==1)) warning(paste0("Partial information in rows ", paste(which(northing_easting_info_test==1), collapse=" ")))	# Warning if any of the coordinates has one letter only

   northing_easting_info_value <- t(apply(coord_split, 1, gsub, pattern="([0-9])*(\\.)*(-)*(’)*(”)*(°)*(\ )*", replacement=""))
   if(any(!na.omit(northing_easting_info_value[nchar(c(northing_easting_info_value)) > 0]) %in% c("N","S","E","W"))) warning(paste0("Unexpected characters in rows ", paste(which(!northing_easting_info_value[nchar(c(northing_easting_info_value)) > 0] %in% c("N","S","E","W")), collapse=" ")))

   direction_order <- ifelse(is.na(northing_easting_info_value[,1]), NA,	# is the latitude written first as expected?
                             ifelse(nchar(northing_easting_info_value[,1]) == 0 & assume_good_order, "good",
                                    ifelse(northing_easting_info_value[,1] %in% c("N","S"), "good", "inversed")))

   # inverting coordinates where needed (second value mistakefully considered as longitude switched to the first position)
   # Code should be improved to get rid of the for loop
   for(i in 1:length(direction_order)) {
      if(!is.na(direction_order[i]) & direction_order[i]=="inversed") {
         coord_split[i, ] <- rev(coord_split[i, ])
         northing_easting_info_value[i, ] <- rev(northing_easting_info_value[i, ])
      }
   }




   # sexagesimal conversion to decimal
   # decimal S and W converted to N and E. If NE not provided, the function assumes easting and northing to be E and N.
   #coord_split[,"long"] <- gsub(x=coord_split[,"lat"], pattern="E", replacement="")
   # the function works better when there is a space separating the easting and northing from the value.
   dat_split <- data.frame(ID=id, apply(coord_split, 2, as.character), stringsAsFactors=F)
   coord_sparsed <- biogeo::dmsparse(dat=dat_split, x="long", y="lat", id="ID")

   coord_sparsed$xnotes[coord_sparsed$xsec > 60] <- "impossible coord value"
   coord_sparsed$ynotes[coord_sparsed$ysec > 60] <- "impossible coord value"
   coord_sparsed$ynotes[abs(coord_sparsed$y) > 90] <- "impossible coord value"


   coord_corrected <- coord_sparsed[, c("ID", "x", "y")]

   return(coord_corrected)
}



if(FALSE)	{	# using biogeo functions from the beginning
	getformat(na.omit(coords))
	uniqueformats(na.omit(coords))	# 80+ different formats
}




# Additional features
#	reading the data
# 		reading from xl limits the possibilities of read.text such as escape characters.
#
#	checking conversions (cf biogeo package tools)
#		against continent
#		against country
#		prompting the user/saving a table with error messages giving clues about the problem
#
