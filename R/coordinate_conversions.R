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
#' @param assume_good_order If there is no information (a letter indicating Northing and
#' Easting), latitude is assumed to come before longitude. Default to \code{TRUE}.
#' @param result_format If \code{complete}, the default, the function returns the table given by
#' \code{\link[biogeo]{dmsparse}} with coordinates decomposed in degrees, minutes, seconds, decimal degrees and comments.
#' If \code{simple}, only latitude and longitude are returned.
#' @return data.frame. If \code{result_format == 'simple'}: two columns, x and y
#' with coordinates in decimal degrees East and North. If \code{result_format == 'complete'}, coordinates are given
#' with comments on errors found in initial values.
#' @details The function uses regular expressions and parsing to split latitude
#' and longitude values using different cues and prepares the coordinate strings
#' to go through \code{\link[biogeo]{dmsparse}}. If coordinates are in a consistent
#' format, \code{\link[biogeo]{dmsparse}} or \code{\link[biogeo]{dmsparsefmt}} can be used.
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
# Additional features
### doSplit = FALSE if the user provides already separated x and y
### See also biogeo::uniqueformats(x)/getformat(x) with biogeo::dmsabs()(for x and y
### in separated columns)
#	checking conversions (cf biogeo package tools)
#		against continent
#		against country
#		prompting the user/saving a table with error messages giving clues about the problem
#

###############


coordinate_cleaning <- function(coords,
                                id,
                                assume_good_order = TRUE,
                                result_format = 'complete') {


   # converting special characters
   # get and include the full list from https://unicode.org/cldr/utility/confusables.jsp?a=%22&r=None
   #	convert line return to space
   coords <- gsub(x=coords, pattern="\r\n", replacement=" ")
   # " to ”
   coords <- gsub(x=coords, pattern="\"", replacement="”")
   # ʺ to ”
   coords <- gsub(x=coords, pattern="\ʺ", replacement="”")
   # ' to ’
   coords <- gsub(x=coords, pattern="\'", replacement="’")
   # ʹ to ’
   coords <- gsub(x=coords, pattern="\ʹ", replacement="’")
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
   northing_easting_position <- ifelse(substr(coords, 1, 1) %in% c('N','S','E','W'),
                                       "beginning",
                                       ifelse(substr(coords, nchar(coords),nchar(coords)) %in% c('N','S','E','W'),
                                              "end",
                                              "none"
                                              )
                                       )
   ###### Can be done with biogeo::getletter()?

   # look for the position of the central NE and split before or after depending on northing_easting_position
   northing_easting_rank <- sapply(c('N','S','E','W'), function(pattern) regexec(text=coords, pattern=pattern))
   northing_easting_rank[northing_easting_rank==-1] <- NA
   northing_easting_rank <- apply(northing_easting_rank, 2, as.numeric)
   splitting_rank <- apply(northing_easting_rank, 1 , function(x) suppressWarnings(min(x[x>1], na.rm=T)))
   splitting_rank <- ifelse(
      northing_easting_position=="beginning",
      splitting_rank,
      ifelse(northing_easting_position=="end", splitting_rank+1, NA)
   )
   # Insertion of a comma
   which_insert_comma <- which(!is.na(comma_number) & comma_number==0 & !is.na(splitting_rank))
   stringi::stri_sub(str = coords[which_insert_comma],
                     from = splitting_rank[which_insert_comma],
                     to = (splitting_rank-1)[which_insert_comma]) <- ", "


   # Potential separator count
   comma_number <- stringi::stri_count(str=coords, fixed=",")
   space_number <- stringi::stri_count(str=coords, fixed=" ")
   semicolon_number <- stringi::stri_count(str=coords, fixed=";")
   dot_number <- stringi::stri_count(str=coords, fixed=".")



   # simple comma split & simple space split & ", " split
   coord_split <- ifelse(comma_number==1 , strsplit(x=coords, split=c(",")),
                         ifelse(comma_number>0 & space_number>0,
                                strsplit(x=coords, split=c(", ")),
                                ifelse(comma_number==0 & space_number==1 & semicolon_number == 0,
                                       strsplit(x=coords, split=c(" ")),
                                       ifelse(semicolon_number == 1,
                                              strsplit(x=coords, split=c(";")),
                                              ifelse(dot_number == 1,
                                                     strsplit(x=coords, split=c("\\.")),
                                                     rep(NA,2)
                                              )
                                       )
                                )
                         )
   )
   coord_split <- t(sapply(coord_split, matrix, nrow=1, ncol=2))
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
   # Does it work when there is letter + sign? W-53.325897 ?
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

   if(assume_good_order) {
      direction_order <- ifelse(is.na(northing_easting_info_value[,1]),
                                NA,
                                ifelse(nchar(northing_easting_info_value[,1]) == 0,
                                       "good",
                                       ifelse(northing_easting_info_value[,1] %in% c("N","S"),
                                              "good",
                                              "inversed")))
   } else {
      direction_order <- ifelse(is.na(northing_easting_info_value[,1]) | nchar(northing_easting_info_value[,1]) == 0,
                                NA,
                                ifelse(northing_easting_info_value[,1] %in% c("N","S"),
                                       "good",
                                       "inversed"))
   }


   # inverting coordinates where needed (second value mistakefully considered as longitude switched to the first position)
   # Code should be improved to get rid of the for loop
   for(i in 1:length(direction_order)) {
      if(!is.na(direction_order[i]) && direction_order[i]=="inversed") {
         coord_split[i, ] <- rev(coord_split[i, ])
         northing_easting_info_value[i, ] <- rev(northing_easting_info_value[i, ])
      }
   }

   # Adding a letter for all values without easting or northing info
   if(assume_good_order) {
      coord_split[northing_easting_info_test == 0, 'lat'] <- paste0('N', coord_split[northing_easting_info_test == 0, 'lat'])
      coord_split[northing_easting_info_test == 0, 'long'] <- paste0('E', coord_split[northing_easting_info_test == 0, 'long'])
   }



   # sexagesimal conversion to decimal
   # decimal S and W converted to N and E. If NE not provided, the function assumes easting and northing to be E and N.
   #coord_split[,"long"] <- gsub(x=coord_split[,"lat"], pattern="E", replacement="")
   # the function works better when there is a space separating the easting and northing from the value.
   dat_split <- data.frame(ID=id, apply(coord_split, 2, as.character), stringsAsFactors=F)
   coord_sparsed <- biogeo::dmsparse(dat=dat_split, x="long", y="lat", id="ID")

   problematic_long_values <- which(coord_sparsed$xmin > 60 | coord_sparsed$xsec > 60 | abs(coord_sparsed$x) > 180)
   coord_sparsed[problematic_long_values, c('xnotes','exclude')] <- rep(c("impossible coord value", 1), each=length(problematic_long_values))

   problematic_lat_values <- which(coord_sparsed$ymin > 60 | coord_sparsed$ysec > 60 | abs(coord_sparsed$y) > 90)
   coord_sparsed[problematic_lat_values, c('ynotes','exclude')] <- rep(c("impossible coord value", 1), each=length(problematic_lat_values))


   coord_corrected <- coord_sparsed[, c("x", "y")]


   switch (result_format,
      'simple' = return(coord_corrected),
      'complete' = return(coord_sparsed)
   )

}

