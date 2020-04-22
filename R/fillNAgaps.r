# http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
# This is adapted from na.locf() in the zoo library.

fillNAgaps <- function(x, firstBack=FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.
    
    # If it's a factor, store the level labels and convert to integer
    lvls <- NULL
    if (is.factor(x)) {
        lvls <- levels(x)
        x    <- as.integer(x)
    }
 
    goodIdx <- !is.na(x)
 
    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack   
    if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
    else             goodVals <- c(NA,            x[goodIdx])

    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx)+1
    
    x <- goodVals[fillIdx]

    # If it was originally a factor, convert it back
    if (!is.null(lvls)) {
        x <- factor(x, levels=seq_along(lvls), labels=lvls)
    }

    x
}



# Sample data
x <- c(NA,NA, "A","A", "B","B","B", NA,NA, "C", NA,NA,NA, "A","A","B", NA,NA)
x
#>  [1] NA  NA  "A" "A" "B" "B" "B" NA  NA  "C" NA  NA  NA  "A" "A" "B" NA  NA

fillNAgaps(x)
#>  [1] NA  NA  "A" "A" "B" "B" "B" "B" "B" "C" "C" "C" "C" "A" "A" "B" "B" "B"

# Fill the leading NA's with the first good value
fillNAgaps(x, firstBack=TRUE)
#>  [1] "A" "A" "A" "A" "B" "B" "B" "B" "B" "C" "C" "C" "C" "A" "A" "B" "B" "B"

# It also works on factors
y <- factor(x)
y
#>  [1] <NA> <NA> A    A    B    B    B    <NA> <NA> C    <NA> <NA> <NA> A    A    B    <NA>
#> [18] <NA>
#> Levels: A B C

fillNAgaps(y)
#>  [1] <NA> <NA> A    A    B    B    B    B    B    C    C    C    C    A    A    B    B   
#> [18] B   
#> Levels: A B C

