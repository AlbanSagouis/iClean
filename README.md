# iClean
Cleaning heterogeneous coordinates, species names and site codes.

This package relies on regular expressions to modify text strings, whether coordinates, species names or site codes roughly
copy-pasted from papers or gathered fron various data sets.
 - raw latitude and longitude values copy-pasted together.
 - species names with many common misprints, heterogeneous abbreviations (Sp, sp, sp. ; msp, msp., Morph1...),
 abbreviated genus names, etc.
 - site codes generally follow different patterns between studies and an homogenisation may be needed before
 sharing and publishing a data set.
 
The coordinate cleaning function is complementary with `CoordinateCleaner` and `biogeo` packages. It is especially useful when
coordinates are stored in a single column and the delimiter is absent or not consistent.

The species name cleaning function is complementary with `taxize` functions that check whether species names are correct or
deprecated.
