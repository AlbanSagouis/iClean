# Preparing a clean data set of species names for training a RNN
if(FALSE){

tax <- read.csv("C:/Users/as80fywe/idiv/collaborations/Olivier Dezerald/tax.csv", h=T, sep=";", stringsAsFactors=F, encoding = "UTF-8")
#library(iClean)

tax<- tax[tax$data.source %in% unique(tax$data.source)[1:20],]
tax$sp.morphosp_old <- tax$sp.morphosp

tst <- species_names_correction(species_vector=tax$sp.morphosp_old,
                                grouping_vector=tax$data.source,
                                delimiter='_',
                                genus_name_extension=T,
                                numberHomogenisation=T,
                                replace_dots=TRUE,
                                replace_question_marks=TRUE,
                                erase_descriptor=FALSE,
                                convert_to_ascii=FALSE)



}
