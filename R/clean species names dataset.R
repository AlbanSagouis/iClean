# Preparing a clean data set of species names for training a RNN

# Dataset from OlivierDezerald
tax <- read.csv("C:/Users/as80fywe/idiv/collaborations/Olivier Dezerald/tax.csv", h=T, sep=";", stringsAsFactors=F, encoding = "UTF-8")
library(iClean)

tax$sp.morphosp_old <- tax$sp.morphosp
tax <- unique(data.frame(species_vector=tax$sp.morphosp_old,
   grouping_vector=tax$data.source))


# dataset from Fragsad
fs <- read.csv("C:/Users/as80fywe/idiv/Data paper/analyses/temp/species_table.csv", h=T, sep=";", stringsAsFactors=F, encoding = "UTF-8")
fs <- unique(data.frame(species_vector=fs$species_long,
                        grouping_vector=fs$id_long))

# dataset from Leana's ISAR
is <- read.csv("C:/Users/as80fywe/idiv/ISAR_meta-analysis/temp/abundance_long_format.csv", h=T, sep=",", stringsAsFactors=F, encoding = "UTF-8")
is <- unique(data.frame(species_vector=is$species,
                        grouping_vector=is$refshort))

messy <- rbind(tax, fs, is)

clean <- species_names_correction(species_vector=messy$species_vector,
                                      grouping_vector=messy$grouping_vector,
                                      delimiter=' ',
                                      genus_name_extension=T,
                                      numberHomogenisation=T,
                                      replace_dots=TRUE,
                                      replace_question_marks=TRUE,
                                      erase_descriptor=TRUE,
                                      convert_to_ascii=TRUE)

train <- na.omit(data.frame(dataset = rep(c("Dezerald","fragsad","isar"), times=c(nrow(tax), nrow(fs), nrow(is))),
                    refshort = messy$grouping_vector,
                    messy = messy$species_vector,
                    clean)
)
write.csv(train, "data/species names training dataset.csv")


if(FALSE) {
   tax[tax$grouping_vector %in% c("Vetter_1989","Mouginot_et_al_2014"),]
}
