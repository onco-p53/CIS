## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2022)

#============Load all the packages needed================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)
#library(svglite)


#============Load data================

PDD.as.imported.df <- read_csv("PDD-export-28-feb-2025.csv",
                                guess_max = Inf, #assign column types
                                show_col_types = FALSE) |>
  glimpse()


#============Check imported data for issues================

# get duplicates based due to component duplication
# may need correction in CIS if TaxonName_C2 = NA, export as a CSV

PDD.dupes <- PDD.as.imported.df  |>
  get_dupes(AccessionNumber) |>
  select(AccessionNumber, dupe_count, CurrentNamePart_C1, TaxonName_C2, Substrate_C2, PartAffected_C2) %>%
  filter(is.na(TaxonName_C2)) |> #comment this out to get all
  write_csv(file='./outputs/PDD/PDD.dupes.csv')


#============Subset and massage the Data================

PDD.df <- PDD.as.imported.df |>
  distinct(AccessionNumber, .keep_all= TRUE) |> #remove dupes
  glimpse()

#list the Classes in Basidiomycota
PDD.df  |>
  filter(Phylum_C1 == "Basidiomycota R.T. Moore") |>
  group_by(Class_C1) |>
  dplyr::summarize(
    PDDspecimens = n(), 
    .groups = "drop"
  ) |> 
  write_csv(file='./outputs/PDD/PDD-basidio-classes.csv')

PDD.df <- PDD.df |>
  mutate(Class_C1 = str_remove(Class_C1, " .*")) |> 
  mutate(Phylum_C1 = str_remove(Phylum_C1, " .*")) |>
  mutate(specimen_kind = case_when(Phylum_C1 == 'Ascomycota' ~ 'ascomycetes',
                                 Class_C1 == 'Agaricomycetes' ~ 'mushrooms',
                                 Class_C1 == 'Agaricostilbomycetes' ~ 'other',
                                 Class_C1 == 'Atractiellomycetes' ~ 'other',
                                 Class_C1 == 'Basidiomycetes' ~ 'mushrooms',
                                 Class_C1 == 'Classiculomycetes' ~ 'rusts and smuts',
                                 Class_C1 == 'Cystobasidiomycetes' ~ 'rusts and smuts',
                                 Class_C1 == 'Dacrymycetes Doweld' ~ 'mushrooms',
                                 Class_C1 == 'Exobasidiomycetes' ~ 'rusts and smuts',
                                 Class_C1 == 'Microbotryomycetes' ~ 'rusts and smuts',
                                 Class_C1 == 'Pucciniomycetes' ~ 'rusts and smuts',
                                 Class_C1 == 'Tremellomycetes' ~ 'mushrooms',
                                 Class_C1 == 'Ustilaginomycetes' ~ 'rusts and smuts',
                                 TRUE ~ 'other')) |>
  glimpse()


# NOTES on Class_C1ification
# In the Agaricomycetes for instance the grouping of the subClass_C1
# Phallomycetidae is meaningful and useful – as a reminder that
# Ramarias, Earthstars and stinkhorns are related, and an old lineage.

# Mushrooms, brackets, paint-splash, clubs, puffballs, stinkhorns and allies
# = subPhylum_C1 agaricomycotina
# Rusts & smuts = subphyla pucciniomycotina & subPhylum_C1 ustilagomycotina
# it's why subphyla (and subClass_C1es within the agarics) are very useful ranks




#subset New Zealand specimens
PDD.NZ.df <- subset(PDD.df,(StandardCountry_CE1 == "New Zealand"))


#all specimens sorted by date. Add a new column date.collected 
PDD.df %>%
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |> 
  arrange(date.collected) |> 
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected") |> 
  slice_head(n=5)

#New Zealand specimens sorted by date. Add a new column date.collected
PDD.NZ.df %>%
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) %>%
  arrange(date.collected) %>%
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected") %>%
  slice_head(n=30)

  


#============Quick data check================
#have a quick look at the data
head(PDD.df)

#save a summary of the data to txt
PDD.string.factors <- read.csv("PDD-export-18-dec-2024.csv",
                                stringsAsFactors = TRUE) %>%
  summary(maxsum=25) %>%
  capture.output(file='./outputs/PDD/PDD-summary.txt')


#does this work:
output<-capture.output(summary(PDD.string.factors), file=NULL,append=FALSE)
output_df <-as.data.frame(output)

glimpse(output_df)


#Just do these as a big table??

# counts the number of unique values per column
sapply(PDD.df, function(x) length(unique(x)))
up <- sapply(PDD.df, function(x) length(unique(x)))
capture.output(up, file = "PDD-unique-count.txt")

# counts the number of unique values per column for NZ
sapply(PDD.NZ.df, function(x) length(unique(x)))

#============Missing biostatus================

#filters for blank occurrence description
#then de-duplicate
PDD.df |>
  select(CurrentNamePart_C1, StandardCountry_CE1, OccurrenceDescription_C1, BiostatusDescription_C1) |> 
  filter(is.na(OccurrenceDescription_C1)) |> 
  filter(StandardCountry_CE1 == "New Zealand") |> 
  distinct() |> 
  arrange(CurrentNamePart_C1) |> 
  write_csv(file='./outputs/PDD/PDD-missing-occurrence.csv')

#============No identification================

#filters for blank identification
#also finds blank name part

PDD.df |>
  select(AccessionNumber, CurrentNamePart_C1, VerbatimName_C1, TaxonName_C1, StandardCountry_CE1) |> 
  filter(is.na(CurrentNamePart_C1)) |> 
  arrange(CurrentNamePart_C1) |> 
  write_csv(file='./outputs/PDD/PDD-missing-identification.csv')


#============Un-sequenced strains================

#chat gpt version, all species from NZ that don't have a sequence
unsequenced.df <- PDD.df |> 
  filter(StandardCountry_CE1 == "New Zealand") |> 
  distinct(CurrentNamePart_C1, GenBank)

# Identify values that have both TRUE and FALSE  
to_exclude <- unsequenced.df %>%
  group_by(CurrentNamePart_C1) %>%
  filter(n_distinct(GenBank) > 1) %>%
  pull(CurrentNamePart_C1) %>%
  unique()

# Filter out those values
filtered_data <- unsequenced.df |> 
  filter(!CurrentNamePart_C1 %in% to_exclude) |> 
  filter(GenBank == "FALSE") |> 
  write_csv(file='./outputs/PDD/PDD-unsequenced-species.csv') 

#============Bevan's Specimens================

bsw.df <- PDD.df |>
  filter(str_detect(StandardCollector_CE1, "^Weir, BS"))
  
ggplot(bsw.df, aes(Images, fill=GenBank)) +
  labs(title = "Bevan's specimens in PDD") +
  labs(x = "with images", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/PDD/Bevans-with-images.png', width=5, height=5)

#bsws <- summary(bsw, maxsum=25)
#bsws


#============Type Specimens================

#ggplot code for type Specimens
d <- subset(PDD.df,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus)) + labs(title = "Types in the PDD") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_types.png', width=5, height=5)

#another one showing just the number of types in each kind of culture?

#ggplot code for type Specimens factored by Specimen type
d <- subset(PDD.df,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=Images)) + labs(title = "Types in the PDD Fungarium with images") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD.types.with.images.png', width=5, height=5)

#============Specimen kind barcharts================

#plain code for a kingdom barchart

ggplot(PDD.df, aes(specimen_kind)) +
  labs(title = "Specimens in the PDD by 'Specimen kind'") +
  labs(x = "kind", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/PDD/PDD_specimen_kind.png', width=5, height=5)

#kingdoms in GenBank
attach(PDD.df)
require(ggplot2)
p <- ggplot(PDD.df, aes(GenBank)) + labs(title = "Specimens in the PDD in GenBank") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_in_genbank.png', width=5, height=5)

#kingdoms with literature
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Literature)) + labs(title = "Specimens in the PDD in Literature") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_in_Literature.png', width=5, height=5)

#kingdoms with images
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Images)) + labs(title = "Specimens in the PDD with images") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_with_images.png', width=5, height=5)

#could also do a stacked bar chart with images, genbank, literature all on one chart.

#kingdoms by Occurrence Description
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(OccurrenceDescription_C1)) + labs(title = "Specimens in the PDD by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_kingdoms_occurrence.png', width=5, height=5)

#CollectionEventMethod
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(CollectionEventMethod)) + labs(title = "Specimens in the PDD by Collection Event Method") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_CollectionEventMethod.png', width=7, height=7)

#kingdoms by Occurrence Description
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(OccurrenceDescription_C1)) + labs(title = "Specimens in the PDD by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_kingdoms_occurrence2.png', width=7, height=7)

#kingdoms by Order Status
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(LoanStatus)) + labs(title = "PDD Order Status") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_LoanStatus.png', width=7, height=7)

#kingdoms by last updated by
attach(PDD.df) 
require(ggplot2)
p <- ggplot(UpdatedBy) + labs(title = "PDD Last updated by") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_updated_by.png', width=7, height=7)

#need a kingdoms by NZ Specimens??


#============High Taxonomy================



#Phylum_C1
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Phylum_C1)) + labs(title = "PDD by Phylum_C1") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_Phylum_C1.png', width=10, height=10)

#ggplot code for Class_C1
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Class_C1)) + labs(title = "PDD by Class_C1") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_Class_C1.png', width=10, height=10)

#ggplot code for Order
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Order)) + labs(title = "PDD by order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_order.png', width=10, height=10)

#ggplot code for Order
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Order, fill=SpecimenType)) + labs(title = "PDD by order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_order-speciemtype.png', width=10, height=10)

#ggplot code for Family
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Family)) + labs(title = "PDD by bacterial family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
#ggsave(print_bars, file='PDD_family.png', width=20, height=10)


# -----  fungal taxon grouping ----- 

f <- subset(PDD.df, SpecimenType == "Fungal Culture")

#ggplot code for fungal Phylum_C1
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Phylum_C1)) + labs(title = "PDD by fungal Phylum_C1") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_fungal-Phylum_C1.png', width=10, height=10)

#ggplot code for fungal Class_C1
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Class_C1)) + labs(title = "PDD by fungal Class_C1") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_fungal-Class_C1.png', width=10, height=10)

#ggplot code for fungal Order
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Order)) + labs(title = "PDD by fungal order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_fungal-order.png', width=10, height=10)

#ggplot code for fungal Family
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Family)) + labs(title = "PDD by fungal family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_fungal-family.png', width=20, height=10)

#============Other names================

# error Kingdom is missing
names.present.fungi <- subset(PDD.df,(Kingdom == "Fungi" & OccurrenceDescription_C1 == "Present"))
summary(names.present.fungi, maxsum=40)

#ggplot code for fungal Phylum_C1
require(ggplot2)
p <- ggplot(names, aes(names$Phlum)) + labs(title = "names by Phylum_C1") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-Phylum_C1.png', width=10, height=10)



#ggplot code for fungal Phylum_C1
require(ggplot2)
p <- ggplot(names, aes(names$Phlum, fill=OccurrenceDescription_C1)) + labs(title = "names by Phylum_C1") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-Phylum_C1-occurrence.png', width=10, height=10)


#ggplot code for Kingdom
require(ggplot2)
p <- ggplot(names, aes(names$Kingdom, fill=OccurrenceDescription_C1)) + labs(title = "names by Kingdom") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-Kindom-occurrence.png', width=10, height=10)


#ggplot code for Kingdom biostatus
require(ggplot2)
p <- ggplot(names, aes(names$Kingdom, fill=BiostatusDescription_C1)) + labs(title = "names by Kingdom") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-Kindom-biostatus.png', width=10, height=10)


#ggplot code for Fungal Family present in NZ
require(ggplot2)
p <- ggplot(names.present.fungi, aes(names.present.fungi$Family)) + labs(title = "names by family in NZ") + labs(x = "Taxon", y = "number of species")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-Family-occurrence-NZ.png', width=10, height=35)


#============Countries================

sort(table(PDD.df$StandardCountry_CE1),decreasing=TRUE)[1:13] #top 11 countries

positions <- c("New Zealand", "United States", "Australia", "Germany", "United Kingdom",
               "Canada", "Austria", "Fiji", "Cook Islands",  "Solomon Islands")

PDD.10county <- subset(PDD.df, (StandardCountry_CE1 == "New Zealand" | StandardCountry_CE1 == "United States" |
                                  StandardCountry_CE1 == "Australia" | StandardCountry_CE1 == "Germany" |
                                  StandardCountry_CE1 == "United Kingdom" | StandardCountry_CE1 == "Canada" |
                                  StandardCountry_CE1 == "Austria" | StandardCountry_CE1 == "Fiji" |
                                  StandardCountry_CE1 == "Cook Islands" | StandardCountry_CE1 == "Solomon Islands"))


PDD.NZ.df <- subset(PDD.df,(StandardCountry_CE1 == "New Zealand"))
attach(PDD.NZ.df) 
require(ggplot2)
p <- ggplot(PDD.NZ.df, aes(NZAreaCode)) + labs(title = "NZ Specimens in the PDD by Area Code") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_NZAreaCode.png', width=7, height=7)




#ggplot code for StandardCountry_CE1
cy <- subset(PDD.df,!(StandardCountry_CE1 == ""))
require(ggplot2)
con <- ggplot(cy, aes(StandardCountry_CE1)) + labs(title = "Top 10 Countries") + labs(x = "StandardCountry_CE1", y = "number")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_StandardCountry_CE1.png', width=10, height=10)


#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "Italy", "France")
c <- subset(PDD.df, (StandardCountry_CE1 == "New Zealand" | StandardCountry_CE1 == "United States" | StandardCountry_CE1 == "Australia" | StandardCountry_CE1 == "United Kingdom" | StandardCountry_CE1 == "Brazil" | StandardCountry_CE1 == "Japan" | StandardCountry_CE1 == "India" | StandardCountry_CE1 == "France" | StandardCountry_CE1 == "China" | StandardCountry_CE1 == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(StandardCountry_CE1, fill=SpecimenType)) + labs(title = "Top 10 Countries in the PDD") + labs(x = "StandardCountry_CE1", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='PDD_StandardCountry_CE1_by_kind.png', width=6, height=5)
ggsave(print_bars, file='PDD_StandardCountry_CE1_by_kind.svg', width=6, height=5)
ggsave(print_bars, file='PDD_StandardCountry_CE1_by_kind.eps', width=6, height=5)

#ggplot code for pacific StandardCountry_CE1
c <- subset(PDD.df, (StandardCountry_CE1 == "Fiji" | StandardCountry_CE1 == "American Samoa" | StandardCountry_CE1 == "Cook Islands" | StandardCountry_CE1 == "Solomon Islands" | StandardCountry_CE1 == "Micronesia" | StandardCountry_CE1 == "New Caledonia" | StandardCountry_CE1 == "Niue" | StandardCountry_CE1 == "Norfolk Island" | StandardCountry_CE1 == "Samoa" | StandardCountry_CE1 == "Vanuatu"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(StandardCountry_CE1, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the PDD") + labs(x = "StandardCountry_CE1", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD-pacific-countries.png', width=10, height=10)


#ggplot code for StandardCountry_CE1
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "France", "Italy", "Canada")
c <- subset(PDD.df, (StandardCountry_CE1 == "Canada" | StandardCountry_CE1 == "United States" | StandardCountry_CE1 == "Australia" | StandardCountry_CE1 == "United Kingdom" | StandardCountry_CE1 == "Brazil" | StandardCountry_CE1 == "Japan" | StandardCountry_CE1 == "India" | StandardCountry_CE1 == "France" | StandardCountry_CE1 == "China" | StandardCountry_CE1 == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(StandardCountry_CE1, fill=SpecimenType)) + labs(title = "Top 10 Countries in the PDD (not including NZ)") + labs(x = "StandardCountry_CE1", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='PDD_StandardCountry_CE1_by_kind_not_nz.png', width=10, height=10)


## could make a pseudo dataset manually replaceing all non target countries with "other"


#============timeline================

#all cultures sorted by date. Add a new column date.collected 
PDD.df$date.collected <- ymd(PDD.df$CollectionDateFromISO_CE1, truncated = 3)
arrange(PDD.df, date.collected) %>%
  select("AccessionNumber","SpecimenType", "StandardCountry_CE1", "date.collected") %>%
  slice_head(n=10)

#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?


#first collections by person

#Cunningham
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
#  filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Cunningham")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/PDD-Cunningham-first.csv')

#Dingley
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Dingley")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/PDD-Dingley-first.csv')

#Weir
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Weir, BS")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/PDD-Weir-first.csv')

#Johnston
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Johnston, PR")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Johnston-first.csv')

#McKenzie
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^McKenzie, EHC")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/McKenzie-first.csv')

#Cooper
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Cooper, JA")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Cooper-first.csv')

#Beever
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Beever")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Beever-first.csv')

#Austwick
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Austwick")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Austwick-first.csv')

#McNabb
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^McNabb")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/McNabb-first.csv')

#Buchanan
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Buchanan")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Buchanan-first.csv')

#Horak
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Horak")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Horak-first.csv')

#Cockayne
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Cockayne")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/Cockayne-first.csv')

#Padamsee
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #  filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Padamsee")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/PDD-Padamsee-first.csv')

#Ridley
PDD.df |>
  mutate(date.collected = ymd(CollectionDateFromISO_CE1, truncated = 3)) |>
  arrange(date.collected) |>
  select("AccessionNumber","specimen_kind", "CurrentNamePart_C1", "StandardCountry_CE1", "date.collected", "StandardCollector_CE1") |>
  #  filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(str_detect(StandardCollector_CE1, "^Ridley")) |>
  slice_head(n=15) |> 
  write_csv(file='./outputs/PDD/PDD-Ridley-first.csv')

#Phylum_C1 to too many lets subset it:
library(tidyverse)
PDD.groups <- PDD.df %>%
  filter(Phylum_C1 == "Basidiomycota" | 
           Phylum_C1 == "Ascomycota" | 
           Phylum_C1 == "Amoebozoa" | 
           Phylum_C1 == "Oomycota") %>%
  filter(Deaccessioned == "false")


#Collection dates

date.collected <-ymd(PDD.df$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.df, aes(date.collected, fill = specimen_kind)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  facet_grid(factor(specimen_kind, levels=c('ascomycetes','mushrooms','rusts and smuts','other')) ~ . , scales = "free")
ggsave(file='./outputs/PDD/PDD-collection-dates-facet.png', width=8, height=5)



#Only basidios:
library(tidyverse)
PDD.basidio <- PDD.df %>%
  filter(Phylum_C1 == "Basidiomycota") %>%
  filter(Deaccessioned == "false")

head(PDD.basidio)


#ICMP isolation dates faceted
attach(PDD.basidio) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.basidio$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.basidio, aes(date.collected, fill = Class_C1)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
#  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") #+
#  facet_grid(Class_C1 ~ .)
ggsave(file='PDD-collection-dates-facet-basidio-Class_C1.png', width=8, height=5)


#ggplot code for Class_C1
attach(PDD.basidio) 
require(ggplot2)
p <- ggplot(PDD.basidio, aes(Class_C1)) + labs(title = "PDD Basidiomyces by Class_C1") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_Class_C1_basidio.png', width=10, height=10)


#Only ascos:
library(tidyverse)
PDD.asco <- PDD.df %>%
  filter(Phylum_C1 == "Ascomycota") %>%
  filter(Deaccessioned == "false")

head(PDD.asco)


#ICMP isolation dates faceted
attach(PDD.asco) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.asco$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.asco, aes(date.collected, fill = Class_C1)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Paired") +
  geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") #+
#  facet_grid(Class_C1 ~ .)
ggsave(file='PDD-collection-dates-facet-asco-Class_C1.png', width=8, height=5)


#Just discos:
library(tidyverse)
PDD.disco <- PDD.df %>%
  filter(Class_C1 == "Leotiomycetes") %>%
  filter(Deaccessioned == "false")

head(PDD.disco)


#ICMP isolation dates faceted
attach(PDD.disco) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.disco$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.disco, aes(date.collected, fill = Order)) +
  labs(title = "Collection dates of PDD Leotiomycetes specimens") +
  labs(x = "Date of collection", y =  "Number of specimens") +
  theme(legend.position = c(0.1, 0.7)) +
  scale_fill_brewer(palette = "Paired") +
  geom_histogram(binwidth=1826.25, show.legend = TRUE) + # 5: 1826.25
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") #+
  #facet_grid(Order ~ .)
ggsave(file='PDD-collection-dates-facet-disco-order.png', width=8, height=5)








help(as.Date)
help(ISOdatetime)

attach(PDD.df) 
require(ggplot2)
di <- ggplot(PDD.df, aes(as.Date(CollectionDateFromISO_CE1, format='%Y-%m-%d'))) + labs(title = "Collection dates of PDD specimens") + labs(x = "Date of collection", y =  "Number of specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-collection-dates.png', width=5, height=5)


PDD.df$topcontrib <- ifelse(PDD.df$StandardCollector_CE1 == "Dingley, JM", "Dingley, JM", "other")
#there must be a better way to do this
PDD.df$topcontrib

attach(PDD.df) 
require(ggplot2)
dr <- ggplot(PDD.df, aes(as.Date(CollectionDateFromISO_CE1, format='%Y-%m-%d'),fill=topcontrib)) + labs(title = "Main Contributors to the PDD collection") + labs(x = "Date of Collection", y =  "Number of Specimens" , fill = "")
dr + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
drp <- di + geom_histogram(binwidth=365.25)
ggsave(drp, file='PDD-collection-dates-collector.png', width=15, height=10)


#============Over months================

PDD.Wellington.df <- subset(PDD.NZ.df,(NZAreaCode == "Wellington"))
glimpse(PDD.Wellington.df)

#new month PDD specimen collected (in NZ)
month.collected <- ymd(PDD.Wellington.df$CollectionDateFromISO_CE1, truncated = 1)
mergemonths <- floor_date(month.collected, unit = "month")
ggplot(PDD.Wellington.df, aes(month(mergemonths, label = TRUE), fill = specimen_kind)) +
  labs(title = "Collection month of PDD specimens from Wellington") + 
  labs(x = "Month of Collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  geom_bar() + 
  scale_x_discrete(na.translate = FALSE)  # this removes NAs
ggsave(file='./outputs/PDD/PDD-Wellington-month.png', width=8, height=5)


#Collection week NZ PDD specimen collected (in NZ)
week.collected <- ymd(PDD.NZ.df$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.NZ.df, aes(isoweek(week.collected), fill = specimen_kind)) +
  labs(title = "Collection week of New Zealand PDD specimens") + 
  labs(x = "Week of Collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  geom_bar()
ggsave(file='./outputs/PDD/PDD-week.png', width=8, height=5)

#Collection week NZ PDD specimen collected (in NZ)
week.collected <- ymd(PDD.NZ.df$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.NZ.df, aes(isoweek(week.collected), fill = specimen_kind)) +
  labs(title = "Collection week of New Zealand PDD specimens") + 
  labs(x = "Week of Collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  geom_bar() +
  facet_wrap(vars(NZAreaCode), scales = "free")
ggsave(file='./outputs/PDD/PDD-week-crosby.png', width=8, height=8)


#Collection month of NZ PDD specimens
attach(PDD.NZ.df) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.NZ.df$CollectionDateFromISO_CE1, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.NZ.df, aes(month(mergemonths, label = TRUE), fill = Phylum_C1)) + labs(title = "Collection month of PDD specimens from NZ") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
ggsave(dip, file='PDD-collection-month-dates.png', width=8, height=5)


library(tidyverse)
date.collected.nz <-ymd(PDD.NZ.df$CollectionDateFromISO_CE1)
arrange(PDD.NZ.df, date.collected.nz) #this is the ealiest isolated NZ specimen



#awheto specimens
PDD.awheto <- subset(PDD.df,(CurrentNamePart_C1 == "Ophiocordyceps robertsii" | CurrentNamePart_C1 == "Cordyceps hauturu"))
summary(PDD.awheto, maxsum=40)
attach(PDD.awheto) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.awheto$CollectionDateFromISO_CE1, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.awheto, aes(month(mergemonths, label = TRUE), fill = CurrentNamePart_C1)) + labs(title = "Collection month of PDD specimens of Awheto") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
ggsave(dip, file='PDD-awheto-dates.png', width=8, height=5)

PDD.awheto <- subset(PDD.df,(CurrentNamePart_C1 == "Ophiocordyceps robertsii" | CurrentNamePart_C1 == "Cordyceps hauturu"))
summary(PDD.awheto, maxsum=40)
attach(PDD.awheto) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.awheto$CollectionDateFromISO_CE1, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
di <- ggplot(PDD.awheto, aes(month(mergemonths, label = TRUE), fill = TaxonName_C2)) + labs(title = "Collection month of PDD specimens of Awheto with host") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE)
ggsave(dip, file='PDD-awheto-dates-host.png', width=8, height=5)

library(tidyverse)
drop_na(PDD.awheto$CollectionDateFromISO_CE1s)

PDD.awheto$CollectionDateFromISO_CE1s %>% drop_na()


#subset New Zealand specimens
PDD.awheto <- subset(PDD.df,(Family == "Glomerellaceae"))
summary(PDD.Glomerellaceae, maxsum=40)
attach(PDD.Glomerellaceae) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.awheto$CollectionDateFromISO_CE1, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.Glomerellaceae, aes(month(mergemonths, label = TRUE))) + labs(title = "Collection month of PDD specimens of Awheto") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar()
dip <- di + geom_bar()
ggsave(dip, file='Glomerellaceae-season.png', width=8, height=5)




date.collected

CollectionDateFromISO_CE1

mergemonths <- round_date(date.collected, unit = "month")
mergemonths


#============Accessions per year================

date.collected <-ymd(PDD.df$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.df, aes(date.collected, fill = specimen_kind)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 10) +
  geom_hline(yintercept=677.8, linetype=2) + 
  annotate("text", x = as.Date("1850-01-01"), y = 720, 
           label = "Last 5 Year average collected", color = "black", hjust = 0) +
  geom_hline(yintercept=1054.6, linetype=2) + 
  annotate("text", x = as.Date("1850-01-01"), y = 1100, 
           label = "Last 10 Year average collected", color = "black", hjust = 0) +
  geom_hline(yintercept=2167, linetype=2, color = "red") + 
  annotate("text", x = as.Date("1850-01-01"), y = 2210, 
           label = "Last 5 Year average accessioned", color = "red", hjust = 0) +
  geom_hline(yintercept=1778.3, linetype=2, color = "red") + 
  annotate("text", x = as.Date("1850-01-01"), y = 1820, 
           label = "Last 10 Year average accessioned", color = "red", hjust = 0)
ggsave(file='./outputs/PDD/PDD-collection-dates.png', width=8, height=5)


#generate date deposited column
date.collected <-ymd(PDD.df$CollectionDateFromISO_CE1, truncated = 3)

# Extract the year from the date using lubridate's year() function
PDD.df <- PDD.df %>% mutate(year.collected = year(date.collected))

# Exclude any rows from 2025 (partial year)
complete_data <- PDD.df %>% filter(year.collected < 2025)

# Count the number of rows per year
yearly_counts <- complete_data %>%
  group_by(year.collected) %>%
  summarize(count = n()) %>%
  arrange(year.collected)

# Calculate the overall average count per year (all complete years)
avg_all <- mean(yearly_counts$count)

# Identify the most recent complete year
last_year <- max(yearly_counts$year.collected)

# Calculate the average count for the past 5 complete years
last_5_years <- yearly_counts %>% filter(year.collected >= (last_year - 4))
avg_last5 <- mean(last_5_years$count)

# Calculate the average count for the past 10 complete years
last_10_years <- yearly_counts %>% filter(year.collected >= (last_year - 9))
avg_last10 <- mean(last_10_years$count)

# Output the results
print(yearly_counts)
cat("Average per year (all complete years):", avg_all, "\n")
cat("Average per year (last 5 years):", avg_last5, "\n")
cat("Average per year (last 10 years):", avg_last10, "\n")


## Using the date of data entry ================

date.databased <-dmy_hms(PDD.df$CreatedDate, truncated = 3) #i think the prob is we have to strip off the times
ggplot(PDD.df, aes(date.databased, fill = specimen_kind)) +
  labs(title = "Database dates of PDD specimens") +
  labs(x = "Date of database", y =  "Number of specimens" , fill = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 10) +
  geom_hline(yintercept=677.8, linetype=2) + 
  annotate("text", x = as.Date("1850-01-01"), y = 720, 
           label = "Last 5 Year Average collected", color = "black", hjust = 0) +
  geom_hline(yintercept=1054.6, linetype=2) + 
  annotate("text", x = as.Date("2020-01-01"), y = 1100, 
           label = "Last 10 Year Average collected", color = "black", hjust = 0) +
  geom_hline(yintercept=2167, linetype=2) + 
  annotate("text", x = as.Date("2020-01-01"), y = 2167, 
           label = "Last 5 Year Average accessioned", color = "red", hjust = 0) +
  geom_hline(yintercept=1778.3, linetype=2) + 
  annotate("text", x = as.Date("2020-01-01"), y = 1778, 
           label = "Last 10 Year Average accessioned", color = "red", hjust = 0) +
  ggsave(file='./outputs/PDD/PDD-database-dates.png', width=8, height=5)

#generate date deposited column
date.databased <-dmy_hms(PDD.df$CreatedDate, truncated = 3)

# Extract the year from the date using lubridate's year() function
PDD.df <- PDD.df %>% mutate(year.databased = year(date.databased))

# Exclude any rows from 2025 (partial year)
complete_data <- PDD.df %>% filter(year.databased < 2025)

# Count the number of rows per year
yearly_counts <- complete_data %>%
  group_by(year.databased) %>%
  summarize(count = n()) %>%
  arrange(year.databased)

# Calculate the overall average count per year (all complete years)
avg_all <- mean(yearly_counts$count)

# Identify the most recent complete year
last_year <- max(yearly_counts$year.databased)

# Calculate the average count for the past 5 complete years
last_5_years <- yearly_counts %>% filter(year.databased >= (last_year - 4))
avg_last5 <- mean(last_5_years$count)

# Calculate the average count for the past 10 complete years
last_10_years <- yearly_counts %>% filter(year.databased >= (last_year - 9))
avg_last10 <- mean(last_10_years$count)

# Output the results
print(yearly_counts)
cat("Average per year (all complete years):", avg_all, "\n")
cat("Average per year (last 5 years):", avg_last5, "\n")
cat("Average per year (last 10 years):", avg_last10, "\n")



#---------------------------------------------------






attach(PDD.df) #this means we don't need the $ sign
require(ggplot2)
di <- ggplot(PDD.df, aes(as.Date(IsolationDateISO))) + labs(title = "Isolation dates of PDD Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-isolation-dates2.png', width=4, height=3)




PDD.df$topcontrib <- ifelse(PDD.df$Contributor == "NZP", "NZP", "other")
PDD.df$topcontrib

attach(PDD.df) #this means we don't need the $ sign
require(ggplot2)
ditc <- ggplot(PDD.df, aes(as.Date(IsolationDateISO, fill=Contributor))) + labs(title = "Isolation dates of PDD Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
ditc <- ditc + scale_x_date()
ditc + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
ditcp <- ditc + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-isolation-dates2.png', width=4, height=3)




attach(PDD.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(PDD.df, aes(as.Date(ReceivedDateISO))) + labs(title = "Received dates of PDD Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='PDD-received-dates.png', width=10, height=10)

## CAN we do this by organism too?

attach(PDD.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(PDD.df, aes(as.Date(ReceivedDateISO),fill=SpecimenType)) + labs(title = "Received dates of PDD Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='PDD-received-dates-organism.png', width=15, height=10)


sum2 <- ggplot_build(drp) #this extracts the values from the histogram
sum2


attach(PDD.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(PDD.df, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the PDD collection") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='PDD-received-dates-contributor.png', width=15, height=10)


PDD.df$topcontrib <- ifelse(PDD.df$Contributor == "NZP", "NZP", "other")

PDD.df$topcontrib

#ggplot code for collections over the years
attach(PDD.df) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(StandardCountry_CE1, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the PDD") + labs(x = "StandardCountry_CE1", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_histogram()+ coord_flip()
print_bars <- con + geom_histogram()+ coord_flip()
ggsave(print_bars, file='PDD-pacific-countries.png', width=10, height=10)

#ggplot code for collections over the years in NZ
c <- subset(PDD.df, (StandardCountry_CE1 == "New Zealand"))

#also need something that plots monthly e.g. fungi versus collection month.


#======GeoGraphic stuff========

#New Zealand Area codes This is good
nz <- subset(PDD.df,(StandardCountry_CE1 == "New Zealand"))
positions <- c("New Zealand", "Campbell Island", "Auckland Islands", "Snares Islands", "Chatham Islands",  "Stewart Island", "Southland", "Fiordland", "Dunedin", "Central Otago", "Otago Lakes", "South Canterbury", "Mackenzie", "Westland", "Mid Canterbury", "North Canterbury", "Buller", "Kaikoura", "Marlborough", "Nelson", "Marlborough Sounds", "South Island", "Wairarapa", "Wellington", "Hawkes Bay", "Rangitikei", "Wanganui", "Gisborne", "Taupo", "Taranaki", "Bay of Plenty", "Waikato", "Coromandel", "Auckland", "Northland", "North Island", "Three Kings Islands", "Kermadec Islands")
attach(nz) 
require(ggplot2)
p <- ggplot(nz, aes(NZAreaCode)) + labs(title = "PDD specimens by NZ region") + labs(x = "Crosby Region", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- p + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='PDD_NZAreaCode.png', width=5, height=5)



con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)






#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp

#---leaflet html map -----------------------------------------------------------

library(leaflet)

#factpal <- colorFactor(topo.colors(5), magic.df$CurrentNamePart_C1)

factpal <- colorFactor(palette = "Paired", domain = PDD.df$specimen_kind)

#Using leaflet
PDD.NZ.leaf <- leaflet(PDD.NZ.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~DecimalLong, 
                   lat = ~DecimalLat, 
                   popup = ~AccessionNumber,
                   label = ~CurrentNamePart_C1,
                   color = ~factpal(specimen_kind))

#Opening up viewer
PDD.NZ.leaf








#======Habitat========

#too many to work
nz <- subset(PDD.df,(StandardCountry_CE1 == "New Zealand"))
attach(nz) 
require(ggplot2)
p <- ggplot(nz, aes(Habitat)) + labs(title = "PDD specimens by Habitat") + labs(x = "Habitat", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_Habitat.png', width=15, height=49)



#======On Hosts========

sort(table(PDD.df$Family_C2),decreasing=TRUE)[1:11] #top 11 families

#barchart of to 10 host families sorted by 'kind' of type, coloured by kind of organism
positions.10hosts <- c("Nothofagaceae", "Gramineae", "Myrtaceae", "Compositae", "Leguminosae", "Pinaceae", "Rosaceae", "Cyperaceae", "Podocarpaceae",  "Arecaceae")
PDD.10hosts <- subset(PDD.df, (Family_C2 == "Nothofagaceae" | Family_C2 == "Gramineae" | Family_C2 == "Compositae" | Family_C2 == "Leguminosae" | Family_C2 == "Pinaceae" | Family_C2 == "Rosaceae" | Family_C2 == "Cyperaceae" | Family_C2 == "Compositae" | Family_C2 == "Podocarpaceae" | Family_C2 == "Arecaceae"))


#Order of 'microbe' on NZ Myrtaceae in the ICMPt
PDD.df.Myrtaceae <- subset(PDD.NZ.df,(Family_C2 == "Myrtaceae"))
ggplot(PDD.df.Myrtaceae, aes(Order, fill=GenBank)) + #fill by type
  labs(title = "Order of 'microbe' on NZ Myrtaceae in the PDD Fungarium") +
  labs(x = "Order", y = "number of isolates") +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD_Myrtaceae-order.png', width=8, height=15)




#grass pathogens collected over time in PDD

PDD.Gramineae <- PDD.NZ.df %>%
  filter(Family_C2 == "Gramineae")

head(PDD.Gramineae)

PDD.Gramineae$date.collected <-ymd(PDD.Gramineae$CollectionDateFromISO_CE1, truncated = 3)
ggplot(PDD.Gramineae, aes(date.collected, fill = "Family")) +
  labs(title = "Collection of grass pathogens in the PDD fungarium over time") +
  labs(x = "Date of Collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = c(0.1, 0.8)) +
  geom_histogram(binwidth=1461, show.legend = FALSE) + # 2y is 730, 4y = 
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")
ggsave(file='./outputs/PDD/PDD-grass-pathogens.png', width=8, height=5)


head(PDD.Gramineae)





#check double numbers

PDD.df.nohost <- subset(PDD.df,(Family_C2 == ""))
summary(PDD.df.nohost$AccessionNumber, maxsum=10)


#======Maps of rusts========

library(sf) #Simple feature
library(ggspatial) #make maps fancy

# Loading in data 
#Reading in a NZ specific map
nz.sf <- st_read(dsn = "./data/nz-coastlines-topo-150k/nz-coastlines-topo-150k.shp", quiet = FALSE) %>%
  st_transform(2193) #Setting map projection - NZGD2000

#Transforming to an SF object
rusts.sf <- PDD.df %>%
  filter(str_detect(CurrentNamePart_C1, "^Puccinia oxalidis")) %>%
  filter(OccurrenceDescription_C1 == "Present") %>%
  filter(!is.na(DecimalLat)) %>% #Removing missing obs as sf doesn't play with these
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) %>% #Defining what the coord columns are
  st_set_crs(4326) %>% #Telling sf it is in WSG84 projection
  st_transform(2193) %>% #Changing it to NZGD2000 to match coastline polygon
  st_crop(st_bbox(nz.sf)) #Cropping out points that are outside the coastline polygons bounding box (e.g. not NZ)

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = rusts.sf, aes(colour = CurrentNamePart_C1),
          size = 2, alpha = 0.8, show.legend = TRUE) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  labs(title = "Collection location of Puccinia oxalidis",
       caption = "data from PDD")

#this is too slow so need to build in a delay
ggsave(file='./outputs/PDD/Puccinia-oxalidis-map.pdf', width=8, height=10)


#======Validations========

#plot higher order taxon against storage location
#Storage location main factor, colour by taxon

#ggplot code for fungal Phylum_C1
ggplot(PDD.df, aes(FilingNumber, fill=Phylum_C1)) +
  labs(title = "PDD Storage locations") +
  labs(x = "storage location", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD-storagelocation.png', width=10, height=10)

#ggplot code for fungal Phylum_C1
require(ggplot2)
ggplot(PDD.df, aes(Phylum_C1, fill=FilingNumber)) +
  labs(title = "PDD Storage locations") +
  labs(x = "storage location", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD-storagelocation2.png', width=10, height=10)


#============Over months================

PDD.colletot <- PDD.df %>% 
  filter (CurrentNamePart_C1 == "Colletotrichum gloeosporioides (Penz.) Penz. & Sacc.")

PDD.colletot

unique(PDD.colletot$FilingNumber)
unique(PDD.colletot$TaxonName)


# leaflet map for checking geo-cords --------------------------------------

library(leaflet)

#Transforming to an SF object
Psilocybe.both.leaf.df <- PDD.df |> 
  filter(str_detect(CurrentNamePart_C1, "^Psilocybe")) |> 
  filter(StandardCountry_CE1 == "New Zealand") |> 
  filter(OccurrenceDescription_C1 == "Present")



factpal <- colorFactor(palette = "Paired", domain = Psilocybe.both.leaf.df$CurrentNamePart_C1)

#To show
head(Psilocybe.both.leaf.df)

#Using leaflet
magic.leaf <- leaflet(Psilocybe.both.leaf.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~DecimalLong, 
                   lat = ~DecimalLat, 
                   popup = ~AccessionNumber,
                   label = ~CurrentNamePart_C1,
                   color = ~factpal(CurrentNamePart_C1))

#Opening up viewer
magic.leaf

#use save as a webpage function in Rstudio to save

psilocybe-html-map.html








