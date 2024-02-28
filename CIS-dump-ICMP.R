## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2022)

#============Load all the packages needed================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
#library(svglite)
library(skimr)
library(janitor)
library(sf) #Simple feature
library(ggspatial) #make maps fancy

#versions
R.version.string

#============Load data================

#loaded as a tibble
ICMP.as.imported.df <- read_csv("ICMP-export-28-feb-2024.csv", #also line 94
                                guess_max = Inf,
                                show_col_types = FALSE)

#============Subset and massage the Data================


#tidyverse way of sub setting - remove viruses and deaccessioned cultures
ICMP.df <- ICMP.as.imported.df %>%
  distinct(AccessionNumber, .keep_all= TRUE) %>% #remove dupes
  filter(SpecimenType == "Bacterial Culture" | 
           SpecimenType == "Chromist Culture" | 
           SpecimenType == "Fungal Culture" | 
           SpecimenType == "Yeast Culture") %>%
  filter(Deaccessioned == "FALSE") %>%
  glimpse()

ICMP.NZ.df <- ICMP.df %>%
  filter(Country == "New Zealand")

ICMP.types <- ICMP.df %>%
  filter(TypeStatus != "")


#============Check imported data for issues================


# get duplicates based due to component duplication
# may need correction in CIS if TaxonName_C2 = NA, export as a CSV:
ICMP.dupes <- ICMP.as.imported.df %>%
  get_dupes(AccessionNumber) %>%
  select(AccessionNumber, dupe_count, CurrentName, TaxonName_C2, Substrate_C2, PartAffected_C2) %>%
  filter(is.na(TaxonName_C2)) %>% #comment this out to get all
  write_csv(file='./outputs/ICMP/ICMP.dupes.csv')


#setting up per specimen type subsets, with summaries of each specimen type
ICMP.bacteria <- ICMP.df %>%
  filter(SpecimenType == "Bacterial Culture")
table(ICMP.bacteria$Phylum) #this is a validation check for misclassified


ICMP.chromist <- ICMP.df %>%
  filter(SpecimenType == "Chromist Culture")
table(ICMP.chromist$Phylum) #this is a validation check for misclassified


ICMP.fungi <- ICMP.df %>%
  filter(SpecimenType == "Fungal Culture")
table(ICMP.fungi$Phylum) #this is a validation check for misclassified

ICMP.yeast <- ICMP.df %>%
  filter(SpecimenType == "Yeast Culture")
table(ICMP.yeast$Phylum) #this is a validation check for misclassified

#change this code below to find errors then fix in CIS
ICMP.fungi %>%
  select("AccessionNumber", "SpecimenType", "CurrentName", "Phylum" ) %>%
  filter(Phylum == "Oomycota" | Phylum == "Amoebozoa")

ICMP.chromist %>%
  select("AccessionNumber", "SpecimenType", "CurrentName", "Phylum" ) %>%
  filter(Phylum == "Ascomycota" | Phylum == "Basidiomycota")



  

#============Data quality checks================


#saves the head of the dataset (to GitHub) to understand data structure
ICMP.as.imported.df %>%
  filter(SpecimenSecurityLevelText == "Public") %>% #public only!
  head() %>%
  write_csv(file='./outputs/ICMP/ICMP-head.csv')

#save a summary of the data to txt
ICMP.string.factors <- read.csv("ICMP-export-29-apr-2023.csv",
                                stringsAsFactors = TRUE) %>%
  summary(maxsum=25) %>%
  capture.output(file='./outputs/ICMP/ICMP-summary.txt')

#check specimen count before and after filtering
count(ICMP.as.imported.df,SpecimenType)
count(ICMP.df,SpecimenType)

#check security level
count(ICMP.df,SpecimenSecurityLevelText)

#skim the data for numerics on each column
ICMP.df %>% 
  skim()

# counts the number of unique values per column
sapply(ICMP.df, function(x) length(unique(x))) %>%
  capture.output(file = "./outputs/ICMP/ICMP-unique-count.txt")

# counts the number of unique values per column for NZ
sapply(ICMP.NZ.df, function(x) length(unique(x)))




#============Colours and notes================


# notes here: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
#display.brewer.all(colorblindFriendly = TRUE)
#display.brewer.all(colorblindFriendly = FALSE)
# OK ones are Paired if you have heaps of data. Others are: Set2


#============Missing biostatus================

#filters for blank occurrence description
#then de-duplicate
ICMP.df |>
  select(CurrentName, Country, OccurrenceDescription, BiostatusDescription) |> 
  filter(is.na(OccurrenceDescription)) |> 
  distinct() |> 
  arrange(CurrentName) |> 
  write_csv(file='./outputs/ICMP/ICMP-missing-occurrence.csv')


#============Unseqeunced strains================

ICMP.df %>%
  filter(Country == "New Zealand") %>%
  filter(GenBank == "FALSE") %>%
  
  
# de-duplicate on CurrentName where TRUE overrides false

  write_csv(file='./outputs/ICMP/ICMP-unseqeunced-species.csv') 


#============General stats================

#Table of number of cultures and types

total.table <- ICMP.df %>% # this one gets a total count
  group_by(SpecimenType) %>%
  dplyr::summarize(
    All.ICMP = n(), 
    .groups = "drop"
  )

types.table <- ICMP.df %>% # this one gets a types count
  filter(TypeStatus != "") %>%
  group_by(SpecimenType) %>%
  dplyr::summarize(
    Types = n(), 
    .groups = "drop"
  )

NZ.table <- ICMP.df %>% # this one gets a New Zealand count
  filter(Country == "New Zealand") %>%
  group_by(SpecimenType) %>%
  dplyr::summarize(
    NZ = n(), 
    .groups = "drop"
  )

NZtype.table <- ICMP.df %>% # NZ and types
  filter(Country == "New Zealand" & TypeStatus != "") %>%
  group_by(SpecimenType) %>%
  dplyr::summarize(
    NZ.Types = n(), 
    .groups = "drop"
  )

icmp.count.table <- left_join(total.table, types.table) %>%
  left_join(NZ.table) %>%
  left_join(NZtype.table) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total"))) %>%
write_csv(file='./outputs/ICMP/ICMP-count-table.csv') 


#This lists the number of different values for each column
ICMP.df %>%
  map_int(n_distinct)

#how to use this to say how many countries?
ICMP.df %>%
  select("AccessionNumber", "Country") %>%
  map_int(n_distinct)

#create a Genus column
#split out genus by matching everything up to the first space:
str_view(ICMP.df$CurrentName, "^[a-zA-Z-]*")

#not perfect but ¯\_(ツ)_/¯ now add it as a mutate:
ICMP.df %>%
  mutate(Genus = str_extract(CurrentName, "^[a-zA-Z-]*")) %>%
  glimpse()


#============Type cultures================

#sorting plots:
#  https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html

#barchart of all ICMP types sorted by 'kind' of type
ggplot(ICMP.types, aes(TypeStatus)) +
  labs(title = "Types in the ICMP") +
  labs(x = "'Kind' of type", y = "number of isolates") +
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_types.png', width=10, height=10)

## KEY CHART ##
#barchart of all ICMP types sorted by 'kind' of type, coloured by kind of organism
positions <- c("Type strain", "Pathotype strain", "Neopathotype strain", "Type", "ex Type",  "Holotype", "ex Holotype", "Paratype", "ex Paratype", "Isotype", "Neotype", "ex Neotype", "Epitype", "ex Epitype", "Syntype", "Reference strain")
ggplot(ICMP.types, aes(TypeStatus, fill=SpecimenType)) +
  labs(title = "Types in the ICMP culture collection") +
  labs(x = "'Kind' of type", y = "number of cultures") +
  theme_bw()+
  geom_bar() + 
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(limits = positions)
ggsave(file='./outputs/ICMP/ICMP.types.by.kind.png', width=10, height=10)

#barchart of all ICMP types sorted by 'kind' of type, with genbank status
ggplot(ICMP.types, aes(SpecimenType, fill=GenBank)) +
  labs(title = "Types in the ICMP with a GenBank seqeunce") +
  labs(x = "'Kind' of type", y = "number of isolates") +
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_types-by-SpecimenType.png', width=10, height=10)

#Types in the ICMP with images
ggplot(ICMP.types, aes(TypeStatus, fill=Images)) +
  labs(title = "Types in the ICMP with images") +
  labs(x = "'Kind' of type", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP.types.with.images.png', width=10, height=10)

#Types in ICMP with sequences in GenBank
ggplot(ICMP.types, aes(TypeStatus, fill=GenBank)) +
  labs(title = "Types in ICMP with sequences in GenBank") +
  labs(x = "'Kind' of type", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP.types.with.sequence.png', width=10, height=10)


#============PIE CHART!================


#this is borked code, but if sorted correctly could be a barcode map of deposits over time
#sort would have to strip ICMP number
#sort(table(ICMP.df$AccessionNumber),decreasing=TRUE)
#ggplot(ICMP.df, aes(x="", y=AccessionNumber, fill=SpecimenType))+
#  geom_bar(width = 1, stat = "identity")

#pie chart by specimen type
ggplot(ICMP.df, aes(x=factor(1), fill=SpecimenType)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='./outputs/ICMP/ICMP_specimen_pie.png', width=5, height=5)


#pie chart by last updated user
ICMP.df$LastUpdatedBy <- sapply(ICMP.df$UpdatedBy, tolower)

ggplot(ICMP.df, aes(x=factor(1), fill=LastUpdatedBy)) +
  labs(title = "Last User to update an ICMP record") +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='./outputs/ICMP/ICMP_last-updated_pie.png', width=5, height=5)


#============Extended specimen data================

#install gridextra
library(gridExtra)

#GenBank status by sample type
#genbank.plot <- ggplot(ICMP.df, aes(SpecimenType, fill=GenBank)) +
ggplot(ICMP.df, aes(SpecimenType, fill=GenBank)) +
  labs(title = "Cultures in the ICMP in GenBank") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
ggsave(file='./outputs/ICMP/extended-specimen-genbank.png', width=7, height=7)

#Literature status by sample type
#literature.plot <- ggplot(ICMP.df, aes(SpecimenType, fill=Literature)) +
ggplot(ICMP.df, aes(SpecimenType, fill=Literature)) +
  labs(title = "Cultures in the ICMP with a citation in literature") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
ggsave(file='./outputs/ICMP/extended-specimen-literature.png', width=7, height=7)

#Image status by sample type
#image.plot <- ggplot(ICMP.df, aes(SpecimenType, fill=Images)) +
  ggplot(ICMP.df, aes(SpecimenType, fill=Images)) +
  labs(title = "Cultures in the ICMP with images") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
ggsave(file='./outputs/ICMP/extended-specimen-images.png', width=7, height=7)

#This is a way to arrange on a grid, need to uncomment above
#grid.arrange(genbank.plot, literature.plot, image.plot, nrow = 2, ncol = 2)


#Add code here from the rmd file for faceted graph


```{r Extended specimen data, echo=TRUE}

#make a new column called GeoRef true false off DecimalLat
ICMP.df$GeoRef <- ifelse(!is.na(ICMP.df$DecimalLat), TRUE, FALSE)

#sub set out the interesting columns using "select" in dpylr
only.ext.specimen <- select(ICMP.df, "AccessionNumber","SpecimenType", "GenBank", "Literature", "Images", "GeoRef")

#create two new columns using "pivot_longer" into a tibble table
tibble.ext.specimen <-pivot_longer(only.ext.specimen, cols=3:6, names_to="ExtendedSpecimen", values_to="present")

# ** Cultures with extended specimen data
ggplot(tibble.ext.specimen, aes(ExtendedSpecimen, fill=present)) +
  labs(title = "Cultures with DNA sequences, georeferences, images, and citations in literature") +
  labs(x = "extended specimen data", y = "proportion of cultures with this data") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill")

```



#============Sample kind Level barcharts================

#Simple sample type barchart
ggplot(ICMP.df, aes(SpecimenType)) +
  labs(title = "Cultures in the ICMP by Sample type") +
  labs(x = "Taxon", y = "number of isolates") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_kingdoms.png', width=7, height=7)


#Occurrence in NZ
ggplot(ICMP.df, aes(SpecimenType, fill=OccurrenceDescription)) +
  labs(title = "Cultures in the ICMP by occurrence in NZ") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='./outputs/ICMP/ICMP_kingdoms_occurrence.png', width=7, height=7)


#Occurrence in NZ
ggplot(ICMP.df, aes(OccurrenceDescription, fill=SpecimenType)) +
  labs(title = "Cultures in the ICMP by occurrence in NZ") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='./outputs/ICMP/ICMP_kingdoms_occurrence-alt.png', width=7, height=7)

#Order Status
ggplot(ICMP.df, aes(SpecimenType, fill= LoanStatus)) +
  labs(title = "ICMP Order Status") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='./outputs/ICMP/ICMP_kingdoms_LoanStatus.png', width=7, height=7)

#kingdoms by last updated
#need to filter out low users and just as a bar or pie graph?
ICMP.df$UpdatedBy3 <- sapply(ICMP.df$UpdatedBy, tolower)
ggplot(ICMP.df, aes(SpecimenType, fill= UpdatedBy3)) +
  labs(title = "ICMP Last updated by") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='./outputs/ICMP/ICMP_kingdoms_updated_by.png', width=7, height=7)

#need a kingdoms by NZ cultures??




#============High Taxonomy================

# ----- bacterial taxon grouping -----

sort(table(ICMP.df$Family),decreasing=TRUE)[1:11] #top 11 families
sort(table(ICMP.NZ.df$Family),decreasing=TRUE)[1:11] #top 11 NZ families


#Bacterial Phylum
ggplot(ICMP.bacteria, aes(Phylum)) +
  labs(title = "ICMP by bacterial phylum") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_bacteria-phylum.png', width=10, height=10)

#Bacterial Class
ggplot(ICMP.bacteria, aes(Class)) +
  labs(title = "ICMP by bacterial class") +
  labs(x = "Taxon", y = "number of isolates") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_bacteria-class.png', width=10, height=10)

#Bacterial Order
ggplot(ICMP.bacteria, aes(Order)) +
  labs(title = "ICMP by bacterial order") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_bacteria-order.png', width=10, height=10)

#Bacterial Family
ggplot(ICMP.bacteria, aes(Family)) +
  labs(title = "ICMP by bacterial family") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_bacteria-family.png', width=10, height=20)


# -----  fungal taxon grouping ----- 

#Fungal Phylum
ggplot(ICMP.fungi, aes(Phylum)) +
  labs(title = "ICMP by fungal phylum") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_fungal-phylum.png', width=10, height=10)


#Fungal Class
ggplot(ICMP.fungi, aes(Class)) +
  labs(title = "ICMP by fungal class") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_fungal-class.png', width=10, height=10)

#Fungal Order
ggplot(ICMP.fungi, aes(Order)) + 
  labs(title = "ICMP by fungal order") + 
  labs(x = "Taxon", y = "number of isolates") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_fungal-order.png', width=10, height=20)

#Fungal Family
ggplot(ICMP.fungi, aes(Family)) +
  labs(title = "ICMP by fungal family") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  geom_bar() + 
  coord_flip()
ggsave(print_bars, file='./outputs/ICMP/ICMP_fungal-family.png', width=0, height=20)



#============Other names================

#Yeast Phylum
ggplot(ICMP.yeast, aes(Phylum)) + 
  labs(title = "ICMP by yeast phylum") + 
  labs(x = "Taxon", y = "number of isolates") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_yeast-phylum.png', width=10, height=10)


#Chromist Phylum
ggplot(ICMP.chromist, aes(Phylum)) +
  labs(title = "ICMP by chromist phylum") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  geom_bar() + 
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP_chromist-phylum.png', width=10, height=10)





# fungi present in NZ
#names.present.fungi <- subset(ICMP.df,(Kingdom == "Fungi" & OccurrenceDescription == "Present"))
#summary(names.present.fungi, maxsum=40)

#ggplot code for fungal Phylum
require(ggplot2)
p <- ggplot(names, aes(names$Phlum)) + labs(title = "names by phylum") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-phylum.png', width=10, height=10)



#ggplot code for fungal Phylum
require(ggplot2)
p <- ggplot(names, aes(names$Phlum, fill=OccurrenceDescription)) + labs(title = "names by phylum") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-phylum-occurrence.png', width=10, height=10)


#ggplot code for Kingdom
require(ggplot2)
p <- ggplot(names, aes(names$Kingdom, fill=OccurrenceDescription)) + labs(title = "names by Kingdom") + labs(x = "Taxon", y = "number of names")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='names-Kindom-occurrence.png', width=10, height=10)


#ggplot code for Kingdom biostatus
require(ggplot2)
p <- ggplot(names, aes(names$Kingdom, fill=BioStatusDescription)) + labs(title = "names by Kingdom") + labs(x = "Taxon", y = "number of names")
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

count(ICMP.df)

ICMP.df |> 
  count(Country == "New Zealand")

ICMP.df |> 
  filter(Country == "New Zealand") |> 
  count()


countNZ <- ICMP.df |>
  count(Country == "New Zealand")

#convert to a percentage

countNZp <-(countNZ / count(ICMP.df))*100






sort(table(ICMP.df$Country),decreasing=TRUE)[1:12] #top 11 countries

#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "Thailand", "China", "India",  "Italy")
ICMP.10county <- subset(ICMP.df, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Thailand" | Country == "China" | Country == "Japan" | Country == "India" | Country == "Italy"))

ggplot(ICMP.10county, aes(Country, fill=SpecimenType)) +
  labs(title = "Top 10 Countries in the ICMP") +
  labs(x = "Country", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  theme_bw() +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(limits = positions)
ggsave(file='./outputs/ICMP/ICMP_country_by_kind.png', width=8, height=4.5)


#ggplot code for pacific country
pacific <- subset(ICMP.df, (Country == "Fiji" | Country == "American Samoa" | Country == "Cook Islands" | Country == "Solomon Islands" | Country == "Micronesia" | Country == "New Caledonia" | Country == "Niue" | Country == "Norfolk Island" | Country == "Samoa" | Country == "Vanuatu"))
ggplot(pacific, aes(Country, fill=SpecimenType)) +
  labs(title = "Pacific Countries cultures in the ICMP") +
  labs(x = "Country", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()
ggsave(file='./outputs/ICMP/ICMP-pacific-countries.png', width=10, height=10)


#ggplot code for country
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "Thailand", "China", "India",  "Italy", "Iran, Islamic Republic Of")
ICMP.10county.noNZ <- subset(ICMP.df, (Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Thailand" | Country == "China" | Country == "Japan" | Country == "India" | Country == "Italy") | Country == "Iran, Islamic Republic Of")
ggplot(ICMP.10county.noNZ, aes(Country, fill=SpecimenType)) +
  labs(title = "Top 10 Countries in the ICMP (not including NZ)") +
  labs(x = "Country", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  scale_fill_brewer(palette = "Set2") +
  geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(file='./outputs/ICMP/ICMP_country_by_kind_not_nz.png', width=10, height=10)


## could make a pseudo dataset manually replacing all non target countries with "other"


#============Over time================

## ideas ##
# Do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do a cumulative graph?


#should make a tibble of the below

#all cultures sorted by date. Add a new column date.isolated 
ICMP.df$date.isolated <- ymd(ICMP.df$IsolationDateISO, truncated = 3)
arrange(ICMP.df, date.isolated) %>%
  select("AccessionNumber","SpecimenType", "Country", "date.isolated") %>%
  slice_head(n=25)

#all cultures sorted by deposited date. Add a new column date.isolated 
ICMP.df$date.deposited <- ymd(ICMP.df$DepositedDateISO, truncated = 3)
arrange(ICMP.df, date.deposited) %>%
  select("AccessionNumber","SpecimenType", "Country", "date.deposited") %>%
  slice_head(n=10)

#New Zealand cultures sorted by date. Add a new column date.isolated
ICMP.NZ.df$date.isolated <- ymd(ICMP.NZ.df$IsolationDateISO, truncated = 3)
arrange(ICMP.NZ.df, date.isolated) %>%
  select("AccessionNumber","SpecimenType", "Country", "date.isolated") %>%
  slice_head(n=25)



#new month ICMP culture isolated (in NZ)
month.isolated <- ymd(ICMP.NZ.df$IsolationDateISO, truncated = 1)
mergemonths <- floor_date(month.isolated, unit = "month")
ggplot(ICMP.NZ.df, aes(month(mergemonths, label = TRUE), fill = SpecimenType)) +
  labs(title = "Isolation month of ICMP cultures from NZ") + 
  labs(x = "Month of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  geom_bar() + 
  scale_x_discrete(na.translate = FALSE)  # this removes NAs
ggsave(file='./outputs/ICMP/ICMP-isolation-month.png', width=8, height=5)


#new week ICMP culture isolated (in NZ)
week.isolated <- ymd(ICMP.NZ.df$IsolationDateISO, truncated = 0)
mergeweeks <- round_date(week.isolated, unit = "week")
ggplot(ICMP.NZ.df, aes(isoweek(mergeweeks), fill = SpecimenType)) +
  labs(title = "Isolation month of ICMP cultures from NZ") + 
  labs(x = "Week of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  geom_bar() + 
  scale_x_discrete(na.translate = FALSE)  # this removes NAs
ggsave(file='./outputs/ICMP/ICMP-isolation-month.png', width=8, height=5)


#years test, this works and produces a bar chart but not super useful
ICMP.df$date.isolated <-ymd(ICMP.df$IsolationDateISO, truncated = 3) %>%
  floor_date(ICMP.df$date.isolated, unit = "year")

ggplot(ICMP.df, aes(date.isolated, fill = SpecimenType)) +
  labs(title = "Isolation dates of ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = c(0.1, 0.8)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_bar()  # this is a bin of two years: binwidth=730





ICMP.df$date.isolated <-ymd(ICMP.df$IsolationDateISO, truncated = 3) %>%
  floor_date(ICMP.df$date.isolated, unit = "year")

ICMP.df$date.isolated
   



#ICMP isolation dates playing with plotting
#could do a histogram with a wider binned freqpoly as a 'trendline'
date.isolated <-ymd(ICMP.df$IsolationDateISO, truncated = 3)
ggplot(ICMP.df, aes(date.isolated, fill = SpecimenType, colour = SpecimenType)) +
  labs(title = "Isolation dates of ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = c(0.1, 0.8)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  #geom_dotplot(binwidth=365.25) #too much data
  #stat_bin(binwidth=365.25) #fine i guess
  geom_freqpoly(binwidth=1461) + # a bin of 4 years
  geom_histogram(binwidth=1461)  # this is a bin of two years: binwidth=730
ggsave(file='./outputs/ICMP/ICMP-isolation-dates2.png', width=8, height=5)


#testing some new code for overlaying a trend
ICMP.bacteria$date.deposited <- ymd(ICMP.bacteria$DepositedDateISO, truncated = 3)
ggplot(ICMP.bacteria, aes(date.deposited)) +
  theme_bw() +
  labs(title = "Date bacterial cultures were deposited in ICMP") +
  labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25, alpha = 0.2) +
  geom_density(aes(y=500 * ..count..), size = 2) #multiplies the density by 500x so it is visible
ggsave(file='./outputs/ICMP/ICMP-bacteria-depost-dates-smoothed.png', width=8, height=5)


#deposits over time
ICMP.df$date.deposited <- ymd(ICMP.df$DepositedDateISO, truncated = 3)
ggplot(ICMP.df, aes(date.deposited)) +
  theme_bw() +
  labs(title = "Date cultures were deposited in ICMP") +
  labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25, alpha = 0.2) +
  geom_density(aes(y=500 * ..count..), size = 2) #multiplies the density by 500x so it is visible
ggsave(file='./outputs/ICMP/ICMP-total-deposits-smoothed.png', width=8, height=5)


#ICMP isolation dates 
date.isolated <-ymd(ICMP.df$IsolationDateISO, truncated = 3)
ggplot(ICMP.df, aes(date.isolated, fill = SpecimenType)) +
  theme_bw()+
  labs(title = "Isolation dates of ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = c(0.1, 0.8)) +
    geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")
ggsave(file='./outputs/ICMP/ICMP-isolation-dates.png', width=8, height=5)


#ICMP isolation dates faceted
date.isolated <-ymd(ICMP.df$IsolationDateISO, truncated = 3)
ggplot(ICMP.df, aes(date.isolated, fill = SpecimenType)) +
  labs(title = "Isolation dates of ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  facet_grid(SpecimenType ~ .)
ggsave(file='./outputs/ICMP/ICMP-isolation-dates-facet.png', width=8, height=5)


#ICMP deposit date faceted
date.deposited <-ymd(ICMP.df$DepositedDateISO, truncated = 3)
ggplot(ICMP.df, aes(date.deposited, fill = SpecimenType)) +
  labs(title = "Deposit dates of ICMP cultures") +
  labs(x = "Date of deposit in ICMP", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + # this is a bin of two years: binwidth=730
  facet_grid(SpecimenType ~ .)
ggsave(file='./outputs/ICMP/ICMP-deposit-dates-facet.png', width=8, height=5)


#ICMP deposit date faceted
date.deposited <-ymd(ICMP.df$DepositedDateISO, truncated = 3)
ggplot(ICMP.df, aes(date.deposited, fill = SpecimenType)) +
  labs(title = "Deposit dates of ICMP cultures") +
  labs(x = "Date of deposit in ICMP", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) # this is a bin of two years: binwidth=730
ggsave(file='./outputs/ICMP/ICMP-deposit-dates.png', width=8, height=5)

#testing some new code
ICMP.df$date.deposited <- ymd(ICMP.df$DepositedDateISO, truncated = 3)
builder <- ggplot(ICMP.df, aes(date.deposited)) +
  labs(title = "Date bacterial cultures were deposited in ICMP") +
  labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
#  geom_hline(yintercept=392, linetype=2)

ggplot_build(builder)
builder.data<-ggplot_build(builder)$data
head(builder.data)
mean(builder.data$count)

#could do this early on and convert all to proper dates?? - nah make a seperate Date column
#for intercep do count per year to seperate table?

#collection dates. really only for comparing with isolation %% CollectionDateISO
ggplot(ICMP.df, aes(as.Date(CollectionDateISO, format='%Y-%m-%d'))) +
  labs(title = "Date cultures were collected") +
  labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") +
  scale_x_date() +
  geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
ggsave(file='./outputs/ICMP/ICMP-Collection-dates.png', width=5, height=5)




ICMP.df$topcontrib <- ifelse(ICMP.df$Contributor == "NZP", "NZP", "other")
ICMP.df$topcontrib


attach(ICMP.df) 
require(ggplot2)
dr <- ggplot(ICMP.df, aes(as.Date(ReceivedDateISO, format='%Y-%m-%d'),fill=topcontrib)) + labs(title = "Main Contributors to the ICMP collection") + labs(x = "Date of Receipt", y =  "Number of cultures" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='ICMP-received-dates-contributor.png', width=15, height=10)






sum2 <- ggplot_build(drp) #this extracts the values from the histogram
sum2


attach(ICMP.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(ICMP.df, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the ICMP collection") + labs(x = "Date of Receipt", y =  "Number of cultures" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='ICMP-received-dates-contributor.png', width=15, height=10)


ICMP.df$topcontrib <- ifelse(ICMP.df$Contributor == "NZP", "NZP", "other")

ICMP.df$topcontrib








#======NZ Maps========

#New Zealand Area codes bar chart
ICMP.NZ.df <- subset(ICMP.df,(Country == "New Zealand"))
positions <- c("New Zealand", "Campbell Island", "Auckland Islands", "Snares Islands", "Chatham Islands",  "Stewart Island", "Southland", "Fiordland", "Dunedin", "Central Otago", "Otago Lakes", "South Canterbury", "Mackenzie", "Westland", "Mid Canterbury", "North Canterbury", "Buller", "Kaikoura", "Marlborough", "Nelson", "Marlborough Sounds", "South Island", "Wairarapa", "Wellington", "Hawkes Bay", "Rangitikei", "Wanganui", "Gisborne", "Taupo", "Taranaki", "Bay of Plenty", "Waikato", "Coromandel", "Auckland", "Northland", "North Island", "Three Kings Islands", "Kermadec Islands")
ggplot(ICMP.NZ.df, aes(NZAreaCode)) +
  theme_bw() +
  labs(title = "ICMP cultures by NZ region") +
  labs(x = "Crosby Region", y = "number of cultures") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_x_discrete(limits = positions)
ggsave(file='./outputs/ICMP/ICMP_NZAreaCode.png', width=8, height=5)


#require(ggplot2)
#require(ggmap)
#require(maps)
#require(mapdata)


#Good code for NZ map plotting
library(dplyr) #Pipes/filter because I am lazy
library(sf) #Simple feature
library(ggplot2) #Mapping
library(ggspatial) #Add's fancy shit to the map

# Loading in data 
#Reading in a NZ specific map
nz.sf <- st_read(dsn = "./data/coastline/coastline.shp") %>%
  st_transform(2193) #Setting map projection - NZGD2000

#Transforming to an SF object
rhizoNZ.sf <- ICMP.df %>%
  filter(!is.na(DecimalLat)) %>% #Removing missing obs as sf doesn't play with these
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) %>% #Defining what the coord columns are
  st_set_crs(4326) %>% #Telling sf it is in WSG84 projection
  st_transform(2193) %>% #Changing it to NZGD2000 to match coastline polygon
  st_crop(st_bbox(nz.sf)) #Cropping out points that are outside the coastline polygons bounding box (e.g. not NZ)

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = rhizoNZ.sf, aes(colour = NZAreaCode),
          size = 1, alpha = 0.2, show.legend = FALSE) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal()
#Saving the code
ggsave(file='./outputs/NZ_map.png', width=8, height=6)






#the map has an issue as a work around remove the Chatham islands

ICMP.no.chat <- ICMP.NZ.df %>%
  filter(NZAreaCode != "Chatham Islands") %>%
  filter(NZAreaCode != "Kermadec Islands")

#this super high res map is for checking weird points
nz <- map_data("nzHires")
ggplot() + 
  geom_polygon(data = nz, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = ICMP.no.chat, aes(x = DecimalLong, y = DecimalLat, colour = NZAreaCode), size = 1, alpha = 0.5, show.legend = FALSE) +
  geom_text(data = ICMP.no.chat, aes(x = DecimalLong, y = DecimalLat, label = AccessionNumber), hjust = 0.0, size = 1, color = "black") + #, angle = 45
  coord_fixed(1.3)
ggsave(file='ICMP_NZ-aeracode-map_labels.png', width=50, height=50, limitsize = FALSE)
ggsave(file='ICMP_NZ-aeracode-map_labels.svg', width=50, height=50, limitsize = FALSE)



#position=position_jitter(width=0,height=0)

antibiotic.map <- read.csv("ICMP-antibiotic-map.csv", header=TRUE, sep=",")
head(antibiotic.map)

nz <- map_data("nzHires")
ggplot() + 
  geom_polygon(data = nz, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = antibiotic.map, aes(x = DecimalLong, y = DecimalLat), color = "black", size = 2) +
  geom_text(data = antibiotic.map, aes(x = DecimalLong, y = DecimalLat, label = AccessionNumber), hjust = -0.2, size = 1, color = "black") + #, angle = 45
  coord_fixed(1.3)
ggsave(file='ICMP_antibiotic_july2021labels.png', width=10, height=10)
ggsave(file='ICMP_antibiotic_july2021labels.svg', width=10, height=10)

#to make the Chatham islands work you need to fudge them over 180 e.g. 183.41667
#will have to melt and add the fudge factor somehow to only Chatams ones


library(leaflet)

#factpal <- colorFactor(topo.colors(5), magic.df$CurrentName)

factpal <- colorFactor(palette = "Dark2", domain = ICMP.df$SpecimenType)

#Using leaflet
ICMP.NZ.leaf <- leaflet(ICMP.NZ.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~DecimalLong, 
                   lat = ~DecimalLat, 
                   popup = ~AccessionNumber,
                   label = ~CurrentName,
                   color = ~factpal(SpecimenType))

#Opening up viewer
ICMP.NZ.leaf



library(leaflet)

factpal <- colorFactor(topo.colors(19), ICMP.NZ.df$NZAreaCode)
#factpal <- colorFactor(palette = "Dark2", domain = ICMP.NZ.df$NZAreaCode)



ICMP.NZ.df <- ICMP.NZ.df %>%
filter(NZAreaCode == "Taranaki")
  
  
#Using leaflet
ICMP.crosby.leaf <- leaflet(ICMP.NZ.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~DecimalLong, 
                   lat = ~DecimalLat, 
                   popup = ~AccessionNumber,
                   label = ~CurrentName,
                   color = blues9)

#Opening up viewer
ICMP.crosby.leaf



#======World Maps========

world <- map_data("world")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = ICMP.df, aes(x = DecimalLong, y = DecimalLat), color = "#1f78b4", size = 1, alpha = 0.2) +
  #geom_text(data = ICMP.df, aes(x = DecimalLong, y = DecimalLat, label = ""), hjust = -0.2, size = 1, color = "black") + #, angle = 45
  coord_fixed(1.3)
ggsave(file='ICMP_worldmap_poster.png', width=12, height=6)




world <- map_data("world2")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = ICMP.df, aes(x = DecimalLong, y = DecimalLat), color = "red", size = 1, alpha = 0.2, wrap=c(0,360)) +
  coord_fixed(1.3)




#======On Hosts========


sort(table(ICMP.df$Family_C2),decreasing=TRUE)[1:11] #top 11 families
sort(table(ICMP.NZ.df$Family_C2),decreasing=TRUE)[1:11] #top 11 NZ families

#substrate does not work well as a controlled vocab.
sort(table(ICMP.df$Substrate),decreasing=TRUE)[1:11] #top 11 substrates
sort(table(ICMP.NZ.df$Substrate),decreasing=TRUE)[1:11] #top 11 NZ substrates


#barchart of to 10 host families sorted by 'kind' of type, coloured by kind of organism
positions.10hosts <- c("Leguminosae", "Solanaceae", "Rosaceae", "Gramineae", "Actinidiaceae", "Myrtaceae", "Vitaceae", "Compositae", "Nothofagaceae",  "Cucurbitaceae")
ICMP.10hosts <- subset(ICMP.df, (Family_C2 == "Leguminosae" | Family_C2 == "Solanaceae" | Family_C2 == "Rosaceae" | Family_C2 == "Gramineae" | Family_C2 == "Actinidiaceae" | Family_C2 == "Myrtaceae" | Family_C2 == "Vitaceae" | Family_C2 == "Compositae" | Family_C2 == "Nothofagaceae" | Family_C2 == "Cucurbitaceae"))

ggplot(ICMP.10hosts, aes(Family_C2, fill=SpecimenType)) +
  labs(title = "Top 10 host Families in the ICMP culture collection") +
  labs(x = "Host plant Family", y = "number of cultures") +
  geom_bar() + 
  coord_flip() +
  theme(legend.position = c(0.85, 0.75)) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(limits = positions.10hosts)


#kiwifruit
require(ggplot2)
ICMP.df.kiwifruit <- subset(ICMP.bacteria,(Family_C2 == "Actinidiaceae" ))
ggplot(ICMP.df.kiwifruit, aes(Family)) +
  labs(title = "Family of microbes on kiwifruit in the ICMP") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(print_bars, file='ICMP_kiwifruit-family.png', width=10, height=10)


#standard all ICMP overtime stats
require(ggplot2)
require(lubridate)
date.isolated <-ymd(ICMP.df.kiwifruit$IsolationDateISO, truncated = 1)
ggplot(ICMP.df.kiwifruit, aes(date.isolated, fill = Country)) +
  labs(title = "Isolation dates of ex-kiwifruit ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  theme(legend.position = c(0.1, 0.7)) +
  geom_histogram(binwidth=365.25) +
  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP-isolation-dates-kiwifruit.png', width=8, height=5)




#NZ Myrtaceae cultures in ICMP with a sequence
ICMP.df.Myrtaceae <- subset(ICMP.NZ.df,(Family_C2 == "Myrtaceae"))
ggplot(ICMP.df.Myrtaceae, aes(SpecimenType, fill=GenBank)) + 
  labs(title = "NZ Myrtaceae cultures in ICMP with a sequence") +
  labs(x = "Taxonomic group", y = "Number of cultures") +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP_Myrtaceae-genbank.png', width=8, height=5)

#Family of 'microbe' on NZ Myrtaceae in the ICMPt
ICMP.df.Myrtaceae <- subset(ICMP.NZ.df,(Family_C2 == "Myrtaceae"))
ggplot(ICMP.df.Myrtaceae, aes(Family, fill=SpecimenType)) + #fill by type
  labs(title = "Family of 'microbe' on NZ Myrtaceae in the ICMP") +
  labs(x = "Family", y = "number of isolates") +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='ICMP_Myrtaceae-family.png', width=8, height=15)


#======people======

summary(ICMP.df$StandardCollector, maxsum=50)

#======pulling stats======

#NgāiTahu fungi
#this data was cleaned for: public, NZ, fungi, and deposited before DOC date: 1 November 2011

NgāiTahu.trimed <- read.csv("ICMP-export-ngaitahu.csv", header=TRUE, sep=",")
head(NgāiTahu.trimed)
summary(NgāiTahu.trimed, maxsum=10)
NgāiTahu <- subset(NgāiTahu.trimed, (NZAreaCode == "Stewart Island" | NZAreaCode == "	Southland" | NZAreaCode == "Dunedin" | NZAreaCode == "Central Otago" | NZAreaCode == "Otago Lakes" | NZAreaCode == "Westland" | NZAreaCode == "Mackenzie" | NZAreaCode == "South Canterbury" | NZAreaCode == "Mid Canterbury" | NZAreaCode == "North Canterbury" | NZAreaCode == "Buller"))
summary(NgāiTahu)

#standard all ICMP overtime stats
attach(NgāiTahu) 
require(ggplot2)
require(lubridate)
date.isolated <-ymd(NgāiTahu$DepositedDateISO, truncated = 1)
di <- ggplot(NgāiTahu, aes(date.isolated, fill = SpecimenType)) + labs(title = "Isolation dates of ICMP cultures") + labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") 
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='NgāiTahu-isolation-dates2.png', width=8, height=5)






library(lubridate)
d=tribble(ICMP.df)
d %>% mutate(date=mdy(textdate)) %>%
  arrange(date)
?arrange

#====== string manipulation======

#Things I want to do are:
#  Split a taxon name into bits and extract Genus and species


library(stringr)
str_c(str_split_fixed("Bacteroides oleiciplenus YIT 12058", " ",4)[,c(1,3)],collapse=" ")

lst <- strsplit(mystring, " ")  # split string on space
lst[[1]][2] # access second element

names <- c("Keisha", "Mohammed", "Jane", "Mathieu")
str_view(CurrentName, "^M")




#merge together the Genbank literature and image data to one column

#sub set out the interesting columns using "select" in dpylr
library(dplyr)
only.ext.specimen <- select(ICMP.df, "AccessionNumber","SpecimenType", "GenBank", "Literature", "Images")
head(only.ext.specimen)

#create two new columns using "pivot_longer" into a tibble table
library(tidyr)
tibble.ext.specimen <-pivot_longer(only.ext.specimen, cols=3:5, names_to="ExtendedSpecimen", values_to="present")
head(tibble.ext.specimen)

# ** Cultures with extended specimen data
ggplot(tibble.ext.specimen, aes(ExtendedSpecimen, fill=present)) +
  labs(title = "Cultures with GenBank DNA sequences, images, and citations in literature") +
  labs(x = "extended specimen data", y = "proportion of cultures with this data") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill")


#Cultures with extended specimen data by SpecimenType with ExtendedSpecimen facet
ggplot(tibble.ext.specimen, aes(SpecimenType, fill=present)) +
  labs(title = "Cultures with GenBank DNA sequences, images, and citations in literature") +
  labs(x = "Taxon", y = "number of cultures") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar() +
  facet_grid(cols = vars(ExtendedSpecimen))

# ** Cultures with extended specimen data by ExtendedSpecimen with SpecimenType facet
ggplot(tibble.ext.specimen, aes(ExtendedSpecimen, fill=present)) +
  labs(title = "Cultures with GenBank DNA sequences, images, and citations in literature") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar() +
  facet_grid(cols = vars(SpecimenType))

#use position = "fill" to scale bars to 100%
ggplot(tibble.ext.specimen, aes(ExtendedSpecimen, fill=present)) +
  labs(title = "Cultures with GenBank DNA sequences, images, and citations in literature") +
  labs(x = "Taxon", y = "number of cultures") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(SpecimenType))



#======Special cases========

# Neofabraea actinidiae -----

#the map has an issue as a work around remove the Chatham islands

library(maps)
library(mapdata)

ICMP.Neofabraea <- ICMP.NZ.df %>%
  filter(NZAreaCode != "Chatham Islands") %>%
  filter(NZAreaCode != "Kermadec Islands") %>%
  filter(CurrentName == "Neofabraea actinidiae") %>%
  filter(TaxonName_C2 != "") %>%
  rename(Host = TaxonName_C2)

#this super high res map is for checking weird points
nz <- map_data("nzHires")
ggplot() + 
  labs(title = "Neofabraea actinidiae on plant hosts in NZ") +
  geom_polygon(data = nz, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = ICMP.Neofabraea, 
             aes(x = DecimalLong, 
                 y = DecimalLat, 
                 colour = Host), 
             size = 5, 
             alpha = 0.5, 
             show.legend = TRUE, 
             na.rm=TRUE) +
  #geom_text(data = ICMP.Neofabraea, position=position_jitter (width=0.5,height=0.2), aes(x = DecimalLong, y = DecimalLat, label = AccessionNumber), hjust = 0.0, size = 3, color = "black") + #little bit of jitter
  coord_map(xlim = c(172, 178), ylim = c(-34, -40))
ggsave(file='./outputs/ICMP/ICMP_Neofabraea_actinidiae_map.png', width=5, height=5, limitsize = FALSE)


# Pseudomonas savastanoi  -----
savastanoi.df <- ICMP.df %>% 
  filter(str_detect(CurrentName, "^Pseudomonas savastanoi")) %>% 
  filter(Country == "New Zealand") %>%
  rename(Host = TaxonName_C2) %>%
  mutate(date.isolated = ymd(IsolationDateISO, truncated = 3)) %>%
  glimpse()

#Calculate savastanoi isolation dates
ggplot(savastanoi.df, aes(date.isolated, fill = Host)) +
  labs(title = "Isolation dates of ICMP Pseudomonas savastanoi cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = c(0.15, 0.7)) +
  geom_histogram(binwidth=365.25, show.legend = TRUE) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")
ggsave(file='./outputs/ICMP/P.savastanoi-date-hosts.png', width=8, height=5)



# magic mushrooms -----

# Loading in data 
#Reading in a NZ specific map
nz.sf <- st_read(dsn = "./data/nz-coastlines-topo-150k/nz-coastlines-topo-150k.shp", quiet = FALSE) %>%
  st_transform(2193) #Setting map projection - NZGD2000

#Transforming to an SF object
Psilocybe.sf <- ICMP.df %>%
  filter(str_detect(CurrentName, "^Psilocybe")) %>%
  filter(OccurrenceDescription == "Present") %>%
  filter(!is.na(DecimalLat)) %>% #Removing missing obs as sf doesn't play with these
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) %>% #Defining what the coord columns are
  st_set_crs(4326) %>% #Telling sf it is in WSG84 projection
  st_transform(2193) %>% #Changing it to NZGD2000 to match coastline polygon
  st_crop(st_bbox(nz.sf)) #Cropping out points that are outside the coastline polygons bounding box (e.g. not NZ)

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = Psilocybe.sf, aes(colour = CurrentName),
          size = 2, alpha = 0.8, show.legend = TRUE) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  labs(title = "Collection location of ICMP Psilocybe cultures",
       caption = "Bevan Weir - 30 Aug 2022 - CC BY 4.0")

#this is too slow so need to build in a delay
ggsave(file='./outputs/ICMP/Psilocybe-ICMP.pdf', width=8, height=10)


#now need to filter and make a csv for sharing

ICMP.df %>%
  filter(Country == "New Zealand") %>%
  select("AccessionNumber",
         "StandardCollector",
         "CollectionDateISO",
         "CurrentName",
         "VerbatimLocality",
         "DecimalLat", 
         "DecimalLong", 
         "BiostatusDescription", 
         "OccurrenceDescription") %>%
  filter(str_detect(CurrentName, "^Psilocybe")) %>%
  write_csv(file='./outputs/ICMP/NZPsilocybe.csv')


## leaflet html map -----------------------------------------------------------

library(leaflet)

#Reading in csv data
magic.df <- read.csv("./outputs/ICMP/NZPsilocybe.csv")

#factpal <- colorFactor(topo.colors(5), magic.df$CurrentName)

factpal <- colorFactor(palette = "Dark2", domain = magic.df$CurrentName)

#To show
head(magic.df)

#Using leaflet
magic.leaf <- leaflet(magic.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~DecimalLong, 
                   lat = ~DecimalLat, 
                   popup = ~AccessionNumber,
                   label = ~CurrentName,
                   color = ~factpal(CurrentName))

#Opening up viewer
magic.leaf

#use save as a webpage function in Rstudio to save

psilocybe-html-map.html


ICMP.df |> 
  select("VerbatimName") |> 
  head(n=22) |> 
  print(n = 22)

