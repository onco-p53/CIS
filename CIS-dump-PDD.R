## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2022)

#============Load all the packages needed================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)
#library(svglite)


#============Load data================

PDD.as.imported.df <- read_csv("PDD-export-15-apr-2023.csv",
                                guess_max = Inf, #assign column types
                                show_col_types = FALSE) %>%
  glimpse()


#============Check imported data for issues================

# get duplicates based due to component duplication
# may need correction in CIS if TaxonName_C2 = NA, export as a CSV

PDD.dupes <- PDD.as.imported.df %>%
  get_dupes(AccessionNumber) %>%
  select(AccessionNumber, dupe_count, CurrentName, TaxonName_C2, Substrate_C2, PartAffected_C2) %>%
  filter(is.na(TaxonName_C2)) %>% #comment this out to get all
  write_csv(file='./outputs/PDD/PDD.dupes.csv')


#============Subset and massage the Data================

PDD.df <- PDD.as.imported.df %>%
  distinct(AccessionNumber, .keep_all= TRUE) %>% #remove dupes
  glimpse()

#list the classes in Basidiomycota
PDD.df %>%
  filter(Phylum == "Basidiomycota") %>%
  group_by(Class) %>%
  dplyr::summarize(
    All.ICMP = n(), 
    .groups = "drop"
  )

PDD.df <- PDD.df %>%
mutate(specimen_kind = case_when(Phylum == 'Ascomycota' ~ 'ascomycetes',
                                 Class == 'Agaricomycetes' ~ 'mushrooms',
                                 Class == 'Agaricostilbomycetes' ~ 'rusts and smuts',
                                 Class == 'Atractiellomycetes' ~ 'rusts and smuts',
                                 Class == 'Basidiomycetes' ~ 'mushrooms',
                                 Class == 'Classiculomycetes' ~ 'rusts and smuts',
                                 Class == 'Cystobasidiomycetes' ~ 'rusts and smuts',
                                 Class == 'Dacrymycetes' ~ 'mushrooms',
                                 Class == 'Entorrhizomycetes' ~ 'other',
                                 Class == 'Exobasidiomycetes' ~ 'rusts and smuts',
                                 Class == 'Microbotryomycetes' ~ 'rusts and smuts',
                                 Class == 'Pucciniomycetes' ~ 'rusts and smuts',
                                 Class == 'Tremellomycetes' ~ 'mushrooms',
                                 Class == 'Ustilaginomycetes' ~ 'rusts and smuts',
                                 TRUE ~ 'other')) %>%
  glimpse()


# NOTES on classification
# In the Agaricomycetes for instance the grouping of the subclass
# Phallomycetidae is meaningful and useful – as a reminder that
# Ramarias, Earthstars and stinkhorns are related, and an old lineage.

# Mushrooms, brackets, paint-splash, clubs, puffballs, stinkhorns and allies
# = subphylum agaricomycotina
# Rusts & smuts = subphyla pucciniomycotina & subphylum ustilagomycotina
# it's why subphyla (and subclasses within the agarics) are very useful ranks




#subset New Zealand specimens
PDD.NZ.df <- subset(PDD.df,(Country == "New Zealand"))
summary(PDD.NZ.df, maxsum=40)



#all specimens sorted by date. Add a new column date.collected 
PDD.df %>%
  mutate(date.collected = ymd(CollectionDateISO, truncated = 3)) %>%
  arrange(date.collected) %>%
  select("AccessionNumber","specimen_kind", "CurrentName", "Country", "date.collected") %>%
  slice_head(n=5)

#New Zealand specimens sorted by date. Add a new column date.collected
PDD.NZ.df %>%
  mutate(date.collected = ymd(CollectionDateISO, truncated = 3)) %>%
  arrange(date.collected) %>%
  select("AccessionNumber","specimen_kind", "CurrentName", "Country", "date.collected") %>%
  slice_head(n=30)

  


#============Quick data check================
#have a quick look at the data
head(PDD.df)

#save a summary of the data to txt
PDD.string.factors <- read.csv("PDD-export-8-feb-2022.csv",
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

#filters for black occurrence description
#then deduplicate
PDD.df |>
  select(CurrentName, Country, OccurrenceDescription, BiostatusDescription) |> 
  filter(is.na(OccurrenceDescription)) |> 
  #filter(Country == "New Zealand") |> 
  distinct() |> 
  write_csv(file='./outputs/PDD/PDD-missing-occurrence.csv')

#============Bevan's Specimens================

bsw <- subset(PDD.df,(StandardCollector == "Weir, BS"))
ggplot(bsw, aes(Images, fill=GenBank)) +
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
p <- ggplot(PDD.df, aes(OccurrenceDescription)) + labs(title = "Specimens in the PDD by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
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
p <- ggplot(PDD.df, aes(OccurrenceDescription)) + labs(title = "Specimens in the PDD by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
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



#Phylum
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Phylum)) + labs(title = "PDD by phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_phylum.png', width=10, height=10)

#ggplot code for Class
attach(PDD.df) 
require(ggplot2)
p <- ggplot(PDD.df, aes(Class)) + labs(title = "PDD by class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_class.png', width=10, height=10)

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

#ggplot code for fungal Phylum
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Phylum)) + labs(title = "PDD by fungal phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_fungal-phylum.png', width=10, height=10)

#ggplot code for fungal Class
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Class)) + labs(title = "PDD by fungal class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_fungal-class.png', width=10, height=10)

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
names.present.fungi <- subset(PDD.df,(Kingdom == "Fungi" & OccurrenceDescription == "Present"))
summary(names.present.fungi, maxsum=40)

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

sort(table(PDD.df$Country),decreasing=TRUE)[1:13] #top 11 countries

positions <- c("New Zealand", "United States", "Australia", "Germany", "United Kingdom",
               "Canada", "Austria", "Fiji", "Cook Islands",  "Solomon Islands")

PDD.10county <- subset(PDD.df, (Country == "New Zealand" | Country == "United States" |
                                  Country == "Australia" | Country == "Germany" |
                                  Country == "United Kingdom" | Country == "Canada" |
                                  Country == "Austria" | Country == "Fiji" |
                                  Country == "Cook Islands" | Country == "Solomon Islands"))


PDD.NZ.df <- subset(PDD.df,(Country == "New Zealand"))
attach(PDD.NZ.df) 
require(ggplot2)
p <- ggplot(PDD.NZ.df, aes(NZAreaCode)) + labs(title = "NZ Specimens in the PDD by Area Code") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_NZAreaCode.png', width=7, height=7)




#ggplot code for country
cy <- subset(PDD.df,!(Country == ""))
require(ggplot2)
con <- ggplot(cy, aes(Country)) + labs(title = "Top 10 Countries") + labs(x = "Country", y = "number")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_country.png', width=10, height=10)


#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "Italy", "France")
c <- subset(PDD.df, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the PDD") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='PDD_country_by_kind.png', width=6, height=5)
ggsave(print_bars, file='PDD_country_by_kind.svg', width=6, height=5)
ggsave(print_bars, file='PDD_country_by_kind.eps', width=6, height=5)

#ggplot code for pacific country
c <- subset(PDD.df, (Country == "Fiji" | Country == "American Samoa" | Country == "Cook Islands" | Country == "Solomon Islands" | Country == "Micronesia" | Country == "New Caledonia" | Country == "Niue" | Country == "Norfolk Island" | Country == "Samoa" | Country == "Vanuatu"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the PDD") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD-pacific-countries.png', width=10, height=10)


#ggplot code for country
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "France", "Italy", "Canada")
c <- subset(PDD.df, (Country == "Canada" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the PDD (not including NZ)") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='PDD_country_by_kind_not_nz.png', width=10, height=10)


## could make a pseudo dataset manually replaceing all non target countries with "other"


#============timeline================

#all cultures sorted by date. Add a new column date.collected 
PDD.df$date.collected <- ymd(PDD.df$CollectionDateISO, truncated = 3)
arrange(PDD.df, date.collected) %>%
  select("AccessionNumber","SpecimenType", "Country", "date.collected") %>%
  slice_head(n=10)

#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?

#phylum to too many lets subset it:
library(tidyverse)
PDD.groups <- PDD.df %>%
  filter(Phylum == "Basidiomycota" | 
           Phylum == "Ascomycota" | 
           Phylum == "Amoebozoa" | 
           Phylum == "Oomycota") %>%
  filter(Deaccessioned == "false")


#Collection dates

date.collected <-ymd(PDD.df$CollectionDateISO, truncated = 3)
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
  filter(Phylum == "Basidiomycota") %>%
  filter(Deaccessioned == "false")

head(PDD.basidio)


#ICMP isolation dates faceted
attach(PDD.basidio) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.basidio$CollectionDateISO, truncated = 3)
ggplot(PDD.basidio, aes(date.collected, fill = Class)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
#  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") #+
#  facet_grid(Class ~ .)
ggsave(file='PDD-collection-dates-facet-basidio-class.png', width=8, height=5)


#ggplot code for Class
attach(PDD.basidio) 
require(ggplot2)
p <- ggplot(PDD.basidio, aes(Class)) + labs(title = "PDD Basidiomyces by class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_class_basidio.png', width=10, height=10)


#Only ascos:
library(tidyverse)
PDD.asco <- PDD.df %>%
  filter(Phylum == "Ascomycota") %>%
  filter(Deaccessioned == "false")

head(PDD.asco)


#ICMP isolation dates faceted
attach(PDD.asco) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.asco$CollectionDateISO, truncated = 3)
ggplot(PDD.asco, aes(date.collected, fill = Class)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Paired") +
  geom_histogram(binwidth=365.25, show.legend = TRUE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") #+
#  facet_grid(Class ~ .)
ggsave(file='PDD-collection-dates-facet-asco-class.png', width=8, height=5)


#Just discos:
library(tidyverse)
PDD.disco <- PDD.df %>%
  filter(Class == "Leotiomycetes") %>%
  filter(Deaccessioned == "false")

head(PDD.disco)


#ICMP isolation dates faceted
attach(PDD.disco) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.disco$CollectionDateISO, truncated = 3)
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
di <- ggplot(PDD.df, aes(as.Date(CollectionDateISO, format='%Y-%m-%d'))) + labs(title = "Collection dates of PDD specimens") + labs(x = "Date of collection", y =  "Number of specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-collection-dates.png', width=5, height=5)


PDD.df$topcontrib <- ifelse(PDD.df$StandardCollector == "Dingley, JM", "Dingley, JM", "other")
#there must be a better way to do this
PDD.df$topcontrib

attach(PDD.df) 
require(ggplot2)
dr <- ggplot(PDD.df, aes(as.Date(CollectionDateISO, format='%Y-%m-%d'),fill=topcontrib)) + labs(title = "Main Contributors to the PDD collection") + labs(x = "Date of Collection", y =  "Number of Specimens" , fill = "")
dr + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
drp <- di + geom_histogram(binwidth=365.25)
ggsave(drp, file='PDD-collection-dates-collector.png', width=15, height=10)


#============Over months================

PDD.Wellington.df <- subset(PDD.NZ.df,(NZAreaCode == "Wellington"))
glimpse(PDD.Wellington.df)

#new month PDD specimen collected (in NZ)
month.collected <- ymd(PDD.Wellington.df$CollectionDateISO, truncated = 1)
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
week.collected <- ymd(PDD.NZ.df$CollectionDateISO, truncated = 3)
ggplot(PDD.NZ.df, aes(isoweek(week.collected), fill = specimen_kind)) +
  labs(title = "Collection week of New Zealand PDD specimens") + 
  labs(x = "Week of Collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  geom_bar()
ggsave(file='./outputs/PDD/PDD-week.png', width=8, height=5)

#Collection week NZ PDD specimen collected (in NZ)
week.collected <- ymd(PDD.NZ.df$CollectionDateISO, truncated = 3)
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
date.collected <-ymd(PDD.NZ.df$CollectionDateISO, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.NZ.df, aes(month(mergemonths, label = TRUE), fill = Phylum)) + labs(title = "Collection month of PDD specimens from NZ") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
ggsave(dip, file='PDD-collection-month-dates.png', width=8, height=5)


library(tidyverse)
date.collected.nz <-ymd(PDD.NZ.df$CollectionDateISO)
arrange(PDD.NZ.df, date.collected.nz) #this is the ealiest isolated NZ specimen



#awheto specimens
PDD.awheto <- subset(PDD.df,(CurrentName == "Ophiocordyceps robertsii" | CurrentName == "Cordyceps hauturu"))
summary(PDD.awheto, maxsum=40)
attach(PDD.awheto) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.awheto$CollectionDateISO, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.awheto, aes(month(mergemonths, label = TRUE), fill = CurrentName)) + labs(title = "Collection month of PDD specimens of Awheto") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
ggsave(dip, file='PDD-awheto-dates.png', width=8, height=5)

PDD.awheto <- subset(PDD.df,(CurrentName == "Ophiocordyceps robertsii" | CurrentName == "Cordyceps hauturu"))
summary(PDD.awheto, maxsum=40)
attach(PDD.awheto) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.awheto$CollectionDateISO, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
di <- ggplot(PDD.awheto, aes(month(mergemonths, label = TRUE), fill = TaxonName_C2)) + labs(title = "Collection month of PDD specimens of Awheto with host") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE)
ggsave(dip, file='PDD-awheto-dates-host.png', width=8, height=5)

library(tidyverse)
drop_na(PDD.awheto$CollectionDateISOs)

PDD.awheto$CollectionDateISOs %>% drop_na()


#subset New Zealand specimens
PDD.awheto <- subset(PDD.df,(Family == "Glomerellaceae"))
summary(PDD.Glomerellaceae, maxsum=40)
attach(PDD.Glomerellaceae) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.awheto$CollectionDateISO, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.Glomerellaceae, aes(month(mergemonths, label = TRUE))) + labs(title = "Collection month of PDD specimens of Awheto") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar()
dip <- di + geom_bar()
ggsave(dip, file='Glomerellaceae-season.png', width=8, height=5)




date.collected

CollectionDateISO

mergemonths <- round_date(date.collected, unit = "month")
mergemonths



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
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the PDD") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_histogram()+ coord_flip()
print_bars <- con + geom_histogram()+ coord_flip()
ggsave(print_bars, file='PDD-pacific-countries.png', width=10, height=10)

#ggplot code for collections over the years in NZ
c <- subset(PDD.df, (Country == "New Zealand"))

#also need something that plots monthly e.g. fungi versus collection month.


#======GeoGraphic stuff========

#New Zealand Area codes This is good
nz <- subset(PDD.df,(Country == "New Zealand"))
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

#factpal <- colorFactor(topo.colors(5), magic.df$CurrentName)

factpal <- colorFactor(palette = "Dark2", domain = PDD.df$specimen_kind)

#Using leaflet
PDD.NZ.leaf <- leaflet(PDD.NZ.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~DecimalLong, 
                   lat = ~DecimalLat, 
                   popup = ~AccessionNumber,
                   label = ~CurrentName,
                   color = ~factpal(specimen_kind))

#Opening up viewer
PDD.NZ.leaf








#======Habitat========

#too many to work
nz <- subset(PDD.df,(Country == "New Zealand"))
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
positions.10hosts <- c("Nothofagaceae", "Gramineae", "Myrtaceae", "Compositae", "Leguminosae", "Pinaceae", "Rosaceae", "Cyperaceae", "Podocarpaceae",  "Lauraceae")
PDD.10hosts <- subset(PDD.df, (Family_C2 == "Nothofagaceae" | Family_C2 == "Gramineae" | Family_C2 == "Compositae" | Family_C2 == "Leguminosae" | Family_C2 == "Pinaceae" | Family_C2 == "Rosaceae" | Family_C2 == "Cyperaceae" | Family_C2 == "Compositae" | Family_C2 == "Podocarpaceae" | Family_C2 == "Lauraceae"))


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

PDD.Gramineae$date.collected <-ymd(PDD.Gramineae$CollectionDateISO, truncated = 3)
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
  filter(str_detect(CurrentName, "^Puccinia oxalidis")) %>%
  filter(OccurrenceDescription == "Present") %>%
  filter(!is.na(DecimalLat)) %>% #Removing missing obs as sf doesn't play with these
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) %>% #Defining what the coord columns are
  st_set_crs(4326) %>% #Telling sf it is in WSG84 projection
  st_transform(2193) %>% #Changing it to NZGD2000 to match coastline polygon
  st_crop(st_bbox(nz.sf)) #Cropping out points that are outside the coastline polygons bounding box (e.g. not NZ)

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = rusts.sf, aes(colour = CurrentName),
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

#ggplot code for fungal Phylum
ggplot(PDD.df, aes(FilingNumber, fill=Phylum)) +
  labs(title = "PDD Storage locations") +
  labs(x = "storage location", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD-storagelocation.png', width=10, height=10)

#ggplot code for fungal Phylum
require(ggplot2)
ggplot(PDD.df, aes(Phylum, fill=FilingNumber)) +
  labs(title = "PDD Storage locations") +
  labs(x = "storage location", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD-storagelocation2.png', width=10, height=10)


#============Over months================

PDD.colletot <- PDD.df %>% 
  filter (CurrentName == "Colletotrichum gloeosporioides (Penz.) Penz. & Sacc.")

PDD.colletot

unique(PDD.colletot$FilingNumber)
unique(PDD.colletot$TaxonName)








