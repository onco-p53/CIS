## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2021)

#============Load and subset data================
PDD.dump <- read.csv("PDD-export-2-dec-2021.csv", header=TRUE, sep=",")
summary(PDD.dump$AccessionNumber, maxsum=10)

# subset out "Deaccessioned=True", not implemented
# PDD.dump <- subset(noviruses,(Deaccessioned == "FALSE"))

#setting up per specimen type subsets, with summaries of each specimen type
PDD.alcohol <- subset(PDD.dump,(SpecimenType == "Alcohol"))
summary(PDD.alcohol, maxsum=40)

#subset New Zealand specimens
PDD.dump.NZ <- subset(PDD.dump,(Country == "New Zealand"))
summary(PDD.dump.NZ, maxsum=40)


#may want to use get_dupes() to deduplicate componenets?

#============Load all the packages needed================

library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer) # notes here: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
library(svglite)

#============Quick data check================
#have a quick look at the data
head(PDD.dump)

# summary(PDD.dump.initial, maxsum=25) #data before subsetting, not implemented
summary(PDD.dump, maxsum=25) #data after subsetting

s <- summary(PDD.dump, maxsum=25)
capture.output(s, file = "PDD-summary.txt")

# counts the number of unique values per collumn
sapply(PDD.dump, function(x) length(unique(x)))
up <- sapply(PDD.dump, function(x) length(unique(x)))
capture.output(up, file = "PDD-unique-count.txt")

# counts the number of unique values per collumn for NZ
sapply(PDD.dump.NZ, function(x) length(unique(x)))

#============Bevan's Specimens================

bsw <- subset(PDD.dump,(StandardCollector == "Weir, BS"))
ggplot(bsw, aes(Images, fill=GenBank)) +
  labs(title = "Bevan's specimens in PDD") +
  labs(x = "with images", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./ouputs/PDD/Bevans-with-images.png', width=5, height=5)

#bsws <- summary(bsw, maxsum=25)
#bsws


#============Type Specimens================

#ggplot code for type Specimens
d <- subset(PDD.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus)) + labs(title = "Types in the PDD") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_types.png', width=5, height=5)

#another one showing just the number of types in each kind of culture?

#ggplot code for type Specimens factored by Specimen type
d <- subset(PDD.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=Images)) + labs(title = "Types in the PDD Fungarium with images") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD.types.with.images.png', width=5, height=5)

#============Kingdom Level barcharts================

#plain code for a kingdom barchart
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(SpecimenType)) + labs(title = "Specimens in the PDD by Specimen type") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_specimen_types.png', width=5, height=5)

#kingdoms in GenBank
attach(PDD.dump)
require(ggplot2)
p <- ggplot(PDD.dump, aes(GenBank)) + labs(title = "Specimens in the PDD in GenBank") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_in_genbank.png', width=5, height=5)

#kingdoms with literature
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Literature)) + labs(title = "Specimens in the PDD in Literature") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_in_Literature.png', width=5, height=5)

#kingdoms with images
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Images)) + labs(title = "Specimens in the PDD with images") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_with_images.png', width=5, height=5)

#could also do a stacked bar chart with images, genbank, literature all on one chart.

#kingdoms by Occurrence Description
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(OccurrenceDescription)) + labs(title = "Specimens in the PDD by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_kingdoms_occurrence.png', width=5, height=5)

#CollectionEventMethod
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(CollectionEventMethod)) + labs(title = "Specimens in the PDD by Collection Event Method") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_CollectionEventMethod.png', width=7, height=7)

#kingdoms by Occurrence Description
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(OccurrenceDescription)) + labs(title = "Specimens in the PDD by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_kingdoms_occurrence2.png', width=7, height=7)

#kingdoms by Order Status
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(LoanStatus)) + labs(title = "PDD Order Status") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_LoanStatus.png', width=7, height=7)

#kingdoms by last updated by
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(UpdatedBy) + labs(title = "PDD Last updated by") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_updated_by.png', width=7, height=7)

#need a kingdoms by NZ Specimens??


#============High Taxonomy================



#Phylum
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Phylum)) + labs(title = "PDD by phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_phylum.png', width=10, height=10)

#ggplot code for Class
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Class)) + labs(title = "PDD by class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_class.png', width=10, height=10)

#ggplot code for Order
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Order)) + labs(title = "PDD by order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_order.png', width=10, height=10)

#ggplot code for Order
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Order, fill=SpecimenType)) + labs(title = "PDD by order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_order-speciemtype.png', width=10, height=10)

#ggplot code for Family
attach(PDD.dump) 
require(ggplot2)
p <- ggplot(PDD.dump, aes(Family)) + labs(title = "PDD by bacterial family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
#ggsave(print_bars, file='PDD_family.png', width=20, height=10)


# -----  fungal taxon grouping ----- 

f <- subset(PDD.dump, SpecimenType == "Fungal Culture")

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
names.present.fungi <- subset(PDD.dump,(Kingdom == "Fungi" & OccurrenceDescription == "Present"))
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


PDD.dump.NZ <- subset(PDD.dump,(Country == "New Zealand"))
attach(PDD.dump.NZ) 
require(ggplot2)
p <- ggplot(PDD.dump.NZ, aes(NZAreaCode)) + labs(title = "NZ Specimens in the PDD by Area Code") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_NZAreaCode.png', width=7, height=7)




#ggplot code for country
cy <- subset(PDD.dump,!(Country == ""))
require(ggplot2)
con <- ggplot(cy, aes(Country)) + labs(title = "Top 10 Countries") + labs(x = "Country", y = "number")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_country.png', width=10, height=10)


#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "Italy", "France")
c <- subset(PDD.dump, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
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
c <- subset(PDD.dump, (Country == "Fiji" | Country == "American Samoa" | Country == "Cook Islands" | Country == "Solomon Islands" | Country == "Micronesia" | Country == "New Caledonia" | Country == "Niue" | Country == "Norfolk Island" | Country == "Samoa" | Country == "Vanuatu"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the PDD") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD-pacific-countries.png', width=10, height=10)


#ggplot code for country
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "France", "Italy", "Canada")
c <- subset(PDD.dump, (Country == "Canada" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
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
PDD.dump$date.collected <- ymd(PDD.dump$CollectionDateISO, truncated = 3)
arrange(PDD.dump, date.collected) %>%
  select("AccessionNumber","SpecimenType", "Country", "date.collected") %>%
  slice_head(n=10)

#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?

#phylum to too many lets subset it:
library(tidyverse)
PDD.groups <- PDD.dump %>%
  filter(Phylum == "Basidiomycota" | 
           Phylum == "Ascomycota" | 
           Phylum == "Amoebozoa" | 
           Phylum == "Oomycota") %>%
  filter(Deaccessioned == "false")


#ICMP isolation dates faceted
attach(PDD.groups) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.groups$CollectionDateISO, truncated = 3)
ggplot(PDD.groups, aes(date.collected, fill = Phylum)) +
  labs(title = "Collection dates of PDD specimens") +
  labs(x = "Date of collection", y =  "Number of specimens" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + # this is a bin of two years: binwidth=730
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  facet_grid(Phylum ~ .)
ggsave(file='PDD-collection-dates-facet.png', width=8, height=5)


#Only basidios:
library(tidyverse)
PDD.basidio <- PDD.dump %>%
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
PDD.asco <- PDD.dump %>%
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
PDD.disco <- PDD.dump %>%
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

attach(PDD.dump) 
require(ggplot2)
di <- ggplot(PDD.dump, aes(as.Date(CollectionDateISO, format='%Y-%m-%d'))) + labs(title = "Collection dates of PDD specimens") + labs(x = "Date of collection", y =  "Number of specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-collection-dates.png', width=5, height=5)


PDD.dump$topcontrib <- ifelse(PDD.dump$StandardCollector == "Dingley, JM", "Dingley, JM", "other")
#there must be a better way to do this
PDD.dump$topcontrib

attach(PDD.dump) 
require(ggplot2)
dr <- ggplot(PDD.dump, aes(as.Date(CollectionDateISO, format='%Y-%m-%d'),fill=topcontrib)) + labs(title = "Main Contributors to the PDD collection") + labs(x = "Date of Collection", y =  "Number of Specimens" , fill = "")
dr + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
drp <- di + geom_histogram(binwidth=365.25)
ggsave(drp, file='PDD-collection-dates-collector.png', width=15, height=10)


#============Over months================

#Collection month of NZ PDD specimens
attach(PDD.dump.NZ) 
require(ggplot2)
require(lubridate)
date.collected <-ymd(PDD.dump.NZ$CollectionDateISO, truncated = 1)
mergemonths <- floor_date(date.collected, unit = "month")
month(date.collected, label = TRUE)
di <- ggplot(PDD.dump.NZ, aes(month(mergemonths, label = TRUE), fill = Phylum)) + labs(title = "Collection month of PDD specimens from NZ") + labs(x = "Month of collection", y =  "Number of specimens" , fill = "") 
di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
dip <- di + geom_bar() + scale_x_discrete(na.translate = FALSE) # this removes NAs
ggsave(dip, file='PDD-collection-month-dates.png', width=8, height=5)


library(tidyverse)
date.collected.nz <-ymd(PDD.dump.NZ$CollectionDateISO)
arrange(PDD.dump.NZ, date.collected.nz) #this is the ealiest isolated NZ specimen



#awheto specimens
PDD.awheto <- subset(PDD.dump,(CurrentName == "Ophiocordyceps robertsii" | CurrentName == "Cordyceps hauturu"))
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

PDD.awheto <- subset(PDD.dump,(CurrentName == "Ophiocordyceps robertsii" | CurrentName == "Cordyceps hauturu"))
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
PDD.awheto <- subset(PDD.dump,(Family == "Glomerellaceae"))
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






attach(PDD.dump) #this means we don't need the $ sign
require(ggplot2)
di <- ggplot(PDD.dump, aes(as.Date(IsolationDateISO))) + labs(title = "Isolation dates of PDD Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-isolation-dates2.png', width=4, height=3)




PDD.dump$topcontrib <- ifelse(PDD.dump$Contributor == "NZP", "NZP", "other")
PDD.dump$topcontrib

attach(PDD.dump) #this means we don't need the $ sign
require(ggplot2)
ditc <- ggplot(PDD.dump, aes(as.Date(IsolationDateISO, fill=Contributor))) + labs(title = "Isolation dates of PDD Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
ditc <- ditc + scale_x_date()
ditc + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
ditcp <- ditc + geom_histogram(binwidth=365.25)
ggsave(dip, file='PDD-isolation-dates2.png', width=4, height=3)




attach(PDD.dump) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(PDD.dump, aes(as.Date(ReceivedDateISO))) + labs(title = "Received dates of PDD Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='PDD-received-dates.png', width=10, height=10)

## CAN we do this by organism too?

attach(PDD.dump) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(PDD.dump, aes(as.Date(ReceivedDateISO),fill=SpecimenType)) + labs(title = "Received dates of PDD Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='PDD-received-dates-organism.png', width=15, height=10)


sum2 <- ggplot_build(drp) #this extracts the values from the histogram
sum2


attach(PDD.dump) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(PDD.dump, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the PDD collection") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='PDD-received-dates-contributor.png', width=15, height=10)


PDD.dump$topcontrib <- ifelse(PDD.dump$Contributor == "NZP", "NZP", "other")

PDD.dump$topcontrib

#ggplot code for collections over the years
attach(PDD.dump) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the PDD") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_histogram()+ coord_flip()
print_bars <- con + geom_histogram()+ coord_flip()
ggsave(print_bars, file='PDD-pacific-countries.png', width=10, height=10)

#ggplot code for collections over the years in NZ
c <- subset(PDD.dump, (Country == "New Zealand"))

#also need something that plots monthly e.g. fungi versus collection month.


#======GeoGraphic stuff========

#New Zealand Area codes This is good
nz <- subset(PDD.dump,(Country == "New Zealand"))
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

#======Habitat========

#too many to work
nz <- subset(PDD.dump,(Country == "New Zealand"))
attach(nz) 
require(ggplot2)
p <- ggplot(nz, aes(Habitat)) + labs(title = "PDD specimens by Habitat") + labs(x = "Habitat", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_Habitat.png', width=15, height=49)



#======On Hosts========



#Order of 'microbe' on NZ Myrtaceae in the ICMPt
PDD.dump.Myrtaceae <- subset(PDD.dump.NZ,(Family_C2 == "Myrtaceae"))
ggplot(PDD.dump.Myrtaceae, aes(Order, fill=GenBank)) + #fill by type
  labs(title = "Order of 'microbe' on NZ Myrtaceae in the PDD Fungarium") +
  labs(x = "Order", y = "number of isolates") +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD_Myrtaceae-order.png', width=8, height=15)




#grass pathogens collected over time in PDD

PDD.Gramineae <- PDD.dump.NZ %>%
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
ggsave(file='./ouputs/PDD/PDD-grass-pathogens.png', width=8, height=5)


head(PDD.Gramineae)





#check double numbers

PDD.dump.nohost <- subset(PDD.dump,(Family_C2 == ""))
summary(PDD.dump.nohost$AccessionNumber, maxsum=10)


#======Validations========

#plot higher order taxon against storage location
#Storage location main factor, colour by taxon

#ggplot code for fungal Phylum
require(ggplot2)
ggplot(PDD.dump, aes(FilingNumber, fill=Phylum)) +
  labs(title = "PDD Storage locations") +
  labs(x = "storage location", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD-storagelocation.png', width=10, height=10)

#ggplot code for fungal Phylum
require(ggplot2)
ggplot(PDD.dump, aes(Phylum, fill=FilingNumber)) +
  labs(title = "PDD Storage locations") +
  labs(x = "storage location", y = "number of specimens") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='PDD-storagelocation2.png', width=10, height=10)




