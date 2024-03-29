## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2022)

#============Load all the packages needed================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)


#============Load and subset data================

NZAC.as.imported.df <- read_csv("NZAC-export-29-jul-2019.csv",
                               guess_max = Inf, #assign column types
                               show_col_types = FALSE) %>%
  glimpse()


NZAC.df <- NZAC.as.imported.df %>%
  distinct(AccessionNumber, .keep_all= TRUE) %>% #remove dupes
  glimpse()


#============Check imported data for issues================

# get duplicates based due to component duplication
# may need correction in CIS if TaxonName_C2 = NA, export as a CSV

NZAC.dupes <- NZAC.as.imported.df %>%
  get_dupes(AccessionNumber) %>%
  select(AccessionNumber, dupe_count, CurrentName, TaxonName_C2, Substrate_C2, PartAffected_C2) %>%
  #filter(is.na(TaxonName_C2)) %>% #comment this out to get all
  write_csv(file='./outputs/NZAC/NZAC.dupes.csv')



#============Quick data check================
#have a quick look at the data
head(NZAC.df)


#save a summary of the data to txt
NZAC.string.factors <- read.csv("NZAC-export-29-jul-2019.csv",
                               stringsAsFactors = TRUE) %>%
  summary(maxsum=25) %>%
  capture.output(file='./outputs/NZAC/NZAC-summary.txt')


#============Type Specimens================

#ggplot code for type Specimens
d <- subset(NZAC.df,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus)) + labs(title = "Types in the NZAC") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_types.png', width=10, height=10)

#another one showing just the number of types in each kind of culture?

#ggplot code for type Specimens factored by Specimen type
d <- subset(NZAC.df,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=SpecimenType)) + labs(title = "Types in the NZAC") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC.types.by.kind.png', width=10, height=10)

#============Kingdom Level barcharts================

#plain code for a kingdom barchart
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType)) + labs(title = "Specimens in the NZAC by Specimen type") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms.png', width=7, height=7)

#kingdoms in GenBank
attach(NZAC.df)
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType, fill=GenBank)) + labs(title = "Specimens in the NZAC in GenBank") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
#ggsave(print_bars, file='NZAC_kingdoms_genbank.png', width=7, height=7)

#kingdoms with literature
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType, fill=Literature)) + labs(title = "Specimens in the NZAC in Literature") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms_Literature.png', width=7, height=7)

#kingdoms with images
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType, fill=Images)) + labs(title = "Specimens in the NZAC with images") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms_images.png', width=7, height=7)

#could also do a stacked bar chart with images, genbank, literature all on one chart.

#kingdoms by Occurrence Description
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType, fill=OccurrenceDescription)) + labs(title = "Specimens in the NZAC by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms_occurrence.png', width=7, height=7)

#CollectionEventMethod
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(CollectionEventMethod)) + labs(title = "Specimens in the NZAC by Collection Event Method") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_CollectionEventMethod.png', width=7, height=7)

#kingdoms by Occurrence Description
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(OccurrenceDescription)) + labs(title = "Specimens in the NZAC by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms_occurrence2.png', width=7, height=7)



#kingdoms by Order Status
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType, fill= LoanStatus)) + labs(title = "NZAC Order Status") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms_ LoanStatus.png', width=7, height=7)

#kingdoms by last updated by
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(SpecimenType, fill= UpdatedBy)) + labs(title = "NZAC Last updated by") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kingdoms_updated_by.png', width=7, height=7)

#need a kingdoms by NZ Specimens??


#============High Taxonomy================



#Phylum
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(Phylum)) + labs(title = "NZAC by phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_phylum.png', width=10, height=10)

#ggplot code for Class
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(Class)) + labs(title = "NZAC by class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_class.png', width=10, height=10)

#ggplot code for Order
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(Order)) + labs(title = "NZAC by order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_order.png', width=10, height=10)

#ggplot code for Order
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(Order, fill=SpecimenType)) + labs(title = "NZAC by order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_order-speciemtype.png', width=10, height=10)

#ggplot code for Family
attach(NZAC.df) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(Family)) + labs(title = "NZAC by family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
#ggsave(print_bars, file='NZAC_family.png', width=20, height=10)


# -----  fungal taxon grouping ----- 

f <- subset(NZAC.df, SpecimenType == "Fungal Culture")

#ggplot code for fungal Phylum
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Phylum)) + labs(title = "NZAC by fungal phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_fungal-phylum.png', width=10, height=10)

#ggplot code for fungal Class
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Class)) + labs(title = "NZAC by fungal class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_fungal-class.png', width=10, height=10)

#ggplot code for fungal Order
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Order)) + labs(title = "NZAC by fungal order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_fungal-order.png', width=10, height=10)

#ggplot code for fungal Family
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Family)) + labs(title = "NZAC by fungal family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_fungal-family.png', width=20, height=10)

#============Other names================

# error Kingdom is missing
names.present.fungi <- subset(NZAC.df,(Kingdom == "Fungi" & OccurrenceDescription == "Present"))
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


NZAC.df.NZ <- subset(NZAC.df,(Country == "New Zealand"))
attach(NZAC.df.NZ) 
require(ggplot2)
p <- ggplot(NZAC.df, aes(NZAreaCode)) + labs(title = "NZ Specimens in the NZAC by Area Code") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_NZAreaCode.png', width=7, height=7)




#ggplot code for country
cy <- subset(NZAC.df,!(Country == ""))
require(ggplot2)
con <- ggplot(cy, aes(Country)) + labs(title = "Top 10 Countries") + labs(x = "Country", y = "number")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_country.png', width=10, height=10)


#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "Italy", "France")
c <- subset(NZAC.df, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the NZAC") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='NZAC_country_by_kind.png', width=6, height=5)
ggsave(print_bars, file='NZAC_country_by_kind.svg', width=6, height=5)
ggsave(print_bars, file='NZAC_country_by_kind.eps', width=6, height=5)

#ggplot code for pacific country
c <- subset(NZAC.df, (Country == "Fiji" | Country == "American Samoa" | Country == "Cook Islands" | Country == "Solomon Islands" | Country == "Micronesia" | Country == "New Caledonia" | Country == "Niue" | Country == "Norfolk Island" | Country == "Samoa" | Country == "Vanuatu"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the NZAC") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC-pacific-countries.png', width=10, height=10)


#ggplot code for country
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "France", "Italy", "Canada")
c <- subset(NZAC.df, (Country == "Canada" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the NZAC (not including NZ)") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='NZAC_country_by_kind_not_nz.png', width=10, height=10)


## could make a pseudo dataset manually replaceing all non target countries with "other"


#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?


attach(NZAC.df) 
require(ggplot2)
di <- ggplot(NZAC.df, aes(as.Date(CollectionDateISO))) + labs(title = "Collection dates of NZAC Specimens") + labs(x = "Date of collection", y =  "Number of Specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='NZAC-collection-dates.png', width=5, height=5)
ggsave(dip, file='NZAC-collection-dates.svg', width=5, height=5)
ggsave(dip, file='NZAC-collection-dates.eps', width=5, height=5)




NZAC.df$topcontrib <- ifelse(NZAC.df$Contributor == "NZP", "NZP", "other")
NZAC.df$topcontrib

attach(NZAC.df) #this means we don't need the $ sign
require(ggplot2)
ditc <- ggplot(NZAC.df, aes(as.Date(IsolationDateISO, fill=Contributor))) + labs(title = "Isolation dates of NZAC Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
ditc <- ditc + scale_x_date()
ditc + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
ditcp <- ditc + geom_histogram(binwidth=365.25)
ggsave(dip, file='NZAC-isolation-dates2.png', width=4, height=3)




attach(NZAC.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(NZAC.df, aes(as.Date(ReceivedDateISO))) + labs(title = "Received dates of NZAC Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='NZAC-received-dates.png', width=10, height=10)

## CAN we do this by organism too?

attach(NZAC.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(NZAC.df, aes(as.Date(ReceivedDateISO),fill=SpecimenType)) + labs(title = "Received dates of NZAC Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='NZAC-received-dates-organism.png', width=15, height=10)


sum2 <- ggplot_build(drp) #this extracts the values from the histogram
sum2


attach(NZAC.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(NZAC.df, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the NZAC collection") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='NZAC-received-dates-contributor.png', width=15, height=10)


NZAC.df$topcontrib <- ifelse(NZAC.df$Contributor == "NZP", "NZP", "other")

NZAC.df$topcontrib

#ggplot code for collections over the years
attach(NZAC.df) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the NZAC") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_histogram()+ coord_flip()
print_bars <- con + geom_histogram()+ coord_flip()
ggsave(print_bars, file='NZAC-pacific-countries.png', width=10, height=10)

#ggplot code for collections over the years in NZ
c <- subset(NZAC.df, (Country == "New Zealand"))

#also need something that plots monthly e.g. fungi versus collection month.


#======MAPS========

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp


#======On Hosts========

# subset out kiwifruit
NZAC.df.kiwifruit <- subset(NZAC.df,(TaxonName_C2 == "Actinidia deliciosa"))

#ggplot code for bacterial Class
attach(b) 
require(ggplot2)
p <- ggplot(NZAC.df.kiwifruit, aes(Family)) + labs(title = "Family of microbes on kiwifruit in the NZAC") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='NZAC_kiwifruit-family.png', width=10, height=10)




