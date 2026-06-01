## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017)

## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2022)

#============Load all the packages needed================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)
#library(svglite)


#============Load data================

CHR.as.imported.df <- read_csv("CIS_CHR export 1 Jul 2025.csv",
                               guess_max = Inf, #assign column types
                               show_col_types = FALSE) |>
  glimpse()


#============Check imported data for issues================

# get duplicates based due to component duplication
# may need correction in CIS if TaxonName_C2 = NA, export as a CSV

CHR.dupes <- CHR.as.imported.df  |>
  get_dupes(AccessionNumber) |>
  select(AccessionNumber, dupe_count, CurrentNamePart_C1, TaxonName_C2, Substrate_C2, PartAffected_C2) %>%
  filter(is.na(TaxonName_C2)) |> #comment this out to get all
  write_csv(file='./outputs/CHR/CHR.dupes.csv')


#============Subset and massage the Data================

CHR.df <- CHR.as.imported.df |>
  distinct(AccessionNumber, .keep_all= TRUE) |> #remove dupes
  glimpse()

#============Unwanted orgs================

#Need to pull in the MPI lists and check

# 1. Load ICMP data and extract "Unwanted Organism"
unwanted_CHR <- CHR.df |> 
  select(AccessionNumber, SpecimenType, CurrentNamePart_C1, SpecimenFlags) |> 
  mutate(SpecimenFlagExtract = str_extract(SpecimenFlags, "Unwanted Organism"))

# 2. Load MPI unwanted organism list
mpi_unwanted <- read_csv("MPI-Pest-Register-29-June-2025.csv")

# 3. Filter ICMP records where CurrentNamePart_C1 matches any Pest name,
#    but the SpecimenFlagExtract is NA (i.e. not marked as 'Unwanted Organism')
results <- unwanted_CHR |> 
  filter(CurrentNamePart_C1 %in% mpi_unwanted$`Pest name`,
         is.na(SpecimenFlagExtract)) |> 
  arrange(CurrentNamePart_C1)

# EXtra check try synonyms
synonyms_results <- unwanted_CHR |> 
  filter(CurrentNamePart_C1 %in% mpi_unwanted$`Scientific name(s)`, 
         is.na(SpecimenFlagExtract)) |> 
  arrange(CurrentNamePart_C1)

# View results
print(results, n=40)
print(synonyms_results, n=40)

# 4. Export results to CSV
write_csv(results, "./outputs/CHR/CHR_unflagged_unwanted_organisms.csv")
write_csv(synonyms_results, "./outputs/CHR/CHR_unflagged_unwanted_organisms_synonyms.csv")

#============Type Specimens================

#ggplot code for type Specimens
d <- subset(CHR.df,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus)) + labs(title = "Types in the CHR") + labs(x = "'Kind' of type", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='./outputs/CHR/CHR_types.png', width=10, height=10)

#another one showing just the number of types in each kind of culture?

#ggplot code for type Specimens factored by Specimen type
d <- subset(CHR.df,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=SpecimenType)) + labs(title = "Types in the CHR") + labs(x = "'Kind' of type", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='./outputs/CHR/CHR.types.by.kind.png', width=10, height=10)

#============Kingdom Level barcharts================

#plain code for a kingdom barchart
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(SpecimenType)) + labs(title = "Specimens in the CHR by Specimen type") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms.png', width=7, height=7)

#kingdoms in GenBank
attach(CHR.df)
require(ggplot2)
p <- ggplot(CHR.df, aes(GenBank)) + labs(title = "Specimens in the CHR in GenBank") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
#ggsave(print_bars, file='CHR_kingdoms_genbank.png', width=7, height=7)

#kingdoms with literature
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(SpecimenType, fill=Literature)) + labs(title = "Specimens in the CHR in Literature") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms_Literature.png', width=7, height=7)

#kingdoms with images
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(SpecimenType, fill=Images)) + labs(title = "Specimens in the CHR with images") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms_images.png', width=7, height=7)

#could also do a stacked bar chart with images, genbank, literature all on one chart.

#kingdoms by Occurrence Description
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(SpecimenType, fill=OccurrenceDescription)) + labs(title = "Specimens in the CHR by occurrence in NZ") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms_occurrence.png', width=7, height=7)

#CollectionEventMethod
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(CollectionEventMethod)) + labs(title = "Specimens in the CHR by Collection Event Method") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_CollectionEventMethod.png', width=7, height=7)

#kingdoms by Occurrence Description
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(OccurrenceDescription)) + labs(title = "Specimens in the CHR by occurrence in NZ") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms_occurrence2.png', width=7, height=7)



#kingdoms by Order Status
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(SpecimenType, fill= LoanStatus)) + labs(title = "CHR Order Status") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms_ LoanStatus.png', width=7, height=7)

#kingdoms by last updated by
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(SpecimenType, fill= UpdatedBy)) + labs(title = "CHR Last updated by") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kingdoms_updated_by.png', width=7, height=7)

#need a kingdoms by NZ Specimens??


#============High Taxonomy================



#Phylum
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(Phylum_C1)) + labs(title = "CHR by phylum") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='./outputs/CHR/CHR_phylum.png', width=10, height=10)

#ggplot code for Class
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(Class_C1)) + labs(title = "CHR by class") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='./outputs/CHR/CHR_class.png', width=10, height=10)

#ggplot code for Order
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(Order)) + labs(title = "CHR by order") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_order.png', width=10, height=10)

#ggplot code for Order
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(Order, fill=SpecimenType)) + labs(title = "CHR by order") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_order-speciemtype.png', width=10, height=10)

#ggplot code for Family
attach(CHR.df) 
require(ggplot2)
p <- ggplot(CHR.df, aes(Family)) + labs(title = "CHR by bacterial family") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
#ggsave(print_bars, file='CHR_family.png', width=20, height=10)


# -----  fungal taxon grouping ----- 

f <- subset(CHR.df, SpecimenType == "Fungal Culture")

#ggplot code for fungal Phylum
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Phylum)) + labs(title = "CHR by fungal phylum") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_fungal-phylum.png', width=10, height=10)

#ggplot code for fungal Class
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Class)) + labs(title = "CHR by fungal class") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_fungal-class.png', width=10, height=10)

#ggplot code for fungal Order
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Order)) + labs(title = "CHR by fungal order") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_fungal-order.png', width=10, height=10)

#ggplot code for fungal Family
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Family)) + labs(title = "CHR by fungal family") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_fungal-family.png', width=20, height=10)

#============Other names================

# error Kingdom is missing
names.present.fungi <- subset(CHR.df,(Kingdom == "Fungi" & OccurrenceDescription == "Present"))
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


CHR.df.NZ <- subset(CHR.df,(Country == "New Zealand"))
attach(CHR.df.NZ) 
require(ggplot2)
p <- ggplot(CHR.df, aes(NZAreaCode)) + labs(title = "NZ Specimens in the CHR by Area Code") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_NZAreaCode.png', width=7, height=7)




#ggplot code for country
cy <- subset(CHR.df,!(Country == ""))
require(ggplot2)
con <- ggplot(cy, aes(Country)) + labs(title = "Top 10 Countries") + labs(x = "Country", y = "number")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_country.png', width=10, height=10)


#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "Italy", "France")
c <- subset(CHR.df, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the CHR") + labs(x = "Country", y = "number of specimens")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='CHR_country_by_kind.png', width=6, height=5)
ggsave(print_bars, file='CHR_country_by_kind.svg', width=6, height=5)
ggsave(print_bars, file='CHR_country_by_kind.eps', width=6, height=5)

#ggplot code for pacific country
c <- subset(CHR.df, (Country == "Fiji" | Country == "American Samoa" | Country == "Cook Islands" | Country == "Solomon Islands" | Country == "Micronesia" | Country == "New Caledonia" | Country == "Niue" | Country == "Norfolk Island" | Country == "Samoa" | Country == "Vanuatu"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the CHR") + labs(x = "Country", y = "number of specimens")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR-pacific-countries.png', width=10, height=10)


#ggplot code for country
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "France", "Italy", "Canada")
c <- subset(CHR.df, (Country == "Canada" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the CHR (not including NZ)") + labs(x = "Country", y = "number of specimens")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='CHR_country_by_kind_not_nz.png', width=10, height=10)


## could make a pseudo dataset manually replaceing all non target countries with "other"


#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?






attach(CHR.df) 
require(ggplot2)
di <- ggplot(CHR.df, aes(as.Date(CollectionDateISO))) + labs(title = "Isolation dates of CHR Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='CHR-isolation-dates.png', width=5, height=5)
ggsave(dip, file='CHR-isolation-dates.svg', width=5, height=5)
ggsave(dip, file='CHR-isolation-dates.eps', width=5, height=5)

attach(CHR.df) 
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO))) + labs(title = "REC dates of CHR Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25)  # this is a bin of two years binwidth=730
drp <- dr + geom_histogram(binwidth=365.25)
ggsave(dip, file='CHR-isolation-dates.png', width=5, height=5)

attach(CHR.df) 
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO))) + labs(title = "REC dates of CHR Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
dr <- dr + scale_x_date()
dr + geom_line()  # this is a bin of two years binwidth=730
drp <- dr + geom_histogram(binwidth=365.25)
ggsave(dip, file='CHR-isolation-dates.png', width=5, height=5)


attach(CHR.df) 
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO))) + labs(title = "REC dates of CHR Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
dr <- dr + scale_x_date()
dr + geom_point
drp <- dr + geom_histogram(binwidth=365.25)
ggsave(dip, file='CHR-isolation-dates.png', width=5, height=5)


CHR.df$topcontrib <- ifelse(CHR.df$Contributor == "NZP", "NZP", "other")
CHR.df$topcontrib


attach(CHR.df) 
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the CHR collection") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='CHR-received-dates-contributor.png', width=15, height=10)


CHR.date <- read.csv("CHR.date.csv") #i removed all nulls


qplot(factor(as.Date(IsolationDateISO)), data=CHR.df, geom="bar") 







attach(CHR.df) #this means we don't need the $ sign
require(ggplot2)
di <- ggplot(CHR.df, aes(as.Date(IsolationDateISO))) + labs(title = "Isolation dates of CHR Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='CHR-isolation-dates2.png', width=4, height=3)




CHR.df$topcontrib <- ifelse(CHR.df$Contributor == "NZP", "NZP", "other")
CHR.df$topcontrib

attach(CHR.df) #this means we don't need the $ sign
require(ggplot2)
ditc <- ggplot(CHR.df, aes(as.Date(IsolationDateISO, fill=Contributor))) + labs(title = "Isolation dates of CHR Specimens") + labs(x = "Date of isolation", y =  "Number of Specimens" , fill = "") 
ditc <- ditc + scale_x_date()
ditc + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
ditcp <- ditc + geom_histogram(binwidth=365.25)
ggsave(dip, file='CHR-isolation-dates2.png', width=4, height=3)




attach(CHR.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO))) + labs(title = "Received dates of CHR Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='CHR-received-dates.png', width=10, height=10)

## CAN we do this by organism too?

attach(CHR.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO),fill=SpecimenType)) + labs(title = "Received dates of CHR Specimens") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='CHR-received-dates-organism.png', width=15, height=10)


sum2 <- ggplot_build(drp) #this extracts the values from the histogram
sum2


attach(CHR.df) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(CHR.df, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the CHR collection") + labs(x = "Date of Receipt", y =  "Number of Specimens" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='CHR-received-dates-contributor.png', width=15, height=10)


CHR.df$topcontrib <- ifelse(CHR.df$Contributor == "NZP", "NZP", "other")

CHR.df$topcontrib

#ggplot code for collections over the years
attach(CHR.df) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries Specimens in the CHR") + labs(x = "Country", y = "number of specimens")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_histogram()+ coord_flip()
print_bars <- con + geom_histogram()+ coord_flip()
ggsave(print_bars, file='CHR-pacific-countries.png', width=10, height=10)

#ggplot code for collections over the years in NZ
c <- subset(CHR.df, (Country == "New Zealand"))

#also need something that plots monthly e.g. fungi versus collection month.


#======MAPS========

#New Zealand Area codes
CHR.nz <- subset(CHR.df,(Country == "New Zealand"))
attach(CHR.nz) 
require(ggplot2)
p <- ggplot(CHR.nz, aes(NZAreaCode)) + labs(title = "CHR specimens by NZ region") + labs(x = "Crosby Region", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_NZAreaCode.png', width=5, height=5)

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp


#======On Hosts========

# subset out kiwifruit
CHR.df.kiwifruit <- subset(CHR.df,(TaxonName_C2 == "Actinidia deliciosa"))

#ggplot code for bacterial Class
attach(b) 
require(ggplot2)
p <- ggplot(CHR.df.kiwifruit, aes(Family)) + labs(title = "Family of microbes on kiwifruit in the CHR") + labs(x = "Taxon", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='CHR_kiwifruit-family.png', width=10, height=10)




