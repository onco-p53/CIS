## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017)

#============Load and subset data================
PDD.dump <- read.csv("PDD-export-6-feb-2019.csv", header=TRUE, sep=",")

# subset out "Deaccessioned=True", not implemented
# PDD.dump <- subset(noviruses,(Deaccessioned == "FALSE"))

#setting up per specimen type subsets, with summaries of each specimen type
PDD.alcohol <- subset(PDD.dump,(SpecimenType == "Alcohol"))
summary(PDD.alcohol, maxsum=40)

#============Quick data check================
#have a quick look at the data
head(PDD.dump)

# summary(PDD.dump.initial, maxsum=25) #data before subsetting, not implemented
summary(PDD.dump, maxsum=25) #data after subsetting

s <- summary(PDD.dump, maxsum=25)
capture.output(s, file = "PDD-summary.txt")

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
p <- ggplot(d, aes(TypeStatus, fill=SpecimenType)) + labs(title = "Types in the PDD") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD.types.by.kind.png', width=5, height=5)

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


#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?

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

#New Zealand Area codes
nz <- subset(PDD.dump,(Country == "New Zealand"))
attach(nz) 
require(ggplot2)
p <- ggplot(nz, aes(NZAreaCode)) + labs(title = "PDD specimens by NZ region") + labs(x = "Crosby Region", y = "number of specimens")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_NZAreaCode.png', width=5, height=5)


#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp


#======On Hosts========

# subset out kiwifruit
PDD.dump.kiwifruit <- subset(PDD.dump,(TaxonName_C2 == "Actinidia deliciosa"))

#ggplot code for bacterial Class
attach(b) 
require(ggplot2)
p <- ggplot(PDD.dump.kiwifruit, aes(Family)) + labs(title = "Family of microbes on kiwifruit in the PDD") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='PDD_kiwifruit-family.png', width=10, height=10)

#======On Hosts========










