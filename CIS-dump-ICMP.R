## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017)

#============Load and subset data================
ICMP.dump.initial <- read.csv("ICMP-export-16-jan-2020.csv", header=TRUE, sep=",")
head(ICMP.dump.initial)


# subset out viruses
noviruses <- subset(ICMP.dump.initial, (SpecimenType == "Bacterial Culture" | SpecimenType == "Chromist Culture" | SpecimenType == "Fungal Culture" | SpecimenType == "Yeast Culture"))

# subset out "Deaccessioned=True"
ICMP.dump <- subset(noviruses,(Deaccessioned == "false"))
head(ICMP.dump)


#setting up per specimen type subsets, with summaries of each specimen type
ICMP.bacteria <- subset(ICMP.dump,(SpecimenType == "Bacterial Culture"))
summary(ICMP.bacteria, maxsum=40)

ICMP.chromist <- subset(ICMP.dump,(SpecimenType == "Chromist Culture"))
summary(ICMP.chromist, maxsum=40)

ICMP.fungi <- subset(ICMP.dump,(SpecimenType == "Fungal Culture"))
summary(ICMP.fungi, maxsum=40)

ICMP.yeast <- subset(ICMP.dump,(SpecimenType == "Yeast Culture"))
summary(ICMP.yeast, maxsum=40)

#subset New Zealand specimens
ICMP.dump.NZ <- subset(ICMP.dump,(Country == "New Zealand"))
summary(ICMP.dump.NZ, maxsum=40)




#============Quick data check================
#have a quick look at the data
head(ICMP.dump)
h <- head(ICMP.dump)
capture.output(h, file = "ICMP-head.txt")

summary(ICMP.dump.initial, maxsum=25) #data before subsetting
summary(ICMP.dump, maxsum=20) #data after subsetting
s <- summary(ICMP.dump, maxsum=25)
capture.output(s, file = "ICMP-summary.txt")

# counts the number of unique values per collumn for NZ
sapply(ICMP.dump, function(x) length(unique(x)))
u <- sapply(ICMP.dump, function(x) length(unique(x)))
capture.output(u, file = "ICMP-unique-count.txt")

# counts the number of unique values per collumn for NZ
sapply(ICMP.dump.NZ, function(x) length(unique(x)))


#============Type cultures================

#ggplot code for type cultures
d <- subset(ICMP.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus)) + labs(title = "Types in the ICMP") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_types.png', width=10, height=10)

#another one showing just the number of types in each kind of culture?

#ggplot code for type cultures factored by Specimen type
d <- subset(ICMP.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=SpecimenType)) + labs(title = "Types in the ICMP") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP.types.by.kind.png', width=10, height=10)

#ggplot code for type cultures factored by Specimen type
d <- subset(ICMP.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=Images)) + labs(title = "Types in the ICMP") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP.types.with.iamges.png', width=10, height=10)

#ggplot code for type cultures factored by Specimen type
d <- subset(ICMP.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=GenBank)) + labs(title = "Types in ICMP with sequences in GenBank") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP.types.with.sequence.png', width=10, height=10)

#============Kingdom Level barcharts================

#plain code for a kingdom barchart
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType)) + labs(title = "Cultures in the ICMP by Kingdom") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms.png', width=7, height=7)

#kingdoms in GenBank
attach(ICMP.dump)
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=GenBank)) + labs(title = "Cultures in the ICMP in GenBank") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_genbank.png', width=7, height=7)

#kingdoms with literature
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=Literature)) + labs(title = "Cultures in the ICMP in Literature") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_Literature.png', width=7, height=7)

#kingdoms with images
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=Images)) + labs(title = "Cultures in the ICMP with images") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_images.png', width=7, height=7)

#could also do a stacked bar chart with images, genbank, literature all on one chart.

# ** kingdoms by Occurrence Description
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=OccurrenceDescription)) + labs(title = "Cultures in the ICMP by occurrence in NZ") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_occurrence.png', width=7, height=7)

#kingdoms by Order Status
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill= LoanStatus)) + labs(title = "ICMP Order Status") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_ LoanStatus.png', width=7, height=7)

#kingdoms by last updated


ICMP.dump$UpdatedBy3 <- sapply(ICMP.dump$UpdatedBy, tolower)
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill= UpdatedBy3)) + labs(title = "ICMP Last updated by") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_updated_by.png', width=7, height=7)

#need a kingdoms by NZ cultures??


#============High Taxonomy================

# ----- bacterial taxon grouping -----

#ggplot code for bacterial Phylum
attach(ICMP.bacteria) 
require(ggplot2)
p <- ggplot(ICMP.bacteria, aes(Phylum)) + labs(title = "ICMP by bacterial phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_bacteria-phylum.png', width=10, height=10)

#ggplot code for bacterial Class
attach(ICMP.bacteria) 
require(ggplot2)
p <- ggplot(ICMP.bacteria, aes(Class)) + labs(title = "ICMP by bacterial class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_bacteria-class.png', width=10, height=10)

#ggplot code for bacterial Order
attach(ICMP.bacteria) 
require(ggplot2)
p <- ggplot(ICMP.bacteria, aes(Order)) + labs(title = "ICMP by bacterial order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_bacteria-order.png', width=10, height=10)

#ggplot code for bacterial Family
attach(ICMP.bacteria) 
require(ggplot2)
p <- ggplot(ICMP.bacteria, aes(Family)) + labs(title = "ICMP by bacterial family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_bacteria-family.png', width=20, height=10)


# -----  fungal taxon grouping ----- 


#ggplot code for fungal Phylum
attach(ICMP.fungi) 
require(ggplot2)
p <- ggplot(ICMP.fungi, aes(Phylum)) + labs(title = "ICMP by fungal phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_fungal-phylum.png', width=10, height=10)

#ggplot code for fungal Class
attach(ICMP.fungi) 
require(ggplot2)
p <- ggplot(ICMP.fungi, aes(Class)) + labs(title = "ICMP by fungal class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_fungal-class.png', width=10, height=10)

#ggplot code for fungal Order
attach(f) 
require(ggplot2)
p <- ggplot(f, aes(Order)) + labs(title = "ICMP by fungal order") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_fungal-order.png', width=10, height=10)

#ggplot code for fungal Family
attach(ICMP.fungi) 
require(ggplot2)
p <- ggplot(ICMP.fungi, aes(Family)) + labs(title = "ICMP by fungal family") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_fungal-family.png', width=20, height=10)



#============Other names================

#ggplot code for yeast Phylum
attach(ICMP.yeast) 
require(ggplot2)
p <- ggplot(ICMP.yeast, aes(Phylum)) + labs(title = "ICMP by yeast phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_yeast-phylum.png', width=10, height=10)


#ggplot code for chromist Phylum
attach(ICMP.chromist) 
require(ggplot2)
p <- ggplot(ICMP.chromist, aes(Phylum)) + labs(title = "ICMP by chromist phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_chromist-phylum.png', width=10, height=10)





# fungi present in NZ
#names.present.fungi <- subset(ICMP.dump,(Kingdom == "Fungi" & OccurrenceDescription == "Present"))
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

#ggplot code for country
cy <- subset(ICMP.dump,!(Country == ""))
require(ggplot2)
con <- ggplot(cy, aes(Country)) + labs(title = "Top 10 Countries") + labs(x = "Country", y = "number")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_country.png', width=10, height=10)

ICMP.dump
sort(table(ICMP.dump$Country),decreasing=TRUE)[1:11] #top 11 countries

sort(table(ICMP.dump$Country),decreasing=TRUE) #top countries


#ggplot code for top ten countries by specimen type
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Thailand", "China", "Japan", "India",  "Italy")
c <- subset(ICMP.dump, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Thailand" | Country == "China" | Country == "Japan" | Country == "India" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the ICMP") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='ICMP_country_by_kind.png', width=6, height=5)
ggsave(print_bars, file='ICMP_country_by_kind.svg', width=6, height=5)
ggsave(print_bars, file='ICMP_country_by_kind.eps', width=6, height=5)

#ggplot code for pacific country
c <- subset(ICMP.dump, (Country == "Fiji" | Country == "American Samoa" | Country == "Cook Islands" | Country == "Solomon Islands" | Country == "Micronesia" | Country == "New Caledonia" | Country == "Niue" | Country == "Norfolk Island" | Country == "Samoa" | Country == "Vanuatu"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries cultures in the ICMP") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip()
print_bars <- con + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP-pacific-countries.png', width=10, height=10)


#ggplot code for country
positions <- c("United States", "Australia", "United Kingdom", "Brazil", "Japan", "India", "China", "France", "Italy", "Canada")
c <- subset(ICMP.dump, (Country == "Canada" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Japan" | Country == "India" | Country == "France" | Country == "China" | Country == "Italy"))
attach(c) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Top 10 Countries in the ICMP (not including NZ)") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- con + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='ICMP_country_by_kind_not_nz.png', width=10, height=10)


## could make a pseudo dataset manually replaceing all non target countries with "other"


#============Over time================

# also do a trend line of growth. so do a scatterplot and fir a trend line to project growth.
# Do on recived date and check for any blanks
#can do a culmalative graph?


#model for PDD too



attach(ICMP.dump) 
require(ggplot2)
di <- ggplot(ICMP.dump, aes(as.Date(IsolationDateISO, format='%Y-%m-%d'))) + labs(title = "Isolation dates of ICMP cultures") + labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='ICMP-isolation-dates.png', width=5, height=5)
ggsave(dip, file='ICMP-isolation-dates.svg', width=5, height=5)
ggsave(dip, file='ICMP-isolation-dates.eps', width=5, height=5)

attach(ICMP.dump) 
require(ggplot2)
dr <- ggplot(ICMP.dump, aes(as.Date(DepositedDateISO, format='%Y-%m-%d'))) + labs(title = "Date cultures were deposited in ICMP") + labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25)  # this is a bin of two years binwidth=730
drp <- dr + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
drp
ggsave(drp, file='ICMP-deposit-dates.png', width=5, height=5)

attach(ICMP.dump) 
require(ggplot2)
cdr <- ggplot(ICMP.dump, aes(as.Date(CollectionDateISO, format='%Y-%m-%d'))) + labs(title = "Date cultures were collected") + labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") 
cdr <- cdr + scale_x_date()
cdr + geom_histogram(binwidth=365.25)  # this is a bin of two years binwidth=730
cdrp <- cdr + geom_histogram(binwidth=365.25)
cdrp <- cdr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
cdrp
ggsave(cdrp, file='ICMP-Collection-dates.png', width=5, height=5)




ICMP.dump$topcontrib <- ifelse(ICMP.dump$Contributor == "NZP", "NZP", "other")
ICMP.dump$topcontrib


attach(ICMP.dump) 
require(ggplot2)
dr <- ggplot(ICMP.dump, aes(as.Date(ReceivedDateISO, format='%Y-%m-%d'),fill=topcontrib)) + labs(title = "Main Contributors to the ICMP collection") + labs(x = "Date of Receipt", y =  "Number of cultures" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='ICMP-received-dates-contributor.png', width=15, height=10)


ICMP.date <- read.csv("ICMP.date.csv") #i removed all nulls


qplot(factor(as.Date(IsolationDateISO)), data=ICMP.dump, geom="bar") 







attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
di <- ggplot(ICMP.dump, aes(as.Date(IsolationDateISO))) + labs(title = "Isolation dates of ICMP cultures") + labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='ICMP-isolation-dates2.png', width=4, height=3)




ICMP.dump$topcontrib <- ifelse(ICMP.dump$Contributor == "NZP", "NZP", "other")
ICMP.dump$topcontrib

attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
ditc <- ggplot(ICMP.dump, aes(as.Date(IsolationDateISO, format='%Y-%m-%d', fill=Contributor))) + labs(title = "Isolation dates of ICMP cultures") + labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") 
ditc <- ditc + scale_x_date()
ditc + geom_histogram(binwidth=365.25) # this is a bin of two years binwidth=730
ditcp <- ditc + geom_histogram(binwidth=365.25)
ggsave(dip, file='ICMP-isolation-dates2.png', width=4, height=3)




attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(ICMP.dump, aes(as.Date(ReceivedDateISO))) + labs(title = "Received dates of ICMP cultures") + labs(x = "Date of Receipt", y =  "Number of cultures" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='ICMP-received-dates.png', width=10, height=10)

## CAN we do this by organism too?

attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(ICMP.dump, aes(as.Date(ReceivedDateISO),fill=SpecimenType)) + labs(title = "Received dates of ICMP cultures") + labs(x = "Date of Receipt", y =  "Number of cultures" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='ICMP-received-dates-organism.png', width=15, height=10)


sum2 <- ggplot_build(drp) #this extracts the values from the histogram
sum2


attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
dr <- ggplot(ICMP.dump, aes(as.Date(ReceivedDateISO),fill=topcontrib)) + labs(title = "Main Contributors to the ICMP collection") + labs(x = "Date of Receipt", y =  "Number of cultures" , fill = "") #Alternatively, dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date. 
dr <- dr + scale_x_date()
dr + geom_hline(yintercept=392, linetype=3) + geom_histogram(binwidth=365.25)
drp <- dr + geom_histogram(binwidth=365.25) + geom_hline(yintercept=392, linetype=2)
ggsave(drp, file='ICMP-received-dates-contributor.png', width=15, height=10)


ICMP.dump$topcontrib <- ifelse(ICMP.dump$Contributor == "NZP", "NZP", "other")

ICMP.dump$topcontrib

#ggplot code for collections over the years
attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
con <- ggplot(c, aes(Country, fill=SpecimenType)) + labs(title = "Pacific Countries cultures in the ICMP") + labs(x = "Country", y = "number of isolates")
con <- con + theme(axis.text.x=element_text(angle=-90, hjust=0))
con + geom_histogram()+ coord_flip()
print_bars <- con + geom_histogram()+ coord_flip()
ggsave(print_bars, file='ICMP-pacific-countries.png', width=10, height=10)

#ggplot code for collections over the years in NZ
c <- subset(ICMP.dump, (Country == "New Zealand"))

#also need something that plots monthly e.g. fungi versus collection month.


#======georefs========

#New Zealand Area codes
ICMP.nz <- subset(ICMP.dump,(Country == "New Zealand"))
positions <- c("New Zealand", "Campbell Island", "Auckland Islands", "Snares Islands", "Chatham Islands",  "Stewart Island", "Southland", "Fiordland", "Dunedin", "Central Otago", "Otago Lakes", "South Canterbury", "Mackenzie", "Westland", "Mid Canterbury", "North Canterbury", "Buller", "Kaikoura", "Marlborough", "Nelson", "Marlborough Sounds", "South Island", "Wairarapa", "Wellington", "Hawkes Bay", "Rangitikei", "Wanganui", "Gisborne", "Taupo", "Taranaki", "Bay of Plenty", "Waikato", "Coromandel", "Auckland", "Northland", "North Island", "Three Kings Islands", "Kermadec Islands")
attach(ICMP.nz) 
require(ggplot2)
p <- ggplot(ICMP.nz, aes(NZAreaCode)) + labs(title = "ICMP cultures by NZ region") + labs(x = "Crosby Region", y = "number of cultures")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
print_bars <- p + geom_bar()+ coord_flip() + scale_x_discrete(limits = positions)
ggsave(print_bars, file='ICMP_NZAreaCode.png', width=5, height=5)

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
mp

#Now Layer the cities on top
mp <- mp + geom_point(data = ICMP.dump, aes(x = DecimalLong, y = DecimalLat), color = "black", size = 5)

mp


#======On Hosts========

# subset out kiwifruit
ICMP.dump.kiwifruit <- subset(ICMP.dump,(TaxonName_C2 == "Actinidia deliciosa"))
attach(b) 
require(ggplot2)
p <- ggplot(ICMP.dump.kiwifruit, aes(Family)) + labs(title = "Family of microbes on kiwifruit in the ICMP") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kiwifruit-family.png', width=10, height=10)




