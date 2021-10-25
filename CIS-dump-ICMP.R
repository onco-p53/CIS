## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017-2021)

#============Load data================

R.version.string

ICMP.dump.initial <- read.csv("ICMP-export-21-oct-2021.csv", header=TRUE, sep=",")
head(ICMP.dump.initial)
summary(ICMP.dump.initial, maxsum=10)
str(ICMP.dump.initial)

#============Load all the packages needed================

library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer) # notes here: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/

display.brewer.all(colorblindFriendly = TRUE)
display.brewer.all(colorblindFriendly = FALSE)

# OK ones are Paired if you have heaps of data. Others are: Set2

#============Sub set the Data================

#tidyverse way of sub setting - remove viruses and deaccessioned cultures
ICMP.dump <- ICMP.dump.initial %>%
  filter(SpecimenType == "Bacterial Culture" | 
           SpecimenType == "Chromist Culture" | 
           SpecimenType == "Fungal Culture" | 
           SpecimenType == "Yeast Culture") %>%
  filter(Deaccessioned == "false")


#setting up per specimen type subsets, with summaries of each specimen type
ICMP.bacteria <- subset(ICMP.dump,(SpecimenType == "Bacterial Culture"))
summary(ICMP.bacteria, maxsum=40)
table(ICMP.bacteria$Phylum) #this is a validation check

ICMP.chromist <- subset(ICMP.dump,(SpecimenType == "Chromist Culture"))
summary(ICMP.chromist, maxsum=40)
table(ICMP.chromist$Phylum) #this is a validation check

ICMP.fungi <- subset(ICMP.dump,(SpecimenType == "Fungal Culture"))
summary(ICMP.fungi, maxsum=40)
table(ICMP.fungi$Phylum) #this is a validation check

ICMP.yeast <- subset(ICMP.dump,(SpecimenType == "Yeast Culture"))
summary(ICMP.yeast, maxsum=40)
table(ICMP.yeast$Phylum) #this is a validation check

ICMP.types <- subset(ICMP.dump,!(TypeStatus == ""))
summary(ICMP.types, maxsum=40)

#subset New Zealand specimens
ICMP.dump.NZ <- subset(ICMP.dump,(Country == "New Zealand"))
summary(ICMP.dump.NZ, maxsum=40)
head(ICMP.dump.NZ)

#counting various things

count(ICMP.dump.initial,SpecimenType)
count(ICMP.dump,SpecimenType)
as.integer( count(ICMP.dump) )

count(ICMP.dump,SpecimenSecurityLevelText)


ICMP.dump %>%
  count(SpecimenType,TypeStatus) %>%
  group_by(SpecimenType) %>%
  group_by(TypeStatus, .add=TRUE) 



summarise(ICMP.dump, c(SpecimenType,TypeStatus))

ICMP.dump %>% 
  group_by(SpecimenType)


ICMP.dump %>% 
  add_tally(wt = TypeStatus) %>%
  group_by(SpecimenType) %>%
  tally()





#============Quick data check================
#have a quick look at the data
head(ICMP.dump)
h <- head(ICMP.dump)
capture.output(h, file = "ICMP-head.txt")

summary(ICMP.dump.initial, maxsum=25) #data before subsetting
summary(ICMP.dump, maxsum=20) #data after subsetting

capture.output(s, file = "ICMP-summary.txt")

# counts the number of unique values per column
sapply(ICMP.dump, function(x) length(unique(x)))
u <- sapply(ICMP.dump, function(x) length(unique(x)))
capture.output(u, file = "ICMP-unique-count.txt")

# counts the number of unique values per column for NZ
sapply(ICMP.dump.NZ, function(x) length(unique(x)))

#============General stats================

# Total number of each organism

table(ICMP.dump$SpecimenType)
table(ICMP.dump.NZ$SpecimenType)

# Total number of each types for each organism

?? as.numeric(length(unique(ICMP.dump$SpecimenType)))
?? lengths(lapply(ICMP.dump$SpecimenType, unique))


#this is useless for this dataset but could be ok for others
library(psych)
describeBy(
  ICMP.dump,
  ICMP.dump$SpecimenType # grouping variable
)



#============Type cultures================

#sorting plots:
#  https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html


ICMP.types.sorted <- ICMP.types %>%
  arrange(TypeStatus) %>%    # First sort by TypeStatus. This sort the dataframe but NOT the factor levels
  mutate(TypeStatus=factor(TypeStatus, levels=TypeStatus)) %>%   # This trick update the factor levels

#maybe need to make a count and make a new column?  
  
head(ICMP.types)
head(ICMP.types.sorted)

#barchart of all ICMP types sorted by 'kind' of type
ggplot(ICMP.types, aes(TypeStatus)) +
  labs(title = "Types in the ICMP") +
  labs(x = "'Kind' of type", y = "number of isolates") +
  geom_bar() + 
  coord_flip()
ggsave(file='ICMP_types.png', width=10, height=10)

## KEY CHART ##
#barchart of all ICMP types sorted by 'kind' of type, coloured by kind of organism
positions <- c("Type strain", "Pathotype strain", "Neopathotype strain", "Type", "ex Type",  "Holotype", "ex Holotype", "Paratype", "ex Paratype", "Isotype", "Neotype", "ex Neotype", "Epitype", "ex Epitype", "Syntype", "Reference strain")
ggplot(ICMP.types, aes(TypeStatus, fill=SpecimenType)) +
  labs(title = "Types in the ICMP culture collection") +
  labs(x = "'Kind' of type", y = "number of cultures") +
  geom_bar() + 
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(limits = positions)
ggsave(file='ICMP.types.by.kind.png', width=10, height=10)


#barchart of all ICMP types sorted by 'kind' of type, with genbank status
ggplot(ICMP.types, aes(SpecimenType, fill=GenBank)) +
  labs(title = "Types in the ICMP with a GenBank seqeunce") +
  labs(x = "'Kind' of type", y = "number of isolates") +
  geom_bar() + 
  coord_flip()
ggsave(file='ICMP_types-by-SpecimenType.png', width=10, height=10)


#Types in the ICMP with images
d <- subset(ICMP.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=Images)) + labs(title = "Types in the ICMP with images") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP.types.with.iamges.png', width=10, height=10)

#Types in ICMP with sequences in GenBank
d <- subset(ICMP.dump,!(TypeStatus == ""))
attach(d) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(d, aes(TypeStatus, fill=GenBank)) + labs(title = "Types in ICMP with sequences in GenBank") + labs(x = "'Kind' of type", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP.types.with.sequence.png', width=10, height=10)

#============PIE CHART!================

#this is borked code, but if sorted correctly could be a visual map of deposits over time
#sort would have to strip ICMP number
sort(table(ICMP.dump$AccessionNumber),decreasing=TRUE)
ggplot(ICMP.dump, aes(x="", y=AccessionNumber, fill=SpecimenType))+
  geom_bar(width = 1, stat = "identity")

#pie chart by specimen type
ggplot(ICMP.dump, aes(x=factor(1), fill=SpecimenType)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='ICMP_specimen_pie.png', width=5, height=5)


#pie chart by last updated user
ICMP.dump$LastUpdatedBy <- sapply(ICMP.dump$UpdatedBy, tolower)

ggplot(ICMP.dump, aes(x=factor(1), fill=LastUpdatedBy)) +
  labs(title = "Last User to update an ICMP record") +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP_last-updated_pie.png', width=5, height=5)


#============Extended specimen data================

#install gridextra
library(gridExtra)

#GenBank status by kingdom
genbank.plot <- ggplot(ICMP.dump, aes(SpecimenType, fill=GenBank)) +
  labs(title = "Cultures in the ICMP in GenBank") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
ggsave(print_bars, file='extended-specimen-genbank.png', width=7, height=7)

#GenBank status by kingdom
literature.plot <- ggplot(ICMP.dump, aes(SpecimenType, fill=Literature)) +
  labs(title = "Cultures in the ICMP with a citation in literature") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
ggsave(print_bars, file='extended-specimen-literature.png', width=7, height=7)

#GenBank status by kingdom
image.plot <- ggplot(ICMP.dump, aes(SpecimenType, fill=Images)) +
  labs(title = "Cultures in the ICMP with images") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
ggsave(print_bars, file='extended-specimen-images.png', width=7, height=7)


grid.arrange(genbank.plot, literature.plot, image.plot, nrow = 2, ncol = 2)





#here want to have:
#genbank
#literature
#iamge
#create a subset of these

#============Kingdom Level barcharts================

#plain code for a kingdom barchart
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType)) + labs(title = "Cultures in the ICMP by Kingdom") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms.png', width=7, height=7)




#GenBank status by kingdom
ggplot(ICMP.dump, aes(SpecimenType, fill=GenBank)) +
  labs(title = "Cultures in the ICMP in GenBank") +
  labs(x = "Taxon", y = "number of isolates") +
  scale_fill_brewer(palette = "Paired") +
  geom_bar()
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

#kingdoms by Occurrence in NZ
attach(ICMP.dump) 
require(ggplot2)
ggplot(ICMP.dump, aes(SpecimenType, fill=OccurrenceDescription)) +
  labs(title = "Cultures in the ICMP by occurrence in NZ") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() #+
#  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP_kingdoms_occurrence.png', width=7, height=7)





#kingdoms by Order Status
attach(ICMP.dump) 
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill= LoanStatus)) + labs(title = "ICMP Order Status") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_ LoanStatus.png', width=7, height=7)

#kingdoms by last updated
#need to filter out low users and just as a bar or pie graph?
ICMP.dump$UpdatedBy3 <- sapply(ICMP.dump$UpdatedBy, tolower)
ggplot(ICMP.dump, aes(SpecimenType, fill= UpdatedBy3)) +
  labs(title = "ICMP Last updated by") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP_kingdoms_updated_by.png', width=7, height=7)

#need a kingdoms by NZ cultures??




#============High Taxonomy================

# ----- bacterial taxon grouping -----

sort(table(ICMP.dump$Family),decreasing=TRUE)[1:11] #top 11 families
sort(table(ICMP.dump.NZ$Family),decreasing=TRUE)[1:11] #top 11 NZ families


ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")





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
ggplot(ICMP.bacteria, aes(Order))
  labs(title = "ICMP by bacterial order") +
  labs(x = "Taxon", y = "number of isolates") +
#  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='ICMP_bacteria-order.png', width=10, height=10)

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
ggplot(ICMP.fungi, aes(Phylum)) +
  labs(title = "ICMP by fungal phylum") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='ICMP_fungal-phylum.png', width=10, height=10)


#ggplot code for fungal Phylum
ggplot(ICMP.fungi, aes(Phylum)) +
  labs(title = "ICMP by fungal phylum") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar(position="fill") +
  coord_flip()
ggsave(file='ICMP_fungal-phylum-stracked.png', width=10, height=10)





#ggplot code for fungal Class
attach(ICMP.fungi) 
require(ggplot2)
p <- ggplot(ICMP.fungi, aes(Class)) + labs(title = "ICMP by fungal class") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_fungal-class.png', width=10, height=10)

#ggplot code for fungal Order
attach(ICMP.fungi) 
require(ggplot2)
p <- ggplot(ICMP.fungi, aes(Order)) + labs(title = "ICMP by fungal order") + labs(x = "Taxon", y = "number of isolates")
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
positions <- c("New Zealand", "United States", "Australia", "United Kingdom", "Brazil", "Japan", "Thailand", "China", "India",  "Italy")
ICMP.10county <- subset(ICMP.dump, (Country == "New Zealand" | Country == "United States" | Country == "Australia" | Country == "United Kingdom" | Country == "Brazil" | Country == "Thailand" | Country == "China" | Country == "Japan" | Country == "India" | Country == "Italy"))

ggplot(ICMP.10county, aes(Country, fill=SpecimenType)) +
  labs(title = "Top 10 Countries in the ICMP") +
  labs(x = "Country", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_x_discrete(limits = positions)
ggsave(file='ICMP_country_by_kind.png', width=8, height=4.5)


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
# Do on received date and check for any blanks
#can do a cumulative graph?

#new month ICMP culture isolated (in NZ)
attach(ICMP.dump.NZ) 
require(ggplot2)
require(lubridate)
month.isolated <- ymd(ICMP.dump.NZ$IsolationDateISO, truncated = 1)
mergemonths <- floor_date(month.isolated, unit = "month")
ggplot(ICMP.dump.NZ, aes(month(mergemonths, label = TRUE), fill = SpecimenType)) +
  labs(title = "Isolation month of ICMP cultures from NZ") + 
  labs(x = "Month of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  geom_bar() + 
  scale_x_discrete(na.translate = FALSE)  # this removes NAs
ggsave(file='ICMP-isolation-month.png', width=8, height=5)


#standard all ICMP overtime stats
attach(ICMP.dump) 
require(ggplot2)
require(lubridate)
date.isolated <-ymd(ICMP.dump$IsolationDateISO, truncated = 1)
ggplot(ICMP.dump, aes(date.isolated, fill = SpecimenType)) +
  labs(title = "Isolation dates of ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = c(0.1, 0.8)) +
  geom_histogram(binwidth=365.25)  # this is a bin of two years: binwidth=730
ggsave(file='ICMP-isolation-dates2.png', width=8, height=5)

#standard all ICMP overtime stats faceted
attach(ICMP.dump) 
require(ggplot2)
require(lubridate)
date.isolated <-ymd(ICMP.dump$IsolationDateISO, truncated = 1)
ggplot(ICMP.dump, aes(date.isolated, fill = SpecimenType)) +
  labs(title = "Isolation dates of ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(binwidth=365.25, show.legend = FALSE) + # this is a bin of two years: binwidth=730
  facet_grid(SpecimenType ~ .)
ggsave(file='ICMP-isolation-dates-facet.png', width=8, height=5)



arrange(date.isolated)

library(tidyverse)
arrange(ICMP.dump, date.isolated) #this is the earliest isolated culture
arrange(ICMP.dump.NZ, month.isolated) #this is the earliest isolated NZ culture


#deposit dates for bacteria
attach(ICMP.bacteria) 
require(ggplot2)
ggplot(ICMP.bacteria, aes(as.Date(DepositedDateISO, format='%Y-%m-%d'))) +
  labs(title = "Date bacterial cultures were deposited in ICMP") +
  labs(x = "Date of deposit", y =  "Number of cultures" , fill = "") +
  scale_x_date() +
  geom_histogram(binwidth=365.25) +  # this is a bin of two years binwidth=730
  geom_hline(yintercept=392, linetype=2)
ggsave(file='ICMP-deposit-dates-bacteria.png', width=5, height=5)

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
ggplot(ICMP.nz, aes(NZAreaCode)) +
  labs(title = "ICMP cultures by NZ region") +
  labs(x = "Crosby Region", y = "number of cultures") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip() +
  scale_x_discrete(limits = positions)
ggsave(file='ICMP_NZAreaCode.png', width=8, height=4.5)


require(ggplot2)
require(ggmap)
require(maps)
require(mapdata)




world <- map_data("world")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = ICMP.dump, aes(x = DecimalLong, y = DecimalLat), color = "red", size = 1, alpha = 0.2) +
  #geom_text(data = ICMP.dump, aes(x = DecimalLong, y = DecimalLat, label = ""), hjust = -0.2, size = 1, color = "black") + #, angle = 45
  coord_fixed(1.3)
ggsave(file='ICMP_worldmap_labels.png', width=40, height=20)



#the map has an issue as a work around remove the Chatham islands

ICMP.no.chat <- ICMP.dump.NZ %>%
  filter(NZAreaCode != "Chatham Islands") %>%
  filter(NZAreaCode != "Kermadec Islands")

nz <- map_data("nzHires")
ggplot() + 
  geom_polygon(data = nz, aes(x=long, y = lat, group = group), fill = "grey") +
  theme_void() +
  geom_point(data = ICMP.no.chat, aes(x = DecimalLong, y = DecimalLat), color = "red", size = 1, alpha = 0.2) +
  #  geom_text(data = ICMP.no.chat, position=position_jitter(width=1,height=15), aes(x = DecimalLong, y = DecimalLat, label = AccessionNumber), hjust = -0.2, size = 3, color = "black") + #, angle = 45
  coord_fixed(1.3)




#to make the Chatham islands work you need to fudge them over 180 e.g. 183.41667
#will have to melt and add the fudge factor somehow to only chatham islands ones



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




#======On Hosts========


#kiwifruit
require(ggplot2)
ICMP.dump.kiwifruit <- subset(ICMP.bacteria,(Family_C2 == "Actinidiaceae" ))
ggplot(ICMP.dump.kiwifruit, aes(Family)) +
  labs(title = "Family of microbes on kiwifruit in the ICMP") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(print_bars, file='ICMP_kiwifruit-family.png', width=10, height=10)


#standard all ICMP overtime stats
require(ggplot2)
require(lubridate)
date.isolated <-ymd(ICMP.dump.kiwifruit$IsolationDateISO, truncated = 1)
ggplot(ICMP.dump.kiwifruit, aes(date.isolated, fill = Country)) +
  labs(title = "Isolation dates of ex-kiwifruit ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  theme(legend.position = c(0.1, 0.7)) +
  geom_histogram(binwidth=365.25) +
  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP-isolation-dates-kiwifruit.png', width=8, height=5)




#NZ Myrtaceae cultures in ICMP with a sequence
ICMP.dump.Myrtaceae <- subset(ICMP.dump.NZ,(Family_C2 == "Myrtaceae"))
ggplot(ICMP.dump.Myrtaceae, aes(SpecimenType, fill=GenBank)) + 
  labs(title = "NZ Myrtaceae cultures in ICMP with a sequence") +
  labs(x = "Taxonomic group", y = "Number of cultures") +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")
ggsave(file='ICMP_Myrtaceae-genbank.png', width=8, height=5)

#Family of 'microbe' on NZ Myrtaceae in the ICMPt
ICMP.dump.Myrtaceae <- subset(ICMP.dump.NZ,(Family_C2 == "Myrtaceae"))
ggplot(ICMP.dump.Myrtaceae, aes(Family, fill=SpecimenType)) + #fill by type
  labs(title = "Family of 'microbe' on NZ Myrtaceae in the ICMP") +
  labs(x = "Family", y = "number of isolates") +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")
ggsave(file='ICMP_Myrtaceae-family.png', width=8, height=15)


#======people======

summary(ICMP.dump$StandardCollector, maxsum=50)

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
d=tribble(ICMP.dump)
d %>% mutate(date=mdy(textdate)) %>%
  arrange(date)
?arrange

#====== Data manipulation======

#merge together the Genbank literature and image data to one column

#sub set out the interesting columns using "select" in dpylr
library(dplyr)
only.ext.specimen <- select(ICMP.dump, "AccessionNumber","SpecimenType", "GenBank", "Literature", "Images")
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


