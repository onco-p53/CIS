## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017)

#============Load and subset data================
ICMP.dump.initial <- read.csv("20170830 ICMP Dump PROD.csv")


# subset out viruses
noviruses <- subset(ICMP.dump.initial, (SpecimenType == "Bacterial Culture" | SpecimenType == "Chromist Culture" | SpecimenType == "Fungal Culture" | SpecimenType == "Yeast Culture"))

# subset out "Deaccessioned=True"
ICMP.dump <- subset(noviruses,(Deaccessioned == "FALSE"))


#setting up per specimen type subsets
ICMP.bacteria <- subset(ICMP.dump,(SpecimenType == "Bacterial Culture"))
summary(ICMP.bacteria, maxsum=40)

ICMP.chromist <- subset(ICMP.dump,(SpecimenType == "Chromist Culture"))
summary(ICMP.chromist, maxsum=40)

ICMP.fungi <- subset(ICMP.dump,(SpecimenType == "Fungal Culture"))
summary(ICMP.fungi, maxsum=40)

ICMP.yeast <- subset(ICMP.dump,(SpecimenType == "Yeast Culture"))
summary(ICMP.yeast, maxsum=40)



#============Quick data check================
#have a quick look at the data
head(ICMP.dump)

summary(ICMP.dump.initial, maxsum=20) #data before subsetting
summary(ICMP.dump, maxsum=20) #data after subsetting

#============Kingdom Level barcharts================

#plain code for a kingdom barchart
attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType)) + labs(title = "Cultures in the ICMP by Kingdom") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms.png', width=7, height=7)


#kingdoms in GenBank
attach(ICMP.dump)
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=GenBank)) + labs(title = "Cultures in the ICMP by Kingdom, in GenBank") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_genbank.png', width=7, height=7)

#kingdoms with literature
attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=Literature)) + labs(title = "Cultures in the ICMP by Kingdom, in Literature") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_Literature.png', width=7, height=7)


#kingdoms with images
attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=Images)) + labs(title = "Cultures in the ICMP by Kingdom, with images") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_images.png', width=7, height=7)
