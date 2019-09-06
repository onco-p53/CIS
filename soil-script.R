#script for soil microbes
#============Load and subset data================
ICMP.soil.dump.initial <- read.csv("soil-microbes.csv", header=TRUE, sep=",")
# subset out viruses
noviruses <- subset(ICMP.soil.dump.initial, (SpecimenType == "Bacterial Culture" | SpecimenType == "Chromist Culture" | SpecimenType == "Fungal Culture" | SpecimenType == "Yeast Culture"))
# subset out "Deaccessioned=True"
ICMP.soil.dump <- subset(noviruses,(Deaccessioned == "false"))
#subset New Zealand specimens
ICMP.soil.dump.NZ <- subset(ICMP.soil.dump,(Country == "New Zealand"))
head(ICMP.soil.dump.NZ)
summary(ICMP.soil.dump.NZ, maxsum=40)

ICMP.soil.fungi <- subset(ICMP.soil.dump.NZ,(SpecimenType == "Fungal Culture"))

#plain code for a kingdom barchart
attach(ICMP.soil.dump.NZ) 
require(ggplot2)
p <- ggplot(ICMP.soil.dump.NZ, aes(SpecimenType)) + labs(title = "ICMP cultures isolated from soil") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_soil_kingdoms.png', width=7, height=7)


#ggplot code for fungal Family
attach(ICMP.soil.fungi) 
require(ggplot2)
p <- ggplot(ICMP.soil.fungi, aes(Family)) + labs(title = "ICMP by fungal phylum") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_soil_fungal-phylum.png', width=10, height=20)

sort(table(CurrentName),decreasing=TRUE)[1:10]
