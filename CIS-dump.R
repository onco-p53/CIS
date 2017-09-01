## R Script to process data exported from the CIS databases ##
# Author: B.S. Weir (2017)

#============Load data================
ICMP.dump.initial <- read.csv("20170830 ICMP Dump PROD.csv")


# subset out viruses
# subset out "Deaccessioned=True"
#============Load data================


#have a quick look at the data
head(ICMP.dump)
summary(ICMP.dump, maxsum=40)




# Here Is Code to generate graphics

#ggplot code for kingdom
attach(ICMP.dump)
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=GenBank)) + labs(title = "ICMP by Kingdom, in GenBank") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_genbank.pdf', width=10, height=10)

#ggplot code for kingdom
attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=Literature)) + labs(title = "ICMP by Kingdom in Literature") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_Literature.pdf', width=10, height=10)


#ggplot code for kingdom
attach(ICMP.dump) #this means we don't need the $ sign
require(ggplot2)
p <- ggplot(ICMP.dump, aes(SpecimenType, fill=Images)) + labs(title = "ICMP by Kingdom, with images") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=-90, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ICMP_kingdoms_images.pdf', width=10, height=10)