
#============Load and subset data================
asp <- read.csv("aspergillus-ICMP-export.csv", header=TRUE, sep=",")
head(asp)
summary(asp)

#graphics

#plot species
attach(asp) 
require(ggplot2)
p <- ggplot(asp, aes(TaxonName)) + labs(title = "Aspergillus cultures in the ICMP") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=0, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ASP-species-new.png', width=7, height=7)

#plot species with a fill
attach(asp) 
require(ggplot2)
p <- ggplot(asp, aes(TaxonName, fill=NZAreaCode)) + labs(title = "Aspergillus cultures in the ICMP") + labs(x = "Taxon", y = "number of isolates")
p <- p + theme(axis.text.x=element_text(angle=0, hjust=0))
p + geom_bar()+ coord_flip()
print_bars <- p + geom_bar()+ coord_flip()
ggsave(print_bars, file='ASP-species-fill-new.png', width=7, height=7)


attach(asp) 
require(ggplot2)
di <- ggplot(asp, aes(as.Date(IsolationDateISO, format='%Y-%m-%d'))) + labs(title = "Isolation dates of Aspergillus ICMP cultures") + labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") 
di <- di + scale_x_date()
di + geom_histogram(binwidth=365.25) # this is a bin of two years: binwidth=730
dip <- di + geom_histogram(binwidth=365.25)
ggsave(dip, file='ASP-isolation-dates-new.png', width=5, height=5)



