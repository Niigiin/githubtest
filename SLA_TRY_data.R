#12-11-2020

## The purpose of this script is to import data download from TRY and subset for species in the LA PAlma science school,
##        To pick out one variable
##        Plot/table data
##        create a csv of the trait-specific dataframe. 


##############################################################################################################
##set up space

rm(list=ls())
setwd("E:/SLA")

##packages
library(data.table)
library(formattable)
library(dplyr)
library(kableExtra)

####################################################################
#import data 
TRYdata <- fread("10100.txt",  ###alter me-- look for the datafile name in alterables below
                 header = T, 
                 sep = "\t", 
                 dec = ".", 
                 quote = "", 
                 data.table = T) 

species<- read.csv2("LaPalma-SLA dataset.csv")
#######################################################################
########################## ALTERABLES #################################
#######################################################################

#options for data download "Data/9821.txt" i.e. leaf stoichimetry
# -------------------------------
#LeafCN 146
#LeafNperarea 50
#LeafNperleafmass 14
#LeafNP 56


#options for data download "Data/9821.txt" i.e. LHS
# -------------------------------
# SeedDryMass 26
# PlantHeightVegetative 3106
# SLA_petiole_excl 3115
# SLA_petiole_incl 3116
# SLA_petiole_undefined 3117

traitno <- 3117
traitname <- "SLA_petiole_undefined"

########################################################################
########################## LET ME GO ###################################
########################################################################
##format data
##subset to only lapalma species, cahnge formats etc
TRYcleaned <- TRYdata[TRYdata$AccSpeciesName %in% species$species, ]
TRYcleaned <- TRYcleaned[,c(7,10,15)]
TRYcleaned <- as.data.frame(TRYcleaned)
TRYcleaned$AccSpeciesName <- as.factor(TRYcleaned$AccSpeciesName)
TRYcleaned$OrigValueStr<- as.numeric(TRYcleaned$OrigValueStr)
TRYcleaned$TraitID<- as.numeric(TRYcleaned$TraitID)

##subset by response variable of interest
trait <- TRYcleaned[TRYcleaned$TraitID== traitno,] 
ag <- aggregate(. ~ AccSpeciesName, trait, 
                function(x) c(mean = mean(x), 
                              sd = sd(x),
                              n = as.integer(length(x))),
                simplify = TRUE)

simplified <- as.data.frame(cbind(as.character(ag$AccSpeciesName), 
                                  round(ag$OrigValueStr[,1], digits = 2),
                                  as.numeric(as.character(round(ag$OrigValueStr[,2], digits = 2))),
                                  ag$OrigValueStr[,3]))



simplified[,3] <- as.numeric(as.character(simplified[,3]))
simplified[,2] <- as.numeric(as.character(simplified[,2]))
simplified[,5]<- round(simplified[,3]/simplified[,2], digits =2)

####format names
coln1<-paste("µ", traitname, sep = " ")
coln2<-paste("s.d.", traitname, sep = " ")
coln3<-paste("n", traitname, sep = " ")
coln4<- paste ("norm_s.d.", traitname, sep = " ")
colnames(simplified) <- c("Taxon", coln1, coln2 , coln3, coln4)


################################################################################
##Print out csv
name<- paste("./outputs/", "TRY",traitname, ".csv",sep="")
write.csv(simplified, name)

################################################################################
# #table
name2<- paste("./Outputs/", "TRY",traitname, ".html",sep="")          
formattable(simplified, 
            align = c("l", rep("r", ncol(simplified) - 1)),
            list(`Taxon` = formatter("span", 
                                     style = ~ style( "font-style:italic")), 
                 `norm_s.d.` = color_tile("white", "darkolivegreen3"))) %>% 
  
  save_kable(file = name2, 
             self_contained = T)

##relationship between norm_sd and n
simplified[,4] <- as.numeric(as.character(simplified[,4]))

name3<- paste("./Outputs/", "n_effect_TRY",traitname, ".pdf",sep="")  
pdf(name3)
plot(name3)
plot(simplified[,5]~ simplified[,4], data = simplified, pch = 16,
     ylab = colnames(simplified)[5],
     xlab = colnames(simplified)[4])
dev.off()
