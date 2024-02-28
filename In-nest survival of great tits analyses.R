##---------------------------------------------------------------------#
##
## Title: ANNUAL VARIATION IN EARLY-LIFE SURVIVAL OF GREAT TITS
##
## Purpose of script: Describing variance in early-life survival among 
## Wytham Woods' nestbox-breeding population of great tits.
##
##    - Quantifying the heritable component to this.
##    - Clarify how variable early-life survival rates influence the importance of early-life in shaping selection
##    - Identifying environmental of short and multi-decadal variation in survival probability
##
## Author: Simon Evans
##
## Date Created: 2024-02-07
##
## (based on: 'Opportunity for selection analyses.R', which was itself
## built on code I wrote to prepare the results I presented at the 
## 2022 Hole-Nesting Birds conference)

#----------------------------------------------------------------------#
# NOTES ===============================================================
#----------------------------------------------------------------------#
#  W(1): survival to fledging
#  W(2): survival to first-year recruitment
#  [eventually] W(3): zygote production in year 1, etc
#  Regarding survival, we can assign failure to survive an individual fitness value of 0, which simplifies calculation of I (see: Brodie & Janzen 1996 Evolution)

#----------------------------------------------------------------------#
# IMPORT PACKAGES =========================================
#----------------------------------------------------------------------#

library(data.table)
library(MasterBayes)

install.packages("~/Downloads/nadiv_2.17.2.tar.gz", repos = NULL, type = "source"); library(nadiv)

#----------------------------------------------------------------------#
# SHORTCUT FOR IMPORTING DATA =========================================
#----------------------------------------------------------------------#

gt.annual.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/gt.annual.data (28Feb2024).csv")
gt.pulli <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/gt.pulli (28Feb2024).csv")
#all_inclusive.gt.ped <- read.csv("~/OneDrive - University of Exeter/Research/Prenatal survival/Ecology Letters special issue/All_inclusive.gt.ped (27Feb2024).csv")

#----------------------------------------------------------------------#
# IMPORTING DATA ======================================================
#----------------------------------------------------------------------#
 
all.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/Wytham GT BT 1960_2023.csv", header = T, na.strings = "NA"); dim(all.data)
all.data <- all.data[!is.na(all.data$year),]; dim(all.data)
all.data$Pnum <- toupper(all.data$Pnum)
all.data$Mother <- toupper(all.data$Mother)
all.data$Father <- toupper(all.data$Father)

## Ringing data (1960-2012) ----
ringing.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/legacy file.csv", header = T, na.strings = "NA"); dim(ringing.data)
ringing.data$bto_ring <- toupper(ringing.data$bto_ring)

### Great tits ----
gt.pulli <- ringing.data[which(ringing.data$bto_species_code == "GRETI" & ringing.data$age == "1" & ringing.data$year >= 1960),]; dim(gt.pulli)
gt.pulli$bto_ring <- toupper(gt.pulli$bto_ring)
gt.pulli$pnum <- toupper(gt.pulli$pnum)

# Exclude pulli from non-Wytham nestboxes:
table(gt.pulli$grid_ref, useNA = "always")
#' SP4406: external rounds
#' SP4504: external
#' SP4604: Cumnor area
#' SP4608: Wytham Woods
#' SP4709: external (field with view across the Thames valley to Oxford, near the annual photograph tree)
#' SP5102: Bagley Wood
gt.pulli <- gt.pulli[which(gt.pulli$grid_ref == "SP4608" | gt.pulli$grid_ref == ""),]; table(gt.pulli$grid_ref, useNA = "always")

## Remove fledglings from non-Wytham boxes ----
gt.pulli$round <- gsub('[[:digit:]]+', '', gt.pulli$site); rev(sort(table(gt.pulli$round, useNA = "always"))); (rounds <- data.frame(rev(sort(table(gt.pulli$round, useNA = "always")))))
gt.pulli <- gt.pulli[which(gt.pulli$round == "EX" | gt.pulli$round == "EXA" | gt.pulli$round == "EXB" | gt.pulli$round == "EXC" | gt.pulli$round == "EXD" | gt.pulli$round == "EXE" | gt.pulli$round == "EXF" | gt.pulli$round == "EXG" | gt.pulli$round == "EXH" | gt.pulli$round == "EXI" 
                           | gt.pulli$round == "B" | gt.pulli$round == "BA" 
                           | gt.pulli$round == "O" | gt.pulli$round == "OA" | gt.pulli$round == "OB"| gt.pulli$round == "OC" | gt.pulli$round == "OD"
                            | gt.pulli$round == "W" | gt.pulli$round == "WA" | gt.pulli$round == "WB"
                           | gt.pulli$round == "MP" | gt.pulli$round == "MPA" 
                           | gt.pulli$round == "SW" | gt.pulli$round == "SWA" 
                           | gt.pulli$round == "C" | gt.pulli$round == "CA" | gt.pulli$round == "CB" | gt.pulli$round == "CC" 
                           | gt.pulli$round == "CP" | gt.pulli$round == "CPA" | gt.pulli$round == "CPB"
                           | gt.pulli$round == "P"),] 
rev(sort(table(gt.pulli$round, useNA = "always")))

length(unique(gt.pulli$bto_ring))
reference <- dim(gt.pulli)[1]

#### Dealing with replicates ----
gt.appearance.count <- rev(sort(table(gt.pulli$bto_ring))); head(gt.appearance.count, n = 30)

# Remove records for unringed 'runt' chicks
gt.pulli <- gt.pulli[which(gt.pulli$bto_ring != "UNRRUNT"),]; dim(gt.pulli)
gt.appearance.count <- rev(sort(table(gt.pulli$bto_ring))); head(gt.appearance.count, n = 15)
gt.pulli <- merge(gt.pulli, data.frame(table(bto_ring = gt.pulli$bto_ring))); table(gt.pulli$Freq)

# Years in which ringnumber repeats occur
table(gt.pulli[gt.pulli$Freq == 2,]$year)

problem.nests <- unique(gt.pulli[gt.pulli$Freq == 2,]$pnum)
all.data[all.data$Pnum == problem.nests[1],]$Experiment.codes

(problem.pulli <- unique(gt.pulli[gt.pulli$Freq == 2,]$bto_ring))
number <- 1
reference <- dim(gt.pulli)[1]
## Number 1
this.one <- problem.pulli[number]; this.one
(these.problem.pnums <- gt.pulli[gt.pulli$bto_ring == this.one,]$pnum)
gt.pulli[gt.pulli$pnum == these.problem.pnums[1] | gt.pulli$pnum == these.problem.pnums[2], 1:13]
gt.pulli <- gt.pulli[!(gt.pulli$bto_ring == this.one & gt.pulli$pnum == "20061O103"),]; dim(gt.pulli)  # Fits with other rings used on pulli at 200610100 so the other record is deleted
stopifnot(dim(gt.pulli)[1] == reference - 1); reference <- reference-1; number <- number+1

## Number 2
this.one <- problem.pulli[number]; this.one
(these.problem.pnums <- gt.pulli[gt.pulli$bto_ring == this.one,]$pnum)
gt.pulli[gt.pulli$pnum == these.problem.pnums[1] | gt.pulli$pnum == these.problem.pnums[2], 1:13]
#' Without the repeating ringnumbers, the number of fledglings in each nest matches the 
#' number of fledglings recorded in the nest datafile.
#' Therefore I delete these records.
gt.pulli <- gt.pulli[!(gt.pulli$bto_ring == this.one),]; dim(gt.pulli)  # Fits with other rings used on pulli at 200610100 so the other record is deleted
number <- number+1

## Number 3 (same as the previous)
this.one <- problem.pulli[number]; this.one
(these.problem.pnums <- gt.pulli[gt.pulli$bto_ring == this.one,]$pnum)
gt.pulli[gt.pulli$pnum == these.problem.pnums[1] | gt.pulli$pnum == these.problem.pnums[2], 1:13]
#' Without the repeating ringnumbers, the number of fledglings in each nest matches the 
#' number of fledglings recorded in the nest datafile.
#' Therefore I delete these records.
dim(gt.pulli); gt.pulli <- gt.pulli[!(gt.pulli$bto_ring == this.one),]; dim(gt.pulli)  # Fits with other rings used on pulli at 200610100 so the other record is deleted
number <- number+1

## Number 4
this.one <- problem.pulli[number]; this.one
(these.problem.pnums <- gt.pulli[gt.pulli$bto_ring == this.one,]$pnum)
gt.pulli[gt.pulli$pnum == these.problem.pnums[1] | gt.pulli$pnum == these.problem.pnums[2], 1:13]
#' The problem ring is continuous with those in 20081O109, so this is kept.
#' I also delete another pullus assigned to the other nest (20081SW102)
dim(gt.pulli); gt.pulli <- gt.pulli[!(gt.pulli$bto_ring == this.one & gt.pulli$pnum == "20081SW102"),]; gt.pulli <- gt.pulli[!(gt.pulli$bto_ring == "TL39501" & gt.pulli$pnum == "20081SW102"),]; dim(gt.pulli)  # Fits with other rings used on pulli at 200610100 so the other record is deleted
number <- number+1

## Number 5
this.one <- problem.pulli[number]; this.one
(these.problem.pnums <- gt.pulli[gt.pulli$bto_ring == this.one,]$pnum)
gt.pulli[gt.pulli$pnum == these.problem.pnums[1] | gt.pulli$pnum == these.problem.pnums[2], 1:13]
#' This ring fits with others at 20001SW75 and so this record was retained
dim(gt.pulli); gt.pulli <- gt.pulli[!(gt.pulli$bto_ring == this.one & gt.pulli$pnum == these.problem.pnums[2]),]; dim(gt.pulli)  # Fits with other rings used on pulli at 200610100 so the other record is deleted

# Sorted?
stopifnot(max(table(gt.pulli$bto_ring)) == 1)


## Ringing data (2013-2023) ----
ringing.data_2 <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/long_term_ringing_data.csv", header = T, na.strings = "NA"); dim(ringing.data)
ringing.data_2$ring <- toupper(ringing.data_2$ring)

gt.pulli_2 <- ringing.data_2[which(ringing.data_2$species == "greti" & ringing.data_2$age == "1" & ringing.data_2$year >= "2013" & ringing.data_2$region == "WYT"),]; dim(gt.pulli_2)
table(gt.pulli_2$region, useNA = "always")

#### Dealing with replicates ----
gt.appearance.count <- rev(sort(table(gt.pulli_2$ring))); head(gt.appearance.count, n = 30)

# Remove records for unringed 'runt' chicks
dim(gt.pulli_2); gt.pulli_2 <- gt.pulli_2[which(gt.pulli_2$ring != "UNRRUNT"),]; dim(gt.pulli_2)
gt.appearance.count <- rev(sort(table(gt.pulli_2$ring))); table(gt.appearance.count)
gt.pulli_2 <- merge(gt.pulli_2, data.frame(table(ring = gt.pulli_2$ring))); table(gt.pulli_2$Freq)

table(gt.pulli_2[which(gt.pulli_2$Freq == 2),]$source, useNA = "always")

# Deal with 'demon' replication
gt.pulli_2[which(gt.pulli_2$Freq == 2 & gt.pulli_2$source == "demon"),]
dim(gt.pulli_2); gt.pulli_2 <- gt.pulli_2[!(gt.pulli_2$Freq == 2 & gt.pulli_2$source == "demon" & gt.pulli_2$retrap == "Retrap"),]; dim(gt.pulli_2)
gt.pulli_2 <- merge(gt.pulli_2, data.frame(table(ring = gt.pulli_2$ring))); table(gt.pulli_2$Freq)

## Need to remove all the other replicates (there are many): 
## duplication seems to be in order to enter the PIT tag values
## I will keep the set of duplicates taken from IPMR (the BTO ringing system)
gt.pulli_2 <- merge(gt.pulli_2, data.frame(table(ring = gt.pulli_2$ring))); table(gt.pulli_2[which(gt.pulli_2$Freq == 2),]$source, useNA = "always")
dim(gt.pulli_2); gt.pulli_2 <- gt.pulli_2[which(gt.pulli_2$Freq == 1 | (gt.pulli_2$Freq ==2 & gt.pulli_2$source == "ebmp")),]; dim(gt.pulli_2)

# Sorted?
stopifnot(max(head(rev(sort(table(gt.pulli_2$ring))))) == 1)

## Saving dataset
# write.csv(gt.pulli, "~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/gt.pulli (28Feb2024).csv")


#----------------------------------------------------------------------#
# DATA CHECKING =======================================================
#----------------------------------------------------------------------#

# Check if pnum is unique
stopifnot(length(unique(all.data$Pnum)) == length(all.data$Pnum)); length(all.data$Pnum)

# Create species-specific annual summaries
## Great tits ----
#TODO EXCLUDE INFLUENTIAL EXPERIMENTAL MANIPULATIONS
gt.nest.data <- all.data[which(all.data$Species == "g" & !is.na(all.data$Clutch.size)),]; length(gt.nest.data$Pnum)
gt.nest.data$Mother <- toupper(gt.nest.data$Mother)
gt.nest.data$Father <- toupper(gt.nest.data$Father)

# Correct the list of dead ringed chicks for one nest (20151C39), which actually were ringed in other nests (20151C9A & 20151C37)
gt.nest.data[gt.nest.data$Pnum == "20151C139",]$Dead.ringed.chick.ids <-  ""

table(gt.nest.data$Num.eggs.manipulated); gt.nest.data <- gt.nest.data[gt.nest.data$Num.eggs.manipulated == 0,]; table(gt.nest.data$Num.eggs.manipulated)
summary(gt.nest.data$Clutch.size); table(gt.nest.data$Clutch.size)
summary(gt.nest.data$Num.fledglings); table(gt.nest.data$Num.fledglings)
gt.nest.data[gt.nest.data$Num.fledglings < 0,]$Num.fledglings <- 0; table(gt.nest.data$Num.fledglings)  # I visually assessed the affected data and this seems a reasonable edit for the 8 affected samples
table(gt.nest.data$Clutch.size - gt.nest.data$Num.fledglings)
gt.nest.data <- gt.nest.data[gt.nest.data$Clutch.size - gt.nest.data$Num.fledglings >= 0,]; table(gt.nest.data$Clutch.size - gt.nest.data$Num.fledglings)  # Remove records from three nests recorded as having more fledglings than eggs (n = 3)

gt.egg.counts <- aggregate(gt.nest.data$Clutch.size, by = list(gt.nest.data$year), FUN = function(x) {sum(x[!is.na(x)])}); colnames(gt.egg.counts) <- c("year", "egg.total")
gt.fledgling.counts <- aggregate(gt.nest.data$Num.fledglings, by = list(gt.nest.data$year), FUN = function(x) {sum(x[!is.na(x)])}); colnames(gt.fledgling.counts) <- c("year", "fledgling.total")
gt.nest.count <- aggregate(gt.nest.data$year, by = list(gt.nest.data$year), FUN = function(x) {length(x[!is.na(x)])}); colnames(gt.nest.count) <- c("year", "nest.count")

# Counting deaths in nests that failed to produce any fledglings
gt.nest.data$nest.failure <- 1; gt.nest.data[gt.nest.data$Num.fledglings > 0,]$nest.failure <- 0;
  gt.failed.nest.count <- aggregate(gt.nest.data$nest.failure, by = list(gt.nest.data$year), FUN = sum); colnames(gt.failed.nest.count) <- c("year", "failed.nest.count")
  gt.mortality.due.to.nest.failure <- aggregate(gt.nest.data[gt.nest.data$nest.failure == 1,]$Clutch.size, by = list(gt.nest.data[gt.nest.data$nest.failure == 1,]$year), FUN = function(x) {sum(x[!is.na(x)])})

# Counting deaths in nests that produced at least one fledgling
gt.individual.mortality.count <- aggregate(gt.nest.data[gt.nest.data$nest.failure == 0,]$Clutch.size - gt.nest.data[gt.nest.data$nest.failure == 0,]$Num.fledglings, by = list(gt.nest.data[gt.nest.data$nest.failure == 0,]$year), FUN = function(x) {sum(x[!is.na(x)])}); c("year", "individual.mortality.count")
gt.nonabandoned.nests.count <- aggregate(gt.nest.data[gt.nest.data$nest.failure == 0,]$Clutch.size, by = list(gt.nest.data[gt.nest.data$nest.failure == 0,]$year), FUN = sum)

gt.annual.data <- cbind(gt.nest.count,  # year and nest count
                        gt.failed.nest.count$failed.nest.count/gt.nest.count$nest.count,  # nest failure rate
                        gt.mortality.due.to.nest.failure[,2]/gt.egg.counts$egg.total,
                        gt.egg.counts$egg.total,  # total number of eggs
                        gt.fledgling.counts$fledgling.total,  # total number of fledglings
                        gt.individual.mortality.count[,2]/gt.nonabandoned.nests.count[,2],  # mortality rate attributable to death in non-failed nests
                        gt.fledgling.counts$fledgling.total/gt.egg.counts$egg.total,  # in-nest survival rate (w1)
                        (1-(gt.fledgling.counts$fledgling.total/gt.egg.counts$egg.total))/(gt.fledgling.counts$fledgling.total/gt.egg.counts$egg.total))  # Opportunity for in-nest selection (I1)
  colnames(gt.annual.data) <- c("year", "nest.count", "nest.failure.rate", "mortality.rate.due.to.nest.failure", "egg.total", "fledgling.total", "individual.failure.rate", "w1", "I1")
  gt.annual.data

## Save dataset as .csv file
# write.csv(gt.annual.data, "~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/gt.annual.data (28Feb2024).csv")

#' Correcting listing of individuals as both Mother and Father within the long-term breeding records. 
#' I am aware of nine entries in which females are also listed as males.
#' These were picked up by the prepPed function

# Individual 1: E985078
gt.nest.data[which(gt.nest.data$Mother == "E985078" | gt.nest.data$Father == "E985078"),c(1:3, 37:38)]  # Individual is listed as a father in three years, a mother once.
#' What of its partner when listed as a Mother?
gt.nest.data[which(gt.nest.data$Mother == "TC58285" | gt.nest.data$Father == "TC58285"),c(1:3, 37:38)]  # Listed as a Father in three years
#' Reset entry as Mother to unknown
gt.nest.data[which(gt.nest.data$Pnum == "20081CP35"),]$Mother <- ""

# Individual 2: X239739
gt.nest.data[which(gt.nest.data$Mother == "X239739" | gt.nest.data$Father == "X239739"),c(1:3, 37:38)]  # Listed once as a father, once as a mother
# What of the ringing record?
ringing.data[ringing.data$bto_ring == "X239739",c(13,3,5,7,10,11)]  # Identified twice as a male, once as a female.
gt.nest.data[which(gt.nest.data$Pnum == "20121EX39"),]$Mother <- ""

# Individual 3: ?
individual <- "Y031429"
gt.nest.data[which(gt.nest.data$Mother == individual | gt.nest.data$Father == individual),c(1:3, 37:38)]
ringing.data[ringing.data$bto_ring == individual, c(13,3,5,7,10,11)] 
ringing.data_2[which(ringing.data_2$ring == individual) ,c(3,12, 10, 7,8)] # Sexed only on these two occassions as a breeder
# Reset both listings
gt.nest.data[which(gt.nest.data$Pnum == "20121W56"),]$Father <- ""
gt.nest.data[which(gt.nest.data$Pnum == "20131W56"),]$Mother <- ""
stopifnot(dim(gt.nest.data[which(gt.nest.data$Mother == individual | gt.nest.data$Father == individual),c(1:3, 37:38)])[1] == 0)

# Individual 4 ?
individual <- "VZ30547"
gt.nest.data[which(gt.nest.data$Mother == individual | gt.nest.data$Father == individual),c(1:3, 37:38)] # Listed once as a father, once as a mother
# What of the ringing record?
ringing.data[ringing.data$bto_ring == individual, c(13,3,5,7,10,11)] 
ringing.data_2[which(ringing.data_2$ring == individual) ,c(3,12, 10, 7,8)] # Identified as male multiple times
# Reset listing as Mother
gt.nest.data[which(gt.nest.data$Pnum == "20211B87"),]$Mother <- ""
stopifnot(dim(gt.nest.data[which(gt.nest.data$Mother == individual | gt.nest.data$Father == individual),])[1] == 1)

# Individual 5 ?
individual <- "VZ29475"
gt.nest.data[which(gt.nest.data$Mother == individual | gt.nest.data$Father == individual),c(1:3, 37:38)]
# Listed as both Mother and Father for 20181EX20, and as father for 20191EX21
gt.nest.data[which(gt.nest.data$Pnum == "20181EX20"),]$Father <- ""
stopifnot(dim(gt.nest.data[which(gt.nest.data$Father == individual),])[1] == 0)

#----------------------------------------------------------------------#
# BUILDING PEDIGREE ===================================================
#----------------------------------------------------------------------#

## Ringed chicks ----
  
# Revert pnums to the nest-of-origin if they may be different
stopifnot(sum(is.na(gt.pulli$pnum)) == 0); gt.pulli[gt.pulli$origin_pnum != "",]$pnum <- gt.pulli[gt.pulli$origin_pnum != "",]$origin_pnum
  
# Do all identified fledglings have a pnum?
dim(gt.pulli[which(gt.pulli$pnum == ""),])[1]  # NO!

### Known pnum ----
#### 1960-2012 (fledge.ped) ----
fledge.ped <- merge(x = gt.pulli[which(gt.pulli$pnum != ""),], y = gt.nest.data[c("Pnum", "Mother", "Father")], by.x = "pnum", by.y = "Pnum", all.x = TRUE, all.y = FALSE); dim(fledge.ped)
head(rev(sort(table(fledge.ped$Mother, useNA = "always"))), n = 10); most.successful.mother.count <- head(rev(sort(table(fledge.ped$Mother, useNA = "always"))))[3]
head(rev(sort(table(fledge.ped$Father, useNA = "always"))), n = 10); most.successful.father.count <- head(rev(sort(table(fledge.ped$Father, useNA = "always"))))[3]

##### Create dummy parent IDs ----
fledge.ped[which(fledge.ped$Mother == "" | is.na(fledge.ped$Mother)),]$Mother <- paste0("dam_", fledge.ped[which(fledge.ped$Mother == "" | is.na(fledge.ped$Mother)),]$pnum)
fledge.ped[which(fledge.ped$Father == "" | is.na(fledge.ped$Father)),]$Father <- paste0("sire_", fledge.ped[which(fledge.ped$Father == "" | is.na(fledge.ped$Father)),]$pnum)                                                             
                                                   
head(rev(sort(table(fledge.ped$Mother, useNA = "always"))), n = 10); stopifnot(max(table(fledge.ped$Mother, useNA = "always")) == most.successful.mother.count)
head(rev(sort(table(fledge.ped$Father, useNA = "always"))), n = 10); stopifnot(max(table(fledge.ped$Father, useNA = "always")) == most.successful.father.count)

###### Unknown pnum ----
# How are these distributed across the study period?
table(gt.pulli[which(gt.pulli$pnum == ""),]$year)

# Do all fledglings of unknown pnum have a known identity for 'grid_ref'?
dim(gt.pulli[which(gt.pulli$pnum == "" & gt.pulli$grid_ref == ""),])[1]  # YES
head(rev(sort(table(gt.pulli[which(gt.pulli$pnum == ""),]$grid_ref))))  # But grid_ref is not unique to nests

# What about 'site'?
rev(sort(table(gt.pulli[which(gt.pulli$pnum == ""),]$site)))

# Erasing ringing records for fledglings in remaining known site identities, since they are unfamiliar to me (presumably external)
gt.pulli <- gt.pulli[which(gt.pulli$pnum != "" & gt.pulli$site != ""),]; dim(gt.pulli); head(rev(sort(table(gt.pulli[which(gt.pulli$pnum == ""),]$site))))

# So replace unknown pnums with a unique dummy nest identity
# Is 'id' unique within this group?
length(unique(gt.pulli[which(gt.pulli$pnum == ""),]$id)); length(gt.pulli[which(gt.pulli$pnum == ""),]$id); stopifnot(length(unique(gt.pulli[which(gt.pulli$pnum == ""),]$id)) == length(gt.pulli[which(gt.pulli$pnum == ""),]$id))  # YES!

# So use 'id' to create dummy nest identities that are unique to each each ringed pullus
gt.pulli[which(gt.pulli$pnum == ""),]$pnum <- paste0(gt.pulli[which(gt.pulli$pnum == ""),]$year, ".", gt.pulli[which(gt.pulli$pnum == ""),]$id)

# Confirm that all blank 'pnum' entries have been filled with dummy identities
stopifnot(length((gt.pulli[which(gt.pulli$pnum == ""),]$pnum)) == 0)

# What are the greatest numbers of ringed chicks per nest?
head(rev(sort(table(gt.pulli$pnum, useNA = "always"))), n = 30)

fledge.ped$dam <- fledge.ped$Mother
fledge.ped$sire <- fledge.ped$Father
fledge.ped$nest <- fledge.ped$pnum
  

###### Unknown parents of ringed chicks ----
head(rev(sort(table(fledge.ped$dam, useNA = "always"))), n = 25)
head(rev(sort(table(fledge.ped$sire, useNA = "always"))), n = 25)
dim(fledge.ped[which(fledge.ped$dam == "" | is.na(fledge.ped$dam)),])[1]  # Number of ringed pulli with unknown mother
dim(fledge.ped[which(fledge.ped$sire == "" | is.na(fledge.ped$sire)),])[1]  # Number of ringed pulli with unknown father

fledge.ped$survival <- 1
# TODO: Reset survival to zero for ringed pulli found in their nest


##### 2013-2023 (recent.fledgling.ped) ----
dim(gt.pulli_2)[1]
dim(gt.pulli_2[gt.pulli_2$location == "" | is.na(gt.pulli_2$location),])[1]  # Number of fledglings of unknown nest-of-origin
stopifnot(length((gt.pulli_2[which(gt.pulli$pnum == ""),]$location)) == 0)  # Confirm that all non-NA nest IDs are known
recent.fledgling.ped <- merge(x = gt.pulli_2, y = gt.nest.data[c("Pnum", "Mother", "Father")], by.x = "location", by.y = "Pnum", all.x = TRUE, all.y = FALSE); dim(recent.fledgling.ped)[1]
head(rev(sort(table(recent.fledgling.ped$Mother, useNA = "always"))), n = 10); recent.most.successful.mother.count <- rev(sort(table(recent.fledgling.ped[which(recent.fledgling.ped$Mother != "" & !is.na(recent.fledgling.ped$Mother)),]$Mother)))[1]  # Excluding blanks and NAs
head(rev(sort(table(recent.fledgling.ped$Father, useNA = "always"))), n = 10); recent.most.successful.father.count <- rev(sort(table(recent.fledgling.ped[which(recent.fledgling.ped$Father != "" & !is.na(recent.fledgling.ped$Father)),]$Father)))[1]  # Excluding blanks and NAs

###### Create dummy parent IDs ----
#' Some location IDs are nestbox only (no year), others have, e.g.,
#' "-WYT (Wytham Great Wood, Oxford)" appended to them

# First remove long labels from nestbox IDs:
recent.fledgling.ped$location[!grepl("^[[:digit:]]", recent.fledgling.ped$location)] <- sub("\\-.*", "", recent.fledgling.ped$location[!grepl("^[[:digit:]]", recent.fledgling.ped$location)]); unique(recent.fledgling.ped$location[!grepl("^[[:digit:]]", recent.fledgling.ped$location)])

# Then add the year to the nestbox ID to create a more specific brood ID
recent.fledgling.ped$location[!grepl("^[[:digit:]]", recent.fledgling.ped$location)] <- paste0(recent.fledgling.ped$year[!grepl("^[[:digit:]]", recent.fledgling.ped$location)], recent.fledgling.ped$location[!grepl("^[[:digit:]]", recent.fledgling.ped$location)])

# Okay, continue with creating dummy parent IDs
recent.fledgling.ped[which(recent.fledgling.ped$Mother == "" | is.na(recent.fledgling.ped$Mother)),]$Mother <- paste0("dam_", recent.fledgling.ped[which(recent.fledgling.ped$Mother == "" | is.na(recent.fledgling.ped$Mother)),]$location)
recent.fledgling.ped[which(recent.fledgling.ped$Father == "" | is.na(recent.fledgling.ped$Father)),]$Father <- paste0("sire_", recent.fledgling.ped[which(recent.fledgling.ped$Father == "" | is.na(recent.fledgling.ped$Father)),]$location)                                                             
                                                   
head(rev(sort(table(recent.fledgling.ped$Mother, useNA = "always"))), n = 10); stopifnot(max(table(recent.fledgling.ped$Mother, useNA = "always")) == recent.most.successful.mother.count)
head(rev(sort(table(recent.fledgling.ped$Father, useNA = "always"))), n = 10); stopifnot(max(table(recent.fledgling.ped$Father, useNA = "always")) == recent.most.successful.father.count)

# What are the greatest numbers of ringed chicks per nest?
head(rev(sort(table(recent.fledgling.ped$location), useNA = "always")), n = 30)
  
# Everyone has been assigned a mother and a father, whether real or dummy
recent.fledgling.ped$id <- tolower(recent.fledgling.ped$ring)
recent.fledgling.ped$dam <- recent.fledgling.ped$Mother
recent.fledgling.ped$sire <- recent.fledgling.ped$Father

# Nest IDs
recent.fledgling.ped$nest <- recent.fledgling.ped$location
head(rev(sort(table(recent.fledgling.ped$nest, useNA = "always"))))


#### Dead ringed chicks (dead.ped) ----
##### Directly recorded (2013-2023) ----
#' Note that only for 2013-2023 are there entries for the gt.nest.data$Dead.ringed.chick.ids column:
aggregate(gt.nest.data[gt.nest.data$Dead.ringed.chick.ids != "",]$Pnum, by = list(gt.nest.data[gt.nest.data$Dead.ringed.chick.ids != "",]$year), FUN = length)

dim(gt.nest.data[gt.nest.data$Dead.ringed.chick.ids != "" & !is.na(gt.nest.data$Dead.ringed.chick.ids),])[1]  # How many nests with dead ringed chicks?
dead.ringed.chicks <- sort(scan(text = gt.nest.data[gt.nest.data$Dead.ringed.chick.ids != "" & !is.na(gt.nest.data$Dead.ringed.chick.ids),]$Dead.ringed.chick.ids, sep = ",", what = "character")); length(dead.ringed.chicks)
dead.ringed.chicks <- trimws(dead.ringed.chicks)
head(rev(sort(table(dead.ringed.chicks))))
length(dead.ringed.chicks); dead.ringed.chicks <- dead.ringed.chicks[!(dead.ringed.chicks == "unrrunt" | dead.ringed.chicks == " unrrunt")]; length(dead.ringed.chicks)
head(rev(sort(table(dead.ringed.chicks))))

recent.fledgling.ped$survival <- 1; recent.fledgling.ped[which(recent.fledgling.ped$id %in% dead.ringed.chicks),]$survival <- 0
table(recent.fledgling.ped$survival)

##### Assigning within nests ----
#' So for older nests I am unlikely to be able to identify individuals that 
#' failed to fledge. I think I will have to identify those that were observed
#' outside the nest and then arbitrarily pick the correct number of nestlings 
#' from each nest so that the number of survivors equates to the recorded
#' number of fledglings

#' Before assigning some individuals a death, I first need to establish 
#' which fledglings were subsequently observed (and thus are known with
#' certainty to have survived to fledging).

d15.nests <- aggregate(gt.pulli$bto_ring, by = list(gt.pulli$pnum), FUN = length); colnames(d15.nests) <- c("pnum", "number.ringed.chicks")
d15.nests <- merge(x = d15.nests, y = gt.nest.data[c("Pnum", "Num.fledglings")], by.x = "pnum", by.y = "Pnum", all.x = T, all.y = F)

#### Eggs that did not generate ringed chicks (1960-2023) ----
table(gt.nest.data$Clutch.size - gt.nest.data$Num.fledglings)  # Nest-level frequency table of ringed chicks failing to fledge
gt.nest.data$nest.deaths <- gt.nest.data$Clutch.size - gt.nest.data$Num.fledglings  # Number of eggs that did not generate a fledgling

# Annual egg production statistics
annual.egg.totals <- aggregate(gt.nest.data$Clutch.size, by = list(Category = gt.nest.data$year), FUN = sum); colnames(annual.egg.totals) <- c("year", "egg.count")
sum(annual.egg.totals$egg.count); mean(annual.egg.totals$egg.count); range(annual.egg.totals$egg.count) # Total number of eggs laid over study period; annual mean; range

# Annual fledgling production statistics
annual.fledgling.totals <- aggregate(gt.nest.data$Num.fledglings, by = list(Category = gt.nest.data$year), FUN = sum); colnames(annual.fledgling.totals) <- c("year", "fledgling.count")
sum(annual.fledgling.totals$fledgling.count); mean(annual.fledgling.totals$fledgling.count); range(annual.fledgling.totals$fledgling.count)  # Total number of fledglings produced over study period; annual mean; range

# In-nest mortality statistics
sum(annual.fledgling.totals$fledgling.count) / sum(annual.egg.totals$egg.count); range(annual.fledgling.totals$fledgling.count / annual.egg.totals$egg.count)

# Back to pedigree construction...
nests.with.dead <- unique(gt.nest.data[which(gt.nest.data$nest.deaths > 0),]$Pnum); length(nests.with.dead); stopifnot(length(nests.with.dead) == length(gt.nest.data[which(gt.nest.data$nest.deaths > 0),]$Pnum))  # Double-checking that Pnum values are unique
dim(gt.nest.data[which(gt.nest.data$Clutch.size > 0),])[1]  # Number of nests with at least one egg
mean(table(gt.nest.data[which(gt.nest.data$Clutch.size > 0),]$year)); min(table(gt.nest.data[which(gt.nest.data$Clutch.size > 0),]$year)); max(table(gt.nest.data[which(gt.nest.data$Clutch.size > 0),]$year))
dim(gt.nest.data[which(gt.nest.data$nest.deaths > 0),])[1]; dim(gt.nest.data[which(gt.nest.data$nest.deaths > 0),])[1]/dim(gt.nest.data[which(gt.nest.data$Clutch.size > 0),])[1]  # Number and proportion of nests with eggs suffering at least one case of in-nest mortality
dim(gt.nest.data[which(gt.nest.data$nest.deaths == gt.nest.data$Clutch.size),])[1]; dim(gt.nest.data[which(gt.nest.data$nest.deaths == gt.nest.data$Clutch.size),])[1]/dim(gt.nest.data[which(gt.nest.data$Clutch.size > 0),])[1]  # Number and proportion of nests with eggs suffering complete failure (i.e., no fledglings)

dummy.id <- NULL
dummy.dam <- NULL
dummy.sire <- NULL
nest <- NULL
year <- NULL

for (i in 1:length(nests.with.dead)){

  for (j in 1:gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$nest.deaths){
  dummy.id <- c(dummy.id, paste0("dead_", gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$Pnum, "_", j))
  dummy.dam <- c(dummy.dam, gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$Mother)
    if (is.na(dummy.dam[length(dummy.dam)]) | dummy.dam[length(dummy.dam)] == ""){
      dummy.dam[length(dummy.dam)] <- paste0("dam_", gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$Pnum)
    }
  dummy.sire <- c(dummy.sire, gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$Father)
    if (is.na(dummy.sire[length(dummy.sire)]) | dummy.sire[length(dummy.sire)] == ""){
      dummy.sire[length(dummy.sire)] <- paste0("sire_", gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$Pnum)
    }
  nest <- c(nest, nests.with.dead[i])
  year <- c(year, gt.nest.data[gt.nest.data$Pnum == nests.with.dead[i],]$year)
  }
}

stopifnot(length(dummy.id) == sum(gt.nest.data$nest.deaths)); stopifnot(sum(gt.nest.data$nest.deaths) + sum(gt.nest.data$Num.fledglings) == sum(gt.nest.data$Clutch.size))  # Check that all in-nest deaths have been accounted for
stopifnot(length(dummy.dam) == length(dummy.dam[dummy.dam != ""])); stopifnot(length(dummy.sire) == length(dummy.sire[dummy.sire != ""]))
dead.ped <- data.frame(dummy.id, dummy.dam, dummy.sire, nest, year); dim(dead.ped)
dead.ped$survival <- 0
colnames(dead.ped) <- c("id", "dam", "sire", "nest", "year", "survival")


## *Combine all pedigree information* ----

all_inclusive.ped <- data.frame(c(dead.ped$id, fledge.ped$id, recent.fledgling.ped$id),
                                c(dead.ped$dam, fledge.ped$dam, recent.fledgling.ped$dam), 
                                c(dead.ped$sire, fledge.ped$sire, recent.fledgling.ped$sire),
                                c(dead.ped$nest, fledge.ped$nest, recent.fledgling.ped$nest),
                                c(dead.ped$year, fledge.ped$year, recent.fledgling.ped$year),
                                c(dead.ped$survival, fledge.ped$survival, recent.fledgling.ped$survival)
                                )
dim(all_inclusive.ped)
colnames(all_inclusive.ped) <- c("id", "dam", "sire", "nest", "year", "survival")
# write.csv(all_inclusive.ped, "~/OneDrive - University of Exeter/Research/Prenatal survival/Ecology Letters special issue/Test.ped (27Feb2024).csv")

## Order and sort pedigree ----
prepped.gt.ped <- prepPed(all_inclusive.ped); dim(prepped.gt.ped)


#----------------------------------------------------------------------#
# OPPORTUNITY FOR SURVIVAL I: conception to fledging ==================
#----------------------------------------------------------------------#
## Overall ----
# Plot w(1)
dev.off()
plot(w1 ~ year, data = gt.annual.data)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$w1, df = 10))
plot(I1 ~ year, data = gt.annual.data)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$I1, df = 10))


# Plotting annual in-nest survival for both species
dev.off(); par(mar = c(4, 4, 1, 1))
gt <- "darkolivegreen3"
plot(w1 ~ year, 
     data = gt.annual.data, 
     xlab = "Year",
     ylab = "In-nest survival rate",
     pch = 21,
     bg = gt,
     bty = "l",
     ylim = c(min(gt.annual.data$w1), max(gt.annual.data$w1)),
     las = 1
     )
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$w1, df = 10), col = gt)

# Plotting annual opportunity for in-nest selection for both species
dev.off(); par(mar = c(4, 4, 1, 1))
plot(I1 ~ year, 
     data = gt.annual.data, 
     xlab = "Year",
     ylab = "Opportunity for in-nest selection",
     pch = 21,
     bg = gt,
     bty = "l",
     ylim = c(min(gt.annual.data$I1), max(gt.annual.data$I1)),
     las = 1
     )
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$I1, df = 10), col = gt)


# Plotted together (stacked)
dev.off(); par(mar = c(4, 5, 1, 1), mfrow = c(2, 1))
gt <- "darkolivegreen3"

# Plot 1: In-nest survival
plot((1-w1) ~ year, 
     data = gt.annual.data, 
     xlab = "",
     ylab = "Annual in-nest mortality rate",
     pch = 21,
     bg = gt,
     bty = "l",
     las = 1
     )
lines(smooth.spline(x = gt.annual.data$year, y = 1-gt.annual.data$w1, df = 10), col = gt, lwd = 2)
points(x = gt.annual.data$year, y = 1-gt.annual.data$w1, pch = 21, bg = gt)
text(labels = expression(italic("a")), x = 1959.5, y = max(1-gt.annual.data$w1), cex = 1.2)

# Plot 2: Opportunity for selection
par(mar = c(4, 4, 1, 1))
plot(I1 ~ year, 
     data = gt.annual.data, 
     xlab = "Year",
     ylab = "Annual opportunity for selection\nvia in-nest survival",
     pch = 21,
     bg = gt,
     bty = "l",
     ylim = c(min(gt.annual.data$I1), max(gt.annual.data$I1)),
     las = 1
     )
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$I1, df = 10), col = gt, lwd =2)
points(x = gt.annual.data$year, y = gt.annual.data$I1, pch = 21, bg = gt)
text(labels = expression(italic("b")), x = 1959.5, y = max(gt.annual.data$I1), cex = 1.2)

# Relationship between the two categories of in-nest mortality
dev.off()
par(mar = c(4,4,1,1))
plot(gt.annual.data$mortality.rate.due.to.nest.failure, gt.annual.data$individual.failure.rate,
     xlab = "Rate of mortality in a partially succesful nest",
     ylab = "Rate of mortality in a failed nest",
     bty = "l",
     las = 1)

# ...and how it changes over time
plot(y = gt.annual.data$mortality.rate.due.to.nest.failure/gt.annual.data$individual.failure.rate, 
     x = gt.annual.data$year,
     bty = "l",
     xlab = "Year",
     ylab = "Relative frequency of mortality in failed nests",
     las = 1)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$mortality.rate.due.to.nest.failure/gt.annual.data$individual.failure.rate, df = 10), lwd =2)

## Separating nest failure and other in-nest mortality events ----
### In-nest survival ----
dev.off()
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))

# Plot 1: Annual rate of mortality attributable to complete nest failure
plot(mortality.rate.due.to.nest.failure ~ year, 
     data = gt.annual.data, 
     xlab = "",
     ylab = "",
     pch = 16,
     bg = "black",
     bty = "l",
     las = 1
     )
mtext("Year", side = 1, line = 2.5)
mtext("Annual mortality rate", side = 2, line = 3)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$mortality.rate.due.to.nest.failure, df = 10), col = "black", lwd = 1)

points(x = gt.annual.data$year, y = gt.annual.data$individual.failure.rate, pch = 1)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$individual.failure.rate, df = 10), col = "black", lty = 2, lwd = 1)

legend("topright", legend = c("Deaths in failed nests", "Deaths in productive nests"), lty = c(1, 2), pch = c(16, 21), bty = "n", cex = 0.9)

plot(gt.annual.data$mortality.rate.due.to.nest.failure+gt.annual.data$individual.failure.rate, 1-gt.annual.data$w1)

# Can breeding density explain changes in in-nest survival?
#' Exclude years before 1975 because we have an explanation for the changes seen then
#' Can candidate explanatory factors account for the long-term trend?

gt.density.model <- lm(gt.annual.data[gt.annual.data$year >= 1975 & gt.annual.data$year <= 2011,]$w1 ~ gt.annual.data[gt.annual.data$year >= 1975 & gt.annual.data$year <= 2011,]$nest.count + gt.annual.data[gt.annual.data$year >= 1975 & gt.annual.data$year <= 2011,]$year)
plot(y = gt.annual.data[gt.annual.data$year >= 1975,]$w1, x = gt.annual.data[gt.annual.data$year >= 1975,]$nest.count,
     pch = 21,
     bg = gt,
     xlab = "Great tit nesting density\n(number of nests)",
     ylab = "In-nest survival rate",
     las = 1,
     bty = "l",
     main = "Great tits (1975-2023)"
     )
abline(gt.density.model, col = gt, lwd = 2)
summary(gt.density.model)

# TODO
# Can we estimate the proportion of adults identified without handling?
#' i.e., via reading RFID tags
#' 
ringing.data_2

# POST-FLEDGING SURVIVAL ----

## Great tits ----
### Males ----
recruited.male.gt.fledglings <- aggregate(!is.na(match(gt.pulli$bto_ring, gt.nest.data$Father)), by = list(gt.pulli$year), FUN = sum)
colnames(recruited.male.gt.fledglings) <- c("year", "total"); recruited.male.gt.fledglings

### Females ----
recruited.female.gt.fledglings <- aggregate(!is.na(match(gt.pulli$bto_ring, gt.nest.data$Mother)), by = list(gt.pulli$year), FUN = sum)
colnames(recruited.female.gt.fledglings) <- c("year", "total"); recruited.female.gt.fledglings

## (HOPEFULLY TEMPORARILY) Omitting the null record for 2012
(recruited.male.gt.fledglings <- recruited.male.gt.fledglings[1:(dim(recruited.male.gt.fledglings)[1]-1),])
recruited.female.gt.fledglings <- recruited.female.gt.fledglings[1:(dim(recruited.female.gt.fledglings)[1]-1),]

#### Sexes combined ----
recruited.gt.fledglings <- data.frame(recruited.male.gt.fledglings$year, recruited.male.gt.fledglings$total + recruited.female.gt.fledglings$total, recruited.female.gt.fledglings$total/recruited.male.gt.fledglings$total)
colnames(recruited.gt.fledglings) <- c("year", "count", "sex ratio (f:m)")  # Sex ratio is used to highlight where, in particular, catching effort for male breeders did not match females.
recruited.gt.fledglings


#----------------------------------------------------------------------#
# OPPORTUNITY FOR SURVIVAL II: fledging to recruitment ================
#----------------------------------------------------------------------#

gt.w.2 <-  gt.nest.data$number.recruits/gt.nest.data$number.fledglings
gt.I.2 <- (1 - gt.w.2)/(gt.w.2)

gt.w.3 <- gt.nest.data$mean.breeder.egg.production
gt.I.3 <- gt.nest.data$I.breeders
 
gt.w.1.2 <- gt.nest.data$number.recruits/gt.nest.data$number.eggs  # Mean egg-to-recruitment survival
gt.I.1.2 <- (1 - gt.w.1.2)/gt.w.1.2

cor.test(gt.I.1.2, gt.I.1 + gt.I.2/gt.w.1)  # Should be identical (i.e., r=1)

gt.I.1.2.3 <- gt.I.1.2 + (gt.I.3/gt.w.2)

# Figure overlaying opportunity for selection pre- and post-fledging
barplot(gt.I.2, ylim = c(0, max(gt.I.2)))
par(new = T)
barplot(gt.I.1, ylim = c(0, max(gt.I.2)))

# Stacked barchart showing decomposition of the opportunity for selection as far as recruitment
data <- data.frame(gt.I.1, gt.I.2/gt.w.1)
par(mar = c(3, 4.5, 1, 1))
barplot(as.matrix(t(data)), las = 1, ylab = "", ylim = c(0, max(gt.I.1.2)))
axis(side = 1, at = c(2, 14, 26, 38, 50, 62), labels = seq(1960, 2010, 10))
axis(side =1, at=c(0, 1.2*length(gt.nest.data$year)), labels = F, lwd.ticks = 0)
axis(side = 2, at = c(0, 100), labels = F)
mtext("Year", side = 1, line = 2, cex = 1.1)
mtext("Opportunity for selection", side = 2, line = 3.5, cex = 1.1)
mtext(expression(paste(italic("I"), ""[(fertilisation:recruitment)])), side = 2, line = 2.2, cex = 1.1)
legend("topright", legend = c("Post-fledging", "Pre-fledging"), bty = "n", fill = rev(grey.colors(2)), cex = 1)

# Relationship between in-nest survival rate and in-nest opportunity for selection
x <- seq(from = 0.01, to = 1, by = 0.005)
I <- (1-x)/x
par(mar = c( 3.5, 3.5, 1, 1))
plot(x = x, y = I, bty = "l", las = 1, type = "l", xlab = "", ylab = "", cex = 0.9, lwd = 2)
mtext("Survival rate", side = 1, line = 2.2)
mtext("Opportunity for selection", side = 2, line = 2.5)

#-------------------------------------------------------------#
# What if we incorporate an estimate of external recruitment? #
#-------------------------------------------------------------#

# Assume 60% of recruits settle outside Wytham (Verhulst et al 1997 Ecology)
global.gt.w.2 <- gt.w.2/(1-0.6)
global.gt.I.2 <- (1-global.gt.w.2)/global.gt.w.2

global.gt.w.1.2 <- gt.w.1*global.gt.w.2   # Mean egg-to-recruitment survival after accounting for external recruitment
global.gt.I.1.2 <- (1 - global.gt.w.1.2)/global.gt.w.1.2

cor.test(global.gt.I.1.2, gt.I.1 + global.gt.I.2/gt.w.1)  # Should be identical (i.e., r=1)


# Proportion of total opportunity for selection via survival to recruitment attributable to survival in the nest
par(mar = c(3.5, 5, 1, 1))
plot(x = gt.nest.data$year, y = gt.I.1/gt.I.1.2, xlab =  "", ylab = "", bty = "l", las = 1, pch = 16, ylim = c(0, 1.15*max(c(gt.I.1/gt.I.1.2, gt.I.1/global.gt.I.1.2))), yaxs = "i")
mtext("Year", side = 1, line = 2.2)
mtext(expression(paste("Proportion ", italic("I"), ""[(fertilisation:recruitment)])), side = 2, line = 3.8)
mtext("attributed to in-nest survival", side = 2, line = 3)
lines(smooth.spline(x = gt.nest.data$year, y = gt.I.1/gt.I.1.2, df = 12), col = "black", lwd = 1)

legend("topright", legend = c("observed local recruitment   ", ""), pt.cex = c(0.95, 1.2), pch = c(16, 18),  col = c("black", "white"), cex = 0.75, bg = "white")

points(x = gt.nest.data$year, y = gt.I.1/(gt.I.1+(global.gt.I.2/gt.w.1)), pch = 18, col = "darkblue", cex =1.1)
lines(smooth.spline(x = gt.nest.data$year, y = gt.I.1/(gt.I.1+(global.gt.I.2/gt.w.1)), df = 12), col = "darkblue", lwd = 1)

legend("topright", legend = c("observed local recruitment   ", "estimated global recruitment"), pt.cex = c(0.95, 1.2), pch = c(16, 18),  col = c("black", "darkblue"), cex = 0.75)

mean((gt.I.1/global.gt.I.1.2) / (gt.I.1/gt.I.1.2))# Mean relative difference

# Opportunity for selection via fledging:recruitment survival
par(mar = c(4, 4, 1, 1))
plot(x = gt.nest.data$year, y = gt.I.2, xlab =  "", ylab = "Opportunity for selection via fledging:recruitment survival", bty = "l", las = 1, pch = 16, cex = 0.95, ylim = c(min(c(gt.I.2, global.gt.I.2)), 1.*max(c(gt.I.2, global.gt.I.2))))
mtext("Year", side = 1, line = 2.5)
lines(smooth.spline(x = gt.nest.data$year, y = gt.I.2, df = 10), col = "black", lwd = 1)

points(x = gt.nest.data$year, y = global.gt.I.2, pch = 18, col = "darkblue", cex =1.2)
lines(smooth.spline(x = gt.nest.data$year, y = global.gt.I.2, df = 10), col = "darkblue", lwd = 1)

legend("topright", legend = c("observed local recruitment", "estimated global recruitment"), pt.cex = c(0.95, 1.2), pch = c(16, 18),  col = c("black", "darkblue"), cex = 0.85)


# So how would our barplot look?
data <- data.frame(gt.I.1, gt.I.2/gt.w.1, global.gt.I.2/gt.w.1)
par(mar = c(3, 4.5, 1, 1))
barplot(as.matrix(t(data)), las = 1, ylab = "", ylim = c(0, max(gt.I.1.2)))
axis(side = 1, at = c(2, 14, 26, 38, 50, 62), labels = seq(1960, 2010, 10))
axis(side =1, at=c(0, 1.2*length(gt.nest.data$year)), labels = F, lwd.ticks = 0)
axis(side = 2, at = c(0, 100), labels = F)
mtext("Year", side = 1, line = 2, cex = 1.1)
mtext("Opportunity for selection", side = 2, line = 3.5, cex = 1.1)
mtext(expression(paste(italic("I"), ""[(fertilisation:recruitment)])), side = 2, line = 2.2, cex = 1.1)
legend("topright", legend = c("","Post-fledging                  ", "Pre-fledging"), bty = "n", fill = c("white", rev(grey.colors(3)[1:2])), cex = 1, box.col = c("white", "black", "black"))
legend("topright", legend = c("Post-fledging (corrected)", "Post-fledging (observed)", "Pre-fledging"), bty = "n", fill = c("grey90", rev(grey.colors(2))), cex = 1)



span <- c(0, max(c(gt.I.1, gt.I.2, gt.I.1.2)))
par(mfrow = c(3,1), mar = c(3, 4, 0, 0))
plot(x = gt.nest.data$year, y = gt.I.1, xlab =  "Year", ylab = "Opportunity for survival selection (to fledging)", bty = "l", las = 1, ylim = span)

plot(x = gt.nest.data$year, y = gt.I.2, xlab =  "Year", ylab = "Opportunity for survival selection (first-year)", bty = "l", las = 1, ylim = span)

plot(x = gt.nest.data$year, y = gt.I.1.2, xlab =  "Year", ylab = "Opportunity for survival selection (to recruitment)", bty = "l", las = 1, ylim = span)

plot(x = gt.nest.data$year, y = gt.I.3, xlab =  "Year", ylab = "Opportunity for selection via egg production", bty = "l", las = 1) 
fit <- smooth.spline(x = gt.nest.data$year, y = gt.I.3, df = 8)
lines(fit, col = "red", lwd = 2)
 
# Proportion of total opportunity for selection via survival to recruitment attributable to survival in the nest
plot(x = gt.nest.data$year, y = gt.I.1/gt.I.1.2, xlab =  "Year", ylab = "Proportion attributed to pre-fledging survival", bty = "l", las = 1, pch = 16)
lines(smooth.spline(x = gt.nest.data$year, y = gt.I.1/gt.I.1.2, df = 15), col = "red", lwd = 2)

# Proportion of total opportunity for selection attributable to recruitment of fledglings
plot(x = gt.nest.data$year, y = gt.I.2/gt.I.1.2.3, xlab =  "Year", ylab = "Proportion total I attributable to post-fledging survival", bty = "l", las = 1, pch = 16)
fit <- smooth.spline(x = gt.nest.data$year, y = gt.I.2/gt.I.1.2.3, df = 8)
lines(fit, col = "red", lwd = 2)

# Proportion of total opportunity for selection attributable to...
plot(x = gt.nest.data$year, y = (gt.I.3/gt.w.2)/gt.I.1.2.3, xlab =  "Year", ylab = "Proportion total I attributable to post-fledging survival", bty = "l", las = 1, pch = 16, col = "green")
points(x = gt.nest.data$year, y = gt.I.1/gt.I.1.2.3,  pch = 16, col = "red")
points(x = gt.nest.data$year, y = (gt.I.2/gt.w.1)/gt.I.1.2.3,  pch = 16, col = "blue")
fit.I.1 <- smooth.spline(x = gt.nest.data$year, y = gt.I.2/gt.I.1.2.3, df = 8)
fit.I.2 <- smooth.spline(x = gt.nest.data$year, y = (gt.w.1*gt.I.2)/gt.I.1.2.3, df = 8)
fit.I.3 <- smooth.spline(x = gt.nest.data$year, y = (gt.w.2*gt.I.3)/gt.I.1.2.3, df = 8)
lines(fit.I.1, col = "red", lwd = 2)
lines(fit.I.2, col = "blue", lwd = 2)
lines(fit.I.3, col = "green", lwd = 2)

# Proportion of 'missing' variance in survival
plot(x = gt.nest.data$year, y = (gt.I.1.2 - (gt.I.1+gt.I.2))/gt.I.1.2, xlab =  "Year", ylab = "'Missing' I", bty = "l", las = 1, pch = 16)
fit <- smooth.spline(x = gt.nest.data$year, y = (gt.I.1.2 - (gt.I.1+gt.I.2))/gt.I.1.2, df = 8)
lines(fit, col = "red", lwd = 2)


I.1 + (I.2*w.1)  # formula for summing total opportunity for selection across ontogeny
w.1



#---------------------------------------------------------------#
# Predation rate (from Appendix 1 of Dunn 1977) ================
#---------------------------------------------------------------#

plot(gt.nest.data$nest.predation.rate, gt.w.1)
cor.test(gt.nest.data$nest.predation.rate, gt.w.1)

plot(gt.nest.data$nest.predation.rate, gt.I.1)
cor.test(gt.nest.data$nest.predation.rate, gt.I.1)

plot(gt.nest.data$nest.predation.rate, gt.I.1/gt.I.1.2)
cor.test(gt.nest.data$nest.predation.rate, gt.I.1/gt.I.1.2)