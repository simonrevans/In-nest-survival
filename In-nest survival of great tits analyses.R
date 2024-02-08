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
# SHORTCUT FOR IMPORTING DATA =========================================
#----------------------------------------------------------------------#

gt.data <- read.csv()
gt.pulli <- read.csv()

#----------------------------------------------------------------------#
# IMPORTING DATA ======================================================
#----------------------------------------------------------------------#
 
all.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/Wytham GT BT 1960_2023.csv", header = T, na.strings = "NA"); dim(all.data)
all.data <- all.data[!is.na(all.data$year),]; dim(all.data)

## Ringing data (1960-2012) ----
ringing.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/legacy file.csv", header = T, na.strings = "NA"); dim(ringing.data)


### Great tits ----
gt.pulli <- ringing.data[which(ringing.data$bto_species_code == "GRETI" & ringing.data$age == "1" & ringing.data$year >= 1960),]; dim(gt.pulli)

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


## Saving dataset ----
# write.csv(bt.data, "~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Ecology Letters special issue/bt data (22Sept23).csv")


#----------------------------------------------------------------------#
# DATA CHECKING =======================================================
#----------------------------------------------------------------------#

# Check if pnum is unique
stopifnot(length(unique(all.data$Pnum)) == length(all.data$Pnum)); length(all.data$Pnum)

# Create species-specific annual summaries
## Great tits ----
#TODO EXCLUDE INFLUENTIAL EXPERIMENTAL MANIPULATIONS
gt.data <- all.data[which(all.data$Species == "g" & !is.na(all.data$Clutch.size)),]; length(gt.data$Pnum)


table(gt.data$Num.eggs.manipulated); gt.data <- gt.data[gt.data$Num.eggs.manipulated == 0,]; table(gt.data$Num.eggs.manipulated)
summary(gt.data$Clutch.size); table(gt.data$Clutch.size)
summary(gt.data$Num.fledglings); table(gt.data$Num.fledglings)
gt.data[gt.data$Num.fledglings < 0,]$Num.fledglings <- 0; table(gt.data$Num.fledglings)  # I visually assessed the affected data and this seems a reasonable edit for the 8 affected samples
table(gt.data$Clutch.size - gt.data$Num.fledglings)
gt.data <- gt.data[gt.data$Clutch.size - gt.data$Num.fledglings >= 0,]; table(gt.data$Clutch.size - gt.data$Num.fledglings)  # Remove records from three nests recorded as having more fledglings than eggs (n = 3)

gt.egg.counts <- aggregate(gt.data$Clutch.size, by = list(gt.data$year), FUN = function(x) {sum(x[!is.na(x)])}); colnames(gt.egg.counts) <- c("year", "egg.total")
gt.fledgling.counts <- aggregate(gt.data$Num.fledglings, by = list(gt.data$year), FUN = function(x) {sum(x[!is.na(x)])}); colnames(gt.fledgling.counts) <- c("year", "fledgling.total")
gt.nest.count <- aggregate(gt.data$year, by = list(gt.data$year), FUN = function(x) {length(x[!is.na(x)])}); colnames(gt.nest.count) <- c("year", "nest.count")
gt.annual.data <- cbind(gt.nest.count, gt.egg.counts$egg.total, gt.fledgling.counts$fledgling.total, gt.fledgling.counts$fledgling.total/gt.egg.counts$egg.total, (1-(gt.fledgling.counts$fledgling.total/gt.egg.counts$egg.total))/(gt.fledgling.counts$fledgling.total/gt.egg.counts$egg.total))
  colnames(gt.annual.data) <- c("year", "nest.count", "egg.total", "fledgling.total", "w1", "I1")
  gt.annual.data

#----------------------------------------------------------------------#
# BUILDING PEDIGREE ===================================================
#----------------------------------------------------------------------#

## Great tits ----

### Ringed chicks ----
  
# Revert pnums to the nest-of-origin if they may be different
stopifnot(sum(is.na(gt.pulli$pnum)) == 0); gt.pulli[gt.pulli$origin_pnum != "",]$pnum <- gt.pulli[gt.pulli$origin_pnum != "",]$origin_pnum

# Do all identified fledglings have a pnum?
dim(gt.pulli[which(gt.pulli$pnum == ""),])[1]  # NO!

#### Known pnum ----
##### 1960-2012 ----
fledge.ped <- merge(x = gt.pulli[which(gt.pulli$pnum != ""),], y = gt.data[c("Pnum", "Mother", "Father")], by.x = "pnum", by.y = "Pnum", all.x = TRUE, all.y = FALSE); dim(fledge.ped)
head(rev(sort(table(fledge.ped$Mother))), n = 10, useNA = "always")
head(rev(sort(table(fledge.ped$Father))), n = 10, useNA = "always")

###### Create dummy parent IDs ----
fledge.ped[which(fledge.ped$Mother == ""),]$Mother <- paste0("dam_", fledge.ped[which(fledge.ped$Mother == ""),]$pnum)
fledge.ped[which(fledge.ped$Father == ""),]$Father <- paste0("dam_", fledge.ped[which(fledge.ped$Father == ""),]$pnum)                                                             
                                                   
head(rev(sort(table(fledge.ped$Mother))), n = 10, useNA = "always")
head(rev(sort(table(fledge.ped$Father))), n = 10, useNA = "always")

###### Unknown pnum ----

# How are these distributed across the study period?
table(gt.pulli[which(gt.pulli$pnum == ""),]$year)

# Do all fledglings of unknown pnum have a known identity for 'grid_ref'?
dim(gt.pulli[which(gt.pulli$pnum == "" & gt.pulli$grid_ref == ""),])[1]  # YES
head(rev(sort(table(gt.pulli[which(gt.pulli$pnum == ""),]$grid_ref))))  # But grid_ref is not unique to nests

# What about 'site'?
rev(sort(table(gt.pulli[which(gt.pulli$pnum == ""),]$site)))

# Generate dummy parents for the fledglings from EX9 in 2011, which haven't been assigned a pnum value
EX9 <- gt.pulli[which(gt.pulli$site == "EX9" & gt.pulli$pnum == ""),]; dim(EX9)
EX9$Mother <- "dam_EX9_2011"
EX9$Father <- "sire_EX9_2011"


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
head(rev(sort(table(gt.pulli$pnum))), n = 30)
  
# Get parent identities (if known) from gt.data
fledge.ped <- merge(x = gt.pulli, y = gt.data[c("Pnum", "Mother", "Father")], by.x = "pnum", by.y = "Pnum", all.x = TRUE, all.y = FALSE); colnames(fledge.ped) <- c("id", "dam", "sire")
head(rev(sort(table(fledge.ped$dam))), n = 50, useNA = "always")
head(rev(sort(table(fledge.ped$sire))), n = 50, useNA = "always")

###### Unknown parents of ringed chicks ----
dim(fledge.ped[which(fledge.ped$dam == "" | is.na(fledge.ped$dam)),])[1]  # Number of ringed pulli with unknown mother
dim(fledge.ped[which(fledge.ped$sire == "" | is.na(fledge.ped$sire)),])[1]  # Number of ringed pulli with unknown father

#### Create parental identities to protect sibships
fledge.ped[which(fledge.ped$dam == "" | is.na(fledge.ped$dam)),]$dam <- paste0("dam_", fledge.ped[which(fledge.ped$dam == "" | is.na(fledge.ped$dam)),]$pnum); stopifnot(dim(fledge.ped[which(fledge.ped$dam == "" | is.na(fledge.ped$dam)),])[1] == 0)
fledge.ped[which(fledge.ped$sire == "" | is.na(fledge.ped$sire)),]$sire <- paste0("sire_", fledge.ped[which(fledge.ped$sire == "" | is.na(fledge.ped$sire)),]$pnum); stopifnot(dim(fledge.ped[which(fledge.ped$sire == "" | is.na(fledge.ped$sire)),])[1] == 0)


###### Dead ringed chicks (INCOMPLETE) ----
sum(is.na(gt.data[gt.data$Dead.ringed.chick.ids != "",]$Dead.ringed.chick.ids))  # How many dead ringed chicks?
length(gt.data[gt.data$Dead.ringed.chick.ids != "" & !is.na(gt.data$Dead.ringed.chick.ids),]$Dead.ringed.chick.ids)  # How many nests with dead ringed chicks?
scan(text = gt.data[gt.data$Dead.ringed.chick.ids != "" & !is.na(gt.data$Dead.ringed.chick.ids),]$Dead.ringed.chick.ids, sep = ",", what = "character")


##### 2013-2023 ----
dim(gt.pulli_2)[1]
dim(gt.pulli_2[gt.pulli_2$location == "" | is.na(gt.pulli_2$location),])[1]  # Number of fledglings of unknown nest-of-origin
stopifnot(length((gt.pulli[which(gt.pulli$pnum == ""),]$location)) == 0)  # Confirm that all non-NA nest IDs are known
recent.fledgling.ped <- merge(x = gt.pulli_2, y = gt.data[c("Pnum", "Mother", "Father")], by.x = "location", by.y = "Pnum", all.x = TRUE, all.y = FALSE); dim(recent.fledgling.ped)[1]
head(rev(sort(table(recent.fledgling.ped$Mother))), n = 10, useNA = "always"); recent.most.successful.mother.count <- rev(sort(table(recent.fledgling.ped$Mother)))[2]
head(rev(sort(table(recent.fledgling.ped$Father))), n = 10, useNA = "always"); recent.most.successful.father.count <- rev(sort(table(recent.fledgling.ped$Father)))[2]

###### Create dummy parent IDs ----
recent.fledgling.ped[which(recent.fledgling.ped$Mother == ""),]$Mother <- paste0("dam_", recent.fledgling.ped[which(recent.fledgling.ped$Mother == ""),]$location)
recent.fledgling.ped[which(recent.fledgling.ped$Father == ""),]$Father <- paste0("dam_", recent.fledgling.ped[which(recent.fledgling.ped$Father == ""),]$location)                                                             
                                                   
head(rev(sort(table(recent.fledgling.ped$Mother))), n = 10, useNA = "always"); stopifnot(max(table(recent.fledgling.ped$Mother)) == recent.most.successful.mother.count)
head(rev(sort(table(recent.fledgling.ped$Father))), n = 10, useNA = "always"); stopifnot(max(table(recent.fledgling.ped$Father)) == recent.most.successful.father.count)

# What are the greatest numbers of ringed chicks per nest?
head(rev(sort(table(recent.fledgling.ped$location), useNA = "always")), n = 30)
  
# Counts of unknown mothers and fathers
dim(recent.fledgling.ped[which(recent.fledgling.ped$Mother == "" | is.na(recent.fledgling.ped$Mother)),])[1]; head(rev(sort(table(recent.fledgling.ped$Mother, useNA = "always"))))
dim(recent.fledgling.ped[which(recent.fledgling.ped$Father == "" | is.na(recent.fledgling.ped$Father)),])[1]; head(rev(sort(table(recent.fledgling.ped$Father, useNA = "always"))))

# Get parent identities (if known) from gt.data
recent.fledgling.ped <- merge(x = gt.pulli_2, y = gt.data[c("Pnum", "Mother", "Father")], by.x = "location", by.y = "Pnum", all.x = TRUE, all.y = FALSE); colnames(recent.fledgling.ped) <- c("id", "dam", "sire")
head(rev(sort(table(recent.fledgling.ped$dam))), n = 50, useNA = "always")
head(rev(sort(table(recent.fledgling.ped$sire))), n = 50, useNA = "always")

#### Create parental identities to protect sibships
recent.fledgling.ped[which(is.na(recent.fledgling.ped$Mother)),]$Mother <- paste0("dam_", recent.fledgling.ped[which(is.na(recent.fledgling.ped$Mother)),]$location); stopifnot(dim(recent.fledgling.ped[which(recent.fledgling.ped$Mother == "dam_" | is.na(recent.fledgling.ped$Mother)),])[1] == 0)
recent.fledgling.ped[which(is.na(recent.fledgling.ped$Father)),]$Father <- paste0("sire_", recent.fledgling.ped[which(is.na(recent.fledgling.ped$Father)),]$location); stopifnot(dim(recent.fledgling.ped[which(recent.fledgling.ped$Father == "dam_" | is.na(recent.fledgling.ped$Father)),])[1] == 0)

# Everyone has been assigned a mother and a father, whether real or dummy
recent.fledgling.ped$id <- recent.fledgling.ped$ring
recent.fledgling.ped$dam <- recent.fledgling.ped$Mother
recent.fledgling.ped$sire <- recent.fledgling.ped$Father

###### Dead ringed chicks (INCOMPLETE) ----
sum(is.na(gt.data[gt.data$Dead.ringed.chick.ids != "",]$Dead.ringed.chick.ids))  # How many dead ringed chicks?
length(gt.data[gt.data$Dead.ringed.chick.ids != "" & !is.na(gt.data$Dead.ringed.chick.ids),]$Dead.ringed.chick.ids)  # How many nests with dead ringed chicks?
scan(text = gt.data[gt.data$Dead.ringed.chick.ids != "" & !is.na(gt.data$Dead.ringed.chick.ids),]$Dead.ringed.chick.ids, sep = ",", what = "character")



#### Eggs that did not generate ringed chicks (1960-2023) ----
table(gt.data$Clutch.size - gt.data$Num.fledglings)  # Nest-level frequency table of ringed chicks failing to fledge
gt.data$nest.deaths <- gt.data$Clutch.size - gt.data$Num.fledglings  # Number of eggs that did not generate a fledgling

# Annual egg production statistics
annual.egg.totals <- aggregate(gt.data$Clutch.size, by = list(Category = gt.data$year), FUN = sum); colnames(annual.egg.totals) <- c("year", "egg.count")
sum(annual.egg.totals$egg.count); mean(annual.egg.totals$egg.count); range(annual.egg.totals$egg.count) # Total number of eggs laid over study period; annual mean; range

# Annual fledgling production statistics
annual.fledgling.totals <- aggregate(gt.data$Num.fledglings, by = list(Category = gt.data$year), FUN = sum); colnames(annual.fledgling.totals) <- c("year", "fledgling.count")
sum(annual.fledgling.totals$fledgling.count); mean(annual.fledgling.totals$fledgling.count); range(annual.fledgling.totals$fledgling.count)  # Total number of fledglings produced over study period; annual mean; range

# In-nest mortality statistics
sum(annual.fledgling.totals$fledgling.count) / sum(annual.egg.totals$egg.count); range(annual.fledgling.totals$fledgling.count / annual.egg.totals$egg.count)

# Back to pedigree construction...
nests.with.dead <- unique(gt.data[which(gt.data$nest.deaths > 0),]$Pnum); length(nests.with.dead); stopifnot(length(nests.with.dead) == length(gt.data[which(gt.data$nest.deaths > 0),]$Pnum))  # Double-checking that Pnum values are unique
dim(gt.data[which(gt.data$Clutch.size > 0),])[1]  # Number of nests with at least one egg
mean(table(gt.data[which(gt.data$Clutch.size > 0),]$year)); min(table(gt.data[which(gt.data$Clutch.size > 0),]$year)); max(table(gt.data[which(gt.data$Clutch.size > 0),]$year))
dim(gt.data[which(gt.data$nest.deaths > 0),])[1]; dim(gt.data[which(gt.data$nest.deaths > 0),])[1]/dim(gt.data[which(gt.data$Clutch.size > 0),])[1]  # Number and proportion of nests with eggs suffering at least one case of in-nest mortality
dim(gt.data[which(gt.data$nest.deaths == gt.data$Clutch.size),])[1]; dim(gt.data[which(gt.data$nest.deaths == gt.data$Clutch.size),])[1]/dim(gt.data[which(gt.data$Clutch.size > 0),])[1]  # Number and proportion of nests with eggs suffering complete failure (i.e., no fledglings)

dummy.id <- NULL
dummy.dam <- NULL
dummy.sire <- NULL

for (i in 1:length(nests.with.dead)){

  for (j in 1:gt.data[gt.data$Pnum == nests.with.dead[i],]$nest.deaths){
  dummy.id <- c(dummy.id, paste0("dead_", gt.data[gt.data$Pnum == nests.with.dead[i],]$Pnum, "_", j))
  dummy.dam <- c(dummy.dam, gt.data[gt.data$Pnum == nests.with.dead[i],]$Mother)
    if (is.na(dummy.dam[length(dummy.dam)])){
      dummy.dam[length(dummy.dam)] <- paste0("dam_", gt.data[gt.data$Pnum == nests.with.dead[i],]$Pnum)
    }
  dummy.sire <- c(dummy.sire, gt.data[gt.data$Pnum == nests.with.dead[i],]$Father)
    if (is.na(dummy.sire[length(dummy.sire)])){
      dummy.sire[length(dummy.sire)] <- paste0("sire_", gt.data[gt.data$Pnum == nests.with.dead[i],]$Pnum)
    }
  }
}

stopifnot(length(dummy.id) == sum(gt.data$nest.deaths)); stopifnot(sum(gt.data$nest.deaths) + sum(gt.data$Num.fledglings) == sum(gt.data$Clutch.size))  # Check that all in-nest deaths have been accounted for
dead.ped <- data.frame(dummy.id, dummy.dam, dummy.sire); dim(dead.ped)


#----------------------------------------------------------------------#
# OPPORTUNITY FOR SURVIVAL I: conception to fledging ==================
#----------------------------------------------------------------------#
# Plot w(1)
dev.off()
plot(w1 ~ year, data = gt.annual.data)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$w1, df = 10))
plot(I1 ~ year, data = gt.annual.data)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$I1, df = 10))


# Plotting annual in-nest survival for both species
dev.off(); par(mar = c(4, 4, 1, 1))
gt <- "darkolivegreen3"
bt <- "cadetblue1"
plot(w1 ~ year, 
     data = gt.annual.data, 
     xlab = "Year",
     ylab = "In-nest survival rate",
     pch = 21,
     bg = gt,
     bty = "l",
     ylim = c(min(c(gt.annual.data$w1, bt.annual.data$w1)), max(c(gt.annual.data$w1, bt.annual.data$w1))),
     las = 1
     )
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$w1, df = 10), col = gt)

points(x = bt.annual.data$year, y = bt.annual.data$w1, pch = 21, bg = bt)
lines(smooth.spline(x = bt.annual.data$year, y = bt.annual.data$w1, df = 10), col = bt)
legend("bottomright", legend = c("Great tits", "Blue tits"), pch = 21, pt.bg = c(gt, bt))

# Plotting annual opportunity for in-nest selection for both species
dev.off(); par(mar = c(4, 4, 1, 1))
plot(I1 ~ year, 
     data = gt.annual.data, 
     xlab = "Year",
     ylab = "Opportunity for in-nest selection",
     pch = 21,
     bg = gt,
     bty = "l",
     ylim = c(min(c(gt.annual.data$I1, bt.annual.data$I1)), max(c(gt.annual.data$I1, bt.annual.data$I1))),
     las = 1
     )
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$I1, df = 10), col = gt)

points(x = bt.annual.data$year, y = bt.annual.data$I1, pch = 21, bg = bt)
lines(smooth.spline(x = bt.annual.data$year, y = bt.annual.data$I1, df = 10), col = bt)
legend("bottomright", legend = c("Great tits", "Blue tits"), pch = 21, pt.bg = c(gt, bt))

## Stacked plot of I ----
dev.off(); par(mfrow = c(2,1))
gt <- "darkolivegreen3"
bt <- "cadetblue1"

# Great tit in-nest opportunity for selection
par(mar = c(2, 5, 4, 1))
plot(I1 ~ year, 
     data = gt.annual.data, 
     xlab = "",
     ylab = "Opportunity for\nin-nest selection",
     pch = 21,
     bg = gt,
     bty = "l",
     xaxt = "n",
     las = 1,
     main = "Great tits"
     )
#text(x = min(gt.annual.data$year), y = max(gt.annual.data$I1), labels = "a", cex = 1.5)
lines(smooth.spline(x = gt.annual.data$year, y = gt.annual.data$I1, df = 10), col = gt)
axis(side = 1, labels = F)

# Blue tit in-nest opportunity for selection
par(mar = c(4, 5, 2, 1))
plot(I1 ~ year, 
     data = bt.annual.data, 
     xlab = "Year",
     ylab = "Opportunity for\nin-nest selection",
     pch = 21,
     bg = bt,
     bty = "l",
     las = 1,
     main = "Blue tits"
     )
#text(x = min(bt.annual.data$year), y = max(bt.annual.data$I1), labels = "b", cex = 1.5)
lines(smooth.spline(x = bt.annual.data$year, y = bt.annual.data$I1, df = 10), col = bt)


# Can breeding density explain changes in in-nest survival?
# Exclude years before 1975 because we have an explanation for the chagnes seen then
gt.density.model <- lm(gt.annual.data[gt.annual.data$year >= 1975,]$w1 ~ gt.annual.data[gt.annual.data$year >= 1975,]$nest.count)
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

# Blue tits
bt.density.model <- lm(bt.annual.data[bt.annual.data$year >= 1975,]$w1 ~ bt.annual.data[bt.annual.data$year >= 1975,]$nest.count + gt.annual.data[gt.annual.data$year >= 1975,]$nest.count)
plot(y = bt.annual.data[bt.annual.data$year >= 1975,]$w1, x = bt.annual.data[bt.annual.data$year >= 1975,]$nest.count)
abline(bt.density.model)
summary(bt.density.model)


# POST-FLEDGING SURVIVAL ----

## Great tits ----
### Males ----
recruited.male.gt.fledglings <- aggregate(!is.na(match(gt.pulli$bto_ring, gt.data$Father)), by = list(gt.pulli$year), FUN = sum)
colnames(recruited.male.gt.fledglings) <- c("year", "total"); recruited.male.gt.fledglings

### Females ----
recruited.female.gt.fledglings <- aggregate(!is.na(match(gt.pulli$bto_ring, gt.data$Mother)), by = list(gt.pulli$year), FUN = sum)
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

gt.w.2 <-  gt.data$number.recruits/gt.data$number.fledglings
gt.I.2 <- (1 - gt.w.2)/(gt.w.2)

gt.w.3 <- gt.data$mean.breeder.egg.production
gt.I.3 <- gt.data$I.breeders
 
gt.w.1.2 <- gt.data$number.recruits/gt.data$number.eggs  # Mean egg-to-recruitment survival
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
axis(side =1, at=c(0, 1.2*length(gt.data$year)), labels = F, lwd.ticks = 0)
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
plot(x = gt.data$year, y = gt.I.1/gt.I.1.2, xlab =  "", ylab = "", bty = "l", las = 1, pch = 16, ylim = c(0, 1.15*max(c(gt.I.1/gt.I.1.2, gt.I.1/global.gt.I.1.2))), yaxs = "i")
mtext("Year", side = 1, line = 2.2)
mtext(expression(paste("Proportion ", italic("I"), ""[(fertilisation:recruitment)])), side = 2, line = 3.8)
mtext("attributed to in-nest survival", side = 2, line = 3)
lines(smooth.spline(x = gt.data$year, y = gt.I.1/gt.I.1.2, df = 12), col = "black", lwd = 1)

legend("topright", legend = c("observed local recruitment   ", ""), pt.cex = c(0.95, 1.2), pch = c(16, 18),  col = c("black", "white"), cex = 0.75, bg = "white")

points(x = gt.data$year, y = gt.I.1/(gt.I.1+(global.gt.I.2/gt.w.1)), pch = 18, col = "darkblue", cex =1.1)
lines(smooth.spline(x = gt.data$year, y = gt.I.1/(gt.I.1+(global.gt.I.2/gt.w.1)), df = 12), col = "darkblue", lwd = 1)

legend("topright", legend = c("observed local recruitment   ", "estimated global recruitment"), pt.cex = c(0.95, 1.2), pch = c(16, 18),  col = c("black", "darkblue"), cex = 0.75)

mean((gt.I.1/global.gt.I.1.2) / (gt.I.1/gt.I.1.2))# Mean relative difference

# Opportunity for selection via fledging:recruitment survival
par(mar = c(4, 4, 1, 1))
plot(x = gt.data$year, y = gt.I.2, xlab =  "", ylab = "Opportunity for selection via fledging:recruitment survival", bty = "l", las = 1, pch = 16, cex = 0.95, ylim = c(min(c(gt.I.2, global.gt.I.2)), 1.*max(c(gt.I.2, global.gt.I.2))))
mtext("Year", side = 1, line = 2.5)
lines(smooth.spline(x = gt.data$year, y = gt.I.2, df = 10), col = "black", lwd = 1)

points(x = gt.data$year, y = global.gt.I.2, pch = 18, col = "darkblue", cex =1.2)
lines(smooth.spline(x = gt.data$year, y = global.gt.I.2, df = 10), col = "darkblue", lwd = 1)

legend("topright", legend = c("observed local recruitment", "estimated global recruitment"), pt.cex = c(0.95, 1.2), pch = c(16, 18),  col = c("black", "darkblue"), cex = 0.85)


# So how would our barplot look?
data <- data.frame(gt.I.1, gt.I.2/gt.w.1, global.gt.I.2/gt.w.1)
par(mar = c(3, 4.5, 1, 1))
barplot(as.matrix(t(data)), las = 1, ylab = "", ylim = c(0, max(gt.I.1.2)))
axis(side = 1, at = c(2, 14, 26, 38, 50, 62), labels = seq(1960, 2010, 10))
axis(side =1, at=c(0, 1.2*length(gt.data$year)), labels = F, lwd.ticks = 0)
axis(side = 2, at = c(0, 100), labels = F)
mtext("Year", side = 1, line = 2, cex = 1.1)
mtext("Opportunity for selection", side = 2, line = 3.5, cex = 1.1)
mtext(expression(paste(italic("I"), ""[(fertilisation:recruitment)])), side = 2, line = 2.2, cex = 1.1)
legend("topright", legend = c("","Post-fledging                  ", "Pre-fledging"), bty = "n", fill = c("white", rev(grey.colors(3)[1:2])), cex = 1, box.col = c("white", "black", "black"))
legend("topright", legend = c("Post-fledging (corrected)", "Post-fledging (observed)", "Pre-fledging"), bty = "n", fill = c("grey90", rev(grey.colors(2))), cex = 1)



span <- c(0, max(c(gt.I.1, gt.I.2, gt.I.1.2)))
par(mfrow = c(3,1), mar = c(3, 4, 0, 0))
plot(x = gt.data$year, y = gt.I.1, xlab =  "Year", ylab = "Opportunity for survival selection (to fledging)", bty = "l", las = 1, ylim = span)

plot(x = gt.data$year, y = gt.I.2, xlab =  "Year", ylab = "Opportunity for survival selection (first-year)", bty = "l", las = 1, ylim = span)

plot(x = gt.data$year, y = gt.I.1.2, xlab =  "Year", ylab = "Opportunity for survival selection (to recruitment)", bty = "l", las = 1, ylim = span)

plot(x = gt.data$year, y = gt.I.3, xlab =  "Year", ylab = "Opportunity for selection via egg production", bty = "l", las = 1) 
fit <- smooth.spline(x = gt.data$year, y = gt.I.3, df = 8)
lines(fit, col = "red", lwd = 2)
 
# Proportion of total opportunity for selection via survival to recruitment attributable to survival in the nest
plot(x = gt.data$year, y = gt.I.1/gt.I.1.2, xlab =  "Year", ylab = "Proportion attributed to pre-fledging survival", bty = "l", las = 1, pch = 16)
lines(smooth.spline(x = gt.data$year, y = gt.I.1/gt.I.1.2, df = 15), col = "red", lwd = 2)

# Proportion of total opportunity for selection attributable to recruitment of fledglings
plot(x = gt.data$year, y = gt.I.2/gt.I.1.2.3, xlab =  "Year", ylab = "Proportion total I attributable to post-fledging survival", bty = "l", las = 1, pch = 16)
fit <- smooth.spline(x = gt.data$year, y = gt.I.2/gt.I.1.2.3, df = 8)
lines(fit, col = "red", lwd = 2)

# Proportion of total opportunity for selection attributable to...
plot(x = gt.data$year, y = (gt.I.3/gt.w.2)/gt.I.1.2.3, xlab =  "Year", ylab = "Proportion total I attributable to post-fledging survival", bty = "l", las = 1, pch = 16, col = "green")
points(x = gt.data$year, y = gt.I.1/gt.I.1.2.3,  pch = 16, col = "red")
points(x = gt.data$year, y = (gt.I.2/gt.w.1)/gt.I.1.2.3,  pch = 16, col = "blue")
fit.I.1 <- smooth.spline(x = gt.data$year, y = gt.I.2/gt.I.1.2.3, df = 8)
fit.I.2 <- smooth.spline(x = gt.data$year, y = (gt.w.1*gt.I.2)/gt.I.1.2.3, df = 8)
fit.I.3 <- smooth.spline(x = gt.data$year, y = (gt.w.2*gt.I.3)/gt.I.1.2.3, df = 8)
lines(fit.I.1, col = "red", lwd = 2)
lines(fit.I.2, col = "blue", lwd = 2)
lines(fit.I.3, col = "green", lwd = 2)

# Proportion of 'missing' variance in survival
plot(x = gt.data$year, y = (gt.I.1.2 - (gt.I.1+gt.I.2))/gt.I.1.2, xlab =  "Year", ylab = "'Missing' I", bty = "l", las = 1, pch = 16)
fit <- smooth.spline(x = gt.data$year, y = (gt.I.1.2 - (gt.I.1+gt.I.2))/gt.I.1.2, df = 8)
lines(fit, col = "red", lwd = 2)


I.1 + (I.2*w.1)  # formula for summing total opportunity for selection across ontogeny
w.1


#------------------#
# WYTHAM BLUE TITS #
#------------------#

bt.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Annual BT totals (eggs, hatchlings, fledglings, recruits).csv", header = T, na.strings = "NA")

# STACKED BAR CHART
in.nest.data <- bt.data[bt.data$year > 1964,]
barplot(c(in.nest.data$number.eggs-in.nest.data$number.hatchlings, in.nest.data$number.hatchlings-in.nest.data$number.fledglings, in.nest.data$number.fledglings),
	names.arg =  in.nest.data[,1],
	las = 1
	)
axis(side = 2, at = c(0, 100000), labels = F, tick = F)	
legend("topleft", legend = c("Fledged", "Nestling mortality", "Prenatal mortality"), bty = "n", fill = rev(gray.colors(3)), cex = 0.9)


# OPPORTUNITY FOR SELECTION
bt.w.1 <- bt.data$number.hatchlings/bt.data$number.eggs
bt.I.1 <- (1 - bt.w.1)/bt.w.1
 
bt.w.2 <-  bt.data$number.fledglings/bt.data$number.hatchlings
bt.I.2 <- (1 - bt.w.2)/(bt.w.2)
 
bt.w.1.2 <- bt.data$number.fledglings/bt.data$number.eggs
bt.I.1.2 <- (1 - bt.w.1.2)/bt.w.1.2

plot(x = bt.data$year, y = bt.I.1/bt.I.1.2, xlab =  "Year", ylab = "Proportion attributed to prenatal survival", bty = "l", las = 1, pch = 16, main = "")
fit <- smooth.spline(x = bt.data$year, y = bt.I.1/bt.I.1.2, df = 8)
lines(fit, col = "red", lwd = 2)

plot(x = bt.data$year, y = bt.I.1.2, xlab =  "Year", ylab = "Opportunity for selection (pre-fledgling)", bty = "l", las = 1, pch = 16, main = "")


# FIGURE COMBINING PLOTS FOR GREAT TITS *AND* BLUE TITS
par(mfrow = c(2, 2), mar = c(3, 3.5, 1, 1.5))
label.size <- 0.8
# Great tits

plot(x = gt.data$year, y = gt.I.1, xlab =  "Year", ylab = "", bty = "l", las = 1, pch = 21, col = "black", bg = "darkred", cex = 0.8)
mtext(text = "Opportunity for selection (in-nest)", side = 2, line = 2.5, cex = label.size)
 mtext(text = "Year", side = 1, line = 2.1, cex = label.size)
lines(smooth.spline(x = gt.data$year, y = gt.I.1, df = 10), col = "darkred", lwd = 2)
par(mar = c(3, 4.5, 1, 0.5))
plot(x = gt.data$year, y = gt.w.1, ylab = "", bty = "l", las = 1, xlab = "", bg = "darkgreen", pch = 21, cex = 0.8)
axis(side = 1, labels = F)
mtext(text = "In-nest survival probability", side = 2, line = 2.5, cex = label.size)
lines(smooth.spline(x = gt.data$year, y = gt.w.1, df = 10), col = "darkgreen", lwd = 2)
 mtext(text = "Year", side = 1, line = 2.1, cex = label.size)

# Blue tits
plot(x = bt.data$year, y = bt.I.1.2, xlab =  "Year", ylab = "", bty = "l", las = 1, pch = 21, col = "black", bg = "darkorange", cex = 0.8)
mtext(text = "Opportunity for selection (in-nest)", side = 2, line = 2.5, cex = label.size)
 mtext(text = "Year", side = 1, line = 2.1, cex = label.size)
lines(smooth.spline(x = bt.data$year, y = bt.I.1.2, df = 10), col = "darkorange", lwd = 2)
par(mar = c(3, 4.5, 1, 0.5))
plot(x = bt.data$year, y = bt.w.1.2, ylab = "", bty = "l", las = 1, xlab = "", bg = "darkblue", pch = 21, cex = 0.8)
axis(side = 1, labels = F)
mtext(text = "In-nest survival probability", side = 2, line = 2.5, cex = label.size)
lines(smooth.spline(x = bt.data$year, y = bt.w.1, df = 10), col = "darkblue", lwd = 2)
 mtext(text = "Year", side = 1, line = 2.1, cex = label.size)


# In-nest survival for the two tit species overlaid
label.size = 1
par(mar = c(3.5, 3.5, 1, 1))
plot(x = gt.data$year-0.1, y = gt.w.1, ylab = "", bty = "l", las = 1, xlab = "", bg = "darkgreen", pch = 21, cex = 0.9, ylim = c(min(gt.w.1, bt.w.1.2), max(gt.w.1, bt.w.1.2)))
mtext(text = "In-nest survival probability", side = 2, line = 2.5, cex = label.size)
lines(smooth.spline(x = gt.data$year+0.1, y = gt.w.1, df = 10), col = "darkgreen", lwd = 2)
mtext(text = "Year", side = 1, line = 2.3, cex = label.size)

points(x = bt.data$year+0.1, y = bt.w.1.2, bg = "cadetblue1", pch = 21, cex = 0.9)
lines(smooth.spline(x = bt.data$year+0.1, y = bt.w.1.2, df = 10), col = "cadetblue1", lwd = 2)

legend("bottomright", legend = c("great tits", "blue tits"), pch = c(21, 21), pt.bg = c("darkgreen", "cadetblue1"), bty = "n", cex = 0.9)


#-----------------#

# Opportunity for selection via in-nest survival for the two tit species overlaid
label.size = 1
plot(x = gt.data$year-0.2, y = gt.I.1, ylab = "", bty = "l", las = 1, xlab = "", bg = "darkred", pch = 21, cex = 0.8, ylim = c(min(gt.I.1, bt.I.1.2), max(gt.I.1, bt.I.1.2)))
mtext(text = "Opportunity for selection (in-nest)", side = 2, line = 2.5, cex = label.size)
lines(smooth.spline(x = gt.data$year, y = gt.I.1, df = 10), col = "darkred", lwd = 2)
mtext(text = "Year", side = 1, line = 2.1, cex = label.size)

points(x = bt.data$year+0.2, y = bt.I.1.2, bg = "#FDD835", pch = 21, cex = 0.8)
lines(smooth.spline(x = bt.data$year, y = bt.I.1.2, df = 10), col = "#FDD835", lwd = 2)

legend("topright", legend = c("great tits", "blue tits"), pch = c(21, 21), pt.bg = c("darkred", "#FDD835"), bty = "n", cex = 0.9)

#-------------------#
# BAGLEY GREAT TITS #
#-------------------#

bagley.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Annual Bagley GT totals (eggs, fledglings, recruits).csv", header = T, na.strings = "NA")
bagley.data <- bagley.data[bagley.data$year <= 2012,]# 2014 field season data is incompleted (data collection interrupted by illness, so 2013 estimate of recruit number is poor)
 
bagley.w.1 <- bagley.data$number.fledglings/bagley.data$number.eggs
bagley.I.1 <- (1 - bagley.w.1)/bagley.w.1
 
bagley.w.2 <-  bagley.data$number.recruits/bagley.data$number.fledglings
bagley.I.2 <- (1 - bagley.w.2)/(bagley.w.2)
 
bagley.w.1.2 <- bagley.data$number.recruits/bagley.data$number.eggs
bagley.I.1.2 <- (1 - bagley.w.1.2)/bagley.w.1.2

plot(x = bagley.data$year, y = bagley.I.1/bagley.I.1.2, xlab =  "Year", ylab = "Proportion attributed to in-nest survival", bty = "l", las = 1, pch = 16, main = "")



#----------------------#
# COLLARED FLYCATCHERS #
#----------------------#

cf.data <- read.csv("~/Library/CloudStorage/OneDrive-UniversityofExeter/Research/Prenatal survival/Annual CF totals (eggs, hatchlings, fledglings, recruits).csv", header = T, na.strings = "NA")
cf.data <- cf.data[complete.cases(cf.data),]
cf.data <- cf.data[cf.data$year <= 2013,]# Recruitment data for these years seem incomplete

cf.w.1 <- cf.data$number.fledglings/cf.data$number.eggs
cf.I.1 <- (1 - cf.w.1)/cf.w.1

cf.w.2 <-  cf.data$number.recruits/cf.data$number.fledglings
cf.I.2 <- (1 - cf.w.2)/(cf.w.2)
 
cf.w.1.2 <- cf.data$number.recruits/cf.data$number.eggs
cf.I.1.2 <- (1 - cf.w.1.2)/cf.w.1.2

# Proportion of viability selection attributable to survival in the nest
plot(x = cf.data$year, y = cf.I.1/cf.I.1.2, xlab =  "Year", ylab = "Proportion attributed to pre-fledging survival", bty = "l", las = 1, pch = 16)
fit <- smooth.spline(x = cf.data$year, y = cf.I.1/cf.I.1.2, df = 8)
lines(fit, col = "red", lwd = 2)



plot(x = cf.data$year, y = cf.I.1.2.3, xlab =  "Year", ylab = "Opportunity for selection via early-life survival", bty = "l", las = 1, pch = 16, ylim = c(0, max(cf.I.1.2.3)))
points(x=cf.data$year, y= cf.I.1, pch = 16, col = "red")

# PLOT OF GREAT TITS AND COLLARED FLYCATCHERS
par(mfrow = c(1,2))
# Great tits
plot(x = gt.data$year, y = gt.I.1/gt.I.1.2, xlab =  "Year", ylab = "Proportion attributed to pre-fledging survival", bty = "l", las = 1, pch = 16, main = "Great tits, Oxford")
fit <- smooth.spline(x = gt.data$year, y = gt.I.1/gt.I.1.2, df = 8)
lines(fit, col = "red", lwd = 2)

# Collared flycatchers
plot(x = cf.data$year, y = cf.I.1/cf.I.1.2, xlab =  "Year", ylab = "Proportion attributed to pre-fledging survival", bty = "l", las = 1, pch = 16, main = "Collared flycatchers, Gotland")
fit <- smooth.spline(x = cf.data$year, y = cf.I.1/cf.I.1.2, df = 8)
lines(fit, col = "red", lwd = 2)


#-----------------------------------------------#
# Predation rate (from Appendix 1 of Dunn 1977) #
#-----------------------------------------------#

plot(gt.data$nest.predation.rate, gt.w.1)
cor.test(gt.data$nest.predation.rate, gt.w.1)

plot(gt.data$nest.predation.rate, gt.I.1)
cor.test(gt.data$nest.predation.rate, gt.I.1)

plot(gt.data$nest.predation.rate, gt.I.1/gt.I.1.2)
cor.test(gt.data$nest.predation.rate, gt.I.1/gt.I.1.2)