##########################################################################################
### R script to see how classes relate to different other variables (step-3 like analysis)
###   - construct an mgm network to see how class-membership (nominal variable) relates to:
###       - isi (sleep)
###       - psqi (sleep)
###       - sf-36 (quality of life)
###       - hads (anxiety and depression)
###       - swls (satisfcation with life)
##########################################################################################

rm(list = ls())

### Load libraries
require(qgraph)
#require(xlsx)
#require(scales)
#require(MatchIt)
require(mgm)


### Load data
setwd("~/Dropbox (S&C)/NIN/01_Projects/01_LCA/00_DataAnalysesComplete/Data")
dataIns <- read.csv("VarMissMax16_scored_NewOrder.csv")
data    <- dataIns


### Load data for controls
setwd("~/Dropbox (S&C)/NIN/01_Projects/01_LCA/00_DataAnalysesComplete/Data")
dataControl <- read.csv("VarMissMax16_controls.csv")


### Load data other variables
setwd("~/Dropbox (S&C)/NIN/01_Projects/01_LCA/zz_DataAnalysesComplete_ddPre0920/01_DataSets/0.SubsequentAnalyses/170130_MGMdata")
isi.items <- read.csv("isi_items.csv")
isi.total <- read.csv("isi.csv")
psqi      <- read.csv("psqi.csv")
psqi_D    <- read.csv("psqi_duration.csv")
qol       <- read.csv("qol.csv")
hads      <- read.csv("hads.csv")
#ids       <- read.csv("ids.csv") # removed ids because of too many missings
swls      <- read.csv("swls.csv")
hsp       <- read.csv("hsp.csv")


### Keep only composite scores (for now) + subject for each variable
psqi   <- psqi[, c("subject", "PSQI")]
psqi_D <- psqi_D[, c('subject', "dur")]
qol    <- qol[, c("subject", "SF36_PCS", "SF36_MCS")]
hads   <- hads[, c("subject", "hads_anxiety", "hads_depression")]
swls   <- swls[, c("subject", "swls")]
hsp    <- hsp[, c("subject", "hsp")]



### Prepare data
###   Change Cluster labels such that cluster 1 = highest
table(data$Cluster)
#data$Cluster <- as.factor(data$Cluster)
order(table(data$Cluster), decreasing = TRUE)

###   Select participants: 
Clust <- data[, c("subject", "Cluster")] # for ins ppn (including cluster membership)
Incl  <- as.data.frame(dataControl[, "subject"]); colnames(Incl) <- "subject"

###   Select variables
include <- list(Clust, isi.total, psqi, qol, hads, swls, hsp) # select for insomnia 
include <- list(Incl, #isi.items, psqi_D, # comment isi.items + psqi_D out when running mgm analyses 
                isi.total, psqi, qol, hads, swls, hsp) # select for controls

data   = Reduce(function(...) merge(..., by = "subject", all.x = TRUE), include)

complete  <- data[complete.cases(data), ]  

table(complete$Cluster)
prop.table(table(complete$Cluster)) # corresponds roughly to percentages in used dataset


### mgm network including class-membership as a nominal node (N = 715)
mymgm <- mgmfit(complete[, -1], type = c("c", rep("g", 8)), lev = c(5, rep(1, 8)))
round(mymgm$wadj,2)

nodeLabels = c("Cluster", "ISI", "PSQI", "SF36\nPhys", 
               "SF36\nMental",  "HADS\nAnx", "HADS\nDep", "SWLS", "HSP")

qgraph(mymgm$wadj, labels = nodeLabels, shape = c("square", rep("circle", 8)),
       layout = "spring", edge.color = mymgm$edgecolor,
       label.cex = .8, label.prop = .8, label.scale.equal = TRUE)


### network analysis on control data set (N = 833)
mymgm <- mgmfit(complete[, -1], type = rep("g", 7), lev = rep(1, 7))
round(mymgm$wadj,2)

nodeLabels = c("ISI", "PSQI", "SF36\nPhys", "SF36\nMental", 
               "HADS\nAnx", "HADS\nDep", "SWLS")

ControlNetwork <- qgraph(mymgm$wadj, layout = "spring")

Lay <- ControlNetwork$layout
Lay[4,2] <- 0

qgraph(mymgm$wadj, labels = nodeLabels, 
       layout = Lay, edge.color = mymgm$edgecolor,
       label.cex = .8, label.prop = .8, label.scale.equal = TRUE)


### PAIRWISE COMPARISONS

aggregate(complete[, "SF36_MCS"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "SF36_MCS"], list(complete$Cluster), sd, na.rm = TRUE)
pairwise.t.test(complete$SF36_MCS, complete$Cluster, p.adjust.method = "bonferroni")

aggregate(complete[, "hads_anxiety"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "hads_anxiety"], list(complete$Cluster), sd, na.rm = TRUE)
pairwise.t.test(complete$hads_anxiety, complete$Cluster, p.adjust.method = "bonferroni")

aggregate(complete[, "swls"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "swls"], list(complete$Cluster), sd, na.rm = TRUE)
pairwise.t.test(complete$swls, complete$Cluster, p.adjust.method = "bonferroni")

aggregate(complete[, "hsp"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "hsp"], list(complete$Cluster), sd, na.rm = TRUE)
pairwise.t.test(complete$hsp, complete$Cluster, p.adjust.method = "bonferroni")

aggregate(complete[, "hads_depression"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "hads_depression"], list(complete$Cluster), sd, na.rm = TRUE)

aggregate(complete[, "SF36_PCS"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "SF36_PCS"], list(complete$Cluster), sd, na.rm = TRUE)

aggregate(complete[, "PSQI"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "PSQI"], list(complete$Cluster), sd, na.rm = TRUE)











idx <- which(dataControl$subject %in% complete$subject)

mean(dataControl[idx, "Age_Years"], na.rm = TRUE)
sd(dataControl[idx, "Age_Years"], na.rm = TRUE)

prop.table(table(dataControl[idx, "Male"]))



idx <- which(dataIns$subject %in% complete$subject)


table(dataIns[idx, "Cluster"]) 

ClustSex <- table(dataIns[idx, "Male"], dataIns[idx, "Cluster"]); rownames(ClustSex) <- c("Female", "Male")
ClustSex[1,] / colSums(ClustSex)








x <- merge(hsp, data[, c("subject", "Cluster")], by = "subject")
aggregate(complete[, "SF36_MCS"], list(complete$Cluster), mean, na.rm = TRUE)
aggregate(complete[, "SF36_MCS"], list(complete$Cluster), sd, na.rm = TRUE)
pairwise.t.test(complete$SF36_MCS, complete$Cluster, p.adjust.method = "bonferroni")
