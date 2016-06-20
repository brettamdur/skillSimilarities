library(tm)


##############################################################
### DATA SETUP  ##############################################
##############################################################

model <- readRDS('model.rds')

# Build a list of skills we're looking to group on.  Here, using list of ~3k skills obtained from XXXXX
skillsOrig <- read.csv('skills_dictionary.csv', stringsAsFactors = FALSE)
skills <- stemDocument(skillsOrig$skill)
skills <- skills[! duplicated(skills)] # get rid of duplicates


##############################################################
### CREATING WORD VECTORS  ###################################
##############################################################

# create a data frame of word vectors -- one vector (in a row) per each word in skills
#skillsVectors <- lapply(skills,function(x) model[[x]])
skillsVectors <- lapply(skills,function(x){
  if(x %in% rownames(model)){
    model[[x]]
  }
})
skillsVectors <- skillsVectors[! sapply(skillsVectors, is.null)] # gets rid of null entries (where skill isn't in corpus):
# skillsVectors is now a list of "vectorSpaceModel"s, so we turn each into a list, then unlist the 
# list of lists in line below:
skillsVectors <- lapply(skillsVectors, function(x) unlist(as.list(x)))
skillsVectorsDF <- do.call(rbind, skillsVectors)
dfColNames <- paste('V', seq(1:ncol(skillsVectorsDF)), sep = "")
colnames(skillsVectorsDF) <- dfColNames
dfRowNames <- sapply(skills, function(x) if(x %in% rownames(model)){x})
dfRowNames <- dfRowNames[! sapply(dfRowNames, is.null)] # gets rid of null entries (where skill isn't in corpus):
rownames(skillsVectorsDF) <- dfRowNames
skillList <- dfRowNames

scaled_skills=scale(skillsVectorsDF)
#get distance measure between the word vectors
d = dist(scaled_skills)

#generate heirarchical cluster using average linkage
fit.complete = hclust(d, method = "complete")



#plot the dendrogram
plot(fit.complete, hang = -1, main = "Dendrogram of Complete Linkage")

#generate alternative plot

plot(as.phylo(fit.complete), cex = 0.9, label.offset = 1)

#generate sub plots so that it is more readable

#20 seems to be a good height for sub-plots so that it can still be readable
dendro =  cut(as.dendrogram(fit.complete), h=20)
dendro

#do a bunch of sub plots, at h=20 I have 22
#plots 7 and 8 are still too big and should be subdivided
plot(dendro$lower[[1]], main = "Branch 1")
plot(dendro$lower[[2]],main = "Branch 2")
plot(dendro$lower[[3]],main = "Branch 3")
plot(dendro$lower[[4]],main = "Branch 4")
plot(dendro$lower[[5]],main = "Branch 5")
plot(dendro$lower[[6]],main = "Branch 6")
plot(dendro$lower[[7]],main = "Branch 7")#too big
plot(dendro$lower[[8]],main = "Branch 8")#too big
plot(dendro$lower[[9]],main = "Branch 9")
plot(dendro$lower[[10]],main = "Branch 10")
plot(dendro$lower[[11]],main = "Branch 11")
plot(dendro$lower[[12]],main = "Branch 12")
plot(dendro$lower[[13]],main = "Branch 13")
plot(dendro$lower[[14]],main = "Branch 14")
plot(dendro$lower[[15]],main = "Branch 15")
plot(dendro$lower[[16]],main = "Branch 16")
plot(dendro$lower[[17]],main = "Branch 17")
plot(dendro$lower[[18]],main = "Branch 18")
plot(dendro$lower[[19]],main = "Branch 19")
plot(dendro$lower[[20]],main = "Branch 20")
plot(dendro$lower[[21]],main = "Branch 21")
plot(dendro$lower[[22]],main = "Branch 22")
plot(dendro$upper)

plot(dendro$lower[[7]])
dendro2=cut(dendro$lower[[7]],h=16)#seems like a good number, gives 19 sub plots
dendro2
plot(dendro2$lower[[1]], main = "Branch 1")
plot(dendro2$lower[[2]],main = "Branch 2")
plot(dendro2$lower[[3]],main = "Branch 3")
plot(dendro2$lower[[4]],main = "Branch 4")
plot(dendro2$lower[[5]],main = "Branch 5")
plot(dendro2$lower[[6]],main = "Branch 6")
plot(dendro2$lower[[7]],main = "Branch 7")
plot(dendro2$lower[[8]],main = "Branch 8")
plot(dendro2$lower[[9]],main = "Branch 9")
plot(dendro2$lower[[10]],main = "Branch 10")
plot(dendro2$lower[[11]],main = "Branch 11")
plot(dendro2$lower[[12]],main = "Branch 12")
plot(dendro2$lower[[13]],main = "Branch 13")
plot(dendro2$lower[[14]],main = "Branch 14")
plot(dendro2$lower[[15]],main = "Branch 15")#still too big
plot(dendro2$lower[[16]],main = "Branch 16")
plot(dendro2$lower[[17]],main = "Branch 17")#still too big
plot(dendro2$lower[[18]],main = "Branch 18")
plot(dendro2$lower[[19]],main = "Branch 19")
plot(dendro2$upper)


#Another way to visualize is in a table that shows grouping by height in the dendrogram
#this is also a convient way to calculate a "distance" between skills
clusters.complete = cutree(fit.complete, h = c(1:30))
clusters.complete
View(clusters.complete)


#this is also a convient way to calculate a "distance" between skills
#function takes two skills and returns a measure of how closely related they are
skill_dist=function(skill1,skill2){
  for(i in 1:(30))
    if(clusters.complete[skill1,i]==clusters.complete[skill2,i]){
      return(i)
    }
}
