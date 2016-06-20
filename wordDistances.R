#############################################################################################
# File:   wordDistances.R                                                                   #
# Author: Brett Amdur                                                                       #
# Client: XXXXX                                                                          #
# Notes:  1) Applies word2vec to resume text                                                #
#         2) Finds clusters of skills among resume words                                    #
#         3) Creates ability to plot distances between a word and other words closest to it #
#############################################################################################


# Requred Packages:
require(tm)
require(ggplot2)
require(dplyr)
require(NbClust)
require(wordVectors)
require(NbClust)

##############################################################
### DATA SETUP  ##############################################
##############################################################

# modelLarge <- train_word2vec('./../../dataHold/resumes.csv', output_file = 'modelLarge.bin', threads = 2, vectors = 100, window = 12)
# model <- readRDS('model.rds')
model = read.vectors("modelLarge.bin")

# Build a list of skills we're looking to group on.  Here, using list of ~3k skills obtained from XXXXX
skillsOrig <- read.csv('./../data/skills_dictionary.csv', stringsAsFactors = FALSE)
skills <- stemDocument(skillsOrig$skill)
skills <- skills[! duplicated(skills)] # get rid of duplicates


##############################################################
### CREATING WORD VECTORS  ###################################
##############################################################

# Create a data frame of word vectors -- one vector (in a row) per each word in skills.
# Start with a list of vectors, then make each vector a row in a dataframe

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


##############################################################
### CLUSTERING  ##############################################
##############################################################

set.seed(1)

# cluster the skills via kmeans.  
kmSkills <- kmeans(skillsVectorsDF, centers = 15)

# generate a skill/cluster dataframe
kmSkillsClustOrd <- kmSkills$cluster[order(kmSkills$cluster)]
kmSkillsDF <- data.frame(skill = names(kmSkillsClustOrd), cluster = unname(kmSkillsClustOrd))

# to look at a particular cluster's words:
names(skillList[kmSkills$cluster == 1])

# to see the cluster for a particular word:
kmSkillsDF[kmSkillsDF$skill == 'linear_regress', ]

# to write cluster list to csv:
write.csv(x = kmSkillsDF, file = '../data/skillClusters_modelLarge.csv')

# breakdown for modelSmall:
# 1 - General Business (very large cluster -- should break this down further)
# 2 - Mobile / Mobile Platforms
# 3 - Banking and Finance
# 4 - Law and Biology
# 5 - Arts / Sports / Media
# 6 - Chemistry and Genetics
# 7 - Hardware (but rougly, this one isn't very tight)
# 8 - Social Media
# 9 - Mobile Acronyms (strange how these got their own cluster)
# 10 - Recruiting / HR
# 11 - Web Development
# 12 - Languages and Digital Art Tools (strange how these were clustered together)
# 13 - Machine Learning
# 14 - Big Data
# 15 - Systems Engineering

# for hierarchical clustering:
hclust(skillsVectorsDF[1:50, ], method = 'single')

# find optimal value for k
nb <- NbClust(skillsVectorsDF, distance = "euclidean", min.nc = 15,
              max.nc = 25, method = "complete", index ="all")



##############################################################
### FINDING NEAREST WORDS  ###################################
##############################################################

# generates a named list of top 50 closest words from words in skills.  Name is word, and value is distance
nearestSkills <-lapply(skills[1:50], function(x){nearest_to(model, model[[x]], 50)})

# plotSkills takes in skillList, which is a list of closest terms in cosine distance to a particular term, and 
# returns a plot of those closest terms.  The first entry in skillList is the reference term
plotSkills <- function(skillList){
  skillDf <- data.frame(skillName = names(skillList), value = unname(skillList))
  skillDf <- skillDf[2:nrow(skillDf), ]
  referenceSkill = names(skillList)[1]
  skillListLength = as.character(length(skillList))
  g <- ggplot(skillDf, aes(value, reorder(skillName, -value))) + geom_point() +
    theme_minimal() +
    ylab('Skill') + xlab('Cosine Distance') +
    ggtitle(paste('Top', skillListLength, 'Closest Terms:\n', referenceSkill))
  return(g)
}

plotSkills(nearestSkills[[10]])

plotSkills(nearest_to(model, model[['machin_learn']], 50))







