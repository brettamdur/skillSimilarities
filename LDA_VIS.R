### reference site
#http://cpsievert.github.io/LDAvis/reviews/reviews.html
setwd("~/Desktop/XXXXX")
library(readr)
library(tm)
library(SnowballC)
library(Matrix)
library(lda)
library(LDAvis)
library(servr)

# read data
jd= read_csv("job_descriptions.csv");

txt = jd$job_description


# pre-processing
txt <- gsub("'", "", txt)  # remove apostrophes
txt <- gsub("[[:punct:]]", " ", txt)  # replace punctuation with space
txt <- gsub("[[:cntrl:]]", " ", txt)  # replace control characters with space
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt <- tolower(txt)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(txt, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# stop words
stop_words =stopwords(kind = "en")

# use the skills we got 
skills= read_csv("skills_dictionary.csv");
skill <-skills$skill
topic<- names(term.table) %in% skill
term.table<- term.table[topic]
vocab <- names(term.table)



# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)



# Compute some statistics related to the data set:
D <- length(documents)
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)

# MCMC and model tuning parameters:
K <- 20
G <- 1000
alpha <- 0.02
eta <- 0.02

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  


### Visualizing the fitted model with LDAvis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './', open.browser = T)
system("mv index.html results.html")
