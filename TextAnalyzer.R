library("stringr")
library("tokenizers")
library("tidyverse")
library("openNLP")
library("NLP")
library("phrasemachine")

# read in all the documents
files <- list.files(path = "~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/journal articles/", pattern = "*.txt")
documents <- c(1:(length(files) + 2))
for (i in 1:length(files)) {
  fileName <- files[i]
  directory <- "~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/journal articles/"
  path = paste(directory, fileName, sep="")
  con <- file(path)
  documents[i] <- paste(readLines(con), collapse = "\n")
  close(con)
}
# Read in the tweets
con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/cleanTweets.txt")
documents[length(files) + 1] <- paste(readLines(con), collapse ="\n")
close(con)
# Get just the tweets (not all that other weird data)
# Read in the other small document
con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/synthetic.lethal.txt")
documents[length(files) + 2] <- paste(readLines(con), collapse ="\n")
close(con)

# Tokenize those documents and calculate how many contain
# the words "gene" or "protien"
documentMentions = 0
for (i in 1:length(documents)) {
  words <- tokenize_words(documents[i])
  wordsFreq <- table(words)
  wordsFreq <- data_frame(word = names(wordsFreq), count = as.numeric(wordsFreq))
  rowNumbers = c(1:nrow(wordsFreq))
  names(rowNumbers) <- wordsFreq[["word"]]
  indexGene <- rowNumbers["gene"]
  indexProtien <- rowNumbers["protein"]
  if (!is.na(indexGene) | !is.na(indexProtien)) {
    documentMentions <- documentMentions + 1
  }
}
# Get the master document
con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/masterDocument.txt")
masterDocument <- readLines(con)
close(con)

# Tokenize that document
words <- tokenize_words(masterDocument)
totalWordsFreq <- table(words[[1]])
totalWordsFreq <- data_frame(word = names(totalWordsFreq), count = as.numeric(totalWordsFreq))


# Sort by order of frequency and find the total number of words
totalWordsFreq <- arrange(totalWordsFreq, desc(count))
totalWords = sum(totalWordsFreq[["count"]])

# Make a data structure so you can find count by word name
rowNumbers = c(1:nrow(totalWordsFreq))
names(rowNumbers) <- totalWordsFreq[["word"]]

# Calculate the number of uses of "i" and also "i'm"
pronoun <- 0
index <- rowNumbers["i"]
pronoun <- as.numeric(totalWordsFreq[index, "count"])
index <- rowNumbers["im"]
pronoun <- pronoun + as.numeric(totalWordsFreq[index, "count"])
pronounPrecentage <- pronoun / totalWords

# Calculate the instances of negation words
negativeWords <- c("no", "not", "none", "no one", "nobody", "noone", "nobody", "nothing", "neither", "nowhere", "never", "hardly", "barely", "scarcely", "doesnt", "wasnt", "shouldnt", "wouldnt", "couldnt")

negations <- 0
for(i in 1:length(negativeWords)) {
  index <- rowNumbers[negativeWords[i]]
  if(!is.na(index)) {
    negations <- negations + as.numeric(totalWordsFreq[index, "count"])
  }
}
# Get word frequencies from a third party for the purpose of
# eliminating words which are in the top ten but do 
# not provide meaniningful information about the text
base_url <- "http://programminghistorian.github.io/ph-submissions/assets/basic-text-processing-in-r"
wf <- read_csv(sprintf("%s/%s", base_url, "word_frequency.csv"))
updatedTotalWF <- inner_join(totalWordsFreq, wf)
filter(updatedTotalWF, frequency < 0.09)

# Break up the corpus into smaller documents so they can be tagged
# con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/masterDocument.txt")
#tempDocument <- paste (readLines(con), collapse = "\n")
#close(con)

# segments <- strsplit(tempDocument, "\n")

# temp <- ""
#for (i in 1:length(segments[[1]])) {
#  temp <- paste(temp, segments[[1]][[i]]) 
#  if ((i %% 10 == 0) & (i != 0)) {
#    fileName <- paste("file", i)
#    fileName <- paste(fileName, ".txt", sep = "")
#    path2 <- paste("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/Small Documents/", fileName, sep = "")
#    con <- file(path2, "w")
#    cat(temp, file = con)
#    close(con)
#    temp <- ""
#  }
#}
# Use koRpus to find parts of speech to find out incidence 
# of negation and coordination
# files <- list.files(path = "~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/Small Documents/", pattern = "*.txt")
# taggedDF = data_frame()
# for (i in 1:length(files)) {
#  print("Now tagging:")
#  print(files[i])
#  path <-paste("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/Small Documents/", files[i], sep = "")
#  tagged <- treetag(path, treetagger = "manual", lang = "en", TT.options = list(path = "~/Code/tree-tagger-MacOSX-3.2", preset = "en"))
#  taggedDF <- rbind(taggedDF, taggedText(tagged))
#}

# Use phrasemachine to tag the documents
#files <- list.files("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/Small Documents/")
#documents <- c(1:length(files))
#for (i in 1:length(files)) {
#  path <- paste("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/Small Documents/", files[i], sep = "")
#  con <- file(path)
#  documents[i] = readLines(con)
#  close(con)
#}
#tagged <- POS_tag_documents(documents)

#Write the messy list into one nice csv for safekeeping

#lapply(tagged, function(x) write.table(data.frame(x), "labeled.csv", append = T, sep = ","))

# Get it back as a nice data frame
#taggedData <- read.csv("~/Code/BasicTextualAnalysis/labeled.csv")

# count the conjunctions
#conjunctions = 0
#for (i in 1:nrow(taggedData)) {
#  if (taggedData[i, 3] == "CC") {
#    conjunctions <- conjunctions + 1
#  }
#}