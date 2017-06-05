library(stringr)

# Read in the tweets
con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/general-tweets.txt")
tweets <- read.table(con, sep = "\t", quote = "")
# Get just the tweets (not all that other weird data)
tweets <- tweets[2]

# Clean the tweets into one nice string of ASCII characters
allTweets <- ""
for (i in 1:nrow(tweets)) {
  allTweets <- paste(allTweets, tweets[i, ], sep = " ")
}

allTweets <- iconv(allTweets, "latin1", "ASCII", sub="")
allTweets <- str_replace_all(allTweets, " ", " ")
allTweets <- str_replace_all(allTweets, "http://t.co/[a-z,A-Z,0-9]*", "")
allTweets <- str_replace_all(allTweets, "RT @[a-z,A-Z]*: ", "")
allTweets <- str_replace_all(allTweets, "#[a-z,A-Z]*","")
allTweets <- str_replace_all(allTweets, "@[a-z,A-Z]*", "")
allTweets <- gsub("&amp", "", allTweets)
allTweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", allTweets)
allTweets <- gsub("@\\w+", "", allTweets)
allTweets <- gsub("[[:punct:]]", "", allTweets)
allTweets <- gsub("[[:digit:]]", "", allTweets)
allTweets <- gsub("http\\w+", "", allTweets)
allTweets <- gsub("[ \t]{2,}", "", allTweets)
allTweets <- gsub("^\\s+|\\s*$", "", allTweets)

# Write just the tweets
con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/cleanTweets.txt", "w")
cat(allTweets, file = con)
close(con)

# Get all the file names in a list
files <- list.files("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/journal articles/", pattern = "*.txt")

# Make a vector so that you don't have to do too much processing
# When you import the data (it will crash if you try to paste 
# the documents together while opening them and cleaning them)

documents <- c(1:length(files) + 2)

# Put the tweets in it
documents[1] <- allTweets

# Put the other text file that's not in the folder with its
# friends in the vector
con <- file("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/synthetic.lethal.txt")
temp <- paste(readLines(con), collapse = "\n")
temp <- iconv(temp, "latin1", "ASCII", sub="")
documents[2] <- temp
close(con)

#Get the files from the folder

for (i in 1:length(files)) {
  path <- paste("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/journal articles/", files[i], sep = "")
  con <- file(path)
  temp <- paste(readLines(con), collapse = "\n")
  temp <- iconv(temp, "latin1", "ASCII", sub="")
  temp <- str_replace_all(temp, " ", " ")
  index <- i + 2
  documents[index] <- temp
  close(con)
}

# Put all the documents into one big document
masterDocument <- ""
for (i in 1:length(documents)) {
  masterDocument <- paste(masterDocument, documents[i])
}

# Save the document to a file
path <- ("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/masterDocument.txt")
con <- file(path, "w")
cat(masterDocument, file = con)
close(con)

# Make some small documents so the tagger won't crash when you try 
# to process them.
segments <- strsplit(masterDocument, "\n")
temp <- ""
for (i in 1:length(segments[[1]])) {
  temp <- paste(temp, segments[[1]][[i]])
  if((i %% 10 == 0) & (i != 0)) {
    fileName <- paste("file", i, sep = "_")
    fileName <- paste(fileName, ".txt", sep = "")
    path <- paste("~/Code/BasicTextualAnalysis/2017 Summer Fellowship data/Small Documents/", fileName, sep = "")
    file <- file(path, "w")
    cat(temp, file = file)
    close(file)
    temp <- ""
  }
}
