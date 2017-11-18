#Importing libraries
libs <- c("RSQLite","tm","wordcloud","dplyr")
sapply(libs, require,character.only = TRUE)

#Importing data
#1st way
tweets <- read.csv("E:/RStudio_Dataset/GOP debate twitter sentiment/output/Sentiment.csv")

#2nd way
#db <- dbConnect(dbDriver("SQLite"),"E:/RStudio_Dataset/GOP debate twitter sentiment/output/database.sqlite")
#tweets <- dbGetQuery(db,"select * from Tweets")

positive <- subset(tweets,sentiment == "Positive")
neutral <- subset(tweets,sentiment == "Neutral")
negative <- subset(tweets,sentiment == "Negative")

# Function to clean and convert text to document term matrix
wc <- function(documents){
  corpusnew <- Corpus(VectorSource(documents))
  corpusnew <- tm_map(corpusnew,content_transformer(tolower))
  corpusnew <- tm_map(corpusnew,removePunctuation)
  corpusnew <- tm_map(corpusnew,stripWhitespace)
  corpusnew <- tm_map(corpusnew,removeWords,stopwords("english"))
  corpusnew <- DocumentTermMatrix(corpusnew)
  corpusnew <- as.data.frame(as.matrix(corpusnew))
  return(corpusnew)
} 

opt <- wc(positive$text)
freq <- colSums(opt)
freq <- sort(freq,decreasing = T)
freq <- freq[c(-1,-2,-7)]

# Plot positive sentiment wordcloud
png("positive_tweets.png")
wordcloud(words = names(freq),freq,min.freq = sort(freq,decreasing = T)[[700]],random.order = FALSE,random.color = TRUE,
            colors = brewer.pal(8, "Dark2"))
dev.off()

opt <- wc(neutral$text)
freq <- colSums(opt)
freq <- sort(freq,decreasing = T)
freq <- freq[c(-1,-2)]

# Plot neutral sentiment wordcloud
png("neutral_tweets.png")
wordcloud(words = names(freq),freq,min.freq = sort(freq,decreasing = T)[[700]],random.order = FALSE,random.color = TRUE,
          colors = brewer.pal(8, "Dark2"))
dev.off()

opt <- wc(negative$text)
freq <- colSums(opt)
freq <- sort(freq,decreasing = T)
freq <- freq[c(-1,-2)]

# Plot negative sentiment wordcloud
png("negative_tweets.png")
wordcloud(words = names(freq),freq,min.freq = sort(freq,decreasing = T)[[700]],random.order = FALSE,random.color = TRUE,
          colors = brewer.pal(8, "Dark2"))
dev.off()