#Importing libraries
libs <- c("RSQLite","dplyr","readr","ggplot2")
sapply(libs, require,character.only = TRUE)

#Importing data
#1st way
#tweets <- read.csv("E:/RStudio_Dataset/GOP debate twitter sentiment/output/Sentiment.csv")

#2nd way
db <- dbConnect(dbDriver("SQLite"),"E:/RStudio_Dataset/GOP debate twitter sentiment/output/database.sqlite")
tweets <- dbGetQuery(db,"select * from Sentiment")
#tweets <- tbl_df(tweets)


positive <- subset(tweets,sentiment == "Positive")
neutral <- subset(tweets,sentiment == "Neutral")
negative <- subset(tweets,sentiment == "Negative")

candidateList = c("Jeb Bush", "Marco Rubio", "Donald Trump", "Ben Carson", 
                  "John Kasich", "Ted Cruz", "Scott Walker", "Mike Huckabee", 
                  "Chris Christie", "Rand Paul")
for(i in 1:length(candidateList)){

candidate <- filter(tweets,candidate == candidateList[i])  
candidate <- select(candidate, sentiment, retweet_count, tweet_created)
candidate <- arrange(candidate,desc(retweet_count)) 
candidate <- na.omit(candidate)
candidate <- candidate[1:50,]

g <- ggplot(candidate,aes(tweet_created,retweet_count,fill=candidate$sentiment))+xlab("Sentiment")+ylab("Counts")+ggtitle("People's Sentiment")+geom_bar(stat = "identity",width = 0.3)
     g+theme(
       text = element_text(colour = "black",size = 16),
       axis.text = element_text(size = 16),
       axis.text.x = element_text(colour="white",size = 16),
       axis.text.y = element_text(size = 16),
       legend.key = element_rect(size = 12, fill = "white"),
       legend.position = "top",
       legend.background = element_rect(fill = "white"),
       legend.text = element_text(size = 12),
       panel.background = element_rect(fill = "white")
       
     )
     
}