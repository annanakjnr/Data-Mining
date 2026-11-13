# Pattern Discovery and Relationship Mining


# Part 1 

# Download the following packages if you haven't
install.packages("arules")
install.packages("arulesViz")
install.packages("tm")
install.packages("wordcloud")
install.packages("igraph")

# Loading the necessary libraries
library(arules)
library(arulesViz)

# Loading the dataset 
transactions_data <- read.csv("transactions.csv")

# Inspecting its structure 
head(transactions_data)
str(transactions_data)

# Converting dataset to a transactions object for arules
item_list  <- strsplit(transactions_data$Items, ",\\s*")
trans_data <- as(item_list, "transactions")
summary(trans_data)


# Applying the Apriori Algorithm 
# Set the minimum support to 0.20 and 
# the minimum confidence to 0.50 
rules <- apriori(trans_data, parameter = list(supp = 0.20, conf = 0.50))

# Viewing discovered rules
summary(rules)

# Re-running Apriori with relaxed thresholds 
# (minimum support to 0.05)
# (minimum confidence to 0.30 )
# so rules can be mined
rules2 <- apriori(trans_data, parameter = list(supp = 0.05, conf = 0.30))
summary(rules2)

# Sorting the rules by lift 
sorted_rules <- sort(rules2, by = "lift", decreasing = TRUE)

# Viewing top 5 rules 
inspect(head(sorted_rules, 5))

# Creating a graph visualization of re-run rules
plot(sorted_rules[1:15], method = "graph", control = list(type = "items"))


# Part 2 

# Loading the necessary libraries
library(tm)
library(wordcloud)

# Loading the dataset
reviews_data <- read.csv("customer_reviews.csv")

# Inspecting its structure 
head(reviews_data)
str(reviews_data)


# Create text corpus
# Note: Change 'review_text' to the actual name of the column containing the reviews
# corpus <- Corpus(VectorSource(reviews_data$review_text)) 
corpus <- Corpus(VectorSource(reviews_data$ReviewText))

# Text Preprocessing 
# Converting all text to lowercase 
corpus <- tm_map(corpus, content_transformer(tolower))

# Removing punctuation 
corpus <- tm_map(corpus, removePunctuation)

# Removing stop words 
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Removing extra whitespace 
corpus <- tm_map(corpus, stripWhitespace)

# Creating a word cloud displaying
# the most frequently occurring terms 
wordcloud(corpus, min.freq = 2, max.words = 100, random.order = FALSE)

# Creating a term-document matrix 
tdm <- TermDocumentMatrix(corpus)

# Identify associations between selected terms 
# Note: Change "quality" to a term you want to investigate from your word cloud 
# associations <- findAssocs(tdm, terms = "quality", corlimit = 0.3) 
associations <- findAssocs(tdm, terms = "quality", corlimit = 0.3) 
associations2 <- findAssocs(tdm, terms = "delivery", corlimit = 0.3) 
associations3 <- findAssocs(tdm, terms = "quick", corlimit = 0.3) 

# Viewing the relationships
print(associations)
print(associations2)
print(associations3)


# Part 3

# Loading necessary libraries
library(igraph)

# Loading the dataset 
connections_data <- read.csv("customer_connections.csv")

# Inspecting the dataset
head(connections_data)
str(connections_data)


# Create a network graph 
# showing customer connections 
network <- graph_from_data_frame(connections_data, directed = FALSE)
plot(network, 
     vertex.label = V(network)$name, 
     vertex.size = 12, 
     vertex.label.cex = 0.65, 
     vertex.label.color = "black",
     vertex.color = "lightblue",
     edge.color = "gray",
     layout = layout_with_fr(network),
     main = "Customer Connections Network")

# Calculating degree centrality
deg_centrality <- degree(network, mode = "all")

# Sorting nodes by highest degree centrality
sorted_nodes <- sort(deg_centrality, decreasing = TRUE)

# Viewing top 10 highly connected nodes
print(head(sorted_nodes, 10))
