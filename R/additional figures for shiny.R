# load libraries
library(tidyverse)
library(statnet) # network statistics
library(visNetwork) # network visualization
library(igraph) # visNetwork requires this package
library(tm) # text mining
library(wordcloud2) # word cloud maker
library(forcats) # categorical data

# load data
load("data/MSEreview.RData")

# Extract the individual authors from the column of authors
MSE_authors<-strsplit(study$AllAuthors,split="; ")

# count number publications by authors
MSE_authors_count<-MSE_authors%>% 
  unlist() %>%
  table() %>% 
  as.data.frame() %>% 
  rename("Authors"=".") %>% 
  arrange(-Freq)

#### Create an author network diagram ####
# Create matrix of authors (rows) in papers (columns)
bipartiteEdges <- lapply(MSE_authors, function(x) {MSE_authors_count$Authors %in% x})
bipartiteEdges <- do.call("cbind", bipartiteEdges) # dimension is number of authors x number of papers
rownames(bipartiteEdges) <- MSE_authors_count$Authors

# Create coauthor matrix of authors (rows) with coauthor (columns)
coauthMat <- bipartiteEdges %*% t(bipartiteEdges) #bipartite to unimode
coauthMat <- coauthMat[order(rownames(coauthMat)), order(rownames(coauthMat))]

# convert coauthor matrix to a network object
wosStatnet <- as.network(coauthMat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
# wosStatnet # view network summary

# I think this scales the size of the network?
wosStatnet%v%"size" = log(rowSums(coauthMat))

# Create static network plot
# plot.network(wosStatnet, edge.col = "gray", edge.lwd = wosStatnet%e%"edge.lwd",
#              label = "vertex.names", label.cex = .5, label.pad = 0, label.pos = 1, vertex.cex = "size")

# Get nodes and edges from the network object
nodes <- data.frame(id = 1:length(wosStatnet%v%"vertex.names"),
                    label = wosStatnet%v%"vertex.names",
                    title = wosStatnet%v%"vertex.names",
                    size = 5*(2+wosStatnet%v%"size"))

edges <- data.frame(from=data.frame(as.edgelist(wosStatnet))$X1, 
                    to=data.frame(as.edgelist(wosStatnet))$X2)

# Create interactive network visualization plot
interactive <- visNetwork(nodes, edges, main = "MSE Co-Author Network", width = 800, height = 800) %>% 
  visIgraphLayout(layout = "layout_nicely", type = "full") %>%
  visNodes(color = list(background = "white", highlight = "red", hover = list(border = "red"))) %>%
  visEdges(selectionWidth = 10, color = list(highlight = "#2B7CE9")) %>%  
  visOptions(nodesIdSelection = list(enabled  = TRUE, useLabels = TRUE, main = "Select by Author"))

# "print" interactive network plot
interactive

#### Author visualization ####
# Report the top 20 MSE authors
MSE_authors_count %>% 
  top_n(20,Freq)

#### Journal visualization ####
journal.plot<-study %>%
  select(Journal) %>% 
  drop_na(Journal) %>% 
  ggplot(aes(x=fct_rev(fct_infreq(Journal)))) + geom_bar() + coord_flip() +
  xlab("Journal") + ylab("Number of MSEs Published")

#plot number of pubs with climate change based on journal 
journals_cc <- ggplot(clim, aes(x=Journal)) + geom_bar() #Can't read labels
journals_cc       

#### Wordclouds ####
wordplot.data<-function(data,remove.words=NULL){
  docs<-Corpus(VectorSource(data)) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, remove.words) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  
  return(df)
}

wordplot.data(study$System,remove.words = "fishery") %>% 
  wordcloud2()

wordplot.data(study$Location) %>% 
  wordcloud2()

wordplot.data(study$Species,remove.words = "pacific") %>% 
  wordcloud2()

wordplot.data(study$Title) %>% 
  wordcloud2()

wordplot.data(study$ProblemDefinition) %>% 
  wordcloud2()

wordplot.data(study$Comments) %>% 
  wordcloud2()




