
setwd('D:\\Desktop\\TESE\\Pratica\\RStudio') ## SET workspace
x <- read.csv("csv_dataset_11112015.csv", header = TRUE)[,c('ID','Experiencia')] ## read dataset

options(java.parameters = "-Xmx6g")

library(tm)
library(NLP)
library(openNLP)

esse <- sapply(x[[2]], function(x) { x<-as.String(x) } )
str(esse)

esse[[58]][[1]]


#### Data preparation
# remove html links
esse <- gsub("http\\S+\\s*|http\\S+$"," ",esse)
esse <- gsub("www\\S+\\s*|www\\S+$"," ",esse)
# remove numbers
esse = gsub("[[:digit:]]", "", esse)
# remove punctuation repetida
esse = gsub("([.,!:;?])(\\1+)", "\\1 ", esse)
#to lower
#esse <- tolower(esse)
#remove whitespaces
esse <- gsub(" {2,}", " ", esse)
#remover entradas em branco
esse <- esse[esse != ""]
esse <- esse[esse != " "]
esse[[29]][[1]]

### Convert to String
esses <- lapply(esse, function(x) { x<-as.String(x) } )

### Part-of-Speech 
reviews.tagged <- lapply(esses, function(taggar){
						sent_token_annotator <- Maxent_Sent_Token_Annotator()
						word_token_annotator <- Maxent_Word_Token_Annotator()
						pos_tag_annotator <- Maxent_POS_Tag_Annotator()
						a2 <- annotate(taggar, list(sent_token_annotator, word_token_annotator))
						a3 <- annotate(taggar, pos_tag_annotator, a2)
						a3w <- subset(a3, type == "word")
						print("Passei por aqui... Bora sapply!")
						tags <- sapply(a3w$features, '[[', "POS")
						r1 = sprintf("%s/%s", taggar[a3w], tags)
						r2 = paste (r1, collapse= " ")
						return(r2)
					} )
			
tegues <- sapply(reviews.tagged, function(x) {
			x<-as.String(x) } )

write.csv(tegues,"tags14112015.csv")

tegues = read.csv("tags14112015.csv", header = TRUE)
tegues <- sapply(tegues$x, function(x) { x<-as.String(x) } )

#### Separar palavras
splitText <- function(text){
	unlist(strsplit(text, " "))
}


#### Select tags we want to extract
selectTaggedWords <- function(tagged.words, target.tag){
	tagged.words[grep(target.tag, tagged.words)]
}

#|/VB |/VBD|/VBG|/VBN|/VBP|/VBZ|/JJ|/JJR|/JJS|/RB|/RBR|/RBS|/NNP
tags <- "^.+(/NN|/NNS|/NNPS)$"


#### Clean words: Remove tags from words
removeTags <-  function(word.pos){
	sub("/[A-Z]{2,3}", "", word.pos)
}

#### Save to file the extracted words
wordes <- ""
for ( i in 1:length(tegues)){
	tagged.words <- splitText(tegues[i])
	tagged.words.keep <- c(selectTaggedWords(tagged.words, tags))
	words <- removeTags(tagged.words.keep)
	wordes[i] <- as.String(words)
}
wordes3[23]

wordes3 = gsub("(\n)", " ", wordes)

wordes2 <- data.frame(wordes)
write.csv(wordes3,"nomes_sempropriosplural_19112015.csv")

wordes2[1][1]
