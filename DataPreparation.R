
### import packages
library(tm)
library(NLP)
library(openNLP)
library(gsubfn)
library(qdap)

setwd('D:\\Desktop\\TESE\\Pratica\\RStudio') ## define workspace
x <- read.csv("nomes_sempropriosplural_19112015.csv", header = TRUE)

### import data content + id
myReader <- readTabular(mapping=list(id= "X",content="x"))
tags <- VCorpus(DataframeSource(x), readerControl=list(reader=myReader))

#convert special caracters to ASCII
idiomadecente <-function (x) iconv(x, to="ASCII//TRANSLIT")
tags <- tm_map(tags, content_transformer(idiomadecente))

#Remove htmls ans apostrofes
htmls1 <- function (x) gsub("http\\S+\\s*|http\\S+$"," ",x)
htmls2 <- function (x) gsub("www\\S+\\s*|www\\S+$"," ",x)
apostrofes <- function (x) gsub("_",".",x)

tags <- tm_map(tags, content_transformer(htmls1))
tags <- tm_map(tags, content_transformer(htmls2))
tags <- tm_map(tags, content_transformer(apostrofes))

#to lower
tags <- tm_map(tags, content_transformer(tolower))

#remove numbers
tags <- tm_map(tags, content_transformer(removeNumbers))

#remove punctuation except _
pontuacao <- function (x) gsub("[^[:alnum:][:space:]_]","", x)
tags<- tm_map(tags, content_transformer(pontuacao))

#Correct some spelling errors
accommod1 <- function(x) gsub("accomodation", "accommodation", x)
accommod2 <- function(x) gsub("accomodations", "accommodations", x)
fam <- function(x) gsub("familly", "family", x)
tags3 <- tm_map(tags, content_transformer(accommod1))
tags3 <- tm_map(tags3, content_transformer(accommod2))
tags3 <- tm_map(tags3, content_transformer(fam))

#Handling negation
str_negate <- function(x) {
  x1 <- gsub("(not|n't|without|unlikely to|never|no|nothing|nowhere|noone|none|havent|hasnt|hadnt|cant|couldnt|shouldnt|wont|wouldnt|dont|doesnt|didnt|isnt|arent|aint) \\K", 'NOT_', x, perl=T, ignore.case = TRUE)
  x2 <- gsubfn('NOT_([a-zA-Z_ ]+)', ~ gsub("\\b(?!(?i:not|n't|never|without|unlikely to))(?=\\w+)", 'NOT_', x, perl=TRUE, ignore.case = TRUE), x1)
  x2
}
tags3 <- tm_map(tags3, content_transformer(str_negate))


#Remover stopwords through a personalized list
padrao1 <- "\\b(experience| year| month| day|lots| bit|lot|things| thing|problems| problem| a|as|about|above|according|accordingly|across|actually|after|afterwards|again|against|aint|all|allow|allows|almost|alone|along|already|also|although|always|am|among|amongst|an|and|another|any|anybody|anyhow|anyone|anything|anyway|anyways|anywhere|apart|appear|are|arent|around|aside|ask|asking|at|available|away|b|be|became|because|become|becomes|becoming|been|before|beforehand|behind|being|believe|below|beside|besides|between|beyond|both|brief|but|by|c|cmon|cs|came|can|cant|cannot|cause|causes|co|com|come|comes|consequently|consider|considering|contain|containing|contains|corresponding|could|couldnt|d|despite|did|didnt|do|does|doesnt|doing|dont|done|down|downwards|during|e|each|edu|eg|eight|either|else|elsewhere|enough|et|etc|even|ever|every|everybody|everyone|everything|everywhere|ex|exactly|example|except|f|fifth|first|five|following|follows|for|former|formerly|forth|four|from|further|furthermore|g|get|gets|getting|given|gives|go|goes|going|gone|got|gotten|h|had|hadnt|happens|hardly|has|hasnt|have|havent|having|he|hes|hello|hence|her|here|heres|hereafter|hereby|herein|hereupon|hers|herself|hi|him|himself|his|hither|how|howbeit|however|i|id|ill|im|ive|ie|if|immediate|in|inasmuch|inc|indeed|indicate|indicated|indicates|inner|inso|instead|into|inward|is|isnt|it|itd|itll|its|itself|j|just|k|keep|keeps|kept|know|knows|known|l|last|least|less|lest|let|lets|ltd|m|mainly|many|may|maybe|me|mean|meanwhile|might|more|moreover|most|mostly|much|must|my|myself|n|name|namely|nd|neither|never|nevertheless|new|next|nine|no|nobody|non|none|noone|nor|normally|not|nothing|novel|now|nowhere|o|obviously|of|off|often|oh|ok|okay|on|once|one|ones|only|onto|or|other|others|otherwise|ought|our|ours|ourselves|out|outside|over|own|p|particular|particularly|per|perhaps|plus|possible|presumably|probably|q|que|quite|qv|r|rather|rd|re|regards|relatively|respectively|right|s|said|same|saw|say|saying|says|second|secondly|see|seeing|seem|seemed|seeming|seems|seen|self|selves|sensible|sent|serious|seriously|seven|several|shall|she|should|shouldnt|since|six|so|some|somebody|somehow|someone|something|sometime|sometimes|somewhat|somewhere|soon|specified|specify|specifying|still|sub|such|sup|sure|t|ts|take|taken|tell|tends|th|than|s|thanx|that|thats|the|their|theirs|them|themselves|then|thence|there|theres|thereafter|thereby|therefore|therein|thereupon|these|they|theyd|theyll|theyre|theyve|think|third|this|thorough|thoroughly|those|though|three|through|throughout|thru|thus|to|together|too|took|toward|towards|tried|tries|truly|try|trying|twice|two|u|un|under|unfortunately|unless|un|until|unto|up|upon|us|use|used|uses|using|usually|uucp|v|value|various|very|via|viz|vs|w|want|wants|was|wasnt|way|we|wed|well|were|weve|welcome|went|werent|what|whats|whatever|when|whence|whenever|where|wheres|whereafter|whereas|whereby|wherein|whereupon|wherever|whether|which|while|whither|who|whos|whoever|whole|whom|whose|why|will|with|within|without|wont|wonder|would|wouldnt|x|y|yes|yet|you|youd|youll|youre|youve|your|yours|yourself|yourselves|z|zero)\\b"
padrao2 <- "\\b(a's|ain't|aren't|c'mon|c's|can't|couldn't|didn't|doesn't|don't|hadn't|hasn't|haven't|he's|here's|i'd|i'll|i'm|i've|isn't|it'd|it'll|it's|let's|shouldn't|t's|that's|there's|they'd|they'll|they're|they've|wasn't|we'd|we'll|we're|we've|weren't|what's|where's|who's|won't|wouldn't|you'd|you'll|you're|you've|NOT_a|NOT_a's|NOT_about|NOT_above|NOT_according|NOT_accordingly|NOT_across|NOT_actually|NOT_after|NOT_afterwards|NOT_again|NOT_against|NOT_ain't|NOT_all|NOT_allow|NOT_allows|NOT_almost|NOT_alone|NOT_along|NOT_already|NOT_also|NOT_although|NOT_always|NOT_am|NOT_among|NOT_amongst|NOT_an|NOT_and|NOT_another|NOT_any|NOT_anybody|NOT_anyhow|NOT_anyone|NOT_anything|NOT_anyway|NOT_anyways|NOT_anywhere|NOT_apart|NOT_appear|NOT_are|NOT_aren't|NOT_around|NOT_as|NOT_aside|NOT_ask|NOT_asking|NOT_at|NOT_available|NOT_away||NOT_b|NOT_be|NOT_became|NOT_because|NOT_become|NOT_becomes|NOT_becoming|NOT_been|NOT_before|NOT_beforehand|NOT_behind|NOT_being|NOT_believe|NOT_below|NOT_beside|NOT_besides|NOT_between|NOT_beyond|NOT_both|NOT_brief|NOT_but|NOT_by|NOT_c|NOT_c'mon|NOT_c's|NOT_came|NOT_can|NOT_can't|NOT_cannot|NOT_cant|NOT_cause|NOT_causes|NOT_co|NOT_com|NOT_come|NOT_comes|NOT_consequently|NOT_consider|NOT_considering|NOT_contain|NOT_containing|NOT_contains|NOT_corresponding|NOT_could|NOT_couldn't|NOT_d|NOT_despite|NOT_did|NOT_didn't|NOT_do|NOT_does|NOT_doesn't|NOT_doing|NOT_don't|NOT_done|NOT_down|NOT_downwards|NOT_during|NOT_e|NOT_each|NOT_edu|NOT_eg|NOT_eight|NOT_either|NOT_else|NOT_elsewhere|NOT_enough|NOT_et|NOT_etc|NOT_even|NOT_ever|NOT_every|NOT_everybody|NOT_everyone|NOT_everything|NOT_everywhere|NOT_ex|NOT_exactly|NOT_example|NOT_except|NOT_f|NOT_fifth|NOT_first|NOT_five|NOT_following|NOT_follows|NOT_for|NOT_former|NOT_formerly|NOT_forth|NOT_four|NOT_from|NOT_further|NOT_furthermore|NOT_g|NOT_get|NOT_gets|NOT_getting|NOT_given|NOT_gives|NOT_go|NOT_goes|NOT_going|NOT_gone|NOT_got|NOT_gotten|NOT_h|NOT_had|NOT_hadn't|NOT_happens|NOT_hardly|NOT_has|NOT_hasn't|NOT_have|NOT_haven't|NOT_having|NOT_he|NOT_he's|NOT_hello|NOT_hence|NOT_her|NOT_here|NOT_here's|NOT_hereafter|NOT_hereby|NOT_herein|NOT_hereupon|NOT_hers|NOT_herself|NOT_hi|NOT_him|NOT_himself|NOT_his|NOT_hither|NOT_how|NOT_howbeit|NOT_however|NOT_i|NOT_i'd|NOT_i'll|NOT_i'm|NOT_i've|NOT_ie|NOT_if||NOT_immediate|NOT_in|NOT_inasmuch|NOT_inc|NOT_indeed|NOT_indicate|NOT_indicated|NOT_indicates|NOT_inner|NOT_inso|NOT_instead|NOT_into|NOT_inward|NOT_is|NOT_isn't|NOT_it|NOT_it'd|NOT_it'll|NOT_it's|NOT_its|NOT_itself|NOT_j|NOT_just|NOT_k|NOT_keep|NOT_keeps|NOT_kept|NOT_know|NOT_knows|NOT_known|NOT_l|NOT_last|NOT_ly|NOT_least|NOT_less|NOT_lest|NOT_let|NOT_let's|NOT_like|NOT_ltd|NOT_m|NOT_mainly|NOT_many|NOT_may|NOT_maybe|NOT_me|NOT_mean|NOT_meanwhile|NOT_might|NOT_more|NOT_moreover|NOT_most|NOT_mostly|NOT_much|NOT_must|NOT_my|NOT_myself|NOT_n|NOT_name|NOT_namely|NOT_nd|NOT_neither|NOT_never|NOT_nevertheless|NOT_new|NOT_next|NOT_nine|NOT_no|NOT_nobody|NOT_non|NOT_none|NOT_noone|NOT_nor|NOT_normally|NOT_not|NOT_nothing|NOT_novel)\\b"
padrao3 <- "\\b(NOT_now|NOT_nowhere|NOT_o|NOT_obviously|NOT_of|NOT_off|NOT_often|NOT_oh|NOT_ok|NOT_okay|NOT_on|NOT_once|NOT_one|NOT_ones|NOT_only|NOT_onto|NOT_or|NOT_other|NOT_others|NOT_otherwise|NOT_ought|NOT_our|NOT_ours|NOT_ourselves|NOT_out|NOT_outside|NOT_over|NOT_own|NOT_p|NOT_particular|NOT_particularly|NOT_per|NOT_perhaps|NOT_plus|NOT_possible|NOT_presumably|NOT_probably|NOT_q|NOT_que|NOT_quite|NOT_qv|NOT_r|NOT_rather|NOT_rd|NOT_re|NOT_regards|NOT_relatively|NOT_respectively|NOT_right|NOT_s|NOT_said|NOT_same|NOT_saw|NOT_say|NOT_saying|NOT_says|NOT_second|NOT_secondly|NOT_see|NOT_seeing|NOT_seem|NOT_seemed|NOT_seeming|NOT_seems|NOT_seen|NOT_self|NOT_selves|NOT_sensible|NOT_sent|NOT_serious|NOT_seriously|NOT_seven|NOT_several|NOT_shall|NOT_she|NOT_should|NOT_shouldn't|NOT_since|NOT_six|NOT_so|NOT_some|NOT_somebody|NOT_somehow|NOT_someone|NOT_something|NOT_sometime|NOT_sometimes|NOT_somewhat|NOT_somewhere|NOT_soon|NOT_specified|NOT_specify|NOT_specifying|NOT_still|NOT_sub|NOT_such|NOT_sup|NOT_sure|NOT_t|NOT_t's|NOT_take|NOT_taken|NOT_tell|NOT_tends|NOT_th|NOT_than|NOT_s|NOT_thanx|NOT_that|NOT_that's|NOT_thats|NOT_the|NOT_their|NOT_theirs|NOT_them|NOT_themselves|NOT_then|NOT_thence|NOT_there|NOT_there's|NOT_thereafter|NOT_thereby|NOT_therefore|NOT_therein|NOT_theres|NOT_thereupon|NOT_these|NOT_they|NOT_they'd|NOT_they'll|NOT_they're|NOT_they've|NOT_think|NOT_third|NOT_this|NOT_thorough|NOT_thoroughly|NOT_those|NOT_though|NOT_three|NOT_through|NOT_throughout|NOT_thru|NOT_thus|NOT_to|NOT_together|NOT_too|NOT_took|NOT_toward|NOT_towards|NOT_tried|NOT_tries|NOT_truly|NOT_try|NOT_trying|NOT_twice|NOT_two|NOT_u|NOT_un|NOT_under|NOT_unfortunately|NOT_unless|NOT_un|NOT_until|NOT_unto|NOT_up|NOT_upon|NOT_us|NOT_use|NOT_used|NOT_uses|NOT_using|NOT_usually|NOT_uucp|NOT_v|NOT_value|NOT_various|NOT_very|NOT_via|NOT_viz|NOT_vs|NOT_w|NOT_want|NOT_wants|NOT_was|NOT_wasn't|NOT_way|NOT_we|NOT_we'd|NOT_we'll|NOT_we're|NOT_we've|NOT_welcome|NOT_well|NOT_went|NOT_were|NOT_weren't|NOT_what|NOT_what's|NOT_whatever|NOT_when|NOT_whence|NOT_whenever|NOT_where|NOT_where's|NOT_whereafter|NOT_whereas|NOT_whereby|NOT_wherein|NOT_whereupon|NOT_wherever|NOT_whether|NOT_which|NOT_while|NOT_whither|NOT_who|NOT_who's|NOT_whoever|NOT_whole|NOT_whom|NOT_whose|NOT_why|NOT_will|NOT_with|NOT_within|NOT_without|NOT_won't|NOT_wonder|NOT_would|NOT_wouldn't|NOT_x|NOT_y|NOT_yes|NOT_yet|NOT_you|NOT_you'd|NOT_you'll|NOT_you're|NOT_you've|NOT_your|NOT_yours|NOT_yourself|NOT_yourselves|NOT_z|NOT_zero|NOT_aint|NOT_arent|NOT_cmon|NOT_cs|NOT_couldnt|NOT_didnt|NOT_doesnt|NOT_dont|NOT_hadnt|NOT_hasnt|NOT_havent|NOT_hes|NOT_heres|NOT_id|NOT_ill|NOT_im|NOT_ive|NOT_isnt|NOT_itd|NOT_itll|NOT_lets|NOT_shouldnt|NOT_ts|NOT_theyd|NOT_theyll|NOT_theyre|NOT_theyve|NOT_wasnt|NOT_wed|NOT_weve|NOT_werent|NOT_whats|NOT_wheres|NOT_whos|NOT_wont|NOT_wouldnt|NOT_youd|NOT_youll|NOT_youre|NOT_youve)\\b"

remover1 <- function (x) {  gsub(padrao1, "", x, ignore.case = TRUE, perl= FALSE) }
remover2 <- function (x) {  gsub(padrao2, "", x, ignore.case = TRUE, perl= FALSE) }
remover3 <- function (x) {  gsub(padrao3, "", x, ignore.case = TRUE, perl= FALSE) }

tags4 <- tm_map(tags3, content_transformer(remover1))
tags4 <- tm_map(tags4, content_transformer(remover2))
tags4 <- tm_map(tags4, content_transformer(remover3))

#remove whitespaces
tags4 <- tm_map(tags4, content_transformer(stripWhitespace))

#stemming
tags4 <- tm_map(tags4, stemDocument)

tags4[[41]][[1]]
tags4[[1]][[1]]

##################################################### DTM MATRIX ##################################################
library(slam)
############## unigramas
novadtm <- DocumentTermMatrix(tags4, control = list( wordLengths=c(3,Inf), bounds = list(global = c(2,Inf))))
#novadtm <- DocumentTermMatrix(tags4, control = list( wordLengths=c(3,Inf), stemming = TRUE))
dim(novadtm)

# Calculate tf-idf
term_tfidf <- tapply(novadtm$v/row_sums(novadtm)[novadtm$i], novadtm$j, mean) * log2(nDocs(novadtm)/col_sums(novadtm > 0))
summary(term_tfidf)

novadtm <- novadtm[, term_tfidf >= 0.13]
novadtm <- novadtm[row_sums(novadtm) > 0,]

freq <- colSums(as.matrix(novadtm))   
freq[tail(order(freq))]


##################################################### Bigram DTM ###########################
library(slam)
### bigrams
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
dtmbitfidf <- DocumentTermMatrix(tags4, control = list( tokenize = BigramTokenizer,
                                                        weighting = function(x)  weightTfIdf(x, normalize = FALSE), 
                                                        wordLengths=c(3,Inf), 
                                                        bounds = list(global = c(5,Inf))))

dtmbitfidf <- dtmbitfidf[row_sums(dtmbitfidf) > 0,]

dim(dtmbitfidf)

###############################################################################################

##wordcloud unigrams
library(wordcloud)
m <- as.matrix(novadtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=150, colors=brewer.pal(8,"Dark2"))

write.csv(d, "freq.csv")

findAssocs(novadtm, "class", corlimit=0.1)




