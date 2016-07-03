######################################### Calculate number topics ########################################

library(topicmodels)
topics <- c(5,10,15,20,25)
#topics <- 10 * c(1:4,10,20)
set.seed(0908)
SEED <- 20080809
D <- nrow(novadtm)

folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
date()
for (k in topics) {
  for (chain in seq_len(10)) {
    FILE <- paste("VEM_", k, "_", chain, ".rda", sep = "")
    training <- LDA(novadtm[folding != chain,], k = k,
                    control = list(seed = SEED))
    testing <- LDA(novadtm[folding == chain,], model = training,
                   control = list(estimate.beta = FALSE, seed = SEED))
    save(training, testing, file = file.path("D:\\Desktop\\TESE\\Pratica\\topicos\\nt\\f2m13", FILE))
  }
}
date()

D <- nrow(novadtm)
folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
AP_test <- AP_alpha <- AP_Lik <- list()


for (method in c("VEM")) {
  AP_alpha[[method]] <- AP_test[[method]] <- AP_Lik[[method]] <- matrix(NA,
                                       nrow = length(topics), ncol = 10, 
                                       dimnames = list(topics, seq_len(10)))
  for (fold in seq_len(10)) {
    for (i in seq_along(topics)) {
      T <- topics[i]
      FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
      load(file.path("D:\\Desktop\\TESE\\Pratica\\topicos\\nt\\f2m13", FILE))
      AP_alpha[[method]][paste(T),fold] <-
        if (is(training, "Gibbs_list")) training@fitted[[1]]@alpha
      else training@alpha
      AP_test[[method]][paste(T),fold] <- perplexity(testing,
                                                     novadtm[folding == fold,], use_theta = FALSE)
      AP_Lik[[method]][paste(T),fold] <- logLik(training)
    }
  }
}

save(AP_alpha, AP_test, file = "D:\\Desktop\\TESE\\Pratica\\topicos\\nt\\f2m13\\qtd.rda")

load(file = "D:\\Desktop\\TESE\\Pratica\\topicos\\nt\\f2m13\\qtd.rda")
library(plyr)
library(ggplot2)

## LOG-LIKELIHOOD
a<- as.matrix(ldply(AP_alpha))
AP_alpha2 <- data.frame(folds=rep(1:10, each=5), ntopic=c(5,10,15,20,25),logLik=as.numeric(a[,2:11]))
ggplot(AP_alpha2, aes(x=ntopic,y=logLik, color=as.factor(folds))) + xlab("Number of Topics") + 
  ylab("Log-Likelihood") + geom_line(size=1) +theme_bw() + scale_x_continuous(breaks=seq(from=0, to=200, by= 2)) 


apa2 <- aggregate(.~ ntopic, data=AP_alpha2, mean)
library(Cairo)
CairoWin()
Cairo(file="D:\\Desktop\\TESE\\Pratica\\topicos\\nt\\f2m13\\log_qtd.png", 
      type="png",
      units="in", 
      width=7, 
      height=6, 
      pointsize=12, 
      dpi=96)
ggplot(apa2[1:12,], aes(x=ntopic,y=logLik, color=as.factor(folds))) + xlab("Number of Topics") + ylab("Log-Likelihood") + geom_line(size=1, colour="black") +theme_bw() + scale_x_continuous(breaks=seq(from=0, to=200, by= 5)) + geom_point( size=3, shape=21, fill="white", colour="black") + guides(colour=FALSE) + geom_point(data=subset(apa2[1:12,],ntopic=="10"), aes(x=ntopic,y=logLik, color=as.factor(folds)), colour="black", size=4)
dev.off()

#PERPLEXITY
b<- as.matrix(ldply(AP_test))
AP_test2 <- data.frame(folds=rep(1:10, each=5),ntopic=c(5,10,15,20,25),logLik=as.numeric(b[,2:11]))
ggplot(AP_test2, aes(x=ntopic,y=logLik, color=as.factor(folds))) + xlab("Number of Topics") + ylab("Perplexity") + geom_line(size=1) +theme_bw() + scale_x_continuous(breaks=seq(from=0, to=200, by= 5)) + geom_point( size=3, shape=21, fill="white")

prep <- aggregate(.~ ntopic, data=AP_test2, mean)
#library(Cairo)
CairoWin()
Cairo(file="D:\\Desktop\\TESE\\Pratica\\topicos\\nt\\f2m13\\per.png", 
      type="png",
      units="in", 
      width=7, 
      height=6, 
      pointsize=12, 
      dpi=96)
ggplot(prep[1:12,], aes(x=ntopic,y=logLik, color=as.factor(folds))) + xlab("Number of Topics") + ylab("Perplexity") + 
  geom_line(size=1, colour="black") +theme_bw() + scale_x_continuous(breaks=seq(from=0, to=200, by= 5)) + 
  geom_point( size=3, shape=21, fill="white", colour="black") + guides(colour=FALSE, aes(x=ntopic,y=logLik, color=as.factor(folds)), colour="black", size=4)
dev.off()

################# Calculate CTM topics ###############
library(topicmodels)
k = 10
SEED = 2010;
my_TMuk = list(
  #VEM = LDA(dtmuk, k = k, control = list(seed = SEED)),
  #VEM_fixed = LDA(dtmuk, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),
  #Gibbs = LDA(dtmuk, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(novadtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
);

Topic = topics(my_TMuk[["CTM"]],1);
table(Topic)[order(table(Topic), decreasing = TRUE)]

#top 5 terms for each topic in LDA
Terms = terms(my_TMuk[["CTM"]], 15);
Terms;

write.table(Terms, "2312015topicos.csv")
