#########################################    Ciclos    #####################################################

for (x in 1:length(tags4)){
  lista2 <- as.list(strsplit(tags4[[x]]$content, " "))
  AddItemDoubling(lista2)
}

for (i in 1:1925){
  if (length(ResultList[[i]][[1]])>0){
    for (j in 1:length(ResultList[[i]][[1]])){
      if (ResultList[[i]][[1]][[j]]%in%colnames(novadtm)) {
        AddItemSemiFinal(ResultList[[i]][[1]][[j]])
      } else {
        #print(paste(c("Linha:",i,"Coluna:",j,"Termo:",ResultList[[i]][[1]][[j]]), collapse = ' '))
      }
    }
  }
  AddItemFinal(as.String(gsub(" {2,}", " ", gsub("NULL", "", paste(c(ResultList2), collapse=' ' )))))
  ResultList2 <- list(NULL)
}


ResultDF <- as.data.frame(do.call(cbind, ResultList3))

write.csv(ResultDF, "TN_freq2_med24.csv")

############################################################################################################
####################################### Append to List  ####################################################


Counter <- 0
ResultList <- list(NULL)
Size <- 1

AddItemDoubling <- function(item)
{
  if( .GlobalEnv$Counter == .GlobalEnv$Size )
  {
    length(.GlobalEnv$ResultList) <- .GlobalEnv$Size <- .GlobalEnv$Size * 2
  }
  
  .GlobalEnv$Counter <- .GlobalEnv$Counter + 1
  
  .GlobalEnv$ResultList[[.GlobalEnv$Counter]] <- item
}


Counter2 <- 0
ResultList2 <- list(NULL)
Size2 <- 1

AddItemSemiFinal <- function(item)
{
  if( .GlobalEnv$Counter2 == .GlobalEnv$Size2 )
  {
    length(.GlobalEnv$ResultList2) <- .GlobalEnv$Size2 <- .GlobalEnv$Size2 * 2
  }
  
  .GlobalEnv$Counter2 <- .GlobalEnv$Counter2 + 1
  
  .GlobalEnv$ResultList2[[.GlobalEnv$Counter2]] <- item
}

Counter3 <- 0
ResultList3 <- list(NULL)
Size3 <- 1

AddItemFinal <- function(item)
{
  if( .GlobalEnv$Counter3 == .GlobalEnv$Size3 )
  {
    length(.GlobalEnv$ResultList3) <- .GlobalEnv$Size3 <- .GlobalEnv$Size3 * 2
  }
  
  .GlobalEnv$Counter3 <- .GlobalEnv$Counter3 + 1
  
  .GlobalEnv$ResultList3[[.GlobalEnv$Counter3]] <- item
}
