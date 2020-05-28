#### NOTE - code is prepared for using different function for evaluate inputs in server.R
#### NOTE - just change the last section of code find_word <- "choosen function" 

### CONFIGURATION

treshold_4 <- -16
treshold_3 <- -16 
treshold_2 <- -16
no_words <- 5


### FUNCTIONS


cap_val_input <- function(word){ ### need to be updated
  
  if (!word %in% cap_gram_1_search$word1) {
    
    temp <- as.character( cap_gram_1_search %>% 
                            filter( str_detect( word1, paste0("^",  word))) %>%
                            arrange(desc(log_freq)) %>% 
                            slice(1) %>% 
                            select(word1))
    
    if (temp == "character(0)") word <- "<unk>"
    else word <- temp
    }
  
  return(word)  
}


cap_prop_floor <- function(prop, floor_prop) {
  
  if (is.na(prop)) prop <- floor_prop
  
  return(prop)
}



cap_find_word_treshold <- function(x4, x3, x2, x1, tres_4= treshold_4, tres_3=treshold_3, tres_2=treshold_2, multyplicator = 0.4, no_word= no_words) {
  
  x4 <- cap_val_input(x4)
  x3 <- cap_val_input(x3)
  x2 <- cap_val_input(x2)
  x1 <- cap_val_input(x1) 
  
  multi <- log(multyplicator)
  
  temp_quatro <- cap_prop_floor(as.numeric(cap_gram_4_search[.(x4, x3, x2, x1), .(log_freq)]), tres_4-1)
  
  if (temp_quatro > tres_4){ ## prob of fourgram is enough high
    
    temp_cinquo_prop <- cap_gram_5_search[.(x4, x3, x2, x1), .(word1,log_freq)] %>%
      arrange(desc(log_freq)) %>%
      slice(1:no_word)
    temp_cinquo_prop$log_freq <- temp_cinquo_prop$log_freq - temp_quatro
    
    temp <- temp_cinquo_prop
    
  } else {
    
    
    temp_trig<- cap_prop_floor( as.numeric(cap_gram_3_search[.(x3, x2, x1), .(log_freq)]), tres_3-1)
    
    if (temp_trig > tres_3){ ## prob of trigram is enough high
      
      temp_quatro_prop <- cap_gram_4_search[.(x3, x2, x1), .(word1, log_freq)] %>% 
        arrange(desc(log_freq)) %>% 
        slice(1:no_word)
      
      temp_quatro_prop$log_freq<-temp_quatro_prop$log_freq - temp_trig
      
      temp<- temp_quatro_prop
      
    } else { ### prob of bigram is enough high
      
      temp_big <- cap_prop_floor( as.numeric(cap_gram_2_search[.(x2, x1), .(log_freq)]) , tres_2-1)
      
      if (temp_big > tres_2) {
        
        temp_tri_prop <- cap_gram_3_search[.(x2, x1), .(word1, log_freq)] %>% 
          arrange(desc(log_freq)) %>% 
          slice(1:no_word)
        
        temp_tri_prop$log_freq<-temp_tri_prop$log_freq - temp_big + multi
        
        temp<-temp_tri_prop
        
      }  else {
        
        temp_unig <- as.numeric(cap_gram_1_search[ .(x1), .(log_freq)])
        
        
        temp_bi_prop <- cap_gram_2_search[ .(x1), .(word1, log_freq)] %>% 
          arrange(desc(log_freq)) %>% 
          slice(1:no_word)
        
        temp_bi_prop$log_freq<-temp_bi_prop$log_freq - temp_unig + 2*multi
        
        temp<-temp_bi_prop
        
      }
    }
  }
  
  temp <- temp %>% mutate(prop=exp(log_freq)) %>% 
    select(c("word1", "prop")) %>%
    arrange(desc(prop)) %>%
    slice(1:no_word)
  
  return(temp)
  
}


cap_find_word <- function(x4,x3,x2,x1, no_word=5){
  
  x4 <- cap_val_input(x4)
  x3 <- cap_val_input(x3)
  x2 <- cap_val_input(x2)
  x1 <- cap_val_input(x1) 
  
  temp_quat<-as.numeric(cap_gram_4_search[.(x4, x3, x2, x1), .(log_freq)])
  
  temp_cinqo_prop <- cap_gram_5_search[.(x4,x3,x2,x1), .(word1, log_freq)] %>%
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_cinqo_prop$log_freq <- temp_cinqo_prop$log_freq - temp_quat
  
  ##
  temp_trig<- as.numeric(cap_gram_3_search[.(x3, x2, x1), .(log_freq)])
  
  temp_quatro_prop <- cap_gram_4_search[.(x3, x2, x1), .(word1, log_freq)] %>% 
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_quatro_prop$log_freq<-temp_quatro_prop$log_freq - temp_trig
  ##
  temp_big <- as.numeric(cap_gram_2_search[.(x2, x1), .(log_freq)])
  
  temp_tri_prop <- cap_gram_3_search[.(x2, x1), .(word1, log_freq)] %>% 
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_tri_prop$log_freq<-temp_tri_prop$log_freq - temp_big
  ##
  temp_unig <- as.numeric(cap_gram_1_search[.(x1), .(log_freq)])
  
  temp_bi_prop <- cap_gram_2_search[.(x2, x1), .(word1, log_freq)] %>% 
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_bi_prop$log_freq<-temp_bi_prop$log_freq - temp_unig
  
  temp <- as.data.frame(rbind(temp_cinqo_prop ,temp_quatro_prop, temp_tri_prop,temp_bi_prop) %>% 
                          group_by (word1) %>% 
                          summarise( prop=max(exp(log_freq))) %>%
                          arrange(desc(prop)) %>%
                          slice(1:no_word) )
  
  return(temp)
  
}



cap_find_word_lambda <- function(x4,x3,x2,x1, no_word=5, lambda=0.4){
  
  x4 <- cap_val_input(x4)
  x3 <- cap_val_input(x3)
  x2 <- cap_val_input(x2)
  x1 <- cap_val_input(x1) 
  lambda <- log(lambda)
  
  temp_quat<-as.numeric(cap_gram_4_search[.(x4, x3, x2, x1), .(log_freq)])
  
  temp_cinqo_prop <- cap_gram_5_search[.(x4,x3,x2,x1), .(word1, log_freq)] %>%
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_cinqo_prop$log_freq <- temp_cinqo_prop$log_freq - temp_quat
  
  ##
  temp_trig<- as.numeric(cap_gram_3_search[.(x3, x2, x1), .(log_freq)])
  
  temp_quatro_prop <- cap_gram_4_search[.(x3, x2, x1), .(word1, log_freq)] %>% 
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_quatro_prop$log_freq<-temp_quatro_prop$log_freq - temp_trig + lambda
  ##
  temp_big <- as.numeric(cap_gram_2_search[.(x2, x1), .(log_freq)])
  
  temp_tri_prop <- cap_gram_3_search[.(x2, x1), .(word1, log_freq)] %>% 
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_tri_prop$log_freq<-temp_tri_prop$log_freq - temp_big + 2*lambda
  ##
  temp_unig <- as.numeric(cap_gram_1_search[.(x1), .(log_freq)])
  
  temp_bi_prop <- cap_gram_2_search[.(x2, x1), .(word1, log_freq)] %>% 
    arrange(desc(log_freq)) %>% 
    slice(1:no_word)
  temp_bi_prop$log_freq<-temp_bi_prop$log_freq - temp_unig + 3*lambda
  
  temp <- as.data.frame(rbind(temp_cinqo_prop ,temp_quatro_prop, temp_tri_prop,temp_bi_prop) %>% 
                          group_by (word1) %>% 
                          summarise( prop=max(exp(log_freq))) %>%
                          arrange(desc(prop)) %>%
                          slice(1:no_word) )
  
  return(temp)
  
}




cap_val_output <- function(temp_output, no_words=5, last_word = "the"){
  
  temp_output <- temp_output[!is.na(temp_output)] ## removes NA
  temp_output <- str_remove(temp_output, "<unk>") 
  
  last_word_valid <- cap_val_input(last_word)
  
  if (!last_word == last_word_valid & !last_word_valid=="<ukn>") temp_output<-c(last_word_valid, temp_output)
  
   temp_output <- c(temp_output, c("the", "to", "and", "a", "of"))[1:no_words]
 

  return(temp_output)
}



####                            ###
####   SEARCH WORD DEFINITION   ### 
####                            ###

find_word <- cap_find_word_lambda


