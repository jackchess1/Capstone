
library(tidyverse)
library(stringr)
library(shiny)

bi_words <- readRDS("bi_words_top.rds")
tri_words  <- readRDS("tri_words_top.rds")
quad_words <- readRDS("quad_words_top.rds")
quint_words <- readRDS("quint_words_top.rds")
sext_words <- readRDS("sext_words_top.rds")


## Create functions using the corresponding data files


bigram <- function(input_words){
  num <- length(input_words)
  filter(bi_words,
         word1==input_words[num]) %>%
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  filter(tri_words,
         word1==input_words[num-1],
         word2==input_words[num])  %>%
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  filter(quad_words,
         word1==input_words[num-2],
         word2==input_words[num-1],
         word3==input_words[num])  %>%
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}

quintgram <- function(input_words){
  num <- length(input_words)
  filter(quint_words,
         word1==input_words[num-3],
         word2==input_words[num-2],
         word3==input_words[num-1],
         word4==input_words[num])  %>%
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 5)) %>%
    as.character() -> out
  ifelse(out=="character(0)", quadgram(input_words), return(out))
}

sextgram <- function(input_words){
  num <- length(input_words)
  filter(sext_words,
         word1==input_words[num-4],
         word2==input_words[num-3],
         word3==input_words[num-2],
         word4==input_words[num-1],
         word5==input_words[num])  %>%
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 6)) %>%
    as.character() -> out            
  ifelse(out=="character(0)", quintgram(input_words), return(out))
}



ngram <- function(input_text){
  input_text <- data_frame(text = input_text)
  replace_reg <- "[^[:alpha:][:space:]]*"
  input_text <- input_text %>%
    mutate(text = str_replace_all(text,replace_reg, ""))
  input_count <- str_count(input_text, boundary("word"))
  input_words <- unlist(str_split(input_text, boundary("word")))
  input_words <- tolower(input_words)
  
  nextWord <- ifelse(input_count==0, "Please enter a word or phrase",
                 ifelse(input_count==1, bigram(input_words),
                        ifelse(input_count==2, trigram(input_words),
                               ifelse(input_count==3,quadgram(input_words),
                                      ifelse(input_count==4,quintgram(input_words), sextgram(input_words))))))
  if(nextWord == "?"){
    nextWord = "Application does not recognize this word or phrase."
  }
  return(nextWord)
}

shinyServer(function(input,output){
  output$prediction <- renderPrint({
    next_word <- ngram(input$EnterText)
    next_word
  })
  
  output$text1 <- renderText({
    input$EnterText
  });
})
