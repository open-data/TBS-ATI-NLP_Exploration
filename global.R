## Required packages
library(tidyverse)
library(magrittr)
library(rlang) # For sym() parsing text programmatically
library(flexdashboard) # For gauges on shiny dashboard
library(DT) # For more attractive data tables in UI
library(shiny)
library(shinydashboard)

## NLP packages (could remove some, with loss of associated features)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)
library(tm)
library(wordcloud)
library(textmineR)
library(ggwordcloud)

source("GGgraphs.R")

### STUFF UNIQUE TO THIS DATASET ###
options(dplyr.summarise.inform=F) 

topNowners = 20

## Functions
clean_ati <- function(dfr) {
  dfr %>%
    mutate(owner = sub("\\|.*", "", owner_org_title)) %>%
    mutate(disposition = sub("/.*", "", disposition) %>% trimws() %>% tolower()) %>%
    mutate(
      month = factor(month, levels = 1:12, labels = month.abb)
      ,year = factor(year)
      ,umd_number = factor(umd_number)
      ,owner = sub("Department of ", "", owner)
      ,owner = sub(" Canada", "", owner)
      ,owner = sub("(and)|(And)", "&", owner)
      ,owner = trimws(owner)
    ) %>%
    filter(year != "2020" | month != "Oct") %>%
    select(-summary_fr)
}

shorten_department_names <- function(depNames) {
  depNames[grepl("Immigration, Ref", depNames)] = "IRCC"
  depNames[grepl("Innovation, Sci", depNames)] = "ISED"
  depNames[grepl("Public Services", depNames)] = "PSPC"
  depNames[grepl("Employment & Social", depNames)] = "ESDC"
  depNames[grepl("Privy Council", depNames)] = "PCO"
  depNames[grepl("Border Services", depNames)] = "CBSA"
  depNames[grepl("Revenue Agency", depNames)] = "CRA"
  depNames[grepl("Mounted Police", depNames)] = "RCMP"
  depNames[grepl("Treasury Board", depNames)] = "TBS"
  depNames[grepl("Climate Change", depNames)] = "ECCC"
  depNames[grepl("Food Inspection", depNames)] = "CFIA"
  depNames[grepl("Nuclear Safety", depNames)] = "CNSC"
  depNames[grepl("Transport Canada", depNames)] = "TC"
  depNames[grepl("Fisheries", depNames)] = "DFO"
  depNames[grepl("Library and Archives", depNames)] = "LAC"
  depNames[grepl("National Defence", depNames)] = "DND"
  depNames[grepl("Public Safety", depNames)] = "PS"
  depNames[grepl("Finance Canada", depNames)] = "FIN"
  depNames[grepl("Natural Resources", depNames)] = "NRCan"
  depNames[grepl("Justice", depNames)] = "JUS"
  depNames[grepl("Canadian Heritage", depNames)] = "PCH"
  depNames
}

count_unigrams <- function(dataset, stopWords) {
  dataset %>%
    mutate(ID = row_number()) %>% 
    select(ID, owner, summary_en) %>%
    unnest_tokens(word, summary_en) %>%
    anti_join(stopWords, by = "word")
}

count_bigrams <- function(dataset, stopWords) {
  dataset %>%
    mutate(ID = row_number()) %>% 
    select(ID, owner, summary_en) %>%
    unnest_tokens(bigram, summary_en, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stopWords$word,
           !word2 %in% stopWords$word) %>%
    group_by(owner) %>% 
    count(word1, word2, sort = TRUE)
}

count_trigrams <- function(dataset, stopWords) {
  dataset %>%
    mutate(ID = row_number()) %>% 
    select(ID, owner, summary_en) %>%
    unnest_tokens(trigram, summary_en, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stopWords$word,
           !word2 %in% stopWords$word,
           !word3 %in% stopWords$word) %>%
    group_by(owner) %>% 
    count(word1, word2, word3, sort = TRUE)
}

count_tetragrams <- function(dataset, stopWords) {
  dataset %>%
    mutate(ID = row_number()) %>% 
    select(ID, owner, summary_en) %>%
    unnest_tokens(tetragram, summary_en, token = "ngrams", n = 4) %>%
    separate(tetragram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    filter(!word1 %in% stopWords$word,
           !word2 %in% stopWords$word,
           !word3 %in% stopWords$word,
           !word4 %in% stopWords$word) %>%
    group_by(owner) %>% 
    count(word1, word2, word3, word4, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

visualize_tfidf <- function(tfidf, topN = 10) {
  names(tfidf)[2] = "word"
  
  tfidf %>%
    ungroup() %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(owner) %>%
    # top_n(topN, tf_idf) %>%
    slice_max(tf_idf, n = topN, with_ties = FALSE) %>% 
    ggplot(aes(x=word, y=tf_idf, fill = owner)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "TF-IDF") +
    facet_wrap(~owner, scales = "free") +
    coord_flip() +
    theme(axis.text = element_text(size = 12 - ceiling(2*(sqrt(topNowners) - 3)) )) # Adjustment for sizing
}


## Variables
ati = read_csv("ati.csv") %>% clean_ati() %>% mutate(owner = shorten_department_names(owner))
ownersTopN = ati %>% group_by(owner) %>% count() %>% ungroup() %>% top_n(topNowners, n) %>% pull(owner)
ati_topN = ati %>% filter(owner %in% ownersTopN)
custWords = read_csv("stop_words_custom.csv") %>% pull(word)

### GENERAL ###

### -------------- ###
### Data Wrangling ###
### -------------- ###

get_summary_stats <- function(dfr, roundDec = 2) {
  ### Returns numeric summary stats for any numeric columns in dfr including dates
  # TODO: dates
  
  dd = dfr[sapply(dfr, class) %in% c("numeric", "integer", "difftime")]
  if(ncol(dd) < 1) return(list(avg= "", sd= "")) # Get outta dodge if no numerics
  
  get_col_stats <- function(col) {
    col = as.numeric(col)
    
    avg  = round(mean(col, na.rm= T), roundDec)
    sd   = round(sd(col, na.rm= T), roundDec)
    
    list(avg= avg, sd= sd)
  }
  
  lapply(dd, get_col_stats) # Compute stats for every numeric column
}

### ----------- ###
### UI Elements ###
### ----------- ###

make_gauge <- function(number, yellowPoint = 0.75, redPoint = 0.5, invert = F) {
  ### Renders a percent on a gauge
  if(invert) number = 1.00 - number
  
  number %>% 
    round(3) %>%
    multiply_by(100) %>% 
    gauge(min = 0, max = 100, symbol = "%", 
          gaugeSectors(success = c(100*yellowPoint, 100), warning = c(100*redPoint, 100*yellowPoint), danger = c(0, 100*redPoint)))
  
}

render_gauge <- function(var, dfr) {
  # One-line wrapper function to renderGauge
  
  dfr %>% 
      select(all_of(var)) %>% 
      pull(var) %>% 
      is.na() %>% 
      mean() %>% 
      make_gauge(invert = T)
}

### ---------------- ###
### Plots and Tables ###
### ---------------- ###

make_multi_plot <- function(dfr, catType="Bar", maxLevels=8, maxGroups=4, maxLabelL=20, cutoff=Inf, iKeepFac=T, iLog=F, 
                            iRotateX=F, iFreqPoly=F, numBins=30, linearFit=F, xy=F, jitter=F) {
  ### 5 cases, depending on type of inputs
  # TODO: Improve doc'n of this function
  # TODO: error checking on inputs
  if(any(apply(is.na(dfr), 2, all))) return(NULL) # If any col has all missings
  
  if(ncol(dfr) < 2 | ncol(dfr) > 3 | nrow(dfr) < 2 | is.null(dfr)) return(NULL)
  
  xName = names(dfr)[1]
  yName = names(dfr)[2]
  gName = names(dfr)[3] # In case it's here
  
  xClass = lapply(dfr, class)[1]
  yClass = lapply(dfr, class)[2]
  
  catClass = c("character", "factor", "logical")
  numClass = c("numeric", "integer", "difftime")
  datClass = c("Date", "POSIXct")
  
  # Convert logicals to factor for now
  if(xClass == "logical") dfr[1] = factor(dfr[1])
  if(yClass == "logical") dfr[2] = factor(dfr[2])
  
  # Case 5: CAT x CAT x CAT (with "Jitter" request)
  if(ncol(dfr) == 3 & catType == "Jitter") {
    gClass = lapply(dfr, class)[3]
    
    # If they are not all categorical, return empty plot
    if(!(xClass[1] %in% catClass & yClass[1] %in% catClass & gClass[1] %in% catClass)) return(NULL)
    
    return(gg_jitter(dfr, maxLevels = maxLevels, maxGroups = maxGroups, iPareto = F, iRotateX = iRotateX, maxLabelL = maxLabelL, x.lab = xName, y.lab = yName, g.lab = gName))
  }
  
  # Case 1: CAT x CAT
  if(xClass[1] %in% catClass & yClass %in% catClass) return(gg_count(dfr[,1:2], type = catType, maxLevels = maxLevels, maxGroups = maxGroups, maxLabelL = maxLabelL, iLog = iLog, iPareto = F, iKeepFac = iKeepFac, iRotateX = iRotateX, x.lab = xName, y.lab = yName))
  
  # Case 2: CAT x NUM (x CAT)
  if(xClass[1] %in% catClass & yClass %in% numClass) return(gg_box(dfr, x.lab = xName, y.lab = yName, maxBoxes = maxLevels, maxGroups = maxGroups, maxLabelL = maxLabelL, cutoff = cutoff, iLog = iLog, iPareto = F, iRotateX = iRotateX))
  
  # Case 3: NUM x CAT 
  if(xClass[1] %in% numClass & yClass %in% catClass) return(gg_hist(dfr[,1:2], iFreqPoly = iFreqPoly, myBins = numBins, maxGroups = maxGroups, cutoff = cutoff, iLog = iLog, x.lab = xName, y.lab = yName))
  
  # Case 4: NUM x NUM (x CAT)
  if(xClass[1] %in% numClass & yClass %in% numClass) return(gg_scatter(dfr, x.lab = xName, y.lab = yName, cutoff = cutoff, maxGroups = maxGroups, maxPoints = Inf, iSmooth=linearFit, iXY=xy, iJitter=jitter, iLog = iLog))
  
  return(NULL)
}

make_summ_text <- function(summStats, statistic, var, tab) {
  if(is.null(var) | tab != "Bivariate") return(NULL)
  
  thisSummStat = extract(summStats, var)[[1]]
  
  as.numeric(extract(thisSummStat, statistic))
}
