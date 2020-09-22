# options(warn = -1) # For suppressing plotly warnings
library(shiny)
library(shinydashboard)

server <- function(input, output, session){
  
  
  ### ----------------- ###
  ### Reactive elements ###
  ### ----------------- ###
  rv = reactiveValues()
  rv$stops = data.frame(word = custWords, lexicon = "CUSTOM") %>% bind_rows(stop_words)
  
  # Figure out which variables are dates, factors, numeric, and binary
  varClasses = reactive({    sapply(sapply(ati_topN, class), "[[", 1)    })
  
  dateList   = reactive({    names(varClasses())[varClasses() %in% c("Date", "POSIXct")]    })
  factorList = reactive({    names(varClasses())[varClasses() %in% c("factor", "character")]    })
  numberList = reactive({    names(varClasses())[varClasses() %in% c("numeric", "integer", "difftime")]    })
  binaryList = reactive({    names(varClasses())[varClasses() %in% c("logical")]    })
  
  # Filtered data set
  filterData = reactive({
    req(input$filterDataSample)
    
    ati_topN %>%
      {if(input$filterDataSample != "All") filter(., owner == input$dept) else .}
  })
  
  
  # Summary descriptive stats
  summaryStats = reactive({
    req(input$tabs == "Bivariate", input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()))
    
    filterData() %>% select(input$multivarX, input$multivarY) %>% get_summary_stats()
  })
    
  nTotal = reactive(  nrow(filterData())   ) # Printed text
  
  
  ### Update custom stop word list
  ## With added words
  observeEvent(c(input$goStop, input$removeDeptName), {
    uiWords = strsplit(input$stopWordInput, ",") %>% 
      unlist() %>% 
      trimws()
    
    addWords = c(custWords, uiWords)
    
    # Remove department name as well, if selected
    if(input$removeDeptName){
      deptWords = strsplit(input$dept, " ") %>% unlist() %>% tolower()
      addWords = c(addWords, deptWords)
    } 
    
    rv$stops = data.frame(word = addWords, lexicon = "CUSTOM") %>%
      bind_rows(stop_words)
  })
  
  ## Removing all customs
  observeEvent(input$ignoreAllStops, {
    uiWords = strsplit(input$stopWordInput, ",") %>% 
      unlist() %>% 
      trimws()
    
    addWords = c(custWords, uiWords)
    
    if(input$removeDeptName){
      deptWords = strsplit(input$dept, " ") %>% unlist() %>% tolower()
      addWords = c(addWords, deptWords)
    }
    
    if(input$ignoreAllStops)  rv$stops = stop_words  
    if(!input$ignoreAllStops){
      rv$stops = data.frame(word = addWords, lexicon = "CUSTOM") %>%
        bind_rows(stop_words)
    } 
    
  })
  
  ## Make N-grams
  unigram = reactive({
    ati_topN %>%
      count_unigrams(rv$stops) %>%
      group_by(owner) %>% 
      count(word)
  })
  
  bigram  = reactive({    ati_topN %>% count_bigrams(rv$stops)  })
  trigram = reactive({    ati_topN %>% count_trigrams(rv$stops)  })
  tetragram = reactive({    ati_topN %>% count_tetragrams(rv$stops)  })
  
  word_cors = reactive({
    req(input$dept, input$bigramN)
    
    topWords = unigram() %>% 
      {if(input$filterDataSample != "All") filter(., owner == input$dept) else .} %>%
      filter(n >= input$bigramN) %>% 
      pull(word)
    
    ati_topN %>% 
      filter(owner == input$dept) %>%
      count_unigrams(rv$stops) %>%
      filter(word %in% topWords) %>%
      pairwise_cor(word, ID, sort = TRUE)
  })
  
  dtm = reactive({
    req(input$dept)
    
    ati_sub_topics = ati_topN %>% filter(owner == input$dept)
    
    # ati_sub_topics %>%
    #   count_unigrams(rv$stops) %>%
    #   group_by(ID, owner) %>%
    #   count(word) %>%
    #   cast_dtm(ID, word, n)
    
    CreateDtm(doc_vec = ati_sub_topics$summary_en,
              doc_names = ati_sub_topics$request_number,
              ngram_window = c(1, 2),
              stopword_vec = c(rv$stops, stopwords::stopwords(source = "smart")),
              lower = TRUE,
              remove_punctuation = TRUE,
              remove_numbers = TRUE
              ) 
  })
  
 
  
  ### -------------- ###
  ### Input elements ###
  ### -------------- ###
  
  ## Sidebar
  ## Uni tab
  output$selectFac <- renderUI(  selectInput("facVar", "Factor Variable:",   choices = intersect(factorList(), names(ati_topN)))  )
  output$selectNum <- renderUI(  selectInput("numVar", "Numeric Variable:",  choices = intersect(c(numberList(), dateList()), names(ati_topN)))  )
  output$selectDat <- renderUI(  selectInput("datVar", "DateTime Variable:", choices = intersect(dateList(), names(ati_topN)))  )
  output$selectBin <- renderUI(  selectInput("binVar", "Binary Variable:",   choices = intersect(binaryList(), names(ati_topN)))  )
  
  ## Bi tab
  output$selectMultivarX <- renderUI(    selectInput("multivarX", "X Variable:",  choices = names(ati_topN))   )
  output$selectMultivarY <- renderUI(    selectInput("multivarY", "Y Variable:",  choices = intersect(c(factorList(), numberList()), names(ati_topN)))  )
  output$selectMultivarG <- renderUI(    selectInput("multivarG", "Grouping Variable (Boxplot & Scatter):",   choices = c("NONE", intersect(c(factorList(), binaryList()), names(ati_topN))))  )
  
  observeEvent(input$switchVarsBi, {
    tempX = input$multivarX
    tempY = input$multivarY
    
    output$selectMultivarX <- renderUI(    selectInput("multivarX", "X Variable:",  choices = names(ati_topN), selected = tempY)   )
    output$selectMultivarY <- renderUI({
      selectInput("multivarY", "Y Variable:",  choices = intersect(c(factorList(), numberList()), names(ati_topN)), selected = tempX)
    })
  })
  
  
  ### --------------- ###
  ### Output elements ###
  ### --------------- ###
  
  ### Tables ###
  ### ------ ###
  
  output$tableBigramN = DT::renderDataTable({
    bigram() %>%
      {if(input$filterDataSample != "All") filter(., owner == input$dept) else .} %>%
      DT::datatable(options = list(pageLength = 20))
  })    
  
  output$tableBigramCorr = DT::renderDataTable({
    word_cors() %>%
      filter(correlation > input$bigramCorrMin) %>%
      DT::datatable(options = list(pageLength = 18))
  })
  
  
  ### Plots ###
  ### ----- ###
  
  ## TF-IDF tab
  output$plotTFIDF = renderPlot({
    req(input$nGramN, input$tfTopN)
    
    if(input$nGramN <= 1) dd = unigram() %>% bind_tf_idf(word, owner, n)
    
    if(input$nGramN == 2) {
      dd = bigram() %>% 
        unite(bigram, word1, word2, sep = " ") %>%
        count(owner, bigram, wt = n) %>%
        bind_tf_idf(bigram, owner, n)
    }
    
    if(input$nGramN == 3) {
      dd = trigram() %>% 
        unite(trigram, word1, word2, word3, sep = " ") %>%
        count(owner, trigram, wt = n) %>%
        bind_tf_idf(trigram, owner, n)
    }
    
    if(input$nGramN > 3) {
      dd = tetragram() %>% 
        unite(tetragram, word1, word2, word3, word4, sep = " ") %>%
        count(owner, tetragram, wt = n) %>%
        bind_tf_idf(tetragram, owner, n)
    }
    
    dd %>%
      arrange(desc(tf_idf)) %>%
      visualize_tfidf(input$tfTopN) +
      theme(text = element_text(size = 16))
    
  }, bg= "transparent")
  
  ## Topic Modeling tab
  output$plotTopics = renderPlot({
    req(dtm())
    
    dtm() %>%
      FitLdaModel(k = input$topicN, iterations = 20, burnin = 5) %>%
      SummarizeTopics() %>%
      mutate(word = gsub("_", ",", top_terms_phi)) %>%
      separate(word, into = letters, sep = ",", remove = T) %>%
      select(-c(topic, top_terms_gamma, top_terms_phi, prevalence, coherence)) %>%
      pivot_longer(cols = a:z, names_to = "junk", values_to = "word") %>%
      select(-junk) %>%
      filter(!is.na(word)) %>%
      group_by(label_1, word) %>%
      count() %>%
      ggplot(aes(
        label = word, size = n,
        color = factor(sample.int(10, nrow(.), replace = TRUE))
      )) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = 20) +
      theme(text = element_text(size = 28)) +
      # theme_minimal() +
      facet_wrap(~label_1)
    
    # ("topic", "label_1", "prevalence", "coherence", "top_terms_phi", "top_terms_gamma")
    
    # dtm() %>%
    #   LDA(k = input$topicN, control = list(seed = 1234)) %>%
    #   tidy(matrix = "beta") %>% # DF: topic, term, beta
    #   group_by(topic) %>%
    #   top_n(input$topicTopN, beta) %>%
    #   ungroup() %>%
    #   arrange(topic, -beta) %>%
    #   mutate(term = reorder_within(term, beta, topic)) %>%
    #   ggplot(aes(term, beta, fill = factor(topic))) +
    #   geom_col(show.legend = FALSE) +
    #   facet_wrap(~ topic, scales = "free") +
    #   coord_flip() +
    #   scale_x_reordered() +
    #   theme(axis.text = element_text(size = 12))
    
  }, bg= "transparent")
  
  ## Uni tab
  output$plotBar  = renderPlot({
    req(input$facVar %in% names(ati_topN))
    
    filterData() %>% select(input$facVar) %>%
      gg_count(type= "Bar", x.lab = input$facVar, maxLevels = input$maxBarsUni, maxLabelL = input$maxLabelUni, 
               iLog = input$iLogBarUni, iLabel = input$iLabelBarUni, iPareto = input$iParetoUni, iRotateX = input$iRotateXUni)
  }, bg= "transparent")
  
  output$plotHist = renderPlot({
    req(input$numVar %in% names(ati_topN))
    
    filterData() %>% select(input$numVar) %>%
      gg_hist(x.lab = input$numVar, myBins = input$numBinsHistUni, iLog = input$iLogHistUni)
  }, bg= "transparent")
  
  output$plotWC = renderPlot({
    req(input$dept, unigram())
    
    ati_wc = unigram() %>%
      {if(input$filterDataSample != "All") filter(., owner == input$dept) else .} %>%
      filter(n >= input$bigramN)
    
    wordcloud(ati_wc$word, ati_wc$n, max.words = 100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
  }, bg= "transparent")
  
  
  ## Bi tab
  output$plotMulti = renderPlot({
    req(input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData())) # , input$tabs == "Bivariate"
    
    filterData() %>%
      {if(input$multivarG == "NONE") select(., input$multivarX, input$multivarY) else select(., input$multivarX, input$multivarY, input$multivarG)} %>%
      make_multi_plot(input$catXcatType, input$maxLevelsMulti, input$maxGroupsMulti, input$maxLabelMulti, input$cutoffMulti, input$iKeepFac, input$iLogMulti, 
                      input$iRotateXMulti, input$iFreqPoly, input$numBinsMulti, input$iLinearMulti, input$iXYMulti, input$iJitterMulti)
  }, bg= "transparent")
  
  ## Bigram tabs
  output$plotBigramN = renderPlot({
    
    bigram() %>%
      {if(input$filterDataSample != "All") filter(., owner == input$dept) else .} %>%
      ungroup() %>%
      select(-owner) %>%
      filter(n > input$bigramN) %>%
      visualize_bigrams()
    
  }, bg= "transparent")
  
  output$plotBigramCorr = renderPlot({
    
    word_cors() %>%
      filter(correlation > input$bigramCorrMin) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
    
  }, bg= "transparent")
  
  
  ### Summary Text ###
  ### ------------ ###

  ## Sidebar
  output$dataNtotal  = renderText(  nTotal()  )
  
  ## Bi tab
  output$summStatsAvgX  = renderText(   make_summ_text(summaryStats(), "avg",  input$multivarX, input$tabs)   )
  output$summStatsSdX   = renderText(   make_summ_text(summaryStats(), "sd",   input$multivarX, input$tabs)   )
  output$summStatsAvgY  = renderText(   make_summ_text(summaryStats(), "avg",  input$multivarY, input$tabs)   )
  output$summStatsSdY   = renderText(   make_summ_text(summaryStats(), "sd",   input$multivarY, input$tabs)   )
  
  ### Gauges ###
  ### ------ ###
  
  ## Univariate tab
  output$gaugeFac = renderGauge({   req(input$facVar %in% names(ati_topN)); render_gauge(input$facVar, filterData())   })
  output$gaugeNum = renderGauge({   req(input$numVar %in% names(ati_topN)); render_gauge(input$numVar, filterData())   })
  
  ## Bivariate tab
  output$gaugeBiComp = renderGauge({
    req(input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()), input$tabs == "Bivariate")
    
    filterData() %>%
      select(input$multivarX, input$multivarY) %>% 
      complete.cases() %>% mean() %>% make_gauge()
  })
  
  ### Download ###
  ### -------- ###
  output$downPlotTFIDF = downloadHandler(
    filename = function() {paste("Most-Important-Terms", input$dept, input$nGramN, "grams.png", sep = "-")},
    
    content = function(file) {      ggsave(file, width = 18, height = 8, units = "in")    }
  )
  
  output$downPlotTopics = downloadHandler(
    filename = function() {paste("Topic-Modeling", input$dept, input$topicN, "topics.png", sep = "-")},
    
    content = function(file) {      ggsave(file, width = 18, height = 8, units = "in")    }
  )
  
  # output$testText1 = renderPrint(input$tabs) # Testing - remove
  # session$onSessionEnded(stopApp) # Uncomment to have R stop on browser close
  
} # END server

