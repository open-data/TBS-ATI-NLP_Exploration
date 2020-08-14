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
  varClasses = reactive({    sapply(sapply(rawDataSet, class), "[[", 1)    })
  
  dateList   = reactive({    names(varClasses())[varClasses() %in% c("Date", "POSIXct")]    })
  factorList = reactive({    names(varClasses())[varClasses() %in% c("factor", "character")]    })
  numberList = reactive({    names(varClasses())[varClasses() %in% c("numeric", "integer", "difftime")]    })
  binaryList = reactive({    names(varClasses())[varClasses() %in% c("logical")]    })
  
  # Filtered data set
  filterData = reactive({
    req(input$filterDataSample)
    
    rawDataSet %>%
      {if(input$filterDataSample != "All") filter(., owner == input$dept) else .}
  })
  
  
  # Summary descriptive stats
  summaryStats = reactive({
    req(input$tabs == "Bivariate", input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()))
    
    filterData() %>% select(input$multivarX, input$multivarY) %>% get_summary_stats()
  })
    
  nTotal = reactive(  nrow(filterData())   ) # Printed text
  
  
  ## Update custom stop word list
  stopWordList = observeEvent(input$goStop, {
    uiWords = strsplit(input$stopWordInput, ",") %>% 
      unlist() %>% 
      trimws()
    
    addWords = c(custWords, uiWords)
    
    rv$stops = data.frame(word = addWords, lexicon = "CUSTOM") %>%
      bind_rows(stop_words)
  })
  
  ## Make N-grams
  unigram = reactive({
    rawDataSet %>% 
      filter(owner %in% ownersTop9) %>%
      count_unigrams(rv$stops) %>%
      group_by(owner) %>% 
      count(word)
  })
  
  bigram = reactive({
    rawDataSet %>% 
      filter(owner %in% ownersTop9) %>%
      count_bigrams(rv$stops)
  })
  
  trigram = reactive({
    rawDataSet %>% 
      filter(owner %in% ownersTop9) %>%
      count_trigrams(rv$stops)
  })
  
  tetragram = reactive({
    rawDataSet %>%
      filter(owner %in% ownersTop9) %>%
      count_tetragrams(rv$stops)
  })
  
  word_cors = reactive({
    req(input$dept, input$bigramN, filterData())
    
    thisDept = filterData() %>% filter(owner == input$dept)
    topWords = unigram() %>% filter(owner == input$dept) %>% filter(n >= input$bigramN) %>% pull(word)
    
    thisDept %>%
      count_unigrams(rv$stops) %>%
      filter(word %in% topWords) %>%
      pairwise_cor(word, ID, sort = TRUE)
  })
  
  dtm = reactive({
    req(input$dept, filterData())
    
    filterData() %>% 
      filter(owner == input$dept) %>%
      count_unigrams(rv$stops) %>%
      group_by(ID, owner) %>% 
      count(word) %>%
      cast_dtm(ID, word, n)
  })
  
 
  
  ### -------------- ###
  ### Input elements ###
  ### -------------- ###
  
  ## Sidebar
  ## Uni tab
  output$selectFac <- renderUI(  selectInput("facVar", "Factor Variable:",   choices = intersect(factorList(), names(rawDataSet)))  )
  output$selectNum <- renderUI(  selectInput("numVar", "Numeric Variable:",  choices = intersect(c(numberList(), dateList()), names(rawDataSet)))  )
  output$selectDat <- renderUI(  selectInput("datVar", "DateTime Variable:", choices = intersect(dateList(), names(rawDataSet)))  )
  output$selectBin <- renderUI(  selectInput("binVar", "Binary Variable:",   choices = intersect(binaryList(), names(rawDataSet)))  )
  
  ## Bi tab
  output$selectMultivarX <- renderUI(    selectInput("multivarX", "X Variable:",  choices = names(rawDataSet))   )
  output$selectMultivarY <- renderUI(    selectInput("multivarY", "Y Variable:",  choices = intersect(c(factorList(), numberList()), names(rawDataSet)))  )
  output$selectMultivarG <- renderUI(    selectInput("multivarG", "Grouping Variable (Boxplot & Scatter):",   choices = c("NONE", intersect(c(factorList(), binaryList()), names(rawDataSet))))  )
  
  observeEvent(input$switchVarsBi, {
    tempX = input$multivarX
    tempY = input$multivarY
    
    output$selectMultivarX <- renderUI(    selectInput("multivarX", "X Variable:",  choices = names(rawDataSet), selected = tempY)   )
    output$selectMultivarY <- renderUI({
      selectInput("multivarY", "Y Variable:",  choices = intersect(c(factorList(), numberList()), names(rawDataSet)), selected = tempX)
    })
  })
  
  
  ### --------------- ###
  ### Output elements ###
  ### --------------- ###
  
  ### Tables ###
  ### ------ ###
  
  output$tableBigramN = DT::renderDataTable(    DT::datatable(filter(bigram(), owner == input$dept), options = list(pageLength = 20))   )
  output$tableBigramCorr = DT::renderDataTable(    DT::datatable(filter(word_cors(), correlation > input$bigramCorrMin), options = list(pageLength = 18))   )
  
  
  ### Plots ###
  ### ----- ###
  
  ## Uni tab
  output$plotBar  = renderPlot({
    req(input$facVar %in% names(rawDataSet))
    
    filterData() %>% select(input$facVar) %>%
      gg_count(type= "Bar", x.lab = input$facVar, maxLevels = input$maxBarsUni, maxLabelL = input$maxLabelUni, 
               iLog = input$iLogBarUni, iLabel = input$iLabelBarUni, iPareto = input$iParetoUni, iRotateX = input$iRotateXUni)
  }, bg= "transparent")
  
  output$plotHist = renderPlot({
    req(input$numVar %in% names(rawDataSet))
    
    filterData() %>% select(input$numVar) %>%
      gg_hist(x.lab = input$numVar, myBins = input$numBinsHistUni, iLog = input$iLogHistUni)
  }, bg= "transparent")

  output$plotWC = renderPlot({
    req(input$dept, unigram())
    
    ati_wc = unigram() %>%
      filter(owner == input$dept) %>%
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
      visualize_tfidf(input$tfTopN)  
    
  }, bg= "transparent")
  
  ## Bigram tabs
  output$plotBigramN = renderPlot({
    
    bigram() %>%
      filter(owner == input$dept) %>%
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
  
  ## Topic Modeling tab
  output$plotTopics = renderPlot({
    req(dtm())
    
    dtm() %>% 
      LDA(k = input$topicN, control = list(seed = 1234)) %>%
      tidy(matrix = "beta") %>%
      group_by(topic) %>%
      top_n(input$topicTopN, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_x_reordered() +
      theme(axis.text = element_text(size = 12))
    
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
  output$gaugeFac = renderGauge({   req(input$facVar %in% names(rawDataSet)); render_gauge(input$facVar, filterData())   })
  output$gaugeNum = renderGauge({   req(input$numVar %in% names(rawDataSet)); render_gauge(input$numVar, filterData())   })
  
  ## Bivariate tab
  output$gaugeBiComp = renderGauge({
    req(input$multivarX %in% names(filterData()), input$multivarY %in% names(filterData()), input$tabs == "Bivariate")
    
    filterData() %>%
      select(input$multivarX, input$multivarY) %>% 
      complete.cases() %>% mean() %>% make_gauge()
  })
  
  # output$testText1 = renderPrint(str(word_cors())) # Testing - remove
  # session$onSessionEnded(stopApp) # Uncomment to have R stop on browser close
  
} # END server

