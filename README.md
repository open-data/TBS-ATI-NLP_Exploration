# TBS-ATI-NLP_Exploration

## Local Setup in RStudio


```
install.packages(c("shiny","shinydashboard","tidyverse","magrittr",
                   "rlang","flexdashboard","DT","shinycssloaders",
                   "tidytext","igraph","ggraph","widyr","tm","wordcloud",
                   "topicmodels","scales","ggthemes","gghighlight","textmineR","ggwordcloud"))
                   
library(shiny)
runGitHub("open-data/TBS-ATI-NLP_Exploration/")

```

## Sidebar

The sample dropdown applies to the Deep Dive tab only. If set to 'Dept', the statistics on those subtabs (and the count in the sidebar) will only reflect the department chosen below. If set to 'All' then it will reflect all of the top 9 Departments. This dropdown does not affect the Overview tab, which always splits by department regardless of the setting. 

![sidebar screenshot](/blob/master/screen_shots/ATI-NLP-01.png)

You can set the minimum count for inclusion in the two bigram tabs, under Deep Dive. The higher the number, the more strict the filter and thus the more sparse the graph/table. 

### Stop Word Management

You can remove the department name from the query by clicking the box. I can see some instances when you'd want to do that, and others when you wouldn't, hence why it's an option. This affects all tabs.

You can also add your own stop words to the list in real time, although it won't save them. To save them permanently, you can enter them into the CSV 'stop_words_custom.csv'. However, if while browsing the app, someone wants to remove something under a What-If scenario, you can use this text box. For multiple words, separate with a comma. Click 'Enhance' when you are done entering words to apply the new filter.

## Main Tabs

### Overview

1. Most Important Terms

This tab shows the TF-IDF scores (Term Frequency - Inverse Document Frequency) for each of the top 9 Departments (by requests). 

You can control the N-grams as follows:

- 1-grams are words
- 2-grams are bigrams, pairs of words in order
- 3-grams are triplets of words, ...

You can also control how many N-grams you want to see in the graphic. Note that sometimes you'll see more than N, if there are ties at the bottom.

Click the button to download a plot of the graphs. You should be prompted to save it to your Downloads folder.

2. Topics 

This tab does some topic modeling using LDA. The main themes are listed in the title of each facet, which shows a wordcloud of words contributing most to the themes. 

You can set the number of topics in the dropdown, although sometimes the algorithm will fit fewer topics. 

Click the button to download a plot of the graphs. You should be prompted to save it to your Downloads folder.

### Deep Dive

1. Univariate

This tab contains some summary graphs for Categorical and Numerical variables occuring in the dataset. 

2. Bivariate

This tab contains some summary graphs for the bivariate relationship between any two variables. The graph will update automatically based on the type of variable, and there are a few options to configure the graph. Anything more specific and you'll have to modify 'GGgraphs.R'. 

3. Bigrams - N

A deeper look into bigram relationships showing the relationship between words, keeping into account the order they occur in the query (note the arrows). 

4. Bigrams - Corr

A deeper look into bigram relationships showing the correlation between words, irrespective of where they occur in the query. You can set a minimum correlation, below which relationships won't appear in the graph or table. 
