list.of.packages <- c("shinythemes", "shiny", "igraph", "tm", "tokenizers","stringr","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinythemes)

ui_prova<-fluidPage(
  theme = shinytheme("united"),
  title = "SemanticEgoApp",
  titlePanel(h1("Semantic Ego-Network of Concepts' Co-occurences per sentence with R",h6("by",em("Iacopo Ghinassi")))),
  
  sidebarLayout(
    sidebarPanel(h2("Input"),width=4,
                 fluidRow(
                   fileInput("file1",h3("File input"))),
                 fluidRow(
                   textInput("dictionary1",label = "Insert dictionary's words",
                             value = "write something"),
                
    submitButton("Submit")),
    br(),
    selectInput('showdata',"Show Co-Occurrences numerical values?",c("no","yes")),
    br(),
    DT::dataTableOutput('data')),
    mainPanel(column(12,h2("Semantic Network"),align="center"),
      column(12,plotOutput(outputId = "graphnet"),align="center",style="background-color:#ccccff"),
      column(12,br()),
    
    
    column(4,selectInput("color","Network color:",c(Red="red",
                                                     Blue="blue",
                                                     Green="green",
                                                     Yellow="yellow", 
                                                     Black="black"))),
    column(4, selectInput("language","Language:",c(english="english", 
                                                    italian="italian",
                                                    spanish="spanish",
                                                    german="german"))),
    column(4,selectInput("stem","Apply stemming?",c("yes","no"))),
    br(),
    column(6,sliderInput(label="Select number of co-occurrences to show",sep = 1,step = 1,inputId = "value",min = 1,max = 20,value = 5)),
    column(6, selectInput("layout","Layouts:",c(layout.mds="layout.mds",layout.graphopt="layout.graphopt",layout.gem="layout.gem",
                                                                layout.circle="layout.circle"))),
    br(),column(12,align="center",a(img(src="AnneBonnieHat.jpg",style="height:300px;"),href="https://github.com/Ighina")))))
    
    server_prova<-function(input,output,session){
      output$graphnet <- renderPlot({
        library(tokenizers)
        library(tm)
        library(stringr)
        library(readr)
        library(igraph)
        if(is.null(input$file1)){
          return(title("No text"))
        }
        else{
        text<-readLines(input$file1$datapath)
        text<-paste(text,collapse = " ")
        preproc_text<-gsub("((?:\b| )?([.,:;!?]+)(?: |\b)?)", " \\1 ", text, perl=T)
        if(input$stem=="yes"){
        preproc_text<-stemDocument(preproc_text)}
        preproc_text<-paste(preproc_text, collapse = " ")
        dict_list<-list()
        dict_list[[1]]<-vector()
        dict_list[[1]]<-input$dictionary1
        dict_list[[1]]<-strsplit(dict_list[[1]], ",")
        dict_list[[1]]<-unlist(dict_list[[1]])
        dict_list[[1]]<-str_trim(dict_list[[1]])
        

          if(length(dict_list[[1]])==0){
            dict_list[[1]][1]<-"write something"
          }
        
        if (dict_list[[1]][1]=="write something") {
          title(warning("WRITE AT LEAST ONE WORD IN THE DICTIONARY!"))
        }
        else{
        if(input$stem=="yes"){
        for (i in 1:length(dict_list)) {
          dict_list[[i]]<-append(dict_list[[i]],stemDocument(dict_list[[i]],language = "english"))# Select the language that apply
        }
        }
        All_dict_words_to1term <- function(starting_doc,dict,sub_term){
          for (i in 1:length(dict)) {
            if (i==1){
              new_doc<-str_replace_all(starting_doc,dict[i],sub_term)
            }
            else{
              new_doc<-str_replace_all(new_doc,dict[i],sub_term)
            }
          }
          return(new_doc)
        }
        
        # Function to iterate the previous function over several dictionaries and create a list of the texts thus processed (the final element of the list is the one processed after all of the dictionaries)
        All_dict_words_to1term_fin <-  function(starting_doc,dictionaries){
          result<-list()
          for (i in 1:length(dictionaries)) {
            if (i==1) {
              result[[i]]<-All_dict_words_to1term(starting_doc,dictionaries[[i]],dictionaries[[i]][1])
            }
            else{
              result[[i]]<-All_dict_words_to1term(result[[i-1]],dictionaries[[i]],dictionaries[[i]][1])
            }
          }
          return(result)
        }
        processed_text<-All_dict_words_to1term_fin(preproc_text,dict_list)
        processed_text<-processed_text[[length(dict_list)]]
        
        clean_token<-function(x){
          library(tokenizers)
          x<-iconv(x, "latin1",'UTF-8', sub = "") #change the parameters of conversion as needed, esepcially if errors are thrown at this stage
          x<-tokenize_sentences(x)
          return(x)
        }
        text_sentence<-clean_token(processed_text)
        
        Corpusv<-VCorpus(VectorSource(unlist(text_sentence)))
        Corpusv <- tm_map(Corpusv, content_transformer(tolower))
        Corpusv <- tm_map(Corpusv, removePunctuation)
        Corpusv <- tm_map(Corpusv, removeNumbers)
        if (input$language=="english") {
        Corpusv <- tm_map(Corpusv, removeWords, stopwords("en"))#select the language of the texts
        }
        if (input$language=="italian") {
          Corpusv <- tm_map(Corpusv, removeWords, stopwords("italian"))#select the language of the texts
        }
        if (input$language=="spanish") {
          Corpusv <- tm_map(Corpusv, removeWords, stopwords("spanish"))#select the language of the texts
        }
        if (input$language=="german") {
          Corpusv <- tm_map(Corpusv, removeWords, stopwords("german"))#select the language of the texts
        }
        Corpusv <- tm_map(Corpusv, stripWhitespace)
        TDM<-TermDocumentMatrix(Corpusv)
        dict_new<-vector()
        for (i in 1:length(dict_list)) {
          dict_new[i] <- dict_list[[i]][1]
        }
        dict_new<-tolower(dict_new)
        print(dict_new)
        prova<-list()
        for (i in 1:length(dict_new)) {
          prova[[i]]<-findAssocs(TDM, dict_new[i],corlimit = .001)
          prova[[i]]<-unlist(prova[[i]])[1:input$value]#Here add the bar for increase the number
        }
        
        vec1<-vector()
        vec2<-vector()
        vec3<-vector()
        vectore<-vector()
        for (i in 1:length(dict_new)) {
          if(i==1){vectore<-append(vectore,c((1+length(dict_new)):((1+length(dict_new))+input$value)))}
          else{
            vectore<-append(vectore,c((vectore[length(vectore)]+1):((vectore[length(vectore)]+1)+5)))}
        }
        for (i in 1:length(dict_new)) {
          vec1<-append(vec1,replicate(input$value,i))
          vec2<-append(vec2,substr(names(prova[[i]]),nchar(dict_new[i])+2,nchar(names(prova[[i]]))))
          vec3<-append(vec3,prova[[i]])
        }
        vectore<-vectore[1:(length(vectore)-length(dict_new))]
        print(vectore)
        print(vec1)
        print(vec2)
        edge_df<-data.frame(vec1,vectore,vec3)
        nodes_df<-c(dict_new,vec2)
        edges_vector<-vector()
        for (j in 1:nrow(edge_df)) {
         edges_vector<-append(edges_vector,as.character(edge_df[j,1:2]))
        }
        print(edges_vector)
        if(length(na.omit(edges_vector))==0){
         title('No Co-Occurrences Found!')
        }
        else{
        graphnet<-graph(edges_vector,directed = FALSE) 
        
        for (j in 1:nrow(edge_df)) {
         graphnet<-set.edge.attribute(graphnet,'Weight',
                                     index = j,value = edge_df[j,3])
        }
        #prova<-unlist(prova)
        #provone<-list()
        #for (i in 1:length(dict_new)) {
         # provone[[i]]<-prova[paste(dict_new[i],".",dict_new,sep = "")]
          #provone[[i]]<-`names<-`(provone[[i]],dict_new)
        #}
        #delete_na<-function(lists){
         # for (i in 1:length(lists)) {
          #  lists[[i]]<-na.omit(lists[[i]])
          #}
          #return(lists)
        #}
        #Delete_dupl_in_corrlist<-function(corr_list,dict){
         # corr_list1<-list()
          #for (i in 1:(length(dict)-1)) {
           # corr_list1[[i]]<-corr_list[[i]][dict[i+1:length(dict)]]
          #}
          #for (i in 1:(length(dict)-1)) {
           # corr_list1[[i]]<-corr_list1[[i]][1:(length(dict)-i)]
          #}
          #return(corr_list1)
        #}
        #provone<-try(Delete_dupl_in_corrlist(provone,dict_new))
        #if(class(provone)=="try-error"){
         #title(warning("Include at least three dictionaries"))
        #}
        #else{
        #provone<-delete_na(provone)
        
        node_reference<- data.frame(dict_new, c(1:length(dict_new)))
        node_reference<-node_reference[,2]
        node_reference<-`names<-`(node_reference,dict_new)
        
        #create_edge_table<-function(weight_list,node_vector){
         # finalist<-list()
          #for (i in 1:length(weight_list)) {
           # finalist[[i]]<-data.frame(rep(names(node_vector[i]),times=length(weight_list[[i]])),rep(node_vector[names(node_vector[i])],times=length(weight_list[[i]])),node_vector[names(weight_list[[i]])],weight_list[[i]])
          #}
          #for (i in 1:length(finalist)) {
           # finalist[[i]]<-`names<-`(finalist[[i]],c('Label','Source','Target','Weight'))
          #}
          #for (i in 1:(length(finalist))) {
           # if (i>1&i<3) {
            #  df<-merge(finalist[[i]],finalist[[i-1]],all.x = TRUE,all.y = TRUE)
            #}
            #if (i>2) {
             # df<-merge(finalist[[i]],df,all.x = TRUE,all.y = TRUE)
            #}
          #}
          #return(df)
        #}
        
        #edge_df<-create_edge_table(provone, node_reference)
        #if(class(edge_df)=="function"){
        #  title(warning("Include at least three dictionaries or,","\n",
         #               "if three dictionaries were included,","\n", 
          #              "no correlation was found between the dictionaries"))
        #}
        #else{
        termFreq2<-function(x){
          termFreq(x, 
                   control = list(content_transformer(tolower),
                                  removePunctuation,removeNumbers,
                                  removeWords, stopwords(kind=input$language),stripWhitespace))# Change the language of stopwords if needed
        }
        
        processed_text<-iconv(processed_text, "UTF-8",'latin1', sub = "") #change the parameters of conversion as needed, esepcially if errors are thrown at this stage
        Text_freq<-termFreq2(processed_text)
        tot_words<-c(dict_new,vec2)
        Text_freq<-(Text_freq[tot_words]/sum(Text_freq))*100000
        print(Text_freq)
        for (i in 1:length(Text_freq)) {
          if (is.na(Text_freq[i])) {
            Text_freq[i]<-1
          }
        }
        if (input$value>4) {
        for (i in 1:length(Text_freq)) {
          if (Text_freq[i]>100) {
            Text_freq[i]<-100
          }}}
        if (input$value>10) {
        for (i in 1:length(Text_freq)) {
          if (Text_freq[i]>80) {
            Text_freq[i]<-80
          }
        }}
          if (input$value>50) {
            for (i in 1:length(Text_freq)) {
              if (Text_freq[i]>50) {
                Text_freq[i]<-50
              }
            }}
        nodes_df<-data.frame(nodes_df,node_reference,Text_freq)
        
        #edges_vector<-vector()
        #for (j in 1:nrow(edge_df)) {
         # edges_vector<-append(edges_vector,as.integer(edge_df[j,2:3]))
        #}
        #print(edges_vector)
        #if(length(na.omit(edges_vector))==0){
         # title('No Co-Occurrences Found!')
        #}
        #else{
        #graphnet<-graph(edges_vector,directed = FALSE) 
        
        #for (j in 1:nrow(edge_df)) {
         # graphnet<-set.edge.attribute(graphnet,'Weight',
          #                             index = j,value = edge_df[j,"Weight"])
        #}
        
        for (j in 1:length(tot_words)) {
          graphnet<-set.vertex.attribute(graphnet,'name',
                                         index = j,value = tot_words[j])
        }
        
        V(graphnet)$color<-input$color
        
        for (j in 1:length(tot_words)) {
          graphnet<-set.vertex.attribute(graphnet,'Occurrence',
                                         index = j,value = nodes_df[j,3])
        }
        
        print(V(graphnet)$Occurrence)
        
        if(length(na.omit(V(graphnet)$Occurrence))==0){
          plot.igraph(graphnet, 
                      edge.width=E(graphnet)$Weight*40)
          title('Words Frequency is Missing!')
        }
        else{
        #print(E(graphnet)$Weight)
          for (i in 1:length(dict_new)) {
            V(graphnet)$Occurrence[i]<-V(graphnet)$Occurrence[i]/40
          }
          for (i in 1:length(V(graphnet)$Occurrence)) {
            if(is.na(V(graphnet)$Occurrence[i])==TRUE){
              V(graphnet)$Occurrence[i]<-100
            }
          }
        if(input$layout=="layout.mds"){
        plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence,edge.width=E(graphnet)$Weight*10,layout=layout.mds)}
        if(input$layout=="layout.graphopt"){
          plot.igraph(graphnet,vertex.size=V(graphnet)$Occurrence,edge.width=E(graphnet)$Weight*10,layout=layout.graphopt)}
        if(input$layout=="layout.gem"){
          plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence,edge.width=E(graphnet)$Weight*10,layout=layout.gem)}
        if(input$layout=="layout.circle"){
          plot.igraph(graphnet, vertex.size=V(graphnet)$Occurrence,edge.width=E(graphnet)$Weight*10,layout=layout.circle)}}}}
      }})
      
        
      
      output$data<-DT::renderDataTable({
        if (input$showdata=="yes") {
        library(tm)
        library(stringr)
        library(readr)
        library(ggplot2)
        if(is.null(input$file1)){
          return(title("No text"))
        }
        else{
          text<-readLines(input$file1$datapath)
          text<-paste(text,collapse = " ")
          preproc_text<-gsub("((?:\b| )?([.,:;!?]+)(?: |\b)?)", " \\1 ", text, perl=T)
          if(input$stem=="yes"){
            preproc_text<-stemDocument(preproc_text)}
          preproc_text<-paste(preproc_text, collapse = " ")
          dict_list<-list()
          dict_list[[1]]<-vector()
          dict_list[[1]]<-input$dictionary1
          dict_list[[1]]<-strsplit(dict_list[[1]], ",")
          dict_list[[1]]<-unlist(dict_list[[1]])
          dict_list[[1]]<-str_trim(dict_list[[1]])
          
          
          if(length(dict_list[[1]])==0){
            dict_list[[1]][1]<-"write something"
          }
          
          if (dict_list[[1]][1]=="write something") {
            title(warning("WRITE AT LEAST ONE WORD IN THE DICTIONARY!"))
          }
          else{
            if(input$stem=="yes"){
              for (i in 1:length(dict_list)) {
                dict_list[[i]]<-append(dict_list[[i]],stemDocument(dict_list[[i]],language = "english"))# Select the language that apply
              }
            }
            All_dict_words_to1term <- function(starting_doc,dict,sub_term){
              for (i in 1:length(dict)) {
                if (i==1){
                  new_doc<-str_replace_all(starting_doc,dict[i],sub_term)
                }
                else{
                  new_doc<-str_replace_all(new_doc,dict[i],sub_term)
                }
              }
              return(new_doc)
            }
            
            # Function to iterate the previous function over several dictionaries and create a list of the texts thus processed (the final element of the list is the one processed after all of the dictionaries)
            All_dict_words_to1term_fin <-  function(starting_doc,dictionaries){
              result<-list()
              for (i in 1:length(dictionaries)) {
                if (i==1) {
                  result[[i]]<-All_dict_words_to1term(starting_doc,dictionaries[[i]],dictionaries[[i]][1])
                }
                else{
                  result[[i]]<-All_dict_words_to1term(result[[i-1]],dictionaries[[i]],dictionaries[[i]][1])
                }
              }
              return(result)
            }
            processed_text<-All_dict_words_to1term_fin(preproc_text,dict_list)
            processed_text<-processed_text[[length(dict_list)]]
            
            clean_token<-function(x){
              library(tokenizers)
              x<-iconv(x, "latin1",'UTF-8', sub = "") #change the parameters of conversion as needed, esepcially if errors are thrown at this stage
              x<-tokenize_sentences(x)
              return(x)
            }
            text_sentence<-clean_token(processed_text)
            
            Corpusv<-VCorpus(VectorSource(unlist(text_sentence)))
            Corpusv <- tm_map(Corpusv, content_transformer(tolower))
            Corpusv <- tm_map(Corpusv, removePunctuation)
            Corpusv <- tm_map(Corpusv, removeNumbers)
            if (input$language=="english") {
              Corpusv <- tm_map(Corpusv, removeWords, stopwords("en"))#select the language of the texts
            }
            if (input$language=="italian") {
              Corpusv <- tm_map(Corpusv, removeWords, stopwords("italian"))#select the language of the texts
            }
            if (input$language=="spanish") {
              Corpusv <- tm_map(Corpusv, removeWords, stopwords("spanish"))#select the language of the texts
            }
            if (input$language=="german") {
              Corpusv <- tm_map(Corpusv, removeWords, stopwords("german"))#select the language of the texts
            }
            Corpusv <- tm_map(Corpusv, stripWhitespace)
            TDM<-TermDocumentMatrix(Corpusv)
            dict_new<-vector()
            for (i in 1:length(dict_list)) {
              dict_new[i] <- dict_list[[i]][1]
            }
            dict_new<-tolower(dict_new)
            prova<-list()
            for (i in 1:length(dict_new)) {
              prova[[i]]<-findAssocs(TDM, dict_new[i],corlimit = .001)
              prova[[i]]<-unlist(prova[[i]])[1:input$value]#Here add the bar for increase the number
            }
            
            Source<-vector()
            Target<-vector()
            Value<-vector()
            vectore<-vector()
            for (i in 1:length(dict_new)) {
              if(i==1){vectore<-append(vectore,c((1+length(dict_new)):((1+length(dict_new))+input$value)))}
              else{
                vectore<-append(vectore,c((vectore[length(vectore)]+1):((vectore[length(vectore)]+1)+5)))}
            }
            for (i in 1:length(dict_new)) {
              Source<-append(Source,replicate(input$value,dict_new[i]))
              Target<-append(Target,substr(names(prova[[i]]),nchar(dict_new[i])+2,nchar(names(prova[[i]]))))
              Value<-append(Value,prova[[i]])
            }
            vectore<-vectore[1:(length(vectore)-length(dict_new))]
            edge_df<-data.frame(Source,Target,Value)
        
            DT::datatable(edge_df)}}}
        else{DT::datatable(data = unname(data.frame("Numerical values not selected")))
          }})
    }

    shinyApp(ui=ui_prova,server = server_prova)