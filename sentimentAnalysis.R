library(twitteR)
library(ROAuth)
library(RCurl)
library(RCurl)
library(shiny)
library(DT)
library(here)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(tidyverse)
option <- c('JNE','Pos Indonesia','J&T Express','FedEx','Sicepat')
ui <- fluidPage(
  title = "Sentimen Analisis",
  headerPanel("Sentimen Analisis Pada Twitt Pemakai Jasa Pengiriman Barang"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="select",
                  label = "Daftar Layanan",
                  choices = option,
                  selected = '01'),
      
      textInput("value", "Jumlah Data", value = "", width = NULL,
                placeholder = NULL),
      
      actionButton("submit", "PROSES", class = "btn-danger"),
     
     
      hr(),
     
      HTML('<center><img src="delivery.jpg"></center>'),
      textInput("search", "", value = "", width = "0.1%",
                placeholder = NULL),
      
      
      
    ),
    mainPanel(
      
      DT::dataTableOutput("mytable"),
    
      plotlyOutput(outputId = "plot"),
      
      plotlyOutput(outputId = "plot2")
      
    
    )
  )
)


server <- function(input,output,session){
  
  #kami menggunakan metode Lexicon Based untuk sentimen analisis 
  #ini adalah perpustakaan kata2 negatfi dan positif
  pos = scan(here("positive_words_id.txt"), what = 'character', comment.char = ';')
  neg = scan(here("negative_words_id.txt"), what = 'character', comment.char = ';')
  
  pos.words = c(pos)
  neg.words = c(neg)

  v <- reactiveValues(data = NULL)
  hasil <<- NULL
  selected <<- ""
  esearch <<- ""
  value <<- 0
  
  #untuk mendapatkan API dapat mendaftar di developer.twitter dan tunggu email balasan yang berisi
  ambildata <- function(search,value){
    #Download sertifikasi curl
    download.file(url = "http://curl.haxx.se/ca/cacert.pem",destfile = "cacert.pem")
    #Meminta izin pada twitter untuk request API
    reqURL <- "https://api.twitter.com/oauth/request_token"
    accessURL <- "https://api.twitter.com/oauth/access_token"
    authURL <- "https://api.twitter.com/oauth/authorize"
    CUSTOMER_KEY <- "upVHlSF3JvxQ5Y3U3oPHzu2m7" 
    CUSTOMER_SECRET <- "vx1ghBuP6VohrQPZgLSNtQk600mR1gXa5R1We6db4NDHUYdNVY"
    ACCESS_TOKEN <- "231268809-OzwpMyU4rjYcwiAG8eC0SvAdvsoCirwHbQIJsdBC"
    ACCESS_secret<- "HXLCY8bmf6HuSlMDHctS117zcZHWM1Oh4yU4k34ZflvSi"
    
    #Setup Authorization
    setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)
    
    #Tahap 2 : engambil data twitter
    search.string <- search
    no.of.tweets <- value
    
    #Versi inggris
    borobudur.Tweets <- searchTwitter(search.string, n = no.of.tweets, lang = "en")
    df_en <- do.call("rbind", lapply(borobudur.Tweets, as.data.frame))
    
    #disini adalah pencarian twitt berdasarkan pilihan yang diambil
    borobudur.Tweets <- searchTwitter(search.string, n = no.of.tweets, lang = "id")
    df_id <- do.call("rbind", lapply(borobudur.Tweets, as.data.frame))
    
    
    
    sentences<-df_id
    
    #dibawah ini merupakan codingan sentimen analisisnya
    score.sentiment = function(sentences,date,retweet, pos.words, neg.words, .progress='none')
    { 
      require(plyr)
      require(stringr)
      
      #diawali dengan pre-processing data untuk membersihkan twitt
      
      scores = laply(sentences, function(sentence, pos.words, neg.words) {
        #gsub() function replaces all matches of a string
        # clean up sentences with R's regex-driven global substitute, gsub():
        #remove html links:
        sentence = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", sentence)
        #remove retweet entities:
        sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", sentence)
        #remove #hashtags:
        sentence = gsub("#\\w+", " ", sentence)
        #remove all "@people":
        sentence = gsub("@\\w+", " ", sentence)
        #remove all punctuations:
        sentence = gsub("[[:punct:]]", " ", sentence)
        #remove numbers, kita hanya butuh teks untuk analytics
        sentence = gsub("[[:digit:]]", " ", sentence)
        #remove unnecessary spaces (white spaces, tabs, etc)
        sentence = gsub("[ \t]{2,}", " ", sentence)
        sentence = gsub("^\\s+|\\s+$", "", sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence) #angka
        # and convert to lower case:
        sentence = tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # Kata kata yang terpecah akan dikomparasikan dengan library kata kata negatif dan positif
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
      }, pos.words, neg.words, .progress=.progress )
      
      scores.df = data.frame(score=scores, text=sentences,date = date,retweet = retweet)
      return(scores.df)
    }
    
    result = score.sentiment(sentences$text,sentences$created,sentences$retweetCount, pos.words, neg.words)
    
    sum(result$score)
    
    #Score akan dihitung dan menghasilkan
    result$klasifikasi<- ifelse(result$score>0,"Positif", ifelse (result$score<0,"Negatif", "Netral"))
    return(result)
    
  }
  observeEvent(input$submit, {
    v$plot = NULL
    v$data = NULL
    v$plot2 = NULL
    
    
    if(value!=input$value | selected != input$select){
      esearch <<- input$search
      value <<- input$value
      selected <<- input$select
      hasil <<- NULL
    }
    if(esearch != input$search){
      esearch <<- input$search
      value <<- input$value
      selected <<- input$select
      hasil <<- NULL
    }
    if(is.null(hasil)){
      if(esearch!=""){
        data <- ambildata(esearch,value)
      }else{
        data <- ambildata(selected,value)
      }
      hasil <<- data%>%select(text,date,klasifikasi,retweet)
      v$data <- hasil
      cnegatif <- hasil%>%filter(klasifikasi=='Negatif')%>%count_()
      cpositif <- hasil%>%filter(klasifikasi=='Positif')%>%count_()
      cnetral <- hasil%>%filter(klasifikasi=='Netral')%>%count_()
      
      #visualisasi data akan berbentuk 3 yaitu tabel twitt, grafik garis, dan grafik plot
      v$plot <- hasil%>%ggplot(aes_string(y='retweet',x='date',colour = 'klasifikasi'))+geom_line(alpha=0.7)+theme_light()
      v$plot2 <- hasil%>%ggplot(aes_string(y='retweet',x='date',colour = 'klasifikasi'))+geom_point(alpha=0.7)+theme_light()
     
    }else{
      v$data <- hasil
      cnegatif <- hasil%>%filter(klasifikasi=='Negatif')%>%count_()
      cpositif <- hasil%>%filter(klasifikasi=='Positif')%>%count_()
      cnetral <- hasil%>%filter(klasifikasi=='Netral')%>%count_()
      v$plot <- hasil%>%ggplot(aes_string(y='retweet',x='date',colour = 'klasifikasi'))+geom_line(alpha=0.7)+theme_light()
      v$plot2 <- hasil%>%ggplot(aes_string(y='retweet',x='date',colour = 'klasifikasi'))+geom_point(alpha=0.7)+theme_light()
     
    }
    })
 #dibawah ini berfungsi untuk merender tabel, grafik garis, dan grafik titik yang nantinya akan di kirim ke bagian front end dan memiliki ID
  output$plot <- renderPlotly({
    if(is.null(v$plot))return()
    ggplotly(v$plot)
  })
  output$plot2 <- renderPlotly({
    if(is.null(v$plot2))return()
    ggplotly(v$plot2)
  })
  output$mytable = DT::renderDataTable({
    if(is.null(v$data))return()
    v$data
  })
}
shinyApp(ui = ui,server = server,options = list(heights="400px"))

