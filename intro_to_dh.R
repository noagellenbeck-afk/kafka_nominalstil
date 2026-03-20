library(stylo)
library(dplyr)
library(stringr)
library(udpipe)

autorenliste <- list.dirs()

dateibenennung <- function(autor){
  dateien <- list.files(path = autor, pattern = "*.txt")
  for (datei in dateien){
    dateipfad <- paste(autor, datei, collapse = NULL, sep = "/")
    neuername <- paste(substring(autor,3), gsub("_","",datei), collapse = NULL, sep = "_")
    file.rename(from = dateipfad, to = neuername)
  }
}

countwordsintext <- function(eingabepfad){
  text <- readLines(eingabepfad, encoding = "UTF-8") %>% paste0(collapse = " ")
  text <- text %>% str_squish()
  text <- strsplit(text, " +")[[1]]
  cat(paste(eingabepfad, "Wortanzahl = ", length(text),"\n"))
  return(length(text))
}

dateiliste <- list.files(pattern = "*.txt")

#Map(dateibenennung,autorenliste)
Map(countwordsintext, dateiliste)
stylo()

#corpus für nomen
model <- udpipe_load_model("german-gsd-ud-2.5-191206.udpipe")

#Funktion die Texte annotiert und nach Nomen filtert
annotatenouns <- function(eingabepfad, ausgabepfad){
  if (file.exists(eingabepfad)!=TRUE){
    cat("falscher eingabepfad")
    break()
  }
  if (file.exists(ausgabepfad)!=TRUE)
    file.create(ausgabepfad)
  if (file.info(ausgabepfad)$size == 0){
    textkafka <- readLines(eingabepfad, encoding = "UTF-8") %>% paste0(collapse = " ")
    textkafka <- textkafka %>% str_squish()
    textkafka_anndf <- udpipe::udpipe_annotate(model, x = textkafka) %>% as.data.frame() %>%
      dplyr::select(-sentence)
    tagged_text <- paste(textkafka_anndf$token, "/", textkafka_anndf$xpos, collapse = " ", sep = "")
    tagged_text <- textkafka_anndf %>%
      filter(upos %in% c("NOUN", "PROPN"))
    textnomen <- paste(tagged_text$token, collapse = " ", sep = "")
    file.create(ausgabepfad)
    writeLines(textnomen, ausgabepfad)
  }
}

#kafka erzählungen Nomen gefiltert
#setwd("~/Desktop/kleinere_epik_vergleich")
dateinamen <- list.files(path = "./original/corpus", pattern = "*.txt")
eingabeliste <- paste("./original/corpus/", dateinamen, collapse = NULL, sep = "")
ausgabeliste <- paste("./nouns/corpus/", dateinamen, collapse = NULL, sep = "")
Map(annotatenouns, eingabeliste, ausgabeliste)

#Funktion die Texte annotiert und nach Verben filtert
annotateverbs <- function(eingabepfad, ausgabepfad){
  if (file.exists(eingabepfad)!=TRUE){
    cat("falscher eingabepfad")
    break()
  }
  if (file.exists(ausgabepfad)!=TRUE)
    file.create(ausgabepfad)
  if (file.info(ausgabepfad)$size == 0){
    textkafka <- readLines(eingabepfad, encoding = "UTF-8") %>% paste0(collapse = " ")
    textkafka <- textkafka %>% str_squish()
    textkafka_anndf <- udpipe::udpipe_annotate(model, x = textkafka) %>% as.data.frame() %>%
      dplyr::select(-sentence)
    tagged_text <- paste(textkafka_anndf$token, "/", textkafka_anndf$xpos, collapse = " ", sep = "")
    tagged_text <- textkafka_anndf %>%
      filter(upos %in% c("VERB"))
    textverben <- paste(tagged_text$token, collapse = " ", sep = "")
    file.create(ausgabepfad)
    writeLines(textverben, ausgabepfad)
  }
}

#kafka erzählungen Verben gefiltert
#setwd("~/Desktop/kleinere_epik_vergleich")
dateinamen <- list.files(path = "./original/corpus", pattern = "*.txt")
eingabeliste <- paste("./original/corpus/", dateinamen, collapse = NULL, sep = "")
ausgabeliste <- paste("./verbs/corpus/", dateinamen, collapse = NULL, sep = "")
Map(annotateverbs, eingabeliste, ausgabeliste)

#Wortanzahl analysieren
autorenliste <- list()
dateinamen <- list.files(pattern = "*.txt")
autoren <- function(text){
    autor <- str_split_i(text,"_", 1) 
    if ((autor %in% autorenliste)==FALSE){
      autorenliste <<- append(autorenliste,autor)
      cat(autor)
    }
}
Map(autoren,dateinamen)
tabelle <- matrix(0, nrow = 3, ncol = length(autorenliste))
colnames(tabelle)<-autorenliste
rownames(tabelle)<- c("Wortanzahl","Textanzahl", "Texte")

Map(wortanzahl,autorenliste)
dateiliste <- list.files(pattern = "*.txt")
wortanzahl <- function(autor){
  liste <- dateiliste[grepl(autor, dateiliste)]
  wortzahl <- sum(sapply(liste, countwordsintext))
  tabelle["Wortanzahl",autor] <<- wortzahl
  tabelle["Textanzahl",autor] <<- length(liste)
  #tabelle["Texte", autor] <<- paste(liste, collapse = "\n")
  cat(wortzahl)
}

tabelle["Wortanzahl", "kafka"] <- tabelle["Wortanzahl", "kafka"] - tabelle["Wortanzahl", "kafkaAmt"]
tabelle["Textanzahl", "kafka"] <- tabelle["Textanzahl", "kafka"] - tabelle["Textanzahl", "kafkaAmt"]

maximum <- max(sapply(dateiliste, countwordsintext))
minimum <- min(sapply(dateiliste, countwordsintext))

  