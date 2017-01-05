# this is a helper script to create a file of German industry names to be used as an input to dataprofiler.R or dataprofiling.ipynb
# it utilizes an open data file of German words that can be downloaded from "https://sourceforge.net/projects/germandict/?source=typ_redirect" as german.dic

# make sure to adjust encoding
require(readr)
german_words <- read_csv("german.dic", locale = locale(encoding = "UTF-8"), col_names = FALSE)

# function: takes word as an input, runs grep over German dictionary and returns list of results
grepAndList <- function(char) {
  char <- as.character(char)
  int_list <- list()
  int_list <- grep(char, german_words$X1)
  word_list <- vector(mode = "list", length = length(int_list))
  counter <- 1
  for(i in int_list){
    word_list[counter] <- german_words$X1[i]
    counter <- counter + 1
  }
  return(word_list)
}

# execute function for a few German industry-specific words 
list_industry <- grepAndList("industrie")
list_department <- grepAndList("abteilung")
list_agency <- grepAndList("vertretung")
list_corporation <- grepAndList("gesellschaft")

# create sample from grepped and random words
grep_words = list()
grep_words <- append(grep_words, c(list_agency, list_industry, list_department, list_corporation))
grep_words <- sample(grep_words, 4960, replace = TRUE)
grep_words <- append(grep_words, c(sample("Industrie", 10, replace = TRUE), sample("Abteilung", 10, replace = TRUE), 
                                   sample("Vertretung", 10, replace = TRUE), sample("Gesellschaft", 10, replace = TRUE)))
random_words <- as.list(sample(german_words$X1, 5000))
sampled_words = list()
sampled_words <- append(grep_words, random_words)
sampled_words_df <- data.frame(unlist(sampled_words))
names(sampled_words_df) <- "name1"

# write output file to wd
out_file <- paste(getwd(), "\\random_name_sample.csv", sep="")
write.csv2(sampled_words_df, out_file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

