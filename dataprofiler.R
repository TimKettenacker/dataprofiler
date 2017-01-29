## 
## data profiler for name components
## 
## this script is applied on each dataset to assess its content's impact
## on validation and matching 
## 

require(readr)
require(plyr)
require(elastic)
require(httr)
require(DT)

raw_data <- read_delim(file.choose(), delim = ";", col_names = TRUE, locale = locale(encoding = "UTF-8"))
## str(raw_data)

# Let's start with name components of persons and/or organizations

# remove special characters and count word occurrences 
# split words into tokens and make a preselection on token length and relative frequency

name <- unlist(lapply(raw_data$name1, strsplit, split = " ")) # name1 is a placeholder for the name column, please change manually
name <- tolower(name)
name <- lapply(name, gsub, pattern = "[[:punct:]]", replacement = " ")
name <- unlist(lapply(name, strsplit, split = " "))
name <- name[order(nchar(name), decreasing = TRUE)]
name <- name[nchar(name) >= 2]
name_token_count <- plyr::count(name)
name_token_count$token_len <- unlist(lapply(as.vector(name_token_count$x), nchar))
# allow to include tokens of length 2 given a certain frequency 
# reasoning is to catch abbreviations used to distinguish company units, like Bank I, Bank II 
name_token_count <- subset(name_token_count, freq >= 5 & token_len >= 3 | freq >= 50)
name_token_count <- arrange(name_token_count, desc(freq))

# load tokens into elastic

connect()
if(!index_exists("bamboo-shack")){
  index_create("bamboo-shack")
}

# since elastics doc_bulks api doesn't handle German umlauts properly (or at least not with this framework), 
# every record is send in by an ordinary POST request in a loop (*apply-family didn't really bring any performance increase to the table)

name_tokens_ls <- setNames(as.list(name), rep_len(c("name"), length(name)))
j <- 1
for(j in 1:length(name)){
  load_silently <- POST(url = "http://localhost:9200/bamboo-shack/bamboo-shack",  body = name_tokens_ls[j], content_type("application/json"), encode = "json")
}

# double-check if all data is loaded to elastic
# you may also have a look at http://localhost:9200/_cat/indices?v as "scan" and "scroll" may deprecate in newer elastic releases

if(elastic::count(index = "bamboo-shack") == length(name)){
  print("All name tokens have been loaded successfully to elasticsearch.")
} else{ print("Warning: Data loaded to elasticsearch doesn't match the amount of name tokens.") }

# query elasticsearch with the pre-selected tokens to detect the famous German word concatenations
# collect both total amount of findings as well as two examples 

examples <- vector(mode = "list", length = length(name_token_count$x))
req_prep_list <- laply(name_token_count$x, sprintf, fmt = '*%s*')
total_hits <- vector(mode = "list", length = length(name_token_count$x))
for(q in 1:length(req_prep_list)){
  response <- Search(index = "bamboo-shack", q = req_prep_list[q], size = 10000)
  total_hits[q] <- response$hits$total
  examples[q] <- paste(as.character(response$hits$hits[[1]]$`_source`$name), 
                                  as.character(response$hits$hits[[3]]$`_source`$name),
                                  sep = " - ")
}

# one can compare the amount of words in the response of elastic with the relative frequency of tokens
# if the total hits in elastic exceed the token frequency, this means the token reappears in other
# words as well and does not only match with itself

name_token_count$total_hits <- unlist(total_hits)
name_token_count$examples <- unlist(examples)

# order according to multiple-hits and display acumulative percentages 

name_token_count <- arrange(name_token_count, desc(total_hits))
name_token_count$perc <- unlist(lapply(name_token_count$total_hits, function(X) round((X/sum(name_token_count$total_hits))*100, digits = 3)))
name_token_count$acc_perc <- cumsum(name_token_count$perc)
name_token_count <- name_token_count[c("x", "examples", "freq", "total_hits", "perc", "acc_perc")]
names(name_token_count) <- c("Token", "Examples in data", "Frequency of the token itself", "Overall frequency (including as part of other words)",  
                             "relative percentage", "accumulated percentage")

# display in layouted, searchable html table

datatable(name_token_count, options = list(pageLength = 10))


# next, let's have a look at some KPIs for postal addresses

# find length of all postal code values

postal_code_length <- nchar(raw_data$zip) # zip is a placeholder for the postal code column, please change manually
postal_code_length_tbl <- table(postal_code_length)
postal_code_blanks <- sum(is.na(raw_data$zip))

# find cities with extraordinary long names (hinting at suburbs in the same field), short names (indicating random dummy values) and blanks

city_exceeding <- which(nchar(raw_data$city) > 20) # city is a placeholder for the city column, please change manually
city_exceeding <- raw_data[city_exceeding,]
city_short <- which(nchar(raw_data$city) < 3)
city_short <- raw_data[city_short,]
city_blanks <- sum(is.na(raw_data$city))



