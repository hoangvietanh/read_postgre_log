# Url
url_root = "http://118.70.184.30:8003/log/"

# Require packages
library(httr)
library(plyr)
library(stringr)
library(RCurl)

# function
read_url = function(url){
        get_url   = GET(url)
        df        = read.table(text=content(get_url, as="text"), sep=",", header=TRUE, skip=2)
        df        = as.data.frame(df)
        names(df) = "content"
  return(df)
}

# Read multi files
content_url              = getURL(url_root, ftp.use.epsv = FALSE, dirlistonly = TRUE)
get_files_name           = ldply(str_match_all(content_url, "postgresql-\\d{4}-\\d{2}-\\d{2}_\\d{6}.log"))
names(get_files_name)    = "file_name"
get_files_name$root_url  = url_root
get_files_name$ulr       = paste(get_files_name$root_url, get_files_name$file_name, sep = "") 
list_files               = split(get_files_name, get_files_name$ulr)


# 02. Subset df
i = 1
n = seq_along(list_files)
list_read   = list()
list_subset = list()
list_spl    = list()
value       = list()
date        = list()
time        = list()
result      = list()


for(i in n){
  list_read[[i]]     = read_url(list_files[[i]]$ulr)
  list_subset[[i]]   = list_read[[i]][grepl('.*parameters*', list_read[[i]]$content), ]
  list_spl[[i]]      = as.data.frame(str_split_fixed(list_subset[[i]], "=", 2))
  value[[i]]         = list_spl[[i]][2]
  date[[i]]          = gsub("\\ .*","",list_spl[[i]]$V1)
  time[[i]]          = ldply(str_match_all(list_spl[[i]]$V1, "\\d{2}\\:\\d{2}\\:\\d{2}"))
  result[[i]]        = data.frame(date[[i]], time[[i]], value[[i]])
  names(result[[i]]) = c("date", "time", "value")
  i = i + 1
}

df = do.call(rbind, result)
setwd("c:/todel")
write.csv(df,"result.csv")
View(df)
