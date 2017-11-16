# Get the files names
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind 合併成一張
lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE))

for(i in 1:length(files)){
  read.csv(files[i], stringsAsFactors = FALSE)
}

myfiles = do.call(rbind, lapply(files,function(x){
  read.csv(x, stringsAsFactors = FALSE)
}))
