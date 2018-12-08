# figure out if things are prices
# generally allows two characters of fudge
# dollar sets whether to allow dollars signs before prices.
  # for tables default should be false

isPrice = #Jane's version - different than Duncan's isPrice
  
  function(x, dollar = FALSE, maybe = FALSE) {
    
    x = as.character(x)
    
    sapply(x, function(x) {
      
      if ( grepl("^([0-9]+\\.[0-9]{2}$)", x)) { return(TRUE) }
      
      else if( grepl("^\\$([0-9]+)\\.[0-9]{2}.{0,2}$", x) | grepl("^\\$([0-9]+).{0,2}$", x)) {
        warning(paste0("Is this (", x, ") a price with a dollar sign? ", collapse = " "))
        ifelse(dollar, TRUE, FALSE)
      }
      
      else if( grepl("^([0-9]+)\\.[0-9]{2}", x )) {
        warning(paste0("Is this (", x, ") a price followed by something else? ", collapse = " "))
        ifelse(maybe,  "number*", FALSE)
      }
      
      else if( grepl("^.{0,2}([0-9]+)\\.[0-9]{2}", x )) { #price with at most two non-dollar characters before it
        warning(paste0("Is this (", x, ") a price preceded by something else? ", collapse = " "))
        return(TRUE)
      }
      
      else if( grepl(".*([0-9]+)\\.[0-9]{2}", x )) {
        warning(paste0("Is this (", x, ") a price preceded by something else? ", collapse = " "))
        ifelse(maybe,  "*number", FALSE) #useful for looking at lines
      }
      
      else if( grepl("^\\$([0-9]+)\\.[0-9]{2}", x) | grepl("^\\$([0-9]+)", x)) {
        warning(paste0("Is this (", x, ") a price with a dollar sign followed by something else? ", collapse = " "))
        ifelse(dollar, TRUE, FALSE)
      }
      
      else if( grepl("^([0-9])", x)) {
        #warning(paste0("This  (", x, ") seems to be number, but not a price ", collapse = " "))
        ifelse(maybe, "number", FALSE)
      }
      
      else if( grepl("^[nN].{0,1}[0-9]+$", x)) {
        #warning(paste0("This  (", x, ") seems to be number, but not a price ", collapse = " "))
        ifelse(maybe, "ID", FALSE)
      }
      
      else{return(FALSE)}
    })
  }

numToPrice = function(numAsChar, dollar = FALSE) {
  #dollar False to not allow numbers will dollar sign in front 
  
  #option 1, just have to remove extra stuff at end, e.g. a period -- at most two
  if( grepl("^([0-9]+)\\.[0-9]{2}.{0,2}$", numAsChar)) return(str_extract(numAsChar, "^([0-9]+)\\.[0-9]{2}"))
  
  #option 2, number with wrong puncuation
  if( grepl("^([0-9]+)[\\ |,][0-9]{2}", numAsChar)) {
    return(gsub("\\ ", "\\.", str_extract(numAsChar, "^([0-9]+)[\\ |,][0-9]{2}")))
  }
  
  #option 3, number with stuff before -- allow at most lots
  if (dollar) {
    if( grepl("^.([0-9]+)\\.[0-9]{2}", numAsChar)) return(str_extract(numAsChar, "([0-9]+)\\.[0-9]{2}"))
  } else {
    if( grepl("\\$[0-9]+", numAsChar)) return(FALSE)
    else {
      if( grepl("([0-9]+)\\.[0-9]{2}", numAsChar)) return(str_extract(numAsChar, "([0-9]+)\\.[0-9]{2}"))
    }  
  }
  
  #option 4, number without punctuation (remove any puncuation at end -- allow at most 3 after)
  
  if (!grepl("[0-9]{3,10}.{0,2}$", numAsChar)) { return(FALSE) }
    else {
    numAsChar = str_extract(numAsChar, "[0-9]{3,10}")     
    num = strsplit(numAsChar, split="")[[1]]
    if(!tail(num,1) %in% c("0","5","9") & paste0(head(num,2), collapse = "") %in% c("18","19")) {
      warning(paste0(numAsChar, " is probably a year"))
      return(FALSE)
    }
    price = paste(paste0(num[1:(length(num)-2)], collapse = ""), ".",
                paste0(tail(num,2), collapse = ""), sep="")
    return(price)
  }
}
  

# Sort a catalog of images by instances of prices, numbers and other text
# will inform if a page has a price table

textTypes <- function(cat_files, catalog = NULL){
  
  if(is.null(catalog)) {catalog = sample(1:length(cat_files), 1)}
  cat1 = cat_files[catalog][[1]]
  cat("catalog number", catalog, "with", nrow(cat1), "files")
  
  text.tmp = lapply(paste(cat1$Sample, cat1$file.jpg, sep = "/"), function(x) {
    text1 = GetText(x)
    table1 = table(sapply(text1, isPrice, maybe = TRUE))
    print(prop.table(table1))
    return(data.frame(as.list(table1))) #if only one type skip -- wouldn't be a table page
  })
  
  text.types= bind_rows(text.tmp)
  names(text.types) = sapply(names(text.types), switch, "FALSE." = "other_text","number" = "number", "TRUE." = "price",
                             "number." = "number*", "X.number" = "*number")
  return(text.types)
}

#get a sample image

#setwd("~/Documents/DSI/OCR_SherryLehmann/Sample")
#img1 = ""
#while(!grepl("jpg", img1)) {
#  img1 = sample(list.files(".", recursive = T), 1)
#}
charWidth <- function(boxes) {
  (boxes$right-boxes$left)/nchar(boxes$text)
}

charTypes <- function(boxes, types = 2, conf.min = 50) { #using k-means
  boxes$charwidth = charWidth(boxes)
  boxes.use = filter(boxes, confidence > conf.min) # only use somewhat high conf boxes for finding char types
  boxes.kmeans = kmeans(boxes.use$charwidth, centers = types)
  cluster = sapply(boxes$charwidth, function(x){
    which.min(abs(x - boxes.kmeans$centers))})
  return(list(membership = cluster, means = boxes.kmeans$centers))
}

#converts a data frame of left, bottom, right, top boxes to left, bottom, width, height for the checkBoxes function
makeCheckBox <- function(df, type  = c("forCheckBox", "forBbox")) {
  if (type[1] == "forCheckBox") return(data.frame(left = df$left, bottom  = df$bottom, width = df$right - df$left, height = df$top - df$bottom))
  if (type[1] == "forBbox") return(data.frame(left = df$left, bottom  = df$bottom, right = df$left + df$width, top = df$bottom + df$height))
}

removeDuplicates <- function(table, buffer = 10) { #price or id table, should have left, bottom, right, top and confidence  
  look.for.duplicates = apply(table[,c("left","bottom","right","top")], 1, 
                              function(x) {apply(table[,c("left","bottom","right","top")], 1, 
                                  function(y) {max(abs(y-x))})})
  look.for.duplicates = data.frame(which(apply(look.for.duplicates, 1, "<",  buffer), arr.ind = T)) %>%  filter(., row > col)
  if(nrow(look.for.duplicates) > 0) {
    remove = apply(look.for.duplicates, 1, function(x) {
      x[which.min(table[x,"confidence"])]
    })
    table = table[-remove,]
  }
  table
}

