# Load library
pkgs <- c("rvest", "dplyr")
sapply(pkgs, require, character.only = T)

# Declare main page URL
url.main <- "https://www.evspecifications.com/en/"

# Scrap URLs
scrap_urls <- F

if (scrap_urls) {
  # Scrap brand URLs
  html.main <- read_html(url.main)
  urls.brand <- html.main %>% html_node(".brand-listing-container-frontpage") %>% 
    html_nodes("a") %>% html_attr("href") %>% unique()
  
  # Scrap car URLs
  urls.car <- c()
  for (url.brand in urls.brand) {
    html.brand <- read_html(url.brand)
    print(strsplit(html.brand %>% html_nodes("title") %>% html_text(), " ")[[1]][1])
    url.car <- html.brand %>% html_nodes(".model-listing-container-80") %>% 
      html_nodes("div") %>% html_nodes("a") %>% html_attr("href") %>% unique()
    urls.car <- c(urls.car, url.car)
  }
} else {
  load("urls.RData")
}

# Declare informations to scrap
info <- c("Brand", 
          "Model", 
          "Model year", 
          "Price", 
          "Curb weight", 
          "Power", 
          "Torque", 
          "Top speed", 
          "Acceleration from 0 to 100 km/h", 
          "NEDC (New European Driving Cycle)", 
          "EPA (Electric car range and efficiency)", 
          "WLTP (Worldwide harmonized Light vehicles Test Procedure)", 
          "Battery capacity", 
          "Voltage")

# scrap car specs
scrap_spec <- F

if (scrap_spec) {
  df.spec <- data.frame(array("X", c(1, length(info))), stringsAsFactors = F)
  url.car <- urls.car[1]
  for (url.car in urls.car){
    spec.temp <- data.frame()
    html.car <- read_html(url.car)
    print(html.car %>% html_nodes("title") %>% html_text() %>% gsub(pattern = " - Specifications", replacement = ""))
    tables.temp <- html.car %>% html_nodes("table") %>% gsub(pattern = "<p>(.*?)</p>", replacement = "")
    tables <- sapply(tables.temp, function(temp) {temp %>% read_html() %>% html_table()})
    for (x in tables) {
      temp <- x[which(x[, 1] %in% info), ]
      if (ncol(temp) >= 3) {
        temp <- spec.temp[NULL, ]
      } else {
        colnames(temp) <- paste0("X", 1:ncol(temp))
      }
      spec.temp <- rbind(spec.temp, temp)
    }
    temp <- rep(NA, length(info))
    for (i in 1:length(info)) {
      id.i <- which(spec.temp[, 1] == info[i])
      if (length(id.i) == 1) {
        temp[i] <- spec.temp[id.i, 2]
      } else if (length(id.i) > 1) {
        temp[i] <- paste0(spec.temp[id.i, 2], collapse = "\n")
      }
    }
    df.spec <- rbind(df.spec, temp)
  }
  df.spec <- df.spec[-1, ]
  row.names(df.spec) <- c(1:nrow(df.spec))
  colnames(df.spec) <- info
  # Sort
  df.spec <- df.spec[, c(1:4, 14, 6:7, 5, 13, 8:12)]
  # Model
  df.spec[, 2] <- df.spec[, 2] %>% strsplit("\n") %>% sapply(function (temp) {if (length(temp) > 1) temp[1] else temp})
  # Year
  df.spec[, 3] <- df.spec[, 3] %>% as.integer()
  # Voltage
  df.spec[, 5] <- df.spec[, 5] %>% strsplit(" ") %>% sapply(function (temp) {temp[1]}) %>% as.numeric()
  # Power
  df.spec[, 6] <- df.spec[, 6] %>% strsplit("\n") %>% sapply(function (temp) {if (length(temp) > 1) as.numeric(strsplit(temp[1], " ")[[1]][1]) + as.numeric(strsplit(temp[2], " ")[[1]][1]) else as.numeric(strsplit(temp, " ")[[1]][1])})
  # Toque
  df.spec[, 7] <- df.spec[, 7] %>% strsplit("\n") %>% sapply(function (temp) {if (length(temp) > 1) as.numeric(strsplit(temp[1], " ")[[1]][1]) + as.numeric(strsplit(temp[2], " ")[[1]][1]) else as.numeric(strsplit(temp, " ")[[1]][1])})
  # Curb weight
  df.spec[, 8] <- df.spec[, 8] %>% strsplit(")") %>% sapply(function (temp) {if (length(temp) > 1) temp[2] else temp}) %>% strsplit(" ") %>% sapply(function (temp) {if (length(temp) > 1) temp[1] else temp}) %>% as.numeric()
  # Capacity
  df.spec[, 9] <- df.spec[, 9] %>% strsplit(" ") %>% sapply(function (temp) {temp[1]}) %>% as.numeric()
  # Top speed
  df.spec[, 10] <- df.spec[, 10] %>% strsplit(")") %>% sapply(function (temp) {if (length(temp) > 1) temp[2] else temp}) %>% strsplit(" ") %>% sapply(function (temp) {if (length(temp) > 1) temp[1] else temp}) %>% as.numeric()
  # 0-100
  df.spec[, 11] <- df.spec[, 11] %>% strsplit(" ") %>% sapply(function (temp) {temp[1]}) %>% as.numeric()
  # Range
  for (i in 12:14) {
    df.spec[, i] <- df.spec[, i] %>% strsplit("/ ") %>% sapply(function (temp) {if (length(temp) > 1) temp[2] else temp}) %>% strsplit(" ") %>% sapply(function (temp) {temp[1]}) %>% as.numeric()
  }
} else {
  load("spec.RData")
}
