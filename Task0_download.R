if(!file.exists("data")) {
  dir.create("data")
}

dataURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(dataURL, destfile = "./data/Swiftkey.zip")