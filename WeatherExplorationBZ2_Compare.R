## WeatherExploration.R



## Download the data file into the ./data directory

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipFile <- "data/StormData.csv.bz2"
download.file(url=url,destfile=zipFile,method="auto")
  
stormData1 <- read.csv(zipFile)
dataSize <- object.size(stormData1)


### Trying to unzip the bz2 file independent of read.csv.....
### This is an optional step, because read.csv works directly
### with the bz2 input file.
BFR.SIZE <- 1e7
filename <-zipFile
dest_file <- "data/StormData.csv"

# Setup input and output connections
inn <- gzfile(filename, open = "rb")
out <- file(description = dest_file, open = "wb")
# Process
nbytes <- 0
repeat {
  bfr <- readBin(inn, what=raw(0L), size=1L, n=BFR.SIZE)
  n <- length(bfr)
  if (n == 0L) break;
  nbytes <- nbytes + n
  writeBin(bfr, con=out, size=1L)
  bfr <- NULL  # Not needed anymore
}
close(inn)
close(out)

### Read the uncompressed file and compare ...
stormData1A <- read.csv("data/StormData.csv")
dataSizeA <- object.size(stormData1A)

