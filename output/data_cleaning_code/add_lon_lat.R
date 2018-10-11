library(ggmap)
library(dplyr)

hospital_info <- read.csv("../merged_2016.csv")

hospital_address <- hospital_info %>% 
  select(Hospital.Name,Provider.ID, Address, City, State, ZIP.Code) %>% 
  distinct()

hospital_address <- hospital_address %>% mutate(Full=paste(Address,City,State,ZIP.Code,"USA",sep=", "))

for(i in 1:nrow(hospital_address))
{
  #print(i)
  result <- geocode(hospital_address$Full[i], output = "latlon", source = "dsk",messaging = FALSE)
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

for (i in which(is.na(hospital_address$lon))) {
  result <- geocode(paste(hospital_address$City[i],hospital_address$State[i],hospital_address$ZIP.Code[i],sep = ","), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

for (i in which(hospital_address$State=="PR")) {
  result <- geocode(paste(hospital_address$Address[i] , hospital_address$City[i],"Puerto Rico"), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

for (i in which(hospital_address$State=="NJ")) {
  result <- geocode(paste(hospital_address$Address[i] , hospital_address$City[i],"New Jersey USA"), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

for (i in which(hospital_address$State=="MA")) {
  result <- geocode(paste(hospital_address$Address[i] , hospital_address$City[i],"Massachusetts USA"), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

flaggedError <- c()
for(i in 1:nrow(hospital_address)) {
  if(hospital_address$lon[i] <= right && hospital_address$lon[i] >= left 
     && hospital_address$lat[i] <= top && hospital_address$lat[i] >= bottom) {
    flaggedError <- c(flaggedError)
  }else{
    flaggedError <- c(flaggedError,i)
  }
}


for (i in flaggedError) {
  result <- geocode(paste(hospital_address$City[i],hospital_address$State[i]), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

# left join original hospital_info
hospital <- hospital_info %>% left_join(hospital_address)

# Write a CSV file containing origAddress to the working directory
write.csv(hospital, "../data/hospital_2016.csv", row.names=FALSE)

