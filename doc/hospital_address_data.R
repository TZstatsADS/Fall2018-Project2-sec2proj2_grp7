library(ggmap)
library(dplyr)


hospital_info <- read.csv("../data/hospital_info.csv")

hospital_address <- hospital_info %>% 
  select(Hospital.Name,Provider.Id, Address, City, State, ZIP.Code) %>% 
  distinct()

hospital_address <- hospital_address %>% mutate(Full=paste(Address,City,State,sep=", "))

for(i in 1:nrow(hospital_address))
{
  result <- geocode(hospital_address$Full[i], output = "latlon", source = "dsk",messaging = FALSE)
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

for (i in which(is.na(hospital_address$lon))) {
  result <- geocode(paste(hospital_address$City[i],hospital_address$State[i],hospital_address$ZIP.Code[i],sep = ","), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}
hospital_address$State[is.na(hospital_address$lon)]

for (i in which(hospital_address$State=="PR")) {
  result <- geocode(paste(hospital_address$Address[i] , hospital_address$City[i],"Puerto Rico"), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}

which(is.na(hospital_address$lon))

for (i in which(is.na(hospital_address$lon))) {
  result <- geocode(paste(hospital_address$City[i],"Puerto Rico"), output = "latlon", source = "dsk")
  hospital_address$lon[i] <- as.numeric(result[1])
  hospital_address$lat[i] <- as.numeric(result[2])
}
hospital_address[4805,c("lon","lat")] <- geocode(paste(hospital_address$Adress[4805],"GUAM"), output = "latlon", source = "dsk")
hospital_address[4365,c("lon","lat")] <- geocode(paste(hospital_address$City[4365],"Puerto Rico"), output = "latlon", source = "dsk")
hospital_address[4354,c("lon","lat")] <- geocode(paste(hospital_address$City[4354],"Puerto Rico"), output = "latlon", source = "dsk")

# Write a CSV file containing origAddress to the working directory
write.csv(hospital_address, "../data/hospital_address.csv", row.names=FALSE)
