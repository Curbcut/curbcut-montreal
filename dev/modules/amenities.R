#### Access to amenities data setup ############################################


# Load libraries and get data ---------------------------------------------

library(tidyverse)
library(sf)
library(qs)
library(r5r)

# # Get shapefiles from open data portals
# dl_unzip <- function(shp_url) {
#   download.file(shp_url, destfile = paste0("dev/data/amenity_access/", "temp",
#                                            ".zip"))
#   
#   unzip(paste0("dev/data/amenity_access/", "temp", ".zip"),
#         exdir = "dev/data/amenity_access/")
#   
#   unlink(paste0("dev/data/amenity_access/", "temp", ".zip"), recursive = TRUE)
# }
# 
# # DL Espace_Vert.shp
# dl_unzip(paste0("https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d",
#                 "79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/downlo",
#                 "ad/shapefile.zip"))
# 
# # DL Enseignement scolaire
# dl_unzip(paste0("https://www.donneesquebec.ca/recherche/dataset/2d3b5cf8-b347-",
#                 "49c7-ad3b-bd6a9c15e443/resource/2ae11c05-03b2-4006-bdb2-a49a4",
#                 "fa41c23/download/etablissements-meq-mes-esrishp.zip"))
# 
# rm(dl_unzip)

# #In this example I am using the supermarkets of Montreal
# qread("dev/data/amenity_access/ttm_bike.qs") -> ttm_bike
# qread("dev/data/amenity_access/ttm_car.qs") -> ttm_car
# qread("dev/data/amenity_access/ttm_transit.qs") -> ttm_transit
# qread("dev/data/amenity_access/ttm_walk.qs") -> ttm_walk

parks <- 
  read_sf("dev/data/amenity_access/Espace_Vert.shp") |> 
  st_transform(4326) |> 
  filter(TYPO1 %in% c("Parc d'arrondissement", "En cours de validation",
                      "Grand Parc", "Autre espace vert")) |> 
  select(ID = OBJECTID)

supermarkets <- 
  read.csv("dev/data/amenity_access/dmti3.csv") 
#Cleaned before. The cleaning code is in the next section

public_schools <- 
  read_sf("dev/data/amenity_access/PPS_Public_Ecole.shp") 

private_schools <- 
  read_sf("dev/data/amenity_access/PPS_Prive_Etablissement.shp") 

dmti <- 
  read.csv("dev/data/amenity_access/dmti_2020.csv")



# Check data --------------------------------------------------------------
#Clean Schools and add new variables for each level of education and language
myvars<-c("ORDRE_ENS", "COORD_X_LL", "COORD_Y_LL", "OBJECTID")
private <- private[myvars]
myvars<-c("ORDRE_ENS", "COORD_X_LL", "COORD_Y_LL", "OBJECTID", "TYPE_CS")
public <- public[myvars]

public$Kindergarden_Pub[public$ORDRE_ENS== "PrÃ©scolaire"] <- 1
public$Kindergarden_Pub[public$ORDRE_ENS== "PrÃ©scolaire - Primaire"] <- 1
public$Kindergarden_Pub[public$ORDRE_ENS== "PrÃ©scolaire - Primaire - Secondaire"] <- 1
public$Kindergarden_Pub[public$ORDRE_ENS== " - Secondaire"] <- 1
public$Primary_Pub[public$ORDRE_ENS== "Primaire"] <- 1
public$Primary_Pub[public$ORDRE_ENS== "PrÃ©scolaire - Primaire"] <- 1
public$Primary_Pub[public$ORDRE_ENS== " - Primaire - Secondaire"] <- 1
public$Primary_Pub[public$ORDRE_ENS== "Primaire - Secondaire"] <- 1
public$Secondary_Pub[public$ORDRE_ENS== "Secondaire"] <- 1
public$Secondary_Pub[public$ORDRE_ENS== "PrÃ©scolaire - Secondaire"] <- 1
public$Secondary_Pub[public$ORDRE_ENS== "PrÃ©scolaire - Primaire - Secondaire"] <- 1
public$Secondary_Pub[public$ORDRE_ENS== "Primaire - Secondaire"] <- 1
public$Secondary_Pub[public$ORDRE_ENS== "Secondaire - Ã‰ducation aux adultes"] <- 1
public$Adults_Professional_pub[public$ORDRE_ENS== "Secondaire - Ã‰ducation aux adultes"] <- 1
public$Adults_Professional_pub[public$ORDRE_ENS== "Formation professionnelle"] <- 1
public$Adults_Professional_pub[public$ORDRE_ENS== "Ã‰ducation aux adultes"] <- 1
public$French[public$TYPE_CS== "Franco"] <- 1
public$English[public$TYPE_CS== "Anglo"] <- 1

public$Kinder_French_Pub[public$Kindergarden_Pub == 1 & public$French == 1] <- 1
public$Primary_French_Pub[public$Primary_Pub == 1 & public$French == 1] <- 1
public$Secondary_French_Pub[public$Secondary_Pub == 1 & public$French == 1] <- 1
public$Adults_Professional_French_Pub[public$Adults_Professional_pub == 1 & public$French == 1] <- 1
public$Kinder_English_Pub[public$Kindergarden_Pub == 1 & public$English == 1] <- 1
public$Primary_English_Pub[public$Primary_Pub == 1 & public$English == 1] <- 1
public$Secondary_English_Pub[public$Secondary_Pub == 1 & public$English == 1] <- 1
public$Adults_Professional_English_Pub[public$Adults_Professional_pub == 1 & public$English == 1] <- 1



private$Kindergarden_Priv[private$ORDRE_ENS== "PrÃ©scolaire"] <- 1
private$Kindergarden_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Primaire"] <- 1
private$Kindergarden_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Primaire - Secondaire"] <- 1
private$Kindergarden_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Secondaire"] <- 1
private$Primary_Priv[private$ORDRE_ENS== "Primaire"] <- 1
private$Primary_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Primaire"] <- 1
private$Primary_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Primaire - Secondaire"] <- 1
private$Primary_Priv[private$ORDRE_ENS== "Primaire - Secondaire"] <- 1
private$Secondary_Priv[private$ORDRE_ENS== "Secondaire"] <- 1
private$Secondary_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Secondaire"] <- 1
private$Secondary_Priv[private$ORDRE_ENS== "PrÃ©scolaire - Primaire - Secondaire"] <- 1
private$Secondary_Priv[private$ORDRE_ENS== "Primaire - Secondaire"] <- 1
private$Secondary_Priv[private$ORDRE_ENS== "Secondaire - Ã‰ducation aux adultes"] <- 1
private$Adults_Professional_Priv[private$ORDRE_ENS== "Secondaire - Ã‰ducation aux adultes"] <- 1
private$Adults_Professional_Priv[private$ORDRE_ENS== "Formation professionnelle"] <- 1
private$Adults_Professional_Priv[private$ORDRE_ENS== "Ã‰ducation aux adultes"] <- 1


public[is.na(public)] <- 0
private[is.na(private)] <- 0

#Set as sf
public2<-public%>%
  filter(COORD_Y_LL>30)%>%
  st_as_sf(coords=c("COORD_X_LL", "COORD_Y_LL"), crs=4326)
private2<-private%>%
  st_as_sf(coords=c("COORD_X_LL", "COORD_Y_LL"), crs=4326)



#Clean parks. Keep variables
myvars <- c("full_id", "lat", "lon")
parks <- parks[myvars]

parks2<-parks%>%
  st_as_sf(coords=c("lon", "lat"), crs=4326)



#Clean daycare from dmti. Daycare code =8351

daycare<- dmti%>%
  filter(SIC1== 8351)
myvars <- c("POI_ID", "LONGITUDE", "LATITUDE")
daycare2 <-daycare[myvars]

daycare2<-daycare2%>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)



#Supermarkets. here is the cleaning for the supermarkets. It was done by looking some words. Those are the ones that sell nutritious food with a lot of variety.
dmti1<-dmti%>%
  mutate(WALMART = grepl("WALMART", NAME))%>%
  mutate(costco = grepl("costco", NAME))%>%
  filter(SIC1== 5411 | SIC1==5421 | SIC1==5431 | 
           SIC2== 5411 | SIC2==5421 | SIC2==5431 | 
           SIC3== 5411 | SIC3==5421 | SIC3==5431 | 
           SIC4== 5411 | SIC4==5421 | SIC4==5431 |
           SIC5== 5411 | SIC5==5421 | SIC5==5431  )%>%
  mutate(depanneur = grepl("NNEUR", NAME))%>%
  mutate(depanneur2 = grepl("DEPAN", NAME))%>%
  mutate(depanneur3= grepl("D?ëPAN", NAME))%>%
  mutate(IGA = grepl("IGA", NAME))%>%
  mutate(IGA2 = grepl("I G A",NAME ))%>%
  mutate(IGA3 = grepl("SOBEY'S INC",NAME ))%>%
  mutate(PROVIGO = grepl("PROVIGO", NAME))%>%
  mutate(METRO = grepl("METRO", NAME))%>%
  mutate(METRO2 = grepl("M?ëTRO", NAME))%>%
  mutate(MARCHE = grepl("MARCH", NAME))%>%
  mutate(SUPERMARCHE = grepl("PERMARCH", NAME))%>%
  mutate(SUPERMARCHE2 = grepl("PER MARCH", NAME))%>%
  mutate(couchtard = grepl("CHE-TARD", NAME))%>%
  mutate(couchtard2 = grepl("CHE TARD", NAME))%>%
  mutate(dollarama = grepl("DOLLARAMA", NAME))%>%
  mutate(MAXI = grepl("MAXI", NAME))%>%
  mutate(BONI = grepl("BONI", NAME))%>%  #type of depanneur
  mutate(CERTIGARD = grepl("CERTIGARD", NAME))%>%  #type of depanneur in gas stations
  mutate(FRUITERIE = grepl("FRUIT", NAME))%>%
  mutate(EPICERIE = grepl ("EPICERIE", NAME))%>%
  mutate(FROMAGERIE = grepl ("FROMAGERIE", NAME))%>%
  mutate(FROMAGERIE2 = grepl ("FROMAGEs", NAME))%>%
  mutate(LOBLAWS = grepl("LOBLAWS", NAME))%>%   #Supermarket chain
  mutate(RESTAURANT = grepl("RESTAURANT", NAME))%>%  #Some of them sell food?
  mutate(SUPERC = grepl("SUPER C", NAME))%>%   #Supermarket chain
  mutate(TABAGIE = grepl("TABAGIE", NAME))%>%  #Sell tobacco
  mutate(WILLIAMJWALTER = grepl("WILLIAM J WALTER", NAME))%>%  #SAUCISSIER
  mutate(BOUCHERIE = grepl("BOUCHEr", NAME))%>%
  mutate(POISSONNERIE =grepl("POISSONNERIE", NAME))%>%
  mutate(depanneur4 =grepl("CHEZ GABY KIM", NAME))%>%
  mutate(Final = grepl ("TRADITION DESCH?èNES", NAME))%>%
  mutate(Final2 = grepl ("TRADITION SUPERMARCH?ë", NAME))


dmti1 <- mutate(dmti1, supermarket= if_else (WALMART== TRUE | IGA== TRUE | IGA2 == TRUE | IGA3==TRUE | PROVIGO==TRUE | METRO==TRUE | METRO2== TRUE |
                                               SUPERMARCHE == TRUE | SUPERMARCHE2 == TRUE | MAXI == TRUE | BONI == TRUE | LOBLAWS == TRUE  |
                                               SUPERC == TRUE | Final == TRUE | Final2== TRUE 
                                             , 1,0))
dmti<-dmti1 %>% 
  filter(supermarket==1)

myvars <- c("POI_ID", "LONGITUDE", "LATITUDE")
supermarkets <- dmti1[myvars]

sm<-supermarkets%>%
  st_as_sf(coords=c("long", "lat"), crs=4326)



# Joins -------------------------------------------------------------------

head(MTL_DA16)
#I cleaned this before. In this case I removed all the variables

#MTL_DA16$Population <- NULL
#MTL_DA16$Households <- NULL
#MTL_DA16$Type <- NULL
#MTL_DA16$CD_UID <- NULL
#MTL_DA16$`Shape Area` <- NULL
#MTL_DA16$Dwellings <- NULL
#MTL_DA16$CSD_UID <- NULL
#MTL_DA16$CMA_UID <- NULL
#MTL_DA16$CT_UID <-NULL
#Joins

#Spatial join using st_join. We want to join the MTL_DA16's columns to the points of interest file
st_join(sm, MTL_DA16) -> sm2
sm2$n <-1
st_join(daycare2, MTL_DA16) -> daycare2
daycare2$n <-1
st_join(parks2, MTL_DA16) ->parks2
st_join(public2, MTL_DA16) -> public2
st_join(private2, MTL_DA16) -> private2

rm(sm, parks, public, private, dmti)
# Accessibility -----------------------------------------------------------
#From here we are going to repeat the process for each mode of transportation

#Originally I ran the ttm in a different project. In this part we should be able to run the ttm again. 
#However, in this case I will only read the ones I had. I will keep the parameters/

#path <- ('~/Housing2') 

#r5r_core <- setup_r5(data_path = path, elevation = "TOBLER",  verbose = FALSE) 

#read.csv("da_centroids_popw2.csv")-> da
#mode <- c("BICYCLE") #Change mode of interest

#max_walk_dist <-  Inf 
#max_trip_duration <- 60
#departure_datetime <- as.POSIXct("19-05-2022 08:00:00",
                                # format = "%d-%m-%Y %H:%M:%S")
#time_window <-60
#draws_per_minute <- 1L
#max_walk_time <- 60
#max_bike_time <- 60
#max_bike_dist <- 60
#max_car_time = 60
#ttm_bike <- travel_time_matrix(r5r_core = r5r_core,
                                #origins = da,
                                #destinations = da,
                                #mode = mode,
                                #departure_datetime = departure_datetime,
                                #max_trip_duration = max_trip_duration,
                                #max_walk_time = max_walk_time,
                                #time_window = time_window, #Only activate this for transit
                                
                                #verbose = FALSE)

#qsave(ttm_bike, "ttm_bike.qs")


#Lets clean the ttm. 15 minutes threshold
da15<- ttm_car%>%  #From here, we do this process for all the transportation modes. 
  filter(travel_time_p50<=15)

da15a <- da15 %>%
  group_by(from_id, to_id)%>%
  summarise(avg = mean(travel_time_p50))




#Rename the GeoUID column to to_id to join with the ttm,and delete geometries. 
names(sm2)[names(sm2) == 'GeoUID'] <- 'to_id'
sm2$geometry<- NULL
names(parks2)[names(parks2) == 'GeoUID'] <- 'to_id'
parks2$geometry<- NULL
names(private2)[names(private2) == 'GeoUID'] <- 'to_id'
private2$geometry<- NULL
names(public2)[names(public2) == 'GeoUID'] <- 'to_id'
public2$geometry<- NULL
names(daycare2)[names(daycare2) == 'GeoUID'] <- 'to_id'
daycare2$geometry <- NULL



#Coun the number of opportunities in each DA
sm2<-sm2%>%
  count(to_id)
parks2 <- parks2 %>%
  count(to_id)
daycare2 <- daycare2%>%
  count(to_id)
public2 <- public2%>%
  group_by(to_id)%>%
  summarise(Kinder_Pub = sum(Kindergarden_Pub),
            Primary_Pub = sum(Primary_Pub),
            Secondary_Pub= sum(Secondary_Pub),
            Adults_Prof_pub = sum(Adults_Professional_pub),
            French_pub = sum(French),
            English_pub = sum(English),
            Kinder_French_Pub = sum(Kinder_French_Pub),
            Primary_French_Pub = sum(Primary_French_Pub),
            Secondary_French_Pub = sum(Secondary_French_Pub),
            Adults_Professional_French_Pub = sum(Adults_Professional_French_Pub),
            Kinder_English_Pub = sum(Kinder_English_Pub),
            Primary_English_Pub = sum(Primary_English_Pub),
            Secondary_English_Pub = sum(Secondary_English_Pub),
            Adults_Professional_English_Pub = sum(Adults_Professional_English_Pub),
  )



private2 <- private2%>%
  group_by(to_id)%>%
  summarise(Kindergarden_Priv = sum(Kindergarden_Priv),
            Primary_Priv = sum(Primary_Priv),
            Secondary_Priv = sum(Secondary_Priv),
            Adults_Professional_Priv = sum(Adults_Professional_Priv),)


names(sm2)[names(sm2) == 'n'] <- 'Supermarkets'  
names(parks2)[names(parks2) == 'n'] <- 'Parks'   
names(daycare2)[names(daycare2) == 'n'] <- 'Daycares'


#Join facilities to time travel matrix
acc1 = merge(x=da15a,y=sm2,by="to_id",all=TRUE)
acc1 = merge(x=acc1,y=parks2,by="to_id",all=TRUE)
acc1 = merge(x=acc1,y=daycare2,by="to_id",all=TRUE)
acc1 = merge(x=acc1,y=public2,by="to_id",all=TRUE)
acc1 = merge(x=acc1,y=private2,by="to_id",all=TRUE)


acc1<-acc1%>%
  filter(!is.na(avg))

as.numeric(acc1$to_id)-> acc1$to_id
as.numeric(acc1$from_id)-> acc1$from_id

acc1<-acc1 %>%
  mutate_all(~replace_na(., 0))


#An now we sum the number of supermarkets within 15 minutes by every origin DA (from_id)
acc2 <- acc1%>% 
  group_by(from_id) %>% 
  summarise(Parks = sum(Parks),
            Daycare = sum(Daycares),
            Supermarkets = sum(Supermarkets),
            Kinder_Pub = sum(Kinder_Pub),
            Primary_Pub = sum(Primary_Pub),
            Secondary_Pub= sum(Primary_Pub),
            Adults_Prof_pub = sum(Adults_Prof_pub),
            Kinder_French_Pub = sum(Kinder_French_Pub),
            Primary_French_Pub = sum(Primary_French_Pub),
            Secondary_French_Pub = sum(Secondary_French_Pub),
            Adults_Professional_French_Pub = sum(Adults_Professional_French_Pub),
            Kinder_English_Pub = sum(Kinder_English_Pub),
            Primary_English_Pub = sum(Primary_English_Pub),
            Secondary_English_Pub = sum(Secondary_English_Pub),
            Adults_Professional_English_Pub = sum(Adults_Professional_English_Pub),
            French_pub = sum(French_pub),
            English_pub = sum(English_pub),
            Kindergarden_Priv = sum(Kindergarden_Priv),
            Primary_Priv = sum(Primary_Priv),
            Secondary_Priv = sum(Secondary_Priv),
            Adults_Professional_Priv = sum(Adults_Professional_Priv),
            Avg_time = mean(avg),
  )

head(acc2)
#Rename from_id to join with the MTL_DA16
names(acc2)[names(acc2) == 'from_id'] <- 'GeoUID'

Housing_car = merge(x=MTL_DA16,y=acc2,by="GeoUID",all=TRUE)

housing<-housing%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)

#Join with MTL_DA16
st_join(housing, MTL_DA16) -> housing2

housing2<-housing2%>%
  group_by(GeoUID)%>%
  summarise(Buildings = sum (nb_log),
            No_Buildings = n(),)

housing2$geometry<-NULL
Housing_car = merge(x=Housing_car,y=housing2,by="GeoUID",all=TRUE)
Housing_car$Buildings[is.na(Housing_car$Buildings)] <- 0
Housing_car$No_Buildings[is.na(Housing_car$No_Buildings)] <- 0


#And save file. 
qsave(Housing_car, "Housing_car.qs")


