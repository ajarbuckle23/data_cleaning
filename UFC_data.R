
##### GENERAL ####
# Loading Packages 
library(pacman)
p_load(tidyr, dplyr)

# Getting data from desktop
metadata <- read.csv("/Users/ajarbuckle/Desktop/Data Grind/Datasets/UFC Data/ufc-fight-metadata.csv")

# Removing rows without data (they all look like headers)
metadata <- metadata %>% filter(EVENT_LOCATION != "EVENT_LOCATION")

# De-duping the table
metadata <- metadata[!duplicated(metadata), ]

# Creating new column that shows whether fight was a title fight 
empty_vector <- c()
for (x in 1:nrow(metadata)){
  if(grepl("title", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
    empty_vector[x] <- TRUE
  }
  else{
    empty_vector[x] <- FALSE
  }
}
metadata$title_fight <- empty_vector
rm(empty_vector)

# Creating new column that shows whether fight was women's or men's fight
empty_vector <- c()
for (x in 1:nrow(metadata)){
  if(grepl("women", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
    empty_vector[x] <- "Women"
  }
  else{
    empty_vector[x] <- "Men"
  }
}
metadata$sex <- empty_vector
rm(empty_vector)

#### SORTING OUT WEIGHT CLASS INFORMATION ####

# Remove fights that don't specify weightclass 
metadata <- subset(metadata, grepl("weight", metadata$FIGHT_WEIGHTCLASS, ignore.case = TRUE))

# Now do the more tedious checks
for (x in 1:nrow(metadata)){
    
    if(grepl("Open Weight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Open Weight"
    }
    
    else if(grepl("Catch Weight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Catch Weight"
    }
    
    else if(grepl("Super Heavyweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Super Heavyweight"
    }
    
    else if(grepl("Light Heavyweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Light Heavyweight"
    }
    
    else if(grepl("Heavyweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Heavyweight"
    }
    
    else if(grepl("Middleweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Middleweight"
    }
    
    else if(grepl("Welterweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Welterweight"
    }
    
    else if(grepl("Lightweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Lightweight"
    }
    
    else if(grepl("Featherweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Featherweight"
    }
    
    else if(grepl("Bantamweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Bantamweight"
    }
    
    else if(grepl("Flyweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Flyweight"
    }
  
    else if(grepl("Strawweight", metadata[[x, "FIGHT_WEIGHTCLASS"]], ignore.case = TRUE)){
      metadata[[x, "FIGHT_WEIGHTCLASS"]] <- "Strawweight"
    }
  
}

#### SORTING OUT GEOGRAPHIC INFORMATION ####

# Separating location column into its distinct parts 
metadata <- metadata %>% separate(EVENT_LOCATION, c("City", "Region", "Country"), ", ")

# If region is the same as the city, then I change the region to NA
for (x in 1:nrow(metadata)){
  if(metadata[[x, "Region"]] == metadata[[x, "City"]]){
    metadata[[x, "Region"]] <- NA
  }
}

# Moving data in region column to country column if only two location items are provided
for (x in 1:nrow(metadata)){
  if(is.na(metadata[[x, "Country"]])){
    metadata[[x, "Country"]] <- metadata[[x, "Region"]]
    metadata[[x, "Region"]] <- NA
  }
}

#### EXPORTING DATAFRAMES AS CSV's ####
write.csv(metadata,"/Users/ajarbuckle/Desktop/ufc_metadata_clean.csv", row.names = TRUE)


