
# load library
library(rredlist)
library(tidyverse)

## HABITAT BY CATEGORY ----

### EX ----

# set r key
rlkey <- "4920c1351e60ef9727d5082509b56440fdaa063580ce403400da004458aae18e"

# choose category
cat <- "EX"

# query database
download <- rl_sp_category(category = cat, key =rlkey)


# extract vector of species
SpeciesList <- download[["result"]]$scientific_name

# need to split into sets of 50
SL <- split(SpeciesList, ceiling(seq_along(SpeciesList)/50))

# create empty data frame for results
habitat <- data.frame(species = character(),
                      code = character(),
                      habitat = character())


for(i in 1:length(SL)){
  
  set <- (SL[[i]])
  
  for(sp in set){
    
    # download habitat information for species
    download <- rl_habitats(name = sp, key = rlkey)
    
    results <- download[["result"]]
    
    # keep if dataframe
    if(is.data.frame(results) == TRUE){
      
      results <- results %>%
        mutate(species = download$name) %>%
        select(species, code, habitat)
      
      habitat <- rbind(habitat, results)
      
      Sys.sleep(2)
      
      cat(sp,"... done\n")
    }
    
    # move on if not dataframe
    else {
      
      cat(sp, "... not in database\n")
      
    }
  }
  
  cat("set", i, "complete\n")
  
  # pause 10 seconds until next set
  Sys.sleep(300)
  
}

# write to file
write_delim(habitat, "data/habitat_ex.txt", delim = "\t")


### CR ----

# set r key
rlkey <- "4920c1351e60ef9727d5082509b56440fdaa063580ce403400da004458aae18e"

# choose category
cat <- "CR"

# query database
download <- rl_sp_category(category = cat, key =rlkey)

# extract vector of species
SpeciesList <- download[["result"]]$scientific_name

# sample 1000 randomly
tmp <- sample(SpeciesList, 1000, replace = FALSE)

# need to split into sets of 50
SL <- split(tmp, ceiling(seq_along(tmp)/50))

# create empty data frame for results
habitat <- data.frame(species = character(),
                      code = character(),
                      habitat = character())


for(i in 13:length(SL)){
  
  set <- (SL[[i]])
  
  for(sp in set){
    
    # download habitat information for species
    download <- rl_habitats(name = sp, key = rlkey)
    
    results <- download[["result"]]
    
    # keep if dataframe
    if(is.data.frame(results) == TRUE){
      
      results <- results %>%
        mutate(species = download$name) %>%
        select(species, code, habitat)
      
      habitat <- rbind(habitat, results)
      
      Sys.sleep(3)
      
      cat(sp,"... done\n")
    }
    
    # move on if not dataframe
    else {
      
      cat(sp, "... not in database\n")
      
    }
  }
  
  cat("set", i, "complete\n")
  
  # pause 10 seconds until next set
  Sys.sleep(300)
  
}

# write to file
write_delim(habitat, "data/habitat_cr.txt", delim = "\t")


### EN ----

# set r key
rlkey <- "4920c1351e60ef9727d5082509b56440fdaa063580ce403400da004458aae18e"

# choose category
cat <- "EN"

# query database
download <- rl_sp_category(category = cat, key = rlkey)


# extract vector of species
SpeciesList <- download[["result"]]$scientific_name


# sample 1000 randomly
tmp <- sample(SpeciesList, 1000, replace = FALSE)

# need to split into sets of 50
SL <- split(tmp, ceiling(seq_along(tmp)/50))

# create empty data frame for results
habitat <- data.frame(species = character(),
                      code = character(),
                      habitat = character())


for(i in 17:length(SL)){
  
  set <- (SL[[i]])
  
  for(sp in set){
    
    # download habitat information for species
    download <- rl_habitats(name = sp, key = rlkey)
    
    results <- download[["result"]]
    
    # keep if dataframe
    if(is.data.frame(results) == TRUE){
      
      results <- results %>%
        mutate(species = download$name) %>%
        select(species, code, habitat)
      
      habitat <- rbind(habitat, results)
      
      Sys.sleep(2)
      
      cat(sp,"... done\n")
    }
    
    # move on if not dataframe
    else {
      
      cat(sp, "... not in database\n")
      
    }
  }
  
  cat("set", i, "complete\n")
  
  # pause 10 seconds until next set
  Sys.sleep(300)
  
}

# write to file
write_delim(habitat, "data/habitat_en.txt", delim = "\t")


### VU ----

# set r key
rlkey <- "4920c1351e60ef9727d5082509b56440fdaa063580ce403400da004458aae18e"

# choose category
cat <- "VU"

# query database
download <- rl_sp_category(category = cat, key =rlkey)

# extract vector of species
SpeciesList <- download[["result"]]$scientific_name

# sample 1000 randomly
tmp <- sample(SpeciesList, 1000, replace = FALSE)

# need to split into sets of 50
SL <- split(tmp, ceiling(seq_along(tmp)/50))

# create empty data frame for results
habitat <- data.frame(species = character(),
                      code = character(),
                      habitat = character())


for(i in 16:length(SL)){
  
  set <- (SL[[i]])
  
  for(sp in set){
    
    # download habitat information for species
    download <- rl_habitats(name = sp, key = rlkey)
    
    results <- download[["result"]]
    
    # keep if dataframe
    if(is.data.frame(results) == TRUE){
      
      results <- results %>%
        mutate(species = download$name) %>%
        select(species, code, habitat)
      
      habitat <- rbind(habitat, results)
      
      Sys.sleep(2)
      
      cat(sp,"... done\n")
    }
    
    # move on if not dataframe
    else {
      
      cat(sp, "... not in database\n")
      
    }
  }
  
  cat("set", i, "complete\n")
  
  # pause 10 seconds until next set
  Sys.sleep(300)
}

# write to file
write_delim(habitat, "data/habitat_vu.txt", delim = "\t")


## THREAT BY COUNTRY ----

### UNITED STATES ----


# choose a country
country <- "US"

# query database - keep only threatened species
download <- rl_sp_country(country = country, key = rlkey)$result %>%
  filter(category %in% c("VU", "EN", "CR"))

# extract vector of species
SpeciesList <- download$scientific_name

# sample 1000 randomly
tmp <- sample(SpeciesList, 500, replace = FALSE)

# need to split into sets of 50
SL <- split(tmp, ceiling(seq_along(tmp)/50))

# create empty data frame for results
threat <- data.frame(code = character(),
                     title = character(),
                     scope = character(),
                     severity = character(),
                     score = character(),
                     invasive = character())


for(i in 1:length(SL)){
  
  set <- (SL[[i]])
  
  for(sp in set){
    
    # download threat information for species
    results <- rl_threats(name = sp, key = rlkey)$result
    
    # keep if dataframe
    if(is.data.frame(results) == TRUE){
      
      results <- results %>%
        mutate(species = download$name)
      
      threat <- rbind(threat, results)
      
      Sys.sleep(2)
      
      cat(sp,"... done\n")
    }
    
    # move on if not dataframe
    else {
      
      cat(sp, "... not in database\n")
      
    }
  }
  
  cat("set", i, "complete\n")
  
  # pause 10 seconds until next set
  Sys.sleep(300)
  
}

# write to file
write_delim(threat, "data/threats_us.txt", delim = "\t")


### MADAGASCAR ----


# choose a country
country <- "MG"

# query database - keep only threatened species
download <- rl_sp_country(country = country, key = rlkey)$result %>%
  filter(category %in% c("VU", "EN", "CR"))

# extract vector of species
SpeciesList <- download$scientific_name

# sample 1000 randomly
tmp <- sample(SpeciesList, 500, replace = FALSE)

# need to split into sets of 50
SL <- split(tmp, ceiling(seq_along(tmp)/50))

# create empty data frame for results
threat <- data.frame(code = character(),
                     title = character(),
                     scope = character(),
                     severity = character(),
                     score = character(),
                     invasive = character())


for(i in 10:length(SL)){
  
  set <- (SL[[i]])
  
  for(sp in set){
    
    # download threat information for species
    results <- rl_threats(name = sp, key = rlkey)$result
    
    # keep if dataframe
    if(is.data.frame(results) == TRUE){
      
      results <- results %>%
        mutate(species = download$name)
      
      threat <- rbind(threat, results)
      
      Sys.sleep(2)
      
      cat(sp,"... done\n")
    }
    
    # move on if not dataframe
    else {
      
      cat(sp, "... not in database\n")
      
    }
  }
  
  cat("set", i, "complete\n")
  
  # pause 10 seconds until next set
  Sys.sleep(300)
  
}

# write to file
write_delim(threat, "data/threats_mg.txt", delim = "\t")
