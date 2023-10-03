#//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//
# Title: DATAZOOM AMAZONIA - FIND ERRORS
# Data: 2023/10/02
# Programmer: Carolina Moniz De Moura
#//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//

###########################################################################
# SET-UP ------------------------------------------------------------------
###########################################################################

#Changing working directory 
#---------------------------------------------------------TO CHANGE IF NEED
setwd("C:/Users/carolinamoura/Desktop/test_errors_datazoom_amazonia")
#-------------------------------------------------------------------------!

# Installing and loading packages (if not exist)
packages_to_install <- c("magrittr","datazoom.amazonia","devtools","future.apply","parallel","openxlsx","RCurl","terra","googledrive")
# Function to install and load a package if not already installed
install_and_load_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}
# Loop through the list of packages and install/load them
for (package in packages_to_install) {
  install_and_load_package(package)
}

# Install the "datazoompuc/datazoom.amazonia" package
devtools::install_github("datazoompuc/datazoom.amazonia")

# Key to allow access to google drive without human supervision
#---------------------------------------------------------TO CHANGE IF NEED
drive_auth(path = "key_googledrive.json", scopes = "https://www.googleapis.com/auth/drive")
#-------------------------------------------------------------------------!

# Get the number of CPU cores
#num_cores <- detectCores()
# Declare what backend process I will be using.
#Here, we use plan(multisession) with the total number of cores less 2
#---------------------------------------------------------TO CHANGE IF NEED
plan(multisession, workers= detectCores() - 2) 
#-------------------------------------------------------------------------!

###########################################################################
# FUNCTIONS TO TEST -------------------------------------------------------
###########################################################################


# PRODES ------------------------------------------------------------------

# Wrapper function for load_prodes_function_1
load_prodes_wrapper_1 <- function() {
  tryCatch({
    load_prodes(raw_data = TRUE,
                time_period = 2015,
                language = 'pt')
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_prodes_wrapper_rawfalse <- function() {
  tryCatch({
    load_prodes(raw_data = FALSE,
                time_period = 2015,
                language = 'pt')
    return("Success")
  }, error = function(e) {
    return(paste("Error in load_prodes_function_1:", e$message))
  })
}



# DETER  ------------------------------------------------------------------
load_deter_wrapper_amz <- function() {
  tryCatch({
    
    load_deter(dataset='deter_amz',
               raw_data=TRUE)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_deter_wrapper_cerrado <- function() {
  tryCatch({
    
    load_deter(dataset='deter_cerrado',
               raw_data=TRUE)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_deter_wrapper_amz_rawfalse <- function() {
  tryCatch({
    
    load_deter(dataset='deter_amz',
               raw_data=FALSE)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


# DEGRAD  ------------------------------------------------------------------

load_degrad_wrapper_1 <- function() {
  tryCatch({
    
    load_degrad(dataset= 'degrad',
                raw_data=TRUE,
                time_period=2010:2012)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_degrad_wrapper_rawfalse <- function() {
  tryCatch({
    
    load_degrad(dataset= 'degrad',
                raw_data=FALSE,
                time_period=2010:2012)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}



# Imazon ------------------------------------------------------------------

load_imazon_wrapper_1 <- function() {
  tryCatch({
    
    load_imazon(raw_data = TRUE)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_imazon_wrapper_rawfalse<- function() {
  tryCatch({
    
    load_imazon(raw_data = FALSE)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}



# IBAMA -------------------------------------------------------------------

load_ibama_wrapper_1 <- function() {
  tryCatch({
    load_ibama(dataset = "embargoed_areas", raw_data = TRUE, 
               language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_rawfalse <- function() {
  tryCatch({
    load_ibama(dataset = "embargoed_areas", raw_data = FALSE, 
               language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_states <- function() {
  tryCatch({
    load_ibama(dataset = "embarged_area", raw_data = FALSE,
               states = c("AC", "AM"), language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_ibama_wrapper_fines <- function() {
  tryCatch({
    
    load_ibama(dataset = "distributed_fines", raw_data = TRUE, 
               language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_fines_rawfalse <- function() {
  tryCatch({
    
    load_ibama(dataset = "distributed_fines", raw_data = FALSE, 
               language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_fines_rawfalse_states <- function() {
  tryCatch({
    
    load_ibama(dataset = "distributed_fines", raw_data = FALSE,
               states =c("AC", "AM"), language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_collected_fines <- function() {
  tryCatch({
    
    load_ibama(dataset = "collected_fines", raw_data = TRUE, 
               language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_collected_fines_rawfalse <- function() {
  tryCatch({
    
    load_ibama(dataset = "collected_fines", raw_data = FALSE, 
               language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_ibama_wrapper_collected_fines_rawfalse_states <- function() {
  tryCatch({
    
    load_ibama(dataset = "collected_fines", raw_data = FALSE,
               states = c("AC", "AM"), language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}



# MAPBIOMAS ---------------------------------------------------------------

load_mapbiomas_wrapper_cover0_state<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = TRUE,
               geo_level = "state",
               language = "pt",
               cover_level = 0)
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_mapbiomas_wrapper_cover0_municipality_rawfalse<- function() {
      tryCatch({
        
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "municipality",
               language = "pt",
               cover_level = 0)
        return("Success")
      }, error = function(e) {
        return(paste("Error :", e$message))
      })
}

load_mapbiomas_wrapper_cover0_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt",
               cover_level = 0)
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_mapbiomas_wrapper_cover1_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt",
               cover_level = 1)
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}

load_mapbiomas_wrapper_cover2_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt",
               cover_level = 2)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_cover3_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt",
               cover_level = 3)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_cover4_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt",
               cover_level = 4)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_covernone_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_cover",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt",
               cover_level = "none")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_transition_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_transition", raw_data = FALSE,
               geo_level = "state", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_transition_state<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_transition", raw_data = TRUE,
               geo_level = "state", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_mapbiomas_wrapper_transition_municipality<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_transition", raw_data = TRUE,
               geo_level = "municipality", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_mapbiomas_wrapper_regeneration_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_deforestation_regeneration", raw_data = FALSE,
               geo_level = "state", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_regeneration_state<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_deforestation_regeneration", raw_data = TRUE,
               geo_level = "state", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_regeneration_municipality<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_deforestation_regeneration", raw_data = TRUE,
               geo_level = "municipality", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_mapbiomas_wrapper_irrigation_state<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_irrigation", raw_data = TRUE,
               geo_level = "state", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_irrigation_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_irrigation", raw_data = FALSE,
               geo_level = "state", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_irrigation_biome_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_irrigation", raw_data = FALSE,
               geo_level = "biome", language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_grazing_rawfalse<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_grazing_quality", raw_data = FALSE, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_grazing<- function() {
  tryCatch({
load_mapbiomas(dataset = "mapbiomas_grazing_quality", raw_data = TRUE, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_mapbiomas_wrapper_mining_country<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_mining",
               raw_data = TRUE,
               geo_level = "country",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_mining_country_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_mining",
               raw_data = FALSE,
               geo_level = "country",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_mining_indigenous_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_mining",
               raw_data = FALSE,
               geo_level = "indigenous_land",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_mining_municipality_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_mining",
               raw_data = FALSE,
               geo_level = "municipality",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_mining_biome_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_mining",
               raw_data = FALSE,
               geo_level = "biome",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_mining_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_mining",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_mapbiomas_wrapper_water_state<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_water",
               raw_data = TRUE,
               geo_level = "state",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_water_biome_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_water",
               raw_data = FALSE,
               geo_level = "biome",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_water_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_water",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_water_municipality_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_water",
               raw_data = FALSE,
               geo_level = "municipality",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_mapbiomas_wrapper_fire_state<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_fire",
               raw_data = TRUE,
               geo_level = "state",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_fire_state_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_fire",
               raw_data = FALSE,
               geo_level = "state",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_mapbiomas_wrapper_fire_municipality_rawfalse<- function() {
  tryCatch({
load_mapbiomas("mapbiomas_fire",
               raw_data = FALSE,
               geo_level = "municipality",
               language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


# CIPO --------------------------------------------------------------------
# 
# load_cipo_wrapper_brazilian_actors_rawfalse<- function() {
#   tryCatch({
# load_cipo(dataset = "brazilian_actors",
#           raw_data = FALSE,
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_brazilian_actors<- function() {
#   tryCatch({
# load_cipo(dataset = "brazilian_actors",
#           raw_data = TRUE,
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_brazilian_actors_ibama<- function() {
#   tryCatch({
# load_cipo(dataset = "brazilian_actors",
#           raw_data = TRUE,
#           search = "ibama",
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_brazilian_actors_ibama_funai<- function() {
#   tryCatch({
# load_cipo(dataset = "brazilian_actors",
#           raw_data = TRUE,
#           search = "ibama|funai",
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# 
# load_cipo_wrapper_international_coop_rawfalse<- function() {
#   tryCatch({
# load_cipo(dataset = "international_cooperation",
#           raw_data = FALSE,
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_international_coop<- function() {
#   tryCatch({
# load_cipo(dataset = "international_cooperation",
#           raw_data = TRUE,
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_international_coop_ibama<- function() {
#   tryCatch({
# load_cipo(dataset = "international_cooperation",
#           raw_data = TRUE,
#           search = "ibama",    
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_international_coop_ibama_funai<- function() {
#   tryCatch({
# load_cipo(dataset = "international_cooperation",
#           raw_data = TRUE,
#           search = "ibama|funai",
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# 
# load_cipo_wrapper_forest_governance_rawfalse<- function() {
#   tryCatch({
# load_cipo(dataset = "forest_governance",
#           raw_data = FALSE,
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_forest_governance<- function() {
#   tryCatch({
# load_cipo(dataset = "forest_governance",
#           raw_data = TRUE,
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_forest_governance_ibama<- function() {
#   tryCatch({
# load_cipo(dataset ="forest_governance",
#           raw_data = TRUE,
#           search = "ibama",
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 
# 
# load_cipo_wrapper_forest_governance_ibama_funai<- function() {
#   tryCatch({
# load_cipo(dataset = "forest_governance",
#           raw_data = TRUE,
#           search = "ibama|funai",
#           language = "pt")
# return("Success")
# }, error = function(e) {
#   return(paste("Error :", e$message))
# })
# }
# 


# TerraClimate  ------------------------------------------------------------------

load_climate_wrapper_maxtemperature <- function() {
  tryCatch({
    
    load_climate(dataset = "max_temperature",
                 raw_data = TRUE, 
                 time_period = 2000,
                 language = "pt")
    
    
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}



load_climate_wrapper_mintemperature <- function() {
  tryCatch({
    load_climate(dataset = "min_temperature",
                 raw_data = TRUE, 
                 time_period = 2000,
                 language = "pt")
    return("Success")
  }, error = function(e) {
    return(paste("Error :", e$message))
  })
}


load_climate_wrapper_windspeed <- function() {
  tryCatch({
load_climate(dataset = "wind_speed",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_climate_wrapper_vaporpressure <- function() {
  tryCatch({
load_climate(dataset = "vapor_pressure",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_climate_wrapper_vaporpressuredeficit <- function() {
  tryCatch({
load_climate(dataset = "vapor_pressure_deficit",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_climate_wrapper_snowwaterequivalent <- function() {
  tryCatch({
load_climate(dataset = "snow_water_equivalent",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_shortwaveradiation <- function() {
  tryCatch({
load_climate(dataset = "shortwave_radiation_flux",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_soilmoisture <- function() {
  tryCatch({
load_climate(dataset = "soil_moisture",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_runoff <- function() {
  tryCatch({
load_climate(dataset = "runoff",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_precipitation <- function() {
  tryCatch({
load_climate(dataset = "precipitation",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_evaporation <- function() {
  tryCatch({
load_climate(dataset = "potential_evaporation",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_waterdeficit <- function() {
  tryCatch({
load_climate(dataset = "climatic_water_deficit",
             raw_data = TRUE, 
             time_period = 2010,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_waterevaporation<- function() {
  tryCatch({
load_climate(dataset = "water_evaporation",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_climate_wrapper_palmerdrought <- function() {
  tryCatch({
load_climate(dataset = "palmer_drought_severity_index",
             raw_data = TRUE, 
             time_period = 2000,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_climate_wrapper_precipitation_leagalamazononly <- function() {
  tryCatch({
load_climate(dataset='precipitation',
             time_period=2010,
             legal_amazon_only = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


# SEEG --------------------------------------------------------------------


load_seeg_wrapper_state <- function() {
  tryCatch({
load_seeg(dataset = "seeg", 
          raw_data = TRUE,
          geo_level = "state",
          language = "pt") 
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_seeg_wrapper_country <- function() {
  tryCatch({
load_seeg(dataset = "seeg", 
          raw_data = TRUE,
          geo_level = "country",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_municipality <- function() {
  tryCatch({
load_seeg(dataset = "seeg", 
         raw_data = TRUE,
         geo_level = "municipality",
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_seeg_wrapper_farming_country_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_farming", 
          raw_data = FALSE,
          geo_level = "country",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_farming_state_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_farming", 
          raw_data = FALSE,
          geo_level = "state",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_farming_municipality_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_farming", 
          raw_data =FALSE,
          geo_level = "municipality",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_seeg_wrapper_industry_country_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_industry", 
          raw_data = FALSE,
          geo_level = "country",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_industry_state_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_industry", 
          raw_data = FALSE,
          geo_level = "state",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_industry_municipality_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_industry", 
          raw_data = FALSE,
          geo_level = "municipality",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_seeg_wrapper_energy_country_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_energy", 
          raw_data = FALSE,
          geo_level = "country",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_energy_state_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_energy", 
          raw_data = FALSE,
          geo_level = "state",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_energy_municipality_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_energy", 
          raw_data = FALSE,
          geo_level = "municipality",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_seeg_wrapper_land_country_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_land", 
          raw_data = FALSE,
          geo_level = "country",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_land_state_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_land", 
          raw_data = FALSE,
          geo_level = "state",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_land_municipality_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_land", 
          raw_data = FALSE,
          geo_level = "municipality",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_seeg_wrapper_residuals_country_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_residuals", 
          raw_data = FALSE,
          geo_level = "country",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_residuals_state_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_residuals", 
          raw_data = FALSE,
          geo_level = "state",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_seeg_wrapper_residuals_municipality_rawfalse <- function() {
  tryCatch({
load_seeg(dataset = "seeg_residuals", 
          raw_data = FALSE,
          geo_level = "municipality",
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



# IPS ---------------------------------------------------------------------



load_ips_wrapper_all <- function() {
  tryCatch({
load_ips(dataset = "all", raw_data = TRUE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_ips_wrapper_all_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "all", raw_data = FALSE,
         time_period = 2014, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_lifequality_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "life_quality", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_sanithabit_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "sanit_habit", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_violence_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "violence", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_educ_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "educ", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_communic_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "communic", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_mortality_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "mortality", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_ips_wrapper_deforest_rawfalse <- function() {
  tryCatch({
load_ips(dataset = "deforest", raw_data = FALSE,
         time_period = 2018, language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




# DATASUS - SIM  ----------------------------------------------------------

load_datasus_wrapper_simdo <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_do",
             time_period = 2010,
             states = "AM",
             raw_data = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdo_rawfalse_keepallfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_do",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = FALSE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdo_rawfalse_keepalltrue <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_do",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_datasus_wrapper_simdofet <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_dofet",
             time_period = 2010,
             states = "AM",
             raw_data = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdofet_rawfalse_keepallfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_dofet",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = FALSE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdofet_rawfalse_keepalltrue <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_dofet",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdoext <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_doext",
             time_period = 2010,
             states = "AM",
             raw_data = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdoext_rawfalse_keepallfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_doext",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = FALSE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdoext_rawfalse_keepalltrue <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_doext",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_datasus_wrapper_simdoinf <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_doinf",
             time_period = 2010,
             states = "AM",
             raw_data = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdoinf_rawfalse_keepallfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_doinf",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = FALSE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdoinf_rawfalse_keepalltrue <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_doinf",
             time_period = 2010,
             states = c("AM", "PA"),
             raw_data = FALSE,
             keep_all = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_datasus_wrapper_simdomat <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_domat",
                     time_period = 2010,
                     states = "AM",
                     raw_data = TRUE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdomat_rawfalse_keepallfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_domat",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                    keep_all = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_simdomat_rawfalse_keepalltrue <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sim_domat",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                    keep_all = TRUE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




# DATASUS - CNES ----------------------------------------------------------


load_datasus_wrapper_cneslt <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_lt",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = TRUE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_datasus_wrapper_cneslt_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_lt",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesst_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_st",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesdc_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_dc",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cneseq_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_eq",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnessr_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_sr",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cneshb_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_hb",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnespf_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_pf",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesep_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_ep",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesrc_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_rc",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesin_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_in",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesee_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_ee",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesef_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_ef",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_datasus_wrapper_cnesgm_rawfalse <- function() {
  tryCatch({
load_datasus(dataset = "datasus_cnes_gm",
                    time_period = 2010,
                    states = c("AM", "PA"),
                    raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}






# DATASUS - SIH  ----------------------------------------------------------

load_datasus_wrapper_sih <- function() {
  tryCatch({
load_datasus(dataset = "datasus_sih",
                    time_period = 2010,
                    states = "AM",
                    raw_data = TRUE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_datasus_wrapper_sih_rawfalse <- function() {
  tryCatch({
 load_datasus(dataset = "datasus_sih",
                    time_period = 2010,
                    states = "AM",
                    raw_data = FALSE,
                       language = "pt")
 return("Success")
 }, error = function(e) {
   return(paste("Error :", e$message))
 })
}





# IEMA --------------------------------------------------------------------



load_iema_wrapper_rawfalse <- function() {
  tryCatch({
load_iema(raw_data = FALSE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_iema_wrapper_2 <- function() {
  tryCatch({
load_iema(raw_data = TRUE,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}





# Population --------------------------------------------------------------


load_population_wrapper_country <- function() {
  tryCatch({
load_population(raw_data = TRUE,
                        geo_level = "country",
                        time_period = 2010:2012,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_population_wrapper_country_rawfalse <- function() {
  tryCatch({
load_population(raw_data = FALSE,
                        geo_level = "country",
                        time_period = 2010:2012,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_population_wrapper_state_rawfalse <- function() {
  tryCatch({
load_population(raw_data = FALSE,
                        geo_level = "state",
                        time_period = 2010:2012,
                       language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_population_wrapper_municipality_rawfalse <- function() {
  tryCatch({
load_population(raw_data = FALSE,
                        geo_level = "municipality",
                        time_period = 2010:2012,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




# COMEX -------------------------------------------------------------------

load_br_trade_wrapper_comexexportmun <- function() {
  tryCatch({
load_br_trade(dataset = "comex_export_mun", 
                      raw_data = TRUE, 
                      time_period = 2020,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_br_trade_wrapper_comexexportmun_rawfalse <- function() {
  tryCatch({
load_br_trade(dataset = "comex_export_mun", 
                      raw_data = FALSE, 
                      time_period = 2020:2021,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_br_trade_wrapper_comexexportprod <- function() {
  tryCatch({
load_br_trade(dataset = "comex_export_prod", 
                      raw_data = TRUE, 
                      time_period = 2020,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_br_trade_wrapper_comexexportprod_rawfalse <- function() {
  tryCatch({
load_br_trade(dataset = "comex_export_prod", 
                      raw_data = FALSE, 
                      time_period = 2020:2021,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_br_trade_wrapper_comeximportmun <- function() {
  tryCatch({
load_br_trade(dataset = "comex_import_mun",
                      raw_data = TRUE, 
                      time_period = 2020,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_br_trade_wrapper_comeximportmun_rawfalse <- function() {
  tryCatch({
load_br_trade(dataset = "comex_import_mun",
                      raw_data = FALSE, 
                      time_period = 2020:2021,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_br_trade_wrapper_comeximportprod <- function() {
  tryCatch({
load_br_trade(dataset = "comex_import_prod",
                      raw_data = TRUE, 
                      time_period = 2020,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_br_trade_wrapper_comeximportprod_rawfalse <- function() {
  tryCatch({
load_br_trade(dataset = "comex_import_prod",
                      raw_data = FALSE, 
                      time_period = 2020:2021,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



# BACI --------------------------------------------------------------------



load_baci_wrapper_1 <- function() {
  tryCatch({
load_baci( raw_data = TRUE,
                        time_period = 2016,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_baci_wrapper_rawfalse <- function() {
  tryCatch({
load_baci( raw_data = FALSE,
                        time_period = 2016,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




# PIB-Munic ---------------------------------------------------------------



load_pibmunic_wrapper_country <- function() {
  tryCatch({
load_pibmunic(raw_data = TRUE,
                      geo_level = "country",
                      time_period = 2010,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pibmunic_wrapper_country_rawfalse <- function() {
  tryCatch({
load_pibmunic(raw_data = FALSE,
                      geo_level = "country",
                      time_period = 2010:2012,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_pibmunic_wrapper_state_rawfalse <- function() {
  tryCatch({
load_pibmunic(raw_data = FALSE,
                      geo_level = "state",
                      time_period = 2010,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_pibmunic_wrapper_municipality_rawfalse <- function() {
  tryCatch({
load_pibmunic(raw_data = FALSE,
                      geo_level = "municipality",
                      time_period = 2011,
                        language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



# CEMPRE ------------------------------------------------------------------

load_cempre_wrapper_country <- function() {
  tryCatch({
load_cempre(raw_data = TRUE,
                      geo_level = "country",
                      time_period = 2010,
                        language = "pt",
                    sectors = FALSE)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_cempre_wrapper_country_rawfalse <- function() {
  tryCatch({
load_cempre(raw_data = FALSE,
                      geo_level = "country",
                      time_period = 2010:2012,
                        language = "pt",
                    sectors = FALSE)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_cempre_wrapper_country_rawfalse_sectorstrue <- function() {
  tryCatch({
load_cempre(raw_data = FALSE,
                      geo_level = "country",
                      time_period = 2010:2012,
                        language = "pt",
                    sectors = TRUE)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_cempre_wrapper_state_rawfalse <- function() {
  tryCatch({
load_cempre(raw_data = FALSE,
                      geo_level = "state",
                      time_period = 2010,
                        language = "pt",
                    sectors = FALSE)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_cempre_wrapper_municipality_rawfalse <- function() {
  tryCatch({
load_cempre(raw_data = FALSE,
                      geo_level = "municipality",
                      time_period = 2011,
                        language = "pt",
                    sectors = FALSE)
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



# PAM ---------------------------------------------------------------------


load_pam_wrapper_allcrops_country <- function() {
  tryCatch({
load_pam(dataset = "all_crops", 
                 raw_data = TRUE, 
                 geo_level = "country", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_pam_wrapper_allcrops_country_rawfalse <- function() {
  tryCatch({
load_pam(dataset = "all_crops", 
                 raw_data = FALSE, 
                 geo_level = "country", 
                 time_period = 2010:2011,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pam_wrapper_allcrops_state_rawfalse <- function() {
  tryCatch({
load_pam(dataset = "all_crops", 
                 raw_data = FALSE, 
                 geo_level = "state", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pam_wrapper_allcrops_municipality_rawfalse <- function() {
  tryCatch({
load_pam(dataset = "all_crops", 
                 raw_data = FALSE, 
                 geo_level = "municipality",
         time_period = 2010, 
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_load_pam_wrapper_allcrops_region_rawfalse <- function() {
  tryCatch({
load_pam(dataset = "all_crops", 
         raw_data = FALSE, 
         geo_level = "region", 
         time_period = 2010,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_load_pam_wrapper_olive_country_rawfalse <- function() {
  tryCatch({
load_pam(dataset = "olive", 
         raw_data = FALSE, 
         geo_level = "country", 
         time_period = 2010,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}





# PEVS --------------------------------------------------------------------


load_pevs_wrapper_siviculture_country <- function() {
  tryCatch({
load_pevs(dataset = "pevs_silviculture", 
          raw_data = TRUE,
          geo_level = "country", 
          time_period = 2010,
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_pevs_wrapper_siviculture_country_rawfalse <- function() {
  tryCatch({
load_pevs(dataset = "pevs_silviculture", 
          raw_data = FALSE,
          geo_level = "country", 
          time_period = 2010:2011,
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pevs_wrapper_siviculture_state_rawfalse <- function() {
  tryCatch({
load_pevs(dataset = "pevs_silviculture", 
          raw_data = FALSE, 
          geo_level = "state", 
          time_period = 2010,
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pevs_wrapper_siviculture_municipality_rawfalse <- function() {
  tryCatch({
load_pevs(dataset = "pevs_silviculture", 
          raw_data = FALSE, 
          geo_level = "municipality", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pevs_wrapper_siviculture_region_rawfalse <- function() {
  tryCatch({
load_pevs(dataset = "pevs_silviculture", 
                 raw_data = FALSE, 
                 geo_level = "region", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_pevs_wrapper_forestcrops_country_rawfalse <- function() {
  tryCatch({
load_pevs(dataset = "pevs_forest_crops", 
                 raw_data = FALSE, 
                 geo_level = "country", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_pevs_wrapper_siviculturearea_country_rawfalse <- function() {
  tryCatch({
load_pevs(dataset = "pevs_silviculture_area", 
                 raw_data = FALSE, 
                 geo_level = "country", 
                 time_period = 2013,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}





# PPM ---------------------------------------------------------------------



load_ppm_wrapper_livestock_country <- function() {
  tryCatch({
load_ppm(dataset = "ppm_livestock_inventory", 
                 raw_data = TRUE, 
                 geo_level = "country", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_ppm_wrapper_livestock_country_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_livestock_inventory", 
                 raw_data = FALSE, 
                 geo_level = "country", 
                 time_period = 2010:2011,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_ppm_wrapper_livestock_state_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_livestock_inventory", 
                 raw_data = FALSE, 
                 geo_level = "state", 
                 time_period = 2010,
                 language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_ppm_wrapper_livestock_municipality_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_livestock_inventory", 
                 raw_data = FALSE, 
                 geo_level = "municipality", 
          time_period = 2010,
          language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_ppm_wrapper_livestock_region_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_livestock_inventory", 
         raw_data = FALSE, 
         geo_level = "region", 
         time_period = 2010,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




load_ppm_wrapper_sheep_country_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_sheep_farming", 
         raw_data = FALSE, 
         geo_level = "country", 
         time_period = 2010,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_ppm_wrapper_animal_orig_country_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_animal_orig_production", 
         raw_data = FALSE, 
         geo_level = "country", 
         time_period = 2013,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_ppm_wrapper_cow_country_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_cow_farming", 
         raw_data = FALSE, 
         geo_level = "country", 
         time_period = 2013,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_ppm_wrapper_aquaculture_country_rawfalse <- function() {
  tryCatch({
load_ppm(dataset = "ppm_aquaculture", 
         raw_data = FALSE, 
         geo_level = "country", 
         time_period = 2013,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




# SIGMINE -----------------------------------------------------------------




load_sigmine_wrapper_1 <- function() {
  tryCatch({
load_sigmine(dataset = 'sigmine_active', 
             raw_data = TRUE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_sigmine_wrapper_rawfalse <- function() {
  tryCatch({
load_sigmine(dataset = 'sigmine_active', 
             raw_data = FALSE,
             language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}




# ANEEL -------------------------------------------------------------------

load_aneel_wrapper_generation <- function() {
  tryCatch({
load_aneel( dataset = "energy generation",
            raw_data = TRUE,
            language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_aneel_wrapper_generation_rawfalse <- function() {
  tryCatch({
load_aneel( dataset = "energy generation",
            raw_data = FALSE,
            language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_aneel_wrapper_development_rawfalse <- function() {
  tryCatch({
load_aneel( dataset = "energy_development_budget",
            raw_data = FALSE,
            language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}

load_aneel_wrapper_enterprises_rawfalse <- function() {
  tryCatch({
load_aneel( dataset = "energy_enterprises_distributed",                     
            raw_data = FALSE,
            language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



# EPE ---------------------------------------------------------------------

load_epe_wrapper_energy_perclass_state <- function() {
  tryCatch({
load_epe(dataset = "energy_consumption_per_class",
         geo_level = "state",
         raw_data = TRUE,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}


load_epe_wrapper_energy_perclass_state_rawfalse <- function() {
  tryCatch({
load_epe(dataset = "energy_consumption_per_class",
         geo_level = "state",
         raw_data = FALSE,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_epe_wrapper_energy_perclass_state_rawfalse_subsystem <- function() {
  tryCatch({
load_epe(dataset = "energy_consumption_per_class",
         geo_level = "subsystem",
         raw_data = FALSE,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}



load_epe_wrapper_national_energy_rawfalse <- function() {
  tryCatch({
load_epe(dataset = "national_energy_balance",
         raw_data = FALSE,
         language = "pt")
return("Success")
}, error = function(e) {
  return(paste("Error :", e$message))
})
}







###########################################################################
# LIST OF THE FUNCTIONS TO TEST -------------------------------------------
###########################################################################

#---------------------------------------------------------TO CHANGE IF NEED
# Create a list of wrapper functions with names
#-------------------------------------------------------------------------!
#The wrapper_functions_list is a list with all the functions mentioned above
#If there is a need to test specific functions please change the wrapper_functions_list at your convenience
wrapper_functions_list <- list(
  load_prodes_wrapper_rawfalse=load_prodes_wrapper_rawfalse,
  load_prodes_wrapper_1=load_prodes_wrapper_1,
  load_deter_wrapper_amz=load_deter_wrapper_amz,
  load_deter_wrapper_cerrado=load_deter_wrapper_cerrado,
  load_deter_wrapper_amz_rawfalse=load_deter_wrapper_amz_rawfalse,
  load_degrad_wrapper_1=load_degrad_wrapper_1,
  load_degrad_wrapper_rawfalse=load_degrad_wrapper_rawfalse,
  load_imazon_wrapper_1=load_imazon_wrapper_1,
  load_imazon_wrapper_rawfalse=load_imazon_wrapper_rawfalse,
  load_ibama_wrapper_1=load_ibama_wrapper_1,
  load_ibama_wrapper_rawfalse=load_ibama_wrapper_rawfalse,
  load_ibama_wrapper_states=load_ibama_wrapper_states,
  load_ibama_wrapper_collected_fines=load_ibama_wrapper_collected_fines,
  load_ibama_wrapper_collected_fines_rawfalse=load_ibama_wrapper_collected_fines_rawfalse,
  load_ibama_wrapper_collected_fines_rawfalse_states=load_ibama_wrapper_collected_fines_rawfalse_states,
  load_ibama_wrapper_fines=load_ibama_wrapper_fines,
  load_ibama_wrapper_fines_rawfalse=load_ibama_wrapper_fines_rawfalse,
  load_ibama_wrapper_fines_rawfalse_states=load_ibama_wrapper_fines_rawfalse_states,
  load_mapbiomas_wrapper_cover0_state=load_mapbiomas_wrapper_cover0_state,
  load_mapbiomas_wrapper_cover0_municipality_rawfalse=load_mapbiomas_wrapper_cover0_municipality_rawfalse,
  load_mapbiomas_wrapper_cover0_state_rawfalse=load_mapbiomas_wrapper_cover0_state_rawfalse,
  load_mapbiomas_wrapper_cover1_state_rawfalse=load_mapbiomas_wrapper_cover1_state_rawfalse,
  load_mapbiomas_wrapper_cover2_state_rawfalse=load_mapbiomas_wrapper_cover2_state_rawfalse,
  load_mapbiomas_wrapper_cover3_state_rawfalse=load_mapbiomas_wrapper_cover3_state_rawfalse,
  load_mapbiomas_wrapper_cover4_state_rawfalse=load_mapbiomas_wrapper_cover4_state_rawfalse,
  load_mapbiomas_wrapper_covernone_state_rawfalse=load_mapbiomas_wrapper_covernone_state_rawfalse,
  load_mapbiomas_wrapper_transition_state_rawfalse=load_mapbiomas_wrapper_transition_state_rawfalse,
  load_mapbiomas_wrapper_transition_state=load_mapbiomas_wrapper_transition_state,
  load_mapbiomas_wrapper_transition_municipality=load_mapbiomas_wrapper_transition_municipality,
  load_mapbiomas_wrapper_regeneration_state_rawfalse=load_mapbiomas_wrapper_regeneration_state_rawfalse,
  load_mapbiomas_wrapper_regeneration_state=load_mapbiomas_wrapper_regeneration_state,
  load_mapbiomas_wrapper_regeneration_municipality=load_mapbiomas_wrapper_regeneration_municipality,
  load_mapbiomas_wrapper_irrigation_state=load_mapbiomas_wrapper_irrigation_state,
  load_mapbiomas_wrapper_irrigation_state_rawfalse=load_mapbiomas_wrapper_irrigation_state_rawfalse,
  load_mapbiomas_wrapper_irrigation_biome_rawfalse=load_mapbiomas_wrapper_irrigation_biome_rawfalse,
  load_mapbiomas_wrapper_grazing_rawfalse=load_mapbiomas_wrapper_grazing_rawfalse,
  load_mapbiomas_wrapper_grazing=load_mapbiomas_wrapper_grazing,
  load_mapbiomas_wrapper_mining_country=load_mapbiomas_wrapper_mining_country,
  load_mapbiomas_wrapper_mining_country_rawfalse=load_mapbiomas_wrapper_mining_country_rawfalse,
  load_mapbiomas_wrapper_mining_indigenous_rawfalse=load_mapbiomas_wrapper_mining_indigenous_rawfalse,
  load_mapbiomas_wrapper_mining_municipality_rawfalse=load_mapbiomas_wrapper_mining_municipality_rawfalse,
  load_mapbiomas_wrapper_mining_biome_rawfalse=load_mapbiomas_wrapper_mining_biome_rawfalse,
  load_mapbiomas_wrapper_mining_state_rawfalse=load_mapbiomas_wrapper_mining_state_rawfalse,
  load_mapbiomas_wrapper_water_state=load_mapbiomas_wrapper_water_state,
  load_mapbiomas_wrapper_water_biome_rawfalse=load_mapbiomas_wrapper_water_biome_rawfalse,
  load_mapbiomas_wrapper_water_state_rawfalse=load_mapbiomas_wrapper_water_state_rawfalse,
  load_mapbiomas_wrapper_water_municipality_rawfalse=load_mapbiomas_wrapper_water_municipality_rawfalse,
  load_mapbiomas_wrapper_fire_state=load_mapbiomas_wrapper_fire_state,
  load_mapbiomas_wrapper_fire_state_rawfalse=load_mapbiomas_wrapper_fire_state_rawfalse,
  load_mapbiomas_wrapper_fire_municipality_rawfalse=load_mapbiomas_wrapper_fire_municipality_rawfalse,
  #load_cipo_wrapper_brazilian_actors_rawfalse=load_cipo_wrapper_brazilian_actors_rawfalse,
  # load_cipo_wrapper_brazilian_actors=load_cipo_wrapper_brazilian_actors,
  # load_cipo_wrapper_brazilian_actors_ibama=load_cipo_wrapper_brazilian_actors_ibama,
  # load_cipo_wrapper_brazilian_actors_ibama_funai=load_cipo_wrapper_brazilian_actors_ibama_funai,
  # load_cipo_wrapper_international_coop_rawfalse=load_cipo_wrapper_international_coop_rawfalse,
  # load_cipo_wrapper_international_coop=load_cipo_wrapper_international_coop,
  # load_cipo_wrapper_international_coop_ibama=load_cipo_wrapper_international_coop_ibama,
  # load_cipo_wrapper_international_coop_ibama_funai=load_cipo_wrapper_international_coop_ibama_funai,
  # load_cipo_wrapper_forest_governance_rawfalse=load_cipo_wrapper_forest_governance_rawfalse,
  # load_cipo_wrapper_forest_governance=load_cipo_wrapper_forest_governance, 
  # load_cipo_wrapper_forest_governance_ibama=load_cipo_wrapper_forest_governance_ibama, 
  # load_cipo_wrapper_forest_governance_ibama_funai=load_cipo_wrapper_forest_governance_ibama_funai,
  load_climate_wrapper_maxtemperature=load_climate_wrapper_maxtemperature,
  load_climate_wrapper_mintemperature=load_climate_wrapper_mintemperature,
  load_climate_wrapper_windspeed=load_climate_wrapper_windspeed,
  load_climate_wrapper_vaporpressure=load_climate_wrapper_vaporpressure,
  load_climate_wrapper_vaporpressuredeficit=load_climate_wrapper_vaporpressuredeficit,
  load_climate_wrapper_snowwaterequivalent=load_climate_wrapper_snowwaterequivalent,
  load_climate_wrapper_shortwaveradiation=load_climate_wrapper_shortwaveradiation,
  load_climate_wrapper_soilmoisture=load_climate_wrapper_soilmoisture,
  load_climate_wrapper_runoff=load_climate_wrapper_runoff,
  load_climate_wrapper_precipitation=load_climate_wrapper_precipitation,
  load_climate_wrapper_evaporation=load_climate_wrapper_evaporation,
  load_climate_wrapper_waterdeficit=load_climate_wrapper_waterdeficit,
  load_climate_wrapper_waterevaporation=load_climate_wrapper_waterevaporation,
  load_climate_wrapper_palmerdrought=load_climate_wrapper_palmerdrought,
  load_climate_wrapper_precipitation_leagalamazononly=load_climate_wrapper_precipitation_leagalamazononly,
  load_seeg_wrapper_state=load_seeg_wrapper_state,
  load_seeg_wrapper_country=load_seeg_wrapper_country,
  load_seeg_wrapper_municipality=load_seeg_wrapper_municipality,
  load_seeg_wrapper_farming_country_rawfalse=load_seeg_wrapper_farming_country_rawfalse,
  load_seeg_wrapper_farming_state_rawfalse=load_seeg_wrapper_farming_state_rawfalse,
  load_seeg_wrapper_farming_municipality_rawfalse=load_seeg_wrapper_farming_municipality_rawfalse,
  load_seeg_wrapper_industry_country_rawfalse=load_seeg_wrapper_industry_country_rawfalse,
  load_seeg_wrapper_industry_state_rawfalse=load_seeg_wrapper_industry_state_rawfalse,
  load_seeg_wrapper_industry_municipality_rawfalse=load_seeg_wrapper_industry_municipality_rawfalse,
  load_seeg_wrapper_energy_country_rawfalse=load_seeg_wrapper_energy_country_rawfalse,
  load_seeg_wrapper_energy_state_rawfalse=load_seeg_wrapper_energy_state_rawfalse,
  load_seeg_wrapper_energy_municipality_rawfalse=load_seeg_wrapper_energy_municipality_rawfalse,
  load_seeg_wrapper_land_country_rawfalse=load_seeg_wrapper_land_country_rawfalse,
  load_seeg_wrapper_land_state_rawfalse=load_seeg_wrapper_land_state_rawfalse,
  load_seeg_wrapper_land_municipality_rawfalse=load_seeg_wrapper_land_municipality_rawfalse,
  load_seeg_wrapper_residuals_country_rawfalse=load_seeg_wrapper_residuals_country_rawfalse,
  load_seeg_wrapper_residuals_state_rawfalse=load_seeg_wrapper_residuals_state_rawfalse,
  load_seeg_wrapper_residuals_municipality_rawfalse=load_seeg_wrapper_residuals_municipality_rawfalse,
  load_ips_wrapper_all=load_ips_wrapper_all,
  load_ips_wrapper_all_rawfalse=load_ips_wrapper_all_rawfalse,
  load_ips_wrapper_lifequality_rawfalse=load_ips_wrapper_lifequality_rawfalse,
  load_ips_wrapper_sanithabit_rawfalse=load_ips_wrapper_sanithabit_rawfalse,
  load_ips_wrapper_violence_rawfalse=load_ips_wrapper_violence_rawfalse,
  load_ips_wrapper_educ_rawfalse=load_ips_wrapper_educ_rawfalse,
  load_ips_wrapper_communic_rawfalse=load_ips_wrapper_communic_rawfalse,
  load_ips_wrapper_mortality_rawfalse=load_ips_wrapper_mortality_rawfalse,
  load_ips_wrapper_deforest_rawfalse=load_ips_wrapper_deforest_rawfalse,
  load_datasus_wrapper_simdo=load_datasus_wrapper_simdo,
  load_datasus_wrapper_simdo_rawfalse_keepallfalse=load_datasus_wrapper_simdo_rawfalse_keepallfalse,
  load_datasus_wrapper_simdo_rawfalse_keepalltrue=load_datasus_wrapper_simdo_rawfalse_keepalltrue,
  load_datasus_wrapper_simdofet=load_datasus_wrapper_simdofet,
  load_datasus_wrapper_simdofet_rawfalse_keepallfalse=load_datasus_wrapper_simdofet_rawfalse_keepallfalse,
  load_datasus_wrapper_simdofet_rawfalse_keepalltrue=load_datasus_wrapper_simdofet_rawfalse_keepalltrue,
  load_datasus_wrapper_simdoext=load_datasus_wrapper_simdoext,
  load_datasus_wrapper_simdoext_rawfalse_keepallfalse=load_datasus_wrapper_simdoext_rawfalse_keepallfalse,
  load_datasus_wrapper_simdoext_rawfalse_keepalltrue=load_datasus_wrapper_simdoext_rawfalse_keepalltrue,
  load_datasus_wrapper_simdoinf=load_datasus_wrapper_simdoinf,
  load_datasus_wrapper_simdoinf_rawfalse_keepallfalse=load_datasus_wrapper_simdoinf_rawfalse_keepallfalse,
  load_datasus_wrapper_simdoinf_rawfalse_keepalltrue=load_datasus_wrapper_simdoinf_rawfalse_keepalltrue,
  load_datasus_wrapper_simdomat=load_datasus_wrapper_simdomat,
  load_datasus_wrapper_simdomat_rawfalse_keepallfalse=load_datasus_wrapper_simdomat_rawfalse_keepallfalse,
  load_datasus_wrapper_simdomat_rawfalse_keepalltrue=load_datasus_wrapper_simdomat_rawfalse_keepalltrue,
  load_datasus_wrapper_cneslt=load_datasus_wrapper_cneslt,
  load_datasus_wrapper_cneslt_rawfalse=load_datasus_wrapper_cneslt_rawfalse,
  load_datasus_wrapper_cnesst_rawfalse=load_datasus_wrapper_cnesst_rawfalse,
  load_datasus_wrapper_cnesdc_rawfalse=load_datasus_wrapper_cnesdc_rawfalse,
  load_datasus_wrapper_cneseq_rawfalse=load_datasus_wrapper_cneseq_rawfalse,
  load_datasus_wrapper_cnessr_rawfalse=load_datasus_wrapper_cnessr_rawfalse,
  load_datasus_wrapper_cneshb_rawfalse=load_datasus_wrapper_cneshb_rawfalse,
  load_datasus_wrapper_cnespf_rawfalse=load_datasus_wrapper_cnespf_rawfalse,
  load_datasus_wrapper_cnesep_rawfalse=load_datasus_wrapper_cnesep_rawfalse,
  load_datasus_wrapper_cnesrc_rawfalse=load_datasus_wrapper_cnesrc_rawfalse,
  load_datasus_wrapper_cnesin_rawfalse=load_datasus_wrapper_cnesin_rawfalse,
  load_datasus_wrapper_cnesee_rawfalse=load_datasus_wrapper_cnesee_rawfalse,
  load_datasus_wrapper_cnesef_rawfalse=load_datasus_wrapper_cnesef_rawfalse,
  load_datasus_wrapper_cnesgm_rawfalse=load_datasus_wrapper_cnesgm_rawfalse,
  load_datasus_wrapper_sih=load_datasus_wrapper_sih,
  load_datasus_wrapper_sih_rawfalse=load_datasus_wrapper_sih_rawfalse,
  load_iema_wrapper_rawfalse=load_iema_wrapper_rawfalse,
  load_iema_wrapper_2=load_iema_wrapper_2,
  load_population_wrapper_country=load_population_wrapper_country,
  load_population_wrapper_country_rawfalse=load_population_wrapper_country_rawfalse,
  load_population_wrapper_state_rawfalse=load_population_wrapper_state_rawfalse,
  load_population_wrapper_municipality_rawfalse=load_population_wrapper_municipality_rawfalse,
  load_br_trade_wrapper_comexexportmun=load_br_trade_wrapper_comexexportmun,
  load_br_trade_wrapper_comexexportmun_rawfalse=load_br_trade_wrapper_comexexportmun_rawfalse,
  load_br_trade_wrapper_comexexportprod=load_br_trade_wrapper_comexexportprod,
  load_br_trade_wrapper_comexexportprod_rawfalse=load_br_trade_wrapper_comexexportprod_rawfalse,
  load_br_trade_wrapper_comeximportmun=load_br_trade_wrapper_comeximportmun,
  load_br_trade_wrapper_comeximportmun_rawfalse=load_br_trade_wrapper_comeximportmun_rawfalse,
  load_br_trade_wrapper_comeximportprod=load_br_trade_wrapper_comeximportprod,
  load_br_trade_wrapper_comeximportprod_rawfalse=load_br_trade_wrapper_comeximportprod_rawfalse,
  load_baci_wrapper_1=load_baci_wrapper_1,
  load_baci_wrapper_rawfalse=load_baci_wrapper_rawfalse,
  load_pibmunic_wrapper_country=load_pibmunic_wrapper_country,
  load_pibmunic_wrapper_country_rawfalse=load_pibmunic_wrapper_country_rawfalse,
  load_pibmunic_wrapper_state_rawfalse=load_pibmunic_wrapper_state_rawfalse,
  load_pibmunic_wrapper_municipality_rawfalse=load_pibmunic_wrapper_municipality_rawfalse,
  load_cempre_wrapper_country=load_cempre_wrapper_country,
  load_cempre_wrapper_country_rawfalse=load_cempre_wrapper_country_rawfalse,
  load_cempre_wrapper_country_rawfalse_sectorstrue=load_cempre_wrapper_country_rawfalse_sectorstrue,
  load_cempre_wrapper_state_rawfalse=load_cempre_wrapper_state_rawfalse,
  load_cempre_wrapper_municipality_rawfalse=load_cempre_wrapper_municipality_rawfalse,
  load_pam_wrapper_allcrops_country=load_pam_wrapper_allcrops_country,
  load_pam_wrapper_allcrops_country_rawfalse=load_pam_wrapper_allcrops_country_rawfalse,
  load_pam_wrapper_allcrops_state_rawfalse=load_pam_wrapper_allcrops_state_rawfalse,
  load_pam_wrapper_allcrops_municipality_rawfalse=load_pam_wrapper_allcrops_municipality_rawfalse,
  load_load_pam_wrapper_allcrops_region_rawfalse=load_load_pam_wrapper_allcrops_region_rawfalse,
  load_load_pam_wrapper_olive_country_rawfalse=load_load_pam_wrapper_olive_country_rawfalse,
  load_pevs_wrapper_siviculture_country=load_pevs_wrapper_siviculture_country,
  load_pevs_wrapper_siviculture_country_rawfalse=load_pevs_wrapper_siviculture_country_rawfalse,
  load_pevs_wrapper_siviculture_state_rawfalse=load_pevs_wrapper_siviculture_state_rawfalse,
  load_pevs_wrapper_siviculture_municipality_rawfalse=load_pevs_wrapper_siviculture_municipality_rawfalse,
  load_pevs_wrapper_siviculture_region_rawfalse=load_pevs_wrapper_siviculture_region_rawfalse,
  load_pevs_wrapper_forestcrops_country_rawfalse=load_pevs_wrapper_forestcrops_country_rawfalse,
  load_pevs_wrapper_siviculturearea_country_rawfalse=load_pevs_wrapper_siviculturearea_country_rawfalse,
  load_ppm_wrapper_livestock_country=load_ppm_wrapper_livestock_country,
  load_ppm_wrapper_livestock_country_rawfalse=load_ppm_wrapper_livestock_country_rawfalse,
  load_ppm_wrapper_livestock_state_rawfalse=load_ppm_wrapper_livestock_state_rawfalse,
  load_ppm_wrapper_livestock_municipality_rawfalse=load_ppm_wrapper_livestock_municipality_rawfalse,
  load_ppm_wrapper_livestock_region_rawfalse=load_ppm_wrapper_livestock_region_rawfalse,
  load_ppm_wrapper_sheep_country_rawfalse=load_ppm_wrapper_sheep_country_rawfalse,
  load_ppm_wrapper_animal_orig_country_rawfalse=load_ppm_wrapper_animal_orig_country_rawfalse,
  load_ppm_wrapper_cow_country_rawfalse=load_ppm_wrapper_cow_country_rawfalse,
  load_ppm_wrapper_aquaculture_country_rawfalse=load_ppm_wrapper_aquaculture_country_rawfalse,
  load_sigmine_wrapper_1=load_sigmine_wrapper_1,
  load_sigmine_wrapper_rawfalse=load_sigmine_wrapper_rawfalse,
  load_aneel_wrapper_generation=load_aneel_wrapper_generation,
  load_aneel_wrapper_generation_rawfalse=load_aneel_wrapper_generation_rawfalse,
  load_aneel_wrapper_development_rawfalse=load_aneel_wrapper_development_rawfalse,
  load_aneel_wrapper_enterprises_rawfalse=load_aneel_wrapper_enterprises_rawfalse,
  load_epe_wrapper_energy_perclass_state=load_epe_wrapper_energy_perclass_state,
  load_epe_wrapper_energy_perclass_state_rawfalse=load_epe_wrapper_energy_perclass_state_rawfalse,
  load_epe_wrapper_energy_perclass_state_rawfalse_subsystem=load_epe_wrapper_energy_perclass_state_rawfalse_subsystem,
  load_epe_wrapper_national_energy_rawfalse=load_epe_wrapper_national_energy_rawfalse
)





# Create 2 distinct lists of wrapper functions with names
# This list does not require googledrive
wrapper_functions_list_nogoogledrive <- list(
  load_prodes_wrapper_rawfalse=load_prodes_wrapper_rawfalse,
  load_prodes_wrapper_1=load_prodes_wrapper_1,
  load_deter_wrapper_amz=load_deter_wrapper_amz,
  load_deter_wrapper_cerrado=load_deter_wrapper_cerrado,
  load_deter_wrapper_amz_rawfalse=load_deter_wrapper_amz_rawfalse,
  load_degrad_wrapper_1=load_degrad_wrapper_1,
  load_degrad_wrapper_rawfalse=load_degrad_wrapper_rawfalse,
  load_ibama_wrapper_1=load_ibama_wrapper_1,
  load_ibama_wrapper_rawfalse=load_ibama_wrapper_rawfalse,
  load_ibama_wrapper_states=load_ibama_wrapper_states,
  load_ibama_wrapper_collected_fines=load_ibama_wrapper_collected_fines,
  load_ibama_wrapper_collected_fines_rawfalse=load_ibama_wrapper_collected_fines_rawfalse,
  load_ibama_wrapper_collected_fines_rawfalse_states=load_ibama_wrapper_collected_fines_rawfalse_states,
  load_ibama_wrapper_fines=load_ibama_wrapper_fines,
  load_ibama_wrapper_fines_rawfalse=load_ibama_wrapper_fines_rawfalse,
  load_ibama_wrapper_fines_rawfalse_states=load_ibama_wrapper_fines_rawfalse_states,
 #load_cipo_wrapper_brazilian_actors_rawfalse=load_cipo_wrapper_brazilian_actors_rawfalse,
  # load_cipo_wrapper_brazilian_actors=load_cipo_wrapper_brazilian_actors,
  # load_cipo_wrapper_brazilian_actors_ibama=load_cipo_wrapper_brazilian_actors_ibama,
  # load_cipo_wrapper_brazilian_actors_ibama_funai=load_cipo_wrapper_brazilian_actors_ibama_funai,
  # load_cipo_wrapper_international_coop_rawfalse=load_cipo_wrapper_international_coop_rawfalse,
  # load_cipo_wrapper_international_coop=load_cipo_wrapper_international_coop,
  # load_cipo_wrapper_international_coop_ibama=load_cipo_wrapper_international_coop_ibama,
  # load_cipo_wrapper_international_coop_ibama_funai=load_cipo_wrapper_international_coop_ibama_funai,
  # load_cipo_wrapper_forest_governance_rawfalse=load_cipo_wrapper_forest_governance_rawfalse,
  # load_cipo_wrapper_forest_governance=load_cipo_wrapper_forest_governance, 
  # load_cipo_wrapper_forest_governance_ibama=load_cipo_wrapper_forest_governance_ibama, 
  # load_cipo_wrapper_forest_governance_ibama_funai=load_cipo_wrapper_forest_governance_ibama_funai,
  load_climate_wrapper_maxtemperature=load_climate_wrapper_maxtemperature,
  load_climate_wrapper_mintemperature=load_climate_wrapper_mintemperature,
  load_climate_wrapper_windspeed=load_climate_wrapper_windspeed,
  load_climate_wrapper_vaporpressure=load_climate_wrapper_vaporpressure,
  load_climate_wrapper_vaporpressuredeficit=load_climate_wrapper_vaporpressuredeficit,
  load_climate_wrapper_snowwaterequivalent=load_climate_wrapper_snowwaterequivalent,
  load_climate_wrapper_shortwaveradiation=load_climate_wrapper_shortwaveradiation,
  load_climate_wrapper_soilmoisture=load_climate_wrapper_soilmoisture,
  load_climate_wrapper_runoff=load_climate_wrapper_runoff,
  load_climate_wrapper_precipitation=load_climate_wrapper_precipitation,
  load_climate_wrapper_evaporation=load_climate_wrapper_evaporation,
  load_climate_wrapper_waterdeficit=load_climate_wrapper_waterdeficit,
  load_climate_wrapper_waterevaporation=load_climate_wrapper_waterevaporation,
  load_climate_wrapper_palmerdrought=load_climate_wrapper_palmerdrought,
  load_climate_wrapper_precipitation_leagalamazononly=load_climate_wrapper_precipitation_leagalamazononly,
  load_ips_wrapper_all=load_ips_wrapper_all,
  load_ips_wrapper_all_rawfalse=load_ips_wrapper_all_rawfalse,
  load_ips_wrapper_lifequality_rawfalse=load_ips_wrapper_lifequality_rawfalse,
  load_ips_wrapper_sanithabit_rawfalse=load_ips_wrapper_sanithabit_rawfalse,
  load_ips_wrapper_violence_rawfalse=load_ips_wrapper_violence_rawfalse,
  load_ips_wrapper_educ_rawfalse=load_ips_wrapper_educ_rawfalse,
  load_ips_wrapper_communic_rawfalse=load_ips_wrapper_communic_rawfalse,
  load_ips_wrapper_mortality_rawfalse=load_ips_wrapper_mortality_rawfalse,
  load_ips_wrapper_deforest_rawfalse=load_ips_wrapper_deforest_rawfalse,
  load_datasus_wrapper_simdo=load_datasus_wrapper_simdo,
  load_datasus_wrapper_simdo_rawfalse_keepallfalse=load_datasus_wrapper_simdo_rawfalse_keepallfalse,
  load_datasus_wrapper_simdo_rawfalse_keepalltrue=load_datasus_wrapper_simdo_rawfalse_keepalltrue,
  load_datasus_wrapper_simdofet=load_datasus_wrapper_simdofet,
  load_datasus_wrapper_simdofet_rawfalse_keepallfalse=load_datasus_wrapper_simdofet_rawfalse_keepallfalse,
  load_datasus_wrapper_simdofet_rawfalse_keepalltrue=load_datasus_wrapper_simdofet_rawfalse_keepalltrue,
  load_datasus_wrapper_simdoext=load_datasus_wrapper_simdoext,
  load_datasus_wrapper_simdoext_rawfalse_keepallfalse=load_datasus_wrapper_simdoext_rawfalse_keepallfalse,
  load_datasus_wrapper_simdoext_rawfalse_keepalltrue=load_datasus_wrapper_simdoext_rawfalse_keepalltrue,
  load_datasus_wrapper_simdoinf=load_datasus_wrapper_simdoinf,
  load_datasus_wrapper_simdoinf_rawfalse_keepallfalse=load_datasus_wrapper_simdoinf_rawfalse_keepallfalse,
  load_datasus_wrapper_simdoinf_rawfalse_keepalltrue=load_datasus_wrapper_simdoinf_rawfalse_keepalltrue,
  load_datasus_wrapper_simdomat=load_datasus_wrapper_simdomat,
  load_datasus_wrapper_simdomat_rawfalse_keepallfalse=load_datasus_wrapper_simdomat_rawfalse_keepallfalse,
  load_datasus_wrapper_simdomat_rawfalse_keepalltrue=load_datasus_wrapper_simdomat_rawfalse_keepalltrue,
  load_datasus_wrapper_cneslt=load_datasus_wrapper_cneslt,
  load_datasus_wrapper_cneslt_rawfalse=load_datasus_wrapper_cneslt_rawfalse,
  load_datasus_wrapper_cnesst_rawfalse=load_datasus_wrapper_cnesst_rawfalse,
  load_datasus_wrapper_cnesdc_rawfalse=load_datasus_wrapper_cnesdc_rawfalse,
  load_datasus_wrapper_cneseq_rawfalse=load_datasus_wrapper_cneseq_rawfalse,
  load_datasus_wrapper_cnessr_rawfalse=load_datasus_wrapper_cnessr_rawfalse,
  load_datasus_wrapper_cneshb_rawfalse=load_datasus_wrapper_cneshb_rawfalse,
  load_datasus_wrapper_cnespf_rawfalse=load_datasus_wrapper_cnespf_rawfalse,
  load_datasus_wrapper_cnesep_rawfalse=load_datasus_wrapper_cnesep_rawfalse,
  load_datasus_wrapper_cnesrc_rawfalse=load_datasus_wrapper_cnesrc_rawfalse,
  load_datasus_wrapper_cnesin_rawfalse=load_datasus_wrapper_cnesin_rawfalse,
  load_datasus_wrapper_cnesee_rawfalse=load_datasus_wrapper_cnesee_rawfalse,
  load_datasus_wrapper_cnesef_rawfalse=load_datasus_wrapper_cnesef_rawfalse,
  load_datasus_wrapper_cnesgm_rawfalse=load_datasus_wrapper_cnesgm_rawfalse,
  load_datasus_wrapper_sih=load_datasus_wrapper_sih,
  load_datasus_wrapper_sih_rawfalse=load_datasus_wrapper_sih_rawfalse,
  load_population_wrapper_country=load_population_wrapper_country,
  load_population_wrapper_country_rawfalse=load_population_wrapper_country_rawfalse,
  load_population_wrapper_state_rawfalse=load_population_wrapper_state_rawfalse,
  load_population_wrapper_municipality_rawfalse=load_population_wrapper_municipality_rawfalse,
  load_br_trade_wrapper_comexexportmun=load_br_trade_wrapper_comexexportmun,
  load_br_trade_wrapper_comexexportmun_rawfalse=load_br_trade_wrapper_comexexportmun_rawfalse,
  load_br_trade_wrapper_comexexportprod=load_br_trade_wrapper_comexexportprod,
  load_br_trade_wrapper_comexexportprod_rawfalse=load_br_trade_wrapper_comexexportprod_rawfalse,
  load_br_trade_wrapper_comeximportmun=load_br_trade_wrapper_comeximportmun,
  load_br_trade_wrapper_comeximportmun_rawfalse=load_br_trade_wrapper_comeximportmun_rawfalse,
  load_br_trade_wrapper_comeximportprod=load_br_trade_wrapper_comeximportprod,
  load_br_trade_wrapper_comeximportprod_rawfalse=load_br_trade_wrapper_comeximportprod_rawfalse,
  load_baci_wrapper_1=load_baci_wrapper_1,
  load_baci_wrapper_rawfalse=load_baci_wrapper_rawfalse,
  load_pibmunic_wrapper_country=load_pibmunic_wrapper_country,
  load_pibmunic_wrapper_country_rawfalse=load_pibmunic_wrapper_country_rawfalse,
  load_pibmunic_wrapper_state_rawfalse=load_pibmunic_wrapper_state_rawfalse,
  load_pibmunic_wrapper_municipality_rawfalse=load_pibmunic_wrapper_municipality_rawfalse,
  load_cempre_wrapper_country=load_cempre_wrapper_country,
  load_cempre_wrapper_country_rawfalse=load_cempre_wrapper_country_rawfalse,
  load_cempre_wrapper_country_rawfalse_sectorstrue=load_cempre_wrapper_country_rawfalse_sectorstrue,
  load_cempre_wrapper_state_rawfalse=load_cempre_wrapper_state_rawfalse,
  load_cempre_wrapper_municipality_rawfalse=load_cempre_wrapper_municipality_rawfalse,
  load_pam_wrapper_allcrops_country=load_pam_wrapper_allcrops_country,
  load_pam_wrapper_allcrops_country_rawfalse=load_pam_wrapper_allcrops_country_rawfalse,
  load_pam_wrapper_allcrops_state_rawfalse=load_pam_wrapper_allcrops_state_rawfalse,
  load_pam_wrapper_allcrops_municipality_rawfalse=load_pam_wrapper_allcrops_municipality_rawfalse,
  load_load_pam_wrapper_allcrops_region_rawfalse=load_load_pam_wrapper_allcrops_region_rawfalse,
  load_load_pam_wrapper_olive_country_rawfalse=load_load_pam_wrapper_olive_country_rawfalse,
  load_pevs_wrapper_siviculture_country=load_pevs_wrapper_siviculture_country,
  load_pevs_wrapper_siviculture_country_rawfalse=load_pevs_wrapper_siviculture_country_rawfalse,
  load_pevs_wrapper_siviculture_state_rawfalse=load_pevs_wrapper_siviculture_state_rawfalse,
  load_pevs_wrapper_siviculture_municipality_rawfalse=load_pevs_wrapper_siviculture_municipality_rawfalse,
  load_pevs_wrapper_siviculture_region_rawfalse=load_pevs_wrapper_siviculture_region_rawfalse,
  load_pevs_wrapper_forestcrops_country_rawfalse=load_pevs_wrapper_forestcrops_country_rawfalse,
  load_pevs_wrapper_siviculturearea_country_rawfalse=load_pevs_wrapper_siviculturearea_country_rawfalse,
  load_ppm_wrapper_livestock_country=load_ppm_wrapper_livestock_country,
  load_ppm_wrapper_livestock_country_rawfalse=load_ppm_wrapper_livestock_country_rawfalse,
  load_ppm_wrapper_livestock_state_rawfalse=load_ppm_wrapper_livestock_state_rawfalse,
  load_ppm_wrapper_livestock_municipality_rawfalse=load_ppm_wrapper_livestock_municipality_rawfalse,
  load_ppm_wrapper_livestock_region_rawfalse=load_ppm_wrapper_livestock_region_rawfalse,
  load_ppm_wrapper_sheep_country_rawfalse=load_ppm_wrapper_sheep_country_rawfalse,
  load_ppm_wrapper_animal_orig_country_rawfalse=load_ppm_wrapper_animal_orig_country_rawfalse,
  load_ppm_wrapper_cow_country_rawfalse=load_ppm_wrapper_cow_country_rawfalse,
  load_ppm_wrapper_aquaculture_country_rawfalse=load_ppm_wrapper_aquaculture_country_rawfalse,
  load_sigmine_wrapper_1=load_sigmine_wrapper_1,
  load_sigmine_wrapper_rawfalse=load_sigmine_wrapper_rawfalse
 )



# This list does require googledrive
wrapper_functions_list_googledrive <- list(
  load_imazon_wrapper_1=load_imazon_wrapper_1,
  load_imazon_wrapper_rawfalse=load_imazon_wrapper_rawfalse,
  load_mapbiomas_wrapper_cover0_state=load_mapbiomas_wrapper_cover0_state,
  load_mapbiomas_wrapper_cover0_municipality_rawfalse=load_mapbiomas_wrapper_cover0_municipality_rawfalse,
  load_mapbiomas_wrapper_cover0_state_rawfalse=load_mapbiomas_wrapper_cover0_state_rawfalse,
  load_mapbiomas_wrapper_cover1_state_rawfalse=load_mapbiomas_wrapper_cover1_state_rawfalse,
  load_mapbiomas_wrapper_cover2_state_rawfalse=load_mapbiomas_wrapper_cover2_state_rawfalse,
  load_mapbiomas_wrapper_cover3_state_rawfalse=load_mapbiomas_wrapper_cover3_state_rawfalse,
  load_mapbiomas_wrapper_cover4_state_rawfalse=load_mapbiomas_wrapper_cover4_state_rawfalse,
  load_mapbiomas_wrapper_covernone_state_rawfalse=load_mapbiomas_wrapper_covernone_state_rawfalse,
  load_mapbiomas_wrapper_transition_state_rawfalse=load_mapbiomas_wrapper_transition_state_rawfalse,
  load_mapbiomas_wrapper_transition_state=load_mapbiomas_wrapper_transition_state,
  load_mapbiomas_wrapper_transition_municipality=load_mapbiomas_wrapper_transition_municipality,
  load_mapbiomas_wrapper_regeneration_state_rawfalse=load_mapbiomas_wrapper_regeneration_state_rawfalse,
  load_mapbiomas_wrapper_regeneration_state=load_mapbiomas_wrapper_regeneration_state,
  load_mapbiomas_wrapper_regeneration_municipality=load_mapbiomas_wrapper_regeneration_municipality,
  load_mapbiomas_wrapper_irrigation_state=load_mapbiomas_wrapper_irrigation_state,
  load_mapbiomas_wrapper_irrigation_state_rawfalse=load_mapbiomas_wrapper_irrigation_state_rawfalse,
  load_mapbiomas_wrapper_irrigation_biome_rawfalse=load_mapbiomas_wrapper_irrigation_biome_rawfalse,
  load_mapbiomas_wrapper_grazing_rawfalse=load_mapbiomas_wrapper_grazing_rawfalse,
  load_mapbiomas_wrapper_grazing=load_mapbiomas_wrapper_grazing,
  load_mapbiomas_wrapper_mining_country=load_mapbiomas_wrapper_mining_country,
  load_mapbiomas_wrapper_mining_country_rawfalse=load_mapbiomas_wrapper_mining_country_rawfalse,
  load_mapbiomas_wrapper_mining_indigenous_rawfalse=load_mapbiomas_wrapper_mining_indigenous_rawfalse,
  load_mapbiomas_wrapper_mining_municipality_rawfalse=load_mapbiomas_wrapper_mining_municipality_rawfalse,
  load_mapbiomas_wrapper_mining_biome_rawfalse=load_mapbiomas_wrapper_mining_biome_rawfalse,
  load_mapbiomas_wrapper_mining_state_rawfalse=load_mapbiomas_wrapper_mining_state_rawfalse,
  load_mapbiomas_wrapper_water_state=load_mapbiomas_wrapper_water_state,
  load_mapbiomas_wrapper_water_biome_rawfalse=load_mapbiomas_wrapper_water_biome_rawfalse,
  load_mapbiomas_wrapper_water_state_rawfalse=load_mapbiomas_wrapper_water_state_rawfalse,
  load_mapbiomas_wrapper_water_municipality_rawfalse=load_mapbiomas_wrapper_water_municipality_rawfalse,
  load_mapbiomas_wrapper_fire_state=load_mapbiomas_wrapper_fire_state,
  load_mapbiomas_wrapper_fire_state_rawfalse=load_mapbiomas_wrapper_fire_state_rawfalse,
  load_mapbiomas_wrapper_fire_municipality_rawfalse=load_mapbiomas_wrapper_fire_municipality_rawfalse,
  load_seeg_wrapper_state=load_seeg_wrapper_state,
  load_seeg_wrapper_country=load_seeg_wrapper_country,
  load_seeg_wrapper_municipality=load_seeg_wrapper_municipality,
  load_seeg_wrapper_farming_country_rawfalse=load_seeg_wrapper_farming_country_rawfalse,
  load_seeg_wrapper_farming_state_rawfalse=load_seeg_wrapper_farming_state_rawfalse,
  load_seeg_wrapper_farming_municipality_rawfalse=load_seeg_wrapper_farming_municipality_rawfalse,
  load_seeg_wrapper_industry_country_rawfalse=load_seeg_wrapper_industry_country_rawfalse,
  load_seeg_wrapper_industry_state_rawfalse=load_seeg_wrapper_industry_state_rawfalse,
  load_seeg_wrapper_industry_municipality_rawfalse=load_seeg_wrapper_industry_municipality_rawfalse,
  load_seeg_wrapper_energy_country_rawfalse=load_seeg_wrapper_energy_country_rawfalse,
  load_seeg_wrapper_energy_state_rawfalse=load_seeg_wrapper_energy_state_rawfalse,
  load_seeg_wrapper_energy_municipality_rawfalse=load_seeg_wrapper_energy_municipality_rawfalse,
  load_seeg_wrapper_land_country_rawfalse=load_seeg_wrapper_land_country_rawfalse,
  load_seeg_wrapper_land_state_rawfalse=load_seeg_wrapper_land_state_rawfalse,
  load_seeg_wrapper_land_municipality_rawfalse=load_seeg_wrapper_land_municipality_rawfalse,
  load_seeg_wrapper_residuals_country_rawfalse=load_seeg_wrapper_residuals_country_rawfalse,
  load_seeg_wrapper_residuals_state_rawfalse=load_seeg_wrapper_residuals_state_rawfalse,
  load_seeg_wrapper_residuals_municipality_rawfalse=load_seeg_wrapper_residuals_municipality_rawfalse,
  load_iema_wrapper_rawfalse=load_iema_wrapper_rawfalse,
  load_iema_wrapper_2=load_iema_wrapper_2,
  load_aneel_wrapper_generation=load_aneel_wrapper_generation,
  load_aneel_wrapper_generation_rawfalse=load_aneel_wrapper_generation_rawfalse,
  load_aneel_wrapper_development_rawfalse=load_aneel_wrapper_development_rawfalse,
  load_aneel_wrapper_enterprises_rawfalse=load_aneel_wrapper_enterprises_rawfalse,
  load_epe_wrapper_energy_perclass_state=load_epe_wrapper_energy_perclass_state,
  load_epe_wrapper_energy_perclass_state_rawfalse=load_epe_wrapper_energy_perclass_state_rawfalse,
  load_epe_wrapper_energy_perclass_state_rawfalse_subsystem=load_epe_wrapper_energy_perclass_state_rawfalse_subsystem,
  load_epe_wrapper_national_energy_rawfalse=load_epe_wrapper_national_energy_rawfalse
)  


###########################################################################
# TEST THE FUNCTIONS ------------------------------------------------------
###########################################################################


# Test the wrapper functions
#Here, I do parallelization of embarassingly parallel processes, given each function
#I want to test is independent of any other and that each of the repetitive processes takes long
#For that, I use future_lapply() instead of lapply()
#parallelization
results_future <- future_lapply(wrapper_functions_list, function(wrapper_func) { 
  library(datazoom.amazonia)
  wrapper_func()
})

#parallelization with the list of functions that do not require google drive 
# results_future_nogoogledrive <- future_lapply(wrapper_functions_list_nogoogledrive, function(wrapper_func) {
#   library(datazoom.amazonia)
#   wrapper_func()
# })

#parallelization with the list of functions that require google drive 
# results_future_googledrive <- future_lapply(wrapper_functions_list_googledrive, function(wrapper_func) {
#   library(datazoom.amazonia)
#   wrapper_func()
# })

#no parallelization
#results <- lapply(wrapper_functions_list, function(wrapper_func) {
#  wrapper_func()
#})


###########################################################################
# EXPORT THE RESULTS TO "results.xlsx" ------------------------------------
###########################################################################

# # Print the results with function names
# for (name in names(wrapper_functions_list)) {
#   cat("Result for", name, ":", results[[name]], "\n")
# }
# 

  # Create a data frame to store the results
  result_df <- data.frame(FunctionName = character(0), Result = character(0),Status = character(0), stringsAsFactors = FALSE)
  
  # Loop through the function names and results
  for (name in names(wrapper_functions_list)) {
    result <- paste(results_future[[name]])
    status <- ifelse(result == "Success", "Success", "Not Success")
    
    
    # Add the data to the data frame
    result_df <- rbind(result_df, data.frame(FunctionName = name, Result = result, Status=status,stringsAsFactors = FALSE))
  }

  
  # Calculate the statistics
  success_count <- sum(result_df$Status == "Success")
  not_success_count <- sum(result_df$Status == "Not Success")
  percentage_success <- (success_count / ( success_count + not_success_count ))*100
  percentage_success <- sprintf("%.2f", percentage_success)
  
  
  # Create a summary data frame
  summary_df <- data.frame(
    Result = c("Success Count:", "Not Success Count:", "Success rate:"),
    Status = c(success_count, not_success_count, paste(percentage_success, "%")),
    stringsAsFactors = FALSE
  )
  
  
   # Define the Excel file name
   excel_file <- "results.xlsx"
   
  #  Load the existing Excel file
   existing_wb <- loadWorkbook(excel_file) 
  
  # Get the current date and format it as "YYYY-MM-DD"
  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  
  # Create the worksheet name
  worksheet_name <- paste("Results")
  
  # Add the worksheet
  #addWorksheet(existing_wb, worksheet_name)
  
  # Write Data 
  writeData(existing_wb, sheet = worksheet_name, x = result_df, startCol = 2, startRow = 8, colNames = FALSE)
  
  # Write the summary data frame to cell F4 on the same worksheet
  writeData(existing_wb, sheet = worksheet_name, x = summary_df, startCol = 6, startRow = 8, colNames = FALSE)
  
  # Write the current date to cell I8 on the same worksheet
  writeData(existing_wb, sheet = worksheet_name, x = current_date, startCol = 9, startRow = 8)
  
  # Save the modified workbook, overwriting the existing file
  saveWorkbook(existing_wb, excel_file, overwrite = TRUE)
  
  # Print a message to confirm the file was saved
  cat("Results have been saved to", excel_file, "\n")
  
  # Print the statistics
  cat("Results have been saved to", excel_file, "\n")
  cat("Number of Success: ", success_count, "\n")
  cat("Number of Not Success: ", not_success_count, "\n")
  cat("Sucess rate: ", percentage_success, "\n")
  



  
  