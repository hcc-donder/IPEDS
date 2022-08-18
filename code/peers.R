library(readr)

get_peer_group <- function(connection, fname){
    mdb_HD_yr <- paste0("HD", ipeds_year)
    mdb_vartable_yr <- paste0("vartable", params$ipeds_data_year)
    mdb_valuesets_yr <- paste0("valuesets", params$ipeds_data_year)
  
    source(fname) # load the peer description
    
    ##################################################
    # assemble the data to filter on
    ##################################################
    
    df_ich <- dbReadTable(connection, mdb_HD_yr) %>%
        select(UNITID, OPEID, INSTNM, SECTOR, C18SZSET, CCBASIC, ICLEVEL, INSTSIZE, STABBR) %>% 
        mutate(OPEID = as.integer(substr(OPEID,1,6))) # convert to 6-digit OPE for comparison
  
    # accreditor dashboard, indexed by OPEID
    accreditors <- read_csv("data/Institutional-Performance-by-Accreditor_2021-07-12.csv", guess_max = 1e5) %>% 
        select(ACCRED = Abbreviation, Accreditor = `Accreditor name`, OPEID = `Six-digit OPEID`) 
        
    df_ich <- df_ich %>% 
        left_join(accreditors) %>% # connect the data by OPEID
        distinct()    # no duplicates, just to be sure
    #df_ich$Accreditor <- "UNKNOWN"

    ##################################################
    # filter UNITID on the critera 
    ##################################################
  
    # start with all institutions as default peer group, about 6800 institutions
    peers <- df_ich
  
    if (!exists("my_peers_only")) {
        my_peers_only <- FALSE
    }

    if(!my_peers_only){
        # filter on the peer list
        if(!is.null(my_SECTOR)){
            peers <- peers %>% 
            filter(SECTOR %in% my_SECTOR)
        }
        
        # filter on the accreditor
        if(!is.null(my_accreditor)){
            peers <- peers %>% 
            filter(ACCRED %in% my_accreditor)
        }
        
        # filter on the carnegie size and residential status
        if(!is.null(my_C18SZSET)){
            peers <- peers %>% 
            filter(C18SZSET %in% my_C18SZSET)
        }
        
        # filter on the Carnegie basic classification
        if(!is.null(my_CCBASIC)){
            peers <- peers %>% 
            filter(CCBASIC %in% my_CCBASIC)
        }
        
        # filter on the level
        if(!is.null(my_ICLEVEL)){
            peers <- peers %>% 
            filter(ICLEVEL %in% my_ICLEVEL)
        }
        
        # filter on the size
        if(!is.null(my_INSTSIZE)){
            peers <- peers %>% 
            filter(INSTSIZE %in% my_INSTSIZE)
        }
        
        # filter on the state
        if(!is.null(my_STABBR)){
            peers <- peers %>% 
            filter(STABBR %in% my_STABBR)
        }
    }

    # include schools on the peer list provided, regardless of other filters
    if(!is.null(my_peers)){
        peers_list <-  df_ich %>% 
            filter(UNITID %in% my_peers)
        
        if(my_peers_only){
            peers <- peers_list
        } else {
            # add these to the existing list
            peers <- rbind(peers, peers_list) %>% 
                distinct()  # leave out duplicates
        }
    }
  
    # ensure that the target institution is included 
    if (!(my_UNITID %in% peers$UNITID)) {
        peers <- rbind(df_ich %>% filter(UNITID == my_UNITID), peers)
    }
    
    #################################################
    # Convert IPEDS codes to meaningful descriptions
    #################################################
    df_ich_vars <- dbReadTable(connection, mdb_vartable_yr) %>%
        filter( TableName == mdb_HD_yr ) %>%
        select( varName, varTitle )
    
    df_ich_values <- dbReadTable(connection, mdb_valuesets_yr) %>% # code table from the data dictionary
      filter( TableName == mdb_HD_yr ) %>%
      select(varName, codeValue=Codevalue, valueLabel) 
    
    pretty_vars <- peers %>% 
        select(UNITID, SECTOR, C18SZSET, CCBASIC, ICLEVEL, INSTSIZE, STABBR) %>% # numeric codes
        gather(varName, codeValue, -UNITID) %>% # make a long version
        mutate(codeValue = as.character(codeValue)) %>% 
        left_join(df_ich_values) %>% 
        left_join(df_ich_vars) %>% 
        mutate(var = paste0("(",varName,") ",varTitle),
            val = paste0("(",codeValue,") ",valueLabel)) %>% 
        select(UNITID, var, val) %>% 
        spread(var,val)
    
    # return the pretty version
    peers %>% 
        select(UNITID, OPEID, INSTNM, Accreditor) %>% 
        left_join(pretty_vars) %>% 
        mutate(Peer = (UNITID != my_UNITID)) %>% # add a flag to indicate the target institution
        return()

    }
