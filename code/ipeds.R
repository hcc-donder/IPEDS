#' Get IPEDS tuition
#' @param idbc Database connector
#' @param UNITIDs IPEDS school IDs. If NULL, gets everything
#' @return A dataframe with UNITID, Year, Tuition, Fees, RoomBoard for undergrad
#' @export
get_tuition <- function(idbc, UNITIDs = NULL){
  
  # find all the tables
  tnames <- odbc::dbListTables(idbc, table_name = "ic%_ay")
  
  out <- data.frame()
  
  for(tname in tnames) {
    
    # use the fall near, not the year on the table name
    year <- as.integer(substr(tname, 3,6)) -1
    
    if(year == 2005) next # it's screwed up
    
    # some of the column names are lowercase, sigh
    df <-  try(
      tbl(idbc, tname)  %>%
        select(UNITID,
               Tuition = TUITION1,
               Fees    = FEE1,
               RoomBoard = chg5ay2), TRUE
    )
    
    if(inherits(df, "try-error")) {
      
      df <-  try(
        tbl(idbc, tname)  %>%
          select(UNITID,
                 Tuition = TUITION1,
                 Fees    = FEE1,
                 RoomBoard = CHG5AY2), TRUE
      )
    }
    
    if (!is.null(UNITIDs)) {
      df <- df %>% filter(UNITID %in% !!UNITIDs)
    }
    
    df <- df %>% collect %>%
      mutate(Year = year) %>%
      mutate(Tuition = as.integer(Tuition),
             Fees = as.integer(Fees),
             RoomBoard = as.integer(RoomBoard),
             TotalCost = Tuition + Fees + RoomBoard)
    
    out <- rbind(out, df)
  }
  
  return(out)
}

#' Get financials
#' @idbc Database connector
#' @UNTIDs vector of UNITIDs to retrieve
#' @return Dataframe with endowment, revenue and other statistics
#' @export
get_finances <- function(idbc, UNITIDs = NULL){
  
  # find all the tables
  tnames <- odbc::dbListTables(idbc, table_name = "f%_f2")
  
  out <- data.frame()
  
  for(tname in tnames) {
    
    # use the fall near, not the year on the table name
    year <- 2000 + as.integer(substr(tname, 2,3))
    
    
    if(year < 2008){
      df <- tbl(idbc,tname) %>%
        select(UNITID,
               F2A05, # Total restricted net assets
               F2A05A, # Permanently restricted net assets included in total restricted net assets
               F2B01,  # total revenue
               F2B02,  # total expenses
               Total_unrestricted_net_assets = F2A04,
               Total_expenses = F2E131,
               Change_in_net_assets = F2B04,
               Net_assets = F2B05,
               Cost_instruction = F2E011,
               Cost_acad_support = F2E041,
               Cost_student_serv = F2E051,
               Cost_inst_support = F2E061,
               Cost_aux_ent      = F2E071,
               Cost_total_salary = F2E132,
               Cost_total_benefit = F2E133,
               Cost_operations    = F2E134,
               Cost_depreciation  = F2E135,
               Cost_interest      = F2E136,
               Cost_other         = F2E137,
               Endowment          = F2H02
        )
      
      if (!is.null(UNITIDs)) {
        df <- df %>% filter(UNITID %in% !!UNITIDs)
      }
      
      df <- df %>%
        collect() %>%
        mutate( Year = year,
                Temporarily_restricted_net_assets = F2A05 - F2A05A,
                Property_Plant_Equipment_net_depreciation = NA,
                Debt_Property_Plant_Equipment = NA,
                Net_total_revenues = F2B01 - F2B02,
                Endowment = as.integer(Endowment)) %>%
        select(-F2A05, -F2A05A, -F2B01, -F2B02)
      
    } else {
      
      df <- tbl(idbc,tname) %>%
        select(UNITID,
               Total_unrestricted_net_assets = F2A04,
               Temporarily_restricted_net_assets = F2A05B,
               Property_Plant_Equipment_net_depreciation = F2A19,
               Debt_Property_Plant_Equipment = F2A03A,
               Total_expenses = F2E131,
               Change_in_net_assets = F2B04,
               Net_assets = F2B05,
               Net_total_revenues = F2D182,
               Cost_instruction = F2E011,
               Cost_acad_support = F2E041,
               Cost_student_serv = F2E051,
               Cost_inst_support = F2E061,
               Cost_aux_ent      = F2E071,
               Cost_total_salary = F2E132,
               Cost_total_benefit = F2E133,
               Cost_operations    = F2E134,
               Cost_depreciation  = F2E135,
               Cost_interest      = F2E136,
               Cost_other         = F2E137,
               Endowment          = F2H02) %>%
        mutate(Year = year,
               Endowment = as.numeric(Endowment))
      
      if (!is.null(UNITIDs)) {
        df <- df %>% filter(UNITID %in% !!UNITIDs)
      }
      
      df <- df %>% collect()
    }
    out <- rbind(out, df)
    
  } # end of loop
  return(out)
}

#' Get grad rates
#' @param idbc Database connector
#' @param UNITIDs Array of identifiers
#' @return Dataframe with year, unitid, 6 year undergrad rate
#' @export
get_grad_rates <- function(idbc, UNITIDs = NULL){
  # expects ef<Year>d.csv
  
  # find all the tables
  tnames <- odbc::dbListTables(idbc, table_name = "gr20__")
  
  out <- data.frame()
  
  for(tname in tnames) {
    
    # use the fall near, not the year on the table name
    year <- as.integer(substr(tname, 3,6)) - 1
    
    
    if(year < 2007){
      
      df <- tbl(idbc,tname) %>%
        filter(GRTYPE %in% c(2,3,29,30)) %>% # 2 is cohort size, 3 is grads in 150%
        select(UNITID,
               GRTYPE,
               N =    GRRACE24 )
      
      if (!is.null(UNITIDs)) {
        df <- df %>% filter(UNITID %in% !!UNITIDs)
      }
      
      df <- df %>% collect() %>%
        mutate(GRTYPE = case_when(
          GRTYPE %in% c(29,30) ~ as.integer(GRTYPE - 27),
          TRUE ~ as.integer(GRTYPE)
        )) %>%
        spread(GRTYPE, N, sep = "_") %>%
        mutate(Grad_rate = GRTYPE_3 / GRTYPE_2,
               Year = year) %>%
        select(UNITID, Year, Grad_rate, Cohort_size = GRTYPE_2)
      
    } else { # Year >= 2008
      
      df <- tbl(idbc,tname) %>%
        filter(GRTYPE %in% c(2,3,29,30)) %>% # 2 is cohort size, 3 is grads in 150%
        select(UNITID,
               GRTYPE,
               N =     GRTOTLT)
      
      if (!is.null(UNITIDs)) {
        df <- df %>% filter(UNITID %in% !!UNITIDs)
      }
      
      df <- df %>% collect() %>%
        mutate(GRTYPE = case_when(
          GRTYPE %in% c(29,30) ~ as.integer(GRTYPE - 27),
          TRUE ~ as.integer(GRTYPE)
        )) %>%
        spread(GRTYPE, N, sep = "_") %>%
        mutate(Grad_rate = GRTYPE_3 / GRTYPE_2,
               Year = year) %>%
        select(UNITID, Year, Grad_rate, Cohort_size = GRTYPE_2)
      
    }
    out <- rbind(out, df)
  }
  return(out)
}

#' Get financial aid
#' @param idbc IPEDS database connection
#' @param ids Optional list of UNITIDs to filter to
#' @export

get_fa_info <- function(idbc, ids = NULL){
  # input should be a sfa file, eg.
  # df <- read_csv("data/IPEDS/2017/sfa1617.csv", guess_max = 5000) %>%
  # adds a given year to the df
  
  # find all the tables
  tnames <- odbc::dbListTables(idbc, table_name = "sfa%")
  
  # leave out the sfav ones
  tnames <- tnames[!str_detect(tnames,"SFAV")]
  
  tname_prefixes <- unique(substr(tnames,1,7))
  
  out <- data.frame()
  
  for(tname_prefix in tname_prefixes) {
    Year <- 2000 + as.integer(substr(tname_prefix,4,5))
    
    tname_set <- tnames[ str_detect(tnames, tname_prefix)]
    
    # start by joining all the tables in the set
    df <- tbl(idbc,tname_set[1])
    
    if(length(tname_set) == 2) df <- df %>% left_join(tbl(idbc,tname_set[2]))
    if(length(tname_set) == 3) df <- df %>% left_join(tbl(idbc,tname_set[3]))
    
    if(Year < 2010) { ############ Old ones lacked some data
      
      df <- df %>%
        select(UNITID,
               N_undergraduates   = SCFA2,
               N_fall_cohort      = SCFA1N,
               Percent_PELL       = FGRNT_P,
               N_inst_aid         = IGRNT_N,
               Avg_inst_aid       = IGRNT_A,
               P_inst_aid         = IGRNT_P
               
        )
      
      if (!is.null(ids)) {
        df <- df %>% filter(UNITID %in% !!ids)
      }
      
      df <- df %>%
        collect() %>%
        mutate(
          Avg_net_price_0k   = NA,
          Avg_net_price_30k  = NA,
          Avg_net_price_48k  = NA,
          Avg_net_price_75k  = NA,
          Avg_net_price_110k = NA,
          Year = Year)
      
    } else {
      df <- df %>%
        select(UNITID,
               N_undergraduates   = SCFA2,
               N_fall_cohort      = SCFA1N,
               Percent_PELL       = FGRNT_P,
               N_inst_aid         = IGRNT_N,
               Avg_inst_aid       = IGRNT_A,
               P_inst_aid         = IGRNT_P,
               Avg_net_price_0k   = NPT412,
               Avg_net_price_30k  = NPT422,
               Avg_net_price_48k  = NPT432,
               Avg_net_price_75k  = NPT442,
               Avg_net_price_110k = NPT452
        )
      
      if (!is.null(ids)) {
        df <- df %>% filter(UNITID %in% !!ids)
      }
      
      df <- df %>%
        collect() %>%
        mutate(Year = Year)
    }
    out <- rbind(out,df)
  }
  return(out)
}



#' Get IPEDS enrollment
#' @description Use the efYEAR tables to retrieve student counts.
#' @param idbc Database connector for the IPEDS database.
#' @param UNITIDs The schools to retrieve, or NULL (default) for all.
#' @param StudentTypeCode One or more numeric codes to specify what type of student
#' to filter to. See Details for, well, details. Defaults to 1, all students.
#' @details The StudentTypeCode is or more of
#' 1	= All students total
#' 2	= All students, Undergraduate total
#' 3	= All students, Undergraduate, Degree/certificate-seeking total
#' 4	= All students, Undergraduate, Degree/certificate-seeking, First-time
#' 5	= All students, Undergraduate, Other degree/certificate-seeking
#' 19	= All students, Undergraduate, Other degree/certifcate-seeking, Transfer-ins
#' 20	= All students, Undergraduate, Other degree/certifcate-seeking, Continuing
#' 11	= All students, Undergraduate, Non-degree/certificate-seeking
#' 12	= All students, Graduate
#' 21	= Full-time students total
#' 22	= Full-time students, Undergraduate total
#' 23	= Full-time students, Undergraduate, Degree/certificate-seeking total
#' 24	= Full-time students, Undergraduate, Degree/certificate-seeking, First-time
#' 25	= Full-time students, Undergraduate, Degree/certificate-seeking, Other degree/certificate-seeking
#' 39	= Full-time students, Undergraduate, Other degree/certifcate-seeking, Transfer-ins
#' 40	= Full-time students, Undergraduate, Other degree/certifcate-seeking, Continuing
#' 31	= Full-time students, Undergraduate, Non-degree/certificate-seeking
#' 32	= Full-time students, Graduate
#' 41	= Part-time students total
#' 42	= Part-time students, Undergraduate total
#' 43	= Part-time students, Undergraduate, Degree/certificate-seeking total
#' 44	= Part-time students, Undergraduate, Degree/certificate-seeking, First-time
#' 45	= Part-time students, Undergraduate, Degree/certificate-seeking, Other degree/certificate-seeking
#' 59	= Part-time students, Undergraduate, Other degree/certifcate-seeking, Transfer-ins
#' 60	= Part-time students, Undergraduate, Other degree/certifcate-seeking, Continuing
#' 51	= Part-time students, Undergraduate, Non-degree/certificate-seeking
#' 52	= Part-time students, Graduate
#' @return A dataframe with UNITID, Year, Total, Men, Women, White, Black, Hispanic, and NRAlien.
#' @export
ipeds_get_enrollment <- function(idbc, UNITIDs = NULL, StudentTypeCode = 1){
  
  # find all the tables
  tnames <- odbc::dbListTables(idbc, table_name = "ef%a")
  
  out <- data.frame()
  
  for(tname in tnames) {
    
    # use the fall near, not the year on the table name
    year <- as.integer(substr(tname, 3,6)) -1
    
    # the varnames switched in 2008
    if(tname <= "EF2007A"){
      
      tdf <- tbl(idbc, tname) %>%
        filter( EFALEVEL %in% !!StudentTypeCode) %>%
        select(UNITID, Total = EFRACE24,
               Men = EFRACE15,
               Women = EFRACE16,
               White = EFRACE22,
               Black = EFRACE18,
               Hispanic = EFRACE21,
               NRAlien = EFRACE17)
    } else if(tname <= "EF2009A"){
      
      tdf <- tbl(idbc, tname) %>%
        filter( EFALEVEL %in% !!StudentTypeCode) %>%
        select(UNITID, Total = EFTOTLT,
               Men = EFTOTLM,
               Women = EFTOTLW,
               White = EFRACE22,
               Black = EFRACE18,
               Hispanic = EFRACE21,
               NRAlien = EFNRALT)
      
    } else {
      
      tdf <- tbl(idbc, tname) %>%
        filter( EFALEVEL %in% !!StudentTypeCode) %>%
        select(UNITID, Total = EFTOTLT,
               Men = EFTOTLM,
               Women = EFTOTLW,
               White = EFWHITT,
               Black = EFBKAAT,
               Hispanic = EFHISPT,
               NRAlien = EFNRALT)
    }
    
    if(!is.null(UNITIDs)) {
      tdf <- tdf %>%
        filter(UNITID %in% !!UNITIDs)
    }
    
    tdf <- tdf %>%
      collect() %>%
      mutate(Year = year)
    
    out <- rbind(out, tdf)
  }
  
  return(out)
}


get_cohort_stats <- function(Year){
  # expects ef<Year>d.csv
  
  fname <- paste0(ipeds_path(Year),
                  "ef",
                  Year,
                  "d.csv")
  
  df <- read_csv(fname, guess_max = 5000) %>%
    mutate(Year = Year) %>%
    select(Year,
           UNITID,
           Cohort_size = GRCOHRT,
           Retention   = RET_PCF)
  return(df)
}

get_admit_funnel <- function(Year){
  # expects ic<Year>d.csv prior to 2014, adm thereafter
  fname <- paste0(ipeds_path(Year),
                  ifelse(Year <2014,"ic","adm"),
                  Year,
                  ".csv")
  
  df <- read_csv(fname, guess_max = 5000) %>%
    mutate(Year = Year) %>%
    select(Year,
           UNITID,
           Male_apps      = APPLCNM,
           Female_apps    = APPLCNW,
           Male_admits    = ADMSSNM,
           Female_admits  = ADMSSNW,
           Male_enrolls   = ENRLFTM,
           Female_enrolls = ENRLFTW,
           SATVR25,  # standardized test quartiles
           SATVR75,
           SATMT25,
           SATMT75,
           ACTCM25,
           ACTCM75,
           ACTEN25,
           ACTEN75,
           ACTMT25,
           ACTMT75
    ) %>%
    mutate(
      Male_apps      = as.integer(Male_apps),
      Female_apps    = as.integer(Female_apps),
      Male_admits    = as.integer(Male_admits),
      Female_admits  = as.integer(Female_admits),
      Male_enrolls   = as.integer(Male_enrolls),
      Female_enrolls = as.integer(Female_enrolls),
      Total_apps = Male_apps + Female_apps,
      Total_admits = Male_admits + Female_admits,
      Total_enrolls = Male_enrolls + Female_enrolls,
      Acceptance_rate = Total_admits / Total_apps,
      Yield_rate     = Total_enrolls / Total_admits,
      SATVR25 = as.integer(SATVR25),  # standardized test quartiles
      SATVR75 = as.integer(SATVR75),
      SATMT25 = as.integer(SATMT25),
      SATMT75 = as.integer(SATMT75),
      ACTCM25 = as.integer(ACTCM25),
      ACTCM75 = as.integer(ACTCM75),
      ACTEN25 = as.integer(ACTEN25),
      ACTEN75 = as.integer(ACTEN75),
      ACTMT25 = as.integer(ACTMT25),
      ACTMT75 = as.integer(ACTMT75)
    )
  return(df)
}

#' Get institutional characteristics
#' @param idbc A database connector
#' @param year The year to get
#' @return a dataframe with institutional characteristics
#' @export
get_characteristics <- function(idbc, year){
  
  # find all the tables
  tnames <- odbc::dbListTables(idbc, table_name = "hd%")
  
  for(tname in tnames) {
    
    # use the fall near, not the year on the table name
    file_year <- as.integer(substr(tname, 3,6)) -1
    
    if(year != file_year) next
    
    tdf <- tbl(idbc, tname) %>%
      select(UNITID,
             OPEID,
             InstName = INSTNM,
             City = CITY,
             State = STABBR,
             Zipcode = ZIP,
             FIPS,
             CensusRegion = OBEREG,
             SECTOR,
             ICLEVEL,
             CONTROL,
             HLOFFER,
             UGOFFER,
             GROFFER,
             HDEGOFR1,
             DEGGRANT,
             HBCU,
             HOSPITAL,
             MEDICAL,
             TRIBAL) %>%
      collect()
  }
  
  return(tdf)
}
