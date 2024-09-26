# remove objects from the environment
#rm(list = ls())

#df1 <- read.csv('sa_liscr_mod_liver_result_df6_575.csv')

#df2 <- read.csv('sa_nLiver_scr_mod_liver_result_df7_575.csv')

# studyid_vector_liver35 <- as.character(df1$STUDYID)
#df2 <- read.csv('nLiver_result_df7.csv')
#studyid_vector_nLiver35 <- as.character(df2$STUDYID)
get_livertox_scores <- function(dbtoken) {


start_time <- Sys.time()

#libraries
library(matrixStats)
library(dplyr)
library(sendigR)
library(tidyverse)
library(tidyr)
library(this.path)
library(reshape2)
library(stringr)
library(purrr)

#Set File Path
#homePath <- dirname(this.path())
#setwd(homePath)

# #Database Load
# dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     #dbPath = "/opt/rstudio/users/MdAminulIslam.Prodhan/DataCentral.db",
#                                     dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
#                                     dbCreate = FALSE)
# Select the PARALLEL STUDY from the DATABASE
parallel_StudyID <- sendigR::getStudiesSDESIGN(dbtoken, studyDesignFilter = "PARALLEL")


# Filtering the Repeat dose Studies
repeat_dose_STUDYIDs <- sendigR::genericQuery(dbtoken, queryString = "SELECT DISTINCT STUDYID
                             FROM ts
                             WHERE TSPARMCD = 'SSTYP'
                             AND TSVAL IN ('REPEAT DOSE TOXICITY', 'REPEAT-DOSE TOXICITY', 'Repeat-Dose Toxicity', 'Repeat Dose Toxicity')",
                                              queryParams = NULL)

# COMMON STUDYIDs from PARALLEL STUDYIDs and repeat_dose_STUDYIDs.....
parallel_repeat_dose_intersect <- intersect(parallel_StudyID$STUDYID,repeat_dose_STUDYIDs$STUDYID)

# converting "parallel_repeat_dose_intersect" to a data frame
parallel_repeat_dose_intersec_df <- data.frame(STUDYID = parallel_repeat_dose_intersect)

# convert to a vector( selected_studies should be always vector)
#selected_studies <- as.vector(parallel_repeat_dose_intersec_df$STUDYID)

# get the studies for the rat only species
rat_STUDYID_ts_species <- sendigR::genericQuery(dbtoken, queryString = "SELECT STUDYID, TSPARMCD, TSVAL
                             FROM ts
                             WHERE TSPARMCD = 'SPECIES' AND UPPER(TSVAL) LIKE '%RAT%'", queryParams = NULL)



selected_studies <- as.vector(rat_STUDYID_ts_species$STUDYID)
#select



#selected_studies <- "1017-3581"

#selected_studies <- c("1470536")

#selected_studies <- c("1017-3581", "1470536", "P19-025-RD")
# master liverToBW_df

master_liverToBW <-  data.frame(STUDYID = NA, liverToBW = NA)

# Master LB list
master_LB_list <- data.frame(STUDYID = NA, avg_alb_zscore = NA, avg_ast_zscore = NA, avg_alp_zscore = NA,
                             avg_alt_zscore = NA, avg_bili_zscore = NA, avg_ggt_zscore = NA)
# master_MI_list
master_MI_list <- list()

# Create FOUR SCORE DATA FRAME for "LiverToBodyweight" , "LB" & "MI" Score
FOUR_Liver_Score <-  data.frame(STUDYID = NA, liverToBW = NA, LB_score = NA, MI_score = NA, scored_liverToBW = NA, scored_LBScore = NA)


# Initialize an empty data frame to store the names of studies with errors
Error_studies <- list()

# Initialize the master error data frame to have the details of the errors
master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)


for (j in selected_studies){

  print(j)

  # Initialize a flag variable at the start of each iteration
  first_block_success <- TRUE

  # First Block with its own tryCatch for CompileData
  tryCatch({
    #Pull relevant domain data for each domain
    bw <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM BW WHERE STUDYID = (:1)",
                                queryParams = j)
    dm <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM DM WHERE STUDYID = (:1)",
                                queryParams = j)
    ds <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM DS WHERE STUDYID = (:1)",
                                queryParams = j)
    ex <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM EX WHERE STUDYID = (:1)",
                                queryParams = j)
    fw <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM FW WHERE STUDYID = (:1)",
                                queryParams = j)
    lb <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM LB WHERE STUDYID = (:1)",
                                queryParams = j)
    mi <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM MI WHERE STUDYID = (:1)",
                                queryParams = j)
    om <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM OM WHERE STUDYID = (:1)",
                                queryParams = j)
    ts <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TS WHERE STUDYID = (:1)",
                                queryParams = j)
    ta <-sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TA WHERE STUDYID = (:1)",
                               queryParams = j)
    tx <-sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TX WHERE STUDYID = (:1)",
                               queryParams = j)
    pooldef <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM POOLDEF WHERE STUDYID = (:1)",
                                     queryParams = j)
    pp <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM PP WHERE STUDYID = (:1)",
                                queryParams = j)

    print(";;;;;;;;;;;;;;;;;;;;;;;;")
    print(dim(mi))
    p_mi <- mi
    print(";;;;;;;;;;;;;;;;;;;;;;;;")

    # # # Combine into list of assigned name
    # studyData <- list('bw' = bw, 'dm' = dm, 'ds' = ds, 'ex' = ex, 'fw' = fw, 'pooldef' = pooldef,
    #                   'lb' = lb, 'mi' = mi, 'om' = om, 'ts' = ts, 'ta' = ta, 'tx' = tx, 'pp' = pp)


    #<><><><><><><><><><><><><>..Creation of compilation data...(Compilation of DM Data)..<><><><><><><>.......
    # Step-1 :: # CompileData is basically the compilation of DM data
    CompileData <- data.frame(STUDYID = NA, Species = NA, USUBJID = NA, SEX = NA, ARMCD = NA, SETCD = NA)

    #Pull all of the relevant DM Data
    Species <- ts$TSVAL[which(ts$TSPARMCD == "SPECIES")]
    TRTName <- ts$TSVAL[which(ts$TSPARMCD == "TRT")]
    Duration <-ts$TSVAL[which(ts$TSPARMCD == "DOSDUR")]

    # Convert duration to days
    if (any(grepl("W",Duration)) ==TRUE){
      days <- as.numeric(gsub("\\D","",Duration))*7
    } else if (any(grepl("M",Duration)) == TRUE){
      days <- as.numeric(gsub("\\D","",Duration))*7*30
    } else {
      days <- as.numeric(gsub("\\D","",Duration))
    }
    Duration <- paste0(days,"D")

    # Make StudyID
    STUDYID <- unique(ts$STUDYID)

    # CREATE DM DATA
    DMData <- data.frame(STUDYID = rep(STUDYID, length(dm$USUBJID)), # creating DM DATA
                         Species = rep(Species, length(dm$USUBJID)),
                         USUBJID = dm$USUBJID,
                         SEX = dm$SEX,
                         ARMCD = dm$ARMCD,
                         SETCD = dm$SETCD)

    #Add to CompileData
    CompileData <- rbind(CompileData, DMData)

    # Remove NAs from the first line
    CompileData <- na.omit(CompileData)

    # Create a copy of CompileData which will not changes with changing the CompileData
    CompileData_copy <- data.frame(CompileData)


    # # Step-2 :: # REMOVE THE RECOVERY ANIMALS from "CompileData"...<>"Recovery animals" cleaning.. using "DS domain"<><><><><><>
    # cat("Displaying unique values in ds$DSDECOD before filtering :\n")
    # print(unique(ds$DSDECOD))

    # filter for specific "DSDECOD" values...( Keep the mentioned four ) ......................................................
    filtered_ds <- ds %>%
      filter(DSDECOD %in% c('TERMINAL SACRIFICE', 'MORIBUND SACRIFICE', 'REMOVED FROM STUDY ALIVE', 'NON-MORIBUND SACRIFICE'))

    # # check the unique value in "DSDECOD" column
    # cat("Displaying unique values in ds$DSDECOD after filtering:\n")
    # print(unique(filtered_ds$DSDECOD))

    #~~~~~~~~~~~~~~~~~~~Filter "CompileData" to keep rows where USUBJID is in "filtered_ds"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Filter "CompileData" to keep rows where USUBJID is in "filtered_ds" meaning removing recovery animals
    recovery_cleaned_CompileData <- CompileData %>%
      filter(USUBJID %in% filtered_ds$USUBJID)


    # Step-3 :: # REMOVE THE TK ANIMALS IF SPECIES IS RAT from the "recovery_cleaned_CompileData" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initialize an empty data frame to store the results
    tK_animals_df <- data.frame(PP_PoolID = character(), STUDYID = character(), USUBJID = character(), POOLID = character(),  stringsAsFactors = FALSE)

    # Initialize a data frame to keep track of studies with no POOLID
    no_poolid_studies <- data.frame(STUDYID = character(), stringsAsFactors = FALSE)

    # check for the species [# Check if the current study is a rat] [{# Convert Species to lowercase for case-insensitive comparison}]

    Species_lower <- tolower(Species)

    if ("rat" %in% Species_lower) {
      # Create TK individuals for "Rat" studies [# Retrieve unique pool IDs (TKPools) from pp table]
      TKPools <- unique(pp$POOLID)

      # Check if TKPools is not empty
      if (length(TKPools) > 0) {
        # For each pool ID in TKPools, retrieve corresponding rows from pooldef table
        for (pool_id in TKPools) {
          pooldef_data <- pooldef[pooldef$POOLID == pool_id, ]

          # Create a temporary data frame if pooldef_data is not empty
          if (nrow(pooldef_data) > 0) {
            temp_df <- data.frame(PP_PoolID = pool_id,
                                  STUDYID = pooldef_data$STUDYID,
                                  USUBJID = pooldef_data$USUBJID,
                                  POOLID = pooldef_data$POOLID,
                                  stringsAsFactors = FALSE)

            # Append the temporary data frame to the results data frame
            tK_animals_df <- rbind(tK_animals_df, temp_df)
          }
        }
      } else {
        # Retrieve STUDYID for the current study
        current_study_id <- bw$STUDYID[1]

        # Add study to no_poolid_studies dataframe
        no_poolid_studies <- rbind(no_poolid_studies, data.frame(STUDYID = current_study_id, stringsAsFactors = FALSE))
      }

    } else {
      # Create a empty data frame named "tK_animals_df"
      tK_animals_df <- data.frame(PP_PoolID = character(), STUDYID = character(), USUBJID = character(), POOLID = character(),  stringsAsFactors = FALSE)
    }


    # Subtract "TK_animals_df" data from the "recovery_cleaned_CompileData"
    tk_recovery_cleaned_CompileData <- recovery_cleaned_CompileData[!(recovery_cleaned_CompileData$USUBJID %in% tK_animals_df$USUBJID),]

    cleaned_CompileData <- tk_recovery_cleaned_CompileData


    ##' @<><><><><><><><>.."vehicle" and "HD animals" selection "for"cleaned_CompileData".....<><><><>..:::::::::::::::::<><><><>

    # tx table  filter by TXPARMCD
    cleaned_CompileData_filtered_tx <- tx %>% filter(TXPARMCD == "TRTDOS")


    #::::::::::::::::::::::::::::: Assign the dose level for "cleaned_CompileData_filtered_tx"  ::::::::::::::::::::::::::::::

    # Step 1:  Create a unified separator pattern
    cleaned_CompileData_separator_pattern <- ";|\\||-|/|:|,"

    # Split and expand the TXVAL column, and add row_state
    cleaned_CompileData_expanded_tx_row_state_added <- cleaned_CompileData_filtered_tx %>%
      mutate(
        is_split = str_detect(TXVAL, cleaned_CompileData_separator_pattern), # Flag rows that will be split
        TXVAL = strsplit(as.character(TXVAL), cleaned_CompileData_separator_pattern)
      ) %>%
      unnest(TXVAL) %>%
      mutate(
        TXVAL = as.numeric(TXVAL),
        row_state = ifelse(is_split, "new_row", "old_row") # If the row was split, mark as new_row, else old_row
      ) %>%
      select(-is_split) # Remove the is_split column

    #<><><><><><><><><><><><><><><><>:::::::::::::...Adding dose_ranking...::::::::<><><><><><><><><><><><><><><><><><><><><><>

    # Initialize an empty data frame for dose_ranking
    dose_ranking <- data.frame()

    dose_ranking_prob_study <- data.frame()

    if (TRUE) {
      study_data <- cleaned_CompileData_expanded_tx_row_state_added

      # Check if all TXVAL values are NA for the STUDYID
      if (all(is.na(study_data$TXVAL))) {
        dose_ranking_prob_study <- rbind(dose_ranking_prob_study, study_data)
      }
      # Check if all SETCD values are the same for the STUDYID
      else if (n_distinct(study_data$SETCD) == 1) {
        dose_ranking_prob_study <- rbind(dose_ranking_prob_study, study_data)
      } else {
        # Process for lowest TXVAL
        lowest_txval <- min(study_data$TXVAL, na.rm = TRUE)
        lowest_data <- study_data %>%
          filter(TXVAL == lowest_txval) %>%
          arrange(SETCD)

        if (nrow(lowest_data) == 1) {
          dose_ranking <- rbind(dose_ranking, lowest_data)

        } else {
          # Select the first old_row if available, else the first new_row
          selected_lowest <- filter(lowest_data, row_state == "old_row") %>%
            slice(1)
          if (nrow(selected_lowest) > 0) {
            dose_ranking <- rbind(dose_ranking, selected_lowest)
          } else {
            selected_lowest <- filter(lowest_data, row_state == "new_row") %>%
              slice(1)
            dose_ranking <- rbind(dose_ranking, selected_lowest)
          }
        }

        # Process for highest TXVAL
        highest_txval <- max(study_data$TXVAL, na.rm = TRUE)
        highest_data <- study_data %>%
          filter(TXVAL == highest_txval) %>%
          arrange(SETCD)

        if (nrow(highest_data) == 1) {
          dose_ranking <- rbind(dose_ranking, highest_data)
        }else if (nrow(highest_data) > 1) {
          selected_highest <- filter(highest_data, row_state == "old_row") %>%
            slice(1)
          if (nrow(selected_highest) > 0) {
            dose_ranking <- rbind(dose_ranking, selected_highest)
          } else {
            # If no old_row is found, select the first new_row
            selected_highest <- filter(highest_data, row_state == "new_row") %>%
              slice(1)
            if (nrow(selected_highest) > 0) {
              dose_ranking <- rbind(dose_ranking, selected_highest)

            }
          }
        }
      }
    }

    #<><><><><><><><>::::::::::::::::.......ADD DOSE_RANKING column in "selected_rows" data frame......<><><><><><><><>::::::::::
    DOSE_RANKED_selected_rows <- dose_ranking %>%
      group_by(STUDYID) %>%
      mutate(
        MinTXVAL = min(TXVAL),
        MaxTXVAL = max(TXVAL),
        DOSE_RANKING = case_when(
          TXVAL == MinTXVAL & TXVAL == MaxTXVAL ~ "Both",
          TXVAL == MinTXVAL ~ "vehicle",
          TXVAL == MaxTXVAL ~ "HD",
          TRUE ~ "Intermediate"
        )
      ) %>%
      select(-MinTXVAL, -MaxTXVAL) %>%
      ungroup()

    #<><><><><><><><><><><><>........Merging "DOSE_RANKED_selected_rows" and "cleaned_CompileData" data framed..........<><><><><>
    DOSE_RANKED_plus_cleaned_CompileData <- inner_join(cleaned_CompileData, DOSE_RANKED_selected_rows, by = c("STUDYID", "SETCD"))

    # rename the Data frame
    master_CompileData1 <- DOSE_RANKED_plus_cleaned_CompileData [,c("STUDYID", "USUBJID","Species", "SEX", "DOSE_RANKING", "SETCD")]

    # Rename the "DOSE_RANKING" column to ARMCD
    # Rename "DOSE_RANKING" to "ARMCD" in master_CompileData
    master_CompileData <- master_CompileData1 %>% rename(ARMCD = DOSE_RANKING)

    # Create a copy of master_CompileData
    master_CompileData_copy <- master_CompileData

  }, error = function(e) {
    # Handling errors
    message("Error in BodyWeight Data Compilation calculation: ", e$message)

    # Log the error
    error_block1 <- data.frame(STUDYID = j, Block = "compileData", ErrorMessage = e$message, Time = Sys.time(), stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block1)

    # Set the flag to FALSE to indicate the first block failed
    first_block_success <<- FALSE

  })

  # Check the flag to decide whether to proceed to the next iteration of the loop
  if (!first_block_success) {

    # Append STUDYID  to the error_studies list
    Error_studies <<- c(Error_studies, j)

    next
  }
  # end of master_CompileData calculation

  tryCatch({

    # Initialize the "FOUR_Liver_Score" [[# Add a new row for the current STUDYID in FOUR_Liver_Score]]

    new_row_in_four_liver_scr <- data.frame(STUDYID = unique(ts$STUDYID), liverToBW = NA,  LB_score = NA, MI_score = NA, scored_liverToBW = NA, scored_LBScore = NA)
    FOUR_Liver_Score <- rbind(FOUR_Liver_Score, new_row_in_four_liver_scr)
  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in FOUR_Liver_Score: ", e$message)

    # Log the error
    error_block_flscrdf <- data.frame(STUDYID = unique(ts$STUDYID), Block = "FOUR_Liver_Score", ErrorMessage = e$message, Time = Sys.time(), stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block_flscrdf)

  })


  #-------------------------------------BodyWeight_zScore-------------------------------------------------------------
  tryCatch({

    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    #............................. "BodyWeight_zScore" .....calculation...........................................
    #................................. Initial BW weight calculation..............................................

    StudyInitialWeights <- data.frame("STUDYID" = NA, "USUBJID" = NA,
                                      "BWSTRESN" = NA, "VISITDY" = NA) #, "BWNOMDY" = NA, "BWNOMLBL"= NA, "BWBLFL" = NA)

    # Initialize dataframe for unmatched USUBJIDs
    UnmatchedUSUBJIDs <- data.frame("USUBJID" = character(), stringsAsFactors = FALSE)


    if (TRUE){
      # Get unique USUBJIDs in the current study
      unique_subjids <- unique(bw$USUBJID)

      for (currentUSUBJID in unique_subjids) {

        # Initialize an empty dataframe for this subject
        SubjectInitialWeight <- data.frame()

        # Data (all rows) for the current USUBJID
        subj_data <- bw[which(bw$USUBJID == currentUSUBJID), ]

        # for any row if  VISITDY column data is empty replace it with the corresponding values from BWDY column
        subj_data <- subj_data %>% mutate(VISITDY = ifelse(is.na(VISITDY) | VISITDY == "", BWDY, VISITDY))

        # 1. Check if VISITDY == 1 is present
        SubjectInitialWeight <- subj_data[subj_data$VISITDY == 1,
                                          c("STUDYID", "USUBJID", "BWSTRESN", "VISITDY")] #,"BWNOMDY","BWNOMLBL","BWBLFL")]


        # 2. If no initial weight with VISITDY == 1,  try VISITDY < 0
        if (nrow(SubjectInitialWeight) == 0) {
          negative_visits <- subj_data[subj_data$VISITDY < 0, ]
          if (nrow(negative_visits) > 0) {
            closest_row <- which.min(abs(negative_visits$VISITDY))
            SubjectInitialWeight <- negative_visits[closest_row, c("STUDYID", "USUBJID", "BWSTRESN", "VISITDY")] #,"BWNOMDY","BWNOMLBL","BWBLFL")]
          }
        }

        # 3. If no initial weight with VISITDY == 1 VISITDY < 0 , try 1<VISITDY<=5
        if (nrow(SubjectInitialWeight) == 0) {
          five_visitdy <- subj_data[subj_data$VISITDY > 1 & subj_data$VISITDY <= 5, ]

          if (nrow(five_visitdy) > 0) {
            # If there are rows where 1 < VISITDY <= 5, choose the one with the minimum VISITDY value
            closest_row_five <- which.min(five_visitdy$VISITDY)
            SubjectInitialWeight <- five_visitdy[closest_row_five, c("STUDYID", "USUBJID", "BWSTRESN", "VISITDY")] #, "BWNOMDY", "BWNOMLBL", "BWBLFL")]
          }
        }

        # 4. If no rows, if VISITDY  >5 , set BWSTRESN value 0
        if (nrow(SubjectInitialWeight) == 0) {
          null_visitdy_large_bw <- subj_data[subj_data$VISITDY > 5, ]

          if (nrow(null_visitdy_large_bw) > 0) {
            # Set BWSTRESN to 0 for the rows that meet the condition
            null_visitdy_large_bw$BWSTRESN <- 0

            # Choose the row with the minimum BWDY value greater than 5
            closest_row_null_visitdy <- which.min(null_visitdy_large_bw$VISITDY)
            SubjectInitialWeight <- null_visitdy_large_bw[closest_row_null_visitdy, c("STUDYID", "USUBJID", "BWSTRESN", "VISITDY")] #, "BWNOMDY", "BWNOMLBL", "BWBLFL")]
          }
        }
        # If SubjectInitialWeight is still empty, add currentUSUBJID to UnmatchedUSUBJIDs
        if (nrow(SubjectInitialWeight) == 0) {
          UnmatchedUSUBJIDs <- rbind(UnmatchedUSUBJIDs, data.frame(USUBJID = currentUSUBJID, stringsAsFactors = FALSE))
        }
        # Store Values to "StudyInitialWeights" data frame
        StudyInitialWeights <- rbind(StudyInitialWeights,SubjectInitialWeight)
      }
    }

    # remove the first row (initialized with NAs)
    StudyInitialWeights <- StudyInitialWeights[-1, ]


    #..................Check for the presence of duplicate "USUBJID" in "StudyInitialWeights"................

    # Check for any duplicate USUBJID
    duplicates_exist <- any(duplicated(StudyInitialWeights$USUBJID))

    # Output result
    if (duplicates_exist) {
      print("There are duplicate USUBJID values in StudyInitialWeights")
    } else {
      print("No duplicate USUBJID values found in StudyInitialWeights")
    }

    #  see the duplicate values
    if (duplicates_exist) {
      duplicate_usubjids <- StudyInitialWeights$USUBJID[duplicated(StudyInitialWeights$USUBJID)]
      print(duplicate_usubjids)
    }

    #........................ Duplicate rows handling....................................................

    # Removing duplicates based on specific column(s)
    # only the first occurrence of each unique USUBJID will be kept, and subsequent duplicates will be removed
    StudyInitialWeights <- StudyInitialWeights[!duplicated(StudyInitialWeights$USUBJID), ]


    #............................................................................................................
    #................................. Final day "StudyBodyWeights" calculation............................................

    #.............................(StudyBodyWeights)-(TERMBW)-(BoDY Weigt)... calculation.......................

    # Initialize "StudyBodyWeights" empty data frame
    StudyBodyWeights <- data.frame("STUDYID" = NA, "USUBJID" = NA, "BWTESTCD" = NA,
                                   "BWSTRESN" = NA, "VISITDY" = NA) #, "BWNOMDY" = NA, "BWNOMLBL"= NA, "BWBLFL" = NA)

    # Initialize dataframe for unmatched USUBJIDs
    BodyWeights_UnmatchedUSUBJIDs <- data.frame("USUBJID" = character(), stringsAsFactors = FALSE)


    if(TRUE) {
      # Get unique USUBJIDs in the current study
      unique_bw_subjids <- unique(bw$USUBJID)

      for (current_bw_USUBJID in unique_bw_subjids) {

        # Initialize an empty dataframe for this subject
        SubjectBodyWeight <- data.frame()

        # Data (all rows) for the current USUBJID
        subj_bw_data <- bw[which(bw$USUBJID == current_bw_USUBJID), ]

        # for any row if  VISITDY column data is empty replace it with the corresponding values from BWDY column
        subj_bw_data <- subj_bw_data %>% mutate(VISITDY = ifelse(is.na(VISITDY) | VISITDY == "", BWDY, VISITDY))


        # 1. Check if BWTESTCD == TERMBW is present
        SubjectBodyWeight <- subj_bw_data[subj_bw_data$BWTESTCD == "TERMBW",
                                          c("STUDYID", "USUBJID", "BWTESTCD","BWSTRESN", "VISITDY")] #,"BWNOMDY","BWNOMLBL","BWBLFL")]

        # If BWTESTCD == TERMBW not present,
        # 2. If no  BWTESTCD == TERMBW,try  VISITDY > 5" ??????????????????????should we do that ???????????????..............
        if (nrow(SubjectBodyWeight) == 0) {
          positive_bw_VISITDY <- subj_bw_data[subj_bw_data$VISITDY > 5 , ]

          if (nrow(positive_bw_VISITDY) > 0) {
            # choose the one with the maximum VISITDY value
            max_VISITDY <- which.max(positive_bw_VISITDY$VISITDY)
            SubjectBodyWeight <- positive_bw_VISITDY[max_VISITDY, c("STUDYID", "USUBJID", "BWTESTCD", "BWSTRESN", "VISITDY")] #, "BWNOMDY", "BWNOMLBL", "BWBLFL")]
          }
        }

        # If SubjectInitialWeight is still empty, add currentUSUBJID to UnmatchedUSUBJIDs
        if (nrow(SubjectBodyWeight) == 0) {
          BodyWeights_UnmatchedUSUBJIDs <- rbind(BodyWeights_UnmatchedUSUBJIDs, data.frame(USUBJID = current_bw_USUBJID, stringsAsFactors = FALSE))
        }

        # Store Values to "StudyBodyWeights" data frame
        StudyBodyWeights  <- rbind(StudyBodyWeights ,SubjectBodyWeight)
      }
    }

    # Remove the first row (initialized with NAs)
    StudyBodyWeights <- StudyBodyWeights[-1, ]


    #....................Check for the presence of duplicate "USUBJID" in "StudyBodyWeights"......................

    # Check for any duplicate USUBJID
    stbw_duplicates_exist <- any(duplicated(StudyBodyWeights$USUBJID))

    # Output result
    if (stbw_duplicates_exist) {
      print("There are duplicate USUBJID values in StudyBodyWeights")
    } else {
      print("No duplicate USUBJID values found in StudyBodyWeights")
    }

    # See the duplicate values
    if (stbw_duplicates_exist) {
      stbw_duplicate_usubjids <- StudyBodyWeights$USUBJID[duplicated(StudyBodyWeights$USUBJID)]
      print( stbw_duplicate_usubjids)
    }

    #........................ Duplicate "StudyBodyWeights" rows handling.......................................

    # Removing duplicates based on specific column(s)
    # only the first occurrence of each unique USUBJID will be kept, and subsequent duplicates will be removed
    # StudyBodyWeights <- StudyBodyWeights[!duplicated(StudyBodyWeights$USUBJID), ]

    # number of unique USUBJID
    unique_StudyBodyWeights_USUBJID <- length(unique(StudyBodyWeights$USUBJID))
    print(unique_StudyBodyWeights_USUBJID)
    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>><><><><><><><><>><><><><><><><><><><><>

    #.............Substract TK animals from the "StudyInitialWeights" and StudyBodyWeights" data frame...........
    tk_less_StudyBodyWeights <- StudyBodyWeights[!(StudyBodyWeights$USUBJID %in% tK_animals_df$USUBJID),]

    # Substract TK animals from the "StudyInitialWeights" data frame...........................................
    tk_less_StudyInitialWeights <- StudyInitialWeights[!(StudyInitialWeights$USUBJID %in% tK_animals_df$USUBJID),]


    # Rename columns in StudyInitialWeights by adding "_Init" suffix
    names(tk_less_StudyInitialWeights) <- ifelse(names(tk_less_StudyInitialWeights) == "USUBJID",
                                                 "USUBJID",
                                                 paste0(names(tk_less_StudyInitialWeights), "_Init"))

    # ... Inner join..."StudyInitialWeights" and StudyBodyWeights".............................................
    #...... an inner join on the USUBJID column for TK_less (StudyInitialWeights & StudyBodyWeights )...
    joined_BW_df <- merge(tk_less_StudyBodyWeights, tk_less_StudyInitialWeights, by = "USUBJID")


    #.................... Select specific columns from joined_BW_df .........................................
    BW_df_selected_column <- joined_BW_df[, c("USUBJID", "STUDYID", "BWSTRESN", "BWSTRESN_Init")]

    # .....................Add "ARMCD","SETCD","SEX" to "selected_df"........................................
    STUDYID_less_master_CompileData <- master_CompileData[, c("USUBJID", "ARMCD","SETCD","SEX")]
    BW_df_merged_ARMCD <- merge(BW_df_selected_column, STUDYID_less_master_CompileData, by = "USUBJID")

    #......."Recovery animals" cleaning.. from "BW_df_merged_ARMCD" .........................................
    # #  master_CompileData is already......TK animals & Recovery animal cleaned...............................

    # all_present <- all(BW_df_merged_ARMCD$USUBJID %in% master_CompileData$USUBJID)
    # print(all_present)


    #::::::::::::::::::::::::::::: "BWzScore Calculation" ::::::::::::::::::::::::::::::::::::::::::::::::::::

    # Create the finalbodyweight column in merged_recovery_tk_cleaned_dose_ranked_df data frame
    bwzscore_BW_df <- BW_df_merged_ARMCD %>%
      mutate(finalbodyweight = abs(BWSTRESN - BWSTRESN_Init))

    # Create the BWZSCORE column:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    bwzscore_BW <- bwzscore_BW_df %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle = mean(finalbodyweight[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle = sd(finalbodyweight[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        BWZSCORE = (finalbodyweight - mean_vehicle) / sd_vehicle
      ) %>%
      select(-mean_vehicle, -sd_vehicle)  # Optionally remove the mean_vehicle and sd_vehicle columns

    # Filter and select specific columns
    HD_BWzScore <- bwzscore_BW %>%
      filter(ARMCD == "HD") %>%
      select(STUDYID, USUBJID, SEX, BWZSCORE)

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in BodyWeight_zScore calculation: ", e$message)

    # Log the error
    error_block2 <- data.frame(STUDYID = unique(ts$STUDYID), Block = "BWZscore", ErrorMessage = e$message, Time = Sys.time(), stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block2)

  })

  # end of BodyWeight_zScore calculation
  print(" end~~~~~of~~~~~BodyWeight_zScore~~~~~~~calculation~~~~~~~~")


  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #<><><><><><><><><><><><><><><><><><>"OM_DATA"-(Liver_Organ to Body Weight zScore) <><><><><><><><><><><><><><><>
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  tryCatch({

    # Initialize data frames to store the OrganWeights_Liver data
    OrganWeights_Liver <- data.frame(USUBJID = character(0), OMSPEC = character(0), OMSTRESN = numeric(0), OMTEST = character(0))

    # Extract data for the current STUDYID
    StudyData_current_liver <- om

    # Pull index of the LIVER data
    Studyidx_liver <- which(str_detect(StudyData_current_liver$OMSPEC, "LIVER"))

    # Pull relevant OM Data for LIVER
    OMD_liver <- StudyData_current_liver[Studyidx_liver, c("USUBJID", "OMSPEC", "OMSTRESN", "OMTEST")]

    # Append to the OrganWeights_Liver  data frame
    OrganWeights_Liver <- rbind(OrganWeights_Liver, OMD_liver)

    # Filter the OrganWeights_Liver data frame
    OrganWeights_Liver_Weight <- OrganWeights_Liver %>%
      filter(OMTEST == "Weight")

    # Filter the OrganWeights_Liver_Weight data frame and select specific columns ("USUBJID", "OMSTRESN")
    OrganWeights_Liver_Weight_Selected_Col <- OrganWeights_Liver_Weight %>%
      filter(OMTEST == "Weight") %>%
      select(USUBJID, OMSTRESN)

    #<><><><>... Remove TK animals and Recovery animals from "OrganWeights_Liver_Weight_Selected_Col"..<><><>....
    #<><><><><><><><> master_CompileData is free of TK animals and Recovery animals<><><><><><><><><><><><><><>
    # Filter the data frame for removing recovery and TK animals.....................................
    OrganWeights_Liver_filtered <- OrganWeights_Liver_Weight_Selected_Col %>%
      filter(USUBJID %in% master_CompileData$USUBJID)

    # Perform a left join to match USUBJID and get ARMCD
    OrganWeights_Liver_with_ARMCD <- OrganWeights_Liver_filtered %>%
      left_join(master_CompileData %>% select(STUDYID, USUBJID, ARMCD), by = "USUBJID")

    # Add "BodyWeight" data to the "OrganWeights_Liver_with_ARMCD" data frame
    OrganWeights_Liver_to_BWeight <- OrganWeights_Liver_with_ARMCD %>%
      left_join(bwzscore_BW %>% select(USUBJID, finalbodyweight), by = "USUBJID") %>%
      mutate(liverToBW = OMSTRESN / finalbodyweight)

    # "liver_organ to BodyWeight" zscore calcualtion.............................................................
    # Create the "LiverZSCORE" column :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    liver_zscore_df <- OrganWeights_Liver_to_BWeight %>%
      group_by(STUDYID) %>%
      # Replace Inf and -Inf with NA in liverToBW
      mutate(liverToBW = replace(liverToBW, is.infinite(liverToBW), NA)) %>%
      # Calculate mean and standard deviation for "vehicle" ARMCD
      mutate(
        mean_vehicle_liverToBW = mean(liverToBW[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_liverToBW = sd(liverToBW[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%

      # Calculate z-score
      mutate(
        liverToBW_zscore = (liverToBW - mean_vehicle_liverToBW) / sd_vehicle_liverToBW
      ) %>%
      select(-mean_vehicle_liverToBW, -sd_vehicle_liverToBW) %>%  # Optionally remove the mean_vehicle and sd_vehicle columns
      select(STUDYID, USUBJID,liverToBW_zscore, ARMCD) %>%
      # Convert z-score to its absolute value
      mutate(liverToBW_zscore = abs(liverToBW_zscore))

    # Filter and select specific columns
    HD_liver_zscore <- liver_zscore_df %>%
      filter(ARMCD == "HD") %>%
      select(STUDYID, USUBJID, liverToBW_zscore, ARMCD)

    # Create final_liverToBW_df for the current STUDYID by averaging..................................
    final_liverToBW_df <- HD_liver_zscore %>%
      group_by(STUDYID) %>%
      mutate(liverToBW_zscore = replace(liverToBW_zscore, is.infinite(liverToBW_zscore), NA)) %>%
      summarize(avg_liverToBW_zscore = mean(liverToBW_zscore, na.rm = TRUE))%>%
      mutate(avg_liverToBW_zscore = abs(avg_liverToBW_zscore))


    # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame..........................................
    # Create "liverToBW_df" for FOUR_Liver_Score
    liverToBW_df <- final_liverToBW_df %>% rename(liverToBW = avg_liverToBW_zscore)

    # add liverToBW_df to master_liverToBW
    master_liverToBW <- bind_rows(master_liverToBW, liverToBW_df)

    # Extract the liverToBW value for the current STUDYID from liverToBW_df
    calculated_liverToBW_value <- liverToBW_df$liverToBW[liverToBW_df$STUDYID == unique(ts$STUDYID)]

    # Update the liverToBW value in FOUR_Liver_Score for the current STUDYID
    FOUR_Liver_Score$liverToBW[FOUR_Liver_Score$STUDYID == unique(ts$STUDYID)] <- calculated_liverToBW_value

    # Score the liverToBW values in the FOUR_Liver_Score data frame and fill "scored_liverToBW" column
    FOUR_Liver_Score$scored_liverToBW <- ifelse(FOUR_Liver_Score$liverToBW >= 3, 3,
                                                ifelse(FOUR_Liver_Score$liverToBW >= 2, 2,
                                                       ifelse(FOUR_Liver_Score$liverToBW >= 1, 1, 0)))


  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in Liver_Organ to Body Weight zScore: ", e$message)

    # Log the error
    error_block3 <- data.frame(STUDYID = unique(ts$STUDYID), Block = "LiverToBW", ErrorMessage = e$message, Time = Sys.time(), stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block3)
  })

  print ( "end of~~~~Liver~~To~~BodyWeight~~~zScore~~~~~calculation" )


  #<><><><><><><><><><><><><><><><><><>"""LB"""" zscoring <><><><><><><><><><><><><><><>
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  tryCatch({

    organTESTCDlist <- list('LIVER' = c('SERUM | ALT',
                                        'SERUM | AST',
                                        'SERUM | ALP',
                                        'SERUM | GGT',
                                        'SERUM | BILI',
                                        'SERUM | ALB',
                                        'PLASMA | ALT',
                                        'PLASMA | AST',
                                        'PLASMA | ALP',
                                        'PLASMA | GGT',
                                        'PLASMA | BILI',
                                        'PLASMA | ALB',
                                        'WHOLE BLOOD | ALT',
                                        'WHOLE BLOOD | AST',
                                        'WHOLE BLOOD | ALP',
                                        'WHOLE BLOOD | GGT',
                                        'WHOLE BLOOD | BILI',
                                        'WHOLE BLOOD | ALB'))


    #Make LB Data Data Frame to Hold Information
    LBData <- data.frame("STUDYID" = NA,"USUBJID" = NA,"LBSPEC" = NA,"LBTESTCD" = NA,
                         "LBSTRESN" = NA, "VISITDY" = NA)

    # for (Name in unique(filtered_combined_lb$STUDYID)) {
    # Filter the data for the current STUDYID
    study_data_LB <- lb

    # Check if LBDY column exists and process accordingly
    if ("LBDY" %in% names(study_data_LB)) {
      LBD <- study_data_LB %>%
        filter(LBDY >= 1) %>%
        select(STUDYID,USUBJID, LBSPEC, LBTESTCD, LBSTRESN, LBDY)

      colnames(LBD) <- c("STUDYID", "USUBJID", "LBSPEC", "LBTESTCD", "LBSTRESN", "VISITDY")

      # Convert LBCAT to LBSPEC if LBSPEC is NA
      if (all(is.na(LBD$LBSPEC))) {
        LBD$LBSPEC <- study_data_LB$LBCAT[study_data_LB$LBDY >= 1]

        if (any(c("HEMATOLOGY", "Hematology","hematology") %in% levels(LBD$LBSPEC))){
          levels(LBD$LBSPEC)[match(c("HEMATOLOGY", "Hematology","hematology"),
                                   levels(LBD$LBSPEC))] <- "WHOLE BLOOD"
        }
        if (any(c("CLINICAL CHEMISTRY","Clinical Chemistry") %in% levels(LBD$LBSPEC))){
          levels(LBD$LBSPEC)[match(c("CLINICAL CHEMISTRY","Clinical Chemistry"),
                                   levels(LBD$LBSPEC))] <- "SERUM"
        }
        if (any(c("URINALYSIS","Urinalysis") %in% levels(LBD$LBSPEC))){
          levels(LBD$LBSPEC)[match(c("URINALYSIS","Urinalysis"),
                                   levels(LBD$LBSPEC))] <- "URINE"
        }
      }
    } else {
      # If LBDY column does not exist, handle accordingly
      LBD <- study_data_LB[which(study_data_LB$VISITDY >= 1),
                           c("STUDYID","USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")]
    }

    # Add to LBData
    LBData <- rbind(LBData, LBD)

    # Remove rows with all NAs
    LBData <- na.omit(LBData)

    # Concatenate LBSPEC and LBTESTCD
    LBData$LBTESTCD <- paste(LBData$LBSPEC, LBData$LBTESTCD, sep = ' | ')

    #Remove Not Included Tests...............................................................
    # This step remove not rows matching test from ogransystem
    test_cleaned_LBData <- LBData[LBData$LBTESTCD %in% organTESTCDlist[['LIVER']],]

    # Create a new data frame with the row having the max VISITDY for each USUBJID and LBTESTCD combination
    max_visitdy_df <- test_cleaned_LBData %>%
      group_by(USUBJID, LBTESTCD) %>%
      filter(VISITDY == max(VISITDY, na.rm = TRUE)) %>%
      ungroup()
    #<><><><><><><><><><><><><><><><>... Remove TK animals and Recovery animals......<><><><><><>.............
    #<><><><><><><><> master_CompileData is free of TK animals and Recovery animals<><><><><><><><><><><><><><>
    # Remove the TK animals and Recovery animals

    LB_tk_recovery_filtered <- max_visitdy_df %>%
      filter(USUBJID %in% master_CompileData$USUBJID)

    # Perform a left join to match USUBJID and get ARMCD ## 020924
    #-inner_join() used instead of left_join()#199
    LB_tk_recovery_filtered_ARMCD <- LB_tk_recovery_filtered %>%
      inner_join(master_CompileData %>% select(USUBJID, ARMCD), by = "USUBJID")


    #::::::::::::::::::::::::::::: "zScore Calculation" for LB data::::::::::::::::::::::::::::::::::::::::::::::::::::
    # First subset the LB_tk_recovery_filtered_ARMCD data frame .....................................................

    # ........................Filtering data for each unique "LBTESTCD" value.........................

    # 1. Sub-setting for 'SERUM | ALT' data frame for "LBzScore Calculation" for...'SERUM | ALT'...........
    df_serum_alt <- LB_tk_recovery_filtered_ARMCD %>%
      filter(LBTESTCD == 'SERUM | ALT' | LBTESTCD == 'PLASMA | ALT'| LBTESTCD == 'WHOLE BLOOD | ALT')

    # calculate the zscore of 'SERUM | ALT'
    zscore_serum_alt <- df_serum_alt %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle_alt = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_alt = sd(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        alt_zscore = (LBSTRESN - mean_vehicle_alt) / sd_vehicle_alt
      )%>%
      mutate(alt_zscore = abs(alt_zscore))

    # averaged zscore per STUDYID for 'SERUM | ALT'
    serum_alt_final_zscore <- zscore_serum_alt %>%
      filter(ARMCD == "HD") %>%  # Step 1: Filter for HD
      group_by(STUDYID) %>%  # Step 2: Group by STUDYID
      summarise(
        avg_alt_zscore = mean(alt_zscore, na.rm = TRUE),  # Step 3: Average alt_zscore
        LBTESTCD = first(LBTESTCD)  # Include LBTESTCD in the summarized data
      ) %>% select (STUDYID, avg_alt_zscore )


    # 2. Sub-setting for 'SERUM | AST' data frame for "BWzScore Calculation" for...'SERUM | AST'...........
    df_serum_ast <- LB_tk_recovery_filtered_ARMCD %>%
      filter(LBTESTCD == 'SERUM | AST' | LBTESTCD == 'PLASMA | AST'| LBTESTCD == 'WHOLE BLOOD | AST')

    # calculate the zscore of 'SERUM | AST'
    zscore_serum_ast <- df_serum_ast %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle_ast = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_ast = sd(LBSTRESN[ARMCD  == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        ast_zscore = (LBSTRESN - mean_vehicle_ast) / sd_vehicle_ast
      )%>%
      mutate(ast_zscore = abs(ast_zscore))

    # averaged zscore per STUDYID for 'SERUM | AST'
    serum_ast_final_zscore <- zscore_serum_ast %>%
      filter(ARMCD  == "HD") %>%  # Step 1: Filter for HD
      group_by(STUDYID) %>%  # Step 2: Group by STUDYID
      summarise(
        avg_ast_zscore = mean(ast_zscore, na.rm = TRUE),  # Step 3: Average alt_zscore
        LBTESTCD = first(LBTESTCD)  # Include LBTESTCD in the summarized data
      )  %>% select(STUDYID, avg_ast_zscore)

    # 3........Sub-setting for 'SERUM | ALP' data frame for "BWzScore Calculation" for...'SERUM | ALP'...........
    df_serum_alp <- LB_tk_recovery_filtered_ARMCD %>%
      filter(LBTESTCD == 'SERUM | ALP'| LBTESTCD == 'PLASMA | ALP'| LBTESTCD == 'WHOLE BLOOD | ALP')

    # calculate the zscore of 'SERUM | ALP'
    zscore_serum_alp <- df_serum_alp %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle_alp = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_alp = sd(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        alp_zscore = (LBSTRESN - mean_vehicle_alp) / sd_vehicle_alp
      )%>%
      mutate(alp_zscore = abs(alp_zscore))

    # averaged zscore per STUDYID for 'SERUM | ALP'
    serum_alp_final_zscore <- zscore_serum_alp %>%
      filter(ARMCD == "HD") %>%  # Step 1: Filter for HD
      group_by(STUDYID) %>%  # Step 2: Group by STUDYID
      summarise(
        avg_alp_zscore = mean(alp_zscore, na.rm = TRUE),  # Step 3: Average alt_zscore
        LBTESTCD = first(LBTESTCD)  # Include LBTESTCD in the summarized data
      ) %>% select (STUDYID, avg_alp_zscore)

    # 4. Sub-setting for 'SERUM | GGT' data frame for "BWzScore Calculation" for...'SERUM | GGT'...........
    df_serum_ggt <- LB_tk_recovery_filtered_ARMCD %>%
      filter(LBTESTCD == 'SERUM | GGT'| LBTESTCD == 'PLASMA | GGT'| LBTESTCD == 'WHOLE BLOOD | GGT')

    # calculate the zscore of 'SERUM | GGT'
    zscore_serum_ggt <- df_serum_ggt %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle_ggt = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_ggt = sd(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        ggt_zscore = (LBSTRESN - mean_vehicle_ggt) / sd_vehicle_ggt
      )%>%
      mutate(ggt_zscore = abs(ggt_zscore))

    # averaged zscore per STUDYID for 'SERUM | GGT'
    serum_ggt_final_zscore <- zscore_serum_ggt %>%
      filter(ARMCD == "HD") %>%  # Step 1: Filter for HD
      group_by(STUDYID) %>%  # Step 2: Group by STUDYID
      summarise(
        avg_ggt_zscore = mean(ggt_zscore, na.rm = TRUE),  # Step 3: Average alt_zscore
        LBTESTCD = first(LBTESTCD)  # Include LBTESTCD in the summarized data
      ) %>% select(STUDYID, avg_ggt_zscore)

    #5.  Sub-setting for 'SERUM | BILI' data frame for "BWzScore Calculation" for...'SERUM | BILI'...........
    df_serum_bili <- LB_tk_recovery_filtered_ARMCD %>%
      filter(LBTESTCD == 'SERUM | BILI'| LBTESTCD == 'PLASMA | BILI'| LBTESTCD == 'WHOLE BLOOD | BILI')

    # calculate the zscore of 'SERUM | BILI'
    zscore_serum_bili <- df_serum_bili %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle_bili = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_bili = sd(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        bili_zscore = (LBSTRESN - mean_vehicle_bili) / sd_vehicle_bili
      )%>%
      mutate(bili_zscore = abs(bili_zscore))

    # averaged zscore per STUDYID for 'SERUM | BILI'
    serum_bili_final_zscore <- zscore_serum_bili %>%
      filter(ARMCD == "HD") %>%  # Step 1: Filter for HD
      group_by(STUDYID) %>%  # Step 2: Group by STUDYID
      summarise(
        avg_bili_zscore = mean(bili_zscore, na.rm = TRUE),  # Step 3: Average alt_zscore
        LBTESTCD = first(LBTESTCD)  # Include LBTESTCD in the summarized data
      )  %>% select(STUDYID, avg_bili_zscore)

    # 6. Sub-setting for 'SERUM | ALB' data frame for "BWzScore Calculation" for...'SERUM | ALB'...........
    df_serum_alb <- LB_tk_recovery_filtered_ARMCD %>%
      filter(LBTESTCD == 'SERUM | ALB'| LBTESTCD == 'PLASMA | ALB'| LBTESTCD == 'WHOLE BLOOD | ALB')

    # calculte the zscore of 'SERUM | ALB'
    zscore_serum_alb <- df_serum_alb %>%
      group_by(STUDYID) %>%
      mutate(
        mean_vehicle_alb = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
        sd_vehicle_alb = sd(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        alb_zscore = (LBSTRESN - mean_vehicle_alb) / sd_vehicle_alb
      )%>%
      mutate(alb_zscore = abs(alb_zscore))

    # averaged zscore per STUDYID for 'SERUM | ALB'
    serum_alb_final_zscore <- zscore_serum_alb %>%
      filter(ARMCD == "HD") %>%  # Step 1: Filter for HD
      group_by(STUDYID) %>%  # Step 2: Group by STUDYID
      summarise(
        avg_alb_zscore = mean(alb_zscore, na.rm = TRUE),  # Step 3: Average alt_zscore
        LBTESTCD = first(LBTESTCD)  # Include LBTESTCD in the summarized data
      ) %>% select(STUDYID, avg_alb_zscore)

    # Merging----------LB----zscores------------values---------------------------
    LB_zscore_merged_df <- serum_alb_final_zscore %>%
      full_join(serum_ast_final_zscore, by = "STUDYID") %>%
      full_join(serum_alp_final_zscore, by = "STUDYID") %>%
      full_join(serum_alt_final_zscore, by = "STUDYID") %>%
      full_join(serum_bili_final_zscore, by = "STUDYID") %>%
      full_join(serum_ggt_final_zscore, by = "STUDYID")


    # Replace Inf, -Inf, and NaN with NA in the selected columns
    selected_cols <- c("avg_alb_zscore", "avg_ast_zscore", "avg_alp_zscore", "avg_alt_zscore", "avg_bili_zscore", "avg_ggt_zscore")
    LB_zscore_merged_df[selected_cols] <- lapply(LB_zscore_merged_df[selected_cols], function(x) replace(x, is.infinite(x) | is.nan(x), NA))

    # add the LB_zscore_merged_df to master_LB_list

    # Add LB_zscore_merged_df to master dataframe
    master_LB_list <- bind_rows(master_LB_list, LB_zscore_merged_df)

    # Calculate the average for each row, ignoring NA values
    LB_zscore_merged_df$avg_all_LB_zscores <- rowMeans(LB_zscore_merged_df[selected_cols], na.rm = TRUE)

    # select the specific columns for calculation
    LB_all_liver_zscore_averaged <- LB_zscore_merged_df %>% select (STUDYID, avg_all_LB_zscores)


    # Assigning the new variables
    LB_final_score <- LB_all_liver_zscore_averaged

    # Append the LB_zscore to the "FOUR_Liver_Score" data frame
    # Create "LB_df" for FOUR_Liver_Score
    LB_df <- LB_final_score %>% rename(LB_score = avg_all_LB_zscores)

    # Extract the LB_score value for the current STUDYID from LB_df
    calculated_LB_value <- LB_df$LB_score[LB_df$STUDYID == unique(ts$STUDYID)]

    # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
    FOUR_Liver_Score$LB_score[FOUR_Liver_Score$STUDYID == unique(ts$STUDYID)] <- calculated_LB_value

    # Score the LB_score values in the FOUR_Liver_Score data frame and fill "scored_LBScore" column
    FOUR_Liver_Score$scored_LBScore <- ifelse(FOUR_Liver_Score$LB_score >= 3, 3,
                                              ifelse(FOUR_Liver_Score$LB_score >= 2, 2,
                                                     ifelse(FOUR_Liver_Score$LB_score >= 1, 1, 0)))

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in LB zscoring: ", e$message)

    # Log the error
    error_block4 <- data.frame(STUDYID = unique(ts$STUDYID), Block = "LB", ErrorMessage = e$message, Time = Sys.time(), stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block4)
  })

  # end of LB scoring
  print ( "~~~~~~~~~~~~~~~~~~~~~~~~~~end of~~~~ LB~~~~scoring~~~~zScore~~~calculation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" )


  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #<><><><><><><><><><><<><><><><><><>Create the "MI compile Data" ( Compilation of MI Data)<><><><><><><><><><><><>
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  tryCatch({
    # Initialize the  MI_final_score DATA FRAME
    MI_final_score <- data.frame( STUDYID = unique(ts$STUDYID), avg_MI_score = NA )

    #Make Data Frame to hold MI Information for the STUDY available in combined_mi for repeat dose
    MIData <- data.frame("USUBJID" = NA,"MISTRESC" = NA,"MISEV" = NA,
                         "MISPEC" = NA)

    # Filter the data for the current STUDYID
    mi_study_data <- mi

    #Pull all of the relevant MI Data using Grepl
    MBD <- mi_study_data[grepl("LIVER", mi_study_data$MISPEC, ignore.case = TRUE),
                         c("USUBJID", "MISTRESC","MISEV","MISPEC")]
    #Add to CompileData
    MIData <- rbind(MIData, MBD)

    # Convert empty strings to NA in the MISEV column
    MIData$MISEV[MIData$MISEV == ""] <- NA

    #MAKE NA Sev's a 0
    MIData$MISEV <-  MIData$MISEV %>% replace_na("0")
    MIData <- na.omit(MIData)

    MIData$MISTRESC <- toupper(MIData$MISTRESC)

    # # Remove the "Normal" values from the "MISTRESC" column(Subset MIData to remove rows where MISTRESC is "Normal")
    # NORMAL_DATA <- MIData[MIData$MISTRESC == "NORMAL", ] # get the rows having value "Normal"
    #
    # MIData <- MIData[MIData$MISTRESC != "NORMAL", ]

    #Convert Severity
    # Replacing various patterns in MISEV with numeric strings

    MIData$MISEV <- str_replace_all(MIData$MISEV, "\\b1\\s*OF\\s*4\\b", "2")
    MIData$MISEV <- str_replace_all(MIData$MISEV, "\\b2\\s*OF\\s*4\\b", "3")
    MIData$MISEV <- str_replace_all(MIData$MISEV, "\\b3\\s*OF\\s*4\\b", "4")
    MIData$MISEV <- str_replace_all(MIData$MISEV, "\\b4\\s*OF\\s*4\\b", "5")
    MIData$MISEV <- str_replace_all(MIData$MISEV, "1 OF 5", "1")
    MIData$MISEV <- str_replace_all(MIData$MISEV, "MINIMAL", "1")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "2 OF 5", "2")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "MILD", "2")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "3 OF 5", "3")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "MODERATE", "3")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "4 OF 5", "4")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "MARKED", "4")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "5 OF 5", "5")
    MIData$MISEV <-  str_replace_all(MIData$MISEV, "SEVERE", "5")

    testing_MIData <- MIData

    # Converting MISEV to an ordered factor
    MIData$MISEV <- ordered(MIData$MISEV, levels= c("0","1", "2", "3", "4","5"))

    # Replace NA values in MISEV with "0"
    MIData$MISEV = MIData$MISEV %>% replace_na("0")

    # Make all the MISPEC value = LIVER
    # replace all the value to Liver which will replace "LIVER/GALLBLADDER" to "LIVER"
    MIData$MISPEC <- "LIVER" # replace the "LIVER/GALLBLADDER" to "LIVER"

    #Combine Levels on Findings
    MIData$MISTRESC <- as.factor(MIData$MISTRESC)
    levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "CELL DEBRIS"] <- "CELLULAR DEBRIS"
    levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Infiltration, mixed cell"] <- "Infiltrate"
    levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Infiltration, mononuclear cell"] <- "Infiltrate"
    levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "INFILTRATION, MONONUCLEAR CELL"] <- "Infiltrate"
    levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Fibrosis"] <- "Fibroplasia/Fibrosis"

    # Check any empty MISRESC column
    empty_strings_count <- sum(MIData$MISTRESC == "")
    print(paste("Number of empty strings in MISTRESC:", empty_strings_count))

    # remove empty
    MIData <- MIData[MIData$MISTRESC != '', ]  # Remove rows with empty MISTRESC
    #........................................................................................
    # Create a copy of MIData
    MIData_copy <- MIData

    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Remove the "Recovery animals and tk animals from "MIData"
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    # Filter the data frame
    tk_recovery_less_MIData <- MIData %>% filter (USUBJID %in% master_CompileData$USUBJID)

    # Perform a left join to match USUBJID and get ARMCD
    tk_recovery_less_MIData_with_ARMCD <- tk_recovery_less_MIData %>%
      left_join(master_CompileData %>% select(STUDYID, USUBJID, ARMCD, SETCD), by = "USUBJID")

    ########## MI Data ###############
    MIData_cleaned <- tk_recovery_less_MIData_with_ARMCD
    MIData_cleaned_copy <- MIData_cleaned

    # cat("MIData_cleaned : \n", toString(head(MIData_cleaned)), "\nDimensions:",
    #     paste(dim(MIData_cleaned), collapse = 'x'), "\n") # ..............................................................

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Merge Severity MI Data into Compile Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    MIIncidencePRIME <-MIData_cleaned[,c(1,2,4)]

    test_MIIncidencePRIME <- MIIncidencePRIME

    Severity <- merge(master_CompileData[,c("STUDYID","USUBJID","Species", "ARMCD")], MIData_cleaned)

    MIData_cleaned_SColmn <- MIData_cleaned %>% select(USUBJID,MISTRESC,MISEV)

    MIData_cleaned_SColmn <- dcast(MIData_cleaned, USUBJID ~ MISTRESC, value.var = "MISEV")

    MIData_cleaned_SColmn[is.na(MIData_cleaned_SColmn)] <- "0" #Fill NAs with Zero

    mi_CompileData <- merge(master_CompileData , MIData_cleaned_SColmn, by = "USUBJID") # Final & working mi_CompileData

    # Back-up data frame for checking  purpose
    final_working_compile_data_bef_normal <- mi_CompileData

    cat("mi_CompileData : \n", toString(head(MIData_cleaned)), "\nDimensions:",
        paste(dim(mi_CompileData), collapse = 'x'), "\n") ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Remove Normal MI Results
    normalIndex <- which(colnames(mi_CompileData) == 'NORMAL')
    if (length(normalIndex) > 0) {
      mi_CompileData <- mi_CompileData[, -normalIndex]
    }

    # Back-up data frame for checking  purpose
    final_working_compile_data_afer_normal <- mi_CompileData

    # cat("mi_CompileData : \n", toString(head(MIData_cleaned)), "\nDimensions:",
    #     paste(dim(mi_CompileData), collapse = 'x'), "\n") ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Check the data types of the columns before conversion
    #str(mi_CompileData)

    #####????????????????????????? check the number~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Convert columns 7 to the last column  of mi_CompileData to numeric
    mi_CompileData[, 7:ncol(mi_CompileData)] <- sapply(mi_CompileData[, 7:ncol(mi_CompileData)], as.numeric)

    # Check the data types of the columns after conversion
    #str(mi_CompileData)


    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-------------------@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #print_MIIncidencePRIME <- MIIncidencePRIME
    #print_mi_CompileData <- mi_CompileData

    # Here Check the number of columns in mi_CompileData
    if (ncol(mi_CompileData) > 6) {
      #Calculate Incidence per group for MI Data
      MIIncidencePRIME <- merge(MIIncidencePRIME, unique(mi_CompileData[,c("STUDYID","USUBJID","ARMCD")]), by = c("USUBJID"))

      # get the name of the columns of " MIIncidencePRIME"
      column_MIIncidencePRIME <- data.frame(names(MIIncidencePRIME)) # column names of MIIncidencePRIME

      # Directly subset the data frame
      MIIncidence <- MIIncidencePRIME[, c("STUDYID","USUBJID", "MISTRESC","ARMCD")]

      test_MIIncidence  <- MIIncidence

      GroupIncid <- data.frame(Treatment = NA,Sex = NA,Finding = NA,Count = NA)

      # Iterate over each unique study
      #for (Study in unique(MIIncidence$STUDYID)){

        # Iterate over sex categories
        for (sex in c('M','F')) {
          # Filter data for the current study
          StudyMI <- MIIncidence[which(MIIncidence$STUDYID==unique(MIIncidence$STUDYID)),]
          #StudyMI <- MIIncidence

          StudyGroupIncid <- data.frame(Treatment = NA,Sex = NA,Finding = NA,Count = NA)

          # Filter data for the current sex
          sexSubjects <- mi_CompileData$USUBJID[which(mi_CompileData$SEX == sex)]
          sexIndex <- which(StudyMI$USUBJID %in% sexSubjects)
          StudyMI <- StudyMI[sexIndex,]

          # Iterate over unique treatment arms (ARMCD)
          for(dose in unique(StudyMI$ARMCD)){
            doseMI <- StudyMI[which(StudyMI$ARMCD == dose),]

            # Calculate the incidence for each finding
            Incid <- data.frame(table(toupper(doseMI$MISTRESC))/length(unique(doseMI$USUBJID)))

            names(Incid)[2] <- "Count"
            names(Incid)[1] <- "Finding"
            Incid$Treatment <- paste0(unique(unique(StudyMI$STUDYID)), " ",dose)
            Incid$Sex <- sex

            StudyGroupIncid <- rbind(StudyGroupIncid,Incid)
          }

          #Removing of Vehicle Baseline
          for (finding in unique(StudyGroupIncid$Finding)) {
            findingIndex <- which(StudyGroupIncid$Finding == finding)
            vehicleIndex <- grep('Vehicle', StudyGroupIncid$Treatment[findingIndex]) # VEHICLE FOR THE CURRENT FINDINGS.....
            if (length(vehicleIndex) > 0) {
              baseline <- StudyGroupIncid$Count[findingIndex][vehicleIndex]
              StudyGroupIncid$Count[findingIndex] <- StudyGroupIncid$Count[findingIndex] - baseline
            }
          }
          negativeIndex <- which(StudyGroupIncid$Count < 0) # when findings in HD group less than the vehicle group
          if (length(negativeIndex) > 0) {
            StudyGroupIncid$Count[negativeIndex] <- 0
          }
          # Combine results
          GroupIncid <- rbind(GroupIncid, StudyGroupIncid)
        }
      #}

      removeIndex <- which(is.na(GroupIncid$Treatment))
      if (length(removeIndex) > 0) {
        GroupIncid <- GroupIncid[-removeIndex,]
      }

      MIIncidence <- GroupIncid

      # cat("MIIncidence : \n", toString(head(MIData_cleaned)), "\nDimensions:",
      #     paste(dim(MIIncidence), collapse = 'x'), "\n") #......................

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~Create a copy of mi_CompileData named mi_CompileData2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Create a copy of mi_CompileData named mi_CompileData2
      mi_CompileData2 <- mi_CompileData #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Severity calculation and recalculation.........................................................................
      # Adjustment of the severity score based on the Incidence score .................................................

      # Initialize ScoredData with the first 6 columns of "mi_CompileData2"
      ScoredData <- mi_CompileData2[,1:6]

      # Initialize a counter for incidence overrides
      IncidenceOverideCount <- 0

      # Define column range for MI Data (from the 6th to the last column of mi_CompileData2)
      colIndex <- seq(7, ncol(mi_CompileData2))

      # Iterate over each column for scoring and adjustments
      for (i in colIndex){
        colName <- colnames(mi_CompileData2)[i]
        ScoredData[[colName]] <- NA

        #Score Severity # changing the current severity value in MISEV column???????????
        # Score Severity based on mi_CompileData2 #

        x <- ifelse(mi_CompileData2[,i] == 5, 5,
                    ifelse(mi_CompileData2[,i] > 3, 3,
                           ifelse(mi_CompileData2[,i] == 3, 2,
                                  ifelse(mi_CompileData2[,i] > 0, 1, 0))))

        # x <- ifelse(mi_CompileData2[,i]>3,3,
        #             ifelse(mi_CompileData2[,i]==3,2,
        #                    ifelse(mi_CompileData2[,i]>0,1,0)))

        ScoredData[,colName] <-x

        # Update mi_CompileData2 with the values from ScoredData for the current column
        mi_CompileData2[,colName] <- x

        #Check the Incidence percentage for each group
          for (sex in c('M','F')) {
            #studyDataStudyIndex <- which(mi_CompileData2$STUDYID == Study)
            studyDataStudyIndex <- which(mi_CompileData2$STUDYID == unique(ScoredData$STUDYID))
            studyDataSexIndex <- which(mi_CompileData2$SEX == sex)
            studyDataIndex <- intersect(studyDataStudyIndex, studyDataSexIndex)
            StudyData <- mi_CompileData2[studyDataIndex,]

            #MIIncidStudyIndex <- grep(Study, MIIncidence$Treatment)
            MIIncidStudyIndex <- grep(unique(ScoredData$STUDYID), MIIncidence$Treatment)
            MIIncidSexIndex <- which(MIIncidence$Sex == sex)
            MIIncidIndex <- intersect(MIIncidStudyIndex, MIIncidSexIndex)
            MIIncidStudy <- MIIncidence[MIIncidIndex,]

            for (Dose2 in unique(StudyData$ARMCD)){
              DoseSevIndex <- which(StudyData$ARMCD == Dose2)
              DoseSev <- StudyData[DoseSevIndex,]
              DoseIncid <- MIIncidStudy[which(word(MIIncidStudy$Treatment, -1) == Dose2),]
              if (colName %in% DoseIncid$Finding) {
                findingIndex <- which(DoseIncid$Finding == colName)

                Incid <- DoseIncid$Count[findingIndex]
                Incid <- ifelse(Incid >= 0.75, 5,
                                ifelse(Incid >= 0.5, 3,
                                       ifelse(Incid >= 0.25, 2,
                                              ifelse(Incid >= 0.1, 1, 0))))

                # Incid <- ifelse(Incid>=0.5,3,
                #                 ifelse(Incid>=0.25,2,
                #                        ifelse(Incid>=0.1,1,0)))
                swapIndex <- which(DoseSev[[colName]] < Incid & DoseSev[[colName]] > 0)
                if (length(swapIndex) > 0) {
                  DoseSev[swapIndex, colName] <- Incid
                  ScoredData[studyDataIndex[DoseSevIndex], colName] <- DoseSev[, colName]
                  IncidenceOverideCount <- IncidenceOverideCount + 1
                }

              }
            }
          }
      }

      # cat("ScoredData : \n", toString(head(MIData_cleaned)), "\nDimensions:",
      #     paste(dim(ScoredData), collapse = 'x'), "\n") #..........................................................


      # subset the ScoredData
      ScoredData_subset_HD <- ScoredData %>% filter (ARMCD == "HD")

      #print(ScoredData_subset_HD[1, ])

      # Convert columns from 7th to the last to numeric
      ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)] <- lapply(
        ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)],
        function(x) as.numeric(as.character(x))
      )

      #numeric_ScoredData_subset_HD <- ScoredData_subset_HD


      #~~~~~~~~~~~~~~.....for kevin....~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Ensure no data inconsistencies
      #print(str(dim(MI_final_score)))  # Check dimensions (rows, columns)

      # Calculate average for 7th column (index 7)
      #seventh_column_average <- colMeans(MI_final_score[, 7, drop = FALSE])

      Kevin_MI_final_score <- ScoredData_subset_HD

      #Average calculation for each of the 7th column to on wards.
      # renaming the data frame "Kevin_MI_final_score"
      ak <- Kevin_MI_final_score

      # Select columns from 7th to the last
      col_7th_to_onward <- ak[, 7:length(colnames(ak)),  drop = FALSE]


      # Select columns from 7th to the last and preserve column names
      #all <- ak[, 7:ncol(ak), drop = FALSE]

      #all <- ak[, 7:length(colnames(ak)), drop = FALSE]

      # Calculate the mean for each selected column
      #akj2 <- lapply(col7th_to_onward, mean)
      mean_col_7th_to_onward <- lapply(col_7th_to_onward, mean)

      # Add a 'studyid' element to the list with value 'STUDYID'
      #akj[['STUDYID']] <- j

      # Define akj1 as an empty list
      #akj1 <- list()
      studyid_current <- list()

      # Add a 'STUDYID' element to akj1 with value 'j'
      #akj1[['STUDYID']] <- j
      studyid_current[['STUDYID']] <- j

      # Append elements from mean_col_7th_to_onward to a
      akj <- append(studyid_current, mean_col_7th_to_onward)

      # Append akj to the master list using numeric index
      master_MI_list[[length(master_MI_list) + 1]] <- akj


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # averaged zscore per STUDYID for 'MI'....................................
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Calculate the highest score for each row from the 7th to the last column
      #from the ScoredData_subset_HD

      # Check the number of columns
      num_cols_ScoredData_subset_HD <- ncol(ScoredData_subset_HD)


      # If number of columns is 7, assign highest_score as the value of the 7th column
      if (num_cols_ScoredData_subset_HD == 7) {
        ScoredData_subset_HD$highest_score <- ScoredData_subset_HD[, 7]
      } else {
        # If number of columns is more than 7, get the max value from column 7 to the end
        #ScoredData_subset_HD$highest_score <- apply(ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)], 1, max, na.rm = TRUE)
        ScoredData_subset_HD$highest_score <- rowMaxs(as.matrix(ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)]), na.rm = TRUE)
      }

      #ScoredData_subset_HD$highest_score <- rowMaxs(as.matrix(ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)]), na.rm = TRUE)

      #print(ScoredData_subset_HD)

      # Move the highest_score column to be the third column
      ScoredData_subset_HD <- ScoredData_subset_HD[, c(1:2,
                                              ncol(ScoredData_subset_HD),
                                              3:(ncol(ScoredData_subset_HD)-1))]

      # Step 1: Filter for HD
      #MI_final_score <- ScoredData_subset_HD [ARMCD == "HD"]
      MI_final_score <- ScoredData_subset_HD %>% filter(ARMCD == "HD")

      # Step 2: Convert highest_score to numeric # FACTOR value to numeric......?????????
      MI_final_score <- MI_final_score %>%  mutate(highest_score = as.numeric(highest_score))

      # Step 3: Group by STUDYID
      MI_final_score <- MI_final_score %>%  group_by(STUDYID)

      # Step 4: Average MI_score
      MI_final_score <- MI_final_score %>%  summarise( avg_MI_score = mean(highest_score, na.rm = TRUE),  )

      # Step 5: final column selection
      MI_final_score <- MI_final_score %>% select(STUDYID, avg_MI_score)

      # cat("MI_final_score : \n", toString(head(MIData_cleaned)), "\nDimensions:",
      #     paste(dim(MI_final_score), collapse = 'x'), "\n") #..........................................


      # Append the "MI_score" to the "FOUR_Liver_Score" data frame
      # Create "brainToBW_df" for FOUR_Liver_Score
      MI_df <- MI_final_score %>% rename(MI_score = avg_MI_score)

      # Extract the MI_score value for the current STUDYID from MI_df
      #calculated_MI_value <- MI_df$MI_score[MI_df$STUDYID == unique(ts$STUDYID)]
      calculated_MI_value <- MI_df$MI_score

      # Update the MI_score value in FOUR_Liver_Score for the current STUDYID
      FOUR_Liver_Score$MI_score[FOUR_Liver_Score$STUDYID == unique(ts$STUDYID)] <- calculated_MI_value


      ### deplyr system...............
      # hhhhhh <- MI_final_score %>%
      #   mutate(across(7:length(colnames(.)), ~ mean(., na.rm = TRUE))) %>% select ( distinct(STUDYID, Species, ))
      #
      # distinct_rows <- distinct(hhhhhh, STUDYID, .keep_all = TRUE)
      #
      #
      # kfssafsasgfaj <- MI_final_score %>% group_by(STUDYID) %>%
      #   summarize(across(7:length(colnames(.)), ~ mean(., na.rm = TRUE)))


    } else {
      # Create MI_final_score data frame with STUDYID and avg_MI_score
      # avg_MI_score is set to 0 and STUDYID values are unique from ts$STUDYID
      MI_final_score <- data.frame(
        STUDYID = unique(ts$STUDYID),
        avg_MI_score = NA
      )

      # Append the "MI_score" to the "FOUR_Liver_Score" data frame
      # Create "MI_score" for FOUR_Liver_Score
      MI_df <- MI_final_score %>% rename(MI_score = avg_MI_score)

      # Extract the MI_score value for the current STUDYID from MI_df
      calculated_MI_value <- MI_df$MI_score[MI_df$STUDYID == unique(ts$STUDYID)]

      # Update the MI_score value in FOUR_Liver_Score for the current STUDYID
      FOUR_Liver_Score$MI_score[FOUR_Liver_Score$STUDYID == unique(ts$STUDYID)] <- calculated_MI_value

    }

  }, error = function(e) {
    # Handling errors of the secondary operation

    # Log the error
    error_block5 <- data.frame(STUDYID = unique(ts$STUDYID),Block = "MI", ErrorMessage = e$message, Time = Sys.time(), stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block5)

    # Create MI_final_score with NA values
    return(data.frame(STUDYID = NA, avg_MI_score = NA))
  })

  print ( "end of~~MI~~~~~zScore~~~~~~calculation ~~~~~~~~~~~" )

  # print(serum_bili_final_zscore)
  # print(serum_alt_final_zscore)
  # print(serum_alb_final_zscore)
  # print(serum_alp_final_zscore)
  # print(serum_ast_final_zscore)
  # print(serum_ggt_final_zscore)
  #................. ............................end of MI scoring.....................~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

}

#...................................working in Four_liver_score.................................................
# Reassigned the variable
liver_scored_Four_Liver_Score <- FOUR_Liver_Score

# Create averaged_liver_score column for un_scored columns
liver_scored_Four_Liver_Score <- liver_scored_Four_Liver_Score %>%
  mutate(averaged_liver_score = rowMeans(select(., liverToBW, LB_score, MI_score), na.rm = TRUE))


# Create scored_averaged_liver_score column for scored columns
liver_scored_Four_Liver_Score <- liver_scored_Four_Liver_Score %>%
  mutate(scored_averaged_liver_score = rowMeans(select(.,MI_score, scored_liverToBW, scored_LBScore), na.rm = TRUE))

# remove NAs from "scored_averaged_liver_score" column
final_liver_scored_Four_Liver_Score <- liver_scored_Four_Liver_Score %>% filter(!is.na(scored_averaged_liver_score))
#.........................................................................................................................


# write a csv file
#write.csv(final_liver_scored_Four_Liver_Score, "studyid_vector_nLiver35.csv", row.names = FALSE)

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

}

#write.csv(final_liver_scored_Four_Liver_Score, "117_final_liver_scored_Four_Liver_Score.csv", row.names = FALSE)
# ~~~~~~~~~~~~~~~~~~ master_liverToBW data frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a new column named "indst_To" with value "Liver"
#master_liverToBW$indst_TO <- "Liver"

# Add the "indst_TO" column to the front of the dataframe
#master_liverToBW <- data.frame(indst_TO = "not_Liver", master_liverToBW)

# Remove the first row from master_liverToBW
#master_liverToBW <- master_liverToBW[-1, ]

#write.csv (master_liverToBW, "not_Liver_master_liverToBW.csv", row.names = FALSE)



# # Add a new column named "indst_To" with value "not_Liver"
# master_liverToBW$indst_TO <- "not_Liver"
# # Remove the first row from master_liverToBW
# master_liverToBW <- master_liverToBW[-1, ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~ LB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a new column named "indst_To" with value "Liver"
#master_LB_list$indst_TO <- "Liver"

#master_LB_list <- data.frame(indst_TO = "not_Liver", master_LB_list)

# Remove the first row from master_liverToBW
#master_LB_list <- master_LB_list[-1, ]

#write.csv (master_LB_list, "not_Liver_master_LB_list.csv", row.names = FALSE)

# # Add a new column named "indst_To" with value "not_Liver"
# master_LB_list$indst_TO <- "not_Liver"
# # Remove the first row from master_liverToBW
# master_LB_list <- master_LB_list[-1, ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MI~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  decipher the master_data frmae list

# Step 3: Bind all the data frames into a single data frame using bind_rows
#master_combined_MI_master_list <- bind_rows(master_MI_list, .id = "iteration")

# Add a new column named "indst_To" with value "Liver"

# Assuming master_combined_MI_master_list is your dataframe
#master_combined_MI_master_list <- master_combined_MI_master_list[, -1]

#master_combined_MI_master_list$indst_TO <- "Liver"
#master_combined_MI_master_list <- data.frame(indst_TO = "not_Liver", master_combined_MI_master_list)


#write.csv (master_combined_MI_master_list, "not_Liver_master_MI_list.csv", row.names = FALSE)

# # Add a new column named "indst_To" with value "not_Liver"
# master_combined_MI_master_list$indst_TO <- "not_Liver"


# # find the uncommon studyid
# dddx1 <- data_frame(STUDYID = df1$STUDYID)
# dddx3 <- data_frame(STUDYID = df2$STUDYID)
# dddx2 <- data_frame(STUDYID = combined_MI_master_list$STUDYID)


# # Find non-matching STUDYID between dddx1 and dddx2
# non_matching <- anti_join(dddx3, dddx2, by = "STUDYID")
#
# #.........................

#Liver_study_which has problems
# 18-6005
# 1019-051
# 8376333
# 20148562

#nLiver_problem list....
# DN17067
# 2698-003
# 20141935
# 8376335
# 1617RD2
# DN19024
#SBL037-085
# 62585
# 8385460
# 8378738

#write.csv(final_liver_scored_Four_Liver_Score, "remain_57_outoF83_studyid_calculated_score.csv", row.names = FALSE)

# Bismillah##""04/30/2024">>>>>>2.05PM>>>>>>>>>>>>>>>



