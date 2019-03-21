#' Set up FreezerCheck
#'
#' Does the heavy lifting of setting up your system to work with your
#' installation of Freezerworks and the Freezerworks ODBC client. This should
#' only need to be run on package installation, but can also be used to reset
#' your version of FreezerCheck. If run again after installation, by default
#' will not overwrite existing data in the user directory, but will reset any
#' data modified during demo mode. This can also be used to reset your
#' Freezerworks DSN assignment in cases of updated or renamed DSNs. Multiple
#' installations of FreezerCheck are supported via alias. When launching from
#' the console, you will be prompted for which to use.
#'
#' @param overwrite_demo Logical indicator to overwrite existing files in the
#'   demonstration directory. Defaults to TRUE.
#' @param overwrite_data Logical indicator to overwrite existing files in the
#'   user-defined data directory. Defaults to FALSE.
#'
#' @export
freezercheck_setup <- function(overwrite_demo = TRUE, overwrite_data = FALSE) {
  if (interactive()) {
    dir_pack <- system.file(package = "NISTfreezercheck")
    dir_sep <- .Platform$file.sep
    reinstall <- "Reinstall Completely"
    if (file.exists(paste(dir_pack, "data", "installed.RDS", sep = dir_sep))) {
      reinstall <- menu(title = "FreezerCheck has already been installed. Select the appropriate number to continue.",
                        choices = c("Abort Installation",
                                    "Reinstall Completely",
                                    "Add Installation"))
      if (reinstall == "Abort Installation") {
        stop ("FreezerCheck will retain prior settings.\nUse freezercheck_start() to launch.\n")
      }
    }
    DSNs <- names(RODBC::odbcDataSources(type = "user"))
    DSN_choice <- menu(title = "Step 1. Choose an installed data source name by number or Enter 0 to exit:",
                       choices = DSNs)
    valid_dsn <- FALSE
    DSN <- NULL
    if (DSN_choice == 0) {
      cat("Please install Freezerworks as a data source in your operating system. Only demo mode is available.\n")
      continue <- askYesNo(msg = "Continue with installation?")
      if (!continue) stop("FreezerCheck setup has been aborted.")
    } else {
      DSN <- DSNs[DSN_choice]
      cat("\t\tVerifying connection status...")
      valid_connection <- class(try(dbConnect(odbc(), DSN), silent = TRUE))
      if (valid_connection == "try-error") {
        cat(paste0("\n\t\tUnable to connect to DSN '", DSN, "'. Check your system settings and try again. Only demo mode is available.\n"))
        continue <- askYesNo(msg = "Continue with installation?")
        if (!continue) {
          stop("FreezerCheck setup has been aborted.")
        }
      } else {
        cat("Success.\n\n")
        valid_dsn <- TRUE
      }
    }
    cat("Step 2. Creating directory structure...\n")
    dir_demo <- paste(dir_pack, "data", "demo", sep = dir_sep)
    if (!dir.exists(dir_demo)) {
      dir.create(dir_demo)
    }
    dir_demo_paused <- paste(dir_pack, "data", "demo", "paused_audits", sep = dir_sep)
    if (!dir.exists(dir_demo_paused)) {
      dir.create(dir_demo_paused)
    }
    if (valid_dsn) {
      if (reinstall == "Add Installation") {
        cat("Step 3. Pick either a new directory or the directory chosen for a prior\n",
            "       installation of FreezerCheck. Multiple instances for different\n",
            "       profiles can be safely added to the same parent directory. Do not\n",
            "       choose the 'FreezerCheck' directory, but rather its parent directory.\n",
            "       (The directory dialog may open in the background.)\n")
      } else {
        cat("Step 3. Set a directory to store FreezerCheck data.\n",
            "       (The directory dialog may open in the background.)\n")
      }
      dir_user <- choose.dir(caption = "Choose a directory to store FreezerCheck data.")
      dir_user <- gsub("\\\\", dir_sep, dir_user)
      dir_user_alias <- readline(prompt = "\t\tWhat alias would you like to use for this installation?\n\t\t")
      dir_user <- paste(dir_user, "FreezerCheck", dir_user_alias, "data", sep = dir_sep)
      dir_user_new <- data.frame(alias = dir_user_alias,
                                 path = dir_user,
                                 stringsAsFactors = FALSE)
      if (!dir.exists(dir_user)) {
        dir.create(dir_user, recursive = TRUE)
      }
    } else {
      cat("Step 3. Skipped user data setup.")
    }
    cat("Step 4. Populating local data structures...\n")
    data_files <- list.files(paste(dir_pack, "data", sep = dir_sep),
                             pattern = ".RDS")
    demo_index <- grep("^demo_", data_files)
    data_files <- list(data_files[demo_index],
                       data_files[-demo_index])
    for (f in data_files[[1]]) {
      file.copy(from = paste(dir_pack, "data", f, sep = dir_sep),
                to = paste(dir_pack, "data", "demo", f, sep = dir_sep),
                overwrite = overwrite_demo)
    }
    if (valid_dsn) {
      for (f in data_files[[2]]) {
        file.copy(from = paste(dir_pack, "data", f, sep = dir_sep),
                  to = paste(dir_user, f, sep = dir_sep),
                  overwrite = overwrite_data)
      }
      dir_user_location <- paste(dir_pack, "data", "data_dir.RDS", sep = dir_sep)
      if (file.exists(dir_user_location)) {
        dir_user_df <- readRDS(dir_user_location)
        dir_user_df <- rbind(dir_user_df,
                             dir_user_new)
      } else {
        dir_user_df <- dir_user_new
      }
      dir_paused <- paste(dir_user, "paused_audits", sep = dir_sep)
      if (!dir.exists(dir_paused))  dir.create(dir_paused)
      saveRDS(DSN, paste(dir_user, "DSN.RDS", sep = dir_sep))
      saveRDS(dir_user_df, dir_user_location)
      freezer_list <- get_freezers(DSN, dir_user)
      saveRDS(freezer_list, paste(dir_user, "freezer_list.RDS", sep = dir_sep))
      progress <- make_progress(freezer_list)
      saveRDS(progress, paste(dir_user, "progress.RDS", sep = dir_sep))
      field_list <- get_fields(DSN, dir_user)
      saveRDS(field_list, paste(dir_user, "fields.RDS", sep = dir_sep))
      cat("Step 5. Tailoring to your staff...\n")
      cat("\t\tPlease edit your staff list.\n\n")
      staff <- readRDS(paste(dir_user, "staff.RDS", sep = dir_sep))
      staff <- edit(staff)
      staff$names <- as.factor(staff$names)
      staff$usernames <- as.factor(staff$usernames)
      saveRDS(staff, paste(dir_user, "staff.RDS", sep = dir_sep))
      cat("NIST FreezerCheck setup for", dir_user_alias, "is complete!\n\n",
          "Objects you may wish to tailor further include:\n",
          "\t\tfields.RDS       | UDFs that will be available for search\n",
          "\t\tfreezer_list.RDS | determines freezers available in this installation\n",
          "\t\tstaff.RDS        | staff changes in this version must be made manually\n\n",
          "All objects for this installation are located at:\n",
          "\t\t", dir_user)
    } else {
      cat("NIST FreezerCheck setup for demo evaluation is complete!\n\n")
    }
    saveRDS(TRUE, paste(dir_pack, "data", "installed.RDS", sep = dir_sep))
  }
}

# Functions required for FreezerCheck

#' Demonstration of Freezercheck
#'
#' Literally only a wrapper to start Freezercheck in demonstration mode.
#'
#' @export
#'
#' @examples
#' freezercheck_demo()
freezercheck_demo <- function() {
  freezercheck_start(demo_mode = TRUE)
}

#' Start the FreezerCheck quality assurance dashboard
#'
#' Use this to launch FreezerCheck from the console. Directory must be in place.
#'
#' @param demo_mode A boolean indicator (default FALSE) of whether to launch
#'   FreezerCheck in demonstration mode.
#'
#' @export
#'
#' @examples
#' start_freezercheck(demo_mode = TRUE)
freezercheck_start <- function(demo_mode = FALSE) {
  freezercheck <- system.file(package = "NISTfreezercheck")
  dir_sep <- .Platform$file.sep
  demo_mode <<- demo_mode
  if (demo_mode) {
    demo_data <<- readRDS(paste(freezercheck, "data", "demo", "demo_data.RDS", sep = dir_sep))
  } else {
    dir_user_df <- readRDS(paste(freezercheck, "data", "data_dir.RDS", sep = dir_sep))
    if (nrow(dir_user_df) > 1) {
      alias_list <- dir_user_df$alias
      install_choice <- menu(title = "Which FreezerCheck installation would you like to use?",
                             choices = alias_list)
      dir_user <<- dir_user_df$path[install_choice]
    } else {
      dir_user <<- dir_user_df$path[1]
    }
  }
  shiny::runApp(appDir = paste(freezercheck, "app", sep = dir_sep),
                launch.browser = TRUE,
                display.mode = "normal",
                test.mode = FALSE)
}

#' Build a list of User Defined Fields (UDFs) for query
#'
#' This function formats UDFs for use with SQL queries. There is no support or
#' checks for invalid headers. This is intended as an internal support function
#' only. It will always include Globally Unique Aliquot ID (GUALIQUOTID).
#'
#' @param addl_columns A character vector of valid UDF SQL headers.
#' @param include_defaults A boolean indicator of whether to include the default
#'   UDFs of "FreezerName", "Position1", "Position2", and "Position3".
#'
#' @return A character scalar of UDFs formatted properly.
#' @export
#'
#' @examples
#' build_UDF_list()
#' build_UDF_list(c("Field_ID_Num", "Project_ID"))
#' build_UDF_list(c("Field_ID_Num", FALSE))
build_UDF_list <- function(addl_columns = NULL,
                           include_defaults = TRUE) {
  defaults <- c('FreezerName', 'Position1', 'Position2', 'Position3')
  UDFs <- c('GUAliquotID', defaults)
  if (!is.null(addl_columns)) {
    if (sum(addl_columns %in% UDFs) > 0) {
      addl_columns <- addl_columns[-which(addl_columns %in% UDFs)]
    }
    if ('Project_ID' %in% addl_columns) {
      UDFs <- c('Project_ID', UDFs)
      addl_columns <- addl_columns[-which(addl_columns == 'Project_ID')]
    }
    UDFs <- c(UDFs, addl_columns)
  }
  if (!include_defaults) {
    UDFs <- UDFs[-which(UDFs %in% defaults)]
  }
  out <- paste0(UDFs, collapse = ", ")
  # This is simply to catch a silly situation that should never happen.
  if (length(UDFs) == 0) {
    warning("No additional columns supplied, but default UDFs were excluded, resulting in zero return.\n  Default UDFs returned instead.")
    out <- build_UDF_list(NULL, TRUE)
  }
  return(out)
}


#' Get the list of freezers from Freezerworks
#'
#' This function creates the list of valid freezers for use with FreezerCheck.
#' Works with demo mode. Ensure DSN is set in your environment.
#'
#' @param DSN Character scalar of the data source name for your Freezerworks
#'   ODBC driver.
#' @param dir_user_path Character scalar of the data path to your user
#'   directory, only use to write back any changes.
#'
#' @return A character vector of freezers.
#' @export
#'
#' @examples
#' demo_mode <- TRUE
#' get_freezers(DSN)
get_freezers <- function(DSN = NULL, dir_user_path = NULL) {
  if (!exists("dir_pack"))  dir_pack  <- system.file(package = "NISTfreezercheck")
  if (!exists("dir_sep"))   dir_sep   <- .Platform$file.sep
  if (!exists("demo_mode")) demo_mode <- FALSE
  if (demo_mode) {
    if (exists("freezer_list")) {
      out <- freezer_list
    } else {
      dir_sep <- .Platform$file.sep
      dir_pack_demo <- paste(dir_pack, "data", "demo", sep = dir_sep)
      out <- read_rds(paste(dir_pack_demo, "demo_freezer_list.RDS", sep = dir_sep))
    }
  } else {
    con <- dbConnect(odbc(), DSN)
    freezer_list <- dbGetQuery(con, "SELECT FreezerPhysName FROM FreezerPhysical")
    dbDisconnect(con)
    freezer_list <- freezer_list[[1]]
    exclude <- unique(
      c(
        grep("temp", tolower(freezer_list)),
        grep("demo", tolower(freezer_list)),
        grep("samples", tolower(freezer_list)),
        grep("test", tolower(freezer_list))
      )
    )
    out <- sort(freezer_list[-exclude])
    if (!is.null(dir_user_path)) {
      write_rds(out, paste(dir_user_path, "freezer_list.RDS", sep = dir_sep))
    }
  }
  return(out)
}


#' Get the list of fields from Freezerworks
#'
#' This function creates the list of valid User Defined Fields (UDFs) used in
#' your Freezerworks tables 'Aliquots' and 'Samples'. Works with demo mode. If
#' demo mode is off, ensure the DSN is valid first. Parameter dir_user_path is
#' only necessary if you wish to overwrite the current list of fields. Edit the
#' object at dir_user_path/fields.RDS to customize this list for use with
#' FreezerCheck and only include a subset of UDFs for audit purposes. By default
#' this will return EVERY UDF present, which is likely not suitable.
#'
#' @param DSN Character scalar of the data source name for your Freezerworks
#'   ODBC driver.
#' @param dir_user_path Character scalar of the data path to your user
#'   directory, only use to write back any changes (defaults to NULL).
#'
#' @return A character vector of UDFs
#' @export
#'
#' @examples
#' demo_mode <- TRUE
#' DSN <- "ODBC_Name"
#' get_fields(DSN)
get_fields <- function(DSN, dir_user_path = NULL) {
  if (!exists("dir_pack"))  dir_pack  <- system.file(package = "NISTfreezercheck")
  if (!exists("dir_sep"))   dir_sep   <- .Platform$file.sep
  if (!exists("demo_mode")) demo_mode <- FALSE
  if (demo_mode) {
    if (exists("fields")) {
      out <- fields
    } else {
      dir_sep <- .Platform$file.sep
      dir_pack_demo <- paste(dir_pack, "data", "demo", sep = dir_sep)
      out <- read_rds(paste(dir_pack_demo, "demo_fields.RDS", sep = dir_sep))
    }
  } else {
    con <- dbConnect(odbc(), DSN)
    fields <- unique(
      c(dbListFields(con, "Aliquots"),
        dbListFields(con, "Samples")))
    dbDisconnect(con)
    out <- sort(fields)
    out_deleted <- grep("DeletedField", out)
    if (length(out_deleted) > 0) {
      out <- out[-out_deleted]
    }
    if (!is.null(dir_user_path)) {
      write_rds(out, paste(dir_user_path, "fields.RDS", sep = dir_sep))
    }
  }
  return(out)
}


#' Get additional information about an aliquot
#'
#' This queries Freezerworks with a list of globally unique aliquot IDs (GUAIDs)
#' and returns additional metadata to assist with the audit process. In order to
#' work, this must include a valid data source name to connect with
#' Freezerworks.
#'
#' @param GUAID A character vector of GUAIDs as stored in Freezerworks.
#' @param addl_columns A character vector of columns to include other than
#'   defaults that will be passed to build_UDF_list().
#' @param include_defaults A boolean indicator of whether to include the default
#'   UDFs of "FreezerName", "Position1", "Position2", and "Position3" that will
#'   be passed to build_UDF_list().
#' @param demo_mode A boolean indicator to use demo data rather than a live DSN
#'   connection.
#'
#' @return Assuming the output of build_UDF_list() is valid, a data frame is
#'   returned containing the requested columns from Freezerworks with one row
#'   for each GUAID provided.
#' @export
#'
#' @examples
#' get_GUAID_info(GUAID = "DEM204978O", demo_mode = TRUE)
#' get_GUAID_info(GUAID = "DEM218508O", addl_columns = "PROJECT_ID", demo_mode = TRUE)
#' get_GUAID_info(GUAID = c("DEM137314O", "DEM220303O", "DEM122985O"), demo_mode = TRUE)
get_GUAID_info <- function(GUAID, addl_columns = NULL,
                           include_defaults = TRUE) {
  GUAID <- str_trim(GUAID)
  if (demo_mode) {
    if (!exists("demo_data")) {
      dir_pack <- system.file(package = "NISTfreezercheck")
      dir_pack_demo <- paste(dir_pack, "data", "demo", sep = dir_sep)
      demo_data <- read_rds(paste(dir_pack_demo, "demo_data.RDS", sep = dir_sep))
    }
    UDFs <- str_split(build_UDF_list(addl_columns, include_defaults), ", ")[[1]]
    UDFs <- toupper(UDFs)
    info <- demo_data %>%
      select(one_of(UDFs)) %>%
      filter(GUALIQUOTID %in% GUAID)
  } else {
    con <- dbConnect(odbc(), DSN)
    info <- dbGetQuery(con, paste0("SELECT ",
                                   build_UDF_list(addl_columns, include_defaults),
                                   " FROM Aliquots AS a
                                   JOIN Samples AS s ON a.FK_SampleUID = s.PK_SampleUID
                                   JOIN FreezerSection AS fs ON a.FK_FreezerSectID = fs.PK_FreezerSectID
                                   WHERE GUAliquotID IN ('",
                                   paste0(GUAID, collapse = "', '"),
                                   "')"))
    dbDisconnect(con)
    rm(con)
  }
  return(info)
}

#' Get positions to audit
#'
#' Obtains a random sampling of positions to audit from a specific freezer
#' within the repository managed by FreezerWorks. Effort optimization is done by
#' condensing structured containers by iterative amounts.
#'
#' @param dsn Character scalar for the data source name to connect to
#'   FreezerWorks.
#' @param freezer Character scalar with the freezer being targeted for audit.
#' @param addl_columns Character vector of any additional User Defined Fields to
#'   include. Failure will occur if these do not exist.
#' @param fraction A umerical scalar, default 0.1, of the fraction of positions
#'   in the freezer to audit from 0 to 1.
#' @param return_all Logical indicator, default FALSE, to trigger a full audit.
#' @param random Logical indicator, default FALSE, to Obtain a true random
#'   selection of positions.
#' @param max_returns Numerical scalar indicating the absolute maximum number of
#'   positions to return.
#' @param focus_racks Logical indicator, default TRUE, of whether to perform
#'   focused condensing for less effort.
#' @param condense_rack_to Numerical scalar from 0 to 1, default 0.33, of the
#'   target fraction of first order containers - e.g. freezer racks - to
#'   condense. For example, if there are 10 boxes in a rack, 0.33 will return 3
#'   of them for audit.
#' @param condense_box_to Numerical scalar from 0 to 1, default 0.25, of the
#'   target fraction of second order containers - e.g. boxes within racks - to
#'   condense. For example, if there are 81 places in a box, 0.25 will return 21
#'   of them for audit.
#' @param demo_mode Logical indicator, default FALSE, on whether to run in demo
#'   mode. Allows for no external database connection and demonstrates
#'   functionality only.
#'
#' @return A data frame of aliquot positions to audit.
#' @export
#'
#' @examples
#' demo_mode <- TRUE
#' get_scan_positions(freezer = "Freezer 1")
get_scan_positions <- function(dsn,
                               freezer,
                               addl_columns = NULL,
                               fraction = 0.1,
                               return_all = FALSE,
                               random = FALSE,
                               max_returns = NA,
                               focus_racks = TRUE,
                               condense_rack_to = 0.33,
                               condense_box_to = 0.25) {
  # Connect to dsn and pull positions requested, then disconnect and clean up
  if (demo_mode) {
    addl_columns <- str_split(build_UDF_list(addl_columns), ", ")[[1]]
    addl_columns <- toupper(addl_columns)
    positions <- demo_data %>%
      filter(FREEZERPHYSNAME == freezer) %>%
      select(one_of(addl_columns))
  } else {
    con <- dbConnect(odbc(), dsn)
    positions <- dbGetQuery(con, paste0("SELECT ",
                                        build_UDF_list(addl_columns),
                                        " FROM Aliquots AS a
                                        JOIN Samples AS s ON a.FK_SampleUID = s.PK_SampleUID
                                        JOIN FreezerSection AS fs ON a.FK_FreezerSectID = fs.PK_FreezerSectID
                                        JOIN FreezerPhysical AS fp on fs.FK_FreezerPhysID = fp.PK_FreezerPhysID
                                        WHERE NOT Position1 = '' AND FreezerPhysName = '",
                                        freezer,
                                        "'"))
    dbDisconnect(con)
    rm(con)
  }
  n_col <- ncol(positions)

  # Check for empty freezer
  if (nrow(positions) == 0) {
    return("No positions currently recorded in this freezer.")
  }

  # Return all?
  if (!return_all) {

    # Evaluate maximum number of samples to check
    if (is.na(max_returns)) {
      fraction_n <- fraction
    } else {
      focus_racks <- FALSE
      fraction_n <- max_returns / nrow(positions)
    }

    # Return the chosen fraction
    if (random) {
      ### True random form
      positions <- minimize(positions, fraction_n)
    } else if (!is.na(max_returns)) {
      ### Deal with absolute return
      total <- 0
      subset_positions <- positions %>% slice(0)
      return_positions <- subset_positions
      while (total < max_returns) {
        containers <- unique(positions$POSITION1)
        if (nrow(return_positions) > 0) containers <- containers[-which(unique(return_positions$POSITION1) %in% containers)]
        if (length(containers) == 0) {
          total <- max_returns
        } else {
          subset_positions <- positions %>%
            filter(POSITION1 == sample(containers, size = 1))
          tube_check <- subset_positions %>%
            filter(POSITION3 == "") %>%
            nrow()
          basket_check <- subset_positions %>%
            filter(grepl("Basket", subset_positions$FREEZERNAME)) %>%
            nrow()
          if (tube_check == 0 & basket_check == 0) {
            subset_positions <- minimize(subset_positions, condense_box_to)
          }
          return_positions <- bind_rows(return_positions, subset_positions)
          total <- nrow(return_positions)
        }
      }
      if (nrow(return_positions) > max_returns) {
        return_positions <- return_positions %>%
          slice(sample(1:nrow(return_positions), size = max_returns))
      }
      positions <- return_positions
    } else {
      ###  - Container-oriented
      ###    Get distinct containers not using POSITION3, e.g. 'Tubes'
      structure_1 <- positions %>%
        filter(POSITION3 == "") %>%
        mutate(POSITION1 = as.numeric(POSITION1)) %>%
        distinct(POSITION1) %>%
        minimize(fraction_n)
      ns1 <- nrow(structure_1)
      ns1 <- ifelse(is.null(ns1), 0, ns1)
      ###    Get distinct containers using POSITION3, e.g. 'Racks'
      structure_2 <- positions %>%
        filter(POSITION3 != "") %>%
        select(POSITION1, POSITION2) %>%
        distinct()
      if (focus_racks) {
        rack_list <- structure_2 %>%
          distinct(POSITION1) %>%
          minimize(fraction_n)
        box_list <- structure_2 %>%
          filter(POSITION1 %in% rack_list$POSITION1) %>%
          select(POSITION1, POSITION2) %>%
          distinct() %>%
          group_by(POSITION1) %>%
          nest() %>%
          mutate(data_min = map(data, minimize, condense_rack_to)) %>%
          select(-data) %>%
          unnest()
      } else {
        box_list <- structure_2 %>%
          distinct(POSITION1, POSITION2) %>%
          minimize(fraction_n)
      }
      structure_2 <- box_list
      ns2 <- nrow(structure_2)
      ns2 <- ifelse(is.null(ns2), 0, ns2)
      ###    Filter each
      if (ns1 > 0) {
        structure_1 <- positions %>%
          filter(POSITION1 %in% as.numeric(structure_1$POSITION1)) %>%
          mutate(POSITION1 = as.numeric(POSITION1),
                 POSITION2 = as.numeric(POSITION2)) %>%
          arrange(POSITION1, POSITION2)
        ###   Catch the special case of a "Case" as in SRMs, which may have only Case 1 Position 1-100
        cases <- sum(grepl("Case", structure_1$FREEZERNAME))
        if (cases > 0) {
          cases <- structure_1 %>%
            slice(grep("Case", FREEZERNAME)) %>%
            minimize(condense_box_to)
          structure_1 <- structure_1 %>%
            slice(-grep("Case", FREEZERNAME))
          structure_1 <- bind_rows(structure_1, cases)
        }
      }
      if (ns2 > 0) {
        structure_2 <- positions %>%
          right_join(structure_2) %>%
          group_by(FREEZERNAME, POSITION1, POSITION2) %>%
          nest() %>%
          mutate(data_min = map(data, minimize, condense_box_to)) %>%
          select(-data) %>%
          unnest() %>%
          mutate(POSITION3 = as.numeric(POSITION3)) %>%
          arrange(POSITION1, POSITION2, POSITION3)
      }
      positions <- rbind(structure_1, structure_2)
      column_order <- c("GUALIQUOTID", "FREEZERNAME", "POSITION1", "POSITION2", "POSITION3")
      other_columns <- names(positions)[which(!names(positions) %in% column_order)]
      positions <- positions[, c(column_order, other_columns)]
    }
  }

  # Shape it up and spit it out.
  if (length(grep("Mechanical", freezer) > 0)) {
    positions <- positions %>%
      mutate(POSITION1 = paste("Shelf", POSITION1)) %>%
      mutate(POSITION2 = paste("Box/Bag", POSITION2)) %>%
      mutate(POSITION3 = paste("Location", POSITION3))
  } else {
    positions <- positions %>%
      mutate(POSITION1 = ifelse(is.na(as.numeric(POSITION1)),
                                ifelse(grepl("Basket", FREEZERNAME),
                                       paste("Pie", POSITION1),
                                       paste("Rack", POSITION1)),
                                ifelse(grepl("Case", FREEZERNAME),
                                       paste("Case", POSITION1),
                                       paste("Tube", POSITION1)))) %>%
      mutate(POSITION2 = ifelse(is.na(as.numeric(POSITION2)),
                                paste("Box", POSITION2),
                                ifelse(grepl("Basket", FREEZERNAME),
                                       paste("Basket", POSITION2),
                                       ifelse(grepl("Case", FREEZERNAME),
                                              paste("Position", POSITION2),
                                              paste("Location", POSITION2))))) %>%
      mutate(POSITION3 = ifelse(is.na(as.numeric(POSITION3)),
                                "",
                                ifelse(grepl("Basket", FREEZERNAME),
                                       paste("Bag", POSITION3),
                                       paste("Position", POSITION3))))
  }
  positions <- positions %>%
    mutate(Status = "Pending",
           Status = factor(Status, levels = c("Pending", "Verified", "Discrepancy")),
           POSITION1 = as.factor(POSITION1),
           POSITION2 = as.factor(POSITION2),
           POSITION3 = as.factor(POSITION3)) %>%
    rename("SECTION" = "FREEZERNAME")
  positions <- positions[, c(n_col + 1, 1:n_col)]
  return(positions)
}

#' Total aliquot number for a single freezer.
#'
#' Query Freezerworks for the total number of aliquots with recorded positions
#' within any given freezer. Calls get_freezers() to build a validation list
#' first. Ensure object DSN (data source name for the Freezerworks ODBC driver)
#' exists.
#'
#' @param freezer Character scalar for the requested freezer.
#'
#' @return Integer scalar representing aliquot count.
#' @export
get_total_positions <- function(freezer) {
  # Connect to dsn and pull positions requested, then disconnect and clean up
  if (!exists("demo_mode")) demo_mode <- FALSE
  if (demo_mode) {
    if (!freezer %in% demo_data$FREEZERPHYSNAME) {
      stop(paste0("'", freezer, "' not found in the demonstration data."))
    }
    positions <- demo_data %>%
      filter(FREEZERPHYSNAME == freezer)
  } else {
    if (!exists("DSN")) {
      stop("Object DSN not found. Please set to your Freezerworks DSN and try again.")
    }
    freezer_list <- get_freezers(DSN)
    if (!freezer %in% freezer_list) {
      stop(paste0("'", freezer, "' not found in Freezerworks."))
    }
    con <- dbConnect(odbc(), dsn = DSN)
    positions <- dbGetQuery(con, paste0("SELECT GUAliquotID FROM Aliquots AS a
                                        JOIN Samples AS s ON a.FK_SampleUID = s.PK_SampleUID
                                        JOIN FreezerSection AS fs ON a.FK_FreezerSectID = fs.PK_FreezerSectID
                                        JOIN FreezerPhysical AS fp on fs.FK_FreezerPhysID = fp.PK_FreezerPhysID
                                        WHERE NOT Position1 = '' AND FreezerPhysName = '",
                                        freezer,
                                        "'"))
    dbDisconnect(con)
    rm(con)
  }
  return(nrow(positions))
}

#' Convenience function to properly format the discrepancies object
#'
#' This is purely an internal function to join the discrepancies and audits
#' tables during launch of FreezerCheck.
#'
#' @return A mutated version of the discrepancies data frame.
#' @export
make_discrep <- function() {
  discrepancies %>%
    left_join(audits) %>%
    select(c(1:4, 7, 5)) %>%
    mutate(lag = ifelse(is.na(date_resolved),
                        Sys.Date() - date_scanned,
                        date_resolved - date_scanned))
}

#' Create the progress data frame.
#'
#' Returns the object necessary to properly plot freezer progress by assigning x
#' and y coordinates and baseline progress value.
#'
#' @param freezers Character vector of freezers being tracked.
#'
#' @return A data frame formatted for graphing.
#' @export
#'
#' @examples
#' demo_mode <- TRUE
#' freezer_list <- get_freezers()
#' progress <- make_progress(freezer_list)
make_progress <- function(freezers) {
  out <- data.frame(percent = 0,
                    category = freezers,
                    y = 0,
                    x = 0)
  n_freezers <- ceiling(length(freezers) / 2)
  col_1y <- n_freezers:1
  col_1x <- rep(1, length(col_1y))
  col_2y <- (length(freezers) - n_freezers):1
  col_2x <- rep(3, length(col_2y))
  out <- out %>%
    mutate(y = c(col_1y, col_2y),
           x = c(col_1x, col_2x))
  overall <- data.frame(percent = 0,
                        category = "Overall",
                        y = 0,
                        x = 0)
  rbind(overall, out)
}

#' Minimize a selection list of top level containers
#'
#' This reduces the total number of aliquots to audit as defined by parameter
#' fraction. It applies multiple times through the standard workflow. This is an
#' internal function for other functions within FreezerCheck. A random selection
#' of the target data set is retained. Reduction focus takes place elsewhere.
#'
#' @param dat A data frame of observations to reduce.
#' @param fraction Numeric scalar between 0 and 1 representing the fraction to
#'   retain. (Default 0.1 for 10 percent.)
#'
#' @return A data frame of observations randomly reduced.
#' @export
#'
#' @examples
#' to_reduce <- demo_data[1:125, ]
#' minimize(to_reduce)
#' minimize(to_reduce, 0.25)
minimize <- function(dat, fraction = 0.1) {
  if (nrow(dat) == 0) return(NULL)
  return_n <- ceiling(nrow(dat) * fraction)
  dat %>% slice(sort(sample(nrow(.), return_n)))
}

#' Get the next audit index
#'
#' This selectively chooses the next audit index when a position is either
#' verified or rejected. By default it selects the next pending position check
#' in line, but also catches overflow and wraps back to prior positions or
#' returns 0 if all positions have been checked. This is intended as an internal
#' function only.
#'
#' @param index Integer scalar of the current audit aliquot index.
#' @param this_audit Data frame of the current audit.
#'
#' @return Integer scalar of the next audit aliquot index.
#' @export
next_audit_index <- function(index, this_audit) {
  if (this_audit$Status[index] == 'Pending') {
    return(index)
  } else {
    new_index <- index + 1
    if (new_index > nrow(this_audit)) new_index <- 1
    if (this_audit$Status[new_index] != "Pending") {
      if (nrow(this_audit %>% filter(Status == 'Pending')) == 0) {
        new_index <- 0
      } else {
        new_index <- min(grep("Pending", this_audit$Status))
      }
    }
    return(new_index)
  }
}

#' Create meaningful descriptors of containers
#'
#' This leverages the Freezerworks Section name (header 'FreezerName') to decide
#' on meaningful prefixes corresponding to values in Position1, Position2, and
#' Position3. This is intended as an internal function only, and heavily slanted
#' toward the use of Freezerworks within the NIST Biorepository. Feel free to
#' modify for your use to customize display options given your Freezerworks
#' setup. This is run on a section-by-section basis and the return is used to
#' modify those columns for tabular display within FreezerCheck.
#'
#' @param section The name of a freezer section from Freezerworks.
#'
#' @return A list of the prefixes for Position1, Position2, and Position3.
#' @export
parse_container <- function(section) {
  if (length(grep("Rack", section)) > 0) {
    out <- list(c1 = "Rack",
                c2 = "Box",
                c3 = "Position",
                structured = TRUE)
  } else if (length(grep("Tube", section)) > 0) {
    out <- list(c1 = "Tube",
                c2 = NULL,
                c3 = NULL,
                structured = FALSE)
  } else if (length(grep("Box", section)) > 0) {
    out <- list(c1 = "Shelf",
                c2 = "Box",
                c3 = "Position",
                structured = TRUE)
  } else if (length(grep("Pie", section)) > 0) {
    out <- list(c1 = "Pie",
                c2 = "Basket",
                c3 = NULL,
                structured = FALSE)
  } else if (length(grep("Case", section)) > 0) {
    out <- list(c1 = "Case",
                c2 = "Position",
                c3 = NULL,
                structured = TRUE)
  } else {
    out <- list(c1 = "Position 1",
                c2 = "Position 2",
                c3 = "Position 3",
                structured = FALSE)
  }
  return(out)
}

#' Clean up table headers
#'
#' Pulling data from Freezerworks often mangles header case presentation and
#' returns SQL names instead of display names. This provides sentence-case
#' formatting using a user-defined separator. As written, provides some
#' additional cleanup of commonly mis-formatted headers used in the NIST
#' Biorepository. Feel free to modify the list for your needs.
#'
#' @param s Character vector of words to clean.
#' @param sep Character scalar indicating word separation character. (Default
#'   "." but most used as "_".)
#'
#' @return Character vector matching parameter s with clean formatting.
#' @export
#'
#' @examples
#' if (!exists("demo_data")) demo_data <- read_rds("data/demo/demo_data.RDS")
#' simple_cap(names(demo_data, sep = "_"))
simple_cap <- function(s, sep = ".") {
  s <- strsplit(as.character(s), split = sep, fixed = TRUE)
  s <- sapply(s, function(x) paste(toupper(substring(x, 1, 1)),
                                   tolower(substring(x, 2)),
                                   sep = "", collapse = " "))
  # Individual overrides specific to NIST Biorepository
  s <- gsub("\\bId\\b", "ID", s)
  s <- gsub("\\bSrm\\b", "SRM", s)
  s <- gsub("\\bNrda\\b", "NRDA", s)
  s <- gsub("\\bSfei\\b", "SFEI", s)
  s <- gsub("\\bUsfws\\b", "USFWS", s)
  s <- gsub("Gusampleid", "GUSampleID", s)
  s <- gsub("Aliquotnumber", "Aliquot Number", s)
  s <- gsub("Aliquotscreationdate", "Aliquots Creation Date", s)
  s <- gsub("Samplestatus", "Sample Status", s)
  s <- gsub("Sampletype", "Sample Type", s)
  s <- gsub("Currentamount", "Current Amount", s)
  return(s)
}

#' View a given audit as a DT datatable
#'
#' This internal helper function does nothing more than consistently create a
#' DT::datatable() object from a given audit.
#'
#' @param audit Data frame of the audit to display
#'
#' @return DT::datatable object of specific type
#' @export
#'
#' @examples
#' # Must have audits present to be valid
#' view_audit(subset(audits, audit_index == 1))
view_audit <- function(audit) {
  datatable(audit %>%
              select(-audit_index, -date_scanned, -User) %>%
              mutate_at(c("Status", "SECTION", "POSITION1", "POSITION2", "POSITION3"),
                        as.factor),
            rownames = FALSE,
            colnames = pretty_colnames,
            selection = list(mode = 'single', selected = NULL),
            filter = 'top',
            options = list(dom = 'lftip',
                           autoWidth = TRUE,
                           columnDefs = list(list(className = 'dt-center', targets = 0:6),
                                             list(visible = FALSE, targets = 1))
                           )
  )
}

#' Describe a discrepancy
#'
#' Creates the HTML to populate a modal display for describing a particular
#' discrepancy. This is intended as an internal function only.
#'
#' @param discrep_index Integer scalar index value identifying a discrepancy.
#'
#' @return HTML snippet
#' @export
view_discrep_details <- function(discrep_index) {
  d <- discrepancies %>% filter(discrepancy_index == discrep_index)
  if (d$Resolved) {
    HTML(paste0('<div class="col-sm-2">',
                '<h3 class=success>Resolved</h3>',
                '<i class="fa fa-check-circle fa-3x success"></i></div>',
                '<div class="col-sm-10"><h3>&nbsp</h3>',
                '<p><b>Issue:</b> ',
                ifelse(d$Issue == "",
                       '(none recorded)',
                       d$Issue), '</p>',
                '<p><b>Action:</b> ',
                d$Action_Taken, '</p>',
                '<p><b>Resolved:</b> ',
                format.Date(d$date_resolved, "%d %b %Y"),
                '</p><hr></div>'))
  } else {
    HTML(paste0('<div class="col-sm-2">',
                '<h3 class=success>Outstanding</h3>',
                '<i class="fa fa-times-circle fa-3x failure"></i></div>',
                '<div class="col-sm-10"><h3>&nbsp</h3>',
                '<p><b>Issue:</b> ',
                ifelse(d$Issue == "",
                       '(none recorded)',
                       d$Issue),
                '</p>',
                '<p><b>Open Discrepancy</b></p><hr></div>'))
  }
}
