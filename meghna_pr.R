meghna_pr <- function(input_data = c("aerobic", "anaerobic", "table"),
                      plate_data, metadata, time_interval, filtered_interval) {

    input_data <- match.arg(input_data)
    cat("This is the 'meghna_pr' program, version 2.50\n")

    # Load required packages
    packages <- c("tidyverse", "ggplot2", "purrr", "readxl", "tidyselect",
                  "dplyr", "openxlsx", "gtools", "stringr", "reshape2", "RColorBrewer")
    load_or_install_packages <- function(packages) {
        for (pkg in packages) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                cat("Package", pkg, "not found. Installing...\n")
                install.packages(pkg)
            }
            suppressPackageStartupMessages(library(pkg, character.only = TRUE))
        }
    }

    load_or_install_packages(packages)

    # Set up color palette
    n <- 60
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    col_vector <- sample(col_vector, n)

    # Create or overwrite QC directory
    create_overwrite_dir <- function(dir_path) {
        if (dir.exists(dir_path)) {
            response <- readline(sprintf("The directory '%s' already exists. Do you want to overwrite it? (y/n): ",
                                         dir_path))
            if (tolower(response) == "y") {
                unlink(dir_path, recursive = TRUE)
                dir.create(dir_path)
                cat(sprintf("The directory '%s' has been overwritten.\n",
                            dir_path))
            } else {
                cat(sprintf("The directory '%s' was not overwritten.\n",
                            dir_path))
            }
        } else {
            dir.create(dir_path)
            cat(sprintf("The directory '%s' has been created.\n", dir_path))
        }
    }

    create_overwrite_dir("QC")
    cat("DONE\n")

    # Validate and Import Plate Data
    validate_plate_data <- function(plate_data, input_data) {
        if (input_data == "aerobic") {
            required_cols <- c("date", "text", rep("numeric", 90))
            check_file_format <- function(file_path, required_cols) {
                tryCatch({
                    df <- read_excel(file_path, range = "B41:CO187", col_names = TRUE,
                                     col_types = required_cols)
                    return(TRUE)
                }, error = function(e) {
                    cat("Error reading aerobic data file:", e$message, "\n")
                    return(FALSE)
                })
            }
            if (!check_file_format(plate_data, required_cols))
                stop("Aerobic data file format is incorrect.")
        } else if (input_data == "anaerobic") {
            tryCatch({
                read.xlsx(plate_data)
                return(TRUE)
            }, error = function(e) {
                cat("Error reading anaerobic data file:", e$message, "\n")
                return(FALSE)
            })
        } else if (input_data == "table") {
            tryCatch({
                first_line <- readLines(plate_data, n = 1)
                sep_char <- ifelse(sum(str_count(first_line, "\t")) > sum(str_count(first_line,
                                                                                   ",")), "\t", ",")

                read.table(plate_data, sep = sep_char, header = TRUE)
                return(TRUE)
            }, error = function(e) {
                cat("Error reading table data file:", e$message, "\n")
                return(FALSE)
            })
        }
    }

    validate_metadata <- function(metadata) {
        tryCatch({
            df <- read.table(metadata, sep = "\t", header = TRUE)
            colnames(df) <- tolower(gsub("\\s+", "", colnames(df)))
            required_cols <- c("well", "id", "condition1", "condition2", "replicate")
            missing_cols <- setdiff(required_cols, colnames(df))
            if (length(missing_cols) > 0)
                stop(paste("Metadata file is missing required columns:",
                           paste(missing_cols, collapse = ", ")))

            # Print contents of the condition columns for debugging
            cat("Condition1 column: ", paste(df$condition1, collapse = ", "), "\n")
            cat("Condition2 column: ", paste(df$condition2, collapse = ", "), "\n")

            if (!any(grepl("blank", df$condition1, ignore.case = TRUE)) &&
                !any(grepl("blank", df$condition2, ignore.case = TRUE))) {
                stop("No 'blank' conditions found in metadata.")
            }

            # Select the condition column with 'blank'
            df$condition <- ifelse(grepl("blank", df$condition1, ignore.case = TRUE),
                                   df$condition1, df$condition2)

            # Remove unused condition columns and duplicate columns
            condition_cols <- grep("^condition", colnames(df), value = TRUE)
            to_remove <- c()
            for (col in condition_cols) {
                for (compare_col in setdiff(condition_cols, col)) {
                    if (all(df[[col]] == df[[compare_col]], na.rm = TRUE)) {
                        if (grepl("[0-9]", col)) {
                            to_remove <- c(to_remove, col)
                        } else {
                            to_remove <- c(to_remove, compare_col)
                        }
                    }
                }
            }
            to_remove <- unique(to_remove)
            df <- df %>% select(-all_of(to_remove))

            return(df)
        }, error = function(e) {
            cat("Error reading metadata file:", e$message, "\n")
            return(FALSE)
        })
    }

    if (!validate_plate_data(plate_data, input_data))
        stop("Invalid plate data.")
    metadata_df <- validate_metadata(metadata)
    if (isFALSE(metadata_df))
        stop("Invalid metadata.")

    # Function to import and clean aerobic data
    import_aerobic_data <- function(plate_data) {
        cat("Importing raw aerobic plate reader data in Excel format\n")
        my_df <- read_excel(plate_data, range = "B41:CO187", col_names = TRUE,
                            col_types = c("date", "text", rep("numeric", 90)))
        cat("DONE\n")

        cat("Removing rows and columns with no data\n")
        not_all_na <- function(x) any(!is.na(x))
        my_df <- my_df %>%
            filter(apply(., 1, not_all_na)) %>%
            select(where(not_all_na))
        colnames(my_df) <- tolower(colnames(my_df))
        cat("DONE\n")

        cat("Removing 'Temperature' column\n")
        my_df <- my_df %>%
            select(-"t° 600")
        cat("DONE\n")

        write.table(my_df, file = "QC/01_raw_data.txt", sep = "\t", col.names = NA)
        return(my_df)
    }

    # Function to import and clean anaerobic data
    import_anaerobic_data <- function(plate_data, time_interval) {
        cat("Importing raw anaerobic plate reader data in Excel format\n")
        excel_data <- read.xlsx(plate_data)
        wavelength_rows <- which(excel_data[, 1] == "Wavelength(nm)")

        data_frames_list <- map(wavelength_rows, ~{
            start_row <- .x + 1
            end_row <- start_row + 8
            grid <- excel_data[start_row:end_row, 1:13]
            row.names(grid) <- grid[, 1]
            colnames(grid) <- grid[1, ]
            grid <- grid[-1, -1]
            grid
        })

        convert_96_well_to_wide <- function(df, time_value) {
            df %>%
                as_tibble(rownames = "Row") %>%
                gather(Column, Value, -Row) %>%
                mutate(Well = paste0(Row, Column)) %>%
                select(Well, Value) %>%
                spread(Well, Value) %>%
                select(mixedsort(colnames(.))) %>%
                mutate_if(is.character, as.numeric) %>%
                select_if(~!all(is.na(.))) %>%
                add_column(Time = time_value, .before = 1)
        }

        time_values <- seq(time_interval, time_interval * length(data_frames_list),
                           by = time_interval)
        my_df <- map2_df(data_frames_list, time_values, convert_96_well_to_wide)
        colnames(my_df) <- tolower(colnames(my_df))

        write.table(my_df, file = "QC/01_raw_data.txt", sep = "\t", col.names = NA)
        cat("DONE\n")
        return(my_df)
    }

    # Function to import and clean table data
    import_table_data <- function(plate_data) {
        cat("Importing plate reader data in tab-separated or comma-separated format\n")
        first_line <- readLines(plate_data, n = 1)
        sep_char <- ifelse(sum(str_count(first_line, "\t")) > sum(str_count(first_line,
                                                                           ",")), "\t", ",")

        my_df <- read.table(plate_data, sep = sep_char, header = TRUE)
        write.table(my_df, file = "QC/01_raw_data.txt", sep = "\t", row.names = FALSE)
        cat("DONE\n")
        return(my_df)
    }

    # Select and import data based on input_data
    my_df <- switch(input_data, aerobic = import_aerobic_data(plate_data),
                    anaerobic = import_anaerobic_data(plate_data, time_interval), table = import_table_data(plate_data))

    # Import metadata table
    cat("Importing metadata table\n")
    import_table_with_column_check <- function(file_path_meta, sep = "\t", header = TRUE) {
        data <- read.table(file_path_meta, sep = sep, header = header)
        colnames(data) <- tolower(gsub("\\s+", "", colnames(data)))
        actual_colnames <- colnames(data)

        condition_cols <- grep("^condition", actual_colnames, value = TRUE)

        if (length(condition_cols) < 1) {
            stop("No 'condition' column found.")
        }

        # Print condition columns for debugging
        cat("Condition columns found: ", paste(condition_cols, collapse = ", "), "\n")

        # Check if any condition column has 'blank'
        has_blank <- sapply(data[condition_cols], function(col) any(grepl("blank", col, ignore.case = TRUE)))

        # Print which condition columns contain 'blank'
        cat("Columns containing 'blank': ", paste(condition_cols[has_blank], collapse = ", "), "\n")

        if (!any(has_blank)) {
            stop("No 'blank' conditions found in metadata.")
        }

        # Select the condition column with 'blank'
        selected_condition_col <- condition_cols[which(has_blank)[1]]
        data$condition <- data[[selected_condition_col]]

        # Print the selected condition column for debugging
        cat("Selected condition column: ", selected_condition_col, "\n")

        data[] <- lapply(data, function(x) gsub("\\s+", "", x, perl = TRUE))
        data[] <- lapply(data, tolower)
        
        # Remove unused condition columns and duplicate columns
        to_remove <- c()
        for (col in condition_cols) {
            for (compare_col in setdiff(condition_cols, col)) {
                if (all(data[[col]] == data[[compare_col]], na.rm = TRUE)) {
                    if (grepl("[0-9]", col)) {
                        to_remove <- c(to_remove, col)
                    } else {
                        to_remove <- c(to_remove, compare_col)
                    }
                }
            }
        }
        to_remove <- unique(to_remove)
        data <- data %>% select(-all_of(to_remove))

        return(data)
    }

    meta <- import_table_with_column_check(metadata)
    cat("DONE\n")

    # Adding time intervals
    cat("Adding time intervals\n")
    num_rows <- nrow(my_df)
    my_df <- my_df %>%
        mutate(time = seq(time_interval, by = time_interval, length.out = num_rows))
    cat("DONE\n")
    write.table(my_df, file = "QC/02_data_time.txt", sep = "\t", col.names = NA)

    # Check and plot blanks
    check_and_plot_blanks <- function(my_df, meta, filtered_interval) {
        cat("Checking that the 'blank' conditions are actually blank\n")
        column_names_blanks <- setdiff(colnames(my_df), "time")

        my_df_melt <- melt(my_df, id.vars = "time", measure.vars = column_names_blanks)

        # Debugging information: Check contents of my_df_melt
        cat("Debugging info: my_df_melt\n")
        print(head(my_df_melt))

        # Debugging information: Check contents of meta
        cat("Debugging info: meta\n")
        print(head(meta))

        my_df_melt_merged <- merge(my_df_melt, meta, by.x = "variable", by.y = "well", all.x = TRUE)

        # Debugging information: Check the result of the merge
        cat("Debugging info: my_df_melt_merged\n")
        print(head(my_df_melt_merged))

        # Dynamically identify all condition columns
        condition_cols <- grep("^condition", colnames(my_df_melt_merged), value = TRUE)

        # Adjust filter to remove rows where all condition columns are NA
        my_df_melt_merged <- my_df_melt_merged %>%
            filter(rowSums(is.na(select(., all_of(condition_cols)))) < length(condition_cols)) %>%
            arrange(variable, time)

        # Debugging information: Check the filtered my_df_melt_merged
        cat("Debugging info: filtered my_df_melt_merged\n")
        print(head(my_df_melt_merged))

        max_time_blanks <- max(as.numeric(my_df_melt_merged$time), na.rm = TRUE)  # Use na.rm = TRUE to avoid NA issues
        cat("max_time_blanks: ", max_time_blanks, "\n")  # Print the max_time_blanks value

        max_time_multiple_blanks <- (max_time_blanks %/% filtered_interval) * filtered_interval
        cat("max_time_multiple_blanks: ", max_time_multiple_blanks, "\n")  # Print the max_time_multiple_blanks value

        if (!is.finite(max_time_multiple_blanks)) {
            stop("Invalid max_time_multiple_blanks value. Check the data.")
        }

        my_df_filt <- my_df_melt_merged %>%
            filter(time %in% seq(0, max_time_multiple_blanks, by = filtered_interval))

        # Adjust filter to include rows where any condition column contains 'blank'
        my_df_filt <- my_df_filt %>%
            filter(rowSums(sapply(select(., all_of(condition_cols)), function(x) grepl("blank", x, ignore.case = TRUE))) > 0)
        cat("DONE\n")

        cat("Plotting individual blank samples\n")
        ggplot(my_df_filt, aes(x = time, y = value, group = variable, color = variable)) +
            geom_line(linewidth = 1) + geom_point(size = 1.5) + theme(text = element_text(size = 12)) +
            labs(x = "Time (mins)", y = "Absorbance") + scale_colour_manual(values = col_vector)

        ggsave("QC/absorbance_blanks_individual_replicates.png", width = 15, height = 10, dpi = "retina")
        ggsave("QC/absorbance_blanks_individual_replicates.eps", width = 15, height = 10, dpi = "retina")
        cat("DONE\n")

        return(my_df_filt)
    }

    my_df_filtered <- check_and_plot_blanks(my_df, meta, filtered_interval)

    # Further processing and plotting
    cat("Removing problematic blank samples, if necessary\n")

    rem_conditions <- data.frame(replicate = character(), condition = character(), variable = character())
    rem_conditions <- rbind(rem_conditions, data.frame(replicate = NA, condition = NA, variable = NA))

    filterReplicate <- function(df, condition) {
        replicate_list <- unique(df$replicate)
        to_remove <- vector()

        for (rep in replicate_list) {
            rep_df <- df[df$replicate == rep, ]
            if (any(rep_df$value > 0.25)) {
                to_remove <- c(to_remove, rep)
                print(paste("Removed replicate:", rep, "from condition:", condition))

                rem_conditions <<- rbind(rem_conditions, data.frame(replicate = rep, condition = condition, variable = rep_df$variable[1]))
            }
        }

        return(df[!df$replicate %in% to_remove, ])
    }

    df_list <- split(my_df_filtered, my_df_filtered$condition)
    filtered_list <- mapply(filterReplicate, df_list, names(df_list), SIMPLIFY = FALSE)
    filtered_df <- do.call(rbind, filtered_list)

    rem_conditions <- rem_conditions[-1, ]
    colnames(rem_conditions) <- c("replicate", "condition", "well")

    if (length(unique(filtered_df$replicate)) < 3) {
        stop("There are not enough blank replicates remaining after removing outlier samples. Analysis cannot continue")
    }

    cat("DONE\n")

    cat("Plotting replicates with outliers removed\n")

    ggplot(filtered_df, aes(x = time, y = value, group = variable, color = variable)) +
        geom_line(linewidth = 1) + geom_point(size = 1.5) + theme(text = element_text(size = 12)) +
        labs(x = "Time (mins)", y = "Absorbance") + scale_colour_manual(values = col_vector)

    ggsave("QC/absorbance_blanks_individual_replicates_outliers_removed.png", width = 15, height = 10, dpi = "retina")
    ggsave("QC/absorbance_blanks_individual_replicates_outliers_removed.eps", width = 15, height = 10, dpi = "retina")

    in_rem_conditions <- function(condition, replicate, well) {
        for (i in 1:nrow(rem_conditions)) {
            if (condition == rem_conditions$condition[i] & replicate == rem_conditions$replicate[i] & well == rem_conditions$well[i]) {
                return(TRUE)
            }
        }
        return(FALSE)
    }

    cat("DONE\n")

    cat("Removing problematic blanks from the metadata and data tables\n")

    if (nrow(rem_conditions) == 0) {
        meta_filtered <- meta
    } else {
        rem_rows <- mapply(in_rem_conditions, meta$condition, meta$replicate, meta$well)
        meta_filtered <- meta[!rem_rows, ]
    }

    cols_to_remove <- rem_conditions$well
    my_df_final_time_filtered <- my_df[, !(names(my_df) %in% cols_to_remove)]

    cat("DONE\n")

    cat("Defining and averaging the 'blank' samples\n")

    blank_cols <- meta_filtered %>%
        filter(if_any(starts_with("condition"), ~str_detect(., "blank"))) %>%
        pull(well)

    my_df_final_time_filtered <- my_df_final_time_filtered %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Blank_Average = mean(dplyr::c_across(dplyr::all_of(blank_cols)), na.rm = TRUE)) %>%
        dplyr::ungroup()

    write.table(my_df_final_time_filtered, file = "QC/04_data_blanks_removed.txt", sep = "\t", col.names = NA)

    my_df_final_time_filtered_blanked <- my_df_final_time_filtered

    my_df_final_time_filtered_blanked[, 2:ncol(my_df_final_time_filtered_blanked)] <-
        my_df_final_time_filtered_blanked[, 2:ncol(my_df_final_time_filtered_blanked)] - my_df_final_time_filtered_blanked$Blank_Average

    my_df_final_time_filtered_blanked$Blank_Average <- NULL

    write.table(
        my_df_final_time_filtered_blanked,
        file = "QC/05_data_blanks_subtracted.txt",
        sep = "\t",
        col.names = NA
    )

    cat("DONE\n")

    cat("Melting data to a long format\n")

    column_names <- setdiff(colnames(my_df_final_time_filtered_blanked), "time")

    my_df_final_time_filtered_blanked_melt <-
        melt(my_df_final_time_filtered_blanked,
             id.vars = "time",
             measure.vars = column_names)

    write.table(
        my_df_final_time_filtered_blanked_melt,
        file = "QC/06_data_melt.txt",
        sep = "\t",
        col.names = NA
    )

    cat("DONE\n")

    cat("Adding metadata to the main data table\n")

    my_df_final_time_filtered_blanked_melt_merged <- merge(my_df_final_time_filtered_blanked_melt,
        meta_filtered, by.x = "variable", by.y = "well", all.x = TRUE)

    my_df_final_time_filtered_blank_melt_merged <- my_df_final_time_filtered_blanked_melt_merged %>%
        filter(variable %in% meta_filtered$well) %>%
        arrange(variable, time)

    # Remove rows with missing data in the "replicate" column
    my_df_final_time_filtered_blank_melt_merged <- my_df_final_time_filtered_blank_melt_merged %>%
        filter(!is.na(replicate) & replicate != "")

    write.table(my_df_final_time_filtered_blank_melt_merged, file = "QC/07_data_filtered.txt",
        sep = "\t", col.names = NA)

    cat("DONE\n")

    cat("Filtering data according to the 'filtered interval'\n")

    max_time <- max(as.numeric(my_df_final_time_filtered_blank_melt_merged$time))
    max_time_multiple <- (max_time %/% filtered_interval) * filtered_interval

    my_df_final_time_filtered_blanked_melt_merged_filt <- my_df_final_time_filtered_blank_melt_merged %>%
        filter(time %in% seq(0, max_time_multiple, by = filtered_interval))

    cat("DONE\n")

    cat("Removing 'blank' conditions from the data table\n")

    my_df_final_time_filtered_blanked_melt_merged_filt <- my_df_final_time_filtered_blanked_melt_merged_filt %>%
        filter_all(all_vars(!grepl("blank", .)))

    # Remove rows with missing data in the "replicate" column
    my_df_final_time_filtered_blanked_melt_merged_filt <- my_df_final_time_filtered_blanked_melt_merged_filt %>%
        filter(!is.na(replicate) & replicate != "")

    write.table(my_df_final_time_filtered_blanked_melt_merged_filt, file = "QC/08_data_meta_merged.txt",
        sep = "\t", col.names = NA)

    cat("DONE\n")

    cat("Calculating summary statistics\n")

    condition_columns <- grep("^condition", colnames(my_df_final_time_filtered_blanked_melt_merged_filt), value = TRUE)

    # Dynamically adjust the grouping by available condition columns
    my_df_final_time_filtered_blanked_melt_merged_filt_stats <- my_df_final_time_filtered_blanked_melt_merged_filt %>%
        dplyr::group_by(across(all_of(c(condition_columns, "time")))) %>%
        dplyr::summarise(count = dplyr::n(), mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE) / sqrt(dplyr::n()), ci_lower = mean(value,
                na.rm = TRUE) - 1.96 * (sd(value, na.rm = TRUE) / sqrt(dplyr::n())),
            ci_upper = mean(value, na.rm = TRUE) + 1.96 * (sd(value, na.rm = TRUE) / sqrt(dplyr::n())), .groups = 'drop')

    # Debugging information: Check the summary statistics
    cat("Debugging info: Summary statistics\n")
    print(head(my_df_final_time_filtered_blanked_melt_merged_filt_stats))

    # Dynamically adjust the filtering condition
    condition_columns <- grep("^condition", colnames(my_df_final_time_filtered_blanked_melt_merged_filt_stats), value = TRUE)

    my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean <- my_df_final_time_filtered_blanked_melt_merged_filt_stats %>%
        filter(!is.na(.[[condition_columns[1]]]))

    cols <- grep("^condition", colnames(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean),
        value = TRUE)

    my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean <- as.data.frame(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean)

    my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean <- my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean %>%
        dplyr::rowwise() %>%
        dplyr::filter(!base::any(dplyr::c_across(dplyr::all_of(cols)) == "blank")) %>%
        dplyr::ungroup()

    write.table(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean,
        file = "QC/09_data_summary_stats.txt", sep = "\t", col.names = NA)

    cat("DONE\n")

    cat("Determining the 'conditions' for plotting\n")

    condition_columns <- grep("^condition", names(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean),
        value = TRUE)

    if (length(condition_columns) > 1) {
        combinations <- combn(condition_columns, 2, simplify = FALSE)
    } else {
        combinations <- list(condition_columns)
    }

    cat("DONE\n")

    # Ensure debugging information is printed
    cat("Debugging info: my_df_final_time_filtered_blanked_melt_merged_filt\n")
    print(head(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean))

    # Ensure there is data to plot
    if (nrow(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean) == 0) {
        stop("No data available for plotting after filtering.")
    }

    cat("Generating plots\n")

    # Generate interaction columns and identify the valid one
    interaction_vars <- vector("character", length(combinations))
    for (i in seq_along(combinations)) {
        if (length(combinations[[i]]) > 1) {
            interaction_var <- paste0("interaction", i)
            my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean[[interaction_var]] <- interaction(
                my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean[[combinations[[i]][1]]],
                my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean[[combinations[[i]][2]]]
            )
            interaction_vars[i] <- interaction_var
        }
    }

    # Identify valid interaction columns (those that do not contain only NA values)
    valid_interaction_vars <- interaction_vars[sapply(interaction_vars, function(col) {
        !all(is.na(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean[[col]]))
    })]

    plot_list <- list()
    if (length(valid_interaction_vars) > 0) {
        for (combination in combinations) {
            for (interaction in valid_interaction_vars) {

                for (condition in combination) {
                    other_condition <- setdiff(combination, condition)

                    # Debugging information
                    cat("Plotting info: condition =", condition, "interaction =", interaction, "\n")
                    print(head(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean))

                    p <- ggplot(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean,
                                aes(x = time, y = mean, color = !!rlang::sym(interaction))) +
                        geom_line() + geom_point(size = 1.5) + geom_errorbar(aes(ymin = ci_lower,
                                                                                 ymax = ci_upper), width = 0.2) +
                        labs(x = "Time (mins)", y = "Absorbance", color = "Conditions (interaction)") +
                        ggtitle(paste("Plot for", condition, "and", interaction, "interaction")) +
                        scale_color_manual(values = col_vector)

                    plot_list[[paste0(condition, "_", interaction, "_individual")]] <- p

                    p <- ggplot(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean,
                                aes(x = time, y = mean, color = !!rlang::sym(other_condition))) +
                        geom_line() + geom_point(size = 1.5) + geom_errorbar(aes(ymin = ci_lower,
                                                                                 ymax = ci_upper), width = 0.2) +
                        labs(x = "Time (mins)", y = "Absorbance", color = other_condition) +
                        ggtitle(paste("Plot for", condition, "separated by", interaction, "interaction")) +
                        facet_wrap(as.formula(paste("~", interaction))) +  # separate by interaction
                        scale_color_manual(values = col_vector)

                    plot_list[[paste0(condition, "_", interaction, "_facet")]] <- p
                }
            }
        }
    } else {
        for (condition in condition_columns) {
            p <- ggplot(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean,
                        aes(x = time, y = mean, color = !!rlang::sym(condition))) +
                geom_line() + geom_point(size = 1.5) + geom_errorbar(aes(ymin = ci_lower,
                                                                         ymax = ci_upper), width = 0.2) +
                labs(x = "Time (mins)", y = "Absorbance", color = condition) +
                ggtitle(paste("Plot for", condition)) +
                scale_color_manual(values = col_vector)

            plot_list[[paste0(condition, "_individual")]] <- p

            p <- ggplot(my_df_final_time_filtered_blanked_melt_merged_filt_stats_clean,
                        aes(x = time, y = mean, color = !!rlang::sym(condition))) +
                geom_line() + geom_point(size = 1.5) + geom_errorbar(aes(ymin = ci_lower,
                                                                         ymax = ci_upper), width = 0.2) +
                labs(x = "Time (mins)", y = "Absorbance", color = condition) +
                ggtitle(paste("Plot for", condition, "facet")) +
                facet_wrap(as.formula(paste("~", condition))) +  # separate by current condition
                scale_color_manual(values = col_vector)

            plot_list[[paste0(condition, "_facet")]] <- p
        }
    }

    for (name in names(plot_list)) {
        plot_name_png <- paste0("plot_", name, ".png")
        plot_name_eps <- paste0("plot_", name, ".eps")

        ggsave(filename = plot_name_png, plot = plot_list[[name]], width = 6, height = 4)
        ggsave(filename = plot_name_eps, plot = plot_list[[name]], width = 6, height = 4)
        cat("DONE\n")
    }

    cat("Excellent work! The script has (hopefully) completed successfully and your plots have been saved to your directory.\n")
}
