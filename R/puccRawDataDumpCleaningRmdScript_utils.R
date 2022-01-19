## PUCC Raw data dump cleaning Rmd script utility functions-------------------------------

#' Cleaning function for PUC data dump
#'
#'

clean_partitioned_pucc_data_dump_file <- function(df, input_file_name, clean_output_folder_rel_path){

  file_keywords_regex <- "certificate|certstring|mas|new|center"
  file_keyword_identifier <- str_extract(input_file_name, file_keywords_regex)


#   #> Convert all character columns to lower case
#   df <- convert_character_col_case(df, "lower")
#  print("converted all character columns to lower case")

  if(file_keyword_identifier == "certificate"){

    #> sort out redundant fuel types and create a MECE list of fuel type categories (need to work on this)

    # from regular expressions for fuel type
    from_reg_exps_fuel_type <- c("(petrol petrol)|(gasolin)", "(E|e)(L|l).+",
                                 "cng only", "lpg only",
                                 "(p|P|c|C)([a-z]|[A-Z])+(-|/)(c|C|p|P).+",
                                 "(p|P|l|L)([a-z]|[A-Z])+(-|/)(l|L|p|P).+",
                                 "(d|D|l|L)([a-z]|[A-Z])+(-|/)(l|L|d|D).+",
                                 "(d|D|c|C)([a-z]|[A-Z])+(-|/)(c|C|d|D).+",
                                 "not applicable",
                                 "white petrol",
                                 "no fuel")

    # to regular expressions for fuel type
    to_reg_exps_fuel_type <- c("petrol", "electric", "cng", "lpg", "petrol-cng", "petrol-lpg",
                               "diesel-lpg", "diesel-cng", "notapplicable",
                               "whitepetrol", "nofuel")

    # replace the values in the fuel type column that fit the "from" expressions into "to" expressions
    df <- replace_col_values(df, "t_1_certificate_vehicle_fuel_type",
                                              from_reg_exps_fuel_type, to_reg_exps_fuel_type)

    print("Step: 1/7 completed.")

    #> create a date only and time only column from time stamp
    df <- df %>%
      separate(t_1_certificate_vehicle_ts, c("t_1_certificate_vehicle_ts_date_only", "t_1_certificate_vehicle_ts_time_only"), " ", remove = FALSE)

    print("Step: 2/7 completed.")

    #> create a petrol code and diesel code column from the pucc number

    df <- df %>%
      mutate(t_1_certificate_vehicle_pucc_number_petrol_diesel_code = str_extract(t_1_certificate_vehicle_pucc_number, "(p|d|P|D)..."))

    print("Step: 3/7 completed.")

    #> create a state code column from the vehicle number column
    df <- df %>%
      mutate(t_1_certificate_vehicle_number_state_code = str_extract(t_1_certificate_vehicle_number, ".."))

    print("Step: 4/7 completed.")

    #> split machine reading column according to regular expressions and fuel types (need to work on this)

    # maximum number of hash delimiter in a given string in the machine reading column
    max_hashes_machine_reading <- 9

    # maximum number of hyphen delimiter in a given string in machine reading
    # max_hyphen_machine_reading <- 10 #?


      df <- df %>%
      separate(t_1_certificate_vehicle_machine_reading, c("t_1_certificate_vehicle_machine_reading_notdiesel_g1", "t_1_certificate_vehicle_machine_reading_notdiesel_g2",
                                                          "t_1_certificate_vehicle_machine_reading_notdiesel_g3",
                                                          "t_1_certificate_vehicle_machine_reading_notdiesel_g4",
                                                          "t_1_certificate_vehicle_machine_reading_notdiesel_g5",
                                                          "t_1_certificate_vehicle_machine_reading_notdiesel_g6",
                                                          "t_1_certificate_vehicle_machine_reading_notdiesel_g7"), "#", remove = FALSE)



    print("Step: 5/7 completed.")

    #> Convert all date, datetime, time character columns into date, datetime, time formats
    df$t_1_certificate_vehicle_ts <- as_datetime(df$t_1_certificate_vehicle_ts)

    df$t_1_certificate_vehicle_ts_date_only <- as_date(df$t_1_certificate_vehicle_ts_date_only)

    df$t_1_certificate_vehicle_ts_time_only <-
      readr::parse_time(df$t_1_certificate_vehicle_ts_time_only)

    print("Step: 6/7 completed.")

    #> write cleaned files to output folder
    input_file_name_ext_removed <- str_remove(input_file_name, ".csv")
    output_file_name <- str_c(input_file_name_ext_removed, "cleaned", ".csv", sep = "_")
    path_store_output_file <- str_c(clean_output_folder_rel_path, output_file_name, sep = "/")
    write_csv(df, path_store_output_file)

    print("Step: 7/7 completed.")

  } else if (file_keyword_identifier == "certstring"){

    #> create a state code column from the vehicle number column
    df <- df %>%
      mutate(t_2_certstring_vehicle_number_state_code = str_extract(t_2_certstring_vehicle_number, ".."))

    print("Step: 1/2 completed.")

    #> write cleaned files to output folder
    input_file_name_ext_removed <- str_remove(input_file_name, ".csv")
    output_file_name <- str_c(input_file_name_ext_removed, "cleaned", ".csv", sep = "_")
    path_store_output_file <- str_c(clean_output_folder_rel_path, output_file_name, sep = "/")
    write_csv(df, path_store_output_file)

    print("Step: 2/2 completed.")

  } else if (file_keyword_identifier == "mas"){

    #> sort out redundant fuel types and create a MECE list of fuel type categories (need to work on this)

    # from regular expressions for fuel type
    from_reg_exps_fuel_type <- c("(petrol petrol)|(gasolin)", "(E|e)(L|l).+",
                                 "cng only", "lpg only",
                                 "(p|P|c|C)([a-z]|[A-Z])+(-|/)(c|C|p|P).+",
                                 "(p|P|l|L)([a-z]|[A-Z])+(-|/)(l|L|p|P).+",
                                 "(d|D|l|L)([a-z]|[A-Z])+(-|/)(l|L|d|D).+",
                                 "(d|D|c|C)([a-z]|[A-Z])+(-|/)(c|C|d|D).+",
                                 "not applicable",
                                 "white petrol",
                                 "no fuel")

    # to regular expressions for fuel type
    to_reg_exps_fuel_type <- c("petrol", "electric", "cng", "lpg", "petrol-cng", "petrol-lpg",
                               "diesel-lpg", "diesel-cng", "notapplicable",
                               "whitepetrol", "nofuel")

    # replace the values in the fuel type column that fit the "from" expressions into "to" expressions
    df <- replace_col_values(df, "t_3_mas_vehicle_fuel_type",
                             from_reg_exps_fuel_type, to_reg_exps_fuel_type)


    print("Step: 1/9 completed.")

    #> sort out redundant engine stroke types and create a MECE list of engine stroke categories (need to work on this)

    # from regular expressions for engine stroke categories
    from_reg_exps_engine_stroke <- c("4", "2")

    # to regular expressions for engine stroke categories
    to_reg_exps_engine_stroke <- c("four_stroke", "two_stroke")

    # replace the values in the engine stroke column that fit the "from" expressions into "to" expressions
    df <- replace_col_values(df, "t_3_mas_vehicle_engine_stroke",
                             from_reg_exps_engine_stroke, to_reg_exps_engine_stroke )



    print("Step: 2/9 completed.")

    #> sort out redundant emission norms types and create a MECE list of emission norms categories (need to work on this)

    # from regular expressions for emission norms categories
    from_reg_exps_emission_norms <- c("(B|b)(?:.+)(6|(VI))", "(B|b)(?:.+)(4|(IV))", "(B|b)(?:.+)(3|(III))",
                                      "(B|b)(?:.+)(2|(II))")


    # to regular expressions for emission norms categories
    to_reg_exps_emission_norms <- c("bs_6", "bs_4", "bs_3", "bs_2")

    # replace the values in the emission norms column that fit the "from" expressions into "to" expressions
    df <- replace_col_values(df, "t_3_mas_vehicle_emission_norms",
                             from_reg_exps_emission_norms, to_reg_exps_emission_norms)

    print("Step: 3/9 completed.")

    #> create a petrol code and diesel code column from the pucc number
    df <- df %>%
      mutate(t_3_mas_vehicle_pucc_number_petrol_diesel_code = str_extract(t_3_mas_vehicle_pucc_number, "(p|d|P|D)..."))

    print("Step: 4/9 completed.")

    #> create a state code column from the vehicle number column
    df <- df %>%
    mutate(t_3_mas_vehicle_number_state_code = str_extract(t_3_mas_vehicle_number, ".."))

    print("Step: 5/9 completed.")

    #> create a date only and time only column from time stamp
    df <- df %>%
      separate(t_3_mas_ts, c("t_3_mas_ts_date_only", "t_3_mas_ts_time_only"), " ", remove = FALSE)

    print("Step: 6/9 completed.")

    #> remove pii
    df <- df %>%
      select(-c("t_3_mas_vehicle_owner_name", "t_3_mas_vehicle_owner_address"))

    print("Step: 7/9 completed.")

    #> Convert all date, datetime, time character columns into date, datetime, time formats
    df$t_3_mas_ts <- as_datetime(df$t_3_mas_ts)

    df$t_3_mas_ts_date_only <- as_date(df$t_3_mas_ts_date_only)

    df$t_3_mas_ts_time_only <-
      readr::parse_time(df$t_3_mas_ts_time_only)

    print("Step: 8/9 completed.")

    #> write cleaned files to output folder
    input_file_name_ext_removed <- str_remove(input_file_name, ".csv")
    output_file_name <- str_c(input_file_name_ext_removed, "cleaned", ".csv", sep = "_")
    path_store_output_file <- str_c(clean_output_folder_rel_path, output_file_name, sep = "/")
    write_csv(df, path_store_output_file)


    print("Step: 9/9 completed.")

  } else if (file_keyword_identifier == "new"){

    #> create a petrol code and diesel code column from the pucc number
    df <- df %>%
      mutate(t_4_new_vehicle_pucc_number_petrol_diesel_code = str_extract(t_4_new_vehicle_pucc_number, "(p|d|P|D)..."))

    print("Step: 1/3 completed.")

    #> Convert all date, datetime, time character columns into date, datetime, time formats (need to work on this)
    df$t_4_new_vehicle_test_date <- as_date(df$t_4_new_vehicle_test_date)

    df$t_4_new_vehicle_test_time <-
      readr::parse_time(as.character(df$t_4_new_vehicle_test_time)) # work here: Why was coercion to character needed, given that I read the column in as character in the first place.

    df$t_4_new_vehicle_pucc_valid_upto <- as_date(df$t_4_new_vehicle_pucc_valid_upto)

    print("Step: 2/3 completed.")

    #> write cleaned files to output folder
    input_file_name_ext_removed <- str_remove(input_file_name, ".csv")
    output_file_name <- str_c(input_file_name_ext_removed, "cleaned", ".csv", sep = "_")
    path_store_output_file <- str_c(clean_output_folder_rel_path, output_file_name, sep = "/")
    write_csv(df, path_store_output_file)

    print("Step: 3/3 completed.")

  } else if (file_keyword_identifier == "center"){

    #> create a date only and time only column from time stamp
    df <- df %>%
      separate(t_5_center_timestamp, c("t_5_center_timestamp_date_only", "t_5_center_timestamp_time_only"), " ", remove = FALSE)

    print("Step: 1/4 completed.")

    #> Convert all date, datetime, time character columns into date, datetime, time formats
    df$t_5_center_timestamp_date_only <- as_date(df$t_5_center_timestamp_date_only)

    print("Step: 2/4 completed.")

    df$t_5_center_timsestamp_time_only <-
      readr::parse_time(df$t_5_center_timestamp_time_only)

    df$t_5_center_timestamp <- as_datetime(df$t_5_center_timestamp)

    #> remove pii
    df <- df %>%
      select(-c("t_5_center_mobile", "t_5_center_total_fail",
                "t_5_center_total_pass",
                "t_5_center_total_pucc",
                "t_5_center_two_wheeler_cancel_count",
                "t_5_center_two_wheeler_fail_count",
                "t_5_center_two_wheeler_pass_count"))

    print("Step: 3/4 completed.")

    #> write cleaned files to output folder
    input_file_name_ext_removed <- str_remove(input_file_name, ".csv")
    output_file_name <- str_c(input_file_name_ext_removed, "cleaned", ".csv", sep = "_")
    path_store_output_file <- str_c(clean_output_folder_rel_path, output_file_name, sep = "/")
    write_csv(df, path_store_output_file)

    print("Step: 4/4 completed.")
  }
}



#' identify primary keys in a dataset
#'
#' This function takes a dataset as its input and returns the primary
#' key columns (if any) for that dataset.
#'
#' @importFrom purrr map
#'
#' @param df data frame whose primary keys need to be figured out
#'
#'
#' @examples
#' find_primary_keys(mtcars)
#'
#' @return a character vector of primary keys (if any), else return a message
#'         that no primary keys are available.
#'
#' @export

find_primary_keys <- function(df){
prim_key_lgl_list <- map(colnames(df), function(x) nrow(unique(df[, x])) == nrow(df))
prim_key_lgl_vec <- unlist(prim_key_lgl_list)
if(sum(!prim_key_lgl_vec) == length(prim_key_lgl_vec)){
  print("There are no primary keys in this dataset!")
} else {
  return(colnames(df)[prim_key_lgl_vec])
 }

}

#' Find column wise duplicates for a given table.
#'
#' This function takes as its input a dataframe and exports in the given output folder
#' one excel file each for each column of that dataframe. This exported excel file
#' contains the list of duplicate values in that column. Works for all types of
#' data, including remote databaes. In fact this was specifically written for
#' a postgres database.


find_duplicates_in_remote_df_cols <- function(df, output_folder_df_col_wise_dup_count){

  df_col_wise_duplicates_list <- list()
  for(i in 1:length(colnames(df))){
    df_col_wise_duplicates_list[[i]] <- df %>% count(!!as.symbol(colnames(df)[i])) %>%
      filter(n > 1) %>%
      collect()

    print(str_c("Column", i, "/",
                length(colnames(df)), "checked for duplicates",
                sep = " "))

  }

  for(j in 1 : length(df_col_wise_duplicates_list)){
    if(nrow(df_col_wise_duplicates_list[[j]]) > 0){
      tmp_output_file_name <- str_c(colnames(df_col_wise_duplicates_list[[j]])[1], ".csv")
      tmp_output_file_path <- str_c(output_folder_df_col_wise_dup_count, tmp_output_file_name, sep = "/")
      readr::write_csv(df_col_wise_duplicates_list[[j]], tmp_output_file_path)
      print(str_c(j, "/", length(df_col_wise_duplicates_list), "column's duplicates exported",  sep = " "))
    } else {
      print(str_c("No duplicates in column", j, "named", colnames(df_col_wise_duplicates_list[[j]])[1], sep = " "))
    }
  }

}


#' find unique or duplicate values in cols of a given remote dataframe.
#' This adds to the "find_duplicates_in_remote_df_cols" function by not allowing
#' for an operation type specification e.g. if we want to find unique values,
#' we enter the keyword: "unique", else we enter the keyword "duplicate".

find_uniq_or_dup_vals_in_remote_df_cols <- function(df, output_folder_df_col_wise_dup_or_unq_count,
                                                    type){

  if(type == "duplicate"){
    df_col_wise_dup_or_unq_list <- list()
    for(i in 1:length(colnames(df))){
      df_col_wise_dup_or_unq_list[[i]] <- df %>% count(!!as.symbol(colnames(df)[i])) %>%
        filter(n > 1) %>%
        collect()

      print(str_c("Column", i, "/",
                  length(colnames(df)), "checked for duplicates",
                  sep = " "))

       }
    } else if (type == "unique"){
      df_col_wise_dup_or_unq_list <- list()
      for(i in 1:length(colnames(df))){
        df_col_wise_dup_or_unq_list[[i]] <- df %>% count(!!as.symbol(colnames(df)[i])) %>%
          select(!!as.symbol(colnames(df)[i])) %>%
          collect()

        print(str_c("Column", i, "/",
                    length(colnames(df)), "checked for unique values",
                    sep = " "))
    }

  }

  for(j in 1 : length(df_col_wise_dup_or_unq_list)){
    if(nrow(df_col_wise_dup_or_unq_list[[j]]) > 0){
      tmp_output_file_name <- str_c(colnames(df_col_wise_dup_or_unq_list[[j]])[1], ".csv")
      tmp_output_file_path <- str_c(output_folder_df_col_wise_dup_or_unq_count, tmp_output_file_name, sep = "/")
      readr::write_csv(df_col_wise_dup_or_unq_list[[j]], tmp_output_file_path)
      print(str_c(j, "/", length(df_col_wise_dup_or_unq_list), "columns exported",  sep = " "))
    } else {
      print(str_c("In column", j, "named", colnames(df_col_wise_dup_or_unq_list[[j]])[1], "there are o rows", sep = " "))
    }
  }

}


#' get unique values for all columns for a given data frame
#'
#' For a given data frame, find the unique values for each column. Return
#' a list whose each element contains the unique values of each corresponding
#' column of the dataset. For example, the first element of the returned
#' list would contain the unique values of the first column of the dataset,
#' and so on.
#'
#' @param df the dataframe for which the column wise unique values are needed.
#'
#' @return a list whose xth element contain the unique values for the xth column
#'         of the \code{df}.
#'
#' @examples
#' find_unique_values_by_col(df = mtcars)
#'
#'
#' @export

find_unique_values_by_col <- function(df){
  return(map(colnames(df), function(x) unique(df[, x])))
}



#' convert character columns into lower/upper case
#'
#' This function converts all columns of type character and converts them
#' into lower or upper case.
#'
#' @param df data frame whose character columns need to be converted
#' @param case this can take one of two values: "lower": for converting to lower case
#'             or "upper" for converting to upper case.
#'
#' @examples
#' convert_character_col_case(df, "upper") # for converting to upper case.
#' convert_character_col_case(df, "lower") # for converting to lower case.
#'
#' @return returns the \code{df} but with character columns now converted to upper/lower
#'         case (as mentioned in the \code{case} parameter.
#'
#' @export


convert_character_col_case <- function(df, case = "lower"){
  colnames_df <- colnames(df)
  for(i in 1:length(colnames_df)){
    if(typeof(unlist(df[, i])) == "character"){
      if(case == "lower"){
        df[, i] <- str_to_lower(as.character(unlist(df[, i])))
      } else {
        df[, i] <- str_to_upper(as.character(unlist(df[, i])))
      }

    } else {
      # do nothing
    }

  }
 return(df)
}


#' Clean columns based on pre-specified regular expressions
#'
#' This function is built specifically for cleaning a column based on regular
#' expressions. This takes in a data frame, a column name from that data frame,
#' a character vector of "from" regular expressions, a character vector of "to"
#' regular expressions. In the column specified, the function detects the entries
#' that match the "from" regular expression pattern and convert them to the "to"
#' regular expression.
#'
#' @param df data frame inside of which the column values need to be replaced.
#' @param fuel_type_col_name name of the column (within which replacements will happen).
#'                           This should be a character vector of length 1.
#'
#' @param from_reg_exps a character vector of "from" regular expressions.
#' @param to_reg_exps a character vector (corresponding to \code{from_reg_exps})
#'                    of "to" regular expressions.
#'
#' @return returns \code{df} but with column values changed as per the \code{to_reg_exps}
#'
#'
#' @examples
#' replace_col_values(datasets::CO2, "Type", "Q", "q")
#'
#' @export

replace_col_values <- function(df, fuel_type_col_name, from_reg_exps, to_reg_exps){
  for(i in 1:length(from_reg_exps)){
   df[, fuel_type_col_name] <- str_replace(as.character(unlist(df[, fuel_type_col_name])), from_reg_exps[i], to_reg_exps[i])
  }
  return(df)
}

#' rename columns for pucc data dump tables
#'
#' This function renames the columns of all given csv files (which should belong to
#' one of the five categories of table types in the pucc data dump) in a given input folder.
#' It takes in as its inputs the path to the input folder, path to the output folder,
#' 5 character vectors (one for each table) containing the new column names for that table.
#' For all the files in the given input directory, it automatically detects the "table
#' type" from the file name (which follows a specific naming scheme) and then reassigns
#' the column names. Once the names are reassigned, it writes and exports these new
#' csv files (with new column names) to the specified output folder.
#'
#'
#'

rename_cols_pucc_data_dump <- function(rel_path_input_folder, rel_path_output_folder,
                                       t1_new_col_names, t2_new_col_names,
                                       t3_new_col_names, t4_new_col_names,
                                       t5_new_col_names){

  input_folder_file_list <- list.files(rel_path_input_folder)
  file_keywords_regex <- "certificate|certstring|mas|new|center"

  for(i in 1:length(input_folder_file_list)){
    cur_file_name <- input_folder_file_list[i]
    file_keyword_identifier <- str_extract(cur_file_name, file_keywords_regex)
    cur_df <- readr::read_csv(str_c(rel_path_input_folder, cur_file_name, sep = "/"))
    if(file_keyword_identifier == "certificate"){
      colnames(cur_df) <- t1_new_col_names
    } else if (file_keyword_identifier == "certstring"){
      colnames(cur_df) <- t2_new_col_names
    } else if (file_keyword_identifier == "mas") {
      colnames(cur_df) <- t3_new_col_names
    } else if (file_keyword_identifier == "new"){
      colnames(cur_df) <- t4_new_col_names
    } else if (file_keyword_identifier == "center"){
      colnames(cur_df) <- t5_new_col_names
    }

    cur_df_output_file_name <- stringr::str_remove(cur_file_name, ".csv")
    cur_df_output_file_name <- str_c(cur_df_output_file_name, "short_colnames", ".csv", sep = "_")

    readr::write_csv(cur_df, str_c(rel_path_output_folder, cur_df_output_file_name, sep = "/"))

    print(str_c("Shortened Column Names File", i, "/",
                length(input_folder_file_list), "exported", sep = " "))
  }


}

