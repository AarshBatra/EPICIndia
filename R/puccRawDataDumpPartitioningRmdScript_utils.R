## PUCC Raw data dump partitioning Rmd script utility functions-------------------------------

#' Partition all .csv Excel files in a given directory for the PUCC data dump
#'
#' This function partitions each .csv file (in a given directory). The total number
#' of partitions depends on the threshold at which we would like to partition the
#' files in the directory. If "pucc_col_type_name_specify" = TRUE, then this function
#' names the columns for the pucc files. As of now, this feature is only work for
#' the files that we received from the transport department. Later on, I will write
#' a more general function. To simply partition the files (without setting column names,
#' column types), set "pucc_col_type_name_specify" = FALSE.
#'
#'
#' @importFrom stringr str_c
#'
#' @param directory_rel_path relative path to the directory containing the .csv files
#' @param test_mode if trying out the function, before the real work, set this to "yes". This tests the
#'                  partitioning on a test dataset. The test dataset can be specified in \code{test_df}
#'                  argument.
#'
#' @param rename_files if this is set to "yes", the files in the directory path specified will be renamed.
#'                     This is again a, pucc_specific argument. Also, as of now, it is inactive, and
#'                     defaults to "no". So, please name your master files appropriately before partitioning.
#'
#' @param partition_at the row number at which, post which a partition should be created. Default to 1e+06
#' @param test_df this is the dataframe used when \code{test_mode} = "yes"
#' @param pucc_col_type_name_specify if this is TRUE, the pucc data columns will be apprpriately named. Also,
#'                                   all column types for pucc would be set. Note that this only applies
#'                                   to the files received from the transport department.
#'
#' @examples
#' partition_data_pucc_specific("./path/to/data/directory", test_mode = "no", partition_at = 500000)
#' partition_data_pucc_specific("./path/to/data/directory", test_mode = "yes", test_df = mtcars)
#'
#' @return returns a list whose first element is a list of lists. In this list of lists, the first layer represents
#'                 the master files, and the second layer contains the partitions of these files
#'                 (again stored as a list). So, the returned object is a 3 layer deep list, which
#'                 is navaigated using x[[i]][[j]][[k]], syntax.
#'
#'
#' @export



partition_data_pucc_specific <- function(directory_rel_path, test_mode = "yes", rename_files = "no",
            partition_at = 1000000, test_df = mtcars, pucc_col_type_name_specify = FALSE){

  # Hyperparameters set-----------------------------------------------------------

  # if test mode = "yes", then set the file_list to "test", else, set the file list to
  # vector of files in the "directory_rel_path" variable.
  if(test_mode == "yes"){
    file_list <- c("test")
  } else {
    file_list <- list.files(directory_rel_path)

  }

  # total number of files in "directory_rel_path"
  file_list_length <- length(file_list)
  print(str_c("Total number of files in the centers_table_5 directory = ", file_list_length))

  # rename files in the "directory_rel_path" (if required, as of now I assume that we have renamed the files before hand)
  if(rename_files == "no"){
    # do not rename files
  } else{
    # rename files in the directory
  }

  ## read in raw centers data files one by one and partition, name and
  ## store them in appropriate folders

  # this is a list of lists. For example: the first element of this list
  # is a list which contains all the partitions of the first file.
  # The second element of this list is a list which contains all the partitions
  # of the second file. And so on.

  list_files_partitions <- list()

  # loop starts: go through each "file" in "directory_rel_path" and for each file
  # make partitions based on the "tmp_data_num_rows_per_part" variable. For each file,
  # store the partitioned df's for that file in a list. Store this list (corresponding)
  # to a given file in another list, whose each element is a list (which itself contains
  # partitions for a specific file).

  for(i in 1:file_list_length){

    # if test_mode = "yes", test partioning on the test dataset in "test_df".
    # Otherwise, read in the datafiles one by one from the "directory_rel_path".

    if(test_mode == "yes"){

      file_list_length <- 1 # (####)
      tmp_data <- test_df
    } else {
      if(pucc_col_type_name_specify == TRUE){
       if(str_detect(file_list[i], "center")){
         tmp_data <- readr::read_csv(str_c(directory_rel_path,
                  list.files(directory_rel_path)[i], sep = "/"),
                  col_names = c("t_5_center_id_I", "t_5_center_name", "t_5_center_address", "t_5_center_pin",
                                "t_5_center_mobile", "t_5_center_zone", "t_5_center_diesel_code",
                                "t_5_center_petrol_code", "t_5_center_timestamp",
                                "t_5_center_total_fail", "t_5_center_total_pass",
                                "t_5_center_total_pucc", "t_5_center_two_wheeler_cancel_count",
                                "t_5_center_two_wheeler_fail_count", "t_5_center_two_wheeler_pass_count"))

       } else if(str_detect(file_list[i], "mas")){

         tmp_data <- readr::read_csv(str_c(directory_rel_path,
                          list.files(directory_rel_path)[i], sep = "/"),
                          col_names = c("t_3_mas_id_I", "t_3_mas_vehicle_pucc_number", "t_3_mas_vehicle_number",
                                        "t_3_mas_vehicle_make", "t_3_mas_vehicle_model", "t_3_mas_vehicle_category",
                                        "t_3_mas_vehicle_manufacturing", "t_3_mas_vehicle_fuel_type",
                                        "t_3_mas_vehicle_engine_stroke", "t_3_mas_vehicle_emission_norms",
                                        "t_3_mas_vehicle_owner_name", "t_3_mas_vehicle_owner_address",
                                        "t_3_mas_ts"))
       } else if(str_detect(file_list[i], "certstring")){
         tmp_data <- readr::read_csv(str_c(directory_rel_path, list.files(directory_rel_path)[i], sep = "/"),
                                     col_names = c("t_2_certstring_uid_I", "t_2_certstring_vehicle_certificate_number_I",
                                                   "t_2_certstring_vehicle_number", "t_2_certstring_vehicle_status",
                                                   "t_2_certstring_certificate_id_I"))
       } else if(str_detect(file_list[i], "new")){

         # contact number column missing, also to check, if second column represents test_date or valid_upto?
         tmp_data <- readr::read_csv(str_c(directory_rel_path, list.files(directory_rel_path)[i], sep = "/"),
                                     col_names = c("t_4_new_vehicle_pucc_number",
                                                   "t_4_new_vehicle_test_date", "t_4_new_vehicle_test_time",
                                                   "t_4_new_vehicle_pucc_valid_upto"),
                                     col_types = list(col_character(), col_character(), col_character(),
                                                      col_character()))
       }
        else if(str_detect(file_list[i], "certificate")){

         tmp_data <- readr::read_csv(str_c(directory_rel_path, list.files(directory_rel_path)[i], sep = "/"),
                                     col_names = c("t_1_certificate_id_I", "t_1_certificate_vehicle_pucc_number",
                                                   "t_1_certificate_uid_I", "t_1_certificate_vehicle_number",
                                                   "t_1_certificate_vehicle_machine_reading", "t_1_certificate_vehicle_test_result",
                                                   "t_1_certificate_vehicle_fuel_type", "t_1_certificate_vehicle_ts"))
       }
      } else{
        tmp_data <- readr::read_csv(str_c(directory_rel_path, list.files(directory_rel_path)[i], sep = "/"), col_names = FALSE)
      }
    }

    # create an empty list that will store the data partitions for a given file.
    list_tmp_data_partitions <- list()

    # number of rows in the current datafile
    tmp_data_num_rows <- nrow(tmp_data)

    # number of partitions to be made for the current data file
    num_partitions <- ceiling(tmp_data_num_rows/partition_at)

    # number of rows to be stored per part for the current data file
    tmp_data_num_rows_per_part <- ceiling(tmp_data_num_rows/num_partitions)

    # if number of rows in the current data file is less than "partition_at"
    # then store the data as is without partitioning. Else, partition the dataset.
    if(tmp_data_num_rows <= partition_at){
      list_files_partitions[[i]] <- tmp_data

    } else {
      # start partioning from row 1, and then update this in every iteration as the below loop runs (####)
      start_part_index <- 1

      # partioning begins, store the partitions for the current file in the list_tmp_data_partitions
      for(j in 1:num_partitions){

        if((j*tmp_data_num_rows_per_part) > nrow(tmp_data)){
          list_tmp_data_partitions[[j]] <- tmp_data[(start_part_index:nrow(tmp_data)), ]
          start_part_index <- (j * tmp_data_num_rows_per_part) + 1
          tmp_data_num_rows_per_part
        } else {
          list_tmp_data_partitions[[j]] <- tmp_data[(start_part_index:(j * tmp_data_num_rows_per_part)), ]
          start_part_index <- (j * tmp_data_num_rows_per_part) + 1
          tmp_data_num_rows_per_part
        }

      }

      list_files_partitions[[i]] <- list_tmp_data_partitions

    }


  }

 return(list(list_files_partitions, file_list))

}



#' Combine partitioned files of a given table into a single master dataframe.
#'
#' This function detects the files to be combined (in a given directory that is supplied as an argument to this function)
#' based on the unique keyword. This keyword is supplied as an argument to this function. It then combines
#' those individual files into a master data frame. It leverages the power of data.table package in R.
#'
#'
#' @importFrom data.table fread rbindlist
#' @importFrom stringr str_c
#'
#' @param rel_path_to_directory relative path to the directory containing files to be combined
#' @param keyword a keyword that uniquely identifies the type of table. The function will look for this
#'                keyword in the file name.
#'
#'
#'
#' @return a data.table that is created by horizontally row-binding all the files (that contains the
#' \code{keyword}) in the the relative directory path.
#'
#'
#' @examples
#' combine_partitioned_files("rel/path/to/directory/containing/filesWithKeyWord", keyword = "foo")
#'
#' @export

combine_partitioned_files <- function(rel_path_to_directory, keyword){
  directory_files_list <- list.files(rel_path_to_directory)
  directory_files_list_filter_keyword_detect <- str_detect(directory_files_list, keyword)
  directory_files_list_filter_keyword <- directory_files_list[directory_files_list_filter_keyword_detect]
  partitioned_files_list <- list()
  counter <- 0
  for(i in 1:length(directory_files_list_filter_keyword)){
    partitioned_files_list[[i]] <- fread(file = str_c(rel_path_to_directory, directory_files_list_filter_keyword[i], sep = "/"))
    counter <- counter + 1
    print(str_c(i, "/", length(directory_files_list_filter_keyword), "files read in", sep = " "))
  }
  print("all files read in, now combining all files in a single dataframe")
  master_df <- rbindlist(partitioned_files_list)
  return(master_df)
}




#' Combine partitioned files of a given table into a single master dataframe (v2).
#'
#' This function detects the files to be combined (in a given directory that is supplied as an argument to this function)
#' based on the unique keyword. This keyword is supplied as an argument to this function. It then combines
#' those individual files into a master data frame. It leverages the power of data.table package in R.
#' V2 allows for a lot of files and can handle more than "x" GB of data.
#'


combine_partitioned_files_v2 <- function(rel_path_to_directory, keyword, output_dir_rel_path, output_file_name){
  directory_files_list <- list.files(rel_path_to_directory)
  directory_files_list_filter_keyword_detect <- str_detect(directory_files_list, keyword)
  directory_files_list_filter_keyword <- directory_files_list[directory_files_list_filter_keyword_detect]

  path_store_output_file <- str_c(output_dir_rel_path, output_file_name, sep = "/")

  for(i in 1:length(directory_files_list_filter_keyword)){
    tmp_dt <- data.table::fread(file = str_c(rel_path_to_directory, directory_files_list_filter_keyword[i], sep = "/"))
    if(i == 1){
      print("Files writing in progress...")
      data.table::fwrite(tmp_dt, path_store_output_file)
    } else {
      data.table::fwrite(tmp_dt, path_store_output_file, append = TRUE)
    }
    if(i < length(directory_files_list_filter_keyword)){
      print(str_c(i, "/", length(directory_files_list_filter_keyword), "files written", sep = " "))
    } else if (i == length(directory_files_list_filter_keyword)) {
      print(str_c(i, "/", length(directory_files_list_filter_keyword), "files written \n. All done!", sep = " "))

    }

  }

}

combine_partitioned_files_v3_rd <- function(con, rel_path_to_directory, keyword, master_output_file_name){
  directory_files_list <- list.files(rel_path_to_directory)
  directory_files_list_filter_keyword_detect <- str_detect(directory_files_list, keyword)
  directory_files_list_filter_keyword <- directory_files_list[directory_files_list_filter_keyword_detect]

  for(i in 1:length(directory_files_list_filter_keyword)){
    tmp_dt <- data.table::fread(file = str_c(rel_path_to_directory, directory_files_list_filter_keyword[i], sep = "/"))
    if(i == 1){
      print("Files writing in progress...")
      dbWriteTable(con, master_output_file_name, tmp_dt)
    } else {
      dbAppendTable(con, master_output_file_name, tmp_dt)
    }
    if(i < length(directory_files_list_filter_keyword)){
      print(str_c(i, "/", length(directory_files_list_filter_keyword), "files written", sep = " "))
    } else if (i == length(directory_files_list_filter_keyword)) {
      print(str_c(i, "/", length(directory_files_list_filter_keyword), "files written \n. All done!", sep = " "))

    }

  }

}

