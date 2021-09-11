
#' tested fucntion for flexibility of renaming table from excel that contains duplicated columns
#' This is useful to replace "unique" in the new version of readxl::read_excel(, .name_repair)
#' @export
make_unique_names <- function(
  names,
  empty_prefix = "X", empty_sep = "_", empty_cases = c("", " "),
  duplicate_sep = "__",
  ...
) {
  .de_dup <- function(names, ...) {
    names_dup <- unique(names[duplicated(names)])
    if(length(names_dup) > 0) {
      for(nm_tmp in names_dup) {
        duplicate_idx <- which(names == nm_tmp)
        duplicate_seq <- seq(length(duplicate_idx)-1)
        names[duplicate_idx] <- c(nm_tmp, paste(nm_tmp, duplicate_seq, sep = duplicate_sep))
      }
    }
    return(names)
  }
  # browser()
  names <- as.character(names)
  empty_idx <- which(names %in% empty_cases | is.na(names))
  empty_n   <- length(empty_idx)
  if(length(empty_n) > 0) {
    names[empty_idx] <- paste(empty_prefix, seq(empty_n), sep = empty_sep)
  }
  .de_dup(names)
}

##################################################################################################################
## File system
##################################################################################################################
#' Check if the_dir existing, if not, create the dir
#' @export
check_create_dir <- function(the_dir) {
  if(!dir.exists(the_dir)) dir.create(the_dir, recursive = TRUE)
}
# duplicated with utils.R -- create_dir_if_missing

#' @export
select_sub_dir <- function(
  proj_dir = NULL,
  sub_dir = "clientdata",
  pattern = NULL
) {
  if(is.null(proj_dir)) proj_dir <- getwd()
  dir_check <- file.path(proj_dir, sub_dir)

  if(is.null(pattern)) {
    dir_o <- dir_check
  } else {
    dirs <- list.dirs(dir_check)
    if(length(dirs) > 1) {
      dir_o <- dirs[grepl(pattern, dirs)]
    } else {
      dir_o <- dir_check
    }
  }
  dir_o
}

#' @export
latest_file_in_dir <- function(
  input_dir = NULL,
  input_files = NULL,
  select_pattern = NULL,
  date_pattern = NULL,
  date_format = NULL,
  select_latest = TRUE,
  full.names = TRUE,
  recursive = TRUE
) {
  if(is.null(input_dir) & is.null(input_files)) {
    stop("missing both input directory and input filepaths")
  } else if(!is.null(input_dir) & is.null(input_files)) {
    input_files <- list.files(path = input_dir, pattern = select_pattern, full.names = full.names, recursive = recursive)
  } else {
    input_files <- unique(input_files, list.files(path = input_dir, pattern = select_pattern, full.names = full.names, recursive = recursive))
  }
  input_files <- input_files[!grepl("\\/\\~\\$", input_files)]
  if(select_latest) {
    latest_file_in_list(input_files = input_files, select_pattern = select_pattern, date_pattern = date_pattern, date_format = date_format)
  } else {
    message("latest file selection was not done")
    return(input_files)
  }
}

#' @export
latest_file_in_list <- function(
  input_files = NULL,
  select_pattern = NULL,
  unselt_pattern = NULL,
  date_pattern = NULL,
  date_format = NULL
) {
  # browser()
  if(!is.null(select_pattern)) input_files <- input_files[grepl(select_pattern, input_files)]
  if(!is.null(unselt_pattern)) input_files <- input_files[!grepl(unselt_pattern, input_files)]
  if(length(which(is.na(input_files))) > 0) stop("=>: input list contains NA")
  if(length(unique(input_files)) == 0)      stop("=>: mislabeled selection pattern")
  file_select <- input_files %>%
    fund_latest_by_date_in_list(., pattern = date_pattern, format = date_format)
}
## duplicated with utils.R -- get_latest_file

#' @export
fund_latest_by_date_in_list <- function(
  xx,
  pattern = NULL,
  format = NULL
) {
  if(length(unique(xx)) == 1) xx_select <- unique(xx)
  if(length(unique(xx)) > 1) {
    formatted <- rep(NA, length(xx))
    if(is.null(pattern)) {
      dates <- stringr::str_extract(basename(xx), pattern = "\\d{2}[a-zA-Z]{3}\\d{4}")
      if(!any(is.na(dates))) formatted <- dates %>% as.Date(format = "%d%B%Y")
      if(any(is.na(formatted))) {
        dates <- stringr::str_extract(basename(xx), pattern = "\\d{4}-\\d{2}-\\d{2}")
        if(!any(is.na(dates))) formatted <- dates %>% as.Date(format = "%Y-%m-%d")
      }
      if(any(is.na(formatted))) {
        dates <- stringr::str_extract(basename(xx), pattern = "\\d{8}")
        if(!any(is.na(dates))) formatted <- dates %>% as.Date(format = "%Y%m%d")
        if(any(is.na(formatted))) formatted <- dates %>% as.Date(format = "%m%d%Y")
      }
      if(any(is.na(formatted))) stop("=>: unable to guess date label pattern and format")
    } else {
      dates <- stringr::str_extract(basename(xx), pattern = pattern)
      if (is.null(format)) {
        formatted <- dates %>% as.Date(format = "%d%B%Y")
        if(any(is.na(formatted))) formatted <- dates %>% as.Date(format = "%d%B%y")
        if(any(is.na(formatted))) formatted <- dates %>% as.Date(format = "\\d{4}-\\d{2}-\\d{2}")
        if(any(is.na(formatted))) formatted <- dates %>% as.Date(format = "%Y%m%d")
        if(any(is.na(formatted))) formatted <- dates %>% as.Date(format = "%m%d%Y")
        if(any(is.na(formatted))) stop("=>: cannot guess date label pattern and format")
      } else {
        formatted <- dates %>% as.Date(format = format)
      }
    }

    if(length(which(is.na(formatted))) == 0) {
      max_idx <- which(formatted == max(formatted))
      if(length(max_idx) == 0) stop(paste("=>: invalid latest date labeled in:", xx))
      if(length(max_idx) == 1) xx_select <- xx[max_idx]
      if(length(max_idx) > 1) {
        warning("=>: multiple selected with same labeled date and selected the last modified one")
        xx_select <- xx[max_idx]
        infos <- file.info(xx_select)
        xx_select <- xx_select[which.max(infos$mtime)]
        xx_select <- xx_select[1]
      }
    } else {
      stop(paste("=>: unsucessful formated dates in:", xx[is.na(formatted)]))
    }
    xx_select
  }
  return(xx_select)
}

#' @export
latest_dir <- function(
  base_dir = NULL,
  select_pattern = NULL,
  unselt_pattern = NULL,
  date_pattern = NULL,
  date_format = NULL
) {
  input_dirs <- list.dirs(base_dir, full.names = T)
  input_dirs <- input_dirs[input_dirs != base_dir]
  if(!is.null(select_pattern)) input_dirs <- input_dirs[grepl(select_pattern, input_dirs)]
  if(!is.null(unselt_pattern)) input_dirs <- input_dirs[!grepl(unselt_pattern, input_dirs)]

  if(length(which(is.na(input_dirs))) > 0) stop("input list contains NA")
  if(length(unique(input_dirs)) == 0) stop("mislabeled selection pattern")
  dir_select <- input_dirs %>%
    fund_latest_by_date_in_list(., pattern = date_pattern, format = date_format)
}

#' function to make a list of filenames in full path
#' @export
#' @author yw
#' @param keys a list of key strings for major part of file names to be created
#' @param out_dir output dictonary
#' @param postfix postfix
#' @param prefix prefix
#' @param extension extension
file_path_list <- function(
  keys = NULL,
  out_dir = NULL,
  prefix = NULL,
  postfix = NULL,
  sep = "_",
  extension = "csv",
  ...
) {
  extension <- paste0(".", gsub("^.*\\.+", "", extension))
  if(!is.null(postfix)) {
    file_names <- lapply(keys, function(x) paste0(paste(x, postfix, sep = sep), extension))
  } else {
    file_names <- lapply(keys, function(x) paste0(x, extension))
  }
  if(!is.null(prefix))  file_names <- lapply(file_names, function(x) paste(prefix, x, sep = sep))
  if(!is.null(out_dir)) file_names <- lapply(file_names, function(x) file.path(out_dir, x))
  return(file_names)
}

#####################################################################################################
## String manipulation
#####################################################################################################
#' @export
#' @author yw
str_insert <- function(
  x,
  insert_at = 1L,
  string = " ",
  to_all = FALSE,
  full_length = NULL,
  ...
) {
  if(to_all) {
    idx_bad <- !is.na(x)
  } else {
    idx_bad <- !is.na(x) & !grepl(string, x)
  }
  if(length(which(idx_bad)) > 0) {
    x_bad <- x[idx_bad]
    if(is.null(full_length)) {
      max_ll <- max(str_length(x_bad), na.rm = T)
      full_length <- max_ll + str_length(string)
    }
    x[idx_bad] <- paste(str_sub(x_bad, 1L, insert_at), str_sub(x_bad, (insert_at + 1), full_length), sep = string)
  }
  x
}

#' @export
#' @author yw
str_trim_by_pattern <- function(
  string,
  side = c("both", "left", "right"),
  pattern = NULL
) {
  if(!is.null(pattern)) {
    pattern <- pattern[!is.na(pattern)]
    if(length(unique(pattern)) == 0) {
      stop("pattern was misspecified as NA")
    } else if (length(unique(pattern)) > 2) {
      stop("pattern was misspecified with more than 2 values")
    } else if (length(unique(pattern)) == 2) {
      .str_trim_left  <- function(string, pattern) gsub(pattern[1], "", string)
      .str_trim_right <- function(string, pattern) gsub(pattern[2], "", string)
      .str_trim_both  <- function(string, pattern) gsub(paste0(pattern[2], "$"), "", gsub(paste0("^", pattern[1]), "", string))
    } else {
      .str_trim_left  <- function(string, pattern) gsub(pattern, "", string)
      .str_trim_right <- function(string, pattern) gsub(pattern, "", string)
      .str_trim_both  <- function(string, pattern) gsub(paste0(pattern, "$"), "", gsub(paste0("^", pattern), "", string))
    }
  }
  if(is.null(pattern)) {
    string <- stringr::str_trim(string, side = side)
    return(string)
  } else {
    side <- match.arg(side)
    switch(side,
           both  = .str_trim_both(string, pattern),
           left  = .str_trim_left(string, pattern),
           right = .str_trim_right(string, pattern))
  }
}

#' @export
#' @author yw
str_sub_df <- function(
  df = NULL,
  vars = NULL,
  patterns = NULL,
  replaces = NULL
) {
  if(!is.null(patterns) & is.null(replaces)) {
    stop("misssing replacements for patterns")
  } else if(is.null(patterns) & !is.null(replaces)) {
    stop("no patterns targeted for replacements")
  } else if(is.null(patterns) & is.null(replaces)) {
    stop("no patterns specified for correction")
  } else {
    patterns <- patterns[!is.na(patterns)]
    replaces <- replaces[!is.na(replaces)]
    patterns <- as.character(patterns)
    replaces <- as.character(replaces)
    if(length(patterns) > 0) {
      if(is.null(vars)) {
        df <- df %>%
          mutate_all(., stringi::stri_replace_all_fixed, pattern = patterns, replacement = replaces, vectorize_all = FALSE)
      } else {
        df <- df %>%
          mutate_at(vars, stringi::stri_replace_all_fixed, pattern = patterns, replacement = replaces, vectorize_all = FALSE)
      }
    } else {
      stop("no valid patterns to be replaced")
    }
  }
  df
}

#####################################################################################################
## dataframes
#####################################################################################################
#' tested function for checking if all target colunms are listed in a dataframe
#' @author yw
#' @export
check_target_colnms <- function(
  target_colnms,
  tab_in
) {
  if(!all(target_colnms %in% names(tab_in)))
    stop(
      paste0("missing target columns: ",
             collapse(setdiff(target_colnms, names(tab_in)), sep = "; "))
    )
}

#' @export
rename_all_columns <- function(
  x,
  colnms = NULL
) {
  colnames(x) <- colnms
  return(x)
}

############################################################################################
## Missing
############################################################################################
#' @export
check_missing <- function(
  my_df = NULL,
  na_add = NULL,
  na_spec = NULL
) {
  ## A complex case to detect columns that are mixing NAs with any term of c("NA", "N/A", "NaN", "", " ", "  ")
  # idx <- apply(my_df, 2, function(x) any(is.na(x)) & any(x %in% c("NA", "N/A", "NaN", "", " ", "  ")))
  ## A simple case to detect columns that contains any term of c("NA", "N/A", "NaN", "", " ", "  ")
  na_case <- c(c("NA", "N?A", "N/A", "NaN", "", " ", "  "), na_add)
  if(!is.null(na_spec)) na_case <- na_spec
  idx <- apply(my_df, 2, function(x) any(x %in% na_case))
  colnames(my_df)[idx]
}

#' @export
remove_empty_columns <- function(
  my_df = NULL
) {
  idx <- apply(my_df, 2, function(x) all(is.na(x) | x == ""))
  my_df <- my_df[, !idx]
}

#' @export
unify_missing <- function(
  my_df = NULL,
  na_add = NULL,
  na_spec = NULL
) {
  ## A complex case to detect columns that are mixing NAs with any term of c("", "N/A", "NA", " ")
  ## Limits, won't detect columns that only contains any term of c("", "N/A", "NA", " ") without NAs
  # df_out <- my_df %>%
  #   mutate_at(colnames(my_df)[apply(my_df, 2, function(x) any(is.na(x)) & any(x %in% c("NA", "N/A", "NaN", "", " ", "  ")))],
  #                    function(x) x <- replace(x, x %in% c("NA", "N/A", "NaN", "", " ", "  "), NA_character_))
  na_case <- c(c("NA", "N?A", "N/A", "NaN", "", " ", "  "), na_add)
  if(!is.null(na_spec)) na_case <- na_spec
  df_out <- my_df %>%
    mutate_at(check_missing(my_df, na_add, na_spec),
              function(x) x <- replace(x, x %in% na_case, NA))
  df_out
}

#' @export
unify_missing_char <- function(df) {
  mod_idx  <- sapply(df, mode)
  char_idx <- mod_idx[mod_idx == "character"] %>% names
  df[, char_idx][is.na(df[, char_idx]) | df[, char_idx]==""] <- NA_character_
  df
}

# nms <- names(ihc_analyte)
# iter_impute_empty(names(ihc_analyte))
# ihc_analyte <- read_xlsx(mosaic_rep_fls[1], skip = 2, n_max = 1, .name_repair = "minimal")
#' @export
iter_impute_empty <- function(x = NULL, ...) {
  idx <- which(x != "")
  for(ii in idx) {
    if(ii != idx[length(idx)]) {
      jj <- idx[which(idx == ii) + 1]
    } else {
      jj <- length(x) + 1
    }
    x[ii:(jj-1)] <- x[ii]
    x <- gsub(" |\\&", "_", x)
  }
  x
}

#######################################################################################
## functions - End
#######################################################################################
