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

