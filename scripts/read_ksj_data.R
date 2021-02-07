# data --------------------------------------------------------------------

col_types <- readr::cols(.default = readr::col_character())

d_matching_types <- readr::read_csv(
  here::here("data", "matching_types.csv"),
  col_types = col_types
)
matching_types <- d_matching_types$matching_type
names(matching_types) <- d_matching_types$id

d_col_info <- readr::read_csv(
  here::here("data", "joined.csv"),
  col_types = col_types
)


# function ----------------------------------------------------------------

`%||%` <- rlang::`%||%`

read_zip_with_cache <- function(f, encoding = "CP932", cache_dir = "./cache") {
  id <- tools::file_path_sans_ext(basename(f))
  
  cache <- file.path(cache_dir, id)
  
  if (file.exists(cache)) {
    if (!dir.exists(cache)) {
      stop(cache, " is a file", call. = FALSE)
    }
  } else {
    dir.create(cache)
    unzip(f, exdir = cache)
    
    orig_names <- list.files(cache, full.names = TRUE)
    # if it contains only 1 directory, move down 1 step
    if (length(orig_names) == 1L && dir.exists(orig_names)) {
      orig_names <- list.files(orig_names, full.names = TRUE)
    }
    
    utf8_names <- iconv(orig_names, from = "CP932", to = "UTF-8")
    
    idx <- orig_names != utf8_names
    if (any(idx)) {
      purrr::walk2(orig_names[idx], utf8_names[idx], ~ {
        msg <- glue::glue("Renaming {.x} to {.y}")
        rlang::inform(msg)
        file.rename(.x, .y)
      })
    }
  }
  
  shp_files <- list.files(cache, pattern = ".*\\.shp$", recursive = TRUE, full.names = TRUE)
  
  # set names to make each layer named
  shp_files <- rlang::set_names(
    shp_files,
    tools::file_path_sans_ext(basename(shp_files))
  )
  
  # read all data
  res <- purrr::map(shp_files,
                    sf::read_sf,
                    # All data is encoded with Shift_JIS as described here:
                    # http://nlftp.mlit.go.jp/ksj/old/old_data.html
                    options = glue::glue("ENCODING={encoding}")
  )
  
  attr(res, "id") <- stringr::str_split(id, "-")[[1]][1]
  res
}

translate_columns <- function(l, id = NULL) {
  id <- id %||% attr(l, "id")
  
  matching_fun <- switch (matching_types[id],
    positional = match_by_position,
    exact      = match_by_name,
    get0(paste0("match_", id), ifnotfound = NULL)
  )
  
  if (is.null(matching_fun)) {
    rlang::abort("Not implemented")
  }
  
  lapply(l, matching_fun, id = id)
}

match_by_position <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  readable_names <- dc$name
  
  # minus 1 is for geometry column
  ncol <- ncol(d) - 1L
  
  if (length(readable_names) != ncol) {
    msg <- glue::glue(
      "The numbers of columns don't match. ",
      "expected: ", nrow(dc), ", actual: ", ncol
    )
    rlang::abort(msg)
    
    readable_names <- readable_names[seq_len(ncol)]
  }
  
  colnames(d)[seq_along(readable_names)] <- readable_names
  d
}

ok_with_no_translation <- list(
  A10 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A11 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A12 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A13 = c("OBJECTID", "Shape_Leng", "Shape_Area")
)

match_by_name <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  readable_names <- setNames(dc$name, dc$code)
  old_names <- colnames(d)
  idx <- match(old_names, dc$code)
  colnames(d)[which(!is.na(idx))] <- dc$name[idx[!is.na(idx)]]

  exclude_cols <- c(ok_with_no_translation[[id]], "geometry")
  no_translated_cols <- setdiff(old_names[is.na(idx)], exclude_cols)
  
  if (length(no_translated_cols) > 0) {
    msg <- glue::glue(
      "There are some columns yet to be translated: ",
      paste(no_translated_cols, collapse = ",")
    )
    rlang::abort(msg)
  }
  
  d
}

