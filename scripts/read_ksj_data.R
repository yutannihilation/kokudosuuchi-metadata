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
  if (!isTRUE(file.exists(f))) {
    rlang::abort(f, " doesn't seem a ZIP file.")
  }
  
  id <- tools::file_path_sans_ext(basename(f))
  
  cache <- file.path(cache_dir, id)
  
  if (file.exists(cache)) {
    if (!dir.exists(cache)) {
      rlang::abort(cache, " is a file")
    }
  } else {
    dir.create(cache)
    unzip(f, exdir = cache)
    rename_to_utf8_recursively(cache)
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

rename_to_utf8_recursively <- function(path, max_depth = 10L) {
  if (max_depth <= 0) {
    rlang::abort("Reached to max depth")
  }
  
  path <- normalizePath(path)
  
  # Convert only the child path, because parent paths might be
  # already converted to UTF-8
  orig_names <- list.files(path)
  if (length(orig_names) == 0) {
    return()
  }
  
  utf8_names <- iconv(orig_names, from = "CP932", to = "UTF-8")

  # file.path() doesn't work for this...
  orig_names <- paste(path, orig_names, sep = .Platform$file.sep)
  utf8_names <- paste(path, utf8_names, sep = .Platform$file.sep)
  
  # Rename
  purrr::walk2(orig_names, utf8_names, function(src, dst) {
    if (identical(src, dst)) {
      return()
    }
    
    msg <- glue::glue("Renaming {src} to {dst}")
    rlang::inform(msg)
    
    file.rename(src, dst)
    if (!file.exists(dst)) {
      msg <- glue::glue("Failed to rename to {dst}")
      rlang::abort(msg)
    }
  })
  
  # If it's a directory, apply recursively
  utf8_names <- utf8_names[file.info(utf8_names)$isdir]
  if (length(utf8_names) > 0) {
    purrr::walk(utf8_names, rename_to_utf8_recursively, max_depth = max_depth - 1L)
  }
}


translate_columns <- function(l, id = NULL) {
  id <- id %||% attr(l, "id")
  
  matching_fun <- get0(paste0("match_", id), ifnotfound = NULL)
  
  if (is.null(matching_fun)) {
    matching_fun <- switch (matching_types[id],
      positional = match_by_position,
      exact      = match_by_name
    )
  }
  
  if (is.null(matching_fun)) {
    rlang::abort("Not implemented")
  }
  
  lapply(l, matching_fun, id = id)
}


ok_with_no_translation <- list(
  A10 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A11 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A12 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A13 = c("OBJECTID", "Shape_Leng", "Shape_Area", "ET_ID", "ET_Source"),
  A15 = c("ORIG_FID"),
  # unexpected columns...
  A19 = c("A19_010", "A19_011", "A19_012", "A19_013")
)

match_by_position <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  readable_names <- dc$name
  
  # minus 1 is for geometry column
  ncol <- ncol(d) - 1L
  
  if (length(readable_names) != ncol && !id %in% ok_with_no_translation[[id]]) {
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

