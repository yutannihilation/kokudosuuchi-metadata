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
  
  attr(res, "id") <- stringr::str_extract(id, "^[A-Z][0-9]{2}[a-z]?(-[a-z])?")
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
  A19 = c("A19_010", "A19_011", "A19_012", "A19_013"),
  C02 = c("C12_018")
)

assert_all_translated <- function(new_names, old_names, id) {
  no_translated_cols <- intersect(new_names, old_names)
  exclude_cols <- c(ok_with_no_translation[[id]], "geometry")
  no_translated_cols <- setdiff(no_translated_cols, exclude_cols)
  
  if (length(no_translated_cols) > 0) {
    msg <- glue::glue(
      "There are some columns yet to be translated: ",
      paste(no_translated_cols, collapse = ",")
    )
    rlang::abort(msg)
  }
}

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

  assert_all_translated(colnames(d), old_names, id)
  
  d
}

# A22-m has two types of colnames; by exact match and by pattern
`match_A22-m` <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  old_names <- colnames(d)
  
  fixed_readable_names <- dc$name
  fixed_idx <- match(colnames(d), dc$code)
  
  colnames(d)[which(!is.na(fixed_idx))] <- dc$name[fixed_idx[!is.na(fixed_idx)]]

  replace_year <- function(d, prefix, format) {
    idx <- stringr::str_detect(colnames(d), paste0(prefix, "[12][0-9]{3}"))
    year <- stringr::str_sub(colnames(d)[idx], -4L)
    colnames(d)[idx] <- glue::glue(format)
    d
  }
  
  d <- replace_year(d, "A22_01", "{year}年度最深積雪")
  d <- replace_year(d, "A22_02", "{year}年度累計降雪量")
  d <- replace_year(d, "A22_03", "{year}年度最低気温")
  d <- replace_year(d, "A22_04", "{year}年度平均風速")
  d <- replace_year(d, "A22_10", "{year}年度死者数")
  d <- replace_year(d, "A22_11", "{year}年度行方不明者数")
  d <- replace_year(d, "A22_12", "{year}年度重傷者数")
  d <- replace_year(d, "A22_13", "{year}年度軽傷者数")
  d <- replace_year(d, "A22_14", "{year}年度住家全壊棟数")
  d <- replace_year(d, "A22_15", "{year}年度住家半壊棟数")
  d <- replace_year(d, "A22_16", "{year}年度住家一部破損数")
  d <- replace_year(d, "A22_17", "{year}年度除雪ボランティア団体数")
  d <- replace_year(d, "A22_18", "{year}年度除雪ボランティア登録人数")
  d <- replace_year(d, "A22_19", "{year}年度除雪ボランティア活動回数")
  d <- replace_year(d, "A22_20", "{year}年度除雪ボランティアの延べ参加人数")
  
  assert_all_translated(colnames(d), old_names, id)

  d
}

match_C02 <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  old_names <- colnames(d)
  
  # PortDistrictBoundary have unknown thrid column, but let's ignore...
  if (ncol(d) <= 4) {
    colnames(d)[1:2] <- c("県コード", "港湾コード")
  } else {
    idx <- seq_len(ncol(d) - 1L)
    colnames(d)[idx] <- dc$name[idx]
  }

  assert_all_translated(colnames(d), old_names, id)

  d
}

match_C09 <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  old_names <- colnames(d)
  
  if (ncol(d) <= 4) {
    colnames(d)[1:2] <- c("県コード", "漁港番号")
  } else {
    idx <- seq_len(ncol(d) - 1L)
    colnames(d)[idx] <- dc$name[idx]
  }
  
  assert_all_translated(colnames(d), old_names, id)
  
  d
}

match_L01 <- function(d, id) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  old_names <- colnames(d)
  
  colnames(d)[seq_along(dc$name)] <- dc$name
  
  # confirm the last positonally matched column is 選定年次ビット
  nenji_bits <- stringr::str_detect(d[["選定年次ビット"]], "^[01]+$")
  if (!all(nenji_bits)) {
    rlang::abort("Failed to match colnames")
  }
  
  nendo <- as.integer(unique(d[["年度"]]))
  if (length(nendo) != 1) {
    rlang::warn("Data seems to over multiple years, using the latest one to calculate colnames...")
    nendo <- max(nendo, na.rm = TRUE)
  }
  
  col_price <- paste0("調査価格_", seq(1983, nendo))
  col_move  <- paste0("属性移動_", seq(1984, nendo))
  
  idx_col_price <- length(dc$name) + seq_along(col_price)
  idx_col_move  <- max(idx_col_price) + seq_along(col_move)
  
  if (max(idx_col_move) != ncol(d) - 1L) {
    rlang::abort("The number of columns doesn't match with the expectation")
  }
  
  is_probably_move <- function(i) {
    all(nchar(d[[i]]) == 14L & stringr::str_detect(d[[i]], "^[0124]+$"))
  }
  
  if (any(vapply(idx_col_price, is_probably_move, logical(1L))) ||
      !all(vapply(idx_col_move, is_probably_move, logical(1L)))) {
    rlang::abort("The values of columns don't match with the expectation")
  }
  
  colnames(d)[idx_col_price] <- col_price
  colnames(d)[idx_col_move] <- col_move
  
  assert_all_translated(colnames(d), old_names, id)
  
  d
}

match_L02 <- match_L01

`match_L03-a` <- function(d, id) d
