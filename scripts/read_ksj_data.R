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
all_of <- dplyr::all_of

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
  
  id_short <- stringr::str_extract(id, "^[A-Z][0-9]{2}[a-z]?[0-9]?(-[a-z])?(-[cu])?")
  attr(res, "id") <- id_short
  
  if (identical(id_short, "A03")) {
    attr(res, "variant") <- tolower(stringr::str_extract(id, "SYUTO|CHUBU|KINKI"))
  }
  
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


translate_columns <- function(l, id = NULL, variant = NULL, translate_codelist = TRUE) {
  id <- id %||% attr(l, "id")
  variant <- variant %||% attr(l, "variant")

  # A19s-aは島嶼単位、A19sは指定地域単位  
  if (identical(id, "A19s-a")) {
    id <- "A19s"
  }
  
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
  
  l <- lapply(l, matching_fun, id = id, variant = variant, translate_codelist = translate_codelist)
  
  # debug
  if (!isTRUE(attr(l[[1]], "translated"))) {
    rlang::abort("Not translated")
  }
  
  l
}


ok_with_no_translation <- list(
  A10 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A11 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A12 = c("OBJECTID", "Shape_Leng", "Shape_Area"),
  A13 = c("OBJECTID", "Shape_Leng", "Shape_Area", "ET_ID", "ET_Source"),
  A15 = c("ORIG_FID"),
  # unexpected columns...
  A19 = c("A19_010", "A19_011", "A19_012", "A19_013"),
  A19s = c("LINK"),
  A37 = c("A37_330002"),
  P20 = c("レベル", "備考", "緯度", "経度", "NO"),
  P21 = c("検査ID"),
  P22 = c("IDO", "KEIDO", "TreeCode", "PosStat", "Origna", "ORigna"),
  # W05_007〜W05_010のと対応してるっぽいので、点のIDのようなもの？
  W05 = c("W05_000")
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

translate_one_column <- function(d, pos, codelist_id) {
  if (length(pos) != 1) {
    rlang::abort(paste("Invalid pos:", paste(pos, collapse = ", ")))
  }
  
  code_orig <- code <- d[[pos]]
  target <- colnames(d)[pos]
  
  csv_file <- here::here("data", "codelist", paste0(codelist_id, ".csv"))
  tbl <- readr::read_csv(csv_file, col_types = "cc")
  
  # if the data is integer, do matching in integer so that e.g. "01" matches 1
  if (is.numeric(code) ||
      # Note: all(NA, na.rm = TRUE) returns TRUE, so we need to eliminate the cases when all codes are NA.
      (any(!is.na(code)) && all(!stringr::str_detect(code, "\\D"), na.rm = TRUE))) {
    # TODO: detect the conversion failures
    tbl$code <- as.character(as.integer(tbl$code))
    
    # code is also needs to be character, otherwise codelist_translation[code]
    # will subset the data by position, not by name
    code <- as.character(as.integer(code))
  }
  
  # codelist_translation <- setNames(tbl$label, tbl$code)
  
  # Some column (e.g. A03_007) contains comma-separated list of codes
  if (any(stringr::str_detect(code, ","), na.rm = TRUE)) {
    label <- lapply(stringr::str_split(code, ","), function(x) {
      x <- x[!is.na(x) & x != ""]
      
      matched_code <- match(x, tbl$code)
      mismatched_code <- unique(x[is.na(matched_code) & !is.na(x)])
      if (length(mismatched_code) > 0) {
        mismatched_code <- paste(mismatched_code, collapse = ", ")
        msg <- glue::glue("Failed to translate these codes in {target}: {mismatched_code}")
        # TODO: AdminAreaCd has many missing codes, because they actually disappeared
        if (identical(codelist_id, "AdminAreaCd")) {
          rlang::warn(msg)
        } else {
          rlang::abort(msg)
        }
      }
      
      tbl$label[matched_code]
    })
  } else {
    matched_code <- match(code, tbl$code)
    
    mismatched_code <- unique(code_orig[is.na(matched_code) & !is.na(code_orig)])
    if (length(mismatched_code) > 0) {
      mismatched_code <- paste(mismatched_code, collapse = ", ")
      msg <- glue::glue("Failed to translate these codes in {target}: {mismatched_code}")
      if (identical(codelist_id, "AdminAreaCd")) {
        rlang::warn(msg)
      } else {
        rlang::abort(msg)
      }
    }
    
    label <- tbl$label[matched_code]
  }
  
  # overwrite the target column with human-readable labels
  d[[pos]] <- label
  # append the original codes right after the original position
  nm <- rlang::sym(glue::glue("{target}_code"))
  dplyr::mutate(d, "{{ nm }}" := code_orig, .after = all_of(pos))
}

match_by_position <- function(d, id, dc = NULL, variant = NULL, translate_codelist = TRUE, skip_check = FALSE) {
  dc <- dc %||% d_col_info[d_col_info$id == id, ]
  
  readable_names <- dc$name
  codelist_id <- dc$codelist_id
  
  old_names <- colnames(d)

  # exlude columns that don't need to be translated
  old_names <- setdiff(old_names, c(ok_with_no_translation[[id]], "geometry"))
  
  ncol <- length(old_names)
  
  if (!isTRUE(skip_check) && length(readable_names) != ncol) {
    msg <- glue::glue(
      "The numbers of columns don't match. ",
      "expected: ", nrow(dc), ", actual: ", ncol
    )
    rlang::abort(msg)
    
    readable_names <- readable_names[1:ncol]
    codelist_id <- codelist_id[1:ncol]
  }
  
  colnames(d)[seq_along(readable_names)] <- readable_names
  
  pos_codelist_id <- which(!is.na(codelist_id))
  for (i in seq_along(pos_codelist_id)) {
    pos <- pos_codelist_id[i]
    # Note: `+ i` is needed because the columns are shifted
    d <- translate_one_column(d, pos + i - 1L, codelist_id[pos])
  }
  
  attr(d, "translated") <- TRUE
  
  d
}

match_by_name <- function(d, id, variant = NULL, dc = NULL, translate_codelist = TRUE, skip_check = FALSE) {
  dc <- dc %||% d_col_info[d_col_info$id == id, ]
  
  old_names <- colnames(d)
  matched <- match(old_names, dc$code)
  
  pos_in_data <- which(!is.na(matched))
  pos_new <- matched[!is.na(matched)]
  
  colnames(d)[pos_in_data] <- dc$name[pos_new]

  if (!skip_check) {
    assert_all_translated(colnames(d), old_names, id)
  }
  
  if (!isTRUE(translate_codelist)) {
    return(d)
  }
  
  # Shrink the index to only those with non-NA codelist_id
  idx_codelist_exists <- which(!is.na(dc$codelist_id[pos_new]))
  pos_in_data <- pos_in_data[idx_codelist_exists]
  pos_new <- pos_new[idx_codelist_exists]

  # As new names will be inserted, the index will shift, so preserve names at this point
  colnames_codelist <- colnames(d)[pos_in_data]
  
  for (i in seq_along(colnames_codelist)) {
    target <- colnames_codelist[i]
    codelist_id <- dc$codelist_id[pos_new[i]]
    
    # current position of the column
    pos <- which(colnames(d) == target)

    d <- translate_one_column(d, pos, codelist_id)
  }

  attr(d, "translated") <- TRUE
  
  d
}

match_A03 <- function(d, id, variant, translate_codelist = TRUE) {
  idx_SectionTypeCd <- which(colnames(d) == "A03_006")
  idx_SectionCd <- which(colnames(d) == "A03_007")
  
  d <- match_by_name(d, id, translate_codelist = translate_codelist)
  
  if (!isTRUE(translate_codelist)) {
    return(d)
  }
  
  d[[idx_SectionTypeCd]] <- translate_SectionTypeCd(d[[idx_SectionTypeCd]], variant)
  
  attr(d, "translated") <- TRUE
  
  d
}

translate_SectionTypeCd <- function(code, variant) {
  csv_file <- here::here("data", "codelist", glue::glue("SectionTypeCd_{variant}.csv"))
  tbl <- readr::read_csv(csv_file, col_types = "cc")
  codelist_translation <- setNames(tbl$label, tbl$code)
  codelist_translation[code]
}

# A22-m has two types of colnames; by exact match and by pattern
`match_A22-m` <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  old_names <- colnames(d)
  
  d <- match_by_name(d, id, skip_check = TRUE)

  replace_year <- function(d, prefix, format) {
    idx <- stringr::str_detect(colnames(d), paste0(prefix, "[12][0-9]{3}"))
    year <- stringr::str_sub(colnames(d)[idx], -4L)
    colnames(d)[idx] <- glue::glue(format)
    d
  }
  
  d <- replace_year(d, "A22_01", "最深積雪_{year}年度")
  d <- replace_year(d, "A22_02", "累計降雪量_{year}年度")
  d <- replace_year(d, "A22_03", "最低気温_{year}年度")
  d <- replace_year(d, "A22_04", "平均風速_{year}年度")
  d <- replace_year(d, "A22_10", "死者数_{year}年度")
  d <- replace_year(d, "A22_11", "行方不明者数_{year}年度")
  d <- replace_year(d, "A22_12", "重傷者数_{year}年度")
  d <- replace_year(d, "A22_13", "軽傷者数_{year}年度")
  d <- replace_year(d, "A22_14", "住家全壊棟数_{year}年度")
  d <- replace_year(d, "A22_15", "住家半壊棟数_{year}年度")
  d <- replace_year(d, "A22_16", "住家一部破損数_{year}年度")
  d <- replace_year(d, "A22_17", "除雪ボランティア団体数_{year}年度")
  d <- replace_year(d, "A22_18", "除雪ボランティア登録人数_{year}年度")
  d <- replace_year(d, "A22_19", "除雪ボランティア活動回数_{year}年度")
  d <- replace_year(d, "A22_20", "除雪ボランティアの延べ参加人数_{year}年度")
  
  assert_all_translated(colnames(d), old_names, id)

  d
}

`match_A37` <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  old_names <- colnames(d)
  
  d <- match_by_name(d, id, skip_check = TRUE)
  
  replace_year <- function(d, prefix, format) {
    idx <- stringr::str_detect(colnames(d), paste0(prefix, "[12][0-9]{3}"))
    year <- stringr::str_sub(colnames(d)[idx], -4L)
    colnames(d)[idx] <- glue::glue(format)
    d
  }
  
  d <- replace_year(d, "A37_34", "救急車出動件数_{year}年")
  d <- replace_year(d, "A37_35", "消防防災ヘリ出動件数_{year}年")
  d <- replace_year(d, "A37_36", "平均現場到着所要時間_{year}年")
  d <- replace_year(d, "A37_37", "平均病院収容時間_{year}年")
  
  assert_all_translated(colnames(d), old_names, id)
  
  d
}


match_C02 <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  # C02_となるべきところがC12_となっているコードが紛れている？
  colnames(d) <- stringr::str_replace(colnames(d), "^C12_", "C02_")
  match_by_name(d, id)
}

# L01は年度によってカラムが異なる。基本的には最新版に前年までのデータも含まれるはずなので最新版だけ対応でよさそう...？
match_L01 <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  dc <- d_col_info[d_col_info$id == id, ]
  
  old_names <- colnames(d)
  
  d <- match_by_position(d, id, dc = dc, translate_codelist = translate_codelist, skip_check = TRUE)
  
  # confirm the last positionally matched column is 選定年次ビット
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
  
  # compensation for inserted rows
  inserted_rows <- sum(!is.na(dc$codelist_id))
  idx_col_price <- length(dc$name) + inserted_rows + seq_along(col_price)
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

`match_L03-a` <- function(d, id, variant = NULL, translate_codelist = TRUE) d
`match_L03-b` <- function(d, id, variant = NULL, translate_codelist = TRUE) d
`match_L03-b-u` <- function(d, id, variant = NULL, translate_codelist = TRUE) d

match_N04 <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  # geometry は抜く
  ncol <- ncol(d) - 1L
  
  dc_N04 <- readr::read_csv(
    here::here("data", glue::glue("{id}.csv")),
    col_types = readr::cols(
      name = readr::col_character(),
      code = readr::col_character(),
      columns = readr::col_integer(),
      codelist_id = readr::col_character()
    )
  )
  
  dc <- dc_N04[dc_N04$columns == ncol, ]
  
  if (nrow(dc) == 0) {
    rlang::abort("Unexpected number of columns")
  }
  
  match_by_name(d, id, dc = dc)
}

`match_S05-a` <- match_N04
`match_S05-b` <- match_N04
`match_P02`   <- match_N04
match_A42     <- match_N04

# exact matchに加えて、ある範囲以上のカラムには「管轄範囲1」、「管轄範囲2」...というルールで名前がついていく
match_P17 <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  old_names <- colnames(d)
  d <- match_by_name(d, id, skip_check = TRUE)
  
  idx <- which(stringr::str_detect(colnames(d), paste0("^", id)))

  if (length(idx) > 0) {
    offset <- nchar(id) + 2L
    num <- as.integer(stringr::str_sub(colnames(d)[idx], offset))
    
    if (any(diff(num) != 1)) {
      colnames_joined <- paste(colnames(d), collapse = ", ")
      msg <- glue::glue("Columns are not sequencial: {colnames_joined}")
      rlang::abort(msg)
    }
    
    colnames(d)[idx] <- paste0("管轄範囲", seq_along(num))
  }
  
  assert_all_translated(colnames(d), old_names, id)
  
  d
}

match_P18 <- match_P17

match_P21 <- function(d, id, variant = NULL, translate_codelist = TRUE) {
  colnames <- colnames(d)
  # wrong colname?
  idx <- stringr::str_detect(colnames, "^P21[A-Z]_00$")
  if (any(idx)) {
    # Uncomment this when moving to kokudosuuchi
    
    # msg <- glue::glue("Found invalid colname(s): {colnames[idx]}")
    # rlang::warn(msg)
    
    colnames(d)[idx] <- stringr::str_replace(colnames[idx], "\\d+$", sprintf("%03d", which(idx)))
  }
  
  match_by_name(d, id)
}
