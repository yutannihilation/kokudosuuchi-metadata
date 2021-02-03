Extract codelist: リンク先に詳細があるタイプ
================

## 方針

コードリストは、2つタイプがある。
そのページに直接列挙されているタイプと、リンク先に詳細があるタイプ。
リンク先に詳細があるタイプは、`html_table()`を使った時点で失われてしまうので、別途収集する。

## データのダウンロード

``` r
library(readr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(stringr)
library(dplyr, warn.conflicts = FALSE)

datalist_files <- list.files(here::here("data-raw", "datalist"), full.names = TRUE)
names(datalist_files) <- stringr::str_replace(basename(datalist_files), "(?:KsjTmplt-)(.*?)(?:-v.*)?(?:\\.html)", "\\1")

extract_links <- function(f) {
  links <- read_html(f) %>% 
    # href に codelist を含む a タグを取得
    html_elements(xpath = "//td/a[contains(@href, 'codelist')]")
  
  tibble(
    name = html_text2(links),
    url  = html_attr(links, "href")
  ) %>% 
    distinct()
}

links_df <- purrr::map_dfr(datalist_files, extract_links, .id = "id")
```

とりあえずこのコードリストとテキストの対応表は別ファイルに書き出しておく。
あとで紐付けるときに使う。

``` r
links_df %>% 
  mutate(
    codelist_id = tools::file_path_sans_ext(basename(url)),
    .keep = "unused"
  ) %>% 
  readr::write_csv(here::here("data", "codelist_list.csv"))
```

ダウンロードする。

``` r
library(polite)

out_dir <- here::here("data-raw", "codelist")
dir.create(out_dir, showWarnings = FALSE)

links <- unique(links_df$url)
session <- bow("https://nlftp.mlit.go.jp/ksj/", delay = 10)

# URL間違い

#（パスに /jpgis/ が入る）
links[which(str_detect(links, "SectionCd_syuto"))] <- "/ksj/jpgis/codelist/SectionCd_syuto.html"

# 相対パス（../） は動かないので /ksj/gml/ に置き換え
dotdot_idx <- which(str_detect(links, "^../"))
links[dotdot_idx] <- str_replace(links[dotdot_idx], fixed("../"), "/ksj/gml/")

# transition_use.pdf はデータの使い方の説明？であってコードリストではないっぽい
links <- links[!which(str_detect(links, "transition_use.pdf"))]

links <- unique(links)

for (l in links) {
  out_file <- file.path(out_dir, basename(l))
  
  # すでにダウンロードしてたら
  if (file.exists(out_file)) {
    message(glue::glue("{l} is already downloaded. Skipped."))
    next
  }

  message(glue::glue("Getting {l}..."))
  
  if (str_ends(l, "\\.html")) {
    nod(session, l) %>% 
      # 念のため、HTMLへの変換をはさまずテキストで受け取ることにする
      scrape(content = "text/plain;charset=UTF-8", verbose = TRUE) %>% 
      brio::write_file(out_file)
  } else {
    # バイナリのものは R のセッションに読み込まずそのままダウンロード
    curl::curl_download(glue::glue("https://nlftp.mlit.go.jp{l}"), destfile = out_file)
  }
}

# 数バイトのファイルはおかしい
codelist_files <- fs::dir_ls(out_dir)
stopifnot(all(fs::file_size(codelist_files) > 10))
```

## 確認

``` r
names(codelist_files) <- tools::file_path_sans_ext(basename(codelist_files))

d <- codelist_files %>% 
  purrr::map(~ {
    if (str_ends(.x, "\\.xlsx")) {
      list(readxl::read_xlsx(.x))
    } else {
      # encoding を明示的に指定しないとエラーになる
      read_html(.x, encoding = "UTF-8") %>% 
        # <th> がないものがほとんどっぽい
        html_table(header = TRUE)
    }
  })

# テーブルが2つ以上あるものはあとで個別対応
d_many_tables <- d %>% 
  purrr::keep(~ length(.) != 1)

names(d_many_tables)
```

    ## [1] "MaritimeOrgCd"       "PubFacAdminCd"       "PubFacMiclassCd_wf" 
    ## [4] "WelfareFacMiclassCd"

``` r
d <- d %>% 
  purrr::map(1L)
```

カラムが2つの場合は「コード」とかの名前のほうがコードで別の方がラベルとわかる。
とりあえず今はこの正規表現で網羅できてそう

``` r
d %>% 
  purrr::keep(~ ncol(.) == 2 && !any(str_detect(colnames(.), "コード$|対応番号|No.")))
```

    ## named list()

その他のテーブルはとりあえずカラム数で分けてみる。

``` r
l <- d %>% 
  purrr::keep(~ ncol(.) != 2) %>%
  split(., lengths(.))

l %>% 
  purrr::map(names)
```

    ## $`1`
    ## [1] "MaritimeOrgCd" "PubFacAdminCd"
    ## 
    ## $`3`
    ##  [1] "BusClassCd"              "ChukyoAreaZoneCd"       
    ##  [3] "ClassFishPortCd"         "ClassHarbor2Cd"         
    ##  [5] "CodeDesignationCd"       "CodeNoncombustibleCd"   
    ##  [7] "DistributionCenterCd"    "EntrepreneurCd"         
    ##  [9] "EstClassCd"              "KasoCd"                 
    ## [11] "KeihanshinAreaStationCd" "KinkiAreaStationCd"     
    ## [13] "LandUseCd-09-u"          "LandUseCd-09"           
    ## [15] "LandUseCd-77"            "LandUseCd-88"           
    ## [17] "LandUseCd-YY"            "PosSpecificLevel"       
    ## [19] "PubOfficeCd"             "RailwayClassCd"         
    ## [21] "ReferenceDataCd"         "UrgentRoadCd"           
    ## [23] "kinouruikeiCd"           "rinshunosaibunCd"       
    ## 
    ## $`4`
    ##  [1] "DistributionCd"       "KeihanshinAreaZoneCd" "KinkiAreaZoneCd"     
    ##  [4] "PortRouteCd"          "PrefCd"               "PrefCdA33"           
    ##  [7] "PubFacMiclassCd"      "PubFacMiclassCd_wf"   "TokyoAreaZoneCd"     
    ## [10] "WelfareFacMiclassCd"  "hoanrinCd"            "midorinokairoCd"     
    ## 
    ## $`5`
    ## [1] "AdminAreaCd_R105"
    ## 
    ## $`6`
    ## [1] "jushuCd"        "shinrinkanriCd"
    ## 
    ## $`7`
    ## [1] "ClimateCd"
    ## 
    ## $`8`
    ## [1] "shouhanshubanCd"
