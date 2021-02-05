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
        html_table(convert = FALSE, header = TRUE)
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
  purrr::discard(~ length(.) != 1) %>% 
  purrr::map(1L)
```

-   `LandUseProperty*`は、列名っぽい。が、そもそも元データの列名は日本語になってるっぽいので省略
-   `*AreaZoneCd`のものは、コードとラベルが1対1に対応していない。どのみち何らかの処理が必要そうなのでラベル翻訳は諦める。
-   `PortRouteCd`
    は、列の値のコードに対応するラベルではなくて、列のコードと属性名の対応で、これはすでにHTMLから抜き出しているので不要
-   `jushuCd` は、変換必要なさそう
-   `shinrinkanriCd` は、データ中にコードもラベルも含まれている
-   `ClimateCd`は、列の説明なので特に対応する必要なし
-   `AreaStationCd`は、データ中にコードもラベルも含まれている

``` r
excl <- 
  str_detect(names(d), "^LandUseProperty.*$|^.*AreaZoneCd$|^.*AreaStationCd$") |
  names(d) %in% c(
    "PortRouteCd",
    "jushuCd",
    "shinrinkanriCd"
  )
names(d)[excl]
```

    ##  [1] "ChukyoAreaZoneCd"        "KeihanshinAreaStationCd"
    ##  [3] "KeihanshinAreaZoneCd"    "KinkiAreaStationCd"     
    ##  [5] "KinkiAreaZoneCd"         "LandUseProperty-07"     
    ##  [7] "LandUseProperty-09"      "LandUseProperty-77"     
    ##  [9] "LandUseProperty-88"      "LandUseProperty-92_98"  
    ## [11] "PortRouteCd"             "TokyoAreaStationCd"     
    ## [13] "TokyoAreaZoneCd"         "jushuCd"                
    ## [15] "shinrinkanriCd"

``` r
d <- d[!excl]
```

カラムが2つの場合は「コード」とかの名前のほうがコードで別の方がラベルとわかる。
とりあえず今はこの正規表現で網羅できてそう

``` r
d %>% 
  purrr::keep(~ ncol(.) == 2 && sum(str_detect(colnames(.), "コード$|対応番号|No.")) != 1)
```

    ## named list()

``` r
d %>% 
  purrr::keep(~ ncol(.) == 2) %>% 
  purrr::iwalk(function(d, codelist_name) {
    f <- here::here("data", "codelist", glue::glue("{codelist_name}.csv"))
    
    idx <- str_detect(colnames(d), "コード$|対応番号|No.")
    tibble::tibble(
      code  = d[[which(idx)]],
      label = d[[which(!idx)]]
    ) %>% 
      mutate(
        code = if(is.character(code)) code else sprintf("%.0f", code)
      ) %>% 
      readr::write_csv(f)
  }) 
```

その他のテーブルはとりあえずカラム数で分けてみる。

``` r
l <- d %>% 
  purrr::keep(~ ncol(.) != 2) %>%
  split(., lengths(.))

l %>% 
  purrr::map(names)
```

    ## $`3`
    ##  [1] "BusClassCd"           "ClassFishPortCd"      "ClassHarbor2Cd"      
    ##  [4] "CodeDesignationCd"    "CodeNoncombustibleCd" "DistributionCenterCd"
    ##  [7] "EntrepreneurCd"       "EstClassCd"           "KasoCd"              
    ## [10] "LandUseCd-09-u"       "LandUseCd-09"         "LandUseCd-77"        
    ## [13] "LandUseCd-88"         "LandUseCd-YY"         "PosSpecificLevel"    
    ## [16] "PubOfficeCd"          "RailwayClassCd"       "ReferenceDataCd"     
    ## [19] "UrgentRoadCd"         "kinouruikeiCd"        "rinshunosaibunCd"    
    ## 
    ## $`4`
    ## [1] "DistributionCd"  "PrefCd"          "PrefCdA33"       "PubFacMiclassCd"
    ## [5] "hoanrinCd"       "midorinokairoCd"
    ## 
    ## $`5`
    ## [1] "AdminAreaCd_R105"
    ## 
    ## $`7`
    ## [1] "ClimateCd"
    ## 
    ## $`8`
    ## [1] "shouhanshubanCd"

カラム名3つのやつは、説明が追加されているだけっぽい。

``` r
l$`3` %>% 
  purrr::map(colnames)
```

    ## $BusClassCd
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $ClassFishPortCd
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $ClassHarbor2Cd
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $CodeDesignationCd
    ## [1] "コード" "区分"   "内容"  
    ## 
    ## $CodeNoncombustibleCd
    ## [1] "コード"         "区分"           "評価指標の算定"
    ## 
    ## $DistributionCenterCd
    ## [1] "コード" "種別"   "説明"  
    ## 
    ## $EntrepreneurCd
    ## [1] "コード"     "事業者分類" "説明"      
    ## 
    ## $EstClassCd
    ## [1] "開設者分類" "コード"     "内容"      
    ## 
    ## $KasoCd
    ## [1] "コード"       "対応する内容" "適用条文"    
    ## 
    ## $`LandUseCd-09-u`
    ## [1] "コード" "種別"   "定義"  
    ## 
    ## $`LandUseCd-09`
    ## [1] "コード" "種別"   "定義"  
    ## 
    ## $`LandUseCd-77`
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $`LandUseCd-88`
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $`LandUseCd-YY`
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $PosSpecificLevel
    ## [1] "コード" "説明"   "説明"  
    ## 
    ## $PubOfficeCd
    ## [1] "対象施設" "コード"   "備考"    
    ## 
    ## $RailwayClassCd
    ## [1] "コード"       "対応する内容" "定義"        
    ## 
    ## $ReferenceDataCd
    ## [1] "内容"   "コード" "内容"  
    ## 
    ## $UrgentRoadCd
    ## [1] "コード" "区分"   "説明"  
    ## 
    ## $kinouruikeiCd
    ## [1] "コード" "内容"   "備考"  
    ## 
    ## $rinshunosaibunCd
    ## [1] "コード" "内容"   "備考"

``` r
l$`3` %>% 
  purrr::iwalk(function(d, codelist_name) {
    nm <- colnames(d)
    
    idx_code <- which(str_detect(nm, "コード"))
    idx_label <- which(str_detect(nm, "区分|種別|分類|駅名|駅の名称|対象施設"))
    
    idx_label <- switch (codelist_name,
      "ReferenceDataCd"  = 1,
      "PosSpecificLevel" = 2,
      if(length(idx_label) == 1) {
        idx_label
      } else {
        which(str_detect(nm, "内容"))
      }
    )
    
    if (length(idx_label) > 1) {
      stop(codelist_name)
    }
    
    f <- here::here("data", "codelist", glue::glue("{codelist_name}.csv"))
    
    d %>% 
      select(
        code  = {{ idx_code }},
        label = {{ idx_label }}
      ) %>% 
      mutate(
        code = if(is.character(code)) code else sprintf("%.0f", code),
        across(everything(), ~ str_remove_all(.x, "\\s"))
      ) %>% 
      readr::write_csv(f)
  }) 
```

列が4つのものは、`DistributionCd`以外は2列が横に並べられている。

``` r
l$`4` %>% 
  purrr::map(colnames)
```

    ## $DistributionCd
    ## [1] "種別"   "コード" "分類"   "説明"  
    ## 
    ## $PrefCd
    ## [1] "コード"       "対応する内容" "コード"       "対応する内容"
    ## 
    ## $PrefCdA33
    ## [1] "コード"       "対応する内容" "コード"       "対応する内容"
    ## 
    ## $PubFacMiclassCd
    ## [1] "コード"       "対応する内容" "コード"       "対応する内容"
    ## 
    ## $hoanrinCd
    ## [1] "コード" "内容"   "コード" "内容"  
    ## 
    ## $midorinokairoCd
    ## [1] "コード" "内容"   "コード" "内容"

``` r
l$`4`$DistributionCd %>% 
  transmute(
    code = コード,
    label = paste(種別, 分類, sep = "_")
  ) %>% 
  readr::write_csv(here::here("data", "codelist", "DistributionCd.csv"))

idx <- names(l$`4`) == "DistributionCd"
l$`4`[!idx] %>% 
  purrr::iwalk(function(d, codelist_name) {
    f <- here::here("data", "codelist", glue::glue("{codelist_name}.csv"))

    dplyr::bind_rows(
      d[, 1:2],
      d[, 3:4]
    ) %>% 
      select(code = 1, label = 2) %>% 
      filter(!is.na(code), code != "") %>% 
      readr::write_csv(f)
  })
```

列が5つのものは1個だけ。

``` r
l$`5`$AdminAreaCd_R105 %>% 
  select(code = 1, label1 = 2, label2 = 3) %>% 
  mutate(
    label = paste0(label1, coalesce(label2, "")),
    .keep = "unused"
  ) %>% 
    readr::write_csv(here::here("data", "codelist", "AdminAreaCd_R105.csv"))
```

列が8つのものは横に並んでいるタイプ

``` r
t8 <- l$`8`$shouhanshubanCd

bind_rows(
  t8[, 1:2],
  t8[, 3:4],
  t8[, 5:6],
  t8[, 7:8]
) %>% 
  select(
    code = 1,
    label = 2
  ) %>% 
  readr::write_csv(here::here("data", "codelist", "shouhanshubanCd.csv"))
```

テーブルが2つあるもの

``` r
# これは2つ目を選んでそのまま使えばいい
d_many_tables$MaritimeOrgCd[[2]] %>% 
  rename(code = 1, label = 2) %>% 
  readr::write_csv(here::here("data", "codelist", "MaritimeOrgCd.csv"))

# 第1版と第2版で違うので別テーブルになっているが、
# かぶらなそうなのでまとめてしまう
d1 <- d_many_tables$PubFacAdminCd[[2]]
d2 <- d_many_tables$PubFacAdminCd[[3]]

d1 <- d1 %>% 
  select(code = コード, label = 対応する内容)

d2 <- d2 %>% 
  select(code = コード, label = 対応する内容)

bind_rows(d1, d2) %>% 
  readr::write_csv(here::here("data", "codelist", "PubFacAdminCd.csv"))

# 年度によって違うので別テーブルになっているが、
# （誤字のやつ以外）かぶらなそうなのでまとめてしまう
d1 <- d_many_tables$PubFacMiclassCd_wf[[1]]
d2 <- d_many_tables$PubFacMiclassCd_wf[[2]]

d1 <- bind_rows(d1[, 1:2], d1[, 3:4])
d2 <- bind_rows(d2[, 1:2], d2[, 3:4])

d3 <- bind_rows(d1, d2)

d3 %>% 
  group_by(コード) %>% 
  filter(n_distinct(対応する内容) > 1)
```

    ## # A tibble: 2 x 2
    ## # Groups:   コード [1]
    ##   コード 対応する内容                      
    ##   <chr>  <chr>                             
    ## 1 17001  一般病院、国立療養所、医療センター
    ## 2 17001  般病院、国立療養所、医療センター

``` r
d3 %>% 
  select(code = 1, label = 2) %>% 
  filter(label != "般病院、国立療養所、医療センター") %>% 
  distinct() %>% 
  readr::write_csv(here::here("data", "codelist", "PubFacMiclassCd_wf.csv"))


# 年度によって違うので別テーブルになっている
# これはほんとにまとめれなさそうなので個別対応になる
d1 <- d_many_tables$WelfareFacMiclassCd[[1]]
d2 <- d_many_tables$WelfareFacMiclassCd[[2]]

d1 <- bind_rows(d1[, 1:2], d1[, 3:4])
d2 <- bind_rows(d2[, 1:2], d2[, 3:4])

# まとめるのむり
bind_rows(d1, d2) %>% 
  group_by(コード) %>% 
  arrange(コード) %>% 
  filter(n_distinct(対応する内容) > 1)
```

    ## # A tibble: 36 x 2
    ## # Groups:   コード [18]
    ##    コード 対応する内容              
    ##    <chr>  <chr>                     
    ##  1 103    養護老人ホーム（一般      
    ##  2 103    養護老人ホーム（一般）    
    ##  3 106    軽費老人ホーム（A型）     
    ##  4 106    軽費老人ホーム（ A型）    
    ##  5 107    軽費老人ホーム（B型）     
    ##  6 107    軽費老人ホーム（ B型）    
    ##  7 109    老人福祉センター（特A型） 
    ##  8 109    老人福祉センター（特 A型）
    ##  9 110    老人福祉センター（A型）   
    ## 10 110    老人福祉センター（ A型）  
    ## # … with 26 more rows

``` r
d1 %>% 
  select(code = 1, label = 2) %>% 
  readr::write_csv(here::here("data", "codelist", "WelfareFacMiclassCd_h23.csv"))

d2 %>% 
  select(code = 1, label = 2) %>% 
  readr::write_csv(here::here("data", "codelist", "WelfareFacMiclassCd_h27.csv"))
```
