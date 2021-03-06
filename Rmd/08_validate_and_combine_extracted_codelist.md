Validate and combine codelist
================

codelistがカラムとマッチするか確認する

``` r
library(readr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)

matching_types <- c("exact", "positional", "other")

d_csv_file <- purrr::map_dfr(matching_types, ~ {
  dir <- here::here("data", glue::glue("colnames_{.x}"))
  tibble::tibble(
    file = fs::dir_ls(dir),
    id = tools::file_path_sans_ext(basename(file)),
    matching_type = .x
  )
})

# 列名のマッチングの仕方の種類は書き出しておく
d_csv_file %>% 
  select(id, matching_type) %>% 
  filter(matching_type != "other") %>% 
  readr::write_csv(here::here("data", "matching_types.csv"))

col_types <- cols(
  name = col_character(),
  code = col_character(),
  description = col_character(),
  type = col_character()
)

d <- purrr::pmap_dfr(d_csv_file, function(file, id, ...) {
  d <- readr::read_csv(file, col_types = col_types)
  d %>% 
    tibble::rowid_to_column() %>% 
    mutate(id = id, .before = 1L)
})
```

いくつか重複があるので、`id`, `code`,
`name`の組がユニークになるようにまとめなおす

``` r
d %>%
  group_by(id, code, name) %>%
  filter(n() > 1) %>%
  knitr::kable()
```

| rowid | id  | name | code | description | type | codelist |
|------:|:----|:-----|:-----|:------------|:-----|:---------|

``` r
d <- d %>% 
  group_by(id, code, name) %>% 
  summarise(
    across(.fns = first),
    .groups = "drop"
  )
```

個別対応したものは先に紐づける。ここで紐付かなかったものに対して、対応表を正規表現でマッチさせる。

``` r
d <- d %>%
  mutate(
    type = str_squish(type),
    codelist_id_manual = case_when(
      code == "A03_007" ~ "tweak_SectionCd",
      
      code == "A15_003" ~ "authority_type",
      code == "A15_004" ~ "protection_area_type",
      
      code == "A26_005" ~ "SedimentDisastersProneAreaCd",
      
      code == "C02_001" ~ "ClassHarbor1Cd",
      code == "C02_002" ~ "ClassHarbor2Cd",
      code == "C02_003" ~ "AdminAreaCd",
      code == "C02_006" ~ "AdminHarborCd",
      code == "C02_010" ~ "MaritimeOrgCd",
      code == "C02_014" ~ "PrefCd",
      code == "C02_016" ~ "PrefCd",
      
      code == "C09_003" ~ "AdminAreaCd",
      code == "C09_004" ~ "ClassFishPortCd",
      code == "C09_005" ~ "FishPortAdminCd",
      code == "C09_011" ~ "PrefCd",
      
      code == "C23_001" ~ "AdminAreaCd",
      code == "C23_002" ~ "AdminSeaLineCd",

      code %in% c("G04a_005", "G04c_005", "G04d_005") ~ "undersea",
      code %in% c("G04a_007", "G04a_009","G04c_007","G04c_009","G04d_007","G04d_009") ~ "direction",
      
      code == "P03_0102" ~ "hydroelectric_power_plant_type",
      code == "P03_0209" ~ "pumpingup_type",
      
      code == "P13_009" ~ "urban_planning_decided",
      
      code == "P15_017" ~ "industrial_waste_disposal",
      code == "P15_018" ~ "industrial_waste_special_treatment",
      
      code == "P17_003" ~ "firehouse_type",
      
      code == "P19_002" ~ "PrefCd",
      
      code == "P21A_003" ~ "water_supply_type",
      
      code == "P24_011" ~ "refereced_from_agri",
      
      code == "L03b_c_002" ~ "tweak_LandUseCd-09",
      
      code == "L01_005" ~ "seaside_type",
      
      code %in% c("P17_002", "P18_002", "P20_001", "P24_002", "P32_003") ~ "AdminAreaCd",
      code == "P22a_001" ~ "AdminAreaCd",
      code == "P22b_001" ~ "AdminAreaCd",
      code == "P23a_001" ~ "AdminAreaCd",
      code == "P23b_001" ~ "AdminAreaCd",
      
      code %in% c("S05a_001", "S05b_001") ~ "PTAreaCd",
      code == "S05a_003" ~ "TripGenerationCd",
      code %in% c("S05a_004", "S05b_003", "S05b_004") ~ "AreaZoneCd", # TODO
      
      code == "W05_001" ~ "WaterSystemCodeCd",
      code == "W05_003" ~ "section_type",
      code == "W05_005" ~ "OriginalDataCodeCd",
      
      code == "W09_002" ~ "AdminAreaCd",
      
      id == "A18s-a" & code == "A18_008" ~ "SpecificAirPortSpecifiedSituationCd",
      id == "A19s"   & code == "A19_009" ~ "RitoCd",
      id == "A19s"   & code == "A19_010" ~ "SpecificAirPortSpecifiedSituationCd",
      id == "A20s"   & code == "A20_008" ~ "SpecificAirPortSpecifiedSituationCd",
      id == "A21s"   & code == "A21_007" ~ "SpecificAirPortSpecifiedSituationCd",
      id == "A22s"   & code == "A22_007" ~ "HeavySnowTypeCode",
      id == "A22s"   & code == "A22_008" ~ "SpecificAirPortSpecifiedSituationCd",

      id == "A35b" & name %in% c("種別", "種別コード") ~ "landscape_district_type",
      id %in% c("C02", "C09") & name == "行政区域コード" ~ "AdminAreaCd",
      id == "L01" & name == "標準地行政区域コード" ~ "AdminAreaCd",
      id == "L02" & name == "基準地行政区域コード" ~ "AdminAreaCd",
      id == "A09" & code == "prefec_cd" ~ "PrefCd",
      id == "A09" & code == "area_cd"   ~ "AdminAreaCd",  # どうやら説明が間違ってるっぽい
      id == "A09" & code == "layer_no"  ~ "A10_layer_no",
      id %in% c("A10", "A11", "A12", "A13") & code == "PREFEC_CD" ~ "PrefCd",
      id %in% c("A10", "A11", "A12", "A13") & code == "AREA_CD"   ~ "A10_area_code",
      id %in% c("A10", "A11", "A12", "A13") & code == "LAYER_NO"  ~ "A10_layer_no",
    )
  )
```

URLから取ってきた分を`d`に紐付ける。
まずは`codelist_list.csv`を読み込み。

``` r
d_codelist <- readr::read_csv(here::here("data", "codelist_list.csv"), col_types = "ccc")

# いくつか除外したやつもあるので、実際に存在するやつだけに絞り込む
codelist_existing <- fs::dir_ls(here::here("data", "codelist")) %>% 
  basename() %>% 
  tools::file_path_sans_ext()

# LandUseCd は tweak_LandUseCd の方を使うので除外
# SectionTypeCd も地域によって微妙に違うので個別対応が必要、ここでは除外する
exclude_pattern <- "^LandUseCd|^SectionTypeCd"

d_codelist <- d_codelist %>% 
  filter(
    codelist_id %in% codelist_existing,
    !str_detect(codelist_id, exclude_pattern)
  ) %>% 
  mutate(type_part = str_trim(name), .keep = "unused")
```

次に、データを手動で紐づけたものとそれ以外に分割し、それ以外の方に正規表現でくっつける。その際、複数マッチする可能性があるのでなんとかがんばる。

``` r
d_split <- d %>% 
  mutate(
    needs_regex = is.na(codelist_id_manual) & !id %in% c("A03", "A10", "A11", "A12", "A13", "A18s-a", "A20", "A21", "C02", "C09", "C23", "W05", "W07")
  )

d_joined_manual <- d_split %>% 
  filter(!needs_regex) %>% 
  select(id, rowid, code, name, type, codelist, codelist_id = codelist_id_manual)

d_joined_regex <- d_split %>% 
  filter(needs_regex) %>% 
  left_join(d_codelist, by = "id") %>% 
  mutate(
    matched = str_detect(type, fixed(type_part)),
    # 複数のパターンにマッチする場合は長い方を採用する（P31のみのはず）
    matched_size = if_else(matched, nchar(type_part), 0L)
  ) %>% 
  group_by(id, rowid, code, name, type, codelist) %>% 
  summarise(
    codelist_id = if (sum(matched, na.rm = TRUE) > 1) {
      list(codelist_id[which.max(matched_size)])
    } else {
      list(codelist_id[matched])
    },
    .groups = "drop"
  )

# data に入っているのが 0 か 1 であるのを確認してから展開
stopifnot(lengths(d_joined_regex$codelist_id) <= 1)

nrow_before <- nrow(d_joined_regex)

d_joined_regex <- d_joined_regex %>%
  tidyr::unchop(codelist_id, keep_empty = TRUE)

stopifnot(nrow_before == nrow(d_joined_regex))

d_joined <- bind_rows(d_joined_manual, d_joined_regex) %>% 
  # rowidで並べ替えて元の順序を保持する
  arrange(id, rowid) %>% 
  select(!rowid)
```

コードリスト型であると判定されているのに `codelist_id`
がないのはおかしそう。ただし、以下は例外。

-   `L03-b`, `L03-b-c`の「土地利用種別」はあとで個別対応
-   `P14`の「福祉施設細分類」はあとで個別対応
-   `S05-d`はあとで個別対応
-   別カラムに都道府県などがあるので対応不要:
    -   `A03`, `A17~A25`
    -   `A38a_001`, `A38b_001`, `A39_001`, `A39_002`, `A45_002`,
        `A45_003`
-   `A45`の「局名称コード」「署名称コード」は別途ラベルもあるので対応不要
-   `A45`の「樹種コード」
-   `C23_005`と`C23_006`はコードとラベルがペアになってるので対応不要

``` r
# 元と同じ行数のはず
stopifnot(nrow(d) == nrow(d_joined))

d_joined %>%
  filter(
    codelist,
    is.na(codelist_id),
    !(id == "A03" & name %in% c("区域区分")),
    !(id %in% c("A03", "A17", "A18", "A19", "A22", "A23", "A24", "A25") & name == "行政区域コード"),
    !code %in% c("A38a_001", "A38b_001", "A39_001", "A39_002", "A45_002", "A45_003", "A45_008", "A45_009", "A45_015", "A45_018", "A45_021", "L05_005", "P14_006", "C23_005", "C23_006"),
    !(id %in% c("L03-b", "L03-b-u") & name == "土地利用種別"),
    !(str_detect(id, "S05-d") & str_detect(name, "ゾーンコード")),
  ) %>%
  { stopifnot(nrow(.) == 0) }

d_joined %>% 
  select(!codelist) %>% 
  readr::write_csv(here::here("data", "joined.csv"))
```
