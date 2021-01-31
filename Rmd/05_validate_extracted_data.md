Validate extracted data
================

``` r
library(readr)
library(dplyr, warn.conflicts = FALSE)
```

``` r
csv_files <- list.files(here::here("data", "attrs"), full.names = TRUE)
names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))

col_types <- cols(
  name = col_character(),
  code = col_character(),
  description = col_character(),
  type = col_character()
)
d <- purrr::map_dfr(csv_files, read_csv, col_types = col_types, .id = "id")
```

## `code`

変なコードが紛れ込んでいないのを目視確認。

``` r
unique(d$code)
```

    ##   [1] NA           "A15_001"    "A15_002"    "A15_003"    "A15_004"   
    ##   [6] "A15_005"    "A15_006"    "A22_000001" "A22_000002" "A22_000003"
    ##  [11] "A22_500001" "A22_010001" "A22_010002" "A22_020001" "A22_020002"
    ##  [16] "A22_030001" "A22_030002" "A22_040001" "A22_040002" "A22_100001"
    ##  [21] "A22_100002" "A22_100003" "A22_100004" "A22_100005" "A22_100006"
    ##  [26] "A22_100007" "A22_40001"  "A22_40002"  "A22_40003"  "A22_40004" 
    ##  [31] "A22_40005"  "A22_40006"  "A22_40007"  "A22_40008"  "A22_40009" 
    ##  [36] "A22_001"    "A22_002"    "A22_003"    "A22_004"    "A22_005"   
    ##  [41] "A22_006"    "A22_007"    "A22_008"    "A22_009"    "A27_005"   
    ##  [46] "A27_006"    "A27_007"    "A27_008"    "A27_001"    "A27_002"   
    ##  [51] "A27_003"    "A27_004"    "A29_001"    "A29_002"    "A29_003"   
    ##  [56] "A29_004"    "A29_005"    "A29_006"    "A29_007"    "A29_008"   
    ##  [61] "A30a5_001"  "A30a5_002"  "A30a5_003"  "A30a5_004"  "A30a5_005" 
    ##  [66] "A30a5_006"  "A30a5_007"  "A30a5_008"  "A30a5_009"  "A30a5_010" 
    ##  [71] "A30b_001"   "A30b_002"   "A30b_003"   "A30b_004"   "A30b_005"  
    ##  [76] "A30b_006"   "A30b_007"   "A30b_008"   "A30b_009"   "A30b_010"  
    ##  [81] "A30b_011"   "A30b_012"   "A30b_013"   "A30b_014"   "A30b_015"  
    ##  [86] "A30b_016"   "A30b_017"   "A30b_018"   "A30b_019"   "A30b_020"  
    ##  [91] "A30b_021"   "A30b_022"   "A30b_023"   "A30b_024"   "A30b_025"  
    ##  [96] "A30b_026"   "A31_101"    "A31_102"    "A31_103"    "A31_104"   
    ## [101] "A31_201"    "A31_202"    "A31_203"    "A31_204"    "A31_301"   
    ## [106] "A31_302"    "A31_303"    "A31_304"    "A31_401"    "A31_402"   
    ## [111] "A31_403"    "A31_404"    "A32_001"    "A32_002"    "A32_003"   
    ## [116] "A32_004"    "A32_005"    "A32_006"    "A32_007"    "A32_008"   
    ## [121] "A32_009"    "A32_010"    "A33_001"    "A33_002"    "A33_003"   
    ## [126] "A33_004"    "A33_005"    "A33_006"    "A33_007"    "A33_008"   
    ## [131] "A34e_001"   "A34e_002"   "A34e_003"   "A34e_004"   "A35c_001"  
    ## [136] "A35c_002"   "A35c_003"   "A35c_004"   "A35c_005"   "A35f_001"  
    ## [141] "A35f_002"   "A35f_003"   "A35f_004"   "A35f_005"   "A35f_006"  
    ## [146] "A35f_007"   "A35g_001"   "A35g_002"   "A35g_003"   "A35g_004"  
    ## [151] "A35g_005"   "A35g_006"   "A35g_007"   "A35g_008"   "A35g_009"  
    ## [156] "A35h_001"   "A35h_002"   "A35h_003"   "A35h_004"   "A35h_005"  
    ## [161] "A35h_006"   "A35h_007"   "A35h_008"   "A35h_009"   "A37_000001"
    ## [166] "A37_000002" "A37_000003" "A37_000004" "A37_000005" "A37_000006"
    ## [171] "A37_100001" "A37_100002" "A37_200001" "A37_200002" "A37_200003"
    ## [176] "A37_200004" "A37_200005" "A37_200006" "A37_210001" "A37_210002"
    ## [181] "A37_300001" "A37_300002" "A37_300003" "A37_300004" "A37_300005"
    ## [186] "A37_300006" "A37_300007" "A37_310001" "A37_310002" "A37_310003"
    ## [191] "A37_310004" "A37_320001" "A37_330001" "A37_382000" "A37_382010"
    ## [196] "A37_392000" "A37_392010" "A37_400001" "A37_400002" "A37_400003"
    ## [201] "A37_410001" "A37_410002" "A37_410003" "A37_420001" "A37_420002"
    ## [206] "A37_420003" "A38a_001"   "A38a_002"   "A38a_003"   "A38a_004"  
    ## [211] "A38a_005"   "A38b_001"   "A38b_002"   "A38b_003"   "A38b_004"  
    ## [216] "A38b_005"   "A38b_006"   "A38b_007"   "A38b_008"   "A38b_009"  
    ## [221] "A38b_010"   "A38b_011"   "A38c_001"   "A38c_002"   "A39_001"   
    ## [226] "A39_002"    "A39_003"    "A39_004"    "A39_005"    "A39_006"   
    ## [231] "A39_007"    "A39_008"    "A39_009"    "A39_010"    "A39_011"   
    ## [236] "A39_012"    "A39_013"    "A39_014"    "A39_015"    "A39_016"   
    ## [241] "A39_017"    "A39_018"    "A39_019"    "A39_020"    "A39_021"   
    ## [246] "A39_022"    "A39_023"    "A39_024"    "A39_025"    "A40_001"   
    ## [251] "A40_002"    "A40_003"    "A42_001"    "A42_002"    "A42_003"   
    ## [256] "A42_004"    "A42_005"    "A42_006"    "A42_007"    "A42_008"   
    ## [261] "A42_009"    "A42_010"    "A43_001"    "A43_002"    "A43_003"   
    ## [266] "A43_004"    "A43_005"    "A43_006"    "A43_007"    "A43_008"   
    ## [271] "A43_009"    "A43_010"    "A43_011"    "A43_012"    "A44_001"   
    ## [276] "A44_002"    "A44_003"    "A44_004"    "A44_005"    "A44_006"   
    ## [281] "A44_007"    "A44_008"    "A44_009"    "A44_010"    "A45_001"   
    ## [286] "A45_002"    "A45_003"    "A45_004"    "A45_005"    "A45_006"   
    ## [291] "A45_007"    "A45_008"    "A45_009"    "A45_010"    "A45_011"   
    ## [296] "A45_012"    "A45_013"    "A45_014"    "A45_015"    "A45_016"   
    ## [301] "A45_017"    "A45_018"    "A45_019"    "A45_020"    "A45_021"   
    ## [306] "A45_022"    "A45_023"    "A45_024"    "A45_025"    "A45_026"   
    ## [311] "A45_027"    "A45_028"    "A45_029"    "A45_030"    "A45_031"   
    ## [316] "A45_032"    "A45_033"    "C28_001"    "C28_003"    "C28_004"   
    ## [321] "C28_005"    "C28_006"    "C28_007"    "C28_008"    "C28_009"   
    ## [326] "C28_010"    "C28_011"    "C28_012"    "C28_013"    "C28_014"   
    ## [331] "C28_015"    "C28_016"    "G04a_001"   "G04a_002"   "G04a_003"  
    ## [336] "G04a_004"   "G04a_005"   "G04a_006"   "G04a_007"   "G04a_008"  
    ## [341] "G04a_009"   "G04a_010"   "G04c_001"   "G04c_002"   "G04c_003"  
    ## [346] "G04c_004"   "G04c_005"   "G04c_006"   "G04c_007"   "G04c_008"  
    ## [351] "G04c_009"   "G04c_010"   "G04d_001"   "G04d_002"   "G04d_003"  
    ## [356] "G04d_004"   "G04d_005"   "G04d_006"   "G04d_007"   "G04d_008"  
    ## [361] "G04d_009"   "G04d_010"   "G08_001"    "G08_002"    "G08_003"   
    ## [366] "L03b_c_001" "L03b_c_002" "L03b_c_003" "L03b_c_004" "L05_001"   
    ## [371] "L05_002"    "L05_003"    "L05_004"    "L05_005"    "L05_006"   
    ## [376] "L05_007"    "L05_008"    "L05_009"    "L05_010"    "L05_011"   
    ## [381] "L05_013"    "L05_014"    "L05_015"    "L05_016"    "N02_001"   
    ## [386] "N02_002"    "N02_003"    "N02_004"    "N02_005"    "N03_001"   
    ## [391] "N03_002"    "N03_003"    "N03_004"    "N03_007"    "N05_001"   
    ## [396] "N05_002"    "N05_003"    "N05_004"    "N05_005e"   "N05_006"   
    ## [401] "N05_007"    "N05_008"    "N05_009"    "N05_010"    "N05_011"   
    ## [406] "N06_001"    "N06_002"    "N06_003"    "N06_004"    "N06_005"   
    ## [411] "N06_006"    "N06_007"    "N06_008"    "N06_009"    "N06_010"   
    ## [416] "N06_011"    "N06_012"    "N06_013"    "N06_014"    "N06_015"   
    ## [421] "N06_016"    "N06_017"    "N06_018"    "N06_019"    "N07_001"   
    ## [426] "N07_002"    "N07_003"    "N07_004"    "N07_005"    "N07_006"   
    ## [431] "N07_007"    "N08_001"    "N08_002"    "N08_003"    "N08_004"   
    ## [436] "N08_005"    "N08_006"    "N08_007"    "N08_008"    "N08_009"   
    ## [441] "N08_010"    "N08_011"    "N08_012"    "N08_013"    "N08_014"   
    ## [446] "N08_015"    "N08_016"    "N08_017"    "N08_018"    "N08_019"   
    ## [451] "N08_020"    "N08_021"    "N08_022"    "N08_023"    "N08_024"   
    ## [456] "N08_025"    "N09_001"    "N09_002"    "N09_003"    "N09_004"   
    ## [461] "N09_005"    "N09_006"    "N09_007"    "N09_008"    "N09_009"   
    ## [466] "N09_010"    "N09_011"    "N09_012"    "N09_013"    "N09_014"   
    ## [471] "N09_015"    "N09_016"    "N09_017"    "N09_018"    "N09_019"   
    ## [476] "N09_020"    "N09_021"    "N09_022"    "N09_023"    "N09_024"   
    ## [481] "N09_025"    "N09_026"    "N09_027"    "N09_028"    "N09_029"   
    ## [486] "N09_030"    "N10_001"    "N10_002"    "N10_003"    "N10_004"   
    ## [491] "N10_005"    "N10_006"    "N10_007"    "N10_008"    "N11_001"   
    ## [496] "N11_002"    "N11_003"    "N11_004"    "N11_005"    "N11_006"   
    ## [501] "N11_007"    "P04_001"    "P04_002"    "P04_003"    "P04_004"   
    ## [506] "P04_005"    "P04_006"    "P04_007"    "P13_001"    "P13_002"   
    ## [511] "P13_003"    "P13_004"    "P13_005"    "P13_006"    "P13_007"   
    ## [516] "P13_008"    "P13_009"    "P13_010"    "P14_001"    "P14_002"   
    ## [521] "P14_003"    "P14_004"    "P14_005"    "P14_006"    "P14_007"   
    ## [526] "P14_008"    "P14_009"    "P14_010"    "P14_011"    "P15_001"   
    ## [531] "P15_002"    "P15_003"    "P15_004"    "P15_005"    "P15_006"   
    ## [536] "P15_007"    "P15_008"    "P15_009"    "P15_010"    "P15_011"   
    ## [541] "P15_012"    "P15_013"    "P15_014"    "P15_015"    "P15_016"   
    ## [546] "P15_017"    "P15_018"    "P16_001"    "P16_002"    "P16_003"   
    ## [551] "P16_004"    "P16_005"    "P16_006"    "P16_007"    "P16_008"   
    ## [556] "P16_009"    "P16_010"    "P16_011"    "P16_012"    "P16_013"   
    ## [561] "P16_014"    "P17_001"    "P17_002"    "P17_003"    "P17_004"   
    ## [566] "P17_005"    "P18_001"    "P18_002"    "P18_003"    "P18_004"   
    ## [571] "P18_005"    "P19_001"    "P19_002"    "P19_003"    "P19_004"   
    ## [576] "P19_005"    "P19_006"    "P19_007"    "P19_008"    "P20_001"   
    ## [581] "P20_002"    "P20_003"    "P20_004"    "P20_005"    "P20_006"   
    ## [586] "P20_007"    "P20_008"    "P20_009"    "P20_010"    "P20_011"   
    ## [591] "P20_012"    "P24_001"    "P24_002"    "P24_003"    "P24_004"   
    ## [596] "P24_005"    "P24_006"    "P24_007"    "P24_008"    "P24_009"   
    ## [601] "P24_010"    "P24_011"    "P24_012"    "P26_001"    "P26_002"   
    ## [606] "P26_003"    "P26_004"    "P26_005"    "P26_006"    "P26_007"   
    ## [611] "P26_008"    "P26_009"    "P26_010"    "P26_011"    "P26_012"   
    ## [616] "P26_013"    "P27_001"    "P27_002"    "P27_003"    "P27_004"   
    ## [621] "P27_005"    "P27_006"    "P27_007"    "P27_008"    "P27_009"   
    ## [626] "P28_001"    "P28_002"    "P28_003"    "P28_004"    "P28_005"   
    ## [631] "P28_006"    "P28_007"    "P29_001"    "P29_002"    "P29_003"   
    ## [636] "P29_004"    "P29_005"    "P29_006"    "P29_007"    "P30_001"   
    ## [641] "P30_002"    "P30_003"    "P30_004"    "P30_005"    "P30_006"   
    ## [646] "P30_007"    "P31_001"    "P31_002"    "P31_003"    "P31_004"   
    ## [651] "P31_005"    "P31_006"    "P31_007"    "P31_008"    "P31_009"   
    ## [656] "P32_001"    "P32_002"    "P32_003"    "P32_004"    "P32_005"   
    ## [661] "P32_006"    "P32_007"    "P32_008"    "P32_009"    "P34_001"   
    ## [666] "P34_002"    "P34_003"    "P34_004"    "P35_001"    "P35_002"   
    ## [671] "P35_003"    "P35_004"    "P35_005"    "P35_006"    "P35_007"   
    ## [676] "P35_008"    "P35_009"    "P35_010"    "P35_011"    "P35_012"   
    ## [681] "P35_013"    "P35_014"    "P35_015"    "P35_016"    "P35_017"   
    ## [686] "P35_018"    "P35_019"    "P35_020"    "P35_021"    "P35_022"   
    ## [691] "P35_023"    "P35_024"    "P35_025"    "P35_026"    "P35_027"   
    ## [696] "P35_028"    "S05d_044"   "S05d_045"   "S05d_046"   "S05d_047"  
    ## [701] "S05d_048"   "S05d_049"   "S05d_050"   "S05d_051"   "S05d_052"  
    ## [706] "S05d_053"   "S05d_054"   "S05d_055"   "S05d_056"   "S05d_057"  
    ## [711] "S05d_058"   "S05d_059"   "S05d_060"   "S05d_061"   "S05d_001"  
    ## [716] "S05d_002"   "S05d_003"   "S05d_004"   "S05d_005"   "S05d_006"  
    ## [721] "S05d_007"   "S05d_008"   "S05d_009"   "S05d_010"   "S05d_011"  
    ## [726] "S05d_012"   "S05d_013"   "S05d_014"   "S05d_015"   "S05d_016"  
    ## [731] "S05d_017"   "S05d_018"   "S05d_019"   "S05d_020"   "S05d_021"  
    ## [736] "S05d_022"   "S05d_023"   "S05d_024"   "S05d_025"   "S05d_026"  
    ## [741] "S05d_027"   "S05d_028"   "S05d_029"   "S05d_030"   "S05d_031"  
    ## [746] "S05d_032"   "S05d_033"   "S05d_034"   "S05d_035"   "S05d_036"  
    ## [751] "S05d_037"   "S05d_038"   "S05d_039"   "S05d_040"   "S05d_041"  
    ## [756] "S05d_042"   "S05d_043"   "S10a_001"   "S10a_002"   "S10a_003"  
    ## [761] "S10a_004"   "S10a_005"   "S10a_006"   "S10a_007"   "S10a_008"  
    ## [766] "S10a_009"   "S10a_010"   "S10a_011"   "S10a_012"   "S10a_013"  
    ## [771] "S10a_014"   "S10a_015"   "S10a_016"   "S10a_017"   "S10a_018"  
    ## [776] "S10a_019"   "S10a_020"   "S10a_021"   "S10a_022"   "S10a_023"  
    ## [781] "S10a_024"   "S10a_025"   "S10a_026"   "S10a_027"   "S10a_028"  
    ## [786] "S10a_029"   "S10a_030"   "S10a_031"   "S10a_032"   "S10a_033"  
    ## [791] "S10a_034"   "S10a_035"   "S10a_036"   "S10a_037"   "S10a_038"  
    ## [796] "S10a_039"   "S10a_040"   "S10a_041"   "S10a_042"   "S10a_043"  
    ## [801] "S10a_044"   "S10a_045"   "S10a_046"   "S10a_047"   "S10a_048"  
    ## [806] "S10a_049"   "S10a_050"   "S10a_051"   "S10a_052"   "S10a_053"  
    ## [811] "S10a_054"   "S10a_055"   "S10a_056"   "S10a_057"   "S10a_058"  
    ## [816] "S10a_059"   "S10a_060"   "S10a_061"   "S10a_062"   "S10a_063"  
    ## [821] "S10a_064"   "S10a_065"   "S10a_066"   "S10a_067"   "S10a_068"  
    ## [826] "S10a_069"   "S10a_070"   "S10a_071"   "S10a_072"   "S10a_073"  
    ## [831] "S10a_074"   "S10a_075"   "S10a_076"   "S10a_077"   "S10a_078"  
    ## [836] "S10a_079"   "S10a_080"   "S10a_081"   "S10a_082"   "S10a_083"  
    ## [841] "S10a_084"   "S10a_085"   "S10a_086"   "S10a_087"   "S10a_088"  
    ## [846] "S10a_089"   "S10a_090"   "S10a_091"   "S10a_092"   "S10a_093"  
    ## [851] "S10a_094"   "S10a_095"   "S10a_096"   "S10a_097"   "S10a_098"  
    ## [856] "S10a_099"   "S10a_100"   "S10a_101"   "S10a_102"   "S10a_103"  
    ## [861] "S10a_104"   "S10a_105"   "S10a_106"   "S10a_107"   "S10a_108"  
    ## [866] "S10a_109"   "S10a_110"   "S10a_111"   "S10a_112"   "S10a_113"  
    ## [871] "S10a_114"   "S10a_115"   "S10a_116"   "S10a_117"   "S10a_118"  
    ## [876] "S10a_119"   "S10a_120"   "S10a_121"   "S10a_122"   "S10a_123"  
    ## [881] "S10a_124"   "S10a_125"   "S10a_126"   "S10b_001"   "S10b_002"  
    ## [886] "S10b_003"   "S10b_004"   "S10b_005"   "S10b_006"   "S10b_007"  
    ## [891] "S10b_008"   "S10b_009"   "S12_001"    "S12_002"    "S12_003"   
    ## [896] "S12_004"    "S12_005"    "S12_006"    "S12_007"    "S12_008"   
    ## [901] "S12_009"    "S12_010"    "S12_011"    "S12_012"    "S12_013"   
    ## [906] "S12_014"    "S12_015"    "S12_016"    "S12_017"    "S12_018"   
    ## [911] "S12_019"    "S12_020"    "S12_021"    "S12_022"    "S12_023"   
    ## [916] "S12_024"    "S12_025"    "S12_026"    "S12_027"    "S12_028"   
    ## [921] "S12_029"    "S12_030"    "S12_031"    "S12_032"    "S12_033"   
    ## [926] "S12_034"    "S12_035"    "S12_036"    "S12_037"    "W01_001"   
    ## [931] "W01_002"    "W01_003"    "W01_004"    "W01_005"    "W01_006"   
    ## [936] "W01_007"    "W01_008"    "W01_009"    "W01_010"    "W01_011"   
    ## [941] "W01_012"    "W01_013"    "W01_014"    "W09_001"    "W09_002"   
    ## [946] "W09_003"    "W09_004"

コード欠損率を表示。

``` r
d %>% 
  group_by(id) %>% 
  summarise(
    n = n(),
    missing_rate = scales::percent(sum(is.na(code)) / n(), accuracy = 0.1)
  ) %>% 
  knitr::kable()
```

| id       |   n | missing\_rate |
|:---------|----:|:--------------|
| A03      |   8 | 100.0%        |
| A10      |   3 | 100.0%        |
| A11      |   3 | 100.0%        |
| A12      |   3 | 100.0%        |
| A13      |   3 | 100.0%        |
| A15      |   6 | 0.0%          |
| A16      |  12 | 100.0%        |
| A17      |   9 | 100.0%        |
| A18      |  10 | 100.0%        |
| A18s-a   |   3 | 100.0%        |
| A19      |   9 | 100.0%        |
| A19s     |   5 | 100.0%        |
| A20s     |   5 | 100.0%        |
| A21      |   2 | 100.0%        |
| A21s     |   5 | 100.0%        |
| A22      |   9 | 0.0%          |
| A22-m    |  43 | 34.9%         |
| A22s     |   3 | 100.0%        |
| A23      |   9 | 100.0%        |
| A24      |  11 | 100.0%        |
| A25      |   8 | 100.0%        |
| A26      |   2 | 100.0%        |
| A27      |   8 | 0.0%          |
| A28      |   1 | 100.0%        |
| A29      |   8 | 0.0%          |
| A30a5    |  10 | 0.0%          |
| A30b     |  26 | 0.0%          |
| A31      |  16 | 0.0%          |
| A32      |  10 | 0.0%          |
| A33      |   8 | 0.0%          |
| A34      |  18 | 77.8%         |
| A35a     |  12 | 58.3%         |
| A35b     |  16 | 56.2%         |
| A35c     |  18 | 0.0%          |
| A37      |  46 | 8.7%          |
| A38      |  21 | 14.3%         |
| A39      |  25 | 0.0%          |
| A40      |   3 | 0.0%          |
| A42      |  19 | 0.0%          |
| A43      |  12 | 0.0%          |
| A44      |  10 | 0.0%          |
| A45      |  33 | 0.0%          |
| C02      |  17 | 100.0%        |
| C09      |  12 | 100.0%        |
| C23      |   6 | 100.0%        |
| C28      |  16 | 6.2%          |
| G02      |  58 | 100.0%        |
| G04-a    |  10 | 0.0%          |
| G04-c    |  10 | 0.0%          |
| G04-d    |  10 | 0.0%          |
| G08      |   3 | 0.0%          |
| L01      |  55 | 100.0%        |
| L02      |  53 | 100.0%        |
| L03-a    |   1 | 100.0%        |
| L03-b    |   2 | 100.0%        |
| L03-b-c  |   4 | 0.0%          |
| L03-b-u  |   2 | 100.0%        |
| L05      |  15 | 0.0%          |
| mesh1000 |  17 | 100.0%        |
| mesh500  |  17 | 100.0%        |
| N02      |   5 | 0.0%          |
| N03      |   5 | 0.0%          |
| N04      |  17 | 100.0%        |
| N05      |  11 | 0.0%          |
| N06      |  19 | 0.0%          |
| N07      |   7 | 0.0%          |
| N08      |  25 | 0.0%          |
| N09      |  30 | 0.0%          |
| N10      |   8 | 0.0%          |
| N11      |   7 | 0.0%          |
| P02      |   7 | 100.0%        |
| P04      |   7 | 0.0%          |
| P05      |   4 | 100.0%        |
| P07      |   2 | 100.0%        |
| P09      |  13 | 100.0%        |
| P11      |   4 | 100.0%        |
| P13      |  10 | 0.0%          |
| P14      |  11 | 0.0%          |
| P15      |  19 | 5.3%          |
| P16      |  15 | 6.7%          |
| P17      |   6 | 16.7%         |
| P18      |   6 | 16.7%         |
| P19      |   8 | 0.0%          |
| P20      |  12 | 0.0%          |
| P21      |   9 | 100.0%        |
| P24      |  12 | 0.0%          |
| P26      |  13 | 0.0%          |
| P27      |   9 | 0.0%          |
| P28      |   7 | 0.0%          |
| P29      |   7 | 0.0%          |
| P30      |   7 | 0.0%          |
| P31      |   9 | 0.0%          |
| P32      |   9 | 0.0%          |
| P34      |   4 | 0.0%          |
| P35      |  28 | 0.0%          |
| S05-a    |  66 | 100.0%        |
| S05-b    |  66 | 100.0%        |
| S05-c    |  57 | 100.0%        |
| S05-d    |  61 | 0.0%          |
| S10a     | 126 | 0.0%          |
| S10b     |   9 | 0.0%          |
| S12      |  37 | 0.0%          |
| W01      |  14 | 0.0%          |
| W05      |   7 | 100.0%        |
| W07      |   5 | 100.0%        |
| W09      |   5 | 20.0%         |

### `A22-m`, `A34`, `A35a`, `A35b`, `A37`, `P15`, `P16`, `P17`, `P18`,

ルールに従って展開する必要がある。あとで個別対応。

``` r
d %>% 
  filter(id == "A22-m", is.na(code)) %>% 
  head() %>% 
  knitr::kable()
```

| id    | name                                        | code | description                                                                                                     | type              |
|:------|:--------------------------------------------|:-----|:----------------------------------------------------------------------------------------------------------------|:------------------|
| A22-m | 各年度別最深積雪（A22\_01XXXX）XXXX：西暦   | NA   | 最古年～平成25（2013）年における各年度別の積雪深の最大値（cm）※欠測の場合：99999998統計資料がない場合：99999999 | 実数型（Real）    |
| A22-m | 各年度別累計降雪量（A22\_02XXXX）XXXX：西暦 | NA   | 最古年～平成25（2013）年における各年度別の累計降雪量（cm）※欠測の場合：99999998統計資料がない場合：99999999     | 実数型（Real）    |
| A22-m | 各年度別最低気温（A22\_03XXXX）XXXX：西暦   | NA   | 最古年～平成25（2013）年における各年度別の気温の最低値（℃）※欠測の場合：99999998統計資料がない場合：99999999    | 実数型（Real）    |
| A22-m | 各年度別平均風速（A22\_04XXXX）XXXX：西暦   | NA   | 最古年～平成25（2013）年における各年度別の平均風速（m/s）※欠測の場合：99999998統計資料がない場合：99999999      | 実数型（Real）    |
| A22-m | 各年別死者数（A22\_10XXXX）XXXX：西暦       | NA   | 最古年～平成25（2013）年の各年別の雪害による死者数死者数が不明の場合は、「-1」                                  | 整数型（Integer） |
| A22-m | 各年別行方不明者数（A22\_11XXXX）XXXX：西暦 | NA   | 最古年～平成25（2013）年の各年別の雪害による行方不明者数行方不明者数が不明の場合は、「-1」                      | 整数型（Integer） |

### `L03-a`

複雑そう。あとで個別対応。

``` r
d %>% 
  filter(id == "L03-a") %>% 
  pull(name)
```

    ## [1] "（田、森林、建物用地などの各利用区分が属性名となる。）※整備年度により異なる。※利用区分については、コードリスト「土地利用種別　（昭和51年度・昭和62年度・平成3年度、9年度、18年度・平成21年度、26年度、平成28年度）」を参照。"

### `mesh1000`, `mesh500`

年代層によって後ろに`A`〜`D`がつく（説明と違って実際には `_`
はないみたい）。これもあとで個別対応。
あと、ここはファイル名が`mesh1000`ではなく`m1000`になってるっぽいのでそこも対応が必要かも。

``` r
d %>% 
  filter(id == "mesh1000") %>% 
  slice(3:4) %>% 
  glimpse()
```

    ## Rows: 2
    ## Columns: 5
    ## $ id          <chr> "mesh1000", "mesh1000"
    ## $ name        <chr> "POP2010", "POP2020（_A～_d〉"
    ## $ code        <chr> NA, NA
    ## $ description <chr> "1kmメッシュ別2010年人口数（補正あり）※2010年人口については、総人口データのみ作成している。", "…
    ## $ type        <chr> "実数型（少数点以下第3位。以下同様）", "実数型"

### `P21`

`-`と`_`の打ち間違いっぽい。そして実際のカラム名は`P21A_001`のようになっていて、`P21`のあとに付くアルファベットが大文字な点にも注意。

``` r
d %>% 
  filter(id == "P21") %>% 
  knitr::kable()
```

| id  | name                     | code | description                                                         | type                                                                           |
|:----|:-------------------------|:-----|:--------------------------------------------------------------------|:-------------------------------------------------------------------------------|
| P21 | 事業主体（P21a-001）     | NA   | 当該給水区域の事業主体                                              | 文字列型                                                                       |
| P21 | 事業名称（P21a-002）     | NA   | 当該給水区域の水道事業の名称                                        | 文字列型                                                                       |
| P21 | 種別（P21a-003）         | NA   | 当該給水区域の種別から得られた種別コード                            | コードリスト型（種別コード）上水道=1、簡易水道（公営）=2、簡易水道（非公営）=3 |
| P21 | 給水人口（P21a-004）     | NA   | 給水区域内に居住し、水道により給水を受けている人口。                | 整数型                                                                         |
| P21 | 日最大給水量（P21a-005） | NA   | 一日に給水することができる最大の水量（水道法第三条6項二）。単位：m3 | 実数型                                                                         |
| P21 | 事業主体（P21b-001）     | NA   | 当該給水区域の事業主体                                              | 文字列型                                                                       |
| P21 | 事業名称（P21b-002）     | NA   | 当該給水区域の水道事業の名称                                        | 文字列型                                                                       |
| P21 | 施設名称（P21b-003）     | NA   | 当該施設の名称                                                      | 文字列型                                                                       |
| P21 | 日最大給水量（P21b-004） | NA   | 一日に給水することができる最大の水量（水道法第三条6項二）。単位：m3 | 実数型                                                                         |

### `N05`

なにやら特殊ルールがあるっぽい。

``` r
d %>%
  filter(id == "N05") %>%
  slice(5) %>% 
  glimpse()
```

    ## Rows: 1
    ## Columns: 5
    ## $ id          <chr> "N05"
    ## $ name        <chr> "設置期間（設置開始）（N05_005b）設置期間（設置終了）"
    ## $ code        <chr> "N05_005e"
    ## $ description <chr> "鉄道路線、駅が設置された年（西暦年）。なお、昭和25年以前に設置された場合は1950とする。鉄道路線、駅が変更…
    ## $ type        <chr> "時間型（TM_Instant）"

## `type`

``` r
d %>% 
  count(
    type = case_when(
      stringr::str_detect(type, "^文字列?型")    ~ "character",
      stringr::str_detect(type, "^整数値?型")    ~ "integer",
      type == "数値型（Integer）"                ~ "integer",
      stringr::str_detect(type, "^実数型")       ~ "double",
      type == "数値型（Decimal）"                ~ "double",
      type == "十進数型（Decimal）"              ~ "double",
      # 時間型は、実際の数字が何桁かで場合分けする必要がありそう（4桁なら年なのでそのまま？、8桁ならDateに変換？）
      stringr::str_detect(type, "^(時間|日付)型") ~ "date_or_year",
      # 順序型は、本来は時刻の日時部分だけだが、これだけ対応するにはちょっと特殊すぎる気がするので character で残す
      stringr::str_detect(type, "^順序型")       ~ "character",
      type == "西暦年4桁、月2桁、日2桁で表す数字8文字" ~ "time",
      # 「コードリスト」は数値型だったりもする（e.g. P16）ので、どの位置でも「コードリスト」が出てくればコードリストとみなす
      stringr::str_detect(type, "コードリスト")  ~ "codelist",
      stringr::str_detect(type, "(行政|参照資料|産廃施設|都道府県|特別管理|不燃領域率定義|防災再開発促進地区指定)コード") ~ "codelist",
      # L03-b-c
      type == "都市地域=1都市地域外=0" ~ "codelist",
      stringr::str_detect(type, "^列挙型")       ~ "factor",
      stringr::str_detect(type, "^真偽値型")     ~ "logical",
      stringr::str_detect(type, "^論理型")       ~ "logical",
      type %in% c("タイプ型「調査内容」") ~ "character",
      TRUE ~ type
    ),
    sort = TRUE
  )
```

    ## # A tibble: 9 x 2
    ##   type             n
    ##   <chr>        <int>
    ## 1 integer        488
    ## 2 character      443
    ## 3 codelist       269
    ## 4 double         165
    ## 5 <NA>           150
    ## 6 logical         47
    ## 7 date_or_year    32
    ## 8 factor          27
    ## 9 time             3