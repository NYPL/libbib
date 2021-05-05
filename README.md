libbib <img src="http://statethatiamin.com/media/libbibsmall.png" align="right" height="126"/>
===

<!-- badges: start -->
[![Cran Version](http://www.r-pkg.org/badges/version/libbib)](https://cran.r-project.org/package=libbib)
[![Build Status](http://travis-ci.org/NYPL/libbib.svg?branch=master)](https://travis-ci.org/NYPL/libbib)
<!-- badges: end -->


## Description
An R package providing WorldCat API communication, functions for validating
and normalizing bibliographic codes such as ISBN; ISSN; and LCCN, translation
from call numbers (Library of Congress and Dewey Decimal) to their subject
classifications or subclassifications, and other related utilities helpful
for assessment librarians. Also provides various loadable data files such
call number / subject crosswalks and code tables.

Speed of execution and robustness are priorities in this package.
To the end of optimizing speed and efficiency, careful consideration
is taken to exploit vectorized functions and efficient data.table joins
in the code.

On a (real life) example of **3 million** very messy ISBN 10s and 13s,
aggressive ISBN normalization took  _less than 1 minute_ (commodity hardware).
It salvaged almost half a million previously unusable ISBNs, bringing
the number of usable ISBNs (for matching, cataloging, etc...) from
less than 90% to 99.6%!

As for robustness, this package is well tested, with over 300
automated tests, at time of writing.

`data.table` is a hard dependency of this package. Using `data.table`
internally makes, for example, the call number -> subject conversions
very fast. Additionally, loading this package also automatically
loads `data.table`. If you don't use `data.table` in your own code,
everything will work just fine! But you might want to look into it!

## Some examples

```r
> get_dewey_decimal_subject_class("823.912")
[1] "Literature (Belles-lettres) and rhetoric"

> get_lc_call_subject_classification(c("ND 237", "PQ2246.M3"),
+                                    subclassification=TRUE)
[1] "Painting"
[2] "French, Italian, Spanish, and Portuguese literature"

> convert_to_isbn_13(c("012491540X", "9004037810"))
[1] "9780124915404" "9789004037816"

> normalize_isbn_13(c("978-9-66-819791-8", "__9__781572411579"))
[1] "9789668197918" "9781572411579"

> normalize_lccn(" 79139101 /AC/r932")
[1] "79139101"

> # Do a WorldCat APU search on 19th century materials on ethics
> # (Dewey code 170s / LC Call prefix BJ)
> results <- worldcat_api_search('($dewey="17*" or $lc_call="bj*")
+                                    and srw.yr="18*"')
> results[,.(oclc, title, result_number, num_results)][1:5]
        oclc                                       title result_number num_results
      <char>                                      <char>         <int>      <char>
1:   8665856 The principles of moral and political ph...             1        1716
2: 191264919                The economy of human life. /             2        1716
3:  22571399                                   Solitude:             3        1716
4:  65250134 The theory of moral sentiments, or, An e...             4        1716
5:  13106952    Letters on the improvement of the mind :             5        1716

> worldcat_api_bib_read_info_by_isbn("9780984201006")
        oclc          isbn   issn                        title
      <char>        <char> <char>                       <char>
1: 462894360 9780984201006   <NA> The Great Debate about Art /
         author                   leader
         <char>                   <char>
1: Harris, Roy, 00000cam a2200000 a 4500
                                       oh08
                                     <char>
1: 091031s2010    ilua     b    000 0 eng c

> worldcat_api_classify_by_oclc("877749545")
        oclc                 title               author total_holdings total_eholdings
      <char>                <char>               <char>          <int>           <int>
1: 877749545 Max Ernst's 'Celebes' Penrose, Roland, Sir              8               0
2: 877749545 Max Ernst's 'Celebes' Penrose, Roland, Sir              8               0
   call_type recommendation holdings http_status_code classify_response_code
      <char>         <char>   <char>            <int>                  <int>
1:       DCC          759.4        6              200                      0
2:       LCC       ND588.E7        6              200                      0

> worldcat_api_locations_by_oclc("877749545", max_libraries=10,
+                                include.bib.info=FALSE)
        oclc institution_identifier
      <char>                 <char>
1: 877749545                    NLE
2: 877749545                    NLW
3: 877749545                    EUM
4: 877749545                    LTU
5: 877749545                    ELU
6: 877749545                  UKUAL
                                institution_name copies
                                          <char> <char>
1:                  National Library of Scotland      1
2:                     National Library of Wales      1
3:              University of Manchester Library      1
4: University of Leicester, David Wilson Library      1
5:     University of London Senate House Library      1
6:                 University of the Arts London      1

> loc_permalink_from_lccn("73167510", format="marcxml")
[1] https://lccn.loc.gov/73167510/marcxml

> loc_permalink_from_lccn(c("2010292065", "2012451004")
[1] "https://lccn.loc.gov/2010292065" "https://lccn.loc.gov/2012451004"

> worldcat_permalink_from_oclc_number("1005106045")
[1] "http://www.worldcat.org/oclc/1005106045"

> get_isbn_10_check_digit("0-124-91540-X", allow.hyphens=TRUE)
[1] "X"

> is_valid_isbn_10(c("012491540X", "9004037812"))
[1] TRUE FALSE

> marc_leader_get_info("00000cam a22000008i 4500")
         record_type      bib_level
              <char>         <char>
1: Language Material Monograph/Item

# The Brothers Karamazov (1970 reissue but original publication date)
> marc_008_get_info("950622r19701880ru            000 0 rus d",
+                    original.pub.date=TRUE)
      pub_date pub_place_code lang_code
         <int>         <char>    <char>
  1:     1880             ru       rus

# reissue publication date
> marc_008_get_info("950622r19701880ru            000 0 rus d")
      pub_date pub_place_code lang_code
         <int>         <char>    <char>
  1:     1970             ru       rus

> data("dewey_subject_crosswalk")
> dewey_subject_crosswalk
Key: <thekey>
     thekey                                           description
     <char>                                                <char>
  1:    000               Computer science, knowledge and systems
  2:    001                                             Knowledge
  ...
990:    998                         Arctic islands and Antarctica
991:    999                               Extraterrestrial worlds

> data("lc_subject_subclassification")
> lc_subject_subclassification
Key: <thekey>
     thekey                                           description
     <char>                                                <char>
  1:     AC                  Collections. Series. Collected works
  2:     AE                                         Encyclopedias
  ...
227:      Z Books (General). Writing. Paleography. Book indust...
228:     ZA                       Information resources/materials

> get_language_from_code(c("yor", "spa"))
[1] "Yoruba"  "Spanish"

> get_country_from_code(c("ck", " NYA"))
[1] "Colombia"         "New York (State)"

> someisbns <- c("9782711875177;garbage-isbn;2711875172;2844268900",
+                "1861897952; 978-1-86189-795-4")
> split_map_filter_reduce(someisbns,
+                         mapfun=
+                           function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)},
+                         filterfun=remove_duplicates_and_nas,
+                         reduxfun=recombine_with_sep_closure())
[1] "9782711875177;9782844268907" "9781861897954"

```


## Functions

### ISBNs
- `get_isbn_10_check_digit`
- `check_isbn_10_check_digit`
- `is_valid_isbn_10`
- `normalize_isbn_10`
- `get_isbn_13_check_digit`
- `check_isbn_13_check_digit`
- `is_valid_isbn_13`
- `convert_to_isbn_13`
- `normalize_isbn_13`
- `normalize_isbn`

### ISSN
- `get_issn_check_digit`
- `check_issn_check_digit`
- `is_valid_issn`
- `normalize_issn`

### LCCN
- `normalize_lccn`

### Library of Congress Call Numbers
- `get_lc_call_subject_classification`
- `is_valid_lc_call`
- `get_lc_call_first_letter`
- `get_all_lc_call_subject_letters`

### Dewey Decimal (DCC) Call Numbers
- `get_dewey_decimal_subject_class`
- `get_dewey_decimal_subject_division`
- `get_dewey_decimal_subject_section`

### WorldCat API
- `worldcat_api_search`
- `worldcat_api_bib_read_info_by_oclc`
- `worldcat_api_bib_read_info_by_isbn`
- `worldcat_api_bib_read_info_by_issn`
- `worldcat_api_locations_by_oclc`
- `worldcat_api_locations_by_isbn`
- `worldcat_api_locations_by_issn`
- `worldcat_api_classify_by_oclc`
- `worldcat_api_classify_by_isbn`
- `worldcat_api_classify_by_issn`

### Interfacing with the web
- `loc_permalink_from_lccn`
- `worldcat_permalink_from_issn`
- `worldcat_permalink_from_isbn`
- `worldcat_permalink_from_oclc_number`
- `oclc_classify_link_from_standard_num`

## Included data files (loadable with `data(datafile)`)
- `language_code_crosswalk`
  (from https://www.loc.gov/marc/languages/language_code.html)
- `country_code_crosswalk`
  (from https://www.loc.gov/marc/countries/countries_code.html)
- `lc_subject_classification`
  (from https://www.loc.gov/catdir/cpso/lcco/)
- `lc_subject_subclassification`
  (from https://www.loc.gov/catdir/cpso/lcco/)
- `dewey_subject_crosswalk`
  (from https://www.oclc.org/content/dam/oclc/dewey/ddc23-summaries.pdf)
- `books_serials_etc_sample` -  A very small sample of books, serials, VHSs,
  CDs, and Computer files and some information including title, control
  numbers, call numbers, and call number subject classifications. Somewhat
  messy/inconsistent (deliberately) and mainly for testing.
  Will be expanded in future versions.

### Marc field deconstruction
- `marc_leader_get_info`
- `marc_008_get_info`

### other Marc code translations
- `get_language_from_code`
- `get_country_from_code`

### split / map / filter / reduce and related utilies
- `car`
- `remove_duplicates_and_nas`
- `recombine_with_sep_closure`
- `split_map_filter_reduce`

### miscellaneous utilities
- `dt_del_cols`
- `dt_keep_cols`
- `dt_counts_and_percents`
- `get_clean_names`
- `dt_set_clean_names`
- `dt_percent_not_na`
- `dt_na_breakdown`
- `dt_add_to_col_names`



