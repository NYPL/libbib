libbib
===

![libbib logo](http://statethatiamin.com/media/libbibsmall.png)

[![Build Status](http://travis-ci.org/NYPL/libbib.svg?branch=master)](https://travis-ci.org/NYPL/libbib)

[![](http://www.r-pkg.org/badges/version/libbib)](https://cran.r-project.org/package=libbib)


## Description
An R package providing functions for for validating and normalizing
bibliographic codes such as ISBN, ISSN, and LCCN. Includes functions to
translate Call numbers (Library of Congress and Dewey Decimal) to their
subject classifications or subclassifications. Also provides various
loadable data files such call number / subject crosswalks and code

Speed of execution and robustness are priorities in this package.
To the end of optimizing speed and efficiency, careful consideration
is taken to exploit vectorized functions and efficient data.table joins
in the code.

On a (real life) example of **3 million** very messy ISBN 10s and 13s,
aggressive ISBN normalization took  _less than 1 minute_ (commodity hardware).
It salvaged almost half a million previously unusable ISBNs, bringing
the number of usable ISBNs (for matching, cataloging, etc...) from
less than 90% to 99.6%!

_Note these timings will change for the better or worse pending_
  - _performance enhancements_
  - _recognition and handling procedures for other ways in which
   malformed ISBNs can be salvaged_

As for robustness, this package is well tested, with over 200
automated tests, at time of writing

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

> loc_permalink_from_lccn(c("2010292065", "2012451004")
[1] "https://lccn.loc.gov/2010292065" "https://lccn.loc.gov/2012451004"

> worldcat_permalink_from_oclc_number("1005106045")
[1] "http://www.worldcat.org/oclc/1005106045"

> get_isbn_10_check_digit("0-124-91540-X", allow.hyphens=TRUE)
[1] "X"

> is_valid_isbn_10(c("012491540X", "9004037812"))
[1] TRUE FALSE

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

### Interfacing with the web
- `loc_permalink_from_lccn`
- `worldcat_permalink_from_issn`
- `worldcat_permalink_from_isbn`
- `worldcat_permalink_from_oclc_number`


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
- `books_and_serials_sample` -A very small sample of books, monographs, and
  serials and their information including title, control numbers, call numbers,
  and call number subject classifications. Mainly for testing.
  Will be expanded in future versions.

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



