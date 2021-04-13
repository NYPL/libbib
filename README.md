libbib
===

![libbib logo](http://statethatiamin.com/media/libbibsmall.png)

[![Build Status](http://travis-ci.org/NYPL/libbib.svg?branch=master)](https://travis-ci.org/NYPL/libbib)

## Description
An R package providing functions for validating and normalizing
bibliographic codes such as ISBN, ISSN, LCCN, and OCLC.

Speed of execution and robustness are priorities in this package.
To the end of optimizing speed and efficiency, careful consideration
is taken to exploit vectorized functions in the code.

On a (real life) example of **3 million** very messy ISBN 10s and 13s,
aggressive ISBN normalization took  _less than 1 minute_ (commodity hardware).
It salvaged almost half a million previously unusable ISBNs, bringing
the number of usable ISBNs (for matching, cataloging, etc...) from
less than 90% to 99.6%!

_Note these timings will change for the better or worse pending_
  - _performance enhancements_
  - _recognition and handling procedures for other ways in which
   malformed ISBNs can be salvaged_


As for robustness, this package is well tested, with over 100
automated tests, at time of writing

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
  (https://www.loc.gov/marc/languages/language_code.html)
- `country_code_crosswalk`
  (https://www.loc.gov/marc/countries/countries_code.html)
- `lc_subject_classification`
  (https://www.loc.gov/catdir/cpso/lcco/)
- `lc_subject_subclassification`
  (https://www.loc.gov/catdir/cpso/lcco/)
- `dewey_subject_crosswalk`
  (https://www.oclc.org/content/dam/oclc/dewey/ddc23-summaries.pdf)
- `books_and_serials_sample`
  A very small sample of books, monographs, and serials and their
  information including title, control numbers, call numbers, and
  call number subject classifications. Mainly for testing.
  Will be expanded in future versions.


## sample examples

```
this
```

