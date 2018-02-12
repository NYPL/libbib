libbib
===

![libbib logo](http://statethatiamin.onlythisrose.com/libbibsmall.png)

## Description
An R package providing functions for validating and normalizing
bibliographic codes such as ISBN, ISSN, LCCN, and OCLC.

Speed of execution and robustness are priorities in this package.
To the end of optimizing speed and efficiency, careful consideration
is taken to exploit vectorized functions in the code.

On a (real life) example of **4 million** very messy ISBN 10s and 13s,
aggressive ISBN normalization took _3.5 minutes_ (commodity hardware).
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
- `check_issn_check_digit`
- `get_issn_check_digit`

---

## THIS IS A STUB. IN EARLY ACTIVE DEVELOPMENT
