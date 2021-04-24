
**If you are viewing this file on CRAN, please check
[latest news on GitHub](https://github.com/NYPL/libbib/blob/master/NEWS.md)
where the formatting is better.**

# libbib 1.5

## new features

1. Implemented Dewey Decimal Code to subject translation

2. Added various helpful utility functions

3. Added split_map_filter_reduce and related utilities

4. Added functions to get language and country names from
   corresponding Marc codes

5. Added function to get links to OCLC's classify service

6. Added ability to link directly  to MARCXML, MODS, MADS, or
   Dublin Core metadata formats in `loc_permalink_from_lccn`

7. Added functions to use the WorldCat Bib Read API,
   read/parse the returned MARCXml, and return a `data.table`
   containing the most pertinent information.

8. Added functions to use the WorldCat Classify API and return a
   `data.table` containing the most popular Dewey and LC call numbers.

9. Added functions to use the WorldCat Location API and return a
   `data.table` containing (optionally, all) of the holding libraries
   for that standard number.

10. Added functions to extract info from MARC leaders and 008 fields

11. Added functions to use the WorldCat SRU search API and return a
    `data.table` containing (optionally, all) of the results.

## bug fixes

1. Expanded lc call <-> subject crosswalk (mainly for 'Law')

2. Fixed bugs in lc call <-> subject translation mechanism

## improvements

1. performance of `is_valid_lc_call` improved

