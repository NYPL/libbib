
**If you are viewing this file on CRAN, please check
[latest news on GitHub](https://github.com/NYPL/libbib/blob/master/NEWS.md)
where the formatting is better.**

# libbib 1.7 (in development)

## TODOs
- Explore manual firstsearch/expertsearch and parameters yielded
- Write searching vignette
  * stress SRU, not OpenSearch
  * explain, for example, frbrGrouping
  * include all those cool links
  * mention that some docs say sru. but it's srw.
- Put date of CRAN release in NEWS
- Can you put spaces between equals?
- Propagate `original.pub.date` in, for example, the search function
- Explore and test limits of every index available
  * include relations operators
  * Play around with any/exact/all
  * save it all for the vignette
- write `translate_sru_syntax` function
  * translates all indexes
  * auto detect if no 'srw\.'
  * maybe translate matereial type and number of holdings, too
  * write a lot of tests. beware of quotes in the regex
- What's going on with the two NYPL identifiers? Search in each
- Test search for 'finnegan's wake' (single apostrophe)
- Take getting the FAST subjects (xpath) seriously
- Make WorldCat search not have to be Inf but above 100
- Make sure all good examples from dsads.R are in docs
- What about pub date for serials?
- If no results are found, give the XML message
  * better handling of bad return/status codes
- Look in to the following repositories
  * xlcnd/isbnlib
    - uses google book and wiki apis?
    - ... blob/dev/isbnlib/\_goob.py
    - uses openlibrary api (for what?)
  * xlcnd/isbntools
  * xlcnd/isbnlib-loc
  * pwssnk/isbnlib-worldcat
- What's in the "Linked Data" section of a worldcat book?
  * associated "work" record?
- What's experiment.worldcat.org/oclc/1.jsonld ?
- What's ThingISBN (LibraryThing?)
  * https://wiki.librarything.com/index.php/LibraryThing_APIs
  * https://blog.librarything.com/thingology/category/apis/
  * https://www.librarything.com/api/thingISBN/9780984201006
- Look into FAST API
- Read my own documentation links again?
- LazyLoad data? Why?
- Better handling on NAs in split_map...?
- Mention hard dependency on data.table, pbapply, and stringr in README
- Multijoin routine?
- Is pbapply linux only?
- Should I be using `message`, or conditions/error-handling?

## new features

## bug fixes
1. `print.progress` conditional now works for Location and Search API functions

## improvements

1. WorldCat API functions will now error if no WSKEY is provided
   (either with `options("libbib.wskey"=...)` or explicitly
   in the formal parameters

2. WorldCat Search function now tolerant of newlines and multiple
   spaces. Now long queries can be look neat

3. More of the diagnostic output now uses `message` instead of `print` or `cat`

## small breaking changes

1. `marc_008_get_info` no longer returns the original publication
   date by default

2. excluding "questionable dates" in `marc_008_get_info` just NAs
   character position 6="q", now

3. The `print.api.responses` parameter in the Classify API functions has
   been changed to `debug` to be more consistent with the other API functions.

-----

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

