
**If you are viewing this file on CRAN, please check
[latest news on GitHub](https://github.com/NYPL/libbib/blob/master/NEWS.md)
where the formatting is better.**

# libbib 1.7 (in development)

## TODOs
- Mention needing an API key in README (and how to get one)
- Explore manual firstsearch/expertsearch and parameters yielded
- Write searching vignette
  * stress SRU, not OpenSearch
  * mention needed API key (and how to get one)
  * explain, for example, frbrGrouping
  * include all those cool links
  * mention that some docs say sru. but it's srw.
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
- Take getting the FAST subjects (xpath) seriously
- Make WorldCat search not have to be Inf but above 100
- Make sure all good examples from dsads.R are in docs
- What about pub date for serials?
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
- Better handling on NAs in split_map...?
- Mention hard dependency on data.table, pbapply, and stringr in README
- Multijoin routine?
- Is pbapply linux only?
- Should I be using conditions/error-handling?
- HathiTrust
  * https://www.hathitrust.org/bib_api
  * https://www.hathitrust.org/hathifiles
- Communicate with Library of Congress SRU servers?
  * https://www.loc.gov/standards/sru/resources/lcServers.html
  * http://lx2.loc.gov:210/lcdb?version=1.1&operation=searchRetrieve&query=%22Marv%20Throneberry%22&startRecord=1&maximumRecords=5&recordSchema=marcxml

## new features

1. Wrote a translation layer to translate more human-readable aliases
   of every WorldCat SRU search index to their formal name. The
   `worldcat_api_search` function now uses this by default. You can still use
   the formal names, and even mix codes in the same query. The function
   documentation has been updated to reflect these new aliases.

2. A vignette on how to use the WorldCat search API is now available
   this a ton of very useful information.

## bug fixes
1. `print.progress` conditional now works for Location and Search API functions

2. Fixed a bug where if the number of search results was an exact multiple
   of 100, the search api function wouldn't return anything

## improvements

1. WorldCat API functions will now error if no WSKEY is provided
   (either with `options("libbib.wskey"=...)` or explicitly
   in the formal parameters

2. WorldCat Search function now tolerant of newlines and multiple
   spaces. Now long queries can be look neat

3. More of the diagnostic output now uses `message` instead of `print` or `cat`

4. If no Search API results are returned, the function prints the any
   diagnostic message returned from the Search API
   (things like Query syntax operators and unsupported indexes)

## small breaking changes

1. `marc_008_get_info` no longer returns the original publication
   date by default

2. excluding "questionable dates" in `marc_008_get_info` just NAs
   character position 6="q", now

3. The `print.api.responses` parameter in the Classify API functions has
   been changed to `debug` to be more consistent with the other API functions.

4. `num_total` column in search results has been renamed the (more descriptive)
   `total_wc_results`

-----

# libbib 1.5 (on CRAN 2021-04-24)

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

