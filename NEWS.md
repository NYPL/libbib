
**If you are viewing this file on CRAN, please check
[latest news on GitHub](https://github.com/NYPL/libbib/blob/master/NEWS.md)
where the formatting is better.**

# libbib 1.7 (in development)

1. Nothing yet

# libbib 1.6.4

1. Bug fix in `dt_counts_and_percents` wherein if the group by column
   was not a character or a factor, the "TOTAL"", and "OTHER" rows would
   be NA.

2. Fixed a bug in `get_clean_names` (and, by extension, `dt_set_clean_names`)
   where "0" was accidentally excluded from the whitelisted character set.

# libbib 1.6.2

## new features

1. Functions `fread_plus_date`, `fwrite_plus_date`, `set_lb_attribute`,
   `set_lb_date`, and `cp_lb_attributes` to read/write files with
   special date attribute and work with date and other special
   attributes.

## bug fixes

1. LC call number functions now call str_trim before anything


# libbib 1.6

## new features

1. Wrote a translation layer to translate more human-readable aliases
   of every WorldCat SRU search index to their formal name. The
   `worldcat_api_search` function now uses this by default. You can still use
   the formal names, and even mix codes in the same query. The function
   documentation has been updated to reflect these new aliases.

2. A vignette on how to use the WorldCat search API is now available
   with a ton of very useful information.

3. Added `dt_na_breakdown` function.

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

5. `get_clean_names` and `dt_set_clean_names` will now never produce
   duplicate column names

6. `dt_keep_cols` will warn user if a supplied column name doesn't
   exist in the supplied data.table

7. `dt_add_to_col_names` will, by default, error if any of the new
   names create any duplicate column names (not setting the new
   names). If `TRUE`, all the column names are made unique,
   potentially renaming excluded column names that were not supposed
   to be changed.

8. Added option in `dt_counts_and_percents` (and `dt_na_breakdown`)
   to specify a `big.mark` which will be used to separate every
   three digits of the count. If `FALSE` (the default) the count
   will remain and integer.

## small breaking changes

1. `marc_008_get_info` no longer returns the original publication
   date by default

2. excluding "questionable dates" in `marc_008_get_info` just NAs
   character position 6="q", now

3. The `print.api.responses` parameter in the Classify API functions has
   been changed to `debug` to be more consistent with the other API functions.

4. `num_total` column in search results has been renamed the (more descriptive)
   `total_wc_results`

5. By default, `get_clean_names` and `dt_set_clean_names` will convert
   all upper case characters to lower case. This can be overidden with
   by setting the `lower` parameter (in both) to `FALSE`.

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

