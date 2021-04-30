
## TODOs

## immediately

- can we make `split_map_filter_reduce` more efficient?
- add warning to del cols and keep cols if vars doesn't exist?
  * take the code from `dt_add_to_col_names`
- check for duplicate names in `clean_names` after cleaning
  * add a suffix if it already exists earlier
- `dt_set_clean_names` has a bad doc title
- can `dt_percent_not_na` return counts like `dt_counts_and_percent`?
  * maybe in a nice table
  * maybe `dt_col_na_breakdown`
- make sure there are no duplicate column names when using
  `dt_add_to_col_names`
- can `dt_col_na_breakdown` and `dt_counts_and_percents` have a pretty
  options to format N with big mark, etc...?
- function to clip year/date before or after current year/date?



### near future

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

