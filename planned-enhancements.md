
## TODOs

## immediately

- can we make `split_map_filter_reduce` more efficient?



### near future

- explain, for example, frbrGrouping in vignette
- Propagate `original.pub.date` in, for example, the search function
- Take getting the FAST subjects (xpath) seriously
- Make WorldCat search not have to be Inf but above 100
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
- function to clip year/date before or after current year/date?

