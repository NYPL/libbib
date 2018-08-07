## Test environments
* local macOS Sierra 10.12 install, R 3.5
* ubuntu (on travis-ci), R 3.5
* win-builder (devel and release)

## R CMD check results

When checked locally with --as-cran and --no-manual
there were no ERRORs or WARNINGs but 4 NOTES related
to the known issue of packages using data.table
getting "no visible binding for global variable..."
