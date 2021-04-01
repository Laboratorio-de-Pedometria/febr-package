# febr 1.6.3

* `morphology`: deals with depth data between parentesis when retrieving soil color data.

# febr 1.6.2

* `morphology`: deal with varying number of soil color records (1 or 2)
* Documentation updates.

# febr 1.6.1

* `morphology`: retrieve soil color data when accents are missing ("umido" or "umida")

# febr 1.6.0

* `febr2sse`: updates nomenclature (SmartSolos Expert and sse); process soil consistency data
* `morphology`: retrieve soil consistency data

# febr 1.5.7

* Patch: guarantee that `rmarkdown` is up to date to build vignettes.

# febr 1.5.6

* Cleans code and documentation.

# febr 1.5.5

* Sets a new file to store the ID of the datasets downloaded using the core function `readFEBR()`.

# febr 1.5.4

* New function `dictionary()`
  * Renames `standard()`
  * Argument `table` has two new valid values: `metadado` and `versionamento`.

# febr 1.5.3

* Code formatting and clean up.
* Improvements on how unitless variables are dealt with.

# febr 1.5.2

* Code formatting and clean up.
* Improves documentation.
* Adds NA option (#N/A).
* Corrects bug in unit conversion.

# febr 1.5.1

* Improves documentation.

# febr 1.5.0

* New function:
  * `taxonomy()`. Extract and process soil taxonomic data from textual soil classification description. A vignette accompanies the new function.
* Bug fix:
  * `observation()`. Corrects an issue in argument checking.
  * Fixes invalid URLs.

# febr 1.4.0

* New function `morphology` to extract and process soil morphological properties from field soil morphology descriptions.
* Adds data conversion routines in `febr2smartsolos` for the following soil variables: taxonomic
  classification, soil matrix color (wet and dry), soil structure.

# febr 1.3.2

* Improves translation of variable names when converting soil profile data between FEBR and SMARTSolos.

# febr 1.3.1

* Deals with suggested packages.

# febr 1.3.0

* New functions `febr2smartsolos()` and `smartsolos2febr()` for soil profile data conversion between FEBR and
  SMARTSolos.

# febr 1.2.4

* Improves documentation for new release.

# febr 1.2.3

* Improves documentation for new release.

# febr 1.2.2

* `readFEBR()`: improves output object.

# febr 1.2.1

* Improves package documentation.
* `readFEBR()`: new general purpose function to download raw data.

# febr 1.1.2

* Bug fix: correct file format i.e. TXT instead of CSV.

# febr 1.1.1

* Improvements:
  + Includes changes to download data from new webserver
  + New function argument `febr.repo` allows user to set where the data should be read from: the remote
    web server or a local directory
* Other changes:
  + `header()` is now deprecated due to the new file structure used in __FEBR__. Users can now rely on the 
    already existing `metadata()` to get the same results
  + `febr2spdf()` is now defunct.

# febr 1.1.0

* Improvements:
  + Packages __googlesheets__, __googlesheets4__ and __readr__ are not dependencies anymore;
  + A new function `febr2sf` is available as a replacement for `febr2sp` -- __sp__ is not dependencies anymore;
  + `febr2xlsx` is now deprecated -- __xlsx__ is not dependencies anymore;
* Bug fix:
  + `layer`: standard variables are only set to type character when `stack = TRUE`.

# febr 1.0.3.9004

* Getting rid of __googlesheets4__.

# febr 1.0.3.9003

* Using `Sys.sleep(time = 10)` to pass checks in functions examples. This is necessary because the new Google
  Sheets API has a limit of 500 requests per 100 seconds per project, and 100 requests per 100 seconds per
  user (https://developers.google.com/sheets/api/limits). This issue has also been found elsewhere, e.g.
  https://stackoverflow.com/questions/53765222/python-google-sheets-api-limit-429-error-with-loop. We soon will
  remove the dependency upon __googlesheets4__ and thus solve this issue.
* Preparing for removal of __googlesheets4__ from the list of imports.

# febr 1.0.3.9002

* Improvements:
  + New function: `febr2sf` as a replacement for `febr2sp`;
  + Replaces __googlesheets__ with `.readGoogleSheets`;
  + `febr2xlsx` is now deprecated and __febr__ does not depend upon __xlsx__ anymore;
  + Improves documentation;
* Bug fixes:
  + `layer`: standard variables are only set to type character when `stack = TRUE`;

# febr 1.0.3.9001

* Improvements:
  + Replaces __sp__ with __sf__;
  + Exports new data reading function: `.readGoogleSheets`;
  + Improves documentation;
* Bug fixes:
  + `metadata`: get correct file id.

# febr 1.0.3.9000

* Replacing __googlesheets__ with __googlesheets4__.

# febr 1.0.3

* New version (patch) including internal changes in download functions and documentation improvements.

# febr 1.0.2.9001

* Preparing for future modifications in data storage in ___febr___. Now the sheet name (`dataset`, 
  `observacao`, `camada` or `metadado`) is explicitly specified withing download functions. The user should not
  have to worry about this change.

# febr 1.0.2.9000

* Bug fix: setting argument `missing = list(coord = 'drop')` in function `observation()` now correctly drops 
  observations missing spatial coordinates. Thanks to Edberto Moura Lima, from the Federal University of Santa
  Maria, who identified the bug. Equivalent modifications were implemented in function `layer()`.

# febr 1.0.2

* New version (patch) -- including several bug fixes.

# febr 1.0.1.9003

* Bug fixes:
    - __knitr__ was moved from Imports to Suggests.
    - `observatio()`: checking if the number of observations with date is greater than the number of 
      observations without date.
    - `unit()`: checking if the value passed to an argument is valid.

# febr 1.0.1.9002

* `#VALUE!` now is considered as `NA`. This can be found when a calculation is performed in one of the
  Google Sheets.

# febr 1.0.1.9001

* `tr` now is considered as `NA`. This is frequently found in the _camada_ table.

# febr 1.0.1.9000

* Bug fixes:
    - Many layers from datasets that have not gone through revision yet are lacking the identification code
      `camada_id`, which is automatically set to `NA_character_`. In these cases, when merging repeated layer 
      data using `layer(standardization = list(repetition = "combine"))`, the entire observation would be 
      merged. This was because for all layers `camada_id = NA_character_`, that is, `camada_id` was duplicated.
      Now duplicates are checked using `duplicated(incomparables = NA)` to avoid incorrect merges.
    - The lowermost limit of some layers is recorded as being a wavy or irregular layer trasitions. This is 
      done using `/`. The depth data of these layes may include a plus sign indicating that the same material 
      can be found deeper in the soil. This poses a difficulty when processing the plus sign with `eval()` and
      `parse()` because `/` is interpreted as a division sign. Now the presence of `/` is checked before 
      deciding how to deal with the plus sign.

# febr 1.0.1

* New version (patch) -- including several bug fixes.

# febr 1.0.0.9009

* Adjustments for new release.

# febr 1.0.0.9008

* Bug fixes:
    - Corrects the arrangement of the columns of the table containing measurement units and fields names.
    - Corrects the stacking of measurement units and fields names.
    - Takes into account that both '<' and '< ' are used to indicate 'less than'.
    - `-` is used as a surrogate measurement unit for variables that are unitless.

# febr 1.0.0.9007

* Bug fix: proceeds with standardization and harmonization only when the dataset has data on the selected 
  variables. This should produce a minor speed improvement.

# febr 1.0.0.9006

* Bug fix: `-` is used as a surrogate measurement unit for variables that are unitless. This is necessary to
  perform data standardization.

# febr 1.0.0.9005

* Bug fix: exports correct data type when combining categorical data values when there are repeated 
  measurements.

# febr 1.0.0.9004

* Bug fix: stops execution of `layer` and `observation` when downloading a single dataset with `stack = TRUE`.

# febr 1.0.0.9003

* Accommodates changes in ___febr___:
  - Field `camada_numero` now is `camada_id`.
  - Field `amostra_codigo` now is `amostra_id`.

# febr 1.0.0.9002

* Bug fix: during the standardization of measurement units, now understands that measurements units are stored
  in the second line of tables `camada` and `observacao`.

# febr 1.0.0.9001

* Bug fixes.
* Updates authorship information.
* Adds METACRAN package download badge (https://www.r-pkg.org/services).
* Adds custom package devel version badge (based on https://github.com/GuangchuangYu/badger).

# febr 1.0.0.9000

* New string used to identify comments in data tables `camada` and `observacao`. Now using `#metadado>` -- as 
  a replacement of `#unidade`. This allows setting multiple lines with comments.
* Functions `layer` and `observation`: the measurement unit, `field_unit`, and true variable name, `field_name`,
  are exported as attributes of the output `data.frame` object.

# febr 1.0.0

* Submission to CRAN.
