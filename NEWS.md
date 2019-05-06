# Version 1.0.2 (2019-05-06)

* New version (patch) -- including several bug fixes.

# Version 1.0.1.9003 (2019-05-05)

* Bug fixes:
    - __knitr__ was moved from Imports to Suggests.
    - `observatio()`: checking if the number of observations with date is greater than the number of 
      observations without date.
    - `unit()`: checking if the value passed to an argument is valid.

# Version 1.0.1.9002 (2019-04-25)

* `#VALUE!` now is considered as `NA`. This can be found when a calculation is performed in one of the
  Google Sheets.

# Version 1.0.1.9001 (2019-03-09)

* `tr` now is considered as `NA`. This is frequently found in the _camada_ table.

# Version 1.0.1.9000 (2018-12-08)

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

# Version 1.0.1 (2018-11-11)

* New version (patch) -- including several bug fixes.

# Version 1.0.0.9009 (2018-11-11)

* Adjustments for new release.

# Version 1.0.0.9008 (2018-11-04)

* Bug fixes:
    - Corrects the arrangement of the columns of the table containing measurement units and fields names.
    - Corrects the stacking of measurement units and fields names.
    - Takes into account that both '<' and '< ' are used to indicate 'less than'.
    - `-` is used as a surrogate measurement unit for variables that are unitless.

# Version 1.0.0.9007 (2018-11-03)

* Bug fix: proceeds with standardization and harmonization only when the dataset has data on the selected 
  variables. This should produce a minor speed improvement.

# Version 1.0.0.9006 (2018-11-03)

* Bug fix: `-` is used as a surrogate measurement unit for variables that are unitless. This is necessary to
  perform data standardization.

# Version 1.0.0.9005 (2018-11-02)

* Bug fix: exports correct data type when combining categorical data values when there are repeated 
  measurements.

# Version 1.0.0.9004 (2018-11-02)

* Bug fix: stops execution of `layer` and `observation` when downloading a single dataset with `stack = TRUE`.

# Version 1.0.0.9003 (2018-11-02)

* Accommodates changes in ___febr___:
  - Field `camada_numero` now is `camada_id`.
  - Field `amostra_codigo` now is `amostra_id`.

# Version 1.0.0.9002 (2018-11-01)

* Bug fix: during the standardization of measurement units, now understands that measurements units are stored
  in the second line of tables `camada` and `observacao`.

# Version 1.0.0.9001 (2018-07-16)

* Bug fixes.
* Updates authorship information.
* Adds METACRAN package download badge (https://www.r-pkg.org/services).
* Adds custom package devel version badge (based on https://github.com/GuangchuangYu/badger).

# Version 1.0.0.9000 (2018-07-13)

* New string used to identify comments in data tables `camada` and `observacao`. Now using `#metadado>` -- as 
  a replacement of `#unidade`. This allows setting multiple lines with comments.
* Functions `layer` and `observation`: the measurement unit, `field_unit`, and true variable name, `field_name`,
  are exported as attributes of the output `data.frame` object.

# Version 1.0.0 (2017-03-09)

* Submission to CRAN.
