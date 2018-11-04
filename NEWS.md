# Version 1.0.0.9008 (2018-11-04)

* Bug fixes:
    - Corrects the arrangement of the columns of the table containing measurement units and fields names.
    - Corrects the stacking of measurement units and fields names.
    - Takes into account that both '<' and '< ' are used to indicate 'less than'.
    - `-` is used as a surrogate measurement unit for variables that are unitless.
* Improvement:
    - Includes temporary code to correct values outside of physical range

# Version 1.0.0.9007 (2018-11-03)

* Bux fix: proceeds with standardization and harmonization only when the dataset has data on the selected 
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

* Acomodates changes in ___febr___:
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
