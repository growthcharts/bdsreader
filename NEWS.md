# bdsreader 0.30.0

- Update to R 4.5.0
- Install `dscore` from CRAN instead of GitHub
- Require R 4.1.0 or higher
- Update example data to the latest version of `jamesdemodata`

# bdsreader 0.29.0

- The `read_bds()` function return an object of class `bdsreader`
- Implement validation of the `bdsreader` class at the end of `read_bds()`
- Adds `print` function for `bdsreader` class
- Transfers datasets from `inst/examples` to the `jamesdemodata` package, but keep the folder for backward compatibility

# bdsreader 0.28.0

- Solves a bug in the de-duplication of parental information (#10)
- Extend tests with a more general JSON schema with duplicated BDS fields for parents

# bdsreader 0.27.0

- Uses update dscore 1.9.0 package
- Updates some hard-coded values in tests of the D-score
- Applies the bug fix in 0.26.0 only to BDS 63

# bdsreader 0.26.0

- Repairs a bug that prevented reading data if two mothers or two fathers were listed, e.g., from the same-sex couples

# bdsreader 0.25.0

- Change definition and processing code for BDS 71 (birth land parents) to character (#9)

# bdsreader 0.24.2

- Adds two demo files: `examples/Laura_S.json` and `examples/Kevin_S.json`

# bdsreader 0.24.1

- Changes schema `inst/schema/bds_v3.0.json` to point to master branch
- Replace non-functional `inst/examples/maria.json` by an older version

# bdsreader 0.24.0

## Non-breaking change in the V3.0 schema definition

- Changed the type definition of BDS field 71 (parental birth country) from `integer` to `number`

## Added support in V3.0 schema definition

- Adds support in `read_bds()` and `write_bds()` for
  + parental country of birth (BDS 71, uses government Table 34 for codes)
  + parental level of eduction (BDS 62/66)
  + parity (BDS 471)
  + 4-digit postal code (BDS 16)

## Other changes

- Removes the empty array writing from `write_bds()`
- Adds a developer vignette outlining the steps needed to add a BDS number
- Renames source file names for improved consistency
- Access `dplyr::case_match()` by requiring `dplyr 1.1.0`
- Correct documentation errors

# bdsreader 0.23.0

- Major overhaul of `bdsreader` package

## Major changes

- Reorganises calculations in function `read_bds()` into 13 steps
- The new argument `validate` to `read_bds()` can bypass the 
`jsonvalidate::json_validate()` to speed up data reading. The default is 
`FALSE`. Use `validate = TRUE` to obtain diagnostic information.
- Function `read_bds()` now writes intermediate JSON file to the working 
directory by setting `intermediate = TRUE`.
- Much faster reading of DDI BDS fields by smarter data mungling
- Prepares for deprecation of JSON schema v1.0 and v2.0 by splitting 
processing in `read_bds()` according to major version number
- Translates communication into the English language

## Minor changes

- Updates all tests according to new functionality
- Retires the `verify()` function and its tests
- Cleans up code by `lintr`

# bdsreader 0.22.0

- Updates example code, tests and documentation to the JSON V3.0 schema

# bdsreader 0.21.0

- Solves problem with `write_bds()` that failed to save gestational age for lollypop data
- Tweaks reading logic so that lollypop data are read
- Redefines 'reference' instead of 'Reference' in V3.0 schema
- Splits `write_bds()` in subfunctions per major version
- Extends tests for `read_bds()` to cover three major versions

# bdsreader 0.20.2

- Updates `write_bds()` so that it conforms to JSON BDS schema V3.0

# bdsreader 0.20.1

- Prevent upstairs assignments within the `switch()` statement

# bdsreader 0.20.0

- Makes the reading of the data more lenient so that `name` and `src` are 
properly read for V1.0-V3.0 data
- Repairs V3.0 examples so that these conform to V3.0 schemas

# bdsreader 0.19.0

## Bug fixes

- Repairs errors in selected bds fields in V2.0 and V3.0 schemas:
  + 66 (added code "09")
  + 911 --> 912
  + 944 --> 945
  + 960 --> 961
  + 965 --> 966
  + 942 --> 943
  + 944 --> 945
  + 973 (added)
  + 1001 (added)
 - Makes `parse_valid()` robust against BDS sequence 
 - Changes default format in `write_bds()` to JSON schema V3.0
 - Updates dependency versions

# bdsreader 0.18.0

## Major changes

- Defines **BDS JSON schema V3.0** for exchanging child-level data between electronic systems. V3.0 uses an English data definition and redefines the nesting structure to a more logical structure. The new schema mostly follows alterations developed by Eaglescience BV for the SRM made in the period 2022-2023. 
- From now on, the advice is to adopt V3.0 for data exchange. Versions V1.0, V1.1 and V2.0 will be retired in the future.
- Adds support for version V3.0 to functions `read_bds()` and `write_bds()`
- Changes the key for calculating the D-score to `dscore(..., key = "gsed2212")`
- Tests new functionality on 74 test files stored in package [jamesdemodata](https://github.com/growthcharts/jamesdemodata)

## Minor changes

- Removes deprecated tidyselect `.data$var` syntax
- Refreshes GH action scripts
- Adds script to simulate user bugs and requests

# bdsreader 0.17.0

- Adds example data `maria` and `examples/maria.json`
- Repairs an error with the coding of "smoking during pregnancy"
- Update documentation

# bdsreader 0.16.1

- Removes class attribute `target`, so now the target data is just a list with elements named `"psn"` and `"xyz"`. 

# bdsreader 0.16.0

- Breaking change: Replace return value of `read_bds()` from "tibble with attribute" to an object of class `target`. This will provide a complete export of the data when converted into JSON format.

# bdsreader 0.15.0

- Do not use dot parameters anymore for `jsonlite::FROMjson()`, which errors on alien argument names

# bdsreader 0.14.0

- Make the `Format` field a required element in schema `bds_2.0.json`

# bdsreader 0.13.0

- Solves a bug that prevented `read_bds(txt)` to read data from a URL

# bdsreader 0.12.0

- Adds a small dataset `minidata` for demo purposes
- Adds function `export_as_bds()` that converts donordata into JSON files

# bdsreader 0.11.0

- Repairs a problem with the validation of JSON string input (#4)
- Updates the test files to more recent `jsonvalidate` output
- Hacks away parts of `parse_valid()` that does not work under format 2.0

# bdsreader 0.10.3

- Specify dependency on `jsonvalidate 1.3.2` 

# bdsreader 0.10.2

- Sets return value of `set_schema()$schema` to the long file name including path

# bdsreader 0.10.1

- Repairs syntax errors in schema `bds_v2.0.json` as detected by `jsonvalidate::json_validate()`
- Adjust test file to changes in messages generated by `jsonvalidate::json_validate()`

# bdsreader 0.10.0 

- Solves a bug that prevented reading DDI under format = "2.0"

# bdsreader 0.9.2

- Solves various runtime bugs

# bdsreader 0.9.1

- Solves a problem with empty names in `bds_write()`

# bdsreader 0.9.0

- Implements `auto_format` that stores the format in the data file (#2)

# bdsreader 0.8.0

- Use `jamesdemodata` package for testing and demo's
- Removes some example files from `inst/examples`
- Adds examples `maria1.json` and `maria2.json`
- Updates README

# bdsreader 0.7.0

This is a major update that introduces schema versioning for JAMES. Schema versioning allows input data to be formatted according to one of multiple JSON-schemas.

- Introduces schema versioning;
- Adds multi-version support to `read_bds()`, `write_bds()` and related functions;
- Makes `format` the most prominent user-facing argument. Alternatively, the user can also specify the schema file directly using the `schema` argument;
- Shortens names of schema files to `bds_v{x.y}.json`;
- Renamee existing schemas as follows: 
  * `bds_schema_str.json` --> `bds_v1.0.json` (format = 1)
  * `bds_schema.json` --> `bds_v1.1.json`
  * `bds_schema_V2.json` -- `bds_v2.0.json` (format = 2)
- For clarity, renames the path from `inst/json` to `inst/schemas`;
- Updates the `$id` field in the schema's to their permanent locations.

The default is `format = 2L`, which is incompatible with early users who code their data with `bds_schema_str.json`. To read/write with the older format, call `read_bds(..., format = 1L)` or `write_bds(..., format = 1L)`. The schema argument is primarily meant for development and testing.

# bdsreader 0.6.0

* Adds GHA R-CMD-check for Ubuntu 18.04
* Tweaks package description

# bdsreader 0.5.0

* Switches on continuous integration
* Adds Github action `pkgdown`
* Adds Github action `R-CMD-check`
* Replaces `docs` folder by `gh-pages` branch

# bdsreader 0.4.0

* Set `bds_schema_v1.1.json` as default!! (not all testfile yet work)
* Replaces the `jamestest` package by `jamesdemodata`
* Supports (testing of) `bds_schema_v1.0.json` (old) and `bds_schema_v1.1.json` (new)
* Adds a `file` argument to `write_bds()`
* Updates scripts to account for double schema

# bdsreader 0.3.1

* Removes the `appendLF = FALSE` argument from `message`

# bdsreader 0.3.0

* Adds new `bds_write()` function
* Sets tighter ranges in `bds_schema_v1.1.json`
* Changes the default JSON validation schema to `bds_schema_v1.1.json`
* Adds example json from `bdsreader` package

# bdsreader 0.2.0

* Adds item splitting code for two BDS fields (head lag, walking)
* Adds a check on the existing of proper attribute in `persondata()`
* Adds a `NEWS.md` file to track changes to the package

# bdsreader 0.1.0

* First working version
