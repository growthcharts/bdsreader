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
