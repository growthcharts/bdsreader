# bdsreader 0.6.0

* Adds GHA R-CMD-check for Ubuntu 18.04
* Tweaks package description

# bdsreader 0.5.0

* Switches on continuous integration
* Adds Github action `pkgdown`
* Adds Github action `R-CMD-check`
* Replaces `docs` folder by `gh-pages` branch

# bdsreader 0.4.0

* Set `bds_schema.json` as default!! (not all testfile yet work)
* Replaces the `jamestest` package by `jamesdemodata`
* Supports (testing of) `bds_schema_str.json` (old) and `bds_schema.json` (new)
* Adds a `file` argument to `write_bds()`
* Updates scripts to account for double schema

# bdsreader 0.3.1

* Removes the `appendLF = FALSE` argument from `message`

# bdsreader 0.3.0

* Adds new `bds_write()` function
* Sets tighter ranges in `bds_schema.json`
* Changes the default JSON validation schema to `bds_schema.json`
* Adds example json from `bdsreader` package

# bdsreader 0.2.0

* Adds item splitting code for two BDS fields (head lag, walking)
* Adds a check on the existing of proper attribute in `persondata()`
* Adds a `NEWS.md` file to track changes to the package

# bdsreader 0.1.0

* First working version
