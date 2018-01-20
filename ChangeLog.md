# Revision history for config-parser

## 1.1.0.0  -- 2018-1-19

* Made parsing errors significantly more informative.
* The `keyValue` parser in `ConfigParser` now takes a `Parser Key`, rather than
  a `Key`, to facilitate easier detection of bad keys.
* Fixed some broken haddocks.

## 1.0.0.0  -- 2017-12-04

* Removed 'ConfigParser' constructor with janky call to 'error'. Non-unique keys
  will now cause parsing to fail, instead.
* Duplicate keys in config files will now cause parsing to fail.
* 'ConfigOption' now has a 'required' field to specify that omitting a
  particular key from a config file is an error. Omiting required keys causes
  parsing to fail.
* The 'boundedIntegral' parser is now correctly exported.

## 0.2.0.0  -- 2017-10-31

* First version. Released on an unsuspecting world.
