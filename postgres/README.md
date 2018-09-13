# Initialization

We actually initialize our postgres database in the Dockerfile
directly, with everythng but actual data.

## Countries

We get the country codes from a small project,
[country_codes](https://github.com/qjhart/country_codes),
[commit](https://github.com/qjhart/country_codes/commit/c22d5021d08661fd7e83e7b76c96ec3c8359594c).
If these change you currently have to manually updated the
initdb.d/b_countries.sql file.
