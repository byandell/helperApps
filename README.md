# helperApps

## Modular Shiny Helper Apps for Reuse

See philosophy in
[Shiny Apps](https://github.com/AttieLab-Systems-Genetics/Documentation/blob/main/ShinyApps.md).

This repo is organized as a minimal package with a few utilities.
However, its main purpose is to illustrate helpful shiny app modules I have
developed over the years to serve modular functions, such as
data input, download of plots and/or tables and password entry.
These might be thought of as pragmatic wrappers that might server a
generic role across apps, or might be a starting point for 
a specialized module in a large app.

The following apps are with the package for legacy reasons. They are included
in the [Projects](Projects/) folder as well.

- [`downloadButtonApp()`](R/downloadButtonApp.R): download App with Buttons
- [`downloadApp()`](R/downloadApp.R): download App with separate PNG & PDF downloads
- [`mergeApp()`](R/mergeApp.R): merge App to combine `download_list`s

These apps are in process of being moved to the Projects described below.
These have (planned)
implemented versions of the helper apps. In the `inst/Projects` folder
are folders for each project. I expect this to grow over time.
For instance, there are separate implementations of `downloadApp()`
in the three projects, developed at different times for slightly different
UI/UX.

- inst
  - Projects
    - [orphaned](inst/Projects/orphaned) (orphaned ideas not tied to particular app)
    - [geyser](inst/Projects/geyser) (see [geyser](https://github.com/byandell/geyser))
    - [qtlApp](inst/Projects/qtlApp) (see [qtlApp](https://github.com/AttieLab-Systems-Genetics/qtlApp))
    - [foundrShiny](inst/Projects/foundrShiny) (see [foundrShiny](https://github.com/AttieLab-Systems-Genetics/foundrShiny))
    - [RPAshiny](inst/Projects/RPAshiny) (see [RPA_Shiny](https://github.com/byandell-envsys/RPA_Shiny)) [private]
    - [qtl2shiny](inst/Projects/qtl2shiny) (see [qtl2shiny](https://github.com/AttieLab-Systems-Genetics/qtl2shiny)) [none yet]

## Modular Apps by Function

Modular apps have an app, a server, and one or more UI functions.
The app is designed to work on its own to illustrate the utility of
the module. One or more of the UI functions may be designed to
illustrate use while other UI functions are specifically for use
in connecting this module to other modules.

Functions can be quite compact, but shiny modules require several
functions, and are most useful when at least sparsely documented.
Thus each module ends up being 50-100 lines, or in some cases over 200 lines.

### Importing Data

Most apps require importing data to function effectively.
Prototype apps in the [Shiny Gallery](https://shiny.posit.co/r/gallery/)
typically use internal R datasets.
My [geyser](https://github.com/byandell/geyser)
mines the internal R data using a simple
[dataApp](inst/Projects/geyser/dataApp.R)
for selected one of two datasets,
or a more complicated
[datasetsApp](inst/Projects/geyser/datasetsApp.R)
to select among R datasets that are matrices or dataframes with
at least two numeric columns.

I have a couple versions of
`importApp`
([qtlApp/importApp.R](inst/Projects/qtlApp/importApp.R)
and
[RPAshiny/importApp.R](inst/Projects/RPAshiny/importApp.R)).
Both rely on a flat (`CSV`) file that has (at least) two columns,
`object` and `filename`.
The `object` is the name to be used for the object,
while `filename` is the absolute or relative address and name
of the file containing the object.
The prototypes are set up to read `CSV`, `XLSX` and `RDS`.
The latter version also allows for `SHP` shapefiles
for geospatial data.

Typically, this does a minimal import
of each `object` using the `filename`,
relying on a function such as
[import_data()](R/import_data.R).
It may be useful to perform more tidying, with a more complicated function
such as 
[qtlApp/R/import_data.R](https://github.com/AttieLab-Systems-Genetics/qtlApp/blob/main/R/import_data.R).
Here several objects (`file_directory`, `chr_breaks`, `annotation_list`, `markers`)
are imported and massaged.

Other parts of a larger app may use these objects as a way to access
a larger body of data.
For instance, the `file_directory` object for `qtlApp` shows where
thousands of data files can be found to upload a
`selected_trait` into and display with the app.

These can get rather specialized for a complicated app such as
`qtlApp` or `qtl2shiny`.
Data are often stored in a `serial` format for quick access.
It may be native R data ([`RDS`](https://www.geeksforgeeks.org/data-serialization-rds-using-r/)),
fast serial data frame ([`FST`](https://www.fstpackage.org/))
or `SQLite` (see
[RSQLite](https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html)
and/or
[dbplyr](https://dbplyr.tidyverse.org/articles/dbplyr.html)).
Code for such data is currently spread throughout these apps.

The `qtl2shiny` package, designed over a decade ago,
does not use this `import` style.
Instead, it has
[read_project()](https://github.com/byandell-sysgen/qtl2shiny/blob/master/R/read_project.R)
to read in a flat file
and from there read in datasets.
This will take more work to explain and unravel,
as the data are in a legacy format.

### Downloading Plots and Tables

The `foundrShiny` package went through an evolution of how to download
plots and tables, finally settling on a
[downloadApp](inst/Projects/foundrShiny/downloadApp.R)
structure that relies on a list of information.
This was modified recently for `qtlApp` package
[downloadApp](inst/Projects/qtlApp/downloadApp.R).

There are several differences.
The `foundrShiny` version
downloads all plots but only one table, and has plot `height`
embedded in the panel where the plot is developed.
The `qtlApp` version
downloads one plot (or one table) and uses a set of preset
plot shapes in the `download`.

Both are meant to reduce code to one set of code for download.
There are style choices as indicated above,
that have to do both with coding style and UI presentation.

### Information About an App

It was helpful to develop and `aboutApp` for the
`foundrShiny` package. This might be useful in other situations.
Part of this includes broad information about founder data,
with options for a particular app instance to add additional information.
For more information,
see
[foundr](https://github.com/byandell-sysgen/foundr)
and the several apps developed from this package.

Of particular note is the use of an option markdown file `help.md`.
This means app developer can write user information in an easy language.
This idea was used in a slightly different way in 
[RPA_Shiny](https://github.com/byandell-envsys/RPA_Shiny).

### Password Entry

Passwords were optionally used with
[foundrShiny](https://github.com/byandell-sysgen/foundrShiny)
with the 
[entryApp](inst/Projects/foundrShiny/entryApp.R).
This enables developer to put an app with protected data
on a public site and only give access to specified users.
This is NOT to be recommended for protected (HIPAA, etc.) data
but may be useful for some collaborations.

