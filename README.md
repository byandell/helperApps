# helperApps

## Modular Shiny Helper Apps for Reuse

See philosophy in
[Shiny Apps](https://github.com/AttieLab-Systems-Genetics/Documentation/blob/main/ShinyApps.md).

This repo is organized as a package, 

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
    - [orphaned](inst/Projects/orphaned) (orphaned ideas)
    - [qtlApp](inst/Projects/qtlApp) (see [qtlApp](https://github.com/AttieLab-Systems-Genetics/qtlApp))
    - [foundrShiny](inst/Projects/foundrShiny) (see [foundrShiny](https://github.com/AttieLab-Systems-Genetics/foundrShiny))
    