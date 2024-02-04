# FMX-GRID
Welcome to this new release of the the A-dato FMX-Grid component.

## Features
FMX-Grid provides the following features:
* Available on all platforms
* Connects to different data sources (Lists, ObjectModel, datatset's)
* Smart row caching results in low memory usage
* Fast scrolling
* Extendable property model based on .Net type system (object properties can be modified at runtime)
* Development backed up by a professional team of FMX developers
* Components used in professional software (Lynx/Lynx-x) in a large user community

## License
One software license is required per developer.
Full source code is included, software may be used during development without a license. A valid license is required when this software is part of a application distributed/sold/made available to customers/users outside your own organization. If you have a valid license, software may be distributed royaly-free. Licenses are valid for 12 months, a subscription is required to keep licenses valid. Subscriptons must be renewed after the end date. If you are renewing your license before (or up to 30 days after) the current expiration date, the new expiration date will be calculated based on the original order date.

You may install the product on more than one machine as long as each developer has their own license. This allows you to install it on your work, home and laptop machines. Run-time royalty free.

Support is only provided to developers with a valid license.
## Installation
Full source code is available from: https://github.com/a-dato/FMX-GRID.git. 
Installation steps:
* Clone the git repository to a directory on your local machine.
* From Delphi, open project "ADato.Packages.Group.groupproj"
* Compile all packages
* Select all design packages, and select 'Install' from the popup menu
* Add the following directpries to your library path:
	adato_grid\adato_datamodel\DataModel
	adato_grid\adato_datamodel\General
	adato_grid\adato_datamodel\ObjectModel
	adato_grid\adato_grid_vcl\Css
	adato_grid\adato_grid_vcl\General
	adato_grid\adato_grid_vcl\Tree
	adato_grid\adato_grid_fmx\Scrollable
	adato_grid\adato_grid_fmx\Tree
	adato_grid\adato_grid_fmx\Package

## Demos
This software comes with sample applications. These applications are available inside directory 'Samples'.
* DB Inspector, easy to use, fast database editor application
* Datasources, this demo shows how data can be fed into the FMX grid
* MultiColumn, this demo shows how a multi column grid can be setup

## IMPORTANT NOTE

Each version of Delphi supports different properties. It can therefore happen that
when running or opening a file from the Lynx demo application an exception is
raised saying that a certain property cannot be read because this property does
not exist.

To overcome this problem, open each file in your development environment,
apply a small change to the form (for example: update the position of a component)
and save your changes. This will remove any conflicting property from the source code.


****************

Read Release.txt to get information about the latest changes made to this release.

