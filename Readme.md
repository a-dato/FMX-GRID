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
* Components used in professional software (Lynx/Lynx-x) by a large user community
* Free and commercial versions available
* Commercial version adds model support and property binding (bind properties to editors for advanced two way syncing)

## Free version

A free version is available which is actually quite capable. The free version is named FMX-GRID. This version can be used by any number of developers and offers royalty free distribution of applications.

The free version is published under the MIT license

## Commercial version
FMX-GRID-MODEL is a commercial product. It adds model support and property binding for easy creation of user interfaces. A license is required per developer. Licenses are valid for 12 months, a subscription is required to keep licenses valid. Subscriptons must be renewed after the end date. If you are renewing your license before (or up to 30 days after) the current expiration date, the new expiration date will be calculated based on the original order date. Updates are only available to users with a valid license.

You may install the product on more than one machine as long as each developer has their own license. This allows you to install it on your work, home and laptop machines. 

Please visit our forum on https://community.a-dato.com for support, comments and questions.
## Installation
Full source code is available from: https://github.com/a-dato/FMX-GRID.git. 
Installation steps:
* Clone the git repository to a directory on your local machine.
* From Delphi, open project "ADato.Packages.Group.groupproj"
* Compile all packages
* Select all design packages, and select 'Install' from the popup menu
* Add the following directpries to your library path:
	fmx-grid\dn4d\Source
	fmx-grid\datamodel\Package
	fmx-grid\datamodel\DataModel
	fmx-grid\datamodel\General
	fmx-grid\datamodel\ObjectModel
	fmx-grid\grid_vcl\Css
	fmx-grid\grid_vcl\General
	fmx-grid\grid_vcl\Tree
	fmx-grid\grid_fmx\Scrollable
	fmx-grid\grid_fmx\Tree
	fmx-grid\grid_fmx\Package

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

