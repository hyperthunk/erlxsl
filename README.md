# ErlXSL - XSLT Bindings for Erlang

The aim of this project is to provide a usable binding for Erlang to call native XSLT processors. The implementation aims to provide a choice between running the native code as an external port program or a linked-in driver. We also hope to provide an API allowing for a choice of XSLT processor providers.  

## Status

This project is pre-alpha at the moment. There are marshalling issues with R14, so R13 is the target erts version at the moment. An initial spike has also been built with very basic functionality, which works on 32bit systems running R12B. 

## Roadmap

This is the current plan - it is subject to change at any time and no dates are provided.

- Initial Prototype (alpha)
	- Run as linked-in driver
	- Support for one native XSLT provider (probably Xalan-C)
- First Beta
	- Run as linked-in driver or port program
	- Support for passing parameters
	- Stress testing for high number of concurrent transformations
- Second Beta
	- Support for plugging in alternative XSLT providers (probably Sablotron which was the first provider we tried)
	- Support for caching stylesheets/transforms
- First Release Candidate
	- A lot more stress testing
	- Static Analysis (SPLint, Frama-C, Blast, etc)

Other long term features are of interest to the developers, including streaming transformations, transformation pipelines and the like.   

## Issue Tracking

Please register issues against the [Repository Issue Tracker](https://github.com/hyperthunk/erlxsl/issues).

## License

This project is distributed under a BSD-style license (please see the accompanying LICENSE document for details).

## Versioning

This project uses [Semantic Versioning](http://semver.org). All major and minor versions will be tagged for release. Release candidates (i.e., revision builds) might be tagged. 

## Installation

See the accompanying INSTALL file.
