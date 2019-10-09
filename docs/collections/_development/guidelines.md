---
title: Development Guidelines
order: 10
---

### Conventions

#### Prefixing
Variables are prefixed using the standard setting in [abapOpenChecks Naming Conventions](http://docs.abapopenchecks.org/checks/69/)

#### Downport
abapGit is targeted for version 702, so the code should only contain expressions/statements that works on 702.
[abaplint](https://abaplint.org) will automatically check every PR for language syntax that is not available on 702.

#### Pretty Printer
Use pretty printer, keywords upper case + indentation, [abapOpenChecks](http://docs.abapopenchecks.org/checks/06/) can be used for checking this
