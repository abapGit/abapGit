---
title: Development Guidelines
order: 10
---

### Practical

* For non-trivial changes make sure there is an open issue. When starting the development add a comment in the issue. This will allow for discussing possible solutions, and avoid having multiple people look into the same issue.

* Keep the commits/PR as small as possible, try not to fix multiple issues in one commit/PR if possible. Smaller changes are easier to review, and are more likely to be accepted.

* commit often, whenever something is working, and is a step in the right direction do a commit or PR. This way other contributors can see the changes, and it will minimize the risk of merge conflicts.

External Links:
[https://guides.github.com/activities/contributing-to-open-source/](https://guides.github.com/activities/contributing-to-open-source/)

### Conventions

#### Prefixing
Variables are prefixed using the standard setting in [abapOpenChecks Naming Conventions](http://docs.abapopenchecks.org/checks/69/)

#### Downport
abapGit is targeted for version 702, so the code should only contain expressions/statements that works on 702.
[abaplint](https://abaplint.org) will automatically check every PR for language syntax that is not available on 702.

#### Pretty Printer
Use pretty printer, keywords upper case + indentation, [abapOpenChecks](http://docs.abapopenchecks.org/checks/06/) can be used for checking this
