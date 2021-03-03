---
title: Changelog
order: 150
---

abapGit allows to show a changelog notes, based on changelog file in the repository base

## File format ##
A file named as pattern `CHANGELOG*` or `changelog*`

Changelog file should have a specific format:

Header with the title
```
abapGit changelog
=================
```

A legend
```
Legend
------
* : fixed
! : changed
+ : added
- : removed
```

Notes section using header format:
* Date in format YYYY-MM-DD
* [Semantic version](https://semver.org/)
* Separator: ------------------
* Version notes
```
2020-06-29 v1.97.0
------------------
+ Notes.....
```

Check [abapGit changelog](https://github.com/abapGit/abapGit/blob/main/changelog.txt) as an example
