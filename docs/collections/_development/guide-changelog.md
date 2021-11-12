---
title: Changelog
order: 150
---

abapGit allows to show a changelog notes, based on changelog file in the repository base, and on [APACK](ref-apack.html) class containing the current version.

abapGit itself does not use APACK and gets the current version from zif_abapgit_version=>c_abap_version. The version is then used to determine if the changelog was updated and should be shown to the user.

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
* Date in format YYYY-MM-DD followed by [Semantic version](https://semver.org/)
* Separator: ------------------
* Version notes using a prefix as described in the legend

```
2020-06-29 v1.97.0
------------------
+ Notes.....
```

Check [abapGit changelog](https://github.com/abapGit/abapGit/blob/main/changelog.txt) as an example.
