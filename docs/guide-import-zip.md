---
title: Import ZIP
category: offline projects
order: 10
---

## Download ZIP File ## 
An offline project is based on a ZIP file that contains all files of a git repository.

First, download the latest version of the project you want to import. 

* For GitHub :
   1. Go to the project main page (e.g. https://github.com/larshp/abapOpenChecks)
   2. Select **Code** > **Download ZIP**
   3. Save file

## Import ZIP File ##
1. Connect to SAP in language English (**EN**)
2. Run abapGit:
  * If you have installed the standalone version: in transaction `SE38`, run the program you created  (`ZABAPGIT_STANDALONE` or `ZABAPGIT_FULL` or other)
  * If you have installed the developer version: run transaction `ZABAPGIT`
3. Select **New Offline**
4. Enter the project name and select an existing package or create a new package
5. Select **Import <sup>zip</sup>**
6. Select the ZIP file (eg. *abapGit-master.zip*)
7. Select **Pull <sup>zip</sup>**

## Hints ##
abapGit can only import files with [`LF`](https://en.wikipedia.org/wiki/Newline) as line-terminator.
If you clone a project on windows with git cli to pack into a ZIP file later on, configure git not replace `LF` by `CRLF`:
```
git config [--global, --local, --system] core.autocrlf false
```
