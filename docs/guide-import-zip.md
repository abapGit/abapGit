---
title: Import zip
category: offline projects
order: 10
---

## Download zip archive ## 
Offline projet is based on a zip archive that contains all the git files.

First download latest version of the projet you want to import. 

* For GitHub :
   1. Go to the projet main page (eg. https://github.com/larshp/abapOpenChecks)
   2. Select **Code** > **Download ZIP**
   3. Save file

## Import zip archive ##
1. Connect to SAP in language **EN**
2. Run abapGit:
  * You have installed standalone version : in transaction `SA38`, run the program you created  (`ZABAPGIT_STANDALONE` or `ZABAPGIT_FULL` or other)
  * You have installed developper version : call transaction `ZABAPGIT`
3. Select **+ Offline**
4. Enter projet name and select existing package or create new package
5. Select **Import <sup>zip</sup>**
6. Select the zip archive file (eg. *abapGit-master.zip*)
7. Select **Pull <sup>zip</sup>**
8. Activate
