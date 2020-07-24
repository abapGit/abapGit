---
title: Installation
category: getting-started
order: 10
---

## Summary ##
abapGit exists in 2 flavours: _standalone_ version or _developper_ version. 

  * Standalone version is targeted at users. It consist of one (huge) program which contains all the needed code. You run standalone version in transaction `SE38`, executing the program you created.
  * Developper version is targeted at developpers contributing to abapGit codebase. It consist of all the ABAP programs/classes/interfaces/etc. of the abapGit project. You run developer version with transaction `ZABAPGIT`.

## Prerequisites ##
abapGit requires SAP BASIS version 702 or higher.

## Install standalone version ##
1. Download the [ABAP code](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap)(right click -> save-as) to a file. 
2. Via `SE38` or `SE80`, create a new report named `ZABAPGIT_STANDALONE` (formerly `ZABAPGIT_FULL`). NB: Don't user `ZABAPGIT` if you plan to install developper version.
3. In source code change mode, upload the code from the file using Utilities -> More Utilities -> Upload/Download -> Upload
4. Activate

Typically abapGit will only be used in the development system, so it can be installed in a local `$` package (eg: `$TMP` or `$ZABAPGIT`).

Now you can use abapGit by executing the report in transaction `SE38`.

## Setup SSL ##
To use the online feature, [SSL must be setup](guide-ssl-setup.html). Offline projects will work behind firewalls and without SSL.

## Install developper version ##
1. Download latest version
   1. Go to https://github.com/larshp/abapGit/
   2. Select **Code** > **Download ZIP**
   3. Save file
2. Connect to SAP in language **EN**
3. In transaction `SE38`, run program `ZABAPGIT_STANDALONE`
4. Select **+ Offline**
5. Select existing package or create new package (eg: a local `$` package)
5. Select **Import <sup>zip</sup>**
6. Select *abapGit-master.zip* file
7. Select **Pull <sup>zip</sup>**
8. Activate

Transaction `ZABAPGIT` is now available
