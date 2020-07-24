---
title: Installation
category: getting-started
order: 10
---

You can install abapGit with an online install or an offline install.

Offline install steps are: 
1. Create ABAP report installer
2. Run installer ABAP report to perform offline projet import

Online install steps are: 
1. Create ABAP report installer
2. Setup SSL
3. Run installer ABAP report to perform online projet import

## Create ABAP Report installer ##
1. Download the [ABAP code](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap)(right click -> save-as) to a file. 
2. Via SE38 or SE80, create a new report named `ZABAPGIT_FULL`.
3. Upload the code from the file using Utilities -> More Utilities -> Upload/Download -> Upload
4. Activate

Typically abapGit will only be used in the development system, so it can be installed in a local `$` package (eg: `$ZABAPGIT`).


## Run installer (offline projet import) ##
1. Download latest version
   1. Go to https://github.com/larshp/abapGit/
   2. Select **Code** > **Download ZIP**
   3. Save file
2. Connect to SAP in language **EN**
3. Run program `ZABAPGIT_FULL`
4. Select **+ Offline**
5. Select existing package or create new package (eg: a local `$` package)
5. Select **Import <sup>zip</sup>**
6. Select *abapGit-master.zip* file
7. Select **Pull <sup>zip</sup>**
8. Activate

Transaction `ZABAPGIT` is now available

## Setup SSL ##
To use the online feature, [SSL must be setup](guide-ssl-setup.html). Offline projects will work behind firewalls and without SSL.


## Run installer (online projet import) ##
1. Connect to SAP in language **EN**
2. Run program `ZABAPGIT_FULL`
3. Follow [online import](guide-online-install.html) steps.

Transaction `ZABAPGIT` is now available
