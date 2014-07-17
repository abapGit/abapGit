# abapGit #

Git client for ABAP

## Design Goals ##
- Easy installation
- Easy upgrade
- Small system footprint
- Code readable in git repository

## Installation Guide ##
Copy the ABAP into new report via SE38 or SE80, setup SSL in STRUST, activate, run (tested on 7.30 and 7.40)

## FAQ ##
##### 1) SSL Setup #####
1) Goto github, find the certificate that it is using
2) Download certificate from https://www.digicert.com/digicert-root-certificates.htm
3) Install in STRUST under "SSL server Standard"
4) Restart ICM using transaction SMICM

Also see blogs
* http://scn.sap.com/people/jens.gleichmann/blog/2008/10/31/calling-webservices-from-abap-via-httpsssl-with-pfx-certificates
* http://scn.sap.com/people/thomas.jung/blog/2004/11/17/bsp-a-developers-journal-part-xiv--consuming-webservices-with-abap

##### 2) I found a bug #####
Add an issue in the tracker, https://github.com/larshp/abapGit/issues
