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
#### 1) SSL Setup ####
##### Option a) #####
1. Goto github, find the certificate that it is using
2. Download certificate from https://www.digicert.com/digicert-root-certificates.htm

##### Option b) #####
1. Use Firefox to go to https://github.com
2. Click on the lock icon and then "More Information ..." and there "View Certificate"
3. Switch to the Details Tab and choose the first certificate of the tree and click Export
4. Do the same for the next certificate in the tree

##### Then continue here #####
1. Install the certificates in STRUST under "SSL System Client SSL Client (Standard)"
2. Restart ICM using transaction SMICM (when you're NetWeaver Release is 7.01 or lower)

Also see blogs
* http://scn.sap.com/people/jens.gleichmann/blog/2008/10/31/calling-webservices-from-abap-via-httpsssl-with-pfx-certificates
* http://scn.sap.com/people/thomas.jung/blog/2004/11/17/bsp-a-developers-journal-part-xiv--consuming-webservices-with-abap

#### 2) Bugs/Comments/Feature Requests ####
Add an issue in the tracker, https://github.com/larshp/abapGit/issues
