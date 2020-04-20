---
title: .abapgit.xml
category: reference
order: 10
---

**.abapgit.xml** is a special abapGit file. It contains meta information of the abapGit project.

.abapgit.xml can be edit via "Repo menu > Advanced > Repo settings" or via "abapGit menu > Advanced > Database util". 

Example: abapGit's own .abapgit.xml

```xml
<?xml version="1.0" encoding="utf-8"?>
<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
 <asx:values>
  <DATA>
   <MASTER_LANGUAGE>E</MASTER_LANGUAGE>
   <STARTING_FOLDER>/src/</STARTING_FOLDER>
   <FOLDER_LOGIC>PREFIX</FOLDER_LOGIC>
   <IGNORE>
    <item>/.travis.yml</item>
    <item>/CONTRIBUTING.md</item>
    <item>/LICENSE</item>
    <item>/README.md</item>
    <item>/package.json</item>
    <item>/changelog.txt</item>
   </IGNORE>
  </DATA>
 </asx:values>
</asx:abap>
```

# Description

## Location
The `.abapgit.xml` file must be located in the root folder of the git repository.

## Master Language

The language in which all documentation and dictionary elements texts will be created. Follows SAP `sy-langu` values. Note that this implies that all objects in a repository will/should have the same master language.

## Starting Folder

The Git repository folder that defines the root folder where deserialization starts.

## Folder Logic

abapGit follows two folder logics: PREFIX and FULL

### PREFIX

A package name must contain its parent package name as a prefix. Examples:

Valid prefix:
* ZFOO
  * **ZFOO**_BAR
    * **ZFOO_BAR**_QUX

will give folder structure /bar/qux/

Invalid prefix:
* ZFOO
  * ZBAR

The folder logic PREFIX allows to install the repository into a different parent package. This can even be local packages(`$*`), in that case no transport order is required.

### FULL

Any package name is accepted

* ZSOMETHING
  * ZHELLO

will give folder structure /zsomething/zhello/

The folder logic FULL forces the installation of the repository into packages with exactly the same name. This can be problematic for contributors who use a system where specific prefixes for the package names are to be used.

## Ignore

Files which abapGit will not download into your ABAP system.

## Requirements

ToDo

## Local Settings

### Write protected

Write protected is described here: [Write protected](http://docs.abapgit.org/ref-write-protect.html)

### Ignore subpackages

Subpackages would be ignored through this option.

### Only local objects 

ToDo

### Code inspector

The repository objects can be checked with the Code inspector or the ABAP Test Cockpit (ATC) before staging. It's possible to perform a code inspector or ABAP Test Cockpit check without maintaing a check variant. In this case the end user is prompted with a F4 search help to choose a check variant during runtime.

#### Code inspector check variant

By entering a Code inspector or ABAP Test Cockpit (ATC) check variant, the check is activated. Local and remote check variants are supported. 

#### Block commit commit/push if code inspection has erros

This option can be used to prevent staging if errors of priority 1 and 2 were found during the Code Inspector or ABAP Test Cockpit (ATC) check. Findings of priority &ge; 3 are not reported. A check variant must be configured to activate this option. abapGit won't change its behavior based on the transport settings of the `Transport Tool Integration` of the ATC setup in transaction `ATC`. If this option is not active and errors were found, the end user can stage anyway. It's not possible to view or request exemptions from within abapGit during the staging process. Furthermore it's not able to access the ATC check documentation for a finding from within abapGit.
