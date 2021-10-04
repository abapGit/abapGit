---
title: Repository (.abapgit.xml)
category: repo settings
order: 10
---

`**.abapgit.xml**` is a special abapGit file. It contains meta information of the abapGit project. The file must be located in the root folder of the git repository.

It is recommended to edit .abapgit.xml using "Repository Settings" ![](img/repo_settings.png) in abapGit. 
(In exceptional cases, you could edit the XML directly via "Utilities > Database Util" ![](img/utilities.png).)

![](img/repo_settings_menu.png)

Example: abapGit's own `.abapgit.xml`

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
    <item>/.gitignore</item>
    <item>/CODE_OF_CONDUCT.md</item>
    <item>/abaplint.json</item>
    <item>/.eslintrc.yaml</item>
   </IGNORE>
  </DATA>
 </asx:values>
</asx:abap>
```

## Repository Settings

The following settings are stored in the repository and therefore valid for all users.

![](img/repo_settings_abapgit_xml.png)

### Main Language

The main language is the languages in which all texts belonging to repository objects will be created. It follows the SAP `sy-langu` values. Note that this implies that all objects in a repository must have the same main language.

### Translation Languages

There are currently two options available for including translations in a repository: 

1. Object-specific logic provided by the object serializer class
2. Generic logic provided by abapGit framework (experimental, #4415) 

If no translation languages are maintained in this setting, the first approach is used. If you maintain a list of translation languages (comma-separated, 2-letter ISO language codes), then these languages will be included in the serialization. See also [Translations and i18n](./ref-translations.html)

Note: You can suppress translations with the local "Only Serialize Main Language" setting (see below).

### Starting Folder

The Git repository folder that defines the root folder where deserialization starts.

### Folder Logic

abapGit follows two folder logics: "Prefix" and "Full".

#### Prefix

A package name must contain its parent package name as a prefix. Examples:

Valid prefix:
* ZFOO
  * **ZFOO**_BAR
    * **ZFOO_BAR**_QUX

will produce folder structure /bar/qux/

Invalid prefix:
* ZFOO
  * ZBAR

The folder logic PREFIX allows to install a repository into a different parent package (in different systems). This can even be local packages (`$*`), in which case no transport order is required.

#### Full

Any package name is accepted.

* ZSOMETHING
  * ZHELLO

will produce folder structure /zsomething/zhello/

The folder logic FULL forces the installation of a repository into packages with exactly the same name. Note that this can be problematic for contributors who use a system where specific prefixes for the package names are to be used.

### Ignore Files

Files which abapGit will not download to your ABAP system. Typically, this includes references to readme, changelog, and license 
files as well as repository configuration related to workflows like build or linting jobs.

Assuming that default starting folder /src/ is used, any files in root / or any other folder than the starting folder are ignored automatically. Therefore it will not be necessary to list files of the root folder into the ignore list (and as a consequence, the default ignore list is empty).

The ignore logic is based on "covers pattern" operator (not regex). A file is ignored, if path & file covers at least one of the patterns listed in this setting.

Example: `/src/hr/zcl_confidential*` will ignore all classes in the /src/hr package that begin with `zcl_confidential`. 

### Requirements

In this section, you can specify the minimum requirements that should be fulfilled to allow installation of the repository. Listed software components should exist in the target system and be at the given release or higher. If the target system matches the minimum release, then it must also be at the given patch level or higher.
