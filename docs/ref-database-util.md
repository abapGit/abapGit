---
title: Database util
category: reference
order: 99
---

`Database util` is a tool for managing database entries created by abapGit. You can access the tool via the tools icon in the top right corner of the abapGit home page > choose first menu item `Database util`.

It's possible to edit database entries of type

- `USER` and
- `REPO`.

A `USER` entry contains meta information like the favorites of an user and their repository configurations e. g. name and email address for Git. This is an example of a `USER` entry:

```xml
<?xml version="1.0" encoding="utf-16"?>
<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
 <asx:values>
  <USER>
   <DEFAULT_GIT_USER>
    <NAME/>
    <EMAIL/>
   </DEFAULT_GIT_USER>
   <REPO_SHOW/>
   <HIDE_FILES/>
   <CHANGES_ONLY/>
   <SHOW_ORDER_BY/>
   <DIFF_UNIFIED/>
   <FAVORITES>
    <item>000000000001</item>
    <item>000000000002</item>
    <item>000000000003</item>
   </FAVORITES>
   <REPO_CONFIG>
    <item>
     <URL>https://[...]/file.git</URL>
     <LOGIN>my_username</LOGIN>
     <GIT_USER>
      <NAME>First and Last Name</NAME>
      <EMAIL>email@example.com</EMAIL>
     </GIT_USER>
     <LAST_CHANGE_SEEN/>
    </item>
    <item>
     <URL>https://[...]/file.git</URL>
     <LOGIN>my_second_user</LOGIN>
     <GIT_USER>
      <NAME>First and Last Name</NAME>
      <EMAIL>email@example.com</EMAIL>
     </GIT_USER>
     <LAST_CHANGE_SEEN/>
    </item>
   </REPO_CONFIG>
   <SETTINGS>
    <MAX_LINES>0</MAX_LINES>
    <ADT_JUMP_ENABLED/>
    <SHOW_DEFAULT_REPO/>
    <LINK_HINTS_ENABLED/>
    <LINK_HINT_KEY/>
    <HOTKEYS/>
    <PARALLEL_PROC_DISABLED/>
    <ICON_SCALING/>
    <UI_THEME/>
    <HIDE_SAPGUI_HINT/>
   </SETTINGS>
  </USER>
 </asx:values>
</asx:abap>
```

The `REPO` entries contain meta data like Git repository URL, branch and package information and information about files known and to be excluded. This is an example:

```xml
﻿<?xml version="1.0" encoding="utf-16"?>
<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
 <asx:values>
  <REPO>
   <URL>https://[...]/file.git</URL>
   <BRANCH_NAME>refs/heads/my_branch</BRANCH_NAME>
   <PACKAGE>Z_MY_PACKAGE</PACKAGE>
   <CREATED_BY>my_user</CREATED_BY>
   <CREATED_AT>20200418201549.200418</CREATED_AT>
   <DESERIALIZED_BY>my_user</DESERIALIZED_BY>
   <DESERIALIZED_AT>20200507134505.184445</DESERIALIZED_AT>
   <OFFLINE/>
   <LOCAL_CHECKSUMS>
    <item>
     <ITEM>
      <OBJ_TYPE/>
      <OBJ_NAME/>
      <DEVCLASS/>
      <INACTIVE/>
     </ITEM>
     <FILES>
      <item>
       <PATH>/</PATH>
       <FILENAME>.abapgit.xml</FILENAME>
       <SHA1>[...]</SHA1>
      </item>
     </FILES>
    </item>
    <item>
     <ITEM>
      <OBJ_TYPE>DEVC</OBJ_TYPE>
      <OBJ_NAME>Z_MY_PACKAGE</OBJ_NAME>
      <DEVCLASS>Z_MY_PACKAGE</DEVCLASS>
      <INACTIVE/>
     </ITEM>
     <FILES>
      <item>
       <PATH>/src/</PATH>
       <FILENAME>package.devc.xml</FILENAME>
       <SHA1>[...]</SHA1>
      </item>
     </FILES>
    </item>
    <item>
     <ITEM>
      <OBJ_TYPE>DEVC</OBJ_TYPE>
      <OBJ_NAME>Z_ANOTHER_PACKAGE</OBJ_NAME>
      <DEVCLASS>Z_ANOTHER_PACKAGE</DEVCLASS>
      <INACTIVE/>
     </ITEM>
     <FILES>
      <item>
       <PATH>/src/abaplinted_sample/</PATH>
       <FILENAME>package.devc.xml</FILENAME>
       <SHA1>[...]</SHA1>
      </item>
     </FILES>
    </item>
    <item>
     <ITEM>
      <OBJ_TYPE>PROG</OBJ_TYPE>
      <OBJ_NAME>Z_MY_REPORT</OBJ_NAME>
      <DEVCLASS>Z_ANOTHER_PACKAGE</DEVCLASS>
      <INACTIVE/>
     </ITEM>
     <FILES>
      <item>
       <PATH>/src/abaplinted_sample/</PATH>
       <FILENAME>z_my_report.prog.abap</FILENAME>
       <SHA1>[...]</SHA1>
      </item>
      <item>
       <PATH>/src/abaplinted_sample/</PATH>
       <FILENAME>z_my_report.prog.xml</FILENAME>
       <SHA1>[...]</SHA1>
      </item>
     </FILES>
    </item>
   </LOCAL_CHECKSUMS>
   <DOT_ABAPGIT>
    <MASTER_LANGUAGE>E</MASTER_LANGUAGE>
    <STARTING_FOLDER>/src/</STARTING_FOLDER>
    <FOLDER_LOGIC>PREFIX</FOLDER_LOGIC>
    <IGNORE>
     <item>/.gitignore</item>
     <item>/LICENSE</item>
     <item>/README.md</item>
     <item>/package.json</item>
     <item>/.travis.yml</item>
     <item>/.gitlab-ci.yml</item>
     <item>/abaplint.json</item>
     <item>/azure-pipelines.yml</item>
     <item>/src/.gitkeep</item>
    </IGNORE>
    <REQUIREMENTS/>
   </DOT_ABAPGIT>
   <HEAD_BRANCH/>
   <LOCAL_SETTINGS>
    <DISPLAY_NAME>My Test Repository</DISPLAY_NAME>
    <IGNORE_SUBPACKAGES/>
    <WRITE_PROTECTED/>
    <ONLY_LOCAL_OBJECTS/>
    <CODE_INSPECTOR_CHECK_VARIANT>ZMY_CHECK_VARIANT</CODE_INSPECTOR_CHECK_VARIANT>
    <BLOCK_COMMIT>X</BLOCK_COMMIT>
    <SERIALIZE_MASTER_LANG_ONLY/>
   </LOCAL_SETTINGS>
  </REPO>
 </asx:values>
</asx:abap>
```

If you remove a repository entry from the database util, the repository is not shown in abapGit anymore. This is useful for example if you want to remove a repository which has a non-existing package assigned and thus can't be opened and deleted normally.

Be careful when you edit these entries from within abapGit. Setting invalid options there might break your abapGit.
