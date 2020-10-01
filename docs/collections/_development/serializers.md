---
title: Serializers
order: 220
---

## Overview

An abapGit Serializer is an ABAP class that supports create, read, update, and delete (CRUD) of objects of a given object type. In abapGit, read of an object is implemented in a 'serialize' method. Create and update are combined into a 'deserialize' method. There is a 'delete' method to remove an object from the system.

All object serializers must implement interface `ZIF_ABAPGIT_OBJECT` and be named `ZCL_ABAPGIT_OBJECT_{type}`, where `{type}` is the corresponding SAP object type (`TADIR-OBJECT`). It's recommended to use `ZCL_ABAPGIT_OBJECTS_SUPER` as a super class since it provides several convenient methods.

In general, only SAP Standard APIs for retriving and updating object information shall be used. If that is not possible, try using `ZCL_ABAPGIT_OBJECTS_GENERIC` which handles any logical transport object.

As code is stored in git, no usernames, timestamps, states (e.g. active/inactive) or other system specific information should be part of the serialized object files. Only the active, most recent version of an object shall be serialized.

## Constructor

The constructor is implemented in the super class and take two parameters as input:

Parameter | Description
----------|------------
`IS_ITEM`     | Contains object type, object name, SAP package, and state (active/inactive)
`IV_LANGUAGE` | Contains the language key for the master language of the repository

These parameters are stored in attributes `MS_ITEM` and `MV_LANGUAGE` respectively.

## Interface 

Serializers must implement all methods of interface [`ZIF_ABAPGIT_OBJECT`](https://github.com/abapGit/abapGit/blob/master/src/objects/zif_abapgit_object.intf.abap):

Method | Description
-------|------------
`SERIALIZE`             | Contains of all process steps to read the relevant object type specific information and serialize it (as one or more files)
`DESERIALIZE`           | Contains of all process steps to create or update an object based on one or more files
`DELETE`                | Contains of all process steps to delete an object based on one or more files
`EXISTS`                | Returns whether a given object already exists in any state (i.e. return `abap_true` for inactive objects)
`IS_LOCKED`             | Returns whether a given object is currently locked 
`IS_ACTIVE`             | Returns whether a given object exists in active state
`CHANGED_BY`            | Returns the name of the use who last changed a given object (if undetermined, return `c_user_unknown`)
`JUMP`                  | Navigates to the corresponding object maintenance screen
`GET_METADATA`          | Returns object specific metadata (see below)
`GET_COMPARATOR`        | Triggered before deserialization to perform checks (for example, to warn the user that database tables are changed)
`GET_DESERIALIZE_STEPS` | Defines the deserialzation step or steps used to build the processing sequence (see below)

Example: [Object type `DOMA`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_object_doma.clas.abap).

### Metadata

It is mandatory to provide the following metadata:

Attribute | Description
----------|------------
`CLASS`        | Technical name used to identify the serializer within serialized XML files (format `LCL_OBJECT_{type}`)
`VERSION`      | Version number of the serializer (format `v1.0.0`)
`DELETE_TADIR` | Set to `abap_true` if the serializer class doesn't remove the TADIR entry during object deletion
`DDIC`         | Set to `abap_true` if it is a DDIC object type (used for mass activation of DDIC objects)
`LATE_DESER`   | obsolete

It's recommended to fill `CLASS` and `VERSION` metadata using `SUPER->GET_METADATA( )` and then changing settings as required. 

### Deserialization Step

It is mandatory to provide at least one deserialization step (see below).

## Super Class

Serializers can take advantage of the following methods in [`ZCL_ABAPGIT_OBJECTS_SUPER`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_objects_super.clas.abap):

Method | Description
-------|------------

## Generic Class

If it's not possible to provide a native implementation for an object serializer, using generic class [`ZCL_ABAPGIT_OBJECTS_GENERIC`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_objects_generic.clas.abap) is possible for logical transport objects  (see table `OBJH`, object type `L`).

Example: [`IWMO`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_object_iwmo.clas.abap).

## Serialize Object

The serialize method needs to produce one or several files containing the data that represents a given object. There are a few methods available to define files and attach data using [`ZIF_ABAPGIT_OUTPUT_XML`](https://github.com/abapGit/abapGit/blob/master/src/xml/zif_abapgit_xml_output.intf.abap) (input parameter `IO_XML`).

Method | Description
-------|------------
`ADD`     | Append a value, structure, or internal table to the output (using ID transformation to XML)
`ADD_XML` | Append an instance of an XML document to the output (`IF_XML_ELEMENT`)
`SET_RAW` | Set the output to an instance of an XML document (`IF_XML_ELEMENT`)


## Deserialize Object


Method | Description
-------|------------

## Internationalization (I18N)


## Testing

When adding new serializers, add at least one test repository to organization [abapGit-tests](https://github.com/abapGit-tests) which the name of the object type in capitals (for example, [`TABL`](https://github.com/abapGit-tests/TABL). This test will be used by [abapGit Continuous Integration](https://github.com/abapGit/CI).

## Processing Order and Dependencies

### Serialize Process

abapGit determines which objects need to be serialized based on the SAP package assigned to a repository (including subpackages unless "Ignore subpackages" is selected in the repository settings). The list of objects is then sorted by package, object type, and object name.

If a sufficient number of work processes is available, abapGit will activate objects in parallel (unless "Disable Parallel Processing" is selected in the repository settings). 

For details, see [`ZCL_ABAPGIT_SERIALIZE`](https://github.com/abapGit/abapGit/blob/master/src/zcl_abapgit_serialize.clas.abap).

### Deserialize Process

Objects are deserialized in three phases. After each phase all objects included in the phase will be activated. 

Step | Description | Activation
-----|-------------|-----------
`DDIC` | Used for DDIC objects which require early processing and activation before other object types | DDIC Mass Activation
`ABAP` | Used for non-DDIC objects (code or mostly anything else) which might depend on DDIC objects   | Workbench Mass Activation
`LATE` | Used for objects that depend on other objects processed in the previous two phases            | DDIC & Workbench Mass Activation

Within each phase, the sequence of objects is determined by abapGit based on known object type dependencies. For details, see [`ZCL_ABAPGIT_OBJECTS->PRIORITIZE_DESER`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_objects.clas.abap#L1047).  

### Uninstall Process

During uninstallation of a repository, abapGit will determine the objects in the same fashion as the serialize process. The sequence of objects is determined by abapGit based on known object type dependencies. For details, see [`ZCL_ABAPGIT_DEPENDENCIES->RESOLVE`](https://github.com/abapGit/abapGit/blob/master/src/zcl_abapgit_dependencies.clas.abap#L69). 

Note: There are suggestions to [refactor the logic to determine the processing order](https://github.com/abapGit/abapGit/issues/3536). 
