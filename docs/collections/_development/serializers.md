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

Example: [`DOMA`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_object_doma.clas.abap).

### Metadata

It is mandatory to provide the following metadata:

Attribute | Description
----------|------------
`CLASS`        | Technical name used to identify the serializer within serialized XML files (format `LCL_OBJECT_{type}`)
`VERSION`      | Version number of the serializer (format `v1.0.0`)
`DELETE_TADIR` | Set to `abap_true` to force the removal of the TADIR entry after deletion of the object
`DDIC`         | Set to `abap_true` if it is a DDIC object type (used for mass activation of DDIC objects)
`LATE_DESER`   | Obsolete (to be removed)

It's recommended to fill `CLASS` and `VERSION` metadata using `SUPER->GET_METADATA( )` and then changing settings as required. 

### Deserialization Step

It is mandatory to provide at least one deserialization step (see below).

## Super Class

Serializers can take advantage of the following methods in [`ZCL_ABAPGIT_OBJECTS_SUPER`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_objects_super.clas.abap):

Method | Description
-------|------------
`GET_METADATA`             | Return default metadata for class and version 
`CORR_INSERT`              | Insert object into a transport (for transportable objects)
`TADIR_INSERT`             | Insert object into TADIR 
`EXISTS_A_LOCK_ENTRY_FOR`  | Check if an enqueue lock exists
`SET_DEFAULT_PACKAGE`      | Set SAP package for it can't be supplied via APIs for RS_CORR_INSERT
`IS_ACTIVE`                | Method to check if an ABAP Workbench object or it's parts are active
`DELETE_DDIC`              | Method to remove DDIC objects
`CHECK_TIMESTAMP`          | Obsolete (to be removed)
`JUMP_ADT`                 | Obsolete (to be removed; use `ZCL_ABAPGIT_OBJECTS->JUMP` instead)
`JUMP_SE11`                | Obsolete (to be removed; use `ZCL_ABAPGIT_OBJECTS->JUMP` instead)

In addition, there are some methods to handle documents associated with an object (transaction `SE61`, table `DOKIL`).

Method | Description
-------|------------
`SERIALIZE_LONGTEXTS`   | Serialize document including I18N handling
`DESERIALIZE_LONGTEXTS` | Deserialize document including I18N handling
`DELETE_LONGTEXTS`      | Delete document 

## Generic Class

If it's not possible to provide a native implementation for an object serializer, using generic class [`ZCL_ABAPGIT_OBJECTS_GENERIC`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_objects_generic.clas.abap) is possible for logical transport objects  (see table `OBJH`, object type `L`).

Example: [`IWMO`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_object_iwmo.clas.abap).

## Serialize Object

The serialize method shall produce one or several files containing the data that represents a given object. There are a few methods available to define files and attach data using [`ZIF_ABAPGIT_OUTPUT_XML`](https://github.com/abapGit/abapGit/blob/master/src/xml/zif_abapgit_xml_output.intf.abap) (input parameter `IO_XML`).

Method | Description
-------|------------
`ADD`         | Append a value, structure, or internal table to the output (using ID transformation to XML suppressing initial fields)
`ADD_XML`     | Append an instance of an XML document to the output (`IF_XML_ELEMENT`)
`SET_RAW`     | Set the output to an instance of an XML document (`IF_XML_ELEMENT`)
`I18N_PARAMS` | Get the settings for internationalization (see below)

## Deserialize Object

The deserialize method shall read the file or files representing a given object and create such object in the system. If such object already exist, it shall be updated according to the  definition in the file or files. There are a few methods available to process files using [`ZIF_ABAPGIT_INPUT_XML`](https://github.com/abapGit/abapGit/blob/master/src/xml/zif_abapgit_xml_input.intf.abap) (input parameter `IO_XML`).

Method | Description
-------|------------
`READ`         | Return a value, structure, or internal table from the input (using ID transformation from XML accepting data loss)
`GET_RAW`      | Return the input as an instance of an XML document (`IF_XML_ELEMENT`)
`GET_METADATA` | Return the metadata used at time of serializing the object

In addition, the deserialize method must add or update the TADIR entry for the given object and insert the object into a transport request (for transportable packages). If the used SAP APIs are not performing these tasks, `TADIR_INSERT( iv_package )` and `CORR_INSERT( iv_package )` shall be called by the deserialize method.

## Activate Object

After deserializing, an object (or dependent objects) might have to be activated. Add such objects to the activation queue using [`ZCL_ABAPGIT_OBJECTS_ACTIVATION`]:

Method | Description
-------|------------
`ADD`      | Append a given object type and name to the queue (for example, `INDX` `{table}` for database indexes when deserializing tables)
`ADD_ITEM` | Append a given object to the queue (for example, use `ms_item` for activating the object itself)

The activation queue is built separately for each phase (see 'Deserialize Process' below).

## Internationalization (I18N)

In general, the serializer class shall process texts of an object in all available languages i.e. the original language as well as any translations. It shall respect the "Serialize Master Language Only" setting of a repository and limit the texts to the language provided to the constructor (`MV_LANGUAGE`). 

The recommended approach is to check `io_xml->i18n_params( )-serialize_master_lang_only = abap_false` and then serialize the additional translations in the XML (typically using `I18N` prefix). During deserialize the translation languages can then be retrieved and processed accordingly (

Example: [`TABL`](https://github.com/abapGit/abapGit/blob/master/src/objects/zcl_abapgit_object_tabl.clas.abap).

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
