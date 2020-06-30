---
title: Serializers
order: 220
---

All object serializers must be named ZCL_ABAPGIT_OBJECT_`type`, where `type` is the corresponding TADIR-OBJECT type

Use SAP Standard APIs for retriving and updating object information where possible.
If not possible try using ZCL_ABAPGIT_OBJECTS_GENERIC

As code is stored in git, no usernames, timestamps, states (e.g. active/inactive) or other system specific information should be part of the serialized object files.

Serializers must implement interface ZIF_ABAPGIT_OBJECT:

``ZIF_ABAPGIT_OBJECT~SERIALIZE`` 
Contains of all process steps to read all relevant object type specific information and serialize it (as one or many files)

``ZIF_ABAPGIT_OBJECT~DESERIALIZE`` 
Contains of all process steps to create or update an object based on one or many files

``ZIF_ABAPGIT_OBJECT~DELETE`` 
Contains of all process steps to delete an object based on one or many files

``ZIF_ABAPGIT_OBJECT~EXISTS`` 
Returns whether a given object already exists

``ZIF_ABAPGIT_OBJECT~IS_LOCKED`` 
Returns whether a given object is currently locked 

``ZIF_ABAPGIT_OBJECT~IS_ACTIVE`` 
Returns whether a given object exists in active state

``ZIF_ABAPGIT_OBJECT~CHANGED_BY`` 
Returns the user information of the last changer

``ZIF_ABAPGIT_OBJECT~JUMP`` 
Navigates to the corresponding object maintenance screen

``ZIF_ABAPGIT_OBJECT~GET_METADATA`` 
Returns object specific metadata 

``ZIF_ABAPGIT_OBJECT~GET_COMARATOR`` 
Triggered before deserialization to perform checks, eg warn the user that database tables are changed.

``ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS`` 
Defines the kind of Deserialzation Step (DDIC, ABAP, LATE), which is used to build the processing sequence of an import run

When adding new serializers, add tests to organization [abapGit-tests](https://github.com/abapGit-tests)

Deserialization sequence/prioritization is handled in `ZCL_ABAPGIT_OBJECTS->PRIORITIZE_DESER`
