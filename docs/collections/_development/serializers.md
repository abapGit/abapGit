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
todo

``ZIF_ABAPGIT_OBJECT~DESERIALIZE`` 
todo

``ZIF_ABAPGIT_OBJECT~DELETE`` 
todo

``ZIF_ABAPGIT_OBJECT~EXISTS`` 
todo

``ZIF_ABAPGIT_OBJECT~IS_LOCKED`` 
todo

``ZIF_ABAPGIT_OBJECT~IS_ACTIVE`` 
todo

``ZIF_ABAPGIT_OBJECT~CHANGED_BY`` 
todo

``ZIF_ABAPGIT_OBJECT~JUMP`` 
todo

``ZIF_ABAPGIT_OBJECT~GET_METADATA`` 
todo

``ZIF_ABAPGIT_OBJECT~GET_COMARATOR`` 
Triggered before deserialization to perform checks, eg warn the user that database tables are changed.

``ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS`` 
todo
