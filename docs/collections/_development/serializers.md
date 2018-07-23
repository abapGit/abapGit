---
title: Serializers
order: 220
---

All object serializers must be named ZCL_ABAPGIT_OBJECT_"type", where type is the corresponding TADIR-OBJECT type

Use SAP Standard APIs for retriving and updating object information where possible.
If not possible try using ZCL_ABAPGIT_OBJECTS_GENERIC

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

``ZIF_ABAPGIT_OBJECT~CHANGED_BY`` 
todo

``ZIF_ABAPGIT_OBJECT~JUMP`` 
todo

``ZIF_ABAPGIT_OBJECT~GET_METADATA`` 
todo

``ZIF_ABAPGIT_OBJECT~HAS_CHANGED_SINCE`` 
todo

``ZIF_ABAPGIT_OBJECT~COMPARE_TO_REMOTE_VERSION`` 
todo