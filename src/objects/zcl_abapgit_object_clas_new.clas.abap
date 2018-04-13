"! This class is just a different name for zcl_zabapgit_object_clas.
"! It has been created to heal repositories of the brave ones who uses abapGit
"! experimental features "! and had the luck to serialize their CLAS objects with
"! the serializer LCL_OBJECT_CLAS_NEW.
"! It can be removed on 2019-04 where we expect all CLAS object being
"! re-serialized with the serializer LCL_OBJECT_CLAS.
"! References: https://github.com/larshp/abapGit/pull/1311
CLASS zcl_abapgit_object_clas_new DEFINITION PUBLIC INHERITING FROM zcl_abapgit_object_clas.

  PROTECTED SECTION.
    METHODS:
      get_metadata REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_object_clas_new IMPLEMENTATION.

  METHOD get_metadata.
    rs_metadata = super->get_metadata( ).
    rs_metadata-class = 'ZCL_ABAPGIT_OBJECT_CLAS'.
  ENDMETHOD.

ENDCLASS.
