CLASS zcl_abapgit_object_ectd DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_ecatt_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      get_object_type REDEFINITION,
      get_upload REDEFINITION,
      get_download REDEFINITION,
      get_lock_object REDEFINITION.

private section.
ENDCLASS.



CLASS zcl_abapgit_object_ectd IMPLEMENTATION.


  METHOD get_download.

    CREATE OBJECT ro_download TYPE zcl_abapgit_ecatt_data_downl.

  ENDMETHOD.


  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TD'.

  ENDMETHOD.


  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_data.

  ENDMETHOD.


  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE zcl_abapgit_ecatt_data_upload.

  ENDMETHOD.
ENDCLASS.
