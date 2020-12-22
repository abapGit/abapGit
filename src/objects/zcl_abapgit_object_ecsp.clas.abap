CLASS zcl_abapgit_object_ecsp DEFINITION
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

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_ecsp IMPLEMENTATION.


  METHOD get_download.

    CREATE OBJECT ro_download TYPE zcl_abapgit_ecatt_sp_download.

  ENDMETHOD.


  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_SP'.

  ENDMETHOD.


  METHOD get_object_type.

* constant missing in 702, cl_apl_ecatt_const=>obj_type_start_profile
    rv_object_type = 'ECSP'.

  ENDMETHOD.


  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE zcl_abapgit_ecatt_sp_upload.

  ENDMETHOD.
ENDCLASS.
