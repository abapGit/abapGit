CLASS zcl_abapgit_object_iwsv DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.

    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_IWSV IMPLEMENTATION.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        is_item = ms_item.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = zcl_abapgit_objects_super=>c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    get_generic( )->delete( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA technical_name TYPE /iwbep/med_grp_technical_name.
    DATA version TYPE /iwbep/med_grp_version.
    DATA bdcdata_tab TYPE TABLE OF bdcdata.
    DATA opt TYPE ctu_params.

    FIELD-SYMBOLS: <fs_bdcdata> LIKE LINE OF bdcdata_tab.

    technical_name = ms_item-obj_name.
    version = ms_item-obj_name+36(4).

    APPEND INITIAL LINE TO bdcdata_tab ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-program  = '/IWBEP/R_DST_SERVICE_BUILDER'.
    <fs_bdcdata>-dynpro   = '0100'.
    <fs_bdcdata>-dynbegin = 'X'.
    APPEND INITIAL LINE TO bdcdata_tab ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'GS_SCREEN_100-TECHNICAL_NAME'.
    <fs_bdcdata>-fval = technical_name.
    APPEND INITIAL LINE TO bdcdata_tab ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'GS_SCREEN_100-VERSION'.
    <fs_bdcdata>-fval = version.

    opt-dismode = 'E'.
    opt-defsize = 'X'.

    TRY.
        CALL TRANSACTION '/IWBEP/REG_SERVICE' WITH AUTHORITY-CHECK
                                USING bdcdata_tab OPTIONS FROM opt.
      CATCH cx_sy_authorization_error.
        zcx_abapgit_exception=>raise( |Transaction could not be started| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
ENDCLASS.
