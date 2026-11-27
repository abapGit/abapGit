CLASS zcl_abapgit_object_scat DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_lock_object TYPE string VALUE 'E_TWB_ATTR'.

    TYPES ty_tc_name TYPE c LENGTH 30. " TWB_NAME = TNODE02_A-TC_NAME

    DATA mo_generic TYPE REF TO zcl_abapgit_objects_generic.
    DATA mv_tc_name TYPE ty_tc_name.

ENDCLASS.



CLASS zcl_abapgit_object_scat IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    IF strlen( is_item-obj_name ) > 30.
      zcx_abapgit_exception=>raise( |CATT procedure name { is_item-obj_name } longer than 30 characters| ).
    ENDIF.

    mv_tc_name = is_item-obj_name.

    " SCAT is a logical transport object (OBJH/OBJSL):
    " TNODE02_A (primary), TNODE02_AT/AC/AP/AS, CATF, CATV, CATP, CATX,
    " EUFUNC (FL), CAT_PARAMS, CAT_VERBS, CAT_VARS, CAT_DP_*, CATFF, CAT_FINFO, CAT_FDATA
    CREATE OBJECT mo_generic
      EXPORTING
        is_item     = is_item
        iv_language = iv_language.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE luser FROM tnode02_a INTO rv_user
      WHERE tc_name = mv_tc_name.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      SELECT SINGLE fuser FROM tnode02_a INTO rv_user
        WHERE tc_name = mv_tc_name.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    " deletes all OBJSL table entries of the object and records it on the transport
    mo_generic->delete( iv_package   = iv_package
                        iv_transport = iv_transport ).

    " object directory entry
    tadir_delete( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    " validate -> delete old table content -> insert from XML -> CORR_INSERT (+ TADIR)
    mo_generic->deserialize( iv_package   = iv_package
                             io_xml       = io_xml
                             iv_transport = iv_transport ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = mo_generic->exists( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    " CATT procedures have no active/inactive versions
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = c_lock_object
      iv_argument    = mv_tc_name ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata WITH DEFAULT KEY,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    " SAPMSCAT, initial screen 0100
    ls_bdcdata-program  = 'SAPMSCAT'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=SHOW'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'CATT-ABLNR'.
    ls_bdcdata-fval = mv_tc_name.
    APPEND ls_bdcdata TO lt_bdcdata.

    TRY.
        CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
          DESTINATION 'NONE'
          STARTING NEW TASK 'GIT'
          EXPORTING
            tcode                   = 'SCAT'
            mode_val                = 'E'
          TABLES
            using_tab               = lt_bdcdata
          EXCEPTIONS
            system_failure          = 1
            communication_failure   = 2
            resource_failure        = 3
            OTHERS                  = 4.
        rv_exit = boolc( sy-subrc = 0 ).
      CATCH cx_root.
        rv_exit = abap_false. " fall back to generic workbench navigation
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    " writes one XML node per OBJSL table (TNODE02_A, CATF, CATP, ...)
    mo_generic->serialize( io_xml ).

  ENDMETHOD.

ENDCLASS.
