CLASS zcl_abapgit_object_oa2p DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_profile TYPE oa2c_profiles-profile.

ENDCLASS.



CLASS zcl_abapgit_object_oa2p IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_profile = is_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_persist     TYPE REF TO cl_oa2p_object_persist,
          lo_profile     TYPE REF TO if_wb_object_data_model,
          lv_profile_key TYPE seu_objkey.

    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist.
    CREATE OBJECT lo_profile TYPE cl_oa2p_object_data.

    TRY.
        lo_persist->if_wb_object_persist~get(
          EXPORTING
            p_object_key                 = lv_profile_key    " Object Key
            p_version                    = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data                = lo_profile ).  " Object Data
      CATCH cx_swb_object_does_not_exist.
        zcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    rv_user = lo_profile->get_changed_by( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_object       TYPE string,
      lv_object_class TYPE string,
      lv_transp_pkg   TYPE abap_bool,
      lv_dummy        TYPE string.

    CONSTANTS: lc_actvt TYPE c LENGTH 2 VALUE `06`.


    DATA: lo_persist     TYPE REF TO cl_oa2p_object_persist,
          lo_profile     TYPE REF TO if_wb_object_data_model,
          lv_profile_key TYPE seu_objkey.

    "authority check
    AUTHORITY-CHECK OBJECT 'S_OA2C_ADM'
      ID 'ACTVT'     FIELD lc_actvt.
    IF sy-subrc <> 0.
      MESSAGE e463(01) WITH mv_profile INTO lv_dummy.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    "delete profile
    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist.
    CREATE OBJECT lo_profile TYPE cl_oa2p_object_data.

    TRY.
        lo_persist->if_wb_object_persist~delete( p_object_key = lv_profile_key ).   " Object Key
      CATCH cx_swb_object_does_not_exist.
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error when deleting OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.


    "collect change in transport
    lv_transp_pkg = zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    IF lv_transp_pkg = abap_true.

      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.

      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object              = lv_object
          object_class        = lv_object_class
          master_language     = mv_language
          global_lock         = abap_true
          mode                = 'D'
          suppress_dialog     = abap_true
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100(
            iv_msgid              = sy-msgid
            iv_msgno              = sy-msgno
            iv_msgv1              = sy-msgv1
            iv_msgv2              = sy-msgv2
            iv_msgv3              = sy-msgv3
            iv_msgv4              = sy-msgv4 ).
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_persist      TYPE REF TO cl_oa2p_object_persist,
          lo_profile      TYPE REF TO if_wb_object_data_model,
          ls_profile_data TYPE oa2c_sx_oa2p_object_data.

    io_xml->read( EXPORTING iv_name = 'PROFILE'
                  CHANGING cg_data = ls_profile_data ).


    CREATE OBJECT lo_profile TYPE cl_oa2p_object_data.
    lo_profile->set_data( p_data = ls_profile_data ).

    CREATE OBJECT lo_persist.
    TRY.
        lo_persist->if_wb_object_persist~save( p_object_data = lo_profile ).   " Object Data
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error deserialize profile { mv_profile }.| ).
    ENDTRY.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = cl_oa2p_object_persist=>check_exists_on_db( i_profile = mv_profile ).

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

    DATA: lv_profile_name TYPE eqegraarg,
          lv_lock_number  TYPE int4,
          lt_locks        TYPE  STANDARD TABLE OF seqg3.

    lv_profile_name = mv_profile.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient = sy-mandt    " Client
        gname   = 'OA2C_PROFILES'    " Granularity name (-> table name)
        garg    = lv_profile_name    " Granularity value(->values of key fields)
      IMPORTING
        number  = lv_lock_number
      TABLES
        enq     = lt_locks.    " Number of chosen lock entries


    IF lv_lock_number > 0.
      rv_is_locked = abap_true.
    ELSE.
      rv_is_locked = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = mv_profile
        object_type   = 'OA2P'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_persist      TYPE REF TO cl_oa2p_object_persist,
          lo_profile      TYPE REF TO if_wb_object_data_model,
          lv_profile_key  TYPE seu_objkey,
          ls_profile_data TYPE oa2c_sx_oa2p_object_data.

    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist.
    CREATE OBJECT lo_profile TYPE cl_oa2p_object_data.

    TRY.
        lo_persist->if_wb_object_persist~get(
          EXPORTING
            p_object_key                 = lv_profile_key    " Object Key
            p_version                    = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data                = lo_profile ).  " Object Data
      CATCH cx_swb_object_does_not_exist.
        zcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    "remove system specific information
    lo_profile->set_changed_by( p_user_name = '' ).
    lo_profile->set_changed_on( p_date = '00000000'
                                p_time = '000000' ).
    lo_profile->set_created_by( p_user_name = '' ).
    lo_profile->set_created_on( p_date = '00000000'
                                p_time = '000000' ).

    lo_profile->get_data( IMPORTING p_data = ls_profile_data ).

    "remove runtime information
    CLEAR ls_profile_data-o_specifics.

    io_xml->add( iv_name = 'PROFILE'
                 ig_data = ls_profile_data ).


  ENDMETHOD.
ENDCLASS.
