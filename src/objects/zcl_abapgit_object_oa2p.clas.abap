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
    DATA: mv_profile TYPE c LENGTH 30.

ENDCLASS.



CLASS zcl_abapgit_object_oa2p IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_profile = is_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_persist     TYPE REF TO object,
          lr_wb          TYPE REF TO data,
          lo_profile     TYPE REF TO object,
          lv_profile_key TYPE seu_objkey.

    FIELD-SYMBOLS: <lo_wb> TYPE any.


    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').

    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.

    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = lv_profile_key    " Object Key
            p_version     = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data = <lo_wb>.  " Object Data
      CATCH cx_swb_object_does_not_exist.
        zcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    lo_profile = <lo_wb>.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~GET_CHANGED_BY')
      RECEIVING
        p_user_name = rv_user.


  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_object       TYPE string,
      lv_object_class TYPE string,
      lv_transp_pkg   TYPE abap_bool,
      lv_dummy        TYPE string.

    CONSTANTS: lc_actvt TYPE c LENGTH 2 VALUE `06`.


    DATA: lo_persist     TYPE REF TO object,
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
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').

    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~DELETE')
          EXPORTING
            p_object_key = lv_profile_key.   " Object Key
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

    DATA: lo_persist      TYPE REF TO object,
          lo_profile      TYPE REF TO object,
          lr_wb           TYPE REF TO data,
          lr_profile_data TYPE REF TO data.
    FIELD-SYMBOLS: <ls_profile_data> TYPE data,
                   <lo_wb>           TYPE any.

    CREATE DATA lr_profile_data TYPE ('OA2C_SX_OA2P_OBJECT_DATA').
    ASSIGN lr_profile_data->* TO <ls_profile_data>.

    io_xml->read( EXPORTING iv_name = 'PROFILE'
                  CHANGING cg_data = <ls_profile_data> ).



    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.

    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_DATA')
      EXPORTING
        p_data = <ls_profile_data>.

    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').
    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~SAVE')
          EXPORTING
            p_object_data = <lo_wb>.   " Object Data
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error deserialize profile { mv_profile }.| ).
    ENDTRY.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL METHOD ('CL_OA2P_OBJECT_PERSIST')=>('CHECK_EXISTS_ON_DB')
      EXPORTING
        i_profile = mv_profile
      RECEIVING
        r_exists  = rv_bool.

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

    DATA: lo_persist      TYPE REF TO object,
          lo_profile      TYPE REF TO object,
          lv_profile_key  TYPE seu_objkey,
          lr_profile_data TYPE REF TO data,
          lr_wb           TYPE REF TO data.

    FIELD-SYMBOLS: <ls_profile_data> TYPE data,
                   <lo_specifics>    TYPE any,
                   <lo_wb>           TYPE any.

    CREATE DATA lr_profile_data TYPE ('OA2C_SX_OA2P_OBJECT_DATA').
    ASSIGN lr_profile_data->* TO <ls_profile_data>.


    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').
    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.


    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = lv_profile_key    " Object Key
            p_version     = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data = <lo_wb>.  " Object Data
      CATCH cx_swb_object_does_not_exist.
        zcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        zcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    "remove system specific information
    lo_profile = <lo_wb>.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CHANGED_BY')
      EXPORTING
        p_user_name = ''.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CHANGED_ON')
      EXPORTING
        p_date = '00000000'
        p_time = '000000'.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CREATED_BY')
      EXPORTING
        p_user_name = ''.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CREATED_ON')
      EXPORTING
        p_date = '00000000'
        p_time = '000000'.

    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      IMPORTING
        p_data = <ls_profile_data>.

    "remove runtime information
    ASSIGN COMPONENT 'O_SPECIFICS' OF STRUCTURE <ls_profile_data> TO <lo_specifics>.
    CLEAR <lo_specifics>.

    io_xml->add( iv_name = 'PROFILE'
                 ig_data = <ls_profile_data> ).


  ENDMETHOD.
ENDCLASS.
