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

    DATA: lv_changed_by TYPE oa2c_profiles-changed_by.

    SELECT SINGLE changed_by FROM oa2c_profiles
                             INTO lv_changed_by
                             WHERE profile = mv_profile.
    IF sy-subrc = 0.
      rv_user = lv_changed_by.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_object       TYPE string,
      lv_object_class TYPE string,
      lv_transp_pkg   TYPE abap_bool,
      lv_dummy        TYPE string.

    CONSTANTS: lc_actvt TYPE c LENGTH 2 VALUE `06`.

    "authority check
    AUTHORITY-CHECK OBJECT 'S_OA2C_ADM'
      ID 'ACTVT'     FIELD lc_actvt.
    IF sy-subrc <> 0.
      MESSAGE e463(01) WITH mv_profile INTO lv_dummy.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    "Enqueue
    CALL FUNCTION 'ENQUEUE_EOA2C_PROFILES'
      EXPORTING
        mode_oa2c_profiles = 'X'
        profile            = mv_profile
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100(
          iv_msgid              = sy-msgid
          iv_msgno              = sy-msgno
          iv_msgv1              = sy-msgv1
          iv_msgv2              = sy-msgv2
          iv_msgv3              = sy-msgv3
          iv_msgv4              = sy-msgv4 ).
    ENDIF.

    DELETE FROM oa2c_profiles WHERE profile  = mv_profile.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error when deleting profile { mv_profile } from table OA2C_PROFILES.| ).
    ENDIF.

    DELETE FROM  oa2p_scopes WHERE profile  = mv_profile.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error when deleting profile { mv_profile } from table OA2P_SCOPES.| ).
    ENDIF.

    CALL FUNCTION 'DEQUEUE_EOA2C_PROFILES'
      EXPORTING
        profile = mv_profile.


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

    DATA: ls_profile TYPE oa2c_profiles,
          lt_scope   TYPE STANDARD TABLE OF oa2p_scopes.


    io_xml->read( EXPORTING iv_name = 'PROFILE'
                  CHANGING cg_data = ls_profile ).

    io_xml->read( EXPORTING iv_name = 'SCOPES'
                  CHANGING cg_data = lt_scope ).


    MODIFY oa2c_profiles FROM ls_profile.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error updating table OA2C_PROFILES when deserialize profile { mv_profile }.| ).
    ENDIF.

    MODIFY oa2p_scopes FROM TABLE lt_scope.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error updating table OA2P_SCOPES when deserialize profile { mv_profile }.| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    SELECT SINGLE profile FROM oa2c_profiles
      INTO mv_profile
      WHERE profile = mv_profile.                       "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

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

    DATA: ls_profile TYPE oa2c_profiles,
          lt_scope   TYPE STANDARD TABLE OF oa2p_scopes.


    SELECT SINGLE * FROM oa2c_profiles
                    INTO ls_profile
                    WHERE profile = mv_profile.         "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Error when geting getails of OAuth2 Profile { mv_profile } from table OA2C_PROFILES.| ).
    ENDIF.
    CLEAR: ls_profile-changed_at,
           ls_profile-changed_by,
           ls_profile-changed_on,
           ls_profile-created_at,
           ls_profile-created_by,
           ls_profile-created_on.


    SELECT * FROM oa2p_scopes
             INTO TABLE lt_scope
             WHERE profile = mv_profile.                "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Error when geting Scopes of OAuth2 Profile { mv_profile } from table OA2C_SCOPES.| ).
    ENDIF.

    io_xml->add( iv_name = 'PROFILE'
                 ig_data = ls_profile ).

    io_xml->add( iv_name = 'SCOPES'
                 ig_data = lt_scope ).

  ENDMETHOD.
ENDCLASS.
