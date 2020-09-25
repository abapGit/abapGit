CLASS zcl_abapgit_object_cmod DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CMOD IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~CHANGED_BY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_USER                        TYPE        XUBNAME
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE anam FROM modattr INTO rv_user WHERE name = ms_item-obj_name.
    IF sy-subrc = 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~delete.

    DATA lv_name TYPE modact-name.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'MOD_KUN_ACTIVATE'
      EXPORTING
        activate           = abap_false
        deactivate         = abap_true
        modname            = lv_name
      EXCEPTIONS
        call_error         = 1
        generate_error     = 2
        modattr_status     = 3
        mod_active         = 4
        mod_enqueued       = 5
        not_activated      = 6
        no_modification    = 7
        permission_failure = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'MOD_KUN_DELETE'
      EXPORTING
        modname            = lv_name
        screen             = abap_false
      EXCEPTIONS
        attr_enqueued      = 1
        mod_active         = 2
        mod_enqueued       = 3
        text_enqueued      = 4
        permission_failure = 5
        OTHERS             = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] IO_XML                         TYPE REF TO ZIF_ABAPGIT_XML_INPUT
* | [--->] IV_STEP                        TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZATION_STEP
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name    TYPE modact-name,
          lt_modact  TYPE TABLE OF modact,
          lt_modtext TYPE TABLE OF modtext,
          lt_modattr TYPE TABLE OF modattr.

    lv_name = ms_item-obj_name.

    DELETE FROM modact WHERE name = lv_name.
    DELETE FROM modtext WHERE name = lv_name.
    DELETE FROM modattr WHERE name = lv_name.

    io_xml->read( EXPORTING iv_name = 'MODACT'
                  CHANGING  cg_data = lt_modact ).

    io_xml->read( EXPORTING iv_name = 'MODTEXT'
                  CHANGING  cg_data = lt_modtext ).

    io_xml->read( EXPORTING iv_name = 'MODATTR'
                  CHANGING  cg_data = lt_modattr ).

    INSERT modact FROM TABLE lt_modact.
    INSERT modtext FROM TABLE lt_modtext.
    INSERT modattr FROM TABLE lt_modattr.

    tadir_insert( iv_package ).

    CALL FUNCTION 'MOD_KUN_ACTIVATE'
      EXPORTING
        activate           = abap_true
        deactivate         = abap_false
        modname            = lv_name
      EXCEPTIONS
        call_error         = 1
        generate_error     = 2
        modattr_status     = 3
        mod_active         = 4
        mod_enqueued       = 5
        not_activated      = 6
        no_modification    = 7
        permission_failure = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_BOOL                        TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE modact-name.

    SELECT SINGLE name FROM modact INTO lv_name WHERE name = ms_item-obj_name.
    IF sy-subrc = 0.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~GET_COMPARATOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RI_COMPARATOR                  TYPE REF TO ZIF_ABAPGIT_COMPARATOR
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_STEPS                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZATION_STEP_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~GET_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_METADATA                    TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_METADATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~IS_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ACTIVE                      TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~IS_LOCKED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_IS_LOCKED                   TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~JUMP
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~jump.
    zcx_abapgit_exception=>raise( |Jump to CMOD is not supported| ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_CMOD->ZIF_ABAPGIT_OBJECT~SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_XML                         TYPE REF TO ZIF_ABAPGIT_XML_OUTPUT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~serialize.

    DATA: lt_modact  TYPE TABLE OF modact,
          lt_modtext TYPE TABLE OF modtext,
          lt_modattr TYPE TABLE OF modattr.

    FIELD-SYMBOLS: <ls_modattr> TYPE modattr.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    SELECT * FROM modact INTO TABLE lt_modact WHERE name = ms_item-obj_name.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'MODACT'
                   ig_data = lt_modact ).
    ENDIF.

    SELECT * FROM modtext INTO TABLE lt_modtext WHERE name = ms_item-obj_name AND sprsl = mv_language.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'MODTEXT'
                   ig_data = lt_modtext ).
    ENDIF.

    SELECT * FROM modattr INTO TABLE lt_modattr WHERE name = ms_item-obj_name.
    IF sy-subrc = 0.
      LOOP AT lt_modattr ASSIGNING <ls_modattr>.
        CLEAR:
          <ls_modattr>-cnam, <ls_modattr>-cdat,
          <ls_modattr>-unam, <ls_modattr>-udat,
          <ls_modattr>-anam, <ls_modattr>-adat,
          <ls_modattr>-fnam, <ls_modattr>-fdat.
      ENDLOOP.

      io_xml->add( iv_name = 'MODATTR'
                   ig_data = lt_modattr ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
