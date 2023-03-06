CLASS zcl_abapgit_object_gsmp DEFINITION
  PUBLIC
    INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
    METHODS zif_abapgit_object~deserialize REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: sv_emsgty TYPE sychar01 VALUE 'S'.          "#EC NOTEXT
ENDCLASS.



CLASS zcl_abapgit_object_gsmp IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA lv_name TYPE c LENGTH 180.

    DATA lx_root TYPE REF TO cx_root.


    TRY.
        lv_name = ms_item-obj_name.

        SELECT SINGLE changed_by FROM ('GSM_MD_PRV_W')
          WHERE provider_id = @lv_name AND
          version = 'I'
          INTO @rv_user.                                    "#EC NOTEXT

        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        SELECT SINGLE changed_by FROM ('GSM_MD_PRV_W')
          WHERE provider_id = @lv_name AND
          version = 'A'
          INTO @rv_user.                                    "#EC NOTEXT

      CATCH cx_root INTO lx_root.
        zcx_abapgit_exception=>raise( iv_text     = lx_root->get_text( )
                                     ix_previous = lx_root ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA ls_properties TYPE if_aff_gsmp_v1=>ty_main.

    DATA ls_msg TYPE cl_gsm_cr_check=>ts_msg.
    DATA lv_is_valid TYPE abap_bool.

    FIELD-SYMBOLS <lg_data>  TYPE any.
    IF ms_item-obj_type <> 'GSMP'.
      RETURN.
    ENDIF.

    TRY.
        DATA(lv_json_as_string) = zif_abapgit_object~mo_files->read_string( iv_ext = 'json' ).

        CALL TRANSFORMATION gsmp_json_aff_v1 SOURCE XML lv_json_as_string
          RESULT root = ls_properties.


        cl_gsm_cr_check=>check_provider_import(
          EXPORTING
            iv_provider_id      = CONV #( ms_item-obj_name )
            iv_provider_type    = ls_properties-implementation-type
            iv_implementation   = ls_properties-implementation-name
            iv_execution_mode   = ls_properties-execution-mode
            iv_language_version = ls_properties-header-abap_language_version
          IMPORTING
            es_msg              = ls_msg
            ev_is_valid         = lv_is_valid
        ).

      CATCH cx_gsm_cr_root
            cx_transformation_error
            zcx_abapgit_exception INTO DATA(lx_exception).
        zcx_abapgit_exception=>raise( iv_text     = lx_exception->get_text( ) ).
    ENDTRY.

    IF lv_is_valid = abap_false .
      MESSAGE ID ls_msg-msgid
      TYPE sv_emsgty
      NUMBER ls_msg-msgno
      WITH ls_msg-attr1
           ls_msg-attr2
           ls_msg-attr3
           ls_msg-attr4 INTO DATA(lv_msg).
      ii_log->add_error(
           iv_msg  = lv_msg
           is_item = ms_item ).
      RETURN.
    ENDIF.

    super->zif_abapgit_object~deserialize(
      EXPORTING
        iv_package         = iv_package
        io_xml             = io_xml
        iv_step            = iv_step
        ii_log             = ii_log
        iv_transport       = iv_transport
    ).

  ENDMETHOD.


ENDCLASS.
