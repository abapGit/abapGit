CLASS zcl_abapgit_object_sush DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Note: This serializer is re-used by zcl_abapgit_object_tran for SU22 data
    " because transaction don't generate a separate SUSH object
    INTERFACES zif_abapgit_object .

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

    DATA ms_key TYPE usobkey.

    METHODS clear_metadata
      CHANGING
        cs_data_head TYPE any
        ct_usobx     TYPE STANDARD TABLE
        ct_usobt     TYPE STANDARD TABLE.

ENDCLASS.



CLASS zcl_abapgit_object_sush IMPLEMENTATION.


  METHOD clear_metadata.

    DATA:
      BEGIN OF ls_empty_metadata,
        modifier  TYPE c LENGTH 12, " usob_sm-modifier
        moddate   TYPE d, " usob_sm-moddate,
        modtime   TYPE t, " usob_sm-modtime,
        srcsystem TYPE tadir-srcsystem,
        author    TYPE tadir-author,
        devclass  TYPE tadir-devclass,
      END OF ls_empty_metadata.

    FIELD-SYMBOLS:
      <ls_usobx> TYPE any,
      <ls_usbot> TYPE any.

    MOVE-CORRESPONDING ls_empty_metadata TO cs_data_head.

    LOOP AT ct_usobx ASSIGNING <ls_usobx>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobx>.
    ENDLOOP.

    LOOP AT ct_usobt ASSIGNING <ls_usbot>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usbot>.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA: lr_data_head TYPE REF TO data.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').

      CATCH cx_sy_create_data_error.
        RAISE EXCEPTION TYPE zcx_abapgit_type_not_supported EXPORTING obj_type = is_item-obj_type.
    ENDTRY.

    ms_key = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE modifier FROM usob_sm INTO rv_user
      WHERE name = ms_key-name AND type = ms_key-type.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lo_su22 TYPE REF TO object,
      lx_err  TYPE REF TO cx_static_check.

    TRY.
        CREATE OBJECT lo_su22 TYPE ('CL_SU22_ADT_OBJECT').

        CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~DELETE')
          EXPORTING
            iv_key     = ms_key
            iv_cleanup = abap_true.
      CATCH cx_static_check INTO lx_err.
        zcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_key       TYPE usobkey,
      lo_su22      TYPE REF TO object,
      lo_appl      TYPE REF TO object,
      lt_usobx     TYPE usobx_t,
      lt_usobt     TYPE usobt_t,
      ls_usobhash  TYPE usobhash,
      lr_appl_head TYPE REF TO data,
      lr_data_head TYPE REF TO data,
      lx_err       TYPE REF TO cx_static_check,
      lv_text      TYPE string.

    FIELD-SYMBOLS:
      <ls_data_head>             TYPE any,
      <ls_appl_head>             TYPE any,
      <lv_abap_language_version> TYPE uccheck,
      <lv_display_name>          TYPE any,
      <ls_devclass>              TYPE any.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        io_xml->read( EXPORTING iv_name = 'HEAD'
                      CHANGING  cg_data = <ls_data_head> ).

        io_xml->read( EXPORTING iv_name = 'USOBX'
                      CHANGING  cg_data = lt_usobx ).

        io_xml->read( EXPORTING iv_name = 'USOBT'
                      CHANGING  cg_data = lt_usobt ).

        io_xml->read( EXPORTING iv_name = 'USOBHASH'
                      CHANGING  cg_data = ls_usobhash ).

        CREATE OBJECT lo_su22 TYPE ('CL_SU22_ADT_OBJECT').

        ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE <ls_data_head> TO <lv_abap_language_version>.
        IF sy-subrc = 0.
          set_abap_language_version( CHANGING cv_abap_language_version = <lv_abap_language_version> ).
        ENDIF.

        IF zif_abapgit_object~exists( ) = abap_false.
          " Older repos will not have USOBHASH so we try to reconstruct it
          IF ls_usobhash IS INITIAL.
            ASSIGN COMPONENT 'DISPLAY_NAME' OF STRUCTURE <ls_data_head> TO <lv_display_name>.
            CASE ms_key-type.
              WHEN 'TR'.
                ls_usobhash-pgmid    = 'R3TR'.
                ls_usobhash-object   = 'TRAN'.
                ls_usobhash-obj_name = <lv_display_name>.
              WHEN 'RF'.
                ls_usobhash-pgmid    = 'R3TR'.
                ls_usobhash-object   = 'FUGR'.
                ls_usobhash-obj_name = <lv_display_name>.
              WHEN 'HT'.
                IF <lv_display_name> CP 'R3TR*'.
                  SPLIT <lv_display_name> AT space INTO ls_usobhash-pgmid ls_usobhash-object ls_usobhash-obj_name.
                ENDIF.
              WHEN 'HS'.
                " TODO: Can we derive them from display name?
                ls_usobhash-service_type = ''.
                ls_usobhash-service      = ''.
              WHEN OTHERS.
                ASSERT 0 = 1.
            ENDCASE.
          ENDIF.

          MOVE-CORRESPONDING ms_key TO ls_usobhash.

          " Not for transactions
          IF ms_key-type <> 'TR'.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~CREATE')
              EXPORTING
                iv_new_key = ls_usobhash
              RECEIVING
                rs_key     = ls_key.
          ENDIF.
        ELSE.
          " check if lead application exists
          CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~CHECK')
            EXPORTING
              id_mode = '02'
            CHANGING
              cs_head = <ls_data_head>.

          CREATE DATA lr_appl_head TYPE ('CL_SU2X=>TS_HEAD').
          ASSIGN lr_appl_head->* TO <ls_appl_head>.

          CREATE OBJECT lo_appl TYPE ('CL_SU22_APPL').

          CALL METHOD lo_appl->('GET_DATA')
            EXPORTING
              is_key  = ms_key
            IMPORTING
              es_head = <ls_appl_head>.

          ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <ls_appl_head> TO <ls_devclass>.
          IF <ls_devclass> <> iv_package.
            lv_text =
            |Lead application of object { ms_item-obj_name } does not exist in package { <ls_devclass> }|.
            zcx_abapgit_exception=>raise( lv_text ).
          ENDIF.
        ENDIF.

        " Not for transactions
        IF ms_key-type <> 'TR'.
          corr_insert( iv_package ).
        ENDIF.

        CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
          EXPORTING
            is_head  = <ls_data_head>
            it_usobx = lt_usobx
            it_usobt = lt_usobt.

      CATCH cx_static_check INTO lx_err.
        zcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA ls_usobhash TYPE usobhash.

    SELECT SINGLE * FROM usobhash INTO ls_usobhash WHERE name = ms_key-name AND type = ms_key-type.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_USOBX'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      lo_su22      TYPE REF TO object,
      ls_usobhash  TYPE usobhash,
      lt_usobx     TYPE usobx_t,
      lt_usobt     TYPE usobt_t,
      lr_head      TYPE REF TO data,
      lr_usobx_ext TYPE REF TO data,
      lr_usobt_ext TYPE REF TO data,
      lx_err       TYPE REF TO cx_static_check.

    FIELD-SYMBOLS:
      <ls_head>                  TYPE any,
      <lv_abap_language_version> TYPE uccheck,
      <lt_usobx_ext>             TYPE ANY TABLE,
      <lt_usobt_ext>             TYPE ANY TABLE.

    TRY.
        CREATE DATA lr_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_head->* TO <ls_head>.

        CREATE DATA lr_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_usobx_ext->* TO <lt_usobx_ext>.

        CREATE DATA lr_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_usobt_ext->* TO <lt_usobt_ext>.

        CREATE OBJECT lo_su22 TYPE ('CL_SU22_ADT_OBJECT').

        CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~SELECT')
          EXPORTING
            iv_key       = ms_key
          IMPORTING
            es_head      = <ls_head>
            et_usobx     = lt_usobx
            et_usobt     = lt_usobt
            et_usobx_ext = <lt_usobx_ext>
            et_usobt_ext = <lt_usobt_ext>.

        clear_metadata(
          CHANGING
            cs_data_head = <ls_head>
            ct_usobx     = lt_usobx
            ct_usobt     = lt_usobt ).

        ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE <ls_head> TO <lv_abap_language_version>.
        IF sy-subrc = 0.
          <lv_abap_language_version> = get_abap_language_version( ).
        ENDIF.

        io_xml->add( iv_name = 'HEAD'
                     ig_data = <ls_head> ).

        io_xml->add( iv_name = 'USOBX'
                     ig_data = lt_usobx ).

        io_xml->add( iv_name = 'USOBT'
                     ig_data = lt_usobt ).

        " Serialize hash data because it contains the leading application name needed for recreating the object
        IF ms_key-type = 'HS' OR ms_key-type = 'HT'.
          SELECT SINGLE * FROM usobhash INTO ls_usobhash WHERE name = ms_key-name AND type = ms_key-type.
          IF sy-subrc = 0.
            CLEAR: ls_usobhash-name, ls_usobhash-type.

            io_xml->add( iv_name = 'USOBHASH'
                         ig_data = ls_usobhash ).
          ENDIF.
        ENDIF.

      CATCH cx_static_check INTO lx_err.
        zcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
