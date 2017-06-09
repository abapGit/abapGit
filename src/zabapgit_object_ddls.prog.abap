*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_VIEW
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ddls DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PROTECTED SECTION.
    METHODS open_adt_stob
      IMPORTING iv_ddls_name TYPE tadir-obj_name
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ddls IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
* todo
    rv_user = c_user_unknown.
  ENDMETHOD.                    "lif_object~changed_by

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).

    rs_metadata-ddic         = abap_true.
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_state TYPE objstate,
          li_ddl   TYPE REF TO object.


    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = li_ddl.

    TRY.
        CALL METHOD li_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name      = ms_item-obj_name
            get_state = 'A'
          IMPORTING
            got_state = lv_state.
        IF lv_state IS INITIAL.
          rv_bool = abap_false.
        ELSE.
          rv_bool = abap_true.
        ENDIF.
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    DATA: lv_typename   TYPE typename.
    DATA: lv_ddtypekind TYPE ddtypekind.

    lv_typename = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = lv_typename
      IMPORTING
        typekind = lv_ddtypekind.

    CASE lv_ddtypekind.
      WHEN 'STOB'.

        me->open_adt_stob( iv_ddls_name = ms_item-obj_name ).
      WHEN OTHERS.
        lcx_exception=>raise( 'DDLS Jump Error' ).
    ENDCASE.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: li_ddl TYPE REF TO object.


    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = li_ddl.

    TRY.
        CALL METHOD li_ddl->('IF_DD_DDL_HANDLER~DELETE')
          EXPORTING
            name = ms_item-obj_name.
      CATCH cx_root.
        lcx_exception=>raise( 'DDLS error' ).
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: li_ddl  TYPE REF TO object,
          lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_field> TYPE any.


    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <ls_data>.

    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = li_ddl.

    TRY.
        CALL METHOD li_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = <ls_data>.
      CATCH cx_root.
        lcx_exception=>raise( 'DDLS error' ).
    ENDTRY.

    ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <ls_data> TO <lv_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lv_field>.
    ASSIGN COMPONENT 'AS4DATE' OF STRUCTURE <ls_data> TO <lv_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lv_field>.
    ASSIGN COMPONENT 'AS4TIME' OF STRUCTURE <ls_data> TO <lv_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lv_field>.

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_data> TO <lv_field>.
    ASSERT sy-subrc = 0.

    mo_files->add_string( iv_ext    = 'asddls'
                          iv_string = <lv_field> ) ##no_text.

    CLEAR <lv_field>.

    io_xml->add( iv_name = 'DDLS'
                 ig_data = <ls_data> ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: li_ddl  TYPE REF TO object,
          lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_field> TYPE any.


    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <ls_data>.

    io_xml->read( EXPORTING iv_name = 'DDLS'
                  CHANGING cg_data  = <ls_data> ).

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_data> TO <lv_field>.
    ASSERT sy-subrc = 0.
    <lv_field> = mo_files->read_string( 'asddls' ) ##no_text.

    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = li_ddl.

    TRY.
        CALL METHOD li_ddl->('IF_DD_DDL_HANDLER~SAVE')
          EXPORTING
            name         = ms_item-obj_name
            put_state    = 'N'
            ddddlsrcv_wa = <ls_data>.

        CALL METHOD li_ddl->('IF_DD_DDL_HANDLER~WRITE_TADIR')
          EXPORTING
            objectname = ms_item-obj_name
            devclass   = iv_package
            prid       = 0.
      CATCH cx_root.
        lcx_exception=>raise( 'DDLS error' ).
    ENDTRY.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.                    "lif_object~compare_to_remote_version

  METHOD open_adt_stob.

    DATA: lr_data                   TYPE REF TO data.
    DATA: li_ddl                    TYPE REF TO object.
    FIELD-SYMBOLS: <lt_ddnames>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lt_entity_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <ls_ddnames>     TYPE any.
    FIELD-SYMBOLS: <ls_entity_view> TYPE any.
    FIELD-SYMBOLS: <lv_ddname>      TYPE any.
    FIELD-SYMBOLS: <lv_ddlname>     TYPE any.
    TRY.

        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_DDOBJ').
        ASSIGN lr_data->* TO <lt_ddnames>.

        CREATE DATA lr_data LIKE LINE OF <lt_ddnames>.
        ASSIGN lr_data->* TO <ls_ddnames>.

        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_ENTITY_OF_VIEW').
        ASSIGN lr_data->* TO <lt_entity_view>.

        CREATE DATA lr_data LIKE LINE OF <lt_entity_view>.
        ASSIGN lr_data->* TO <ls_entity_view>.

        CLEAR <lt_ddnames>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_ddnames> TO <lv_ddname>.
        <lv_ddname> = iv_ddls_name.
        INSERT <ls_ddnames> INTO TABLE <lt_ddnames>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = li_ddl.

        CALL METHOD li_ddl->('IF_DD_DDL_HANDLER~GET_VIEWNAME_FROM_ENTITYNAME')
          EXPORTING
            ddnames        = <lt_ddnames>
          IMPORTING
            view_of_entity = <lt_entity_view>.

        READ TABLE <lt_entity_view> ASSIGNING <ls_entity_view> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <ls_entity_view> TO <lv_ddlname>.

          jump_adt( i_obj_name = <lv_ddlname>
                    i_obj_type = 'DDLS' ).

        ENDIF.

      CATCH cx_root.
        lcx_exception=>raise( 'DDLS Jump Error' ).
    ENDTRY.

  ENDMETHOD.                    "open_adt_stob

ENDCLASS.                    "lcl_object_view IMPLEMENTATION
