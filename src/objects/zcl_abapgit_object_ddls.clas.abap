CLASS zcl_abapgit_object_ddls DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
    METHODS open_adt_stob
      IMPORTING iv_ddls_name TYPE tadir-obj_name
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_DDLS IMPLEMENTATION.


  METHOD open_adt_stob.

    DATA: lr_data                   TYPE REF TO data.
    DATA: lo_ddl                    TYPE REF TO object.

    FIELD-SYMBOLS: <lt_ddnames>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lt_entity_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lg_ddnames>     TYPE any.
    FIELD-SYMBOLS: <lg_entity_view> TYPE any.
    FIELD-SYMBOLS: <lg_ddname>      TYPE any.
    FIELD-SYMBOLS: <lg_ddlname>     TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_DDOBJ').
        ASSIGN lr_data->* TO <lt_ddnames>.

        CREATE DATA lr_data LIKE LINE OF <lt_ddnames>.
        ASSIGN lr_data->* TO <lg_ddnames>.

        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_ENTITY_OF_VIEW').
        ASSIGN lr_data->* TO <lt_entity_view>.

        CREATE DATA lr_data LIKE LINE OF <lt_entity_view>.
        ASSIGN lr_data->* TO <lg_entity_view>.

        CLEAR <lt_ddnames>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_ddnames> TO <lg_ddname>.
        <lg_ddname> = iv_ddls_name.
        INSERT <lg_ddnames> INTO TABLE <lt_ddnames>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~GET_VIEWNAME_FROM_ENTITYNAME')
          EXPORTING
            ddnames        = <lt_ddnames>
          IMPORTING
            view_of_entity = <lt_entity_view>.

        READ TABLE <lt_entity_view> ASSIGNING <lg_entity_view> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_entity_view> TO <lg_ddlname>.

          jump_adt( i_obj_name = <lg_ddlname>
                    i_obj_type = 'DDLS' ).

        ENDIF.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'DDLS Jump Error' ).
    ENDTRY.

  ENDMETHOD.                    "open_adt_stob


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_ddl   TYPE REF TO object,
          lr_data  TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <lg_data>.


    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = lo_ddl.

    TRY.
        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.                    "zif_abapgit_object~changed_by


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.                    "zif_abapgit_object~compare_to_remote_version


  METHOD zif_abapgit_object~delete.

    DATA: lo_ddl TYPE REF TO object.


    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = lo_ddl.

    TRY.
        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~DELETE')
          EXPORTING
            name = ms_item-obj_name.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'DDLS error deleting' ).
    ENDTRY.

  ENDMETHOD.                    "delete


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_ddl  TYPE REF TO object,
          lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <lg_data>.

    io_xml->read( EXPORTING iv_name = 'DDLS'
                  CHANGING cg_data  = <lg_data> ).

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = mo_files->read_string( 'asddls' ) ##no_text.

    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = lo_ddl.

    TRY.
        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~SAVE')
          EXPORTING
            name         = ms_item-obj_name
            put_state    = 'N'
            ddddlsrcv_wa = <lg_data>.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~WRITE_TADIR')
          EXPORTING
            objectname = ms_item-obj_name
            devclass   = iv_package
            prid       = 0.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'DDLS error writing TADIR' ).
    ENDTRY.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize


  METHOD zif_abapgit_object~exists.

    DATA: lv_state TYPE objstate,
          lo_ddl   TYPE REF TO object.


    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = lo_ddl.

    TRY.
        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
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

  ENDMETHOD.                    "zif_abapgit_object~exists


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).

    rs_metadata-ddic         = abap_true.
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.                    "zif_abapgit_object~get_metadata


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since


  METHOD zif_abapgit_object~jump.

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
        zcx_abapgit_exception=>raise( 'DDLS Jump Error' ).
    ENDCASE.

  ENDMETHOD.                    "jump


  METHOD zif_abapgit_object~serialize.

    DATA: lo_ddl  TYPE REF TO object,
          lr_data TYPE REF TO data,
          lt_clr_comps TYPE STANDARD TABLE OF fieldname WITH DEFAULT KEY.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any,
                   <lv_comp>  LIKE LINE OF lt_clr_comps.


    CREATE DATA lr_data TYPE ('DDDDLSRCV').
    ASSIGN lr_data->* TO <lg_data>.

    CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
      RECEIVING
        handler = lo_ddl.

    TRY.
        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = <lg_data>.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'DDLS error reading' ).
    ENDTRY.

    APPEND 'AS4USER' TO lt_clr_comps.
    APPEND 'AS4DATE' TO lt_clr_comps.
    APPEND 'AS4TIME' TO lt_clr_comps.
    APPEND 'ACTFLAG' TO lt_clr_comps.
    APPEND 'CHGFLAG' TO lt_clr_comps.

    LOOP AT lt_clr_comps ASSIGNING <lv_comp>.
      ASSIGN COMPONENT <lv_comp> OF STRUCTURE <lg_data> TO <lg_field>.
      ASSERT sy-subrc = 0.
      CLEAR <lg_field>.
    ENDLOOP.

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.

    mo_files->add_string( iv_ext    = 'asddls'
                          iv_string = <lg_field> ) ##no_text.

    CLEAR <lg_field>.

    io_xml->add( iv_name = 'DDLS'
                 ig_data = <lg_data> ).

  ENDMETHOD.                    "serialize
ENDCLASS.
