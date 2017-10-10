*&---------------------------------------------------------------------*
*& Include zabapgit_object_ucsa
*&---------------------------------------------------------------------*

CLASS lcl_object_ucsa DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    TYPES:
      ty_id TYPE c LENGTH 30.

    METHODS:
      get_persistence
        IMPORTING
          iv_id                 TYPE ty_id
        RETURNING
          VALUE(ro_persistence) TYPE REF TO object.

ENDCLASS.

CLASS lcl_object_ucsa IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_id          TYPE ty_id,
          lo_persistence TYPE REF TO object.

    lv_id = ms_item-obj_name.

    TRY.
        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = 'A'
            language = sy-langu.

      CATCH cx_root.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lv_id          TYPE ty_id,
          lx_root        TYPE REF TO cx_root,
          lv_text        TYPE string,
          lo_persistence TYPE REF TO object,
          lr_sa          TYPE REF TO data.

    FIELD-SYMBOLS: <sa>     TYPE any,
                   <header> TYPE any,
                   <field>  TYPE any.

    lv_id = ms_item-obj_name.

    TRY.
        CREATE DATA lr_sa TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_sa->* TO <sa>.
        ASSERT sy-subrc = 0.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = 'A'
            language = sy-langu
          IMPORTING
            sa       = <sa>.

        ASSIGN COMPONENT 'HEADER' OF STRUCTURE <sa>
               TO <header>.
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT 'CREATEDBY' OF STRUCTURE <header>
               TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR: <field>.

        ASSIGN COMPONENT 'CREATEDON' OF STRUCTURE <header>
               TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR: <field>.

        ASSIGN COMPONENT 'CREATEDAT' OF STRUCTURE <header>
               TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR: <field>.

        ASSIGN COMPONENT 'CHANGEDBY' OF STRUCTURE <header>
               TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR: <field>.

        ASSIGN COMPONENT 'CHANGEDON' OF STRUCTURE <header>
               TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR: <field>.

        ASSIGN COMPONENT 'CHANGEDAT' OF STRUCTURE <header>
               TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR: <field>.

        io_xml->add( iv_name = 'UCSA'
                     ig_data = <sa> ).

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lv_id          TYPE ty_id,
          lx_root        TYPE REF TO cx_root,
          lv_text        TYPE string,
          lo_persistence TYPE REF TO object,
          lr_sa          TYPE REF TO data.

    FIELD-SYMBOLS: <sa> TYPE any.

    TRY.
        CREATE DATA lr_sa TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_sa->* TO <sa>.
        ASSERT sy-subrc = 0.

        io_xml->read(
          EXPORTING
            iv_name = 'UCSA'
          CHANGING
            cg_data = <sa> ).

        lv_id = ms_item-obj_name.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~CREATE').

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~SAVE')
          EXPORTING
            sa      = <sa>
            version = 'A'.

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: lv_id          TYPE ty_id,
          lx_root        TYPE REF TO cx_root,
          lv_text        TYPE string,
          lo_persistence TYPE REF TO object.

    TRY.
        lv_id = ms_item-obj_name.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~DELETE')
          EXPORTING
            version = 'A'.

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_TOOL_ACCESS' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

  METHOD get_persistence.

    CALL METHOD ('CL_UCON_SA_DB_PERSIST')=>('IF_UCON_SA_PERSIST~GET_INSTANCE')
      EXPORTING
        id       = iv_id
      RECEIVING
        instance = ro_persistence.

  ENDMETHOD.

ENDCLASS.
