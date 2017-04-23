*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_dcls
*&---------------------------------------------------------------------*

CLASS lcl_object_dcls DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.

CLASS lcl_object_dcls IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).

    rs_metadata-ddic         = abap_true.
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: li_dcl TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = li_dcl.

        CALL METHOD li_dcl->('CHECK_EXISTENCE')
          EXPORTING
            iv_objectname = ms_item-obj_name
          RECEIVING
            rv_exists     = rv_bool.

      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~jump.

    TRY.

        jump_adt( i_obj_name = ms_item-obj_name
                  i_obj_type = ms_item-obj_type ).

      CATCH lcx_exception.
        lcx_exception=>raise( 'DCLS Jump Error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: li_dcl TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = li_dcl.

        CALL METHOD li_dcl->('DELETE')
          EXPORTING
            iv_dclname = ms_item-obj_name.

      CATCH cx_root.
        lcx_exception=>raise( 'DCLS error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lr_data TYPE REF TO data,
          li_dcl  TYPE REF TO object.

    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <field>   TYPE any.

    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <ls_data>.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = li_dcl.

        CALL METHOD li_dcl->('READ')
          EXPORTING
            iv_dclname = ms_item-obj_name
          IMPORTING
            es_dclsrc  = <ls_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <ls_data> TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR <field>.

        ASSIGN COMPONENT 'AS4DATE' OF STRUCTURE <ls_data> TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR <field>.

        ASSIGN COMPONENT 'AS4TIME' OF STRUCTURE <ls_data> TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR <field>.

        ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <ls_data> TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR <field>.

        ASSIGN COMPONENT 'CREATED_DATE' OF STRUCTURE <ls_data> TO <field>.
        ASSERT sy-subrc = 0.
        CLEAR <field>.

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_data> TO <field>.
        ASSERT sy-subrc = 0.

        mo_files->add_string( iv_ext = 'asdcls'  iv_string = <field> ).

        CLEAR <field>.

        io_xml->add( iv_name = 'DCLS'
                     ig_data = <ls_data> ).

      CATCH cx_root.
        lcx_exception=>raise( 'DCLS error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lr_data TYPE REF TO data,
          li_dcl  TYPE REF TO object.

    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <field>   TYPE any.

    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = 'DCLS'
      CHANGING
        cg_data = <ls_data> ).

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_data> TO <field>.
    ASSERT sy-subrc = 0.
    <field> = mo_files->read_string( 'asdcls' ).

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = li_dcl.

        CALL METHOD li_dcl->('SAVE')
          EXPORTING
            iv_dclname     = ms_item-obj_name
            iv_put_state   = 'A'
            is_dclsrc      = <ls_data>
            iv_devclass    = iv_package
            iv_access_mode = 'INSERT'.

        tadir_insert( iv_package ).

      CATCH cx_root.
        lcx_exception=>raise( 'DCLS error' ).
    ENDTRY.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.
