*&---------------------------------------------------------------------*
*& Include zabapgit_object_ddlx
*&---------------------------------------------------------------------*

CLASS lcl_object_ddlx DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    DATA: mo_persistence TYPE REF TO if_wb_object_persist.

  PRIVATE SECTION.
    METHODS:
      get_persistence
        RETURNING
          VALUE(ri_persistence) TYPE REF TO if_wb_object_persist
        RAISING
          zcx_abapgit_exception,

      clear_fields
        CHANGING
          cs_data TYPE any,

      clear_field
        IMPORTING
          iv_fieldname TYPE csequence
        CHANGING
          cs_metadata  TYPE any.

ENDCLASS.

CLASS lcl_object_ddlx IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    rv_bool = abap_true.

    TRY.
        get_persistence( )->get( p_object_key           = lv_object_key
                                 p_version              = swbm_version_active
                                 p_existence_check_only = abap_true ).

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    TRY.
        jump_adt( i_obj_name = ms_item-obj_name
                  i_obj_type = ms_item-obj_type ).

      CATCH zcx_abapgit_exception.
        zcx_abapgit_exception=>raise( 'DDLX Jump Error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey,
          lo_data_model TYPE REF TO if_wb_object_data_model,
          lv_text       TYPE string,
          lx_error      TYPE REF TO cx_root.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        get_persistence( )->delete( p_object_key = lv_object_key
                                    p_version    = swbm_version_active ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA: lv_object_key TYPE seu_objkey,
          lo_data_model TYPE REF TO if_wb_object_data_model,
          lr_data       TYPE REF TO data,
          lv_text       TYPE string,
          lx_error      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_data> TYPE any.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <ls_data>.

        CREATE OBJECT lo_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        get_persistence( )->get(
          EXPORTING
            p_object_key           = lv_object_key
            p_version              = swbm_version_active
          CHANGING
            p_object_data          = lo_data_model ).

        lo_data_model->get_data(
          IMPORTING
            p_data = <ls_data> ).

        clear_fields( CHANGING cs_data = <ls_data> ).

        io_xml->add( iv_name = 'DDLX'
                     ig_data = <ls_data> ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA: lo_data_model TYPE REF TO if_wb_object_data_model,
          lr_data       TYPE REF TO data,
          lv_text       TYPE string,
          lx_error      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_data> TYPE any.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <ls_data>.

        io_xml->read(
          EXPORTING
            iv_name = 'DDLX'
          CHANGING
            cg_data = <ls_data> ).

        CREATE OBJECT lo_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        lo_data_model->set_data( <ls_data> ).

        get_persistence( )->save( lo_data_model ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.


  METHOD get_persistence.

    TRY.
        IF mo_persistence IS NOT BOUND.

          CREATE OBJECT mo_persistence
                 TYPE ('CL_DDLX_ADT_OBJECT_PERSIST').

        ENDIF.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( `DDLX not supported` ).
    ENDTRY.

    ri_persistence = mo_persistence.

  ENDMETHOD.


  METHOD clear_fields.

    FIELD-SYMBOLS: <metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA'
           OF STRUCTURE cs_data
           TO <metadata>.
    ASSERT sy-subrc = 0.

    clear_field( EXPORTING iv_fieldname = 'CHANGED_AT'
                 CHANGING  cs_metadata  = <metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CHANGED_BY'
                 CHANGING  cs_metadata  = <metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_AT'
                 CHANGING  cs_metadata  = <metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_BY'
                 CHANGING  cs_metadata  = <metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cs_metadata  = <metadata> ).

    clear_field( EXPORTING iv_fieldname = 'PACKAGE_REF-NAME'
                 CHANGING  cs_metadata  = <metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CONTAINER_REF-PACKAGE_NAME'
                 CHANGING  cs_metadata  = <metadata> ).

  ENDMETHOD.


  METHOD clear_field.

    FIELD-SYMBOLS: <field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cs_metadata
           TO <field>.
    ASSERT sy-subrc = 0.

    CLEAR: <field>.

  ENDMETHOD.

ENDCLASS.
