*&---------------------------------------------------------------------*
*& Include zabapgit_object_ddlx
*&---------------------------------------------------------------------*

CLASS lcl_object_ddlx DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

    DATA: mo_persistence TYPE REF TO if_wb_object_persist.

ENDCLASS.

CLASS lcl_object_ddlx IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mo_persistence
      TYPE ('CL_DDLX_ADT_OBJECT_PERSIST').

  ENDMETHOD.

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

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    rv_bool = abap_true.

    TRY.
        mo_persistence->get( p_object_key           = lv_object_key
                             p_version              = 'A'
                             p_existence_check_only = abap_true ).

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~jump.

    TRY.

        jump_adt( i_obj_name = ms_item-obj_name
                  i_obj_type = ms_item-obj_type ).

      CATCH zcx_abapgit_exception.
        zcx_abapgit_exception=>raise( 'DDLX Jump Error' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: lv_object_key TYPE seu_objkey,
          lo_data_model TYPE REF TO if_wb_object_data_model,
          lv_text       TYPE string,
          lx_error      TYPE REF TO cx_root.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        mo_persistence->delete( p_object_key = lv_object_key
                                p_version    = 'A' ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lv_object_key TYPE seu_objkey,
          lo_data_model TYPE REF TO if_wb_object_data_model,
          ls_data       TYPE cl_ddlx_wb_object_data=>ty_object_data,
          lv_text       TYPE string,
          lx_error      TYPE REF TO cx_root.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        mo_persistence->get(
          EXPORTING
            p_object_key           = lv_object_key
            p_version              = 'A'
          CHANGING
            p_object_data          = lo_data_model ).

        lo_data_model->get_data(
          IMPORTING
            p_data = ls_data ).

        CLEAR: ls_data-metadata-changed_at,
               ls_data-metadata-changed_by,
               ls_data-metadata-created_at,
               ls_data-metadata-created_by,
               ls_data-metadata-package_ref-name,
               ls_data-metadata-container_ref-package_name.

        io_xml->add( iv_name = 'DDLX'
                     ig_data = ls_data ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lo_data_model TYPE REF TO if_wb_object_data_model,
          ls_data       TYPE cl_ddlx_wb_object_data=>ty_object_data,
          lv_text       TYPE string,
          lx_error      TYPE REF TO cx_root.

    io_xml->read(
      EXPORTING
        iv_name = 'DDLX'
      CHANGING
        cg_data = ls_data ).

    TRY.
        CREATE OBJECT lo_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        lo_data_model->set_data( ls_data ).

        mo_persistence->save( lo_data_model ).

      CATCH cx_root INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.

ENDCLASS.
