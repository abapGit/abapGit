*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_prag
*&---------------------------------------------------------------------*

CLASS lcl_object_prag DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.

CLASS lcl_object_prag IMPLEMENTATION.

  METHOD lif_object~changed_by.

    lcx_exception=>raise( 'PRAG not implemented' ).

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    lcx_exception=>raise( 'PRAG not implemented' ).

  ENDMETHOD.

  METHOD lif_object~delete.

    lcx_exception=>raise( 'PRAG not implemented' ).

  ENDMETHOD.

  METHOD lif_object~deserialize.

    lcx_exception=>raise( 'PRAG not implemented' ).

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA(persist) = NEW cl_wb_abpr_persist( ).

    TRY.
        persist->if_wb_object_persist~get( p_object_key 					= |{ ms_item-obj_name }|     " Object Key
                                           p_version							= 'A'    " Version (Active/Inactive)
                                           p_existence_check_only = abap_true     " Perform Existence Check Only (no existence -> exception)
        ).

      CATCH cx_swb_exception INTO DATA(error).
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.


*    DATA: object_data TYPE REF TO if_wb_object_data_model.
*
*    DATA(persist) = NEW cl_wb_abpr_persist( ).
*
*    TRY.
*        persist->if_wb_object_persist~get(
*          EXPORTING
*            p_object_key                 = |{ ms_item-obj_name }|     " Object Key
*            p_version                    = 'A'    " Version (Active/Inactive)
**        p_language                   = SY-LANGU    " Language Key (Important: must not trigger exception)
**        p_if_none_match              =
**        p_existence_check_only       = ABAP_FALSE    " Perform Existence Check Only (no existence -> exception)
**        p_data_selection             = C_ALL_DATA    " Selection (or Filter) of Object Data
**        p_wb_rest                    =
**      IMPORTING
**        p_langu_is_not_maintained    =     " 'X': No data exists in specified language
**        p_etag                       =
**        p_other_existing_versions    =
*      CHANGING
*        p_object_data                = object_data
*        ).
*
*      CATCH cx_swb_object_does_not_exist cx_swb_exception INTO DATA(error).
*        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*    ENDTRY.
*
*    DATA(ui) = NEW cl_wb_abpr_ui( ).
*
*    DATA(tool) = NEW cl_wb_abpr_tool(
*        p_ui      = ui
*        p_persist = persist
*    ).
*
*    DATA(tool_data) = NEW cl_wb_generic_tool_data( ).
*
*    tool_data->if_wb_tool_data_model~set_edit_mode( 'EDIT' ).
*
*    ui->if_wb_tool_ui~set_data(
*      EXPORTING
*        p_object_data = object_data     " Object Data
*        p_tool_data   = tool_data
*    ).
*
*    ui->if_wb_tool_ui~start(
*      EXPORTING
*        p_tool_ref     = tool    " Reference to WB Tool
*      EXCEPTIONS
*        error_occurred = 1
*        OTHERS         = 2 ).

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'    " Operation
        object_name         = ms_item-obj_name    " Object Name
        object_type         = ms_item-obj_type    " Object Type
*        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

  ENDMETHOD.

  METHOD lif_object~serialize.

    lcx_exception=>raise( 'PRAG not implemented' ).

  ENDMETHOD.

ENDCLASS.
