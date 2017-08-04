*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_dial
*&---------------------------------------------------------------------*


CLASS lcl_object_dial DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_dialog_module,
             tdct     TYPE tdct,
             dia_pars TYPE STANDARD TABLE OF diapar
                           WITH NON-UNIQUE DEFAULT KEY,
           END OF ty_dialog_module.

    METHODS:
      _read_tdct
        RETURNING
          VALUE(rs_tdct) TYPE tdct.

ENDCLASS.

CLASS lcl_object_dial IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: ls_tdct TYPE tdct.

    ls_tdct = _read_tdct( ).

    rv_bool = boolc( ls_tdct IS NOT INITIAL ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: ls_dialog_module TYPE ty_dialog_module.

    ls_dialog_module-tdct = _read_tdct( ).

    SELECT * FROM diapar
             INTO TABLE ls_dialog_module-dia_pars
             WHERE dnam = ls_dialog_module-tdct-dnam.

    io_xml->add( iv_name = 'DIAL'
                 ig_data = ls_dialog_module ).

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: ls_dialog_module TYPE ty_dialog_module.

    io_xml->read(
      EXPORTING
        iv_name = 'DIAL'
      CHANGING
        cg_data = ls_dialog_module ).

    CALL FUNCTION 'RS_DIALOG_CREATE'
      EXPORTING
        dialogname            = ls_dialog_module-tdct-dnam
        dynpronumber          = ls_dialog_module-tdct-dynr
        programname           = ls_dialog_module-tdct-prog
*     It seems that dia_par parameter doesn't do anything, but we can't omit it
*     Parameters are inserted below
      TABLES
        dia_par               = ls_dialog_module-dia_pars
      EXCEPTIONS
        dialog_already_exists = 1
        invalid_name          = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error deserializing dialogmodule { ms_item-obj_name }| ).
    ENDIF.

    " It seems that there's no API for diapar, therefore we manipulate it directly
    INSERT diapar FROM TABLE ls_dialog_module-dia_pars.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: ls_tdct TYPE tdct.

    " We don't use RS_DIALOG_DELETE because it interacts with the GUI

    ls_tdct = _read_tdct( ).

    PERFORM fu_delete_dialog IN PROGRAM sapmsdia USING ls_tdct.

  ENDMETHOD.


  METHOD lif_object~jump.

    DATA: objectname TYPE tdct-dnam.

    objectname = ms_item-obj_name.

    CALL FUNCTION 'RS_DIALOG_SHOW'
      EXPORTING
        objectname       = objectname
        type             = 'VW'
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from RS_DIALOG_SHOW, DIAL| ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.


  METHOD _read_tdct.

    DATA: dnam TYPE tdct-dnam.

    dnam = ms_item-obj_name.

    SELECT SINGLE * FROM tdct
           INTO rs_tdct
           WHERE dnam = dnam.

  ENDMETHOD.

ENDCLASS.
