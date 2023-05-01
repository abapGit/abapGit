CLASS zcl_abapgit_object_dial DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
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



CLASS zcl_abapgit_object_dial IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSDIA'.
    ls_bcdata-dynpro   = '1010'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'DIAPAR-DNAM'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RS38L-PARM'.
    ls_bcdata-fval     = abap_true.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DELF'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-program  = 'SAPLSPO1'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=YES'.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-program  = 'SAPMSDIA'.
    ls_bcdata-dynpro   = '1010'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=BACK'.
    APPEND ls_bcdata TO lt_bcdata.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SE35'
      it_bdcdata    = lt_bcdata
      iv_new_window = abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

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
        suppress_corr_check   = abap_false
*     It seems that dia_par parameter doesn't do anything, but we can't omit it
*     Parameters are inserted below
      TABLES
        dia_par               = ls_dialog_module-dia_pars
      EXCEPTIONS
        dialog_already_exists = 1
        invalid_name          = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deserializing dialogmodule { ms_item-obj_name }| ).
    ENDIF.

    " It seems that there's no API for diapar, therefore we manipulate it directly
    INSERT diapar FROM TABLE ls_dialog_module-dia_pars.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_tdct TYPE tdct.

    ls_tdct = _read_tdct( ).

    rv_bool = boolc( ls_tdct IS NOT INITIAL ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lv_objectname TYPE tdct-dnam.

    lv_objectname = ms_item-obj_name.

    CALL FUNCTION 'RS_DIALOG_SHOW'
      EXPORTING
        objectname       = lv_objectname
        type             = 'VW'
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA ls_dialog_module TYPE ty_dialog_module.

    ls_dialog_module-tdct = _read_tdct( ).

    SELECT * FROM diapar
      INTO TABLE ls_dialog_module-dia_pars
      WHERE dnam = ls_dialog_module-tdct-dnam
      ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'DIAL'
                 ig_data = ls_dialog_module ).

  ENDMETHOD.


  METHOD _read_tdct.

    DATA: lv_dnam TYPE tdct-dnam.

    lv_dnam = ms_item-obj_name.

    SELECT SINGLE * FROM tdct
           INTO rs_tdct
           WHERE dnam = lv_dnam.

  ENDMETHOD.
ENDCLASS.
