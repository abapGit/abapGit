CLASS zcl_abapgit_object_doct DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_id TYPE dokhl-id VALUE 'TX' ##NO_TEXT.
    CONSTANTS c_name TYPE string VALUE 'DOC' ##NO_TEXT.
    DATA mi_longtexts TYPE REF TO zif_abapgit_longtexts .
ENDCLASS.



CLASS zcl_abapgit_object_doct IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mi_longtexts = zcl_abapgit_factory=>get_longtexts( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = mi_longtexts->changed_by(
                  iv_object_name = ms_item-obj_name
                  iv_longtext_id = c_id ).

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    mi_longtexts->delete(
        iv_object_name = ms_item-obj_name
        iv_longtext_id = c_id ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    mi_longtexts->deserialize(
      iv_longtext_name = c_name
      iv_object_name   = ms_item-obj_name
      iv_longtext_id   = c_id
      ii_xml           = io_xml
      iv_main_language = mv_language ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_id     TYPE dokil-id,
          lv_object TYPE dokhl-object.


    lv_object = ms_item-obj_name.

    SELECT SINGLE id FROM dokil INTO lv_id
      WHERE id         = c_id
        AND object     = lv_object.                     "#EC CI_GENBUFF

    rv_bool = boolc( sy-subrc = 0 ).

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

    DATA: ls_dokentry TYPE dokentry,
          ls_bcdata   TYPE bdcdata,
          lt_bcdata   TYPE STANDARD TABLE OF bdcdata.

    " We need to modify dokentry directly, otherwise
    " Batch Input on SE61 wouldn't work because it stores
    " the last seen Document Class in this table. There's
    " no standard function to do this. SE61 does this
    " directly in its dialog modules
    ls_dokentry-username = sy-uname.
    ls_dokentry-langu    = mv_language.
    ls_dokentry-class    = c_id.
    MODIFY dokentry FROM ls_dokentry.

    ls_bcdata-program  = 'SAPMSDCU'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSDCU-OBJECT7'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=SHOW'.
    APPEND ls_bcdata TO lt_bcdata.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE61'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    mi_longtexts->serialize(
        iv_longtext_name = c_name
        iv_object_name = ms_item-obj_name
        iv_longtext_id = c_id
        ii_xml         = io_xml ).

  ENDMETHOD.
ENDCLASS.
