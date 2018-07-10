class ZCL_ABAPGIT_OBJECT_AVAS definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .
PROTECTED SECTION.

  TYPES: BEGIN OF ty_header,
           guid      TYPE guid_32,
           attribute TYPE cls_attribute_name,
           object    TYPE pak_object_key,
         END OF ty_header.

  TYPES: BEGIN OF ty_avas,
           header TYPE ty_header,
           values TYPE cls_value_assignments,
           links  TYPE cls_linked_objects,
         END OF ty_avas.

  METHODS instantiate
    RETURNING
      VALUE(ro_avas) TYPE REF TO cl_cls_attr_value_assignment .
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_AVAS IMPLEMENTATION.


  METHOD instantiate.

    DATA: lv_id TYPE guid_32.


    lv_id = ms_item-obj_name.

    TRY.
        CREATE OBJECT ro_avas
          EXPORTING
*           im_object        =
*           im_attribute     =
            im_assignment_id = lv_id
*           im_package       =
*           im_cache         =
*           im_report_run_result    =
          .
      CATCH cx_pak_wb_object_locked.
        BREAK-POINT.
      CATCH cx_pak_not_authorized.
        BREAK-POINT.
      CATCH cx_pak_invalid_state.
        BREAK-POINT.
      CATCH cx_pak_invalid_data.
        BREAK-POINT.
    ENDTRY.


  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment.


    lo_avas = instantiate( ).

    lo_avas->if_pak_wb_object~get_last_changed(
      IMPORTING
        ex_changed_by = rv_user ).

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment.


    lo_avas = instantiate( ).

    TRY.
        lo_avas->if_cls_attr_value_assignment~lock_and_refresh(
          im_allow_popups = abap_false ).
      CATCH cx_pak_invalid_state
          cx_pak_invalid_data
          cx_pak_not_authorized
          cx_pak_wb_object_locked.
        BREAK-POINT.
    ENDTRY.

    lo_avas->if_pak_wb_object~delete( ).

    lo_avas->if_pak_wb_object~save( ).

    lo_avas->if_pak_wb_object_internal~unlock( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment,
          ls_avas TYPE ty_avas.


    io_xml->read( EXPORTING iv_name = 'AVAS'
                  CHANGING cg_data = ls_avas ).

    tadir_insert( iv_package ).

* todo, this creates a new GUID
    TRY.
        CREATE OBJECT lo_avas
          EXPORTING
            im_object    = ls_avas-header-object
            im_attribute = ls_avas-header-attribute
            im_package   = iv_package.

        IF lo_avas->if_cls_attr_value_assignment~is_initialized( ) = abap_false.
          lo_avas->if_cls_attr_value_assignment~create( ).
        ENDIF.

        lo_avas->if_cls_attr_value_assignment~lock_and_refresh(
          im_allow_popups = abap_false ).
      CATCH cx_pak_wb_object_locked.
        zcx_abapgit_exception=>raise( |AVAS locked, deserialize| ).
    ENDTRY.

    lo_avas->if_cls_attr_value_assignment~set_values(
      CHANGING ch_values = ls_avas-values ).

    lo_avas->if_cls_attr_value_assignment~set_links(
      CHANGING ch_links = ls_avas-links ).

* set default package, see function module RS_CORR_INSERT
    SET PARAMETER ID 'EUK' FIELD iv_package.

    lo_avas->if_pak_wb_object~save( ).

    SET PARAMETER ID 'EUK' FIELD ''.

    lo_avas->if_pak_wb_object~activate( ).

    lo_avas->if_pak_wb_object_internal~unlock( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_guid TYPE cls_assignment-guid.

    SELECT SINGLE guid FROM cls_assignment INTO lv_guid
      WHERE guid = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
* todo
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    zcx_abapgit_exception=>raise( |Todo, AVAS jump| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment,
          ls_avas TYPE ty_avas.

    FIELD-SYMBOLS: <ls_value> LIKE LINE OF ls_avas-values,
                   <ls_link>  LIKE LINE OF ls_avas-links.


    lo_avas = instantiate( ).

    ls_avas-header-guid = lo_avas->if_cls_attr_value_assignment~get_guid( ).

    ls_avas-header-attribute = lo_avas->if_cls_attr_value_assignment~get_attribute( ).

    ls_avas-header-object = lo_avas->if_cls_attr_value_assignment~get_object( ).

    lo_avas->if_cls_attr_value_assignment~get_values(
      IMPORTING
        ex_values = ls_avas-values ).

    lo_avas->if_cls_attr_value_assignment~get_links(
      IMPORTING
        ex_links = ls_avas-links ).

    LOOP AT ls_avas-values ASSIGNING <ls_value>.
      CLEAR: <ls_value>-set_by,
             <ls_value>-changed_on.
    ENDLOOP.

    LOOP AT ls_avas-links ASSIGNING <ls_link>.
      CLEAR: <ls_link>-set_by,
             <ls_link>-changed_on.
    ENDLOOP.

    io_xml->add(
      iv_name = 'AVAS'
      ig_data = ls_avas ).

  ENDMETHOD.
ENDCLASS.
