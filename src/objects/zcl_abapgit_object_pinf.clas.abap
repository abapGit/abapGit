CLASS zcl_abapgit_object_pinf DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_pinf,
             attributes TYPE scompidtln,
             elements   TYPE STANDARD TABLE OF scomeldtln WITH DEFAULT KEY,
           END OF ty_pinf.

    TYPES: ty_elements TYPE STANDARD TABLE OF tpak_package_interf_elem_ref WITH DEFAULT KEY.

    METHODS:
      create_or_load
        IMPORTING is_pinf             TYPE ty_pinf
                  iv_package          TYPE devclass
        RETURNING VALUE(ri_interface) TYPE REF TO if_package_interface
        RAISING   zcx_abapgit_exception,
      delete_elements
        IMPORTING ii_interface TYPE REF TO if_package_interface
        RAISING   zcx_abapgit_exception,
      update_attributes
        IMPORTING is_pinf      TYPE ty_pinf
                  ii_interface TYPE REF TO if_package_interface
        RAISING   zcx_abapgit_exception,
      update_elements
        IMPORTING is_pinf      TYPE ty_pinf
                  ii_interface TYPE REF TO if_package_interface
        RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_pinf IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM intf INTO rv_user
      WHERE intf_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lv_pack_name TYPE intf-pack_name,
          lv_main_pack TYPE tdevc-mainpack.


    SELECT SINGLE pack_name FROM intf INTO lv_pack_name
      WHERE intf_name = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

    IF rv_bool = abap_true.
      SELECT SINGLE mainpack FROM tdevc INTO lv_main_pack
        WHERE devclass = lv_pack_name.                  "#EC CI_GENBUFF
      rv_bool = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA: ls_pinf      TYPE ty_pinf,
          lv_name      TYPE scomifnam,
          lt_elements  TYPE ty_elements,
          li_interface TYPE REF TO if_package_interface.

    FIELD-SYMBOLS: <lg_any>     TYPE any,
                   <li_element> LIKE LINE OF lt_elements,
                   <ls_element> LIKE LINE OF ls_pinf-elements.


    lv_name = ms_item-obj_name.

    cl_package_interface=>load_package_interface(
      EXPORTING
        i_package_interface_name = lv_name
        i_force_reload           = abap_true
      IMPORTING
        e_package_interface      = li_interface ).

    li_interface->get_all_attributes(
      IMPORTING e_package_interface_data = ls_pinf-attributes ).

    CLEAR: ls_pinf-attributes-pack_name,
           ls_pinf-attributes-author,
           ls_pinf-attributes-created_by,
           ls_pinf-attributes-created_on,
           ls_pinf-attributes-changed_by,
           ls_pinf-attributes-changed_on,
           ls_pinf-attributes-tadir_devc.

* fields does not exist in older SAP versions
    ASSIGN COMPONENT 'SW_COMP_LOGICAL_PACKAGE' OF STRUCTURE ls_pinf-attributes TO <lg_any>.
    IF sy-subrc = 0.
      CLEAR <lg_any>.
    ENDIF.
    ASSIGN COMPONENT 'SW_COMP_TADIR_PACKAGE' OF STRUCTURE ls_pinf-attributes TO <lg_any>.
    IF sy-subrc = 0.
      CLEAR <lg_any>.
    ENDIF.

    li_interface->get_elements( IMPORTING e_elements = lt_elements ).

    LOOP AT lt_elements ASSIGNING <li_element>.
      APPEND INITIAL LINE TO ls_pinf-elements ASSIGNING <ls_element>.
      <li_element>->get_all_attributes( IMPORTING e_element_data = <ls_element> ).
      CLEAR <ls_element>-elem_pack.
    ENDLOOP.

    io_xml->add( ig_data = ls_pinf
                 iv_name = 'PINF' ).

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA: li_interface TYPE REF TO if_package_interface,
          ls_pinf      TYPE ty_pinf.


    io_xml->read( EXPORTING iv_name = 'PINF'
                  CHANGING cg_data = ls_pinf ).

    li_interface = create_or_load(
      is_pinf = ls_pinf
      iv_package = iv_package ).

    update_attributes(
      is_pinf      = ls_pinf
      ii_interface = li_interface ).

    update_elements(
      is_pinf      = ls_pinf
      ii_interface = li_interface ).

  ENDMETHOD.

  METHOD update_attributes.

    DATA: ls_sign       TYPE scompisign,
          lv_changeable TYPE flag.


    ii_interface->get_changeable( IMPORTING e_changeable = lv_changeable ).
    IF lv_changeable = abap_false.
* at creation the object is already in change mode
      ii_interface->set_changeable( abap_true ).
    ENDIF.

    ls_sign-descript       = abap_true.
    ls_sign-pinftype       = abap_true.
    ls_sign-restricted     = abap_true.
    ls_sign-default_if     = abap_true.
    ls_sign-def_sever      = abap_true.
    ls_sign-acl_flag       = abap_true.
    ls_sign-pifstablty     = abap_true.
    ls_sign-release_status = abap_true.

    ii_interface->set_all_attributes(
      i_package_interface_data = is_pinf-attributes
      i_data_sign              = ls_sign ).

* looks like setting "i_suppress_dialog = abap_true" will make
* it fail for local($) packages
    ii_interface->save( ).

    ii_interface->set_changeable( abap_false ).

  ENDMETHOD.

  METHOD update_elements.

    DATA: lt_existing TYPE ty_elements,
          lt_add      TYPE scomeldata,
          lv_index    TYPE i,
          lv_found    TYPE abap_bool,
*          ls_sign     TYPE scomelsign,
          ls_attr     TYPE scomeldtln.

    FIELD-SYMBOLS: <li_element> LIKE LINE OF lt_existing,
                   <ls_element> LIKE LINE OF is_pinf-elements.


*    ls_sign-usag_restr                 = abap_true.
*    ls_sign-stability                  = abap_true.
*    ls_sign-no_check                   = abap_true.
*    ls_sign-useastype                  = abap_true.
*    ls_sign-asforgnkey                 = abap_true.
*    ls_sign-deprecation_type           = abap_true. backport
*    ls_sign-replacement_object_type    = abap_true. backport
*    ls_sign-replacement_object_name    = abap_true. backport
*    ls_sign-replacement_subobject_type = abap_true. backport
*    ls_sign-replacement_subobject_name = abap_true. backport

    ii_interface->set_elements_changeable( abap_true ).

    ii_interface->get_elements( IMPORTING e_elements = lt_existing ).

    LOOP AT is_pinf-elements ASSIGNING <ls_element>.

      lv_found = abap_false.
      LOOP AT lt_existing ASSIGNING <li_element>.
        lv_index = sy-tabix.
        <li_element>->get_all_attributes( IMPORTING e_element_data = ls_attr ).
        IF <ls_element>-elem_type = ls_attr-elem_type
            AND <ls_element>-elem_key = ls_attr-elem_key.
          DELETE lt_existing INDEX lv_index.
          CONTINUE. " current loop
        ENDIF.
      ENDLOOP.

      IF lv_found = abap_false.
        APPEND <ls_element> TO lt_add.
      ENDIF.
    ENDLOOP.

    ii_interface->remove_elements( lt_existing ).

    ii_interface->add_elements( lt_add ).

    ii_interface->save_elements( ).

    ii_interface->set_elements_changeable( abap_false ).

  ENDMETHOD.

  METHOD create_or_load.

    IF zif_abapgit_object~exists( ) = abap_false.
      cl_package_interface=>create_new_package_interface(
        EXPORTING
          i_pkg_interface_name    = is_pinf-attributes-intf_name
          i_publisher_pkg_name    = iv_package
        IMPORTING
          e_package_interface     = ri_interface
        EXCEPTIONS
          object_already_existing = 1
          object_just_created     = 2
          interface_name_invalid  = 3
          unexpected_error        = 4
          OTHERS                  = 7 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error creating new package interface' ).
      ENDIF.
    ELSE.
      cl_package_interface=>load_package_interface(
        EXPORTING
          i_package_interface_name   = is_pinf-attributes-intf_name
          i_force_reload             = abap_true
        IMPORTING
          e_package_interface        = ri_interface
        EXCEPTIONS
          db_read_error              = 1
          unexpected_error           = 2
          object_not_existing        = 3
          shorttext_not_existing     = 4
          object_locked_and_modified = 5
          OTHERS                     = 6 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error loading package interface' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD delete_elements.

    DATA: lt_elements TYPE ty_elements.

    FIELD-SYMBOLS: <li_element> LIKE LINE OF lt_elements.


    ii_interface->set_elements_changeable( abap_true ).

    ii_interface->get_elements( IMPORTING e_elements = lt_elements ).

    LOOP AT lt_elements ASSIGNING <li_element>.
      <li_element>->delete( ).
    ENDLOOP.

    ii_interface->save_elements( ).

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.

    DATA: lv_name      TYPE scomifnam,
          li_interface TYPE REF TO if_package_interface.


    lv_name = ms_item-obj_name.

    cl_package_interface=>load_package_interface(
      EXPORTING
        i_package_interface_name   = lv_name
      IMPORTING
        e_package_interface        = li_interface
      EXCEPTIONS
        db_read_error              = 1
        unexpected_error           = 2
        object_not_existing        = 3
        shorttext_not_existing     = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error loading package interface, delete' ).
    ENDIF.

* elements must be deleted before the package interface
* can be deleted
    delete_elements( li_interface ).

    li_interface->set_changeable( abap_true ).

    li_interface->delete( ).

    li_interface->save( ).

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PINF'
        in_new_window = abap_true.

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.

ENDCLASS.
