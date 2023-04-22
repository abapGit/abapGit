CLASS zcl_abapgit_object_pinf DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_pinf,
        attributes TYPE scompidtln,
        elements   TYPE STANDARD TABLE OF scomeldtln WITH DEFAULT KEY,
      END OF ty_pinf .
    TYPES:
      ty_elements TYPE STANDARD TABLE OF tpak_package_interf_elem_ref WITH DEFAULT KEY .

    METHODS create_or_load
      IMPORTING
        !is_pinf            TYPE ty_pinf
        !iv_package         TYPE devclass
      RETURNING
        VALUE(ri_interface) TYPE REF TO lif_package_interface_facade
      RAISING
        zcx_abapgit_exception .
    METHODS delete_elements
      IMPORTING
        !ii_interface TYPE REF TO lif_package_interface_facade
      RAISING
        zcx_abapgit_exception .
    METHODS update_attributes
      IMPORTING
        !iv_package   TYPE devclass
        !is_pinf      TYPE ty_pinf
        !ii_interface TYPE REF TO lif_package_interface_facade
      RAISING
        zcx_abapgit_exception .
    METHODS update_elements
      IMPORTING
        !iv_package   TYPE devclass
        !is_pinf      TYPE ty_pinf
        !ii_interface TYPE REF TO lif_package_interface_facade
      RAISING
        zcx_abapgit_exception .
    METHODS load
      IMPORTING
        iv_name             TYPE scomifnam
      RETURNING
        VALUE(ri_interface) TYPE REF TO lif_package_interface_facade.
    METHODS create_facade
      IMPORTING
        ii_interface     TYPE REF TO if_package_interface
      RETURNING
        VALUE(ri_facade) TYPE REF TO lif_package_interface_facade.

ENDCLASS.



CLASS zcl_abapgit_object_pinf IMPLEMENTATION.


  METHOD create_facade.

    CREATE OBJECT ri_facade TYPE lcl_package_interface_facade
      EXPORTING
        ii_interface = ii_interface.

  ENDMETHOD.


  METHOD create_or_load.

    DATA: li_interface          TYPE REF TO if_package_interface,
          lv_pkg_interface_data TYPE scompidtln.

    lv_pkg_interface_data-default_if = is_pinf-attributes-default_if.
    lv_pkg_interface_data-tadir_devc = iv_package.

    "Important if the package name comes from another package
    IF is_pinf-attributes-pack_name IS INITIAL.
      lv_pkg_interface_data-pack_name = iv_package.
    ELSE.
      lv_pkg_interface_data-pack_name = is_pinf-attributes-pack_name.
    ENDIF.

    IF zif_abapgit_object~exists( ) = abap_false.
      cl_package_interface=>create_new_package_interface(
        EXPORTING
          i_pkg_interface_name    = is_pinf-attributes-intf_name
          i_publisher_pkg_name    = lv_pkg_interface_data-pack_name
          i_pkg_interface_data    = lv_pkg_interface_data
        IMPORTING
          e_package_interface     = li_interface
        EXCEPTIONS
          object_already_existing = 1
          object_just_created     = 2
          interface_name_invalid  = 3
          unexpected_error        = 4
          OTHERS                  = 7 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error creating new package interface' ).
      ENDIF.

      ri_interface = create_facade( li_interface ).

    ELSE.

      ri_interface = load( is_pinf-attributes-intf_name ).

    ENDIF.

  ENDMETHOD.


  METHOD delete_elements.

    DATA: lt_elements TYPE ty_elements.

    FIELD-SYMBOLS: <li_element> LIKE LINE OF lt_elements.


    ii_interface->set_elements_changeable( abap_true ).

    lt_elements = ii_interface->get_elements( ).

    LOOP AT lt_elements ASSIGNING <li_element>.
      <li_element>->delete( ).
    ENDLOOP.

    ii_interface->save_elements( ).

  ENDMETHOD.


  METHOD load.

    DATA: li_interface TYPE REF TO  if_package_interface.

    cl_package_interface=>load_package_interface(
      EXPORTING
        i_package_interface_name = iv_name
        i_force_reload           = abap_true
      IMPORTING
        e_package_interface      = li_interface ).

    ri_interface = create_facade( li_interface ).

  ENDMETHOD.


  METHOD update_attributes.

    DATA: ls_sign       TYPE scompisign,
          lv_changeable TYPE abap_bool.


    lv_changeable = ii_interface->get_changeable( ).
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
      is_package_interface_data = is_pinf-attributes
      is_data_sign              = ls_sign ).

    set_default_package( iv_package ).
* looks like setting "i_suppress_dialog = abap_true" will make
* it fail for local($) packages
    ii_interface->save( ).

    ii_interface->set_changeable( abap_false ).

  ENDMETHOD.


  METHOD update_elements.

    DATA: lt_existing TYPE ty_elements,
          ls_element  LIKE LINE OF is_pinf-elements,
          lt_add      TYPE scomeldata,
          lv_index    TYPE i,
          lv_found    TYPE abap_bool,
          ls_attr     TYPE scomeldtln.

    FIELD-SYMBOLS <li_element> LIKE LINE OF lt_existing.

    ii_interface->set_elements_changeable( abap_true ).

    lt_existing = ii_interface->get_elements( ).

    LOOP AT is_pinf-elements INTO ls_element.

      lv_found = abap_false.
      LOOP AT lt_existing ASSIGNING <li_element>.
        lv_index = sy-tabix.
        <li_element>->get_all_attributes( IMPORTING e_element_data = ls_attr ).
        IF ls_element-elem_type = ls_attr-elem_type
            AND ls_element-elem_key = ls_attr-elem_key.
          DELETE lt_existing INDEX lv_index.
          CONTINUE. " current loop
        ENDIF.
      ENDLOOP.

      IF lv_found = abap_false.
        ls_element-elem_pack = iv_package.
        APPEND ls_element TO lt_add.
      ENDIF.
    ENDLOOP.

    ii_interface->remove_elements( lt_existing ).

    ii_interface->add_elements( lt_add ).

    ii_interface->save_elements( ).

    ii_interface->set_elements_changeable( abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM intf INTO rv_user
      WHERE intf_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: li_interface TYPE REF TO lif_package_interface_facade.

    corr_insert( iv_package ).

    li_interface = load( |{ ms_item-obj_name }| ).

* elements must be deleted before the package interface
* can be deleted
    delete_elements( li_interface ).

    li_interface->set_changeable( abap_true ).

    li_interface->delete( ).

    li_interface->save( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: li_interface TYPE REF TO lif_package_interface_facade,
          ls_pinf      TYPE ty_pinf.


    io_xml->read( EXPORTING iv_name = 'PINF'
                  CHANGING cg_data = ls_pinf ).

    "needed for update_attributes
    ls_pinf-attributes-tadir_devc = iv_package.

    li_interface = create_or_load(
      is_pinf    = ls_pinf
      iv_package = iv_package ).

    update_attributes(
      iv_package   = iv_package
      is_pinf      = ls_pinf
      ii_interface = li_interface ).

    update_elements(
      iv_package   = iv_package
      is_pinf      = ls_pinf
      ii_interface = li_interface ).

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

    DATA: lv_argument TYPE eqegraarg.

    lv_argument = |PF{ ms_item-obj_name }|.
    OVERLAY lv_argument WITH '                                          *'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_pinf      TYPE ty_pinf,
          lt_elements  TYPE ty_elements,
          li_interface TYPE REF TO lif_package_interface_facade.

    FIELD-SYMBOLS: <lg_any>     TYPE any,
                   <li_element> LIKE LINE OF lt_elements,
                   <ls_element> LIKE LINE OF ls_pinf-elements.

    li_interface = load( |{ ms_item-obj_name }| ).

    ls_pinf-attributes = li_interface->get_all_attributes( ).

    "Delete the package name if it comes from the same package
    IF ls_pinf-attributes-tadir_devc = ls_pinf-attributes-pack_name OR
      ms_item-devclass = ls_pinf-attributes-pack_name.
      CLEAR ls_pinf-attributes-pack_name.
    ENDIF.

    CLEAR: ls_pinf-attributes-author,
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

    lt_elements = li_interface->get_elements( ).

    LOOP AT lt_elements ASSIGNING <li_element>.
      APPEND INITIAL LINE TO ls_pinf-elements ASSIGNING <ls_element>.
      <li_element>->get_all_attributes( IMPORTING e_element_data = <ls_element> ).
      CLEAR <ls_element>-elem_pack.
    ENDLOOP.

    io_xml->add( ig_data = ls_pinf
                 iv_name = 'PINF' ).

  ENDMETHOD.
ENDCLASS.
