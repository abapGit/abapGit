CLASS zcl_abapgit_persist_packages DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_persist_factory.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_packages.

    METHODS init.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_packages TYPE zif_abapgit_persist_packages=>ty_packages.

    METHODS from_xml
      IMPORTING
        iv_xml             TYPE string
      RETURNING
        VALUE(rt_packages) TYPE zif_abapgit_persist_packages=>ty_packages
      RAISING
        zcx_abapgit_exception.
    METHODS to_xml
      IMPORTING
        it_packages   TYPE zif_abapgit_persist_packages=>ty_packages
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_persist_packages IMPLEMENTATION.


  METHOD from_xml.

    DATA lo_input TYPE REF TO zif_abapgit_xml_input.

    CREATE OBJECT lo_input TYPE zcl_abapgit_xml_input EXPORTING iv_xml = iv_xml.

    lo_input->read(
      EXPORTING
        iv_name = zcl_abapgit_persistence_db=>c_type_packages
      CHANGING
        cg_data = rt_packages ).

  ENDMETHOD.


  METHOD init.

    TRY.
        " Might have changed in another session so always get latest
        mt_packages = from_xml( zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = zcl_abapgit_persistence_db=>c_type_packages
          iv_value = '' ) ).
      CATCH zcx_abapgit_exception zcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD to_xml.

    DATA li_output TYPE REF TO zif_abapgit_xml_output.

    CREATE OBJECT li_output TYPE zcl_abapgit_xml_output.

    li_output->add(
      iv_name = zcl_abapgit_persistence_db=>c_type_packages
      ig_data = it_packages ).

    rv_xml = li_output->render( ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_packages~modify.

    DATA ls_package LIKE LINE OF mt_packages.

    FIELD-SYMBOLS <ls_package> LIKE LINE OF mt_packages.

    init( ).

    IF iv_component IS INITIAL AND iv_comp_posid IS INITIAL.
      DELETE mt_packages WHERE devclass = iv_package.
    ELSE.
      READ TABLE mt_packages ASSIGNING <ls_package> WITH TABLE KEY devclass = iv_package.
      IF sy-subrc = 0.
        <ls_package>-component  = iv_component.
        <ls_package>-comp_posid = iv_comp_posid.
      ELSE.
        ls_package-devclass   = iv_package.
        ls_package-component  = iv_component.
        ls_package-comp_posid = iv_comp_posid.
        INSERT ls_package INTO TABLE mt_packages.
      ENDIF.
    ENDIF.

    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = zcl_abapgit_persistence_db=>c_type_packages
      iv_value      = ''
      iv_data       = to_xml( mt_packages ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_abapgit_persist_packages~read.

    init( ).

    READ TABLE mt_packages INTO rs_package WITH TABLE KEY devclass = iv_package.
    IF sy-subrc <> 0.
      rs_package-devclass = iv_package. " no component
    ENDIF.

  ENDMETHOD.
ENDCLASS.
