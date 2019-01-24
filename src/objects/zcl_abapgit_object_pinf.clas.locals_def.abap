INTERFACE lif_package_interface_facade.

  METHODS:
    get_elements
      IMPORTING
        iv_with_deleted_elements TYPE flag DEFAULT 'X'
      EXPORTING
        et_elements              TYPE tpak_package_interf_elem_list
      RAISING
        zcx_abapgit_exception,

    set_elements_changeable
      IMPORTING
        VALUE(iv_changeable) TYPE flag
      RAISING
        zcx_abapgit_exception,

    save_elements
      IMPORTING
        iv_transport_request TYPE trkorr OPTIONAL
        iv_suppress_dialog   TYPE flag DEFAULT ' '
      RAISING
        zcx_abapgit_exception,

    get_all_attributes
      EXPORTING
        es_package_interface_data TYPE scompidtln
      RAISING
        zcx_abapgit_exception,

    set_changeable
      IMPORTING
        VALUE(iv_changeable) TYPE flag
      RAISING
        zcx_abapgit_exception,

    delete
      IMPORTING
        iv_suppress_dialog TYPE flag DEFAULT abap_false
      RAISING
        zcx_abapgit_exception,

    save
      IMPORTING
        iv_transport_request TYPE trkorr OPTIONAL
        iv_suppress_dialog   TYPE flag DEFAULT ' '
      RAISING
        zcx_abapgit_exception,

    remove_elements
      IMPORTING
        it_elements TYPE tpak_package_interf_elem_list
      RAISING
        zcx_abapgit_exception,

    add_elements
      IMPORTING
        is_elements_data TYPE scomeldata
      RAISING
        zcx_abapgit_exception,

    set_all_attributes
      IMPORTING
        is_package_interface_data TYPE scompidtln
        is_data_sign              TYPE scompisign
      RAISING
        zcx_abapgit_exception,

    get_changeable
      EXPORTING
        VALUE(ev_changeable) TYPE flag
      RAISING
        zcx_abapgit_exception.

ENDINTERFACE.

CLASS lcl_package_interface_facade DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      lif_package_interface_facade.

    METHODS:
      constructor
        IMPORTING
          ii_interface TYPE REF TO if_package_interface.

  PRIVATE SECTION.
    DATA: mi_interface TYPE REF TO if_package_interface.

ENDCLASS.
