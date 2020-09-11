INTERFACE lif_package_interface_facade.
  TYPES ty_tpak_package_interf_elem_tt TYPE STANDARD TABLE OF tpak_package_interf_elem_ref WITH DEFAULT KEY.

  METHODS:
    get_elements
      RETURNING
        VALUE(rt_elements) TYPE ty_tpak_package_interf_elem_tt
      RAISING
        zcx_abapgit_exception,

    set_elements_changeable
      IMPORTING
        VALUE(iv_changeable) TYPE abap_bool
      RAISING
        zcx_abapgit_exception,

    save_elements
      RAISING
        zcx_abapgit_exception,

    get_all_attributes
      RETURNING
        VALUE(rs_package_interface_data) TYPE scompidtln
      RAISING
        zcx_abapgit_exception,

    set_changeable
      IMPORTING
        VALUE(iv_changeable) TYPE abap_bool
      RAISING
        zcx_abapgit_exception,

    delete
      RAISING
        zcx_abapgit_exception,

    save
      RAISING
        zcx_abapgit_exception,

    remove_elements
      IMPORTING
        it_elements TYPE tpak_package_interf_elem_list
      RAISING
        zcx_abapgit_exception,

    add_elements
      IMPORTING
        it_elements_data TYPE scomeldata
      RAISING
        zcx_abapgit_exception,

    set_all_attributes
      IMPORTING
        is_package_interface_data TYPE scompidtln
        is_data_sign              TYPE scompisign
      RAISING
        zcx_abapgit_exception,

    get_changeable
      RETURNING
        VALUE(rv_changeable) TYPE abap_bool
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
