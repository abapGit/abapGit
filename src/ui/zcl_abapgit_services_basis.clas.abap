CLASS zcl_abapgit_services_basis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create_package
      IMPORTING
        iv_prefill_package TYPE devclass OPTIONAL
      RETURNING
        VALUE(rv_package) TYPE devclass
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS test_changed_by
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_SERVICES_BASIS IMPLEMENTATION.


  METHOD create_package.

    DATA ls_package_data TYPE scompkdtln.
    DATA lv_create       TYPE abap_bool.

    ls_package_data-devclass = to_upper( iv_prefill_package ).

    zcl_abapgit_ui_factory=>get_popups( )->popup_to_create_package(
      IMPORTING
        es_package_data = ls_package_data
        ev_create       = lv_create ).

    IF lv_create = abap_true.
      zcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
      rv_package = ls_package_data-devclass.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD test_changed_by.

    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.
    DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_user  TYPE xubname.

    ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.

    lv_user = zcl_abapgit_objects=>changed_by( ls_item ).

    MESSAGE lv_user TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
