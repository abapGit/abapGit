CLASS zcl_abapgit_package_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS update
      IMPORTING
        !it_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_package_tree IMPLEMENTATION.


  METHOD update.

    DATA: lv_package LIKE LINE OF it_packages,
          lv_tree    TYPE string.

    LOOP AT it_packages INTO lv_package.
* update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
