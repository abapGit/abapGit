CLASS ltcl_sap_package DEFINITION FOR TESTING.

  PUBLIC SECTION.
    TYPES:
      ty_package TYPE STANDARD TABLE OF devclass
                                WITH NON-UNIQUE DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING
          iv_package TYPE devclass,

      set_sub_packages
        IMPORTING
          it_sub_packages TYPE ty_package.

    INTERFACES: zif_abapgit_sap_package.

  PRIVATE SECTION.
    DATA: mv_package      TYPE devclass,
          mt_sub_packages TYPE ty_package.

ENDCLASS.

CLASS ltcl_sap_package IMPLEMENTATION.

  METHOD constructor.

    mv_package = iv_package.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~validate_name.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_description.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_responsible.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_subpackages.

    IF mv_package = 'Z_MAIN'.

      rt_list = mt_sub_packages.

    ENDIF.

  ENDMETHOD.


  METHOD set_sub_packages.

    mt_sub_packages = it_sub_packages.

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~are_changes_recorded_in_tr_req. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_child. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~create_local. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~exists. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_type. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~get_transport_layer. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~list_superpackages. "##needed

  ENDMETHOD.

  METHOD zif_abapgit_sap_package~read_parent. "##needed

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_resolve_packages DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mt_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt,
      mt_sub_packages TYPE ltcl_sap_package=>ty_package.

    METHODS:
      resolve_single FOR TESTING RAISING cx_static_check,

      given_tadir
        IMPORTING
          iv_object        TYPE tadir-object
          iv_obj_name      TYPE tadir-obj_name
          iv_korrnum       TYPE tadir-korrnum
          iv_super_package TYPE devclass,

      when_packages_are_resolved,

      then_korrnum_should_be
        IMPORTING
          iv_line    TYPE i
          iv_korrnum TYPE tadir-korrnum.

ENDCLASS.

CLASS zcl_abapgit_dependencies DEFINITION LOCAL FRIENDS ltcl_resolve_packages.

CLASS ltcl_resolve_packages IMPLEMENTATION.

  METHOD resolve_single.


    given_tadir( iv_object        = 'DEVC'
                 iv_obj_name      = 'Z_MAIN'
                 iv_korrnum       = '9990'
                 iv_super_package = '' ).

    given_tadir( iv_object        = 'DEVC'
                 iv_obj_name      = 'Z_SUB1'
                 iv_korrnum       = '9990'
                 iv_super_package = 'Z_MAIN' ).

    given_tadir( iv_object        = 'DEVC'
                 iv_obj_name      = 'Z_SUB2'
                 iv_korrnum       = '9990'
                 iv_super_package = 'Z_MAIN' ).

    when_packages_are_resolved( ).

    then_korrnum_should_be( iv_line    = 1
                            iv_korrnum = '9990' ).
    then_korrnum_should_be( iv_line    = 2
                            iv_korrnum = '9989' ).
    then_korrnum_should_be( iv_line    = 3
                            iv_korrnum = '9989' ).

  ENDMETHOD.


  METHOD given_tadir.

    DATA: ls_tadir   LIKE LINE OF mt_tadir,
          lv_package TYPE devclass.

    ls_tadir-object   = iv_object.
    ls_tadir-obj_name = iv_obj_name.
    ls_tadir-korrnum  = iv_korrnum.
    INSERT ls_tadir INTO TABLE mt_tadir.

    IF iv_super_package IS NOT INITIAL.
      lv_package = iv_obj_name.
      INSERT lv_package INTO TABLE mt_sub_packages.
    ENDIF.

  ENDMETHOD.


  METHOD when_packages_are_resolved.

    DATA: lo_mock_sap_package TYPE REF TO ltcl_sap_package.

    CREATE OBJECT lo_mock_sap_package
      EXPORTING
        iv_package = 'Z_MAIN'.

    lo_mock_sap_package->set_sub_packages( mt_sub_packages ).

    zcl_abapgit_injector=>set_sap_package( iv_package     = 'Z_MAIN'
                                           ii_sap_package = lo_mock_sap_package ).

    zcl_abapgit_dependencies=>resolve_packages( CHANGING ct_tadir = mt_tadir ).

  ENDMETHOD.


  METHOD then_korrnum_should_be.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF mt_tadir.

    READ TABLE mt_tadir INDEX iv_line
                        ASSIGNING <ls_tadir>.

    cl_abap_unit_assert=>assert_equals( exp = iv_korrnum
                                        act = <ls_tadir>-korrnum ).

  ENDMETHOD.

ENDCLASS.
