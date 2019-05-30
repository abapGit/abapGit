CLASS zcl_abapgit_apack_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_package_name TYPE devclass .

    CLASS-METHODS create_instance
      IMPORTING
        !iv_package_name          TYPE ty_package_name
      RETURNING
        VALUE(ro_manifest_reader) TYPE REF TO zcl_abapgit_apack_reader .
    METHODS get_manifest_descriptor
      RETURNING
        VALUE(rs_manifest_descriptor) TYPE zif_abapgit_apack_definitions=>ty_descriptor .
    METHODS set_manifest_descriptor
      IMPORTING
        !is_manifest_descriptor TYPE zif_abapgit_apack_definitions=>ty_descriptor .
    METHODS has_manifest
      RETURNING
        VALUE(rv_has_manifest) TYPE abap_bool .
    METHODS constructor
      IMPORTING
        !iv_package_name TYPE ty_package_name .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_manifest_declaration,
        clsname  TYPE seometarel-clsname,
        devclass TYPE devclass,
      END OF ty_s_manifest_declaration .

    DATA mv_package_name TYPE ty_package_name .
    DATA ms_cached_descriptor TYPE zif_abapgit_apack_definitions=>ty_descriptor .
    DATA mv_is_cached TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ABAPGIT_APACK_READER IMPLEMENTATION.


  METHOD constructor.
    me->mv_package_name = iv_package_name.
  ENDMETHOD.


  METHOD create_instance.
    CREATE OBJECT ro_manifest_reader EXPORTING iv_package_name = iv_package_name.
  ENDMETHOD.


  METHOD get_manifest_descriptor.

    DATA: lo_manifest_provider       TYPE REF TO object,
          ls_manifest_implementation TYPE ty_s_manifest_declaration.

    FIELD-SYMBOLS: <lg_descriptor> TYPE any.

    IF mv_is_cached IS INITIAL AND mv_package_name IS NOT INITIAL.
      SELECT SINGLE seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
         INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
         INTO ls_manifest_implementation
         WHERE tadir~pgmid = 'R3TR' AND
               tadir~object = 'CLAS' AND
               seometarel~version = '1' AND
               seometarel~refclsname = 'ZIF_APACK_MANIFEST' AND
               tadir~devclass = me->mv_package_name.
      IF ls_manifest_implementation IS NOT INITIAL.
        TRY.
            CREATE OBJECT lo_manifest_provider TYPE (ls_manifest_implementation-clsname).
          CATCH cx_sy_create_object_error.
            CLEAR: rs_manifest_descriptor.
        ENDTRY.
        IF lo_manifest_provider IS BOUND.
          ASSIGN lo_manifest_provider->('ZIF_APACK_MANIFEST~DESCRIPTOR') TO <lg_descriptor>.
          IF <lg_descriptor> IS ASSIGNED.
            MOVE-CORRESPONDING <lg_descriptor> TO me->ms_cached_descriptor.
          ENDIF.
        ENDIF.
      ENDIF.
      me->mv_is_cached = abap_true.
    ENDIF.

    rs_manifest_descriptor = me->ms_cached_descriptor.
  ENDMETHOD.


  METHOD has_manifest.

    DATA: ls_returned_manifest TYPE zif_abapgit_apack_definitions=>ty_descriptor.

    ls_returned_manifest = me->get_manifest_descriptor( ).

    rv_has_manifest = abap_false.
    IF ls_returned_manifest IS NOT INITIAL.
      rv_has_manifest = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD set_manifest_descriptor.
    me->mv_is_cached = abap_true.
    me->ms_cached_descriptor = is_manifest_descriptor.
  ENDMETHOD.
ENDCLASS.
