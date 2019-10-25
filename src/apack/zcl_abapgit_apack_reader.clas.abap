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
    METHODS copy_manifest_descriptor
      IMPORTING
        !io_manifest_provider TYPE REF TO object .
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



CLASS zcl_abapgit_apack_reader IMPLEMENTATION.


  METHOD constructor.
    me->mv_package_name = iv_package_name.
  ENDMETHOD.


  METHOD create_instance.
    CREATE OBJECT ro_manifest_reader EXPORTING iv_package_name = iv_package_name.
  ENDMETHOD.


  METHOD get_manifest_descriptor.

    DATA: lo_manifest_provider       TYPE REF TO object,
          ls_manifest_implementation TYPE ty_s_manifest_declaration.

    IF mv_is_cached IS INITIAL AND mv_package_name IS NOT INITIAL.
      SELECT SINGLE seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
         INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
         INTO ls_manifest_implementation
         WHERE tadir~pgmid = 'R3TR' AND
               tadir~object = 'CLAS' AND
               seometarel~version = '1' AND
               seometarel~refclsname = 'ZIF_APACK_MANIFEST' AND
               tadir~devclass = me->mv_package_name.
      IF ls_manifest_implementation IS INITIAL.
        SELECT SINGLE seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
           INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
           INTO ls_manifest_implementation
           WHERE tadir~pgmid = 'R3TR' AND
                 tadir~object = 'CLAS' AND
                 seometarel~version = '1' AND
                 seometarel~refclsname = 'IF_APACK_MANIFEST' AND
                 tadir~devclass = me->mv_package_name.
      ENDIF.
      IF ls_manifest_implementation IS NOT INITIAL.
        TRY.
            CREATE OBJECT lo_manifest_provider TYPE (ls_manifest_implementation-clsname).
          CATCH cx_sy_create_object_error.
            CLEAR: rs_manifest_descriptor.
        ENDTRY.
        IF lo_manifest_provider IS BOUND.
          me->copy_manifest_descriptor( io_manifest_provider = lo_manifest_provider ).
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

  METHOD copy_manifest_descriptor.

    DATA: ls_my_manifest_wo_deps TYPE zif_abapgit_apack_definitions=>ty_descriptor_wo_dependencies,
          ls_my_dependency       TYPE zif_abapgit_apack_definitions=>ty_dependency.

    FIELD-SYMBOLS: <lg_descriptor>   TYPE any,
                   <lt_dependencies> TYPE ANY TABLE,
                   <lg_dependency>   TYPE any.

    ASSIGN io_manifest_provider->('ZIF_APACK_MANIFEST~DESCRIPTOR') TO <lg_descriptor>.
    IF <lg_descriptor> IS NOT ASSIGNED.
      ASSIGN io_manifest_provider->('IF_APACK_MANIFEST~DESCRIPTOR') TO <lg_descriptor>.
    ENDIF.
    IF <lg_descriptor> IS ASSIGNED.
      " A little more complex than a normal MOVE-CORRSPONDING
      " to avoid dumps in case of future updates to the dependencies table structure
      ASSIGN COMPONENT 'DEPENDENCIES' OF STRUCTURE <lg_descriptor> TO <lt_dependencies>.
      IF <lt_dependencies> IS ASSIGNED.
        LOOP AT <lt_dependencies> ASSIGNING <lg_dependency>.
          MOVE-CORRESPONDING <lg_dependency> TO ls_my_dependency.
          INSERT ls_my_dependency INTO TABLE me->ms_cached_descriptor-dependencies.
        ENDLOOP.
        MOVE-CORRESPONDING <lg_descriptor> TO ls_my_manifest_wo_deps.
        MOVE-CORRESPONDING ls_my_manifest_wo_deps TO me->ms_cached_descriptor.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
