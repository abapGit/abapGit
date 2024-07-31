CLASS zcl_abapgit_where_used_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_devc_range TYPE RANGE OF tadir-devclass.
    TYPES:
      BEGIN OF ty_dependency,
        package       TYPE devclass,
        obj_type      TYPE tadir-object,
        obj_prog_type TYPE trdir-subc,
        obj_name      TYPE tadir-obj_name,
        obj_cls       TYPE string,
        dep_package   TYPE devclass,
        dep_obj_type  TYPE tadir-object,
        dep_obj_name  TYPE tadir-obj_name,
        dep_used_cls  TYPE string,
        dep_used_obj  TYPE string,
      END OF ty_dependency.
    TYPES:
      ty_dependency_tt TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY.

    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_where_used_tools.

    " the initial version of this utility is also available as a standalone tool
    " here: https://github.com/sbcgua/crossdeps
    METHODS select_external_usages
      IMPORTING
        iv_package       TYPE tadir-devclass
        iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        ir_package_scope TYPE ty_devc_range OPTIONAL
      RETURNING
        VALUE(rt_objs)   TYPE ty_dependency_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_obj_signature,
        package  TYPE devclass,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF ty_obj_signature.

    TYPES ty_where_used_tt TYPE STANDARD TABLE OF rsfindlst WITH DEFAULT KEY.
    TYPES ty_seu_obj TYPE STANDARD TABLE OF seu_obj WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_dev_object,
        type  TYPE seu_stype,
        tadir TYPE trobjtype,
      END OF ty_dev_object.

    DATA mt_object_packages TYPE HASHED TABLE OF ty_obj_signature WITH UNIQUE KEY obj_type obj_name.
    DATA mt_dev_obj_cache TYPE HASHED TABLE OF ty_dev_object WITH UNIQUE KEY type.

    METHODS get_where_used
      IMPORTING
        iv_obj_type        TYPE euobj-id
        iv_obj_name        TYPE tadir-obj_name
        it_scope           TYPE ty_seu_obj OPTIONAL
        ir_package_scope   TYPE ty_devc_range OPTIONAL
      RETURNING
        VALUE(rt_findings) TYPE ty_where_used_tt
      RAISING
        zcx_abapgit_exception.

    METHODS get_obj_package
      IMPORTING
        iv_obj_type       TYPE tadir-object
        iv_obj_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rv_package) TYPE tadir-devclass.

    METHODS get_func_package
      IMPORTING
        iv_func_name      TYPE tadir-obj_name
      RETURNING
        VALUE(rv_package) TYPE tadir-devclass.

    METHODS get_incl_package
      IMPORTING
        iv_prog_name      TYPE tadir-obj_name
      RETURNING
        VALUE(rv_package) TYPE tadir-devclass.

    METHODS build_package_scope
      IMPORTING
        it_tadir                TYPE STANDARD TABLE
        ir_package_scope        TYPE ty_devc_range
      RETURNING
        VALUE(rt_package_scope) TYPE ty_devc_range.

    METHODS collect_where_used
      IMPORTING
        it_tadir         TYPE STANDARD TABLE
        ir_package_scope TYPE ty_devc_range
      RETURNING
        VALUE(rt_objs)   TYPE ty_dependency_tt
      RAISING
        zcx_abapgit_exception.

    METHODS convert_list
      IMPORTING
        iv_package     TYPE ty_dependency-dep_package
        iv_obj_type    TYPE ty_dependency-dep_obj_type
        iv_obj_name    TYPE ty_dependency-dep_obj_name
        it_where_used  TYPE ty_where_used_tt
      RETURNING
        VALUE(rt_objs) TYPE ty_dependency_tt.

    METHODS decode_obj_type
      IMPORTING
        iv_type        TYPE rsfindlst-object_cls
      RETURNING
        VALUE(rv_type) TYPE ty_dev_object-tadir.

ENDCLASS.



CLASS ZCL_ABAPGIT_WHERE_USED_TOOLS IMPLEMENTATION.


  METHOD build_package_scope.

    FIELD-SYMBOLS <ls_tadir> TYPE zif_abapgit_definitions=>ty_tadir.
    FIELD-SYMBOLS <ls_pkg> LIKE LINE OF rt_package_scope.

    rt_package_scope = ir_package_scope.
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      CHECK <ls_tadir>-object = 'DEVC'.
      APPEND INITIAL LINE TO rt_package_scope ASSIGNING <ls_pkg>.
      <ls_pkg>-sign   = 'E'.
      <ls_pkg>-option = 'EQ'.
      <ls_pkg>-low    = <ls_tadir>-obj_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD collect_where_used.

    DATA li_progress TYPE REF TO zif_abapgit_progress.
    DATA lt_where_used TYPE ty_where_used_tt.
    DATA lt_objs_portion LIKE rt_objs.

    FIELD-SYMBOLS <ls_tadir> TYPE zif_abapgit_definitions=>ty_tadir.

    li_progress = zcl_abapgit_progress=>get_instance( lines( it_tadir ) ).

    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      CHECK <ls_tadir>-object <> 'DEVC'.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |{ <ls_tadir>-object } { <ls_tadir>-obj_name }| ).

      lt_where_used = get_where_used(
        iv_obj_type = |{ <ls_tadir>-object }|
        iv_obj_name = <ls_tadir>-obj_name
        ir_package_scope = ir_package_scope ).

      lt_objs_portion = convert_list(
        iv_package    = <ls_tadir>-devclass
        iv_obj_type   = <ls_tadir>-object
        iv_obj_name   = <ls_tadir>-obj_name
        it_where_used = lt_where_used ).

      APPEND LINES OF lt_objs_portion TO rt_objs.

    ENDLOOP.

    li_progress->off( ).

  ENDMETHOD.


  METHOD convert_list.

    " See also CL_FINB_GN_BBI=>GET_CROSSREF

    FIELD-SYMBOLS <ls_dep> LIKE LINE OF rt_objs.
    FIELD-SYMBOLS <ls_use> LIKE LINE OF it_where_used.

    LOOP AT it_where_used ASSIGNING <ls_use>.

      APPEND INITIAL LINE TO rt_objs ASSIGNING <ls_dep>.
      <ls_dep>-dep_package  = iv_package.
      <ls_dep>-dep_obj_type = iv_obj_type.
      <ls_dep>-dep_obj_name = iv_obj_name.

      <ls_dep>-dep_used_obj = <ls_use>-used_obj.
      <ls_dep>-dep_used_cls = <ls_use>-used_cls.

      <ls_dep>-obj_cls  = <ls_use>-object_cls.
      <ls_dep>-obj_name = <ls_use>-encl_objec.
      IF <ls_dep>-obj_name IS INITIAL.
        <ls_dep>-obj_name = <ls_use>-object.
      ENDIF.

      IF <ls_use>-object_cls = 'FF'. " Function module
        <ls_dep>-obj_type = 'FUNC'.
        <ls_dep>-package = get_func_package( <ls_dep>-obj_name ).

      ELSE.
        <ls_dep>-obj_type = decode_obj_type( <ls_use>-object_cls ).

        <ls_dep>-package = get_obj_package(
          iv_obj_type = <ls_dep>-obj_type
          iv_obj_name = <ls_dep>-obj_name ).

        IF <ls_dep>-package IS INITIAL AND <ls_dep>-obj_type = 'CLAS'.
          <ls_dep>-package = get_obj_package(
            iv_obj_type = 'INTF'
            iv_obj_name = <ls_dep>-obj_name ).
          IF <ls_dep>-package IS NOT INITIAL.
            <ls_dep>-obj_type = 'INTF'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <ls_dep>-package IS INITIAL.
        IF <ls_dep>-obj_type = 'PROG'. " Maybe it is an include

          <ls_dep>-package = get_incl_package( <ls_dep>-obj_name ).
          IF <ls_dep>-package IS INITIAL.
            SELECT SINGLE subc INTO <ls_dep>-obj_prog_type FROM trdir WHERE name = <ls_dep>-obj_name.
            IF <ls_dep>-obj_prog_type IS NOT INITIAL AND <ls_dep>-obj_prog_type <> '1'. " Exec. prog
              <ls_dep>-obj_type = 'INCL'.
            ENDIF.
          ENDIF.

        ENDIF.

        IF <ls_dep>-package IS INITIAL.
          <ls_dep>-package = '????'.
        ENDIF.
      ENDIF.

    ENDLOOP.

    " some includes are ENHO ...
    " include detection TRDIR, D010INC ???
    " how to find connection with ENHO ?
    " Useful: https://github.com/abaplint/abaplint-sci-client/blob/main/src/deps/zcl_abaplint_deps_find.clas.abap
    " And cl_wb_manager->if_wb_manager~request_tool_access
    " And discussions in https://github.com/abapGit/abapGit/pull/6897

  ENDMETHOD.


  METHOD decode_obj_type.

    FIELD-SYMBOLS <ls_devobj> LIKE LINE OF mt_dev_obj_cache.

    IF mt_dev_obj_cache IS INITIAL.
      SELECT type tadir INTO TABLE mt_dev_obj_cache
        FROM euobjedit.
    ENDIF.

    READ TABLE mt_dev_obj_cache ASSIGNING <ls_devobj> WITH KEY type = iv_type.
    IF sy-subrc = 0.
      rv_type = <ls_devobj>-tadir.
    ENDIF.

  ENDMETHOD.


  METHOD get_func_package.

    " See also: FUNCTION_INCLUDE_INFO, TFDIR, find main program -> get its pkg

    DATA ls_obj_sig LIKE LINE OF mt_object_packages.

    READ TABLE mt_object_packages INTO ls_obj_sig WITH KEY obj_type = 'FUNC' obj_name = iv_func_name.

    IF sy-subrc <> 0.
      SELECT SINGLE devclass INTO ls_obj_sig-package
        FROM info_func
        WHERE funcname = iv_func_name.
      IF ls_obj_sig-package IS NOT INITIAL.
        ls_obj_sig-obj_type = 'FUNC'.
        ls_obj_sig-obj_name = iv_func_name.
        INSERT ls_obj_sig INTO TABLE mt_object_packages.
      ENDIF.
    ENDIF.

    rv_package = ls_obj_sig-package.

  ENDMETHOD.


  METHOD get_incl_package.

    DATA lv_program TYPE progname.
    DATA lv_area    TYPE rs38l_area.

    lv_program = iv_prog_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
      CHANGING
        program                  = lv_program
        complete_area            = lv_area
      EXCEPTIONS
        not_enough_input         = 1
        no_function_pool         = 2
        delimiter_wrong_position = 3
        OTHERS                   = 4.

    IF lv_area IS INITIAL.
      SELECT SINGLE master FROM d010inc INTO lv_program
        WHERE include = iv_prog_name.

      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        CHANGING
          program                  = lv_program
          complete_area            = lv_area
        EXCEPTIONS
          not_enough_input         = 1
          no_function_pool         = 2
          delimiter_wrong_position = 3
          OTHERS                   = 4.
    ENDIF.

    IF lv_area IS NOT INITIAL.
      rv_package = get_obj_package(
        iv_obj_type = 'FUGR'
        iv_obj_name = |{ lv_area }| ).
      RETURN.
    ENDIF.

    " TODO more ...

  ENDMETHOD.


  METHOD get_obj_package.

    " see also zcl_abapgit_tadir->get_object_package for checks

    DATA ls_obj_sig LIKE LINE OF mt_object_packages.

    READ TABLE mt_object_packages INTO ls_obj_sig WITH KEY obj_type = iv_obj_type obj_name = iv_obj_name.

    IF sy-subrc <> 0.
      ls_obj_sig-package = zcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = iv_obj_type
        iv_obj_name = iv_obj_name )-devclass.
      IF ls_obj_sig-package IS NOT INITIAL.
        ls_obj_sig-obj_type = iv_obj_type.
        ls_obj_sig-obj_name = iv_obj_name.
        INSERT ls_obj_sig INTO TABLE mt_object_packages.
      ENDIF.
    ENDIF.

    rv_package = ls_obj_sig-package.

  ENDMETHOD.


  METHOD get_where_used.

    DATA lt_findstrings TYPE string_table.
    DATA lt_scope       LIKE it_scope.
    DATA lv_findstring  LIKE LINE OF lt_findstrings.

    IF iv_obj_name IS INITIAL.
      RETURN.
    ENDIF.

    lt_scope = it_scope.

    lv_findstring = iv_obj_name.
    INSERT lv_findstring INTO TABLE lt_findstrings.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls           = iv_obj_type
        no_dialog                = abap_true
        without_text             = abap_true
      TABLES
        i_findstrings            = lt_findstrings
        o_founds                 = rt_findings
        i_scope_object_cls       = lt_scope
        i_scope_devclass         = ir_package_scope
      EXCEPTIONS
        not_executed             = 1
        not_found                = 2
        illegal_object           = 3
        no_cross_for_this_object = 4
        batch                    = 5
        batchjob_error           = 6
        wrong_type               = 7
        object_not_exist         = 8
        OTHERS                   = 9.

    IF sy-subrc = 1 OR sy-subrc = 2 OR lines( rt_findings ) = 0.
      RETURN.
    ELSEIF sy-subrc > 2.
      zcx_abapgit_exception=>raise( |RS_EU_CROSSREF({ sy-subrc }) for { iv_obj_type } { iv_obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.


  METHOD select_external_usages.

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_package_scope LIKE ir_package_scope.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
     iv_package            = iv_package
     iv_ignore_subpackages = iv_ignore_subpackages ).

    lt_package_scope = build_package_scope(
      ir_package_scope = ir_package_scope
      it_tadir         = lt_tadir ).

    rt_objs = collect_where_used(
      ir_package_scope = lt_package_scope
      it_tadir         = lt_tadir ).

    SORT rt_objs.
    DELETE ADJACENT DUPLICATES FROM rt_objs.
    " Duplicates happen e.g. because where-used is found by method.
    " However here this functionality aggregates them to the object
    " These are not true duplicates, so if ever the method name (or any other duplicate cause)
    " will be extracted, this sort can be removed

  ENDMETHOD.
ENDCLASS.
