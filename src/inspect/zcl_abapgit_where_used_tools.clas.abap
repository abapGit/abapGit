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
        obj_cls       TYPE rsfindlst-object_cls,
        dep_package   TYPE devclass,
        dep_obj_type  TYPE tadir-object,
        dep_obj_name  TYPE tadir-obj_name,
        dep_used_cls  TYPE rsfindlst-used_cls,
        dep_used_obj  TYPE rsfindlst-used_obj,
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

    FIELD-SYMBOLS <tadir> TYPE zif_abapgit_definitions=>ty_tadir.
    FIELD-SYMBOLS <pkg> LIKE LINE OF rt_package_scope.

    rt_package_scope = ir_package_scope.
    LOOP AT it_tadir ASSIGNING <tadir>.
      CHECK <tadir>-object = 'DEVC'.
      APPEND INITIAL LINE TO rt_package_scope ASSIGNING <pkg>.
      <pkg>-sign   = 'E'.
      <pkg>-option = 'EQ'.
      <pkg>-low    = <tadir>-obj_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD collect_where_used.

    DATA li_progress TYPE REF TO zif_abapgit_progress.
    DATA lt_where_used TYPE ty_where_used_tt.
    DATA lt_objs_portion LIKE rt_objs.

    FIELD-SYMBOLS <tadir> TYPE zif_abapgit_definitions=>ty_tadir.

    li_progress = zcl_abapgit_progress=>get_instance( lines( it_tadir ) ).

    LOOP AT it_tadir ASSIGNING <tadir>.
      CHECK <tadir>-object <> 'DEVC'.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |{ <tadir>-object } { <tadir>-obj_name }| ).

      lt_where_used = get_where_used(
        iv_obj_type = |{ <tadir>-object }|
        iv_obj_name = <tadir>-obj_name
        ir_package_scope = ir_package_scope ).

      lt_objs_portion = convert_list(
        iv_package    = <tadir>-devclass
        iv_obj_type   = <tadir>-object
        iv_obj_name   = <tadir>-obj_name
        it_where_used = lt_where_used ).

      APPEND LINES OF lt_objs_portion TO rt_objs.

    ENDLOOP.

    li_progress->off( ).

  ENDMETHOD.


  METHOD convert_list.

    " See also CL_FINB_GN_BBI=>GET_CROSSREF

    FIELD-SYMBOLS <dep> LIKE LINE OF rt_objs.
    FIELD-SYMBOLS <use> LIKE LINE OF it_where_used.

    LOOP AT it_where_used ASSIGNING <use>.

      APPEND INITIAL LINE TO rt_objs ASSIGNING <dep>.
      <dep>-dep_package  = iv_package.
      <dep>-dep_obj_type = iv_obj_type.
      <dep>-dep_obj_name = iv_obj_name.

      <dep>-dep_used_obj = <use>-used_obj.
      <dep>-dep_used_cls = <use>-used_cls.

      <dep>-obj_cls  = <use>-object_cls.
      <dep>-obj_name = <use>-encl_objec.
      IF <dep>-obj_name IS INITIAL.
        <dep>-obj_name = <use>-object.
      ENDIF.

      IF <use>-object_cls = 'FF'. " Function module
        <dep>-obj_type = 'FUNC'.
        <dep>-package = get_func_package( <dep>-obj_name ).

      ELSE.
        <dep>-obj_type = decode_obj_type( <use>-object_cls ).

        <dep>-package = get_obj_package(
          iv_obj_type = <dep>-obj_type
          iv_obj_name = <dep>-obj_name ).

        IF <dep>-package IS INITIAL AND <dep>-obj_type = 'CLAS'.
          <dep>-package = get_obj_package(
            iv_obj_type = 'INTF'
            iv_obj_name = <dep>-obj_name ).
          IF <dep>-package IS NOT INITIAL.
            <dep>-obj_type = 'INTF'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <dep>-package IS INITIAL.
        <dep>-package = '????'.

        IF <dep>-obj_type = 'PROG'. " Maybe it is an include
          SELECT SINGLE subc INTO <dep>-obj_prog_type FROM trdir WHERE name = <dep>-obj_name.
          IF <dep>-obj_prog_type IS NOT INITIAL AND <dep>-obj_prog_type <> '1'. " Exec. prog
            <dep>-obj_type = 'INCL'.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    " some includes are FUGR and some are ENHO ...
    " include detection TRDIR, D010INC ???
    " How to find FUGR by main program name ???
    " how to find connection with ENHO ?

  ENDMETHOD.


  METHOD decode_obj_type.

    FIELD-SYMBOLS <devobj> LIKE LINE OF mt_dev_obj_cache.

    IF mt_dev_obj_cache IS INITIAL.
      SELECT type tadir INTO TABLE mt_dev_obj_cache
        FROM euobjedit.
    ENDIF.

    READ TABLE mt_dev_obj_cache ASSIGNING <devobj> WITH KEY type = iv_type.
    IF sy-subrc = 0.
      rv_type = <devobj>-tadir.
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

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( iv_package ).

    lt_package_scope = build_package_scope(
      ir_package_scope = ir_package_scope
      it_tadir         = lt_tadir ).

    rt_objs = collect_where_used(
      ir_package_scope = lt_package_scope
      it_tadir         = lt_tadir ).

  ENDMETHOD.
ENDCLASS.
