*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_TADIR
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_tadir DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS lcl_tadir DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      read
        IMPORTING iv_package            TYPE tadir-devclass
                  iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
                  io_dot                TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
                  io_log                TYPE REF TO zcl_abapgit_log OPTIONAL
        RETURNING VALUE(rt_tadir)       TYPE zif_abapgit_definitions=>ty_tadir_tt
        RAISING   zcx_abapgit_exception,
      read_single
        IMPORTING iv_pgmid        TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object       TYPE tadir-object
                  iv_obj_name     TYPE tadir-obj_name
        RETURNING VALUE(rs_tadir) TYPE tadir
        RAISING   zcx_abapgit_exception,
      get_object_package
        IMPORTING iv_pgmid           TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object          TYPE tadir-object
                  iv_obj_name        TYPE tadir-obj_name
        RETURNING VALUE(rv_devclass) TYPE tadir-devclass
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_exists
        IMPORTING it_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt
        RETURNING VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
        RAISING   zcx_abapgit_exception,
      build
        IMPORTING iv_package            TYPE tadir-devclass
                  iv_top                TYPE tadir-devclass
                  io_dot                TYPE REF TO zcl_abapgit_dot_abapgit
                  iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
                  io_log                TYPE REF TO zcl_abapgit_log OPTIONAL
        RETURNING VALUE(rt_tadir)       TYPE zif_abapgit_definitions=>ty_tadir_tt
        RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_tadir DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_tadir IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tadir IMPLEMENTATION.

  METHOD read_single.

    IF iv_object = 'SICF'.
      rs_tadir = zcl_abapgit_object_sicf=>read_tadir_sicf(
        iv_pgmid    = iv_pgmid
        iv_obj_name = iv_obj_name ).
    ELSE.
      SELECT SINGLE * FROM tadir INTO rs_tadir
        WHERE pgmid = iv_pgmid
        AND object = iv_object
        AND obj_name = iv_obj_name.                       "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.                    "read_single

  METHOD get_object_package.

    DATA ls_tadir TYPE tadir.

    ls_tadir = read_single( iv_pgmid    = iv_pgmid
                            iv_object   = iv_object
                            iv_obj_name = iv_obj_name ).

    IF ls_tadir-delflag = 'X'.
      RETURN. "Mark for deletion -> return nothing
    ENDIF.

    rv_devclass = ls_tadir-devclass.

  ENDMETHOD.  "get_object_package.

  METHOD check_exists.

    DATA: lv_exists   TYPE abap_bool,
          lo_progress TYPE REF TO zcl_abapgit_progress,
          ls_item     TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( it_tadir ).

* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      IF sy-tabix MOD 200 = 0.
        lo_progress->show(
          iv_current = sy-tabix
          iv_text    = |Check object exists { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDIF.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      ls_item-devclass = <ls_tadir>-devclass.

      IF lcl_objects=>is_supported( ls_item ) = abap_true.
        lv_exists = lcl_objects=>exists( ls_item ).
        IF lv_exists = abap_true.
          APPEND <ls_tadir> TO rt_tadir.
        ENDIF.
      ELSE.
        APPEND <ls_tadir> TO rt_tadir.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "check_exists

  METHOD read.

* start recursion
* hmm, some problems here, should TADIR also build path?
    rt_tadir = build( iv_package            = iv_package
                      iv_top                = iv_package
                      io_dot                = io_dot
                      iv_ignore_subpackages = iv_ignore_subpackages
                      io_log                = io_log ).

    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.                    "read

  METHOD build.

    DATA: lt_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_tdevc        TYPE STANDARD TABLE OF tdevc,
          lv_path         TYPE string,
          lo_skip_objects TYPE REF TO zcl_abapgit_skip_objects,
          lt_excludes     TYPE RANGE OF trobjtype,
          ls_exclude      LIKE LINE OF lt_excludes.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc,
                   <ls_tadir> LIKE LINE OF rt_tadir.

    ls_exclude-sign = 'I'.
    ls_exclude-option = 'EQ'.

    ls_exclude-low = 'SOTR'.
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low = 'SFB1'.
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low = 'SFB2'.
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low = 'STOB'. " auto generated by core data services
    APPEND ls_exclude TO lt_excludes.

    SELECT * FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_tadir
      WHERE devclass = iv_package
      AND pgmid = 'R3TR'
      AND object NOT IN lt_excludes
      AND delflag = abap_false
      ORDER BY PRIMARY KEY.               "#EC CI_GENBUFF "#EC CI_SUBRC

    CREATE OBJECT lo_skip_objects.
    rt_tadir = lo_skip_objects->skip_sadl_generated_objects(
      it_tadir = rt_tadir
      io_log   = io_log ).

    " Local packages are not in TADIR, only in TDEVC, act as if they were
    IF iv_package CP '$*'. " OR iv_package CP 'T*' ).
      APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-pgmid    = 'R3TR'.
      <ls_tadir>-object   = 'DEVC'.
      <ls_tadir>-obj_name = iv_package.
      <ls_tadir>-devclass = iv_package.
    ENDIF.

    IF NOT io_dot IS INITIAL.
      lv_path = zcl_abapgit_folder_logic=>package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = iv_package ).
    ENDIF.

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-path = lv_path.

      CASE <ls_tadir>-object.
        WHEN 'SICF'.
* replace the internal GUID with a hash of the path
          <ls_tadir>-obj_name+15 = zcl_abapgit_object_sicf=>read_sicf_url( <ls_tadir>-obj_name ).
      ENDCASE.
    ENDLOOP.

* look for subpackages
    IF iv_ignore_subpackages = abap_false.
      SELECT * FROM tdevc INTO TABLE lt_tdevc
        WHERE parentcl = iv_package
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
    ENDIF.

    LOOP AT lt_tdevc ASSIGNING <ls_tdevc>.
      lt_tadir = build( iv_package = <ls_tdevc>-devclass
                        iv_top     = iv_top
                        io_dot     = io_dot
                        io_log     = io_log ).
      APPEND LINES OF lt_tadir TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.                    "build

ENDCLASS.                    "lcl_tadir IMPLEMENTATION
