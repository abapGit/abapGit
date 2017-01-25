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
        RETURNING VALUE(rt_tadir)       TYPE ty_tadir_tt
        RAISING   lcx_exception,
      read_single
        IMPORTING iv_pgmid        TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object       TYPE tadir-object
                  iv_obj_name     TYPE tadir-obj_name
        RETURNING VALUE(rs_tadir) TYPE tadir,
      get_object_package
        IMPORTING iv_pgmid           TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object          TYPE tadir-object
                  iv_obj_name        TYPE tadir-obj_name
        RETURNING VALUE(rv_devclass) TYPE tadir-devclass.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_exists
        IMPORTING it_tadir        TYPE ty_tadir_tt
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception,
      build
        IMPORTING iv_package            TYPE tadir-devclass
                  iv_path               TYPE string
                  iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rt_tadir)       TYPE ty_tadir_tt
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_tadir DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_tadir IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tadir IMPLEMENTATION.

  METHOD read_single.

    DATA: lv_obj_name TYPE tadir-obj_name.


    IF iv_object = 'SICF'.
      CONCATENATE iv_obj_name '%' INTO lv_obj_name.
    ELSE.
      lv_obj_name = iv_obj_name.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name LIKE lv_obj_name.      "#EC CI_SUBRC "#EC CI_GENBUFF

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

    DATA: lv_exists TYPE abap_bool,
          ls_item   TYPE ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

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
    rt_tadir = build( iv_package            = iv_package
                      iv_path               = ''
                      iv_ignore_subpackages = iv_ignore_subpackages ).

    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.                    "read

  METHOD build.

    DATA: lv_index    TYPE i,
          lt_tadir    TYPE ty_tadir_tt,
          lt_tdevc    TYPE STANDARD TABLE OF tdevc,
          lv_len      TYPE i,
          lv_message  TYPE string,
          lv_path     TYPE string,
          lv_category TYPE seoclassdf-category.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc,
                   <ls_tadir> LIKE LINE OF rt_tadir.


    SELECT * FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_tadir
      WHERE devclass = iv_package
      AND object <> 'DEVC'
      AND object <> 'SOTR'
      AND object <> 'SFB1'
      AND object <> 'SFB2'
      AND delflag = abap_false
      ORDER BY PRIMARY KEY.               "#EC CI_GENBUFF "#EC CI_SUBRC

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.

      <ls_tadir>-path = iv_path.

      CASE <ls_tadir>-object.
        WHEN 'SICF'.
          <ls_tadir>-obj_name = <ls_tadir>-obj_name(15).
        WHEN 'INTF'.
          SELECT SINGLE category FROM seoclassdf INTO lv_category
            WHERE clsname = <ls_tadir>-obj_name
            AND ( version = '1'
            OR version = '0' ) ##warn_ok.               "#EC CI_GENBUFF
          IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
            DELETE rt_tadir INDEX lv_index.
          ENDIF.
      ENDCASE.
    ENDLOOP.

* look for subpackages
    IF iv_ignore_subpackages = abap_false.
      SELECT * FROM tdevc INTO TABLE lt_tdevc
        WHERE parentcl = iv_package
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
    ENDIF.

    LOOP AT lt_tdevc ASSIGNING <ls_tdevc>.
      lv_len = strlen( iv_package ).
      IF <ls_tdevc>-devclass(lv_len) <> iv_package.
* if abapGit project is installed in package ZZZ, all subpackages should be named
* ZZZ_something. This will define the folder name in the zip file to be "something",
* similarily with online projects
        lv_message = 'Unexpected package naming(' &&
          <ls_tdevc>-devclass && ')' ##no_text.
        MESSAGE lv_message TYPE 'I'.
        CONTINUE.
      ENDIF.

      lv_path = <ls_tdevc>-devclass+lv_len.
      IF lv_path(1) = '_'.
        lv_path = lv_path+1.
      ENDIF.
      TRANSLATE lv_path TO LOWER CASE.
      CONCATENATE iv_path lv_path '/' INTO lv_path.

      lt_tadir = build( iv_package = <ls_tdevc>-devclass
                        iv_path    = lv_path ).
      APPEND LINES OF lt_tadir TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.                    "build

ENDCLASS.                    "lcl_tadir IMPLEMENTATION
