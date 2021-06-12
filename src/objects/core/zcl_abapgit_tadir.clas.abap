CLASS zcl_abapgit_tadir DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS exists
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_exists) TYPE abap_bool .
    METHODS check_exists
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS build
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
        !ii_log                TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_tadir)        TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS select_objects
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool
      EXPORTING
        !et_packages           TYPE zif_abapgit_sap_package=>ty_devclass_tt
        !et_tadir              TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS skip_objects
      IMPORTING
        !iv_package TYPE tadir-devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log     TYPE REF TO zif_abapgit_log OPTIONAL
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_local_packages
      IMPORTING
        !it_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt
      CHANGING
        !ct_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_namespaces
      IMPORTING
        !iv_package TYPE devclass
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_path
      IMPORTING
        !iv_package TYPE tadir-devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS adjust_objects
      IMPORTING
        !iv_package TYPE tadir-devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_TADIR IMPLEMENTATION.


  METHOD add_local_packages.

    FIELD-SYMBOLS:
      <lv_package> LIKE LINE OF it_packages,
      <ls_tadir>   LIKE LINE OF ct_tadir.

    LOOP AT it_packages ASSIGNING <lv_package>.

      " Local packages are not in TADIR, only in TDEVC, act as if they were
      IF <lv_package> CP '$*'. " OR <package> CP 'T*' ).
        APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-pgmid    = 'R3TR'.
        <ls_tadir>-object   = 'DEVC'.
        <ls_tadir>-obj_name = <lv_package>.
        <ls_tadir>-devclass = <lv_package>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_namespaces.

    DATA:
      lv_name      TYPE progname,
      lv_namespace TYPE namespace.

    FIELD-SYMBOLS:
      <ls_tadir> LIKE LINE OF ct_tadir,
      <ls_nspc>  LIKE LINE OF ct_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE obj_name(1) = '/'.

      " Namespaces are not in TADIR, but are necessary for creating objects in transportable packages
      lv_name = <ls_tadir>-obj_name.

      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace = lv_name
        IMPORTING
          namespace           = lv_namespace
        EXCEPTIONS
          delimiter_error     = 1
          OTHERS              = 2.

      IF sy-subrc = 0 AND lv_namespace IS NOT INITIAL.
        READ TABLE ct_tadir TRANSPORTING NO FIELDS
          WITH KEY pgmid = 'R3TR' object = 'NSPC' obj_name = lv_namespace.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_nspc>.
          <ls_nspc>-pgmid    = 'R3TR'.
          <ls_nspc>-object   = 'NSPC'.
          <ls_nspc>-obj_name = lv_namespace.
          <ls_nspc>-devclass = iv_package.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD adjust_objects.

    " Todo, replace with solution that will work with any object type (might depend on iv_package and io_dot)

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.

      IF <ls_tadir>-object = 'SICF'.
        " Replace the internal GUID with a hash of the path
        TRY.
            CALL METHOD ('ZCL_ABAPGIT_OBJECT_SICF')=>read_sicf_url
              EXPORTING
                iv_obj_name = <ls_tadir>-obj_name
              RECEIVING
                rv_hash     = <ls_tadir>-obj_name+15.

          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
            " SICF might not be supported in some systems, assume this code is not called
        ENDTRY.
      ENDIF.

      CLEAR <ls_tadir>-korrnum.
    ENDLOOP.

  ENDMETHOD.


  METHOD build.

    DATA lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    select_objects(
      EXPORTING
        iv_package            = iv_package
        iv_ignore_subpackages = iv_ignore_subpackages
        iv_only_local_objects = iv_only_local_objects
      IMPORTING
        et_tadir              = rt_tadir
        et_packages           = lt_packages ).

    skip_objects(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
        ii_log     = ii_log
      CHANGING
        ct_tadir   = rt_tadir ).

    add_local_packages(
      EXPORTING
        it_packages = lt_packages
      CHANGING
        ct_tadir    = rt_tadir ).

    add_namespaces(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = rt_tadir ).

    determine_path(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
      CHANGING
        ct_tadir   = rt_tadir ).

    adjust_objects(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
      CHANGING
        ct_tadir   = rt_tadir ).

  ENDMETHOD.


  METHOD check_exists.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          ls_item     TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    li_progress = zcl_abapgit_progress=>get_instance( lines( it_tadir ) ).

* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      IF sy-tabix MOD 200 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = |Check object exists { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDIF.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      ls_item-devclass = <ls_tadir>-devclass.

      IF exists( ls_item ) = abap_true.
        APPEND <ls_tadir> TO rt_tadir.
      ENDIF.
    ENDLOOP.

    li_progress->off( ).

  ENDMETHOD.


  METHOD determine_path.

    DATA:
      lv_path         TYPE string,
      lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic,
      lv_last_package TYPE devclass VALUE cl_abap_char_utilities=>horizontal_tab.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.

      IF lv_last_package <> <ls_tadir>-devclass.
        "Change in Package
        lv_last_package = <ls_tadir>-devclass.

        IF NOT io_dot IS INITIAL.
          lv_path = lo_folder_logic->package_to_path(
            iv_top     = iv_package
            io_dot     = io_dot
            iv_package = <ls_tadir>-devclass ).
        ENDIF.
      ENDIF.

      <ls_tadir>-path = lv_path.

    ENDLOOP.
  ENDMETHOD.


  METHOD exists.

    IF is_item IS INITIAL.
      RETURN.
    ENDIF.

    IF zcl_abapgit_objects=>is_supported( is_item ) = abap_false.
      rv_exists = abap_true.
      RETURN.
    ENDIF.

    rv_exists = zcl_abapgit_objects=>exists( is_item ).

  ENDMETHOD.


  METHOD select_objects.

    DATA:
      lt_excludes  TYPE RANGE OF trobjtype,
      ls_exclude   LIKE LINE OF lt_excludes,
      lt_srcsystem TYPE RANGE OF tadir-srcsystem,
      ls_srcsystem LIKE LINE OF lt_srcsystem.

    " Determine packages to read
    IF iv_ignore_subpackages = abap_false.
      et_packages = zcl_abapgit_factory=>get_sap_package( iv_package )->list_subpackages( ).
    ENDIF.
    INSERT iv_package INTO et_packages INDEX 1.

    " Exclude object types with tadir entries that are included elsewhere
    ls_exclude-sign   = 'I'.
    ls_exclude-option = 'EQ'.
    ls_exclude-low    = 'SOTR'. " automatically create for sap packages (DEVC)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB1'. " covered by business function sets (SFBS)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB2'. " covered by business functions (SFBF)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'STOB'. " auto generated by core data services (DDLS)
    APPEND ls_exclude TO lt_excludes.

    " Limit to objects belonging to this system
    IF iv_only_local_objects = abap_true.
      ls_srcsystem-sign   = 'I'.
      ls_srcsystem-option = 'EQ'.
      ls_srcsystem-low    = sy-sysid.
      APPEND ls_srcsystem TO lt_srcsystem.
    ENDIF.

    IF et_packages IS NOT INITIAL.
      SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE et_tadir
        FOR ALL ENTRIES IN et_packages
        WHERE devclass = et_packages-table_line
        AND pgmid      = 'R3TR'
        AND object     NOT IN lt_excludes
        AND delflag    = abap_false
        AND srcsystem  IN lt_srcsystem
        ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF "#EC CI_SUBRC
    ENDIF.

    SORT et_tadir BY devclass pgmid object obj_name.

  ENDMETHOD.


  METHOD skip_objects.

    " Todo, replace with solution that will work with any object type (might depend on iv_package and io_dot)

    DATA lo_skip_objects TYPE REF TO zcl_abapgit_skip_objects.

    CREATE OBJECT lo_skip_objects.

    ct_tadir = lo_skip_objects->skip_sadl_generated_objects(
      it_tadir = ct_tadir
      ii_log   = ii_log ).

  ENDMETHOD.


  METHOD zif_abapgit_tadir~get_object_package.

    DATA: ls_tadir TYPE zif_abapgit_definitions=>ty_tadir,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.

    ls_tadir = zif_abapgit_tadir~read_single(
      iv_pgmid    = iv_pgmid
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).

    IF ls_tadir-delflag = 'X'.
      RETURN. "Mark for deletion -> return nothing
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.
    ls_item-devclass = ls_tadir-devclass.
    IF exists( ls_item ) = abap_false.
      RETURN.
    ENDIF.

    rv_devclass = ls_tadir-devclass.

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read.

    DATA: li_exit TYPE REF TO zif_abapgit_exit.

    " Start recursion
    " hmm, some problems here, should TADIR also build path?
    rt_tadir = build(
      iv_package            = iv_package
      io_dot                = io_dot
      iv_ignore_subpackages = iv_ignore_subpackages
      iv_only_local_objects = iv_only_local_objects
      ii_log                = ii_log ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_tadir(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
      CHANGING
        ct_tadir   = rt_tadir ).

    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read_single.

    SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC
    CLEAR rs_tadir-korrnum.

  ENDMETHOD.
ENDCLASS.
