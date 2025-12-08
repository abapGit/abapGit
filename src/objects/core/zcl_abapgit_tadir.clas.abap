CLASS zcl_abapgit_tadir DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

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
    METHODS add_namespace
      IMPORTING
        !iv_package    TYPE devclass
        !iv_object     TYPE csequence
      CHANGING
        !ct_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt
        !ct_tadir_nspc TYPE zif_abapgit_definitions=>ty_tadir_tt
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
ENDCLASS.



CLASS zcl_abapgit_tadir IMPLEMENTATION.


  METHOD add_local_packages.

    FIELD-SYMBOLS:
      <lv_package> LIKE LINE OF it_packages,
      <ls_tadir>   LIKE LINE OF ct_tadir.

    LOOP AT it_packages ASSIGNING <lv_package>.

      " Local packages are not in TADIR, only in TDEVC, act as if they were
      IF <lv_package> CP '$*'. " OR <package> CP 'T*' ).
        APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-pgmid      = 'R3TR'.
        <ls_tadir>-object     = 'DEVC'.
        <ls_tadir>-obj_name   = <lv_package>.
        <ls_tadir>-devclass   = <lv_package>.
        <ls_tadir>-srcsystem  = sy-sysid.
        <ls_tadir>-masterlang = sy-langu.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_namespace.

    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.
    DATA ls_obj_with_namespace TYPE zif_abapgit_definitions=>ty_obj_namespace.

    TRY.
        ls_obj_with_namespace = zcl_abapgit_factory=>get_sap_namespace( )->split_by_name( iv_object ).
      CATCH zcx_abapgit_exception.
        "Ignore the exception like before the replacement of the FM RS_NAME_SPLIT_NAMESPACE
        RETURN.
    ENDTRY.

    IF ls_obj_with_namespace-namespace IS NOT INITIAL.

      READ TABLE ct_tadir_nspc TRANSPORTING NO FIELDS
        WITH KEY pgmid = 'R3TR' object = 'NSPC' obj_name = ls_obj_with_namespace-namespace.
      IF sy-subrc <> 0.
        ls_tadir-pgmid      = 'R3TR'.
        ls_tadir-object     = 'NSPC'.
        ls_tadir-obj_name   = ls_obj_with_namespace-namespace.
        ls_tadir-devclass   = iv_package.
        ls_tadir-srcsystem  = sy-sysid.
        ls_tadir-masterlang = sy-langu.
        INSERT ls_tadir INTO TABLE ct_tadir.
        INSERT ls_tadir INTO TABLE ct_tadir_nspc.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD add_namespaces.

    DATA lt_tadir_nspc TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    " Namespaces are not in TADIR, but are necessary for creating objects in transportable packages
    LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE obj_name(1) = '/'.
      add_namespace(
        EXPORTING
          iv_package    = iv_package
          iv_object     = <ls_tadir>-obj_name
        CHANGING
          ct_tadir      = ct_tadir
          ct_tadir_nspc = lt_tadir_nspc ).
    ENDLOOP.

    " Root package of repo might not exist yet but needs to be considered, too
    IF iv_package CP '/*'.
      add_namespace(
        EXPORTING
          iv_package    = iv_package
          iv_object     = iv_package
        CHANGING
          ct_tadir      = ct_tadir
          ct_tadir_nspc = lt_tadir_nspc ).
    ENDIF.

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

      IF zcl_abapgit_objects=>exists( ls_item ) = abap_true.
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
      <ls_tadir>-korrnum = ''.
    ENDLOOP.

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
    ls_exclude-low    = 'SOTR'. " automatically created for SAP packages (DEVC)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SOTS'. " automatically created for SAP packages (DEVC)
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


  METHOD zif_abapgit_tadir~delete_single.

    DATA ls_tadir TYPE tadir.

    " cast
    ls_tadir-pgmid    = iv_pgmid.
    ls_tadir-object   = iv_object.
    ls_tadir-obj_name = iv_obj_name.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry          = abap_true
        wi_tadir_pgmid                 = ls_tadir-pgmid
        wi_tadir_object                = ls_tadir-object
        wi_tadir_obj_name              = ls_tadir-obj_name
        wi_test_modus                  = abap_false
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc > 1.
      " No error if entry does not exist
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_tadir~get_object_package.

    DATA: ls_tadir TYPE zif_abapgit_definitions=>ty_tadir,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.

    ls_tadir = zif_abapgit_tadir~read_single(
      iv_pgmid    = iv_pgmid
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).

    IF ls_tadir-delflag = abap_true.
      RETURN. "Mark for deletion -> return nothing
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.
    ls_item-devclass = ls_tadir-devclass.

    IF zcl_abapgit_objects=>exists( ls_item ) = abap_false.
      RETURN.
    ENDIF.

    rv_devclass = ls_tadir-devclass.

  ENDMETHOD.


  METHOD zif_abapgit_tadir~insert_single.

    DATA ls_tadir TYPE tadir.

    " cast
    ls_tadir-pgmid    = iv_pgmid.
    ls_tadir-object   = iv_object.
    ls_tadir-obj_name = iv_obj_name.
    ls_tadir-devclass = iv_package.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = abap_false
        wi_tadir_pgmid                 = ls_tadir-pgmid
        wi_tadir_object                = ls_tadir-object
        wi_tadir_obj_name              = ls_tadir-obj_name
        wi_tadir_author                = sy-uname
        wi_tadir_devclass              = ls_tadir-devclass
        wi_tadir_masterlang            = iv_language
        wi_tadir_srcsystem             = iv_srcsystem
        wi_set_genflag                 = iv_set_genflag
        iv_set_edtflag                 = iv_set_edtflag
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read.

    DATA li_exit TYPE REF TO zif_abapgit_exit.
    DATA lr_tadir TYPE REF TO zif_abapgit_definitions=>ty_tadir.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_dot_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.

    ASSERT iv_package IS NOT INITIAL.

    " Start recursion
    " hmm, some problems here, should TADIR also build path?
    rt_tadir = build(
      iv_package            = iv_package
      io_dot                = io_dot
      iv_ignore_subpackages = iv_ignore_subpackages
      iv_only_local_objects = iv_only_local_objects ).

    IF io_dot IS NOT INITIAL.
      ls_dot_data = io_dot->get_data( ).
    ENDIF.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_tadir(
      EXPORTING
        iv_package            = iv_package
        ii_log                = ii_log
        is_dot_abapgit        = ls_dot_data
        iv_ignore_subpackages = iv_ignore_subpackages
        iv_only_local_objects = iv_only_local_objects
      CHANGING
        ct_tadir              = rt_tadir ).

    IF it_filter IS NOT INITIAL.
      "Apply filter manually instead of calling zcl_abapgit_repo_filter->apply,
      "so that we can execute a unit test. The method applies addition filtering
      "and does therefore additional selects
      lt_filter = it_filter.
      SORT lt_filter BY object obj_name.
      LOOP AT rt_tadir REFERENCE INTO lr_tadir.
        READ TABLE lt_filter TRANSPORTING NO FIELDS
                 WITH KEY object = lr_tadir->object
                          obj_name = lr_tadir->obj_name
                          BINARY SEARCH.
        IF sy-subrc <> 0.
          DELETE rt_tadir.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF iv_check_exists = abap_true.
      rt_tadir = check_exists( rt_tadir ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_tadir~read_single.

    SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC
    CLEAR rs_tadir-korrnum.

  ENDMETHOD.
ENDCLASS.
