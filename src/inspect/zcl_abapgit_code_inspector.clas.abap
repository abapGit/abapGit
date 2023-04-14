CLASS zcl_abapgit_code_inspector DEFINITION
  PUBLIC
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_code_inspector .

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS validate_check_variant
      IMPORTING
        !iv_check_variant_name TYPE sci_chkv
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
    DATA mv_package TYPE devclass .

    METHODS create_variant
      IMPORTING
        !iv_variant       TYPE sci_chkv
      RETURNING
        VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant
      RAISING
        zcx_abapgit_exception .
    METHODS cleanup
      IMPORTING
        !io_set TYPE REF TO cl_ci_objectset
      RAISING
        zcx_abapgit_exception .
    METHODS skip_object
      IMPORTING
        !is_obj        TYPE scir_objs
      RETURNING
        VALUE(rv_skip) TYPE abap_bool.
  PRIVATE SECTION.

    DATA mv_success TYPE abap_bool .
    DATA mv_summary TYPE string.

    TYPES: ty_run_mode TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF co_run_mode,
        run_with_popup   TYPE ty_run_mode VALUE 'P',
        run_after_popup  TYPE ty_run_mode VALUE 'A',
        run_via_rfc      TYPE ty_run_mode VALUE 'R',
        run_in_batch     TYPE ty_run_mode VALUE 'B',
        run_loc_parallel TYPE ty_run_mode VALUE 'L',
        run_direct       TYPE ty_run_mode VALUE 'L',
      END OF co_run_mode .
    DATA mo_inspection TYPE REF TO cl_ci_inspection .
    DATA mv_name TYPE sci_objs .
    DATA mv_run_mode TYPE c LENGTH 1 .

    METHODS create_objectset
      RETURNING
        VALUE(ro_set) TYPE REF TO cl_ci_objectset .
    METHODS run_inspection
      IMPORTING
        !io_inspection TYPE REF TO cl_ci_inspection
      RETURNING
        VALUE(rt_list) TYPE scit_alvlist
      RAISING
        zcx_abapgit_exception .
    METHODS create_inspection
      IMPORTING
        !io_set              TYPE REF TO cl_ci_objectset
        !io_variant          TYPE REF TO cl_ci_checkvariant
      RETURNING
        VALUE(ro_inspection) TYPE REF TO cl_ci_inspection
      RAISING
        zcx_abapgit_exception .
    METHODS decide_run_mode
      RETURNING
        VALUE(rv_run_mode) TYPE ty_run_mode .
    METHODS filter_inspection
      CHANGING
        !ct_list TYPE scit_alvlist .
ENDCLASS.



CLASS zcl_abapgit_code_inspector IMPLEMENTATION.


  METHOD cleanup.

    IF mo_inspection IS BOUND.

      mo_inspection->delete(
        EXCEPTIONS
          locked              = 1
          error_in_enqueue    = 2
          not_authorized      = 3
          exceptn_appl_exists = 4
          OTHERS              = 5 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Couldn't delete inspection. Subrc = { sy-subrc }| ).
      ENDIF.

    ENDIF.

    io_set->delete(
      EXCEPTIONS
        exists_in_insp   = 1
        locked           = 2
        error_in_enqueue = 3
        not_authorized   = 4
        exists_in_objs   = 5
        OTHERS           = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Couldn't delete objectset. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    IF iv_package IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply package| ).
    ENDIF.

    mv_package = iv_package.

    " We create the inspection and objectset with dummy names.
    " Because we want to persist them so we can run it in parallel.
    " Both are deleted afterwards.
    mv_name = |{ sy-uname }_{ sy-datum }_{ sy-uzeit }|.
    mv_run_mode = decide_run_mode( ).

  ENDMETHOD.


  METHOD create_inspection.

    cl_ci_inspection=>create(
      EXPORTING
        p_user           = sy-uname
        p_name           = mv_name
      RECEIVING
        p_ref            = ro_inspection
      EXCEPTIONS
        locked           = 1
        error_in_enqueue = 2
        not_authorized   = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to create inspection. Subrc = { sy-subrc }| ).
    ENDIF.

    ro_inspection->set(
      p_chkv = io_variant
      p_objs = io_set ).

    ro_inspection->save(
      EXCEPTIONS
        missing_information = 1
        insp_no_name        = 2
        not_enqueued        = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Failed to save inspection. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD create_objectset.

    DATA: lt_objs       TYPE scit_objs,
          ls_obj        TYPE scir_objs,
          lt_objs_check TYPE scit_objs,
          ls_item       TYPE zif_abapgit_definitions=>ty_item,
          lt_packages   TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    lt_packages = zcl_abapgit_factory=>get_sap_package( mv_package )->list_subpackages( ).
    INSERT mv_package INTO TABLE lt_packages.

    SELECT object AS objtype obj_name AS objname
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE lt_objs
      FOR ALL ENTRIES IN lt_packages
      WHERE devclass = lt_packages-table_line
      AND delflag = abap_false
      AND pgmid = 'R3TR' ##TOO_MANY_ITAB_FIELDS.        "#EC CI_GENBUFF

    LOOP AT lt_objs INTO ls_obj.

      IF skip_object( ls_obj ) = abap_true.
        CONTINUE.
      ENDIF.

      ls_item-obj_type = ls_obj-objtype.
      ls_item-obj_name = ls_obj-objname.

      IF zcl_abapgit_objects=>exists( ls_item ) = abap_false.
        CONTINUE.
      ENDIF.

      INSERT ls_obj INTO TABLE lt_objs_check.

    ENDLOOP.

    ro_set = cl_ci_objectset=>save_from_list(
      p_name    = mv_name
      p_objects = lt_objs_check ).

  ENDMETHOD.


  METHOD create_variant.

    IF iv_variant IS INITIAL.
      zcx_abapgit_exception=>raise( |No check variant supplied.| ).
    ENDIF.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_variant
      RECEIVING
        p_ref                    = ro_variant
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    CASE sy-subrc.
      WHEN 1.
        zcx_abapgit_exception=>raise( |Check variant { iv_variant } doesn't exist| ).
      WHEN 2.
        zcx_abapgit_exception=>raise( |Parameter missing for check variant { iv_variant }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD decide_run_mode.

    DATA lo_settings TYPE REF TO zcl_abapgit_settings.
    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    IF sy-batch = abap_true.
      " We have to disable parallelization in batch because of lock errors.
      rv_run_mode = co_run_mode-run_via_rfc.
    ELSEIF lo_settings->get_parallel_proc_disabled( ) = abap_false.
      rv_run_mode = co_run_mode-run_loc_parallel.
    ELSE.
      rv_run_mode = co_run_mode-run_via_rfc.
    ENDIF.

  ENDMETHOD.


  METHOD filter_inspection.

    " Remove findings in LSVIM* includes which are part of generated maintenance screens
    DELETE ct_list WHERE sobjtype = 'PROG' AND sobjname CP 'LSVIM*'.

  ENDMETHOD.


  METHOD run_inspection.

    io_inspection->run(
      EXPORTING
        p_howtorun            = mv_run_mode
      EXCEPTIONS
        invalid_check_version = 1
        OTHERS                = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Code inspector run failed. Subrc = { sy-subrc }| ).
    ENDIF.

    io_inspection->plain_list( IMPORTING p_list = rt_list ).

    filter_inspection( CHANGING ct_list = rt_list ).

    SORT rt_list BY objtype objname test code sobjtype sobjname line col.

    DELETE ADJACENT DUPLICATES FROM rt_list.

  ENDMETHOD.


  METHOD skip_object.

    DATA ls_program_type TYPE subc.

    CASE is_obj-objtype.
      WHEN 'PROG'.

        SELECT SINGLE subc
          INTO ls_program_type
          FROM trdir
          WHERE name = is_obj-objname.

        rv_skip = boolc( ls_program_type = 'I' ). " Include program.

      WHEN OTHERS.
        rv_skip = abap_false.

    ENDCASE.

  ENDMETHOD.


  METHOD validate_check_variant.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_check_variant_name
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |No valid check variant { iv_check_variant_name  }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_code_inspector~get_summary.
    rv_summary = mv_summary.
  ENDMETHOD.


  METHOD zif_abapgit_code_inspector~is_successful.

    rv_success = mv_success.

  ENDMETHOD.


  METHOD zif_abapgit_code_inspector~run.

    DATA: lo_set     TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lv_count   TYPE i,
          lt_list    TYPE scit_alvlist,
          ls_list    LIKE LINE OF lt_list,
          ls_result  LIKE LINE OF rt_list,
          lo_timer   TYPE REF TO zcl_abapgit_timer,
          lx_error   TYPE REF TO zcx_abapgit_exception.

    TRY.
        lo_set = create_objectset( ).

        lv_count = lines( lo_set->iobjlst-objects ).
        IF lv_count = 0.
          " no objects, nothing to check
          RETURN.
        ENDIF.

        lo_timer = zcl_abapgit_timer=>create( iv_count = lv_count )->start( ).

        lo_variant = create_variant( iv_variant ).

        mo_inspection = create_inspection(
          io_set     = lo_set
          io_variant = lo_variant ).

        lt_list = run_inspection( mo_inspection ).

        cleanup( lo_set ).

        LOOP AT lt_list INTO ls_list.
          MOVE-CORRESPONDING ls_list TO ls_result.
          INSERT ls_result INTO TABLE rt_list.
        ENDLOOP.

        IF iv_save = abap_true.
          READ TABLE rt_list TRANSPORTING NO FIELDS WITH KEY kind = 'E'.
          mv_success = boolc( sy-subrc <> 0 ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO lx_error.

        " ensure cleanup
        cleanup( lo_set ).
        zcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

    mv_summary = lo_timer->end( ).

  ENDMETHOD.
ENDCLASS.
