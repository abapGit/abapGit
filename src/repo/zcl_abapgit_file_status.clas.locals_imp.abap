CLASS lcl_status_consistency_checks DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_root_package TYPE devclass
        io_dot          TYPE REF TO zcl_abapgit_dot_abapgit.

    METHODS run_checks
      IMPORTING
        it_results    TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(ri_log) TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.

    DATA mv_root_package TYPE devclass.
    DATA mo_dot          TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA mi_log          TYPE REF TO zif_abapgit_log.

    METHODS check_package_move
      IMPORTING
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS check_files_folder
      IMPORTING
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS check_package_sub_package
      IMPORTING
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
        !iv_top     TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS check_package_folder
      IMPORTING
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS check_multiple_files
      IMPORTING
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS check_namespace
      IMPORTING
        !it_results      TYPE zif_abapgit_definitions=>ty_results_tt
        !iv_root_package TYPE devclass
      RAISING
        zcx_abapgit_exception .

ENDCLASS.

CLASS lcl_status_consistency_checks IMPLEMENTATION.

  METHOD constructor.
    mv_root_package = iv_root_package.
    mo_dot          = io_dot.
  ENDMETHOD.

  METHOD run_checks.

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.

    " Find all objects which were assigned to a different package
    check_package_move( it_results ).

    " Check files for one object is in the same folder
    check_files_folder( it_results ).

    " Check that sub packages are included in the package hierarchy
    check_package_sub_package(
      it_results = it_results
      iv_top     = mv_root_package ).

    " Check that objects are created in package corresponding to folder
    check_package_folder(
      it_results = it_results
      io_dot     = mo_dot
      iv_top     = mv_root_package ).

    " Check for multiple files with same filename
    check_multiple_files( it_results ).

    " Check if namespaces exist already
    check_namespace(
      it_results      = it_results
      iv_root_package = mv_root_package ).

    ri_log = mi_log.

  ENDMETHOD.

  METHOD check_files_folder.

    DATA:
      ls_item     TYPE zif_abapgit_definitions=>ty_item,
      lt_res_sort LIKE it_results,
      lt_item_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>     LIKE LINE OF it_results,
      <ls_result_idx> LIKE LINE OF it_results.

    " TODO optimize ?
    " sort by obj, path
    " loop, and compare to first object record

    " Collect object index
    lt_res_sort = it_results.
    SORT lt_res_sort BY obj_type ASCENDING obj_name ASCENDING.

    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL AND packmove = abap_false.

      IF NOT ( <ls_result>-obj_type = ls_item-obj_type
          AND <ls_result>-obj_name = ls_item-obj_name ).
        APPEND INITIAL LINE TO lt_item_idx ASSIGNING <ls_result_idx>.
        <ls_result_idx>-obj_type = <ls_result>-obj_type.
        <ls_result_idx>-obj_name = <ls_result>-obj_name.
        <ls_result_idx>-path     = <ls_result>-path.
        MOVE-CORRESPONDING <ls_result> TO ls_item.
      ENDIF.

    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT obj_type IS INITIAL AND obj_type <> 'DEVC' AND packmove = abap_false.

      READ TABLE lt_item_idx ASSIGNING <ls_result_idx>
        WITH KEY obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted above

      IF sy-subrc <> 0 OR <ls_result>-path <> <ls_result_idx>-path. " All paths are same
        mi_log->add_warning( |Files for object { <ls_result>-obj_type } { <ls_result>-obj_name }|
         && | are not placed in the same folder| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_multiple_files.

    DATA:
      lt_res_sort LIKE it_results,
      ls_file     TYPE zif_abapgit_git_definitions=>ty_file_signature.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_result> WHERE obj_type <> 'DEVC' AND packmove = abap_false.
      IF <ls_result>-filename IS NOT INITIAL AND <ls_result>-filename = ls_file-filename.
        mi_log->add_warning( |Multiple files with same filename, { <ls_result>-filename }| ).
      ENDIF.

      IF <ls_result>-filename IS INITIAL.
        mi_log->add_warning( |Filename is empty for object { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
      ENDIF.

      MOVE-CORRESPONDING <ls_result> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_namespace.

    DATA:
      li_namespace       TYPE REF TO zif_abapgit_sap_namespace,
      lv_namespace       TYPE namespace,
      lt_namespace       TYPE TABLE OF namespace,
      lv_namespace_found TYPE abap_bool.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    " Collect all namespaces based on name of xml- and json-files
    LOOP AT it_results ASSIGNING <ls_result>.
      FIND REGEX '^#([a-zA-Z0-9]+)#.*\..*\.xml$' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
      FIND REGEX '^\(([a-zA-Z0-9]+)\).*\..*\.json$' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
    ENDLOOP.

    li_namespace = zcl_abapgit_factory=>get_sap_namespace( ).

    LOOP AT lt_namespace INTO lv_namespace.
      IF iv_root_package CS lv_namespace.
        lv_namespace_found = abap_true.
      ENDIF.

      IF li_namespace->exists( lv_namespace ) = abap_false.
        mi_log->add_warning( |Namespace { lv_namespace } does not exist.|
          && | Pull it first (or create it in transaction SE03)| ).
      ELSEIF li_namespace->is_editable( lv_namespace ) = abap_false.
        mi_log->add_warning( |Namespace { lv_namespace } is not modifiable. Check it in transaction SE03| ).
      ENDIF.
    ENDLOOP.

    IF lt_namespace IS NOT INITIAL AND lv_namespace_found = abap_false.
      mi_log->add_error( |Package { iv_root_package } is not part of the contained namespaces.|
          && | Remove repository and use a different package| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_package_folder.

    DATA:
      lv_path         TYPE string,
      lv_object       TYPE string,
      lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT package IS INITIAL AND NOT path IS INITIAL AND packmove = abap_false.

      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_result>-package ).

      lv_object = |{ <ls_result>-obj_type } { <ls_result>-obj_name }|.

      IF lv_path IS INITIAL.
        mi_log->add_error( |{ lv_object } already exists outside of { iv_top } package hierarchy| ).
      ELSEIF lv_path <> <ls_result>-path.
        mi_log->add_warning( |Package and path do not match for object { lv_object }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_move.

    DATA lt_move_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>      LIKE LINE OF it_results,
      <ls_result_move> LIKE LINE OF it_results.

    " TODO: optimize ?
    " delete where packmove = false, delete adj duplicates and fire messages ?
    LOOP AT it_results ASSIGNING <ls_result>
      WHERE lstate = zif_abapgit_definitions=>c_state-added AND packmove = abap_true.

      READ TABLE lt_move_idx TRANSPORTING NO FIELDS
        WITH KEY
          obj_type = <ls_result>-obj_type
          obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted since it_result is sorted
      IF sy-subrc <> 0.
        mi_log->add_warning( |Changed package assignment for object|
          && | { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
        APPEND INITIAL LINE TO lt_move_idx ASSIGNING <ls_result_move>.
        <ls_result_move>-obj_type = <ls_result>-obj_type.
        <ls_result_move>-obj_name = <ls_result>-obj_name.
        <ls_result_move>-path     = <ls_result>-path.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_sub_package.

    DATA lv_msg TYPE string.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result> WHERE package IS INITIAL AND obj_type = 'DEVC'.

      IF zcl_abapgit_factory=>get_sap_package( |{ <ls_result>-obj_name }| )->exists( ) = abap_true.
        " If package already exist but is not included in the package hierarchy of
        " the package assigned to the repository, then a manual change of the package
        " is required i.e. setting a parent package to the repo package (or one of its
        " subpackages). We don't do this automatically since it's not clear where in the
        " hierarchy the new package should be located or whether the sub package shall be
        " removed from the repo.
        lv_msg = |Package { <ls_result>-obj_name } already exists but is not a sub-package of { iv_top }. |
              && |Check your package and folder logic, and either assign { <ls_result>-obj_name } |
              && |to the package hierarchy of { iv_top } or remove package { <ls_result>-obj_name } |
              && |from the repository.|.
        mi_log->add_warning( lv_msg ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
