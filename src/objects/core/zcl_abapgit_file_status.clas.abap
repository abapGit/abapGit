CLASS zcl_abapgit_file_status DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS status
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS calculate_status
      IMPORTING
        !iv_devclass      TYPE devclass
        !io_dot           TYPE REF TO zcl_abapgit_dot_abapgit
        !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote        TYPE zif_abapgit_definitions=>ty_files_tt
        !it_cur_state     TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS prepare_remote
      IMPORTING
        !io_dot          TYPE REF TO zcl_abapgit_dot_abapgit
        !it_remote       TYPE zif_abapgit_definitions=>ty_files_tt
      RETURNING
        VALUE(rt_remote) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS process_local
      IMPORTING
        !iv_devclass  TYPE devclass
        !io_dot       TYPE REF TO zcl_abapgit_dot_abapgit
        !it_local     TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_state_idx TYPE zif_abapgit_definitions=>ty_file_signatures_ts
      CHANGING
        !ct_remote    TYPE zif_abapgit_definitions=>ty_files_tt
        !ct_items     TYPE zif_abapgit_definitions=>ty_items_tt
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS process_items
      IMPORTING
        !iv_devclass TYPE devclass
        !io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
        !it_local    TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote   TYPE zif_abapgit_definitions=>ty_files_tt
      CHANGING
        !ct_items    TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS process_remote
      IMPORTING
        !iv_devclass  TYPE devclass
        !io_dot       TYPE REF TO zcl_abapgit_dot_abapgit
        !it_local     TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote    TYPE zif_abapgit_definitions=>ty_files_tt
        !it_state_idx TYPE zif_abapgit_definitions=>ty_file_signatures_ts
        !it_items_idx TYPE zif_abapgit_definitions=>ty_items_ts
      CHANGING
        !ct_results   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS run_checks
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS build_existing
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
        !is_remote       TYPE zif_abapgit_definitions=>ty_file
        !it_state        TYPE zif_abapgit_definitions=>ty_file_signatures_ts
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result .
    CLASS-METHODS build_new_local
      IMPORTING
        !is_local        TYPE zif_abapgit_definitions=>ty_file_item
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result .
    CLASS-METHODS build_new_remote
      IMPORTING
        !iv_devclass     TYPE devclass
        !io_dot          TYPE REF TO zcl_abapgit_dot_abapgit
        !is_remote       TYPE zif_abapgit_definitions=>ty_file
        !it_items        TYPE zif_abapgit_definitions=>ty_items_ts
        !it_state        TYPE zif_abapgit_definitions=>ty_file_signatures_ts
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_object_package
      IMPORTING
        !iv_object         TYPE tadir-object
        !iv_obj_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rv_devclass) TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_package_move
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_files_folder
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_package_folder
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_multiple_files
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_namespace
      IMPORTING
        !ii_log     TYPE REF TO zif_abapgit_log
        !it_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_file_status IMPLEMENTATION.


  METHOD build_existing.

    DATA: ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    IF is_local-file-sha1 = is_remote-sha1.
      rs_result-match = abap_true.
      RETURN.
    ENDIF.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY path = is_local-file-path
      filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
      rs_result-match = boolc( rs_result-lstate IS INITIAL
        AND rs_result-rstate IS INITIAL ).
    ELSE.
      " This is a strange situation. As both local and remote exist
      " the state should also be present. Maybe this is a first run of the code.
      " In this case just compare hashes directly and mark both changed
      " the user will presumably decide what to do after checking the actual diff
      rs_result-match = boolc( is_local-file-sha1 = is_remote-sha1 ).
      IF rs_result-match = abap_false.
        rs_result-lstate = zif_abapgit_definitions=>c_state-modified.
        rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD build_new_local.

    " Item
    rs_result-obj_type = is_local-item-obj_type.
    rs_result-obj_name = is_local-item-obj_name.
    rs_result-package  = is_local-item-devclass.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match
    rs_result-match    = abap_false.
    rs_result-lstate   = zif_abapgit_definitions=>c_state-added.

  ENDMETHOD.


  METHOD build_new_remote.

    DATA: ls_item     LIKE LINE OF it_items,
          ls_file_sig LIKE LINE OF it_state.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = zif_abapgit_definitions=>c_state-added.

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = is_remote-filename
        iv_path     = is_remote-path
        iv_devclass = iv_devclass
        io_dot      = io_dot
      IMPORTING
        es_item     = ls_item ).

    " Check if in item index + get package
    READ TABLE it_items INTO ls_item
      WITH KEY obj_type = ls_item-obj_type obj_name = ls_item-obj_name
      BINARY SEARCH.

    IF sy-subrc = 0.

      " Completely new (xml, abap) and new file in an existing object
      rs_result-obj_type = ls_item-obj_type.
      rs_result-obj_name = ls_item-obj_name.
      rs_result-package  = ls_item-devclass.

      READ TABLE it_state INTO ls_file_sig
        WITH KEY path = is_remote-path filename = is_remote-filename
        BINARY SEARCH.

      " Existing file but from another package
      " was not added during local file proc as was not in tadir for repo package
      IF sy-subrc = 0.
        IF ls_file_sig-sha1 = is_remote-sha1.
          rs_result-match = abap_true.
          CLEAR rs_result-rstate.
        ELSE.
          rs_result-rstate = zif_abapgit_definitions=>c_state-modified.
        ENDIF.

        " Item is in state and in cache but with no package - it was deleted
        " OR devclass is the same as repo package (see #532)
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = iv_devclass.
          rs_result-match  = abap_false.
          rs_result-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.


  METHOD calculate_status.

    DATA:
      lt_remote    LIKE it_remote,
      lt_items     TYPE zif_abapgit_definitions=>ty_items_tt,
      lt_items_idx TYPE zif_abapgit_definitions=>ty_items_ts, " Sorted by obj_type+obj_name
      lt_state_idx TYPE zif_abapgit_definitions=>ty_file_signatures_ts. " Sorted by path+filename

    lt_state_idx = it_cur_state. " Force sort it

    " Prepare remote files
    lt_remote = prepare_remote(
      io_dot    = io_dot
      it_remote = it_remote ).

    " Process local files and new local files
    process_local(
      EXPORTING
        iv_devclass  = iv_devclass
        io_dot       = io_dot
        it_local     = it_local
        it_state_idx = lt_state_idx
      CHANGING
        ct_remote    = lt_remote
        ct_items     = lt_items
        ct_results   = rt_results ).

    " Complete item index for unmarked remote files
    process_items(
      EXPORTING
        iv_devclass = iv_devclass
        io_dot      = io_dot
        it_local    = it_local
        it_remote   = lt_remote
      CHANGING
        ct_items    = lt_items ).

    lt_items_idx = lt_items. " Self protection + UNIQUE records assertion

    " Process new remote files (marked above with empty SHA1)
    process_remote(
      EXPORTING
        iv_devclass  = iv_devclass
        io_dot       = io_dot
        it_local     = it_local
        it_remote    = lt_remote
        it_state_idx = lt_state_idx
        it_items_idx = lt_items_idx
      CHANGING
        ct_results   = rt_results ).

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING
      path ASCENDING.

  ENDMETHOD.


  METHOD check_files_folder.

    DATA:
      ls_item     TYPE zif_abapgit_definitions=>ty_item,
      lt_res_sort LIKE it_results,
      lt_item_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>     LIKE LINE OF it_results,
      <ls_result_idx> LIKE LINE OF it_results.

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
        ii_log->add( iv_msg = |Files for object { <ls_result>-obj_type } {
                              <ls_result>-obj_name } are not placed in the same folder|
                     iv_type = 'W'
                     iv_rc   = '1' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_multiple_files.

    DATA:
      lt_res_sort LIKE it_results,
      ls_file     TYPE zif_abapgit_definitions=>ty_file_signature.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_result> WHERE obj_type <> 'DEVC' AND packmove = abap_false.
      IF <ls_result>-filename IS NOT INITIAL AND <ls_result>-filename = ls_file-filename.
        ii_log->add( iv_msg  = |Multiple files with same filename, { <ls_result>-filename }|
                     iv_type = 'W'
                     iv_rc   = '3' ).
      ENDIF.

      IF <ls_result>-filename IS INITIAL.
        ii_log->add( iv_msg  = |Filename is empty for object { <ls_result>-obj_type } { <ls_result>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '4' ).
      ENDIF.

      MOVE-CORRESPONDING <ls_result> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_namespace.

    DATA:
      lv_namespace TYPE namespace,
      lt_namespace TYPE TABLE OF namespace,
      ls_trnspace  TYPE trnspace.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    " Collect all namespaces based on name of xml-files
    LOOP AT it_results ASSIGNING <ls_result>.
      FIND REGEX '#([a-zA-Z0-9]+)#.*\..*\.xml' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_namespace INTO lv_namespace.
      CALL FUNCTION 'TR_READ_NAMESPACE'
        EXPORTING
          iv_namespace           = lv_namespace
        IMPORTING
          es_trnspace            = ls_trnspace
        EXCEPTIONS
          namespace_not_existing = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        ii_log->add( iv_msg  = |Namespace { lv_namespace } does not exist. Create it in transaction SE03|
                     iv_type = 'W'
                     iv_rc   = '6' ).
      ELSEIF ls_trnspace-editflag <> 'X'.
        ii_log->add( iv_msg  = |Namespace { lv_namespace } is not modifiable. Check it in transaction SE03|
                     iv_type = 'W'
                     iv_rc   = '6' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_folder.

    DATA:
      lv_path         TYPE string,
      lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT package IS INITIAL AND NOT path IS INITIAL AND packmove = abap_false.

      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_result>-package ).

      IF lv_path <> <ls_result>-path.
        ii_log->add( iv_msg = |Package and path does not match for object, {
                       <ls_result>-obj_type } { <ls_result>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '2' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_move.

    DATA:
      lt_move_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>      LIKE LINE OF it_results,
      <ls_result_move> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE lstate = zif_abapgit_definitions=>c_state-added AND packmove = abap_true.

      READ TABLE lt_move_idx TRANSPORTING NO FIELDS
        WITH KEY obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted since it_result is sorted
      IF sy-subrc <> 0.
        ii_log->add( iv_msg  = |Changed package assignment for object {
                               <ls_result>-obj_type } { <ls_result>-obj_name }|
                     iv_type = 'W'
                     iv_rc   = '5' ).
        APPEND INITIAL LINE TO lt_move_idx ASSIGNING <ls_result_move>.
        <ls_result_move>-obj_type = <ls_result>-obj_type.
        <ls_result_move>-obj_name = <ls_result>-obj_name.
        <ls_result_move>-path     = <ls_result>-path.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_object_package.
    DATA: lv_name    TYPE devclass,
          li_package TYPE REF TO zif_abapgit_sap_package.

    rv_devclass = zcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).
    IF rv_devclass IS INITIAL AND iv_object = 'DEVC' AND iv_obj_name(1) = '$'.
      " local packages usually have no tadir entry
      lv_name = iv_obj_name.
      li_package = zcl_abapgit_factory=>get_sap_package( lv_name ).
      IF li_package->exists( ) = abap_true.
        rv_devclass = lv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD prepare_remote.

    DATA lv_index TYPE sy-index.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.

    rt_remote = it_remote.
    SORT rt_remote BY path filename.

    " Skip ignored files
    LOOP AT rt_remote ASSIGNING <ls_remote>.
      lv_index = sy-tabix.
      IF io_dot->is_ignored( iv_path     = <ls_remote>-path
                             iv_filename = <ls_remote>-filename ) = abap_true.
        DELETE rt_remote INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_items.

    DATA:
      ls_item         LIKE LINE OF ct_items,
      lv_is_xml       TYPE abap_bool,
      lv_sub_fetched  TYPE abap_bool,
      lt_sub_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
      lv_msg          TYPE string.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.

    LOOP AT it_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.

      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_remote>-filename
          iv_path     = <ls_remote>-path
          io_dot      = io_dot
          iv_devclass = iv_devclass
        IMPORTING
          es_item     = ls_item
          ev_is_xml   = lv_is_xml ).

      CHECK lv_is_xml = abap_true. " only object definitions

      ls_item-devclass = get_object_package(
        iv_object   = ls_item-obj_type
        iv_obj_name = ls_item-obj_name ).

      IF NOT ls_item-devclass IS INITIAL AND iv_devclass <> ls_item-devclass.
        IF lv_sub_fetched = abap_false.
          lt_sub_packages = zcl_abapgit_factory=>get_sap_package( iv_devclass )->list_subpackages( ).
          lv_sub_fetched = abap_true.
          SORT lt_sub_packages BY table_line. "Optimize Read Access
        ENDIF.

        " Make sure the package is under the repo main package
        READ TABLE lt_sub_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_item-devclass
          BINARY SEARCH.
        IF sy-subrc <> 0.
          IF ls_item-obj_type = 'DEVC'.
            " If package already exist but is not included in the package hierarchy of
            " the package assigned to the repository, then a manual change of the package
            " is required i.e. setting a parent package to iv_devclass (or one of its
            " subpackages). We don't this automatically since it's not clear where in the
            " hierarchy the new package should be located. (#4108)
            lv_msg = |Package { ls_item-devclass } is not a subpackage of { iv_devclass
                     }. Assign { ls_item-devclass } to package hierarchy of { iv_devclass
                     } and repeat process.|.
            zcx_abapgit_exception=>raise( lv_msg ).
          ELSE.
            CLEAR ls_item-devclass.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_item TO ct_items.
    ENDLOOP.

    SORT ct_items DESCENDING. " Default key - type, name, pkg, inactive
    DELETE ADJACENT DUPLICATES FROM ct_items COMPARING obj_type obj_name devclass.

  ENDMETHOD.


  METHOD process_local.

    DATA lv_msg TYPE string.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF ct_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_state>  LIKE LINE OF it_state_idx,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_local ASSIGNING <ls_local>.
      " Skip ignored files
      IF io_dot->is_ignored( iv_path     = <ls_local>-file-path
                             iv_filename = <ls_local>-file-filename ) = abap_true.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.
      IF <ls_local>-item IS NOT INITIAL.
        APPEND <ls_local>-item TO ct_items. " Collect for item index
      ENDIF.

      READ TABLE ct_remote ASSIGNING <ls_remote>
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      IF sy-subrc = 0.  " Exist local and remote
        <ls_result> = build_existing(
          is_local  = <ls_local>
          is_remote = <ls_remote>
          it_state  = it_state_idx ).
        IF <ls_remote>-sha1 IS INITIAL.
          IF <ls_local>-file-filename = zcl_abapgit_filename_logic=>c_package_file.
            lv_msg = |Package name conflict { <ls_local>-item-obj_type } { <ls_local>-item-obj_name }. | &&
              |Rename package or use FULL folder logic|.
          ELSE.
            lv_msg = |Checksum conflict { <ls_local>-item-obj_type } { <ls_local>-item-obj_name }. | &&
              |Please create an issue on Github|.
          ENDIF.
          zcx_abapgit_exception=>raise( lv_msg ).
        ENDIF.
        CLEAR <ls_remote>-sha1. " Mark as processed
      ELSE.             " Only L exists
        <ls_result> = build_new_local( <ls_local> ).
        " Check if same file exists in different location
        READ TABLE ct_remote ASSIGNING <ls_remote>
          WITH KEY filename = <ls_local>-file-filename.
        IF sy-subrc = 0 AND <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ELSEIF sy-subrc = 4.
          " Check if file existed before and was deleted remotely
          READ TABLE it_state_idx ASSIGNING <ls_state>
            WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
            BINARY SEARCH.
          IF sy-subrc = 0.
            IF <ls_local>-file-sha1 = <ls_state>-sha1.
              <ls_result>-lstate = zif_abapgit_definitions=>c_state-unchanged.
            ELSE.
              <ls_result>-lstate = zif_abapgit_definitions=>c_state-modified.
            ENDIF.
            <ls_result>-rstate = zif_abapgit_definitions=>c_state-deleted.
          ENDIF.
        ENDIF.
      ENDIF.
      <ls_result>-inactive = <ls_local>-item-inactive.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_remote.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF it_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_remote ASSIGNING <ls_remote> WHERE sha1 IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      <ls_result> = build_new_remote(
        iv_devclass = iv_devclass
        io_dot      = io_dot
        is_remote   = <ls_remote>
        it_items    = it_items_idx
        it_state    = it_state_idx ).

      " Check if same file exists in different location
      READ TABLE it_local ASSIGNING <ls_local>
        WITH KEY file-filename = <ls_remote>-filename.
      IF sy-subrc = 0.
        <ls_result>-match = abap_false.
        <ls_result>-lstate = zif_abapgit_definitions=>c_state-deleted.
        <ls_result>-rstate = zif_abapgit_definitions=>c_state-unchanged.
        IF <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ENDIF.
      ELSEIF sy-subrc = 4.
        " Check if file existed before and was deleted locally
        READ TABLE it_state_idx TRANSPORTING NO FIELDS
          WITH KEY path = <ls_remote>-path filename = <ls_remote>-filename
          BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_result>-lstate = zif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD run_checks.

    " This method just adds messages to the log. No log, nothing to do here
    IF ii_log IS INITIAL.
      RETURN.
    ENDIF.

    " Find all objects which were assigned to a different package
    check_package_move(
      ii_log     = ii_log
      it_results = it_results ).

    " Check files for one object is in the same folder
    check_files_folder(
      ii_log     = ii_log
      it_results = it_results ).

    " Check that objects are created in package corresponding to folder
    check_package_folder(
      ii_log     = ii_log
      it_results = it_results
      io_dot     = io_dot
      iv_top     = iv_top ).

    " Check for multiple files with same filename
    check_multiple_files(
      ii_log     = ii_log
      it_results = it_results ).

    " Check if namespaces exist already
    check_namespace(
      ii_log     = ii_log
      it_results = it_results ).

  ENDMETHOD.


  METHOD status.

    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_remote TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA li_exit TYPE REF TO zif_abapgit_exit.

    lt_local = io_repo->get_files_local( ii_log ).

    IF lines( lt_local ) <= 2.
      " Less equal two means that we have only the .abapgit.xml and the package in
      " our local repository. In this case we have to update our local .abapgit.xml
      " from the remote one. Otherwise we get errors when e.g. the folder starting
      " folder is different.
      io_repo->find_remote_dot_abapgit( ).
    ENDIF.

    lt_remote = io_repo->get_files_remote( ).

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->pre_calculate_repo_status(
      EXPORTING
        is_repo_meta = io_repo->ms_data
      CHANGING
        ct_local  = lt_local
        ct_remote = lt_remote ).

    rt_results = calculate_status(
      iv_devclass  = io_repo->get_package( )
      io_dot       = io_repo->get_dot_abapgit( )
      it_local     = lt_local
      it_remote    = lt_remote
      it_cur_state = io_repo->get_local_checksums_per_file( ) ).

    run_checks(
      ii_log     = ii_log
      it_results = rt_results
      io_dot     = io_repo->get_dot_abapgit( )
      iv_top     = io_repo->get_package( ) ).

  ENDMETHOD.
ENDCLASS.
