CLASS zcl_abapgit_dot_abapgit_local DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_file,
        path          TYPE string,
        filename      TYPE string,
        sha1_original TYPE zif_abapgit_git_definitions=>ty_sha1,
        sha1_new      TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_file.
    TYPES:
      ty_files TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_marker,
        system_id              TYPE sy-sysid,
        client                 TYPE sy-mandt,
        basis_release          TYPE string,
        basis_sp               TYPE string,
        abapgit_version        TYPE string,
        exported_at            TYPE timestampl,
        protect_system_changes TYPE zif_abapgit_persistence=>ty_local_settings-protect_system_changes,
        files                  TYPE ty_files,
        base                   TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt,
      END OF ty_marker.

    CONSTANTS:
      BEGIN OF c_state,
        applied  TYPE c LENGTH 1 VALUE 'A',
        to_apply TYPE c LENGTH 1 VALUE 'P',
        conflict TYPE c LENGTH 1 VALUE 'C',
      END OF c_state.

    TYPES:
      BEGIN OF ty_mark_file,
        path     TYPE string,
        filename TYPE string,
        state    TYPE c LENGTH 1,
      END OF ty_mark_file.
    TYPES:
      ty_mark_files TYPE HASHED TABLE OF ty_mark_file WITH UNIQUE KEY path filename.
    TYPES:
      BEGIN OF ty_mark_object,
        obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type,
        obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name,
        state    TYPE c LENGTH 1,
      END OF ty_mark_object.
    TYPES:
      ty_mark_objects TYPE HASHED TABLE OF ty_mark_object WITH UNIQUE KEY obj_type obj_name.
    TYPES:
      BEGIN OF ty_marking,
        files   TYPE ty_mark_files,
        objects TYPE ty_mark_objects,
      END OF ty_marking.

    "! Marker file for the export, a full export lists the exported files as base list
    CLASS-METHODS create_for_export
      IMPORTING
        !ii_repo         TYPE REF TO zif_abapgit_repo
        !it_files        TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rs_marker) TYPE ty_marker.

    CLASS-METHODS to_file
      IMPORTING
        !is_marker     TYPE ty_marker
      RETURNING
        VALUE(rs_file) TYPE zif_abapgit_git_definitions=>ty_file
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS from_file
      IMPORTING
        !is_file         TYPE zif_abapgit_git_definitions=>ty_file
      RETURNING
        VALUE(rs_marker) TYPE ty_marker
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS from_xml
      IMPORTING
        !iv_xml          TYPE string
      RETURNING
        VALUE(rs_marker) TYPE ty_marker
      RAISING
        zcx_abapgit_exception.

    "! Pull of the repo is restricted to the files listed in the marker file
    CLASS-METHODS is_active
      IMPORTING
        !ii_repo         TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rv_active) TYPE abap_bool.

    "! Check the marker file of the ZIP against the current system, see check_files. The objects and
    "! the table content of the listed files are read again before the check
    CLASS-METHODS check_pull
      IMPORTING
        !ii_repo        TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_ts
      RAISING
        zcx_abapgit_exception.

    "! Compare every listed file with the system file and the ZIP file. Raise an exception if the
    "! list is invalid or a listed change conflicts with the system, return the objects to pull
    CLASS-METHODS check_files
      IMPORTING
        !it_files       TYPE ty_files
        !it_local       TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote      TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_package     TYPE devclass
        !io_dot         TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_ts
      RAISING
        zcx_abapgit_exception.

    "! Keep only the results of the given objects
    CLASS-METHODS filter_results
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
        !it_items         TYPE zif_abapgit_definitions=>ty_items_ts
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

    "! Keep only the results of objects that have a decision
    CLASS-METHODS filter_results_by_overwrite
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
        !it_overwrite     TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

    "! Raise an exception if an object to pull has no decision, the system changed since the decisions
    CLASS-METHODS check_decided
      IMPORTING
        !it_items     TYPE zif_abapgit_definitions=>ty_items_ts
        !it_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception.

    "! Serialize the given objects again into the local files of the repo, like a full serialization.
    "! Table content is skipped
    CLASS-METHODS refresh_objects
      IMPORTING
        !ii_repo  TYPE REF TO zif_abapgit_repo
        !it_items TYPE zif_abapgit_definitions=>ty_items_ts
      RAISING
        zcx_abapgit_exception.

    "! Keep only the decisions of the given objects
    CLASS-METHODS filter_overwrite
      IMPORTING
        !it_overwrite       TYPE zif_abapgit_definitions=>ty_overwrite_tt
        !it_items           TYPE zif_abapgit_definitions=>ty_items_ts
      RETURNING
        VALUE(rt_overwrite) TYPE zif_abapgit_definitions=>ty_overwrite_tt.

    "! Files as the pull leaves them: the files listed in the marker file from the ZIP, all other
    "! files as they are in the system. Without a valid marker file nothing is listed
    CLASS-METHODS apply_listed_files
      IMPORTING
        !it_remote      TYPE zif_abapgit_git_definitions=>ty_files_tt
        !it_system      TYPE zif_abapgit_git_definitions=>ty_files_tt OPTIONAL
        !io_dot         TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt.

    "! State of the listed files and their objects for the repository view, conflicts included.
    "! Files and objects that are not returned are not listed and stay untouched by the pull
    CLASS-METHODS get_marking
      IMPORTING
        !ii_repo          TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rs_marking) TYPE ty_marking
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_row,
        path          TYPE string,
        filename      TYPE string,
        sha1_original TYPE zif_abapgit_git_definitions=>ty_sha1,
        sha1_new      TYPE zif_abapgit_git_definitions=>ty_sha1,
        sha1_system   TYPE zif_abapgit_git_definitions=>ty_sha1,
        state         TYPE c LENGTH 1,
        listed        TYPE abap_bool,
        obj_type      TYPE zif_abapgit_definitions=>ty_item-obj_type,
        obj_name      TYPE zif_abapgit_definitions=>ty_item-obj_name,
        data_name     TYPE zif_abapgit_definitions=>ty_item-obj_name,
      END OF ty_row.
    TYPES:
      ty_rows TYPE SORTED TABLE OF ty_row WITH UNIQUE KEY path filename.
    TYPES:
      BEGIN OF ty_local,
        path     TYPE string,
        filename TYPE string,
        sha1     TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_local.
    TYPES:
      ty_local_ts TYPE HASHED TABLE OF ty_local WITH UNIQUE KEY path filename.
    TYPES:
      BEGIN OF ty_object,
        obj_type  TYPE zif_abapgit_definitions=>ty_item-obj_type,
        obj_name  TYPE zif_abapgit_definitions=>ty_item-obj_name,
        pending   TYPE abap_bool,
        to_apply  TYPE abap_bool,
        known     TYPE abap_bool,
        in_zip    TYPE abap_bool,
        in_system TYPE abap_bool,
        missing   TYPE abap_bool,
        clash     TYPE abap_bool,
      END OF ty_object.
    TYPES:
      ty_objects TYPE SORTED TABLE OF ty_object WITH UNIQUE KEY obj_type obj_name.
    TYPES:
      ty_files_ts TYPE HASHED TABLE OF ty_file WITH UNIQUE KEY path filename.
    TYPES:
      ty_names TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    CLASS-METHODS to_xml
      IMPORTING
        !is_marker    TYPE ty_marker
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception.

    "! Marker file of the ZIP with its change list. Files abapGit ignores are not in the change list
    CLASS-METHODS read_marker
      IMPORTING
        !it_remote       TYPE zif_abapgit_git_definitions=>ty_files_tt
        !io_dot          TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rs_marker) TYPE ty_marker
      RAISING
        zcx_abapgit_exception.

    "! The marker XML has a FILES element, an empty one included, directly in asx:abap/asx:values/DATA
    CLASS-METHODS has_files_element
      IMPORTING
        !iv_xml        TYPE string
      RETURNING
        VALUE(rv_bool) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_child
      IMPORTING
        !ii_parent      TYPE REF TO if_ixml_node
        !iv_name        TYPE string
      RETURNING
        VALUE(ri_child) TYPE REF TO if_ixml_node.

    "! Change list of a marker file without one: the files of the ZIP that differ from the base list.
    "! A file of the base list that is missing in the ZIP is not deleted, a partial ZIP stays valid
    CLASS-METHODS derive_files
      IMPORTING
        !it_base        TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
        !it_remote      TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(rt_files) TYPE ty_files
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS read_listed
      IMPORTING
        !it_remote       TYPE zif_abapgit_git_definitions=>ty_files_tt
        !io_dot          TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rt_listed) TYPE ty_files_ts.

    CLASS-METHODS refresh_listed_objects
      IMPORTING
        !ii_repo  TYPE REF TO zif_abapgit_repo
        !it_files TYPE ty_files
      RAISING
        zcx_abapgit_exception.

    "! System files for the check and the repository view. With iv_reread, the listed objects and
    "! tables are read again, else only the tables whose data config selection the pull changes
    CLASS-METHODS get_local
      IMPORTING
        !ii_repo        TYPE REF TO zif_abapgit_repo
        !it_files       TYPE ty_files
        !it_remote      TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_reread      TYPE abap_bool
      RETURNING
        VALUE(rt_local) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS check_listed_data
      IMPORTING
        !it_listed  TYPE ty_files_ts
        !it_configs TYPE zif_abapgit_data_config=>ty_config_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS refresh_listed_data
      IMPORTING
        !ii_repo   TYPE REF TO zif_abapgit_repo
        !it_files  TYPE ty_files
        !it_remote TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_reread TYPE abap_bool
      CHANGING
        !ct_local  TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

    "! Freshly read data files get the same exit processing as in the export (with the data config
    "! files), so their SHA1 is comparable with the exported file. Only the given data files are kept
    CLASS-METHODS postprocess_data
      IMPORTING
        !iv_package     TYPE devclass
        !io_dot         TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_config      TYPE REF TO zif_abapgit_data_config
        !it_names       TYPE ty_names
        !it_files       TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS evaluate
      IMPORTING
        !it_files     TYPE ty_files
        !it_local     TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_package   TYPE devclass
        !io_dot       TYPE REF TO zcl_abapgit_dot_abapgit
      EXPORTING
        !et_rows      TYPE ty_rows
        !et_unlisted  TYPE ty_rows
        !et_missing   TYPE ty_rows
        !et_objects   TYPE ty_objects
        !et_conflicts TYPE string_table
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS mark_files
      IMPORTING
        !it_rows        TYPE ty_rows
        !it_objects     TYPE ty_objects
      RETURNING
        VALUE(rt_files) TYPE ty_mark_files.

    CLASS-METHODS mark_objects
      IMPORTING
        !it_rows          TYPE ty_rows
        !it_objects       TYPE ty_objects
        !it_files         TYPE ty_mark_files
      RETURNING
        VALUE(rt_objects) TYPE ty_mark_objects.

    CLASS-METHODS map_data_file
      IMPORTING
        !iv_path       TYPE string
        !iv_filename   TYPE string
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rv_name) TYPE zif_abapgit_definitions=>ty_item-obj_name
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS index_local
      IMPORTING
        !it_local       TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_local) TYPE ty_local_ts.

    CLASS-METHODS build_rows
      IMPORTING
        !it_files      TYPE ty_files
        !it_local      TYPE ty_local_ts
        !it_remote     TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_package    TYPE devclass
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rt_rows) TYPE ty_rows
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS evaluate_row
      IMPORTING
        !it_local TYPE ty_local_ts
      CHANGING
        !cs_row   TYPE ty_row.

    CLASS-METHODS is_object_file
      IMPORTING
        !iv_path       TYPE string
        !iv_filename   TYPE string
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS map_listed_file
      IMPORTING
        !iv_path       TYPE string
        !iv_filename   TYPE string
        !iv_package    TYPE devclass
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rs_item) TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS build_objects
      IMPORTING
        !it_rows          TYPE ty_rows
        !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_objects) TYPE ty_objects.

    CLASS-METHODS find_unlisted
      IMPORTING
        !it_rows           TYPE ty_rows
        !it_local          TYPE ty_local_ts
        !it_remote         TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_package        TYPE devclass
        !io_dot            TYPE REF TO zcl_abapgit_dot_abapgit
        !it_objects        TYPE ty_objects
      RETURNING
        VALUE(rt_unlisted) TYPE ty_rows.

    CLASS-METHODS check_objects
      IMPORTING
        !it_unlisted TYPE ty_rows
      CHANGING
        !ct_objects  TYPE ty_objects.

    CLASS-METHODS collect_object_conflicts
      IMPORTING
        !it_objects         TYPE ty_objects
      RETURNING
        VALUE(rt_conflicts) TYPE string_table.

    CLASS-METHODS collect_row_conflicts
      IMPORTING
        !it_rows            TYPE ty_rows
        !it_objects         TYPE ty_objects
      RETURNING
        VALUE(rt_conflicts) TYPE string_table.

    CLASS-METHODS find_missing_in_zip
      IMPORTING
        !it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_rows          TYPE ty_rows
        !it_remote        TYPE zif_abapgit_git_definitions=>ty_files_tt
        !it_objects       TYPE ty_objects
      RETURNING
        VALUE(rt_missing) TYPE ty_rows.

    CLASS-METHODS raise_invalid
      IMPORTING
        !iv_path     TYPE string
        !iv_filename TYPE string
        !iv_reason   TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS raise_conflicts
      IMPORTING
        !it_conflicts TYPE string_table
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS fits_t100_text
      IMPORTING
        !iv_text      TYPE string
      RETURNING
        VALUE(rv_fit) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_dot_abapgit_local IMPLEMENTATION.


  METHOD apply_listed_files.

    DATA lt_listed TYPE ty_files_ts.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_remote.

    lt_listed = read_listed(
      it_remote = it_remote
      io_dot    = io_dot ).

    LOOP AT it_system ASSIGNING <ls_file>.
      READ TABLE lt_listed TRANSPORTING NO FIELDS
        WITH TABLE KEY path = <ls_file>-path filename = <ls_file>-filename.
      IF sy-subrc <> 0.
        INSERT <ls_file> INTO TABLE rt_files.
      ENDIF.
    ENDLOOP.

    LOOP AT it_remote ASSIGNING <ls_file>.
      READ TABLE lt_listed TRANSPORTING NO FIELDS
        WITH TABLE KEY path = <ls_file>-path filename = <ls_file>-filename.
      IF sy-subrc = 0.
        INSERT <ls_file> INTO TABLE rt_files.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_objects.

    DATA ls_object TYPE ty_object.

    FIELD-SYMBOLS <ls_row> LIKE LINE OF it_rows.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF rt_objects.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.

    LOOP AT it_rows ASSIGNING <ls_row> WHERE obj_type IS NOT INITIAL.

      READ TABLE rt_objects ASSIGNING <ls_object>
        WITH TABLE KEY obj_type = <ls_row>-obj_type obj_name = <ls_row>-obj_name.
      IF sy-subrc <> 0.
        CLEAR ls_object.
        ls_object-obj_type = <ls_row>-obj_type.
        ls_object-obj_name = <ls_row>-obj_name.
        INSERT ls_object INTO TABLE rt_objects ASSIGNING <ls_object>.
      ENDIF.

      IF <ls_row>-sha1_original IS NOT INITIAL.
        <ls_object>-known = abap_true.
      ENDIF.
      IF <ls_row>-sha1_new IS NOT INITIAL.
        <ls_object>-in_zip = abap_true.
      ENDIF.
      IF <ls_row>-state <> c_state-applied.
        <ls_object>-pending = abap_true.
      ENDIF.
      IF <ls_row>-state = c_state-to_apply.
        <ls_object>-to_apply = abap_true.
      ENDIF.

    ENDLOOP.

    IF rt_objects IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_local ASSIGNING <ls_local> WHERE item-obj_type IS NOT INITIAL.
      READ TABLE rt_objects ASSIGNING <ls_object>
        WITH TABLE KEY obj_type = <ls_local>-item-obj_type obj_name = <ls_local>-item-obj_name.
      IF sy-subrc = 0.
        <ls_object>-in_system = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_rows.

    DATA ls_row TYPE ty_row.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_in_zip TYPE abap_bool.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.
    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.

    LOOP AT it_files ASSIGNING <ls_file>.

      CLEAR ls_row.
      ls_row-path          = <ls_file>-path.
      ls_row-filename      = <ls_file>-filename.
      ls_row-sha1_original = <ls_file>-sha1_original.
      ls_row-sha1_new      = <ls_file>-sha1_new.
      ls_row-listed        = abap_true.

      IF ls_row-path IS INITIAL OR ls_row-filename IS INITIAL.
        raise_invalid(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_reason   = 'path or filename missing' ).
      ELSEIF ls_row-sha1_original IS INITIAL AND ls_row-sha1_new IS INITIAL.
        raise_invalid(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_reason   = 'original and new SHA1 both empty' ).
      ENDIF.

      " The new SHA1 is the file in the ZIP, empty for a file deleted in the ZIP
      READ TABLE it_remote ASSIGNING <ls_remote> WITH TABLE KEY file_path
        COMPONENTS path = ls_row-path filename = ls_row-filename.
      lv_in_zip = boolc( sy-subrc = 0 ).
      IF lv_in_zip = abap_false AND ls_row-sha1_new IS NOT INITIAL.
        raise_invalid(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_reason   = 'not in the ZIP' ).
      ELSEIF lv_in_zip = abap_true AND ls_row-sha1_new IS INITIAL.
        raise_invalid(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_reason   = 'listed as deleted, but in the ZIP' ).
      ELSEIF lv_in_zip = abap_true AND <ls_remote>-sha1 <> ls_row-sha1_new.
        raise_invalid(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_reason   = 'new SHA1 differs from the file in the ZIP' ).
      ENDIF.

      evaluate_row(
        EXPORTING
          it_local = it_local
        CHANGING
          cs_row   = ls_row ).

      IF is_object_file( iv_path     = ls_row-path
                         iv_filename = ls_row-filename ) = abap_true.
        ls_item = map_listed_file(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_package  = iv_package
          io_dot      = io_dot ).
        ls_row-obj_type = ls_item-obj_type.
        ls_row-obj_name = ls_item-obj_name.
      ELSEIF ls_row-path = zif_abapgit_data_config=>c_default_path.
        ls_row-data_name = map_data_file(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          io_dot      = io_dot ).
      ENDIF.

      INSERT ls_row INTO TABLE rt_rows.
      IF sy-subrc <> 0.
        raise_invalid(
          iv_path     = ls_row-path
          iv_filename = ls_row-filename
          iv_reason   = 'listed more than once' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_decided.

    DATA lt_missing TYPE string_table.
    DATA lv_missing TYPE string.
    DATA lv_text TYPE string.
    DATA lv_longtext TYPE string.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.
      READ TABLE it_overwrite TRANSPORTING NO FIELDS WITH TABLE KEY object_type_and_name
        COMPONENTS obj_type = <ls_item>-obj_type obj_name = <ls_item>-obj_name.
      IF sy-subrc <> 0.
        lv_missing = |{ <ls_item>-obj_type } { <ls_item>-obj_name }|.
        APPEND lv_missing TO lt_missing.
      ENDIF.
    ENDLOOP.

    IF lt_missing IS INITIAL.
      RETURN.
    ENDIF.

    lv_text = |Pull stopped, the system changed since the check: { lines( lt_missing ) } object(s) to pull |
      && |have no decision. Refresh the repository and pull again|.

    lv_longtext = concat_lines_of(
      table = lt_missing
      sep   = cl_abap_char_utilities=>newline ).

    zcx_abapgit_exception=>raise(
      iv_text     = lv_text
      iv_longtext = lv_longtext ).

  ENDMETHOD.


  METHOD check_files.

    DATA lt_rows TYPE ty_rows.
    DATA lt_objects TYPE ty_objects.
    DATA lt_conflicts TYPE string_table.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.
    FIELD-SYMBOLS <ls_row> LIKE LINE OF lt_rows.

    evaluate(
      EXPORTING
        it_files     = it_files
        it_local     = it_local
        it_remote    = it_remote
        iv_package   = iv_package
        io_dot       = io_dot
      IMPORTING
        et_rows      = lt_rows
        et_objects   = lt_objects
        et_conflicts = lt_conflicts ).

    IF lt_conflicts IS NOT INITIAL.
      raise_conflicts( lt_conflicts ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE to_apply = abap_true.
      CLEAR ls_item.
      ls_item-obj_type = <ls_object>-obj_type.
      ls_item-obj_name = <ls_object>-obj_name.
      INSERT ls_item INTO TABLE rt_items.
    ENDLOOP.

    " Table content is pulled as object TABU, like the status and the pull decisions name it. The
    " content is written only if its own file is to apply, a changed data config alone is only saved
    LOOP AT lt_rows ASSIGNING <ls_row> WHERE state = c_state-to_apply AND data_name IS NOT INITIAL
        AND filename NP |*.{ zif_abapgit_data_config=>c_config }.{ zif_abapgit_data_config=>c_default_format }|.
      CLEAR ls_item.
      ls_item-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
      ls_item-obj_name = <ls_row>-data_name.
      INSERT ls_item INTO TABLE rt_items.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_listed_data.

    DATA lt_names TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA lv_filename TYPE string.

    FIELD-SYMBOLS <ls_config> LIKE LINE OF it_configs.
    FIELD-SYMBOLS <ls_listed> LIKE LINE OF it_listed.

    LOOP AT it_configs ASSIGNING <ls_config>.
      lv_filename = zcl_abapgit_data_utils=>build_data_filename( <ls_config> ).
      INSERT lv_filename INTO TABLE lt_names.
    ENDLOOP.

    " Table content is written only for tables in the data config, listed content must have one
    LOOP AT it_listed ASSIGNING <ls_listed> WHERE sha1_new IS NOT INITIAL
        AND filename NP |*.{ zif_abapgit_data_config=>c_config }.{ zif_abapgit_data_config=>c_default_format }|.
      READ TABLE lt_names TRANSPORTING NO FIELDS WITH TABLE KEY table_line = <ls_listed>-filename.
      IF sy-subrc <> 0.
        raise_invalid(
          iv_path     = <ls_listed>-path
          iv_filename = <ls_listed>-filename
          iv_reason   = 'table content without a data config in the ZIP or the system' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_objects.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_unlisted> LIKE LINE OF it_unlisted.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF ct_objects.

    " An unlisted file in the ZIP is unchanged since the export, so its object existed then
    LOOP AT it_unlisted ASSIGNING <ls_unlisted>.
      READ TABLE ct_objects ASSIGNING <ls_object>
        WITH TABLE KEY obj_type = <ls_unlisted>-obj_type obj_name = <ls_unlisted>-obj_name.
      IF sy-subrc = 0.
        <ls_object>-known  = abap_true.
        <ls_object>-in_zip = abap_true.
      ENDIF.
    ENDLOOP.

    LOOP AT ct_objects ASSIGNING <ls_object> WHERE in_system = abap_false.
      IF <ls_object>-known = abap_true AND <ls_object>-in_zip = abap_true.
        <ls_object>-missing = abap_true.
      ELSEIF <ls_object>-known = abap_false AND <ls_object>-pending = abap_true.
        " A new object must not exist outside of the repository
        CLEAR ls_item.
        ls_item-obj_type = <ls_object>-obj_type.
        ls_item-obj_name = <ls_object>-obj_name.
        <ls_object>-clash = zcl_abapgit_objects=>exists( ls_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_pull.

    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_marker TYPE ty_marker.

    lt_remote = ii_repo->get_files_remote( ).
    ls_marker = read_marker(
      it_remote = lt_remote
      io_dot    = ii_repo->get_dot_abapgit( ) ).

    lt_local = get_local(
      ii_repo   = ii_repo
      it_files  = ls_marker-files
      it_remote = lt_remote
      iv_reread = abap_true ).

    rt_items = check_files(
      it_files   = ls_marker-files
      it_local   = lt_local
      it_remote  = lt_remote
      iv_package = ii_repo->get_package( )
      io_dot     = ii_repo->get_dot_abapgit( ) ).

  ENDMETHOD.


  METHOD collect_object_conflicts.

    DATA lv_conflict TYPE string.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF it_objects.

    LOOP AT it_objects ASSIGNING <ls_object> WHERE missing = abap_true OR clash = abap_true.
      IF <ls_object>-missing = abap_true.
        lv_conflict = |{ <ls_object>-obj_type } { <ls_object>-obj_name }: object missing in the system, |
          && |deleted after the export or by an interrupted pull, restore it or export again|.
      ELSE.
        lv_conflict = |{ <ls_object>-obj_type } { <ls_object>-obj_name }: new object already exists in the system|.
      ENDIF.
      APPEND lv_conflict TO rt_conflicts.
    ENDLOOP.

  ENDMETHOD.


  METHOD collect_row_conflicts.

    DATA lv_reason TYPE string.
    DATA lv_conflict TYPE string.

    FIELD-SYMBOLS <ls_row> LIKE LINE OF it_rows.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF it_objects.

    LOOP AT it_rows ASSIGNING <ls_row> WHERE state = c_state-conflict.

      " Skip objects reported as a whole and objects without changes to apply
      READ TABLE it_objects ASSIGNING <ls_object>
        WITH TABLE KEY obj_type = <ls_row>-obj_type obj_name = <ls_row>-obj_name.
      IF sy-subrc = 0 AND ( <ls_object>-missing = abap_true OR <ls_object>-pending = abap_false ).
        CONTINUE.
      ENDIF.

      IF <ls_row>-sha1_system IS INITIAL.
        lv_reason = 'missing in the system'.
      ELSEIF <ls_row>-sha1_original IS INITIAL.
        lv_reason = 'already exists in the system'.
      ELSE.
        lv_reason = 'changed in the system'.
      ENDIF.

      IF <ls_row>-listed = abap_false.
        lv_reason = |not listed, { lv_reason }|.
      ENDIF.

      lv_conflict = |{ <ls_row>-path }{ <ls_row>-filename }: { lv_reason }|.
      APPEND lv_conflict TO rt_conflicts.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_for_export.

    DATA ls_release TYPE zif_abapgit_environment=>ty_release_sp.
    DATA ls_base LIKE LINE OF rs_marker-base.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.

    ls_release = zcl_abapgit_factory=>get_environment( )->get_basis_release( ).

    rs_marker-system_id              = sy-sysid.
    rs_marker-client                 = sy-mandt.
    rs_marker-basis_release          = ls_release-release.
    rs_marker-basis_sp               = ls_release-sp.
    rs_marker-abapgit_version        = zif_abapgit_version=>c_abap_version.
    rs_marker-protect_system_changes = ii_repo->get_local_settings( )-protect_system_changes.

    GET TIME STAMP FIELD rs_marker-exported_at.

    IF rs_marker-protect_system_changes <> zif_abapgit_persistence=>c_protect_system_changes-full_export.
      RETURN.
    ENDIF.

    " The pull finds the changes by comparing the ZIP with the exported files
    LOOP AT it_files ASSIGNING <ls_file>.
      IF <ls_file>-file-path = zif_abapgit_definitions=>c_root_dir
          AND <ls_file>-file-filename = zif_abapgit_definitions=>c_dot_abapgit_local.
        CONTINUE.
      ENDIF.
      ls_base-path     = <ls_file>-file-path.
      ls_base-filename = <ls_file>-file-filename.
      ls_base-sha1     = <ls_file>-file-sha1.
      APPEND ls_base TO rs_marker-base.
    ENDLOOP.

    SORT rs_marker-base BY path filename.

  ENDMETHOD.


  METHOD derive_files.

    DATA lt_base TYPE zif_abapgit_git_definitions=>ty_file_signatures_ts.
    DATA ls_file TYPE ty_file.

    FIELD-SYMBOLS <ls_base> LIKE LINE OF it_base.
    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.

    LOOP AT it_base ASSIGNING <ls_base>.
      IF strlen( <ls_base>-sha1 ) <> 40 OR <ls_base>-sha1 CN '0123456789abcdef'.
        raise_invalid(
          iv_path     = <ls_base>-path
          iv_filename = <ls_base>-filename
          iv_reason   = 'in the base list, SHA1 is empty or invalid' ).
      ENDIF.
      INSERT <ls_base> INTO TABLE lt_base.
      IF sy-subrc <> 0.
        raise_invalid(
          iv_path     = <ls_base>-path
          iv_filename = <ls_base>-filename
          iv_reason   = 'in the base list more than once' ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_remote ASSIGNING <ls_remote>.

      IF <ls_remote>-path = zif_abapgit_definitions=>c_root_dir
          AND <ls_remote>-filename = zif_abapgit_definitions=>c_dot_abapgit_local.
        CONTINUE.
      ENDIF.

      CLEAR ls_file.
      ls_file-path     = <ls_remote>-path.
      ls_file-filename = <ls_remote>-filename.
      ls_file-sha1_new = <ls_remote>-sha1.

      " A file not in the base list is new
      READ TABLE lt_base ASSIGNING <ls_base>
        WITH TABLE KEY path = <ls_remote>-path filename = <ls_remote>-filename.
      IF sy-subrc = 0.
        IF <ls_base>-sha1 = <ls_remote>-sha1.
          CONTINUE.
        ENDIF.
        ls_file-sha1_original = <ls_base>-sha1.
      ENDIF.

      APPEND ls_file TO rt_files.

    ENDLOOP.

  ENDMETHOD.


  METHOD evaluate.

    DATA lt_local TYPE ty_local_ts.
    DATA lt_more TYPE string_table.
    DATA lv_conflict TYPE string.

    FIELD-SYMBOLS <ls_missing> LIKE LINE OF et_missing.

    CLEAR: et_rows, et_unlisted, et_missing, et_objects, et_conflicts.

    lt_local = index_local( it_local ).

    et_rows = build_rows(
      it_files   = it_files
      it_local   = lt_local
      it_remote  = it_remote
      iv_package = iv_package
      io_dot     = io_dot ).

    et_objects = build_objects(
      it_rows  = et_rows
      it_local = it_local ).

    et_unlisted = find_unlisted(
      it_rows    = et_rows
      it_local   = lt_local
      it_remote  = it_remote
      iv_package = iv_package
      io_dot     = io_dot
      it_objects = et_objects ).

    check_objects(
      EXPORTING
        it_unlisted = et_unlisted
      CHANGING
        ct_objects  = et_objects ).

    et_missing = find_missing_in_zip(
      it_local   = it_local
      it_rows    = et_rows
      it_remote  = it_remote
      it_objects = et_objects ).

    et_conflicts = collect_object_conflicts( et_objects ).

    lt_more = collect_row_conflicts(
      it_rows    = et_rows
      it_objects = et_objects ).
    APPEND LINES OF lt_more TO et_conflicts.

    lt_more = collect_row_conflicts(
      it_rows    = et_unlisted
      it_objects = et_objects ).
    APPEND LINES OF lt_more TO et_conflicts.

    LOOP AT et_missing ASSIGNING <ls_missing>.
      lv_conflict = |{ <ls_missing>-path }{ <ls_missing>-filename }: not listed, missing in the ZIP|.
      APPEND lv_conflict TO et_conflicts.
    ENDLOOP.

  ENDMETHOD.


  METHOD evaluate_row.

    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.

    READ TABLE it_local ASSIGNING <ls_local>
      WITH TABLE KEY path = cs_row-path filename = cs_row-filename.
    IF sy-subrc = 0.
      cs_row-sha1_system = <ls_local>-sha1.
    ELSE.
      CLEAR cs_row-sha1_system.
    ENDIF.

    IF cs_row-sha1_system = cs_row-sha1_new.
      cs_row-state = c_state-applied.
    ELSEIF cs_row-sha1_system = cs_row-sha1_original.
      cs_row-state = c_state-to_apply.
    ELSE.
      cs_row-state = c_state-conflict.
    ENDIF.

  ENDMETHOD.


  METHOD filter_overwrite.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF it_overwrite.

    LOOP AT it_overwrite ASSIGNING <ls_overwrite>.
      READ TABLE it_items TRANSPORTING NO FIELDS WITH TABLE KEY
        obj_type = <ls_overwrite>-obj_type
        obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc = 0.
        INSERT <ls_overwrite> INTO TABLE rt_overwrite.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_results.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result>.
      READ TABLE it_items TRANSPORTING NO FIELDS WITH TABLE KEY
        obj_type = <ls_result>-obj_type
        obj_name = <ls_result>-obj_name.
      IF sy-subrc = 0.
        APPEND <ls_result> TO rt_results.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_results_by_overwrite.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result>.
      READ TABLE it_overwrite TRANSPORTING NO FIELDS WITH TABLE KEY object_type_and_name
        COMPONENTS obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name.
      IF sy-subrc = 0.
        APPEND <ls_result> TO rt_results.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fits_t100_text.

    " ZCX_ABAPGIT_EXCEPTION=>RAISE keeps the text in four variables of 50 characters
    " (CL_MESSAGE_HELPER=>SET_MSG_VARS_FOR_CLIKE): a blank in the last position of a variable
    " moves to the next one, and trailing blanks of a variable are lost.
    DATA lv_length TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_last TYPE i.
    DATA lv_prev TYPE i.
    DATA lv_char TYPE c LENGTH 1.
    DATA lv_before TYPE c LENGTH 1.

    lv_length = strlen( iv_text ).

    DO 3 TIMES.
      lv_last = lv_offset + 49.
      IF lv_last >= lv_length.
        rv_fit = abap_true.
        RETURN.
      ENDIF.

      lv_char = iv_text+lv_last(1).
      IF lv_char IS INITIAL.
        lv_prev = lv_last - 1.
        lv_before = iv_text+lv_prev(1).
        IF lv_before IS INITIAL.
          RETURN.
        ENDIF.
        lv_offset = lv_offset + 49.
      ELSE.
        lv_offset = lv_offset + 50.
      ENDIF.
    ENDDO.

    rv_fit = boolc( lv_length <= lv_offset + 50 ).

  ENDMETHOD.


  METHOD find_missing_in_zip.

    DATA ls_row TYPE ty_row.

    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF it_objects.

    IF it_objects IS INITIAL.
      RETURN.
    ENDIF.

    " An object to pull is deserialized completely, so the ZIP must have all of its files
    LOOP AT it_local ASSIGNING <ls_local> WHERE item-obj_type IS NOT INITIAL.

      READ TABLE it_objects ASSIGNING <ls_object>
        WITH TABLE KEY obj_type = <ls_local>-item-obj_type obj_name = <ls_local>-item-obj_name.
      IF sy-subrc <> 0 OR <ls_object>-pending = abap_false.
        CONTINUE.
      ENDIF.

      READ TABLE it_rows TRANSPORTING NO FIELDS
        WITH TABLE KEY path = <ls_local>-file-path filename = <ls_local>-file-filename.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_remote TRANSPORTING NO FIELDS WITH TABLE KEY file_path
        COMPONENTS path = <ls_local>-file-path filename = <ls_local>-file-filename.
      IF sy-subrc <> 0.
        CLEAR ls_row.
        ls_row-path        = <ls_local>-file-path.
        ls_row-filename    = <ls_local>-file-filename.
        ls_row-sha1_system = <ls_local>-file-sha1.
        ls_row-state       = c_state-conflict.
        ls_row-obj_type    = <ls_local>-item-obj_type.
        ls_row-obj_name    = <ls_local>-item-obj_name.
        INSERT ls_row INTO TABLE rt_missing.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_unlisted.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA ls_row TYPE ty_row.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.

    IF it_objects IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_remote ASSIGNING <ls_remote>.

      READ TABLE it_rows TRANSPORTING NO FIELDS
        WITH TABLE KEY path = <ls_remote>-path filename = <ls_remote>-filename.
      IF sy-subrc = 0 OR is_object_file( iv_path     = <ls_remote>-path
                                         iv_filename = <ls_remote>-filename ) = abap_false
          OR io_dot->is_ignored( iv_path     = <ls_remote>-path
                                 iv_filename = <ls_remote>-filename ) = abap_true.
        CONTINUE.
      ENDIF.

      CLEAR ls_item.
      TRY.
          zcl_abapgit_filename_logic=>file_to_object(
            EXPORTING
              iv_filename = <ls_remote>-filename
              iv_path     = <ls_remote>-path
              iv_devclass = iv_package
              io_dot      = io_dot
            IMPORTING
              es_item     = ls_item ).
        CATCH zcx_abapgit_exception.
          " not a file of a listed object
          CLEAR ls_item.
      ENDTRY.

      READ TABLE it_objects TRANSPORTING NO FIELDS
        WITH TABLE KEY obj_type = ls_item-obj_type obj_name = ls_item-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " Not listed means unchanged since the export
      CLEAR ls_row.
      ls_row-path          = <ls_remote>-path.
      ls_row-filename      = <ls_remote>-filename.
      ls_row-sha1_original = <ls_remote>-sha1.
      ls_row-sha1_new      = <ls_remote>-sha1.
      ls_row-obj_type      = ls_item-obj_type.
      ls_row-obj_name      = ls_item-obj_name.

      evaluate_row(
        EXPORTING
          it_local = it_local
        CHANGING
          cs_row   = ls_row ).

      INSERT ls_row INTO TABLE rt_unlisted.

    ENDLOOP.

  ENDMETHOD.


  METHOD from_file.

    DATA lv_xml TYPE string.

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ).

    rs_marker = from_xml( lv_xml ).

  ENDMETHOD.


  METHOD from_xml.

    DATA lv_xml TYPE string.
    DATA lx_xslt TYPE REF TO cx_xslt_format_error.

    lv_xml = iv_xml.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML lv_xml
          RESULT data = rs_marker.
      CATCH cx_xslt_format_error INTO lx_xslt.
        zcx_abapgit_exception=>raise(
          iv_text     = 'Bad format for .abapgit.local.xml'
          ix_previous = lx_xslt ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_local.

    IF iv_reread = abap_true.
      refresh_listed_objects(
        ii_repo  = ii_repo
        it_files = it_files ).
    ENDIF.

    rt_local = ii_repo->get_files_local( ).

    refresh_listed_data(
      EXPORTING
        ii_repo   = ii_repo
        it_files  = it_files
        it_remote = it_remote
        iv_reread = iv_reread
      CHANGING
        ct_local  = rt_local ).

  ENDMETHOD.


  METHOD get_marking.

    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_marker TYPE ty_marker.
    DATA lt_rows TYPE ty_rows.
    DATA lt_unlisted TYPE ty_rows.
    DATA lt_missing TYPE ty_rows.
    DATA lt_objects TYPE ty_objects.
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.

    lt_remote = ii_repo->get_files_remote( ).
    ls_marker = read_marker(
      it_remote = lt_remote
      io_dot    = ii_repo->get_dot_abapgit( ) ).

    " Same files as the check of the pull, objects and unchanged table selections from the cache
    lt_local = get_local(
      ii_repo   = ii_repo
      it_files  = ls_marker-files
      it_remote = lt_remote
      iv_reread = abap_false ).

    evaluate(
      EXPORTING
        it_files    = ls_marker-files
        it_local    = lt_local
        it_remote   = lt_remote
        iv_package  = ii_repo->get_package( )
        io_dot      = ii_repo->get_dot_abapgit( )
      IMPORTING
        et_rows     = lt_rows
        et_unlisted = lt_unlisted
        et_missing  = lt_missing
        et_objects  = lt_objects ).

    " Listed, unlisted and missing rows are distinct files
    INSERT LINES OF lt_unlisted INTO TABLE lt_rows.
    INSERT LINES OF lt_missing INTO TABLE lt_rows.

    rs_marking-files = mark_files(
      it_rows    = lt_rows
      it_objects = lt_objects ).

    rs_marking-objects = mark_objects(
      it_rows    = lt_rows
      it_objects = lt_objects
      it_files   = rs_marking-files ).

  ENDMETHOD.


  METHOD find_child.

    DATA li_child TYPE REF TO if_ixml_node.
    DATA lt_parts TYPE string_table.
    DATA lv_name TYPE string.

    li_child = ii_parent->get_first_child( ).

    WHILE li_child IS BOUND.
      IF li_child->get_type( ) = if_ixml_node=>co_node_element.
        " The name without a namespace prefix
        lv_name = li_child->get_name( ).
        SPLIT lv_name AT ':' INTO TABLE lt_parts.
        READ TABLE lt_parts INDEX lines( lt_parts ) INTO lv_name.
        IF sy-subrc = 0 AND lv_name = iv_name.
          ri_child = li_child.
          RETURN.
        ENDIF.
      ENDIF.
      li_child = li_child->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD has_files_element.

    DATA li_ixml TYPE REF TO if_ixml.
    DATA li_doc TYPE REF TO if_ixml_document.
    DATA li_stream_factory TYPE REF TO if_ixml_stream_factory.
    DATA li_istream TYPE REF TO if_ixml_istream.
    DATA li_parser TYPE REF TO if_ixml_parser.
    DATA li_node TYPE REF TO if_ixml_node.
    DATA lt_path TYPE string_table.
    DATA lv_name TYPE string.

    li_ixml = cl_ixml=>create( ).
    li_doc = li_ixml->create_document( ).
    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = li_ixml->create_parser(
      stream_factory = li_stream_factory
      istream        = li_istream
      document       = li_doc ).

    IF li_parser->parse( ) <> 0.
      zcx_abapgit_exception=>raise( 'Bad format for .abapgit.local.xml' ).
    ENDIF.

    li_istream->close( ).

    li_node = li_doc.

    APPEND 'abap' TO lt_path.
    APPEND 'values' TO lt_path.
    APPEND 'DATA' TO lt_path.
    APPEND 'FILES' TO lt_path.

    LOOP AT lt_path INTO lv_name.
      li_node = find_child(
        ii_parent = li_node
        iv_name   = lv_name ).
      IF li_node IS NOT BOUND.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD index_local.

    DATA ls_local TYPE ty_local.

    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.

    LOOP AT it_local ASSIGNING <ls_local>.
      ls_local-path     = <ls_local>-file-path.
      ls_local-filename = <ls_local>-file-filename.
      ls_local-sha1     = <ls_local>-file-sha1.
      INSERT ls_local INTO TABLE rt_local.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_active.

    DATA lv_mode TYPE zif_abapgit_persistence=>ty_local_settings-protect_system_changes.

    IF ii_repo->is_offline( ) = abap_false.
      RETURN.
    ENDIF.

    lv_mode = ii_repo->get_local_settings( )-protect_system_changes.

    rv_active = boolc( lv_mode = zif_abapgit_persistence=>c_protect_system_changes-full_export
      OR lv_mode = zif_abapgit_persistence=>c_protect_system_changes-change_list ).

  ENDMETHOD.


  METHOD is_object_file.

    " Repository settings, data config and table content are no objects
    rv_bool = boolc( iv_path <> zif_abapgit_data_config=>c_default_path
      AND NOT ( iv_path = zif_abapgit_definitions=>c_root_dir
        AND ( iv_filename = zif_abapgit_definitions=>c_dot_abapgit
          OR iv_filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest ) ) ).

  ENDMETHOD.


  METHOD map_data_file.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    TRY.
        zcl_abapgit_filename_logic=>file_to_object(
          EXPORTING
            iv_filename = iv_filename
            iv_path     = iv_path
            io_dot      = io_dot
          IMPORTING
            es_item     = ls_item ).
      CATCH zcx_abapgit_exception.
        CLEAR ls_item.
    ENDTRY.

    " Table content and data config files map to object TABU, as in the status of the repository
    IF ls_item-obj_type <> zif_abapgit_data_config=>c_data_type-tabu OR ls_item-obj_name IS INITIAL.
      raise_invalid(
        iv_path     = iv_path
        iv_filename = iv_filename
        iv_reason   = 'no table content or data config file' ).
    ENDIF.

    rv_name = ls_item-obj_name.

  ENDMETHOD.


  METHOD map_listed_file.

    DATA lv_text TYPE string.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    lv_text = |Invalid { zif_abapgit_definitions=>c_dot_abapgit_local }, |
      && |{ iv_path }{ iv_filename }: no object for this file|.

    TRY.
        zcl_abapgit_filename_logic=>file_to_object(
          EXPORTING
            iv_filename = iv_filename
            iv_path     = iv_path
            iv_devclass = iv_package
            io_dot      = io_dot
          IMPORTING
            es_item     = rs_item ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapgit_exception=>raise(
          iv_text     = lv_text
          ix_previous = lx_error ).
    ENDTRY.

    IF rs_item-obj_type IS INITIAL OR rs_item-obj_name IS INITIAL.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF zcl_abapgit_objects=>is_type_supported( rs_item-obj_type ) = abap_false.
      zcx_abapgit_exception=>raise( |Pull refused, object type { rs_item-obj_type } of |
        && |{ iv_path }{ iv_filename } is not supported in this system| ).
    ENDIF.

    CLEAR rs_item-devclass.

  ENDMETHOD.


  METHOD mark_files.

    DATA ls_file TYPE ty_mark_file.
    DATA lv_found TYPE abap_bool.

    FIELD-SYMBOLS <ls_row> LIKE LINE OF it_rows.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF it_objects.

    LOOP AT it_rows ASSIGNING <ls_row>.

      CLEAR ls_file.
      ls_file-path     = <ls_row>-path.
      ls_file-filename = <ls_row>-filename.
      ls_file-state    = <ls_row>-state.

      READ TABLE it_objects ASSIGNING <ls_object>
        WITH TABLE KEY obj_type = <ls_row>-obj_type obj_name = <ls_row>-obj_name.
      lv_found = boolc( sy-subrc = 0 ).

      IF lv_found = abap_true AND ( <ls_object>-missing = abap_true OR <ls_object>-clash = abap_true ).
        ls_file-state = c_state-conflict.
      ELSEIF lv_found = abap_true AND <ls_object>-pending = abap_false AND <ls_row>-listed = abap_false.
        " object without changes to apply
        CONTINUE.
      ENDIF.

      " An unlisted file is marked only if it stops the pull of its object
      IF <ls_row>-listed = abap_false AND ls_file-state <> c_state-conflict.
        CONTINUE.
      ENDIF.

      INSERT ls_file INTO TABLE rt_files.

    ENDLOOP.

  ENDMETHOD.


  METHOD mark_objects.

    DATA ls_object TYPE ty_mark_object.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF it_objects.
    FIELD-SYMBOLS <ls_row> LIKE LINE OF it_rows.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.
    FIELD-SYMBOLS <ls_mark> LIKE LINE OF rt_objects.

    LOOP AT it_objects ASSIGNING <ls_object>.
      CLEAR ls_object.
      ls_object-obj_type = <ls_object>-obj_type.
      ls_object-obj_name = <ls_object>-obj_name.
      IF <ls_object>-missing = abap_true OR <ls_object>-clash = abap_true.
        ls_object-state = c_state-conflict.
      ELSEIF <ls_object>-pending = abap_false.
        ls_object-state = c_state-applied.
      ELSE.
        ls_object-state = c_state-to_apply.
      ENDIF.
      INSERT ls_object INTO TABLE rt_objects.
    ENDLOOP.

    " A conflicting file stops its object, table content takes the state of its files
    LOOP AT it_rows ASSIGNING <ls_row>.

      READ TABLE it_files ASSIGNING <ls_file>
        WITH TABLE KEY path = <ls_row>-path filename = <ls_row>-filename.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR ls_object.
      IF <ls_row>-data_name IS NOT INITIAL.
        ls_object-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
        ls_object-obj_name = <ls_row>-data_name.
      ELSE.
        ls_object-obj_type = <ls_row>-obj_type.
        ls_object-obj_name = <ls_row>-obj_name.
      ENDIF.
      IF ls_object-obj_type IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE rt_objects ASSIGNING <ls_mark>
        WITH TABLE KEY obj_type = ls_object-obj_type obj_name = ls_object-obj_name.
      IF sy-subrc <> 0.
        ls_object-state = <ls_file>-state.
        INSERT ls_object INTO TABLE rt_objects.
      ELSEIF <ls_file>-state = c_state-conflict
          OR ( <ls_file>-state = c_state-to_apply AND <ls_mark>-state = c_state-applied ).
        <ls_mark>-state = <ls_file>-state.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD postprocess_data.

    DATA lt_files LIKE it_files.
    DATA lt_config_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA li_exit TYPE REF TO zif_abapgit_exit.

    FIELD-SYMBOLS <ls_config_file> LIKE LINE OF lt_config_files.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_files.

    IF it_files IS INITIAL.
      RETURN.
    ENDIF.

    " Config files first, then the content, as in the export. The exit may format by the config
    lt_config_files = ii_config->to_json( ).
    LOOP AT lt_config_files ASSIGNING <ls_config_file>.
      APPEND INITIAL LINE TO lt_files ASSIGNING <ls_file>.
      <ls_file>-file = <ls_config_file>.
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-file-filename
          iv_path     = <ls_file>-file-path
          io_dot      = io_dot
        IMPORTING
          es_item     = <ls_file>-item ).
      <ls_file>-item-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
    ENDLOOP.
    APPEND LINES OF it_files TO lt_files.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->serialize_postprocess(
      EXPORTING
        iv_package = iv_package
        ii_log     = li_log
      CHANGING
        ct_files   = lt_files ).

    LOOP AT lt_files ASSIGNING <ls_file> WHERE file-path = zif_abapgit_data_config=>c_default_path.
      READ TABLE it_names TRANSPORTING NO FIELDS WITH TABLE KEY table_line = <ls_file>-file-filename.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      " The SHA1 of the ZIP file is computed from its content, an exit may change the content only
      <ls_file>-file-sha1 = zcl_abapgit_hash=>sha1_blob( <ls_file>-file-data ).
      APPEND <ls_file> TO rt_files.
    ENDLOOP.

  ENDMETHOD.


  METHOD raise_conflicts.

    DATA lt_conflicts TYPE string_table.
    DATA lv_text TYPE string.
    DATA lv_longtext TYPE string.

    FIELD-SYMBOLS <lv_first> LIKE LINE OF lt_conflicts.

    lt_conflicts = it_conflicts.
    SORT lt_conflicts.
    DELETE ADJACENT DUPLICATES FROM lt_conflicts.

    READ TABLE lt_conflicts INDEX 1 ASSIGNING <lv_first>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lines( lt_conflicts ) = 1 AND fits_t100_text( <lv_first> ) = abap_true.
      zcx_abapgit_exception=>raise( <lv_first> ).
    ENDIF.

    lv_text = |Pull stopped, { lines( lt_conflicts ) } conflict(s) with changes in the system. |
      && |First: { <lv_first> }|.

    lv_longtext = concat_lines_of(
      table = lt_conflicts
      sep   = cl_abap_char_utilities=>newline ).

    zcx_abapgit_exception=>raise(
      iv_text     = lv_text
      iv_longtext = lv_longtext ).

  ENDMETHOD.


  METHOD raise_invalid.

    zcx_abapgit_exception=>raise( |Invalid { zif_abapgit_definitions=>c_dot_abapgit_local }, |
      && |{ iv_path }{ iv_filename }: { iv_reason }| ).

  ENDMETHOD.


  METHOD read_listed.

    DATA ls_marker TYPE ty_marker.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF ls_marker-files.

    TRY.
        ls_marker = read_marker(
          it_remote = it_remote
          io_dot    = io_dot ).
      CATCH zcx_abapgit_exception.
        " no valid marker file, nothing is listed
        RETURN.
    ENDTRY.

    LOOP AT ls_marker-files ASSIGNING <ls_file>.
      INSERT <ls_file> INTO TABLE rt_listed.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_marker.

    DATA lv_xml TYPE string.
    DATA lv_files_given TYPE abap_bool.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF rs_marker-files.

    READ TABLE it_remote ASSIGNING <ls_remote> WITH TABLE KEY file_path
      COMPONENTS path     = zif_abapgit_definitions=>c_root_dir
                 filename = zif_abapgit_definitions=>c_dot_abapgit_local.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Pull refused, Protect System Changes on Pull is set |
        && |but the ZIP has no { zif_abapgit_definitions=>c_dot_abapgit_local }| ).
    ENDIF.

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_remote>-data ).
    rs_marker = from_xml( lv_xml ).

    IF rs_marker-system_id <> sy-sysid OR rs_marker-client <> sy-mandt.
      zcx_abapgit_exception=>raise( |Pull refused, the ZIP was exported from system { rs_marker-system_id } |
        && |client { rs_marker-client }, this is system { sy-sysid } client { sy-mandt }| ).
    ENDIF.

    " An empty table does not tell a missing FILES element from an explicit, empty one
    lv_files_given = has_files_element( lv_xml ).

    " Both modes pull the same way, an explicit change list (even an empty one) wins over the base list
    IF lv_files_given = abap_false.
      IF rs_marker-base IS INITIAL.
        zcx_abapgit_exception=>raise( |Pull refused, { zif_abapgit_definitions=>c_dot_abapgit_local } has no |
          && |change list and no base list. Export again with Protect System Changes set to Full Export, |
          && |or add a change list made by a tool| ).
      ENDIF.
      rs_marker-files = derive_files(
        it_base   = rs_marker-base
        it_remote = it_remote ).
    ENDIF.

    " The pull leaves ignored files alone (outside the starting folder, ignore list), for example a
    " README of the Git repository. A file in the starting folder without an object is still refused
    LOOP AT rs_marker-files ASSIGNING <ls_file>.
      IF io_dot->is_ignored( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-filename ) = abap_true.
        DELETE rs_marker-files.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD refresh_listed_data.

    DATA lt_listed TYPE ty_files_ts.
    DATA li_config TYPE REF TO zif_abapgit_data_config.
    DATA li_listed TYPE REF TO zif_abapgit_data_config.
    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA lt_system TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA lt_data TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA lt_new LIKE ct_local.
    DATA ls_local LIKE LINE OF ct_local.
    DATA lt_names TYPE ty_names.
    DATA lv_filename TYPE string.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.
    FIELD-SYMBOLS <ls_config> LIKE LINE OF lt_configs.
    FIELD-SYMBOLS <ls_system> LIKE LINE OF lt_system.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF ct_local.

    LOOP AT it_files ASSIGNING <ls_file> WHERE path = zif_abapgit_data_config=>c_default_path.
      INSERT <ls_file> INTO TABLE lt_listed.
    ENDLOOP.

    IF lt_listed IS INITIAL.
      RETURN.
    ENDIF.

    " The pull writes the content in the selection of the data config it leaves behind
    lt_system = ii_repo->get_data_config( )->get_configs( ).
    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.
    li_config->from_json( apply_listed_files(
      it_remote = it_remote
      it_system = ii_repo->get_data_config( )->to_json( )
      io_dot    = ii_repo->get_dot_abapgit( ) ) ).
    lt_configs = li_config->get_configs( ).

    check_listed_data(
      it_listed  = lt_listed
      it_configs = lt_configs ).

    " Table content can change outside of abapGit, the check reads the listed tables again. The
    " repository view reads only tables whose selection changes, the cache has the others
    CREATE OBJECT li_listed TYPE zcl_abapgit_data_config.

    LOOP AT lt_configs ASSIGNING <ls_config>.
      lv_filename = zcl_abapgit_data_utils=>build_data_filename( <ls_config> ).
      READ TABLE lt_listed TRANSPORTING NO FIELDS
        WITH TABLE KEY path = zif_abapgit_data_config=>c_default_path filename = lv_filename.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF iv_reread = abap_false.
        READ TABLE lt_system ASSIGNING <ls_system>
          WITH TABLE KEY type = <ls_config>-type name = <ls_config>-name.
        IF sy-subrc = 0 AND <ls_system> = <ls_config>.
          CONTINUE.
        ENDIF.
      ENDIF.
      li_listed->add_config( <ls_config> ).
    ENDLOOP.

    lt_configs = li_listed->get_configs( ).
    IF lt_configs IS INITIAL.
      RETURN.
    ENDIF.

    lt_data = zcl_abapgit_data_factory=>get_serializer( )->serialize( li_listed ).

    LOOP AT lt_configs ASSIGNING <ls_config>.
      lv_filename = zcl_abapgit_data_utils=>build_data_filename( <ls_config> ).
      INSERT lv_filename INTO TABLE lt_names.

      CLEAR ls_local.
      READ TABLE lt_data INTO ls_local-file WITH TABLE KEY file_path
        COMPONENTS path = zif_abapgit_data_config=>c_default_path filename = lv_filename.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ls_local-item-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
      ls_local-item-obj_name = <ls_config>-name.
      APPEND ls_local TO lt_new.
    ENDLOOP.

    lt_new = postprocess_data(
      iv_package = ii_repo->get_package( )
      io_dot     = ii_repo->get_dot_abapgit( )
      ii_config  = li_config
      it_names   = lt_names
      it_files   = lt_new ).

    LOOP AT ct_local ASSIGNING <ls_local> WHERE file-path = zif_abapgit_data_config=>c_default_path.
      lv_index = sy-tabix.
      READ TABLE lt_names TRANSPORTING NO FIELDS WITH TABLE KEY table_line = <ls_local>-file-filename.
      IF sy-subrc = 0.
        DELETE ct_local INDEX lv_index.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF lt_new TO ct_local.

  ENDMETHOD.

  METHOD refresh_listed_objects.

    DATA lt_items TYPE zif_abapgit_definitions=>ty_items_ts.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_package TYPE devclass.
    DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.

    lv_package = ii_repo->get_package( ).
    lo_dot     = ii_repo->get_dot_abapgit( ).

    LOOP AT it_files ASSIGNING <ls_file>.

      IF is_object_file( iv_path     = <ls_file>-path
                         iv_filename = <ls_file>-filename ) = abap_false.
        CONTINUE.
      ENDIF.

      CLEAR ls_item.
      TRY.
          zcl_abapgit_filename_logic=>file_to_object(
            EXPORTING
              iv_filename = <ls_file>-filename
              iv_path     = <ls_file>-path
              iv_devclass = lv_package
              io_dot      = lo_dot
            IMPORTING
              es_item     = ls_item ).
        CATCH zcx_abapgit_exception.
          " reported by the check
          CLEAR ls_item.
      ENDTRY.

      IF ls_item-obj_type IS NOT INITIAL AND ls_item-obj_name IS NOT INITIAL.
        CLEAR ls_item-devclass.
        INSERT ls_item INTO TABLE lt_items.
      ENDIF.

    ENDLOOP.

    refresh_objects(
      ii_repo  = ii_repo
      it_items = lt_items ).

  ENDMETHOD.


  METHOD refresh_objects.

    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_filter LIKE LINE OF lt_filter.
    DATA li_filter TYPE REF TO zif_abapgit_object_filter.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.
      " table content (TABU) and other unsupported types have no object files
      IF zcl_abapgit_objects=>is_type_supported( <ls_item>-obj_type ) = abap_false.
        CONTINUE.
      ENDIF.
      CLEAR ls_filter.
      ls_filter-object   = <ls_item>-obj_type.
      ls_filter-obj_name = <ls_item>-obj_name.
      APPEND ls_filter TO lt_filter.
    ENDLOOP.

    IF lt_filter IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT li_filter TYPE zcl_abapgit_object_filter_obj
      EXPORTING
        it_filter = lt_filter.

    ii_repo->refresh_local_objects( li_filter ).

  ENDMETHOD.


  METHOD to_file.

    DATA lv_xml TYPE string.

    lv_xml = to_xml( is_marker ).

    rs_file-path     = zif_abapgit_definitions=>c_root_dir.
    rs_file-filename = zif_abapgit_definitions=>c_dot_abapgit_local.
    rs_file-data     = zcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).
    rs_file-sha1     = zcl_abapgit_hash=>sha1_blob( rs_file-data ).

  ENDMETHOD.


  METHOD to_xml.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = is_marker
      RESULT XML rv_xml.

    rv_xml = zcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>' ##REGEX_POSIX.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
