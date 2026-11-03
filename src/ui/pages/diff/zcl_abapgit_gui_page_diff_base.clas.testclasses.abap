CLASS ltcl_filter_changed_by DEFINITION DEFERRED.
CLASS zcl_abapgit_gui_page_diff_base DEFINITION LOCAL FRIENDS ltcl_filter_changed_by.

CLASS ltcl_filter_changed_by DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS c_me TYPE syuname VALUE 'ME'.
    CONSTANTS c_other TYPE syuname VALUE 'OTHER'.

    DATA mt_diff_files TYPE zif_abapgit_gui_diff=>ty_file_diffs.

    METHODS add_diff
      IMPORTING
        iv_filename   TYPE string
        iv_lstate     TYPE zif_abapgit_git_definitions=>ty_item_state
        iv_rstate     TYPE zif_abapgit_git_definitions=>ty_item_state
        iv_changed_by TYPE syuname.
    METHODS filter
      RETURNING
        VALUE(rt_filenames) TYPE string_table.

    METHODS keeps_own_local_changes FOR TESTING.
    METHODS drops_changes_of_others FOR TESTING.
    METHODS drops_remote_only_changes FOR TESTING.

ENDCLASS.

CLASS ltcl_filter_changed_by IMPLEMENTATION.

  METHOD add_diff.

    DATA ls_diff LIKE LINE OF mt_diff_files.

    ls_diff-path       = '/src/'.
    ls_diff-filename   = iv_filename.
    ls_diff-lstate     = iv_lstate.
    ls_diff-rstate     = iv_rstate.
    ls_diff-changed_by = iv_changed_by.
    APPEND ls_diff TO mt_diff_files.

  ENDMETHOD.

  METHOD filter.

    FIELD-SYMBOLS <ls_diff> LIKE LINE OF mt_diff_files.

    zcl_abapgit_gui_page_diff_base=>filter_changed_by(
      EXPORTING
        iv_changed_by = c_me
      CHANGING
        ct_diff_files = mt_diff_files ).

    LOOP AT mt_diff_files ASSIGNING <ls_diff>.
      APPEND <ls_diff>-filename TO rt_filenames.
    ENDLOOP.

  ENDMETHOD.

  METHOD keeps_own_local_changes.

    DATA lt_filenames TYPE string_table.
    DATA lt_expected TYPE string_table.

    add_diff( iv_filename   = 'zcl_modified.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-modified
              iv_rstate     = zif_abapgit_definitions=>c_state-unchanged
              iv_changed_by = c_me ).
    add_diff( iv_filename   = 'zcl_both.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-modified
              iv_rstate     = zif_abapgit_definitions=>c_state-modified
              iv_changed_by = c_me ).
    add_diff( iv_filename   = 'zcl_deleted.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-deleted
              iv_rstate     = zif_abapgit_definitions=>c_state-unchanged
              iv_changed_by = c_me ).

    lt_filenames = filter( ).

    APPEND 'zcl_modified.clas.abap' TO lt_expected.
    APPEND 'zcl_both.clas.abap' TO lt_expected.
    APPEND 'zcl_deleted.clas.abap' TO lt_expected.
    cl_abap_unit_assert=>assert_equals(
      exp = lt_expected
      act = lt_filenames ).

  ENDMETHOD.

  METHOD drops_changes_of_others.

    DATA lt_filenames TYPE string_table.

    add_diff( iv_filename   = 'zcl_theirs.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-modified
              iv_rstate     = zif_abapgit_definitions=>c_state-unchanged
              iv_changed_by = c_other ).
    add_diff( iv_filename   = 'zcl_unknown.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-deleted
              iv_rstate     = zif_abapgit_definitions=>c_state-unchanged
              iv_changed_by = zcl_abapgit_objects_super=>c_user_unknown ).

    lt_filenames = filter( ).

    cl_abap_unit_assert=>assert_initial( lt_filenames ).

  ENDMETHOD.

  METHOD drops_remote_only_changes.

    DATA lt_filenames TYPE string_table.

    " Last changed by me (e.g. by my pull), but the change is someone else's push
    add_diff( iv_filename   = 'zcl_pushed_by_other.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-unchanged
              iv_rstate     = zif_abapgit_definitions=>c_state-modified
              iv_changed_by = c_me ).
    add_diff( iv_filename   = 'zcl_added_remotely.clas.abap'
              iv_lstate     = zif_abapgit_definitions=>c_state-unchanged
              iv_rstate     = zif_abapgit_definitions=>c_state-added
              iv_changed_by = c_me ).

    lt_filenames = filter( ).

    cl_abap_unit_assert=>assert_initial( lt_filenames ).

  ENDMETHOD.

ENDCLASS.
