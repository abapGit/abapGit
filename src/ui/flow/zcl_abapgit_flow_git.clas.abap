CLASS zcl_abapgit_flow_git DEFINITION PUBLIC.
  PUBLIC SECTION.
* various git related methods

    CLASS-METHODS find_changes_in_git
      IMPORTING
        ii_repo_online   TYPE REF TO zif_abapgit_repo_online
        it_branches      TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      EXPORTING
        et_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE zif_abapgit_flow_logic=>ty_features
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE zif_abapgit_flow_logic=>ty_path_name_tt
        ii_repo                   TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(rt_changed_objects) TYPE zif_abapgit_definitions=>ty_items_ts
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_flow_git IMPLEMENTATION.

  METHOD find_changes_in_git.

    DATA ls_branch          LIKE LINE OF it_branches.
    DATA lt_sha1            TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects         TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_starting_folder TYPE string.
    DATA ls_main            LIKE LINE OF it_branches.
    DATA li_repo            TYPE REF TO zif_abapgit_repo.
    DATA lo_find            TYPE REF TO lcl_find_changes.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.


    CLEAR et_main_expanded.

    li_repo = ii_repo_online.

    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_objects = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_no_blobs_multi(
      iv_url  = ii_repo_online->get_url( )
      it_sha1 = lt_sha1 ).

    lv_starting_folder = li_repo->get_dot_abapgit( )->get_starting_folder( ) && '*'.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    ASSERT sy-subrc = 0.

    et_main_expanded = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = ls_main-sha1 ).
    DELETE et_main_expanded WHERE path NP lv_starting_folder.

    CREATE OBJECT lo_find EXPORTING it_objects = lt_objects.

    LOOP AT ct_features ASSIGNING <ls_branch> WHERE branch-display_name <> zif_abapgit_flow_logic=>c_main.
      <ls_branch>-changed_files = lo_find->find_changes(
        iv_main            = ls_main-sha1
        iv_branch          = <ls_branch>-branch-sha1
        iv_starting_folder = lv_starting_folder ).

      <ls_branch>-changed_objects = map_files_to_objects(
        ii_repo  = li_repo
        it_files = <ls_branch>-changed_files ).
    ENDLOOP.

  ENDMETHOD.

  METHOD map_files_to_objects.

    DATA ls_item    TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_package TYPE devclass.
    DATA lo_dot     TYPE REF TO zcl_abapgit_dot_abapgit.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.

    lv_package = ii_repo->get_package( ).
    lo_dot = ii_repo->get_dot_abapgit( ).

    LOOP AT it_files ASSIGNING <ls_file>.
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-filename
          iv_path     = <ls_file>-path
          iv_devclass = lv_package
          io_dot      = lo_dot
        IMPORTING
          es_item     = ls_item ).
      INSERT ls_item INTO TABLE rt_changed_objects.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
