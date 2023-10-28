*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_path_name,
        path TYPE string,
        name TYPE string,
      END OF ty_path_name .
    TYPES:
      ty_path_name_tt TYPE HASHED TABLE OF ty_path_name WITH UNIQUE KEY path name.

    TYPES: BEGIN OF ty_branch,
             display_name  TYPE string,
             sha1          TYPE zif_abapgit_git_definitions=>ty_sha1,
             up_to_date    TYPE abap_bool,
             changed_files TYPE ty_path_name_tt,
           END OF ty_branch.
    TYPES ty_branches TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY.

    CLASS-METHODS get_branch_information
      IMPORTING
        io_online          TYPE REF TO zcl_abapgit_repo_online
      RETURNING
        VALUE(rt_branches) TYPE ty_branches
      RAISING
        zcx_abapgit_exception.
  PRIVATE SECTION.
    CONSTANTS c_main TYPE string VALUE 'main'.

    CLASS-METHODS find_changed_files_all
      IMPORTING
        io_online          TYPE REF TO zcl_abapgit_repo_online
        it_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_branches TYPE ty_branches
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_changed_files
      IMPORTING
        it_expanded1    TYPE zif_abapgit_git_definitions=>ty_expanded_tt
        it_expanded2    TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RETURNING
        VALUE(rt_files) TYPE ty_path_name_tt.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD get_branch_information.

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_branch   LIKE LINE OF lt_branches.
    DATA ls_result   LIKE LINE OF rt_branches.


    lt_branches = zcl_abapgit_gitv2_porcelain=>list_branches(
      iv_url    = io_online->get_url( )
      iv_prefix = 'refs/heads/' )->get_all( ).

    LOOP AT lt_branches INTO ls_branch WHERE display_name <> c_main.
      ls_result-display_name = ls_branch-display_name.
      ls_result-sha1 = ls_branch-sha1.
      INSERT ls_result INTO TABLE rt_branches.
    ENDLOOP.

    find_changed_files_all(
      EXPORTING
        io_online   = io_online
        it_branches = lt_branches
      CHANGING
        ct_branches = rt_branches ).

  ENDMETHOD.

  METHOD find_changed_files_all.

    DATA ls_branch          LIKE LINE OF it_branches.
    DATA lt_sha1            TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects         TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_starting_folder TYPE string.
    DATA ls_main            LIKE LINE OF it_branches.
    DATA lt_expanded        TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lt_main_expanded   TYPE zif_abapgit_git_definitions=>ty_expanded_tt.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_branches.


    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_objects = zcl_abapgit_gitv2_porcelain=>list_no_blobs_multi(
      iv_url  = io_online->get_url( )
      it_sha1 = lt_sha1 ).

    lv_starting_folder = io_online->get_dot_abapgit( )->get_starting_folder( ) && '*'.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = c_main.
    ASSERT sy-subrc = 0.

    lt_main_expanded = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = ls_main-sha1 ).
    DELETE lt_main_expanded WHERE path NP lv_starting_folder.

    LOOP AT ct_branches ASSIGNING <ls_branch> WHERE display_name <> c_main.
      lt_expanded = zcl_abapgit_git_porcelain=>full_tree(
        it_objects = lt_objects
        iv_parent  = <ls_branch>-sha1 ).
      DELETE lt_expanded WHERE path NP lv_starting_folder.

      <ls_branch>-changed_files = find_changed_files(
        it_expanded1 = lt_main_expanded
        it_expanded2 = lt_expanded ).
    ENDLOOP.

  ENDMETHOD.

  METHOD find_changed_files.
* dont care if its added or removed or changed, just remove identical
* also list identical moved files

    DATA ls_path_name LIKE LINE OF rt_files.

    FIELD-SYMBOLS <ls_expanded1> LIKE LINE OF it_expanded1.
    FIELD-SYMBOLS <ls_expanded2> LIKE LINE OF it_expanded1.

    LOOP AT it_expanded1 ASSIGNING <ls_expanded1>.
      READ TABLE it_expanded2 ASSIGNING <ls_expanded2>
        WITH TABLE KEY path_name COMPONENTS
        path = <ls_expanded1>-path
        name = <ls_expanded1>-name.
      IF sy-subrc = 0 AND <ls_expanded1>-sha1 = <ls_expanded2>-sha1.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_expanded1> TO ls_path_name.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

    LOOP AT it_expanded2 ASSIGNING <ls_expanded2>.
      READ TABLE it_expanded1 ASSIGNING <ls_expanded1>
        WITH TABLE KEY path_name COMPONENTS
        path = <ls_expanded2>-path
        name = <ls_expanded2>-name.
      IF sy-subrc = 0 AND <ls_expanded1>-sha1 = <ls_expanded2>-sha1.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_expanded2> TO ls_path_name.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
