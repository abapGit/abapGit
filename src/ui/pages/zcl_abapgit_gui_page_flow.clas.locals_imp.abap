*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS list_changes_per_branch
      IMPORTING
        io_online TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD list_changes_per_branch.

    DATA lt_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_branch   LIKE LINE OF lt_branches.
    DATA lt_sha1     TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt.
    DATA lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_starting_folder TYPE string.

    lt_branches = zcl_abapgit_gitv2_porcelain=>list_branches(
      iv_url    = io_online->get_url( )
      iv_prefix = 'refs/heads/' )->get_all( ).
    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_objects = zcl_abapgit_gitv2_porcelain=>list_no_blobs_multi(
      iv_url  = io_online->get_url( )
      it_sha1 = lt_sha1 ).

    lv_starting_folder = io_online->get_dot_abapgit( )->get_starting_folder( ) && '*'.

    LOOP AT lt_branches INTO ls_branch WHERE is_head = abap_false.
      lt_expanded = zcl_abapgit_git_porcelain=>full_tree(
        it_objects = lt_objects
        iv_parent  = ls_branch-sha1 ).
      DELETE lt_expanded WHERE path NP lv_starting_folder.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
