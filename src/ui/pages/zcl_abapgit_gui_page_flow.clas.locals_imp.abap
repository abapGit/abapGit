*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_sha1_stack DEFINITION.
  PUBLIC SECTION.
    METHODS clear
      RETURNING
        VALUE(ro_stack) TYPE REF TO lcl_sha1_stack.

    METHODS push
      IMPORTING
        iv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS pop
      RETURNING
        VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i.
  PRIVATE SECTION.
    DATA mt_list TYPE STANDARD TABLE OF zif_abapgit_git_definitions=>ty_sha1 WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_sha1_stack IMPLEMENTATION.
  METHOD clear.
    CLEAR mt_list.
    ro_stack = me.
  ENDMETHOD.

  METHOD push.
    INSERT iv_sha1 INTO mt_list INDEX 1.
  ENDMETHOD.

  METHOD pop.
    READ TABLE mt_list INDEX 1 INTO rv_sha1.
    ASSERT sy-subrc = 0.
    DELETE mt_list INDEX 1.
  ENDMETHOD.

  METHOD size.
    rv_size = lines( mt_list ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_path_name,
        path TYPE string,
        name TYPE string,
      END OF ty_path_name.
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
        io_online   TYPE REF TO zcl_abapgit_repo_online
        it_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_branches TYPE ty_branches
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
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

    find_up_to_date(
      EXPORTING
        iv_url      = io_online->get_url( )
        it_branches = lt_branches
      CHANGING
        ct_branches = rt_branches ).

  ENDMETHOD.

  METHOD find_up_to_date.

    TYPES: BEGIN OF ty_commit,
             commit  TYPE zif_abapgit_git_definitions=>ty_sha1,
             parent  TYPE zif_abapgit_git_definitions=>ty_sha1,
             parent2 TYPE zif_abapgit_git_definitions=>ty_sha1,
           END OF ty_commit.

    DATA ls_branch  LIKE LINE OF it_branches.
    DATA lt_commits TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_main    LIKE LINE OF it_branches.
    DATA lv_current TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lo_visit   TYPE REF TO lcl_sha1_stack.
    DATA ls_raw     TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA lt_parsed  TYPE HASHED TABLE OF ty_commit WITH UNIQUE KEY commit.
    DATA ls_parsed  LIKE LINE OF lt_parsed.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_branches.
    FIELD-SYMBOLS <ls_commit> LIKE LINE OF lt_commits.


    READ TABLE it_branches INTO ls_main WITH KEY display_name = c_main.
    ASSERT sy-subrc = 0.

    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_commits = zcl_abapgit_gitv2_porcelain=>commits_last_year(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    CREATE OBJECT lo_visit.
    LOOP AT ct_branches ASSIGNING <ls_branch>.
      <ls_branch>-up_to_date = abap_false.
      lo_visit->clear( )->push( <ls_branch>-sha1 ).

      WHILE lo_visit->size( ) > 0.
        lv_current = lo_visit->pop( ).
        IF lv_current = ls_main-sha1.
          <ls_branch>-up_to_date = abap_true.
          EXIT.
        ENDIF.

        READ TABLE lt_parsed INTO ls_parsed WITH KEY commit = lv_current.
        IF sy-subrc <> 0.
          READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
          IF sy-subrc <> 0.
            " if not found in lt_commits its more than a year old
            CONTINUE.
          ENDIF.
          ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
          ls_parsed-commit = lv_current.
          ls_parsed-parent = ls_raw-parent.
          ls_parsed-parent2 = ls_raw-parent2.
          INSERT ls_parsed INTO TABLE lt_parsed.
        ENDIF.

        lo_visit->push( ls_parsed-parent ).
        IF ls_parsed-parent2 IS NOT INITIAL.
          lo_visit->push( ls_parsed-parent2 ).
        ENDIF.
      ENDWHILE.

    ENDLOOP.

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
