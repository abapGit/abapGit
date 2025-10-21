CLASS zcl_abapgit_flow_git DEFINITION PUBLIC.
  PUBLIC SECTION.
* various git related methods

    CLASS-METHODS find_changes_in_git
      IMPORTING
        iv_url           TYPE string
        io_dot           TYPE REF TO zcl_abapgit_dot_abapgit
        iv_package       TYPE devclass
        it_branches      TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      EXPORTING
        et_main_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE zif_abapgit_flow_logic=>ty_features
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE zif_abapgit_flow_logic=>ty_features
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE zif_abapgit_flow_logic=>ty_path_name_tt
        io_dot                    TYPE REF TO zcl_abapgit_dot_abapgit
        iv_package                TYPE devclass
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
    DATA lo_find            TYPE REF TO lcl_find_changes.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.


    CLEAR et_main_expanded.

    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_objects = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_no_blobs_multi(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    lv_starting_folder = io_dot->get_starting_folder( ) && '*'.

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
        io_dot     = io_dot
        iv_package = iv_package
        it_files   = <ls_branch>-changed_files ).
    ENDLOOP.

    find_up_to_date(
      EXPORTING
        iv_url      = iv_url
        it_branches = it_branches
      CHANGING
        ct_features = ct_features ).

  ENDMETHOD.

  METHOD map_files_to_objects.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.


    LOOP AT it_files ASSIGNING <ls_file>.
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-filename
          iv_path     = <ls_file>-path
          iv_devclass = iv_package
          io_dot      = io_dot
        IMPORTING
          es_item     = ls_item ).
      INSERT ls_item INTO TABLE rt_changed_objects.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_up_to_date.

    DATA ls_branch  LIKE LINE OF it_branches.
    DATA lt_commits TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_main    LIKE LINE OF it_branches.
    DATA lv_current TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lo_visit   TYPE REF TO lcl_sha1_stack.
    DATA ls_raw     TYPE zcl_abapgit_git_pack=>ty_commit.

    DATA lt_main_reachable TYPE HASHED TABLE OF zif_abapgit_git_definitions=>ty_sha1 WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_commit> LIKE LINE OF lt_commits.


    IF lines( it_branches ) = 1.
      " only main branch
      RETURN.
    ENDIF.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = zif_abapgit_flow_logic=>c_main.
    ASSERT sy-subrc = 0.

    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_commits = zcl_abapgit_git_factory=>get_v2_porcelain( )->commits_last_year(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    CREATE OBJECT lo_visit.
    lo_visit->clear( )->push( ls_main-sha1 ).
    WHILE lo_visit->size( ) > 0.
      lv_current = lo_visit->pop( ).
      INSERT lv_current INTO TABLE lt_main_reachable.
      READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
      IF sy-subrc = 0.
        ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
        lo_visit->push( ls_raw-parent ).
        IF ls_raw-parent2 IS NOT INITIAL.
          lo_visit->push( ls_raw-parent2 ).
        ENDIF.
      ENDIF.
    ENDWHILE.

    LOOP AT ct_features ASSIGNING <ls_branch>.
      <ls_branch>-branch-up_to_date = abap_undefined.
      lo_visit->clear( )->push( <ls_branch>-branch-sha1 ).

      WHILE lo_visit->size( ) > 0.
        lv_current = lo_visit->pop( ).
        IF lv_current = ls_main-sha1.
          <ls_branch>-branch-up_to_date = abap_true.
          EXIT.
        ENDIF.

        READ TABLE lt_main_reachable WITH KEY table_line = lv_current TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_branch>-branch-up_to_date = abap_false.
          EXIT.
        ENDIF.

        READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
        IF sy-subrc = 0.
          ls_raw = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
          lo_visit->push( ls_raw-parent ).
          IF ls_raw-parent2 IS NOT INITIAL.
            lo_visit->push( ls_raw-parent2 ).
          ENDIF.
        ENDIF.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
