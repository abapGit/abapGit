INTERFACE lif_find_changes.

  METHODS find_changes
    IMPORTING
      iv_main            TYPE zif_abapgit_git_definitions=>ty_sha1
      iv_branch          TYPE zif_abapgit_git_definitions=>ty_sha1
      iv_first_commit    TYPE zif_abapgit_git_definitions=>ty_sha1
      iv_starting_folder TYPE string
    RETURNING
      VALUE(rt_files)    TYPE zif_abapgit_flow_logic=>ty_path_name_tt
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.

****************************************************************************

CLASS lcl_walker DEFINITION.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        it_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    METHODS walk
      IMPORTING
        iv_path         TYPE string
        iv_tree_main    TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_tree_branch  TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_flow_logic=>ty_path_name_tt
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tree_cache,
             sha1  TYPE string,
             nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
           END OF ty_tree_cache.
    DATA mt_tree_cache TYPE HASHED TABLE OF ty_tree_cache WITH UNIQUE KEY sha1.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    METHODS decode_tree
      IMPORTING
        iv_tree         TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_nodes) TYPE zcl_abapgit_git_pack=>ty_nodes_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_walker IMPLEMENTATION.

  METHOD constructor.
    mt_objects = it_objects.
  ENDMETHOD.

  METHOD walk.

    DATA lt_main TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA lt_branch TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node_main LIKE LINE OF lt_main.
    DATA ls_node_branch LIKE LINE OF lt_branch.
    DATA ls_file LIKE LINE OF rt_files.
    DATA lt_files LIKE rt_files.

    IF iv_tree_main IS NOT INITIAL.
      lt_main = decode_tree( iv_tree_main ).
    ENDIF.

    IF iv_tree_branch IS NOT INITIAL.
      lt_branch = decode_tree( iv_tree_branch ).
    ENDIF.

    LOOP AT lt_main INTO ls_node_main.
      CLEAR ls_node_branch.
      READ TABLE lt_branch INTO ls_node_branch WITH KEY name = ls_node_main-name.
      IF sy-subrc = 0.
        DELETE lt_branch INDEX sy-tabix.
        IF ls_node_branch-sha1 = ls_node_main-sha1.
          CONTINUE.
        ENDIF.
      ENDIF.

      CASE ls_node_main-chmod.
        WHEN zif_abapgit_git_definitions=>c_chmod-dir.
          lt_files = walk(
            iv_path         = iv_path && ls_node_main-name && '/'
            iv_tree_main    = ls_node_main-sha1
            iv_tree_branch  = ls_node_branch-sha1 ).
          INSERT LINES OF lt_files INTO TABLE rt_files.
        WHEN zif_abapgit_git_definitions=>c_chmod-file.
          CLEAR ls_file.
          ls_file-path = iv_path.
          ls_file-filename = ls_node_main-name.
          ls_file-remote_sha1 = ls_node_branch-sha1.
          INSERT ls_file INTO TABLE rt_files.
        WHEN OTHERS.
          " ignore other types
      ENDCASE.
    ENDLOOP.

* new in branch, not in main
    LOOP AT lt_branch INTO ls_node_branch.
      CASE ls_node_branch-chmod.
        WHEN zif_abapgit_git_definitions=>c_chmod-dir.
          lt_files = walk(
            iv_path         = iv_path && ls_node_branch-name && '/'
            iv_tree_branch  = ls_node_branch-sha1 ).
          INSERT LINES OF lt_files INTO TABLE rt_files.
        WHEN zif_abapgit_git_definitions=>c_chmod-file.
          CLEAR ls_file.
          ls_file-path = iv_path.
          ls_file-filename = ls_node_branch-name.
          ls_file-remote_sha1 = ls_node_branch-sha1.
          INSERT ls_file INTO TABLE rt_files.
        WHEN OTHERS.
          " ignore other types
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD decode_tree.
    DATA ls_cache LIKE LINE OF mt_tree_cache.
    DATA ls_object LIKE LINE OF mt_objects.

    FIELD-SYMBOLS <ls_cache> LIKE LINE OF mt_tree_cache.

    READ TABLE mt_tree_cache ASSIGNING <ls_cache> WITH KEY sha1 = iv_tree.
    IF sy-subrc = 0.
      rt_nodes = <ls_cache>-nodes.
    ELSE.
      READ TABLE mt_objects INTO ls_object WITH TABLE KEY type
        COMPONENTS sha1 = iv_tree type = zif_abapgit_git_definitions=>c_type-tree.
      ASSERT sy-subrc = 0.

      rt_nodes = zcl_abapgit_git_pack=>decode_tree( ls_object-data ).

      ls_cache-sha1 = iv_tree.
      ls_cache-nodes = rt_nodes.
      INSERT ls_cache INTO TABLE mt_tree_cache.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

****************************************************************************

CLASS lcl_find_changes_new DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    INTERFACES lif_find_changes.
  PRIVATE SECTION.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA mo_walker TYPE REF TO lcl_walker.

    METHODS find_changed_in_commit
      IMPORTING
        is_commit TYPE zcl_abapgit_git_pack=>ty_commit
      CHANGING
        ct_files  TYPE zif_abapgit_flow_logic=>ty_path_name_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_find_changes_new IMPLEMENTATION.
  METHOD constructor.
    mt_objects = it_objects.
    CREATE OBJECT mo_walker EXPORTING it_objects = it_objects.
  ENDMETHOD.

  METHOD find_changed_in_commit.

    DATA ls_parent_commit TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA lt_files LIKE ct_files.
    DATA ls_file LIKE LINE OF lt_files.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF mt_objects.


    ASSERT is_commit-parent IS NOT INITIAL.
* as per calling logic, never called if there is a parent2
    ASSERT is_commit-parent2 IS INITIAL.

    READ TABLE mt_objects ASSIGNING <ls_object> WITH TABLE KEY sha COMPONENTS sha1 = is_commit-parent.
    ASSERT sy-subrc = 0.
    ls_parent_commit = zcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

    lt_files = mo_walker->walk(
      iv_path       = '/'
      iv_tree_main  = ls_parent_commit-tree
      iv_tree_branch = is_commit-tree ).

    LOOP AT lt_files INTO ls_file.
* if its already there, then skip, we want the latest(top) change in the list
      INSERT ls_file INTO TABLE ct_files.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_find_changes~find_changes.
    DATA lv_current TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA ls_commit  TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS <ls_commit> LIKE LINE OF mt_objects.


    ASSERT iv_first_commit IS NOT INITIAL.
    ASSERT iv_main IS NOT INITIAL.
    ASSERT iv_branch IS NOT INITIAL.

    lv_current = iv_branch.

    DO.
      READ TABLE mt_objects ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
      ASSERT sy-subrc = 0.

      ls_commit = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
      IF ls_commit-parent2 IS INITIAL.
* analyze changed files in commit
        find_changed_in_commit(
          EXPORTING is_commit = ls_commit
          CHANGING  ct_files  = rt_files ).
      ENDIF.

      IF lv_current = iv_first_commit.
        EXIT.
      ENDIF.

      lv_current = ls_commit-parent.
    ENDDO.

  ENDMETHOD.
ENDCLASS.

****************************************************************************

CLASS lcl_find_changes DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    INTERFACES lif_find_changes.

  PRIVATE SECTION.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA mo_walker TYPE REF TO lcl_walker.

ENDCLASS.

CLASS lcl_find_changes IMPLEMENTATION.

  METHOD constructor.
    mt_objects = it_objects.
    CREATE OBJECT mo_walker EXPORTING it_objects = it_objects.
  ENDMETHOD.

  METHOD lif_find_changes~find_changes.
* don't care if its added or removed or changed, just remove identical
* also list identical moved files
    DATA ls_object LIKE LINE OF mt_objects.
    DATA lv_tree_main TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_tree_branch TYPE zif_abapgit_git_definitions=>ty_sha1.

    READ TABLE mt_objects WITH TABLE KEY type
      COMPONENTS sha1 = iv_main type = zif_abapgit_git_definitions=>c_type-commit INTO ls_object.
    ASSERT sy-subrc = 0.
    lv_tree_main = zcl_abapgit_git_pack=>decode_commit( ls_object-data )-tree.

    READ TABLE mt_objects WITH TABLE KEY type
      COMPONENTS sha1 = iv_branch type = zif_abapgit_git_definitions=>c_type-commit INTO ls_object.
    ASSERT sy-subrc = 0.
    lv_tree_branch = zcl_abapgit_git_pack=>decode_commit( ls_object-data )-tree.

    rt_files = mo_walker->walk(
      iv_path        = '/'
      iv_tree_main   = lv_tree_main
      iv_tree_branch = lv_tree_branch ).

    DELETE rt_files WHERE path NP iv_starting_folder.
  ENDMETHOD.
ENDCLASS.

***************************************************************************

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
