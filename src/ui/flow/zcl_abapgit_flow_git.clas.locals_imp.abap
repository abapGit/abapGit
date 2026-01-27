INTERFACE lif_find_changes.

  METHODS find_changes
    IMPORTING
      iv_main                TYPE zif_abapgit_git_definitions=>ty_sha1
      iv_branch              TYPE zif_abapgit_git_definitions=>ty_sha1
      iv_first_commit        TYPE zif_abapgit_git_definitions=>ty_sha1
      iv_latest_merge_commit TYPE zif_abapgit_git_definitions=>ty_sha1
    RETURNING
      VALUE(rt_files)        TYPE zif_abapgit_flow_logic=>ty_path_name_tt
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.

****************************************************************************

CLASS lcl_walker DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS initialize
      IMPORTING
        it_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    CLASS-METHODS walk
      IMPORTING
        iv_path         TYPE string
        iv_tree_main    TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        iv_tree_branch  TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_flow_logic=>ty_path_name_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS expand
      IMPORTING
        iv_parent          TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_expanded) TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
* the amount of data here should be manageable in memory
    TYPES: BEGIN OF ty_tree_cache,
             sha1  TYPE string,
             nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
           END OF ty_tree_cache.
    CLASS-DATA gt_tree_cache TYPE HASHED TABLE OF ty_tree_cache WITH UNIQUE KEY sha1.
    CLASS-DATA gt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    CLASS-METHODS walk_tree
      IMPORTING
        iv_tree            TYPE zif_abapgit_git_definitions=>ty_sha1
        iv_base            TYPE string
      RETURNING
        VALUE(rt_expanded) TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS decode_tree
      IMPORTING
        iv_tree         TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_nodes) TYPE zcl_abapgit_git_pack=>ty_nodes_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_walker IMPLEMENTATION.

  METHOD initialize.
    gt_objects = it_objects.
  ENDMETHOD.

  METHOD expand.

    DATA: ls_object LIKE LINE OF gt_objects,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    READ TABLE gt_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_parent.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'commit not found' ).
    ENDIF.
    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.

  METHOD walk_tree.

    DATA: lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    lt_nodes = decode_tree( iv_tree ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN zif_abapgit_git_definitions=>c_chmod-file
            OR zif_abapgit_git_definitions=>c_chmod-executable
            OR zif_abapgit_git_definitions=>c_chmod-symbolic_link
            OR zif_abapgit_git_definitions=>c_chmod-submodule.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path  = iv_base.
          <ls_exp>-name  = <ls_node>-name.
          <ls_exp>-sha1  = <ls_node>-sha1.
          <ls_exp>-chmod = <ls_node>-chmod.
        WHEN zif_abapgit_git_definitions=>c_chmod-dir.
          lt_expanded = walk_tree(
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |walk_tree: unknown chmod { <ls_node>-chmod }| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD walk.

    DATA lt_main        TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA lt_branch      TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_node_main   LIKE LINE OF lt_main.
    DATA ls_node_branch LIKE LINE OF lt_branch.
    DATA ls_file        LIKE LINE OF rt_files.
    DATA lt_files       LIKE rt_files.

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
* the file or folder matches, skip
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
    DATA ls_cache LIKE LINE OF gt_tree_cache.

    FIELD-SYMBOLS <ls_cache> LIKE LINE OF gt_tree_cache.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF gt_objects.

    READ TABLE gt_tree_cache ASSIGNING <ls_cache> WITH KEY sha1 = iv_tree.
    IF sy-subrc = 0.
      rt_nodes = <ls_cache>-nodes.
    ELSE.
      READ TABLE gt_objects ASSIGNING <ls_object> WITH TABLE KEY type
        COMPONENTS sha1 = iv_tree type = zif_abapgit_git_definitions=>c_type-tree.
      ASSERT sy-subrc = 0.

      rt_nodes = zcl_abapgit_git_pack=>decode_tree( <ls_object>-data ).

      ls_cache-sha1 = iv_tree.
      ls_cache-nodes = rt_nodes.
      INSERT ls_cache INTO TABLE gt_tree_cache.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

****************************************************************************

* this one allows branches not being up to date with main
*  branch:         A---B---C---D
*                 /       /
*  main:      X---Y---Z---Q---W
CLASS lcl_find_changes_new DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    INTERFACES lif_find_changes.
  PRIVATE SECTION.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

ENDCLASS.

CLASS lcl_find_changes_new IMPLEMENTATION.
  METHOD constructor.
    mt_objects = it_objects.
  ENDMETHOD.

  METHOD lif_find_changes~find_changes.

    DATA ls_commit1 TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA ls_commit2 TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS <ls_commit> LIKE LINE OF mt_objects.


    ASSERT iv_first_commit IS NOT INITIAL.
    ASSERT iv_branch IS NOT INITIAL.


    READ TABLE mt_objects ASSIGNING <ls_commit> WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit sha1 = iv_branch.
    ASSERT sy-subrc = 0.
    ls_commit1 = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).

    IF iv_latest_merge_commit IS NOT INITIAL.
      READ TABLE mt_objects ASSIGNING <ls_commit> WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit sha1 = iv_latest_merge_commit.
      ASSERT sy-subrc = 0.
      ls_commit2 = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
      ASSERT ls_commit2-parent2 IS NOT INITIAL.
      READ TABLE mt_objects ASSIGNING <ls_commit> WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit sha1 = ls_commit2-parent2.
      ASSERT sy-subrc = 0.
      ls_commit2 = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
    ELSE.
      READ TABLE mt_objects ASSIGNING <ls_commit> WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit sha1 = iv_first_commit.
      ASSERT sy-subrc = 0.
      ls_commit2 = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
      READ TABLE mt_objects ASSIGNING <ls_commit> WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit sha1 = ls_commit2-parent.
      ASSERT sy-subrc = 0.
      ls_commit2 = zcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
    ENDIF.

    rt_files = lcl_walker=>walk(
      iv_path        = '/'
      iv_tree_main   = ls_commit2-tree
      iv_tree_branch = ls_commit1-tree ).

  ENDMETHOD.
ENDCLASS.

****************************************************************************

* assumes branches are up to date with main
CLASS lcl_find_changes DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    INTERFACES lif_find_changes.

  PRIVATE SECTION.
    DATA mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

ENDCLASS.

CLASS lcl_find_changes IMPLEMENTATION.

  METHOD constructor.
    mt_objects = it_objects.
  ENDMETHOD.

  METHOD lif_find_changes~find_changes.
* don't care if its added or removed or changed, just remove identical
* also list identical moved files
    DATA ls_object      LIKE LINE OF mt_objects.
    DATA lv_tree_main   TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA lv_tree_branch TYPE zif_abapgit_git_definitions=>ty_sha1.

    READ TABLE mt_objects WITH TABLE KEY type
      COMPONENTS sha1 = iv_main type = zif_abapgit_git_definitions=>c_type-commit INTO ls_object.
    ASSERT sy-subrc = 0.
    lv_tree_main = zcl_abapgit_git_pack=>decode_commit( ls_object-data )-tree.

    READ TABLE mt_objects WITH TABLE KEY type
      COMPONENTS sha1 = iv_branch type = zif_abapgit_git_definitions=>c_type-commit INTO ls_object.
    ASSERT sy-subrc = 0.
    lv_tree_branch = zcl_abapgit_git_pack=>decode_commit( ls_object-data )-tree.

    rt_files = lcl_walker=>walk(
      iv_path        = '/'
      iv_tree_main   = lv_tree_main
      iv_tree_branch = lv_tree_branch ).

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
