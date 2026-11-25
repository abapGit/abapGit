CLASS zcl_abapgit_git_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS build_files
      IMPORTING
        !iv_commit      TYPE zif_abapgit_git_definitions=>ty_sha1
        !it_objects     TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS full_tree
      IMPORTING
        !it_objects        TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_parent         TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_expanded) TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS walk
      IMPORTING
        !it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_path    TYPE string
      CHANGING
        !ct_files   TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS walk_tree
      IMPORTING
        !it_objects        TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_tree           TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_base           TYPE string
      RETURNING
        VALUE(rt_expanded) TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_git_objects IMPLEMENTATION.


  METHOD build_files.

    DATA: ls_object TYPE zif_abapgit_definitions=>ty_object,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_commit.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Commit/Branch not found.' ).
    ENDIF.

    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = it_objects
                    iv_sha1    = ls_commit-tree
                    iv_path    = '/'
          CHANGING  ct_files   = rt_files ).

  ENDMETHOD.



  METHOD full_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_parent.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'commit not found' ).
    ENDIF.
    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.



  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree>
      WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_sha1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Walk, tree not found' ).
    ENDIF.

    lt_nodes = zcl_abapgit_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = zif_abapgit_git_definitions=>c_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY type COMPONENTS
            type = zif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_node>-sha1.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Walk, blob not found' ).
        ENDIF.

        CLEAR ls_file.
        ls_file-path     = iv_path.
        ls_file-filename = <ls_node>-name.
        ls_file-data     = <ls_blob>-data.
        ls_file-sha1     = <ls_blob>-sha1.
        APPEND ls_file TO ct_files.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = zif_abapgit_git_definitions=>c_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.

      walk( EXPORTING it_objects = it_objects
                      iv_sha1    = <ls_node>-sha1
                      iv_path    = lv_path
            CHANGING  ct_files   = ct_files ).
    ENDLOOP.

  ENDMETHOD.



  METHOD walk_tree.

    DATA: ls_object   LIKE LINE OF it_objects,
          lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_tree.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'tree not found' ).
    ENDIF.
    lt_nodes = zcl_abapgit_git_pack=>decode_tree( ls_object-data ).

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
            it_objects = it_objects
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |walk_tree: unknown chmod { <ls_node>-chmod }| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
