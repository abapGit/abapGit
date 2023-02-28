CLASS zcl_abapgit_git_porcelain DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_pull_result,
        files   TYPE zif_abapgit_git_definitions=>ty_files_tt,
        objects TYPE zif_abapgit_definitions=>ty_objects_tt,
        commit  TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_pull_result .
    TYPES:
      BEGIN OF ty_push_result,
        new_files     TYPE zif_abapgit_git_definitions=>ty_files_tt,
        branch        TYPE zif_abapgit_git_definitions=>ty_sha1,
        updated_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt,
        new_objects   TYPE zif_abapgit_definitions=>ty_objects_tt,
      END OF ty_push_result .

    CLASS-METHODS pull_by_branch
      IMPORTING
        !iv_url          TYPE string
        !iv_branch_name  TYPE string
        !iv_deepen_level TYPE i DEFAULT 1
      RETURNING
        VALUE(rs_result) TYPE ty_pull_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS pull_by_commit
      IMPORTING
        !iv_url          TYPE string
        !iv_commit_hash  TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_deepen_level TYPE i DEFAULT 1
      RETURNING
        VALUE(rs_result) TYPE ty_pull_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS push
      IMPORTING
        !is_comment      TYPE zif_abapgit_git_definitions=>ty_comment
        !io_stage        TYPE REF TO zcl_abapgit_stage
        !it_old_objects  TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_parent       TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_url          TYPE string
        !iv_branch_name  TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_push_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_branch
      IMPORTING
        !iv_url  TYPE string
        !iv_name TYPE string
        !iv_from TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_tag
      IMPORTING
        !iv_url TYPE string
        !is_tag TYPE zif_abapgit_git_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_branch
      IMPORTING
        !iv_url    TYPE string
        !is_branch TYPE zif_abapgit_git_definitions=>ty_git_branch
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_tag
      IMPORTING
        !iv_url TYPE string
        !is_tag TYPE zif_abapgit_git_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS full_tree
      IMPORTING
        !it_objects        TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_parent         TYPE zif_abapgit_git_definitions=>ty_sha1
      RETURNING
        VALUE(rt_expanded) TYPE zif_abapgit_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tree,
        path TYPE string,
        data TYPE xstring,
        sha1 TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_tree .
    TYPES:
      ty_trees_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_folder,
        path  TYPE string,
        count TYPE i,
        sha1  TYPE zif_abapgit_git_definitions=>ty_sha1,
      END OF ty_folder .
    TYPES:
      ty_folders_tt TYPE STANDARD TABLE OF ty_folder WITH DEFAULT KEY .

    CONSTANTS c_zero TYPE zif_abapgit_git_definitions=>ty_sha1
      VALUE '0000000000000000000000000000000000000000' ##NO_TEXT.

    CLASS-METHODS build_trees
      IMPORTING
        !it_expanded    TYPE zif_abapgit_definitions=>ty_expanded_tt
      RETURNING
        VALUE(rt_trees) TYPE ty_trees_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS find_folders
      IMPORTING
        !it_expanded      TYPE zif_abapgit_definitions=>ty_expanded_tt
      RETURNING
        VALUE(rt_folders) TYPE ty_folders_tt .
    CLASS-METHODS pull
      IMPORTING
        !iv_commit      TYPE zif_abapgit_git_definitions=>ty_sha1
        !it_objects     TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception.
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
        VALUE(rt_expanded) TYPE zif_abapgit_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS receive_pack_push
      IMPORTING
        !is_comment     TYPE zif_abapgit_git_definitions=>ty_comment
        !it_trees       TYPE ty_trees_tt
        !it_blobs       TYPE zif_abapgit_git_definitions=>ty_files_tt
        !iv_parent      TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_parent2     TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
        !iv_url         TYPE string
        !iv_branch_name TYPE string
      EXPORTING
        !ev_new_commit  TYPE zif_abapgit_git_definitions=>ty_sha1
        !et_new_objects TYPE zif_abapgit_definitions=>ty_objects_tt
        !ev_new_tree    TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_annotated_tag
      IMPORTING
        !is_tag TYPE zif_abapgit_git_definitions=>ty_git_tag
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_lightweight_tag
      IMPORTING
        is_tag TYPE zif_abapgit_git_definitions=>ty_git_tag
        iv_url TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_annotated_tag
      IMPORTING
        !iv_url TYPE string
        !is_tag TYPE zif_abapgit_git_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_lightweight_tag
      IMPORTING
        !iv_url TYPE string
        !is_tag TYPE zif_abapgit_git_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS empty_packfile
      RETURNING
        VALUE(rv_pack) TYPE xstring
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_git_porcelain IMPLEMENTATION.


  METHOD build_trees.

    DATA: lt_nodes   TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
          ls_tree    LIKE LINE OF rt_trees,
          lv_len     TYPE i,
          lt_folders TYPE ty_folders_tt.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF lt_folders,
                   <ls_node>   LIKE LINE OF lt_nodes,
                   <ls_sub>    LIKE LINE OF lt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    lt_folders = find_folders( it_expanded ).

* start with the deepest folders
    SORT lt_folders BY count DESCENDING.

    LOOP AT lt_folders ASSIGNING <ls_folder>.
      CLEAR lt_nodes.

* files
      LOOP AT it_expanded ASSIGNING <ls_exp> WHERE path = <ls_folder>-path.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = <ls_exp>-chmod.
        <ls_node>-name  = <ls_exp>-name.
        <ls_node>-sha1  = <ls_exp>-sha1.
      ENDLOOP.

* folders
      LOOP AT lt_folders ASSIGNING <ls_sub> WHERE count = <ls_folder>-count + 1.
        lv_len = strlen( <ls_folder>-path ).
        IF strlen( <ls_sub>-path ) > lv_len AND <ls_sub>-path(lv_len) = <ls_folder>-path.
          APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
          <ls_node>-chmod = zif_abapgit_definitions=>c_chmod-dir.

* extract folder name, this can probably be done easier using regular expressions
          <ls_node>-name = <ls_sub>-path+lv_len.
          lv_len = strlen( <ls_node>-name ) - 1.
          <ls_node>-name = <ls_node>-name(lv_len).

          <ls_node>-sha1 = <ls_sub>-sha1.
        ENDIF.
      ENDLOOP.

      CLEAR ls_tree.
      ls_tree-path = <ls_folder>-path.
      ls_tree-data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ).
      ls_tree-sha1 = zcl_abapgit_hash=>sha1_tree( ls_tree-data ).
      APPEND ls_tree TO rt_trees.

      <ls_folder>-sha1 = ls_tree-sha1.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_annotated_tag.

    DATA: lv_tag          TYPE xstring,
          lt_objects      TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack         TYPE xstring,
          ls_object       LIKE LINE OF lt_objects,
          ls_tag          TYPE zcl_abapgit_git_pack=>ty_tag,
          lv_new_tag_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.

* new tag
    ls_tag-object       = is_tag-sha1.
    ls_tag-type         = zif_abapgit_definitions=>c_type-commit.
    ls_tag-tag          = is_tag-name.
    ls_tag-tagger_name  = is_tag-tagger_name.
    ls_tag-tagger_email = is_tag-tagger_email.
    ls_tag-message      = is_tag-message
                      && |{ cl_abap_char_utilities=>newline }|
                      && |{ cl_abap_char_utilities=>newline }|
                      && is_tag-body.

    lv_tag = zcl_abapgit_git_pack=>encode_tag( ls_tag ).

    lv_new_tag_sha1 = zcl_abapgit_hash=>sha1_tag( lv_tag ).

    ls_object-sha1  = lv_new_tag_sha1.
    ls_object-type  = zif_abapgit_definitions=>c_type-tag.
    ls_object-data  = lv_tag.
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = lv_new_tag_sha1
      iv_branch_name = is_tag-name
      iv_pack        = lv_pack ).

  ENDMETHOD.


  METHOD create_branch.

    IF iv_name CS ` `.
      zcx_abapgit_exception=>raise( 'Branch name cannot contain blank spaces' ).
    ENDIF.

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = iv_from
      iv_branch_name = iv_name
      iv_pack        = empty_packfile( ) ).

  ENDMETHOD.


  METHOD create_lightweight_tag.

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = is_tag-sha1
      iv_branch_name = is_tag-name
      iv_pack        = empty_packfile( ) ).

  ENDMETHOD.


  METHOD create_tag.

    IF is_tag-name CS ` `.
      zcx_abapgit_exception=>raise( 'Tag name cannot contain blank spaces' ).
    ENDIF.

    CASE is_tag-type.
      WHEN zif_abapgit_definitions=>c_git_branch_type-annotated_tag.

        create_annotated_tag(
          is_tag = is_tag
          iv_url = iv_url ).

      WHEN zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.

        create_lightweight_tag(
          is_tag = is_tag
          iv_url = iv_url ).

      WHEN OTHERS.

        zcx_abapgit_exception=>raise( |Invalid tag type: { is_tag-type }| ).

    ENDCASE.

  ENDMETHOD.


  METHOD delete_annotated_tag.

    DATA:
      lo_branches TYPE REF TO zcl_abapgit_git_branch_list,
      lv_tag      TYPE string,
      ls_tag      TYPE zif_abapgit_git_definitions=>ty_git_branch,
      lt_tags     TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.

    " For annotated tags, find the correct commit
    lo_branches = zcl_abapgit_git_transport=>branches( iv_url ).
    lt_tags     = lo_branches->get_tags_only( ).
    lv_tag      = zcl_abapgit_git_tag=>remove_peel( is_tag-name ).

    READ TABLE lt_tags INTO ls_tag WITH KEY name_key COMPONENTS name = lv_tag.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Annotated tag { lv_tag } not found| ).
    ENDIF.

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = ls_tag-sha1
      iv_new         = c_zero
      iv_branch_name = ls_tag-name ).

  ENDMETHOD.


  METHOD delete_branch.

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_branch-sha1
      iv_new         = c_zero
      iv_branch_name = is_branch-name ).

  ENDMETHOD.


  METHOD delete_lightweight_tag.

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_tag-sha1
      iv_new         = c_zero
      iv_branch_name = is_tag-name ).

  ENDMETHOD.


  METHOD delete_tag.

    IF is_tag-name CS zif_abapgit_definitions=>c_git_branch-peel.

      delete_annotated_tag(
        is_tag = is_tag
        iv_url = iv_url ).

    ELSE.

      delete_lightweight_tag(
        is_tag = is_tag
        iv_url = iv_url ).

    ENDIF.

  ENDMETHOD.


  METHOD empty_packfile.

    " For avoiding "client MUST send an empty packfile" error
    " https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt#L595-L599

    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    rv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

  ENDMETHOD.


  METHOD find_folders.

    DATA: lt_paths TYPE TABLE OF string,
          lv_split TYPE string,
          lv_path  TYPE string.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF rt_folders,
                   <ls_new>    LIKE LINE OF rt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    LOOP AT it_expanded ASSIGNING <ls_exp>.
      READ TABLE rt_folders WITH KEY path = <ls_exp>-path TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_folder>.
        <ls_folder>-path = <ls_exp>-path.
      ENDIF.
    ENDLOOP.

* add empty folders
    LOOP AT rt_folders ASSIGNING <ls_folder>.
      SPLIT <ls_folder>-path AT '/' INTO TABLE lt_paths.

      CLEAR lv_path.
      LOOP AT lt_paths INTO lv_split.
        CONCATENATE lv_path lv_split '/' INTO lv_path.
        READ TABLE rt_folders WITH KEY path = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_new>.
          <ls_new>-path = lv_path.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT rt_folders ASSIGNING <ls_folder>.
      FIND ALL OCCURRENCES OF '/' IN <ls_folder>-path MATCH COUNT <ls_folder>-count.
    ENDLOOP.

  ENDMETHOD.


  METHOD full_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_definitions=>c_type-commit
        sha1 = iv_parent.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'commit not found' ).
    ENDIF.
    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.


  METHOD pull.

    DATA: ls_object TYPE zif_abapgit_definitions=>ty_object,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_definitions=>c_type-commit
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


  METHOD pull_by_branch.

    zcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = iv_url
        iv_branch_name  = iv_branch_name
        iv_deepen_level = iv_deepen_level
      IMPORTING
        et_objects      = rs_result-objects
        ev_branch       = rs_result-commit ).

    rs_result-files = pull( iv_commit  = rs_result-commit
                            it_objects = rs_result-objects ).

  ENDMETHOD.


  METHOD pull_by_commit.

    zcl_abapgit_git_transport=>upload_pack_by_commit(
      EXPORTING
        iv_url          = iv_url
        iv_hash         = iv_commit_hash
        iv_deepen_level = iv_deepen_level
      IMPORTING
        et_objects      = rs_result-objects
        ev_commit       = rs_result-commit ).

    rs_result-files = pull( iv_commit  = rs_result-commit
                            it_objects = rs_result-objects ).

  ENDMETHOD.


  METHOD push.

    DATA: lt_expanded TYPE zif_abapgit_definitions=>ty_expanded_tt,
          lt_blobs    TYPE zif_abapgit_git_definitions=>ty_files_tt,
          lv_sha1     TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_new_tree TYPE zif_abapgit_git_definitions=>ty_sha1,
          lt_trees    TYPE ty_trees_tt,
          lt_stage    TYPE zif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage>   LIKE LINE OF lt_stage,
                   <ls_updated> LIKE LINE OF rs_result-updated_files,
                   <ls_exp>     LIKE LINE OF lt_expanded.

    lt_expanded = full_tree( it_objects = it_old_objects
                             iv_parent  = iv_parent ).

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.

      " Save file ref to updated files table
      APPEND INITIAL LINE TO rs_result-updated_files ASSIGNING <ls_updated>.
      MOVE-CORRESPONDING <ls_stage>-file TO <ls_updated>.

      CASE <ls_stage>-method.
        WHEN zif_abapgit_definitions=>c_method-add.

          APPEND <ls_stage>-file TO lt_blobs.

          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH KEY
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          IF sy-subrc <> 0. " new files
            APPEND INITIAL LINE TO lt_expanded ASSIGNING <ls_exp>.
            <ls_exp>-name  = <ls_stage>-file-filename.
            <ls_exp>-path  = <ls_stage>-file-path.
            <ls_exp>-chmod = zif_abapgit_definitions=>c_chmod-file.
          ENDIF.

          lv_sha1 = zcl_abapgit_hash=>sha1_blob( <ls_stage>-file-data ).
          IF <ls_exp>-sha1 <> lv_sha1.
            <ls_exp>-sha1 = lv_sha1.
          ENDIF.

          <ls_updated>-sha1 = lv_sha1.   "New sha1

        WHEN zif_abapgit_definitions=>c_method-rm.
          DELETE lt_expanded
            WHERE name = <ls_stage>-file-filename
            AND path = <ls_stage>-file-path.
          ASSERT sy-subrc = 0.

          CLEAR <ls_updated>-sha1.       " Mark as deleted

        WHEN OTHERS.
          zcx_abapgit_exception=>raise( 'stage method not supported, todo' ).
      ENDCASE.
    ENDLOOP.

    lt_trees = build_trees( lt_expanded ).

    receive_pack_push(
      EXPORTING
        is_comment     = is_comment
        it_trees       = lt_trees
        iv_branch_name = iv_branch_name
        iv_url         = iv_url
        iv_parent      = iv_parent
        iv_parent2     = io_stage->get_merge_source( )
        it_blobs       = lt_blobs
      IMPORTING
        ev_new_commit  = rs_result-branch
        et_new_objects = rs_result-new_objects
        ev_new_tree    = lv_new_tree ).

    APPEND LINES OF it_old_objects TO rs_result-new_objects.

    walk( EXPORTING it_objects = rs_result-new_objects
                    iv_sha1    = lv_new_tree
                    iv_path    = '/'
          CHANGING  ct_files   = rs_result-new_files ).

  ENDMETHOD.


  METHOD receive_pack_push.

    DATA: lv_time   TYPE zcl_abapgit_git_time=>ty_unixtime,
          lv_commit TYPE xstring,
          lv_pack   TYPE xstring,
          ls_object LIKE LINE OF et_new_objects,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit,
          lv_uindex TYPE sy-index.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_trees,
                   <ls_blob> LIKE LINE OF it_blobs.


    lv_time = zcl_abapgit_git_time=>get_unix( ).

    READ TABLE it_trees ASSIGNING <ls_tree> WITH KEY path = '/'.
    ASSERT sy-subrc = 0.

* new commit
    ls_commit-committer = |{ is_comment-committer-name
      } <{ is_comment-committer-email }> { lv_time }|.
    IF is_comment-author-name IS NOT INITIAL.
      ls_commit-author = |{ is_comment-author-name
        } <{ is_comment-author-email }> { lv_time }|.
    ELSE.
      ls_commit-author = ls_commit-committer.
    ENDIF.

    ls_commit-tree      = <ls_tree>-sha1.
    ls_commit-parent    = iv_parent.
    ls_commit-parent2   = iv_parent2.
    ls_commit-body      = is_comment-comment.
    lv_commit = zcl_abapgit_git_pack=>encode_commit( ls_commit ).

    ls_object-sha1 = zcl_abapgit_hash=>sha1_commit( lv_commit ).
    ls_object-type = zif_abapgit_definitions=>c_type-commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO et_new_objects.

    LOOP AT it_trees ASSIGNING <ls_tree>.
      CLEAR ls_object.
      ls_object-sha1 = <ls_tree>-sha1.

      READ TABLE et_new_objects
        WITH KEY type COMPONENTS
          type = zif_abapgit_definitions=>c_type-tree
          sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical trees added at the same time, only add one to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = zif_abapgit_definitions=>c_type-tree.
      ls_object-data = <ls_tree>-data.
      lv_uindex = lv_uindex + 1.
      ls_object-index = lv_uindex.
      APPEND ls_object TO et_new_objects.
    ENDLOOP.

    LOOP AT it_blobs ASSIGNING <ls_blob>.
      CLEAR ls_object.
      ls_object-sha1 = zcl_abapgit_hash=>sha1_blob( <ls_blob>-data ).

      READ TABLE et_new_objects
        WITH KEY type COMPONENTS
          type = zif_abapgit_definitions=>c_type-blob
          sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical files added at the same time, only add one blob to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = zif_abapgit_definitions=>c_type-blob.
* note <ls_blob>-data can be empty, #1857 allow empty files - some more checks needed?
      ls_object-data = <ls_blob>-data.
      lv_uindex = lv_uindex + 1.
      ls_object-index = lv_uindex.
      APPEND ls_object TO et_new_objects.
    ENDLOOP.

    lv_pack = zcl_abapgit_git_pack=>encode( et_new_objects ).

    ev_new_commit = zcl_abapgit_hash=>sha1_commit( lv_commit ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = iv_parent
      iv_new         = ev_new_commit
      iv_branch_name = iv_branch_name
      iv_pack        = lv_pack ).

    ev_new_tree = ls_commit-tree.

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
        type = zif_abapgit_definitions=>c_type-tree
        sha1 = iv_sha1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Walk, tree not found' ).
    ENDIF.

    lt_nodes = zcl_abapgit_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = zif_abapgit_definitions=>c_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY type COMPONENTS
            type = zif_abapgit_definitions=>c_type-blob
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

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = zif_abapgit_definitions=>c_chmod-dir.
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
        type = zif_abapgit_definitions=>c_type-tree
        sha1 = iv_tree.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'tree not found' ).
    ENDIF.
    lt_nodes = zcl_abapgit_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN zif_abapgit_definitions=>c_chmod-file
            OR zif_abapgit_definitions=>c_chmod-executable
            OR zif_abapgit_definitions=>c_chmod-submodule.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path  = iv_base.
          <ls_exp>-name  = <ls_node>-name.
          <ls_exp>-sha1  = <ls_node>-sha1.
          <ls_exp>-chmod = <ls_node>-chmod.
        WHEN zif_abapgit_definitions=>c_chmod-dir.
          lt_expanded = walk_tree(
            it_objects = it_objects
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( 'walk_tree: unknown chmod' ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
