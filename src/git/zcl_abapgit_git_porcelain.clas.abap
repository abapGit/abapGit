CLASS zcl_abapgit_git_porcelain DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS pull
      IMPORTING
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
      EXPORTING
        !et_files       TYPE zif_abapgit_definitions=>ty_files_tt
        !et_objects     TYPE zif_abapgit_definitions=>ty_objects_tt
        !ev_branch      TYPE zif_abapgit_definitions=>ty_sha1
        !eo_branch_list TYPE REF TO zcl_abapgit_git_branch_list
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS push
      IMPORTING
        !io_repo          TYPE REF TO zcl_abapgit_repo_online
        !is_comment       TYPE zif_abapgit_definitions=>ty_comment
        !io_stage         TYPE REF TO zcl_abapgit_stage
      EXPORTING
        !ev_branch        TYPE zif_abapgit_definitions=>ty_sha1
        !et_updated_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_branch
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo_online
        !iv_name TYPE string
        iv_from  TYPE zif_abapgit_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_tag
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo_online
        is_tag   TYPE zif_abapgit_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_branch
      IMPORTING
        !io_repo   TYPE REF TO zcl_abapgit_repo_online
        !is_branch TYPE zif_abapgit_definitions=>ty_git_branch
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_tag
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo_online
        !is_tag  TYPE zif_abapgit_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS full_tree
      IMPORTING
        !it_objects        TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_branch         TYPE zif_abapgit_definitions=>ty_sha1
      RETURNING
        VALUE(rt_expanded) TYPE zif_abapgit_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tree,
             path TYPE string,
             data TYPE xstring,
             sha1 TYPE zif_abapgit_definitions=>ty_sha1,
           END OF ty_tree.

    TYPES: ty_trees_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_folder,
             path  TYPE string,
             count TYPE i,
             sha1  TYPE zif_abapgit_definitions=>ty_sha1,
           END OF ty_folder.

    TYPES: ty_folders_tt TYPE STANDARD TABLE OF ty_folder WITH DEFAULT KEY.

    CONSTANTS: c_zero TYPE zif_abapgit_definitions=>ty_sha1 VALUE '0000000000000000000000000000000000000000'.

    CLASS-METHODS build_trees
      IMPORTING it_expanded     TYPE zif_abapgit_definitions=>ty_expanded_tt
      RETURNING VALUE(rt_trees) TYPE ty_trees_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS find_folders
      IMPORTING it_expanded       TYPE zif_abapgit_definitions=>ty_expanded_tt
      RETURNING VALUE(rt_folders) TYPE ty_folders_tt.

    CLASS-METHODS walk
      IMPORTING it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
                iv_sha1    TYPE zif_abapgit_definitions=>ty_sha1
                iv_path    TYPE string
      CHANGING  ct_files   TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS walk_tree
      IMPORTING it_objects         TYPE zif_abapgit_definitions=>ty_objects_tt
                iv_tree            TYPE zif_abapgit_definitions=>ty_sha1
                iv_base            TYPE string
      RETURNING VALUE(rt_expanded) TYPE zif_abapgit_definitions=>ty_expanded_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS receive_pack_push
      IMPORTING is_comment       TYPE zif_abapgit_definitions=>ty_comment
                io_repo          TYPE REF TO zcl_abapgit_repo_online
                it_trees         TYPE ty_trees_tt
                it_blobs         TYPE zif_abapgit_definitions=>ty_files_tt
                io_stage         TYPE REF TO zcl_abapgit_stage
      RETURNING VALUE(rv_branch) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS receive_pack_create_tag
      IMPORTING is_tag  TYPE zif_abapgit_definitions=>ty_git_tag
                io_repo TYPE REF TO zcl_abapgit_repo_online
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS create_annotated_tag
      IMPORTING
        is_tag  TYPE zif_abapgit_definitions=>ty_git_tag
        io_repo TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS create_lightweight_tag
      IMPORTING
        is_tag  TYPE zif_abapgit_definitions=>ty_git_tag
        io_repo TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception.

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
          <ls_node>-chmod = zif_abapgit_definitions=>gc_chmod-dir.

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
      ls_tree-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-tree iv_data = ls_tree-data ).
      APPEND ls_tree TO rt_trees.

      <ls_folder>-sha1 = ls_tree-sha1.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_branch.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack    TYPE xstring.

    IF iv_name CS ` `.
      zcx_abapgit_exception=>raise( 'Branch name cannot contain blank spaces' ).
    ENDIF.

* "client MUST send an empty packfile"
* https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt#L514
    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = io_repo->get_url( )
      iv_old         = c_zero
      iv_new         = iv_from
      iv_branch_name = iv_name
      iv_pack        = lv_pack ).

  ENDMETHOD.


  METHOD create_tag.

    IF is_tag-name CS ` `.
      zcx_abapgit_exception=>raise( 'Tag name cannot contain blank spaces' ).
    ENDIF.

    CASE is_tag-type.
      WHEN zif_abapgit_definitions=>c_git_branch_type-annotated_tag.

        create_annotated_tag(
            is_tag  = is_tag
            io_repo = io_repo ).

      WHEN  zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.

        create_lightweight_tag(
            is_tag  = is_tag
            io_repo = io_repo ).

      WHEN OTHERS.

        zcx_abapgit_exception=>raise( |Invalid tag type: { is_tag-type }| ).

    ENDCASE.

  ENDMETHOD.


  METHOD delete_branch.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack    TYPE xstring.


* "client MUST send an empty packfile"
* https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt#L514
    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = io_repo->get_url( )
      iv_old         = is_branch-sha1
      iv_new         = c_zero
      iv_branch_name = is_branch-name
      iv_pack        = lv_pack ).

  ENDMETHOD.


  METHOD delete_tag.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack    TYPE xstring.


* "client MUST send an empty packfile"
* https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt#L514
    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = io_repo->get_url( )
      iv_old         = is_tag-sha1
      iv_new         = c_zero
      iv_branch_name = is_tag-name
      iv_pack        = lv_pack ).

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


    READ TABLE it_objects INTO ls_object WITH KEY sha1 = iv_branch type = zif_abapgit_definitions=>gc_type-commit.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'commit not found' ).
    ENDIF.
    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.                    "root_tree


  METHOD pull.

    DATA: ls_object LIKE LINE OF et_objects,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.


    CLEAR et_files.
    CLEAR et_objects.
    CLEAR ev_branch.
    CLEAR eo_branch_list.

    zcl_abapgit_git_transport=>upload_pack(
      EXPORTING
        iv_url         = io_repo->get_url( )
        iv_branch_name = io_repo->get_branch_name( )
      IMPORTING
        et_objects     = et_objects
        ev_branch      = ev_branch
        eo_branch_list = eo_branch_list ).

    READ TABLE et_objects INTO ls_object WITH KEY sha1 = ev_branch type = zif_abapgit_definitions=>gc_type-commit.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Commit/branch not found' ).
    ENDIF.
    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = et_objects
                    iv_sha1 = ls_commit-tree
                    iv_path = '/'
          CHANGING ct_files = et_files ).

  ENDMETHOD.                    "pull


  METHOD push.

    DATA: lt_expanded TYPE zif_abapgit_definitions=>ty_expanded_tt,
          lt_blobs    TYPE zif_abapgit_definitions=>ty_files_tt,
          lv_sha1     TYPE zif_abapgit_definitions=>ty_sha1,
          lt_trees    TYPE ty_trees_tt,
          lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lt_branches TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
          lt_stage    TYPE zcl_abapgit_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage>   LIKE LINE OF lt_stage,
                   <ls_updated> LIKE LINE OF et_updated_files,
                   <ls_branch>  LIKE LINE OF lt_branches,
                   <ls_exp>     LIKE LINE OF lt_expanded.


    CLEAR et_updated_files.

    IF io_stage->get_branch_sha1( ) = io_repo->get_sha1_remote( ).
* objects cached in io_repo can be used, if pushing to the branch configured in repo
      lt_objects = io_repo->get_objects( ).
    ELSE.
      APPEND INITIAL LINE TO lt_branches ASSIGNING <ls_branch>.
      <ls_branch>-name = io_stage->get_branch_name( ).
      <ls_branch>-sha1 = io_stage->get_branch_sha1( ).

      zcl_abapgit_git_transport=>upload_pack(
        EXPORTING
          iv_url         = io_repo->get_url( )
          iv_branch_name = io_repo->get_branch_name( )
          it_branches    = lt_branches
        IMPORTING
          et_objects     = lt_objects ).
    ENDIF.

    lt_expanded = full_tree( it_objects = lt_objects
                             iv_branch  = io_stage->get_branch_sha1( ) ).

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.

      " Save file ref to updated files table
      APPEND INITIAL LINE TO et_updated_files ASSIGNING <ls_updated>.
      MOVE-CORRESPONDING <ls_stage>-file TO <ls_updated>.

      CASE <ls_stage>-method.
        WHEN zcl_abapgit_stage=>c_method-add.

          APPEND <ls_stage>-file TO lt_blobs.

          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH KEY
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          IF sy-subrc <> 0. " new files
            APPEND INITIAL LINE TO lt_expanded ASSIGNING <ls_exp>.
            <ls_exp>-name  = <ls_stage>-file-filename.
            <ls_exp>-path  = <ls_stage>-file-path.
            <ls_exp>-chmod = zif_abapgit_definitions=>gc_chmod-file.
          ENDIF.

          lv_sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob
                                            iv_data = <ls_stage>-file-data ).
          IF <ls_exp>-sha1 <> lv_sha1.
            <ls_exp>-sha1 = lv_sha1.
          ENDIF.

          <ls_updated>-sha1 = lv_sha1.   "New sha1

        WHEN zcl_abapgit_stage=>c_method-rm.
          DELETE lt_expanded
            WHERE name = <ls_stage>-file-filename
            AND   path = <ls_stage>-file-path.
          ASSERT sy-subrc = 0.

          CLEAR <ls_updated>-sha1.       " Mark as deleted

        WHEN OTHERS.
          zcx_abapgit_exception=>raise( 'stage method not supported, todo' ).
      ENDCASE.
    ENDLOOP.

    lt_trees = build_trees( lt_expanded ).

    ev_branch = receive_pack_push( is_comment = is_comment
                                   io_repo    = io_repo
                                   it_trees   = lt_trees
                                   it_blobs   = lt_blobs
                                   io_stage   = io_stage ).

  ENDMETHOD.                    "push


  METHOD receive_pack_create_tag.

    DATA: lv_tag          TYPE xstring,
          lt_objects      TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack         TYPE xstring,
          ls_object       LIKE LINE OF lt_objects,
          ls_tag          TYPE zcl_abapgit_git_pack=>ty_tag,
          lv_new_tag_sha1 TYPE zif_abapgit_definitions=>ty_sha1.

* new tag
    ls_tag-object       = is_tag-sha1.
    ls_tag-type         = zif_abapgit_definitions=>gc_type-commit.
    ls_tag-tag          = is_tag-name.
    ls_tag-tagger_name  = is_tag-tagger_name.
    ls_tag-tagger_email = is_tag-tagger_email.
    ls_tag-message      = is_tag-message
                      && |{ zif_abapgit_definitions=>gc_newline }|
                      && |{ zif_abapgit_definitions=>gc_newline }|
                      && is_tag-body.

    lv_tag = zcl_abapgit_git_pack=>encode_tag( ls_tag ).

    lv_new_tag_sha1 = zcl_abapgit_hash=>sha1(
      iv_type = zif_abapgit_definitions=>gc_type-tag
      iv_data = lv_tag ).

    CLEAR ls_object.
    ls_object-sha1 = lv_new_tag_sha1.
    ls_object-type = zif_abapgit_definitions=>gc_type-tag.
    ls_object-data = lv_tag.
    APPEND ls_object TO lt_objects.

    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = io_repo->get_url( )
      iv_old         = c_zero
      iv_new         = lv_new_tag_sha1
      iv_branch_name = is_tag-name
      iv_pack        = lv_pack ).

  ENDMETHOD.


  METHOD receive_pack_push.

    DATA: lv_time    TYPE zcl_abapgit_time=>ty_unixtime,
          lv_commit  TYPE xstring,
          lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack    TYPE xstring,
          lt_files   TYPE zif_abapgit_definitions=>ty_files_tt,
          ls_object  LIKE LINE OF lt_objects,
          ls_commit  TYPE zcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_trees,
                   <ls_blob> LIKE LINE OF it_blobs.


    lv_time = zcl_abapgit_time=>get( ).

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
    ls_commit-parent    = io_stage->get_branch_sha1( ).
    ls_commit-parent2   = io_stage->get_merge_source( ).
    ls_commit-body      = is_comment-comment.
    lv_commit = zcl_abapgit_git_pack=>encode_commit( ls_commit ).

    CLEAR ls_object.
    ls_object-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-commit iv_data = lv_commit ).
    ls_object-type = zif_abapgit_definitions=>gc_type-commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO lt_objects.

    LOOP AT it_trees ASSIGNING <ls_tree>.
      CLEAR ls_object.
      ls_object-sha1 = <ls_tree>-sha1.

      READ TABLE lt_objects WITH KEY type = zif_abapgit_definitions=>gc_type-tree sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical trees added at the same time, only add one to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = zif_abapgit_definitions=>gc_type-tree.
      ls_object-data = <ls_tree>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    LOOP AT it_blobs ASSIGNING <ls_blob>.
      CLEAR ls_object.
      ls_object-sha1 = zcl_abapgit_hash=>sha1(
        iv_type = zif_abapgit_definitions=>gc_type-blob
        iv_data = <ls_blob>-data ).

      READ TABLE lt_objects WITH KEY type = zif_abapgit_definitions=>gc_type-blob sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical files added at the same time, only add one blob to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = zif_abapgit_definitions=>gc_type-blob.
      ASSERT NOT <ls_blob>-data IS INITIAL.
      ls_object-data = <ls_blob>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    rv_branch = zcl_abapgit_hash=>sha1(
      iv_type = zif_abapgit_definitions=>gc_type-commit
      iv_data = lv_commit ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = io_repo->get_url( )
      iv_old         = io_stage->get_branch_sha1( )
      iv_new         = rv_branch
      iv_branch_name = io_stage->get_branch_name( )
      iv_pack        = lv_pack ).

* update objects in repo, we know what has been pushed
    APPEND LINES OF io_repo->get_objects( ) TO lt_objects.
    io_repo->set_objects( lt_objects ).
    walk( EXPORTING it_objects = lt_objects
                    iv_sha1 = ls_commit-tree
                    iv_path = '/'
          CHANGING ct_files = lt_files ).
    io_repo->set_files_remote( lt_files ).

  ENDMETHOD.                    "receive_pack


  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree> WITH KEY sha1 = iv_sha1 type = zif_abapgit_definitions=>gc_type-tree.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Walk, tree not found' ).
    ENDIF.

    lt_nodes = zcl_abapgit_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = zif_abapgit_definitions=>gc_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY sha1 = <ls_node>-sha1 type = zif_abapgit_definitions=>gc_type-blob.
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

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = zif_abapgit_definitions=>gc_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.
      walk( EXPORTING it_objects = it_objects
                      iv_sha1 = <ls_node>-sha1
                      iv_path = lv_path
            CHANGING ct_files = ct_files ).
    ENDLOOP.

  ENDMETHOD.                    "walk


  METHOD walk_tree.

    DATA: ls_object   LIKE LINE OF it_objects,
          lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE zcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects INTO ls_object
      WITH KEY sha1 = iv_tree
      type = zif_abapgit_definitions=>gc_type-tree.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'tree not found' ).
    ENDIF.
    lt_nodes = zcl_abapgit_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN zif_abapgit_definitions=>gc_chmod-file
            OR zif_abapgit_definitions=>gc_chmod-executable.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path  = iv_base.
          <ls_exp>-name  = <ls_node>-name.
          <ls_exp>-sha1  = <ls_node>-sha1.
          <ls_exp>-chmod = <ls_node>-chmod.
        WHEN zif_abapgit_definitions=>gc_chmod-dir.
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

  METHOD create_annotated_tag.

    receive_pack_create_tag(
        is_tag  = is_tag
        io_repo = io_repo ).

  ENDMETHOD.


  METHOD create_lightweight_tag.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack    TYPE xstring.

* "client MUST send an empty packfile"
* https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt#L514
    lv_pack = zcl_abapgit_git_pack=>encode( lt_objects ).

    zcl_abapgit_git_transport=>receive_pack(
      iv_url         = io_repo->get_url( )
      iv_old         = c_zero
      iv_new         = is_tag-sha1
      iv_branch_name = is_tag-name
      iv_pack        = lv_pack ).

  ENDMETHOD.

ENDCLASS.
