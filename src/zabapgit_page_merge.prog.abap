*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_MERGE
*&---------------------------------------------------------------------*

CLASS lcl_merge DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_ancestor,
             commit TYPE ty_sha1,
             tree   TYPE ty_sha1,
             time   TYPE string,
             body   TYPE string,
           END OF ty_ancestor.

    TYPES: BEGIN OF ty_merge,
             repo   TYPE REF TO lcl_repo_online,
             source TYPE lcl_git_transport=>ty_branch_list,
             target TYPE lcl_git_transport=>ty_branch_list,
             common TYPE ty_ancestor,
           END OF ty_merge.

    CLASS-METHODS:
      run
        IMPORTING io_repo         TYPE REF TO lcl_repo_online
                  iv_source       TYPE string
                  iv_target       TYPE string
        RETURNING VALUE(rs_merge) TYPE ty_merge
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-DATA: gs_merge   TYPE ty_merge,
                gt_objects TYPE ty_objects_tt.

    TYPES: ty_ancestor_tt TYPE STANDARD TABLE OF ty_ancestor WITH DEFAULT KEY.

    CLASS-METHODS:
      find_ancestors
        IMPORTING iv_commit           TYPE ty_sha1
        RETURNING VALUE(rt_ancestors) TYPE ty_ancestor_tt
        RAISING   lcx_exception,
      find_first_common
        IMPORTING it_list1         TYPE ty_ancestor_tt
                  it_list2         TYPE ty_ancestor_tt
        RETURNING VALUE(rs_common) TYPE ty_ancestor
        RAISING   lcx_exception,
      fetch_git
        IMPORTING iv_source TYPE string
                  iv_target TYPE string
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_merge IMPLEMENTATION.

  METHOD run.

    DATA: lt_asource TYPE ty_ancestor_tt,
          lt_atarget TYPE ty_ancestor_tt.


    ASSERT NOT iv_source = iv_target.

    CLEAR gs_merge.

    gs_merge-repo = io_repo.

    fetch_git( iv_source = iv_source
               iv_target = iv_target ).

    lt_asource = find_ancestors( gs_merge-source-sha1 ).
    lt_atarget = find_ancestors( gs_merge-target-sha1 ).

    gs_merge-common = find_first_common( it_list1 = lt_asource
                                         it_list2 = lt_atarget ).

    rs_merge = gs_merge.

  ENDMETHOD.

  METHOD find_first_common.

    FIELD-SYMBOLS: <ls_list1> LIKE LINE OF it_list1,
                   <ls_list2> LIKE LINE OF it_list2.


    LOOP AT it_list1 ASSIGNING <ls_list1>.
      LOOP AT it_list2 ASSIGNING <ls_list2>.
        IF <ls_list1>-tree = <ls_list2>-tree.
          rs_common = <ls_list1>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    _raise 'error finding common ancestor'.

  ENDMETHOD.

  METHOD find_ancestors.

    DATA: lv_commit TYPE ty_sha1,
          ls_commit TYPE lcl_git_pack=>ty_commit,
          lt_visit  TYPE STANDARD TABLE OF ty_sha1.

    FIELD-SYMBOLS: <ls_ancestor> LIKE LINE OF rt_ancestors,
                   <ls_object>   LIKE LINE OF gt_objects.

    DEFINE _visit.
      IF NOT &1 IS INITIAL.
        READ TABLE lt_visit FROM &1 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND &1 TO lt_visit.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.


    APPEND iv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE gt_objects ASSIGNING <ls_object>
        WITH KEY type = gc_type-commit sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      ls_commit = lcl_git_pack=>decode_commit( <ls_object>-data ).

      _visit ls_commit-parent.
      _visit ls_commit-parent2.

      APPEND INITIAL LINE TO rt_ancestors ASSIGNING <ls_ancestor>.
      <ls_ancestor>-commit = lv_commit.
      <ls_ancestor>-tree = ls_commit-tree.
      <ls_ancestor>-body = ls_commit-body.
      FIND REGEX '^[\w\s]+ <(.*)> (\d{10}) .\d{4}$' IN ls_commit-author
        SUBMATCHES <ls_ancestor>-time ##NO_TEXT.
      ASSERT sy-subrc = 0.
    ENDLOOP.

    SORT rt_ancestors BY time ASCENDING.

  ENDMETHOD.

  METHOD fetch_git.

    DATA: lv_name     TYPE string,
          lt_branches TYPE lcl_git_transport=>ty_branch_list_tt,
          lt_upload   TYPE lcl_git_transport=>ty_branch_list_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_upload.

    DEFINE _find.
      lv_name = 'refs/heads/' && &1.
      READ TABLE lt_branches INTO &2 WITH KEY name = lv_name.
      ASSERT sy-subrc = 0.
      APPEND &2 TO lt_upload.
    END-OF-DEFINITION.


    lt_branches = lcl_git_transport=>branches( gs_merge-repo->get_url( ) ).

    _find iv_source gs_merge-source.
    _find iv_target gs_merge-target.

    lcl_git_transport=>upload_pack( EXPORTING io_repo = gs_merge-repo
                                              iv_deepen = abap_false
                                              it_branches = lt_upload
                                    IMPORTING et_objects = gt_objects ).

    DELETE gt_objects WHERE type = gc_type-blob.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_merge DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo   TYPE REF TO lcl_repo_online
                  iv_source TYPE string
                  iv_target TYPE string
        RAISING   lcx_exception,
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          ms_merge TYPE lcl_merge=>ty_merge.

ENDCLASS.                       "lcl_gui_page_merge DEFINITION

CLASS lcl_gui_page_merge IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.

    ms_merge = lcl_merge=>run(
      io_repo   = io_repo
      iv_source = iv_source
      iv_target = iv_target ).

  ENDMETHOD.

  METHOD lif_gui_page~on_event.
    BREAK-POINT.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'MERGE' ) ).
    _add '<div id="toc">'.
    ro_html->add( render_repo_top( mo_repo ) ).

    ro_html->add( 'Source:' ).
    ro_html->add( ms_merge-source-name ).
    ro_html->add( '<br>' ).

    ro_html->add( 'Target:' ).
    ro_html->add( ms_merge-target-name ).
    ro_html->add( '<br>' ).

    ro_html->add( 'Ancestor:' ).
    ro_html->add( ms_merge-common-commit ).
    ro_html->add( '<br>' ).

    ro_html->add( 'Todo' ).
    _add '</div>'.
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.