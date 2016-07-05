*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_BRANCH_OVERVIEW
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_branch_overview DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo     TYPE REF TO lcl_repo_online,
          mt_branches TYPE lcl_git_transport=>ty_branch_list_tt,
          mt_objects  TYPE ty_objects_tt.

    METHODS:
      body
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      get_git_objects
        RAISING lcx_exception.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_branch_overview IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
  ENDMETHOD.

  METHOD get_git_objects.

    lcl_progress=>show( iv_key     = 'Get git objects'
                        iv_current = 1
                        iv_total   = 1
                        iv_text    = mo_repo->get_name( ) ) ##NO_TEXT.

* get objects directly from git, mo_repo only contains a shallow clone of only
* the selected branch

* todo, memory use?
    mt_branches = lcl_git_transport=>branches( mo_repo->get_url( ) ).

    lcl_git_transport=>upload_pack( EXPORTING io_repo = mo_repo
                                              iv_deepen = abap_false
                                              it_branches = mt_branches
                                    IMPORTING et_objects = mt_objects ).


  ENDMETHOD.

  METHOD body.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF mt_branches.


    CREATE OBJECT ro_html.

    get_git_objects( ).
    LOOP AT mt_branches ASSIGNING <ls_branch>.
      ro_html->add( |{ <ls_branch>-sha1 } { <ls_branch>-name }<br>| ).
    ENDLOOP.

    ro_html->add( |Object count: { lines( mt_objects ) }<br>| ).

    _add '<br>'.
    _add 'todo, see https://github.com/larshp/abapGit/issues/272'.
    _add '<br>'.
    _add '<svg width="100" height="100">'.
    _add '<circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />'.
    _add '</svg>'.
    _add '<br>'.
    _add '<canvas id="myCanvas" width="200" height="100" style="border:1px solid #000000;">'.
    _add '</canvas>'.
    _add '<script>'.
    _add 'var c = document.getElementById("myCanvas");'.
    _add 'var ctx = c.getContext("2d");'.
    _add 'ctx.moveTo(0,0);'.
    _add 'ctx.lineTo(200,100);'.
    _add 'ctx.stroke();'.
    _add '</script>'.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BRANCH_OVERVIEW' ) ).
    ro_html->add( body( ) ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.