*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_TUTORIAL
*&---------------------------------------------------------------------*

CLASS lcl_gui_view_tutorial DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    METHODS render_content
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.                       "lcl_gui_view_tutorial

CLASS lcl_gui_view_tutorial IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="tutorial">' ).
    ro_html->add( render_content( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "lif_gui_page~render

  METHOD render_content.

    CREATE OBJECT ro_html.

    _add '<h1>Tutorial</h1>'.
    _add '<hr>'.

    _add '<h2>Adding and cloning repos</h2>'.
    _add '<p><ul>'.
    _add `<li>To clone a remote repo (e.g. from github) click `.
    ro_html->add_anchor( iv_txt = '+ Clone' iv_act = gc_action-repo_clone ).
    _add ' from the top menu. This will copy a remote repo to your system.</li>'.
    _add `<li>To add a local package as a repo click `.
    ro_html->add_anchor( iv_txt = '+ Offline' iv_act = gc_action-repo_newoffline ).
    _add ' from the top menu. This will track a repo which already exist in'.
    _add ' the system with abapGit. You''ll be able to attach it to remote origin'.
    _add ' or just serialize as a zip file</li>'.
    _add '</ul></p>'.

    _add '<h2>Repository list and favorites</h2>'.
    _add '<p><ul>'.
    _add '<li>To choose a repo press <img src="img/burger"> at the favorite bar.</li>'.
    _add '<li>To favorite a repo click <img src="img/star-grey"> icon at repo toolbar.</li>'.
    _add '</ul></p>'.

    _add '<h2>abapGit related repositories</h2>'.
    _add '<p><ul>'.
    _add '<li>'.
    ro_html->add_anchor( iv_txt = 'install abapGit repo' iv_act = gc_action-abapgit_install ).
    _add ' - To keep abapGit up-to-date (or also to contribute) you need to'.
    _add 'install it as a repository.</li>'.
    _add '<li>'.
    ro_html->add_anchor( iv_txt = 'install abapGit plugins' iv_act = gc_action-abapgit_install_pi ).
    _add ' - you can also install plugins to extend supported object types</li>'.
    _add '</ul></p>'.

  ENDMETHOD. " render_content.

ENDCLASS.                       "lcl_gui_view_tutorial