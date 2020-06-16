CLASS zcl_abapgit_gui_view_tutorial DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS render_content
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

ENDCLASS.



CLASS zcl_abapgit_gui_view_tutorial IMPLEMENTATION.


  METHOD render_content.

    DATA: lv_devclass TYPE tadir-devclass.

    CREATE OBJECT ro_html.

    ro_html->add( '<h1>Tutorial</h1>' ).
    ro_html->add( '<hr>' ).

    ro_html->add( '<h2>Adding and cloning repos</h2>' ).
    ro_html->add( '<p><ul>' ).

    ro_html->add( `<li>To clone a remote repo (e.g. from github) click ` ).
    ro_html->add_a( iv_txt = '+ Online'
                    iv_act = zif_abapgit_definitions=>c_action-repo_newonline ).
    ro_html->add( ' from the top menu. This will copy a remote repo to your system.</li>' ).

    ro_html->add( `<li>To add a local package as a repo click ` ).
    ro_html->add_a( iv_txt = '+ Offline'
                    iv_act = zif_abapgit_definitions=>c_action-repo_newoffline ).
    ro_html->add( ' from the top menu. This will track a repo which already exist in' ).
    ro_html->add( ' the system with abapGit. You''ll be able to attach it to remote origin' ).
    ro_html->add( ' or just serialize as a zip file</li>' ).

    ro_html->add( `<li>Go ` ).
    ro_html->add_a( iv_txt = 'Explore'
                    iv_act = zif_abapgit_definitions=>c_action-go_explore ).
    ro_html->add( ' to find projects using abapGit</li>' ).

    ro_html->add( '</ul></p>' ).

    ro_html->add( '<h2>Repository list and favorites</h2>' ).
    ro_html->add( '<p><ul>' ).
    ro_html->add( |<li>To favorite a repository, click {
                  zcl_abapgit_html=>icon( 'star/darkgrey' ) } icon at repo toolbar.</li>| ).
    ro_html->add( |<li>To go to a repository, click on the repository name.</li>| ).
    ro_html->add( |<li>To go back to your favorites, click the abapgit logo.</li>| ).
    ro_html->add( '</ul></p>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="tutorial">' ).
    ri_html->add( render_content( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
