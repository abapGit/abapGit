CLASS zcl_abapgit_gui_view_tutorial DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_page.
    ALIASES render FOR zif_abapgit_gui_page~render.

  PRIVATE SECTION.
    METHODS render_content
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_VIEW_TUTORIAL IMPLEMENTATION.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<h1>Tutorial</h1>' ).
    ro_html->add( '<hr>' ).

    ro_html->add( '<h2>Adding and cloning repos</h2>' ).
    ro_html->add( '<p><ul>' ).

    ro_html->add( `<li>To clone a remote repo (e.g. from github) click ` ).
    ro_html->add_a( iv_txt = '+ Online' iv_act = zif_abapgit_definitions=>gc_action-repo_newonline ).
    ro_html->add( ' from the top menu. This will copy a remote repo to your system.</li>' ).

    ro_html->add( `<li>To add a local package as a repo click ` ).
    ro_html->add_a( iv_txt = '+ Offline' iv_act = zif_abapgit_definitions=>gc_action-repo_newoffline ).
    ro_html->add( ' from the top menu. This will track a repo which already exist in' ).
    ro_html->add( ' the system with abapGit. You''ll be able to attach it to remote origin' ).
    ro_html->add( ' or just serialize as a zip file</li>' ).

    ro_html->add( `<li>Go ` ).
    ro_html->add_a( iv_txt = 'Explore' iv_act = zif_abapgit_definitions=>gc_action-go_explore ).
    ro_html->add( ' to find projects using abapGit</li>' ).

    ro_html->add( '</ul></p>' ).

    ro_html->add( '<h2>Repository list and favorites</h2>' ).
    ro_html->add( '<p><ul>' ).
    ro_html->add( |<li>To choose a repo press {
                  zcl_abapgit_html=>icon( 'three-bars/blue' ) } at the favorite bar.</li>| ).
    ro_html->add( |<li>To favorite a repo click {
                  zcl_abapgit_html=>icon( 'star/darkgrey' ) } icon at repo toolbar.</li>| ).
    ro_html->add( '</ul></p>' ).

    ro_html->add( '<h2>abapGit related repositories</h2>' ).
    ro_html->add( '<p><ul>' ).
    ro_html->add( '<li>' ).
    IF zcl_abapgit_services_abapgit=>is_installed( ) = abap_true.
      ro_html->add( 'abapGit installed in package&nbsp;' ).
      ro_html->add( zcl_abapgit_services_abapgit=>c_package_abapgit ).
    ELSE.
      ro_html->add_a( iv_txt = 'install abapGit repo' iv_act = zif_abapgit_definitions=>gc_action-abapgit_install ).
      ro_html->add( ' - To keep abapGit up-to-date (or also to contribute) you need to' ).
      ro_html->add( 'install it as a repository.' ).
    ENDIF.
    ro_html->add( '</li>' ).
    ro_html->add( '<li>' ).
    IF zcl_abapgit_services_abapgit=>is_installed_pi( ) = abap_true.
      ro_html->add( 'abapGit plugins installed in package&nbsp;' ).
      ro_html->add( zcl_abapgit_services_abapgit=>c_package_plugins ).
    ELSE.
      ro_html->add_a( iv_txt = 'install abapGit plugins'
                      iv_act = zif_abapgit_definitions=>gc_action-abapgit_install_pi ).
      ro_html->add( ' - you can also install plugins to extend supported object types' ).
    ENDIF.
    ro_html->add( '</li>' ).
    ro_html->add( '</ul></p>' ).

  ENDMETHOD. " render_content.


  METHOD zif_abapgit_gui_page~on_event.
    ev_state = zif_abapgit_definitions=>gc_event_state-not_handled.
  ENDMETHOD.  " lif_gui_page~on_event.


  METHOD zif_abapgit_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="tutorial">' ).
    ro_html->add( render_content( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "lif_gui_page~render
ENDCLASS.
