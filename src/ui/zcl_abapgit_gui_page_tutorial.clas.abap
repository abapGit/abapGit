CLASS zcl_abapgit_gui_page_tutorial DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_page
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      RAISING zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_gui_page_tutorial IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Tutorial'.
  ENDMETHOD.


  METHOD render_content.

    DATA: lv_devclass TYPE tadir-devclass.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="tutorial">' ).

    ri_html->add( '<h1>Tutorial</h1>' ).
    ri_html->add( '<hr>' ).

    ri_html->add( '<h2>Online repositories</h2>' ).
    ri_html->add( '<p><ul>' ).

    ri_html->add( `<li>To clone a remote repository (e.g. from github) click ` ).
    ri_html->add_a( iv_txt = '+ Online'
                    iv_act = zif_abapgit_definitions=>c_action-repo_newonline ).
    ri_html->add( ' from the top menu. This will link a remote repository with a package on your system.</li>' ).
    ri_html->add( '<li>Use the pull button to retrieve and activate the remote objects.</li>' ).
    ri_html->add( '<li>If the remote repository is updated,' ).
    ri_html->add( ' you will see the changes and can pull to apply the updates.</li>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '<h2>Offline repositories</h2>' ).
    ri_html->add( '<p><ul>' ).

    ri_html->add( `<li>To add a package as an offline repository, click ` ).
    ri_html->add_a( iv_txt = '+ Offline'
                    iv_act = zif_abapgit_definitions=>c_action-repo_newoffline ).
    ri_html->add( ' from the top menu.' ).
    ri_html->add( '<li>abapGit will start tracking changes for the package ' ).
    ri_html->add( 'without linking it to an online git repository.</li>' ).
    ri_html->add( '<li>You can link the package later or just export the package content as a zip file.</li>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '<h2>Repository list and favorites</h2>' ).
    ri_html->add( '<p><ul>' ).
    ri_html->add( |<li>To favorite a repository, use the {
                  zcl_abapgit_html=>icon( 'star/darkgrey' ) } icon in the repository list.</li>| ).
    ri_html->add( |<li>To go to a repository, click on the repository name.</li>| ).
    ri_html->add( |<li>To go back to your favorites, click the abapgit logo.</li>| ).

    ri_html->add( `<li>` ).
    ri_html->add_a( iv_txt = 'Explore'
                    iv_act = zif_abapgit_definitions=>c_action-go_explore ).
    ri_html->add( ' to find projects using abapGit</li>' ).


    ri_html->add( '</ul></p>' ).
    ri_html->add( '</div>' ).


  ENDMETHOD.

ENDCLASS.
