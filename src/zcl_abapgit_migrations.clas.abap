CLASS zcl_abapgit_migrations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS rebuild_local_checksums_161112
      RAISING zcx_abapgit_exception.
    CLASS-METHODS local_dot_abapgit
      RAISING zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_migrations IMPLEMENTATION.


  METHOD local_dot_abapgit.

    DATA: lt_repos       TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lv_shown       TYPE abap_bool,
          lo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
          lv_txt1        TYPE string,
          lv_txt2        TYPE string,
          lx_exception   TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS: <lo_repo> LIKE LINE OF lt_repos.


    lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repos ASSIGNING <lo_repo>.
      lo_dot_abapgit = <lo_repo>->get_dot_abapgit( ).
      IF lo_dot_abapgit->get_data( ) IS INITIAL.
        IF <lo_repo>->is_offline( ) = abap_true.
          lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
        ELSE.
          IF lv_shown = abap_false.
            CALL FUNCTION 'POPUP_TO_INFORM'
              EXPORTING
                titel = 'Migration'
                txt1  = '.abapgit.xml is migrated to local state'
                txt2  = 'Login to remote repositories if needed'.
            lv_shown = abap_true.
          ENDIF.

          " Skip repos that cannot be fetched.
          " Particuarly useful on systems where users do not allow
          " everybody to fetch their repos.
          TRY.
              <lo_repo>->refresh( ).
            CATCH zcx_abapgit_exception INTO lx_exception.
              lv_txt1 = lx_exception->get_text( ).
              lv_txt2 = |Please do not use the "{ <lo_repo>->get_name( ) }" repository until migrated|.
              CALL FUNCTION 'POPUP_TO_INFORM'
                EXPORTING
                  titel = 'Migration has failed'
                  txt1  = lv_txt1
                  txt2  = lv_txt2
                  txt3  = 'You will be prompted to migrate the repository every time you run abapGit.'
                  txt4  = 'You can safely remove the repository in its ''Advanced -> Remove'' menu.'.
              CONTINUE.
          ENDTRY.

          lo_dot_abapgit = <lo_repo>->find_remote_dot_abapgit( ).
          IF lo_dot_abapgit IS INITIAL. " .abapgit.xml is not in the remote repo yet
            lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
          ENDIF.
        ENDIF.
        <lo_repo>->set_dot_abapgit( lo_dot_abapgit ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD rebuild_local_checksums_161112.

    DATA: lt_repos     TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lv_repo_list TYPE string,
          lv_question  TYPE string,
          lv_answer    TYPE c,
          lv_index     TYPE i,
          lo_repo      TYPE REF TO zcl_abapgit_repo_online.

    FIELD-SYMBOLS: <lo_repo> LIKE LINE OF lt_repos.


    lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repos ASSIGNING <lo_repo>.
      lv_index = sy-tabix.

      IF <lo_repo>->is_offline( ) = abap_true. " Skip local repos
        DELETE lt_repos INDEX lv_index.
        CONTINUE.
      ENDIF.

      " Ignore empty repos or repos with file checksums
      IF lines( <lo_repo>->get_local_checksums( ) ) = 0
          OR lines( <lo_repo>->get_local_checksums_per_file( ) ) > 0.
        DELETE lt_repos INDEX lv_index.
        CONTINUE.
      ENDIF.

      lv_repo_list = lv_repo_list && `, ` && <lo_repo>->get_name( ).

    ENDLOOP.

    IF lines( lt_repos ) = 0.
      RETURN. " All OK
    ENDIF.

    SHIFT lv_repo_list BY 2 PLACES LEFT. " Remove leading ', '

    lv_question = 'abapGit wants to rebuild missing local checksums'
               && ' (changes from 2016-12-11).'
               && ' Generally this is safe except if there are both local '
               && ' and remote changes at the same time. If unsure, please'
               && ' skip and update repos individually'
               && ' by "Advances/Update local checksums" command.'
               && | Repos affected: { lv_repo_list }|.

    lv_answer = zcl_abapgit_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = lv_question
      text_button_1         = 'OK'
      icon_button_1         = 'ICON_OK'
      text_button_2         = 'Skip update'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    LOOP AT lt_repos ASSIGNING <lo_repo>.
      lo_repo ?= <lo_repo>.
      lo_repo->rebuild_local_checksums( ).
    ENDLOOP.

  ENDMETHOD.  " rebuild_local_checksums_20161112.


  METHOD run.

    " Migrate STDTEXT to TABLE
    zcl_abapgit_persist_migrate=>run( ).

    " Rebuild local file checksums
    rebuild_local_checksums_161112( ).

    " local .abapgit.xml state, issue #630
    local_dot_abapgit( ).

  ENDMETHOD.  " run.
ENDCLASS.
