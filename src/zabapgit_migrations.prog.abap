*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_MIGRATIONS
*&---------------------------------------------------------------------*

CLASS lcl_migrations DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS run
      RAISING lcx_exception.
    CLASS-METHODS rebuild_local_checksums_161112
      RAISING lcx_exception.

ENDCLASS. "lcl_migrations

CLASS lcl_migrations IMPLEMENTATION.

  METHOD run.
    " Migrate STDTEXT to TABLE
    lcl_persistence_migrate=>run( ).

    " Rebuild local file checksums
    rebuild_local_checksums_161112( ).

  ENDMETHOD.  " run.

  METHOD rebuild_local_checksums_161112.

    DATA: lt_repos     TYPE lcl_repo_srv=>ty_repo_tt,
          lv_repo_list TYPE string,
          lv_question  TYPE string,
          lv_answer    TYPE c,
          lv_index     TYPE i,
          lo_repo      TYPE REF TO lcl_repo_online.

    FIELD-SYMBOLS: <repo> LIKE LINE OF lt_repos.

    lt_repos = lcl_app=>repo_srv( )->list( ).

    LOOP AT lt_repos ASSIGNING <repo>.
      lv_index = sy-tabix.

      IF <repo>->is_offline( ) = abap_true. " Skip local repos
        DELETE lt_repos INDEX lv_index.
        CONTINUE.
      ENDIF.

      " Ignore empty repos or repos with file checksums
      IF lines( <repo>->get_local_checksums( ) ) = 0
          OR lines( <repo>->get_local_checksums_per_file( ) ) > 0.
        DELETE lt_repos INDEX lv_index.
        CONTINUE.
      ENDIF.

      lv_repo_list = lv_repo_list && `, ` && <repo>->get_name( ).

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

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = lv_question
      text_button_1         = 'OK'
      icon_button_1         = 'ICON_OK'
      text_button_2         = 'Skip update'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).               "#EC NOTEXT

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    LOOP AT lt_repos ASSIGNING <repo>.
      lo_repo ?= <repo>.
      lo_repo->rebuild_local_checksums( ).
    ENDLOOP.

  ENDMETHOD.  " rebuild_local_checksums_20161112.

ENDCLASS. "lcl_migrations
