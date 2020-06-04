CLASS zcl_abapgit_services_git DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS pull
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS reset
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS create_branch
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS switch_branch
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS delete_branch
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS delete_tag
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS switch_tag
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS tag_overview
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS commit
      IMPORTING
        !io_repo   TYPE REF TO zcl_abapgit_repo_online
        !is_commit TYPE zif_abapgit_services_git=>ty_commit_fields
        !io_stage  TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_commit_value_tab,
             sha1     TYPE zif_abapgit_definitions=>ty_sha1,
             message  TYPE c LENGTH 50,
             datetime TYPE c LENGTH 20,
           END OF ty_commit_value_tab.
    TYPES: ty_commit_value_tab_tt TYPE STANDARD TABLE OF ty_commit_value_tab WITH DEFAULT KEY .

    CLASS-METHODS get_unnecessary_local_objs
      IMPORTING
        !io_repo                            TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rt_unnecessary_local_objects) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    CLASS-METHODS checkout_commit_build_popup
      IMPORTING
        !it_commits         TYPE zif_abapgit_definitions=>ty_commit_tt
      EXPORTING
        !es_selected_commit TYPE zif_abapgit_definitions=>ty_commit
      CHANGING
        !ct_value_tab       TYPE ty_commit_value_tab_tt
      RAISING
        zcx_abapgit_exception .

ENDCLASS.



CLASS zcl_abapgit_services_git IMPLEMENTATION.


  METHOD checkout_commit_build_popup.

    DATA: lt_columns         TYPE zif_abapgit_definitions=>ty_alv_column_tt,
          li_popups          TYPE REF TO zif_abapgit_popups,
          lt_selected_values TYPE ty_commit_value_tab_tt.

    FIELD-SYMBOLS: <ls_value_tab> TYPE ty_commit_value_tab,
                   <ls_column>    TYPE zif_abapgit_definitions=>ty_alv_column.

    CLEAR: es_selected_commit.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'SHA1'.
    <ls_column>-text   = 'Hash'.
    <ls_column>-length = 7.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'MESSAGE'.
    <ls_column>-text = 'Message'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DATETIME'.
    <ls_column>-text = 'Datetime'.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_value_tab
        iv_title              = |Checkout Commit|
        iv_end_column         = 83
        iv_striped_pattern    = abap_true
        iv_optimize_col_width = abap_false
        iv_selection_mode     = if_salv_c_selection_mode=>single
        it_columns_to_display = lt_columns
      IMPORTING
        et_list              = lt_selected_values ).

    IF lt_selected_values IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    READ TABLE lt_selected_values
      ASSIGNING <ls_value_tab>
      INDEX 1.

    IF <ls_value_tab> IS NOT ASSIGNED.
      zcx_abapgit_exception=>raise( |Though result set of popup wasn't empty selected value couldn't retrieved.| ).
    ENDIF.

    READ TABLE it_commits
      INTO es_selected_commit
      WITH KEY sha1 = <ls_value_tab>-sha1.

  ENDMETHOD.


  METHOD commit.

    DATA: ls_comment TYPE zif_abapgit_definitions=>ty_comment,
          li_user    TYPE REF TO zif_abapgit_persist_user.

    li_user = zcl_abapgit_persistence_user=>get_instance( ).
    li_user->set_repo_git_user_name( iv_url      = io_repo->get_url( )
                                     iv_username = is_commit-committer_name ).
    li_user->set_repo_git_user_email( iv_url     = io_repo->get_url( )
                                      iv_email   = is_commit-committer_email ).

    IF is_commit-committer_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: Committer name empty' ).
    ELSEIF is_commit-committer_email IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: Committer email empty' ).
    ELSEIF is_commit-author_email IS NOT INITIAL AND is_commit-author_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: Author name empty' ). " Opposite should be OK ?
    ELSEIF is_commit-comment IS INITIAL.
      zcx_abapgit_exception=>raise( 'Commit: empty comment' ).
    ENDIF.

    ls_comment-committer-name  = is_commit-committer_name.
    ls_comment-committer-email = is_commit-committer_email.
    ls_comment-author-name     = is_commit-author_name.
    ls_comment-author-email    = is_commit-author_email.
    ls_comment-comment         = is_commit-comment.

    IF NOT is_commit-body IS INITIAL.
      CONCATENATE ls_comment-comment '' is_commit-body
        INTO ls_comment-comment SEPARATED BY zif_abapgit_definitions=>c_newline.
    ENDIF.

    io_repo->push( is_comment = ls_comment
                   io_stage   = io_stage ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          lv_msg    TYPE string,
          li_popups TYPE REF TO zif_abapgit_popups.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->create_branch_popup(
      IMPORTING
        ev_name   = lv_name
        ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo->create_branch( lv_name ).

    lv_msg = |Switched to new branch { zcl_abapgit_git_branch_list=>get_display_name( lv_name ) }|.
    MESSAGE lv_msg TYPE 'S' ##NO_TEXT.

  ENDMETHOD.


  METHOD delete_branch.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          ls_branch TYPE zif_abapgit_definitions=>ty_git_branch,
          lv_msg    TYPE string,
          li_popups TYPE REF TO zif_abapgit_popups.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    ls_branch = li_popups->branch_list_popup( iv_url         = lo_repo->get_url( )
                                              iv_hide_branch = lo_repo->get_branch_name( )
                                              iv_hide_head   = abap_true ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_git_porcelain=>delete_branch(
      iv_url    = lo_repo->get_url( )
      is_branch = ls_branch ).

    lv_msg = |Branch { ls_branch-display_name } deleted|.
    MESSAGE lv_msg TYPE 'S'.

  ENDMETHOD.


  METHOD delete_tag.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          ls_tag  TYPE zif_abapgit_definitions=>ty_git_tag,
          lv_text TYPE string.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_tag = zcl_abapgit_ui_factory=>get_tag_popups( )->tag_select_popup( lo_repo ).
    IF ls_tag IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_git_porcelain=>delete_tag(
      iv_url = lo_repo->get_url( )
      is_tag = ls_tag ).

    lv_text = |Tag { zcl_abapgit_git_tag=>remove_tag_prefix( ls_tag-name ) } deleted| ##NO_TEXT.

    MESSAGE lv_text TYPE 'S'.

  ENDMETHOD.


  METHOD get_unnecessary_local_objs.

    DATA: lt_tadir        TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_tadir_unique TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_tadir
                               WITH UNIQUE KEY pgmid object obj_name,
          lt_status       TYPE zif_abapgit_definitions=>ty_results_tt,
          lv_package      TYPE zif_abapgit_persistence=>ty_repo-package.

    FIELD-SYMBOLS: <ls_status> TYPE zif_abapgit_definitions=>ty_result,
                   <ls_tadir>  TYPE zif_abapgit_definitions=>ty_tadir.



    " delete objects which are added locally but are not in remote repo
    lt_status = io_repo->status( ).

    lv_package = io_repo->get_package( ).
    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( lv_package ).
    SORT lt_tadir BY pgmid ASCENDING object ASCENDING obj_name ASCENDING devclass ASCENDING.

    LOOP AT lt_status ASSIGNING <ls_status>
                      WHERE lstate = zif_abapgit_definitions=>c_state-added.

      READ TABLE lt_tadir ASSIGNING <ls_tadir>
                          WITH KEY pgmid    = 'R3TR'
                                   object   = <ls_status>-obj_type
                                   obj_name = <ls_status>-obj_name
                                   devclass = <ls_status>-package
                          BINARY SEARCH.
      IF sy-subrc <> 0.
* skip objects that does not exist locally
        CONTINUE.
      ENDIF.

      INSERT <ls_tadir> INTO TABLE lt_tadir_unique.

    ENDLOOP.

    rt_unnecessary_local_objects = lt_tadir_unique.

  ENDMETHOD.


  METHOD pull.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo.

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lo_repo->refresh( ).

    zcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD reset.

    DATA: lo_repo                   TYPE REF TO zcl_abapgit_repo,
          lv_answer                 TYPE c LENGTH 1,
          lt_unnecessary_local_objs TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_selected               LIKE lt_unnecessary_local_objs,
          lt_columns                TYPE zif_abapgit_definitions=>ty_alv_column_tt,
          ls_checks                 TYPE zif_abapgit_definitions=>ty_delete_checks,
          li_popups                 TYPE REF TO zif_abapgit_popups.

    FIELD-SYMBOLS: <ls_column> TYPE zif_abapgit_definitions=>ty_alv_column.

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    IF lo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot reset. Local code is write-protected by repo config' ).
    ENDIF.

* todo, separate UI and logic
    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = 'Reset local objects?'
      iv_text_button_1         = 'Ok'
      iv_icon_button_1         = 'ICON_OKAY'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).              "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lt_unnecessary_local_objs = get_unnecessary_local_objs( lo_repo ).

    IF lines( lt_unnecessary_local_objs ) > 0.

      APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
      <ls_column>-name = 'OBJECT'.
      APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
      <ls_column>-name = 'OBJ_NAME'.

      li_popups = zcl_abapgit_ui_factory=>get_popups( ).
      li_popups->popup_to_select_from_list(
        EXPORTING
          it_list               = lt_unnecessary_local_objs
          iv_header_text        = |Which unnecessary objects should be deleted?|
          iv_select_column_text = 'Delete?'
          it_columns_to_display = lt_columns
        IMPORTING
          et_list              = lt_selected ).

      IF lines( lt_selected ) > 0.

        ls_checks = lo_repo->delete_checks( ).
        IF ls_checks-transport-required = abap_true.
          ls_checks-transport-transport = zcl_abapgit_ui_factory=>get_popups(
                                            )->popup_transport_request( ls_checks-transport-type ).
        ENDIF.

        zcl_abapgit_objects=>delete( it_tadir  = lt_selected
                                     is_checks = ls_checks ).

        lo_repo->refresh( ).

      ENDIF.

    ENDIF.

    zcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

  ENDMETHOD.


  METHOD switch_branch.

    DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          ls_branch TYPE zif_abapgit_definitions=>ty_git_branch.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    IF lo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot switch branch. Local code is write-protected by repo config' ).
    ENDIF.

    ls_branch = zcl_abapgit_ui_factory=>get_popups( )->branch_list_popup(
      iv_url             = lo_repo->get_url( )
      iv_default_branch  = lo_repo->get_branch_name( )
      iv_show_new_option = abap_true ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    IF ls_branch-name = zif_abapgit_popups=>c_new_branch_label.
      create_branch( iv_key ).
      RETURN.
    ENDIF.

    lo_repo->set_branch_name( ls_branch-name ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD switch_tag.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online,
          ls_tag  TYPE zif_abapgit_definitions=>ty_git_tag.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_tag = zcl_abapgit_ui_factory=>get_tag_popups( )->tag_select_popup( lo_repo ).
    IF ls_tag IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo->set_branch_name( ls_tag-name ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD tag_overview.

    DATA: lo_repo TYPE REF TO zcl_abapgit_repo_online.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    zcl_abapgit_ui_factory=>get_tag_popups( )->tag_list_popup( lo_repo ).

  ENDMETHOD.
ENDCLASS.
