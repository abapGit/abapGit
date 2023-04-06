CLASS zcl_abapgit_popups DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_popups .

    CLASS-METHODS center
      IMPORTING
        !iv_width          TYPE i
        !iv_height         TYPE i
      RETURNING
        VALUE(rs_position) TYPE zif_abapgit_popups=>ty_popup_position.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_lt_fields TYPE STANDARD TABLE OF sval WITH DEFAULT KEY .

    TYPES:
      BEGIN OF ty_commit_value_tab,
        commit   TYPE zif_abapgit_git_definitions=>ty_sha1,
        message  TYPE c LENGTH 100,
        datetime TYPE c LENGTH 20,
      END OF ty_commit_value_tab.
    TYPES:
      ty_commit_value_tab_tt TYPE STANDARD TABLE OF ty_commit_value_tab WITH DEFAULT KEY.

    CONSTANTS c_answer_cancel      TYPE c LENGTH 1 VALUE 'A' ##NO_TEXT.

    DATA ms_position TYPE zif_abapgit_popups=>ty_popup_position.

    METHODS add_field
      IMPORTING
        !iv_tabname    TYPE sval-tabname
        !iv_fieldname  TYPE sval-fieldname
        !iv_fieldtext  TYPE sval-fieldtext
        !iv_value      TYPE clike DEFAULT ''
        !iv_field_attr TYPE sval-field_attr DEFAULT ''
        !iv_obligatory TYPE spo_obl OPTIONAL
      CHANGING
        !ct_fields     TYPE zif_abapgit_popups=>ty_sval_tt .
    METHODS _popup_3_get_values
      IMPORTING
        !iv_popup_title    TYPE string
        !iv_no_value_check TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !ev_value_1        TYPE spo_value
        !ev_value_2        TYPE spo_value
        !ev_value_3        TYPE spo_value
      CHANGING
        !ct_fields         TYPE ty_lt_fields
      RAISING
        zcx_abapgit_exception .
    METHODS commit_list_build
      IMPORTING
        !iv_repo_url    TYPE string
        !iv_branch_name TYPE string
      EXPORTING
        !et_value_tab   TYPE ty_commit_value_tab_tt
        !et_commits     TYPE zif_abapgit_definitions=>ty_commit_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_popups IMPLEMENTATION.


  METHOD add_field.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF ct_fields.

    APPEND INITIAL LINE TO ct_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = iv_tabname.
    <ls_field>-fieldname  = iv_fieldname.
    <ls_field>-fieldtext  = iv_fieldtext.
    <ls_field>-value      = iv_value.
    <ls_field>-field_attr = iv_field_attr.
    <ls_field>-field_obl  = iv_obligatory.

  ENDMETHOD.


  METHOD center.

    CONSTANTS:
      lc_min_size TYPE i VALUE 10,
      lc_min_pos  TYPE i VALUE 5.

    " Magic math to approximate starting position of popup
    IF sy-scols > lc_min_size AND iv_width > 0 AND sy-scols > iv_width.
      rs_position-start_column = nmax(
        val1 = ( sy-scols - iv_width ) / 2
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_column = lc_min_pos.
    ENDIF.

    IF sy-srows > lc_min_size AND iv_height > 0 AND sy-srows > iv_height.
      rs_position-start_row = nmax(
        val1 = ( sy-srows - iv_height ) / 2 - 1
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_row = lc_min_pos.
    ENDIF.

    rs_position-end_column = rs_position-start_column + iv_width.
    rs_position-end_row = rs_position-start_row + iv_height.

  ENDMETHOD.


  METHOD commit_list_build.

    DATA:
      lv_unix_time   TYPE zcl_abapgit_git_time=>ty_unixtime,
      lv_date        TYPE d,
      lv_date_string TYPE c LENGTH 12,
      lv_time        TYPE t,
      lv_time_string TYPE c LENGTH 10.

    FIELD-SYMBOLS:
      <ls_commit>    TYPE zif_abapgit_definitions=>ty_commit,
      <ls_value_tab> TYPE ty_commit_value_tab.

    CLEAR: et_commits, et_value_tab.

    et_commits = zcl_abapgit_git_commit=>get_by_branch( iv_branch_name  = iv_branch_name
                                                        iv_repo_url     = iv_repo_url
                                                        iv_deepen_level = 99
                                                        iv_sorted       = abap_false )-commits.

    IF et_commits IS INITIAL.
      zcx_abapgit_exception=>raise( |No commits are available in this branch.| ).
    ENDIF.

    SORT et_commits BY time DESCENDING.

    LOOP AT et_commits ASSIGNING <ls_commit>.

      APPEND INITIAL LINE TO et_value_tab ASSIGNING <ls_value_tab>.
      <ls_value_tab>-commit  = <ls_commit>-sha1.
      <ls_value_tab>-message = <ls_commit>-message.
      lv_unix_time = <ls_commit>-time.
      zcl_abapgit_git_time=>get_utc(
        EXPORTING
          iv_unix = lv_unix_time
        IMPORTING
          ev_time = lv_time
          ev_date = lv_date ).
      WRITE: lv_date TO lv_date_string,
             lv_time TO lv_time_string.
      <ls_value_tab>-datetime = |{ lv_date_string }, | &&
                                |{ lv_time_string }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_popups~branch_list_popup.

    DATA: lo_branches    TYPE REF TO zcl_abapgit_git_branch_list,
          lt_branches    TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt,
          lv_answer      TYPE c LENGTH 1,
          lv_default     TYPE i,
          lv_head_suffix TYPE string,
          lv_head_symref TYPE string,
          lv_text        TYPE string,
          lt_selection   TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel>    LIKE LINE OF lt_selection,
                   <ls_branch> LIKE LINE OF lt_branches.


    lo_branches    = zcl_abapgit_git_transport=>branches( iv_url ).
    lt_branches    = lo_branches->get_branches_only( ).
    lv_head_suffix = | ({ zif_abapgit_definitions=>c_head_name })|.
    lv_head_symref = lo_branches->get_head_symref( ).

    IF iv_hide_branch IS NOT INITIAL.
      DELETE lt_branches WHERE name = iv_hide_branch.
    ENDIF.

    IF iv_hide_head IS NOT INITIAL.
      DELETE lt_branches WHERE name    = zif_abapgit_definitions=>c_head_name
                            OR is_head = abap_true.
    ENDIF.

    IF lt_branches IS INITIAL.
      IF iv_hide_head IS NOT INITIAL.
        lv_text = 'main'.
      ENDIF.
      IF iv_hide_branch IS NOT INITIAL AND iv_hide_branch <> zif_abapgit_definitions=>c_git_branch-main.
        IF lv_text IS INITIAL.
          lv_text = iv_hide_branch && ' is'.
        ELSE.
          CONCATENATE lv_text 'and' iv_hide_branch 'are' INTO lv_text SEPARATED BY space.
        ENDIF.
      ELSE.
        lv_text = lv_text && ' is'.
      ENDIF.
      IF lv_text IS NOT INITIAL.
        zcx_abapgit_exception=>raise( 'No branches available to select (' && lv_text && ' hidden)' ).
      ELSE.
        zcx_abapgit_exception=>raise( 'No branches are available to select' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_branches ASSIGNING <ls_branch>.

      CHECK <ls_branch>-name IS NOT INITIAL. " To ensure some below ifs

      IF <ls_branch>-is_head = abap_true.

        IF <ls_branch>-name = zif_abapgit_definitions=>c_head_name. " HEAD
          IF <ls_branch>-name <> lv_head_symref AND lv_head_symref IS NOT INITIAL.
            " HEAD but other HEAD symref exists - ignore
            CONTINUE.
          ELSE.
            INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
            <ls_sel>-varoption = <ls_branch>-name.
          ENDIF.
        ELSE.
          INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
          <ls_sel>-varoption = <ls_branch>-display_name && lv_head_suffix.
        ENDIF.

        IF lv_default > 0. " Shift down default if set
          lv_default = lv_default + 1.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
        <ls_sel>-varoption = <ls_branch>-display_name.
      ENDIF.

      IF <ls_branch>-name = iv_default_branch.
        IF <ls_branch>-is_head = abap_true.
          lv_default = 1.
        ELSE.
          lv_default = sy-tabix.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF iv_show_new_option = abap_true.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = zif_abapgit_popups=>c_new_branch_label.
    ENDIF.

    ms_position = center(
      iv_width  = 30
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Select Branch'
        textline1  = 'Select a branch'
        start_col  = ms_position-start_column
        start_row  = ms_position-start_row
        cursorline = lv_default
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    IF iv_show_new_option = abap_true AND <ls_sel>-varoption = zif_abapgit_popups=>c_new_branch_label.
      rs_branch-name = zif_abapgit_popups=>c_new_branch_label.
    ELSE.
      REPLACE FIRST OCCURRENCE OF lv_head_suffix IN <ls_sel>-varoption WITH ''.
      READ TABLE lt_branches WITH KEY display_name = <ls_sel>-varoption ASSIGNING <ls_branch>.
      IF sy-subrc <> 0.
* branch name longer than 65 characters
        LOOP AT lt_branches ASSIGNING <ls_branch> WHERE display_name CS <ls_sel>-varoption.
          EXIT. " current loop
        ENDLOOP.
      ENDIF.
      ASSERT <ls_branch> IS ASSIGNED.
      rs_branch = lo_branches->find_by_name( <ls_branch>-name ).
      lv_text = |Branch switched from { zcl_abapgit_git_branch_list=>get_display_name( iv_default_branch ) } to {
        zcl_abapgit_git_branch_list=>get_display_name( rs_branch-name ) } |.
      MESSAGE lv_text TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_popups~choose_code_insp_check_variant.

    DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

    FIELD-SYMBOLS: <ls_return> LIKE LINE OF lt_return.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'SCI_DYNP'
        fieldname         = 'CHKV'
      TABLES
        return_tab        = lt_return
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_return ASSIGNING <ls_return>
                         WITH KEY retfield = 'SCI_DYNP-CHKV'.
    IF sy-subrc = 0.
      rv_check_variant = <ls_return>-fieldval.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_popups~choose_pr_popup.

    DATA lv_answer    TYPE c LENGTH 1.
    DATA lt_selection TYPE TABLE OF spopli.
    FIELD-SYMBOLS <ls_sel>  LIKE LINE OF lt_selection.
    FIELD-SYMBOLS <ls_pull> LIKE LINE OF it_pulls.

    IF lines( it_pulls ) = 0.
      zcx_abapgit_exception=>raise( 'No pull requests to select from' ).
    ENDIF.

    LOOP AT it_pulls ASSIGNING <ls_pull>.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = |{ <ls_pull>-number } - { <ls_pull>-title } @{ <ls_pull>-user }|.
    ENDLOOP.

    ms_position = center(
      iv_width  = 74
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        textline1 = 'Select pull request'
        titel     = 'Select pull request'
        start_col = ms_position-start_column
        start_row = ms_position-start_row
      IMPORTING
        answer    = lv_answer
      TABLES
        t_spopli  = lt_selection
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    READ TABLE it_pulls INTO rs_pull INDEX sy-tabix.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_popups~commit_list_popup.

    DATA:
      lt_commits         TYPE zif_abapgit_definitions=>ty_commit_tt,
      lt_value_tab       TYPE ty_commit_value_tab_tt,
      lt_selected_values TYPE ty_commit_value_tab_tt,
      lt_columns         TYPE zif_abapgit_popups=>ty_alv_column_tt.

    FIELD-SYMBOLS:
      <ls_value_tab> TYPE ty_commit_value_tab,
      <ls_column>    TYPE zif_abapgit_popups=>ty_alv_column.

    commit_list_build(
      EXPORTING
        iv_branch_name = iv_branch_name
        iv_repo_url    = iv_repo_url
      IMPORTING
        et_value_tab   = lt_value_tab
        et_commits     = lt_commits ).

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'COMMIT'.
    <ls_column>-text   = 'Hash'.
    <ls_column>-length = 8.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'MESSAGE'.
    <ls_column>-text = 'Message'.
    <ls_column>-length = 60.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DATETIME'.
    <ls_column>-text = 'Datetime'.
    <ls_column>-length = 17.

    zif_abapgit_popups~popup_to_select_from_list(
      EXPORTING
        it_list               = lt_value_tab
        iv_title              = |Select a commit|
        iv_end_column         = 100
        iv_striped_pattern    = abap_true
        iv_optimize_col_width = abap_false
        iv_selection_mode     = if_salv_c_selection_mode=>single
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected_values ).

    IF lt_selected_values IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    READ TABLE lt_selected_values ASSIGNING <ls_value_tab> INDEX 1.
    ASSERT sy-subrc = 0.

    READ TABLE lt_commits INTO rs_commit WITH KEY sha1 = <ls_value_tab>-commit.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_popups~create_branch_popup.

    DATA: lt_fields TYPE TABLE OF sval.
    DATA: lv_name   TYPE spo_value.

    CLEAR: ev_name, ev_cancel.

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Name'
                         iv_value     = 'new-branch-name'
               CHANGING  ct_fields    = lt_fields ).

    TRY.

        _popup_3_get_values(
          EXPORTING iv_popup_title = |Create branch from {
            zcl_abapgit_git_branch_list=>get_display_name( iv_source_branch_name ) }|
          IMPORTING ev_value_1     = lv_name
          CHANGING  ct_fields      = lt_fields ).

        ev_name = zcl_abapgit_git_branch_list=>complete_heads_branch_name(
              zcl_abapgit_git_branch_list=>normalize_branch_name( lv_name ) ).

      CATCH zcx_abapgit_cancel.
        ev_cancel = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_folder_logic.

    DATA:
      lt_selection TYPE TABLE OF spopli,
      lv_answer    TYPE c LENGTH 1.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-selflag   = abap_true.
    <ls_sel>-varoption = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-varoption = zif_abapgit_dot_abapgit=>c_folder_logic-full.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-varoption = zif_abapgit_dot_abapgit=>c_folder_logic-mixed.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel     = 'Folder logic'
        textline1 = 'Select folder logic'
        start_col = ms_position-start_column
        start_row = ms_position-start_row
      IMPORTING
        answer    = lv_answer
      TABLES
        t_spopli  = lt_selection
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      zcx_abapgit_cancel=>raise( |Canceled| ).
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    rv_folder_logic = <ls_sel>-varoption.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_search_help.

    DATA lt_ret TYPE TABLE OF ddshretval.
    DATA ls_ret LIKE LINE OF lt_ret.
    DATA lv_tabname TYPE dfies-tabname.
    DATA lv_fieldname TYPE dfies-fieldname.

    SPLIT iv_tab_field AT '-' INTO lv_tabname lv_fieldname.
    lv_tabname = to_upper( lv_tabname ).
    lv_fieldname = to_upper( lv_fieldname ).

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |F4IF_FIELD_VALUE_REQUEST error [{ iv_tab_field }]| ).
    ENDIF.

    IF lines( lt_ret ) > 0.
      READ TABLE lt_ret WITH KEY fieldname = lv_fieldname INTO ls_ret.
      IF sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ELSE.
        READ TABLE lt_ret INDEX 1 INTO ls_ret.
        ASSERT sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_select_tr_requests.
    DATA ls_r_trkorr TYPE LINE OF zif_abapgit_definitions=>ty_trrngtrkor_tt.
    DATA lr_request TYPE REF TO trwbo_request_header.
    DATA lt_request TYPE trwbo_request_headers.

    ms_position = center(
      iv_width  = 120
      iv_height = 10 ).

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = iv_username_pattern
        is_selection           = is_selection
        iv_complete_projects   = abap_false
        is_popup               = ms_position
        iv_via_selscreen       = 'X'
        iv_title               = iv_title
      IMPORTING
        et_requests            = lt_request
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Selection canceled' ).
    ENDIF.

    IF lt_request IS INITIAL.
      zcx_abapgit_exception=>raise( 'No Request Found' ).
    ENDIF.

    IF lines( lt_request ) > 10000.
      zcx_abapgit_exception=>raise( 'Too many requests selected (max 10000)' ).
    ENDIF.

    LOOP AT lt_request REFERENCE INTO lr_request.
      ls_r_trkorr-sign = 'I'.
      ls_r_trkorr-option = 'EQ'.
      ls_r_trkorr-low = lr_request->trkorr.
      INSERT ls_r_trkorr INTO TABLE rt_r_trkorr.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_select_wb_tc_tr_and_tsk.
    DATA ls_selection  TYPE trwbo_selection.
    DATA lv_title TYPE trwbo_title.

    ls_selection-trkorrpattern = space.
    ls_selection-connect_req_task_conditions = 'X'.
    ls_selection-reqfunctions = 'KTRXS'.
    ls_selection-reqstatus = 'RNODL'.
    ls_selection-taskstatus = 'RNODL'.
    CONDENSE ls_selection-reqfunctions NO-GAPS.
    ls_selection-taskfunctions = 'QRSX'.
    CONCATENATE sy-sysid '*' INTO ls_selection-trkorrpattern.

    lv_title = 'Select Transports / Tasks'.

    rt_r_trkorr = zif_abapgit_popups~popup_select_tr_requests(
      is_selection        = ls_selection
      iv_title            = lv_title
      iv_username_pattern = '*' ).
  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_to_confirm.

    ms_position = center(
      iv_width  = 65
      iv_height = 5 ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_titlebar
        text_question         = iv_text_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
        start_column          = ms_position-start_column
        start_row             = ms_position-start_row
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_to_create_package.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'PB_POPUP_PACKAGE_CREATE'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      zcx_abapgit_exception=>raise( 'Your system does not support automatic creation of packages.' &&
        'Please, create the package manually.' ).
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = es_package_data
      EXCEPTIONS
        action_cancelled = 1.
    ev_create = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_to_create_transp_branch.
    DATA: lt_fields             TYPE TABLE OF sval,
          lv_transports_as_text TYPE string,
          lv_desc_as_text       TYPE string,
          ls_transport_header   LIKE LINE OF it_transport_headers.
    DATA: lv_branch_name        TYPE spo_value.
    DATA: lv_commit_text        TYPE spo_value.

    CLEAR: rs_transport_branch-branch_name, rs_transport_branch-commit_text.

    " If we only have one transport selected set branch name to Transport
    " name and commit description to transport description.
    IF lines( it_transport_headers ) = 1.
      READ TABLE it_transport_headers INDEX 1 INTO ls_transport_header.
      lv_transports_as_text = ls_transport_header-trkorr.
      lv_desc_as_text = zcl_abapgit_factory=>get_cts_api( )->read_description( ls_transport_header-trkorr ).
    ELSE.   " Else set branch name and commit message to 'Transport(s)_TRXXXXXX_TRXXXXX'
      lv_transports_as_text = 'Transport(s)'.
      LOOP AT it_transport_headers INTO ls_transport_header.
        CONCATENATE lv_transports_as_text '_' ls_transport_header-trkorr INTO lv_transports_as_text.
      ENDLOOP.
      lv_desc_as_text = lv_transports_as_text.

    ENDIF.
    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Branch name'
                         iv_value     = lv_transports_as_text
               CHANGING  ct_fields    = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'ABAPTXT255'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Commit text'
                         iv_value     = lv_desc_as_text
               CHANGING  ct_fields    = lt_fields ).

    _popup_3_get_values( EXPORTING iv_popup_title = 'Transport to new Branch'
                         IMPORTING ev_value_1     = lv_branch_name
                                   ev_value_2     = lv_commit_text
                         CHANGING  ct_fields      = lt_fields ).

    rs_transport_branch-branch_name = lv_branch_name.
    rs_transport_branch-commit_text = lv_commit_text.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_to_select_from_list.

    DATA lo_popup TYPE REF TO lcl_object_descision_list.

    CLEAR et_list.

    ms_position = center(
      iv_width  = iv_end_column - iv_start_column
      iv_height = iv_end_line - iv_start_line ).

    CREATE OBJECT lo_popup
      EXPORTING
        it_list               = it_list
        iv_title              = iv_title
        iv_header_text        = iv_header_text
        is_position           = ms_position
        iv_striped_pattern    = iv_striped_pattern
        iv_optimize_col_width = iv_optimize_col_width
        iv_selection_mode     = iv_selection_mode
        iv_select_column_text = iv_select_column_text
        it_columns_to_display = it_columns_to_display
        it_preselected_rows   = it_preselected_rows.

    lo_popup->display( ).
    lo_popup->get_selected( IMPORTING et_list = et_list ).

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_to_select_labels.

    DATA:
      lt_all_labels         TYPE zif_abapgit_repo_srv=>ty_labels,
      ls_label              LIKE LINE OF lt_all_labels,
      lt_current_labels     TYPE string_table,
      lt_selected_labels    LIKE lt_all_labels,
      lt_columns_to_display TYPE zif_abapgit_popups=>ty_alv_column_tt,
      lt_preselected_rows   TYPE zif_abapgit_popups=>ty_rows,
      ls_columns_to_display LIKE LINE OF lt_columns_to_display,
      lv_save_tabix         TYPE i,
      li_popup              TYPE REF TO zif_abapgit_popups.

    FIELD-SYMBOLS: <lv_label>         TYPE zif_abapgit_repo_srv=>ty_label,
                   <lv_current_label> TYPE LINE OF string_table.

    lt_current_labels = zcl_abapgit_repo_labels=>split( iv_labels ).

    lt_all_labels = zcl_abapgit_repo_srv=>get_instance( )->get_label_list( ).

    " Add labels which are not saved yet
    LOOP AT lt_current_labels ASSIGNING <lv_current_label>.

      READ TABLE lt_all_labels TRANSPORTING NO FIELDS
                               WITH KEY key_label
                               COMPONENTS label = <lv_current_label>.
      IF sy-subrc <> 0.
        ls_label-label = <lv_current_label>.
        INSERT ls_label INTO TABLE lt_all_labels.
      ENDIF.

    ENDLOOP.

    IF lines( lt_all_labels ) = 0.
      zcx_abapgit_exception=>raise( |No labels maintained yet| ).
    ENDIF.

    SORT lt_all_labels.
    DELETE ADJACENT DUPLICATES FROM lt_all_labels.

    " Preselect current labels
    LOOP AT lt_all_labels ASSIGNING <lv_label>.

      lv_save_tabix = sy-tabix.

      READ TABLE lt_current_labels TRANSPORTING NO FIELDS
                                   WITH KEY table_line = <lv_label>-label.
      IF sy-subrc = 0.
        INSERT lv_save_tabix INTO TABLE lt_preselected_rows.
      ENDIF.

    ENDLOOP.

    ls_columns_to_display-name = 'LABEL'.
    ls_columns_to_display-text = 'Label'.
    INSERT ls_columns_to_display INTO TABLE lt_columns_to_display.

    li_popup = zcl_abapgit_ui_factory=>get_popups( ).
    li_popup->popup_to_select_from_list(
      EXPORTING
        iv_header_text        = 'Select labels'
        iv_select_column_text = 'Add label'
        it_list               = lt_all_labels
        iv_selection_mode     = if_salv_c_selection_mode=>multiple
        it_columns_to_display = lt_columns_to_display
        it_preselected_rows   = lt_preselected_rows
        iv_start_column       = 15
        iv_end_column         = 55
      IMPORTING
        et_list               = lt_selected_labels ).

    LOOP AT lt_selected_labels ASSIGNING <lv_label>.
      IF rv_labels IS NOT INITIAL.
        rv_labels = rv_labels && ','.
      ENDIF.
      rv_labels = rv_labels && <lv_label>-label.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_to_select_transports.

* todo, method to be renamed, it only returns one transport

    DATA: lv_trkorr TYPE e070-trkorr,
          ls_trkorr LIKE LINE OF rt_trkorr.


    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = lv_trkorr.

    IF NOT lv_trkorr IS INITIAL.
      ls_trkorr-trkorr = lv_trkorr.
      APPEND ls_trkorr TO rt_trkorr.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_popups~popup_transport_request.

    DATA: lt_e071    TYPE STANDARD TABLE OF e071,
          lt_e071k   TYPE STANDARD TABLE OF e071k,
          lv_order   TYPE trkorr,
          ls_e070use TYPE e070use.
    DATA lv_category TYPE e070-korrdev.

    " If default transport is set and its type matches, then use it as default for the popup
    ls_e070use = zcl_abapgit_default_transport=>get_instance( )->get( ).

    IF ( ls_e070use-trfunction = is_transport_type-request OR ls_e070use-trfunction IS INITIAL )
      AND iv_use_default_transport = abap_true.
      lv_order = ls_e070use-ordernum.
    ENDIF.

    " Differentiate between customizing and WB requests
    IF is_transport_type-request = zif_abapgit_cts_api=>c_transport_type-cust_request.
      lv_category = zif_abapgit_cts_api=>c_transport_category-customizing.
    ELSE.
      lv_category = zif_abapgit_cts_api=>c_transport_category-workbench.
    ENDIF.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type          = is_transport_type-request
        wi_task_type           = is_transport_type-task
        wi_category            = lv_category
        wi_order               = lv_order
      IMPORTING
        we_order               = rv_transport
      TABLES
        wt_e071                = lt_e071
        wt_e071k               = lt_e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.

    IF sy-subrc = 1.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ELSEIF sy-subrc > 1.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_popups~tag_list_popup.

    DATA: lo_branches  TYPE REF TO zcl_abapgit_git_branch_list,
          lt_tags      TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt,
          ls_branch    TYPE zif_abapgit_git_definitions=>ty_git_branch,
          lv_answer    TYPE c LENGTH 1,
          lv_default   TYPE i,
          lv_tag       TYPE string,
          lt_selection TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection,
                   <ls_tag> LIKE LINE OF lt_tags.


    lo_branches = zcl_abapgit_git_transport=>branches( iv_url ).
    lt_tags     = lo_branches->get_tags_only( ).

    LOOP AT lt_tags ASSIGNING <ls_tag> WHERE name NP '*' && zif_abapgit_definitions=>c_git_branch-peel.

      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = zcl_abapgit_git_tag=>remove_tag_prefix( <ls_tag>-name ).

    ENDLOOP.

    IF lt_selection IS INITIAL.
      zcx_abapgit_exception=>raise( 'No tags are available to select' ).
    ENDIF.

    ms_position = center(
      iv_width  = 30
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Select Tag'
        textline1  = 'Select a tag'
        start_col  = ms_position-start_column
        start_row  = ms_position-start_row
        cursorline = lv_default
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    lv_tag = zcl_abapgit_git_tag=>add_tag_prefix( <ls_sel>-varoption ).

    READ TABLE lt_tags WITH KEY name_key COMPONENTS name = lv_tag ASSIGNING <ls_tag>.
    IF sy-subrc <> 0.
      " tag name longer than 65 characters
      LOOP AT lt_tags ASSIGNING <ls_tag> WHERE name CS lv_tag.
        EXIT.
      ENDLOOP.
    ENDIF.
    ASSERT <ls_tag> IS ASSIGNED.

    ls_branch = lo_branches->find_by_name( <ls_tag>-name ).
    MOVE-CORRESPONDING ls_branch TO rs_tag.

  ENDMETHOD.


  METHOD _popup_3_get_values.

    DATA lv_answer TYPE c LENGTH 1.
    FIELD-SYMBOLS: <ls_field> TYPE sval.

    ms_position = center(
      iv_width  = 120
      iv_height = lines( ct_fields ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check = iv_no_value_check
        popup_title    = iv_popup_title
        start_column   = ms_position-start_column
        start_row      = ms_position-start_row
      IMPORTING
        returncode     = lv_answer
      TABLES
        fields         = ct_fields
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    IF ev_value_1 IS SUPPLIED.
      READ TABLE ct_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_1 = <ls_field>-value.
    ENDIF.

    IF ev_value_2 IS SUPPLIED.
      READ TABLE ct_fields INDEX 2 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_2 = <ls_field>-value.
    ENDIF.

    IF ev_value_3 IS SUPPLIED.
      READ TABLE ct_fields INDEX 3 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_3 = <ls_field>-value.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
