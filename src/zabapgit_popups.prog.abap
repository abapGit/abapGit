*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_POPUPS
*&---------------------------------------------------------------------*

"! various popups, static methods, no class/object state
CLASS lcl_popups DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_popup,
             url         TYPE string,
             package     TYPE devclass,
             branch_name TYPE string,
             cancel      TYPE abap_bool,
           END OF ty_popup.

    CONSTANTS c_new_branch_label TYPE string VALUE '+ create new ...' ##NO_TEXT.

    CLASS-METHODS:
      popup_package_export
        EXPORTING ev_package      TYPE devclass
                  ev_folder_logic TYPE string
        RAISING   zcx_abapgit_exception,
      popup_object
        RETURNING VALUE(rs_tadir) TYPE tadir
        RAISING   zcx_abapgit_exception,
      create_branch_popup
        EXPORTING ev_name   TYPE string
                  ev_cancel TYPE abap_bool
        RAISING   zcx_abapgit_exception,
      run_page_class_popup
        EXPORTING ev_name   TYPE string
                  ev_cancel TYPE abap_bool
        RAISING   zcx_abapgit_exception,
      repo_new_offline
        RETURNING VALUE(rs_popup) TYPE ty_popup
        RAISING   zcx_abapgit_exception,
      branch_list_popup
        IMPORTING iv_url             TYPE string
                  iv_default_branch  TYPE string OPTIONAL
                  iv_show_new_option TYPE abap_bool OPTIONAL
        RETURNING VALUE(rs_branch)   TYPE lcl_git_branch_list=>ty_git_branch
        RAISING   zcx_abapgit_exception,
      repo_popup
        IMPORTING iv_url            TYPE string
                  iv_package        TYPE devclass  OPTIONAL
                  iv_branch         TYPE string    DEFAULT 'refs/heads/master'
                  iv_freeze_package TYPE abap_bool OPTIONAL
                  iv_freeze_url     TYPE abap_bool OPTIONAL
                  iv_title          TYPE clike     DEFAULT 'Clone repository ...'
        RETURNING VALUE(rs_popup)   TYPE ty_popup
        RAISING   zcx_abapgit_exception ##NO_TEXT,
      popup_to_confirm
        IMPORTING
                  titlebar              TYPE clike
                  text_question         TYPE clike
                  text_button_1         TYPE clike     DEFAULT 'Yes'
                  icon_button_1         TYPE icon-name DEFAULT space
                  text_button_2         TYPE clike     DEFAULT 'No'
                  icon_button_2         TYPE icon-name DEFAULT space
                  default_button        TYPE char1 DEFAULT '1'
                  display_cancel_button TYPE char1 DEFAULT abap_true
        RETURNING VALUE(rv_answer)      TYPE char1
        RAISING   zcx_abapgit_exception,
      popup_to_inform
        IMPORTING
                  titlebar     TYPE clike
                  text_message TYPE clike
        RAISING   zcx_abapgit_exception,
      popup_to_create_package
        EXPORTING es_package_data TYPE scompkdtln
                  ev_create       TYPE boolean
        RAISING   zcx_abapgit_exception,
      popup_to_create_transp_branch
        IMPORTING it_transport_headers       TYPE trwbo_request_headers
        RETURNING VALUE(rs_transport_branch) TYPE zif_abapgit_definitions=>ty_transport_to_branch
        RAISING   zcx_abapgit_exception
                  zcx_abapgit_cancel,
      popup_to_select_transports
        RETURNING VALUE(rt_trkorr) TYPE trwbo_request_headers,
      popup_to_select_from_list
        IMPORTING it_list               TYPE STANDARD TABLE
                  i_header_text         TYPE csequence
                  i_select_column_text  TYPE csequence
                  it_columns_to_display TYPE stringtab
        EXPORTING VALUE(et_list)        TYPE STANDARD TABLE
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES: ty_sval_tt TYPE STANDARD TABLE OF sval WITH DEFAULT KEY.

    CONSTANTS: co_fieldname_selected TYPE lvc_fname VALUE `SELECTED`.

    CLASS-DATA:
      mo_select_list_popup TYPE REF TO cl_salv_table,
      mr_table             TYPE REF TO data,
      mo_table_descr       TYPE REF TO cl_abap_tabledescr.

    CLASS-METHODS:
      add_field
        IMPORTING iv_tabname    TYPE sval-tabname
                  iv_fieldname  TYPE sval-fieldname
                  iv_fieldtext  TYPE sval-fieldtext
                  iv_value      TYPE clike DEFAULT ''
                  iv_field_attr TYPE sval-field_attr DEFAULT ''
        CHANGING  ct_fields     TYPE ty_sval_tt,

      create_new_table
        IMPORTING
          it_list TYPE STANDARD TABLE,

      get_selected_rows
        EXPORTING
          et_list TYPE INDEX TABLE,

      on_select_list_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            row
            column,

      on_select_list_function_click FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
            e_salv_function,

      extract_field_values
        IMPORTING
          it_fields  TYPE ty_sval_tt
        EXPORTING
          ev_url     TYPE abaptxt255-line
          ev_package TYPE tdevc-devclass
          ev_branch  TYPE textl-line.

ENDCLASS.

CLASS lcl_popups IMPLEMENTATION.

  METHOD add_field.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF ct_fields.

    APPEND INITIAL LINE TO ct_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = iv_tabname.
    <ls_field>-fieldname  = iv_fieldname.
    <ls_field>-fieldtext  = iv_fieldtext.
    <ls_field>-value      = iv_value.
    <ls_field>-field_attr = iv_field_attr.

  ENDMETHOD.

  METHOD popup_object.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    add_field( EXPORTING iv_tabname   = 'TADIR'
                         iv_fieldname = 'OBJECT'
                         iv_fieldtext = 'Type'
               CHANGING ct_fields     = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'TADIR'
                         iv_fieldname = 'OBJ_NAME'
                         iv_fieldtext = 'Name'
               CHANGING ct_fields     = lt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Object'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.
    rs_tadir-object = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.
    rs_tadir-obj_name = <ls_field>-value.

    rs_tadir = lcl_tadir=>read_single( iv_object   = rs_tadir-object
                                       iv_obj_name = rs_tadir-obj_name ).

  ENDMETHOD.

  METHOD popup_package_export.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    add_field( EXPORTING iv_tabname   = 'TDEVC'
                         iv_fieldname = 'DEVCLASS'
                         iv_fieldtext = 'Package'
               CHANGING ct_fields     = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'TDEVC'
                         iv_fieldname = 'INTSYS'
                         iv_fieldtext = 'Folder logic'
                         iv_value     = 'PREFIX'
               CHANGING ct_fields     = lt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Export package'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.
    ev_package = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.
    ev_folder_logic = <ls_field>-value.

  ENDMETHOD.                    "popup_package_export

  METHOD create_branch_popup.

    DATA: lv_answer TYPE c LENGTH 1,
          lt_fields TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CLEAR: ev_name, ev_cancel.

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Name'
                         iv_value     = 'new-branch-name'
               CHANGING ct_fields     = lt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Create branch'
      IMPORTING
        returncode      = lv_answer
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2 ##NO_TEXT.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_answer = 'A'.
      ev_cancel = abap_true.
    ELSE.
      READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_name = lcl_git_branch_list=>complete_heads_branch_name(
        lcl_git_branch_list=>normalize_branch_name( <ls_field>-value ) ).
    ENDIF.

  ENDMETHOD.

  METHOD run_page_class_popup.

    DATA: lv_answer TYPE c LENGTH 1,
          lt_fields TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CLEAR: ev_name, ev_cancel.

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Name'
                         iv_value     = 'lcl_gui_page_'
               CHANGING ct_fields     = lt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Run page manually'
      IMPORTING
        returncode      = lv_answer
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2 ##NO_TEXT.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_answer = 'A'.
      ev_cancel = abap_true.
    ELSE.
      READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_name = to_upper( <ls_field>-value ).
    ENDIF.

  ENDMETHOD.  "run_page_class_popup

  METHOD repo_new_offline.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval,
          lv_icon_ok    TYPE icon-name,
          lv_button1    TYPE svalbutton-buttontext,
          lv_icon1      TYPE icon-name,
          lv_finished   TYPE abap_bool,
          lx_error      TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    add_field( EXPORTING iv_tabname   = 'ABAPTXT255'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Name'
               CHANGING ct_fields     = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'TDEVC'
                         iv_fieldname = 'DEVCLASS'
                         iv_fieldtext = 'Package'
               CHANGING ct_fields     = lt_fields ).

    WHILE lv_finished = abap_false.

      lv_icon_ok  = icon_okay.
      lv_button1 = 'Create package' ##NO_TEXT.
      lv_icon1   = icon_folder.

      CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
        EXPORTING
          popup_title       = 'New Offline Project'
          programname       = sy-repid
          formname          = 'PACKAGE_POPUP'
          ok_pushbuttontext = ''
          icon_ok_push      = lv_icon_ok
          first_pushbutton  = lv_button1
          icon_button_1     = lv_icon1
          second_pushbutton = ''
          icon_button_2     = ''
        IMPORTING
          returncode        = lv_returncode
        TABLES
          fields            = lt_fields
        EXCEPTIONS
          error_in_fields   = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
      ENDIF.

      IF lv_returncode = 'A'.
        rs_popup-cancel = abap_true.
        RETURN.
      ENDIF.

      READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      rs_popup-url = <ls_field>-value.

      READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      TRANSLATE <ls_field>-value TO UPPER CASE.
      rs_popup-package = <ls_field>-value.

      lv_finished = abap_true.

      TRY.
          lcl_app=>repo_srv( )->validate_package( rs_popup-package ).

        CATCH zcx_abapgit_exception INTO lx_error.
          " in case of validation errors we display the popup again
          MESSAGE lx_error->text TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR lv_finished.
      ENDTRY.

    ENDWHILE.

  ENDMETHOD.                    "repo_new_offline

  METHOD branch_list_popup.

    DATA: lo_branches    TYPE REF TO lcl_git_branch_list,
          lt_branches    TYPE lcl_git_branch_list=>ty_git_branch_list_tt,
          lv_answer      TYPE c LENGTH 1,
          lv_default     TYPE i,
          lv_head_suffix TYPE string,
          lv_head_symref TYPE string,
          lt_selection   TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel>    LIKE LINE OF lt_selection,
                   <ls_branch> LIKE LINE OF lt_branches.


    lo_branches    = lcl_git_transport=>branches( iv_url ).
    lt_branches    = lo_branches->get_branches_only( ).
    lv_head_suffix = | ({ lcl_git_branch_list=>c_head_name })|.
    lv_head_symref = lo_branches->get_head_symref( ).

    LOOP AT lt_branches ASSIGNING <ls_branch>.

      CHECK <ls_branch>-name IS NOT INITIAL. " To ensure some below ifs

      IF <ls_branch>-is_head = abap_true.

        IF <ls_branch>-name = lcl_git_branch_list=>c_head_name. " HEAD
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
      <ls_sel>-varoption = c_new_branch_label.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        textline1          = 'Select branch'
        titel              = 'Select branch'
        start_col          = 30
        start_row          = 5
        cursorline         = lv_default
      IMPORTING
        answer             = lv_answer
      TABLES
        t_spopli           = lt_selection
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.                             "#EC NOTEXT
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = 'A'. " cancel
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    IF iv_show_new_option = abap_true AND <ls_sel>-varoption = c_new_branch_label.
      rs_branch-name = c_new_branch_label.
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
    ENDIF.

  ENDMETHOD.

  METHOD repo_popup.

    DATA: lv_returncode TYPE c,
          lv_icon_ok    TYPE icon-name,
          lv_icon_br    TYPE icon-name,
          lt_fields     TYPE TABLE OF sval,
          lv_uattr      TYPE spo_fattr,
          lv_pattr      TYPE spo_fattr,
          lv_button2    TYPE svalbutton-buttontext,
          lv_icon2      TYPE icon-name,
          lv_package    TYPE tdevc-devclass,
          lv_url        TYPE abaptxt255-line,
          lv_branch     TYPE textl-line,
          lv_finished   TYPE abap_bool,
          lx_error      TYPE REF TO zcx_abapgit_exception.

    IF iv_freeze_url = abap_true.
      lv_uattr = '05'.
    ENDIF.

    IF iv_freeze_package = abap_true.
      lv_pattr = '05'.
    ENDIF.

    IF iv_package IS INITIAL. " Empty package -> can be created
      lv_button2 = 'Create package' ##NO_TEXT.
      lv_icon2   = icon_folder.
    ENDIF.

    lv_package = iv_package.
    lv_url     = iv_url.
    lv_branch  = iv_branch.

    WHILE lv_finished = abap_false.

      CLEAR: lt_fields.

      add_field( EXPORTING iv_tabname    = 'ABAPTXT255'
                           iv_fieldname  = 'LINE'
                           iv_fieldtext  = 'Git clone URL'
                           iv_value      = lv_url
                           iv_field_attr = lv_uattr
                 CHANGING ct_fields      = lt_fields ).

      add_field( EXPORTING iv_tabname    = 'TDEVC'
                           iv_fieldname  = 'DEVCLASS'
                           iv_fieldtext  = 'Target package'
                           iv_value      = lv_package
                           iv_field_attr = lv_pattr
                 CHANGING ct_fields      = lt_fields ).

      add_field( EXPORTING iv_tabname    = 'TEXTL'
                           iv_fieldname  = 'LINE'
                           iv_fieldtext  = 'Branch'
                           iv_value      = lv_branch
                           iv_field_attr = '05'
                 CHANGING ct_fields      = lt_fields ).

      lv_icon_ok  = icon_okay.
      lv_icon_br  = icon_workflow_fork.

      CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
        EXPORTING
          popup_title       = iv_title
          programname       = sy-repid
          formname          = 'BRANCH_POPUP'
          ok_pushbuttontext = 'OK'
          icon_ok_push      = lv_icon_ok
          first_pushbutton  = 'Select branch'
          icon_button_1     = lv_icon_br
          second_pushbutton = lv_button2
          icon_button_2     = lv_icon2
        IMPORTING
          returncode        = lv_returncode
        TABLES
          fields            = lt_fields
        EXCEPTIONS
          error_in_fields   = 1
          OTHERS            = 2.                              "#EC NOTEXT

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
      ENDIF.

      IF lv_returncode = 'A'.
        rs_popup-cancel = abap_true.
        RETURN.
      ENDIF.

      extract_field_values(
        EXPORTING
          it_fields  = lt_fields
        IMPORTING
          ev_url     = lv_url
          ev_package = lv_package
          ev_branch  = lv_branch ).

      lv_finished = abap_true.

      TRY.
          zcl_abapgit_url=>name( |{ lv_url }| ).
          IF iv_freeze_package = abap_false.
            lcl_app=>repo_srv( )->validate_package( lv_package ).
          ENDIF.
        CATCH zcx_abapgit_exception INTO lx_error.
          MESSAGE lx_error->text TYPE 'S' DISPLAY LIKE 'E'.
          " in case of validation errors we display the popup again
          CLEAR: lv_finished.
      ENDTRY.

    ENDWHILE.

    rs_popup-url         = lv_url.
    rs_popup-package     = lv_package.
    rs_popup-branch_name = lv_branch.

  ENDMETHOD.

  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = titlebar
        text_question         = text_question
        text_button_1         = text_button_1
        icon_button_1         = icon_button_1
        text_button_2         = text_button_2
        icon_button_2         = icon_button_2
        default_button        = default_button
        display_cancel_button = display_cancel_button
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.  "popup_to_confirm

  METHOD popup_to_inform.

    DATA: lv_line1 TYPE char70,
          lv_line2 TYPE char70.

    lv_line1 = text_message.
    IF strlen( text_message ) > 70.
      lv_line2 = text_message+70.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = titlebar
        txt1  = lv_line1
        txt2  = lv_line2.

  ENDMETHOD.  " popup_to_inform.

  METHOD popup_to_create_package.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'PB_POPUP_PACKAGE_CREATE'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      zcx_abapgit_exception=>raise( 'Function module PB_POPUP_PACKAGE_CREATE does not exist' ).
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = es_package_data
      EXCEPTIONS
        action_cancelled = 1.
    IF sy-subrc = 0.
      ev_create = abap_true.
    ELSE.
      ev_create = abap_false.
    ENDIF.
  ENDMETHOD.  " popup_to_create_package

  METHOD popup_to_select_transports.
    DATA: lrs_trfunction TYPE trsel_trs_function,
          lv_types       TYPE string,
          ls_ranges      TYPE trsel_ts_ranges.

    " Fill all request types
    lv_types = 'KWTCOEMPDRSXQFG'.
    lrs_trfunction-sign   = 'I'.
    lrs_trfunction-option = 'EQ'.
    WHILE lv_types <> space.
      lrs_trfunction-low = lv_types(1).
      APPEND lrs_trfunction TO ls_ranges-request_funcs.
      SHIFT lv_types.
    ENDWHILE.

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = sy-uname
        iv_via_selscreen       = 'X'
        iv_complete_projects   = ''
        "is_popup               = ''
        iv_title               = 'abapGit: Transport Request Selection'
      IMPORTING
        et_requests            = rt_trkorr
      CHANGING
        cs_ranges              = ls_ranges
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.

  ENDMETHOD.

  METHOD popup_to_create_transp_branch.
    DATA: lv_returncode         TYPE c,
          lt_fields             TYPE TABLE OF sval,
          lv_transports_as_text TYPE string,
          ls_transport_header   LIKE LINE OF it_transport_headers.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    lv_transports_as_text = 'Transport(s)'.
    LOOP AT it_transport_headers INTO ls_transport_header.
      CONCATENATE lv_transports_as_text '_' ls_transport_header-trkorr INTO lv_transports_as_text.
    ENDLOOP.

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Branch name'
                         iv_value     = lv_transports_as_text
               CHANGING ct_fields     = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'ABAPTXT255'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Commit text'
                         iv_value     = lv_transports_as_text
               CHANGING ct_fields     = lt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Transport to new Branch'
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_returncode = 'A'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_transport_branch-branch_name = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_transport_branch-commit_text = <ls_field>-value.
  ENDMETHOD.

  METHOD popup_to_select_from_list.

    DATA:
      lo_events       TYPE REF TO cl_salv_events_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lt_columns      TYPE salv_t_column_ref,
      ls_column       TYPE salv_s_column_ref,
      lo_column       TYPE REF TO cl_salv_column_list,
      lo_table_header TYPE REF TO cl_salv_form_text.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    CLEAR: et_list.

    create_new_table( it_list = it_list ).

    ASSIGN mr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_select_list_popup
                                CHANGING  t_table = <table> ).

        mo_select_list_popup->set_screen_status( pfstatus = '102'
                                                 report = 'SAPMSVIM' ).

        mo_select_list_popup->set_screen_popup( start_column = 1
                                                end_column   = 65
                                                start_line   = 1
                                                end_line     = 20 ).

        lo_events = mo_select_list_popup->get_event( ).

        SET HANDLER on_select_list_link_click FOR lo_events.
        SET HANDLER on_select_list_function_click FOR lo_events.

        CREATE OBJECT lo_table_header
          EXPORTING
            text = i_header_text.

        mo_select_list_popup->set_top_of_list( lo_table_header ).

        lo_columns = mo_select_list_popup->get_columns( ).
        lo_columns->set_optimize( abap_true ).
        lt_columns = lo_columns->get( ).

        LOOP AT lt_columns INTO ls_column.

          IF ls_column-columnname = co_fieldname_selected.
            lo_column ?= ls_column-r_column.
            lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
            lo_column->set_output_length( 20 ).
            lo_column->set_short_text( |{ i_select_column_text }| ).
            lo_column->set_medium_text( |{ i_select_column_text }| ).
            lo_column->set_long_text( |{ i_select_column_text }| ).
            CONTINUE.
          ENDIF.

          READ TABLE it_columns_to_display TRANSPORTING NO FIELDS
                                           WITH KEY table_line = ls_column-columnname.
          IF sy-subrc <> 0.
            ls_column-r_column->set_technical( abap_true ).
          ENDIF.

        ENDLOOP.

        mo_select_list_popup->display( ).

      CATCH cx_salv_msg.
        zcx_abapgit_exception=>raise( 'Error from POPUP_SELECT_OBJ_OVERWRITE' ).
    ENDTRY.

    get_selected_rows(
      IMPORTING
        et_list = et_list ).

    CLEAR: mo_select_list_popup,
           mr_table,
           mo_table_descr.

  ENDMETHOD.

  METHOD create_new_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA: lr_struct       TYPE REF TO data,
          lt_components   TYPE cl_abap_structdescr=>component_table,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          struct_descr    TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <table>     TYPE STANDARD TABLE,
                   <component> TYPE abap_componentdescr,
                   <line>      TYPE data,
                   <data>      TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
    lt_components = lo_struct_descr->get_components( ).

    INSERT INITIAL LINE INTO lt_components ASSIGNING <component> INDEX 1.
    ASSERT sy-subrc = 0.

    <component>-name = co_fieldname_selected.
    <component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    struct_descr = cl_abap_structdescr=>create( p_components = lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( p_line_type = struct_descr ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE struct_descr.
    ASSIGN lr_struct->* TO <line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <data>.

      CLEAR: <line>.
      MOVE-CORRESPONDING <data> TO <line>.
      INSERT <line> INTO TABLE <table>.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_select_list_function_click.

    FIELD-SYMBOLS: <table>    TYPE STANDARD TABLE,
                   <line>     TYPE any,
                   <selected> TYPE flag.

    ASSIGN mr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    CASE e_salv_function.
      WHEN 'O.K.'.
        mo_select_list_popup->close_screen( ).

      WHEN 'ABR'.
        "Canceled: clear list to overwrite nothing
        CLEAR <table>.
        mo_select_list_popup->close_screen( ).

      WHEN 'SALL'.

        LOOP AT <table> ASSIGNING <line>.

          ASSIGN COMPONENT co_fieldname_selected
                 OF STRUCTURE <line>
                 TO <selected>.
          ASSERT sy-subrc = 0.

          <selected> = abap_true.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN 'DSEL'.

        LOOP AT <table> ASSIGNING <line>.

          ASSIGN COMPONENT co_fieldname_selected
                 OF STRUCTURE <line>
                 TO <selected>.
          ASSERT sy-subrc = 0.

          <selected> = abap_false.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN OTHERS.
        CLEAR <table>.
        mo_select_list_popup->close_screen( ).
    ENDCASE.

  ENDMETHOD.

  METHOD on_select_list_link_click.

    DATA: lv_line  TYPE sytabix.

    FIELD-SYMBOLS: <table>    TYPE STANDARD TABLE,
                   <line>     TYPE any,
                   <selected> TYPE flag.

    ASSIGN mr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    lv_line = row.

    READ TABLE <table> ASSIGNING <line>
                       INDEX lv_line.
    IF sy-subrc = 0.

      ASSIGN COMPONENT co_fieldname_selected
             OF STRUCTURE <line>
             TO <selected>.
      ASSERT sy-subrc = 0.

      IF <selected> = abap_true.
        <selected> = abap_false.
      ELSE.
        <selected> = abap_true.
      ENDIF.

    ENDIF.

    mo_select_list_popup->refresh( ).
  ENDMETHOD.


  METHOD get_selected_rows.

    DATA: lv_condition TYPE string,
          lr_exporting TYPE REF TO data.

    FIELD-SYMBOLS: <ls_exporting> TYPE any,
                   <table>        TYPE STANDARD TABLE,
                   <ls_line>      TYPE any.

    lv_condition = |{ co_fieldname_selected } = ABAP_TRUE|.

    ASSIGN mr_table->* TO <table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <ls_exporting>.

    LOOP AT <table> ASSIGNING <ls_line>
                    WHERE (lv_condition).

      CLEAR: <ls_exporting>.
      MOVE-CORRESPONDING <ls_line> TO <ls_exporting>.
      APPEND <ls_exporting> TO et_list.

    ENDLOOP.

  ENDMETHOD.


  METHOD extract_field_values.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF it_fields.

    CLEAR: ev_url,
           ev_package,
           ev_branch.

    READ TABLE it_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    ev_url = <ls_field>-value.

    READ TABLE it_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    ev_package = <ls_field>-value.
    TRANSLATE ev_package TO UPPER CASE.

    READ TABLE it_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    ev_branch = <ls_field>-value.

  ENDMETHOD.

ENDCLASS.
