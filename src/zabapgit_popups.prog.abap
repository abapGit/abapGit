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
        RETURNING VALUE(rv_package) TYPE devclass
        RAISING   lcx_exception,
      popup_object
        RETURNING VALUE(rs_tadir) TYPE tadir
        RAISING   lcx_exception,
      create_branch_popup
        EXPORTING ev_name   TYPE string
                  ev_cancel TYPE abap_bool
        RAISING   lcx_exception,
      run_page_class_popup
        EXPORTING ev_name   TYPE string
                  ev_cancel TYPE abap_bool
        RAISING   lcx_exception,
      repo_new_offline
        RETURNING VALUE(rs_popup) TYPE ty_popup
        RAISING   lcx_exception,
      branch_list_popup
        IMPORTING iv_url             TYPE string
                  iv_default_branch  TYPE string OPTIONAL
                  iv_show_new_option TYPE abap_bool OPTIONAL
        RETURNING VALUE(rs_branch)   TYPE lcl_git_branch_list=>ty_git_branch
        RAISING   lcx_exception,
      repo_popup
        IMPORTING iv_url            TYPE string
                  iv_package        TYPE devclass  OPTIONAL
                  iv_branch         TYPE string    DEFAULT 'refs/heads/master'
                  iv_freeze_package TYPE abap_bool OPTIONAL
                  iv_freeze_url     TYPE abap_bool OPTIONAL
                  iv_title          TYPE clike     DEFAULT 'Clone repository ...'
        RETURNING VALUE(rs_popup)   TYPE ty_popup
        RAISING   lcx_exception ##NO_TEXT,
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
        RAISING   lcx_exception,
      popup_to_inform
      IMPORTING
                titlebar              TYPE clike
                text_message          TYPE clike
      RAISING   lcx_exception.


ENDCLASS.

CLASS lcl_popups IMPLEMENTATION.

  DEFINE _add_dialog_fld.
    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = &1.                             "#EC NOTEXT
    <ls_field>-fieldname  = &2.                             "#EC NOTEXT
    <ls_field>-fieldtext  = &3.                             "#EC NOTEXT
    <ls_field>-value      = &4.                             "#EC NOTEXT
    <ls_field>-field_attr = &5.                             "#EC NOTEXT
  END-OF-DEFINITION.


  METHOD popup_object.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'TADIR'      'OBJECT'   'Type'    ''                  ''.
    _add_dialog_fld 'TADIR'      'OBJ_NAME' 'Name'    ''                  ''.

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
      lcx_exception=>raise( 'Error from POPUP_GET_VALUES' ).
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

    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Package' ''                  ''.

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
      lcx_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.

    rv_package = <ls_field>-value.

  ENDMETHOD.                    "popup_package_export

  METHOD create_branch_popup.

    DATA: lv_answer TYPE c LENGTH 1,
          lt_fields TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CLEAR: ev_name, ev_cancel.

*                   TAB     FLD   LABEL   DEF                       ATTR
    _add_dialog_fld 'TEXTL' 'LINE' 'Name' 'new_branch_name'         ''.

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
      lcx_exception=>raise( 'error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_answer = 'A'.
      ev_cancel = abap_true.
    ELSE.
      READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_name = lcl_git_branch_list=>complete_heads_branch_name( <ls_field>-value ).
    ENDIF.

  ENDMETHOD.

  METHOD run_page_class_popup.

    DATA: lv_answer TYPE c LENGTH 1,
          lt_fields TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CLEAR: ev_name, ev_cancel.

*                   TAB     FLD   LABEL   DEF                       ATTR
    _add_dialog_fld 'TEXTL' 'LINE' 'Name' 'lcl_gui_page_'          ''.

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
      lcx_exception=>raise( 'error from POPUP_GET_VALUES' ).
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
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'ABAPTXT255' 'LINE'     'Name'    ''                  ''.
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Package' ''                  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'New Offline Project'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from POPUP_GET_VALUES' ).
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
    rs_popup-package = <ls_field>-value.
    TRANSLATE rs_popup-package TO UPPER CASE.

  ENDMETHOD.                    "repo_new_offline

  METHOD branch_list_popup.

    DATA: lo_branches  TYPE REF TO lcl_git_branch_list,
          lt_branches  TYPE lcl_git_branch_list=>ty_git_branch_list_tt,
          lv_answer    TYPE c LENGTH 1,
          lv_default   TYPE i VALUE 1, "Default cursor position
          lt_selection TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel>    LIKE LINE OF lt_selection,
                   <ls_branch> LIKE LINE OF lt_branches.


    lo_branches = lcl_git_transport=>branches( iv_url ).

    lt_branches = lo_branches->get_branches_only( ).
    LOOP AT lt_branches ASSIGNING <ls_branch>.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = <ls_branch>-name.

      IF iv_default_branch IS NOT INITIAL AND iv_default_branch = <ls_branch>-name.
        lv_default = sy-tabix.
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
      lcx_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = 'A'. " cancel
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    IF iv_show_new_option = abap_true AND <ls_sel>-varoption = c_new_branch_label.
      rs_branch-name = c_new_branch_label.
    ELSE.
      rs_branch = lo_branches->find_by_name( <ls_sel>-varoption ).
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
          lv_icon2      TYPE icon-name.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

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

*                   TAB           FLD       LABEL            DEF        ATTR
    _add_dialog_fld 'ABAPTXT255' 'LINE'     'Git clone URL'  iv_url     lv_uattr.
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Target package' iv_package lv_pattr.
    _add_dialog_fld 'TEXTL'      'LINE'     'Branch'         iv_branch  '05'.

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
      lcx_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.
    IF lv_returncode = 'A'.
      rs_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-url = <ls_field>-value.
    lcl_url=>name( rs_popup-url ).         " validate

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-package = <ls_field>-value.
    TRANSLATE rs_popup-package TO UPPER CASE.

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-branch_name = <ls_field>-value.

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
      lcx_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
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

ENDCLASS.