CLASS zcl_abapgit_requirement_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS requirements_popup
      IMPORTING
        !it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_requirements_met
      IMPORTING
        !it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt
      RETURNING
        VALUE(rv_status) TYPE zif_abapgit_definitions=>ty_yes_no
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_requirement_status,
        met               TYPE abap_bool,
        component         TYPE tdevc-dlvunit,
        description       TYPE string,
        installed_release TYPE saprelease,
        installed_patch   TYPE sappatchlv,
        required_release  TYPE saprelease,
        required_patch    TYPE sappatchlv,
      END OF ty_requirement_status .
    TYPES:
      ty_requirement_status_tt TYPE STANDARD TABLE OF ty_requirement_status WITH DEFAULT KEY .

    CLASS-METHODS show_requirement_popup
      IMPORTING
        !it_requirements TYPE ty_requirement_status_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_requirement_met_status
      IMPORTING
        !it_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt
      RETURNING
        VALUE(rt_status) TYPE ty_requirement_status_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_version_greater_or_equal
      IMPORTING
        !is_status     TYPE ty_requirement_status
      RETURNING
        VALUE(rv_true) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ABAPGIT_REQUIREMENT_HELPER IMPLEMENTATION.


  METHOD get_requirement_met_status.

    DATA: lt_installed TYPE STANDARD TABLE OF cvers_sdu.

    FIELD-SYMBOLS: <ls_requirement>    TYPE zif_abapgit_dot_abapgit=>ty_requirement,
                   <ls_status>         TYPE ty_requirement_status,
                   <ls_installed_comp> TYPE cvers_sdu.


    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ).
    ENDIF.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO rt_status ASSIGNING <ls_status>.
      <ls_status>-component = <ls_requirement>-component.
      <ls_status>-required_release = <ls_requirement>-min_release.
      <ls_status>-required_patch = <ls_requirement>-min_patch.

      READ TABLE lt_installed WITH KEY component = <ls_requirement>-component
                              ASSIGNING <ls_installed_comp>.
      IF sy-subrc = 0.
        " Component is installed, requirement is met if the installed version is greater or equal
        " to the required one.
        <ls_status>-installed_release = <ls_installed_comp>-release.
        <ls_status>-installed_patch = <ls_installed_comp>-extrelease.
        <ls_status>-description = <ls_installed_comp>-desc_text.
        <ls_status>-met = is_version_greater_or_equal( <ls_status> ).
      ELSE.
        " Component is not installed at all
        <ls_status>-met = abap_false.
      ENDIF.

      UNASSIGN <ls_installed_comp>.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_requirements_met.

    DATA: lt_met_status TYPE ty_requirement_status_tt.

    lt_met_status = get_requirement_met_status( it_requirements ).

    READ TABLE lt_met_status TRANSPORTING NO FIELDS WITH KEY met = abap_false.
    IF sy-subrc = 0.
      rv_status = zif_abapgit_definitions=>c_no.
    ELSE.
      rv_status = zif_abapgit_definitions=>c_yes.
    ENDIF.

  ENDMETHOD.


  METHOD is_version_greater_or_equal.

    DATA:
      lv_installed_release TYPE n LENGTH 4,
      lv_installed_patch   TYPE n LENGTH 4,
      lv_required_release  TYPE n LENGTH 4,
      lv_required_patch    TYPE n LENGTH 4.

    TRY.
        MOVE EXACT: is_status-installed_release TO lv_installed_release,
                    is_status-installed_patch   TO lv_installed_patch,
                    is_status-required_release  TO lv_required_release,
                    is_status-required_patch    TO lv_required_patch.
      CATCH cx_sy_conversion_error.
        " Cannot compare by number, assume requirement not fullfilled (user can force install
        " anyways if this was an error)
        rv_true = abap_false.
        RETURN.
    ENDTRY.

    " Versions are comparable by number, compare release and if necessary patch level
    IF lv_installed_release > lv_required_release
        OR ( lv_installed_release = lv_required_release
         AND ( lv_required_patch = 0
            OR lv_installed_patch >= lv_required_patch ) ).

      rv_true = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD requirements_popup.

    DATA: lt_met_status TYPE ty_requirement_status_tt,
          lv_answer     TYPE c LENGTH 1.


    lt_met_status = get_requirement_met_status( it_requirements ).

    show_requirement_popup( lt_met_status ).

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Warning'
      iv_text_question = 'The project has unmet requirements. Do you want to continue?' ).

    IF lv_answer <> '1'.
      zcx_abapgit_exception=>raise( 'Cancelling because of unmet requirements.' ).
    ENDIF.

  ENDMETHOD.


  METHOD show_requirement_popup.

    TYPES: BEGIN OF ty_color_line,
             color TYPE lvc_t_scol.
             INCLUDE TYPE ty_requirement_status.
    TYPES: END OF ty_color_line.

    TYPES: ty_color_tab TYPE STANDARD TABLE OF ty_color_line WITH DEFAULT KEY.

    DATA: lo_alv            TYPE REF TO cl_salv_table,
          lo_column         TYPE REF TO cl_salv_column,
          lo_columns        TYPE REF TO cl_salv_columns_table,
          lt_color_table    TYPE ty_color_tab,
          lt_color_negative TYPE lvc_t_scol,
          lt_color_positive TYPE lvc_t_scol,
          ls_color          TYPE lvc_s_scol,
          ls_position       TYPE zif_abapgit_popups=>ty_popup_position,
          lx_ex             TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_line>        TYPE ty_color_line,
                   <ls_requirement> LIKE LINE OF it_requirements.


    ls_color-color-col = col_negative.
    APPEND ls_color TO lt_color_negative.

    ls_color-color-col = col_positive.
    APPEND ls_color TO lt_color_positive.

    CLEAR ls_color.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO lt_color_table ASSIGNING <ls_line>.
      MOVE-CORRESPONDING <ls_requirement> TO <ls_line>.
    ENDLOOP.

    LOOP AT lt_color_table ASSIGNING <ls_line>.
      IF <ls_line>-met = abap_false.
        <ls_line>-color = lt_color_negative.
      ELSE.
        <ls_line>-color = lt_color_positive.
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_line>.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING t_table       = lt_color_table ).

        lo_columns = lo_alv->get_columns( ).
        lo_columns->get_column( 'MET' )->set_short_text( 'Met' ).
        lo_columns->set_color_column( 'COLOR' ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'REQUIRED_RELEASE' ).
        lo_column->set_short_text( 'Req. Rel.' ).

        lo_column = lo_columns->get_column( 'REQUIRED_PATCH' ).
        lo_column->set_short_text( 'Req. SP L.' ).

        ls_position = zcl_abapgit_popups=>center(
          iv_width  = 70
          iv_height = 10 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_alv->get_display_settings( )->set_list_header( 'Requirements' ).
        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error INTO lx_ex.
        zcx_abapgit_exception=>raise( lx_ex->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
