*&---------------------------------------------------------------------*
*&  Include  zabapgit_requirements
*&---------------------------------------------------------------------*

"! Helper class for checking requirements / dependencies
CLASS lcl_requirement_helper DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_status,
        met               TYPE abap_bool,
        component         TYPE dlvunit,
        description       TYPE text80,
        installed_release TYPE saprelease,
        installed_patch   TYPE sappatchlv,
        required_release  TYPE saprelease,
        required_patch    TYPE sappatchlv,
      END OF gty_status,
      gty_status_tab TYPE STANDARD TABLE OF gty_status WITH DEFAULT KEY.
    CLASS-METHODS:
      "! Check if the given requirements are met with user interaction
      "! <p>
      "! Shows a popup if requested and asks the user if he wants to continue if there are unmet
      "! requirements. If not an exception is raised.
      "! </p>
      "! @parameter it_requirements | The requirements to check
      "! @parameter iv_show_popup | Show popup with requirements
      "! @raising lcx_exception | Cancelled by user or internal error
      check_requirements IMPORTING it_requirements TYPE lcl_dot_abapgit=>ty_requirement_tab
                                   iv_show_popup   TYPE abap_bool DEFAULT abap_true
                         RAISING   lcx_exception,
      "! Get a table with information about each requirement
      "! @parameter it_requirements | Requirements
      "! @parameter rt_status | Result
      "! @raising lcx_exception | Internal error
      get_requirement_met_status IMPORTING it_requirements  TYPE lcl_dot_abapgit=>ty_requirement_tab
                                 RETURNING VALUE(rt_status) TYPE gty_status_tab
                                 RAISING   lcx_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      show_requirement_popup IMPORTING it_requirements TYPE gty_status_tab
                             RAISING   lcx_exception,
      version_greater_or_equal IMPORTING is_status      TYPE gty_status
                               RETURNING VALUE(rv_true) TYPE abap_bool.
ENDCLASS.

CLASS lcl_requirement_helper IMPLEMENTATION.
  METHOD check_requirements.
    DATA: lt_met_status TYPE gty_status_tab,
          lv_answer     TYPE c LENGTH 1.

    lt_met_status = get_requirement_met_status( it_requirements ).

    IF iv_show_popup = abap_true.
      show_requirement_popup( lt_met_status ).
    ENDIF.

    LOOP AT lt_met_status TRANSPORTING NO FIELDS WHERE met = abap_false.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question = 'The project has unmet requirements. Install anyways?'
        IMPORTING
          answer        = lv_answer.
      IF lv_answer <> '1'.
        lcx_exception=>raise( 'Cancelling because of unmet requirements.' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_requirement_met_status.
    DATA: lt_installed TYPE STANDARD TABLE OF cvers_sdu.
    FIELD-SYMBOLS: <ls_requirement>    TYPE lcl_dot_abapgit=>ty_requirement,
                   <ls_status>         TYPE gty_status,
                   <ls_installed_comp> TYPE cvers_sdu.

    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ) ##NO_TEXT.
    ENDIF.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO rt_status ASSIGNING <ls_status>.
      <ls_status>-component = <ls_requirement>-component.
      <ls_status>-required_release = <ls_requirement>-min_release.
      <ls_status>-required_patch = <ls_requirement>-min_patch.

      READ TABLE lt_installed WITH KEY component = <ls_requirement>-component
                              ASSIGNING <ls_installed_comp>.
      IF sy-subrc = 0.
        <ls_status>-installed_release = <ls_installed_comp>-release.
        <ls_status>-installed_patch = <ls_installed_comp>-extrelease.
        <ls_status>-description = <ls_installed_comp>-desc_text.

        <ls_status>-met = version_greater_or_equal( <ls_status> ).
      ENDIF.

      UNASSIGN <ls_installed_comp>.
    ENDLOOP.
  ENDMETHOD.

  METHOD version_greater_or_equal.
    DATA: lv_number TYPE numc4.

    TRY.
        MOVE EXACT: is_status-installed_release TO lv_number,
                    is_status-installed_patch   TO lv_number,
                    is_status-required_release  TO lv_number,
                    is_status-required_patch    TO lv_number.
      CATCH cx_sy_conversion_error.
        " Cannot compare by number, assume requirement not fullfilled (user can force install
        " anyways if this was an error)
        rv_true = abap_false.
        RETURN.
    ENDTRY.

    " Check if versions are comparable by number
    IF is_status-installed_release > is_status-required_release
       OR ( is_status-installed_release = is_status-required_release
            AND ( is_status-required_patch IS INITIAL OR
                  is_status-installed_patch >= is_status-required_patch ) ).

      rv_true = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD show_requirement_popup.
    CONSTANTS: lc_color_column_name TYPE lvc_fname VALUE 'COLOR'.
    DATA: lo_alv              TYPE REF TO cl_salv_table,
          lo_column           TYPE REF TO cl_salv_column,
          lr_colored_table    TYPE REF TO data,
          lo_tab_descr        TYPE REF TO cl_abap_tabledescr,
          lo_struct_descr     TYPE REF TO cl_abap_structdescr,
          lo_enh_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_enh_tab_descr    TYPE REF TO cl_abap_tabledescr,
          lt_comps            TYPE cl_abap_structdescr=>component_table,
          lt_color_neg        TYPE lvc_t_scol,
          lt_color_pos        TYPE lvc_t_scol,
          ls_color            TYPE lvc_s_scol,
          lx_ex               TYPE REF TO cx_root.
    FIELD-SYMBOLS: <ls_comp>    TYPE abap_componentdescr,
                   <lt_pointer> TYPE STANDARD TABLE,
                   <lg_line>    TYPE data,
                   <lt_color>   TYPE lvc_t_scol,
                   <lv_met>     TYPE abap_bool.

    ls_color-color-col = col_negative.
    APPEND ls_color TO lt_color_neg.

    ls_color-color-col = col_positive.
    APPEND ls_color TO lt_color_pos.

    CLEAR ls_color.

    lo_tab_descr ?= cl_abap_typedescr=>describe_by_data( it_requirements ).
    lo_struct_descr ?= lo_tab_descr->get_table_line_type( ).
    lt_comps = lo_struct_descr->get_components( ).
    APPEND INITIAL LINE TO lt_comps ASSIGNING <ls_comp>.
    <ls_comp>-name = lc_color_column_name.
    <ls_comp>-type ?= cl_abap_typedescr=>describe_by_name( 'LVC_T_SCOL' ).
    lo_enh_struct_descr = cl_abap_structdescr=>get( lt_comps ).
    lo_enh_tab_descr = cl_abap_tabledescr=>get( lo_enh_struct_descr ).

    CREATE DATA lr_colored_table TYPE HANDLE lo_enh_tab_descr.
    ASSERT lr_colored_table IS BOUND.
    ASSIGN lr_colored_table->* TO <lt_pointer>.
    ASSERT <lt_pointer> IS ASSIGNED.

    MOVE-CORRESPONDING it_requirements TO <lt_pointer>.

    LOOP AT <lt_pointer> ASSIGNING <lg_line>.
      ASSIGN COMPONENT 'MET' OF STRUCTURE <lg_line> TO <lv_met>.
      ASSERT <lv_met> IS ASSIGNED.
      ASSIGN COMPONENT lc_color_column_name OF STRUCTURE <lg_line> TO <lt_color>.
      ASSERT <lt_color> IS ASSIGNED.
      IF <lv_met> = abap_false.
        <lt_color> = lt_color_neg.
      ELSE.
        <lt_color> = lt_color_pos.
      ENDIF.
      UNASSIGN: <lt_color>, <lv_met>.
    ENDLOOP.
    UNASSIGN <lg_line>.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_alv
          CHANGING
            t_table        = <lt_pointer>
        ).

        lo_alv->get_columns(:
          )->get_column( 'MET' )->set_short_text( 'Met' ),
          )->set_color_column( lc_color_column_name ),
          )->set_optimize( ),
        ).

        lo_column = lo_alv->get_columns( )->get_column( 'REQUIRED_RELEASE' ).
        lo_column->set_fixed_header_text( 'S').
        lo_column->set_short_text( 'Req. Rel.' ).

        lo_column = lo_alv->get_columns( )->get_column( 'REQUIRED_PATCH' ).
        lo_column->set_fixed_header_text( 'S').
        lo_column->set_short_text( 'Req. SP L.' ).

        lo_alv->set_screen_popup( start_column = 30
                                  end_column   = 100
                                  start_line   = 10
                                  end_line     = 20 ).
        lo_alv->get_display_settings( )->set_list_header( 'Requirements' ).
        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error INTO lx_ex.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            iv_text     = lx_ex->get_text( )
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
