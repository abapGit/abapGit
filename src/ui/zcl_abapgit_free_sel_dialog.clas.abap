"! Free Selections Dialog
CLASS zcl_abapgit_free_sel_dialog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_free_sel_field,
        name             TYPE fieldname,
        only_parameter   TYPE abap_bool,
        param_obligatory TYPE abap_bool,
        value            TYPE string,
        value_range      TYPE rsds_selopt_t,
        ddic_tabname     TYPE tabname,
        ddic_fieldname   TYPE fieldname,
        text             TYPE rsseltext,
      END OF ty_free_sel_field,
      ty_free_sel_field_tab TYPE STANDARD TABLE OF ty_free_sel_field WITH DEFAULT KEY.

    TYPES: ty_syst_title TYPE c LENGTH 70.

    METHODS:
      constructor IMPORTING iv_title      TYPE ty_syst_title OPTIONAL
                            iv_frame_text TYPE ty_syst_title OPTIONAL,
      set_fields CHANGING ct_fields TYPE ty_free_sel_field_tab,
      show RAISING zcx_abapgit_cancel
                   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_field_text_tab TYPE STANDARD TABLE OF rsdstexts WITH DEFAULT KEY.
    METHODS:
      convert_input_fields EXPORTING et_default_values TYPE rsds_trange
                                     es_restriction    TYPE sscr_restrict_ds
                                     et_fields         TYPE rsdsfields_t
                                     et_field_texts    TYPE ty_field_text_tab,
      free_selections_init IMPORTING it_default_values TYPE rsds_trange
                                     is_restriction    TYPE sscr_restrict_ds
                           EXPORTING ev_selection_id   TYPE dynselid
                           CHANGING  ct_fields         TYPE rsdsfields_t
                                     ct_field_texts    TYPE ty_field_text_tab
                           RAISING   zcx_abapgit_exception,
      free_selections_dialog IMPORTING iv_selection_id  TYPE dynselid
                             EXPORTING et_result_ranges TYPE rsds_trange
                             CHANGING  ct_fields        TYPE rsdsfields_t
                             RAISING   zcx_abapgit_cancel
                                       zcx_abapgit_exception,
      validate_results IMPORTING it_result_ranges TYPE rsds_trange
                       RAISING   zcx_abapgit_exception,
      transfer_results_to_input IMPORTING it_result_ranges TYPE rsds_trange.
    DATA:
      mr_fields     TYPE REF TO ty_free_sel_field_tab,
      mv_title      TYPE ty_syst_title,
      mv_frame_text TYPE ty_syst_title.
ENDCLASS.



CLASS ZCL_ABAPGIT_FREE_SEL_DIALOG IMPLEMENTATION.


  METHOD constructor.
    mv_title = iv_title.
    mv_frame_text = iv_frame_text.
  ENDMETHOD.


  METHOD convert_input_fields.
    CONSTANTS: lc_only_eq_optlist_name TYPE c LENGTH 10 VALUE 'ONLYEQ'.
    DATA: ls_parameter_opt_list TYPE sscr_opt_list.
    FIELD-SYMBOLS: <ls_input_field>            TYPE ty_free_sel_field,
                   <lt_input_fields>           TYPE ty_free_sel_field_tab,
                   <ls_free_sel_field>         TYPE rsdsfields,
                   <ls_restriction_ass>        TYPE sscr_ass_ds,
                   <ls_text>                   TYPE rsdstexts,
                   <ls_default_value>          TYPE rsds_range,
                   <ls_default_value_range>    TYPE rsds_frange,
                   <ls_default_val_range_line> TYPE rsdsselopt.

    ASSERT mr_fields IS BOUND.
    ASSIGN mr_fields->* TO <lt_input_fields>.

    LOOP AT <lt_input_fields> ASSIGNING <ls_input_field>.
      APPEND INITIAL LINE TO et_fields ASSIGNING <ls_free_sel_field>.
      <ls_free_sel_field>-fieldname = <ls_input_field>-ddic_fieldname.
      <ls_free_sel_field>-tablename = <ls_input_field>-ddic_tabname.

      IF <ls_input_field>-only_parameter = abap_true.
        IF es_restriction IS INITIAL.
          ls_parameter_opt_list-name = lc_only_eq_optlist_name.
          ls_parameter_opt_list-options-eq = abap_true.
          APPEND ls_parameter_opt_list TO es_restriction-opt_list_tab.
        ENDIF.

        APPEND INITIAL LINE TO es_restriction-ass_tab ASSIGNING <ls_restriction_ass>.
        <ls_restriction_ass>-kind = 'S'.
        <ls_restriction_ass>-fieldname = <ls_input_field>-ddic_fieldname.
        <ls_restriction_ass>-tablename = <ls_input_field>-ddic_tabname.
        <ls_restriction_ass>-sg_main = 'I'.
        <ls_restriction_ass>-sg_addy = 'N'.
        <ls_restriction_ass>-op_main = lc_only_eq_optlist_name.
      ENDIF.

      IF <ls_input_field>-text IS NOT INITIAL.
        APPEND INITIAL LINE TO et_field_texts ASSIGNING <ls_text>.
        <ls_text>-fieldname = <ls_input_field>-ddic_fieldname.
        <ls_text>-tablename = <ls_input_field>-ddic_tabname.
        <ls_text>-text = <ls_input_field>-text.
      ENDIF.

      IF <ls_input_field>-value IS NOT INITIAL OR <ls_input_field>-value_range IS NOT INITIAL.
        READ TABLE et_default_values WITH KEY tablename = <ls_input_field>-ddic_tabname
                                     ASSIGNING <ls_default_value>.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO et_default_values ASSIGNING <ls_default_value>.
          <ls_default_value>-tablename = <ls_input_field>-ddic_tabname.
        ENDIF.

        APPEND INITIAL LINE TO <ls_default_value>-frange_t ASSIGNING <ls_default_value_range>.
        <ls_default_value_range>-fieldname = <ls_input_field>-ddic_fieldname.

        IF <ls_input_field>-value IS NOT INITIAL.
          APPEND INITIAL LINE TO <ls_default_value_range>-selopt_t ASSIGNING <ls_default_val_range_line>.
          <ls_default_val_range_line>-sign = 'I'.
          <ls_default_val_range_line>-option = 'EQ'.
          <ls_default_val_range_line>-low = <ls_input_field>-value.
        ELSEIF <ls_input_field>-value_range IS NOT INITIAL.
          <ls_default_value_range>-selopt_t = <ls_input_field>-value_range.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD free_selections_dialog.
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = iv_selection_id
        title           = mv_title
        frame_text      = mv_frame_text
        status          = 1
        as_window       = abap_true
        no_intervals    = abap_true
        tree_visible    = abap_false
      IMPORTING
        field_ranges    = et_result_ranges
      TABLES
        fields_tab      = ct_fields
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.
    CASE sy-subrc.
      WHEN 0 ##NEEDED.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Error from FREE_SELECTIONS_DIALOG: { sy-subrc }| ).
    ENDCASE.
  ENDMETHOD.


  METHOD free_selections_init.
    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'F'
        field_ranges_int         = it_default_values
        restriction              = is_restriction
      IMPORTING
        selection_id             = ev_selection_id
      TABLES
        fields_tab               = ct_fields
        field_texts              = ct_field_texts
      EXCEPTIONS
        fields_incomplete        = 1
        fields_no_join           = 2
        field_not_found          = 3
        no_tables                = 4
        table_not_found          = 5
        expression_not_supported = 6
        incorrect_expression     = 7
        illegal_kind             = 8
        area_not_found           = 9
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13
        dup_field                = 14
        field_no_type            = 15
        field_ill_type           = 16
        dup_event_field          = 17
        node_not_in_ldb          = 18
        area_no_field            = 19
        OTHERS                   = 20.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from FREE_SELECTIONS_INIT: { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD set_fields.
    GET REFERENCE OF ct_fields INTO mr_fields.
  ENDMETHOD.


  METHOD show.
    DATA: lt_default_values   TYPE rsds_trange,
          ls_restriction      TYPE sscr_restrict_ds,
          lt_fields           TYPE rsdsfields_t,
          lt_field_texts      TYPE ty_field_text_tab,
          lv_repeat_dialog    TYPE abap_bool VALUE abap_true,
          lv_selection_id     TYPE dynselid,
          lt_results          TYPE rsds_trange,
          lx_validation_error TYPE REF TO zcx_abapgit_exception.

    convert_input_fields(
      IMPORTING
        et_default_values = lt_default_values
        es_restriction    = ls_restriction
        et_fields         = lt_fields
        et_field_texts    = lt_field_texts ).

    WHILE lv_repeat_dialog = abap_true.
      lv_repeat_dialog = abap_false.

      free_selections_init(
        EXPORTING
          it_default_values = lt_default_values
          is_restriction    = ls_restriction
        IMPORTING
          ev_selection_id   = lv_selection_id
        CHANGING
          ct_fields         = lt_fields
          ct_field_texts    = lt_field_texts ).

      free_selections_dialog(
        EXPORTING
          iv_selection_id  = lv_selection_id
        IMPORTING
          et_result_ranges = lt_results
        CHANGING
          ct_fields        = lt_fields ).

      TRY.
          validate_results( lt_results ).
        CATCH zcx_abapgit_exception INTO lx_validation_error.
          lv_repeat_dialog = abap_true.
          lt_default_values = lt_results.
          MESSAGE lx_validation_error TYPE 'I' DISPLAY LIKE 'E'.
          CONTINUE.
      ENDTRY.

      transfer_results_to_input( lt_results ).
    ENDWHILE.
  ENDMETHOD.


  METHOD transfer_results_to_input.
    FIELD-SYMBOLS: <ls_input_field>          TYPE ty_free_sel_field,
                   <lt_input_fields>         TYPE ty_free_sel_field_tab,
                   <ls_result_range_for_tab> TYPE rsds_range,
                   <ls_result_range_line>    TYPE rsds_frange,
                   <ls_selopt_line>          TYPE rsdsselopt.

    ASSIGN mr_fields->* TO <lt_input_fields>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_input_fields> ASSIGNING <ls_input_field>.
      READ TABLE it_result_ranges WITH KEY tablename = <ls_input_field>-ddic_tabname
                                  ASSIGNING <ls_result_range_for_tab>.
      IF sy-subrc = 0.
        READ TABLE <ls_result_range_for_tab>-frange_t WITH KEY fieldname = <ls_input_field>-ddic_fieldname
                                                      ASSIGNING <ls_result_range_line>.
        IF sy-subrc = 0 AND <ls_result_range_line>-selopt_t IS NOT INITIAL.
          IF <ls_input_field>-only_parameter = abap_true.
            ASSERT lines( <ls_result_range_line>-selopt_t ) = 1.

            READ TABLE <ls_result_range_line>-selopt_t INDEX 1 ASSIGNING <ls_selopt_line>.
            ASSERT sy-subrc = 0.

            ASSERT <ls_selopt_line>-sign = 'I' AND
                   <ls_selopt_line>-option = 'EQ' AND
                   <ls_selopt_line>-high IS INITIAL.

            <ls_input_field>-value = <ls_selopt_line>-low.
          ELSE.
            <ls_input_field>-value_range = <ls_result_range_line>-selopt_t.
          ENDIF.
        ELSE.
          CLEAR: <ls_input_field>-value, <ls_input_field>-value_range.
        ENDIF.
      ELSE.
        CLEAR: <ls_input_field>-value, <ls_input_field>-value_range.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_results.
    DATA: ls_error_msg      TYPE symsg,
          lv_ddut_fieldname TYPE fnam_____4,
          lv_value          TYPE rsdsselop_.
    FIELD-SYMBOLS: <ls_result_range_for_tab> TYPE rsds_range,
                   <ls_result_range_line>    TYPE rsds_frange,
                   <ls_input_field>          TYPE ty_free_sel_field,
                   <lt_input_fields>         TYPE ty_free_sel_field_tab,
                   <ls_selopt_line>          TYPE rsdsselopt.

    ASSIGN mr_fields->* TO <lt_input_fields>.
    ASSERT sy-subrc = 0.

    LOOP AT it_result_ranges ASSIGNING <ls_result_range_for_tab>.
      LOOP AT <ls_result_range_for_tab>-frange_t ASSIGNING <ls_result_range_line>.
        READ TABLE <lt_input_fields> WITH KEY ddic_tabname = <ls_result_range_for_tab>-tablename
                                              ddic_fieldname = <ls_result_range_line>-fieldname
                                     ASSIGNING <ls_input_field>.
        ASSERT sy-subrc = 0.
        IF <ls_input_field>-only_parameter = abap_false.
          CONTINUE.
        ENDIF.

        CASE lines( <ls_result_range_line>-selopt_t ).
          WHEN 0.
            CLEAR lv_value.
          WHEN 1.
            READ TABLE <ls_result_range_line>-selopt_t INDEX 1 ASSIGNING <ls_selopt_line>.
            ASSERT sy-subrc = 0.
            lv_value = <ls_selopt_line>-low.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

        CLEAR ls_error_msg.
        lv_ddut_fieldname = <ls_input_field>-ddic_fieldname.

        CALL FUNCTION 'DDUT_INPUT_CHECK'
          EXPORTING
            tabname            = <ls_input_field>-ddic_tabname
            fieldname          = lv_ddut_fieldname
            value              = lv_value
            accept_all_initial = abap_true
            value_list         = 'S'
          IMPORTING
            msgid              = ls_error_msg-msgid
            msgty              = ls_error_msg-msgty
            msgno              = ls_error_msg-msgno
            msgv1              = ls_error_msg-msgv1
            msgv2              = ls_error_msg-msgv2
            msgv3              = ls_error_msg-msgv3
            msgv4              = ls_error_msg-msgv4.
        IF ls_error_msg IS NOT INITIAL.
          zcx_abapgit_exception=>raise_t100(
            iv_msgid = ls_error_msg-msgid
            iv_msgno = ls_error_msg-msgno
            iv_msgv1 = ls_error_msg-msgv1
            iv_msgv2 = ls_error_msg-msgv2
            iv_msgv3 = ls_error_msg-msgv3
            iv_msgv4 = ls_error_msg-msgv4 ).
        ELSEIF <ls_input_field>-param_obligatory = abap_true AND lv_value IS INITIAL.
          zcx_abapgit_exception=>raise( |Field '{ <ls_input_field>-name }' is obligatory| ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
