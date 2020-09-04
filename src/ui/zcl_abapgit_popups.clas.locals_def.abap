*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_free_selections_dialog DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_title      TYPE syst_title OPTIONAL
                            iv_frame_text TYPE syst_title OPTIONAL,
      set_fields CHANGING ct_fields TYPE zcl_abapgit_popups=>ty_free_sel_field_tab,
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
      mr_fields     TYPE REF TO zcl_abapgit_popups=>ty_free_sel_field_tab,
      mv_title      TYPE syst_title,
      mv_frame_text TYPE syst_title.
ENDCLASS.
