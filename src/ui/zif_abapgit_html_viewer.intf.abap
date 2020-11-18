INTERFACE zif_abapgit_html_viewer
  PUBLIC .


  CONSTANTS m_id_sapevent TYPE i VALUE 1 ##NO_TEXT.

  EVENTS sapevent
    EXPORTING
      VALUE(action) TYPE c OPTIONAL
      VALUE(frame) TYPE c OPTIONAL
      VALUE(getdata) TYPE c OPTIONAL
      VALUE(postdata) TYPE cnht_post_data_tab OPTIONAL
      VALUE(query_table) TYPE cnht_query_table OPTIONAL .

  METHODS load_data
    IMPORTING
      !iv_url          TYPE c OPTIONAL
      !iv_type         TYPE c DEFAULT 'text'
      !iv_subtype      TYPE c DEFAULT 'html'
      !iv_size         TYPE i DEFAULT 0
    EXPORTING
      !ev_assigned_url TYPE c
    CHANGING
      !ct_data_table   TYPE STANDARD TABLE
    EXCEPTIONS
      dp_invalid_parameter
      dp_error_general
      cntl_error
      html_syntax_notcorrect .
  METHODS set_registered_events
    IMPORTING
      !it_events TYPE cntl_simple_events
    EXCEPTIONS
      cntl_error
      cntl_system_error
      illegal_event_combination .
  METHODS show_url
    IMPORTING
      !iv_url TYPE c
    EXCEPTIONS
      cntl_error
      cnht_error_not_allowed
      cnht_error_parameter
      dp_error_general .
  METHODS free .
  METHODS close_document .
  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE w3url.
  METHODS back .
  METHODS set_visiblity IMPORTING iv_visible TYPE abap_bool.
ENDINTERFACE.
