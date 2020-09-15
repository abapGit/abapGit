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
      !url              TYPE c OPTIONAL
      !type             TYPE c DEFAULT 'text'
      !subtype          TYPE c DEFAULT 'html'
      !size             TYPE i DEFAULT 0
      !encoding         TYPE sychar60 OPTIONAL
      !charset          TYPE sychar60 OPTIONAL
      !needfiltering    TYPE i DEFAULT 0
      !language         TYPE lang OPTIONAL
      !i_tidyt          TYPE boolean OPTIONAL
    EXPORTING
      !assigned_url     TYPE c
    CHANGING
      !data_table       TYPE STANDARD TABLE
      !iscontentchanged TYPE boolean OPTIONAL
    EXCEPTIONS
      dp_invalid_parameter
      dp_error_general
      cntl_error
      html_syntax_notcorrect .
  METHODS set_registered_events
    IMPORTING
      !events TYPE cntl_simple_events
    EXCEPTIONS
      cntl_error
      cntl_system_error
      illegal_event_combination .
  METHODS show_url
    IMPORTING
      !url TYPE c
    EXCEPTIONS
      cntl_error
      cnht_error_not_allowed
      cnht_error_parameter
      dp_error_general .
  METHODS free .
  METHODS close_document .
ENDINTERFACE.
