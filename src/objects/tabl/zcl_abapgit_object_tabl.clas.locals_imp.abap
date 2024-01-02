CLASS lcl_tabl_xml DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING
        io_xml      TYPE REF TO zif_abapgit_xml_output
        is_internal TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS read
      IMPORTING
        io_xml            TYPE REF TO zif_abapgit_xml_input
      RETURNING
        VALUE(rs_internal) TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_tabl_xml IMPLEMENTATION.
  METHOD add.

* adding to xml must be done in the right sequence to avoid changes
    io_xml->add( iv_name = 'DD02V'
                 ig_data = is_internal-dd02v ).
    IF NOT is_internal-dd09l IS INITIAL.
      io_xml->add( iv_name = 'DD09L'
                   ig_data = is_internal-dd09l ).
    ENDIF.
    io_xml->add( iv_name = 'DD03P_TABLE'
                 ig_data = is_internal-dd03p ).
    io_xml->add( iv_name = 'DD05M_TABLE'
                 ig_data = is_internal-dd05m ).
    io_xml->add( iv_name = 'DD08V_TABLE'
                 ig_data = is_internal-dd08v ).
    io_xml->add( iv_name = 'DD12V'
                 ig_data = is_internal-dd12v ).
    io_xml->add( iv_name = 'DD17V'
                 ig_data = is_internal-dd17v ).
    io_xml->add( iv_name = 'DD35V_TALE'
                 ig_data = is_internal-dd35v ).
    io_xml->add( iv_name = 'DD36M'
                 ig_data = is_internal-dd36m ).

    IF lines( is_internal-i18n_langs ) > 0.
      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = is_internal-i18n_langs ).

      io_xml->add( iv_name = 'DD02_TEXTS'
                   ig_data = is_internal-dd02_texts ).
    ENDIF.

    io_xml->add( iv_name = 'LONGTEXTS'
                 ig_data = is_internal-longtexts ).

    io_xml->add( iv_name = zif_abapgit_object_tabl=>c_s_dataname-segment_definition
                 ig_data = is_internal-segment_definitions ).

    io_xml->add( iv_name = zif_abapgit_object_tabl=>c_s_dataname-tabl_extras
                 ig_data = is_internal-extras ).

  ENDMETHOD.

  METHOD read.

    io_xml->read(
      EXPORTING iv_name = zif_abapgit_object_tabl=>c_s_dataname-segment_definition
      CHANGING  cg_data = rs_internal-segment_definitions ).
    io_xml->read(
      EXPORTING iv_name = 'DD02V'
      CHANGING cg_data = rs_internal-dd02v ).
    io_xml->read(
      EXPORTING iv_name = 'DD09L'
      CHANGING cg_data = rs_internal-dd09l ).
    io_xml->read(
      EXPORTING iv_name  = 'DD03P_TABLE'
      CHANGING cg_data = rs_internal-dd03p ).
    io_xml->read(
      EXPORTING iv_name = 'DD05M_TABLE'
      CHANGING cg_data = rs_internal-dd05m ).
    io_xml->read(
      EXPORTING iv_name = 'DD08V_TABLE'
      CHANGING cg_data = rs_internal-dd08v ).
    io_xml->read(
      EXPORTING iv_name = 'DD35V_TALE'
      CHANGING cg_data = rs_internal-dd35v ).
    io_xml->read(
      EXPORTING iv_name = 'DD36M'
      CHANGING cg_data = rs_internal-dd36m ).
    io_xml->read(
      EXPORTING iv_name = zif_abapgit_object_tabl=>c_s_dataname-tabl_extras
      CHANGING cg_data = rs_internal-extras ).
    io_xml->read(
      EXPORTING iv_name = 'DD12V'
      CHANGING cg_data = rs_internal-dd12v ).
    io_xml->read(
      EXPORTING iv_name = 'DD17V'
      CHANGING cg_data = rs_internal-dd17v ).
    io_xml->read(
      EXPORTING iv_name = 'I18N_LANGS'
      CHANGING  cg_data = rs_internal-i18n_langs ).
    io_xml->read(
      EXPORTING iv_name = 'DD02_TEXTS'
      CHANGING  cg_data = rs_internal-dd02_texts ).
    io_xml->read(
      EXPORTING iv_name = 'LONGTEXTS'
      CHANGING  cg_data = rs_internal-longtexts ).

  ENDMETHOD.
ENDCLASS.
