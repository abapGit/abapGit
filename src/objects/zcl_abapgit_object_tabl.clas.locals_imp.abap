CLASS lcl_tabl_xml DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING
        io_xml      TYPE REF TO zif_abapgit_xml_output
        is_internal TYPE ty_internal
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_tabl_xml IMPLEMENTATION.
  METHOD add.
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
  ENDMETHOD.
ENDCLASS.
