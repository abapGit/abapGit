INTERFACE zif_abapgit_objects
  PUBLIC.

  TYPES:
    BEGIN OF ty_serialization,
      files TYPE zif_abapgit_definitions=>ty_files_tt,
      item  TYPE zif_abapgit_definitions=>ty_item,
    END OF ty_serialization .
  TYPES:
    BEGIN OF ty_deserialization,
      obj     TYPE REF TO zif_abapgit_object,
      xml     TYPE REF TO zif_abapgit_xml_input,
      package TYPE devclass,
      item    TYPE zif_abapgit_definitions=>ty_item,
    END OF ty_deserialization .
  TYPES:
    ty_deserialization_tt TYPE STANDARD TABLE OF ty_deserialization WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_step_data,
      step_id      TYPE zif_abapgit_definitions=>ty_deserialization_step,
      order        TYPE i,
      descr        TYPE string,
      is_ddic      TYPE abap_bool,
      syntax_check TYPE abap_bool,
      objects      TYPE ty_deserialization_tt,
    END OF ty_step_data .
  TYPES:
    ty_step_data_tt TYPE STANDARD TABLE OF ty_step_data
                                WITH DEFAULT KEY .

ENDINTERFACE.
