CLASS zcl_abapgit_html_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add_part
      IMPORTING
        !iv_collection TYPE string
        !ii_part TYPE REF TO zif_abapgit_html .
    METHODS get_parts
      IMPORTING
        !iv_collection TYPE string
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_html=>ty_table_of .
    METHODS get_collection_names
      RETURNING
        VALUE(rt_list) TYPE string_table .
    METHODS get_collection_size
      IMPORTING
        !iv_collection TYPE string
      RETURNING
        VALUE(rv_size) TYPE i .
    METHODS clear.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_named_collection,
        name TYPE string,
        pile TYPE zif_abapgit_html=>ty_table_of,
      END OF ty_named_collection.
    TYPES:
      ty_named_collections TYPE STANDARD TABLE OF ty_named_collection WITH KEY name.

    DATA mt_part_collections TYPE ty_named_collections.

    METHODS get_collection
      IMPORTING
        !iv_collection TYPE string
        !iv_create_if_missing TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rr_collection) TYPE REF TO ty_named_collection .

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_PARTS IMPLEMENTATION.


  METHOD add_part.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection(
      iv_collection = iv_collection
      iv_create_if_missing = abap_true ).
    APPEND ii_part TO lr_collection->pile.

  ENDMETHOD.


  METHOD clear.
    CLEAR mt_part_collections.
  ENDMETHOD.


  METHOD get_collection.

    READ TABLE mt_part_collections REFERENCE INTO rr_collection WITH KEY name = iv_collection.
    IF sy-subrc <> 0 AND iv_create_if_missing = abap_true.
      APPEND INITIAL LINE TO mt_part_collections REFERENCE INTO rr_collection.
      rr_collection->name = iv_collection.
    ENDIF.

  ENDMETHOD.


  METHOD get_collection_names.

    FIELD-SYMBOLS <ls_coll> LIKE LINE OF mt_part_collections.
    LOOP AT mt_part_collections ASSIGNING <ls_coll>.
      APPEND <ls_coll>-name TO rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_collection_size.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection( iv_collection ).
    IF lr_collection IS BOUND.
      rv_size = lines( lr_collection->pile ).
    ENDIF.

  ENDMETHOD.


  METHOD get_parts.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection( iv_collection ).
    IF lr_collection IS BOUND.
      rt_parts = lr_collection->pile.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
