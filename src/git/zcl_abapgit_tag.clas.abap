CLASS zcl_abapgit_tag DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      add_tag_prefix
        IMPORTING
          iv_text        TYPE csequence
        RETURNING
          VALUE(rv_text) TYPE string,

      remove_tag_prefix
        IMPORTING
          iv_text        TYPE string
        RETURNING
          VALUE(rv_text) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPGIT_TAG IMPLEMENTATION.


  METHOD add_tag_prefix.

    rv_text = zif_abapgit_definitions=>c_tag_prefix && iv_text.

  ENDMETHOD.


  METHOD remove_tag_prefix.

    rv_text = iv_text.

    REPLACE FIRST OCCURRENCE OF zif_abapgit_definitions=>c_tag_prefix
            IN rv_text
            WITH ''.

  ENDMETHOD.
ENDCLASS.
