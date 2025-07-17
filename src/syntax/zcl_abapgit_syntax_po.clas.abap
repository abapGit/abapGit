CLASS zcl_abapgit_syntax_po DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_style,
        msgid    TYPE string VALUE 'keyword',
        msgstr   TYPE string VALUE 'xml_tag',
        comment  TYPE string VALUE 'comment',
      END OF c_style.
    CONSTANTS:
      BEGIN OF c_token,
        msgid    TYPE c VALUE 'I',
        msgstr   TYPE c VALUE 'S',
        comment  TYPE c VALUE 'C',
      END OF c_token.
    CONSTANTS:
      BEGIN OF c_regex,
        msgid    TYPE string VALUE '^msgid\b',
        msgstr   TYPE string VALUE '^msgstr\b',
        comment  TYPE string VALUE '^#.*',
      END OF c_regex.

ENDCLASS.



CLASS ZCL_ABAPGIT_SYNTAX_PO IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    add_rule(
      iv_regex = c_regex-msgid
      iv_token = c_token-msgid
      iv_style = c_style-msgid ).

    add_rule(
      iv_regex = c_regex-msgstr
      iv_token = c_token-msgstr
      iv_style = c_style-msgstr ).

    add_rule(
      iv_regex = c_regex-comment
      iv_token = c_token-comment
      iv_style = c_style-comment ).

    " TODO maybe add rule to highlight empty msgstr with red

  ENDMETHOD.
ENDCLASS.
