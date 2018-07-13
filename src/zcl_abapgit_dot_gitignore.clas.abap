CLASS zcl_abapgit_dot_gitignore DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      deserialize
        IMPORTING
          !iv_xstr              TYPE xstring
        RETURNING
          VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_gitignore
        RAISING
          zcx_abapgit_exception .

    METHODS:
      constructor
        IMPORTING
          !it_gitignore TYPE zif_abapgit_definitions=>tty_dot_gitignore,

      serialize
        RETURNING
          VALUE(rv_xstr) TYPE xstring
        RAISING
          zcx_abapgit_exception,

      get_data
        RETURNING
          VALUE(rt_gitignore) TYPE zif_abapgit_definitions=>tty_dot_gitignore,

      is_ignored
        IMPORTING
          !iv_path          TYPE string
          !iv_filename      TYPE string
        RETURNING
          VALUE(rv_ignored) TYPE abap_bool,

      get_signature
        RETURNING
          VALUE(rs_signature) TYPE zif_abapgit_definitions=>ty_file_signature
        RAISING
          zcx_abapgit_exception .

  PRIVATE SECTION.
    CLASS-METHODS:
      from_text
        IMPORTING
          iv_text        TYPE string
        RETURNING
          VALUE(rt_data) TYPE zif_abapgit_definitions=>tty_dot_gitignore.

    DATA:
      mt_gitignore TYPE zif_abapgit_definitions=>tty_dot_gitignore.

    METHODS:
      to_text
        IMPORTING
          it_gitignore   TYPE stringtab
        RETURNING
          VALUE(rv_text) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_dot_gitignore IMPLEMENTATION.


  METHOD constructor.
    mt_gitignore = it_gitignore.
  ENDMETHOD.


  METHOD deserialize.

    DATA: lv_text TYPE string,
          lt_data TYPE zif_abapgit_definitions=>tty_dot_gitignore.

    lv_text = zcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    lt_data = from_text( lv_text ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        it_gitignore = lt_data.

  ENDMETHOD.


  METHOD from_text.

    rt_data = zcl_abapgit_convert=>split_string( iv_text ).

  ENDMETHOD.


  METHOD get_data.
    rt_gitignore = mt_gitignore.
  ENDMETHOD.


  METHOD get_signature.

    rs_signature-path     = zif_abapgit_definitions=>gc_root_dir.
    rs_signature-filename = zif_abapgit_definitions=>gc_dot_gitignore.
    rs_signature-sha1     = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>gc_type-blob
                                                    iv_data = serialize( ) ).


  ENDMETHOD.


  METHOD is_ignored.

    DATA: lv_name    TYPE string,
          lv_pattern TYPE string.

    FIELD-SYMBOLS: <lv_pattern> TYPE LINE OF stringtab.

    LOOP AT mt_gitignore ASSIGNING <lv_pattern>.

      lv_name = iv_path && iv_filename.

      lv_pattern = <lv_pattern>.

      IF lv_pattern(1) <> zif_abapgit_definitions=>gc_root_dir.
        lv_pattern = |*{ lv_pattern }|.
      ENDIF.

      IF lv_name CP lv_pattern.
        rv_ignored = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize.

    DATA: lv_text TYPE string.

    lv_text = to_text( mt_gitignore ).

    rv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( lv_text ).

  ENDMETHOD.


  METHOD to_text.

    IF lines( it_gitignore ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE LINES OF it_gitignore
                INTO rv_text
                SEPARATED BY cl_abap_char_utilities=>cr_lf.

    rv_text = rv_text && cl_abap_char_utilities=>cr_lf.

  ENDMETHOD.
ENDCLASS.
