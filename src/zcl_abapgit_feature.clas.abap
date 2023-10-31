CLASS zcl_abapgit_feature DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " For dependency injection/testing, use the following
    " zcl_abapgit_persist_factory=>get_settings( )->read( )->set_experimental_features( )

    CLASS-METHODS is_enabled
      IMPORTING
        !iv_feature   TYPE string OPTIONAL
      RETURNING
        VALUE(rv_run) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_feature IMPLEMENTATION.


  METHOD is_enabled.

    DATA:
      lv_features TYPE string,
      lt_features TYPE string_table.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      RETURN.
    ENDIF.

    lv_features = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ).
    CONDENSE lv_features NO-GAPS.

    rv_run = boolc( lv_features = abap_true ).

    IF iv_feature IS NOT INITIAL.
      SPLIT lv_features AT ',' INTO TABLE lt_features.
      READ TABLE lt_features TRANSPORTING NO FIELDS WITH TABLE KEY table_line = iv_feature.
      rv_run = boolc( rv_run = abap_true OR sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
