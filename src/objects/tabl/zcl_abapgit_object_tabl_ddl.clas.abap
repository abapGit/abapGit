CLASS zcl_abapgit_object_tabl_ddl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read_data
      IMPORTING
        !iv_name       TYPE tadir-obj_name
        !iv_language   TYPE sy-langu DEFAULT sy-langu
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS serialize
      IMPORTING
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize
      IMPORTING
        !iv_ddl        TYPE string
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_adt
      IMPORTING
        !iv_name      TYPE tadir-obj_name
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        cx_static_check .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_token,
             value  TYPE string,
             offset TYPE i,
           END OF ty_token.
    TYPES ty_tokens TYPE STANDARD TABLE OF ty_token WITH DEFAULT KEY.
    TYPES ty_fields TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS tokenize
      IMPORTING
        !iv_ddl          TYPE string
      RETURNING
        VALUE(rt_tokens) TYPE ty_tokens
      RAISING
        zcx_abapgit_exception .
    METHODS parse_error
      IMPORTING
        !iv_context TYPE clike
        !iv_token   TYPE clike
        !iv_offset  TYPE i
      RAISING
        zcx_abapgit_exception .
    METHODS get_replacement_object
      IMPORTING
        !iv_viewref      TYPE clike
      RETURNING
        VALUE(rv_object) TYPE string .
    METHODS get_replacement_view
      IMPORTING
        !iv_entityname     TYPE clike
      RETURNING
        VALUE(rv_viewname) TYPE string .
    METHODS parse_replacement_object
      IMPORTING
        !iv_value TYPE clike
        !iv_name  TYPE clike
      CHANGING
        !cs_data  TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS parse_top_annotations
      IMPORTING
        !it_tokens TYPE ty_tokens
      CHANGING
        !cv_index  TYPE i
        !cs_data   TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS parse_field_annotations
      IMPORTING
        !it_tokens TYPE ty_tokens
      CHANGING
        !cv_index  TYPE i
        !cs_dd03p  TYPE dd03p
        !cs_dd08v  TYPE dd08v
      RAISING
        zcx_abapgit_exception .
    METHODS parse_field
      IMPORTING
        !it_tokens TYPE ty_tokens
      CHANGING
        !cv_index  TYPE i
        !cs_data   TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS parse_include
      IMPORTING
        !it_tokens TYPE ty_tokens
      CHANGING
        !cv_index  TYPE i
        !cs_dd03p  TYPE dd03p
      RAISING
        zcx_abapgit_exception .
    METHODS parse_foreign_key
      IMPORTING
        !it_tokens    TYPE ty_tokens
        !iv_fieldname TYPE clike
      CHANGING
        !cv_index     TYPE i
        !cs_dd08v     TYPE dd08v
        !cs_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS parse_value_help
      IMPORTING
        !it_tokens    TYPE ty_tokens
        !iv_fieldname TYPE clike
      CHANGING
        !cv_index     TYPE i
        !cs_dd35v     TYPE dd35v
        !cs_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RAISING
        zcx_abapgit_exception .
    METHODS parse_extension
      IMPORTING
        !it_tokens TYPE ty_tokens
      CHANGING
        !cv_index  TYPE i
        !cs_data   TYPE zif_abapgit_object_tabl=>ty_internal
        !cs_dd08v  TYPE dd08v
        !cs_dd35v  TYPE dd35v
      RAISING
        zcx_abapgit_exception .
    METHODS parse_cardinality
      IMPORTING
        !iv_token  TYPE clike
        !iv_offset TYPE i
      CHANGING
        !cs_dd08v  TYPE dd08v
      RAISING
        zcx_abapgit_exception .
    METHODS parse_type
      IMPORTING
        !iv_token TYPE string
      CHANGING
        !cs_dd03p TYPE dd03p
      RAISING
        zcx_abapgit_exception .
    METHODS set_builtin_type
      IMPORTING
        !iv_base     TYPE string
        !iv_length   TYPE string
        !iv_decimals TYPE string
        !iv_offset   TYPE i
      CHANGING
        !cs_dd03p    TYPE dd03p
      RAISING
        zcx_abapgit_exception .
    METHODS set_character_type
      IMPORTING
        !iv_base  TYPE string
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS set_numeric_type
      IMPORTING
        !iv_base     TYPE string
        !iv_length   TYPE string
        !iv_decimals TYPE string
      CHANGING
        !cs_dd03p    TYPE dd03p .
    METHODS set_integer_type
      IMPORTING
        !iv_base  TYPE string
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS set_date_type
      IMPORTING
        !iv_base  TYPE string
      CHANGING
        !cs_dd03p TYPE dd03p .
    METHODS set_decfloat_type
      IMPORTING
        !iv_base     TYPE string
        !iv_length   TYPE string
        !iv_decimals TYPE string
      CHANGING
        !cs_dd03p    TYPE dd03p .
    METHODS serialize_top
      IMPORTING
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_extend
      IMPORTING
        !is_dd03p     TYPE dd03p
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS has_more_extensions
      IMPORTING
        !it_fields     TYPE ty_fields
        !is_data       TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_more) TYPE abap_bool .
    METHODS serialize_field_annotations
      IMPORTING
        !iv_fieldname TYPE clike
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string .
    METHODS serialize_fkey_annotations
      IMPORTING
        !iv_fieldname TYPE clike
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_field_foreign_key
      IMPORTING
        !iv_fieldname TYPE clike
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_value_help
      IMPORTING
        !iv_fieldname TYPE clike
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string .
    METHODS escape_string
      IMPORTING
        !iv_string       TYPE clike
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS unescape_string
      IMPORTING
        !iv_string       TYPE clike
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS serialize_type
      IMPORTING
        !is_dd03p      TYPE dd03p
      RETURNING
        VALUE(rv_type) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_type_core
      IMPORTING
        !is_dd03p      TYPE dd03p
      RETURNING
        VALUE(rv_type) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_type_special
      IMPORTING
        !is_dd03p      TYPE dd03p
      RETURNING
        VALUE(rv_type) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_tabl_ddl IMPLEMENTATION.


  METHOD deserialize.

    DATA lt_tokens TYPE ty_tokens.
    DATA ls_token TYPE ty_token.
    DATA ls_probe TYPE ty_token.
    DATA lv_index TYPE i VALUE 1.
    DATA lv_probe TYPE i.
    DATA lv_end TYPE abap_bool.
    DATA ls_dd03p TYPE dd03p.
    DATA ls_dd08v TYPE dd08v.
    DATA ls_dd35v TYPE dd35v.

    lt_tokens = tokenize( iv_ddl ).
    parse_top_annotations(
      EXPORTING
        it_tokens = lt_tokens
      CHANGING
        cv_index = lv_index
        cs_data = rs_data ).

    READ TABLE lt_tokens INDEX lv_index INTO ls_token.
    IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'define'.
      parse_error(
        iv_context = 'expected DEFINE TABLE header'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    lv_index = lv_index + 1.
    READ TABLE lt_tokens INDEX lv_index INTO ls_token.
    IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'table'.
      parse_error(
        iv_context = 'expected TABLE after DEFINE'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    lv_index = lv_index + 1.
    READ TABLE lt_tokens INDEX lv_index INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
      parse_error(
        iv_context = 'expected table name'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    rs_data-dd02v-tabname = to_upper( ls_token-value ).
    lv_index = lv_index + 1.
    READ TABLE lt_tokens INDEX lv_index INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-value <> '{'.
      parse_error(
        iv_context = 'expected opening brace'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    lv_index = lv_index + 1.

    WHILE lv_index <= lines( lt_tokens ).
      READ TABLE lt_tokens INDEX lv_index INTO ls_token.
      IF ls_token-value = '}'.
        lv_end = abap_true.
        lv_index = lv_index + 1.
        EXIT.
      ELSEIF ls_token-value = ';'.
        lv_index = lv_index + 1.
      ELSEIF to_lower( ls_token-value ) = 'extend'.
        CLEAR: ls_dd08v, ls_dd35v.
        parse_extension(
          EXPORTING it_tokens = lt_tokens
          CHANGING
            cv_index = lv_index
            cs_data = rs_data
            cs_dd08v = ls_dd08v
            cs_dd35v = ls_dd35v ).
      ELSEIF strlen( ls_token-value ) > 0 AND ls_token-value(1) = '@'.
        lv_probe = lv_index.
        WHILE lv_probe <= lines( lt_tokens ).
          READ TABLE lt_tokens INDEX lv_probe INTO ls_probe.
          IF strlen( ls_probe-value ) = 0 OR ls_probe-value(1) <> '@'.
            EXIT.
          ENDIF.
          lv_probe = lv_probe + 1.
          READ TABLE lt_tokens INDEX lv_probe INTO ls_probe.
          IF sy-subrc = 0 AND ls_probe-value = ':'.
            lv_probe = lv_probe + 2.
          ENDIF.
        ENDWHILE.
        READ TABLE lt_tokens INDEX lv_probe INTO ls_probe.
        IF sy-subrc = 0 AND to_lower( ls_probe-value ) = 'extend'.
          CLEAR: ls_dd03p, ls_dd08v, ls_dd35v.
          parse_field_annotations(
            EXPORTING it_tokens = lt_tokens
            CHANGING
              cv_index = lv_index
              cs_dd03p = ls_dd03p
              cs_dd08v = ls_dd08v ).
          parse_extension(
            EXPORTING it_tokens = lt_tokens
            CHANGING
              cv_index = lv_index
              cs_data = rs_data
              cs_dd08v = ls_dd08v
              cs_dd35v = ls_dd35v ).
        ELSE.
          parse_field(
            EXPORTING it_tokens = lt_tokens
            CHANGING
              cv_index = lv_index
              cs_data = rs_data ).
        ENDIF.
      ELSE.
        parse_field(
          EXPORTING it_tokens = lt_tokens
          CHANGING
            cv_index = lv_index
            cs_data = rs_data ).
      ENDIF.
    ENDWHILE.
    IF lv_end = abap_false.
      parse_error(
        iv_context = 'missing closing brace'
        iv_token = ''
        iv_offset = strlen( iv_ddl ) ).
    ENDIF.
    WHILE lv_index <= lines( lt_tokens ).
      READ TABLE lt_tokens INDEX lv_index INTO ls_token.
      IF ls_token-value <> ';'.
        parse_error(
          iv_context = 'unexpected token after table definition'
          iv_token = ls_token-value
          iv_offset = ls_token-offset ).
      ENDIF.
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD escape_string.
    DATA lv_escaped TYPE string.
    lv_escaped = replace(
      val = iv_string
      sub = |'|
      with = |''|
      occ = 0 ).
    rv_string = |'{ lv_escaped }'|.
  ENDMETHOD.


  METHOD unescape_string.
    rv_string = iv_string.
    REPLACE FIRST OCCURRENCE OF REGEX |^'| IN rv_string WITH || ##REGEX_POSIX.
    REPLACE FIRST OCCURRENCE OF REGEX |'$| IN rv_string WITH || ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF |''| IN rv_string WITH |'|.
  ENDMETHOD.


  METHOD parse_error.
    zcx_abapgit_exception=>raise( |TABL DDL parse error at offset { iv_offset }: { iv_context } ({ iv_token })| ).
  ENDMETHOD.


  METHOD get_replacement_object.

    DATA lv_view_name TYPE string.

    lv_view_name = to_upper( iv_viewref ).
    IF lv_view_name IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        " DD02V-VIEWREF contains the database view name. DDL uses the
        " corresponding CDS entity name instead.
        CALL METHOD ('CL_SBD_DDLS_UTILITY')=>('MAP_TO_REPLACEMENT_DDLS')
          EXPORTING
            i_view_name = lv_view_name
          IMPORTING
            e_entityname = rv_object.
      CATCH cx_root.
        " The utility is not available on older releases and is also absent
        " from the open-abap test runtime. In that case no annotation is
        " emitted rather than serializing DD02V-VIEWREF with the wrong
        " meaning.
        CLEAR rv_object.
    ENDTRY.

  ENDMETHOD.


  METHOD get_replacement_view.

    DATA lv_entityname TYPE string.

    lv_entityname = to_upper( iv_entityname ).
    IF lv_entityname IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        " The reverse mapping is needed when DDL is saved back to TABL:
        " DD02V-VIEWREF must receive the database view name.
        CALL METHOD ('CL_SBD_DDLS_UTILITY')=>('MAP_TO_REPLACEMENT_VIEW')
          EXPORTING
            i_entityname = lv_entityname
          IMPORTING
            e_view_name = rv_viewname.
      CATCH cx_root.
        " Keep source-only parsing usable on releases without the SAP
        " utility. A SAP system with the utility returns the resolved view
        " name, or initial for an entity that cannot be resolved.
        rv_viewname = iv_entityname.
    ENDTRY.

  ENDMETHOD.


  METHOD parse_replacement_object.
    DATA lv_entityname TYPE string.

    lv_entityname = unescape_string( iv_value ).
    IF lv_entityname IS INITIAL.
      parse_error(
        iv_context = 'replacement object is missing'
        iv_token = iv_name
        iv_offset = 0 ).
    ENDIF.
    cs_data-dd02v-viewref = get_replacement_view( lv_entityname ).
    IF cs_data-dd02v-viewref IS INITIAL.
      parse_error(
        iv_context = 'replacement object cannot be resolved'
        iv_token = lv_entityname
        iv_offset = 0 ).
    ENDIF.
  ENDMETHOD.


  METHOD tokenize.

    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.
    DATA lv_start TYPE i.
    DATA lv_next TYPE i.
    DATA lv_quoted TYPE abap_bool.
    DATA lv_char TYPE string.
    DATA lv_current TYPE string.
    DATA ls_token TYPE ty_token.

    lv_length = strlen( iv_ddl ).
    WHILE lv_offset < lv_length.
      lv_char = iv_ddl+lv_offset(1).
      IF lv_quoted = abap_true.
        lv_current = lv_current && lv_char.
        IF lv_char = |'|.
          lv_next = lv_offset + 1.
          IF lv_next < lv_length AND iv_ddl+lv_next(1) = |'|.
            lv_current = lv_current && |'|.
            lv_offset = lv_offset + 2.
            CONTINUE.
          ENDIF.
          lv_quoted = abap_false.
        ENDIF.
        lv_offset = lv_offset + 1.
        CONTINUE.
      ENDIF.
      IF lv_char = |'|.
        IF lv_current IS INITIAL.
          lv_start = lv_offset.
        ENDIF.
        lv_quoted = abap_true.
        lv_current = lv_current && lv_char.
      ELSEIF lv_char = '/' AND lv_offset + 1 < lv_length
          AND ( iv_ddl+lv_offset(2) = '//' OR iv_ddl+lv_offset(2) = '/*' ).
        IF lv_current IS NOT INITIAL.
          CLEAR ls_token.
          ls_token-value = lv_current.
          ls_token-offset = lv_start.
          APPEND ls_token TO rt_tokens.
          CLEAR lv_current.
        ENDIF.
        IF iv_ddl+lv_offset(2) = '//'.
          WHILE lv_offset < lv_length AND iv_ddl+lv_offset(1) <> |\n|.
            lv_offset = lv_offset + 1.
          ENDWHILE.
        ELSE.
          lv_start = lv_offset.
          lv_offset = lv_offset + 2.
          WHILE lv_offset + 1 < lv_length
              AND iv_ddl+lv_offset(2) <> '*/'.
            lv_offset = lv_offset + 1.
          ENDWHILE.
          IF lv_offset + 1 >= lv_length.
            parse_error(
              iv_context = 'unterminated block comment'
              iv_token = '/*'
              iv_offset = lv_start ).
          ENDIF.
          lv_offset = lv_offset + 2.
        ENDIF.
        CONTINUE.
      ELSEIF lv_char = '-' AND lv_offset + 1 < lv_length
          AND iv_ddl+lv_offset(2) = '--'.
        IF lv_current IS NOT INITIAL.
          CLEAR ls_token.
          ls_token-value = lv_current.
          ls_token-offset = lv_start.
          APPEND ls_token TO rt_tokens.
          CLEAR lv_current.
        ENDIF.
        WHILE lv_offset < lv_length AND iv_ddl+lv_offset(1) <> |\n|.
          lv_offset = lv_offset + 1.
        ENDWHILE.
        CONTINUE.
      ELSEIF lv_char = | | OR lv_char = |\n| OR lv_char = |\r| OR lv_char = |\t|.
        IF lv_current IS NOT INITIAL.
          CLEAR ls_token.
          ls_token-value = lv_current.
          ls_token-offset = lv_start.
          APPEND ls_token TO rt_tokens.
          CLEAR lv_current.
        ENDIF.
      ELSEIF lv_char = ':' OR lv_char = ';' OR lv_char = '='
          OR lv_char = '{' OR lv_char = '}'.
        IF lv_current IS NOT INITIAL.
          CLEAR ls_token.
          ls_token-value = lv_current.
          ls_token-offset = lv_start.
          APPEND ls_token TO rt_tokens.
          CLEAR lv_current.
        ENDIF.
        CLEAR ls_token.
        ls_token-value = lv_char.
        ls_token-offset = lv_offset.
        APPEND ls_token TO rt_tokens.
      ELSE.
        IF lv_current IS INITIAL.
          lv_start = lv_offset.
        ENDIF.
        lv_current = lv_current && lv_char.
      ENDIF.
      lv_offset = lv_offset + 1.
    ENDWHILE.
    IF lv_current IS NOT INITIAL.
      CLEAR ls_token.
      ls_token-value = lv_current.
      ls_token-offset = lv_start.
      APPEND ls_token TO rt_tokens.
    ENDIF.
    IF lv_quoted = abap_true.
      parse_error(
        iv_context = 'unterminated string literal'
        iv_token = lv_current
        iv_offset = lv_start ).
    ENDIF.

  ENDMETHOD.


  METHOD parse_top_annotations.

    DATA ls_token TYPE ty_token.
    DATA lv_name TYPE string.
    DATA lv_value TYPE string.
    DATA lv_compare TYPE string.
    FIELD-SYMBOLS <lv_is_gtt> TYPE abap_bool.
    FIELD-SYMBOLS <lv_pk_is_invhash> TYPE c.

    WHILE cv_index <= lines( it_tokens ).
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF strlen( ls_token-value ) = 0 OR ls_token-value(1) <> '@'.
        EXIT.
      ENDIF.
      lv_name = to_lower( ls_token-value ).
      CONDENSE lv_name NO-GAPS.
      IF lv_name = '@abapcatalog.enhancement.category'.
        " Accept the dotted spelling used by current table DDL.
        lv_name = '@abapcatalog.enhancementcategory'.
      ENDIF.
      cv_index = cv_index + 1.
      CLEAR lv_value.
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF sy-subrc = 0 AND ls_token-value = ':'.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0.
          parse_error(
            iv_context = 'annotation value is missing'
            iv_token = lv_name
            iv_offset = 0 ).
        ENDIF.
        lv_value = ls_token-value.
        cv_index = cv_index + 1.
      ENDIF.
      lv_compare = lv_value.
      CONDENSE lv_compare NO-GAPS.
      CASE lv_name.
        WHEN '@endusertext.label'.
          IF lv_value IS INITIAL.
            parse_error(
              iv_context = 'annotation value is missing'
              iv_token = lv_name
              iv_offset = 0 ).
          ENDIF.
          cs_data-dd02v-ddtext = unescape_string( lv_value ).
        WHEN '@abapcatalog.enhancementcategory'.
          CASE to_upper( lv_compare ).
            WHEN '#NOT_CLASSIFIED'.
              cs_data-dd02v-exclass = '0'.
            WHEN '#NOT_EXTENSIBLE'.
              cs_data-dd02v-exclass = '1'.
            WHEN '#EXTENSIBLE_CHARACTER'.
              cs_data-dd02v-exclass = '2'.
            WHEN '#EXTENSIBLE_CHARACTER_NUMERIC'.
              cs_data-dd02v-exclass = '3'.
            WHEN '#EXTENSIBLE_ANY'.
              cs_data-dd02v-exclass = '4'.
            WHEN OTHERS.
              parse_error(
                iv_context = 'unsupported enhancement category'
                iv_token = lv_value
                iv_offset = 0 ).
          ENDCASE.
        WHEN '@abapcatalog.tablecategory'.
          CASE to_upper( lv_compare ).
            WHEN '#TRANSPARENT'.
              cs_data-dd02v-tabclass = 'TRANSP'.
            WHEN '#GLOBAL_TEMPORARY'.
              cs_data-dd02v-tabclass = 'TRANSP'.
              ASSIGN COMPONENT 'IS_GTT' OF STRUCTURE cs_data-dd02v TO <lv_is_gtt>.
              IF sy-subrc = 0.
                <lv_is_gtt> = abap_true.
              ENDIF.
            WHEN OTHERS.
              parse_error(
                iv_context = 'unsupported table category'
                iv_token = lv_value
                iv_offset = 0 ).
          ENDCASE.
        WHEN '@abapcatalog.activationtype'.
          CASE to_upper( lv_compare ).
            WHEN '#NAMETAB_GENERATION_OFFLINE'.
              cs_data-dd02v-authclass = '01'.
            WHEN '#ADAPT_C_STRUCTURES'.
              cs_data-dd02v-authclass = '02'.
            WHEN OTHERS.
              parse_error(
                iv_context = 'unsupported activation type'
                iv_token = lv_value
                iv_offset = 0 ).
          ENDCASE.
        WHEN '@abapcatalog.deliveryclass'.
          IF strlen( lv_compare ) <> 2 OR lv_compare(1) <> '#'.
            parse_error(
              iv_context = 'invalid delivery class'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          cs_data-dd02v-contflag = to_upper( lv_compare+1 ).
        WHEN '@abapcatalog.datamaintenance'.
          CASE to_upper( lv_compare ).
            WHEN '#ALLOWED'.
              cs_data-dd02v-mainflag = abap_true.
            WHEN '#RESTRICTED'.
              CLEAR cs_data-dd02v-mainflag.
            WHEN '#LIMITED'.
              CLEAR cs_data-dd02v-mainflag.
            WHEN '#NOT_ALLOWED'.
              cs_data-dd02v-mainflag = 'N'.
            WHEN OTHERS.
              parse_error(
                iv_context = 'unsupported data maintenance value'
                iv_token = lv_value
                iv_offset = 0 ).
          ENDCASE.
        WHEN '@abapcatalog.replacementobject'.
          parse_replacement_object(
            EXPORTING
              iv_value = lv_value
              iv_name = lv_name
            CHANGING
              cs_data = cs_data ).
        WHEN '@abapcatalog.primarykey.invertedhashindex'.
          IF to_lower( lv_compare ) <> 'true' AND to_lower( lv_compare ) <> 'false'.
            parse_error(
              iv_context = 'invalid inverted hash index value'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          ASSIGN COMPONENT 'PK_IS_INVHASH' OF STRUCTURE cs_data-dd02v TO <lv_pk_is_invhash>.
          IF sy-subrc = 0.
            IF to_lower( lv_compare ) = 'true'.
              <lv_pk_is_invhash> = abap_true.
            ELSE.
              CLEAR <lv_pk_is_invhash>.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          parse_error(
            iv_context = 'unsupported table annotation'
            iv_token = lv_name
            iv_offset = 0 ).
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_field_annotations.

    DATA ls_token TYPE ty_token.
    DATA lv_name TYPE string.
    DATA lv_value TYPE string.
    DATA lv_compare TYPE string.
    DATA lv_reference TYPE string.
    DATA lv_table TYPE string.
    DATA lv_field TYPE string.
    FIELD-SYMBOLS <lv_outputstyle> TYPE zif_abapgit_aff_doma_v1=>ty_output_style.

    WHILE cv_index <= lines( it_tokens ).
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF strlen( ls_token-value ) = 0 OR ls_token-value(1) <> '@'.
        EXIT.
      ENDIF.
      lv_name = to_lower( ls_token-value ).
      CONDENSE lv_name NO-GAPS.
      cv_index = cv_index + 1.
      CLEAR lv_value.
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF sy-subrc = 0 AND ls_token-value = ':'.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0.
          parse_error(
            iv_context = 'field annotation value is missing'
            iv_token = lv_name
            iv_offset = 0 ).
        ENDIF.
        lv_value = ls_token-value.
        cv_index = cv_index + 1.
      ENDIF.
      lv_compare = lv_value.
      CONDENSE lv_compare NO-GAPS.
      CASE lv_name.
        WHEN '@endusertext.label'.
          IF lv_value IS INITIAL.
            parse_error(
              iv_context = 'field annotation value is missing'
              iv_token = lv_name
              iv_offset = 0 ).
          ENDIF.
          cs_dd03p-ddtext = unescape_string( lv_value ).
        WHEN '@abapcatalog.textlanguage'.
          cs_dd03p-languflag = abap_true.
        WHEN '@semantics.amount.currencycode'.
          lv_reference = unescape_string( lv_value ).
          CLEAR: lv_table, lv_field.
          SPLIT lv_reference AT '.' INTO lv_table lv_field.
          IF lv_table IS INITIAL OR lv_field IS INITIAL.
            parse_error(
              iv_context = 'invalid currency reference'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          cs_dd03p-reftable = to_upper( lv_table ).
          cs_dd03p-reffield = to_upper( lv_field ).
        WHEN '@semantics.quantity.unitofmeasure'.
          lv_reference = unescape_string( lv_value ).
          CLEAR: lv_table, lv_field.
          SPLIT lv_reference AT '.' INTO lv_table lv_field.
          IF lv_table IS INITIAL OR lv_field IS INITIAL.
            parse_error(
              iv_context = 'invalid unit reference'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          cs_dd03p-reftable = to_upper( lv_table ).
          cs_dd03p-reffield = to_upper( lv_field ).
        WHEN '@abapcatalog.decfloat.outputstyle'.
          IF strlen( lv_compare ) < 2 OR lv_compare(1) <> '#'.
            parse_error(
              iv_context = 'invalid decfloat output style'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE cs_dd03p TO <lv_outputstyle>.
          IF sy-subrc <> 0.
            parse_error(
              iv_context = 'decfloat output style is unavailable'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          IF to_upper( lv_compare+1 ) = 'NORMAL'.
            <lv_outputstyle> = '00'.
          ELSE.
            parse_error(
              iv_context = 'unsupported decfloat output style'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
        WHEN '@abapcatalog.foreignkey.label'.
          IF lv_value IS INITIAL.
            parse_error(
              iv_context = 'foreign key annotation value is missing'
              iv_token = lv_name
              iv_offset = 0 ).
          ENDIF.
          cs_dd08v-ddtext = unescape_string( lv_value ).
        WHEN '@abapcatalog.foreignkey.keytype'.
          IF strlen( lv_compare ) < 2 OR lv_compare(1) <> '#'.
            parse_error(
              iv_context = 'invalid foreign key type'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
          cs_dd08v-frkart = to_upper( lv_compare+1 ).
          IF cs_dd08v-frkart = 'TEXT_KEY'.
            cs_dd08v-frkart = 'TEXT'.
          ELSEIF cs_dd08v-frkart = 'NON_KEY'.
            cs_dd08v-frkart = 'REF'.
          ENDIF.
        WHEN '@abapcatalog.foreignkey.screencheck'.
          IF to_lower( lv_compare ) = 'true'.
            CLEAR cs_dd08v-checkflag.
          ELSEIF to_lower( lv_compare ) = 'false'.
            cs_dd08v-checkflag = abap_true.
          ELSE.
            parse_error(
              iv_context = 'invalid foreign key screen check value'
              iv_token = lv_value
              iv_offset = 0 ).
          ENDIF.
        WHEN '@abapcatalog.foreignkey.messageclass'.
          IF lv_value IS INITIAL.
            parse_error(
              iv_context = 'foreign key message class is missing'
              iv_token = lv_name
              iv_offset = 0 ).
          ENDIF.
          cs_dd08v-arbgb = unescape_string( lv_value ).
        WHEN '@abapcatalog.foreignkey.messagenumber'.
          IF lv_value IS INITIAL.
            parse_error(
              iv_context = 'foreign key message number is missing'
              iv_token = lv_name
              iv_offset = 0 ).
          ENDIF.
          cs_dd08v-msgnr = unescape_string( lv_value ).
        WHEN OTHERS.
          parse_error(
            iv_context = 'unsupported field annotation'
            iv_token = lv_name
            iv_offset = 0 ).
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_field.

    DATA ls_token TYPE ty_token.
    DATA ls_next TYPE ty_token.
    DATA ls_dd03p TYPE dd03p.
    DATA ls_dd08v TYPE dd08v.
    DATA ls_dd35v TYPE dd35v.
    DATA lv_fieldname TYPE string.
    DATA lv_key TYPE abap_bool.
    DATA lv_is_include TYPE abap_bool.

    parse_field_annotations(
      EXPORTING it_tokens = it_tokens
      CHANGING
        cv_index = cv_index
        cs_dd03p = ls_dd03p
        cs_dd08v = ls_dd08v ).
    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc <> 0.
      parse_error(
        iv_context = 'field definition is missing'
        iv_token = ''
        iv_offset = 0 ).
    ENDIF.
    READ TABLE it_tokens INDEX cv_index + 1 INTO ls_next.
    IF to_lower( ls_token-value ) = 'key' AND sy-subrc = 0 AND ls_next-value <> ':'.
      lv_key = abap_true.
      cv_index = cv_index + 1.
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
    ENDIF.
    ls_dd03p-keyflag = lv_key.
    IF to_lower( ls_token-value ) = 'include'.
      lv_is_include = abap_true.
      ls_dd03p-fieldname = '.INCLU'.
      cv_index = cv_index + 1.
      parse_include( EXPORTING it_tokens = it_tokens
                    CHANGING
                      cv_index = cv_index
                      cs_dd03p = ls_dd03p ).
    ELSE.
      lv_fieldname = to_upper( ls_token-value ).
      ls_dd03p-fieldname = lv_fieldname.
      cv_index = cv_index + 1.
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF sy-subrc <> 0 OR ls_token-value <> ':'.
        parse_error(
          iv_context = 'expected colon after field name'
          iv_token = ls_token-value
          iv_offset = ls_token-offset ).
      ENDIF.
      cv_index = cv_index + 1.
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF sy-subrc <> 0.
        parse_error(
          iv_context = 'field type is missing'
          iv_token = ''
          iv_offset = 0 ).
      ENDIF.
      IF to_lower( ls_token-value ) = 'include'.
        lv_is_include = abap_true.
        ls_dd03p-groupname = lv_fieldname.
        ls_dd03p-fieldname = '.INCLU'.
        cv_index = cv_index + 1.
        parse_include( EXPORTING it_tokens = it_tokens
                      CHANGING
                        cv_index = cv_index
                        cs_dd03p = ls_dd03p ).
      ELSE.
        parse_type( EXPORTING
          iv_token = ls_token-value CHANGING
          cs_dd03p = ls_dd03p ).
        cv_index = cv_index + 1.
        WHILE cv_index <= lines( it_tokens ).
          READ TABLE it_tokens INDEX cv_index INTO ls_token.
          IF to_lower( ls_token-value ) = 'not'.
            cv_index = cv_index + 1.
            READ TABLE it_tokens INDEX cv_index INTO ls_token.
            IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'null'.
              parse_error(
                iv_context = 'expected NULL after NOT'
                iv_token = ls_token-value
                iv_offset = ls_token-offset ).
            ENDIF.
            ls_dd03p-notnull = abap_true.
            cv_index = cv_index + 1.
          ELSEIF ls_token-value = ';'.
            EXIT.
          ELSEIF to_lower( ls_token-value ) = 'with'.
            cv_index = cv_index + 1.
            READ TABLE it_tokens INDEX cv_index INTO ls_token.
            IF sy-subrc <> 0.
              parse_error(
                iv_context = 'relationship kind is missing'
                iv_token = ''
                iv_offset = 0 ).
            ENDIF.
            IF to_lower( ls_token-value ) = 'foreign'.
              cv_index = cv_index + 1.
              READ TABLE it_tokens INDEX cv_index INTO ls_token.
              IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'key'.
                parse_error(
                  iv_context = 'expected KEY after FOREIGN'
                  iv_token = ls_token-value
                  iv_offset = ls_token-offset ).
              ENDIF.
              cv_index = cv_index + 1.
              parse_foreign_key(
                EXPORTING
                  it_tokens = it_tokens
                  iv_fieldname = ls_dd03p-fieldname
                CHANGING
                  cv_index = cv_index
                  cs_dd08v = ls_dd08v
                  cs_data = cs_data ).
            ELSEIF to_lower( ls_token-value ) = 'value'.
              cv_index = cv_index + 1.
              READ TABLE it_tokens INDEX cv_index INTO ls_token.
              IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'help'.
                parse_error(
                  iv_context = 'expected HELP after VALUE'
                  iv_token = ls_token-value
                  iv_offset = ls_token-offset ).
              ENDIF.
              cv_index = cv_index + 1.
              parse_value_help(
                EXPORTING
                  it_tokens = it_tokens
                  iv_fieldname = ls_dd03p-fieldname
                CHANGING
                  cv_index = cv_index
                  cs_dd35v = ls_dd35v
                  cs_data = cs_data ).
            ELSE.
              parse_error(
                iv_context = 'unsupported relationship kind'
                iv_token = ls_token-value
                iv_offset = ls_token-offset ).
            ENDIF.
          ELSE.
            parse_error(
              iv_context = 'unexpected token in field definition'
              iv_token = ls_token-value
              iv_offset = ls_token-offset ).
          ENDIF.
        ENDWHILE.
      ENDIF.
    ENDIF.
    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc = 0 AND ls_token-value = ';'.
      cv_index = cv_index + 1.
    ELSEIF lv_is_include = abap_true
        AND sy-subrc = 0
        AND ( strlen( ls_token-value ) > 0 AND ls_token-value(1) = '@'
          OR to_lower( ls_token-value ) = 'extend' ).
      " Some ADT table DDL omits the terminator when an include is
      " immediately followed by its component extensions.
    ELSE.
      parse_error(
        iv_context = 'expected semicolon after field definition'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    ls_dd03p-adminfield = '0'.
    IF ls_dd08v IS NOT INITIAL.
      ls_dd08v-fieldname = ls_dd03p-fieldname.
      ls_dd03p-checktable = ls_dd08v-checktable.
      ls_dd03p-shlporigin = 'P'.
      APPEND ls_dd08v TO cs_data-dd08v.
    ENDIF.
    IF ls_dd35v IS NOT INITIAL.
      ls_dd35v-fieldname = ls_dd03p-fieldname.
      ls_dd03p-shlporigin = 'F'.
      APPEND ls_dd35v TO cs_data-dd35v.
    ENDIF.
    APPEND ls_dd03p TO cs_data-dd03p.

  ENDMETHOD.


  METHOD parse_include.

    DATA ls_token TYPE ty_token.
    DATA lv_suffix TYPE string.

    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
      parse_error(
        iv_context = 'include name is missing'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    cs_dd03p-precfield = to_upper( ls_token-value ).
    cv_index = cv_index + 1.
    WHILE cv_index <= lines( it_tokens ).
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      IF to_lower( ls_token-value ) = 'with'.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'suffix'.
          parse_error(
            iv_context = 'expected SUFFIX after WITH'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
          parse_error(
            iv_context = 'include suffix is missing'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        lv_suffix = to_upper( ls_token-value ).
        cs_dd03p-fieldname = |.INCLU-{ lv_suffix }|.
        cv_index = cv_index + 1.
      ELSEIF to_lower( ls_token-value ) = 'not'.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'null'.
          parse_error(
            iv_context = 'expected NULL after NOT'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        cs_dd03p-notnull = abap_true.
        cv_index = cv_index + 1.
      ELSEIF ls_token-value = ';'
          OR ( strlen( ls_token-value ) > 0 AND ls_token-value(1) = '@' )
          OR to_lower( ls_token-value ) = 'extend'.
        EXIT.
      ELSE.
        parse_error(
          iv_context = 'unexpected token in include definition'
          iv_token = ls_token-value
          iv_offset = ls_token-offset ).
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_cardinality.
    CASE iv_token.
      WHEN '[1,0..1]'.
        cs_dd08v-cardleft = 'C'.
        cs_dd08v-card = '1'.
      WHEN '[0..1,1]'.
        cs_dd08v-cardleft = '1'.
        cs_dd08v-card = 'C'.
      WHEN '[1,1]'.
        cs_dd08v-cardleft = '1'.
        cs_dd08v-card = '1'.
      WHEN '[1..*,1]'.
        cs_dd08v-cardleft = '1'.
        cs_dd08v-card = 'N'.
      WHEN '[0..*,1]'.
        cs_dd08v-cardleft = '1'.
        cs_dd08v-card = 'CN'.
      WHEN '[0..*,0..1]'.
        cs_dd08v-cardleft = 'C'.
        cs_dd08v-card = 'CN'.
      WHEN '[0..1,0..1]'.
        cs_dd08v-cardleft = 'C'.
        cs_dd08v-card = 'C'.
      WHEN '[1..*,]'.
        cs_dd08v-cardleft = 'N'.
        cs_dd08v-card = 'N'.
      WHEN '[1..*,0..1]'.
        cs_dd08v-cardleft = 'C'.
        cs_dd08v-card = 'N'.
      WHEN OTHERS.
        parse_error(
          iv_context = 'unsupported foreign key cardinality'
          iv_token = iv_token
          iv_offset = iv_offset ).
    ENDCASE.
  ENDMETHOD.


  METHOD parse_foreign_key.

    DATA ls_token TYPE ty_token.
    DATA ls_dd05m TYPE dd05m.
    DATA lv_left TYPE string.
    DATA lv_right TYPE string.
    DATA lv_primpos TYPE i.

    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc <> 0.
      parse_error(
        iv_context = 'foreign key target is missing'
        iv_token = ''
        iv_offset = 0 ).
    ENDIF.
    IF ls_token-value(1) = '['.
      parse_cardinality(
        EXPORTING
          iv_token = ls_token-value
          iv_offset = ls_token-offset
        CHANGING
          cs_dd08v = cs_dd08v ).
      cv_index = cv_index + 1.
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
    ENDIF.
    IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
      parse_error(
        iv_context = 'foreign key check table is missing'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    cs_dd08v-checktable = to_upper( ls_token-value ).
    cv_index = cv_index + 1.
    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc = 0 AND to_lower( ls_token-value ) = 'where'.
      cv_index = cv_index + 1.
      WHILE cv_index <= lines( it_tokens ).
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF ls_token-value = ';' OR to_lower( ls_token-value ) = 'with'.
          EXIT.
        ENDIF.
        lv_left = ls_token-value.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR ls_token-value <> '='.
          parse_error(
            iv_context = 'expected equals in foreign key condition'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
          parse_error(
            iv_context = 'foreign key condition value is missing'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        lv_right = ls_token-value.
        CLEAR ls_dd05m.
        ls_dd05m-fieldname = iv_fieldname.
        ls_dd05m-checktable = cs_dd08v-checktable.
        ls_dd05m-checkfield = to_upper( lv_left ).
        IF lv_right(1) = |'|.
          ls_dd05m-fortable = lv_right.
        ELSE.
          SPLIT lv_right AT '.' INTO ls_dd05m-fortable ls_dd05m-forkey.
          IF ls_dd05m-fortable IS INITIAL OR ls_dd05m-forkey IS INITIAL.
            parse_error(
              iv_context = 'foreign key target must be table.field'
              iv_token = lv_right
              iv_offset = ls_token-offset ).
          ENDIF.
          TRANSLATE ls_dd05m-fortable TO UPPER CASE.
          TRANSLATE ls_dd05m-forkey TO UPPER CASE.
        ENDIF.
        LOOP AT cs_data-dd05m TRANSPORTING NO FIELDS WHERE fieldname = iv_fieldname.
          lv_primpos = lv_primpos + 1.
        ENDLOOP.
        ls_dd05m-primpos = lv_primpos + 1.
        APPEND ls_dd05m TO cs_data-dd05m.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'and'.
          EXIT.
        ENDIF.
        cv_index = cv_index + 1.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.


  METHOD parse_value_help.

    DATA ls_token TYPE ty_token.
    DATA ls_dd36m TYPE dd36m.
    DATA lv_left TYPE string.
    DATA lv_right TYPE string.
    DATA lv_position TYPE i.

    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
      parse_error(
        iv_context = 'value help name is missing'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    cs_dd35v-shlpname = to_upper( ls_token-value ).
    cv_index = cv_index + 1.
    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc = 0 AND to_lower( ls_token-value ) = 'where'.
      cv_index = cv_index + 1.
      WHILE cv_index <= lines( it_tokens ).
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF ls_token-value = ';' OR to_lower( ls_token-value ) = 'with'.
          EXIT.
        ENDIF.
        lv_left = ls_token-value.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR ls_token-value <> '='.
          parse_error(
            iv_context = 'expected equals in value help condition'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
          parse_error(
            iv_context = 'value help condition value is missing'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
        lv_right = ls_token-value.
        CLEAR ls_dd36m.
        ls_dd36m-fieldname = iv_fieldname.
        ls_dd36m-shlpname = cs_dd35v-shlpname.
        ls_dd36m-shlpfield = to_upper( lv_left ).
        IF lv_right(1) = |'|.
          ls_dd36m-shtype = 'C'.
          ls_dd36m-shtable = lv_right.
        ELSE.
          SPLIT lv_right AT '.' INTO ls_dd36m-shtable ls_dd36m-shfield.
          IF ls_dd36m-shtable IS INITIAL OR ls_dd36m-shfield IS INITIAL.
            parse_error(
              iv_context = 'value help target must be table.field'
              iv_token = lv_right
              iv_offset = ls_token-offset ).
          ENDIF.
          TRANSLATE ls_dd36m-shtable TO UPPER CASE.
          TRANSLATE ls_dd36m-shfield TO UPPER CASE.
        ENDIF.
        LOOP AT cs_data-dd36m TRANSPORTING NO FIELDS WHERE fieldname = iv_fieldname.
          lv_position = lv_position + 1.
        ENDLOOP.
        ls_dd36m-flposition = lv_position + 1.
        APPEND ls_dd36m TO cs_data-dd36m.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'and'.
          EXIT.
        ENDIF.
        cv_index = cv_index + 1.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.


  METHOD parse_extension.

    DATA ls_token TYPE ty_token.
    DATA ls_dd03p TYPE dd03p.
    DATA ls_dd08v TYPE dd08v.
    DATA ls_dd35v TYPE dd35v.
    DATA lv_fieldname TYPE string.
    DATA lv_keyword TYPE string.
    DATA lv_done TYPE abap_bool.

    ls_dd08v = cs_dd08v.
    ls_dd35v = cs_dd35v.

    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    cv_index = cv_index + 1.
    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-value IS INITIAL.
      parse_error(
        iv_context = 'extension field is missing'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    lv_fieldname = to_upper( ls_token-value ).
    cv_index = cv_index + 1.
    READ TABLE it_tokens INDEX cv_index INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-value <> ':'.
      parse_error(
        iv_context = 'expected colon after extension field'
        iv_token = ls_token-value
        iv_offset = ls_token-offset ).
    ENDIF.
    cv_index = cv_index + 1.
    ls_dd03p-fieldname = lv_fieldname.
    ls_dd03p-adminfield = '1'.
    APPEND ls_dd03p TO cs_data-dd03p.
    WHILE cv_index <= lines( it_tokens ).
      READ TABLE it_tokens INDEX cv_index INTO ls_token.
      lv_keyword = to_lower( ls_token-value ).
      IF ls_token-value = ';'.
        cv_index = cv_index + 1.
        lv_done = abap_true.
        EXIT.
      ELSEIF ( strlen( ls_token-value ) > 0 AND ls_token-value(1) = '@' )
          OR lv_keyword = 'extend'.
        " Some ADT table DDL omits the terminator between extension blocks.
        lv_done = abap_true.
        EXIT.
      ELSEIF lv_keyword = 'remove'.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0.
          parse_error(
            iv_context = 'remove operation is incomplete'
            iv_token = ''
            iv_offset = 0 ).
        ENDIF.
        IF to_lower( ls_token-value ) = 'foreign'.
          cv_index = cv_index + 1.
          READ TABLE it_tokens INDEX cv_index INTO ls_token.
          IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'key'.
            parse_error(
              iv_context = 'expected KEY after REMOVE FOREIGN'
              iv_token = ls_token-value
              iv_offset = ls_token-offset ).
          ENDIF.
          CLEAR ls_dd08v.
          ls_dd08v-fieldname = lv_fieldname.
          ls_dd08v-checktable = '*'.
          ls_dd08v-noinherit = 'Y'.
          APPEND ls_dd08v TO cs_data-dd08v.
          cv_index = cv_index + 1.
        ELSEIF to_lower( ls_token-value ) = 'value'.
          cv_index = cv_index + 1.
          READ TABLE it_tokens INDEX cv_index INTO ls_token.
          IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'help'.
            parse_error(
              iv_context = 'expected HELP after REMOVE VALUE'
              iv_token = ls_token-value
              iv_offset = ls_token-offset ).
          ENDIF.
          CLEAR ls_dd35v.
          ls_dd35v-fieldname = lv_fieldname.
          ls_dd35v-shlpname = '*'.
          APPEND ls_dd35v TO cs_data-dd35v.
          cv_index = cv_index + 1.
        ELSE.
          parse_error(
            iv_context = 'unsupported remove operation'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
      ELSEIF lv_keyword = 'with'.
        cv_index = cv_index + 1.
        READ TABLE it_tokens INDEX cv_index INTO ls_token.
        IF sy-subrc <> 0.
          parse_error(
            iv_context = 'extension relationship is missing'
            iv_token = ''
            iv_offset = 0 ).
        ENDIF.
        IF to_lower( ls_token-value ) = 'foreign'.
          cv_index = cv_index + 1.
          READ TABLE it_tokens INDEX cv_index INTO ls_token.
          IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'key'.
            parse_error(
              iv_context = 'expected KEY after FOREIGN'
              iv_token = ls_token-value
              iv_offset = ls_token-offset ).
          ENDIF.
          cv_index = cv_index + 1.
          parse_foreign_key(
            EXPORTING
              it_tokens = it_tokens
              iv_fieldname = lv_fieldname
            CHANGING
              cv_index = cv_index
              cs_dd08v = ls_dd08v
              cs_data = cs_data ).
          ls_dd08v-fieldname = lv_fieldname.
          ls_dd08v-noinherit = 'Y'.
          APPEND ls_dd08v TO cs_data-dd08v.
        ELSEIF to_lower( ls_token-value ) = 'value'.
          cv_index = cv_index + 1.
          READ TABLE it_tokens INDEX cv_index INTO ls_token.
          IF sy-subrc <> 0 OR to_lower( ls_token-value ) <> 'help'.
            parse_error(
              iv_context = 'expected HELP after VALUE'
              iv_token = ls_token-value
              iv_offset = ls_token-offset ).
          ENDIF.
          cv_index = cv_index + 1.
          parse_value_help(
            EXPORTING
              it_tokens = it_tokens
              iv_fieldname = lv_fieldname
            CHANGING
              cv_index = cv_index
              cs_dd35v = ls_dd35v
              cs_data = cs_data ).
          ls_dd35v-fieldname = lv_fieldname.
          APPEND ls_dd35v TO cs_data-dd35v.
        ELSE.
          parse_error(
            iv_context = 'unsupported extension relationship'
            iv_token = ls_token-value
            iv_offset = ls_token-offset ).
        ENDIF.
      ELSE.
        parse_error(
          iv_context = 'unexpected token in extension'
          iv_token = ls_token-value
          iv_offset = ls_token-offset ).
      ENDIF.
    ENDWHILE.
    IF lv_done = abap_false.
      parse_error(
        iv_context = 'missing semicolon after extension'
        iv_token = ''
        iv_offset = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD parse_type.

    DATA lv_token TYPE string.
    DATA lv_base TYPE string.
    DATA lv_params TYPE string.
    DATA lv_length TYPE string.
    DATA lv_decimals TYPE string.
    DATA lv_offset TYPE i.
    DATA lv_close TYPE i.

    lv_token = to_lower( iv_token ).
    IF lv_token NP 'abap.*'.
      cs_dd03p-rollname = to_upper( iv_token ).
      CLEAR: cs_dd03p-inttype, cs_dd03p-intlen.
      RETURN.
    ENDIF.
    lv_token = lv_token+5.
    FIND FIRST OCCURRENCE OF '(' IN lv_token MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      lv_close = strlen( lv_token ) - 1.
      IF lv_token+lv_close(1) <> ')'.
        parse_error(
          iv_context = 'type parameter list is not closed'
          iv_token = lv_token
          iv_offset = lv_offset ).
      ENDIF.
      lv_base = lv_token(lv_offset).
      lv_close = lv_offset + 1.
      lv_params = lv_token+lv_close.
      lv_close = strlen( lv_params ) - 1.
      lv_params = lv_params(lv_close).
      SPLIT lv_params AT ',' INTO lv_length lv_decimals.
      CONDENSE lv_length.
      CONDENSE lv_decimals.
    ELSE.
      lv_base = lv_token.
    ENDIF.
    set_builtin_type(
      EXPORTING
        iv_base = lv_base
        iv_length = lv_length
        iv_decimals = lv_decimals
        iv_offset = lv_offset
      CHANGING cs_dd03p = cs_dd03p ).

  ENDMETHOD.


  METHOD set_builtin_type.
    IF iv_length IS NOT INITIAL AND iv_length CN '0123456789'.
      parse_error(
        iv_context = 'type length is not numeric'
        iv_token = iv_length
        iv_offset = iv_offset ).
    ENDIF.
    IF iv_decimals IS NOT INITIAL AND iv_decimals CN '0123456789'.
      parse_error(
        iv_context = 'type decimals are not numeric'
        iv_token = iv_decimals
        iv_offset = iv_offset ).
    ENDIF.
    IF iv_base = 'char' OR iv_base = 'numc' OR iv_base = 'raw'
        OR iv_base = 'string' OR iv_base = 'rawstring' OR iv_base = 'sstring'
        OR iv_base = 'unit'.
      IF iv_decimals IS NOT INITIAL.
        parse_error(
          iv_context = 'character type accepts one parameter'
          iv_token = iv_base
          iv_offset = iv_offset ).
      ENDIF.
      cs_dd03p-leng = iv_length.
      set_character_type( EXPORTING
        iv_base = iv_base CHANGING
        cs_dd03p = cs_dd03p ).
    ELSEIF iv_base = 'dec' OR iv_base = 'curr' OR iv_base = 'quan'.
      IF iv_length IS INITIAL OR iv_decimals IS INITIAL.
        parse_error(
          iv_context = 'decimal type requires length and decimals'
          iv_token = iv_base
          iv_offset = iv_offset ).
      ENDIF.
      set_numeric_type(
        EXPORTING
          iv_base = iv_base
          iv_length = iv_length
          iv_decimals = iv_decimals
        CHANGING cs_dd03p = cs_dd03p ).
    ELSEIF iv_base = 'df16_dec' OR iv_base = 'df34_dec'.
      IF iv_length IS INITIAL OR iv_decimals IS INITIAL.
        parse_error(
          iv_context = 'decimal floating type requires length and decimals'
          iv_token = iv_base
          iv_offset = iv_offset ).
      ENDIF.
      set_decfloat_type(
        EXPORTING
          iv_base = iv_base
          iv_length = iv_length
          iv_decimals = iv_decimals
        CHANGING cs_dd03p = cs_dd03p ).
    ELSEIF iv_length IS NOT INITIAL OR iv_decimals IS NOT INITIAL.
      parse_error(
        iv_context = 'parameterless built-in type has parameters'
        iv_token = iv_base
        iv_offset = iv_offset ).
    ELSEIF iv_base = 'int1' OR iv_base = 'int2' OR iv_base = 'int4'
        OR iv_base = 'int8' OR iv_base = 'fltp'.
      set_integer_type( EXPORTING
        iv_base = iv_base CHANGING
        cs_dd03p = cs_dd03p ).
    ELSEIF iv_base = 'accp' OR iv_base = 'lang' OR iv_base = 'datn'
        OR iv_base = 'timn' OR iv_base = 'utcl' OR iv_base = 'd16n'
        OR iv_base = 'd34n' OR iv_base = 'cuky' OR iv_base = 'dats'
        OR iv_base = 'tims' OR iv_base = 'clnt'.
      set_date_type( EXPORTING
        iv_base = iv_base CHANGING
        cs_dd03p = cs_dd03p ).
    ELSEIF iv_base = 'df16_raw' OR iv_base = 'df16_scl'
        OR iv_base = 'df34_scl' OR iv_base = 'df34_raw'.
      set_decfloat_type(
        EXPORTING
          iv_base = iv_base
          iv_length = ''
          iv_decimals = ''
        CHANGING cs_dd03p = cs_dd03p ).
    ELSE.
      parse_error(
        iv_context = 'unsupported built-in type'
        iv_token = iv_base
        iv_offset = iv_offset ).
    ENDIF.
  ENDMETHOD.


  METHOD set_character_type.
    CASE iv_base.
      WHEN 'char'.
        cs_dd03p-datatype = 'CHAR'.
        cs_dd03p-inttype = 'C'.
        cs_dd03p-intlen = cs_dd03p-leng * 2.
      WHEN 'numc'.
        cs_dd03p-datatype = 'NUMC'.
        cs_dd03p-inttype = 'N'.
        cs_dd03p-intlen = cs_dd03p-leng * 2.
      WHEN 'raw'.
        cs_dd03p-datatype = 'RAW'.
        cs_dd03p-inttype = 'X'.
        cs_dd03p-intlen = cs_dd03p-leng.
      WHEN 'string'.
        cs_dd03p-datatype = 'STRG'.
        cs_dd03p-inttype = 'g'.
        cs_dd03p-intlen = 8.
      WHEN 'rawstring'.
        cs_dd03p-datatype = 'RSTR'.
        cs_dd03p-inttype = 'x'.
        cs_dd03p-intlen = 8.
      WHEN 'sstring'.
        cs_dd03p-datatype = 'SSTR'.
        cs_dd03p-inttype = 'C'.
        cs_dd03p-intlen = cs_dd03p-leng * 2.
      WHEN 'unit'.
        cs_dd03p-datatype = 'UNIT'.
        cs_dd03p-inttype = 'C'.
        cs_dd03p-intlen = cs_dd03p-leng * 2.
    ENDCASE.
  ENDMETHOD.


  METHOD set_numeric_type.
    cs_dd03p-datatype = to_upper( iv_base ).
    cs_dd03p-leng = iv_length.
    cs_dd03p-decimals = iv_decimals.
    cs_dd03p-inttype = 'P'.
    cs_dd03p-intlen = ( cs_dd03p-leng + 2 ) DIV 2.
  ENDMETHOD.


  METHOD set_integer_type.
    CASE iv_base.
      WHEN 'int1'.
        cs_dd03p-datatype = 'INT1'.
        cs_dd03p-inttype = 'b'.
        cs_dd03p-intlen = 1.
      WHEN 'int2'.
        cs_dd03p-datatype = 'INT2'.
        cs_dd03p-inttype = 's'.
        cs_dd03p-intlen = 2.
      WHEN 'int4'.
        cs_dd03p-datatype = 'INT4'.
        cs_dd03p-inttype = 'I'.
        cs_dd03p-intlen = 4.
      WHEN 'int8'.
        cs_dd03p-datatype = 'INT8'.
        cs_dd03p-inttype = '8'.
        cs_dd03p-intlen = 8.
      WHEN 'fltp'.
        cs_dd03p-datatype = 'FLTP'.
        cs_dd03p-inttype = 'F'.
        cs_dd03p-intlen = 8.
    ENDCASE.
  ENDMETHOD.


  METHOD set_date_type.
    CASE iv_base.
      WHEN 'accp'.
        cs_dd03p-datatype = 'ACCP'.
        cs_dd03p-inttype = 'N'.
        cs_dd03p-leng = 6.
        cs_dd03p-intlen = 12.
      WHEN 'lang'.
        cs_dd03p-datatype = 'LANG'.
        cs_dd03p-inttype = 'C'.
        cs_dd03p-leng = 1.
        cs_dd03p-intlen = 2.
      WHEN 'datn'.
        cs_dd03p-datatype = 'DATN'.
        cs_dd03p-inttype = 'D'.
        cs_dd03p-leng = 8.
        cs_dd03p-intlen = 16.
      WHEN 'timn'.
        cs_dd03p-datatype = 'TIMN'.
        cs_dd03p-inttype = 'T'.
        cs_dd03p-leng = 6.
        cs_dd03p-intlen = 12.
      WHEN 'utcl'.
        cs_dd03p-datatype = 'UTCL'.
        cs_dd03p-inttype = 'P'.
      WHEN 'd16n'.
        cs_dd03p-datatype = 'D16N'.
        cs_dd03p-inttype = 'a'.
        cs_dd03p-intlen = 8.
      WHEN 'd34n'.
        cs_dd03p-datatype = 'D34N'.
        cs_dd03p-inttype = 'e'.
        cs_dd03p-intlen = 16.
      WHEN 'cuky'.
        cs_dd03p-datatype = 'CUKY'.
        cs_dd03p-inttype = 'C'.
        cs_dd03p-leng = 5.
        cs_dd03p-intlen = 10.
      WHEN 'dats'.
        cs_dd03p-datatype = 'DATS'.
        cs_dd03p-inttype = 'D'.
        cs_dd03p-leng = 8.
        cs_dd03p-intlen = 16.
      WHEN 'tims'.
        cs_dd03p-datatype = 'TIMS'.
        cs_dd03p-inttype = 'T'.
        cs_dd03p-leng = 6.
        cs_dd03p-intlen = 12.
      WHEN 'clnt'.
        cs_dd03p-datatype = 'CLNT'.
        cs_dd03p-inttype = 'C'.
        cs_dd03p-leng = 3.
        cs_dd03p-intlen = 6.
    ENDCASE.
  ENDMETHOD.


  METHOD set_decfloat_type.
    IF iv_base = 'df16_dec' OR iv_base = 'df16_raw' OR iv_base = 'df16_scl'.
      cs_dd03p-inttype = 'a'.
      cs_dd03p-intlen = 8.
    ELSE.
      cs_dd03p-inttype = 'e'.
      cs_dd03p-intlen = 16.
    ENDIF.
    IF iv_base = 'df16_dec'.
      cs_dd03p-datatype = 'D16D'.
    ELSEIF iv_base = 'df16_raw'.
      cs_dd03p-datatype = 'D16R'.
    ELSEIF iv_base = 'df16_scl'.
      cs_dd03p-datatype = 'D16S'.
    ELSEIF iv_base = 'df34_dec'.
      cs_dd03p-datatype = 'D34D'.
    ELSEIF iv_base = 'df34_raw'.
      cs_dd03p-datatype = 'D34R'.
    ELSE.
      cs_dd03p-datatype = 'D34S'.
    ENDIF.
    IF iv_length IS NOT INITIAL.
      cs_dd03p-leng = iv_length.
      cs_dd03p-decimals = iv_decimals.
    ENDIF.
  ENDMETHOD.


  METHOD read_data.
    DATA lv_name TYPE ddobjname.
    lv_name = iv_name.
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING name = lv_name langu = iv_language
      IMPORTING dd02v_wa = rs_data-dd02v dd09l_wa = rs_data-dd09l
      TABLES dd03p_tab = rs_data-dd03p dd05m_tab = rs_data-dd05m
             dd08v_tab = rs_data-dd08v dd12v_tab = rs_data-dd12v
             dd17v_tab = rs_data-dd17v dd35v_tab = rs_data-dd35v
             dd36m_tab = rs_data-dd36m
      EXCEPTIONS illegal_input = 1 OTHERS = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from DDIF_TABL_GET for { lv_name }| ).
    ENDIF.
  ENDMETHOD.


  METHOD serialize.

    DATA ls_dd03p LIKE LINE OF is_data-dd03p.
    DATA lv_key TYPE string.
    DATA lv_type TYPE string.
    DATA lv_pre TYPE string.
    DATA lv_int TYPE i.
    DATA lv_suffix TYPE string.
    DATA lv_notnull TYPE string.
    DATA lv_colon TYPE i.
    DATA lv_include TYPE string.
    DATA lv_extend TYPE string.

    rv_ddl = serialize_top( is_data ).
    rv_ddl = rv_ddl && |define table { to_lower( is_data-dd02v-tabname ) } \{\n\n|.
    LOOP AT is_data-dd03p INTO ls_dd03p
        WHERE ( fieldname NP '.INCLU*' OR groupname IS NOT INITIAL ) AND adminfield = '0'.
      lv_int = 0.
      IF ls_dd03p-keyflag = abap_true.
        lv_int = 4.
      ENDIF.
      IF ls_dd03p-groupname IS INITIAL.
        lv_int = lv_int + strlen( ls_dd03p-fieldname ).
      ELSE.
        lv_int = lv_int + strlen( ls_dd03p-groupname ).
      ENDIF.
      IF lv_int > lv_colon.
        lv_colon = lv_int.
      ENDIF.
    ENDLOOP.
    LOOP AT is_data-dd03p INTO ls_dd03p WHERE adminfield = '0'.
      CLEAR: lv_key, lv_notnull, lv_suffix.
      IF ls_dd03p-keyflag = abap_true.
        lv_key = |key |.
      ENDIF.
      lv_pre = |{ lv_key }{ to_lower( ls_dd03p-fieldname ) }|.
      IF ls_dd03p-groupname IS NOT INITIAL.
        lv_pre = |{ lv_key }{ to_lower( ls_dd03p-groupname ) }|.
      ENDIF.
      IF strlen( lv_pre ) < lv_colon.
        lv_pre = lv_pre && repeat(
          val = | |
          occ = lv_colon - strlen( lv_pre ) ).
      ENDIF.
      IF ls_dd03p-fieldname = '.INCLU--AP'.
        CONTINUE.
      ELSEIF ls_dd03p-fieldname CP '.INCLU*'.
        IF ls_dd03p-notnull = abap_true.
          lv_notnull = | not null|.
        ENDIF.
        lv_include = ls_dd03p-fieldname.
        IF lv_include CA '-'.
          SPLIT lv_include AT '-' INTO lv_include lv_suffix.
          lv_suffix = | with suffix { to_lower( lv_suffix ) }|.
        ENDIF.
        IF ls_dd03p-groupname IS INITIAL.
          rv_ddl = rv_ddl && |  { lv_key }include { to_lower( ls_dd03p-precfield ) }{ lv_suffix }{ lv_notnull }|.
        ELSE.
          rv_ddl = rv_ddl && |  { lv_pre } : include { to_lower( ls_dd03p-precfield ) }{ lv_suffix }{ lv_notnull }|.
        ENDIF.
        lv_extend = serialize_extend(
          is_dd03p = ls_dd03p
          is_data = is_data ).
        IF lv_extend IS INITIAL.
          rv_ddl = rv_ddl && |;\n|.
        ELSE.
          " ADT omits the terminator for an include that owns extensions.
          rv_ddl = rv_ddl && |\n|.
          rv_ddl = rv_ddl && lv_extend.
        ENDIF.
        CONTINUE.
      ENDIF.
      rv_ddl = rv_ddl && serialize_field_annotations(
        iv_fieldname = ls_dd03p-fieldname
        is_data = is_data ).
      rv_ddl = rv_ddl && serialize_fkey_annotations(
        iv_fieldname = ls_dd03p-fieldname
        is_data = is_data ).
      lv_type = serialize_type( ls_dd03p ).
      rv_ddl = rv_ddl && |  { lv_pre } : { lv_type }|.
      rv_ddl = rv_ddl && serialize_field_foreign_key(
        iv_fieldname = ls_dd03p-fieldname
        is_data = is_data ).
      rv_ddl = rv_ddl && serialize_value_help(
        iv_fieldname = ls_dd03p-fieldname
        is_data = is_data ).
      rv_ddl = rv_ddl && |;\n|.
    ENDLOOP.
    rv_ddl = rv_ddl && |\n\}|.
  ENDMETHOD.


  METHOD serialize_adt.
    DATA ls_object_type TYPE wbobjtype.
    DATA lv_object_key TYPE seu_objkey.
    DATA li_object_data TYPE REF TO if_wb_object_data_model.
    DATA lo_operator TYPE REF TO object.
    ls_object_type-objtype_tr = 'TABL'.
    ls_object_type-subtype_wb = 'DT'.
    lv_object_key = iv_name.
    CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
      EXPORTING
        object_type = ls_object_type
        object_key = lv_object_key
      RECEIVING
        result = lo_operator.
    CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
      EXPORTING
        version = 'A'
      IMPORTING
        eo_object_data = li_object_data.
    CALL METHOD li_object_data->('GET_CONTENT')
      IMPORTING
        p_data = rv_ddl.
  ENDMETHOD.


  METHOD has_more_extensions.

    DATA ls_dd08v LIKE LINE OF is_data-dd08v.
    DATA ls_dd35v LIKE LINE OF is_data-dd35v.

    LOOP AT is_data-dd08v INTO ls_dd08v
        WHERE ( noinherit = 'Y' OR checktable = '*' ) AND noinherit <> 'N'.
      READ TABLE it_fields TRANSPORTING NO FIELDS
        WITH KEY table_line = ls_dd08v-fieldname.
      IF sy-subrc = 0.
        rv_more = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    LOOP AT is_data-dd35v INTO ls_dd35v.
      IF ls_dd35v-shlpinher = abap_true.
        CONTINUE.
      ENDIF.
      READ TABLE it_fields TRANSPORTING NO FIELDS
        WITH KEY table_line = ls_dd35v-fieldname.
      IF sy-subrc = 0.
        rv_more = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_extend.

    DATA lv_index TYPE i.
    DATA ls_dd03p LIKE LINE OF is_data-dd03p.
    DATA lt_fields TYPE ty_fields.
    DATA lv_field LIKE LINE OF lt_fields.
    DATA ls_dd08v LIKE LINE OF is_data-dd08v.
    DATA ls_dd35v LIKE LINE OF is_data-dd35v.
    DATA lv_more TYPE abap_bool.

    READ TABLE is_data-dd03p TRANSPORTING NO FIELDS
      WITH KEY fieldname = is_dd03p-fieldname precfield = is_dd03p-precfield.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    lv_index = sy-tabix + 1.
    LOOP AT is_data-dd03p FROM lv_index INTO ls_dd03p.
      IF ls_dd03p-adminfield = '0'.
        EXIT.
      ENDIF.
      APPEND ls_dd03p-fieldname TO lt_fields.
    ENDLOOP.
    LOOP AT is_data-dd08v INTO ls_dd08v
        WHERE ( noinherit = 'Y' OR checktable = '*' ) AND noinherit <> 'N'.
      READ TABLE lt_fields TRANSPORTING NO FIELDS
        WITH KEY table_line = ls_dd08v-fieldname.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DELETE lt_fields WHERE table_line = ls_dd08v-fieldname.
      rv_ddl = rv_ddl && |\n|.
      IF ls_dd08v-checktable <> '*'.
        rv_ddl = rv_ddl && serialize_fkey_annotations(
          iv_fieldname = ls_dd08v-fieldname
            is_data = is_data ).
      ENDIF.
      rv_ddl = rv_ddl && |  extend { to_lower( ls_dd08v-fieldname ) } :|.
      IF ls_dd08v-checktable = '*'.
        rv_ddl = rv_ddl && |\n    remove foreign key|.
      ELSE.
        rv_ddl = rv_ddl && serialize_field_foreign_key(
          iv_fieldname = ls_dd08v-fieldname
            is_data = is_data ).
      ENDIF.
      READ TABLE is_data-dd35v INTO ls_dd35v WITH KEY fieldname = ls_dd08v-fieldname.
      IF sy-subrc = 0.
        IF ls_dd35v-shlpname = '*'.
          rv_ddl = rv_ddl && |\n    remove value help|.
        ELSEIF ls_dd35v-shlpinher <> abap_true.
          rv_ddl = rv_ddl && serialize_value_help(
            iv_fieldname = ls_dd08v-fieldname
              is_data = is_data ).
        ENDIF.
      ENDIF.
      lv_more = has_more_extensions(
        it_fields = lt_fields
        is_data = is_data ).
      IF lv_more = abap_true.
        rv_ddl = rv_ddl && |\n|.
      ELSE.
        rv_ddl = rv_ddl && |;\n|.
      ENDIF.
    ENDLOOP.
    LOOP AT is_data-dd35v INTO ls_dd35v.
      READ TABLE lt_fields INTO lv_field
        WITH KEY table_line = ls_dd35v-fieldname.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF ls_dd35v-shlpinher = abap_true.
        CONTINUE.
      ENDIF.
      DELETE lt_fields WHERE table_line = ls_dd35v-fieldname.
      rv_ddl = rv_ddl && |\n  extend { to_lower( lv_field ) } :|.
      IF ls_dd35v-shlpname = '*'.
        rv_ddl = rv_ddl && |\n    remove value help|.
      ELSEIF ls_dd35v-shlpinher <> abap_true.
        rv_ddl = rv_ddl && serialize_value_help(
          iv_fieldname = lv_field
            is_data = is_data ).
      ENDIF.
      lv_more = has_more_extensions(
        it_fields = lt_fields
        is_data = is_data ).
      IF lv_more = abap_true.
        rv_ddl = rv_ddl && |\n|.
      ELSE.
        rv_ddl = rv_ddl && |;\n|.
      ENDIF.
    ENDLOOP.
    REPLACE ALL OCCURRENCES OF |\n  | IN rv_ddl WITH |\n    |.
  ENDMETHOD.


  METHOD serialize_field_annotations.
    DATA ls_dd03p LIKE LINE OF is_data-dd03p.
    DATA ls_reference LIKE LINE OF is_data-dd03p.
    DATA lv_is_amount TYPE abap_bool.
    DATA lv_reference TYPE string.
    READ TABLE is_data-dd03p INTO ls_dd03p WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF ( ls_dd03p-rollname IS INITIAL AND ls_dd03p-precfield IS INITIAL
        OR ls_dd03p-comptype = 'R' AND ls_dd03p-reftype = 'B' )
        AND ls_dd03p-ddtext IS NOT INITIAL.
      rv_ddl = rv_ddl && |  @EndUserText.label : { escape_string( ls_dd03p-ddtext ) }\n|.
    ENDIF.
    IF ls_dd03p-languflag = abap_true.
      rv_ddl = rv_ddl && |  @AbapCatalog.textLanguage\n|.
    ENDIF.
    IF ls_dd03p-reftable IS NOT INITIAL AND ls_dd03p-reffield IS NOT INITIAL.
      READ TABLE is_data-dd03p INTO ls_reference WITH KEY fieldname = ls_dd03p-reffield.
      IF sy-subrc = 0 AND ls_reference-datatype = 'CUKY'.
        lv_is_amount = abap_true.
      ELSEIF ls_dd03p-datatype = 'CURR'.
        lv_is_amount = abap_true.
      ENDIF.
      IF lv_is_amount = abap_true.
        lv_reference = |{ to_lower( ls_dd03p-reftable ) }.{ to_lower( ls_dd03p-reffield ) }|.
        rv_ddl = rv_ddl && |  @Semantics.amount.currencyCode : '{ lv_reference }'\n|.
      ELSE.
        lv_reference = |{ to_lower( ls_dd03p-reftable ) }.{ to_lower( ls_dd03p-reffield ) }|.
        rv_ddl = rv_ddl && |  @Semantics.quantity.unitOfMeasure : '{ lv_reference }'\n|.
      ENDIF.
    ENDIF.
    IF ls_dd03p-rollname IS INITIAL
        AND ( ls_dd03p-datatype(3) = 'D16' OR ls_dd03p-datatype(3) = 'D34' ).
      rv_ddl = rv_ddl && |  @AbapCatalog.decfloat.outputStyle : #NORMAL\n|.
    ENDIF.
  ENDMETHOD.


  METHOD serialize_field_foreign_key.

    DATA ls_dd08v LIKE LINE OF is_data-dd08v.
    DATA ls_dd05m LIKE LINE OF is_data-dd05m.
    DATA lt_dd05m TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY.
    DATA lv_pre TYPE string.
    DATA lv_cardinality TYPE string.
    DATA lv_target TYPE string.
    READ TABLE is_data-dd08v INTO ls_dd08v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    " Cardinality is optional in table DDL; keep the separator when omitted.
    lv_cardinality = | |.
    IF ls_dd08v-cardleft = 'C' AND ls_dd08v-card = '1'.
      lv_cardinality = | [1,0..1] |.
    ELSEIF ls_dd08v-cardleft = '1' AND ls_dd08v-card = 'C'.
      lv_cardinality = | [0..1,1] |.
    ELSEIF ls_dd08v-cardleft = '1' AND ls_dd08v-card = '1'.
      lv_cardinality = | [1,1] |.
    ELSEIF ls_dd08v-cardleft = '1' AND ls_dd08v-card = 'N'.
      lv_cardinality = | [1..*,1] |.
    ELSEIF ls_dd08v-cardleft = '1' AND ls_dd08v-card = 'CN'.
      lv_cardinality = | [0..*,1] |.
    ELSEIF ls_dd08v-cardleft = 'C' AND ls_dd08v-card = 'CN'.
      lv_cardinality = | [0..*,0..1] |.
    ELSEIF ls_dd08v-cardleft = 'C' AND ls_dd08v-card = 'C'.
      lv_cardinality = | [0..1,0..1] |.
    ELSEIF ls_dd08v-cardleft = 'N' AND ls_dd08v-card = 'N'.
      lv_cardinality = | [1..*,] |.
    ELSEIF ls_dd08v-cardleft = 'C' AND ls_dd08v-card = 'N'.
      lv_cardinality = | [1..*,0..1] |.
    ELSEIF ls_dd08v-cardleft IS NOT INITIAL OR ls_dd08v-card IS NOT INITIAL.
      " DDIC may contain legacy or incomplete cardinality values which have no
      " direct DDL representation. Cardinality is optional in table DDL, so
      " preserve the foreign key and omit only the cardinality in this case.
      lv_cardinality = | |.
    ENDIF.
    rv_ddl = rv_ddl && |\n    with foreign key{ lv_cardinality }{ to_lower( ls_dd08v-checktable ) }|.
    LOOP AT is_data-dd05m INTO ls_dd05m
        WHERE fieldname = iv_fieldname AND fortable <> '*'.
      APPEND ls_dd05m TO lt_dd05m.
    ENDLOOP.
    SORT lt_dd05m BY primpos ASCENDING.
    LOOP AT lt_dd05m INTO ls_dd05m.
      IF lv_pre IS INITIAL.
        lv_pre = |\n      where |.
      ELSE.
        lv_pre = |\n        and |.
      ENDIF.
      IF ls_dd05m-fortable(1) = |'|.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd05m-checkfield ) } = { ls_dd05m-fortable }|.
      ELSE.
        lv_target = |{ to_lower( ls_dd05m-fortable ) }.{ to_lower( ls_dd05m-forkey ) }|.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd05m-checkfield ) } = { lv_target }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD serialize_fkey_annotations.
    DATA ls_dd08v LIKE LINE OF is_data-dd08v.
    READ TABLE is_data-dd08v INTO ls_dd08v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF ls_dd08v-ddtext IS NOT INITIAL.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.label : { escape_string( ls_dd08v-ddtext ) }\n|.
    ENDIF.
    IF ls_dd08v-frkart IS INITIAL.
    ELSEIF ls_dd08v-frkart = 'TEXT'.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #TEXT_KEY\n|.
    ELSEIF ls_dd08v-frkart = 'REF'.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #NON_KEY\n|.
    ELSEIF ls_dd08v-frkart = 'KEY'.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #KEY\n|.
    ELSE.
      zcx_abapgit_exception=>raise(
        |TABL DDL serialization error: unsupported foreign key type { ls_dd08v-frkart }| ).
    ENDIF.
    IF ls_dd08v-checkflag = abap_false OR ls_dd08v-checkflag = 'N'.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.screenCheck : true\n|.
    ELSE.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.screenCheck : false\n|.
    ENDIF.
    IF ls_dd08v-arbgb IS NOT INITIAL.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.messageClass : { escape_string( ls_dd08v-arbgb ) }\n|.
    ENDIF.
    IF ls_dd08v-msgnr IS NOT INITIAL.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.messageNumber : { escape_string( ls_dd08v-msgnr ) }\n|.
    ENDIF.
  ENDMETHOD.


  METHOD serialize_top.
    FIELD-SYMBOLS <lv_pk_is_invhash> TYPE c.
    FIELD-SYMBOLS <lv_is_gtt> TYPE abap_bool.
    DATA lv_replacement_object TYPE string.
    IF is_data-dd02v-exclass NOT BETWEEN '0' AND '4'.
      zcx_abapgit_exception=>raise(
        |TABL DDL serialization error: unsupported enhancement category { is_data-dd02v-exclass }| ).
    ENDIF.
    IF is_data-dd02v-contflag IS INITIAL.
      zcx_abapgit_exception=>raise( 'TABL DDL serialization error: delivery class is missing' ).
    ENDIF.
    rv_ddl = rv_ddl && |@EndUserText.label : { escape_string( is_data-dd02v-ddtext ) }\n|.
    CASE is_data-dd02v-exclass.
      WHEN '0'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancement.category : #NOT_CLASSIFIED\n|.
      WHEN '1'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE\n|.
      WHEN '2'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER\n|.
      WHEN '3'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER_NUMERIC\n|.
      WHEN '4'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancement.category : #EXTENSIBLE_ANY\n|.
    ENDCASE.
    IF is_data-dd02v-tabclass <> 'TRANSP'.
      zcx_abapgit_exception=>raise(
        |TABL DDL serialization error: unsupported table category { is_data-dd02v-tabclass }| ).
    ENDIF.
    ASSIGN COMPONENT 'IS_GTT' OF STRUCTURE is_data-dd02v TO <lv_is_gtt>.
    IF sy-subrc = 0 AND <lv_is_gtt> = abap_true.
      rv_ddl = rv_ddl && |@AbapCatalog.tableCategory : #GLOBAL_TEMPORARY\n|.
    ELSE.
      rv_ddl = rv_ddl && |@AbapCatalog.tableCategory : #TRANSPARENT\n|.
    ENDIF.
    IF is_data-dd02v-authclass = '01'.
      rv_ddl = rv_ddl && |@AbapCatalog.activationType : #NAMETAB_GENERATION_OFFLINE\n|.
    ELSEIF is_data-dd02v-authclass = '02'.
      rv_ddl = rv_ddl && |@AbapCatalog.activationType : #ADAPT_C_STRUCTURES\n|.
    ELSEIF is_data-dd02v-authclass IS NOT INITIAL.
      zcx_abapgit_exception=>raise(
        |TABL DDL serialization error: unsupported activation type { is_data-dd02v-authclass }| ).
    ENDIF.
    rv_ddl = rv_ddl && |@AbapCatalog.deliveryClass : #{ is_data-dd02v-contflag }\n|.
    IF is_data-dd02v-mainflag = abap_true.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #ALLOWED\n|.
    ELSEIF is_data-dd02v-mainflag = 'N'.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #NOT_ALLOWED\n|.
    ELSEIF is_data-dd02v-mainflag IS INITIAL.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #RESTRICTED\n|.
    ELSE.
      zcx_abapgit_exception=>raise(
        |TABL DDL serialization error: unsupported data maintenance value { is_data-dd02v-mainflag }| ).
    ENDIF.
    lv_replacement_object = get_replacement_object( is_data-dd02v-viewref ).
    IF lv_replacement_object IS NOT INITIAL.
      rv_ddl = rv_ddl && |@AbapCatalog.replacementObject : { escape_string( to_lower( lv_replacement_object ) ) }\n|.
    ENDIF.
    ASSIGN COMPONENT 'PK_IS_INVHASH' OF STRUCTURE is_data-dd02v TO <lv_pk_is_invhash>.
    IF sy-subrc = 0 AND <lv_pk_is_invhash> = abap_true.
      rv_ddl = rv_ddl && |@AbapCatalog.primaryKey.invertedHashIndex : true\n|.
    ENDIF.
  ENDMETHOD.


  METHOD serialize_type.
    DATA lv_notnull TYPE string.
    IF is_dd03p-notnull = abap_true.
      lv_notnull = | not null|.
    ENDIF.
    IF is_dd03p-rollname IS NOT INITIAL.
      rv_type = |{ to_lower( is_dd03p-rollname ) }{ lv_notnull }|.
    ELSE.
      rv_type = serialize_type_core( is_dd03p ).
      rv_type = rv_type && lv_notnull.
    ENDIF.
  ENDMETHOD.


  METHOD serialize_type_core.
    DATA lv_leng TYPE i.
    DATA lv_decimals TYPE i.
    lv_leng = is_dd03p-leng.
    lv_decimals = is_dd03p-decimals.
    IF is_dd03p-datatype = 'STRG'.
      rv_type = |abap.string({ lv_leng })|.
    ELSEIF is_dd03p-datatype = 'RSTR'.
      rv_type = |abap.rawstring({ lv_leng })|.
    ELSEIF is_dd03p-datatype = 'SSTR'.
      rv_type = |abap.sstring({ lv_leng })|.
    ELSEIF is_dd03p-datatype = 'DEC' OR is_dd03p-datatype = 'CURR'
        OR is_dd03p-datatype = 'QUAN'.
      rv_type = |abap.{ to_lower( is_dd03p-datatype ) }({ lv_leng },{ lv_decimals })|.
    ELSEIF is_dd03p-datatype = 'D16D' OR is_dd03p-datatype = 'D34D'.
      IF is_dd03p-datatype = 'D16D'.
        rv_type = |abap.df16_dec({ lv_leng },{ lv_decimals })|.
      ELSE.
        rv_type = |abap.df34_dec({ lv_leng },{ lv_decimals })|.
      ENDIF.
    ELSEIF is_dd03p-datatype = 'CHAR' OR is_dd03p-datatype = 'NUMC'
        OR is_dd03p-datatype = 'RAW' OR is_dd03p-datatype = 'UNIT'.
      rv_type = |abap.{ to_lower( is_dd03p-datatype ) }({ lv_leng })|.
    ELSE.
      rv_type = serialize_type_special( is_dd03p ).
    ENDIF.
  ENDMETHOD.


  METHOD serialize_type_special.
    CASE is_dd03p-datatype.
      WHEN 'INT4'.
        rv_type = |abap.int4|.
      WHEN 'ACCP'.
        rv_type = |abap.accp|.
      WHEN 'LANG'.
        rv_type = |abap.lang|.
      WHEN 'DATN'.
        rv_type = |abap.datn|.
      WHEN 'TIMN'.
        rv_type = |abap.timn|.
      WHEN 'UTCL'.
        rv_type = |abap.utcl|.
      WHEN 'INT8'.
        rv_type = |abap.int8|.
      WHEN 'D16R'.
        rv_type = |abap.df16_raw|.
      WHEN 'D16S'.
        rv_type = |abap.df16_scl|.
      WHEN 'D16N'.
        rv_type = |abap.d16n|.
      WHEN 'D34S'.
        rv_type = |abap.df34_scl|.
      WHEN 'D34R'.
        rv_type = |abap.df34_raw|.
      WHEN 'D34N'.
        rv_type = |abap.d34n|.
      WHEN 'INT2'.
        rv_type = |abap.int2|.
      WHEN 'INT1'.
        rv_type = |abap.int1|.
      WHEN 'CUKY'.
        rv_type = |abap.cuky|.
      WHEN 'DATS'.
        rv_type = |abap.dats|.
      WHEN 'TIMS'.
        rv_type = |abap.tims|.
      WHEN 'FLTP'.
        rv_type = |abap.fltp|.
      WHEN 'CLNT'.
        rv_type = |abap.clnt|.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise(
          |TABL DDL serialization error: unsupported field type { is_dd03p-datatype }| ).
    ENDCASE.
  ENDMETHOD.


  METHOD serialize_value_help.
    DATA ls_dd35v LIKE LINE OF is_data-dd35v.
    DATA ls_dd36m LIKE LINE OF is_data-dd36m.
    DATA lt_dd36m TYPE STANDARD TABLE OF dd36m WITH DEFAULT KEY.
    DATA lv_pre TYPE string.
    DATA lv_target TYPE string.
    READ TABLE is_data-dd35v INTO ls_dd35v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0 OR ls_dd35v-shlpname = '*'.
      RETURN.
    ENDIF.
    rv_ddl = rv_ddl && |\n    with value help { to_lower( ls_dd35v-shlpname ) }|.
    LOOP AT is_data-dd36m INTO ls_dd36m
        WHERE fieldname = iv_fieldname AND shlpname = ls_dd35v-shlpname AND shtype <> 'G'.
      APPEND ls_dd36m TO lt_dd36m.
    ENDLOOP.
    SORT lt_dd36m BY shlpfield ASCENDING flposition ASCENDING.
    LOOP AT lt_dd36m INTO ls_dd36m.
      IF lv_pre IS INITIAL.
        lv_pre = |\n      where |.
      ELSE.
        lv_pre = |\n        and |.
      ENDIF.
      IF ls_dd36m-shtype = 'C'.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd36m-shlpfield ) } = { ls_dd36m-shtable }|.
      ELSE.
        lv_target = |{ to_lower( ls_dd36m-shtable ) }.{ to_lower( ls_dd36m-shfield ) }|.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd36m-shlpfield ) } = { lv_target }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
