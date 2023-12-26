CLASS zcl_abapgit_object_tabl_ddl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_dd03p,
             fieldname TYPE c LENGTH 30,
             keyflag   TYPE abap_bool,
             notnull   TYPE abap_bool,
             datatype  TYPE c LENGTH 30,
             rollname  TYPE c LENGTH 30,
             leng      TYPE n LENGTH 6,
             intlen    TYPE n LENGTH 6,
             inttype   TYPE c LENGTH 1,
           END OF ty_dd03p.

    TYPES: BEGIN OF ty_dd05m,
             fieldname  TYPE c LENGTH 30,
             fortable   TYPE c LENGTH 30,
             forkey     TYPE c LENGTH 30,
             checktable TYPE c LENGTH 30,
             checkfield TYPE c LENGTH 30,
             primpos    TYPE n LENGTH 4,
           END OF ty_dd05m.

    TYPES: BEGIN OF ty_dd08v,
             fieldname  TYPE c LENGTH 30,
             checktable TYPE c LENGTH 30,
             ddtext     TYPE string,
             frkart     TYPE c LENGTH 10,
             card       TYPE c LENGTH 1,
             cardleft   TYPE c LENGTH 1,
           END OF ty_dd08v.

    TYPES: BEGIN OF ty_internal,
             BEGIN OF dd02v,
               tabname  TYPE c LENGTH 30,
               contflag TYPE c LENGTH 1,
               exclass  TYPE c LENGTH 1,
               mainflag TYPE c LENGTH 1,
               ddtext   TYPE string,
               tabclass TYPE string,
             END OF dd02v,
             dd03p_table TYPE STANDARD TABLE OF ty_dd03p WITH DEFAULT KEY,
             dd05m_table TYPE STANDARD TABLE OF ty_dd05m WITH DEFAULT KEY,
             dd08v_table TYPE STANDARD TABLE OF ty_dd08v WITH DEFAULT KEY,
           END OF ty_internal.

    METHODS serialize
      IMPORTING
        is_data TYPE ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string.

    METHODS deserialize
      IMPORTING
        iv_ddl TYPE string
      RETURNING
        VALUE(rs_data) TYPE ty_internal.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS parse_top_annotations
      CHANGING
        cs_data TYPE ty_internal
        cv_ddl  TYPE string.

    METHODS parse_field_annotations
      EXPORTING
        es_dd08v TYPE ty_dd08v
      CHANGING
        cv_ddl TYPE string.

    METHODS parse_field
      IMPORTING
        iv_field TYPE string
      CHANGING
        cs_data TYPE ty_internal.

    METHODS serialize_top
      IMPORTING
        is_data TYPE ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string.

    METHODS serialize_field_annotations
      IMPORTING
        iv_fieldname  TYPE clike
        is_data       TYPE ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string.

    METHODS serialize_field_foreign_key
      IMPORTING
        iv_fieldname  TYPE clike
        is_data       TYPE ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string.

    METHODS escape_string
      IMPORTING
        iv_string        TYPE clike
      RETURNING
        VALUE(rv_string) TYPE string.

    METHODS unescape_string
      IMPORTING
        iv_string        TYPE clike
      RETURNING
        VALUE(rv_string) TYPE string.

    METHODS serialize_type
      IMPORTING is_dd03p TYPE ty_dd03p
      RETURNING VALUE(rv_type) TYPE string.

    METHODS parse_type
      IMPORTING iv_token TYPE string
      CHANGING cs_dd03p TYPE ty_dd03p.
ENDCLASS.



CLASS zcl_abapgit_object_tabl_ddl IMPLEMENTATION.

  METHOD unescape_string.
    rv_string = iv_string.
    REPLACE FIRST OCCURRENCE OF REGEX |^'| IN rv_string WITH ||.
    REPLACE FIRST OCCURRENCE OF REGEX |'$| IN rv_string WITH ||.
  ENDMETHOD.

  METHOD escape_string.
* todo
    rv_string = |'{ iv_string }'|.
  ENDMETHOD.

  METHOD parse_type.

    DATA lv_token TYPE string.

    lv_token = iv_token.
    IF lv_token CP 'abap.*'.
      lv_token = lv_token+5.
      IF lv_token(4) = 'char'.
* todo, length
        cs_dd03p-datatype = 'CHAR'.
      ELSEIF lv_token(6) = 'string'.
        cs_dd03p-intlen = 8.
        cs_dd03p-inttype = 'g'.
        cs_dd03p-datatype = 'STRG'.
      ELSE.
        ASSERT 1 = 'todo'.
      ENDIF.
    ELSE.
      cs_dd03p-rollname = to_upper( lv_token ).
    ENDIF.

  ENDMETHOD.

  METHOD parse_top_annotations.

    DATA lv_annotation TYPE string.
    DATA lv_name       TYPE string.
    DATA lv_value      TYPE string.


    WHILE cv_ddl CP '@*'.
      SPLIT cv_ddl AT |\n| INTO lv_annotation cv_ddl.
      SPLIT lv_annotation AT ':' INTO lv_name lv_value.
      CONDENSE lv_name.
      CONDENSE lv_value.
      ASSERT lv_name IS NOT INITIAL.
      ASSERT lv_value IS NOT INITIAL.


      CASE lv_name.
        WHEN '@EndUserText.label'.
          cs_data-dd02v-ddtext = unescape_string( lv_value ).
        WHEN '@AbapCatalog.enhancementCategory'.
          CASE lv_value.
            WHEN '#NOT_EXTENSIBLE'.
              cs_data-dd02v-contflag = '1'.
            WHEN OTHERS.
              ASSERT 1 = 'todo'.
          ENDCASE.
        WHEN '@AbapCatalog.tableCategory'.
          CASE lv_value.
            WHEN '#TRANSPARENT'.
              cs_data-dd02v-tabclass = 'TRANSP'.
            WHEN OTHERS.
              ASSERT 1 = 'todo'.
          ENDCASE.
        WHEN '@AbapCatalog.deliveryClass'.
          ASSERT lv_value(1) = '#'.
          cs_data-dd02v-contflag = lv_value+1.
        WHEN '@AbapCatalog.dataMaintenance'.
          CASE lv_value.
            WHEN '#ALLOWED'.
              cs_data-dd02v-mainflag = abap_true.
            WHEN '#LIMITED'.
              cs_data-dd02v-mainflag = abap_false.
            WHEN OTHERS.
              ASSERT 1 = 'todo'.
          ENDCASE.
        WHEN OTHERS.
          WRITE: / 'todo:', lv_name, lv_value.
          ASSERT 1 = 'todo'.
      ENDCASE.

    ENDWHILE.

  ENDMETHOD.

  METHOD deserialize.

* https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenddicddl_define_table.htm

* CL_DDL_PARSER, CL_SBD_STRUCTURE_OBJDATA serializer in local class?
" todo, NEW cl_sbd_structure_persist( )->get_source(
"   EXPORTING
"     i_object_key = 'ZABAPGIT'
"   IMPORTING
"     e_source = DATA(sdf) ).

    DATA lv_ddl    TYPE string.
    DATA lv_fields TYPE string.
    DATA lv_start  TYPE i.
    DATA lv_length TYPE i.
    DATA lv_end    TYPE i.
    DATA lt_fields TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_field  TYPE string.


    lv_ddl = iv_ddl.

    parse_top_annotations( CHANGING
      cs_data = rs_data
      cv_ddl  = lv_ddl ).

    FIND FIRST OCCURRENCE OF '{' IN lv_ddl MATCH OFFSET lv_start.
    ASSERT lv_start > 0.
    FIND FIRST OCCURRENCE OF '}' IN lv_ddl MATCH OFFSET lv_end.
    ASSERT lv_end > 0.

    lv_start = lv_start + 1.
    lv_length = lv_end - lv_start - 1.
    lv_fields = lv_ddl+lv_start(lv_length).
    SPLIT lv_fields AT |;| INTO TABLE lt_fields.

    LOOP AT lt_fields INTO lv_field WHERE table_line IS NOT INITIAL.
      parse_field( EXPORTING iv_field = lv_field CHANGING cs_data = rs_data ).
    ENDLOOP.

  ENDMETHOD.

  METHOD parse_field_annotations.

    DATA lv_annotation TYPE string.
    DATA lv_name       TYPE string.
    DATA lv_value      TYPE string.


    REPLACE FIRST OCCURRENCE OF REGEX '^[\n ]*' IN cv_ddl WITH ||.

    WHILE cv_ddl CP '@*'.
      SPLIT cv_ddl AT |\n| INTO lv_annotation cv_ddl.
      CONDENSE cv_ddl.

      SPLIT lv_annotation AT ':' INTO lv_name lv_value.
      CONDENSE lv_name.
      CONDENSE lv_value.
      ASSERT lv_name IS NOT INITIAL.
      ASSERT lv_value IS NOT INITIAL.

      CASE lv_name.
        WHEN '@AbapCatalog.foreignKey.label'.
          es_dd08v-ddtext = unescape_string( lv_value ).
        WHEN '@AbapCatalog.foreignKey.keyType'.
          ASSERT lv_value(1) = '#'.
          es_dd08v-frkart = lv_value+1.
        WHEN '@AbapCatalog.foreignKey.screenCheck'.
          ASSERT lv_value = 'true'.
        WHEN OTHERS.
          WRITE: / 'todo:', lv_name, lv_value.
          ASSERT 1 = 'todo'.
      ENDCASE.
    ENDWHILE.

  ENDMETHOD.

  METHOD parse_field.

    CONSTANTS: BEGIN OF lc_mode,
                 start TYPE i VALUE 0,
                 colon TYPE i VALUE 1,
                 type  TYPE i VALUE 2,
                 aftertype TYPE i VALUE 2,
                 null      TYPE i VALUE 2,
                 afternull TYPE i VALUE 2,
               END OF lc_mode.

    DATA lv_field  TYPE string.
    DATA lv_mode   TYPE i.
    DATA lt_tokens TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_token  TYPE string.
    DATA ls_dd08v  TYPE ty_dd08v.

    FIELD-SYMBOLS <ls_dd03p> LIKE LINE OF cs_data-dd03p_table.


    lv_field = iv_field.
    parse_field_annotations(
      IMPORTING es_dd08v = ls_dd08v
      CHANGING cv_ddl = lv_field ).

    SPLIT lv_field AT space INTO TABLE lt_tokens.

    APPEND INITIAL LINE TO cs_data-dd03p_table ASSIGNING <ls_dd03p>.

    LOOP AT lt_tokens INTO lv_token WHERE table_line IS NOT INITIAL.
      CASE lv_mode.
        WHEN lc_mode-start.
* todo, is it possible to have a key field named "key" ?
          IF lv_token = 'key'.
            <ls_dd03p>-keyflag = abap_true.
          ELSE.
            <ls_dd03p>-fieldname = to_upper( lv_token ).
            lv_mode = lc_mode-colon.
          ENDIF.
        WHEN lc_mode-colon.
          ASSERT lv_token = ':'.
          lv_mode = lc_mode-type.
        WHEN lc_mode-type.
          parse_type(
            EXPORTING iv_token = lv_token
            CHANGING cs_dd03p = <ls_dd03p> ).
          RETURN.
        WHEN lc_mode-aftertype.
          IF lv_token = 'not'.
            <ls_dd03p>-notnull = abap_true.
            lv_mode = lc_mode-null.
          ENDIF.
        WHEN lc_mode-null.
          ASSERT lv_token = 'null'.
          lv_mode = lc_mode-afternull.
        WHEN lc_mode-afternull.
          ASSERT lv_token = 'with'.
          RETURN. " todo
        WHEN OTHERS.
          ASSERT 1 = 'todo'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize_top.

    rv_ddl = rv_ddl && |@EndUserText.label : { escape_string( is_data-dd02v-ddtext ) }\n|.

    CASE is_data-dd02v-exclass.
      WHEN '1'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE\n|.
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

    CASE is_data-dd02v-tabclass.
      WHEN 'TRANSP'.
        rv_ddl = rv_ddl && |@AbapCatalog.tableCategory : #TRANSPARENT\n|.
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

    rv_ddl = rv_ddl && |@AbapCatalog.deliveryClass : #{ is_data-dd02v-contflag }\n|.

    IF is_data-dd02v-mainflag = abap_true.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #ALLOWED\n|.
    ELSE.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #LIMITED\n|.
    ENDIF.

  ENDMETHOD.

  METHOD serialize_field_annotations.

    DATA ls_dd08v LIKE LINE OF is_data-dd08v_table.

    READ TABLE is_data-dd08v_table INTO ls_dd08v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_dd08v-ddtext IS NOT INITIAL.
      rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.label : { escape_string( ls_dd08v-ddtext ) }\n|.
    ENDIF.

    rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #{ ls_dd08v-frkart }\n|.

    rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.screenCheck : true\n|.

  ENDMETHOD.

  METHOD serialize_field_foreign_key.

    DATA ls_dd08v       LIKE LINE OF is_data-dd08v_table.
    DATA ls_dd05m       LIKE LINE OF is_data-dd05m_table.
    DATA lv_pre         TYPE string.
    DATA lv_cardinality TYPE string.

    READ TABLE is_data-dd08v_table INTO ls_dd08v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_dd08v-cardleft = 'C' AND ls_dd08v-card = '1'.
      lv_cardinality = |[1,0..1]|.
    ELSEIF ls_dd08v-cardleft = '1' AND ls_dd08v-card = 'N'.
      lv_cardinality = |[1..*,1]|.
    ELSE.
      ASSERT 1 = 'todo'.
    ENDIF.

    rv_ddl = rv_ddl && |\n    with foreign key { lv_cardinality } { to_lower( ls_dd08v-checktable ) }|.

* assumption: dd05m table is sorted by PRIMPOS ascending
    LOOP AT is_data-dd05m_table INTO ls_dd05m WHERE fieldname = iv_fieldname.
      IF lv_pre IS INITIAL.
        lv_pre = |\n      where |.
      ELSE.
        lv_pre = |\n        and |.
      ENDIF.
      rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd05m-checkfield ) } = {
        to_lower( ls_dd05m-fortable ) }.{ to_lower( ls_dd05m-forkey ) }|.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize.

    DATA ls_dd03p LIKE LINE OF is_data-dd03p_table.
    DATA lv_key   TYPE string.
    DATA lv_type  TYPE string.
    DATA lv_pre   TYPE string.
    DATA lv_int   TYPE i.
    DATA lv_colon TYPE i.


    rv_ddl = rv_ddl && serialize_top( is_data ).

    rv_ddl = rv_ddl && |define table { to_lower( is_data-dd02v-tabname ) } \{\n|.

    LOOP AT is_data-dd03p_table INTO ls_dd03p.
      lv_int = 0.
      IF ls_dd03p-keyflag = abap_true.
        lv_int = 4.
      ENDIF.
      lv_int = lv_int + strlen( ls_dd03p-fieldname ).
      IF lv_int > lv_colon.
        lv_colon = lv_int.
      ENDIF.
    ENDLOOP.

    LOOP AT is_data-dd03p_table INTO ls_dd03p.
      CLEAR lv_key.
      IF ls_dd03p-keyflag = abap_true.
        lv_key = |key |.
      ENDIF.

      rv_ddl = rv_ddl && serialize_field_annotations(
        iv_fieldname = ls_dd03p-fieldname
        is_data      = is_data ).

      lv_type = serialize_type( ls_dd03p ).

      lv_pre = |{ lv_key }{ to_lower( ls_dd03p-fieldname ) }|.
      IF strlen( lv_pre ) < lv_colon.
        lv_pre = lv_pre && repeat(
          val = | |
          occ = lv_colon - strlen( lv_pre ) ).
      ENDIF.
      rv_ddl = rv_ddl && |  { lv_pre } : { lv_type }|.
      rv_ddl = rv_ddl && serialize_field_foreign_key(
        iv_fieldname = ls_dd03p-fieldname
        is_data      = is_data ).
      rv_ddl = rv_ddl && |;\n|.
    ENDLOOP.
    rv_ddl = rv_ddl && |\n|.

    rv_ddl = rv_ddl && |\}|.

  ENDMETHOD.

  METHOD serialize_type.

    DATA lv_notnull TYPE string.
    DATA lv_int TYPE i.

    IF is_dd03p-notnull = abap_true.
      lv_notnull = | not null|.
    ENDIF.

    IF is_dd03p-rollname IS NOT INITIAL.
      rv_type = |{ to_lower( is_dd03p-rollname ) }{ lv_notnull }|.
    ELSE.
      lv_int = is_dd03p-leng.
      CASE is_dd03p-datatype.
        WHEN 'STRG'.
          rv_type = |abap.string(0)|.
        WHEN OTHERS.
          rv_type = |abap.{ to_lower( is_dd03p-datatype ) }({ lv_int }){ lv_notnull }|.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
