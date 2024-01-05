CLASS zcl_abapgit_object_tabl_ddl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read_data
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_object_tabl=>ty_internal .
    METHODS serialize
      IMPORTING
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string .
    METHODS deserialize
      IMPORTING
        !iv_ddl        TYPE string
      RETURNING
        VALUE(rs_data) TYPE zif_abapgit_object_tabl=>ty_internal .
    METHODS serialize_adt
      IMPORTING
        !iv_name      TYPE tadir-obj_name
      RETURNING
        VALUE(rv_ddl) TYPE string
      RAISING
        cx_static_check .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS parse_top_annotations
      CHANGING
        !cs_data TYPE zif_abapgit_object_tabl=>ty_internal
        !cv_ddl  TYPE string .
    METHODS parse_field_annotations
      EXPORTING
        !es_dd08v TYPE dd08v
      CHANGING
        !cv_ddl   TYPE string .
    METHODS parse_field
      IMPORTING
        !iv_field TYPE string
      CHANGING
        !cs_data  TYPE zif_abapgit_object_tabl=>ty_internal .
    METHODS serialize_top
      IMPORTING
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string .
    METHODS serialize_field_annotations
      IMPORTING
        !iv_fieldname TYPE clike
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string .
    METHODS serialize_field_foreign_key
      IMPORTING
        !iv_fieldname TYPE clike
        !is_data      TYPE zif_abapgit_object_tabl=>ty_internal
      RETURNING
        VALUE(rv_ddl) TYPE string .
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
        VALUE(rv_type) TYPE string .
    METHODS parse_type
      IMPORTING
        !iv_token TYPE string
      CHANGING
        !cs_dd03p TYPE dd03p .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_TABL_DDL IMPLEMENTATION.


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


  METHOD escape_string.
    rv_string = |'{ replace( val  = iv_string
                             sub  = |'|
                             with = |''|
                             occ  = 0 ) }'|.
  ENDMETHOD.


  METHOD parse_field.

    CONSTANTS: BEGIN OF lc_mode,
                 start     TYPE i VALUE 0,
                 colon     TYPE i VALUE 1,
                 type      TYPE i VALUE 2,
                 aftertype TYPE i VALUE 2,
                 null      TYPE i VALUE 2,
                 afternull TYPE i VALUE 2,
               END OF lc_mode.

    DATA lv_field  TYPE string.
    DATA lv_mode   TYPE i.
    DATA lt_tokens TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_token  TYPE string.
    DATA ls_dd08v  TYPE dd08v.

    FIELD-SYMBOLS <ls_dd03p> LIKE LINE OF cs_data-dd03p.


    lv_field = iv_field.
    parse_field_annotations(
      IMPORTING es_dd08v = ls_dd08v
      CHANGING cv_ddl = lv_field ).

    SPLIT lv_field AT space INTO TABLE lt_tokens.

    APPEND INITIAL LINE TO cs_data-dd03p ASSIGNING <ls_dd03p>.

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


  METHOD read_data.
* temporary method for testing

    DATA lv_name TYPE ddobjname.

    lv_name = iv_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = 'E'
      IMPORTING
        dd02v_wa      = rs_data-dd02v
        dd09l_wa      = rs_data-dd09l
      TABLES
        dd03p_tab     = rs_data-dd03p
        dd05m_tab     = rs_data-dd05m
        dd08v_tab     = rs_data-dd08v
        dd12v_tab     = rs_data-dd12v
        dd17v_tab     = rs_data-dd17v
        dd35v_tab     = rs_data-dd35v
        dd36m_tab     = rs_data-dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD serialize.

    DATA ls_dd03p   LIKE LINE OF is_data-dd03p.
    DATA lv_key     TYPE string.
    DATA lv_type    TYPE string.
    DATA lv_pre     TYPE string.
    DATA lv_int     TYPE i.
    DATA lv_suffix  TYPE string.
    DATA lv_notnull TYPE string.
    DATA lv_colon   TYPE i.


    rv_ddl = rv_ddl && serialize_top( is_data ).

    rv_ddl = rv_ddl && |define table { to_lower( is_data-dd02v-tabname ) } \{\n|.

    LOOP AT is_data-dd03p INTO ls_dd03p
        WHERE ( fieldname <> '.INCLUDE' OR groupname IS NOT INITIAL )
        AND adminfield = '0'.
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

* ADMINFIELD: skip fields inside .INCLUDEs
    LOOP AT is_data-dd03p INTO ls_dd03p WHERE adminfield = '0'.
      CLEAR lv_key.
      CLEAR lv_notnull.
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

      IF ls_dd03p-fieldname CP '.INCLU*'.
        IF ls_dd03p-notnull = abap_true.
          lv_notnull = | not null|.
        ENDIF.
        CLEAR lv_suffix.
        IF ls_dd03p-fieldname CA '-'.
          SPLIT ls_dd03p-fieldname AT '-' INTO lv_suffix lv_suffix.
          lv_suffix = | with suffix { to_lower( lv_suffix ) }|.
        ENDIF.
        IF ls_dd03p-groupname IS INITIAL.
          rv_ddl = rv_ddl && |  { lv_key }include { to_lower( ls_dd03p-precfield ) }{ lv_suffix }{ lv_notnull };\n|.
        ELSE.
          rv_ddl = rv_ddl && |  { lv_pre } : include { to_lower( ls_dd03p-precfield ) }{ lv_suffix }{ lv_notnull };\n|.
        ENDIF.
        CONTINUE.
      ENDIF.

      rv_ddl = rv_ddl && serialize_field_annotations(
        iv_fieldname = ls_dd03p-fieldname
        is_data      = is_data ).

      lv_type = serialize_type( ls_dd03p ).
      rv_ddl = rv_ddl && |  { lv_pre } : { lv_type }|.
      rv_ddl = rv_ddl && serialize_field_foreign_key(
        iv_fieldname = ls_dd03p-fieldname
        is_data      = is_data ).
      rv_ddl = rv_ddl && serialize_value_help(
        iv_fieldname = ls_dd03p-fieldname
        is_data      = is_data ).
      rv_ddl = rv_ddl && |;\n|.
    ENDLOOP.
    rv_ddl = rv_ddl && |\n|.

    rv_ddl = rv_ddl && |\}|.

  ENDMETHOD.


  METHOD serialize_adt.

    DATA ls_object_type TYPE wbobjtype.
    DATA lv_object_key  TYPE seu_objkey.
    DATA li_object_data TYPE REF TO if_wb_object_data_model.
    DATA lo_operator    TYPE REF TO object.


    ls_object_type-objtype_tr = 'TABL'.
    ls_object_type-subtype_wb = 'DT'.

    lv_object_key = iv_name.

    CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
      EXPORTING
        object_type = ls_object_type
        object_key  = lv_object_key
      RECEIVING
        result      = lo_operator.

    CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
      EXPORTING
        version        = 'A'
      IMPORTING
        eo_object_data = li_object_data.

    li_object_data->get_content( IMPORTING p_data = rv_ddl ).

  ENDMETHOD.


  METHOD serialize_field_annotations.

    DATA ls_dd08v LIKE LINE OF is_data-dd08v.
    DATA ls_dd03p LIKE LINE OF is_data-dd03p.


    READ TABLE is_data-dd03p INTO ls_dd03p WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.
      IF ( ls_dd03p-rollname IS INITIAL AND ls_dd03p-precfield IS INITIAL
        OR ls_dd03p-comptype = 'R' AND ls_dd03p-reftype = 'B' )
        AND ls_dd03p-ddtext IS NOT INITIAL.
        rv_ddl = rv_ddl && |  @EndUserText.label : { escape_string( ls_dd03p-ddtext ) }\n|.
      ENDIF.

      IF ls_dd03p-languflag = abap_true.
        rv_ddl = rv_ddl && |  @AbapCatalog.textLanguage\n|.
      ENDIF.

      IF ls_dd03p-reftable IS NOT INITIAL AND ls_dd03p-reffield IS NOT INITIAL.
        IF ls_dd03p-datatype = 'CURR'.
          rv_ddl = rv_ddl && |  @Semantics.amount.currencyCode : '{ to_lower( ls_dd03p-reftable ) }.{
            to_lower( ls_dd03p-reffield ) }'\n|.
        ELSEIF ls_dd03p-datatype = 'UNIT'.
          rv_ddl = rv_ddl && |  @Semantics.quantity.unitOfMeasure : '{ to_lower( ls_dd03p-reftable ) }.{
            to_lower( ls_dd03p-reffield ) }'\n|.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE is_data-dd08v INTO ls_dd08v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc = 0.
      IF ls_dd08v-ddtext IS NOT INITIAL.
        rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.label : { escape_string( ls_dd08v-ddtext ) }\n|.
      ENDIF.

      IF ls_dd08v-frkart IS NOT INITIAL.
        CASE ls_dd08v-frkart.
          WHEN 'TEXT'.
            rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #TEXT_KEY\n|.
          WHEN 'REF'.
            rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #NON_KEY\n|.
          WHEN OTHERS.
            rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.keyType : #{ ls_dd08v-frkart }\n|.
        ENDCASE.
      ENDIF.

      IF ls_dd08v-checkflag = abap_false.
        rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.screenCheck : true\n|.
      ELSE.
        rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.screenCheck : false\n|.
      ENDIF.

      IF ls_dd08v-arbgb IS NOT INITIAL.
        rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.messageClass : '{ ls_dd08v-arbgb }'\n|.
      ENDIF.
      IF ls_dd08v-msgnr IS NOT INITIAL.
        rv_ddl = rv_ddl && |  @AbapCatalog.foreignKey.messageNumber : '{ ls_dd08v-msgnr }'\n|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_field_foreign_key.

    DATA ls_dd08v       LIKE LINE OF is_data-dd08v.
    DATA ls_dd05m       LIKE LINE OF is_data-dd05m.
    DATA lv_pre         TYPE string.
    DATA lv_cardinality TYPE string.

    READ TABLE is_data-dd08v INTO ls_dd08v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

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
    ELSEIF ls_dd08v-cardleft IS INITIAL OR ls_dd08v-card IS INITIAL.
      lv_cardinality = | |.
    ELSE.
      ASSERT 1 = 'todo'.
    ENDIF.

    rv_ddl = rv_ddl && |\n    with foreign key{ lv_cardinality }{ to_lower( ls_dd08v-checktable ) }|.

* assumption: dd05m table is sorted by PRIMPOS ascending
    LOOP AT is_data-dd05m INTO ls_dd05m WHERE fieldname = iv_fieldname AND fortable <> '*'.
      IF lv_pre IS INITIAL.
        lv_pre = |\n      where |.
      ELSE.
        lv_pre = |\n        and |.
      ENDIF.
      IF ls_dd05m-fortable(1) = |'|.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd05m-checkfield ) } = {
          ls_dd05m-fortable }|.
      ELSE.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd05m-checkfield ) } = {
          to_lower( ls_dd05m-fortable ) }.{ to_lower( ls_dd05m-forkey ) }|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_top.

    rv_ddl = rv_ddl && |@EndUserText.label : { escape_string( is_data-dd02v-ddtext ) }\n|.

    CASE is_data-dd02v-exclass.
      WHEN '0'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancementCategory : #NOT_CLASSIFIED\n|.
      WHEN '1'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE\n|.
      WHEN '2'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancementCategory : #EXTENSIBLE_CHARACTER\n|.
      WHEN '3'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancementCategory : #EXTENSIBLE_CHARACTER_NUMERIC\n|.
      WHEN '4'.
        rv_ddl = rv_ddl && |@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY\n|.
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

    CASE is_data-dd02v-tabclass.
      WHEN 'TRANSP'.
        rv_ddl = rv_ddl && |@AbapCatalog.tableCategory : #TRANSPARENT\n|.
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

    IF is_data-dd02v-authclass = '02'.
      rv_ddl = rv_ddl && |@AbapCatalog.activationType : #ADAPT_C_STRUCTURES\n|.
    ENDIF.

    rv_ddl = rv_ddl && |@AbapCatalog.deliveryClass : #{ is_data-dd02v-contflag }\n|.

    IF is_data-dd02v-mainflag = abap_true.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #ALLOWED\n|.
    ELSEIF is_data-dd02v-mainflag = 'N'.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #NOT_ALLOWED\n|.
    ELSE.
      rv_ddl = rv_ddl && |@AbapCatalog.dataMaintenance : #LIMITED\n|.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_type.

    DATA lv_notnull TYPE string.
    DATA lv_leng TYPE i.
    DATA lv_decimals TYPE i.

    IF is_dd03p-notnull = abap_true.
      lv_notnull = | not null|.
    ENDIF.

    IF is_dd03p-rollname IS NOT INITIAL.
      rv_type = |{ to_lower( is_dd03p-rollname ) }{ lv_notnull }|.
    ELSE.
      lv_leng = is_dd03p-leng.
      lv_decimals = is_dd03p-decimals.
      CASE is_dd03p-datatype.
        WHEN 'STRG'.
          rv_type = |abap.string({ lv_leng }){ lv_notnull }|.
        WHEN 'RSTR'.
          rv_type = |abap.rawstring({ lv_leng }){ lv_notnull }|.
        WHEN 'INT4'.
          rv_type = |abap.int4{ lv_notnull }|.
        WHEN 'INT2'.
          rv_type = |abap.int2{ lv_notnull }|.
        WHEN 'INT1'.
          rv_type = |abap.int1{ lv_notnull }|.
        WHEN 'DATS'.
          rv_type = |abap.dats{ lv_notnull }|.
        WHEN 'TIMS'.
          rv_type = |abap.tims{ lv_notnull }|.
        WHEN 'DEC'.
          rv_type = |abap.dec({ lv_leng },{ lv_decimals }){ lv_notnull }|.
        WHEN OTHERS.
          rv_type = |abap.{ to_lower( is_dd03p-datatype ) }({ lv_leng }){ lv_notnull }|.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_value_help.

    DATA ls_dd35v       LIKE LINE OF is_data-dd35v.
    DATA ls_dd36m       LIKE LINE OF is_data-dd36m.
    DATA lv_pre         TYPE string.

    READ TABLE is_data-dd35v INTO ls_dd35v WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_ddl = rv_ddl && |\n    with value help { to_lower( ls_dd35v-shlpname ) }|.

    LOOP AT is_data-dd36m INTO ls_dd36m
        WHERE fieldname = iv_fieldname
        AND shlpname = ls_dd35v-shlpname
        AND shtype <> 'G'.
      IF lv_pre IS INITIAL.
        lv_pre = |\n      where |.
      ELSE.
        lv_pre = |\n        and |.
      ENDIF.
      IF ls_dd36m-shtype = 'C'.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd36m-shlpfield ) } = {
          ls_dd36m-shtable }|.
      ELSE.
        rv_ddl = rv_ddl && |{ lv_pre }{ to_lower( ls_dd36m-shlpfield ) } = {
          to_lower( ls_dd36m-shtable ) }.{ to_lower( ls_dd36m-shfield ) }|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD unescape_string.
    rv_string = iv_string.
    REPLACE FIRST OCCURRENCE OF REGEX |^'| IN rv_string WITH ||.
    REPLACE FIRST OCCURRENCE OF REGEX |'$| IN rv_string WITH ||.
    REPLACE ALL OCCURRENCES OF |''| IN rv_string WITH |'|.
  ENDMETHOD.
ENDCLASS.
