CLASS zcl_abapgit_objects_generic DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
        !iv_language   TYPE spras DEFAULT sy-langu
        io_field_rules TYPE REF TO zif_abapgit_field_rules OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize
      IMPORTING
        !iv_package TYPE devclass
        !io_xml     TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS exists
      RETURNING
        VALUE(rv_bool) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS serialize
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_s_objkey,
        num   TYPE n LENGTH 3,
        value TYPE c LENGTH 128,
      END OF ty_s_objkey .
    TYPES:
      ty_t_objkey TYPE SORTED TABLE OF ty_s_objkey WITH UNIQUE KEY num .

    DATA ms_object_header TYPE objh .
    DATA:
      mt_object_table TYPE STANDARD TABLE OF objsl WITH DEFAULT KEY .
    DATA:
      mt_object_method TYPE STANDARD TABLE OF objm WITH DEFAULT KEY .
    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .
    DATA mv_language TYPE spras .

    METHODS after_import .
    METHODS before_export .
    METHODS corr_insert
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_data
      IMPORTING
        !io_xml     TYPE REF TO zif_abapgit_xml_input
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS distribute_name_to_components
      IMPORTING
        !it_key_component TYPE ddfields
      CHANGING
        !ct_objkey        TYPE ty_t_objkey
        !cs_objkey        TYPE ty_s_objkey
        !cv_non_value_pos TYPE numc3 .
    METHODS get_key_fields
      IMPORTING
        !iv_table      TYPE objsl-tobj_name
      RETURNING
        VALUE(rt_keys) TYPE ddfields
      RAISING
        zcx_abapgit_exception .
    METHODS get_primary_table
      RETURNING
        VALUE(rv_table) TYPE objsl-tobj_name
      RAISING
        zcx_abapgit_exception .
    METHODS get_where_clause
      IMPORTING
        !iv_tobj_name   TYPE objsl-tobj_name
      RETURNING
        VALUE(rv_where) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_data
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS split_value_to_keys
      IMPORTING
        !it_key_component TYPE ddfields
      CHANGING
        !ct_objkey        TYPE ty_t_objkey
        !cs_objkey        TYPE ty_s_objkey
        !cv_non_value_pos TYPE numc3 .
    METHODS validate
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    DATA mo_field_rules TYPE REF TO zif_abapgit_field_rules .

    METHODS apply_clear_logic
      IMPORTING
        !iv_table TYPE objsl-tobj_name
      CHANGING
        !ct_data  TYPE STANDARD TABLE .
    METHODS apply_fill_logic
      IMPORTING
        !iv_table   TYPE objsl-tobj_name
        !iv_package TYPE devclass
      CHANGING
        !ct_data    TYPE STANDARD TABLE .
ENDCLASS.



CLASS zcl_abapgit_objects_generic IMPLEMENTATION.


  METHOD after_import.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    ls_cts_object_entry-pgmid    = seok_pgmid_r3tr.
    ls_cts_object_entry-object   = ms_item-obj_type.
    ls_cts_object_entry-obj_name = ms_item-obj_name.
    INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method = 'AFTER_IMP'.
    IF sy-subrc = 0.
* client is actually optional for most AIM, but let's supply it and hope
* that those client-independent-ones just ignore it
      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_tarclient  = sy-mandt
          iv_is_upgrade = abap_false
        TABLES
          tt_e071       = lt_cts_object_entry
          tt_e071k      = lt_cts_key.
    ENDIF.

  ENDMETHOD.


  METHOD apply_clear_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_clear_logic( EXPORTING iv_table = |{ iv_table }|
                                         CHANGING  ct_data  = ct_data ).
    ENDIF.
  ENDMETHOD.


  METHOD apply_fill_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_fill_logic(
        EXPORTING
          iv_table   = |{ iv_table }|
          iv_package = iv_package
        CHANGING
          ct_data    = ct_data ).
    ENDIF.
  ENDMETHOD.


  METHOD before_export.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
          lv_client           TYPE trclient.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method     = 'BEFORE_EXP'.
    IF sy-subrc = 0.
      lv_client = sy-mandt.

      ls_cts_object_entry-pgmid    = seok_pgmid_r3tr.
      ls_cts_object_entry-object   = ms_item-obj_type.
      ls_cts_object_entry-obj_name = ms_item-obj_name.
      INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_client = lv_client
        TABLES
          tt_e071   = lt_cts_object_entry
          tt_e071k  = lt_cts_key.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CONSTANTS lc_logical_transport_object TYPE c LENGTH 1 VALUE 'L'.


    SELECT SINGLE * FROM objh INTO ms_object_header
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Not found in OBJH, or not supported' ).
    ENDIF.

* object tables
    SELECT * FROM objsl INTO CORRESPONDING FIELDS OF TABLE mt_object_table
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      AND tobject = 'TABU'
      ORDER BY PRIMARY KEY.
    IF mt_object_table IS INITIAL.
      zcx_abapgit_exception=>raise( |Obviously corrupted object-type { is_item-obj_type }: No tables defined| ).
    ENDIF.
* only unique tables
    SORT mt_object_table BY tobj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM mt_object_table COMPARING tobj_name.

* object methods
    SELECT * FROM objm INTO TABLE mt_object_method
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      ORDER BY PRIMARY KEY.

    ms_item = is_item.
    mv_language = iv_language.
    mo_field_rules = io_field_rules.

  ENDMETHOD.


  METHOD corr_insert.

* this will also insert into TADIR
    zcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.


  METHOD delete.

    DATA: lv_where   TYPE string,
          lv_primary TYPE objsl-tobj_name.

    FIELD-SYMBOLS <ls_table> LIKE LINE OF mt_object_table.


    lv_primary = get_primary_table( ).

    LOOP AT mt_object_table ASSIGNING <ls_table>.
      lv_where = get_where_clause( <ls_table>-tobj_name ).
      ASSERT NOT lv_where IS INITIAL.

      DELETE FROM (<ls_table>-tobj_name) WHERE (lv_where).

      IF <ls_table>-tobj_name = lv_primary.
        ASSERT sy-dbcnt <= 1. "Just to be on the very safe side
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD deserialize.

    validate( io_xml ).

    delete( iv_package ).

    deserialize_data(
      io_xml     = io_xml
      iv_package = iv_package ).

    after_import( ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD deserialize_data.

    DATA: lr_ref TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_table> LIKE LINE OF mt_object_table.


    LOOP AT mt_object_table ASSIGNING <ls_table>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_table>-tobj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      io_xml->read(
        EXPORTING
          iv_name = <ls_table>-tobj_name
        CHANGING
          cg_data = <lt_data> ).
      apply_fill_logic(
        EXPORTING
          iv_table   = <ls_table>-tobj_name
          iv_package = iv_package
        CHANGING
          ct_data    = <lt_data> ).

      INSERT (<ls_table>-tobj_name) FROM TABLE <lt_data>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error inserting data, { <ls_table>-tobj_name }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD distribute_name_to_components.

    DATA: lt_key_component_uncovered  LIKE it_key_component,
          ls_key_component_uncovered  LIKE LINE OF lt_key_component_uncovered,
          ls_objkey_sub               LIKE cs_objkey,
          lv_objkey_sub_pos           TYPE i,
          lv_remaining_length         TYPE i,
          lv_count_components_covered LIKE ls_objkey_sub-num.

    DATA lv_len LIKE ls_key_component_uncovered-leng.


    lt_key_component_uncovered = it_key_component.
    ls_objkey_sub-num = cs_objkey-num.
    lv_objkey_sub_pos = 0.

*    we want to fill the atribute values which are not covered by explicit key components yet
    lv_count_components_covered = ls_objkey_sub-num - 1.
    DO lv_count_components_covered TIMES.
      DELETE lt_key_component_uncovered INDEX 1.
    ENDDO.

    LOOP AT lt_key_component_uncovered INTO ls_key_component_uncovered.
      CLEAR ls_objkey_sub-value.

*      Some datatype used in the key might exceed the total remaining characters length (e. g. SICF)
      TRY.
          lv_remaining_length = strlen( |{ substring( val = cs_objkey-value
                                                      off = lv_objkey_sub_pos ) }| ).
        CATCH cx_sy_range_out_of_bounds.
          lv_remaining_length = 0.
          RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>
      ENDTRY.
      IF ls_key_component_uncovered-leng <= lv_remaining_length.
        lv_len = ls_key_component_uncovered-leng.
      ELSE.
        lv_len = lv_remaining_length.
      ENDIF.

      ls_objkey_sub-value = |{ substring( val = cs_objkey-value
                                          off = lv_objkey_sub_pos
                                          len = lv_len ) }|.
      ls_objkey_sub-num = cv_non_value_pos.

      INSERT ls_objkey_sub INTO TABLE ct_objkey.

      lv_objkey_sub_pos = lv_objkey_sub_pos + ls_key_component_uncovered-leng.
      cv_non_value_pos = cv_non_value_pos + 1.
      CLEAR ls_objkey_sub.

      IF lv_objkey_sub_pos = strlen( cs_objkey-value ).
        cs_objkey-num = cv_non_value_pos.
        EXIT. "end splitting - all characters captured
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD exists.

    DATA: lv_where_clause TYPE string,
          lv_primary      TYPE objsl-tobj_name,
          lr_table_line   TYPE REF TO data.

    FIELD-SYMBOLS: <lg_table_line> TYPE any.


    lv_primary = get_primary_table( ).

    lv_where_clause = get_where_clause( lv_primary ).

    CREATE DATA lr_table_line TYPE (lv_primary).
    ASSIGN lr_table_line->* TO <lg_table_line>.

    SELECT SINGLE * FROM (lv_primary) INTO <lg_table_line> WHERE (lv_where_clause).
    rv_bool = boolc( sy-dbcnt > 0 ).

  ENDMETHOD.


  METHOD get_key_fields.

    DATA: lv_table TYPE ddobjname.


    lv_table = iv_table.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = lv_table
      TABLES
        dfies_tab = rt_keys
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE rt_keys WHERE keyflag = abap_false.

  ENDMETHOD.


  METHOD get_primary_table.

    DATA: ls_object_table LIKE LINE OF mt_object_table.


    READ TABLE mt_object_table INTO ls_object_table WITH KEY prim_table = abap_true.
    IF sy-subrc <> 0.
*    Fallback. For some objects, no primary table is explicitly flagged
*    The, the one with only one key field shall be chosen
      READ TABLE mt_object_table INTO ls_object_table WITH KEY tobjkey = '/&'. "#EC CI_SUBRC
    ENDIF.
    IF ls_object_table IS INITIAL.
      zcx_abapgit_exception=>raise( |Object { ms_item-obj_type } has got no defined primary table| ).
    ENDIF.

    rv_table = ls_object_table-tobj_name.

  ENDMETHOD.


  METHOD get_where_clause.

    DATA: lv_objkey_pos      TYPE i,
          lv_next_objkey_pos TYPE i,
          lv_value_pos       TYPE i,
          lv_objkey_length   TYPE i,
          lt_objkey          TYPE ty_t_objkey,
          ls_objkey          LIKE LINE OF lt_objkey,
          lv_non_value_pos   TYPE numc3,
          lt_key_fields      TYPE ddfields.

    DATA: lv_is_asterix      TYPE abap_bool,
          lv_where_statement TYPE string,
          lv_key_pos         TYPE i,
          lv_value128        TYPE string.

    FIELD-SYMBOLS <ls_object_table> LIKE LINE OF mt_object_table.

    FIELD-SYMBOLS <ls_table_field> LIKE LINE OF lt_key_fields.


    READ TABLE mt_object_table ASSIGNING <ls_object_table> WITH KEY tobj_name = iv_tobj_name.
    ASSERT sy-subrc = 0.

    lt_key_fields = get_key_fields( iv_tobj_name ).

*   analyze the object key and compose the key (table)
    CLEAR lt_objkey.
    CLEAR ls_objkey.
    lv_objkey_pos = 0.
    lv_non_value_pos = 1.
    lv_value_pos = 0.
    lv_objkey_length = strlen( <ls_object_table>-tobjkey ).

    WHILE lv_objkey_pos <= lv_objkey_length.
      ls_objkey-num = lv_non_value_pos.
*     command
      IF <ls_object_table>-tobjkey+lv_objkey_pos(1) = '/'.
        IF NOT ls_objkey-value IS INITIAL.
*        We reached the end of a key-definition.
*        this key part may address multiple fields.
*        E. g. six characters may address one boolean field and a five-digit version field.
*        Thus, we need to analyze the remaining key components which have not been covered yet.
          split_value_to_keys(
            EXPORTING
              it_key_component = lt_key_fields
            CHANGING
              ct_objkey        = lt_objkey
              cs_objkey        = ls_objkey
              cv_non_value_pos = lv_non_value_pos ).
        ENDIF.
        lv_next_objkey_pos = lv_objkey_pos + 1.
*       '*' means all further key values
        IF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = '*'.
          ls_objkey-value = '*'.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
*       object name
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = '&'.
          "TODO
          ls_objkey-value = ms_item-obj_name.
*    The object name might comprise multiple key components (e. g. WDCC)
*    This string needs to be split
          distribute_name_to_components(
            EXPORTING
              it_key_component = lt_key_fields
            CHANGING
              ct_objkey        = lt_objkey
              cs_objkey        = ls_objkey
              cv_non_value_pos = lv_non_value_pos ).
          CLEAR ls_objkey.
          lv_objkey_pos = lv_objkey_pos + 1.
*       language
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = 'L'.
          ls_objkey-value = mv_language.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
*       Client
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = 'C'.
          ls_objkey-value = sy-mandt.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
        ENDIF.
        lv_value_pos = 0.
*     value
      ELSE.
        ls_objkey-value+lv_value_pos(1) = <ls_object_table>-tobjkey+lv_objkey_pos(1).
        lv_value_pos = lv_value_pos + 1.
      ENDIF.

      lv_objkey_pos = lv_objkey_pos + 1.
    ENDWHILE.

*    Similarly to that, fixed values might be supplied in the object key which actually make up key components
    IF NOT ls_objkey-value IS INITIAL.
      split_value_to_keys(
        EXPORTING
          it_key_component = lt_key_fields
        CHANGING
          ct_objkey        = lt_objkey
          cs_objkey        = ls_objkey
          cv_non_value_pos = lv_non_value_pos ).
    ENDIF.

*   compose the where clause
    lv_is_asterix = abap_false.
    lv_key_pos = 1.

    LOOP AT lt_key_fields ASSIGNING <ls_table_field>.
      READ TABLE lt_objkey INTO ls_objkey
        WITH TABLE KEY num = lv_key_pos.
      IF sy-subrc <> 0 OR <ls_table_field>-fieldname = 'LANGU'.
        CLEAR ls_objkey.
        lv_key_pos = lv_key_pos + 1.
        CONTINUE.
      ENDIF.
      IF ls_objkey-value = '*'.
        lv_is_asterix = abap_true.
      ENDIF.
      IF lv_is_asterix = abap_true.
        CONTINUE.
      ENDIF.
      IF NOT lv_where_statement IS INITIAL.
        CONCATENATE lv_where_statement 'AND' INTO lv_where_statement
          SEPARATED BY space.
      ENDIF.
      lv_value128 = cl_abap_dyn_prg=>quote( ls_objkey-value ).
      CONCATENATE lv_where_statement <ls_table_field>-fieldname '='
        lv_value128 INTO lv_where_statement SEPARATED BY space.
      lv_key_pos = lv_key_pos + 1.
    ENDLOOP.

    rv_where = condense( lv_where_statement ).

  ENDMETHOD.


  METHOD serialize.

    before_export( ).

    serialize_data( io_xml ).

  ENDMETHOD.


  METHOD serialize_data.

    DATA: lr_ref   TYPE REF TO data,
          lv_where TYPE string.

    FIELD-SYMBOLS: <lt_data>         TYPE STANDARD TABLE,
                   <ls_object_table> LIKE LINE OF mt_object_table.


    LOOP AT mt_object_table ASSIGNING <ls_object_table>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_object_table>-tobj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      lv_where = get_where_clause( <ls_object_table>-tobj_name ).

      SELECT * FROM (<ls_object_table>-tobj_name)
        INTO TABLE <lt_data>
        WHERE (lv_where)
        ORDER BY PRIMARY KEY.

      apply_clear_logic( EXPORTING iv_table = <ls_object_table>-tobj_name
                         CHANGING  ct_data  = <lt_data> ).

      io_xml->add(
        iv_name = <ls_object_table>-tobj_name
        ig_data = <lt_data> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD split_value_to_keys.

    DATA: lt_key_component_uncovered LIKE it_key_component,
          ls_dummy                   LIKE LINE OF ct_objkey,
          ls_key_component_uncovered LIKE LINE OF lt_key_component_uncovered,
          ls_objkey_sub              LIKE cs_objkey,
          lv_objkey_sub_pos          TYPE i.


    lt_key_component_uncovered = it_key_component.

*    we want to fill the atribute values which are not covered by explicit key components yet
    LOOP AT ct_objkey INTO ls_dummy.
      DELETE lt_key_component_uncovered INDEX 1.
    ENDLOOP.

    ls_objkey_sub-num = cs_objkey-num.
    lv_objkey_sub_pos = 0.
    LOOP AT lt_key_component_uncovered INTO ls_key_component_uncovered.
      CLEAR ls_objkey_sub-value.
      ls_objkey_sub-value = cs_objkey-value+lv_objkey_sub_pos(ls_key_component_uncovered-leng).
      ls_objkey_sub-num = cv_non_value_pos.

      INSERT ls_objkey_sub INTO TABLE ct_objkey.

      lv_objkey_sub_pos = lv_objkey_sub_pos + ls_key_component_uncovered-leng.
      cv_non_value_pos = cv_non_value_pos + 1.
      CLEAR ls_objkey_sub.

      IF lv_objkey_sub_pos = strlen( cs_objkey-value ).
        cs_objkey-num = cv_non_value_pos.
        EXIT. "end splitting - all characters captured
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate.

    DATA: lv_where   TYPE string,
          lv_primary TYPE objsl-tobj_name,
          lr_ref     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.


    lv_primary = get_primary_table( ).

    CREATE DATA lr_ref TYPE STANDARD TABLE OF (lv_primary).
    ASSIGN lr_ref->* TO <lt_data>.

    io_xml->read(
      EXPORTING
        iv_name = lv_primary
      CHANGING
        cg_data = <lt_data> ).

    IF lines( <lt_data> ) = 0.
      zcx_abapgit_exception=>raise( |Primary table { lv_primary } not found in imported container| ).
    ELSEIF lines( <lt_data> ) <> 1.
      zcx_abapgit_exception=>raise( |Primary table { lv_primary } contains more than one instance!| ).
    ENDIF.

    lv_where = get_where_clause( lv_primary ).

*  validate that max one local instance was affected by the import
    SELECT COUNT(*) FROM (lv_primary) WHERE (lv_where).
    IF sy-dbcnt > 1.
      zcx_abapgit_exception=>raise( |More than one instance exists locally in primary table { lv_primary }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
