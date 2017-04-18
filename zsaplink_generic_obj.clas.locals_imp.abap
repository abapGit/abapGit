CLASS lcl_rso_tlogo_xml_bridge IMPLEMENTATION.

  METHOD activate .

    FIELD-SYMBOLS: <l_s_tr_prot> TYPE rs_s_tr_prot.
    DATA: l_t_tr_prot TYPE rs_t_tr_prot,
          l_t_e071    TYPE STANDARD TABLE OF e071,
          l_s_e071    TYPE e071,
          l_t_e071k   TYPE STANDARD TABLE OF e071k.
    DATA: l_s_msg     TYPE rs_s_msg.

    CHECK NOT p_ts_tlogo_tables IS INITIAL.

* start the after import method
    l_s_e071-pgmid  = rs_c_pgmid_r3tr.
    l_s_e071-object = p_tlogo.
    l_s_e071-obj_name = p_objnm.
    INSERT l_s_e071 INTO TABLE l_t_e071.

    CALL FUNCTION 'RS_AFTER_IMPORT'
      EXPORTING
        i_mode      = rs_c_after_import_mode-activate
      IMPORTING
        e_t_tr_prot = l_t_tr_prot
      TABLES
        tt_e071     = l_t_e071
        tt_e071k    = l_t_e071k.
    LOOP AT l_t_tr_prot ASSIGNING <l_s_tr_prot>.
      l_s_msg-msgid = <l_s_tr_prot>-ag.
      l_s_msg-msgno = <l_s_tr_prot>-msgnr.
      l_s_msg-msgv1 = <l_s_tr_prot>-var1.
      l_s_msg-msgv2 = <l_s_tr_prot>-var2.
      l_s_msg-msgv3 = <l_s_tr_prot>-var3.
      l_s_msg-msgv4 = <l_s_tr_prot>-var4.
      CALL METHOD cl_rso_application_log=>if_rso_application_log~add_message_as_structure
        EXPORTING
          i_s_msg = l_s_msg.
    ENDLOOP.

  ENDMETHOD.                    "

  METHOD before_export .

    FIELD-SYMBOLS: <l_s_objm> TYPE objm.
    DATA: l_t_e071  TYPE STANDARD TABLE OF e071,
          l_s_e071  TYPE e071,
          l_t_e071k TYPE STANDARD TABLE OF e071k,
          l_s_e071k TYPE e071k,
          l_client  TYPE trclient.

    l_client = sy-mandt.

    l_s_e071-pgmid  = rs_c_pgmid_r3tr.
    l_s_e071-object = p_tlogo.
    l_s_e071-obj_name = p_objnm.
    INSERT l_s_e071 INTO TABLE l_t_e071.

    READ TABLE p_ts_objm ASSIGNING <l_s_objm>
      WITH TABLE KEY objectname = p_tlogo objecttype = 'L' method = 'BEFORE_EXP'.
    IF sy-subrc = 0.
      CALL FUNCTION <l_s_objm>-methodname
        EXPORTING
          iv_client = l_client
        TABLES
          tt_e071   = l_t_e071
          tt_e071k  = l_t_e071k.
    ENDIF.

  ENDMETHOD.                    "

  METHOD class_constructor .

* repository
    p_r_repository = cl_rso_repository=>get_repository( ).
* iXML stream factor
    p_r_stream_factory = cl_rso_repository=>get_stream_factory( ).
* iXML library
    p_r_ixml = cl_ixml=>create( ).

  ENDMETHOD.                    "

  METHOD compose_xml .

    TYPES: BEGIN OF lt_s_objkey ,
             num   TYPE numc3,
             value TYPE char128,
           END OF lt_s_objkey,
           lt_ts_objkey TYPE SORTED TABLE OF lt_s_objkey
WITH UNIQUE KEY num.

    FIELD-SYMBOLS: <l_s_objsl> TYPE objsl.
    FIELD-SYMBOLS: <l_t_data> TYPE STANDARD TABLE,
                   <l_s_data> TYPE any.
    FIELD-SYMBOLS: <l_s_dfies> TYPE dfies.
    FIELD-SYMBOLS: <l_fs>      TYPE any.
    DATA: l_t_dfies TYPE STANDARD TABLE OF dfies,
          l_tabname TYPE ddobjname.
    DATA: l_s_data TYPE REF TO data,
          l_t_data TYPE REF TO data.
    DATA: l_where_stmt TYPE string.
    DATA: l_n   TYPE i,
          l_m   TYPE i,
          l_k   TYPE i,
          l_len TYPE i.
    DATA: l_s_objkey  TYPE lt_s_objkey,
          l_ts_objkey TYPE lt_ts_objkey,
          l_num       TYPE numc3.
    DATA: l_value128 TYPE char128.
    DATA: l_r_document           TYPE REF TO if_ixml_document.
    DATA: l_r_element_transport TYPE REF TO if_ixml_element,
          l_r_element_table     TYPE REF TO if_ixml_element,
          l_r_element_row       TYPE REF TO if_ixml_element,
          l_r_element_key       TYPE REF TO if_ixml_element,
          l_r_element           TYPE REF TO if_ixml_element.
    DATA: l_value TYPE string,
          l_name  TYPE string.
    DATA: l_rc                   TYPE i.
    DATA: l_r_ostream            TYPE REF TO if_ixml_ostream.
    DATA: l_r_renderer           TYPE REF TO if_ixml_renderer.
    DATA: l_r_encoding           TYPE REF TO if_ixml_encoding.
    DATA: l_r_cdata              TYPE REF TO if_ixml_cdata_section.
    DATA: l_flag_asterics        TYPE rs_bool.
    DATA: l_timestmp             TYPE rstimestmp.

    p_objnm = i_objnm.

* call the before export method
    CALL METHOD before_export.

* create the output stream for the output string
    CLEAR e_string_xml.
    l_r_ostream = p_r_stream_factory->create_ostream_cstring( string = e_string_xml ).
* create the XML document
    l_r_document = p_r_ixml->create_document( ).
* set  UTF-8 encoding
    l_r_encoding = cl_rso_repository=>get_xml_encoding(
      i_byte_order = 0 i_character_set = 'latin1' ).
    CALL METHOD l_r_document->set_encoding(
        encoding = l_r_encoding ).

* XML: <TRANSPORT>
    l_name = p_c_xml_tag_transport.
    CLEAR l_value.
    l_r_element_transport = l_r_document->create_element( name = l_name ).
    l_rc = l_r_document->append_child( l_r_element_transport ).
* XML: date (for comparison with object, last changed ) - modified oj: Do not always include this element
    IF mv_include_last_changed = abap_true.
      l_name = if_rso_object_xmic=>timestmp.
      GET TIME STAMP FIELD l_timestmp.
      l_value = cl_rso_repository=>timestamp_to_xml( l_timestmp ).
      l_r_element = l_r_document->create_simple_element(
        name = l_name value = l_value parent = l_r_element_transport ).
    ENDIF.

* parse all tables of this TLOGO object
    LOOP AT p_ts_objsl ASSIGNING <l_s_objsl>.
      CLEAR l_where_stmt.

*   XML: <TABLE>
      l_name = p_c_xml_tag_table.
      CLEAR l_value.
      l_r_element_table = l_r_document->create_simple_element(
          name =  l_name
          value = l_value
          parent = l_r_element_transport ).
*   add attribute:  name (ID)
      l_name  = if_rso_repository_xml_const=>id.
      l_value = <l_s_objsl>-tobj_name.
      l_rc = l_r_element_table->set_attribute(
         name = l_name value = l_value ).

*   get the DDIC info of the table structure
      l_tabname = <l_s_objsl>-tobj_name.
      CALL FUNCTION 'DDIF_NAMETAB_GET'
        EXPORTING
          tabname   = l_tabname
        TABLES
          dfies_tab = l_t_dfies
        EXCEPTIONS
          not_found = 1.
      IF sy-subrc <> 0.
*     message
        EXIT.
      ENDIF.

*   compose the key (table)
      CLEAR l_ts_objkey.
      CLEAR l_s_objkey.
      l_n = 0.
      l_num = 1.
      l_k = 0.
      l_len = strlen( <l_s_objsl>-tobjkey ).
      WHILE l_n <= l_len.
        l_s_objkey-num = l_num.
*     command
        IF <l_s_objsl>-tobjkey+l_n(1) = '/'.
          IF NOT l_s_objkey-value IS INITIAL.
            INSERT l_s_objkey INTO TABLE l_ts_objkey.
            ADD 1 TO l_num.
            CLEAR l_s_objkey.
            l_s_objkey-num = l_num.
          ENDIF.
          l_m = l_n + 1.
*       '*' means all futher key values
          IF <l_s_objsl>-tobjkey+l_m(1) = '*'.
            l_s_objkey-value = '*'.
            INSERT l_s_objkey INTO TABLE l_ts_objkey.
            CLEAR l_s_objkey.
            ADD 1 TO l_num.
            ADD 1 TO l_n.
*       object name
          ELSEIF <l_s_objsl>-tobjkey+l_m(1) = '&'.
            l_s_objkey-value = i_objnm.
* #CP-SUPPRESS: FP no risc
            INSERT l_s_objkey INTO TABLE l_ts_objkey.
            CLEAR l_s_objkey.
            ADD 1 TO l_num.
            ADD 1 TO l_n.
*       language
          ELSEIF <l_s_objsl>-tobjkey+l_m(1) = 'L'.
            l_s_objkey-value = sy-langu.
            INSERT l_s_objkey INTO TABLE l_ts_objkey.
            CLEAR l_s_objkey.
            ADD 1 TO l_num.
            ADD 1 TO l_n.
          ENDIF.
          l_k = 0.
*     value
        ELSE.
          l_s_objkey-value+l_k(1) = <l_s_objsl>-tobjkey+l_n(1).
          ADD 1 TO l_k.
        ENDIF.

        ADD 1 TO l_n.
      ENDWHILE.
      IF NOT l_s_objkey-value IS INITIAL.
        INSERT l_s_objkey INTO TABLE l_ts_objkey.
      ENDIF.

*   compose the where clause
      l_flag_asterics = rs_c_false.
*   XML: <KEY>
      l_name = p_c_xml_tag_key.
      l_r_element_key = l_r_document->create_simple_element(
          name =  l_name
          parent = l_r_element_table ).
      l_num = 1.
      CLEAR l_where_stmt.
*    CONCATENATE '''' i_objvers '''' INTO l_value128.
*    CONCATENATE 'OBJVERS' '=' l_value128 INTO l_where_stmt
*     SEPARATED BY space.
      LOOP AT l_t_dfies ASSIGNING <l_s_dfies>
        WHERE NOT keyflag IS INITIAL.
* #CP-SUPPRESS: FP no risc
        READ TABLE l_ts_objkey INTO l_s_objkey
          WITH TABLE KEY num = l_num.
        IF sy-subrc <> 0
        OR <l_s_dfies>-fieldname = 'LANGU'.
          CLEAR l_s_objkey.
          ADD 1 TO l_num.
          CONTINUE.
        ENDIF.
        IF l_s_objkey-value = '*'.
          l_flag_asterics = rs_c_true.
        ENDIF.
        IF l_flag_asterics = rs_c_true.
          CONTINUE.
        ENDIF.
        IF NOT l_where_stmt IS INITIAL.
* #CP-SUPPRESS: FP no risc
          CONCATENATE l_where_stmt 'AND' INTO l_where_stmt
            SEPARATED BY space.
        ENDIF.
* #CP-SUPPRESS: FP no risc
        CONCATENATE '''' l_s_objkey-value '''' INTO l_value128.
* #CP-SUPPRESS: FP no risc
        CONCATENATE l_where_stmt <l_s_dfies>-fieldname '='
          l_value128 INTO l_where_stmt SEPARATED BY space.
        ADD 1 TO l_num.
*     XML: <FIELD>
        l_name = <l_s_dfies>-fieldname.
        l_r_element = l_r_document->create_simple_element(
            name =  l_name parent = l_r_element_key ).
        l_value = l_s_objkey-value.
        l_r_cdata = l_r_document->create_cdata_section( l_value ).
        l_rc = l_r_element->append_child( l_r_cdata ).

      ENDLOOP.

*   select from database table using the key
      CREATE DATA l_s_data TYPE (<l_s_objsl>-tobj_name).
      ASSIGN l_s_data->* TO <l_s_data>.
      CREATE DATA l_t_data TYPE STANDARD TABLE OF (<l_s_objsl>-tobj_name).
      ASSIGN l_t_data->* TO <l_t_data>.

* #CP-SUPPRESS: FP no risc
      SELECT * FROM (<l_s_objsl>-tobj_name) INTO TABLE <l_t_data>
        WHERE (l_where_stmt).

*   write all selected rows
      LOOP AT <l_t_data> ASSIGNING <l_s_data>.
*     XML: <ROW>
        l_name = p_c_xml_tag_row.
        CLEAR l_value.
        l_r_element_row = l_r_document->create_simple_element(
            name =  l_name
            value = l_value
            parent = l_r_element_table ).
*     write field/value pairs to to XML stream
        LOOP AT l_t_dfies ASSIGNING <l_s_dfies>.
          ASSIGN COMPONENT <l_s_dfies>-fieldname OF STRUCTURE <l_s_data> TO <l_fs>.
*       XML: <FIELD>
          l_name = p_c_xml_tag_field.
          l_r_element = l_r_document->create_simple_element(
              name =  l_name
              parent = l_r_element_row ).
          IF NOT <l_fs> IS INITIAL.
            l_value = <l_fs>.
            l_r_element = l_r_document->create_simple_element(
                name =  l_name
                parent = l_r_element_row ).
            l_r_cdata = l_r_document->create_cdata_section( l_value ).
            l_rc = l_r_element->append_child( l_r_cdata ).
          ENDIF.
*       add attribute: fieldname name (ID)
          l_name  = if_rso_repository_xml_const=>id.
          l_value = <l_s_dfies>-fieldname.
          l_rc = l_r_element->set_attribute(
             name = l_name value = l_value ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

* render the DOM tree into an XML document
    l_r_renderer = p_r_ixml->create_renderer(
       ostream  = l_r_ostream document = l_r_document ).
    l_rc = l_r_renderer->render( ).

    REPLACE 'encoding="sap*"' IN e_string_xml WITH 'encoding="latin1"' . "#EC NOTEXT

  ENDMETHOD.                    "


  METHOD constructor .

* read properties of the TLOGO type
    p_tlogo = i_tlogo.
    CALL METHOD read_tlogo_prop.

    mv_include_last_changed = iv_include_last_changed.
  ENDMETHOD.                    "


  METHOD get_timestmp_of_data .

    r_timestmp = p_timestmp.

  ENDMETHOD.                    "


  METHOD parse_xml .

    FIELD-SYMBOLS: <l_t_data> TYPE STANDARD TABLE,
                   <l_s_data> TYPE any.
    FIELD-SYMBOLS: <l_s_dfies> TYPE dfies.
    FIELD-SYMBOLS: <l_fs>      TYPE any.

    DATA: l_s_tlogo_tables      TYPE pt_s_tlogo_tables.
    DATA: l_r_child_table TYPE REF TO if_ixml_node,
          l_r_child_row   TYPE REF TO if_ixml_node,
          l_r_child_field TYPE REF TO if_ixml_node,
          l_r_child_key   TYPE REF TO if_ixml_node.
    DATA: l_r_element_transport TYPE REF TO if_ixml_element,
          l_r_element_table     TYPE REF TO if_ixml_element,
          l_r_element_row       TYPE REF TO if_ixml_element,
          l_r_element_field     TYPE REF TO if_ixml_element,
          l_r_element_key       TYPE REF TO if_ixml_element.
    DATA: l_tagname             TYPE string.
    DATA: l_dummy               TYPE c.
    DATA: l_t_dfies  TYPE STANDARD TABLE OF dfies,
          l_th_dfies TYPE HASHED TABLE OF dfies WITH UNIQUE KEY fieldname.
    DATA: l_s_data              TYPE REF TO data.
    DATA: l_name      TYPE string,
          l_value     TYPE string,
          l_fieldname TYPE string.
    DATA: l_r_ixml              TYPE REF TO if_ixml.
    DATA: l_r_document          TYPE REF TO if_ixml_document.
    DATA: l_r_element           TYPE REF TO if_ixml_element.
    DATA: l_r_renderer          TYPE REF TO if_ixml_renderer.
    DATA: l_r_encoding          TYPE REF TO if_ixml_encoding.
    DATA: l_rc                  TYPE i.
    DATA: l_r_istream           TYPE REF TO if_ixml_istream.
    DATA: l_returncode          TYPE sysubrc.
    DATA: l_r_parser            TYPE REF TO if_ixml_parser.
    DATA: l_r_cdata             TYPE REF TO if_ixml_cdata_section.
    DATA: l_detlevel            TYPE bal_s_msg-detlevel.
    DATA: l_detlevel2           TYPE bal_s_msg-detlevel.

    CLEAR p_ts_tlogo_tables.

    e_subrc = 0.

    p_objnm = i_objnm.
    l_detlevel  = i_detlevel + 1.
    l_detlevel2 = l_detlevel + 1.

* create a XML document and parse it
* iXML library
    l_r_ixml = cl_ixml=>create( ).
* create an input stream
    l_r_istream = p_r_stream_factory->create_istream_cstring( i_string_xml ).

* create the XML document
    l_r_document = l_r_ixml->create_document( ).
* create a parser (used to be once only !!!)
    l_r_parser = l_r_ixml->create_parser( stream_factory = p_r_stream_factory
                                  istream        = l_r_istream
                                  document       = l_r_document ).

* parse the document
    IF l_r_parser->parse( ) NE 0.
      IF l_r_parser->num_errors( ) NE 0.
        DATA: parseerror TYPE REF TO if_ixml_parse_error,
              str        TYPE string,
              line       TYPE i,
              column     TYPE i,
              count      TYPE i,
              index      TYPE i.
        count = l_r_parser->num_errors( ).
        IF count > 0.
          MESSAGE e207(rsoxml) WITH p_objnm p_tlogo count INTO l_dummy.
          CALL METHOD cl_rso_application_log=>if_rso_application_log~add_message_level
            EXPORTING
              i_probclass = if_rso_application_log=>probclass_high
              i_detlevel  = l_detlevel.
          e_subrc = 12.
          index = 0.
          WHILE index < count.
            parseerror = l_r_parser->get_error( index = index ).
            line = parseerror->get_line( ).
            column = parseerror->get_column( ).
            str = parseerror->get_reason( ).
            MESSAGE e202(rsoxml) WITH line column str INTO l_dummy.
            CALL METHOD cl_rso_application_log=>if_rso_application_log~add_message_level
              EXPORTING
                i_probclass = if_rso_application_log=>probclass_high
                i_detlevel  = l_detlevel2.
            ADD 1 TO index.
          ENDWHILE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

* parsed OK
    CHECK l_returncode = 0.

* get the root element
    l_r_element_transport = l_r_document->get_root_element( ).

* the TRANSPORT tag was imported
* get the parameters and references
    l_r_child_table = l_r_element_transport->get_first_child( ).
    WHILE NOT l_r_child_table IS INITIAL.
      l_tagname = l_r_child_table->get_name( ).
      l_r_element_table ?= l_r_child_table.
*   TIMESTAMP
      IF l_tagname = if_rso_object_xmic=>timestmp.
        p_timestmp = l_r_element_table->get_value( ).
*   TABLE tag
      ELSEIF l_tagname = p_c_xml_tag_table.
        CLEAR l_s_tlogo_tables.
*     get the DDIC info of the table structure
        l_name = if_rso_repository_xml_const=>id.
        l_s_tlogo_tables-tabname = l_r_element_table->get_attribute( name = l_name ).
        CALL FUNCTION 'DDIF_NAMETAB_GET'
          EXPORTING
            tabname   = l_s_tlogo_tables-tabname
          TABLES
            dfies_tab = l_t_dfies
          EXCEPTIONS
            not_found = 1.
        IF sy-subrc <> 0.
*      error message
        ELSE.
          l_th_dfies = l_t_dfies.
*       dynamic creation of tables
          CREATE DATA l_s_data TYPE (l_s_tlogo_tables-tabname).
          ASSIGN l_s_data->* TO <l_s_data>.
          CREATE DATA l_s_tlogo_tables-data TYPE STANDARD TABLE OF (l_s_tlogo_tables-tabname).
          ASSIGN l_s_tlogo_tables-data->* TO <l_t_data>.

          CLEAR l_s_tlogo_tables-where_clause.
          l_r_child_row = l_r_element_table->get_first_child( ).
          WHILE NOT l_r_child_row IS INITIAL.
            l_tagname = l_r_child_row->get_name( ).
            l_r_element_row ?= l_r_child_row.
*         KEY tag
            IF l_tagname = p_c_xml_tag_key.
              l_r_child_key = l_r_element_row->get_first_child( ).
              WHILE NOT l_r_child_key IS INITIAL.
                l_tagname = l_r_child_key->get_name( ).
                l_r_element_key ?= l_r_child_key.
*             fields and values
                l_fieldname = l_tagname.
                l_value = l_r_element_key->get_value( ).
*             modify OBJVERS as R3TRANS does
                IF l_fieldname = 'OBJVERS'.
                  IF l_value = rs_c_objvers-active.
                    l_value = rs_c_objvers-modified.
                  ENDIF.
                ENDIF.
                IF NOT l_s_tlogo_tables-where_clause IS INITIAL.
                  CONCATENATE l_s_tlogo_tables-where_clause 'AND'
                    INTO l_s_tlogo_tables-where_clause
                    SEPARATED BY space.
                ENDIF.
                CONCATENATE '''' l_value '''' INTO l_value.
                CONCATENATE l_s_tlogo_tables-where_clause
                  l_fieldname '=' l_value
                  INTO l_s_tlogo_tables-where_clause
                  SEPARATED BY space.
                l_r_child_key = l_r_child_key->get_next( ).
              ENDWHILE.
*         ROW tag
            ELSEIF l_tagname = p_c_xml_tag_row.
              CLEAR <l_s_data>.
              l_r_child_field = l_r_element_row->get_first_child( ).
              WHILE NOT l_r_child_field IS INITIAL.
                l_tagname = l_r_child_field->get_name( ).
                l_r_element_field ?= l_r_child_field.
*             FIELD tag
                IF l_tagname = p_c_xml_tag_field.
                  l_name = if_rso_repository_xml_const=>id.
                  l_fieldname = l_r_element_field->get_attribute( name = l_name )..
                  READ TABLE l_th_dfies TRANSPORTING NO FIELDS
                    WITH TABLE KEY fieldname = l_fieldname.
                  IF sy-subrc = 0.
                    ASSIGN COMPONENT l_fieldname OF STRUCTURE <l_s_data> TO <l_fs>.
                    <l_fs> = l_r_element_field->get_value( ).
*                 modify OBJVERS as R3TRANS does
                    IF l_fieldname = 'OBJVERS'.
                      IF <l_fs> = rs_c_objvers-active.
                        <l_fs> = rs_c_objvers-modified.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
                l_r_child_field = l_r_child_field->get_next( ).
              ENDWHILE.
              INSERT <l_s_data> INTO TABLE <l_t_data>.
            ENDIF.
            l_r_child_row = l_r_child_row->get_next( ).
          ENDWHILE.
          INSERT l_s_tlogo_tables INTO TABLE p_ts_tlogo_tables.
        ENDIF.
      ENDIF.
      l_r_child_table = l_r_child_table->get_next( ).
    ENDWHILE.

  ENDMETHOD.                    "


  METHOD read_tlogo_prop .

* get the BW TLOGO properties
    CALL METHOD cl_rso_repository=>if_rso_tlogo_properties~get_tlogoprop
      EXPORTING
        i_tlogo       = p_tlogo
      IMPORTING
        e_s_tlogoprop = p_s_tlogoprop.

* get the TLOGO properties as stored in transaction SOBJ
* object header (table OBJH)
    SELECT SINGLE * FROM objh INTO p_s_objh
      WHERE objectname = p_tlogo
      AND   objecttype = 'L'.
* object tables
    SELECT * FROM objsl INTO TABLE p_ts_objsl
      WHERE objectname = p_tlogo
      AND   objecttype = 'L'.
* object methods
    SELECT * FROM objm INTO TABLE p_ts_objm
      WHERE objectname = p_tlogo
      AND   objecttype = 'L'.

  ENDMETHOD.                    "


  METHOD save .

    FIELD-SYMBOLS: <l_s_tlogo_tables> TYPE pt_s_tlogo_tables.
    FIELD-SYMBOLS: <l_t_data>  TYPE STANDARD TABLE.
    DATA: l_dummy TYPE c.

    CHECK NOT p_ts_tlogo_tables IS INITIAL.

    e_subrc = 0.

* insert into TLOGO tables
    LOOP AT p_ts_tlogo_tables ASSIGNING <l_s_tlogo_tables>.
*   delete old records first
      IF <l_s_tlogo_tables>-where_clause IS INITIAL.
        MESSAGE e220(rsoxml) WITH p_objnm p_tlogo INTO l_dummy.
        CALL METHOD cl_rso_application_log=>if_rso_application_log~add_message_level
          EXPORTING
            i_probclass = if_rso_application_log=>probclass_high
            i_detlevel  = i_detlevel.
        e_subrc = 8.
      ELSE.
        do_delete( iv_tlogo_table = <l_s_tlogo_tables>-tabname
                   iv_where_clause = <l_s_tlogo_tables>-where_clause ).
      ENDIF.
*   actual insert
      ASSIGN <l_s_tlogo_tables>-data->* TO <l_t_data>.
      do_insert(    iv_tlogo_table  = <l_s_tlogo_tables>-tabname
                    it_data         = <l_t_data> ).
      IF sy-subrc <> 0.
        MESSAGE e221(rsoxml) WITH p_objnm p_tlogo <l_s_tlogo_tables>-tabname INTO l_dummy.
        CALL METHOD cl_rso_application_log=>if_rso_application_log~add_message_level
          EXPORTING
            i_probclass = if_rso_application_log=>probclass_high
            i_detlevel  = i_detlevel.
        e_subrc = sy-subrc.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "

  METHOD do_delete.

* #CP-SUPPRESS: FP secure coding, no user input
    DELETE FROM (iv_tlogo_table) WHERE (iv_where_clause).

  ENDMETHOD.


  METHOD do_insert.

    INSERT (iv_tlogo_table) FROM TABLE it_data.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_tlogo_xml_bridge IMPLEMENTATION.

  METHOD exists.
*    check whether an object with this name exists in the primary table
    DATA(ls_objsl) = get_prim_table_objsl( ).
    DATA(lv_where_clause) = me->get_where_clause(
                            iv_tobj_name     = ls_objsl-tobj_name
                            iv_objname       = iv_objname
                        ).

    SELECT COUNT(*) FROM (ls_objsl-tobj_name) WHERE (lv_where_clause).

    rv_object_exists = boolc( sy-dbcnt > 0 ).

    validate_count_prim_table( iv_dbcount = sy-dbcnt  iv_objname = iv_objname ).

  ENDMETHOD.

  METHOD validate_count_prim_table.
*      The method shall validate that by accident (bad programming of this class) unwanted
*      entries from the object's tables are being modified or deleted. The general assumption
*      is that from th key-table, only one entry is affected.
*      however, this is not true for some exceptions, where the primary table features a
*      compound key (e. g. checkpoint group activation variants, AVAR)
    DATA(ls_objsl) = get_prim_table_objsl( ).
    DATA(lv_where_clause) = me->get_where_clause(
                            iv_tobj_name     = ls_objsl-tobj_name
                            iv_objname       = iv_objname
                        ).
*    count the key components
    DATA lv_count_key_components TYPE i.
    DATA lt_dfies TYPE dfies_table.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = CONV ddobjname( ls_objsl-tobj_name )
      TABLES
        dfies_tab = lt_dfies
      EXCEPTIONS
        not_found = 1.
    ASSERT sy-subrc = 0.

    CLEAR lv_count_key_components.
    LOOP AT lt_dfies TRANSPORTING NO FIELDS WHERE keyflag = abap_true.
      ADD 1 TO lv_count_key_components.
    ENDLOOP.

*    if all key-components are covered by the where clause, obviously only one record must be matched by the where-clause
    SPLIT lv_where_clause AT | AND | INTO TABLE DATA(lt_where_components).

    IF lines( lt_where_components ) = lv_count_key_components.
      ASSERT iv_dbcount <= 1. "Sanity-check: there must not be more than one matching entry in the primary table
    ENDIF.
  ENDMETHOD.

  METHOD delete_object.

*  first, check existence of the object
    IF me->exists( iv_objname = iv_objname ) = abap_true.
      LOOP AT p_ts_objsl ASSIGNING FIELD-SYMBOL(<ls_objsl>).
        DATA(lv_where_clause) = me->get_where_clause(
                                iv_tobj_name     = <ls_objsl>-tobj_name
                                iv_objname       = iv_objname
                            ).

*        Some sanity checks
        ASSERT lv_where_clause IS NOT INITIAL.

        do_delete(  iv_tlogo_table  = conv #( <ls_objsl>-tobj_name )
                    iv_where_clause = lv_where_clause ).
        IF <ls_objsl>-prim_table = abap_true.
          DATA(lv_dbcnt) = sy-dbcnt.
          validate_count_prim_table( iv_dbcount = lv_dbcnt  iv_objname = iv_objname ).
          IF lv_dbcnt > 0.
            rv_deleted = abap_true.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_where_clause.

*    Sadly, the superclass does not provide a reuse-method which generates where-clauses,
*    thus the code is copied and refactored (a bit)

    TYPES: BEGIN OF lt_s_objkey ,
             num   TYPE numc3,
             value TYPE char128,
           END OF lt_s_objkey,
           lt_ts_objkey TYPE SORTED TABLE OF lt_s_objkey WITH UNIQUE KEY num.

    READ TABLE p_ts_objsl ASSIGNING FIELD-SYMBOL(<l_s_objsl>) WITH KEY tobj_name = iv_tobj_name.
    ASSERT sy-subrc = 0.

    DATA: l_t_dfies   TYPE dfies_table,
          l_n         TYPE i,
          l_m         TYPE i,
          l_k         TYPE i,
          l_len       TYPE i,
          l_s_objkey  TYPE lt_s_objkey,
          l_ts_objkey TYPE lt_ts_objkey,
          l_num       TYPE numc3.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = CONV ddobjname( <l_s_objsl>-tobj_name )
      TABLES
        dfies_tab = l_t_dfies
      EXCEPTIONS
        not_found = 1.
    ASSERT sy-subrc = 0.

*   compose the key (table)
    CLEAR l_ts_objkey.
    CLEAR l_s_objkey.
    l_n = 0.
    l_num = 1.
    l_k = 0.
    l_len = strlen( <l_s_objsl>-tobjkey ).
    WHILE l_n <= l_len.
      l_s_objkey-num = l_num.
*     command
      IF <l_s_objsl>-tobjkey+l_n(1) = '/'.
        IF NOT l_s_objkey-value IS INITIAL.
          INSERT l_s_objkey INTO TABLE l_ts_objkey.
          ADD 1 TO l_num.
          CLEAR l_s_objkey.
          l_s_objkey-num = l_num.
        ENDIF.
        l_m = l_n + 1.
*       '*' means all futher key values
        IF <l_s_objsl>-tobjkey+l_m(1) = '*'.
          l_s_objkey-value = '*'.
          INSERT l_s_objkey INTO TABLE l_ts_objkey.
          CLEAR l_s_objkey.
          ADD 1 TO l_num.
          ADD 1 TO l_n.
*       object name
        ELSEIF <l_s_objsl>-tobjkey+l_m(1) = '&'.
          l_s_objkey-value = iv_objname.
* #CP-SUPPRESS: FP no risc
          INSERT l_s_objkey INTO TABLE l_ts_objkey.
          CLEAR l_s_objkey.
          ADD 1 TO l_num.
          ADD 1 TO l_n.
*       language
        ELSEIF <l_s_objsl>-tobjkey+l_m(1) = 'L'.
          l_s_objkey-value = sy-langu.
          INSERT l_s_objkey INTO TABLE l_ts_objkey.
          CLEAR l_s_objkey.
          ADD 1 TO l_num.
          ADD 1 TO l_n.
        ENDIF.
        l_k = 0.
*     value
      ELSE.
        l_s_objkey-value+l_k(1) = <l_s_objsl>-tobjkey+l_n(1).
        ADD 1 TO l_k.
      ENDIF.

      ADD 1 TO l_n.
    ENDWHILE.
    IF NOT l_s_objkey-value IS INITIAL.
      INSERT l_s_objkey INTO TABLE l_ts_objkey.
    ENDIF.

*   compose the where clause
    DATA(l_flag_asterics) = rs_c_false.
    l_num = 1.
    DATA(l_where_stmt) = ||.

    LOOP AT l_t_dfies ASSIGNING FIELD-SYMBOL(<l_s_dfies>)
      WHERE NOT keyflag IS INITIAL.
* #CP-SUPPRESS: FP no risc
      READ TABLE l_ts_objkey INTO l_s_objkey
        WITH TABLE KEY num = l_num.
      IF sy-subrc <> 0
      OR <l_s_dfies>-fieldname = 'LANGU'.
        CLEAR l_s_objkey.
        ADD 1 TO l_num.
        CONTINUE.
      ENDIF.
      IF l_s_objkey-value = '*'.
        l_flag_asterics = rs_c_true.
      ENDIF.
      IF l_flag_asterics = rs_c_true.
        CONTINUE.
      ENDIF.
      IF NOT l_where_stmt IS INITIAL.
* #CP-SUPPRESS: FP no risc
        CONCATENATE l_where_stmt 'AND' INTO l_where_stmt
          SEPARATED BY space.
      ENDIF.
* #CP-SUPPRESS: FP no risc
      CONCATENATE '''' l_s_objkey-value '''' INTO DATA(l_value128).
* #CP-SUPPRESS: FP no risc
      CONCATENATE l_where_stmt <l_s_dfies>-fieldname '='
        l_value128 INTO l_where_stmt SEPARATED BY space.
      ADD 1 TO l_num.
    ENDLOOP.
    rv_where_on_keys = l_where_stmt.
  ENDMETHOD.

  METHOD timestamp_to_xml.
    "copy of cl_rso_tlogo=>timestamp_to_xml
* this would be more readable, but must be converted before stored on DB
* additional problem: different time formats (language dependent)
*    CONVERT TIME STAMP l_conttimestmp TIME ZONE sy-zonlo
*      INTO DATE l_date TIME l_time.
*    WRITE: l_date TO l_date_c.
*    WRITE: l_time TO l_time_c.
*    CONCATENATE l_date_c l_time_c INTO l_value SEPARATED BY ', '.

* simple, but works
    rv_string = iv_timestamp.

  ENDMETHOD.


  METHOD get_prim_table_objsl.
    READ TABLE p_ts_objsl INTO rs_objsl WITH KEY prim_table = abap_true.
    IF sy-subrc <> 0.
*    Fallback. For some objects, no primary table is explicitly flagged
*    The, the one with only one key field shall be chosen
      READ TABLE p_ts_objsl INTO rs_objsl WITH KEY tobjkey = '/&'.
    ENDIF.
    ASSERT rs_objsl IS NOT INITIAL.
  ENDMETHOD.


  METHOD get_table_metadata.

* parse all tables of this TLOGO object
    LOOP AT p_ts_objsl ASSIGNING FIELD-SYMBOL(<ls_objsl>).
      DATA ls_metadata LIKE LINE OF rt_metadata.
      ls_metadata-db_table = <ls_objsl>-tobj_name.

      INSERT ls_metadata INTO TABLE rt_metadata ASSIGNING FIELD-SYMBOL(<ls_table_metdata>).

      DATA(lv_where_clause) = me->get_where_clause(
                          iv_tobj_name     = <ls_objsl>-tobj_name
                          iv_objname       = iv_objname
                      ).

      SELECT COUNT(*) INTO <ls_table_metdata>-count FROM (<ls_objsl>-tobj_name) WHERE (lv_where_clause).

*   get the DDIC info of the table structure
      CALL FUNCTION 'DDIF_NAMETAB_GET'
        EXPORTING
          tabname   = CONV ddobjname( <ls_objsl>-tobj_name )
        TABLES
          dfies_tab = <ls_table_metdata>-fields_definition
        EXCEPTIONS
          not_found = 1.
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.