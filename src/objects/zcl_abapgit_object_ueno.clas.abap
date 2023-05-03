CLASS zcl_abapgit_object_ueno DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras.
  PROTECTED SECTION.



  PRIVATE SECTION.

    TYPES BEGIN OF ty_docu.
    TYPES language TYPE dm40t-sprache.
    TYPES header   TYPE thead.
    TYPES content TYPE xstring.
    TYPES itf     TYPE tsftext.
    TYPES END OF ty_docu.

    TYPES ty_docu_lines TYPE STANDARD TABLE OF ty_docu WITH DEFAULT KEY.

    DATA mv_entity_id TYPE udentity.

    CONSTANTS c_text_object_type TYPE lxeobjtype VALUE 'IM' ##NO_TEXT.
    CONSTANTS c_active_state TYPE as4local VALUE 'A' ##NO_TEXT.


    METHODS build_text_name
      IMPORTING VALUE(iv_id)     TYPE tdid
      RETURNING VALUE(rv_result) TYPE doku_obj.

    METHODS is_name_permitted
      RAISING
        zcx_abapgit_exception.

    METHODS delete_docu_uen
      RAISING zcx_abapgit_exception.

    METHODS delete_docu_url
      RAISING zcx_abapgit_exception.

    METHODS delete_docu_usp
      RAISING zcx_abapgit_exception.



    METHODS deserialize_docu_uen
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_url
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_usp
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.



    METHODS serialize_docu_uen
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_url
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_xxxx
      IMPORTING VALUE(iv_id)     TYPE tdid
      RETURNING VALUE(rt_result) TYPE ty_docu_lines.

    METHODS serialize_docu_usp
      IMPORTING
        io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_xxxx
      IMPORTING
        it_docu TYPE ty_docu_lines
      RAISING
        zcx_abapgit_exception.


    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception .
    METHODS get_field_rules
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abapgit_field_rules.
ENDCLASS.



CLASS zcl_abapgit_object_ueno IMPLEMENTATION.


  METHOD build_text_name.

    TYPES BEGIN OF ty_text_name.
    TYPES id TYPE c LENGTH 4.
    TYPES entity TYPE c LENGTH 26.
    TYPES modifier TYPE c LENGTH 2.
    TYPES END OF ty_text_name.

    DATA ls_text_name TYPE ty_text_name.

    ls_text_name-id = iv_id.
    ls_text_name-entity = mv_entity_id.
    ls_text_name-modifier = 'A%'.

    rv_result = ls_text_name.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item  =  is_item
                        iv_language = iv_language ).

    mv_entity_id = is_item-obj_name.

  ENDMETHOD.


  METHOD delete_docu_uen.

    DATA lt_dm02l TYPE STANDARD TABLE OF dm02l WITH DEFAULT KEY.
    DATA ls_dm02l TYPE dm02l.

    SELECT *
      FROM dm02l
      INTO TABLE lt_dm02l
      WHERE entid = mv_entity_id.

    LOOP AT lt_dm02l INTO ls_dm02l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UENC' "Entity Comments
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UEND' "Entity Definition
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UENE' "Entity Example
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD delete_docu_url.

    DATA lt_dm42s TYPE STANDARD TABLE OF dm42s WITH DEFAULT KEY.
    DATA ls_dm42s LIKE LINE OF lt_dm42s.

    SELECT *
      FROM dm42s
      INTO TABLE lt_dm42s
      WHERE entidto = mv_entity_id.

    LOOP AT lt_dm42s INTO ls_dm42s.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URL1'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URL2'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URLC'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.


  ENDMETHOD.


  METHOD delete_docu_usp.

    DATA lt_dm45l TYPE STANDARD TABLE OF dm45l WITH DEFAULT KEY.
    DATA ls_dm45l LIKE LINE OF lt_dm45l.

    SELECT *
      FROM dm45l
      INTO TABLE lt_dm45l
      WHERE entid = ms_item-obj_name.

    LOOP AT lt_dm45l INTO ls_dm45l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'USPD'
          key1     = ls_dm45l-entid
          key2     = ls_dm45l-as4local
          key3     = ls_dm45l-spezid
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.


  ENDMETHOD.


  METHOD deserialize_docu_uen.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_UENC'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).


    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_UEND'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).


    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_UENE'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_url.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_URL1'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_URL2'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_URLC'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_usp.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_USPD'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_xxxx.

    DATA ls_docu LIKE LINE OF it_docu.
    DATA lv_objname TYPE lxeobjname.
    DATA lv_change_flag TYPE char1.
    DATA lv_error_status  TYPE lxestatprc.

    LOOP AT it_docu INTO ls_docu.

      ls_docu-header-tdfuser = sy-uname.
      ls_docu-header-tdfdate = sy-datum.
      ls_docu-header-tdftime = sy-uzeit.
      ls_docu-header-tdfreles = sy-saprl.

      ls_docu-header-tdluser = sy-uname.
      ls_docu-header-tdldate = sy-datum.
      ls_docu-header-tdltime = sy-uzeit.
      ls_docu-header-tdlreles = sy-saprl.

      lv_objname = ls_docu-header-tdname.

      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          slang       = mv_language
          tlang       = ls_docu-language
          objtype     = ls_docu-header-tdid
          objname     = lv_objname
          header      = ls_docu-header
          content     = ls_docu-content
        IMPORTING
          change_flag = lv_change_flag
          pstatus     = lv_error_status.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_field_rules.

    DATA:
      lt_fields    TYPE TABLE OF string,
      lv_fields    TYPE string,
      lv_table     TYPE tabname,
      lv_field     TYPE string,
      lv_rule      TYPE string,
      lv_rule_iter TYPE string,
      lv_fill_rule TYPE zif_abapgit_field_rules=>ty_fill_rule,
      lv_prefix    TYPE fieldname,
      lv_suffix    TYPE fieldname.

    ro_result = zcl_abapgit_field_rules=>create( ).

    " Many tables and fields with date,time,user so we encode them
    APPEND 'DM02L,FL,DTU' TO lt_fields.
    APPEND 'DM02T,L,DTU' TO lt_fields.
    APPEND 'DM03S,FL,DTU' TO lt_fields.
    APPEND 'DM25L,FL,DTU' TO lt_fields.
    APPEND 'DM26L,FL,DTU' TO lt_fields.
    APPEND 'DM42S,FL,DTU' TO lt_fields.
    APPEND 'DM42T,L,DTU' TO lt_fields.
    APPEND 'DM43T,L,DU' TO lt_fields.
    APPEND 'DM45L,FL,DTU' TO lt_fields.
    APPEND 'DM45T,L,DTU' TO lt_fields.
    APPEND 'DM46S,FL,DTU' TO lt_fields.

    LOOP AT lt_fields INTO lv_fields.
      SPLIT lv_fields AT ',' INTO lv_table lv_field lv_rule_iter.

      DO strlen( lv_field ) TIMES.
        CASE lv_field(1).
          WHEN 'F'.
            lv_prefix = 'FST'.
          WHEN 'L'.
            lv_prefix = 'LST'.
        ENDCASE.

        lv_rule = lv_rule_iter.
        DO strlen( lv_rule ) TIMES.
          CASE lv_rule(1).
            WHEN 'D'.
              lv_suffix    = 'DATE'.
              lv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-date.
            WHEN 'T'.
              lv_suffix    = 'TIME'.
              lv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-time.
            WHEN 'U'.
              lv_suffix    = 'USER'.
              lv_fill_rule = zif_abapgit_field_rules=>c_fill_rule-user.
          ENDCASE.

          ro_result->add(
            iv_table     = lv_table
            iv_field     = lv_prefix && lv_suffix
            iv_fill_rule = lv_fill_rule ).

          SHIFT lv_rule LEFT.
        ENDDO.

        SHIFT lv_field LEFT.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.


  METHOD is_name_permitted.

    " It is unlikely that a serialized entity will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the entity name.
    " So to be safe, we check. Tx SD11 does this check.

    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        obj_name   = ms_item-obj_name
        obj_type   = ms_item-obj_type
      EXCEPTIONS
        wrong_type = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_docu_uen.

    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'UENC' ).

    io_xml->add( iv_name = 'DOCU_UENC'
                 ig_data = lt_docu ).


    lt_docu = serialize_docu_xxxx( 'UEND' ).

    io_xml->add( iv_name = 'DOCU_UEND'
                 ig_data = lt_docu ).

    lt_docu = serialize_docu_xxxx( 'UENE' ).

    io_xml->add( iv_name = 'DOCU_UENE'
                 ig_data = lt_docu ).
  ENDMETHOD.


  METHOD serialize_docu_url.


    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'URL1' ).
    io_xml->add( iv_name = 'DOCU_URL1'
                 ig_data = lt_docu ).


    lt_docu = serialize_docu_xxxx( 'URL2' ).
    io_xml->add( iv_name = 'DOCU_URL2'
                 ig_data = lt_docu ).

    lt_docu = serialize_docu_xxxx( 'URLC' ).
    io_xml->add( iv_name = 'DOCU_URLC'
                 ig_data = lt_docu ).

  ENDMETHOD.


  METHOD serialize_docu_usp.

    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'USPD' ).

    io_xml->add( iv_name = 'DOCU_USPD'
                 ig_data = lt_docu ).


  ENDMETHOD.


  METHOD serialize_docu_xxxx.

    DATA ls_docu            TYPE ty_docu.
    DATA ls_dokvl           TYPE dokvl.
    DATA lt_dokvl           TYPE STANDARD TABLE OF dokvl.
    DATA lv_error_status    TYPE lxestatprc.
    DATA lv_objname         TYPE lxeobjname.


    ls_dokvl-object = build_text_name( iv_id = iv_id ).

    SELECT id object langu
      FROM dokvl
      INTO CORRESPONDING FIELDS OF TABLE lt_dokvl
      WHERE id = c_text_object_type
      AND   object LIKE ls_dokvl-object ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_dokvl INTO ls_dokvl.

      ls_docu-language = ls_dokvl-langu.
      lv_objname = ls_dokvl-object.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          lang    = ls_docu-language
          objtype = c_text_object_type
          objname = lv_objname
        IMPORTING
          header  = ls_docu-header
          content = ls_docu-content
          itf     = ls_docu-itf
          pstatus = lv_error_status.

      CHECK lv_error_status = 'S'. "Success

      " Administrative information is not
      CLEAR ls_docu-header-tdfuser.
      CLEAR ls_docu-header-tdfdate.
      CLEAR ls_docu-header-tdftime.
      CLEAR ls_docu-header-tdfreles.

      CLEAR ls_docu-header-tdluser.
      CLEAR ls_docu-header-tdldate.
      CLEAR ls_docu-header-tdltime.
      CLEAR ls_docu-header-tdlreles.

      APPEND ls_docu TO rt_result.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lstuser INTO rv_user
      FROM dm02l
      WHERE entid = mv_entity_id
      AND as4local = c_active_state.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    " The deletion of the documentation occurs before the deletion of
    " the associated tables - otherwise we don't know what
    " documentation needs deletion
    delete_docu_uen( ).
    delete_docu_url( ).
    delete_docu_usp( ).

    " the deletion of the tables of the entity
    get_generic( )->delete( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    " Is the entity type name compliant with naming conventions?
    " Entity Type have their own conventions.
    is_name_permitted( ).

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

    deserialize_docu_uen( io_xml ).
    deserialize_docu_url( io_xml ).
    deserialize_docu_usp( io_xml ).

    " You are reminded that entity types are not relevant for activation.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'ESDUM'
      iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    " The function module listed below do not open a new window - so we revert to BDC.
    "    CALL FUNCTION 'SDU_MODEL_SHOW'
    "    CALL FUNCTION 'RS_TOOL_ACCESS'

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMUD00'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-ENTI'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-OBJ_KEY'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SD11'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

    serialize_docu_uen( io_xml ).
    serialize_docu_url( io_xml ).
    serialize_docu_usp( io_xml ).

  ENDMETHOD.
ENDCLASS.
