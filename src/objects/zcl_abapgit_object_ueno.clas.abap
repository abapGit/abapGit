class ZCL_ABAPGIT_OBJECT_UENO definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .

  methods CONSTRUCTOR
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
      !IV_LANGUAGE type SPRAS .
  PROTECTED SECTION.

    METHODS corr_insert
        REDEFINITION.

  PRIVATE SECTION.


    " You are reminded that the text serialisation / de-serialisation methods depend upon a common type.
    " To make the dependency explicit, the types are defined as class types not method types.
    TYPES BEGIN OF ty_udmo_text_type.
    TYPES sprache TYPE dm40t-sprache.
    TYPES dmoid TYPE dm40t-dmoid.
    TYPES langbez TYPE dm40t-langbez.
    TYPES as4local TYPE dm40t-as4local.
    TYPES END OF ty_udmo_text_type.

    TYPES BEGIN OF ty_language_type.
    TYPES language TYPE dm40t-sprache.
    TYPES END OF ty_language_type.

    TYPES BEGIN OF ty_docu.
    TYPES language TYPE dm40t-sprache.
    TYPES header   TYPE thead.
    TYPES content TYPE xstring.
    TYPES itf     TYPE tsftext.
    TYPES END OF ty_docu.

    TYPES ty_docu_lines TYPE STANDARD TABLE OF ty_docu WITH DEFAULT KEY.

    DATA mv_master_language TYPE masterlang.
    DATA mv_entity_id TYPE udentity.
    DATA mv_data_model TYPE uddmodl.

    DATA mv_lxe_text_name TYPE lxeobjname.
    DATA mv_activation_state TYPE as4local.
    DATA ms_object_type TYPE rsdeo.

    CONSTANTS c_transport_object_class TYPE trobjtype VALUE 'SUDM' ##NO_TEXT.
    CONSTANTS c_text_object_type TYPE lxeobjtype VALUE 'IM' ##NO_TEXT.
    CONSTANTS c_correction_object_type TYPE rsdeo-objtype VALUE 'UENO' ##NO_TEXT.
    CONSTANTS c_active_state TYPE as4local VALUE 'A' ##NO_TEXT.



    METHODS build_text_name
      IMPORTING VALUE(iv_id)     TYPE tdid
      RETURNING VALUE(rv_result) TYPE doku_obj.

    METHODS is_name_permitted
      RAISING
        zcx_abapgit_exception.

    METHODS update_tree.

    METHODS delete_dm02l
      RAISING zcx_abapgit_exception.
    METHODS delete_dm02s
      RAISING zcx_abapgit_exception.
    METHODS delete_dm02t
      RAISING zcx_abapgit_exception.
    METHODS delete_dm03s
      RAISING zcx_abapgit_exception.
    METHODS delete_dm25l
      RAISING zcx_abapgit_exception.
    METHODS delete_dm26l
      RAISING zcx_abapgit_exception.
    METHODS delete_dm42s
      RAISING zcx_abapgit_exception.
    METHODS delete_dm42t
      RAISING zcx_abapgit_exception.
    METHODS delete_dm43t
      RAISING zcx_abapgit_exception.
    METHODS delete_dm45l
      RAISING zcx_abapgit_exception.
    METHODS delete_dm45t
      RAISING zcx_abapgit_exception.
    METHODS delete_dm46s
      RAISING zcx_abapgit_exception.

    METHODS deserialize_master_language
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.


    METHODS deserialize_dm02l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm02s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm02t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm03s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm25l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm26l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.


    METHODS deserialize_dm42s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm42t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm43t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm45l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_dm45t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.


    METHODS deserialize_dm46s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_uenc
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.



    METHODS deserialize_docu_uend
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_uene
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_url1
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.


    METHODS deserialize_docu_url2
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu_urlc
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.


    METHODS deserialize_docu_uspd
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception.


    METHODS access_modify
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS access_free
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.



    METHODS serialize_master_language
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm02l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm02t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm42s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm02s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm45l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm45t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm03s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm25l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm43t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm42t
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm46s
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_dm26l
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.



    METHODS serialize_docu_uend
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_uenc
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_uene
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_url1
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_url2
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_urlc
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_docu_xxxx
      IMPORTING
                iv_doku_obj      TYPE doku_obj
      RETURNING VALUE(rt_result) TYPE ty_docu_lines.


    METHODS serialize_docu_uspd
      IMPORTING
        io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_docu
      IMPORTING
        it_docu TYPE ty_docu_lines
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_UENO IMPLEMENTATION.


  METHOD access_free.

    " Release the lock on the object.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode                     = 'FREE'
        object                   = me->ms_object_type
        object_class             = me->c_transport_object_class
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 2
        enqueue_system_failure   = 3
        illegal_parameter_values = 4
        locked_by_author         = 5
        no_modify_permission     = 6
        no_show_permission       = 7
        permission_failure       = 8
        request_language_denied  = 9
        OTHERS                   = 10.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD access_modify.

* You are reminded that mode modify is the same as insert, with one important difference:

* Mode INSERT is intended for newly created objects, for which a TADIR entry does not yet
* exist. In that case, the system shows a pop-up for the entry of the package, which isn't
* desirable when the SAPGUI is not available.

* In the context of abapGit, the package is known.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check          = abap_true
        global_lock              = abap_true
        mode                     = 'MODIFY'
        object                   = me->ms_object_type
        object_class             = me->c_transport_object_class
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 2
        enqueue_system_failure   = 3
        illegal_parameter_values = 4
        locked_by_author         = 5
        no_modify_permission     = 6
        no_show_permission       = 7
        permission_failure       = 8
        request_language_denied  = 9
        OTHERS                   = 10.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD build_text_name.

    TYPES BEGIN OF ty_text_name.
    TYPES id TYPE c LENGTH 4.
    TYPES entity TYPE c LENGTH 26.
    TYPES modifier TYPE c LENGTH 2.
    TYPES END OF ty_text_name.

    DATA ls_text_name TYPE ty_text_name.

    ls_text_name-id = iv_id.
    ls_text_name-entity = me->mv_entity_id.
    ls_text_name-modifier = 'A%'.

    rv_result = ls_text_name.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item  =  is_item  iv_language = iv_language ).

    " Conversion to Entity Identity
    me->mv_entity_id = is_item-obj_name.
    " Default activation state is active
    me->mv_activation_state = c_active_state.
    " Correction and Transport System object
    me->ms_object_type-objtype = c_correction_object_type.
    me->ms_object_type-objname = is_item-obj_name.

  ENDMETHOD.


  METHOD corr_insert.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = me->ms_object_type
        object_class        = me->c_transport_object_class
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'I'
        global_lock         = abap_true
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.


  METHOD delete_dm02l.

    DATA lt_dm02l TYPE STANDARD TABLE OF dm02l WITH DEFAULT KEY.
    DATA ls_dm02l TYPE dm02l.

    SELECT *
      FROM dm02l
      INTO TABLE lt_dm02l
      WHERE entid    EQ me->mv_entity_id.

    LOOP AT lt_dm02l INTO ls_dm02l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = sy-langu
          obj_id   = 'UENC' "Entity Comments
        EXCEPTIONS
          ret_code = 0.


      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = sy-langu
          obj_id   = 'UEND' "Entity Definition
        EXCEPTIONS
          ret_code = 0.



      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = sy-langu
          obj_id   = 'UENE' "Entity Example
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.

    DELETE FROM dm02l
      WHERE entid    EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm02s.
    DELETE FROM dm02s
      WHERE entid    EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm02t.

    DELETE FROM dm02t
      WHERE entid    EQ me->mv_entity_id.


  ENDMETHOD.


  METHOD delete_dm03s.

    DELETE FROM dm03s
      WHERE entid    EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm25l.

    DELETE FROM dm25l
      WHERE entid    EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm26l.

    DELETE FROM dm26l
      WHERE entid    EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm42s.

    DATA lt_dm42s TYPE STANDARD TABLE OF dm42s WITH DEFAULT KEY.
    DATA ls_dm42s LIKE LINE OF lt_dm42s.

    SELECT *
      FROM dm42s
      INTO TABLE lt_dm42s
      WHERE entidto  EQ me->mv_entity_id.

    LOOP AT lt_dm42s INTO ls_dm42s.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = sy-langu
          obj_id   = 'URL1'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = sy-langu
          obj_id   = 'URL2'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = sy-langu
          obj_id   = 'URLC'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.

    DELETE FROM dm42s
      WHERE entidto  EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm42t.
    " Relationship texts
    DELETE FROM dm42t
      WHERE entidto EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm43t.

    DELETE FROM dm43t
      WHERE entid EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm45l.
    " Specializ. categories

    DATA lt_dm45l TYPE STANDARD TABLE OF dm45l WITH DEFAULT KEY.
    DATA ls_dm45l LIKE LINE OF lt_dm45l.

    SELECT *
      FROM dm45l
      INTO TABLE lt_dm45l
      WHERE entid EQ me->mv_entity_id.

    LOOP AT lt_dm45l INTO ls_dm45l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = sy-langu
          obj_id   = 'USPD'
          key1     = ls_dm45l-entid
          key2     = ls_dm45l-as4local
          key3     = ls_dm45l-spezid
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.

    DELETE FROM dm45l
      WHERE entid EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm45t.
    " Specialization texts

    DELETE FROM dm45t
                      WHERE entid    EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD delete_dm46s.

    DELETE FROM dm46s
                      WHERE entidto  EQ me->mv_entity_id.

  ENDMETHOD.


  METHOD deserialize_dm02l.

    " See SDU_ENTITY_PUT

    DATA ls_dm02l TYPE dm02l.

    io_xml->read( EXPORTING iv_name = 'DM02L'
                  CHANGING  cg_data = ls_dm02l ).

    " Update administration information
    ls_dm02l-fstuser = sy-uname.
    ls_dm02l-fstdate = sy-datum.
    ls_dm02l-fsttime = sy-uzeit.
    ls_dm02l-lstuser = sy-uname.
    ls_dm02l-lstdate = sy-datum.
    ls_dm02l-lsttime = sy-uzeit.

    " Persist
    MODIFY dm02l FROM ls_dm02l.


  ENDMETHOD.


  METHOD deserialize_dm02s.

    DATA ls_dm02s TYPE dm02s.
    DATA lt_dm02s TYPE STANDARD TABLE OF dm02s WITH DEFAULT KEY.

    io_xml->read( EXPORTING iv_name = 'DM02S'
                  CHANGING  cg_data = lt_dm02s ).


    LOOP AT lt_dm02s INTO ls_dm02s.

      " Update administration information
      ls_dm02s-fstuser = sy-uname.
      ls_dm02s-fstdate = sy-datum.
      ls_dm02s-fsttime = sy-uzeit.

      ls_dm02s-lstuser = sy-uname.
      ls_dm02s-lstdate = sy-datum.
      ls_dm02s-lsttime = sy-uzeit.

      " Persist
      MODIFY dm02s FROM ls_dm02s.

    ENDLOOP.


  ENDMETHOD.


  METHOD deserialize_dm02t.

    DATA ls_dm02t TYPE dm02t.
    DATA lt_dm02t TYPE STANDARD TABLE OF dm02t WITH DEFAULT KEY.

    io_xml->read( EXPORTING iv_name = 'DM02T'
                  CHANGING  cg_data = lt_dm02t ).


    LOOP AT lt_dm02t INTO ls_dm02t.

      " Update administration information
      ls_dm02t-lstuser = sy-uname.
      ls_dm02t-lstdate = sy-datum.
      ls_dm02t-lsttime = sy-uzeit.

      " Persist
      MODIFY dm02t FROM ls_dm02t.

    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_dm03s.

    DATA ls_dm03s TYPE dm03s.
    DATA lt_dm03s TYPE STANDARD TABLE OF dm03s WITH DEFAULT KEY.

    io_xml->read( EXPORTING iv_name = 'DM03S'
                  CHANGING  cg_data = lt_dm03s ).


    LOOP AT lt_dm03s INTO ls_dm03s.

      " Update administration information
      ls_dm03s-fstuser = sy-uname.
      ls_dm03s-fstdate = sy-datum.
      ls_dm03s-fsttime = sy-uzeit.

      ls_dm03s-lstuser = sy-uname.
      ls_dm03s-lstdate = sy-datum.
      ls_dm03s-lsttime = sy-uzeit.

      " Persist
      MODIFY dm03s FROM ls_dm03s.

    ENDLOOP.


  ENDMETHOD.


  METHOD deserialize_dm25l.

    DATA lt_dm25l TYPE STANDARD TABLE OF dm25l WITH DEFAULT KEY.
    DATA ls_dm25l TYPE dm25l.

    io_xml->read( EXPORTING iv_name = 'DM25L'
                  CHANGING  cg_data = lt_dm25l ).


    LOOP AT lt_dm25l INTO ls_dm25l.

      " Update administration information
      ls_dm25l-fstuser = sy-uname.
      ls_dm25l-fstdate = sy-datum.
      ls_dm25l-fsttime = sy-uzeit.

      ls_dm25l-lstuser = sy-uname.
      ls_dm25l-lstdate = sy-datum.
      ls_dm25l-lsttime = sy-uzeit.

      " Persist
      MODIFY dm25l FROM ls_dm25l.

    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_dm26l.

    DATA lt_dm26l TYPE STANDARD TABLE OF dm26l WITH DEFAULT KEY.
    DATA ls_dm26l TYPE dm26l.

    io_xml->read( EXPORTING iv_name = 'DM26L'
                  CHANGING  cg_data = lt_dm26l ).


    LOOP AT lt_dm26l INTO ls_dm26l.

      " Update administration information
      ls_dm26l-fstuser = sy-uname.
      ls_dm26l-fstdate = sy-datum.
      ls_dm26l-fsttime = sy-uzeit.

      ls_dm26l-lstuser = sy-uname.
      ls_dm26l-lstdate = sy-datum.
      ls_dm26l-lsttime = sy-uzeit.

      " Persist
      MODIFY dm26l FROM ls_dm26l.

    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_dm42s.

    DATA lt_dm42s TYPE STANDARD TABLE OF dm42s WITH DEFAULT KEY.
    DATA ls_dm42s TYPE dm42s.

    io_xml->read( EXPORTING iv_name = 'DM42S'
                  CHANGING  cg_data = lt_dm42s ).


    LOOP AT lt_dm42s INTO ls_dm42s.

      " Update administration information
      ls_dm42s-fstuser = sy-uname.
      ls_dm42s-fstdate = sy-datum.
      ls_dm42s-fsttime = sy-uzeit.

      ls_dm42s-lstuser = sy-uname.
      ls_dm42s-lstdate = sy-datum.
      ls_dm42s-lsttime = sy-uzeit.

      " Persist
      MODIFY dm42s FROM ls_dm42s.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_dm42t.

    DATA lt_dm42t TYPE STANDARD TABLE OF dm42t WITH DEFAULT KEY.
    DATA ls_dm42t TYPE dm42t.

    io_xml->read( EXPORTING iv_name = 'DM42T'
                  CHANGING  cg_data = lt_dm42t ).


    LOOP AT lt_dm42t INTO ls_dm42t.

      " Update administration information
      ls_dm42t-lstuser = sy-uname.
      ls_dm42t-lstdate = sy-datum.
      ls_dm42t-lsttime = sy-uzeit.

      " Persist
      MODIFY dm42t FROM ls_dm42t.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_dm43t.

    DATA lt_dm43t TYPE STANDARD TABLE OF dm43t WITH DEFAULT KEY.
    DATA ls_dm43t TYPE dm43t.

    io_xml->read( EXPORTING iv_name = 'DM43T'
                  CHANGING  cg_data = lt_dm43t ).


    LOOP AT lt_dm43t INTO ls_dm43t.

      " Update administration information
      ls_dm43t-lstuser = sy-uname.
      ls_dm43t-lstdate = sy-datum.

      " Persist
      MODIFY dm43t FROM ls_dm43t.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_dm45l.
    DATA lt_dm45l TYPE STANDARD TABLE OF dm45l WITH DEFAULT KEY.
    DATA ls_dm45l TYPE dm45l.

    io_xml->read( EXPORTING iv_name = 'DM45L'
                  CHANGING  cg_data = lt_dm45l ).


    LOOP AT lt_dm45l INTO ls_dm45l.

      " Update administration information
      ls_dm45l-lstuser = sy-uname.
      ls_dm45l-lstdate = sy-datum.
      ls_dm45l-lsttime = sy-uzeit.

      ls_dm45l-fstuser = sy-uname.
      ls_dm45l-fstdate = sy-datum.
      ls_dm45l-fsttime = sy-uzeit.


      " Persist
      MODIFY dm45l FROM ls_dm45l.

    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_dm45t.
    DATA lt_dm45t TYPE STANDARD TABLE OF dm45t WITH DEFAULT KEY.
    DATA ls_dm45t TYPE dm45t.

    io_xml->read( EXPORTING iv_name = 'DM45T'
                  CHANGING  cg_data = lt_dm45t ).


    LOOP AT lt_dm45t INTO ls_dm45t.

      " Update administration information
      ls_dm45t-lstuser = sy-uname.
      ls_dm45t-lstdate = sy-datum.

      " Persist
      MODIFY dm45t FROM ls_dm45t.

    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_dm46s.

    DATA lt_dm46s TYPE STANDARD TABLE OF dm46s WITH DEFAULT KEY.
    DATA ls_dm46s TYPE dm46s.

    io_xml->read( EXPORTING iv_name = 'DM46S'
                  CHANGING  cg_data = lt_dm46s ).


    LOOP AT lt_dm46s INTO ls_dm46s.

      " Update administration information
      ls_dm46s-lstuser = sy-uname.
      ls_dm46s-lstdate = sy-datum.
      ls_dm46s-lsttime = sy-uzeit.

      ls_dm46s-fstuser = sy-uname.
      ls_dm46s-fstdate = sy-datum.
      ls_dm46s-fsttime = sy-uzeit.

      " Persist
      MODIFY dm46s FROM ls_dm46s.

    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_docu.

    DATA ls_docu LIKE LINE OF it_docu.
    DATA lv_objname TYPE lxeobjname.
    DATA lv_change_flag TYPE char1.
    DATA lv_error_status  TYPE lxestatprc.

    LOOP AT it_docu INTO ls_docu.

      ls_docu-header-tdfuser = sy-uname.
      ls_docu-header-tdfdate = sy-datum.
      ls_docu-header-tdftime = sy-uzeit.

      ls_docu-header-tdluser = sy-uname.
      ls_docu-header-tdldate = sy-datum.
      ls_docu-header-tdltime = sy-uzeit.

      lv_objname = ls_docu-header-tdname.

      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          slang       = me->mv_master_language
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


  METHOD deserialize_docu_uenc.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_UENC'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_uend.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_UEND'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_uene.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_UENE'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_url1.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_URL1'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_url2.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_URL2'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_urlc.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_URLC'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_docu_uspd.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_USPD'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu( lt_docu ).

  ENDMETHOD.


  METHOD deserialize_master_language.

    io_xml->read( EXPORTING iv_name = 'MASTER_LANGUAGE'
                  CHANGING cg_data = me->mv_master_language ).


  ENDMETHOD.


  METHOD is_name_permitted.

    " It is unlikely that a serialized entity will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the entity name.
    " So to be safe, we check. Tx SD11 does this check.
    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        obj_name   = me->ms_object_type-objname
        obj_type   = me->ms_object_type-objtype
      EXCEPTIONS
        wrong_type = 01.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_dm02l.
    " The entity
    DATA ls_dm02l TYPE dm02l.

    " You are reminded that the uses of the indicator LOESCHZ was deprecated around 2000
    SELECT SINGLE *
      FROM dm02l INTO ls_dm02l
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from UENO - entity serialisation' ).
    ENDIF.

    " You are reminded that administrative data is not serialised.
    CLEAR ls_dm02l-lstdate.
    CLEAR ls_dm02l-lsttime.
    CLEAR ls_dm02l-lstuser.

    CLEAR ls_dm02l-fstdate.
    CLEAR ls_dm02l-fsttime.
    CLEAR ls_dm02l-fstuser.

    io_xml->add( iv_name = 'DM02L'
                 ig_data = ls_dm02l ).

  ENDMETHOD.


  METHOD serialize_dm02s.

    " The entity variants
    DATA lt_dm02s TYPE STANDARD TABLE OF dm02s.
    DATA ls_dm02s TYPE dm02s.

    SELECT *
      FROM dm02s
      INTO TABLE lt_dm02s
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm02s
      FROM ls_dm02s
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM02S'
                 ig_data = lt_dm02s ).

  ENDMETHOD.


  METHOD serialize_dm02t.
    " The entity descriptions - there can be 0, 1 or more
    DATA lt_dm02t TYPE STANDARD TABLE OF dm02t.
    DATA ls_dm02t TYPE dm02t.

    SELECT *
      FROM dm02t
      INTO TABLE lt_dm02t
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.


    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm02t
      FROM ls_dm02t
      TRANSPORTING lstdate lsttime lstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM02T'
                 ig_data = lt_dm02t ).



  ENDMETHOD.


  METHOD serialize_dm03s.
    " The entity attributes
    DATA lt_dm03s TYPE STANDARD TABLE OF dm03s.
    DATA ls_dm03s TYPE dm03s.

    SELECT *
      FROM dm03s
      INTO TABLE lt_dm03s
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm03s
      FROM ls_dm03s
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM03S'
                 ig_data = lt_dm03s ).

  ENDMETHOD.


  METHOD serialize_dm25l.
    " The entity view assignment
    DATA lt_dm25l TYPE STANDARD TABLE OF dm25l.
    DATA ls_dm25l TYPE dm25l.

    SELECT *
      FROM dm25l
      INTO TABLE lt_dm25l
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm25l
      FROM ls_dm25l
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM25L'
                 ig_data = lt_dm25l ).

  ENDMETHOD.


  METHOD serialize_dm26l.
    " The entity table assignment
    DATA lt_dm26l TYPE STANDARD TABLE OF dm26l.
    DATA ls_dm26l TYPE dm26l.

    " You are reminded that transport system doesn't transport logically deleted objects
    " so neither does abapGit.
    SELECT *
      FROM dm26l
      INTO TABLE lt_dm26l
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialized.
    MODIFY lt_dm26l
      FROM ls_dm26l
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM26L'
                 ig_data = lt_dm26l ).

  ENDMETHOD.


  METHOD serialize_dm42s.
    " The entity relationships
    DATA lt_dm42s TYPE STANDARD TABLE OF dm42s.
    DATA ls_dm42s TYPE dm42s.

    " You are also reminded that the transport system doesn't transport outgoing relations.
    " so neither does abapGit.
    SELECT  *
      FROM dm42s
      INTO TABLE lt_dm42s
      WHERE entidto  EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm42s
      FROM ls_dm42s
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM42S'
                 ig_data = lt_dm42s ).

  ENDMETHOD.


  METHOD serialize_dm42t.

    " The entity relationship short text
    DATA lt_dm42t TYPE STANDARD TABLE OF dm42t.
    DATA ls_dm42t TYPE dm42t.

    " You are reminded that logical deletion indication was retired around 2000. See RUDDELLZ
    " You are also reminded that the transport system doesn't transport outgoing relations.
    " so neither does abapGit.
    SELECT *
      FROM dm42t
      INTO TABLE lt_dm42t
      WHERE entidto    EQ me->mv_entity_id
      AND   as4local   EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialized.
    MODIFY lt_dm42t
      FROM ls_dm42t
      TRANSPORTING lstdate lsttime lstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM42T'
                 ig_data = lt_dm42t ).



  ENDMETHOD.


  METHOD serialize_dm43t.

    " The entity aliases
    DATA lt_dm43t TYPE STANDARD TABLE OF dm43t.
    DATA ls_dm43t TYPE dm43t.

    " You are reminded that DM43T doesn't have a deletion indicator
    SELECT *
      FROM dm43t
      INTO TABLE lt_dm43t
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm43t
      FROM ls_dm43t
      TRANSPORTING lstdate lstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM43T'
                 ig_data = lt_dm43t ).



  ENDMETHOD.


  METHOD serialize_dm45l.
    " The specialization categories
    DATA lt_dm45l TYPE STANDARD TABLE OF dm45l.
    DATA ls_dm45l TYPE dm45l.

    SELECT *
      FROM dm45l
      INTO TABLE lt_dm45l
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm45l
      FROM ls_dm45l
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM45L'
                 ig_data = lt_dm45l ).

  ENDMETHOD.


  METHOD serialize_dm45t.
    " The specialization short texts
    DATA lt_dm45t TYPE STANDARD TABLE OF dm45t.
    DATA ls_dm45t TYPE dm45t.

    SELECT * FROM dm45t
      INTO TABLE lt_dm45t
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialised.
    MODIFY lt_dm45t
      FROM ls_dm45t
      TRANSPORTING lstdate lsttime lstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM45T'
                 ig_data = lt_dm45t ).

  ENDMETHOD.


  METHOD serialize_dm46s.
    " The entity specialization
    DATA lt_dm46s TYPE STANDARD TABLE OF dm46s.
    DATA ls_dm46s TYPE dm46s.

    " You are also reminded that the transport system doesn't transport outgoing.
    " so neither does abapGit.
    SELECT  * FROM dm46s INTO TABLE lt_dm46s
                  WHERE entidto  EQ me->mv_entity_id
                  AND   as4local EQ me->mv_activation_state.

    " You are reminded that administrative data is not serialized.
    MODIFY lt_dm46s
      FROM ls_dm46s
      TRANSPORTING lstdate lsttime lstuser fstdate fsttime fstuser
      WHERE as4local = me->mv_activation_state.

    io_xml->add( iv_name = 'DM46S'
                 ig_data = lt_dm46s ).

  ENDMETHOD.


  METHOD serialize_docu_uenc.

    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.

    lv_doku_obj = me->build_text_name( iv_id = 'UENC' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_UENC'
                 ig_data = lt_docu ).

  ENDMETHOD.


  METHOD serialize_docu_uend.

    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.


    lv_doku_obj = me->build_text_name( iv_id = 'UEND' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_UEND'
                 ig_data = lt_docu ).

  ENDMETHOD.


  METHOD serialize_docu_uene.

    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.

    lv_doku_obj = me->build_text_name( iv_id = 'UENE' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_UENE'
                 ig_data = lt_docu ).

  ENDMETHOD.


  METHOD serialize_docu_url1.
    " URL identifies relationship towards according to SAPMUDI2
    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.

    lv_doku_obj = me->build_text_name( iv_id = 'URL1' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_URL1'
                 ig_data = lt_docu ).


  ENDMETHOD.


  METHOD serialize_docu_url2.
    " URL2 identifies relationship backwards according to SAPMUDI2
    " Seems no longer maintained although very old objects may have
    " such documentation
    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.

    lv_doku_obj = me->build_text_name( iv_id = 'URL2' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_URL2'
                 ig_data = lt_docu ).


  ENDMETHOD.


  METHOD serialize_docu_urlc.

    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.

    lv_doku_obj = me->build_text_name( iv_id = 'URLC' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_URLC'
                 ig_data = lt_docu ).



  ENDMETHOD.


  METHOD serialize_docu_uspd.

    DATA lv_doku_obj        TYPE doku_obj.
    DATA lt_docu            TYPE ty_docu_lines.

    lv_doku_obj = me->build_text_name( iv_id = 'USPD' ).

    lt_docu = serialize_docu_xxxx( lv_doku_obj ).

    io_xml->add( iv_name = 'DOCU_USPD'
                 ig_data = lt_docu ).


  ENDMETHOD.


  METHOD serialize_docu_xxxx.

    DATA ls_docu            TYPE ty_docu.
    DATA ls_dokvl           TYPE dokvl.
    DATA lt_docu            TYPE ty_docu_lines.
    DATA lt_dokvl           TYPE STANDARD TABLE OF dokvl.
    DATA lv_error_status    TYPE lxestatprc.
    DATA lv_objname         TYPE lxeobjname.

    ls_dokvl-object = iv_doku_obj.

    SELECT id object langu
      FROM dokvl
      INTO CORRESPONDING FIELDS OF TABLE lt_dokvl
      WHERE id = c_text_object_type
      AND   object LIKE ls_dokvl-object.

    LOOP AT lt_dokvl INTO ls_dokvl.

      ls_docu-language = ls_dokvl-langu.
      lv_objname = ls_dokvl-object.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          lang    = ls_docu-language
          objtype = 'IM'
          objname = lv_objname
        IMPORTING
          header  = ls_docu-header
          content = ls_docu-content
          pstatus = lv_error_status.

      CHECK lv_error_status = 'S'. "Success

      " Administrative information is not serialised
      CLEAR ls_docu-header-tdfuser.
      CLEAR ls_docu-header-tdfdate.
      CLEAR ls_docu-header-tdftime.

      CLEAR ls_docu-header-tdluser.
      CLEAR ls_docu-header-tdldate.
      CLEAR ls_docu-header-tdltime.

      APPEND ls_docu TO rt_result.

    ENDLOOP.


  ENDMETHOD.


  METHOD serialize_master_language.

    " Determine master language of the repository object
    " This is to ensure the object is correctly deserialized.
    " See SDU_LANGUAGE_GET

    SELECT SINGLE masterlang
      FROM tadir
      INTO me->mv_master_language
      WHERE pgmid = 'R3TR'
      AND  object = 'UENO'
      AND  obj_name = me->mv_entity_id.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from UENO - entity serialisation' ).
    ENDIF.

    io_xml->add( iv_name = 'MASTER_LANGUAGE'
                 ig_data = me->mv_master_language ).


  ENDMETHOD.


  METHOD update_tree.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = me->mv_entity_id
        operation = 'INSERT'
        type      = me->c_correction_object_type.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lstuser INTO rv_user
      FROM dm02l
      WHERE entid    EQ me->mv_entity_id
      AND   as4local EQ me->mv_activation_state.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    " The entire object and its dependent parts are deleted.
    " Unfortunately there is an error in SDU_ENTITY_DELETE
    " reported to SAP under incident 510880 / 2019
    " hence the resort to lower level actions.
    TRY.

        delete_dm02l( ).
        delete_dm02s( ).
        delete_dm02t( ).
        delete_dm03s( ).
        delete_dm25l( ).
        delete_dm26l( ).
        delete_dm42s( ).
        delete_dm42t( ).
        delete_dm43t( ).
        delete_dm45l( ).
        delete_dm45t( ).
        delete_dm46s( ).
        " Looking for deletion of documentation?
        " Undertaken during the deletion of the associated dependent objects above

      CATCH zcx_abapgit_exception.
        zcx_abapgit_exception=>raise( 'Error in deletion of UENO' ).

    ENDTRY.


  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* You are reminded that this method checks for
*  - validity of entity type name with regard to naming conventions
*  - permissions and locks
*  - connection to transport and correction system
*  - insert of entity type, relations and all documentation
*  - update of object tree
*  - releasing of lock

* Is the entity type name compliant with naming conventions?
    is_name_permitted( ).

* Access Permission granted?
    access_modify( ).

* Connection to transport and correction system
    corr_insert( iv_package ).

* Insert the entity type entity and associated documentation
    TRY.

        deserialize_master_language( io_xml ).
        deserialize_dm02l( io_xml ).
        deserialize_dm02s( io_xml ).
        deserialize_dm02t( io_xml ).
        deserialize_dm03s( io_xml ).
        deserialize_dm25l( io_xml ).
        deserialize_dm26l( io_xml ).
        deserialize_dm42s( io_xml ).
        deserialize_dm42t( io_xml ).
        deserialize_dm43t( io_xml ).
        deserialize_dm45l( io_xml ).
        deserialize_dm45t( io_xml ).
        deserialize_dm46s( io_xml ).
        deserialize_docu_uenc( io_xml ).
        deserialize_docu_uend( io_xml ).
        deserialize_docu_uene( io_xml ).
        deserialize_docu_url1( io_xml ).
        deserialize_docu_url2( io_xml ).
        deserialize_docu_urlc( io_xml ).
        deserialize_docu_uspd( io_xml ).


        update_tree( ).
        access_free( ).

      CATCH zcx_abapgit_exception.

        me->access_free( ).

        zcx_abapgit_exception=>raise( 'Error in deserialisation of UENO' ).
    ENDTRY.

    " You are reminded that entity types are not relevant for activation.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    SELECT COUNT( * ) FROM  dm02l
           WHERE  entid     = me->mv_entity_id
           AND    as4local  = me->mv_activation_state.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
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

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SD11'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_master_language( io_xml ).
    serialize_dm02l( io_xml ).
    serialize_dm02s( io_xml ).
    serialize_dm02t( io_xml ).
    serialize_dm03s( io_xml ).
    serialize_dm25l( io_xml ).
    serialize_dm26l( io_xml ).
    serialize_dm42s( io_xml ).
    serialize_dm42t( io_xml ).
    serialize_dm43t( io_xml ).
    serialize_dm45l( io_xml ).
    serialize_dm45t( io_xml ).
    serialize_dm46s( io_xml ).
    serialize_docu_uend( io_xml ).
    serialize_docu_uenc( io_xml ).
    serialize_docu_uene( io_xml ).
    serialize_docu_url1( io_xml ).
    serialize_docu_url2( io_xml ).
    serialize_docu_urlc( io_xml ).
    serialize_docu_uspd( io_xml ).


  ENDMETHOD.
ENDCLASS.
