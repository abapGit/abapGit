CLASS zcl_abapgit_object_sicf DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_hash TYPE c LENGTH 25.

    TYPES:
      ty_icfhandler_tt TYPE STANDARD TABLE OF icfhandler WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_sicf_key,
        icf_name   TYPE icfservice-icf_name,
        icfparguid TYPE icfservice-icfparguid,
      END OF ty_sicf_key .

    METHODS serialize_otr
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_otr
      IMPORTING
        !iv_package TYPE devclass
        !io_xml     TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS read
      IMPORTING
        !iv_clear      TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !es_icfservice TYPE icfservice
        !es_icfdocu    TYPE icfdocu
        !et_icfhandler TYPE ty_icfhandler_tt
        !ev_url        TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS insert_sicf
      IMPORTING
        !is_icfservice TYPE icfservice
        !is_icfdocu    TYPE icfdocu
        !it_icfhandler TYPE ty_icfhandler_tt
        !iv_package    TYPE devclass
        !iv_url        TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS change_sicf
      IMPORTING
        !is_icfservice TYPE icfservice
        !is_icfdocu    TYPE icfdocu
        !it_icfhandler TYPE ty_icfhandler_tt
        !iv_package    TYPE devclass
        !iv_parent     TYPE icfparguid
      RAISING
        zcx_abapgit_exception .
    METHODS to_icfhndlist
      IMPORTING
        !it_list       TYPE ty_icfhandler_tt
      RETURNING
        VALUE(rt_list) TYPE icfhndlist .
    METHODS find_parent
      IMPORTING
        !iv_url          TYPE string
      RETURNING
        VALUE(rv_parent) TYPE icfparguid
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS get_hash_from_object
      IMPORTING
        !iv_obj_name   TYPE tadir-obj_name
      RETURNING
        VALUE(rv_hash) TYPE ty_hash
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_sicf IMPLEMENTATION.


  METHOD change_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          lt_existing   TYPE TABLE OF icfhandler,
          ls_icfserdesc TYPE icfserdesc.

    FIELD-SYMBOLS: <ls_existing> LIKE LINE OF lt_existing.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).

* Do not add handlers if they already exist, it will make the below
* call to SAP standard code raise an exception
    SELECT * FROM icfhandler INTO TABLE lt_existing
      WHERE icf_name = is_icfservice-icf_name.
    LOOP AT lt_existing ASSIGNING <ls_existing>.
      DELETE TABLE lt_icfhndlist FROM <ls_existing>-icfhandler.
    ENDLOOP.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~change_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = iv_parent
        icfdocu                   = is_icfdocu
        doculang                  = mv_language
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD deserialize_otr.

    DATA:
      lt_sots     TYPE zcl_abapgit_sots_handler=>ty_sots_tt,
      lt_sots_use TYPE zcl_abapgit_sots_handler=>ty_sots_use_tt.

    FIELD-SYMBOLS:
      <ls_sots_use> LIKE LINE OF lt_sots_use.

    io_xml->read( EXPORTING iv_name = 'SOTS'
                  CHANGING cg_data = lt_sots ).
    io_xml->read( EXPORTING iv_name = 'SOTS_USE'
                  CHANGING cg_data = lt_sots_use ).

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use>.
      <ls_sots_use>-obj_name = ms_item-obj_name.
    ENDLOOP.

    zcl_abapgit_sots_handler=>create_sots_from_data(
      iv_package  = iv_package
      it_sots     = lt_sots
      it_sots_use = lt_sots_use ).

  ENDMETHOD.


  METHOD find_parent.

    cl_icf_tree=>if_icf_tree~service_from_url(
      EXPORTING
        url                   = iv_url
        hostnumber            = 0
      IMPORTING
        icfnodguid            = rv_parent
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_hash_from_object.

    DATA:
      lv_icfnodguid TYPE icfservice-icfnodguid,
      lv_url        TYPE icfurlbuf,
      lv_ext_url    TYPE string.

    SELECT SINGLE icfnodguid FROM icfservice INTO lv_icfnodguid
      WHERE icf_name = iv_obj_name(15)
      AND icfparguid = iv_obj_name+15.

    IF sy-subrc = 0.
      CALL FUNCTION 'HTTP_GET_URL_FROM_NODGUID'
        EXPORTING
          nodguid      = lv_icfnodguid
        IMPORTING
          url          = lv_url
          extended_url = lv_ext_url
        EXCEPTIONS
          icf_inconst  = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        " It's possible that the URL contains the system id, for example for WD applications with names
        " longer than 15 characters. In that case, use the extended URL to generate the hash (#5064)
        IF lv_ext_url <> lv_url.
          rv_hash = zcl_abapgit_hash=>sha1_raw( zcl_abapgit_convert=>string_to_xstring_utf8( |{ lv_ext_url }| ) ).
        ELSE.
          rv_hash = zcl_abapgit_hash=>sha1_raw( zcl_abapgit_convert=>string_to_xstring_utf8( |{ lv_url }| ) ).
        ENDIF.
      ENDIF.
    ELSE.
      rv_hash = to_lower( iv_obj_name+15 ).
    ENDIF.

  ENDMETHOD.


  METHOD insert_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc,
          ls_icfdocu    TYPE icfdocu,
          lv_icfnodguid TYPE icfnodguid,
          lv_parent     TYPE icfparguid.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).
    lv_parent = find_parent( iv_url ).

* nice, it seems that the structure should be mistreated
    ls_icfdocu = is_icfdocu-icf_docu.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~insert_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = lv_parent
        icfdocu                   = ls_icfdocu
        doculang                  = mv_language
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
        icfaltnme                 = is_icfservice-icfaltnme
      IMPORTING
        icfnodguid                = lv_icfnodguid
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Update item with name assigned by system
    SELECT SINGLE icfparguid INTO ms_item-obj_name+15 FROM icfservice
      WHERE icfnodguid = lv_icfnodguid.

  ENDMETHOD.


  METHOD read.

    DATA: lt_serv_info TYPE icfservtbl,
          ls_serv_info LIKE LINE OF lt_serv_info,
          ls_key       TYPE ty_sicf_key.

    FIELD-SYMBOLS: <ls_icfhandler> LIKE LINE OF et_icfhandler.


    CLEAR es_icfservice.
    CLEAR es_icfdocu.
    CLEAR et_icfhandler.
    CLEAR ev_url.

    ls_key-icf_name   = ms_item-obj_name(15).
    ls_key-icfparguid = ms_item-obj_name+15.

    cl_icf_tree=>if_icf_tree~get_info_from_serv(
      EXPORTING
        icf_name          = ls_key-icf_name
        icfparguid        = ls_key-icfparguid
        icf_langu         = mv_language
      IMPORTING
        serv_info         = lt_serv_info
        icfdocu           = es_icfdocu
        url               = ev_url
      EXCEPTIONS
        wrong_name        = 1
        wrong_parguid     = 2
        incorrect_service = 3
        no_authority      = 4
        OTHERS            = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ASSERT lines( lt_serv_info ) = 1.
    READ TABLE lt_serv_info INDEX 1 INTO ls_serv_info.
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING ls_serv_info-service TO es_icfservice.
    IF iv_clear = abap_true.
      CLEAR es_icfservice-icf_cuser.
      CLEAR es_icfservice-icf_cdate.
      CLEAR es_icfservice-icf_muser.
      CLEAR es_icfservice-icf_mdate.
    ENDIF.

    CLEAR es_icfdocu-icfparguid.

    APPEND LINES OF ls_serv_info-handlertbl TO et_icfhandler.
    LOOP AT et_icfhandler ASSIGNING <ls_icfhandler>.
      CLEAR <ls_icfhandler>-icfparguid.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_otr.

    DATA:
      lt_sots     TYPE zcl_abapgit_sots_handler=>ty_sots_tt,
      lt_sots_use TYPE zcl_abapgit_sots_handler=>ty_sots_use_tt.

    FIELD-SYMBOLS:
      <ls_sots_use> LIKE LINE OF lt_sots_use.

    zcl_abapgit_sots_handler=>read_sots(
      EXPORTING
        iv_object   = ms_item-obj_type
        iv_obj_name = ms_item-obj_name
      IMPORTING
        et_sots     = lt_sots
        et_sots_use = lt_sots_use ).

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use>.
      CLEAR <ls_sots_use>-obj_name.
    ENDLOOP.

    io_xml->add( iv_name = 'SOTS'
                 ig_data = lt_sots ).
    io_xml->add( iv_name = 'SOTS_USE'
                 ig_data = lt_sots_use ).

  ENDMETHOD.


  METHOD to_icfhndlist.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF it_list.


    " Convert to sorted table
    LOOP AT it_list ASSIGNING <ls_list>.
      INSERT <ls_list>-icfhandler INTO TABLE rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_icfservice TYPE icfservice.


    read( EXPORTING iv_clear = abap_false
          IMPORTING es_icfservice = ls_icfservice ).

    rv_user = ls_icfservice-icf_muser.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA ls_icfservice TYPE icfservice.

    read( IMPORTING es_icfservice = ls_icfservice ).

    IF ls_icfservice IS INITIAL.
      " It seems that the ICF service doesn't exist anymore.
      " But that's ok, because some objects like SAPC manage
      " the lifecycle of its ICF service by itself and already
      " deleted the service.
      RETURN.
    ENDIF.

    IF ls_icfservice-icfparguid CO '0'.
      " not supported by the SAP standard API
      zcx_abapgit_exception=>raise( 'SICF - cannot delete root node, delete node manually' ).
    ENDIF.

    " OTR long texts
    zcl_abapgit_sots_handler=>delete_sots(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).

    " Delete Application Customizing Data the hard way, as it isn't done by the API.
    " If we wouldn't we would get errors from the API if entrys exist.
    " Transaction SICF does the same.
    DELETE FROM icfapplcust
      WHERE icf_name = ls_icfservice-icf_name
      AND icfparguid = ls_icfservice-icfparguid.

    cl_icf_tree=>if_icf_tree~delete_node(
      EXPORTING
        icfparguid                  = ls_icfservice-icfparguid
      CHANGING
        icf_name                    = ls_icfservice-icf_name
      EXCEPTIONS
        no_virtual_host_delete      = 1
        special_service_error       = 2
        enqueue_error               = 3
        node_not_existing           = 4
        node_has_childs             = 5
        node_is_aliased             = 6
        node_not_in_original_system = 7
        transport_error             = 8
        tadir_error                 = 9
        db_error                    = 10
        no_authority                = 11
        OTHERS                      = 12 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_read       TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lv_exists     TYPE abap_bool,
          lt_icfhandler TYPE TABLE OF icfhandler.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'ICFSERVICE'
                  CHANGING cg_data = ls_icfservice ).
    io_xml->read( EXPORTING iv_name = 'ICFDOCU'
                  CHANGING cg_data = ls_icfdocu ).
    io_xml->read( EXPORTING iv_name = 'ICFHANDLER_TABLE'
                  CHANGING cg_data = lt_icfhandler ).

    lv_exists = zif_abapgit_object~exists( ).
    IF lv_exists = abap_false.
      insert_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_url        = lv_url ).
    ELSE.
      read( IMPORTING es_icfservice = ls_read ).
      change_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_parent     = ls_read-icfparguid ).
    ENDIF.

    " OTR long texts
    deserialize_otr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA ls_key TYPE ty_sicf_key.

    SELECT SINGLE icfaltnme FROM icfservice INTO ls_key-icf_name
      WHERE icf_name = ms_item-obj_name(15)
      AND icfparguid = ms_item-obj_name+15.
    rv_bool = boolc( sy-subrc = 0 ).

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

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name(15).
    lv_argument+15(1) = '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESICFSER'
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'RSICFTREE'.
    ls_bcdata-dynpro   = '1000'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-dynpro   = space.
    ls_bcdata-dynbegin = space.
    ls_bcdata-fnam     = 'ICF_SERV'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=ONLI'.
    APPEND ls_bcdata TO lt_bcdata.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SICF'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.

    DATA:
      lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
      lv_hash     TYPE ty_hash,
      lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF lt_tadir.

    lv_obj_name = to_upper( iv_filename(15) ) && '%'.
    lv_hash     = iv_filename+15(25).

    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE pgmid = 'R3TR'
      AND object  = 'SICF'
      AND obj_name LIKE lv_obj_name
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.      "#EC CI_GENBUFF

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF get_hash_from_object( <ls_tadir>-obj_name ) = lv_hash.
        cs_item-obj_name = <ls_tadir>-obj_name.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.

    DATA:
      lv_rest     TYPE string,
      lv_old_name TYPE string.

    SPLIT cv_filename AT '.' INTO lv_old_name lv_rest.
    cv_filename = |{ cv_filename(15) }{ get_hash_from_object( is_item-obj_name ) }.{ lv_rest }|.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lt_icfhandler TYPE TABLE OF icfhandler.

    read( IMPORTING es_icfservice = ls_icfservice
                    es_icfdocu    = ls_icfdocu
                    et_icfhandler = lt_icfhandler
                    ev_url        = lv_url ).

    IF ls_icfservice IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_icfservice-icf_mandt.
    CLEAR ls_icfservice-icfnodguid.
    CLEAR ls_icfservice-icfparguid.
    CLEAR ls_icfservice-icfchildno.
    CLEAR ls_icfservice-icfaliasno.
    CLEAR ls_icfservice-icf_user.
    CLEAR ls_icfservice-icf_cclnt.
    CLEAR ls_icfservice-icf_mclnt.
    CLEAR ls_icfservice-icfaltnme_orig.
    CLEAR ls_icfservice-icfbitmap.

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'ICFSERVICE'
                 ig_data = ls_icfservice ).
    io_xml->add( iv_name = 'ICFDOCU'
                 ig_data = ls_icfdocu ).
    io_xml->add( iv_name = 'ICFHANDLER_TABLE'
                 ig_data = lt_icfhandler ).

    " OTR long texts
    serialize_otr( io_xml ).

  ENDMETHOD.
ENDCLASS.
