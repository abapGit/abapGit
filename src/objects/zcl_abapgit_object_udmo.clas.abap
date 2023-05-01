CLASS zcl_abapgit_object_udmo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
  PROTECTED SECTION.

    METHODS corr_insert
        REDEFINITION .
  PRIVATE SECTION.

    TYPES:
        " You are reminded that the text serialisation / de-serialisation methods depend upon a common type.
        " To make the dependency explicit, there is one common definition.
      BEGIN OF ty_udmo_text_type.
    TYPES sprache TYPE dm40t-sprache.
    TYPES dmoid TYPE dm40t-dmoid.
    TYPES langbez TYPE dm40t-langbez.
    TYPES as4local TYPE dm40t-as4local.
    TYPES END OF ty_udmo_text_type .

    DATA mv_data_model TYPE uddmodl .
    DATA mv_text_object TYPE doku_obj .
    DATA mv_lxe_text_name TYPE lxeobjname .
    DATA mv_activation_state TYPE as4local .
    DATA ms_object_type TYPE rsdeo .
    CONSTANTS c_transport_object_class TYPE trobjtype VALUE 'SUDM' ##NO_TEXT.
    CONSTANTS c_lxe_text_type TYPE lxeobjtype VALUE 'IM' ##NO_TEXT.
    CONSTANTS c_correction_object_type TYPE rsdeo-objtype VALUE 'UDMO' ##NO_TEXT.
    CONSTANTS c_active_state TYPE as4local VALUE 'A' ##NO_TEXT.

    METHODS is_name_permitted
      RAISING
        zcx_abapgit_exception .
    METHODS update_tree .
    METHODS serialize_short_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_short_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_long_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_long_texts
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_entities
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_entities
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS access_modify
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS access_free
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_model
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_model
      IMPORTING
        !io_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_udmo IMPLEMENTATION.


  METHOD access_free.

    " Release the lock on the object.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode                     = 'FREE'
        object                   = ms_object_type
        object_class             = c_transport_object_class
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
        object                   = ms_object_type
        object_class             = c_transport_object_class
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


  METHOD constructor.

    super->constructor( is_item  =  is_item
                        iv_language = iv_language ).


    " Conversion to Data model
    mv_data_model = is_item-obj_name.
    " Default activation state is active
    mv_activation_state = c_active_state.
    " Derive the data model's text object
    mv_text_object = 'UDMD' && is_item-obj_name.
    " And set the text object to active
    mv_text_object+30(1) = mv_activation_state.
    mv_lxe_text_name = mv_text_object.

    " Correction and Transport System object
    ms_object_type-objtype = c_correction_object_type.
    ms_object_type-objname = is_item-obj_name.


  ENDMETHOD.


  METHOD corr_insert.

    DATA lv_obj_name TYPE tadir-obj_name.

    " You are reminded that SUDM - Data Model has no part objects e.g. no LIMU
    " Therefore global lock is always appropriate

    " You are reminded that the main language (in TADIR) is taken from MV_LANGUAGE.
    lv_obj_name = ms_object_type.

    zcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = c_transport_object_class
      iv_obj_name = lv_obj_name
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.


  METHOD deserialize_entities.

    DATA lt_udmo_entities TYPE STANDARD TABLE OF dm41s WITH DEFAULT KEY.
    DATA ls_udmo_entity LIKE LINE OF lt_udmo_entities.


    io_xml->read( EXPORTING iv_name = 'UDMO_ENTITIES'
                  CHANGING  cg_data = lt_udmo_entities ).

    LOOP AT lt_udmo_entities INTO ls_udmo_entity.

      CALL FUNCTION 'SDU_DMO_ENT_PUT'
        EXPORTING
          object = ls_udmo_entity
        EXCEPTIONS
          OTHERS = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_long_texts.

    DATA BEGIN OF ls_udmo_long_text.
    DATA language TYPE dm40t-sprache.
    DATA header   TYPE thead.
    DATA content TYPE xstring.
    DATA END OF ls_udmo_long_text.

    DATA lt_udmo_long_texts LIKE STANDARD TABLE OF ls_udmo_long_text.
    DATA ls_header TYPE thead.

    io_xml->read( EXPORTING iv_name = 'UDMO_LONG_TEXTS'
                  CHANGING  cg_data = lt_udmo_long_texts ).

    LOOP AT lt_udmo_long_texts INTO ls_udmo_long_text.

      ls_udmo_long_text-header-tdfuser = sy-uname.
      ls_udmo_long_text-header-tdfdate = sy-datum.
      ls_udmo_long_text-header-tdftime = sy-uzeit.

      " You are reminded that the target system may already have some texts in
      " existence. So we determine the highest existent version.

      CLEAR ls_header-tdversion.

      SELECT MAX( dokversion )
      INTO ls_header-tdversion
      FROM dokhl
      WHERE id = c_lxe_text_type
      AND   object = mv_text_object
      AND   langu = ls_udmo_long_text-language.

      " Increment the version
      ls_header-tdversion = ls_header-tdversion + 1.
      ls_udmo_long_text-header-tdversion = ls_header-tdversion.

      " This function module takes care of the variation in text processing between various objects.
      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          slang   = mv_language
          tlang   = ls_udmo_long_text-language
          objtype = c_lxe_text_type
          objname = mv_lxe_text_name
          header  = ls_udmo_long_text-header
          content = ls_udmo_long_text-content.


    ENDLOOP.


  ENDMETHOD.


  METHOD deserialize_model.

    DATA ls_dm40l TYPE dm40l.


    io_xml->read( EXPORTING iv_name = 'DM40L'
                  CHANGING cg_data = ls_dm40l ).


    " See SDU_MODEL_PUT
    GET TIME.

    ls_dm40l-flg_frame = abap_true.
    ls_dm40l-fstdate   = sy-datum.
    ls_dm40l-fsttime   = sy-uzeit.
    ls_dm40l-fstuser   = sy-uname.
    ls_dm40l-lstdate   = sy-datum.
    ls_dm40l-lsttime   = sy-uzeit.
    ls_dm40l-lstuser   = sy-uname.

    MODIFY dm40l FROM ls_dm40l.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SDU_MODEL_PUT' ).
    ENDIF.



  ENDMETHOD.


  METHOD deserialize_short_texts.

    DATA lt_udmo_texts TYPE STANDARD TABLE OF ty_udmo_text_type WITH DEFAULT KEY.
    DATA ls_udmo_text  TYPE ty_udmo_text_type.
    DATA ls_dm40t TYPE dm40t.


    " Deserialize the XML
    io_xml->read( EXPORTING iv_name = 'UDMO_TEXTS'
                  CHANGING  cg_data = lt_udmo_texts ).

    " For every text provided
    LOOP AT lt_udmo_texts INTO ls_udmo_text.

      " Does the text already exist? This is the same logic as used
      " in the FM SDU_MODEL_PUT
      SELECT SINGLE *
        FROM dm40t
        INTO ls_dm40t
        WHERE sprache  = ls_udmo_text-sprache
        AND   dmoid    = ls_udmo_text-dmoid
        AND   as4local = mv_activation_state.

      IF sy-subrc = 0.
        " There is already an active description for this language
        " but the provided description differs
        IF ls_dm40t-langbez <> ls_udmo_text-langbez.

          ls_dm40t-langbez = ls_udmo_text-langbez.
          ls_dm40t-lstdate = sy-datum.
          ls_dm40t-lsttime = sy-uzeit.
          ls_dm40t-lstuser = sy-uname.

          MODIFY dm40t FROM ls_dm40t.

        ENDIF.
      ELSE.

        " There is no EXISTING active description in this language

        ls_dm40t-as4local = ls_udmo_text-as4local.
        ls_dm40t-dmoid    = ls_udmo_text-dmoid.
        ls_dm40t-langbez  = ls_udmo_text-langbez.
        ls_dm40t-lstdate  = sy-datum.
        ls_dm40t-lsttime  = sy-uzeit.
        ls_dm40t-lstuser  = sy-uname.
        ls_dm40t-sprache  = ls_udmo_text-sprache.


        INSERT dm40t FROM ls_dm40t.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD is_name_permitted.

    " It is unlikely that a serialised data model will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the data model name.
    " So to be safe, we check. Tx SD11 does this check.


    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        obj_name   = ms_object_type-objname
        obj_type   = ms_object_type-objtype
      EXCEPTIONS
        wrong_type = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_entities.

    DATA lt_udmo_entities TYPE STANDARD TABLE OF dm41s WITH DEFAULT KEY.
    FIELD-SYMBOLS <ls_udmo_entity> TYPE dm41s.

    SELECT * FROM dm41s
      INTO TABLE lt_udmo_entities
      WHERE dmoid = mv_data_model
      AND as4local = mv_activation_state
      ORDER BY PRIMARY KEY.

    LOOP AT lt_udmo_entities ASSIGNING <ls_udmo_entity>.
      " You are reminded that administrative information, such as last changed by user, date, time is not serialised.
      CLEAR <ls_udmo_entity>-lstuser.
      CLEAR <ls_udmo_entity>-lstdate.
      CLEAR <ls_udmo_entity>-lsttime.
      CLEAR <ls_udmo_entity>-fstuser.
      CLEAR <ls_udmo_entity>-fstdate.
      CLEAR <ls_udmo_entity>-fsttime.
    ENDLOOP.

    " You are reminded that descriptions in other languages do not have to be in existence, although they may.
    IF lines( lt_udmo_entities ) > 0.
      io_xml->add( iv_name = 'UDMO_ENTITIES'
                   ig_data = lt_udmo_entities ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_long_texts.

    " The model has short texts in multiple languages. These are held in DM40T.

    " The model has a long description also in a main language, with other long descriptions
    " maintained as translations using SE63 Translation Editor. All of these long texts are held in DOK*

    TYPES BEGIN OF ty_language_type.
    TYPES language TYPE dm40t-sprache.
    TYPES END OF ty_language_type.

    DATA BEGIN OF ls_udmo_long_text.
    DATA language TYPE dm40t-sprache.
    DATA header   TYPE thead.
    DATA content TYPE xstring.
    DATA END OF ls_udmo_long_text.

    DATA lt_udmo_long_texts LIKE STANDARD TABLE OF ls_udmo_long_text.
    DATA lt_udmo_languages TYPE STANDARD TABLE OF ty_language_type.
    DATA ls_udmo_language  LIKE LINE OF lt_udmo_languages.
    DATA: lv_error_status  TYPE lxestatprc.


    " In which languages are the short texts are maintained.
    SELECT sprache AS language
      FROM dm40t
      INTO TABLE lt_udmo_languages
      WHERE dmoid    = mv_data_model
      AND   as4local = mv_activation_state
      ORDER BY sprache ASCENDING.                       "#EC CI_NOFIRST

    " For every language for which a short text is maintained,
    LOOP AT lt_udmo_languages INTO ls_udmo_language.

      CLEAR ls_udmo_long_text.
      CLEAR lv_error_status.

      ls_udmo_long_text-language = ls_udmo_language-language.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          lang    = ls_udmo_language-language
          objtype = c_lxe_text_type
          objname = mv_lxe_text_name
        IMPORTING
          header  = ls_udmo_long_text-header
          content = ls_udmo_long_text-content
          pstatus = lv_error_status.

      CHECK lv_error_status = 'S'. "Success

      " Administrative information is not serialised
      CLEAR ls_udmo_long_text-header-tdfuser.
      CLEAR ls_udmo_long_text-header-tdfdate.
      CLEAR ls_udmo_long_text-header-tdftime.

      CLEAR ls_udmo_long_text-header-tdluser.
      CLEAR ls_udmo_long_text-header-tdldate.
      CLEAR ls_udmo_long_text-header-tdltime.

      APPEND ls_udmo_long_text TO lt_udmo_long_texts.

    ENDLOOP.

    " You are reminded that long texts do not have to be in existence
    IF lines( lt_udmo_long_texts ) > 0.
      io_xml->add( iv_name = 'UDMO_LONG_TEXTS'
                   ig_data = lt_udmo_long_texts ).
    ENDIF.


  ENDMETHOD.


  METHOD serialize_model.

    DATA ls_dm40l TYPE dm40l.

    " See SDU_MODEL_GET.
    SELECT SINGLE *
    FROM dm40l
    INTO ls_dm40l
    WHERE dmoid    = mv_data_model
    AND   as4local = mv_activation_state.


    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from UDMO - model serialisation' ).
    ENDIF.

    " You are reminded that administrative data is not serialised.
    CLEAR ls_dm40l-lstdate.
    CLEAR ls_dm40l-lsttime.
    CLEAR ls_dm40l-lstuser.
    CLEAR ls_dm40l-fstdate.
    CLEAR ls_dm40l-fsttime.
    CLEAR ls_dm40l-fstuser.

    io_xml->add( iv_name = 'DM40L'
                 ig_data = ls_dm40l ).

  ENDMETHOD.


  METHOD serialize_short_texts.

    DATA lt_udmo_texts TYPE STANDARD TABLE OF ty_udmo_text_type WITH DEFAULT KEY.
    " You are reminded that administrative information, such as last changed by user, date, time is not serialised.

    " You are reminded that active short texts of all (existent) languages are serialised.

    SELECT sprache dmoid as4local langbez
      FROM dm40t
      INTO CORRESPONDING FIELDS OF TABLE lt_udmo_texts
      WHERE dmoid    = mv_data_model
      AND   as4local = mv_activation_state
      ORDER BY sprache ASCENDING.                       "#EC CI_NOFIRST

    " You are reminded that descriptions in other languages do not have to be in existence.
    IF lines( lt_udmo_texts ) > 0.
      io_xml->add( iv_name = 'UDMO_TEXTS'
                   ig_data = lt_udmo_texts ).
    ENDIF.


  ENDMETHOD.


  METHOD update_tree.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = mv_data_model
        operation = 'INSERT'
        type      = c_correction_object_type.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lstuser INTO rv_user
      FROM dm40l
      WHERE dmoid = mv_data_model
      AND as4local = mv_activation_state.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

* You are reminded that this function model checks for
*  - permissions
*  - locks
*  - connection to transport and correction system
*  - deletion of data model, model relations and all documentation
*  - update of object tree
*  - releasing of lock

    CALL FUNCTION 'RPY_DATAMODEL_DELETE'
      EXPORTING
        model_name       = mv_data_model
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        is_used          = 4
        OTHERS           = 5.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* You are reminded that this method checks for
*  - validity of data model name with regard to naming conventions
*  - permissions and locks
*  - connection to transport and correction system
*  - insert of data model, model relations and all documentation
*  - update of object tree
*  - releasing of lock


* Is the data model name compliant with naming conventions?
    is_name_permitted( ).

* Access Permission granted?
    access_modify( ).

* Connection to transport and correction system
    corr_insert( iv_package ).

* Insert the data model, relations and documentation
    TRY.
        deserialize_model( io_xml ).
        deserialize_entities( io_xml ).
        deserialize_short_texts( io_xml ).
        deserialize_long_texts( io_xml ).
        update_tree( ).
        access_free( ).

      CATCH zcx_abapgit_exception.

        access_free( ).

        zcx_abapgit_exception=>raise( 'Error in deserialisation of UDMO' ).


    ENDTRY.

    " You are reminded that data models are not relevant for activation.


  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    "  See Function Module SDU_MODEL_EXISTS

    SELECT COUNT( * ) FROM  dm40l
           WHERE  dmoid     = mv_data_model
           AND    as4local  = mv_activation_state.

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
    <ls_bdcdata>-fnam = 'RSUD3-DATM'.
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

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_model( io_xml ).
    serialize_entities( io_xml ).
    serialize_short_texts( io_xml ).
    serialize_long_texts( io_xml ).

  ENDMETHOD.
ENDCLASS.
