CLASS zcl_abapgit_object_idoc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.
    CLASS-METHODS clear_idoc_segement_fields CHANGING cg_structure TYPE any.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_idoc,
        attributes TYPE edi_iapi01,
        t_syntax   TYPE STANDARD TABLE OF edi_iapi02 WITH NON-UNIQUE DEFAULT KEY,
      END OF ty_idoc.

    DATA: mv_idoctyp TYPE edi_iapi00-idoctyp.

    CLASS-METHODS clear_idoc_segement_field
      IMPORTING iv_fieldname TYPE csequence
      CHANGING  cg_structure TYPE any.

    METHODS is_closed
      RETURNING
        VALUE(rv_closed) TYPE abap_bool.
ENDCLASS.



CLASS zcl_abapgit_object_idoc IMPLEMENTATION.


  METHOD clear_idoc_segement_field.

    FIELD-SYMBOLS <lg_any_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cg_structure TO <lg_any_field>.
    IF sy-subrc = 0.
      CLEAR <lg_any_field>.
    ENDIF.

  ENDMETHOD.


  METHOD clear_idoc_segement_fields.

    clear_idoc_segement_field( EXPORTING iv_fieldname = 'DEVC'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PLAST'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PWORK'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PRESP'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CREDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CRETIME'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LTIME'
                               CHANGING  cg_structure = cg_structure ).
  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_idoctyp = ms_item-obj_name.

  ENDMETHOD.


  METHOD is_closed.

    DATA ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.
    rv_closed = boolc( sy-subrc = 0 AND ls_idoc-attributes-closed = abap_true ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_attributes TYPE edi_iapi01.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_user = ls_attributes-plast.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'IDOCTYPE_DELETE'
      EXPORTING
        pi_idoctyp          = mv_idoctyp
      EXCEPTIONS
        object_not_found    = 1
        lock_error          = 2
        action_not_possible = 3
        transport_error     = 4
        db_error            = 5
        no_authority        = 6
        OTHERS              = 7.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_idoc       TYPE ty_idoc,
          lv_transport  TYPE trkorr,
          ls_edbas      TYPE edbas,
          ls_attributes TYPE edi_iapi05.

    io_xml->read(
      EXPORTING
        iv_name = 'IDOC'
      CHANGING
        cg_data = ls_idoc ).

    MOVE-CORRESPONDING ls_idoc-attributes TO ls_attributes.

    IF zif_abapgit_object~exists( ) = abap_false.
      CALL FUNCTION 'IDOCTYPE_CREATE'
        EXPORTING
          pi_idoctyp       = mv_idoctyp
          pi_devclass      = iv_package
          pi_attributes    = ls_attributes
        TABLES
          pt_syntax        = ls_idoc-t_syntax
        EXCEPTIONS
          object_not_found = 1
          object_exists    = 2
          syntax_error     = 3
          segment_error    = 4
          transport_error  = 5
          db_error         = 6
          no_authority     = 7
          OTHERS           = 8.
    ELSE.
      IF is_closed( ) = abap_true.
        CALL FUNCTION 'IDOCTYPE_UNCLOSE'
          EXPORTING
            pi_idoctyp          = mv_idoctyp
          EXCEPTIONS
            object_not_found    = 1
            action_not_possible = 2
            db_error            = 3
            no_authority        = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      CALL FUNCTION 'IDOCTYPE_UPDATE'
        EXPORTING
          pi_idoctyp       = mv_idoctyp
          pi_attributes    = ls_attributes
        TABLES
          pt_syntax        = ls_idoc-t_syntax
        EXCEPTIONS
          object_not_found = 1
          object_exists    = 2
          syntax_error     = 3
          segment_error    = 4
          transport_error  = 5
          db_error         = 6
          no_authority     = 7
          OTHERS           = 8.
    ENDIF.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_idoc-attributes-closed = abap_true.
      IF iv_transport IS NOT INITIAL.
        lv_transport = iv_transport.

        CALL FUNCTION 'IDOCTYPE_CLOSE'
          EXPORTING
            pi_idoctyp          = mv_idoctyp
          CHANGING
            pc_order            = lv_transport
          EXCEPTIONS
            object_not_found    = 1
            action_not_possible = 2
            db_error            = 3
            no_authority        = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      " IDOCTYPE_CLOSE saves current release but it should be same as in repo
      SELECT SINGLE * FROM edbas INTO ls_edbas WHERE idoctyp = mv_idoctyp.
      ls_edbas-released = ls_idoc-attributes-released.
      ls_edbas-applrel  = ls_idoc-attributes-applrel.
      ls_edbas-closed   = ls_idoc-attributes-closed.
      UPDATE edbas FROM ls_edbas.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error updating IDOC { mv_idoctyp }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'IDOCTYPE_EXISTENCE_CHECK'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        OTHERS           = 3.

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
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSED5'.
    <ls_bdcdata>-dynpro   = '0010'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-SELECT_ORG'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISP'.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'WE30'
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

    DATA: ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      TABLES
        pt_syntax        = ls_idoc-t_syntax
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    clear_idoc_segement_fields( CHANGING cg_structure = ls_idoc-attributes ).

    io_xml->add( iv_name = 'IDOC'
                 ig_data = ls_idoc ).

  ENDMETHOD.
ENDCLASS.
