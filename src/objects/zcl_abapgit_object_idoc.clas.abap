CLASS zcl_abapgit_object_idoc DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_idoc,
        attributes TYPE edi_iapi01,
        t_syntax   TYPE STANDARD TABLE OF edi_iapi02 WITH NON-UNIQUE DEFAULT KEY,
      END OF ty_idoc.

    DATA:
      mv_idoctyp TYPE edi_iapi00-idoctyp.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_IDOC IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_idoctyp = ms_item-obj_name.

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


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
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
          ls_attributes TYPE edi_iapi05.

    io_xml->read(
      EXPORTING
        iv_name = 'IDOC'
      CHANGING
        cg_data = ls_idoc ).

    MOVE-CORRESPONDING ls_idoc-attributes TO ls_attributes.

    CALL FUNCTION 'IDOCTYPE_CREATE'
      EXPORTING
        pi_idoctyp          = mv_idoctyp
        pi_devclass         = iv_package
        pi_attributes       = ls_attributes
      TABLES
        pt_syntax           = ls_idoc-t_syntax
      EXCEPTIONS
        object_not_found    = 1
        object_exists       = 2
        action_not_possible = 3
        syntax_error        = 4
        segment_error       = 5
        transport_error     = 6
        db_error            = 7
        no_authority        = 8
        OTHERS              = 9.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
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

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'WE30'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

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

    CLEAR: ls_idoc-attributes-devc,
           ls_idoc-attributes-plast,
           ls_idoc-attributes-credate,
           ls_idoc-attributes-cretime,
           ls_idoc-attributes-ldate,
           ls_idoc-attributes-ltime.

    io_xml->add( iv_name = 'IDOC'
                 ig_data = ls_idoc ).

  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.

ENDCLASS.
