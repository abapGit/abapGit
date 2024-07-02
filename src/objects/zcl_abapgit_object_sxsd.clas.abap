CLASS zcl_abapgit_object_sxsd DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    METHODS constructor
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_language    TYPE spras
        io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_badi_attr TYPE sxc_attr.


ENDCLASS.



CLASS zcl_abapgit_object_sxsd IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    SELECT SINGLE * FROM sxc_attr INTO ms_badi_attr WHERE imp_name = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    IF ms_badi_attr-uname IS NOT INITIAL.
      rv_user = ms_badi_attr-uname.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    "Not allowed, SAP Object
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    " Not allowed, SAP Object
    " Can't create Z-BADIs since at least 7.0
    zcx_abapgit_exception=>raise_t100(
      iv_msgid = 'ENHANCEMENT'
      iv_msgno = '269' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.
    rv_bool = boolc( ms_badi_attr IS NOT INITIAL ).
  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    "Not allowed, SAP Object
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    "Serialize only, irrelevant
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA lv_exit_name TYPE rsexscrn-exit_name.

    lv_exit_name = ms_item-obj_name.

    CALL FUNCTION 'SXO_BADI_SHOW'
      EXPORTING
        exit_name         = lv_exit_name
      EXCEPTIONS
        action_canceled   = 1
        access_failure    = 2
        badi_not_exixting = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100(
        iv_msgid = sy-msgid
        iv_msgno = sy-msgno
        iv_msgv1 = sy-msgv1
        iv_msgv2 = sy-msgv2
        iv_msgv3 = sy-msgv3
        iv_msgv4 = sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    TYPES: BEGIN OF ty_badi_definition,
             badi               TYPE badi_data,
             mast_langu         TYPE sy-langu,
             ext_clname         TYPE seoclsname,
             fcodes             TYPE seex_fcode_table,
             cocos              TYPE seex_coco_table,
             intas              TYPE seex_table_table,
             scrns              TYPE seex_screen_table,
             methods            TYPE seex_mtd_table,
             inactive_tabstrips TYPE seex_inactive_tabstrips,
           END OF ty_badi_definition.

    DATA ls_badi_definition TYPE ty_badi_definition.
    DATA lv_exit_name TYPE rsexscrn-exit_name.

    lv_exit_name = ms_item-obj_name.

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name          = lv_exit_name
      IMPORTING
        badi               = ls_badi_definition-badi
        mast_langu         = ls_badi_definition-mast_langu
        ext_clname         = ls_badi_definition-ext_clname
      TABLES
        fcodes             = ls_badi_definition-fcodes
        cocos              = ls_badi_definition-cocos
        intas              = ls_badi_definition-intas
        scrns              = ls_badi_definition-scrns
        methods            = ls_badi_definition-methods
        inactive_tabstrips = ls_badi_definition-inactive_tabstrips
      EXCEPTIONS
        read_failure       = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Could not read definition for BAdI { lv_exit_name }| ).
    ENDIF.

    CLEAR: ls_badi_definition-badi-uname,
           ls_badi_definition-badi-udate,
           ls_badi_definition-badi-utime.

    io_xml->add(
      iv_name = 'SXSD'
      ig_data = ls_badi_definition ).

  ENDMETHOD.
ENDCLASS.
