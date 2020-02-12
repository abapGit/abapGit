CLASS zcl_abapgit_object_chdo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.
    METHODS get_generic
      RETURNING
        VALUE(ro_generic) TYPE REF TO zcl_abapgit_objects_generic
      RAISING
        zcx_abapgit_exception.

    METHODS after_import
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_change_document,
             reports_generated TYPE SORTED TABLE OF tcdrps WITH UNIQUE KEY object reportname,
             objects           TYPE SORTED TABLE OF tcdobs WITH UNIQUE KEY object tabname,
             objects_text      TYPE SORTED TABLE OF tcdobts WITH UNIQUE KEY spras object,
           END OF ty_change_document,
           tt_change_document TYPE STANDARD TABLE OF ty_change_document.

    DATA: mv_object TYPE cdobjectcl.

ENDCLASS.



CLASS zcl_abapgit_object_chdo IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_object = is_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE updname INTO rv_user
      FROM tcdrp
      WHERE object = mv_object.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_msg TYPE symsgv.

    CALL FUNCTION 'CHDO_DELETE'
      EXPORTING
        iv_object        = mv_object
        iv_with_tadir    = abap_true
      EXCEPTIONS
        object_is_space  = 1
        object_not_found = 2
        other_error      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      lv_msg = mv_object.
      zcx_abapgit_exception=>raise_t100( iv_msgid = 'CD'
                                         iv_msgno = '869'
                                         iv_msgv1 = lv_msg ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_change_object TYPE ty_change_document.

    io_xml->read( EXPORTING iv_name = 'CHDO'
                  CHANGING  cg_data = ls_change_object ).

    DELETE FROM tcdobs  WHERE object = mv_object.
    DELETE FROM tcdobts WHERE object = mv_object.
    DELETE FROM tcdrps  WHERE object = mv_object.

    INSERT tcdobs  FROM TABLE ls_change_object-objects.
    INSERT tcdobts FROM TABLE ls_change_object-objects_text.
    INSERT tcdrps  FROM TABLE ls_change_object-reports_generated.

    after_import( ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    SELECT COUNT(*)
      FROM tcdrp
      WHERE object = mv_object.

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
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-program  = 'SAPMSCDO_NEW'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'TCDOB-OBJECT'.
    ls_bdcdata-fval = mv_object.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=DISP'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SCDO'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |ERROR: Jump { mv_object }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_change_object TYPE ty_change_document.
    FIELD-SYMBOLS: <ls_tcdrps> LIKE LINE OF ls_change_object-reports_generated.

    SELECT *
      FROM tcdrp
      INTO TABLE ls_change_object-reports_generated
      WHERE object = mv_object.
    LOOP AT ls_change_object-reports_generated ASSIGNING <ls_tcdrps>.
      " At import, when CHDO is generated date & time change, so always detects changes for this fields
      CLEAR: <ls_tcdrps>-datum, <ls_tcdrps>-uzeit.
    ENDLOOP.

    SELECT *
      FROM tcdob
      INTO TABLE ls_change_object-objects
      WHERE object  = mv_object.

    SELECT *
      FROM tcdobt
      INTO TABLE ls_change_object-objects_text
      WHERE object  = mv_object.

    io_xml->add( iv_name = 'CHDO'
                 ig_data = ls_change_object ).

  ENDMETHOD.


  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        is_item = ms_item.

  ENDMETHOD.


  METHOD after_import.

    CONSTANTS lc_logical_transport_object TYPE c LENGTH 1 VALUE 'L'.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.

    ls_cts_object_entry-pgmid    = seok_pgmid_r3tr.
    ls_cts_object_entry-object   = ms_item-obj_type.
    ls_cts_object_entry-obj_name = ms_item-obj_name.
    INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

    CALL FUNCTION 'AFTER_IMP_CHDO'
      EXPORTING
        iv_tarclient  = sy-mandt
        iv_is_upgrade = abap_false
      TABLES
        tt_e071       = lt_cts_object_entry
        tt_e071k      = lt_cts_key.

  ENDMETHOD.


ENDCLASS.
