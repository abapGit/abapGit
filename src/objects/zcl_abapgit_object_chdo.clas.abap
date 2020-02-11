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
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_change_document,
             reports_generated TYPE SORTED TABLE OF tcdrp WITH UNIQUE KEY object reportname,
             objects           TYPE SORTED TABLE OF tcdob WITH UNIQUE KEY object tabname,
             objects_text      TYPE SORTED TABLE OF tcdobt WITH UNIQUE KEY spras object,
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

    DATA: ls_change_object TYPE ty_change_document,
          lt_object_clas   TYPE cdobjectclas_tab.

    io_xml->read( EXPORTING iv_name = 'CHDO'
                  CHANGING  cg_data = ls_change_object ).

    APPEND mv_object TO lt_object_clas.

    tadir_insert( iv_package ).

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

*    CLEAR: ls_bdcdata.
*    ls_bdcdata-fnam = 'BDC_OKCODE'.
*    ls_bdcdata-fval = 'TCDOB-OBJECT'.
*    APPEND ls_bdcdata TO lt_bdcdata.

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

    SELECT *
      FROM tcdrp
      INTO TABLE ls_change_object-reports_generated
      WHERE object = mv_object.

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


ENDCLASS.
