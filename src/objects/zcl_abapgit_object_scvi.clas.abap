CLASS zcl_abapgit_object_scvi DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_screen_variant,
        shdsvci    TYPE shdsvci,
        shdsvtxci  TYPE STANDARD TABLE OF shdsvtxci  WITH DEFAULT KEY,
        shdsvfvci  TYPE STANDARD TABLE OF shdsvfvci  WITH DEFAULT KEY,
        shdguixt   TYPE STANDARD TABLE OF shdguixt   WITH DEFAULT KEY,
        shdgxtcode TYPE STANDARD TABLE OF shdgxtcode WITH DEFAULT KEY,
      END OF ty_screen_variant .
ENDCLASS.



CLASS zcl_abapgit_object_scvi IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    SELECT SINGLE chuser
    FROM shdsvci
    INTO rv_user
    WHERE scvariant = lv_screen_variant.
    IF sy-subrc <> 0
    OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_DELETE_SC_VARIANT'
      EXPORTING
        scvariant        = lv_screen_variant
      EXCEPTIONS
        variant_enqueued = 1
        no_correction    = 2
        scvariant_used   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_screen_variant TYPE ty_screen_variant.

    io_xml->read(
      EXPORTING
        iv_name = 'SCVI'
      CHANGING
        cg_data = ls_screen_variant ).

    CALL FUNCTION 'ENQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-shdsvci-scvariant
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE e413(ms) WITH ls_screen_variant-shdsvci-scvariant INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package = iv_package ).

*   Populate user details
    ls_screen_variant-shdsvci-crdate = sy-datum.
    ls_screen_variant-shdsvci-cruser = sy-uname.
    ls_screen_variant-shdsvci-chdate = sy-datum.
    ls_screen_variant-shdsvci-chuser = sy-uname.

    MODIFY shdsvci    FROM ls_screen_variant-shdsvci.
    MODIFY shdsvtxci  FROM TABLE ls_screen_variant-shdsvtxci[].
    MODIFY shdsvfvci  FROM TABLE ls_screen_variant-shdsvfvci[].
    MODIFY shdguixt   FROM TABLE ls_screen_variant-shdguixt[].
    MODIFY shdgxtcode FROM TABLE ls_screen_variant-shdgxtcode[].

    CALL FUNCTION 'DEQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-shdsvci-scvariant.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_SC_VARIANT_DB'
      EXPORTING
        scvariant  = lv_screen_variant
      EXCEPTIONS
        no_variant = 1
        OTHERS     = 2.
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
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_screen_variant TYPE ty_screen_variant.

    ls_screen_variant-shdsvci-scvariant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_SC_VARIANT_DB'
      EXPORTING
        scvariant        = ls_screen_variant-shdsvci-scvariant
      IMPORTING
        header_scvariant = ls_screen_variant-shdsvci
      TABLES
        values_scvariant = ls_screen_variant-shdsvfvci[]
        guixt_scripts    = ls_screen_variant-shdguixt[]
      EXCEPTIONS
        no_variant       = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

*   Clear all user details
    CLEAR: ls_screen_variant-shdsvci-crdate,
           ls_screen_variant-shdsvci-cruser,
           ls_screen_variant-shdsvci-chdate,
           ls_screen_variant-shdsvci-chuser.

    SELECT *
    FROM shdsvtxci
    INTO TABLE ls_screen_variant-shdsvtxci[]
    WHERE scvariant = ls_screen_variant-shdsvci-scvariant
    ORDER BY PRIMARY KEY.

    SELECT *
    FROM shdgxtcode
    INTO TABLE ls_screen_variant-shdgxtcode[]
    WHERE scvariant = ls_screen_variant-shdsvci-scvariant
    ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'SCVI'
                 ig_data = ls_screen_variant ).

  ENDMETHOD.
ENDCLASS.
