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



CLASS ZCL_ABAPGIT_OBJECT_SCVI IMPLEMENTATION.


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
    ENDIF. " IF sy-subrc <> 0

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
    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_screen_variant TYPE ty_screen_variant.

    DATA: lv_text TYPE natxt.

    io_xml->read(
      EXPORTING
        iv_name = 'SCVI'
      CHANGING
        cg_data = ls_screen_variant ).

    CALL FUNCTION 'ENQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-shdsvci-scvariant
      EXCEPTIONS
        OTHERS    = 01.
    IF sy-subrc <> 0.
      MESSAGE e413(ms) WITH ls_screen_variant-shdsvci-scvariant INTO lv_text.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF. " IF sy-subrc <> 0

    corr_insert( iv_package = iv_package ).

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

    DATA: lo_screen_variant TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_screen_variant
      EXPORTING
        is_item = ms_item.

    rv_bool = lo_screen_variant->exists( ).

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

    rv_is_locked = abap_false. " ' '

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    zcx_abapgit_exception=>raise( |TODO: Jump| ).

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
    ENDIF. " IF sy-subrc <> 0

    SELECT *
    FROM shdsvtxci
    INTO TABLE ls_screen_variant-shdsvtxci[]
    WHERE scvariant = ls_screen_variant-shdsvci-scvariant.

    SELECT *
    FROM shdgxtcode
    INTO TABLE ls_screen_variant-shdgxtcode[]
    WHERE scvariant = ls_screen_variant-shdsvci-scvariant.

    io_xml->add( iv_name = 'SCVI'
                 ig_data = ls_screen_variant ).

  ENDMETHOD.
ENDCLASS.
