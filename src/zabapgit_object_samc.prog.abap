*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_SAMC
*&---------------------------------------------------------------------*

CLASS lcl_object_samc DEFINITION INHERITING FROM lcl_objects_channel_super FINAL.

  PUBLIC SECTION.
    METHODS: lif_object~exists REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      get_persistence_cls_name REDEFINITION,
      get_appl_obj_cls_name REDEFINITION,
      get_data_structure_name REDEFINITION.

ENDCLASS.                    "lcl_object_samc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_samc IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_samc IMPLEMENTATION.

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir.

    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.                    "lif_object~exists

  METHOD get_appl_obj_cls_name.

    r_data_class_name = 'CL_AMC_APPLICATION_OBJ_DATA'.

  ENDMETHOD.

  METHOD get_data_structure_name.

    r_data_structure_name = 'AMC_APPLICATION_COMPLETE'.

  ENDMETHOD.

  METHOD get_persistence_cls_name.

    r_persistence_class_name = 'CL_AMC_APPLICATION_OBJ_PERS'.

  ENDMETHOD.

ENDCLASS.                    "lcl_object_samc IMPLEMENTATION
