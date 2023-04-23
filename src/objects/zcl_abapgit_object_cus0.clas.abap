CLASS zcl_abapgit_object_cus0 DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_img_activity_texts TYPE STANDARD TABLE OF cus_imgact
                                       WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF ty_img_activity,
             header TYPE cus_imgach,
             texts  TYPE ty_img_activity_texts,
           END OF ty_img_activity.
    DATA: mv_img_activity TYPE cus_img_ac.

ENDCLASS.



CLASS zcl_abapgit_object_cus0 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_activity = ms_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_DELETE'
      EXPORTING
        img_activity = mv_img_activity
      IMPORTING
        message      = ls_message.

    IF ls_message-msgty <> 'S'.
      zcx_abapgit_exception=>raise( |error from delete CUS0 { mv_img_activity } S_CUS_IMG_ACTIVITY_DELETE| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_img_activity TYPE ty_img_activity,
          ls_text         LIKE LINE OF ls_img_activity-texts.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS0'
      CHANGING
        cg_data = ls_img_activity ).

    READ TABLE ls_img_activity-texts INTO ls_text
                                     WITH KEY spras = mv_language.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_SAVE'
      EXPORTING
        img_activity  = ls_img_activity-header-activity
        i_docu        = ls_img_activity-header-docu_id
        i_attributes  = ls_img_activity-header-attributes
        i_activity    = ls_img_activity-header-c_activity
        i_description = ls_text
        i_tcode       = ls_img_activity-header-tcode.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_EXISTS'
      EXPORTING
        img_activity = mv_img_activity
      IMPORTING
        message      = ls_message.

    rv_bool = boolc( ls_message IS INITIAL ).

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
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    DATA: lv_img_activity TYPE cus_img_ac.

    lv_img_activity = mv_img_activity.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_MAINTAIN'
      EXPORTING
        i_display    = abap_true
      CHANGING
        img_activity = lv_img_activity.

    rv_exit = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_img_activity TYPE ty_img_activity.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_READ'
      EXPORTING
        img_activity        = mv_img_activity
      IMPORTING
        img_activity_header = ls_img_activity-header
      TABLES
        img_activity_texts  = ls_img_activity-texts.

    CLEAR: ls_img_activity-header-fuser,
           ls_img_activity-header-fdate,
           ls_img_activity-header-ftime,
           ls_img_activity-header-luser,
           ls_img_activity-header-ldate,
           ls_img_activity-header-ltime.

    IF io_xml->i18n_params( )-main_language_only = abap_true.
      DELETE ls_img_activity-texts WHERE spras <> mv_language.
    ENDIF.

    SORT ls_img_activity-texts.

    io_xml->add( iv_name = 'CUS0'
                 ig_data = ls_img_activity ).

  ENDMETHOD.
ENDCLASS.
