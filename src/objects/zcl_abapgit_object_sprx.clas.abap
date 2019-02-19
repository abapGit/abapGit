CLASS zcl_abapgit_object_sprx DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_proxy,
        data   TYPE string VALUE 'PROXY_DATA' ##NO_TEXT,
        header TYPE string VALUE 'PROXY_HEADER' ##NO_TEXT,
      END OF c_proxy .
    DATA mv_object TYPE sproxhdr-object .
    DATA mv_obj_name TYPE sproxhdr-obj_name .

    METHODS get_object_and_name
      EXPORTING
        !ev_object   TYPE sproxhdr-object
        !ev_obj_name TYPE sproxhdr-obj_name .
    METHODS delta_handling
      IMPORTING
        !io_xml          TYPE REF TO zcl_abapgit_xml_input
      EXPORTING
        !et_sproxhdr_new TYPE sprx_hdr_t
        !et_sproxdat_new TYPE sprx_dat_t
      RAISING
        zcx_abapgit_exception .
    METHODS check_sprx_tadir
      RAISING
        zcx_abapgit_exception .
    METHODS save
      IMPORTING
        !it_sproxhdr_new TYPE sprx_hdr_t
        !it_sproxdat_new TYPE sprx_dat_t .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SPRX IMPLEMENTATION.


  METHOD check_sprx_tadir.

    DATA: lt_abap_keys TYPE prx_abapobjects,
          ls_abap_key  LIKE LINE OF lt_abap_keys,
          lx_error     TYPE REF TO cx_proxy_gen_error.

    ls_abap_key-object   = mv_object.
    ls_abap_key-obj_name = mv_obj_name.
    APPEND ls_abap_key TO lt_abap_keys.

    TRY.
        cl_proxy_utils=>check_sprx_tadir(
            objects = lt_abap_keys
            repair  = abap_true ).

      CATCH cx_proxy_gen_error INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text     = |{ lx_error->get_text( ) }|
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    get_object_and_name(
      IMPORTING
        ev_object   = mv_object
        ev_obj_name = mv_obj_name ).

  ENDMETHOD.


  METHOD delta_handling.

    DATA: lo_proxy   TYPE REF TO cl_proxy,
          lt_delta   TYPE sprx_t_delta,
          ls_db_data TYPE sprx_db_data.

    "add Delta-Handling to avoid that single objects created without the dependent objects.
    "Thereby the dependent objects will be deleted
    TRY.
        lo_proxy = cl_proxy_fact=>load_by_abap_name(
                       object   = mv_object
                       obj_name = mv_obj_name ).


        lt_delta = lo_proxy->get_delta_all( ).

        ls_db_data = cl_proxy_db=>serialize(
                         proxy    = lo_proxy
                         inactive = abap_false
                         delta    = lt_delta ).

        et_sproxhdr_new = ls_db_data-sproxhdr.
        et_sproxdat_new = ls_db_data-sproxdat.

      CATCH cx_proxy_gen_error.
        "No delta for this object -> create

        io_xml->read(
          EXPORTING
            iv_name = c_proxy-header
          CHANGING
            cg_data = et_sproxhdr_new ).

        IF et_sproxhdr_new IS INITIAL.
          zcx_abapgit_exception=>raise( |SPRX - error deserialize: { ms_item-obj_name }| ).
        ENDIF.

        io_xml->read(
          EXPORTING
            iv_name = c_proxy-data
          CHANGING
            cg_data = et_sproxdat_new ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_object_and_name.

    ev_object   = ms_item-obj_name(4).
    ev_obj_name = ms_item-obj_name+4.

  ENDMETHOD.


  METHOD save.

    DATA:
      lt_sproxhdr_old  TYPE sprx_hdr_t,
      lt_sproxdat_old  TYPE sprx_dat_t,
      lt_sproxsvar_old TYPE sprx_svar_t,
      lt_sproxintf_old TYPE sprx_matchintf_t,
      lt_sproxsvar_new TYPE sprx_svar_t,
      lt_sproxintf_new TYPE sprx_matchintf_t.

    cl_proxy_data=>db_save(
        sproxhdr_old  = lt_sproxhdr_old
        sproxdat_old  = lt_sproxdat_old
        sproxsvar_old = lt_sproxsvar_old
        sproxintf_old = lt_sproxintf_old
        sproxhdr_new  = it_sproxhdr_new
        sproxdat_new  = it_sproxdat_new
        sproxsvar_new = lt_sproxsvar_new
        sproxintf_new = lt_sproxintf_new ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA lv_changed_by TYPE sproxhdr-changed_by.

    rv_user = c_user_unknown.

    SELECT SINGLE changed_by
           FROM sproxhdr
           INTO lv_changed_by
           WHERE object     = mv_object
           AND   obj_name   = mv_obj_name
           AND   inactive   = abap_false.

    IF  sy-subrc = 0
    AND lv_changed_by IS NOT INITIAL.
      rv_user = lv_changed_by.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_object      TYPE sproxhdr-object,
      lv_obj_name    TYPE sproxhdr-obj_name,
      lv_return_code TYPE i,
      lt_log         TYPE sprx_log_t.

    get_object_and_name(
      IMPORTING
        ev_object   = lv_object
        ev_obj_name = lv_obj_name ).

    cl_proxy_data=>delete_single_proxy(
       EXPORTING
         object           = lv_object
         obj_name         = lv_obj_name
       CHANGING
         c_return_code    = lv_return_code
         ct_log           = lt_log ).
    IF lv_return_code <> 0.
      zcx_abapgit_exception=>raise( 'SPRX: Error from DELETE_SINGLE_PROXY' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lt_sproxhdr_new TYPE sprx_hdr_t,
          lt_sproxdat_new TYPE sprx_dat_t.

    tadir_insert( iv_package ).

    delta_handling(
      EXPORTING
        io_xml = io_xml
      IMPORTING
        et_sproxhdr_new = lt_sproxhdr_new
        et_sproxdat_new = lt_sproxdat_new ).

    save(
      it_sproxhdr_new = lt_sproxhdr_new
      it_sproxdat_new = lt_sproxdat_new ).

    COMMIT WORK.

    check_sprx_tadir( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA:
      lv_status      TYPE prx_status,
      lv_status_text TYPE prx_status_t.

    cl_proxy_data=>db_get_status(
      EXPORTING
        object      = mv_object
        obj_name    = mv_obj_name
      IMPORTING
        status      = lv_status
        status_text = lv_status_text ).

    IF lv_status = if_proxy=>c_state_active.
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true. "dummy implementation
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      lo_proxy           TYPE REF TO cl_proxy,
      ls_sprx_db_data    TYPE sprx_db_data,
      lt_delta           TYPE sprx_t_delta,
      lx_proxy_gen_error TYPE REF TO cx_proxy_gen_error.

    FIELD-SYMBOLS:
      <ls_sproxheader> LIKE LINE OF ls_sprx_db_data-sproxhdr,
      <ls_sproxdat>    TYPE sprx_dat.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_sprx_db_data = cl_proxy_db=>load_by_abap_name(
      object   = mv_object
      obj_name = mv_obj_name ).

    DELETE ls_sprx_db_data-sproxhdr WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxdat WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxsvar WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxpck WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxintf WHERE object <> mv_object OR obj_name <> mv_obj_name.

    IF lines( ls_sprx_db_data-sproxhdr ) <> 1.
      zcx_abapgit_exception=>raise( |SPRX, no header found, { mv_object }, { mv_obj_name }| ).
    ENDIF.

    LOOP AT ls_sprx_db_data-sproxhdr ASSIGNING <ls_sproxheader>.

      CLEAR:
        <ls_sproxheader>-created_by,
        <ls_sproxheader>-created_on,
        <ls_sproxheader>-changed_by,
        <ls_sproxheader>-changed_on.

    ENDLOOP.

    LOOP AT ls_sprx_db_data-sproxdat ASSIGNING <ls_sproxdat>.

      CLEAR <ls_sproxdat>-warnings.

    ENDLOOP.

    io_xml->add(
        iv_name = c_proxy-header
        ig_data = ls_sprx_db_data-sproxhdr ).

    io_xml->add(
        iv_name = c_proxy-data
        ig_data = ls_sprx_db_data-sproxdat ).

  ENDMETHOD.
ENDCLASS.
