CLASS zcl_abapgit_repo_pre_filter DEFINITION
  PUBLIC
  CREATE PUBLIC.


  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_pre_filter.

  PROTECTED SECTION.
    METHODS adjust_local_filter
      IMPORTING
                it_e071_filter   TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter_tt
                iv_package       TYPE tadir-devclass
      RETURNING VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
                zcx_abapgit_exception.

  PRIVATE SECTION.

    DATA mt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt .
    DATA mt_r_trkorr TYPE zif_abapgit_repo_pre_filter=>ty_trrngtrkor_tt .
    DATA mt_r_file_filter TYPE zif_abapgit_repo_pre_filter=>ty_file_filter_tt .
    DATA mv_package TYPE tadir-devclass.

    METHODS generate_local_filter
      IMPORTING
        iv_package       TYPE tadir-devclass
        it_r_trkorr      TYPE zif_abapgit_repo_pre_filter=>ty_trrngtrkor_tt
      RETURNING
        VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .

    METHODS generate_file_filter
      IMPORTING
        it_filter               TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_r_file_filter) TYPE zif_abapgit_repo_pre_filter=>ty_file_filter_tt
      RAISING
        zcx_abapgit_exception .

    METHODS init .

    METHODS get_all_sub_packages
      IMPORTING
        iv_package       TYPE tadir-devclass
      RETURNING
        VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS zcl_abapgit_repo_pre_filter IMPLEMENTATION.

  METHOD zif_abapgit_repo_pre_filter~filter_files.
    IF mt_r_file_filter IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ct_files WHERE filename NOT IN mt_r_file_filter.
  ENDMETHOD.

  METHOD generate_file_filter.
    DATA lr_filter TYPE REF TO zif_abapgit_definitions=>ty_tadir.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_pattern TYPE string.
    DATA ls_r_file_filter TYPE zif_abapgit_repo_pre_filter=>ty_file_filter.
    DATA lo_obj_files TYPE REF TO zcl_abapgit_objects_files.

    CLEAR rt_r_file_filter.

    IF it_filter IS INITIAL.
      RETURN.
    ENDIF.

    ls_r_file_filter-sign = 'I'.
    ls_r_file_filter-option = 'EQ'.
    ls_r_file_filter-low = zif_abapgit_definitions=>c_dot_abapgit.
    INSERT ls_r_file_filter INTO TABLE rt_r_file_filter.

    LOOP AT it_filter REFERENCE INTO lr_filter.
      CLEAR ls_item.
      CLEAR ls_r_file_filter.
      ls_item-obj_type = lr_filter->object.
      ls_item-obj_name = lr_filter->obj_name.


      CREATE OBJECT lo_obj_files
        EXPORTING
          is_item = ls_item.

      lv_pattern = lo_obj_files->get_file_pattern( ).

      ls_r_file_filter-sign = 'I'.
      ls_r_file_filter-option = 'CP'.
      ls_r_file_filter-low = lv_pattern.
      INSERT ls_r_file_filter INTO TABLE rt_r_file_filter.
    ENDLOOP.

  ENDMETHOD.


  METHOD generate_local_filter.
    DATA lt_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter_tt.

    SELECT DISTINCT pgmid
                object
                obj_name
           INTO CORRESPONDING FIELDS OF TABLE lt_e071_filter
           FROM e071
      WHERE trkorr IN it_r_trkorr.
    IF sy-subrc <> 0.
      CLEAR lt_e071_filter.
    ENDIF.
    rt_filter = adjust_local_filter(
      iv_package     = iv_package
      it_e071_filter = lt_e071_filter ).
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~get_filter_values.
    et_r_trkorr = mt_r_trkorr.
    ev_package = mv_package.
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~get_local_filter.
    rt_filter = mt_filter.
  ENDMETHOD.

  METHOD init.
    CLEAR mt_filter.
    CLEAR mt_r_file_filter.
    CLEAR mt_r_trkorr.
    CLEAR mv_package.
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~set_filter_values.
    init( ).
    mt_r_trkorr = it_r_trkorr.
    mv_package = iv_package.
    IF it_r_trkorr IS NOT INITIAL.
      mt_filter = generate_local_filter(
        iv_package  = mv_package
        it_r_trkorr = mt_r_trkorr ).
      mt_r_file_filter = generate_file_filter( mt_filter ).
    ENDIF.
  ENDMETHOD.

  METHOD adjust_local_filter.

    DATA lt_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter_tt.
    DATA lr_e071_filter TYPE REF TO zif_abapgit_repo_pre_filter=>ty_e071_filter.
    DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lv_trobj_name_new TYPE trobj_name.
    DATA lv_trobj_type_new TYPE tadir-object.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_cts_api TYPE REF TO zif_abapgit_cts_api.

    lt_e071_filter = it_e071_filter.

    LOOP AT lt_e071_filter REFERENCE INTO lr_e071_filter.

      IF lr_e071_filter->pgmid = 'LIMU'.
        "Get Main Object from LIMU Object (Example the Class (R3TR) of a Method (LIMU))

        lr_cts_api = zcl_abapgit_factory=>get_cts_api( ).

        TRY.
            lr_cts_api->get_r3tr_obj_for_limu_obj(
              EXPORTING
                iv_object   = lr_e071_filter->object
                iv_obj_name = lr_e071_filter->obj_name
              IMPORTING
                ev_object   = lv_trobj_type_new
                ev_obj_name = lv_trobj_name_new ).
          CATCH zcx_abapgit_exception.
            CONTINUE.
        ENDTRY.

        CLEAR ls_filter.
        ls_filter-pgmid = 'R3TR'.
        ls_filter-object = lv_trobj_type_new.
        ls_filter-obj_name = lv_trobj_name_new.
      ELSE.
        ls_filter-pgmid = lr_e071_filter->pgmid.
        ls_filter-object = lr_e071_filter->object.
        ls_filter-obj_name = lr_e071_filter->obj_name.
      ENDIF.
      INSERT ls_filter INTO TABLE rt_filter.
    ENDLOOP.

    IF iv_package IS NOT INITIAL.
      ls_filter-pgmid = 'R3TR'.
      ls_filter-object = 'DEVC'.
      ls_filter-obj_name = iv_package.
      INSERT ls_filter INTO TABLE rt_filter.

      lt_filter = get_all_sub_packages( iv_package ).
      INSERT LINES OF lt_filter INTO TABLE rt_filter.

    ENDIF.

    SORT rt_filter.
    DELETE ADJACENT DUPLICATES FROM rt_filter.

    IF rt_filter IS INITIAL.

      zcx_abapgit_exception=>raise( 'No objects found for transport filter' ).

    ENDIF.

  ENDMETHOD.

  METHOD get_all_sub_packages.

    DATA li_package TYPE REF TO zif_abapgit_sap_package.
    DATA lt_list TYPE  zif_abapgit_sap_package=>ty_devclass_tt.
    DATA lr_list TYPE REF TO devclass.
    DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.

    li_package = zcl_abapgit_factory=>get_sap_package( iv_package = iv_package ).
    lt_list = li_package->list_subpackages( ).
    LOOP AT lt_list REFERENCE INTO lr_list.
      ls_filter-pgmid = 'R3TR'.
      ls_filter-object = 'DEVC'.
      ls_filter-obj_name = lr_list->*.
      INSERT ls_filter INTO TABLE rt_filter.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
