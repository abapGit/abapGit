*&---------------------------------------------------------------------*
*& Include zabapgit_object_ucsa
*&---------------------------------------------------------------------*

CLASS lcl_object_ucsa DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

ENDCLASS.

CLASS lcl_object_ucsa IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_id TYPE uconservid.

    lv_id = ms_item-obj_name.

    TRY.
        DATA(lo_persistence) = cl_ucon_sa_db_persist=>if_ucon_sa_persist~get_instance(  lv_id ).

        lo_persistence->load( version  = 'A'
                              language = sy-langu ).

      CATCH cx_ucon_base.
        rv_bool = abap_false.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lv_id   TYPE uconservid,
          lx_root TYPE REF TO cx_root,
          lv_text TYPE string,
          sa      TYPE uconservascomplete.

    lv_id = ms_item-obj_name.

    TRY.
        DATA(lo_persistence) = cl_ucon_sa_db_persist=>if_ucon_sa_persist~get_instance( lv_id ).

        lo_persistence->load(
          EXPORTING
            version  = 'A'
            language = sy-langu
          IMPORTING
            sa       = sa ).

        CLEAR: sa-header-createdby,
               sa-header-createdon,
               sa-header-createdat,
               sa-header-changedby,
               sa-header-changedon,
               sa-header-changedat.

        io_xml->add( iv_name = 'UCSA'
                     ig_data = sa ).

      CATCH cx_ucon_base INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lv_id   TYPE uconservid,
          lx_root TYPE REF TO cx_root,
          lv_text TYPE string,
          sa      TYPE uconservascomplete.

    io_xml->read(
      EXPORTING
        iv_name = 'UCSA'
      CHANGING
        cg_data = sa ).

    lv_id = ms_item-obj_name.

    TRY.
        DATA(lo_persistence) = cl_ucon_sa_db_persist=>if_ucon_sa_persist~get_instance( lv_id ).

        lo_persistence->create( ).

        lo_persistence->save( sa      = sa
                              version = 'A' ).

        tadir_insert( iv_package ).

      CATCH cx_ucon_base INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

    DATA: lv_id   TYPE uconservid,
          lx_root TYPE REF TO cx_root,
          lv_text TYPE string,
          sa      TYPE uconservascomplete.

    lv_id = ms_item-obj_name.

    TRY.
        DATA(lo_persistence) = cl_ucon_sa_db_persist=>if_ucon_sa_persist~get_instance( lv_id ).

        lo_persistence->delete( version = 'A' ).

      CATCH cx_ucon_base INTO lx_root.
        lv_text = lx_root->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.


  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_TOOL_ACCESS' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

ENDCLASS.
