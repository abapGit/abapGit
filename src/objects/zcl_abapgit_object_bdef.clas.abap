CLASS zcl_abapgit_object_bdef DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          !is_item     TYPE zif_abapgit_definitions=>ty_item
          !iv_language TYPE spras.

  PRIVATE SECTION.
    ALIASES mo_files
      FOR zif_abapgit_object~mo_files.
    ALIASES changed_by
      FOR zif_abapgit_object~changed_by.
    ALIASES compare_to_remote_version
      FOR zif_abapgit_object~compare_to_remote_version.
    ALIASES delete
      FOR zif_abapgit_object~delete.
    ALIASES deserialize
      FOR zif_abapgit_object~deserialize.
    ALIASES exists
      FOR zif_abapgit_object~exists.
    ALIASES has_changed_since
      FOR zif_abapgit_object~has_changed_since.
    ALIASES is_locked
      FOR zif_abapgit_object~is_locked.
    ALIASES jump
      FOR zif_abapgit_object~jump.
    ALIASES serialize
      FOR zif_abapgit_object~serialize.
*    TYPES:
*      BEGIN OF ty_idoc,
*        attributes TYPE edi_iapi01,
*        t_syntax   TYPE STANDARD TABLE OF edi_iapi02 WITH NON-UNIQUE DEFAULT KEY,
*      END OF ty_idoc.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_BDEF IMPLEMENTATION.


    METHOD constructor.
      super->constructor( is_item = is_item
                          iv_language = iv_language ).
    ENDMETHOD.


    METHOD zif_abapgit_object~changed_by.
      DATA lx_root TYPE REF TO cx_root.

      DATA:
        lo_bdef            TYPE REF TO object,
        lr_data            TYPE REF TO data,
        lr_object_type_tmp TYPE REF TO data,
        lr_object_name_tmp TYPE REF TO data.

      FIELD-SYMBOLS:
        <ls_data>            TYPE any,
        <ls_object_type_tmp> TYPE any,
        <lv_objtype_tr>      TYPE any,
        <lv_subtype_wb>      TYPE any,
        <lv_object_name_tmp> TYPE any,
        <lg_struct>          TYPE any,
        <lg_field>           TYPE any.

      TRY.
          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_type_tmp TYPE ('WBOBJTYPE').
          ASSIGN lr_object_type_tmp->* TO <ls_object_type_tmp>.

          ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_object_type_tmp> TO <lv_objtype_tr>.
          <lv_objtype_tr> = ms_item-obj_type.
          ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_object_type_tmp> TO <lv_subtype_wb>.
          <lv_subtype_wb> = 'BDO'.

          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_name_tmp TYPE ('SEU_OBJKEY').
          ASSIGN lr_object_name_tmp->* TO <lv_object_name_tmp>.
          <lv_object_name_tmp> = ms_item-obj_name.

          CALL METHOD ('zcl_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
            EXPORTING
              object_type = <ls_object_type_tmp>
              object_key  = <lv_object_name_tmp>
            RECEIVING
              result      = lo_bdef.

          CREATE DATA lr_data TYPE ('zcl_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
          ASSIGN lr_data->* TO <ls_data>.

          CALL METHOD lo_bdef->('zif_WB_OBJECT_OPERATOR~READ')
            EXPORTING
              version        = 'A'
              data_selection = 'AL'
            IMPORTING
              data           = <ls_data>.

          ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_data> TO <lg_struct>.
          ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_struct> TO <lg_field>.
          if sy-subrc = 0.
          rv_user = <lg_field>.
          endif.

        CATCH cx_root INTO lx_root.
          zcx_abapgit_exception=>raise('BDEF read error').
      ENDTRY.
    ENDMETHOD.


    METHOD zif_abapgit_object~compare_to_remote_version.
      CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
    ENDMETHOD.


    METHOD zif_abapgit_object~delete.
      DATA lx_root TYPE REF TO cx_root.

      DATA:
        lo_bdef            TYPE REF TO object,
        lr_data            TYPE REF TO data,
        lr_object_type_tmp TYPE REF TO data,
        lr_object_name_tmp TYPE REF TO data.

      FIELD-SYMBOLS:
        <ls_data>            TYPE any,
        <ls_object_type_tmp> TYPE any,
        <lv_objtype_tr>      TYPE any,
        <lv_subtype_wb>      TYPE any,
        <lv_object_name_tmp> TYPE any.

      TRY.
          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_type_tmp TYPE ('WBOBJTYPE').
          ASSIGN lr_object_type_tmp->* TO <ls_object_type_tmp>.

          ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_object_type_tmp> TO <lv_objtype_tr>.
          <lv_objtype_tr> = ms_item-obj_type.
          ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_object_type_tmp> TO <lv_subtype_wb>.
          <lv_subtype_wb> = 'BDO'.

          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_name_tmp TYPE ('SEU_OBJKEY').
          ASSIGN lr_object_name_tmp->* TO <lv_object_name_tmp>.
          <lv_object_name_tmp> = ms_item-obj_name.

          CALL METHOD ('zcl_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
            EXPORTING
              object_type = <ls_object_type_tmp>
              object_key  = <lv_object_name_tmp>
            RECEIVING
              result      = lo_bdef.

          CALL METHOD lo_bdef->('zif_WB_OBJECT_OPERATOR~DELETE').

        CATCH cx_root INTO lx_root.
          zcx_abapgit_exception=>raise('BDEF deletion error').
      ENDTRY.
    ENDMETHOD.


    METHOD zif_abapgit_object~deserialize.

      DATA:
        lr_data                      TYPE REF TO data,
        lr_object_type_tmp           TYPE REF TO data,
        lr_object_name_tmp           TYPE REF TO data,
        bdef_object_data             TYPE REF TO if_wb_object_data_model,
        lx_wb_object_operation_error TYPE REF TO cx_wb_object_operation_error,
        bdef_data                    TYPE cl_blue_source_object_data=>ty_object_data.

      FIELD-SYMBOLS:
        <ls_data>            TYPE any,
        <ls_field>           TYPE any,
        <ls_object_type_tmp> TYPE any,
        <lv_objtype_tr>      TYPE any,
        <lv_subtype_wb>      TYPE any,
        <lv_object_name_tmp> TYPE any.

      TRY.
          CREATE DATA lr_data TYPE ('ZCL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
          ASSIGN lr_data->* TO <ls_data>.
          io_xml->read(
            EXPORTING iv_name = 'BDEF'
            CHANGING cg_data  = <ls_data>
          ).

*        zif zcl_cos_utilities=>is_sap_zcloud_platform( ).
*          ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE <ls_data> TO <ls_field>.
*          ASSERT sy-subrc = 0.
*          <ls_field> = zif_abapgit_object=>c_abap_version_sap_cp.
*        ENDzif.


          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_type_tmp TYPE ('WBOBJTYPE').
          ASSIGN lr_object_type_tmp->* TO <ls_object_type_tmp>.

          ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_object_type_tmp> TO <lv_objtype_tr>.
          <lv_objtype_tr> = ms_item-obj_type.
          ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_object_type_tmp> TO <lv_subtype_wb>.
          <lv_subtype_wb> = 'BDO'.

          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_name_tmp TYPE ('SEU_OBJKEY').
          ASSIGN lr_object_name_tmp->* TO <lv_object_name_tmp>.
          <lv_object_name_tmp> = ms_item-obj_name.

          DATA(obj_operator) =  cl_wb_object_operator_factory=>create_object_operator(
                               object_type = <ls_object_type_tmp>
                               object_key  = <lv_object_name_tmp> ).



          DATA(lv_tr_request) = zcl_abapgit_default_transport=>get_instance( )->get( ).
          bdef_data = <ls_data>.

          IF bdef_data-metadata-created_by IS INITIAL.
            bdef_data-metadata-created_by = sy-uname.
          ENDIF.

          IF bdef_data-metadata-created_at IS INITIAL.
            DATA lv_ts TYPE timestamp.
            cl_abap_tstmp=>systemtstmp_syst2utc(
              EXPORTING syst_date = sy-datlo
                        syst_time = sy-timlo
              IMPORTING utc_tstmp = lv_ts ).
            bdef_data-metadata-created_at = lv_ts.
          ENDIF.

          bdef_data-content-source = mo_files->read_string('asbdef').

          CREATE OBJECT bdef_object_data TYPE cl_blue_source_object_data.
          CALL METHOD bdef_object_data->set_data( EXPORTING p_data = bdef_data ).

          IF exists( ).
            obj_operator->update(
               io_object_data = bdef_object_data
               data_selection = if_wb_object_data_selection_co=>c_data_content
               version           = swbm_version_active
               transport_request = lv_tr_request-ordernum
            ).
          ELSE.
            obj_operator->create(
              io_object_data  = bdef_object_data
              data_selection    = if_wb_object_data_selection_co=>c_properties
              version           = swbm_version_inactive
              package           = iv_package
              transport_request = lv_tr_request-ordernum
            ).

            obj_operator->update(
              io_object_data  = bdef_object_data
              data_selection    = if_wb_object_data_selection_co=>c_data_content
              version           = swbm_version_inactive
              transport_request = lv_tr_request-ordernum
            ).
          ENDIF.

          obj_operator->activate( ).

        CATCH cx_wb_object_operation_error INTO lx_wb_object_operation_error.
          zcx_abapgit_exception=>raise('BDEF import error').
      ENDTRY.
    ENDMETHOD.


    METHOD zif_abapgit_object~exists.
      DATA lx_root TYPE REF TO cx_root.

      DATA:
        lo_bdef            TYPE REF TO object,
        lr_data            TYPE REF TO data,
        lr_object_type_tmp TYPE REF TO data,
        lr_object_name_tmp TYPE REF TO data.

      FIELD-SYMBOLS:
        <ls_data>            TYPE any,
        <ls_object_type_tmp> TYPE any,
        <lv_objtype_tr>      TYPE any,
        <lv_subtype_wb>      TYPE any,
        <lv_object_name_tmp> TYPE any.

      TRY.
          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_type_tmp TYPE ('WBOBJTYPE').
          ASSIGN lr_object_type_tmp->* TO <ls_object_type_tmp>.

          ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_object_type_tmp> TO <lv_objtype_tr>.
          <lv_objtype_tr> = ms_item-obj_type.
          ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_object_type_tmp> TO <lv_subtype_wb>.
          <lv_subtype_wb> = 'BDO'.

          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_name_tmp TYPE ('SEU_OBJKEY').
          ASSIGN lr_object_name_tmp->* TO <lv_object_name_tmp>.
          <lv_object_name_tmp> = ms_item-obj_name.

          CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
            EXPORTING
              object_type = <ls_object_type_tmp>
              object_key  = <lv_object_name_tmp>
            RECEIVING
              result      = lo_bdef.

          CREATE DATA lr_data TYPE ('zcl_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
          ASSIGN lr_data->* TO <ls_data>.

          CALL METHOD lo_bdef->('IF_WB_OBJECT_OPERATOR~READ')
            EXPORTING
              version        = 'A'
              data_selection = 'AL'
            IMPORTING
              data           = <ls_data>.

          if <ls_data> is assigned.
          rv_bool = abap_true.
        ELSE.
          rv_bool = abap_false.
          endif.

        CATCH cx_root INTO lx_root.
          rv_bool = abap_false.
      ENDTRY.
    ENDMETHOD.


    METHOD zif_abapgit_object~get_metadata.
      rs_metadata = get_metadata( ).
      rs_metadata-ddic = abap_true.
    ENDMETHOD.


    METHOD zif_abapgit_object~has_changed_since.
      rv_changed = abap_true.
    ENDMETHOD.


    METHOD zif_abapgit_object~is_locked.
      rv_is_locked = exists_a_lock_entry_for(
        iv_lock_object = 'ESDICT'
        iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }|
      ).
    ENDMETHOD.


    METHOD zif_abapgit_object~jump.
      TRY.
          jump_adt( i_obj_name = ms_item-obj_name
                    i_obj_type = ms_item-obj_type ).
        CATCH zcx_abapgit_exception.
          zcx_abapgit_exception=>raise('BDEF Jump Error').
      ENDTRY.
    ENDMETHOD.


    METHOD zif_abapgit_object~serialize.
      DATA lx_wb_object_operation_error TYPE REF TO cx_wb_object_operation_error.

      DATA:
        lr_data            TYPE REF TO data,
        lt_zclr_comps      TYPE STANDARD TABLE OF fieldname WITH DEFAULT KEY,
        lr_object_type_tmp TYPE REF TO data,
        lr_object_name_tmp TYPE REF TO data,
        lo_bdef            TYPE REF TO object,
        bdef_data          TYPE cl_blue_source_object_data=>ty_object_data.

      FIELD-SYMBOLS:
        <ls_data>            TYPE any,
        <ls_object_type_tmp> TYPE any,
        <lv_objtype_tr>      TYPE any,
        <lv_subtype_wb>      TYPE any,
        <lv_object_name_tmp> TYPE any,
        <ls_content>         TYPE any,
        <ls_source>          TYPE any,
        <ls_metadata>        TYPE any,
        <ls_comp>            LIKE LINE OF lt_zclr_comps,
        <ls_field>           TYPE any.

      TRY.
          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_type_tmp TYPE ('WBOBJTYPE').
          ASSIGN lr_object_type_tmp->* TO <ls_object_type_tmp>.

          ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_object_type_tmp> TO <lv_objtype_tr>.
          <lv_objtype_tr> = ms_item-obj_type.
          ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_object_type_tmp> TO <lv_subtype_wb>.
          <lv_subtype_wb> = 'BDO'.

          " Remapping to API data structure due to backward compatibility to NW 7.02
          CREATE DATA lr_object_name_tmp TYPE ('SEU_OBJKEY').
          ASSIGN lr_object_name_tmp->* TO <lv_object_name_tmp>.
          <lv_object_name_tmp> = ms_item-obj_name.

          CALL METHOD ('zcl_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
            EXPORTING
              object_type = <ls_object_type_tmp>
              object_key  = <lv_object_name_tmp>
            RECEIVING
              result      = lo_bdef.

          CREATE DATA lr_data TYPE ('zcl_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
          ASSIGN lr_data->* TO <ls_data>.

          CALL METHOD lo_bdef->('zif_WB_OBJECT_OPERATOR~READ')
            EXPORTING
              version        = 'A'
              data_selection = 'AL'
            IMPORTING
              data           = <ls_data>.

          " Annonymize user speczific information
          APPEND 'CHANGED_AT' TO lt_zclr_comps.
          APPEND 'CHANGED_BY' TO lt_zclr_comps.
          APPEND 'CREATED_AT' TO lt_zclr_comps.
          APPEND 'CREATED_BY' TO lt_zclr_comps.
          APPEND 'RESPONSIBLE' TO lt_zclr_comps.
          APPEND 'PACKAGE_REF' TO lt_zclr_comps.

          ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_data> TO <ls_metadata>.

          LOOP AT lt_zclr_comps ASSIGNING <ls_comp>.
            ASSIGN COMPONENT <ls_comp> OF STRUCTURE <ls_metadata> TO <ls_field>.
            if sy-subrc = 0.
            clear <ls_field>.
            endif.
          ENDLOOP.

          " Read the source code from the <ls_content> into <ls_source>.
          ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <ls_data> TO <ls_content>.
          ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_content> TO <ls_source>.
          ASSERT sy-subrc = 0.

          " Save the source code as file
          mo_files->add_string(
            iv_ext    = 'asbdef'
            iv_string = <ls_source>
          ) ##no_text.

          " zclear <ls_source>, because it contains the source and we have already
          " written the source code to the file.
          clear <ls_source>.

          " Add the 'MANzifEST' and 'CONTENT' part to the 'BDEF' xml object.
          io_xml->add(
            iv_name = 'BDEF'
            ig_data = <ls_data>
          ).

        CATCH cx_wb_object_operation_error INTO lx_wb_object_operation_error.
          zcx_abapgit_exception=>raise('Error serializing BDEF').
      ENDTRY.
    ENDMETHOD.
ENDCLASS.
