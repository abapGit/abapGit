CLASS lcl_replacement_mapping DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES lif_replacement_mapping.

ENDCLASS.


CLASS lcl_replacement_mapping IMPLEMENTATION.

  METHOD lif_replacement_mapping~to_entity.

    DATA lv_entityname TYPE ddobjname.

    TRY.
        CALL METHOD ('CL_SBD_DDLS_UTILITY')=>('MAP_TO_REPLACEMENT_DDLS')
          EXPORTING
            i_view_name  = iv_view_name
          IMPORTING
            e_entityname = lv_entityname.
        rv_entityname = lv_entityname.
      CATCH cx_root.
        " The utility is not available on older releases and is also absent
        " from the open-abap test runtime. In that case no annotation is
        " emitted rather than serializing DD02V-VIEWREF with the wrong
        " meaning.
        CLEAR rv_entityname.
    ENDTRY.

  ENDMETHOD.


  METHOD lif_replacement_mapping~to_view.

    DATA lv_view_name TYPE ddobjname.

    TRY.
        CALL METHOD ('CL_SBD_DDLS_UTILITY')=>('MAP_TO_REPLACEMENT_VIEW')
          EXPORTING
            i_entityname = iv_entityname
          IMPORTING
            e_view_name  = lv_view_name.
        rv_view_name = lv_view_name.
      CATCH cx_root.
        " Keep source-only parsing usable on releases without the SAP
        " utility. A SAP system with the utility returns the resolved view
        " name, or initial for an entity that cannot be resolved.
        rv_view_name = iv_entityname.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
