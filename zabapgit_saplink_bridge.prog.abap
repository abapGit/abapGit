**********************************************************************
* provide an adapter adapting to the saplink-method-signatures
* requires ZSAPLINK to be modified: Provide an additional public method delete!
CLASS lcl_saplink_adapter DEFINITION INHERITING FROM lcl_objects_super ABSTRACT.

  PUBLIC SECTION.

    INTERFACES lif_object.

    METHODS constructor
      IMPORTING
        iv_saplink_classname TYPE classname
        is_item              TYPE ty_item.

  PRIVATE SECTION.
    DATA mo_saplink TYPE REF TO zsaplink.
    DATA mv_saplink_classname TYPE classname.
ENDCLASS.

CLASS lcl_saplink_adapter IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_item = is_item ).

    mv_saplink_classname = iv_saplink_classname.

    TRY.
        CREATE OBJECT mo_saplink TYPE (iv_saplink_classname)
            EXPORTING
                name = CONV string( ms_item-obj_name ).
      CATCH cx_sy_create_object_error INTO DATA(lx_saplink_not_created).
        "leave mo_saplink_wrapper initial => check this in future calls.
    ENDTRY.

  ENDMETHOD.

  DEFINE check_valid_saplink.
    if mo_saplink is INITIAL.
      raise EXCEPTION type lcx_exception
        EXPORTING
          iv_text = |No valid saplink-implementation found - class { mv_saplink_classname } cannot be instantiated|.
    endif.
  END-OF-DEFINITION.

  METHOD lif_object~serialize.

    check_valid_saplink.

    TRY.
        DATA(ixmldoc) = mo_saplink->createixmldocfromobject( ).
      CATCH zcx_saplink INTO DATA(lx_saplink).
        RAISE EXCEPTION TYPE lcx_exception EXPORTING iv_text = lx_saplink->get_text( ).
    ENDTRY.

    mo_files->add_xml( NEW lcl_xml( iv_xml = zsaplink=>convertixmldoctostring( ixmldoc ) ) ).
  ENDMETHOD.

  METHOD lif_object~delete.

    check_valid_saplink.

    TRY.
        mo_saplink->delete( ).
      CATCH zcx_saplink INTO DATA(lx_saplink).
        RAISE EXCEPTION TYPE lcx_exception EXPORTING iv_text = lx_saplink->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    check_valid_saplink.

    DATA(ixmldoc) = zsaplink=>convertstringtoixmldoc( mo_files->read_xml( )->xml_render( ) ).

    TRY.
        mo_saplink->createobjectfromixmldoc(
          EXPORTING
            ixmldocument = ixmldoc    " IF_IXML_DOCUMENT
            devclass     = iv_package
            overwrite    = abap_true "Always overwrite seems to be paradigm in ABAPGit
        ).
      CATCH zcx_saplink INTO DATA(lx_saplink).
        RAISE EXCEPTION TYPE lcx_exception EXPORTING iv_text = lx_saplink->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~exists.

    check_valid_saplink.

    rv_bool = mo_saplink->checkexists( ).

  ENDMETHOD.

  METHOD lif_object~jump ##needed.

  ENDMETHOD.

ENDCLASS.
* End of SAPLink-Adapter
**********************************************************************