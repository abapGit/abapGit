CLASS ltcl_abapgit_popups_mock DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_popups.

ENDCLASS.

CLASS ltcl_no_dependency_injection DEFINITION FOR TESTING
                              RISK LEVEL HARMLESS
                              DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      no_injection FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_dependency_inject DEFINITION FOR TESTING
                                  RISK LEVEL HARMLESS
                                  DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      simple_injection FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_abapgit_popups_mock IMPLEMENTATION.

  METHOD zif_abapgit_popups~branch_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~commit_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~create_branch_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_folder_logic.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_confirm.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_package.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_transp_branch.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_from_list.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_transports.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_transport_request.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_search_help.

  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_pr_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_select_tr_requests.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_select_wb_tc_tr_and_tsk.

  ENDMETHOD.

  METHOD zif_abapgit_popups~tag_list_popup.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_labels.

  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_code_insp_check_variant.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_no_dependency_injection IMPLEMENTATION.

  METHOD no_injection.

    DATA: li_popups      TYPE REF TO zif_abapgit_popups,
          lo_class_descr TYPE REF TO cl_abap_classdescr.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).

    lo_class_descr ?= cl_abap_classdescr=>describe_by_object_ref( li_popups ).

    cl_abap_unit_assert=>assert_equals(
      exp = '\CLASS=ZCL_ABAPGIT_POPUPS'
      act = lo_class_descr->absolute_name ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_simple_dependency_inject IMPLEMENTATION.

  METHOD setup.

    DATA: lo_popups_mock TYPE REF TO ltcl_abapgit_popups_mock.

    CREATE OBJECT lo_popups_mock.

    zcl_abapgit_ui_injector=>set_popups( lo_popups_mock ).

  ENDMETHOD.

  METHOD simple_injection.

    DATA: li_popups      TYPE REF TO zif_abapgit_popups,
          lo_class_descr TYPE REF TO cl_abap_classdescr.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).

    lo_class_descr ?= cl_abap_classdescr=>describe_by_object_ref( li_popups ).

    cl_abap_unit_assert=>assert_equals(
      exp = '\CLASS-POOL=ZCL_ABAPGIT_UI_INJECTOR\CLASS=LTCL_ABAPGIT_POPUPS_MOCK'
      act = lo_class_descr->absolute_name ).

  ENDMETHOD.

ENDCLASS.
