*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_devc
*&---------------------------------------------------------------------*

CLASS lcl_object_devc DEFINITION
  INHERITING FROM lcl_objects_super
  FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_object.
    ALIASES:
      mo_files FOR lif_object~mo_files.
    METHODS:
      constructor IMPORTING is_item     TYPE lif_defs=>ty_item
                            iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_package IMPORTING iv_devclass       TYPE devclass
                  RETURNING VALUE(ri_package) TYPE REF TO if_package
                  RAISING   lcx_exception,
      update_pinf_usages IMPORTING ii_package    TYPE REF TO if_package
                                   it_usage_data TYPE scomppdata
                         RAISING   lcx_exception,
      set_lock IMPORTING ii_package TYPE REF TO if_package
                         iv_lock    TYPE abap_bool
               RAISING   lcx_exception.
    DATA:
      mv_local_devclass          TYPE devclass,
      mv_repo_devclass           TYPE devclass,
      mv_is_installation_package TYPE abap_bool.
ENDCLASS.

CLASS lcl_object_devc IMPLEMENTATION.
  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).
    mv_local_devclass = is_item-devclass. " This may be empty at this point
    mv_repo_devclass = is_item-obj_name.
  ENDMETHOD.

  METHOD get_package.
    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_devclass
        i_force_reload             = abap_true
      IMPORTING
        e_package                  = ri_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( |Error from CL_PACKAGE_FACTORY=>LOAD_PACKAGE { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~changed_by.
    rv_user = get_package( mv_local_devclass )->changed_by.
  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
  ENDMETHOD.

  METHOD lif_object~delete.
    DATA: li_package    TYPE REF TO if_package,
          lv_changeable TYPE abap_bool.

    RETURN ##TODO.

    " Package deletion is a bit tricky. A package can only be deleted if there are no objects
    " contained in it. This includes subpackages, so first the leaf packages need to be deleted.
    " Unfortunately deleted objects that are still contained in an unreleased transport request
    " also count towards the contained objects counter.

    li_package = get_package( mv_local_devclass ).
    li_package->get_changeable( IMPORTING e_changeable = lv_changeable ).

    IF lv_changeable = abap_false.
      li_package->set_changeable(
        EXPORTING
          i_changeable                = abap_true
        EXCEPTIONS
          object_locked_by_other_user = 1
          permission_failure          = 2
          object_already_changeable   = 3
          object_already_unlocked     = 4
          object_just_created         = 5
          object_deleted              = 6
          object_modified             = 7
          object_not_existing         = 8
          object_invalid              = 9
          unexpected_error            = 10
          OTHERS                      = 11 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( |Error from IF_PACKAGE->SET_CHANGEABLE { sy-subrc }| ).
      ENDIF.
    ENDIF.

    li_package->delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        intern_err            = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE->DELETE { sy-subrc }| ).
    ENDIF.

    li_package->save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE->SAVE { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~deserialize.
    DATA: li_package         TYPE REF TO if_package,
          ls_package_data    TYPE scompkdtln,
          ls_data_sign       TYPE scompksign,
          lv_changeable      TYPE abap_bool,
          lt_usage_data      TYPE scomppdata,
          lt_permissions     TYPE tpak_permission_to_use_list,
          li_usage           TYPE REF TO if_package_permission_to_use,
          ls_usage_data_sign TYPE scomppsign,
          ls_save_sign       TYPE paksavsign.
    FIELD-SYMBOLS: <ls_usage_data> TYPE scomppdtln.

    mv_local_devclass = iv_package.

    io_xml->read(
      EXPORTING
        iv_name = 'DEVC'
      CHANGING
        cg_data = ls_package_data ).

    ASSERT mv_repo_devclass = ls_package_data-devclass.

    " Is it the top level package?
    IF ls_package_data-parentcl IS INITIAL.
      mv_is_installation_package = abap_true.
*      " Check if the local installation package has a different name
*      IF mv_installation_package <> ls_package_data-devclass.
*        " The package is serialized under a different name -> change it
*        ls_package_data-devclass = mv_installation_package.
*        mv_local_devclass = mv_installation_package.
*      ENDIF.
    ENDIF.

    li_package = get_package( mv_local_devclass ).

    " Swap out repository package name with the local installation package name
    ls_package_data-devclass = mv_local_devclass.

    " Parent package is not changed. Assume the folder logic already created the package and set
    " the hierarchy before.
    CLEAR ls_package_data-parentcl.

    ls_data_sign-ctext            = abap_true.
*    ls_data_sign-korrflag         = abap_true.
    ls_data_sign-as4user          = abap_true.
    ls_data_sign-pdevclass        = abap_true.
*    ls_data_sign-dlvunit          = abap_true.
    ls_data_sign-comp_posid       = abap_true.
    ls_data_sign-component        = abap_true.
*    ls_data_sign-parentcl         = abap_true. " No parent package change here
    ls_data_sign-perminher        = abap_true.
    ls_data_sign-intfprefx        = abap_true.
    ls_data_sign-packtype         = abap_true.
*    ls_data_sign-restricted       = abap_true.
    ls_data_sign-mainpack         = abap_true.
    ls_data_sign-srv_check        = abap_true.
    ls_data_sign-cli_check        = abap_true.
    ls_data_sign-ext_alias        = abap_true.
    ls_data_sign-project_guid     = abap_true.
    ls_data_sign-project_id       = abap_true.
    ls_data_sign-project_passdown = abap_true.

    IF ls_package_data-ctext IS INITIAL.
      ls_package_data-ctext = mv_local_devclass.
    ENDIF.
    IF ls_package_data-dlvunit IS INITIAL.
      ls_package_data-dlvunit = 'HOME'.
    ENDIF.

    ls_package_data-as4user = cl_abap_syst=>get_user_name( ).

    IF li_package IS BOUND.
      " Package already exists, change it
      set_lock( ii_package = li_package iv_lock = abap_true ).

      li_package->set_all_attributes(
        EXPORTING
          i_package_data             = ls_package_data
          i_data_sign                = ls_data_sign
        EXCEPTIONS
          object_not_changeable      = 1
          object_deleted             = 2
          object_invalid             = 3
          short_text_missing         = 4
          author_not_existing        = 5
          local_package              = 6
          software_component_invalid = 7
          layer_invalid              = 8
          korrflag_invalid           = 9
          component_not_existing     = 10
          component_missing          = 11
          authorize_failure          = 12
          prefix_in_use              = 13
          unexpected_error           = 14
          intern_err                 = 15
          wrong_mainpack_value       = 16
          superpackage_invalid       = 17
          OTHERS                     = 18 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( |Error from IF_PACKAGE->SET_ALL_ATTRIBUTES { sy-subrc }| ).
      ENDIF.

*      " If the application component was cleared SET_ALL_ATTRIBUTES doesn't change it
*      IF ls_package_data-component IS INITIAL AND li_package->application_component IS NOT INITIAL.
*
*      ENDIF.

    ELSE.
      " Package does not exist yet, create it
      " This shouldn't really happen, because the folder logic initially creates the packages.
      cl_package_factory=>create_new_package(
        IMPORTING
          e_package                  = li_package
        CHANGING
          c_package_data             = ls_package_data
        EXCEPTIONS
          object_already_existing    = 1
          object_just_created        = 2
          not_authorized             = 3
          wrong_name_prefix          = 4
          undefined_name             = 5
          reserved_local_name        = 6
          invalid_package_name       = 7
          short_text_missing         = 8
          software_component_invalid = 9
          layer_invalid              = 10
          author_not_existing        = 11
          component_not_existing     = 12
          component_missing          = 13
          prefix_in_use              = 14
          unexpected_error           = 15
          intern_err                 = 16
          no_access                  = 17
          invalid_translation_depth  = 18
          wrong_mainpack_value       = 19
          superpackage_invalid       = 20
          error_in_cts_checks        = 21
          OTHERS                     = 22 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( |Error from CL_PACKAGE_FACTORY=>CREATE_NEW_PACKAGE { sy-subrc }| ).
      ENDIF.
    ENDIF.

    " Load package interface usages
    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PERMISSION'
          CHANGING
            cg_data = lt_usage_data ).
      CATCH lcx_exception ##NO_HANDLER.
        " No permissions saved
    ENDTRY.

    update_pinf_usages( ii_package    = li_package
                        it_usage_data = lt_usage_data ).

    ls_save_sign-pack = ls_save_sign-permis = ls_save_sign-elems = ls_save_sign-interf = abap_true.
    li_package->save_generic(
      EXPORTING
        i_save_sign           = ls_save_sign
      EXCEPTIONS
        cancelled_in_corr     = 1
        permission_failure    = 2
        object_not_changeable = 3
        object_invalid        = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE->SAVE_GENERIC { sy-subrc }| ).
    ENDIF.

    set_lock( ii_package = li_package iv_lock = abap_false ).
  ENDMETHOD.

  METHOD lif_object~exists.
    ASSERT mv_local_devclass IS NOT INITIAL.

    cl_package_helper=>check_package_existence(
      EXPORTING
        i_package_name          = mv_local_devclass
      IMPORTING
        e_package_exists        = rv_bool
      EXCEPTIONS
        intern_err              = 1
        package_hierarchy_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from CL_PACKAGE_HELPER=>CHECK_PACKAGE_EXISTENCE { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD lif_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from RS_TOOL_ACCESS, DEVC| ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~serialize.
    DATA: ls_package_data TYPE scompkdtln,
          li_package      TYPE REF TO if_package,
          lt_intf_usages  TYPE tpak_permission_to_use_list,
          lt_usage_data   TYPE scomppdata,
          ls_usage_data   TYPE scomppdtln,
          li_usage        TYPE REF TO if_package_permission_to_use.

    li_package = get_package( mv_local_devclass ).
    IF li_package IS NOT BOUND.
      lcx_exception=>raise( |Could not find package to serialize.| ).
    ENDIF.

    li_package->get_all_attributes(
      IMPORTING
        e_package_data  = ls_package_data
      EXCEPTIONS
        object_invalid  = 1
        package_deleted = 2
        intern_err      = 3
        OTHERS          = 4 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE->GET_ALL_ATTRIBUTES { sy-subrc }| ).
    ENDIF.

    ls_package_data-devclass = mv_repo_devclass.
*    IF mv_is_installation_package = abap_true.
    CLEAR ls_package_data-parentcl.
*    ENDIF.

    " Clear administrative data to prevent diffs
    CLEAR: ls_package_data-created_by,
           ls_package_data-created_on,
           ls_package_data-changed_by,
           ls_package_data-changed_on,
           ls_package_data-as4user.

    " Clear text descriptions that might be localized
    CLEAR: ls_package_data-comp_text,
           ls_package_data-dlvu_text.

    " Clear things related to local installation package
    CLEAR: ls_package_data-namespace,
           ls_package_data-dlvunit.

    CLEAR: ls_package_data-korrflag.

    io_xml->add( iv_name = 'DEVC' ig_data = ls_package_data ).

    " Save package interface usages
    li_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_intf_usages
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE->GET_PERMISSION_TO_USE { sy-subrc }| ).
    ENDIF.

    LOOP AT lt_intf_usages INTO li_usage.
      li_usage->get_all_attributes(
        IMPORTING
          e_permission_data = ls_usage_data
        EXCEPTIONS
          object_invalid    = 1
          intern_err        = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise(
          |Error from IF_PACKAGE_PERMISSION_TO_USE->GET_ALL_ATTRIBUTES { sy-subrc }| ).
      ENDIF.

      " Swap the package name
      ls_usage_data-client_pak = mv_repo_devclass.
      " The name of the package where the interface belongs to is also retrieved, but should not
      " be serialized because it may be a local package name and not the 'original' one. The
      " package interface name should be unique enough anyways.
      CLEAR ls_usage_data-pack_name.

      APPEND ls_usage_data TO lt_usage_data.
    ENDLOOP.

    IF lt_usage_data IS NOT INITIAL.
      io_xml->add( iv_name = 'PERMISSION' ig_data = lt_usage_data ).
    ENDIF.
  ENDMETHOD.

  METHOD update_pinf_usages.
    DATA: lt_current_permissions TYPE tpak_permission_to_use_list,
          li_usage               TYPE REF TO if_package_permission_to_use,
          ls_data_sign           TYPE scomppsign,
          ls_add_permission_data TYPE pkgpermdat,
          lt_handled             TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS: <ls_usage_data> LIKE LINE OF it_usage_data.

    " Get the current permissions
    ii_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_current_permissions
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE=>GET_PERMISSIONS_TO_USE { sy-subrc }| ).
    ENDIF.

    ls_data_sign-err_sever = abap_true.

    " New permissions
    LOOP AT it_usage_data ASSIGNING <ls_usage_data>.
      READ TABLE lt_current_permissions
           WITH KEY table_line->package_interface_name = <ls_usage_data>-intf_name
           INTO li_usage.

      IF sy-subrc = 0 AND li_usage IS BOUND.
        INSERT sy-tabix INTO TABLE lt_handled.

        " Permission already exists, update attributes
        li_usage->set_all_attributes(
          EXPORTING
            i_permission_data     = <ls_usage_data>
            i_data_sign           = ls_data_sign
          EXCEPTIONS
            object_not_changeable = 1
            object_invalid        = 2
            intern_err            = 3
            OTHERS                = 4 ).
        IF sy-subrc <> 0.
          lcx_exception=>raise(
            |Error from IF_PACKAGE_PERMISSION_TO_USE->SET_ALL_ATTRIBUTES { sy-subrc }| ).
        ENDIF.

      ELSE.
        " Permission does not exist yet, add it
        MOVE-CORRESPONDING <ls_usage_data> TO ls_add_permission_data.
        ii_package->add_permission_to_use(
          EXPORTING
            i_pkg_permission_data   = ls_add_permission_data
          EXCEPTIONS
            object_not_changeable   = 1
            object_access_error     = 2
            object_already_existing = 3
            object_invalid          = 4
            unexpected_error        = 5
            OTHERS                  = 6 ).
        IF sy-subrc <> 0.
          lcx_exception=>raise( |Error from IF_PACKAGE->ADD_PERMISSION_TO_USE { sy-subrc }| ).
        ENDIF.

      ENDIF.

      FREE li_usage.
    ENDLOOP.

    " Delete missing usages
    LOOP AT lt_current_permissions INTO li_usage.
      READ TABLE lt_handled WITH TABLE KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      li_usage->delete(
        EXCEPTIONS
          object_not_changeable = 1
          object_invalid        = 2
          deletion_not_allowed  = 3
          intern_err            = 4
          OTHERS                = 5 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( |Error from IF_PACKAGE->DELETE { sy-subrc }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_lock.
    DATA: lv_changeable TYPE abap_bool.

    ii_package->get_changeable( IMPORTING e_changeable = lv_changeable ).
    IF lv_changeable <> iv_lock.
      ii_package->set_changeable(
        EXPORTING
          i_changeable                = iv_lock
        EXCEPTIONS
          object_locked_by_other_user = 1
          permission_failure          = 2
          object_already_changeable   = 3
          object_already_unlocked     = 4
          object_just_created         = 5
          object_deleted              = 6
          object_modified             = 7
          object_not_existing         = 8
          object_invalid              = 9
          unexpected_error            = 10
          OTHERS                      = 11 ).
      IF sy-subrc <> 0.
        lcx_exception=>raise( |Error from IF_PACKAGE->SET_CHANGEABLE { sy-subrc }| ).
      ENDIF.
    ENDIF.

    ii_package->set_permissions_changeable(
      EXPORTING
        i_changeable                = iv_lock
        i_suppress_dialog           = abap_true
      EXCEPTIONS
        object_already_changeable   = 1
        object_already_unlocked     = 2
        object_locked_by_other_user = 3
        object_modified             = 4
        object_just_created         = 5
        object_deleted              = 6
        permission_failure          = 7
        object_invalid              = 8
        unexpected_error            = 9
        OTHERS                      = 10 ).
    IF ( sy-subrc = 1 AND iv_lock = abap_true ) OR ( sy-subrc = 2 AND iv_lock = abap_false ).
      " There's no getter to find out beforehand...
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( |Error from IF_PACKAGE->SET_PERMISSIONS_CHANGEABLE { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
