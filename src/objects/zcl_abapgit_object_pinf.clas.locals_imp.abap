CLASS lcl_package_interface_facade IMPLEMENTATION.

  METHOD constructor.

    mi_interface = ii_interface.

  ENDMETHOD.

  METHOD lif_package_interface_facade~get_elements.

    mi_interface->get_elements(
      IMPORTING
        e_elements     = rt_elements
      EXCEPTIONS
        object_invalid = 1
        intern_err     = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~set_elements_changeable.

    mi_interface->set_elements_changeable(
      EXPORTING
        i_changeable                = iv_changeable
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

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~save_elements.

    mi_interface->save_elements(
      EXCEPTIONS
        object_not_changeable = 1
        object_invalid        = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~get_all_attributes.

    mi_interface->get_all_attributes(
      IMPORTING
        e_package_interface_data = rs_package_interface_data
      EXCEPTIONS
        object_invalid           = 1
        OTHERS                   = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~set_changeable.

    mi_interface->set_changeable(
      EXPORTING
        i_changeable                 = iv_changeable
      EXCEPTIONS
        object_locked_by_other_user  = 1
        permission_failure           = 2
        object_already_changeable    = 3
        object_already_unlocked      = 4
        object_just_created          = 5
        object_deleted               = 6
        object_modified              = 7
        object_not_existing          = 8
        object_invalid               = 9
        unexpected_error             = 10
        OTHERS                       = 11 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~delete.

    mi_interface->delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        intern_err            = 4
        OTHERS                = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~save.

    mi_interface->save(
      EXCEPTIONS
        short_text_missing    = 1
        object_not_changeable = 2
        object_invalid        = 3
        cancelled_in_corr     = 4
        permission_failure    = 5
        unexpected_error      = 6
        intern_err            = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~remove_elements.

    mi_interface->remove_elements(
      EXPORTING
        i_elements            = it_elements
      EXCEPTIONS
        object_deleted        = 1
        object_invalid        = 2
        object_not_changeable = 3
        element_not_contained = 4
        intern_err            = 5
        OTHERS                = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~add_elements.

    DATA:
      lt_mismatched TYPE scomeldata,
      ls_mismatched LIKE LINE OF lt_mismatched.

    mi_interface->add_elements(
      EXPORTING
        i_elements_data        = it_elements_data
      IMPORTING
        e_mismatched_elem_data = lt_mismatched
      EXCEPTIONS
        object_invalid         = 1
        intern_err             = 2
        OTHERS                 = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_mismatched INTO ls_mismatched.
      zcx_abapgit_exception=>raise( |Object { ls_mismatched-elem_type } { ls_mismatched-elem_key } | &&
                                    |from different package { ls_mismatched-elem_pack }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_package_interface_facade~set_all_attributes.

    mi_interface->set_all_attributes(
      EXPORTING
        i_package_interface_data     = is_package_interface_data
        i_data_sign                  = is_data_sign
      EXCEPTIONS
        object_deleted               = 1
        object_not_changeable        = 2
        interface_not_empty          = 3
        acl_not_empty                = 4
        author_not_existing          = 5
        object_type_mismatch         = 6
        object_invalid               = 7
        OTHERS                       = 8 ).
* Downport: exception "logical_package_types_differ"
* does not exist in lower versions

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_package_interface_facade~get_changeable.

    mi_interface->get_changeable(
      IMPORTING
        e_changeable   = rv_changeable
      EXCEPTIONS
        object_invalid = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
