CLASS zcl_abapgit_object_enho_hook DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.

    INTERFACES: zif_abapgit_object_enho.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_spaces,
             full_name TYPE string,
             spaces    TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
           END OF ty_spaces.

    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF ty_spaces WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_file,
             name TYPE string,
             file TYPE string,
           END OF ty_file.

    TYPES: ty_files TYPE HASHED TABLE OF ty_file WITH UNIQUE KEY name.

    CONSTANTS c_enhancement TYPE string VALUE 'ENHANCEMENT 0 *.' ##NO_TEXT.
    CONSTANTS c_endenhancement TYPE string VALUE 'ENDENHANCEMENT.' ##NO_TEXT.

    DATA: ms_item TYPE zif_abapgit_definitions=>ty_item.
    DATA: mo_files TYPE REF TO zcl_abapgit_objects_files.

    METHODS add_sources
      CHANGING
        !ct_enhancements TYPE enh_hook_impl_it
        !ct_files        TYPE ty_files
      RAISING
        zcx_abapgit_exception .
    METHODS read_sources
      CHANGING
        !ct_enhancements TYPE enh_hook_impl_it
        !ct_files        TYPE ty_files
      RAISING
        zcx_abapgit_exception .
    METHODS hook_impl_deserialize
      IMPORTING
        !it_spaces TYPE ty_spaces_tt
      CHANGING
        !ct_impl   TYPE enh_hook_impl_it
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_enho_hook IMPLEMENTATION.


  METHOD add_sources.

    DATA lv_source TYPE string.
    DATA ls_file LIKE LINE OF ct_files.

    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.

    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      " Use hash as filename since full_name is very long
      CLEAR ls_file.
      ls_file-name = <ls_enhancement>-full_name.
      ls_file-file = substring(
        val = zcl_abapgit_hash=>sha1_string( <ls_enhancement>-full_name )
        len = 8 ).
      INSERT ls_file INTO TABLE ct_files.

      " Add full name as comment and put code between enhancement statements
      lv_source = c_enhancement.
      REPLACE '*' IN lv_source WITH ms_item-obj_name.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

      lv_source = |"Name: { <ls_enhancement>-full_name }|.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

      APPEND c_endenhancement TO <ls_enhancement>-source.

      mo_files->add_abap( iv_extra = ls_file-file
                          it_abap  = <ls_enhancement>-source ).

      CLEAR <ls_enhancement>-source.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.


  METHOD hook_impl_deserialize.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF ct_impl,
                   <lv_line>   TYPE string,
                   <lv_space>  TYPE i,
                   <ls_spaces> LIKE LINE OF it_spaces.


    LOOP AT ct_impl ASSIGNING <ls_impl>.
      READ TABLE it_spaces ASSIGNING <ls_spaces> WITH KEY full_name = <ls_impl>-full_name.
      IF sy-subrc = 0.
        LOOP AT <ls_impl>-source ASSIGNING <lv_line>.
          READ TABLE <ls_spaces>-spaces ASSIGNING <lv_space> INDEX sy-tabix.
          IF sy-subrc = 0 AND <lv_space> > 0.
            DO <lv_space> TIMES.
              CONCATENATE space <lv_line> INTO <lv_line> RESPECTING BLANKS.
            ENDDO.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_sources.

    DATA lv_source TYPE string.
    DATA ls_file LIKE LINE OF ct_files.
    DATA lv_from TYPE i.
    DATA lv_to TYPE i.

    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.

    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      READ TABLE ct_files INTO ls_file WITH TABLE KEY name = <ls_enhancement>-full_name.
      IF sy-subrc = 0.
        <ls_enhancement>-source = mo_files->read_abap( iv_extra = ls_file-file ).
        " Get code between enhancement statements
        LOOP AT <ls_enhancement>-source INTO lv_source.
          IF lv_source CP c_enhancement.
            lv_from = sy-tabix.
          ENDIF.
          IF lv_source CP c_endenhancement.
            lv_to = sy-tabix.
          ENDIF.
        ENDLOOP.
        DELETE <ls_enhancement>-source FROM lv_to.
        DELETE <ls_enhancement>-source TO lv_from.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_files           TYPE ty_files,
          lt_enhancements    TYPE enh_hook_impl_it,
          lx_enh_root        TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_original_object ).
    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data  = lt_enhancements ).
    ii_xml->read( EXPORTING iv_name = 'FILES'
                  CHANGING cg_data  = lt_files ).
    ii_xml->read( EXPORTING iv_name = 'SPACES'
                  CHANGING cg_data  = lt_spaces ).

    " todo: kept for compatibility, remove after grace period #3680
    hook_impl_deserialize( EXPORTING it_spaces = lt_spaces
                           CHANGING ct_impl    = lt_enhancements ).

    read_sources( CHANGING ct_enhancements = lt_enhancements
                           ct_files        = lt_files ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_hook_impl ?= li_tool.

        lo_hook_impl->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_hook_impl->set_original_object(
            pgmid       = ls_original_object-pgmid
            obj_name    = ls_original_object-org_obj_name
            obj_type    = ls_original_object-org_obj_type
            program     = ls_original_object-programname
            main_type   = ls_original_object-org_main_type
            main_name   = ls_original_object-org_main_name ).
        lo_hook_impl->set_include_bound( ls_original_object-include_bound ).

        LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
          lo_hook_impl->add_hook_impl(
              overwrite        = <ls_enhancement>-overwrite
              method           = <ls_enhancement>-method
              enhmode          = <ls_enhancement>-enhmode
              full_name        = <ls_enhancement>-full_name
              source           = <ls_enhancement>-source
              spot             = <ls_enhancement>-spotname
              parent_full_name = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( run_dark = abap_true ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_hook_impl->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        zcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object_enho~serialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_files           TYPE ty_files,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.

    lo_hook_impl ?= ii_enh_tool.

    lv_shorttext = lo_hook_impl->if_enh_object_docu~get_shorttext( ).
    lo_hook_impl->get_original_object(
      IMPORTING
        pgmid     = ls_original_object-pgmid
        obj_name  = ls_original_object-org_obj_name
        obj_type  = ls_original_object-org_obj_type
        main_type = ls_original_object-org_main_type
        main_name = ls_original_object-org_main_name
        program   = ls_original_object-programname ).
    ls_original_object-include_bound = lo_hook_impl->get_include_bound( ).
    lt_enhancements = lo_hook_impl->get_hook_impls( ).

    LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
      CLEAR: <ls_enhancement>-extid,
             <ls_enhancement>-id.
    ENDLOOP.

    add_sources( CHANGING ct_enhancements = lt_enhancements
                          ct_files        = lt_files ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'SHORTTEXT'
                 ig_data = lv_shorttext ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_original_object ).
    ii_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).
    ii_xml->add( iv_name = 'FILES'
                 ig_data = lt_files ).
    ii_xml->add( iv_name = 'SPACES'
                 ig_data = lt_spaces ).

  ENDMETHOD.
ENDCLASS.
