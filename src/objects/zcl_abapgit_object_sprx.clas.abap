CLASS zcl_abapgit_object_sprx DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_proxy,
        data        TYPE string VALUE 'PROXY_DATA' ##NO_TEXT,
        header      TYPE string VALUE 'PROXY_HEADER' ##NO_TEXT,
        webi_policy TYPE string VALUE 'WEBI_POLICY' ##NO_TEXT,
      END OF c_proxy .
    " Web service "External View" properties (Pattern / Security Level /
    " operation Pattern) live in the auto-generated service definition (WEBI),
    " not in SPROXHDR/SPROXDAT. The WEBI is invisible to abapGit (auto_generated),
    " so we piggy-back its policy here on the SPRX. See docs/abapgit-webi-pattern-security-diagnosis.md
    TYPES:
      BEGIN OF ty_webi_policy,
        vepname       TYPE vepname,
        pwsheader     TYPE STANDARD TABLE OF wsheader WITH DEFAULT KEY,
        pwssoapprop   TYPE STANDARD TABLE OF wssoapprop WITH DEFAULT KEY,
        pvepvisoapext TYPE STANDARD TABLE OF vepvisoapext WITH DEFAULT KEY,
      END OF ty_webi_policy .
    DATA mv_object TYPE sproxhdr-object .
    DATA mv_obj_name TYPE sproxhdr-obj_name .

    METHODS load_db
      RETURNING
        VALUE(rs_data) TYPE sprx_db_data .
    " Returns the auto-generated service definition (WEBI) bound to this proxy,
    " or empty when there is none / it is not auto-generated (handled by ZCL_..._WEBI).
    METHODS get_webi_name
      RETURNING
        VALUE(rv_vepname) TYPE vepname .
    METHODS serialize_webi_policy
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_webi_policy
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS get_object_and_name
      EXPORTING
        !ev_object   TYPE sproxhdr-object
        !ev_obj_name TYPE sproxhdr-obj_name .
    METHODS delta_handling
      IMPORTING
        !ii_xml          TYPE REF TO zif_abapgit_xml_input
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



CLASS zcl_abapgit_object_sprx IMPLEMENTATION.


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
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

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

        ii_xml->read(
          EXPORTING
            iv_name = c_proxy-header
          CHANGING
            cg_data = et_sproxhdr_new ).

        IF et_sproxhdr_new IS INITIAL.
          zcx_abapgit_exception=>raise( |SPRX - error deserialize: { ms_item-obj_name }| ).
        ENDIF.

        ii_xml->read(
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


  METHOD load_db.

* method cl_proxy_db=>load_by_abap_name does not exist in lower releases

    DATA: lt_packages TYPE prx_t_namespace_package,
          ls_package  LIKE LINE OF lt_packages,
          ls_hdr      TYPE prx_s_proxy_hdr,
          lv_package  TYPE tadir-devclass,
          lt_ids      TYPE prx_ids.

    cl_proxy_query=>get_hdr_by_abap_name(
      EXPORTING
        object   = mv_object
        obj_name = mv_obj_name
      IMPORTING
        hdr      = ls_hdr ).
    APPEND ls_hdr-id TO lt_ids.

    IF ls_hdr-gen_appl = 'WEBSERVICES'.
      cl_proxy_utils=>get_package(
        EXPORTING
          object   = mv_object
          obj_name = mv_obj_name
        RECEIVING
          rval     = lv_package
        EXCEPTIONS
          OTHERS   = 0 ).

      ls_package-namespace = ls_hdr-esr_nspce.
      ls_package-prefix    = ls_hdr-prefix.
      ls_package-package   = lv_package.
      APPEND ls_package TO lt_packages.
    ENDIF.

    rs_data = cl_proxy_db=>load(
      inactive               = abap_false
      ids                    = lt_ids
      generating_application = ls_hdr-gen_appl
      packages               = lt_packages ).

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
      WHERE object = mv_object
      AND obj_name = mv_obj_name
      AND inactive = abap_false.

    IF sy-subrc = 0 AND lv_changed_by IS NOT INITIAL.
      rv_user = lv_changed_by.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lv_object      TYPE sproxhdr-object,
      lv_obj_name    TYPE sproxhdr-obj_name,
      lv_transp_flag TYPE abap_bool,
      lv_return_code TYPE i,
      lt_log         TYPE sprx_log_t.

    IF iv_package(1) <> '$'.
      lv_transp_flag = abap_true.
    ENDIF.

    get_object_and_name(
      IMPORTING
        ev_object   = lv_object
        ev_obj_name = lv_obj_name ).

    TRY.
        CALL METHOD ('CL_PROXY_DATA')=>('DELETE_SINGLE_PROXY')
          EXPORTING
            object           = lv_object
            obj_name         = lv_obj_name
            i_transport      = lv_transp_flag
            suppress_dialogs = abap_true
          CHANGING
            c_return_code    = lv_return_code
            ct_log           = lt_log.
      CATCH cx_root.
        cl_proxy_data=>delete_single_proxy(
          EXPORTING
            object        = lv_object
            obj_name      = lv_obj_name
            i_transport   = lv_transp_flag
          CHANGING
            c_return_code = lv_return_code
            ct_log        = lt_log ).
    ENDTRY.
    IF lv_return_code <> 0.
      zcx_abapgit_exception=>raise( 'SPRX: Error from DELETE_SINGLE_PROXY' ).
    ENDIF.

    corr_insert( iv_package ).

    tadir_delete( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lt_sproxhdr_new TYPE sprx_hdr_t,
          lt_sproxdat_new TYPE sprx_dat_t.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    delta_handling(
      EXPORTING
        ii_xml          = io_xml
      IMPORTING
        et_sproxhdr_new = lt_sproxhdr_new
        et_sproxdat_new = lt_sproxdat_new ).

    save(
      it_sproxhdr_new = lt_sproxhdr_new
      it_sproxdat_new = lt_sproxdat_new ).

    COMMIT WORK.

    check_sprx_tadir( ).

    " the steps above recreate the auto-generated service definition with default
    " policy; re-apply the versioned Pattern / Security Level / operation policy
    deserialize_webi_policy( io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_status TYPE prx_status.

    cl_proxy_data=>db_get_status(
      EXPORTING
        object   = mv_object
        obj_name = mv_obj_name
      IMPORTING
        status   = lv_status ).

    rv_bool = boolc( lv_status = if_proxy=>c_state_active OR lv_status = if_proxy=>c_state_inactive ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

    DATA lv_status TYPE prx_status.

    cl_proxy_data=>db_get_status(
      EXPORTING
        object   = mv_object
        obj_name = mv_obj_name
      IMPORTING
        status   = lv_status ).

    rv_active = boolc( lv_status = if_proxy=>c_state_active ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      ls_sprx_db_data TYPE sprx_db_data.

    FIELD-SYMBOLS:
      <ls_sproxheader> LIKE LINE OF ls_sprx_db_data-sproxhdr,
      <ls_sproxdat>    LIKE LINE OF ls_sprx_db_data-sproxdat.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_sprx_db_data = load_db( ).

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

    serialize_webi_policy( io_xml ).

  ENDMETHOD.


  METHOD get_webi_name.

    DATA lv_generated TYPE vependpoint-auto_generated.

    " The proxy (e.g. INTF, gen_appl WEBSERVICES) owns a service definition (WEBI).
    " The link is in VEPCROSSREF (sub_type/sub_name point back to the proxy).
    SELECT SINGLE vepname FROM vepcrossref INTO rv_vepname
      WHERE sub_type = mv_object AND sub_name = mv_obj_name.
    IF sy-subrc <> 0.
      CLEAR rv_vepname.
      RETURN.
    ENDIF.

    " Only auto-generated service definitions are handled here. Non-generated
    " ones are visible to abapGit and versioned by ZCL_ABAPGIT_OBJECT_WEBI.
    SELECT SINGLE auto_generated FROM vependpoint INTO lv_generated
      WHERE vepname = rv_vepname AND version = 'A'.
    IF sy-subrc <> 0 OR lv_generated <> abap_true.
      CLEAR rv_vepname.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_webi_policy.

    DATA: lv_vepname TYPE vepname,
          ls_policy  TYPE ty_webi_policy.

    " tables required by the WEBI_GET_OBJECT signature but not persisted here
    DATA: lt_modilog       TYPE STANDARD TABLE OF smodilog WITH DEFAULT KEY,
          lt_vepheader     TYPE STANDARD TABLE OF vepheader WITH DEFAULT KEY,
          lt_vepfunction   TYPE STANDARD TABLE OF vepfunction WITH DEFAULT KEY,
          lt_vepfault      TYPE STANDARD TABLE OF vepfault WITH DEFAULT KEY,
          lt_vepparameter  TYPE STANDARD TABLE OF vepparameter WITH DEFAULT KEY,
          lt_veptype       TYPE STANDARD TABLE OF veptype WITH DEFAULT KEY,
          lt_vepelemtype   TYPE STANDARD TABLE OF vepelemtype WITH DEFAULT KEY,
          lt_veptabletype  TYPE STANDARD TABLE OF veptabletype WITH DEFAULT KEY,
          lt_vepstrutype   TYPE STANDARD TABLE OF vepstrutype WITH DEFAULT KEY,
          lt_veptypesoap   TYPE STANDARD TABLE OF veptypesoapext WITH DEFAULT KEY,
          lt_vepeletypsoap TYPE STANDARD TABLE OF vepeletypsoap WITH DEFAULT KEY,
          lt_veptabtypsoap TYPE STANDARD TABLE OF veptabtypsoap WITH DEFAULT KEY,
          lt_vepfuncsoap   TYPE STANDARD TABLE OF vepfuncsoapext WITH DEFAULT KEY,
          lt_vepfieldref   TYPE STANDARD TABLE OF vepfieldref WITH DEFAULT KEY,
          lt_vependpoint   TYPE STANDARD TABLE OF vependpoint WITH DEFAULT KEY,
          lt_vepparasoap   TYPE STANDARD TABLE OF vepparasoapext WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_wsheader> TYPE wsheader.

    lv_vepname = get_webi_name( ).
    IF lv_vepname IS INITIAL.
      RETURN. " not a web service proxy, or service definition not auto-generated
    ENDIF.

    CALL FUNCTION 'WEBI_GET_OBJECT'
      EXPORTING
        webiname          = lv_vepname
      TABLES
        psmodilog         = lt_modilog
        pvepheader        = lt_vepheader
        pvepfunction      = lt_vepfunction
        pvepfault         = lt_vepfault
        pvepparameter     = lt_vepparameter
        pveptype          = lt_veptype
        pvepelemtype      = lt_vepelemtype
        pveptabletype     = lt_veptabletype
        pvepstrutype      = lt_vepstrutype
        pveptypesoapext   = lt_veptypesoap
        pvepeletypsoap    = lt_vepeletypsoap
        pveptabtypsoap    = lt_veptabtypsoap
        pvepfuncsoapext   = lt_vepfuncsoap
        pvepfieldref      = lt_vepfieldref
        pvependpoint      = lt_vependpoint
        pvepvisoapext     = ls_policy-pvepvisoapext
        pvepparasoapext   = lt_vepparasoap
        pwsheader         = ls_policy-pwsheader
        pwssoapprop       = ls_policy-pwssoapprop
      EXCEPTIONS
        version_not_found = 1
        webi_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RETURN. " no active service definition -> nothing to carry
    ENDIF.

    IF ls_policy-pwssoapprop IS INITIAL.
      RETURN.
    ENDIF.

    ls_policy-vepname = lv_vepname.

    " strip volatile fields so the serialized file stays stable across systems
    LOOP AT ls_policy-pwsheader ASSIGNING <ls_wsheader>.
      CLEAR: <ls_wsheader>-author,
             <ls_wsheader>-createdon,
             <ls_wsheader>-changedby,
             <ls_wsheader>-changedon,
             <ls_wsheader>-ctime,
             <ls_wsheader>-utime.
    ENDLOOP.

    SORT ls_policy-pwssoapprop BY wsname version feature soapapp funcref propnum.
    SORT ls_policy-pwsheader BY wsname version.
    SORT ls_policy-pvepvisoapext BY vepname version.

    ii_xml->add(
      iv_name = c_proxy-webi_policy
      ig_data = ls_policy ).

  ENDMETHOD.


  METHOD deserialize_webi_policy.

    DATA ls_policy TYPE ty_webi_policy.

    ii_xml->read(
      EXPORTING
        iv_name = c_proxy-webi_policy
      CHANGING
        cg_data = ls_policy ).

    " absent node (older repo) or proxy without custom policy -> nothing to do
    IF ls_policy-pwssoapprop IS INITIAL.
      RETURN.
    ENDIF.

    " At this point SPRX deserialize already recreated the auto-generated service
    " definition WITH DEFAULT policy. We overwrite that policy with the versioned
    " one. The WSSOAPPROP feature rows share the same primary key as the defaults,
    " so MODIFY upserts them in place (Pattern/Security/operation = the 3 profiles).
    "
    " NOTE: trace-faithful persistence (see docs section 7). Prefer the official
    "       CL_WS_MD_* "apply profile" API here once its signature is confirmed.
    MODIFY wsheader FROM TABLE ls_policy-pwsheader.
    MODIFY wssoapprop FROM TABLE ls_policy-pwssoapprop.
    MODIFY vepvisoapext FROM TABLE ls_policy-pvepvisoapext.

    " invalidate the regenerable WSDL cache so it is rebuilt from the new policy
    " (SAP itself deletes SPROXWSDL on save - see trace section 7)
    DELETE FROM sproxwsdl WHERE object = mv_object AND obj_name = mv_obj_name.

    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
