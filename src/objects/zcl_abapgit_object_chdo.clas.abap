CLASS zcl_abapgit_object_chdo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_change_doc_object TYPE udentity .



ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CHDO IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item  =  is_item  iv_language = iv_language ).

    " Conversion to Change Document Object
    me->mv_change_doc_object = is_item-obj_name.


  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE author INTO rv_user
      FROM tcdrp
      WHERE object   EQ me->mv_change_doc_object.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    lo_generic->delete( ).


  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    lo_generic->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    " What is existence for a Change Document object?
    " Answer: in this context, when the change document object has generated
    " the write document function module and associated includes.
    "
    " abapGit determines existence from TCDRP
    "
    " The generic exists method checks against the primary table TCDRPS
    " However, TCDRPS is a shadow table only filled on export of change document
    " or serialisation from abapGit

    " If only TCDRPS were considered, the change document object would not be visible
    " in the repository listing in abapGit until the object had been serialized.
    " For this reason, abapGit determines existence from TCDRP

    "------------------------------------------------------------------------------------
    " Case                                    TCDOB     TCDRP    TCDRPS    abapGit Repo
    "------------------------------------------------------------------------------------
    " Created                                 Yes       No       No        Does NOT Exist
    " Generated                               Yes       Yes      No        Exists
    " Serialized                              Yes       Yes      Yes       Exists
    " Deleted fully after serializ            No        No       Yes       Does NOT Exist
    " Deleted gen.objects after serialisation Yes       No       Yes       Does NOT Exist
    "-------------------------------------------------------------------------------------

    SELECT COUNT( * )
      FROM tcdrp
      WHERE object = ms_item-obj_name
      AND   datagen = abap_true.

    rv_bool = boolc( 0 = sy-subrc ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    " Testing shows there is no lock object for change document objects.
    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    " Change Document Objects cannot be accessed from SE80.
    " Like RSCDOK99 - use BDC
    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSCDO_NEW'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = 'TCDOB-OBJECT'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TCDOB-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SCDO'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_generic TYPE REF TO zcl_abapgit_objects_generic.

    CREATE OBJECT lo_generic
      EXPORTING
        is_item = ms_item.

    lo_generic->serialize( io_xml ).

  ENDMETHOD.
ENDCLASS.
