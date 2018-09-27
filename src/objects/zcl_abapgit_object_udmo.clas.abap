CLASS ZCL_ABAPGIT_OBJECT_UDMO DEFINITION
  PUBLIC
  INHERITING FROM ZCL_ABAPGIT_OBJECTS_SUPER
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_ABAPGIT_OBJECT .

    METHODS CONSTRUCTOR
      IMPORTING
        !IS_ITEM     TYPE ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
        !IV_LANGUAGE TYPE SPRAS .
  PROTECTED SECTION.

    METHODS CORR_INSERT
         REDEFINITION .
  PRIVATE SECTION.

    TYPES:
      " You are reminded that the text serialisation / de-serialisation methods depend upon a common type.
      " To make the dependency explicit, there is one common definition.
      BEGIN OF TY_UDMO_TEXT_TYPE.
    TYPES SPRACHE TYPE DM40T-SPRACHE.
    TYPES DMOID TYPE DM40T-DMOID.
    TYPES LANGBEZ TYPE DM40T-LANGBEZ.
    TYPES AS4LOCAL TYPE DM40T-AS4LOCAL.
    TYPES END OF TY_UDMO_TEXT_TYPE .

    DATA MV_DATA_MODEL TYPE UDDMODL .
    DATA MV_TEXT_OBJECT TYPE DOKU_OBJ .
    DATA MV_LXE_TEXT_NAME TYPE LXEOBJNAME .
    DATA MV_ACTIVATION_STATE TYPE AS4LOCAL .
    DATA MS_OBJECT_TYPE TYPE RSDEO .
    CONSTANTS C_TRANSPORT_OBJECT_CLASS TYPE TROBJTYPE VALUE 'SUDM' ##NO_TEXT.
    CONSTANTS C_LXE_TEXT_TYPE TYPE LXEOBJTYPE VALUE 'IM' ##NO_TEXT.
    CONSTANTS C_CORRECTION_OBJECT_TYPE TYPE RSDEO-OBJTYPE VALUE 'UDMO' ##NO_TEXT.
    CONSTANTS C_ACTIVE_STATE TYPE AS4LOCAL VALUE 'A' ##NO_TEXT.

    METHODS IS_NAME_PERMITTED
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS UPDATE_TREE .
    METHODS SERIALIZE_SHORT_TEXTS
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_OUTPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS DESERIALIZE_SHORT_TEXTS
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_INPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS SERIALIZE_LONG_TEXTS
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_OUTPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS DESERIALIZE_LONG_TEXTS
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_INPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS SERIALIZE_ENTITIES
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_OUTPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS DESERIALIZE_ENTITIES
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_INPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS ACCESS_MODIFY
      RETURNING
        VALUE(RV_RESULT) TYPE ABAP_BOOL
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS ACCESS_FREE
      RETURNING
        VALUE(RV_RESULT) TYPE ABAP_BOOL
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS DESERIALIZE_MODEL
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_INPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
    METHODS SERIALIZE_MODEL
      IMPORTING
        !IO_XML TYPE REF TO ZCL_ABAPGIT_XML_OUTPUT
      RAISING
        ZCX_ABAPGIT_EXCEPTION .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_UDMO IMPLEMENTATION.


  METHOD ACCESS_FREE.

    " Release the lock on the object.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        MODE                     = 'FREE'
        OBJECT                   = ME->MS_OBJECT_TYPE
        OBJECT_CLASS             = ME->C_TRANSPORT_OBJECT_CLASS
      EXCEPTIONS
        CANCELED_IN_CORR         = 1
        ENQUEUED_BY_USER         = 2
        ENQUEUE_SYSTEM_FAILURE   = 3
        ILLEGAL_PARAMETER_VALUES = 4
        LOCKED_BY_AUTHOR         = 5
        NO_MODIFY_PERMISSION     = 6
        NO_SHOW_PERMISSION       = 7
        PERMISSION_FAILURE       = 8
        REQUEST_LANGUAGE_DENIED  = 9
        OTHERS                   = 10.

    IF SY-SUBRC NE 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE_T100( ).
    ELSE.
      RV_RESULT = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD ACCESS_MODIFY.

* You are reminded that mode modify is the same as insert, with one important difference:

* Mode INSERT is intended for newly created objects, for which a TADIR entry does not yet
* exist. In that case, the system shows a pop-up for the entry of the package, which isn't
* desirable when the SAPGUI is not available.

* In the context of abapGit, the package is known.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        AUTHORITY_CHECK          = ABAP_TRUE
        GLOBAL_LOCK              = ABAP_TRUE
        MODE                     = 'MODIFY'
        OBJECT                   = ME->MS_OBJECT_TYPE
        OBJECT_CLASS             = ME->C_TRANSPORT_OBJECT_CLASS
      EXCEPTIONS
        CANCELED_IN_CORR         = 1
        ENQUEUED_BY_USER         = 2
        ENQUEUE_SYSTEM_FAILURE   = 3
        ILLEGAL_PARAMETER_VALUES = 4
        LOCKED_BY_AUTHOR         = 5
        NO_MODIFY_PERMISSION     = 6
        NO_SHOW_PERMISSION       = 7
        PERMISSION_FAILURE       = 8
        REQUEST_LANGUAGE_DENIED  = 9
        OTHERS                   = 10.

    IF SY-SUBRC NE 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE_T100( ).
    ELSE.
      RV_RESULT = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    SUPER->CONSTRUCTOR( IS_ITEM  =  IS_ITEM  IV_LANGUAGE = IV_LANGUAGE ).


    " Conversion to Data model
    ME->MV_DATA_MODEL = IS_ITEM-OBJ_NAME.
    " Default activation state is active
    ME->MV_ACTIVATION_STATE = C_ACTIVE_STATE.
    " Derive the data model's text object
    MV_TEXT_OBJECT = 'UDMD' && IS_ITEM-OBJ_NAME.
    " And set the text object to active
    MV_TEXT_OBJECT+30(1) = MV_ACTIVATION_STATE.
    MV_LXE_TEXT_NAME = MV_TEXT_OBJECT.

    " Correction and Transport System object
    ME->MS_OBJECT_TYPE-OBJTYPE = C_CORRECTION_OBJECT_TYPE.
    ME->MS_OBJECT_TYPE-OBJNAME = IS_ITEM-OBJ_NAME.


  ENDMETHOD.


  METHOD CORR_INSERT.

    " You are reminded that SUDM - Data Model has no part objects e.g. no LIMU
    " Therefore global lock is always appropriate

    " You are reminded that the master language (in TADIR) is taken from MV_LANGUAGE.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        OBJECT              = ME->MS_OBJECT_TYPE
        OBJECT_CLASS        = ME->C_TRANSPORT_OBJECT_CLASS
        DEVCLASS            = IV_PACKAGE
        MASTER_LANGUAGE     = MV_LANGUAGE
        MODE                = 'INSERT'
        GLOBAL_LOCK         = ABAP_TRUE
      EXCEPTIONS
        CANCELLED           = 1
        PERMISSION_FAILURE  = 2
        UNKNOWN_OBJECTCLASS = 3
        OTHERS              = 4.
    IF SY-SUBRC = 1.
      ZCX_ABAPGIT_EXCEPTION=>RAISE( 'Cancelled' ).
    ELSEIF SY-SUBRC <> 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE( 'Error from RS_CORR_INSERT' ).
    ENDIF.
  ENDMETHOD.


  METHOD DESERIALIZE_ENTITIES.

    DATA LT_UDMO_ENTITIES TYPE STANDARD TABLE OF DM41S WITH DEFAULT KEY.
    DATA LS_UDMO_ENTITY LIKE LINE OF LT_UDMO_ENTITIES.


    IO_XML->READ( EXPORTING IV_NAME = 'UDMO_ENTITIES'
                  CHANGING  CG_DATA = LT_UDMO_ENTITIES ).

    LOOP AT LT_UDMO_ENTITIES INTO LS_UDMO_ENTITY.

      CALL FUNCTION 'SDU_DMO_ENT_PUT'
        EXPORTING
          OBJECT   = LS_UDMO_ENTITY
        EXCEPTIONS
          RET_CODE = 0
          OTHERS   = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD DESERIALIZE_LONG_TEXTS.

    DATA BEGIN OF LS_UDMO_LONG_TEXT.
    DATA LANGUAGE TYPE DM40T-SPRACHE.
    DATA HEADER   TYPE THEAD.
    DATA CONTENT TYPE XSTRING.
    DATA END OF LS_UDMO_LONG_TEXT.

    TYPES BEGIN OF LANGUAGE_TYPE.
    TYPES LANGUAGE TYPE DM40T-SPRACHE.
    TYPES END OF LANGUAGE_TYPE.

    DATA LT_UDMO_LONG_TEXTS LIKE STANDARD TABLE OF LS_UDMO_LONG_TEXT.
    DATA LT_UDMO_LANGUAGES TYPE STANDARD TABLE OF LANGUAGE_TYPE.
    DATA LV_DOKVERSION TYPE TDVERSION.

    IO_XML->READ( EXPORTING IV_NAME = 'UDMO_LONG_TEXTS'
                  CHANGING  CG_DATA = LT_UDMO_LONG_TEXTS ).

    LOOP AT LT_UDMO_LONG_TEXTS INTO LS_UDMO_LONG_TEXT.

      LS_UDMO_LONG_TEXT-HEADER-TDFUSER = SY-UNAME.
      LS_UDMO_LONG_TEXT-HEADER-TDFDATE = SY-DATUM.
      LS_UDMO_LONG_TEXT-HEADER-TDFTIME = SY-UZEIT.

      " You are reminded that the target system may already have some texts in
      " existence. So we determine the highest existent version.

      CLEAR LV_DOKVERSION.

      SELECT MAX( DOKVERSION ) INTO LV_DOKVERSION
      FROM DOKHL
      WHERE ID = ME->C_LXE_TEXT_TYPE
      AND   OBJECT = ME->MV_TEXT_OBJECT
      AND   LANGU = LS_UDMO_LONG_TEXT-LANGUAGE.

      LS_UDMO_LONG_TEXT-HEADER-TDVERSION = LV_DOKVERSION + 1.

      " This very handy function module takes care of all the variation in text processing
      " between various objects.
      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          SLANG   = ME->MV_LANGUAGE
          TLANG   = LS_UDMO_LONG_TEXT-LANGUAGE
          OBJTYPE = ME->C_LXE_TEXT_TYPE
          OBJNAME = ME->MV_LXE_TEXT_NAME
          HEADER  = LS_UDMO_LONG_TEXT-HEADER
          CONTENT = LS_UDMO_LONG_TEXT-CONTENT.


    ENDLOOP.


  ENDMETHOD.


  METHOD DESERIALIZE_MODEL.

    DATA LS_DM40L TYPE DM40L.


    IO_XML->READ( EXPORTING IV_NAME = 'DM40L'
                  CHANGING CG_DATA = LS_DM40L ).


    " See SDU_MODEL_PUT
    GET TIME.

    LS_DM40L-FLG_FRAME = ABAP_TRUE.
    LS_DM40L-FSTDATE   = SY-DATUM.
    LS_DM40L-FSTTIME   = SY-UZEIT.
    LS_DM40L-FSTUSER   = SY-UNAME.
    LS_DM40L-LSTDATE   = SY-DATUM.
    LS_DM40L-LSTTIME   = SY-UZEIT.
    LS_DM40L-LSTUSER   = SY-UNAME.

    MODIFY DM40L FROM LS_DM40L.

    IF SY-SUBRC <> 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE( 'error from SDU_MODEL_PUT' ).
    ENDIF.



  ENDMETHOD.


  METHOD DESERIALIZE_SHORT_TEXTS.

    DATA LT_UDMO_TEXTS TYPE STANDARD TABLE OF TY_UDMO_TEXT_TYPE WITH DEFAULT KEY.
    DATA LS_UDMO_TEXT  TYPE TY_UDMO_TEXT_TYPE.
    DATA LS_DM40T TYPE DM40T.


    " Deserialize the XML
    IO_XML->READ( EXPORTING IV_NAME = 'UDMO_TEXTS'
                  CHANGING  CG_DATA = LT_UDMO_TEXTS ).

    " For every text provided
    LOOP AT LT_UDMO_TEXTS INTO LS_UDMO_TEXT.

      " Does the text already exist? This is the same logic as used
      " in the FM SDU_MODEL_PUT
      SELECT SINGLE *
      FROM DM40T
      INTO LS_DM40T
      WHERE SPRACHE  EQ LS_UDMO_TEXT-SPRACHE
      AND   DMOID    EQ LS_UDMO_TEXT-DMOID
      AND   AS4LOCAL EQ ME->MV_ACTIVATION_STATE.

      IF SY-SUBRC EQ 0.
        " There is already an active description for this language
        " but the provided description differs
        IF LS_DM40T-LANGBEZ NE LS_UDMO_TEXT-LANGBEZ.

          LS_DM40T-LANGBEZ = LS_UDMO_TEXT-LANGBEZ.
          LS_DM40T-LSTDATE = SY-DATUM.
          LS_DM40T-LSTTIME = SY-UZEIT.
          LS_DM40T-LSTUSER = SY-UNAME.

          MODIFY DM40T FROM LS_DM40T.

        ENDIF.
      ELSE.

        " There is no EXISTING active description in this language

        LS_DM40T-AS4LOCAL = LS_UDMO_TEXT-AS4LOCAL.
        LS_DM40T-DMOID    = LS_UDMO_TEXT-DMOID.
        LS_DM40T-LANGBEZ  = LS_UDMO_TEXT-LANGBEZ.
        LS_DM40T-LSTDATE  = SY-DATUM.
        LS_DM40T-LSTTIME  = SY-UZEIT.
        LS_DM40T-LSTUSER  = SY-UNAME.
        LS_DM40T-SPRACHE  = LS_UDMO_TEXT-SPRACHE.


        INSERT DM40T FROM LS_DM40T.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD IS_NAME_PERMITTED.

    " It is unlikely that a serialised data model will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the data model name.
    " So to be safe, we check. Tx SD11 does this check.


    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        OBJ_NAME   = ME->MS_OBJECT_TYPE-OBJNAME
        OBJ_TYPE   = ME->MS_OBJECT_TYPE-OBJTYPE
      EXCEPTIONS
        WRONG_TYPE = 01.

    IF SY-SUBRC <> 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE_T100( ).
    ENDIF.

  ENDMETHOD.


  METHOD SERIALIZE_ENTITIES.

    DATA LT_UDMO_ENTITIES TYPE STANDARD TABLE OF DM41S WITH DEFAULT KEY.
    FIELD-SYMBOLS <UDMO_ENTITY> TYPE DM41S.

    SELECT * FROM DM41S
      INTO TABLE LT_UDMO_ENTITIES
      WHERE DMOID EQ ME->MV_DATA_MODEL
      AND AS4LOCAL EQ ME->MV_ACTIVATION_STATE.


    LOOP AT LT_UDMO_ENTITIES ASSIGNING <UDMO_ENTITY> .

      " You are reminded that administrative information, such as last changed by user, date, time is not serialised.
      CLEAR <UDMO_ENTITY>-LSTUSER.
      CLEAR <UDMO_ENTITY>-LSTDATE.
      CLEAR <UDMO_ENTITY>-LSTTIME.
      CLEAR <UDMO_ENTITY>-FSTUSER.
      CLEAR <UDMO_ENTITY>-FSTDATE.
      CLEAR <UDMO_ENTITY>-FSTTIME.


    ENDLOOP.

    " You are reminded that descriptions in other languages do not have to be in existence, although they may.
    IF LINES( LT_UDMO_ENTITIES ) GT 0.
      IO_XML->ADD( IV_NAME = 'UDMO_ENTITIES'
                   IG_DATA = LT_UDMO_ENTITIES ).
    ENDIF.



  ENDMETHOD.


  METHOD SERIALIZE_LONG_TEXTS.

    " The model has short texts in multiple languages. These are held in DM40T.

    " The model has a long description also in a master language, with other long descriptions
    " maintained as translations using SE63 Translation Editor. All of these long texts are held in DOK*


    DATA BEGIN OF LS_UDMO_LONG_TEXT.
    DATA LANGUAGE TYPE DM40T-SPRACHE.
    DATA HEADER   TYPE THEAD.
    DATA CONTENT TYPE XSTRING.
    DATA END OF LS_UDMO_LONG_TEXT.

    TYPES BEGIN OF LS_LANGUAGE_TYPE.
    TYPES LANGUAGE TYPE DM40T-SPRACHE.
    TYPES END OF LS_LANGUAGE_TYPE.

    DATA LT_UDMO_LONG_TEXTS LIKE STANDARD TABLE OF LS_UDMO_LONG_TEXT.
    DATA LT_UDMO_LANGUAGES TYPE STANDARD TABLE OF LS_LANGUAGE_TYPE.
    DATA LS_UDMO_LANGUAGE  LIKE LINE OF LT_UDMO_LANGUAGES.
    DATA: LV_ERROR_STATUS  TYPE LXESTATPRC .


    " In which languages are the short texts are maintained.
    SELECT SPRACHE AS LANGUAGE
    FROM DM40T
    INTO TABLE LT_UDMO_LANGUAGES
    WHERE DMOID    EQ ME->MV_DATA_MODEL
    AND   AS4LOCAL EQ ME->MV_ACTIVATION_STATE
    ORDER BY SPRACHE ASCENDING.                         "#EC CI_NOFIRST

    " For every language for which a short text is maintained,
    LOOP AT LT_UDMO_LANGUAGES INTO LS_UDMO_LANGUAGE.

      CLEAR LS_UDMO_LONG_TEXT.
      CLEAR LV_ERROR_STATUS.

      LS_UDMO_LONG_TEXT-LANGUAGE = LS_UDMO_LANGUAGE-LANGUAGE.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          LANG    = LS_UDMO_LANGUAGE-LANGUAGE
          OBJTYPE = ME->C_LXE_TEXT_TYPE
          OBJNAME = ME->MV_LXE_TEXT_NAME
        IMPORTING
          HEADER  = LS_UDMO_LONG_TEXT-HEADER
          CONTENT = LS_UDMO_LONG_TEXT-CONTENT
          PSTATUS = LV_ERROR_STATUS.

      CHECK LV_ERROR_STATUS EQ 'S'. "Success

      " Administrative information is not serialised
      CLEAR LS_UDMO_LONG_TEXT-HEADER-TDFUSER.
      CLEAR LS_UDMO_LONG_TEXT-HEADER-TDFDATE.
      CLEAR LS_UDMO_LONG_TEXT-HEADER-TDFTIME.

      CLEAR LS_UDMO_LONG_TEXT-HEADER-TDLUSER.
      CLEAR LS_UDMO_LONG_TEXT-HEADER-TDLDATE.
      CLEAR LS_UDMO_LONG_TEXT-HEADER-TDLTIME.

      APPEND LS_UDMO_LONG_TEXT TO LT_UDMO_LONG_TEXTS.

    ENDLOOP.

    " You are reminded that long texts do not have to be in existence
    IF LINES( LT_UDMO_LONG_TEXTS ) GT 0.
      IO_XML->ADD( IV_NAME = 'UDMO_LONG_TEXTS'
                   IG_DATA = LT_UDMO_LONG_TEXTS ).
    ENDIF.


  ENDMETHOD.


  METHOD SERIALIZE_MODEL.

    DATA LS_DM40L TYPE DM40L.

    " See SDU_MODEL_GET.
    SELECT SINGLE *
    FROM DM40L
    INTO LS_DM40L
    WHERE DMOID    EQ ME->MV_DATA_MODEL
    AND   AS4LOCAL EQ ME->MV_ACTIVATION_STATE.


    IF SY-SUBRC NE 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE( 'error from UDMO - model serialisation' ).
    ENDIF.

    " You are reminded that administrative data is not serialised.
    CLEAR LS_DM40L-LSTDATE.
    CLEAR LS_DM40L-LSTTIME.
    CLEAR LS_DM40L-LSTUSER.
    CLEAR LS_DM40L-FSTDATE.
    CLEAR LS_DM40L-FSTTIME.
    CLEAR LS_DM40L-FSTUSER.

    IO_XML->ADD( IV_NAME = 'DM40L'
                 IG_DATA = LS_DM40L ).

  ENDMETHOD.


  METHOD SERIALIZE_SHORT_TEXTS.

    DATA LT_UDMO_TEXTS TYPE STANDARD TABLE OF TY_UDMO_TEXT_TYPE WITH DEFAULT KEY.
    " You are reminded that administrative information, such as last changed by user, date, time is not serialised.

    " You are reminded that active short texts of all (existent) languages are serialised.

    SELECT SPRACHE DMOID AS4LOCAL LANGBEZ
      FROM DM40T
      INTO CORRESPONDING FIELDS OF TABLE LT_UDMO_TEXTS
      WHERE DMOID    EQ ME->MV_DATA_MODEL
      AND   AS4LOCAL EQ ME->MV_ACTIVATION_STATE
      ORDER BY SPRACHE ASCENDING.                       "#EC CI_NOFIRST

    " You are reminded that descriptions in other languages do not have to be in existence.
    IF LINES( LT_UDMO_TEXTS ) GT 0.
      IO_XML->ADD( IV_NAME = 'UDMO_TEXTS'
                   IG_DATA = LT_UDMO_TEXTS ).
    ENDIF.


  ENDMETHOD.


  METHOD UPDATE_TREE.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        OBJECT    = ME->MV_DATA_MODEL
        OPERATION = 'INSERT'
        TYPE      = ME->C_CORRECTION_OBJECT_TYPE.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~CHANGED_BY.

    SELECT SINGLE LSTUSER INTO RV_USER
     FROM  DM40L
     WHERE  DMOID     = ME->MV_DATA_MODEL
     AND    AS4LOCAL  = ME->MV_ACTIVATION_STATE.

    IF SY-SUBRC NE 0.
      RV_USER = C_USER_UNKNOWN.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~COMPARE_TO_REMOTE_VERSION.
    CREATE OBJECT RO_COMPARISON_RESULT TYPE ZCL_ABAPGIT_COMPARISON_NULL.
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~DELETE.

* You are reminded that this function model checks for
*  - permissions
*  - locks
*  - connection to transport and correction system
*  - deletion of data model, model relations and all documentation
*  - update of object tree
*  - releasing of lock

    CALL FUNCTION 'RPY_DATAMODEL_DELETE'
      EXPORTING
        MODEL_NAME       = ME->MV_DATA_MODEL
      EXCEPTIONS
        CANCELLED        = 1
        PERMISSION_ERROR = 2
        NOT_FOUND        = 3
        IS_USED          = 4
        OTHERS           = 5.

    IF SY-SUBRC NE 0.
      ZCX_ABAPGIT_EXCEPTION=>RAISE_T100( ).
    ENDIF.


  ENDMETHOD.                    "zif_abapgit_object~delete


  METHOD ZIF_ABAPGIT_OBJECT~DESERIALIZE.

* You are reminded that this method checks for
*  - validity of data model name with regard to naming conventions
*  - permissions and locks
*  - connection to transport and correction system
*  - insert of data model, model relations and all documentation
*  - update of object tree
*  - releasing of lock


* Is the data model name compliant with naming conventions?
    IS_NAME_PERMITTED( ).

* Access Permission granted?
    ACCESS_MODIFY( ).

* Connection to transport and correction system
    CORR_INSERT( IV_PACKAGE = IV_PACKAGE ).

* Insert the data model, relations and documentation
    TRY.
        " Deserialize the data model
        DESERIALIZE_MODEL( IO_XML = IO_XML ).

        " Deserialize relations to entities
        DESERIALIZE_ENTITIES( IO_XML = IO_XML ).

        " Deserialize texts
        DESERIALIZE_SHORT_TEXTS( IO_XML   = IO_XML ).

        " Deserialize long texts
        DESERIALIZE_LONG_TEXTS( IO_XML   = IO_XML ).

        " Update object tree
        UPDATE_TREE( ).

        " Release the object locks
        ACCESS_FREE( ).

      CATCH ZCX_ABAPGIT_EXCEPTION.

        ME->ACCESS_FREE( ).

        ZCX_ABAPGIT_EXCEPTION=>RAISE( 'Error in deserialisation of UDMO' ).


    ENDTRY.

    " You are reminded that data models are not relevant for activation.


  ENDMETHOD.                    "zif_abapgit_object~deserialize


  METHOD ZIF_ABAPGIT_OBJECT~EXISTS.

    "  See Function Module SDU_MODEL_EXISTS

    SELECT COUNT( * ) FROM  DM40L
           WHERE  DMOID     = ME->MV_DATA_MODEL
           AND    AS4LOCAL  = ME->MV_ACTIVATION_STATE.

    RV_BOOL = BOOLC( SY-SUBRC = 0 ).



  ENDMETHOD.                    "zif_abapgit_object~exists


  METHOD ZIF_ABAPGIT_OBJECT~GET_METADATA.
    RS_METADATA = GET_METADATA( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata


  METHOD ZIF_ABAPGIT_OBJECT~HAS_CHANGED_SINCE.

    DATA LV_DATE TYPE DATS.
    DATA LV_TIME TYPE TIMS.


    SELECT SINGLE LSTDATE LSTTIME FROM DM40L
      INTO (LV_DATE, LV_TIME)
           WHERE  DMOID     = ME->MV_DATA_MODEL
           AND    AS4LOCAL  = ME->MV_ACTIVATION_STATE.

    RV_CHANGED = CHECK_TIMESTAMP(
      IV_TIMESTAMP = IV_TIMESTAMP
      IV_DATE      = LV_DATE
      IV_TIME      = LV_TIME ).


  ENDMETHOD.  "zif_abapgit_object~has_changed_since


  METHOD ZIF_ABAPGIT_OBJECT~IS_LOCKED.

    RV_IS_LOCKED = EXISTS_A_LOCK_ENTRY_FOR( IV_LOCK_OBJECT = 'ESDUM'
                                             IV_ARGUMENT    = |{ MS_ITEM-OBJ_TYPE }{ MS_ITEM-OBJ_NAME }| ).

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~JUMP.

    " The function module listed below do not open a new window - so we revert to BDC.
    "    CALL FUNCTION 'SDU_MODEL_SHOW'
    "    CALL FUNCTION 'RS_TOOL_ACCESS'

    DATA LT_BDCDATA TYPE TABLE OF BDCDATA.

    FIELD-SYMBOLS: <LS_BDCDATA> LIKE LINE OF LT_BDCDATA.

    APPEND INITIAL LINE TO LT_BDCDATA ASSIGNING <LS_BDCDATA>.
    <LS_BDCDATA>-PROGRAM  = 'SAPMUD00'.
    <LS_BDCDATA>-DYNPRO   = '0100'.
    <LS_BDCDATA>-DYNBEGIN = ABAP_TRUE.

    APPEND INITIAL LINE TO LT_BDCDATA ASSIGNING <LS_BDCDATA>.
    <LS_BDCDATA>-FNAM = 'BDC_OKCODE'.
    <LS_BDCDATA>-FVAL = '=SHOW'.

    APPEND INITIAL LINE TO LT_BDCDATA ASSIGNING <LS_BDCDATA>.
    <LS_BDCDATA>-FNAM = 'RSUD3-DATM'.
    <LS_BDCDATA>-FVAL = ABAP_TRUE.

    APPEND INITIAL LINE TO LT_BDCDATA ASSIGNING <LS_BDCDATA>.
    <LS_BDCDATA>-FNAM = 'RSUD3-OBJ_KEY'.
    <LS_BDCDATA>-FVAL = MS_ITEM-OBJ_NAME.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        TCODE                 = 'SD11'
        MODE_VAL              = 'E'
      TABLES
        USING_TAB             = LT_BDCDATA
      EXCEPTIONS
        SYSTEM_FAILURE        = 1
        COMMUNICATION_FAILURE = 2
        RESOURCE_FAILURE      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC


  ENDMETHOD.                    "zif_abapgit_object~jump


  METHOD ZIF_ABAPGIT_OBJECT~SERIALIZE.


    IF ZIF_ABAPGIT_OBJECT~EXISTS( ) = ABAP_FALSE.
      RETURN.
    ENDIF.

    " Serialisation of the data model
    SERIALIZE_MODEL( IO_XML ).

    " Serialisation of related entities
    ME->SERIALIZE_ENTITIES( IO_XML  ).

    " Serialisation of other languages
    ME->SERIALIZE_SHORT_TEXTS( IO_XML ).

    "Serialisation of long descriptions
    ME->SERIALIZE_LONG_TEXTS(  IO_XML ).

  ENDMETHOD.                    "zif_abapgit_object~serialize
ENDCLASS.
