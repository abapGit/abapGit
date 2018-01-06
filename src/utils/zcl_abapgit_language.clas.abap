*----------------------------------------------------------------------*
* This helper class is used to set and restore the current language.
* As some of the SAP functions used rely on SY-LANGU containing the
* master language, this class is used to temporarily change and then
* restore the value of SY-LANGU.
*----------------------------------------------------------------------*
CLASS zcl_abapgit_language DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA:
      current_language TYPE langu READ-ONLY,
      login_language   TYPE langu READ-ONLY.

    CLASS-METHODS:
      class_constructor,
      restore_login_language,
      set_current_language
        IMPORTING
          !iv_language TYPE langu .

ENDCLASS.



CLASS ZCL_ABAPGIT_LANGUAGE IMPLEMENTATION.


  METHOD class_constructor.


    DATA lv_dummy TYPE string.

    GET LOCALE LANGUAGE login_language COUNTRY lv_dummy MODIFIER lv_dummy.

  ENDMETHOD.


  METHOD restore_login_language.

    SET LOCALE LANGUAGE login_language.

  ENDMETHOD.


  METHOD set_current_language.

    SET LOCALE LANGUAGE iv_language.

  ENDMETHOD.
ENDCLASS.
