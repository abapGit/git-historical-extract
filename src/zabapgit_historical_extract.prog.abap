REPORT zabapgit_historical_extract.

TABLES: e070, sscrfields, tadir.

SELECT-OPTIONS s_trkorr FOR e070-trkorr OBLIGATORY.
SELECT-OPTIONS s_object FOR tadir-object.

PARAMETERS p_gurl TYPE string OBLIGATORY DEFAULT 'https://github.com/larshp/test-hist.git' LOWER CASE.
PARAMETERS p_gbr TYPE string OBLIGATORY DEFAULT 'main' LOWER CASE.
PARAMETERS p_skip TYPE abap_bool AS CHECKBOX DEFAULT abap_true.

INCLUDE zabapgit_password_dialog.
INCLUDE zabapgit_forms.

START-OF-SELECTION.
  PERFORM extract.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.

FORM extract.
  DATA lt_fields TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

  TRY.
      IF p_skip = abap_false.
        APPEND VALUE #(
          tabname   = 'SPOP'
          fieldname = 'VARFIELD'
          fieldtext = 'Token' ) TO lt_fields.
        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Enter GitHub Token'
          TABLES
            fields          = lt_fields
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.
        IF sy-subrc <> 0 OR lt_fields[ 1 ]-value IS INITIAL.
          MESSAGE 'Git push cancelled' TYPE 'I'.
          RETURN.
        ENDIF.

        zcl_abapgit_login_manager=>set_basic(
          iv_uri      = p_gurl
          iv_username = CONV #( sy-uname )
          iv_password = lt_fields[ 1 ]-value ).
      ENDIF.

      NEW zcl_abapgit_historical_extract( )->run(
        it_transports = s_trkorr[]
        it_object     = s_object[]
        iv_url        = p_gurl
        iv_branch     = p_gbr
        iv_skip_git   = p_skip ).
    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.
