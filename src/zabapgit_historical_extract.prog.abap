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
  TRY.
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
