REPORT zabapgit_historical_extract.

TABLES: tadir, sscrfields.

SELECT-OPTIONS s_devc FOR tadir-devclass OBLIGATORY.

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
      NEW zcl_abapgit_historical_extract( )->run( s_devc[] ).
    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.
