REPORT zabapgit_historical_extract.

TABLES tadir.

SELECT-OPTIONS s_devc FOR tadir-devclass OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  TRY.
      NEW zcl_abapgit_historical_extract( )->run( s_devc[] ).
    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

ENDFORM.
