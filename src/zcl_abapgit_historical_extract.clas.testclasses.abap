CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_extract DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_historical_extract.

    METHODS:
      setup,
      build FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD build.

* todo

    mo_cut->build( ).

  ENDMETHOD.

ENDCLASS.
