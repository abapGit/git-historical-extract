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

    DATA(ls_tadir) = VALUE zif_abapgit_definitions=>ty_tadir(
      object   = 'CLAS'
      obj_name = 'ZCL_FOOBAR' ).
    DATA(lt_vrsd) = VALUE zcl_abapgit_historical_extract=>ty_vrsd_tt(
      ( objtype = 'CPUB' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'hello' ) ).

    mo_cut->build(
      is_tadir   = ls_tadir
      it_vrsd    = lt_vrsd ).

  ENDMETHOD.

ENDCLASS.
