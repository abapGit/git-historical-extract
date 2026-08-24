CLASS zcl_abapgit_historical_objects DEFINITION PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS read
      IMPORTING
        iv_objtype      TYPE vrsd-objtype
        iv_objname      TYPE vrsd-objname
        iv_korrnum      TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_historical_extract=>ty_files_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS create
      IMPORTING
        is_tadir         TYPE zif_abapgit_definitions=>ty_tadir
      RETURNING
        VALUE(ri_object) TYPE REF TO zif_abapgit_historical_object .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_OBJECTS IMPLEMENTATION.


  METHOD create.

    CASE is_tadir-object.
      WHEN 'CLAS'.
        ri_object = NEW zcl_abapgit_historical_clas( is_tadir ).
      WHEN 'DOMD'.
        ri_object = NEW zcl_abapgit_historical_domd( is_tadir ).
      WHEN 'INTF'.
        ri_object = NEW zcl_abapgit_historical_intf( is_tadir ).
      WHEN 'PROG'.
        ri_object = NEW zcl_abapgit_historical_prog( is_tadir ).
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

  ENDMETHOD.


  METHOD read.

    DATA ls_vrsd TYPE vrsd.

    DATA(lv_objtype) = iv_objtype.
* translate
    IF lv_objtype = 'DOMA'.
      BREAK-POINT.
      lv_objtype = 'DOMD'.
    ENDIF.

    SELECT SINGLE * FROM vrsd INTO @ls_vrsd
      WHERE objtype = @lv_objtype
      AND objname = @iv_objname
      AND korrnum = @iv_korrnum.
    IF sy-subrc <> 0.
      " non handled object types, todo
      " then its a deletion? maybe?
      RETURN.
    ENDIF.

    DATA(li_object) = create( VALUE #(
      obj_name = ls_vrsd-objname
      object   = ls_vrsd-objtype
      devclass = 'TODO' ) ).

    rt_files = li_object->build_files( iv_korrnum ).

  ENDMETHOD.
ENDCLASS.
