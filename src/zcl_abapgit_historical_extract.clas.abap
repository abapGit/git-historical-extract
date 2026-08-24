CLASS zcl_abapgit_historical_extract DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_trkorr_range TYPE RANGE OF e070-trkorr .
    TYPES ty_object_range TYPE RANGE OF tadir-object .

    METHODS run
      IMPORTING
        it_transports TYPE ty_trkorr_range
        it_object     TYPE ty_object_range
        iv_url        TYPE string
        iv_branch     TYPE string
        iv_skip_git   TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_EXTRACT IMPLEMENTATION.


  METHOD run.

    DATA lt_files TYPE zif_abapgit_historical_extract=>ty_files_tt.

    SELECT trkorr FROM e070
      INTO TABLE @DATA(lt_trkorr)
      WHERE trkorr IN @it_transports
      AND trstatus = @zif_abapgit_cts_api=>c_transport_status-released
      AND trfunction = @zif_abapgit_cts_api=>c_transport_type-wb_request
      AND strkorr = ''
      ORDER BY PRIMARY KEY.

    LOOP AT lt_trkorr INTO DATA(ls_trkorr).
      IF sy-tabix MOD 10 = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Processing transport { ls_trkorr-trkorr }, { sy-tabix }/{ lines( lt_trkorr ) }|
          i_processed          = sy-tabix
          i_total              = lines( lt_trkorr )
          i_output_immediately = abap_true ).
      ENDIF.

      DATA(lt_list) = zcl_abapgit_factory=>get_cts_api( )->list_r3tr_by_request( ls_trkorr-trkorr ).

      CLEAR lt_files.
      LOOP AT lt_list INTO DATA(ls_list) WHERE object IN it_object.
        DATA(lt_obj_files) = zcl_abapgit_historical_objects=>read(
          iv_objtype = ls_list-object
          iv_objname = CONV #( ls_list-obj_name )
          iv_korrnum = ls_trkorr-trkorr ).
        INSERT LINES OF lt_obj_files INTO TABLE lt_files.
      ENDLOOP.

      IF iv_skip_git = abap_false.
        zcl_abapgit_historical_git=>push(
          iv_trkorr = ls_trkorr-trkorr
          it_files  = lt_files
          iv_url    = iv_url
          iv_branch = iv_branch ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
