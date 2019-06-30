class ltcl_i18n_key_strategy definition
  for testing
  risk level harmless
  duration short
  final.

  private section.
    methods build_with_zeropad for testing.
    methods build_with_optional for testing.
    methods build_with_config for testing.
    methods build_with_remove_rows for testing.

    methods parse_with_zeropad for testing raising zcx_abapgit_exception.
endclass.

class ltcl_i18n_key_strategy implementation.

  method build_with_zeropad.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      )->use_sub_type(
      )->add_textkey_segment( iv_len = 10
      )->add_textkey_segment( iv_len = 5 iv_zeropad_int = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      00001'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT,1:60' ).

    lo_cut->use_sub_name( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      00001'
        iv_max_size = 60
      )
      exp = 'DYNP:2001:SRH4:DTXT,1:60' ).

  endmethod.

  method build_with_optional.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      )->use_sub_type(
      )->add_textkey_segment( iv_len = 10
      )->add_textkey_segment( iv_len = 5 iv_optional_int = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      20'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT,20:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT:60' ).

  endmethod.

  method build_with_config.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      )->use_sub_type(
      )->configure_textkey( '10,5z' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      00001'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT,1:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT,0:60' ).

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      )->use_sub_type(
      )->configure_textkey( '10,5i' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      20'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT,20:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT'
        iv_max_size = 60
      )
      exp = 'DYNP:SRH4:DTXT:60' ).

  endmethod.

  method build_with_remove_rows.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      )->configure_textkey( '2,2,2,2' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = '12345678'
        iv_max_size = 60
      )
      exp = 'SRH4:12,34,56,78:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_key(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = '12  56'
        iv_max_size = 60
      )
      exp = 'SRH4:12,,56:60' ).

  endmethod.

  method parse_with_zeropad.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    data lv_sub_type type zif_abapgit_i18n=>ty_obj_type.
    data lv_sub_name type zif_abapgit_i18n=>ty_sub_name.
    data lv_dev_type type zif_abapgit_i18n=>ty_obj_type.
    data lv_textkey type lxetextkey.
    data lv_max_size type i.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_textkey_config = '6,5z' ).

    lo_cut->parse_key(
      exporting
        iv_key = 'DYNP:SRH4:DTXT,10:60'
      importing
        ev_sub_type = lv_sub_type
        ev_sub_name = lv_sub_name
        ev_dev_type = lv_dev_type
        ev_textkey = lv_textkey
        ev_max_size = lv_max_size ).

    cl_abap_unit_assert=>assert_equals( act = lv_sub_type exp = 'DYNP' ).
    cl_abap_unit_assert=>assert_equals( act = lv_sub_name exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_dev_type exp = 'SRH4' ).
    cl_abap_unit_assert=>assert_equals( act = lv_textkey  exp = 'DTXT  00010' ).
    cl_abap_unit_assert=>assert_equals( act = lv_max_size exp = 60 ).

    clear: lv_sub_type, lv_sub_name, lv_dev_type, lv_textkey, lv_max_size.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '6,5i' ).

    lo_cut->parse_key(
      exporting
        iv_key = 'SRH4:DTXT,10:60'
      importing
        ev_sub_type = lv_sub_type
        ev_sub_name = lv_sub_name
        ev_dev_type = lv_dev_type
        ev_textkey  = lv_textkey
        ev_max_size = lv_max_size ).

    cl_abap_unit_assert=>assert_equals( act = lv_sub_type exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_sub_name exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_dev_type exp = 'SRH4' ).
    cl_abap_unit_assert=>assert_equals( act = lv_textkey  exp = 'DTXT  10' ).
    cl_abap_unit_assert=>assert_equals( act = lv_max_size exp = 60 ).

    clear: lv_sub_type, lv_sub_name, lv_dev_type, lv_textkey, lv_max_size.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_use_sub_name = abap_true
      iv_textkey_config = '6,5i' ).

    lo_cut->parse_key(
      exporting
        iv_key = 'DYNP:2001:SRH4:DTXT,10:60'
      importing
        ev_sub_type = lv_sub_type
        ev_sub_name = lv_sub_name
        ev_dev_type = lv_dev_type
        ev_textkey  = lv_textkey
        ev_max_size = lv_max_size ).

    cl_abap_unit_assert=>assert_equals( act = lv_sub_type exp = 'DYNP' ).
    cl_abap_unit_assert=>assert_equals( act = lv_sub_name exp = '2001' ).
    cl_abap_unit_assert=>assert_equals( act = lv_dev_type exp = 'SRH4' ).
    cl_abap_unit_assert=>assert_equals( act = lv_textkey  exp = 'DTXT  10' ).
    cl_abap_unit_assert=>assert_equals( act = lv_max_size exp = 60 ).

    clear: lv_sub_type, lv_sub_name, lv_dev_type, lv_textkey, lv_max_size.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '6,5i' ).

    lo_cut->parse_key(
      exporting
        iv_key = 'SRH4:DTXT:60'
      importing
        ev_sub_type = lv_sub_type
        ev_sub_name = lv_sub_name
        ev_dev_type = lv_dev_type
        ev_textkey  = lv_textkey
        ev_max_size = lv_max_size ).

    cl_abap_unit_assert=>assert_equals( act = lv_sub_type exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_sub_name exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_dev_type exp = 'SRH4' ).
    cl_abap_unit_assert=>assert_equals( act = lv_textkey  exp = 'DTXT' ).
    cl_abap_unit_assert=>assert_equals( act = lv_max_size exp = 60 ).

    clear: lv_sub_type, lv_sub_name, lv_dev_type, lv_textkey, lv_max_size.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '6,5z' ).

    lo_cut->parse_key(
      exporting
        iv_key = 'SRH4:DTXT:60'
      importing
        ev_sub_type = lv_sub_type
        ev_sub_name = lv_sub_name
        ev_dev_type = lv_dev_type
        ev_textkey  = lv_textkey
        ev_max_size = lv_max_size ).

    cl_abap_unit_assert=>assert_equals( act = lv_sub_type exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_sub_name exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_dev_type exp = 'SRH4' ).
    cl_abap_unit_assert=>assert_equals( act = lv_textkey  exp = 'DTXT  00000' ).
    cl_abap_unit_assert=>assert_equals( act = lv_max_size exp = 60 ).

  endmethod.

endclass.
