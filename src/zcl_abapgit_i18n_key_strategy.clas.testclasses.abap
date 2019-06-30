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

endclass.
