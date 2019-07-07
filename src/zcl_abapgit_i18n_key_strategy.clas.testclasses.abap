class ltcl_i18n_key_strategy definition
  for testing
  risk level harmless
  duration short
  final.

  private section.
    methods build_with_zeropad for testing.
    methods build_with_optional for testing.
    methods build_with_remove_rows for testing.
    methods parse_with_zeropad for testing raising zcx_abapgit_exception.
    methods parse_with_optional for testing raising zcx_abapgit_exception.
endclass.

class ltcl_i18n_key_strategy implementation.

  method build_with_zeropad.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_textkey_config = '10,5z' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      00001'
        iv_max_size = 60
      )
*      exp = 'DYNP:SRH4:DTXT,1:60' ).
      exp = 'DTXT,1:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT'
        iv_max_size = 60
      )
*      exp = 'DYNP:SRH4:DTXT,0:60' ).
      exp = 'DTXT,0:60' ).

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_use_sub_name = abap_true
      iv_textkey_config = '10,5z' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      00001'
        iv_max_size = 60
      )
*      exp = 'DYNP:2001:SRH4:DTXT,1:60' ).
      exp = 'DTXT,1:60' ).

  endmethod.

  method build_with_optional.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_textkey_config = '10,5i' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT      20'
        iv_max_size = 60
      )
*      exp = 'DYNP:SRH4:DTXT,20:60' ).
      exp = 'DTXT,20:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = 'DTXT'
        iv_max_size = 60
      )
*      exp = 'DYNP:SRH4:DTXT:60' ).
      exp = 'DTXT:60' ).

  endmethod.

  method build_with_remove_rows.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.

    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '2,2,2,2' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = '12345678'
        iv_max_size = 60
      )
*      exp = 'SRH4:12,34,56,78:60' ).
      exp = '12,34,56,78:60' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->build_id(
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_textkey  = '12  56'
        iv_max_size = 60
      )
*      exp = 'SRH4:12,,56:60' ).
      exp = '12,,56:60' ).

  endmethod.

  method parse_with_zeropad.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.
    data ls_act_key type zcl_abapgit_i18n_key_strategy=>ty_destructured_key.
    data ls_exp_key type zcl_abapgit_i18n_key_strategy=>ty_destructured_key.

    clear: ls_exp_key.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_textkey_config = '6,5z' ).

*    ls_act_key = lo_cut->parse_key( 'DYNP:SRH4:DTXT,10:60' ).
    ls_act_key = lo_cut->parse_id( 'DTXT,10:60' ).

*    ls_exp_key-sub_type = 'DYNP'.
*    ls_exp_key-dev_type = 'SRH4'.
    ls_exp_key-textkey  = 'DTXT  00010'.
    ls_exp_key-max_size = 60.
    cl_abap_unit_assert=>assert_equals( act = ls_act_key exp = ls_exp_key ).

    clear: ls_exp_key.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '6,5z' ).

*    ls_act_key = lo_cut->parse_key( 'SRH4:DTXT:60' ).
    ls_act_key = lo_cut->parse_id( 'DTXT:60' ).

*    ls_exp_key-dev_type = 'SRH4'.
    " Maybe a bug - see the somment inside parse_key
    ls_exp_key-textkey  = 'DTXT'.
    ls_exp_key-max_size = 60.
    cl_abap_unit_assert=>assert_equals( act = ls_act_key exp = ls_exp_key ).

  endmethod.

  method parse_with_optional.

    data lo_cut type ref to zcl_abapgit_i18n_key_strategy.
    data ls_act_key type zcl_abapgit_i18n_key_strategy=>ty_destructured_key.
    data ls_exp_key type zcl_abapgit_i18n_key_strategy=>ty_destructured_key.

    clear: ls_exp_key.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '6,5i' ).

*    ls_act_key = lo_cut->parse_key( 'SRH4:DTXT,10:60' ).
    ls_act_key = lo_cut->parse_id( 'DTXT,10:60' ).

*    ls_exp_key-dev_type = 'SRH4'.
    ls_exp_key-textkey  = 'DTXT  10'.
    ls_exp_key-max_size = 60.
    cl_abap_unit_assert=>assert_equals( act = ls_act_key exp = ls_exp_key ).

    clear: ls_exp_key.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_use_sub_type = abap_true
      iv_use_sub_name = abap_true
      iv_textkey_config = '6,5i' ).

*    ls_act_key = lo_cut->parse_key( 'DYNP:2001:SRH4:DTXT,10:60' ).
    ls_act_key = lo_cut->parse_id( 'DTXT,10:60' ).

*    ls_exp_key-sub_type = 'DYNP'.
*    ls_exp_key-sub_name = '2001'.
*    ls_exp_key-dev_type = 'SRH4'.
    ls_exp_key-textkey  = 'DTXT  10'.
    ls_exp_key-max_size = 60.

    clear: ls_exp_key.
    lo_cut = zcl_abapgit_i18n_key_strategy=>create(
      iv_textkey_config = '6,5i' ).

*    ls_act_key = lo_cut->parse_key( 'SRH4:DTXT:60' ).
    ls_act_key = lo_cut->parse_id( 'DTXT:60' ).

*    ls_exp_key-dev_type = 'SRH4'.
    ls_exp_key-textkey  = 'DTXT'.
    ls_exp_key-max_size = 60.
    cl_abap_unit_assert=>assert_equals( act = ls_act_key exp = ls_exp_key ).

  endmethod.

endclass.
