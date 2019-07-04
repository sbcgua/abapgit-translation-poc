class ltct_bapgit_i18n_lxe definition
  for testing
  duration short
  risk level harmless.
  private section.
    methods get_lang_iso4 for testing.
    methods merge_portion_one for testing.
    methods merge_portion_many for testing.
endclass.

class zcl_abapgit_i18n_lxe definition local friends ltct_bapgit_i18n_lxe.

class ltct_bapgit_i18n_lxe implementation.

  method get_lang_iso4.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_i18n_lxe=>get_lang_iso4( 'DE' )
      exp = 'deDE' ).
  endmethod.

  method merge_portion_one.

    data lo_key_strategy type ref to zcl_abapgit_i18n_key_strategy.
    data lo_cut   type ref to zcl_abapgit_i18n_lxe.
    data lt_langs type zif_abapgit_i18n=>tt_langid.
    data lt_tobjs_act type zif_abapgit_i18n=>tt_text_object.
    data lt_tobjs_exp type zif_abapgit_i18n=>tt_text_object.
    data lt_trans type lxe_tt_pcx_s1.

    field-symbols <ls_lxe> like line of lt_trans.
    field-symbols <ls_tobj> like line of lt_tobjs_act.
    field-symbols <ls_lang> like line of <ls_tobj>-texts.

    append 'DE' to lt_langs.

    " Never mind params, not testing strategies here
    lo_key_strategy = zcl_abapgit_i18n_key_strategy=>create( iv_textkey_config = '32' ).

    create object lo_cut
      exporting
        iv_orig_lang = 'EN'
        it_alt_langs = lt_langs.

    append initial line to lt_trans assigning <ls_lxe>.
    <ls_lxe>-s_text = 'ABC'.
    <ls_lxe>-t_text = 'ABC'.
    <ls_lxe>-textkey = '1'.

    append initial line to lt_trans assigning <ls_lxe>.
    <ls_lxe>-s_text = 'EFG'.
    <ls_lxe>-t_text = 'EFG'.
    <ls_lxe>-textkey = '2'.

    append initial line to lt_tobjs_exp assigning <ls_tobj>.
    <ls_tobj>-sub_type = 'DYNP'.
    <ls_tobj>-sub_name = '2001'.
    <ls_tobj>-dev_type = 'SRH4'.
    append initial line to <ls_tobj>-texts assigning <ls_lang>.
    <ls_lang>-lang = 'EN'.
    <ls_lang>-text = 'ABC'.

    append initial line to lt_tobjs_exp assigning <ls_tobj>.
    <ls_tobj>-sub_type = 'DYNP'.
    <ls_tobj>-sub_name = '2001'.
    <ls_tobj>-dev_type = 'SRH4'.
    append initial line to <ls_tobj>-texts assigning <ls_lang>.
    <ls_lang>-lang = 'EN'.
    <ls_lang>-text = 'EFG'.

    lo_cut->merge_portion(
      exporting
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_iso4     = 'enUS'
        it_portion  = lt_trans
        io_key_strategy = lo_key_strategy
      changing
        ct_tobjs = lt_tobjs_act ).

    loop at lt_tobjs_act assigning <ls_tobj>.
      clear <ls_tobj>-id. " Ignore keys for this test
    endloop.

    cl_abap_unit_assert=>assert_equals( act = lt_tobjs_act exp = lt_tobjs_exp ).

  endmethod.

  method merge_portion_many.

    data lo_key_strategy type ref to zcl_abapgit_i18n_key_strategy.
    data lo_cut   type ref to zcl_abapgit_i18n_lxe.
    data lt_langs type zif_abapgit_i18n=>tt_langid.
    data lt_tobjs_act type zif_abapgit_i18n=>tt_text_object.
    data lt_tobjs_exp type zif_abapgit_i18n=>tt_text_object.
    data lt_trans type lxe_tt_pcx_s1.

    field-symbols <ls_lxe> like line of lt_trans.
    field-symbols <ls_tobj> like line of lt_tobjs_act.
    field-symbols <ls_lang> like line of <ls_tobj>-texts.

    append 'DE' to lt_langs.

    " Never mind params, not testing strategies here
    lo_key_strategy = zcl_abapgit_i18n_key_strategy=>create( iv_textkey_config = '32' ).

    create object lo_cut
      exporting
        iv_orig_lang = 'EN'
        it_alt_langs = lt_langs.

    " Expected

    append initial line to lt_tobjs_exp assigning <ls_tobj>.
    <ls_tobj>-sub_type = 'DYNP'.
    <ls_tobj>-sub_name = '2001'.
    <ls_tobj>-dev_type = 'SRH4'.
    append initial line to <ls_tobj>-texts assigning <ls_lang>.
    <ls_lang>-lang = 'EN'.
    <ls_lang>-text = 'ABC'.
    append initial line to <ls_tobj>-texts assigning <ls_lang>.
    <ls_lang>-lang = 'DE'.
    <ls_lang>-text = 'ABC2'.

    append initial line to lt_tobjs_exp assigning <ls_tobj>.
    <ls_tobj>-sub_type = 'DYNP'.
    <ls_tobj>-sub_name = '2001'.
    <ls_tobj>-dev_type = 'SRH4'.
    append initial line to <ls_tobj>-texts assigning <ls_lang>.
    <ls_lang>-lang = 'EN'.
    <ls_lang>-text = 'EFG'.
    append initial line to <ls_tobj>-texts assigning <ls_lang>.
    <ls_lang>-lang = 'DE'.
    <ls_lang>-text = 'EFG2'.

    " LXE postion 1

    append initial line to lt_trans assigning <ls_lxe>.
    <ls_lxe>-s_text = 'ABC'.
    <ls_lxe>-t_text = 'ABC'.
    <ls_lxe>-textkey = '1'.

    append initial line to lt_trans assigning <ls_lxe>.
    <ls_lxe>-s_text = 'EFG'.
    <ls_lxe>-t_text = 'EFG'.
    <ls_lxe>-textkey = '2'.

    lo_cut->merge_portion(
      exporting
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_iso4     = 'enUS'
        it_portion  = lt_trans
        io_key_strategy = lo_key_strategy
      changing
        ct_tobjs = lt_tobjs_act ).

    " LXE postion 2

    clear lt_trans.

    append initial line to lt_trans assigning <ls_lxe>.
    <ls_lxe>-s_text = 'ABC'.
    <ls_lxe>-t_text = 'ABC2'.
    <ls_lxe>-textkey = '1'.

    append initial line to lt_trans assigning <ls_lxe>.
    <ls_lxe>-s_text = 'EFG'.
    <ls_lxe>-t_text = 'EFG2'.
    <ls_lxe>-textkey = '2'.

    lo_cut->merge_portion(
      exporting
        iv_sub_type = 'DYNP'
        iv_sub_name = '2001'
        iv_dev_type = 'SRH4'
        iv_iso4     = 'deDE'
        it_portion  = lt_trans
        io_key_strategy = lo_key_strategy
      changing
        ct_tobjs = lt_tobjs_act ).

    loop at lt_tobjs_act assigning <ls_tobj>.
      clear <ls_tobj>-id. " Ignore keys for this test
    endloop.

    cl_abap_unit_assert=>assert_equals( act = lt_tobjs_act exp = lt_tobjs_exp ).

  endmethod.
endclass.
