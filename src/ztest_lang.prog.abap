REPORT ztest_lang.

**********************************************************************
* APP
**********************************************************************

class lcl_app definition final.

  public section.

    methods read
      importing
        iv_s_lang type laiso
        iv_t_lang type string
        iv_objname type sobj_name
      returning
        value(rt_file) type string_table.

endclass.

class lcl_app implementation.

  method read.

    data lo_lxe type ref to zcl_abapgit_i18n_lxe.
    data lt_langs type zif_abapgit_i18n=>tt_langid.
    data lt_texts type zif_abapgit_i18n=>tt_text_object.
    data lv_str type string.
    data lo_container type ref to zcl_abapgit_i18n_file.

    split iv_t_lang at ',' into table lt_langs.

    create object lo_container.
    create object lo_lxe
      exporting
        iv_orig_lang = iv_s_lang
        it_alt_langs = lt_langs.

    " INFO: different objects are not to be mixed in fact

    lt_texts = lo_lxe->read(
      iv_obj_type     = 'PROG' " Not needed ?
      iv_textkey_conf = '8,8,10i'
      iv_obj_name     = 'ZTEST_LANG' " iv_objname
      iv_sub_type     = 'REPT' ).
    lo_container->put( lt_texts ).

    lt_texts = lo_lxe->read(
      iv_obj_type     = 'PROG'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG' " iv_objname
      iv_sub_type     = 'DYNP'
      iv_sub_name     = '2001' ).
    lo_container->put( lt_texts ).

    lt_texts = lo_lxe->read(
      iv_obj_type     = 'PROG'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG' " iv_objname
      iv_sub_type     = 'CUAD' ).
    lo_container->put( lt_texts ).

    lt_texts = lo_lxe->read(
      iv_obj_type     = 'DOMA'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG_DOMA' ).
    lo_container->put( lt_texts ).

    lt_texts = lo_lxe->read(
      iv_obj_type     = 'DTEL'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG_DTEL' ).
    lo_container->put( lt_texts ).

    lt_texts = lo_lxe->read(
      iv_obj_type     = 'TRAN'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG' ).
    lo_container->put( lt_texts ).

    lt_texts = lo_lxe->read( " DOES NOT WORK PROPERLY, unexpected object name
      iv_obj_type     = 'MSAG'
      iv_textkey_conf = '10,5z'
      iv_obj_name     = 'ZTEST_LANG' ).
    lo_container->put( lt_texts ).

    lv_str = lo_container->render( ).
    split lv_str at cl_abap_char_utilities=>newline into table rt_file.

    cl_demo_output=>display_data( rt_file ).

  endmethod.

endclass.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
*selection-screen comment (24) txt_test for field p_test.
parameter p_test type char20.
selection-screen end of line.

selection-screen end of block b1.

initialization.
  txt_b1   = 'Source program'.          "#EC NOTEXT

form main.

  data lo_i18n type ref to lcl_app.
  data lt_file type string_table.
  field-symbols <s> like line of lt_file.

  create object lo_i18n.

  lt_file = lo_i18n->read(
    iv_s_lang = 'EN'
    iv_t_lang = 'DE,UK'
    iv_objname = '' ). " p_prog

  loop at lt_file assigning <s>.
    write: / <s>.
  endloop.

endform.

start-of-selection.
  perform main.
