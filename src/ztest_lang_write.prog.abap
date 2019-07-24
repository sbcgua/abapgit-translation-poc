REPORT ztest_lang_write.

include ztest_lang_textbox_popup.

**********************************************************************
* APP
**********************************************************************

class lcl_app definition final.

  public section.

    methods constructor
      importing
        iv_s_lang type laiso
        iv_t_lang type laiso
        iv_obj_type type zif_abapgit_i18n=>ty_obj_type
        iv_obj_name type sobj_name
        iv_sub_type type zif_abapgit_i18n=>ty_obj_type
        iv_sub_name type zif_abapgit_i18n=>ty_sub_name
        iv_textkey_config type string
        iv_colob_config type string.

    methods run.

    methods read
      returning
        value(rv_file) type string.

    methods write
      importing
        iv_file type string.

  private section.
    data:
      mv_s_lang type laiso,
      mv_t_lang type laiso,
      mv_obj_type type zif_abapgit_i18n=>ty_obj_type,
      mv_obj_name type sobj_name,
      mv_sub_type type zif_abapgit_i18n=>ty_obj_type,
      mv_sub_name type zif_abapgit_i18n=>ty_sub_name,
      mv_textkey_config type string,
      mv_colob_config type string.

endclass.

class lcl_app implementation.

  method constructor.

    mv_s_lang = iv_s_lang.
    mv_t_lang = iv_t_lang.
    mv_obj_type = iv_obj_type.
    mv_obj_name = iv_obj_name.
    mv_sub_type = iv_sub_type.
    mv_sub_name = iv_sub_name.
    mv_textkey_config = iv_textkey_config.
    mv_colob_config = iv_colob_config.

  endmethod.

  method run.

    if mv_obj_type is initial
      or mv_obj_name is initial
      or mv_textkey_config is initial.
      message 'Obj type, name and strategy must be filled' type 'E' display like 'I'.
      return.
    endif.

    data lv_file type string.
    lv_file = read( ).

    data lt_tab type string_table.
    split lv_file at cl_abap_char_utilities=>newline into table lt_tab.

    lv_file = lcl_textbox_popup=>show(
      iv_window_title = 'Edit translation file'
      iv_text = lv_file ).

    if lv_file is initial. " Kinda exit
      return.
    endif.

    replace all occurrences of cl_abap_char_utilities=>cr_lf in lv_file with cl_abap_char_utilities=>newline.

    write( lv_file ).

    lv_file = read( ).
    cl_demo_output=>display_data( lt_tab ).

  endmethod.

  method read.

    data lo_lxe type ref to zcl_abapgit_i18n_lxe.
    data lo_container type ref to zcl_abapgit_i18n_file.
    data lt_langs type zif_abapgit_i18n=>tt_langid.
    data lt_texts type zif_abapgit_i18n=>tt_text_object.
    data lv_str type string.

    append mv_t_lang to lt_langs.

    create object lo_container.
    create object lo_lxe
      exporting
        iv_orig_lang = mv_s_lang
        it_alt_langs = lt_langs.

    lt_texts = lo_lxe->read(
      iv_obj_type     = mv_obj_type
      iv_obj_name     = mv_obj_name
      iv_textkey_conf = mv_textkey_config
      iv_sub_type     = mv_sub_type
      iv_sub_name     = mv_sub_name ).
    lo_container->put( lt_texts ).

    rv_file = lo_container->render( ).

  endmethod.

  method write.

*    data lt_tmp type lxe_tt_pcx_s1.
*    data ls_status type lxestatprc.
*    field-symbols <t> like line of lt_tmp.
*
*    call function 'LXE_OBJ_TEXT_PAIR_READ'
*      exporting
*        s_lang  = 'enUS'
*        t_lang  = 'deDE'
*        custmnr = cl_lxe_constants=>c_trl_area_local
*        objtype = 'DOMA'
*        objname = 'ZTEST_LANG_DOMA_WRITE'
*        read_only = abap_false
*      tables
*        lt_pcx_s1 = lt_tmp.
*
*    clear lt_tmp.
*    append initial line to lt_tmp assigning <t>.
*    <t>-textkey = 'DDTEXT    00001'.
*    <t>-s_text = 'Test DOMA EN'.
*    <t>-t_text = 'Test DOMA XX'.
*    <t>-unitmlt = 60.
*
*    call function 'LXE_OBJ_TEXT_PAIR_WRITE'
*      exporting
*        s_lang  = 'enUS'
*        t_lang  = 'deDE'
*        custmnr = cl_lxe_constants=>c_trl_area_local
*        objtype = 'DOMA'
*        objname = 'ZTEST_LANG_DOMA_WRITE'
*      importing
*        pstatus = ls_status
*      tables
*        lt_pcx_s1 = lt_tmp.

  endmethod.

endclass.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

selection-screen begin of block b1 with frame title txt_b1.

  parameter p_otype type zif_abapgit_i18n=>ty_obj_type  default 'DOMA'.
  parameter p_oname type sobj_name                      default 'ZTEST_LANG_DOMA_WRITE'.
  parameter p_stype type zif_abapgit_i18n=>ty_obj_type. " eg. DYNP
  parameter p_sname type zif_abapgit_i18n=>ty_sub_name. " eg. 2001
  parameter p_tconf type string                         default '10,5z'. " 32 just outputs all
  parameter p_cconf type string                         default ''.

selection-screen end of block b1.

selection-screen begin of screen 100.
selection-screen end of screen 100.

initialization.
  txt_b1   = 'Source program'.          "#EC NOTEXT

form main.

  data lo_app type ref to lcl_app.
  data lt_file type string_table.
  field-symbols <s> like line of lt_file.

*
*DATA: mo_dialogbox TYPE REF TO cl_gui_dialogbox_container,
*      mo_text_ctl TYPE REF TO cl_gui_textedit,
*      mv_data type string.
*
*      mv_data =
*        '@@DOMA' && cl_abap_char_utilities=>newline &&
*        '@DDTEXT,1:60' && cl_abap_char_utilities=>newline &&
*        'EN:Test DOMA EN' && cl_abap_char_utilities=>newline &&
*        'DE:Test DOMA XX' && cl_abap_char_utilities=>newline &&
*        'UK:Тест DOMA UK'.
*
*
*    create object mo_dialogbox
*      exporting
*        top     = 50
*        left    = 200
*        height  = 150
*        width   = 500
*        caption = 'Input text'.
*    create object mo_text_ctl
*      exporting
*        parent = mo_dialogbox.
*    mo_text_ctl->set_font_fixed(  ).
*    mo_text_ctl->set_textstream( mv_data ).

*  return.

  create object lo_app
    exporting
      iv_s_lang   = 'EN'
      iv_t_lang   = 'DE'
      iv_obj_type = p_otype
      iv_obj_name = p_oname
      iv_sub_type = p_stype
      iv_sub_name = p_sname
      iv_textkey_config = p_tconf
      iv_colob_config = p_cconf.

  lo_app->run( ).

endform.

start-of-selection.
  perform main.
