
* based on https://codezentrale.de/tag/cl_gui_dialogbox_container/

class lcl_textbox_popup definition.

  public section.

    class-methods show
      importing
        iv_window_title type string
        iv_text type string
      returning
        value(rv_text) type string.

  private section.
    class-methods on_close
        for event close of cl_gui_dialogbox_container
        importing sender.

endclass.

class lcl_textbox_popup implementation.

  method show.

    data lo_dialogbox type ref to cl_gui_dialogbox_container.
    data lo_text_ctl type ref to cl_gui_textedit.

    create object lo_dialogbox
      exporting
        parent = cl_gui_container=>screen0
        caption = |{ iv_window_title }|
        width   = 800
        height  = 400
        top     = 50
        left    = 100
        no_autodef_progid_dynnr = abap_true.

    set handler on_close for lo_dialogbox.

    create object lo_text_ctl
      exporting
        parent = lo_dialogbox.
    lo_text_ctl->set_font_fixed(  ).
    lo_text_ctl->set_textstream( iv_text ).

    call selection-screen 100.

    lo_text_ctl->get_textstream( importing text = rv_text ).
    cl_gui_cfw=>flush( ).
    lo_text_ctl->free( ).
    lo_dialogbox->free( ).

  endmethod.

  method on_close.
*    if sender is not initial.
*      sender->free( ).
*    endif.
    leave to screen 0.
  endmethod.

endclass.
