*&---------------------------------------------------------------------*
*& Include ZSDCRM001_SUB_PBO
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TS 'SO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE so_active_tab_set OUTPUT.
  so-activetab = g_so-pressed_tab.
  CASE g_so-pressed_tab.
    WHEN c_so-tab1.
      g_so-subscreen = '0902'.
    WHEN c_so-tab2.
      g_so-subscreen = '0903'.
    WHEN c_so-tab3.
      g_so-subscreen = '0904'.
    WHEN c_so-tab4.
      g_so-subscreen = '0905'.
    WHEN c_so-tab5.
      g_so-subscreen = '0906'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0906 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0906 OUTPUT.
  PERFORM screen_init_splitter.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form screen_init_splitter
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_init_splitter .
  IF o_container IS INITIAL.
    o_container = NEW #( container_name = 'C0906' ).
    o_splitter = NEW #( parent = o_container rows = 1 columns = 2 ).
    o_container_tdid = o_splitter->get_container( row = 1 column = 1 ).
    o_container_longtext = o_splitter->get_container( row = 1 column = 2 ).
    o_splitter->set_column_width( id = 1 width = 20 ).
*    PERFORM repo_init.
    PERFORM repo_init_simple.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form repo_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM repo_init .
  DATA: ls_header    TYPE treev_hhdr,
        ls_varnt     TYPE disvariant,
        lt_exfun     TYPE ui_functions,
        ls_fcat      TYPE lvc_s_fcat,
        lt_fcat_tree TYPE lvc_t_fcat.
  DATA: lt_event TYPE cntl_simple_events,
        ls_event TYPE cntl_simple_event.
* Create a tree control
  CREATE OBJECT o_tree_longtext
    EXPORTING
*     lifetime                    =
      parent                      = o_container_tdid
*     shellstyle                  =
*     node_selection_mode         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SIN
*     hide_selection              = 'X'
*     item_selection              = 'X'
      no_toolbar                  = 'X'
      no_html_header              = 'X'
*     i_print                     =
*     i_fcat_complete             =
*     i_model_mode                =
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7
      OTHERS                      = 8.
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.
  o_tree_longtext->get_registered_events( IMPORTING events = lt_event ).
  ls_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND ls_event TO lt_event.

* Catch double clic to open query
  ls_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  ls_event-appl_event = abap_true. " no PAI if event occurs
  APPEND ls_event TO lt_event.

  CALL METHOD o_tree_longtext->set_registered_events
    EXPORTING
      events                    = lt_event
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

  CREATE OBJECT o_handle_event.
* Assign event handlers in the application class to each desired event
  SET HANDLER o_handle_event->handle_node_double_click FOR o_tree_longtext.
  SET HANDLER o_handle_event->handle_item_double_click FOR o_tree_longtext.

  CLEAR lt_fcat_tree.
  ls_fcat-fieldname = 'TDID'.
  ls_fcat-coltext   = '文本ID'.
  APPEND ls_fcat TO lt_fcat_tree.
  CLEAR ls_fcat.
  ls_header-heading = '文本类'.
  ls_header-width = 32.
  ls_header-width_pix = 'X'.
  ls_varnt-report  = sy-repid.
  CALL METHOD o_tree_longtext->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = ls_header
      is_variant           = ls_varnt
      it_toolbar_excluding = lt_exfun
      i_save               = 'A'
    CHANGING
      it_fieldcatalog      = lt_fcat_tree
      it_outtab            = t_node_tdid.
  PERFORM repo_fill.
  CREATE OBJECT o_textedit
    EXPORTING
      parent                     = o_container_longtext
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 132
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
      OTHERS                     = 1.
  o_textedit->set_readonly_mode( readonly_mode = 1 ).
  o_textedit->set_statusbar_mode( statusbar_mode = 0 ).
  o_textedit->set_toolbar_mode( toolbar_mode = 0 ).
ENDFORM.

FORM repo_fill.                                             "#EC NEEDED
  DATA: lv_dndh TYPE i,
        ls_layn TYPE lvc_s_layn,
        lt_layi TYPE lvc_t_layi,
        ls_layi TYPE lvc_s_layi.
  CALL METHOD o_tree_longtext->delete_all_nodes( ).
  CLEAR:t_node_tdid.
  LOOP AT lt_ttxit ASSIGNING FIELD-SYMBOL(<lt_ttxit>) WHERE tdobject = 'VBBK'.
    MOVE-CORRESPONDING <lt_ttxit> TO  gs_tree_key.
    CALL METHOD o_tree_longtext->add_node
      EXPORTING
        i_relat_node_key     = ''
        i_relationship       = cl_gui_column_tree=>relat_last_child
        is_outtab_line       = <lt_ttxit>
*       is_node_layout       =
*       it_item_layout       =
        i_node_text          = CONV lvc_value( <lt_ttxit>-tdtext )
      IMPORTING
        e_new_node_key       = gs_tree_key-key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.

    INSERT gs_tree_key INTO TABLE gt_tree_key.
  ENDLOOP.
  CALL METHOD o_tree_longtext->frontend_update.
ENDFORM.                    " REPO_FILL
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  LOOP AT SCREEN.
    IF gs_out-state = '待处理'.
      CASE screen-group1.
        WHEN 'G1'.
          screen-input = 1.
      ENDCASE.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_LIST_BOX OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_list_box OUTPUT.
  PERFORM set_list_box.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SHOWITEMS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE showitems OUTPUT.
  IF o_customcontainer IS INITIAL.
    o_customcontainer = NEW cl_gui_custom_container( container_name = 'CONTI' ).
    alv_grid_item = NEW #( i_parent =  o_customcontainer ).
    CREATE OBJECT lcl_alv_receiver.
    SET HANDLER lcl_alv_receiver->handle_double_click FOR alv_grid_item.
    SET HANDLER lcl_alv_receiver->handle_data_changed FOR alv_grid_item.
    SET HANDLER lcl_alv_receiver->handle_hotspot_click FOR alv_grid_item.
    SET HANDLER lcl_alv_receiver->handle_toolbar FOR alv_grid_item.
    SET HANDLER lcl_alv_receiver->handle_user_command FOR alv_grid_item.
    SET HANDLER lcl_alv_receiver->handle_on_f4 FOR alv_grid_item.
    alv_grid_item->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified )."mc_evt_modified
    alv_grid_item->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter )."mc_evt_enter
    alv_grid_item->register_delayed_event( i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select )."MC_EVT_DELAYED_CHANGE_SELECT
    CLEAR:it_f4.
*    CLEAR:wa_f4.
*    wa_f4-fieldname = 'MATNR'.
*    wa_f4-register = 'X'.
*    wa_f4-getbefore = ''.
*    wa_f4-chngeafter = 'X'.
*    INSERT wa_f4 INTO TABLE it_f4.
    alv_grid_item->register_f4_for_fields( it_f4 ).
    PERFORM:callalv_item.
  ELSE.
    PERFORM:frm_refresh_alv_item.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form callalv_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM callalv_item .
  gs_slayt_item-stylefname = 'FIELD_STYLE'.
  gs_slayt_item-box_fname  = 'SEL'.
  gs_slayt_item-zebra      = 'X'.
  PERFORM callalv_oo IN PROGRAM zvariant_compare
  TABLES gt_item USING alv_grid_item gt_fldct_item 'P1' gs_slayt_item.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_refresh_alv_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_alv_item .
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        ls_stable  TYPE lvc_s_stbl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CHECK alv_grid_item IS NOT INITIAL.

  CALL METHOD alv_grid_item->refresh_table_display
    EXPORTING
      is_stable = ls_stable
*     I_SOFT_REFRESH = 'X'
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form repo_init_simple
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM repo_init_simple .
  DATA: lt_event TYPE cntl_simple_events,
        ls_event TYPE cntl_simple_event.

* Create a tree control
  CREATE OBJECT o_tree_repository
    EXPORTING
      parent              = o_container_tdid
      node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single
    EXCEPTIONS
      lifetime_error      = 1
      cntl_system_error   = 2
      create_error        = 3
      failed              = 4
      OTHERS              = 5.
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* Catch double clic to open query
  ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  ls_event-appl_event = abap_true. " no PAI if event occurs
  APPEND ls_event TO lt_event.

  CALL METHOD o_tree_repository->set_registered_events
    EXPORTING
      events                    = lt_event
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.
  CREATE OBJECT o_handle_simple_event.
* Assign event handlers in the application class to each desired event
  SET HANDLER o_handle_simple_event->hnd_repo_dblclick
      FOR o_tree_repository.
*  SET HANDLER o_handle_simple_event->hnd_ddic_item_dblclick
*      FOR o_tree_repository.
  CREATE OBJECT o_textedit
    EXPORTING
      parent                     = o_container_longtext
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 132
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
      OTHERS                     = 1.
  o_textedit->set_readonly_mode( readonly_mode = 1 ).
  o_textedit->set_statusbar_mode( statusbar_mode = 0 ).
  o_textedit->set_toolbar_mode( toolbar_mode = 0 ).
  PERFORM repo_fill_simple.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form repo_fill_simple
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM repo_fill_simple .
  REFRESH t_node_repository.
  CALL METHOD o_tree_repository->delete_all_nodes.
  CLEAR:t_node_repository.
  LOOP AT lt_ttxit ASSIGNING FIELD-SYMBOL(<lt_ttxit>) GROUP BY ( tdobject =  <lt_ttxit>-tdobject )
    ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
    CLEAR s_node_repository.
    s_node_repository-node_key  = <group>-tdobject."对象名
    s_node_repository-relatkey  = ''."上级节点
    s_node_repository-isfolder  = 'X'."是否为文件夹
    s_node_repository-relatship = ''.
    IF <group>-tdobject = 'VBBK'.
      s_node_repository-text      = '抬头文本'."描述
      APPEND s_node_repository TO t_node_repository.
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
        CLEAR s_node_repository.
        s_node_repository-node_key  = |{ <mem>-tdid }|."对象名
        s_node_repository-relatkey  = <group>-tdobject."上级节点
        s_node_repository-isfolder  = ''."是否为文件夹
        s_node_repository-relatship = cl_gui_simple_tree=>relat_last_child."
        s_node_repository-text      = <mem>-tdtext."描述
        s_node_repository-n_image   = '@0P@'.
        s_node_repository-tdid = <mem>-tdid.
        s_node_repository-sapno = gs_out-new_contractid.
        APPEND s_node_repository TO t_node_repository.
      ENDLOOP.
    ELSE.
      s_node_repository-text      = '明细文本'."描述
      APPEND s_node_repository TO t_node_repository.
      LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<gt_item>).
        CLEAR s_node_repository.
        s_node_repository-node_key  = |{ <group>-tdobject }{ <gt_item>-posnr }|."对象名
        s_node_repository-relatkey  = <group>-tdobject."上级节点
        s_node_repository-isfolder  = 'X'."是否为文件夹
        s_node_repository-relatship = ''.
        s_node_repository-text      = |{ <gt_item>-posnr ALPHA = OUT }行文本|."描述
        APPEND s_node_repository TO t_node_repository.
        LOOP AT GROUP <group> ASSIGNING <mem>.
          CLEAR s_node_repository.
          s_node_repository-node_key  = |{ <gt_item>-posnr }{ <mem>-tdid }|."对象名
          s_node_repository-relatkey  = |{ <group>-tdobject }{ <gt_item>-posnr }|."上级节点
          s_node_repository-isfolder  = ''."是否为文件夹
          s_node_repository-relatship = cl_gui_simple_tree=>relat_last_child."
          s_node_repository-text      = <mem>-tdtext."描述
          s_node_repository-n_image   = '@0P@'.
          s_node_repository-tdid = <mem>-tdid.
          s_node_repository-sapno = |{ gs_out-new_contractid }{ <gt_item>-new_contractdetailid }|.
          APPEND s_node_repository TO t_node_repository.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  CALL METHOD o_tree_repository->add_nodes
    EXPORTING
      table_structure_name           = 'MTREESNODE'
      node_table                     = t_node_repository
    EXCEPTIONS
      failed                         = 1
      error_in_node_table            = 2
      dp_error                       = 3
      table_structure_name_not_found = 4
      OTHERS                         = 5.
  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* Exand all root nodes (my, shared, history)
  CALL METHOD o_tree_repository->expand_root_nodes.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_list_box
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_list_box .

ENDFORM.
