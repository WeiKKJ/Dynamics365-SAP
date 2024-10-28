*&---------------------------------------------------------------------*
*& Report ZSDCRM001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdcrm001.
INCLUDE zsdcrm001_top.
INCLUDE zlongtext.
INCLUDE zsdcrm001_define.
INCLUDE zsdcrm001_class.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS p_werks TYPE plaf-plwrk DEFAULT '2000'.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM outdata.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD sy-tcode.
  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH '无事务码权限:'(m01) sy-tcode.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& getdata
*&---------------------------------------------------------------------*
FORM getdata.
  CLEAR gt_out.

  SELECT
    z~*,
    ag~name1 AS name_ag,
    we~name1 AS name_we
    FROM ztcrm_so_head AS z
    LEFT JOIN kna1 AS ag ON z~kunnr_ag = ag~kunnr
    LEFT JOIN kna1 AS we ON z~kunnr_we = we~kunnr
    WHERE EXISTS ( SELECT * FROM ztcrm_so_item WHERE new_contractid = z~new_contractid AND action NE '' )
    ORDER BY z~new_contractid
    INTO CORRESPONDING FIELDS OF TABLE @gt_out.
  IF gt_out IS INITIAL.
    MESSAGE s000(oo) WITH '无数据'.
    EXIT.
  ENDIF.
  SELECT
    domname,
    domvalue_l,
    ddtext
    FROM dd07t
    WHERE domname IN ( 'ZD_ZISDXS','ZD_ZHTJGFS','ZD_ZHWLX','ZD_ZCPYT','ZD_ZCPSX' )
    AND ddlanguage = @sy-langu
    AND as4local = 'A'
    ORDER BY domname,domvalue_l
    INTO TABLE @lt_domdes
    .

  SELECT vkorg,vtext
    FROM tvkot
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvkot).
  SELECT vtweg,vtext
    FROM tvtwt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvtwt).
  SELECT vkbur,bezei
    FROM tvkbt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvkbt).
  SELECT vkgrp,bezei
    FROM tvgrt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvgrt).
  SELECT spart,vtext
    FROM tspat
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tspat).
  SELECT kdgrp,ktext
    FROM t151t
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_t151t).
  SELECT zid,zname
    FROM ztsd226
    FOR ALL ENTRIES IN @gt_out
    WHERE zid = @gt_out-province
    OR zid = @gt_out-city
    OR zid = @gt_out-county
    INTO TABLE @DATA(lt_226).
  LOOP AT gt_out INTO gs_out.
    getdomdes 'ZD_ZISDXS' gs_out-zisdxs gs_out-zisdxs_des.
    getdomdes 'ZD_ZHTJGFS' gs_out-zhtjgfs gs_out-zhtjgfs_des.
    getdomdes 'ZD_ZHWLX' gs_out-zhwlx gs_out-zhwlx_des.
    getdomdes 'ZD_ZCPYT' gs_out-zcpyt gs_out-zcpyt_des.
    getdomdes 'ZD_ZCPSX' gs_out-zcpsx gs_out-zcpsx_des.
    READ TABLE lt_tvkot INTO DATA(wa1) WITH KEY vkorg = gs_out-vkorg.
    IF sy-subrc EQ 0.
      gs_out-vkorg_des = wa1-vtext.
    ENDIF.
    READ TABLE lt_tvtwt INTO DATA(wa2) WITH KEY vtweg = gs_out-vtweg.
    IF sy-subrc EQ 0.
      gs_out-vtweg_des = wa2-vtext.
    ENDIF.
    READ TABLE lt_tvkbt INTO DATA(wa3) WITH KEY vkbur = gs_out-vkbur.
    IF sy-subrc EQ 0.
      gs_out-vkbur_des = wa3-bezei.
    ENDIF.
    READ TABLE lt_tvgrt INTO DATA(wa4) WITH KEY vkgrp = gs_out-vkgrp.
    IF sy-subrc EQ 0.
      gs_out-vkgrp_des = wa4-bezei.
    ENDIF.
    READ TABLE lt_tspat INTO DATA(wa5) WITH KEY spart = gs_out-spart.
    IF sy-subrc EQ 0.
      gs_out-spart_des = wa5-vtext.
    ENDIF.
    READ TABLE lt_t151t INTO DATA(wa6) WITH KEY kdgrp = gs_out-kdgrp.
    IF sy-subrc EQ 0.
      gs_out-kdgrp_des = wa6-ktext.
    ENDIF.
    READ TABLE lt_226 INTO DATA(wa226) WITH KEY zid = gs_out-province.
    IF sy-subrc EQ 0.
      gs_out-province_des = wa226-zname.
    ENDIF.
    READ TABLE lt_226 INTO wa226 WITH KEY zid = gs_out-city.
    IF sy-subrc EQ 0.
      gs_out-city_des = wa226-zname.
    ENDIF.
    READ TABLE lt_226 INTO wa226 WITH KEY zid = gs_out-county.
    IF sy-subrc EQ 0.
      gs_out-county_des = wa226-zname.
    ENDIF.
    MODIFY gt_out FROM gs_out.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
* outdata
*---------------------------------------------------------------------*
FORM outdata.
  CHECK gt_out IS NOT INITIAL.
  gv_repid        = sy-repid.
  gs_slayt-zebra  = 'X'.
  gs_slayt-box_fname  = 'SEL'.
  gs_varnt-report = sy-repid.
  gs_varnt-handle = 1.
  DATA dfies_tab LIKE TABLE OF dfies WITH HEADER LINE.
  CLEAR:gt_fldct.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = 'ZTCRM_SO_HEAD'
*     FIELDNAME = ' '
      langu     = sy-langu
*     LFIELDNAME           = ' '
*     ALL_TYPES = ' '
*     GROUP_NAMES          = ' '
*     UCLEN     =
*     DO_NOT_WRITE         = ' '
* IMPORTING
*     X030L_WA  =
*     DDOBJTYPE =
*     DFIES_WA  =
*     LINES_DESCR          =
    TABLES
      dfies_tab = dfies_tab[]
*     FIXED_VALUES         =
* EXCEPTIONS
*     NOT_FOUND = 1
*     INTERNAL_ERROR       = 2
*     OTHERS    = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  LOOP AT dfies_tab.
    IF dfies_tab-fieldname = 'MANDT' OR dfies_tab-fieldname = 'NEW_CONTRACTID'.
      CONTINUE.
    ENDIF.
    CASE dfies_tab-fieldname.
      WHEN 'VBELN'.
        PERFORM catset TABLES gt_fldct USING:
              dfies_tab-fieldname dfies_tab-tabname dfies_tab-fieldname '合同号'.
      WHEN OTHERS.
        PERFORM catset TABLES gt_fldct USING:
              dfies_tab-fieldname dfies_tab-tabname dfies_tab-fieldname dfies_tab-fieldtext.
    ENDCASE.
  ENDLOOP.
  PERFORM catset TABLES gt_fldct USING:
        'VKGRP_DES'  '' '' '销售组描述'   .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc          = gt_fldct
      i_save                   = 'A'
      is_variant               = gs_varnt
      is_layout_lvc            = gs_slayt
      i_callback_program       = gv_repid
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_top_of_page   = 'TOP_OF_PAGEA'
    TABLES
      t_outtab                 = gt_out.
ENDFORM.

*抬头框
FORM top_of_pagea.
  DATA:it_list_commentary TYPE slis_t_listheader,
       wa_list_commentary TYPE slis_listheader,
       sjtms              TYPE i.
  CLEAR:wa_list_commentary,sjtms.
  REFRESH:it_list_commentary.

  sjtms = lines( gt_out ).

  wa_list_commentary-typ = 'S'.
  wa_list_commentary-key = 'CRM销售合同列表条目数:'.
  wa_list_commentary-info = sjtms.
  APPEND wa_list_commentary TO it_list_commentary.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_list_commentary[]
    EXCEPTIONS
      OTHERS             = 1.
ENDFORM.

*&---------------------------------------------------------------------*
*& set_status
*&---------------------------------------------------------------------*
FORM set_status USING pt_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'STD_FULL' EXCLUDING pt_extab.
ENDFORM.

*&--------------------------------------------------------------------*
*& ALV user_command
*&--------------------------------------------------------------------*
FORM user_command USING pv_ucomm TYPE sy-ucomm ##CALLED
      pv_field TYPE slis_selfield.
  READ TABLE gt_out INTO gs_out INDEX pv_field-tabindex.
  CASE pv_ucomm.
    WHEN '&IC1'.
      CASE pv_field-fieldname.
        WHEN 'MATNR' OR 'MAKTX'.
*          SET PARAMETER ID 'MXX' FIELD 'K'.
*          SET PARAMETER ID 'MAT' FIELD gs_out-matnr.
*          SET PARAMETER ID 'WRK' FIELD p_werks.
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'REFRE'.
      PERFORM getdata.
      pv_field-row_stable = 'X'.
      pv_field-col_stable = 'X'.
      pv_field-refresh    = 'X'.
    WHEN 'ITEMS'.

      destroy_control_object o_container          .
      destroy_control_object o_splitter           .
      destroy_control_object o_container_tdid     .
      destroy_control_object o_container_longtext .
      destroy_control_object o_tree_longtext      .
      destroy_control_object o_textedit           .
      destroy_control_object o_tree_repository    .
      destroy_control_object o_customcontainer    .
      destroy_control_object item_container    .
      destroy_control_object alv_grid_item     .
      FREE o_handle_event     .
      FREE o_handle_simple_event     .
      PERFORM getitems.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
* set fieldcat
*---------------------------------------------------------------------*
FORM catset TABLES t_fldcat
USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.    "字段名
  ls_fldcat-scrtext_l =  pv_text.     "长描述
  ls_fldcat-scrtext_s =  pv_text.     "短描述
  ls_fldcat-scrtext_m =  pv_text.     "中描述
  ls_fldcat-coltext   =  pv_text.     "列描述
  ls_fldcat-ref_table =  pv_reftab.   "参考表名
  ls_fldcat-ref_field =  pv_reffld.   "参考字段名
  ls_fldcat-col_opt   = 'A'.          "自动优化列宽

  CASE ls_fldcat-fieldname.
    WHEN 'GSMNG'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'MENGE'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'WRBTR'.
      ls_fldcat-cfieldname = 'WAERS'.
    WHEN 'LIFNR' OR 'AUFNR' OR 'KUNNR'.
      ls_fldcat-edit_mask = '==ALPHA'.
    WHEN 'MATNR' OR 'IDNRK'.
      ls_fldcat-edit_mask = '==MATN1'.
    WHEN 'MEINS' .
      ls_fldcat-edit_mask = '==CUNIT'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getitems
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getitems .
  DATA:posnr TYPE posnr.
  CLEAR:l_ref_alv,msg.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_ref_alv.
  CALL METHOD l_ref_alv->get_selected_rows
    IMPORTING
*     et_index_rows =
      et_row_no = DATA(rows).
  IF lines( rows ) NE 1.
    MESSAGE '请选择一行数据' TYPE 'E'.
  ENDIF.
  READ TABLE rows ASSIGNING FIELD-SYMBOL(<row>) INDEX 1.
  READ TABLE gt_out INTO gs_out INDEX <row>-row_id.
  CLEAR:gt_item.
  SELECT
    *
    FROM ztcrm_so_item
    WHERE new_contractid = @gs_out-new_contractid
    AND action NE ''
    ORDER BY new_contractdetailid
    INTO CORRESPONDING FIELDS OF TABLE @gt_item
    .
  IF gt_item IS INITIAL.
    MESSAGE '无明细数据' TYPE 'E'.
  ENDIF.
  SELECT
    *
    FROM vbak
    WHERE new_contractid = @gs_out-new_contractid
    INTO TABLE @DATA(lt_countvn)
  .
  IF lines( lt_countvn ) GT 1.
    msg = |CRM合同ID[{ gs_out-new_contractid }]存在于[{ lines( lt_countvn ) }]个合同！！！|.
    MESSAGE msg TYPE 'E'.
  ENDIF.
  IF lt_countvn IS NOT INITIAL.
    READ TABLE lt_countvn INTO DATA(wa_countvn) INDEX 1.
    gs_out-vbeln = wa_countvn-vbeln.
    gs_out-auart = wa_countvn-auart.
  ENDIF.
  PERFORM ezsdr IN PROGRAM saplzfg_crm USING '' gs_out-new_contractid CHANGING msg.
  IF msg IS NOT INITIAL.
    MESSAGE msg TYPE 'E'.
  ENDIF.

  SELECT
    ttxit~*
    FROM tvak
    JOIN ttxern ON tvak~txtgr = ttxern~txtgr
    JOIN ttxit ON ttxern~tdid = ttxit~tdid AND ttxern~tdobject = ttxit~tdobject
    WHERE tvak~auart = @gs_out-auart
    AND ttxern~tdobject IN ( 'VBBK','VBBP' )
    AND ttxern~tdid LIKE 'Z%'
    AND ttxit~tdspras = @sy-langu
    ORDER BY ttxit~tdobject,ttxit~tdid
    INTO TABLE @lt_ttxit.

  CLEAR:gt_longtext_im.
  LOOP AT lt_ttxit ASSIGNING FIELD-SYMBOL(<lt_ttxit>) WHERE tdobject = 'VBBK'.
    CLEAR gs_longtext_im.
    gs_longtext_im-sapmk = 'SD'.
    gs_longtext_im-tdid  = <lt_ttxit>-tdid.
    gs_longtext_im-sapno = gs_out-new_contractid.
    APPEND gs_longtext_im TO gt_longtext_im.
  ENDLOOP.
  SELECT
    mara~*,
    makt~maktx
    FROM mara
    LEFT JOIN makt ON mara~matnr = makt~matnr AND spras = @sy-langu
    FOR ALL ENTRIES IN @gt_item
    WHERE mara~chandi = @gt_item-chandi
    AND mara~caizhi = @gt_item-caizhi
    AND mara~width = @gt_item-width
    AND mara~houdu = @gt_item-houdu
    AND mara~yl2 = @gt_item-yl2
    AND mara~yl3 = @gt_item-yl3
    AND mara~yl4 = @gt_item-yl4
    AND mara~hcl = @gt_item-hcl
    AND mara~groes = @gt_item-groes
    INTO TABLE @DATA(lt_mara)
    .
*  SORT lt_mara BY matnr.
  SELECT MAX( posnr ) FROM vbap WHERE vbeln = @gs_out-vbeln INTO @posnr.
  LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<gt_item>).
    IF <gt_item>-posnr IS INITIAL.
      ADD 10 TO posnr.
    ELSE.
      posnr = <gt_item>-posnr.
    ENDIF.

    <gt_item>-posnr = posnr.
    READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<lt_mara>) WITH KEY mara-chandi = <gt_item>-chandi
    mara-caizhi = <gt_item>-caizhi mara-width = <gt_item>-width mara-houdu = <gt_item>-houdu mara-yl2 = <gt_item>-yl2
    mara-yl3 = <gt_item>-yl3 mara-yl4 = <gt_item>-yl4 mara-hcl = <gt_item>-hcl mara-groes = <gt_item>-groes.
    IF sy-subrc EQ 0.
      <gt_item>-matnr = <lt_mara>-mara-matnr.
      <gt_item>-maktx = <lt_mara>-maktx.
    ENDIF.
    LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
      gs_longtext_im-sapmk = 'SD'.
      gs_longtext_im-tdid  = <lt_ttxit>-tdid.
      gs_longtext_im-sapno = |{ gs_out-new_contractid }{ <gt_item>-new_contractdetailid }|.
      APPEND gs_longtext_im TO gt_longtext_im.
    ENDLOOP.
  ENDLOOP.
  PERFORM get_long_text_img TABLES gt_longtext_im.
  IF o_tree_repository IS BOUND.
*  IF o_tree_longtext IS BOUND.
*    PERFORM repo_fill.
    PERFORM repo_fill_simple .
    o_textedit->delete_text( ).
  ENDIF.
  LOOP AT gt_item TRANSPORTING NO FIELDS WHERE action NE ''.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    gs_out-state = '待处理'.
  ELSE.
    gs_out-state = '已处理'.
  ENDIF.

  LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
    READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY sapno = gs_out-new_contractid tdid = <lt_ttxit>-tdid BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE gs_out TO FIELD-SYMBOL(<tdidv>).
      IF sy-subrc EQ 0.
        <tdidv> = gs_longtext_im-longtext.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT gt_item ASSIGNING <gt_item>.
    LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
      DATA(sapno) = CONV ztlongtext-sapno( |{ <gt_item>-new_contractid }{ <gt_item>-new_contractdetailid }| ).
      READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY sapno = sapno tdid = <lt_ttxit>-tdid BINARY SEARCH.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <gt_item> TO FIELD-SYMBOL(<tdidev>).
        IF sy-subrc EQ 0.
          <tdidev> = gs_longtext_im-longtext.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  MODIFY TABLE gt_out FROM gs_out.
  PERFORM fillfldct.
  CALL SCREEN 900.
ENDFORM.

INCLUDE zsdcrm001_pbo_0900.

INCLUDE zsdcrm001_pai_0900.

*&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zsdcrm001_sub_data .
*&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zsdcrm001_sub_pbo .
INCLUDE zsdcrm001_sub_pai .
*&---------------------------------------------------------------------*
*& Form fillfldct
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fillfldct .
  CLEAR gt_fldct_item.
  CASE gs_out-spart.
    WHEN '11' OR '12' OR '13'.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR '  'ZTCRM_SO_ITEM' 'POSNR ' '项目     '   ,
            'MATNR '  'ZTCRM_SO_ITEM' 'MATNR ' '物料编码  '   ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    ,
            'GROES '  'ZTCRM_SO_ITEM' 'GROES ' '品名     '   ,
            'CHANDI'  'ZTCRM_SO_ITEM' 'CHANDI' '产地     '   ,
            'HOUDU '  'ZTCRM_SO_ITEM' 'HOUDU ' '厚度     '   ,
            'WIDTH '  'ZTCRM_SO_ITEM' 'WIDTH ' '宽度     '   ,
            'CAIZHI'  'ZTCRM_SO_ITEM' 'CAIZHI' '材质     '   ,
            'ZHDGC '  'ZTCRM_SO_ITEM' 'ZHDGC ' '厚度公差  '   ,
            'ZKDGC '  'ZTCRM_SO_ITEM' 'ZKDGC ' '宽度公差  '   ,
            'YL9   '  'ZTCRM_SO_ITEM' 'YL9   ' '包装方式  '   ,
            'VRKME '  'ZTCRM_SO_ITEM' 'VRKME ' '销售单位  '   ,
            'KWMENG'  'ZTCRM_SO_ITEM' 'KWMENG' '订单数量  '   ,
            'KBETR '  'ZTCRM_SO_ITEM' 'KBETR ' '金额     '   ,
            'Z001  '  'ZTCRM_SO_ITEM' 'Z001  ' '合同项目备'   ,
            'WERKS '  'ZTCRM_SO_ITEM' 'WERKS ' '工厂     '   ,
            'ACTION'  'ZTCRM_SO_ITEM' 'ACTION' '行状态   '   .
    WHEN '14'.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR ' 'ZTCRM_SO_ITEM' 'POSNR ' '项目       '    ,
            'MATNR ' 'ZTCRM_SO_ITEM' 'MATNR ' '物料编码   '    ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    ,
            'GROES ' 'ZTCRM_SO_ITEM' 'GROES ' '品名       '    ,
            'CHANDI' 'ZTCRM_SO_ITEM' 'CHANDI' '产地       '    ,
            'HOUDU ' 'ZTCRM_SO_ITEM' 'HOUDU ' '厚度       '    ,
            'WIDTH ' 'ZTCRM_SO_ITEM' 'WIDTH ' '宽度       '    ,
            'YL3   ' 'ZTCRM_SO_ITEM' 'YL3   ' '镀层含量   '    ,
            'CAIZHI' 'ZTCRM_SO_ITEM' 'CAIZHI' '材质       '    ,
            'YL4   ' 'ZTCRM_SO_ITEM' 'YL4   ' '表面处理   '    ,
            'HCL   ' 'ZTCRM_SO_ITEM' 'HCL   ' '后处理     '    ,
            'ZHDGC ' 'ZTCRM_SO_ITEM' 'ZHDGC ' '厚度公差   '    ,
            'ZKDGC ' 'ZTCRM_SO_ITEM' 'ZKDGC ' '宽度公差   '    ,
            'YL9   ' 'ZTCRM_SO_ITEM' 'YL9   ' '包装方式   '    ,
            'VRKME ' 'ZTCRM_SO_ITEM' 'VRKME ' '销售单位   '    ,
            'KWMENG' 'ZTCRM_SO_ITEM' 'KWMENG' '订单数量   '    ,
            'KBETR ' 'ZTCRM_SO_ITEM' 'KBETR ' '金额       '    ,
            'Z001  ' 'ZTCRM_SO_ITEM' 'Z001  ' '合同项目备注'    ,
            'WERKS ' 'ZTCRM_SO_ITEM' 'WERKS ' '工厂       '    ,
            'ACTION' 'ZTCRM_SO_ITEM' 'ACTION' '行状态     '    .
    WHEN '15'.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR ' 'ZTCRM_SO_ITEM' 'POSNR ' '项目     '  ,
            'MATNR ' 'ZTCRM_SO_ITEM' 'MATNR ' '物料编码 '   ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    ,
            'GROES ' 'ZTCRM_SO_ITEM' 'GROES ' '品名     '  ,
            'CHANDI' 'ZTCRM_SO_ITEM' 'CHANDI' '产地     '  ,
            'HOUDU ' 'ZTCRM_SO_ITEM' 'HOUDU ' '厚度     '  ,
            'WIDTH ' 'ZTCRM_SO_ITEM' 'WIDTH ' '宽度     '  ,
            'YL3   ' 'ZTCRM_SO_ITEM' 'YL3   ' '镀层含量 '   ,
            'CAIZHI' 'ZTCRM_SO_ITEM' 'CAIZHI' '材质     '  ,
            'ZSXQB ' 'ZTCRM_SO_ITEM' 'ZSXQB ' '涂层种类 '   ,
            'YL2   ' 'ZTCRM_SO_ITEM' 'YL2   ' '颜色     '  ,
            'ZHDGC ' 'ZTCRM_SO_ITEM' 'ZHDGC ' '厚度公差 '   ,
            'ZKDGC ' 'ZTCRM_SO_ITEM' 'ZKDGC ' '宽度公差 '   ,
            'YL9   ' 'ZTCRM_SO_ITEM' 'YL9   ' '包装方式 '   ,
            'VRKME ' 'ZTCRM_SO_ITEM' 'VRKME ' '销售单位 '   ,
            'KWMENG' 'ZTCRM_SO_ITEM' 'KWMENG' '订单数量 '   ,
            'KBETR ' 'ZTCRM_SO_ITEM' 'KBETR ' '金额     '  ,
            'Z001  ' 'ZTCRM_SO_ITEM' 'Z001  ' '合同项目备'  ,
            'WERKS ' 'ZTCRM_SO_ITEM' 'WERKS ' '工厂     '  ,
            'ACTION' 'ZTCRM_SO_ITEM' 'ACTION' '行状态   '  .
  ENDCASE.
  LOOP AT gt_fldct_item ASSIGNING FIELD-SYMBOL(<fldct>).
    CASE <fldct>-fieldname.
      WHEN 'MATNR'.
        <fldct>-edit = 'X'.
        <fldct>-f4availabl = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
