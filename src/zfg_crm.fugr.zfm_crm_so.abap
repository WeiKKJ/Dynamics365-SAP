FUNCTION zfm_crm_so.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(ACTION) TYPE  ZE_SO_ACTION DEFAULT 'S'
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(VBELN) TYPE  VBELN
*"  CHANGING
*"     VALUE(DATA) TYPE  ZSCRM_SO
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_SO'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  IF action NE 'S'.
    IF data IS INITIAL.
      fillmsg 'E' '请传入抬头数据后再调用本接口'.
    ENDIF.
  ELSE.
    IF data IS INITIAL OR data-items IS INITIAL.
      fillmsg 'E' '请传入抬头和明细数据后再调用本接口'.
    ENDIF.
  ENDIF.

  CLEAR:rtype,rtmsg,vbeln.

  TYPES:BEGIN OF zsvbap,
          vbeln  TYPE vbak-vbeln,
          posnr  TYPE vbap-posnr,
          matnr  TYPE vbap-matnr,
          werks  TYPE vbap-werks,
          pstyv  TYPE vbap-pstyv,
          bstkd  TYPE vbkd-bstkd,
          znodel TYPE char1,
          zmatnr TYPE char1,
        END OF zsvbap.
  DATA:it_vbap TYPE TABLE OF zsvbap WITH HEADER LINE.

  DATA:sales_header_in      TYPE bapisdhd1,
       sales_header_inx     TYPE bapisdhd1x,
       order_header_in      TYPE bapisdh1,
       order_header_inx     TYPE bapisdh1x,
       salesdocument_ex     TYPE bapivbeln-vbeln,
       return               TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE,
       sales_items_in       TYPE STANDARD TABLE OF bapisditm WITH HEADER LINE,
       sales_items_inx      TYPE STANDARD TABLE OF bapisditmx WITH HEADER LINE,
       sales_partners       TYPE STANDARD TABLE OF bapiparnr WITH HEADER LINE,
       sales_schedules_in   TYPE STANDARD TABLE OF bapischdl WITH HEADER LINE,
       sales_schedules_inx  TYPE STANDARD TABLE OF bapischdlx WITH HEADER LINE,
       sales_conditions_in  TYPE STANDARD TABLE OF bapicond WITH HEADER LINE,
       sales_conditions_inx TYPE STANDARD TABLE OF bapicondx WITH HEADER LINE,
       extensionin          TYPE TABLE OF bapiparex WITH HEADER LINE,
       extensionex          TYPE TABLE OF bapiparex WITH HEADER LINE.
  DATA:order_header    TYPE bapisdh1,
       order_headerx   TYPE bapisdh1x,
       salesdocument   TYPE bapivbeln-vbeln,
       order_item_in   TYPE TABLE OF bapisditm  WITH HEADER LINE,
       order_item_inx  TYPE TABLE OF bapisditmx WITH HEADER LINE,
       schedule_lines  TYPE TABLE OF bapischdl  WITH HEADER LINE,
       schedule_linesx TYPE TABLE OF bapischdlx WITH HEADER LINE,
       order_text      TYPE TABLE OF bapisdtext WITH HEADER LINE,
       conditions_in   TYPE TABLE OF bapicond   WITH HEADER LINE,
       conditions_inx  TYPE TABLE OF bapicondx  WITH HEADER LINE.

  DATA:BEGIN OF text_stream OCCURS 0,
         text TYPE char2048,
       END OF text_stream,
       lines TYPE TABLE OF tline WITH HEADER LINE.
  DATA:wa_extk  TYPE bape_vbak,
       wa_extkx TYPE bape_vbakx,
       wa_extp  TYPE bape_vbap,
       wa_extpx TYPE bape_vbapx,
       subrc    TYPE sy-subrc,
       mes      TYPE string.
  DATA:posnr TYPE vbap-posnr.
**********************************
  DATA:wa_head TYPE ztcrm_so_head,
       lt_item TYPE TABLE OF ztcrm_so_item.

  PERFORM ezsdr USING '' data-new_contractid CHANGING rtmsg.
  IF rtmsg IS NOT INITIAL.
    fillmsg 'E' rtmsg.
  ENDIF.

  PERFORM domain_value_check USING action CHANGING rtmsg.
  IF rtmsg IS NOT INITIAL.
    fillmsg 'E' rtmsg.
  ENDIF.
  SELECT
    ttxit~*
    FROM tvak
    JOIN ttxern ON tvak~txtgr = ttxern~txtgr
    JOIN ttxit ON ttxern~tdid = ttxit~tdid AND ttxern~tdobject = ttxit~tdobject
    WHERE tvak~auart = @data-auart
    AND ttxern~tdobject IN ( 'VBBK','VBBP' )
    AND ttxern~tdid LIKE 'Z%'
    AND ttxit~tdspras = @sy-langu
    ORDER BY ttxit~tdobject,ttxit~tdid
    INTO TABLE @DATA(lt_ttxit).
  "补0
  PERFORM addzero(zpubform) CHANGING data-kunnr_we.
  PERFORM addzero(zpubform) CHANGING data-kunnr_ag.
  " 数据校验  24.09.2024 15:18:51 by kkw
  checkinitial data-new_contractid       'CRM合同ID'            .
  checkinitial data-bstkd       '客户参考'            .
  checkinitial data-auart       '销售凭证类型'          .
  checkinitial data-kunnr_we    '送达方'             .
  checkinitial data-kunnr_ag    '售达方'             .
  checkinitial data-vkorg       '销售组织'            .
  checkinitial data-vtweg       '分销渠道'            .
  checkinitial data-prsdt       '凭证日期'            .
  checkinitial data-guebg       '有效期自'            .
  checkinitial data-gueen       '有效期至'            .
  checkinitial data-zhtyf       '合同月份'            .
  checkinitial data-spart       '产品组'             .
  checkinitial data-zhwlx       '货物类型'            .
  checkinitial data-vkbur       '销售办事处'           .
  checkinitial data-vkgrp       '销售组'             .
  checkinitial data-waerk       '凭证货币'            .
  checkinitial data-kursk       '汇率'              .
  checkinitial data-zterm       '付款条件'            .
  checkinitial data-zhtjgfs     '合同加工方式'          .
  checkinitial data-zdjbl       '定金比例'            .
  checkinitial data-zisdxs      '销售类型'            .
  checkinitial data-zcpyt       '产品用途'            .

  DATA(comp) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( data ) )->components.
  LOOP AT comp ASSIGNING FIELD-SYMBOL(<comp>).
    ASSIGN COMPONENT <comp>-name OF STRUCTURE data TO FIELD-SYMBOL(<data_value>).
    IF sy-subrc EQ 0.
      PERFORM domain_value_check USING <data_value> CHANGING rtmsg.
      UNASSIGN <data_value>.
      IF rtmsg IS NOT INITIAL.
        fillmsg 'E' rtmsg.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT data-items ASSIGNING FIELD-SYMBOL(<item>) GROUP BY ( new_contractdetailid = <item>-new_contractdetailid
    index = GROUP INDEX size = GROUP SIZE
     ) ASSIGNING FIELD-SYMBOL(<group>).
    IF <group>-size NE 1.
      rtmsg = |CRM合同明细ID[{ <group>-new_contractdetailid }]重复[{ <group>-size }]次，请核实明细数据|.
      fillmsg 'E' rtmsg.
    ENDIF.
    LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
      DATA(tabix) = sy-tabix.
      mes = '第' && tabix && '行,' && '品名'           .
      checkinitial <mem>-groes mes.
      mes = '第' && tabix && '行,' && 'CRM合同明细ID'           .
      checkinitial <mem>-new_contractdetailid mes.
      mes = '第' && tabix && '行,' && '厚度'           .
      checkinitial <mem>-houdu mes.
      mes = '第' && tabix && '行,' && '宽度'           .
      checkinitial <mem>-width mes.
      mes = '第' && tabix && '行,' && '材质'           .
      checkinitial <mem>-caizhi mes.
      mes = '第' && tabix && '行,' && '工厂'           .
      checkinitial <mem>-werks mes.
      mes = '第' && tabix && '行,' && '销售单位'         .
      checkinitial <mem>-vrkme mes.
      mes = '第' && tabix && '行,' && '订单数量'         .
      checkinitial <mem>-kwmeng mes.
      mes = '第' && tabix && '行,' && '价格'           .
      checkinitial <mem>-kbetr mes.
      mes = '第' && tabix && '行,' && '税率'           .
      checkinitial <mem>-mwskz mes.
      comp = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <mem> ) )->components.
      LOOP AT comp ASSIGNING <comp>.
        ASSIGN COMPONENT <comp>-name OF STRUCTURE <mem> TO <data_value>.
        IF sy-subrc EQ 0.
          PERFORM domain_value_check USING <data_value> CHANGING rtmsg.
          UNASSIGN <data_value>.
          IF rtmsg IS NOT INITIAL.
            rtmsg = |明细第{ tabix }行,{ rtmsg }|.
            fillmsg 'E' rtmsg.
          ENDIF.
        ENDIF.
      ENDLOOP.
*单位转换
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = <mem>-vrkme
          language       = sy-langu
        IMPORTING
          output         = <mem>-vrkme
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
    ENDLOOP.
  ENDLOOP.

  CASE action.
    WHEN 'S'.

      " 校验SO的存在性用于确定增删改  21.09.2024 11:38:03 by kkw
      SELECT
        vbak~vbeln,
        vbap~posnr,
        vbak~new_contractid,
        vbap~new_contractdetailid,
        @space AS action
        FROM ztcrm_so_item AS vbap
        JOIN ztcrm_so_head AS vbak ON vbap~new_contractid = vbap~new_contractid
        WHERE vbak~new_contractid = @data-new_contractid
        INTO TABLE @DATA(lt_check)
        .
      SORT lt_check BY new_contractdetailid.
      MOVE-CORRESPONDING data TO wa_head.
      wa_head-kunnr_we = |{ wa_head-kunnr_we ALPHA = IN }|.
      wa_head-kunnr_ag = |{ wa_head-kunnr_ag ALPHA = IN }|.
      CLEAR wa_head-vbeln.
      MOVE-CORRESPONDING data-items TO lt_item.
      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<lt_item>).
        CLEAR:<lt_item>-posnr.
        <lt_item>-new_contractid = data-new_contractid.
        IF lt_check IS NOT INITIAL.
          READ TABLE lt_check ASSIGNING FIELD-SYMBOL(<lt_check>) WITH KEY new_contractdetailid = <lt_item>-new_contractdetailid.
          IF sy-subrc EQ 0.
            wa_head-vbeln = |{ <lt_check>-vbeln ALPHA = IN }|.
            <lt_item>-posnr = |{ <lt_check>-posnr ALPHA = IN }|.
            IF NOT ( <lt_item>-action = 'U' OR <lt_item>-action = 'D' ).
              rtmsg = |明细ID[{ <lt_item>-new_contractdetailid }]已存在，只能做修改或删除操作|.
              fillmsg 'E' rtmsg.
            ENDIF.
          ELSE.
            IF NOT <lt_item>-action = 'A'.
              rtmsg = |明细ID[{ <lt_item>-new_contractdetailid }]未存在，只能做增行操作|.
              fillmsg 'E' rtmsg.
            ENDIF.
          ENDIF.
        ELSE.
          IF NOT <lt_item>-action = 'I'.
            rtmsg = |单ID[{ data-new_contractid }]未存在，只能做创建操作|.
            fillmsg 'E' rtmsg.
          ENDIF.
        ENDIF.
        LOOP AT lt_ttxit ASSIGNING FIELD-SYMBOL(<lt_ttxit>) WHERE tdobject = 'VBBP'.
          ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <lt_item> TO FIELD-SYMBOL(<zlongtext>).
          IF sy-subrc EQ 0.
            DATA(sapno) = |{ <lt_item>-new_contractid }{ <lt_item>-new_contractdetailid }|.
            PERFORM i_longtext USING sapno <lt_ttxit>-tdid <zlongtext>.
            CLEAR <zlongtext>.
            UNASSIGN <zlongtext>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE wa_head TO <zlongtext>.
        IF sy-subrc EQ 0.
          PERFORM i_longtext USING wa_head-new_contractid <lt_ttxit>-tdid <zlongtext>.
          CLEAR <zlongtext>.
          UNASSIGN <zlongtext>.
        ENDIF.
      ENDLOOP.
      MODIFY ztcrm_so_item FROM TABLE lt_item.
      IF sy-subrc NE 0.
        subrc = 4.
      ENDIF.
      MODIFY ztcrm_so_head FROM wa_head.
      IF sy-subrc NE 0.
        subrc = 4.
      ENDIF.
      IF subrc EQ 0.
        COMMIT WORK.
        rtmsg = 'S:数据保存成功'.
        rtype = 'S'.
        zfmdatasave2 'R'.
      ELSE.
        ROLLBACK WORK.
        rtmsg = 'E:数据保存失败'.
        fillmsg rtmsg(1) rtmsg.
      ENDIF.

      RETURN.
    WHEN 'I'.
      SELECT
        COUNT(*)
        FROM vbkd
        WHERE bstkd = @data-bstkd
        .
      IF sy-subrc = 0.
        rtmsg = |外部合同号：[{ data-bstkd }]已创建销售订单，请确认！|.
        fillmsg 'E' rtmsg.
      ENDIF.
      SELECT
        COUNT( DISTINCT vbeln ) AS countvn
        FROM vbak
        WHERE new_contractid = @data-new_contractid
        INTO @DATA(countvn)
      .
      IF countvn NE 0.
        rtmsg = |CRM合同ID[{ data-new_contractid }]存在于[{ countvn }]个合同！！！|.
        fillmsg 'E' rtmsg.
      ENDIF.
      IF data-items IS INITIAL.
        fillmsg 'E' '合同明细不能为空'.
      ENDIF.

      CLEAR: sales_header_in        .
      CLEAR: sales_header_inx       .
      CLEAR: salesdocument_ex       .
      CLEAR: return                 ,    return[]               .
      CLEAR: sales_items_in         ,    sales_items_in[]       .
      CLEAR: sales_items_inx        ,    sales_items_inx[]      .
      CLEAR: sales_partners         ,    sales_partners[]       .
      CLEAR: sales_schedules_in     ,    sales_schedules_in[]   .
      CLEAR: sales_schedules_inx    ,    sales_schedules_inx[]  .
      CLEAR: sales_conditions_in    ,    sales_conditions_in[]  .
      CLEAR: sales_conditions_inx   ,    sales_conditions_inx[] .
      CLEAR: order_text             ,    order_text[]           .
      CLEAR: extensionin            ,    extensionin[]          .

      "抬头
*******************************************************20230214
      sales_header_in-doc_type   = data-auart ."订单类型
      sales_header_in-sales_org  = data-vkorg ."销售组织
      sales_header_in-purch_no_c = data-bstkd ."外部合同号
      sales_header_in-division   = data-spart ."产品组
      sales_header_in-sales_grp  = data-vkgrp ."销售组
      sales_header_in-distr_chan = data-vtweg ."分销渠道
      sales_header_in-sales_off  = data-vkbur ."销售办事处
      sales_header_in-currency   = data-waerk.
      sales_header_in-doc_date   = data-prsdt."凭证日期 (接收/发送日期)
      sales_header_in-incoterms1 = data-inco1."*  国际贸易条款（第 1 部分）
      sales_header_in-incoterms2 = data-inco2_l."*  国际贸易条款（第 2 部分）
      sales_header_in-price_date = data-prsdt.
      sales_header_in-ct_valid_f = data-guebg.
      sales_header_in-ct_valid_t = data-gueen.
      sales_header_in-cust_group = data-kdgrp.
      PERFORM setbapix USING sales_header_in CHANGING sales_header_inx.

*增强字段
      CLEAR wa_extk.
      MOVE-CORRESPONDING data TO wa_extk.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAK'.
      extensionin+30(960) = wa_extk.
      APPEND extensionin.
      CLEAR:extensionin.
      PERFORM setbapix USING wa_extk CHANGING wa_extkx.
      extensionin-structure = 'BAPE_VBAKX'.
      extensionin+30(960) = wa_extkx.
      APPEND extensionin.
**********************************
      "合作伙伴
      CLEAR sales_partners.
      sales_partners-partn_role = 'AG'.
      sales_partners-partn_numb = data-kunnr_ag.
      APPEND sales_partners.
      CLEAR sales_partners.
      sales_partners-partn_role = 'WE'.
      sales_partners-partn_numb = data-kunnr_we.
      APPEND sales_partners.

      LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE data TO <zlongtext>.
        IF sy-subrc EQ 0.
          "切割文本
          CLEAR:text_stream,text_stream[],lines[],lines.
          text_stream-text = <zlongtext>.
          APPEND text_stream.
          CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
            TABLES
              text_stream = text_stream
              itf_text    = lines.
          LOOP AT lines.
            CLEAR:order_text.
            order_text-text_id    = <lt_ttxit>-tdid.
            order_text-langu      = sy-langu.
            order_text-format_col = '*' .
            order_text-text_line  = lines-tdline.
            APPEND order_text.
          ENDLOOP.
          UNASSIGN <zlongtext>.
        ENDIF.
      ENDLOOP.
**********************************
      "行项目
      posnr = 0 .
      LOOP AT data-items ASSIGNING <item> .
        CLEAR:sales_items_in,sales_items_inx,sales_schedules_in,sales_schedules_inx .
        IF <item>-posnr IS NOT INITIAL.
          posnr = <item>-posnr.
        ELSE.
          ADD 10 TO posnr.
        ENDIF.
        <item>-posnr = posnr.
        sales_items_in-itm_number     = posnr.
        sales_items_in-material       = <item>-matnr.
        PERFORM addzero(zpubform) CHANGING sales_items_in-material.
        sales_items_in-target_qty     = <item>-kwmeng.
        sales_items_in-target_qu      = <item>-vrkme.
        sales_items_in-sales_unit     = <item>-vrkme.
        PERFORM transunit_to_inside(zpubform) CHANGING sales_items_in-target_qu.
        PERFORM transunit_to_inside(zpubform) CHANGING sales_items_in-sales_unit.
        sales_items_in-plant          = <item>-werks.
        sales_items_in-currency       = data-waerk.
        PERFORM setbapix USING sales_items_in CHANGING sales_items_inx.
        APPEND:sales_items_in,sales_items_inx.
        "计划行
        sales_schedules_in-itm_number = posnr .
        sales_schedules_in-sched_line = '0001'.
        sales_schedules_in-req_qty    = <item>-kwmeng.
        sales_schedules_in-req_date   = sy-datum .
        PERFORM setbapix USING sales_schedules_in CHANGING sales_schedules_inx.
        APPEND: sales_schedules_in,sales_schedules_inx.

*增强字段
        CLEAR:extensionin,wa_extp.
        MOVE-CORRESPONDING <item> TO wa_extp.
        extensionin-structure = 'BAPE_VBAP'.
        extensionin+30(960) = wa_extp.
        APPEND extensionin.
        CLEAR:extensionin,wa_extpx .
        PERFORM setbapix USING wa_extp CHANGING wa_extpx.
        extensionin-structure = 'BAPE_VBAPX'.
        extensionin+30(960) = wa_extpx.
        APPEND extensionin.

*文本
        LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
          ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <item> TO <zlongtext>.
          IF sy-subrc EQ 0.
            "切割文本
            CLEAR:text_stream,text_stream[],lines[],lines.
            text_stream-text = <zlongtext>.
            APPEND text_stream.
            CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
              TABLES
                text_stream = text_stream
                itf_text    = lines.
            LOOP AT lines.
              CLEAR:order_text.
              order_text-itm_number = posnr.
              order_text-text_id    = <lt_ttxit>-tdid.
              order_text-langu      = sy-langu .
              order_text-format_col = '*' .
              order_text-text_line  = lines-tdline.
              APPEND order_text.
            ENDLOOP.
            UNASSIGN <zlongtext>.
          ENDIF.
        ENDLOOP.
        CLEAR:sales_conditions_in,sales_conditions_inx.
        sales_conditions_in-itm_number = posnr.
        sales_conditions_in-cond_type  = 'ZPR0'.
        sales_conditions_in-cond_value = <item>-kbetr.
        sales_conditions_in-cond_p_unt = 1.
        sales_conditions_in-currency   = data-waerk.
        PERFORM setbapix USING sales_conditions_in CHANGING sales_conditions_inx.
        APPEND:sales_conditions_in,sales_conditions_inx.
      ENDLOOP.
**********************************
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          sales_header_in      = sales_header_in
          sales_header_inx     = sales_header_inx
        IMPORTING
          salesdocument_ex     = salesdocument_ex
        TABLES
          return               = return
          sales_items_in       = sales_items_in
          sales_items_inx      = sales_items_inx
          sales_partners       = sales_partners
          sales_schedules_in   = sales_schedules_in
          sales_schedules_inx  = sales_schedules_inx
          sales_conditions_in  = sales_conditions_in
          sales_conditions_inx = sales_conditions_inx
          sales_text           = order_text
          extensionin          = extensionin
        EXCEPTIONS
          OTHERS               = 1.

      IF sy-subrc NE 0.
        PERFORM msgtotext(zpubform) USING '' '' '' '' '' '' CHANGING rtmsg.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CONCATENATE return-message rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc EQ 0.
        ADD 4 TO subrc.
      ENDIF.

      IF subrc = 0 AND salesdocument_ex IS NOT INITIAL.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtype = 'S'.
        rtmsg = |创建销售合同成功，单号：[{ salesdocument_ex }]|.
        vbeln = salesdocument_ex.
        data-vbeln = vbeln.
        fillmsg 'S' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ENDIF.
    WHEN 'U'.
      CLEAR:salesdocument,order_item_in,order_item_inx,schedule_lines,schedule_linesx,
      order_header,order_headerx,order_text,conditions_in,conditions_inx,subrc.
      REFRESH:
      order_item_in,order_item_inx,schedule_lines,schedule_linesx,
      order_text,conditions_in,conditions_inx.
      "补0
      PERFORM addzero(zpubform) CHANGING data-vbeln.
      SELECT
        vbak~vbeln
        vbap~posnr
        vbap~matnr
        vbap~werks
        vbap~pstyv
        vbkd~bstkd
        INTO CORRESPONDING FIELDS OF TABLE it_vbap
        FROM vbak
        JOIN vbap ON vbak~vbeln = vbap~vbeln
        JOIN vbkd ON vbak~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
        WHERE vbkd~bstkd = data-bstkd
        AND vbak~vbeln = data-vbeln
        AND vbak~vbtyp = 'G' "合同
        .
      IF sy-subrc NE 0.
        fillmsg 'E' '只允许创建或修改一个合同！' .
      ENDIF.
      READ TABLE it_vbap INDEX 1.
      "抬头
      salesdocument = data-vbeln.

      order_header-sales_org  = data-vkorg ."销售组织
      order_header-purch_no_c = data-bstkd ."外部合同号
      order_header-division   = data-spart ."产品组
      order_header-sales_grp  = data-vkgrp ."销售组
      order_header-distr_chan = data-vtweg ."分销渠道
      order_header-sales_off  = data-vkbur ."销售办事处
      order_header-currency   = data-waerk.
      order_header-doc_date   = data-prsdt."凭证日期 (接收/发送日期)
      order_header-incoterms1 = data-inco1."*  国际贸易条款（第 1 部分）
      order_header-incoterms2 = data-inco2_l."*  国际贸易条款（第 2 部分）
      order_header-price_date = data-prsdt.
      order_header-ct_valid_f = data-guebg.
      order_header-ct_valid_t = data-gueen.
      order_header-cust_group = data-kdgrp.
      PERFORM setbapix USING order_header CHANGING order_headerx.
      order_headerx-updateflag = 'U'.

*增强字段
      CLEAR wa_extk.
      MOVE-CORRESPONDING data TO wa_extk.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAK'.
      extensionin+30(960) = wa_extk.
      APPEND extensionin.
      CLEAR:extensionin.
      PERFORM setbapix USING wa_extk CHANGING wa_extkx.
      extensionin-structure = 'BAPE_VBAKX'.
      extensionin+30(960) = wa_extkx.
      APPEND extensionin.

      LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE data TO <zlongtext>.
        IF sy-subrc EQ 0.
          "切割文本
          CLEAR:text_stream,text_stream[],lines[],lines.
          text_stream-text = <zlongtext>.
          APPEND text_stream.
          CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
            TABLES
              text_stream = text_stream
              itf_text    = lines.
          LOOP AT lines.
            CLEAR:order_text.
            order_text-text_id    = <lt_ttxit>-tdid.
            order_text-langu      = sy-langu.
            order_text-format_col = '*' .
            order_text-text_line  = lines-tdline.
            APPEND order_text.
          ENDLOOP.
          UNASSIGN <zlongtext>.
        ENDIF.
      ENDLOOP.
**********************************
*      "合作伙伴
*      SALES_PARTNERS-PARTN_ROLE = 'AG'.
*      SALES_PARTNERS-PARTN_NUMB = data-KUNNR.
*      PERFORM ADDZERO(ZPUBFORM) CHANGING SALES_PARTNERS-PARTN_NUMB.
*      APPEND SALES_PARTNERS.
*      CLEAR SALES_PARTNERS.
**********************************
      "行项目
      "新增行取最大行项目
      SELECT MAX( vbap~posnr )
        INTO posnr
        FROM vbap
        WHERE vbeln = data-vbeln
        .

      SORT it_vbap BY bstkd posnr.
      LOOP AT data-items ASSIGNING <item> .
        CLEAR:order_item_in, order_item_inx, schedule_lines,schedule_linesx,wa_extp,wa_extpx.
        READ TABLE it_vbap WITH KEY bstkd = data-bstkd posnr = <item>-posnr BINARY SEARCH.
        IF sy-subrc = 0 .
          tabix = sy-tabix.
          "更新未被删除标志
          it_vbap-znodel = 'X'.
          MODIFY it_vbap INDEX tabix TRANSPORTING znodel.
          order_item_in-itm_number  = it_vbap-posnr.
          order_item_in-material    = <item>-matnr.
          order_item_in-target_qty  = <item>-kwmeng.
          order_item_in-target_qu   = <item>-vrkme.
          order_item_in-sales_unit  = <item>-vrkme.
          order_item_in-plant       = <item>-werks.
          order_item_in-currency    = data-waerk.
          PERFORM setbapix USING order_item_in CHANGING order_item_inx.
          order_item_inx-updateflag = 'U'.
          APPEND:order_item_in,order_item_inx.
          "计划行
          schedule_lines-itm_number = it_vbap-posnr.
          schedule_lines-sched_line = '0001'.
          schedule_lines-req_qty    = <item>-kwmeng.
          schedule_lines-req_date   = sy-datum .
          PERFORM setbapix USING schedule_lines CHANGING schedule_linesx.
          schedule_linesx-updateflag = 'U'.
          APPEND:schedule_lines,schedule_linesx.
*文本
          LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
            ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <item> TO <zlongtext>.
            IF sy-subrc EQ 0.
              "切割文本
              CLEAR:text_stream,text_stream[],lines[],lines.
              text_stream-text = <zlongtext>.
              APPEND text_stream.
              CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
                TABLES
                  text_stream = text_stream
                  itf_text    = lines.
              LOOP AT lines.
                CLEAR:order_text.
                order_text-itm_number = posnr.
                order_text-text_id    = <lt_ttxit>-tdid.
                order_text-langu      = sy-langu .
                order_text-format_col = '*' .
                order_text-text_line  = lines-tdline.
                APPEND order_text.
              ENDLOOP.
              UNASSIGN <zlongtext>.
            ENDIF.
          ENDLOOP.
*增强字段
          CLEAR:extensionin,wa_extp.
          MOVE-CORRESPONDING <item> TO wa_extp.
          extensionin-structure = 'BAPE_VBAP'.
          extensionin+30(960) = wa_extp.
          APPEND extensionin.
          CLEAR:extensionin,wa_extpx .
          PERFORM setbapix USING wa_extp CHANGING wa_extpx.
          extensionin-structure = 'BAPE_VBAPX'.
          extensionin+30(960) = wa_extpx.
          APPEND extensionin.

          SELECT SINGLE
            vbak~*
            INTO @DATA(vbak)
            FROM vbak
            WHERE vbak~vbeln = @data-vbeln.

          CLEAR:conditions_in,conditions_inx.
          PERFORM fillcond USING it_vbap-posnr 'ZPR0'  <item>-kbetr vbak
                CHANGING conditions_in conditions_inx.
          APPEND:conditions_in,conditions_inx.
        ELSE.
          IF <item>-posnr IS NOT INITIAL.
            posnr = <item>-posnr.
          ELSE.
            ADD 10 TO posnr.
          ENDIF.
          <item>-posnr = posnr.
          CLEAR:order_item_in, order_item_inx, schedule_lines,schedule_linesx,wa_extp,wa_extpx.
          order_item_in-itm_number  = <item>-posnr.
          order_item_in-material    = <item>-matnr.
          order_item_in-target_qty  = <item>-kwmeng.
          order_item_in-target_qu   = <item>-vrkme.
          order_item_in-sales_unit  = <item>-vrkme.
          order_item_in-plant       = <item>-werks.
          order_item_in-currency    = data-waerk.
          PERFORM setbapix USING order_item_in CHANGING order_item_inx.
          order_item_inx-updateflag  = 'I'.
          APPEND:order_item_in,order_item_inx.

          schedule_lines-itm_number = <item>-posnr.
          schedule_lines-sched_line = '0001'.
          schedule_lines-req_qty    = <item>-kwmeng.
          schedule_lines-req_date   = sy-datum .
          PERFORM setbapix USING order_item_in CHANGING order_item_inx.
          PERFORM setbapix USING schedule_lines CHANGING schedule_linesx.
          schedule_linesx-updateflag = 'I'.
          APPEND:schedule_lines,schedule_linesx.

*文本
          LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
            ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <item> TO <zlongtext>.
            IF sy-subrc EQ 0.
              "切割文本
              CLEAR:text_stream,text_stream[],lines[],lines.
              text_stream-text = <zlongtext>.
              APPEND text_stream.
              CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
                TABLES
                  text_stream = text_stream
                  itf_text    = lines.
              LOOP AT lines.
                CLEAR:order_text.
                order_text-itm_number = posnr.
                order_text-text_id    = <lt_ttxit>-tdid.
                order_text-langu      = sy-langu .
                order_text-format_col = '*' .
                order_text-text_line  = lines-tdline.
                APPEND order_text.
              ENDLOOP.
              UNASSIGN <zlongtext>.
            ENDIF.
          ENDLOOP.
*增强字段
          CLEAR:extensionin,wa_extp.
          MOVE-CORRESPONDING <item> TO wa_extp.
          extensionin-structure = 'BAPE_VBAP'.
          extensionin+30(960) = wa_extp.
          APPEND extensionin.
          CLEAR:extensionin,wa_extpx .
          PERFORM setbapix USING wa_extp CHANGING wa_extpx.
          extensionin-structure = 'BAPE_VBAPX'.
          extensionin+30(960) = wa_extpx.
          APPEND extensionin.

          "价格
          CLEAR:conditions_in,conditions_inx.
          conditions_in-itm_number = <item>-posnr.
          conditions_in-cond_type  = 'ZPR0'.
          conditions_in-cond_value =  <item>-kbetr .
          conditions_in-cond_p_unt = 1.
          conditions_in-currency   = data-waerk.
          PERFORM setbapix USING conditions_in CHANGING conditions_inx.
          conditions_inx-updateflag = 'I'.
          APPEND:conditions_in,conditions_inx.
        ENDIF.
      ENDLOOP.

      LOOP AT it_vbap WHERE znodel = ''.
        CLEAR:order_item_in,order_item_inx.
        order_item_in-itm_number  = it_vbap-posnr.
        PERFORM setbapix USING order_item_in CHANGING order_item_inx.
        order_item_inx-updateflag  = 'D'.
        APPEND:order_item_in,order_item_inx.
      ENDLOOP.
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header
          order_header_inx = order_headerx
        TABLES
          return           = return
          order_item_in    = order_item_in
          order_item_inx   = order_item_inx
          schedule_lines   = schedule_lines
          schedule_linesx  = schedule_linesx
          order_text       = order_text
          conditions_in    = conditions_in
          conditions_inx   = conditions_inx
          extensionin      = extensionin
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc NE 0.
        PERFORM msgtotext(zpubform) USING '' '' '' '' '' '' CHANGING rtmsg.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CONCATENATE return-message rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc NE 0 AND subrc EQ 0.
        SET UPDATE TASK LOCAL.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtmsg = |更改销售合同成功，单号：[{ salesdocument }]|.
        vbeln = salesdocument.
        fillmsg 'S' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ENDIF.
    WHEN 'D'.
      SELECT SINGLE
        vbak~vbeln
        INTO @vbeln
        FROM vbak
        JOIN vbkd ON vbak~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
        WHERE vbak~vbeln = @data-vbeln
        AND vbkd~bstkd = @data-bstkd
        .
      IF sy-subrc <> 0.
        fillmsg 'E' '未查到合同！' .
      ENDIF.
      salesdocument = vbeln.
      CLEAR:order_header_in, order_header_inx.
      order_header_in-collect_no = data-vbeln.
      order_header_inx-collect_no = data-vbeln.
      order_header_inx-updateflag  = 'D'.

*调用BAPI_SALESORDER_CHANGE bapi
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header_in
          order_header_inx = order_header_inx
        TABLES
          return           = return
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc NE 0.
        PERFORM msgtotext(zpubform) USING '' '' '' '' '' '' CHANGING rtmsg.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CONCATENATE return-message rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc = 0 OR subrc NE 0.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtmsg = |删除销售合同成功，单号：[{ salesdocument_ex }]|.
        fillmsg 'S' rtmsg.
      ENDIF.
    WHEN OTHERS.
      rtmsg = '不支持保存、新增、更新、删除以外其他功能'.
      fillmsg 'E' rtmsg.
  ENDCASE.


  zfmdatasave2 'R'.


ENDFUNCTION.


FORM fillcond USING p_posnr
                    p_kschl
*                    P_UNT
                    p_kbetr
                    p_vbak TYPE vbak
              CHANGING o_cond STRUCTURE bapicond
                       o_condx STRUCTURE bapicondx.
  TABLES:t683s.

  DATA:kalsm TYPE t683v-kalsm.
  CLEAR:o_cond,o_condx,t683s.
*  CHECK P_KSCHL IS NOT INITIAL
*  AND P_KBETR IS NOT INITIAL.

  PERFORM getkalsm(zpubform) USING p_vbak p_kschl
        CHANGING kalsm.

  SELECT SINGLE * FROM t683s
   WHERE kvewe = 'A'
     AND kappl = 'V'
     AND kalsm = kalsm
     AND kschl = p_kschl.
  o_cond-itm_number  = p_posnr.
  o_condx-itm_number = p_posnr.
  o_cond-cond_st_no  = t683s-stunr.
  o_condx-cond_st_no = t683s-stunr.
  o_cond-cond_count  = '01'.
  o_condx-cond_count = '01'.
  o_cond-cond_type   = p_kschl.
  o_condx-cond_type  = p_kschl.
  o_cond-cond_p_unt = '1'.
  o_condx-cond_p_unt = 'X'.
  o_cond-cond_value  = p_kbetr.
  o_condx-cond_value = 'X'.
*  O_COND-CONDVALUE   = P_KBETR * 10.
  o_cond-currency    = p_vbak-waerk.
  IF t683s IS NOT INITIAL.
    o_condx-updateflag = 'U'.
  ELSE.
    o_condx-updateflag = 'I'.
  ENDIF.
ENDFORM.


FORM setbapix USING fs CHANGING fsx    .
  FIELD-SYMBOLS : <fs>,<fsx>.
  DATA: outfieldcat TYPE slis_t_fieldcat_alv,
        wa_field    TYPE slis_fieldcat_alv.
  CLEAR:outfieldcat,wa_field,fsx.

  PERFORM gettabstru_se11(zpubform) USING fsx CHANGING outfieldcat.

  LOOP AT outfieldcat INTO wa_field.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fs TO <fs>.
    IF sy-subrc <> 0 .
      CONTINUE.
    ENDIF.
    IF <fs> IS NOT INITIAL.
      ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fsx TO <fsx>.
      IF sy-subrc <> 0 .
        EXIT.
      ENDIF.
*对于长度为1，但是不是赋值X的字段特殊处理
      IF wa_field-rollname = 'CHAR1' OR wa_field-rollname = 'BAPIUPDATE' .
        <fsx> = 'X'.
      ELSE.
        <fsx> = <fs>.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM setbapix_initial USING fs CHANGING fsx .

  FIELD-SYMBOLS : <fs>,<fsx>.
  DATA: outfieldcat TYPE slis_t_fieldcat_alv,
        wa_field    TYPE slis_fieldcat_alv.
  CLEAR:outfieldcat,wa_field,fsx.

  PERFORM gettabstru_se11(zpubform) USING fsx CHANGING outfieldcat.

  LOOP AT outfieldcat INTO wa_field.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fs TO <fs>.
    IF sy-subrc <> 0 .
      CONTINUE.
    ENDIF.
*    IF <FS> IS NOT INITIAL.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fsx TO <fsx>.
    IF sy-subrc <> 0 .
      EXIT.
    ENDIF.
*对于长度为1，但是不是赋值X的字段特殊处理
    IF wa_field-rollname = 'CHAR1' OR wa_field-rollname = 'BAPIUPDATE' .
      <fsx> = 'X'.
    ELSE.
      <fsx> = <fs>.
    ENDIF.
*    ENDIF.
  ENDLOOP.

ENDFORM.

FORM addunit USING matnr wsmei CHANGING message.

  DATA: headdata        TYPE bapimathead,
        return          TYPE bapiret2,
        clientdata      TYPE bapi_mara,
        clientdatax     TYPE bapi_marax,
        returnmessages  TYPE TABLE OF bapi_matreturn2 WITH HEADER LINE,
        unitsofmeasure  TYPE TABLE OF bapi_marm WITH HEADER LINE,
        unitsofmeasurex TYPE TABLE OF bapi_marmx WITH HEADER LINE.

  headdata-material = matnr.
  clientdata-uomusage = 'B'.
  clientdatax-uomusage = 'X'.
*维护比例产量单位
  IF wsmei IS NOT INITIAL.
    CLEAR:unitsofmeasure,unitsofmeasurex.
    unitsofmeasure-alt_unit = wsmei.
    unitsofmeasure-numerator = 1.
    unitsofmeasure-denominatr = 1.
    unitsofmeasurex-alt_unit = wsmei.
    unitsofmeasurex-numerator = 'X'.
    unitsofmeasurex-denominatr = 'X'.
    APPEND:unitsofmeasure,unitsofmeasurex.
  ENDIF.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata        = headdata
      clientdata      = clientdata
      clientdatax     = clientdatax
    IMPORTING
      return          = return
    TABLES
      returnmessages  = returnmessages
      unitsofmeasure  = unitsofmeasure
      unitsofmeasurex = unitsofmeasurex.

  LOOP AT returnmessages WHERE type CA 'AEX'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0 OR return-type CA 'AEX'.
    message = 'E'.
    LOOP AT returnmessages WHERE type CA 'AEX'.
      message = message && returnmessages-message && '/' .
    ENDLOOP.
    message = message && return-message && '/' .
    PERFORM bapirun(zpubform) USING ''.
  ELSE.
    SET UPDATE TASK LOCAL.
    PERFORM bapirun(zpubform) USING 'X'.
    message = 'S:成功'.
  ENDIF.

ENDFORM.
