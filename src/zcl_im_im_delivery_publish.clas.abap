class ZCL_IM_IM_DELIVERY_PUBLISH definition
  public
  final
  create public .

public section.

  interfaces IF_EX_DELIVERY_PUBLISH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_IM_DELIVERY_PUBLISH IMPLEMENTATION.


  METHOD if_ex_delivery_publish~publish_after_save.
*    FIELD-SYMBOLS:<v_likp> TYPE likp.
**    CHECK sy-tcode = 'VL01N' OR sy-tcode = 'VL02N'.
*    DATA(v_likp) = '(SAPMV50A)LIKP'.
*    ASSIGN (v_likp) TO <v_likp>.
*    CHECK <v_likp> IS ASSIGNED.
*
*    CALL FUNCTION 'ZFM_CRM_DN_SYNC' STARTING NEW TASK 'DN'
*      EXPORTING
*        v_likp = <v_likp>
** IMPORTING
**       RTYPE  =
**       RTMSG  =
**       CRM_RETURN       =
**       INSTR  =
**       OUTSTR =
**       STATUS =
**       MSG    =
** CHANGING
**       DATA   =
*      .
*    UNASSIGN <v_likp>.
  ENDMETHOD.


  METHOD if_ex_delivery_publish~publish_before_commit.
*    FIELD-SYMBOLS:<v_likp> TYPE likp.
**    CHECK sy-tcode = 'VL01N' OR sy-tcode = 'VL02N'.
*    DATA(v_likp) = '(SAPMV50A)LIKP'.
*    ASSIGN (v_likp) TO <v_likp>.
*    CHECK <v_likp> IS ASSIGNED.
*    CALL FUNCTION 'ZFM_CRM_DN_SYNC' STARTING NEW TASK 'DN'
*      EXPORTING
*        v_likp = <v_likp>
** IMPORTING
**       RTYPE  =
**       RTMSG  =
**       CRM_RETURN       =
**       INSTR  =
**       OUTSTR =
**       STATUS =
**       MSG    =
** CHANGING
**       DATA   =
*      .
*    UNASSIGN <v_likp>.
  ENDMETHOD.
ENDCLASS.
