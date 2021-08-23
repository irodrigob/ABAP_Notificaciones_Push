*&---------------------------------------------------------------------*
*& Report ZPUSH_R_SEND_PUSH
*&---------------------------------------------------------------------*
*& Descripción: Prueba de envio de mensajes push
*&---------------------------------------------------------------------*
REPORT zpush_r_send_push.


DATA mv_users TYPE zpush_e_user.

PARAMETERS: p_appl  TYPE zpush_t001-appl OBLIGATORY,
            p_msg   TYPE c LENGTH 80,
            p_badge TYPE i.
SELECT-OPTIONS: s_users FOR mv_users NO INTERVALS.


START-OF-SELECTION.

  TRY.
      DATA(lo_push) = NEW zcl_push_main( iv_appl = p_appl ).

      lo_push->send_push( EXPORTING it_users = VALUE #( FOR <wa> IN s_users ( <wa>-low ) )
                                it_notification = VALUE #( ( message = p_msg
                                                             badge = p_badge ) )
                      IMPORTING et_return = DATA(lt_return) ).

      IF lt_return IS NOT INITIAL.
        WRITE:/ 'Result: ', lt_return[ 1 ]-message.
      ELSE.
        WRITE:/ 'Unknow result'.
      ENDIF.

    CATCH zcx_push INTO DATA(lx_push).
      MESSAGE  ID lx_push->if_t100_message~t100key-msgid TYPE 'S' NUMBER lx_push->if_t100_message~t100key-msgno
               WITH lx_push->mv_msgv1 lx_push->mv_msgv2 lx_push->mv_msgv3 lx_push->mv_msgv4 INTO DATA(lv_msg).
      WRITE:/ 'Exception: ', lv_msg.
  ENDTRY.
