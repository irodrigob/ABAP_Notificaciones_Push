INTERFACE zif_push_data
  PUBLIC .
  TYPES: tt_users TYPE STANDARD TABLE OF zpush_e_user.
  TYPES: BEGIN OF ts_return,
           type    TYPE bapi_mtype,
           message TYPE string,
         END OF ts_return.
  TYPES: tt_return TYPE STANDARD TABLE OF ts_return WITH EMPTY KEY.
  TYPES: BEGIN OF ts_register_data,
           id_register TYPE zpush_t005-id_register,
           username    TYPE zpush_t005-username,
           hardware_id TYPE zpush_t005-hardware_id,
           device_id   TYPE zpush_t005-device_id,
           device_type TYPE zpush_t005-device_type,
         END OF ts_register_data.
  TYPES: tt_register_data TYPE STANDARD TABLE OF ts_register_data WITH EMPTY KEY.
  TYPES: BEGIN OF ts_notification,
           message TYPE string,
           badge   TYPE i,
           sound type string,
         END OF ts_notification.
  TYPES: tt_notification TYPE STANDARD TABLE OF ts_notification WITH EMPTY KEY.
  CONSTANTS: BEGIN OF cs_message,
               id TYPE arbgb VALUE 'ZPUSH',
               BEGIN OF type,
                 error  TYPE bapi_mtype VALUE 'E',
                 success TYPE bapi_mtype VALUE 'S',
               END OF type,
             END OF cs_message.
  CONSTANTS: cv_provider_interface TYPE vseoifimpl-refclsname VALUE ' ZIF_PUSH_PROVIDER'.

ENDINTERFACE.
