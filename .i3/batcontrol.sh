#!/bin/bash

BATT_STATUS=`acpi --battery | awk '{print substr($4,1,3)}'`
AC_POWER_STATUS=`acpi --ac-adapter | awk '{print $3}'`

if [ "${BATT_STATUS}" = "" ]
then
    echo ""
else
    if [ "${AC_POWER_STATUS}" = "on-line" ]
    then
	echo " Charging" ${BATT_STATUS}
    else
	echo " Discharging" ${BATT_STATUS}
    fi
fi
