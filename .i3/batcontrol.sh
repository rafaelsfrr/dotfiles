#    This script get the battery status.
#
#    Copyright (C) 2015  Rafael Ferreira
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
#

#!/bin/bash

BATT_STATUS=`acpi --battery | awk '{print substr($4,1,3)}'`
AC_POWER_STATUS=`acpi --ac-adapter | awk '{print $3}'`

if [ "${BATT_STATUS}" = "" ]
then
    echo " AC "
else
    if [ "${AC_POWER_STATUS}" = "on-line" ]
    then
	echo " Charging" ${BATT_STATUS}
	if [ "${BATT_STATUS}" = "100%" ]
	then
	    notify-send  -t 5000 "Battery Charged"
	fi
    else
	echo " Discharging" ${BATT_STATUS}
	if [ "${BATT_STATUS}" = "20%" ]
	then
	    notify-send -u critical -t 5000 "Battery Running Low"
	fi
	
    fi
fi
