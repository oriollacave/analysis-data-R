#!/bin/bash
for dir in `ls /home/oriol/WORK/superValidation/OBS/`;do
	if [ -f "/home/oriol/WORK/superValidation/OBS/$dir/data.dni.nc" ];then
		if [ -f "/home/oriol/OLD_WORK/ghi2dni/RUNS_FROM_SERVER/$dir/remod.RUIZ.nc" ];then
			aaa=" /home/oriol/OLD_WORK/ghi2dni/RUNS_FROM_SERVER/$dir/remod.RUIZ.nc nc DNI_syn DNI-RUIZ "
		fi
		if [ -f "/home/oriol/OLD_WORK/ghi2dni/RUNS_FROM_SERVER/$dir/remod.DISC.nc" ];then
			extrastr="$aaa /home/oriol/OLD_WORK/ghi2dni/RUNS_FROM_SERVER/$dir/remod.DISC.nc nc DNI_syn DNI-DISC "
		fi
		./compare-vortex-obs.sh $dir ghi-dni-andObs /home/oriol/WORK/superValidation/OBS/$dir/data.dni.nc nc DNI_real DNI-obs /home/oriol/WORK/superValidation/OBS/$dir/data.ghi.nc nc GHI_real GHI-obs /home/oriol/WORK/superValidation/RUNS_FROM_SERVER/$dir/remod.met.dni.nc nc DNI DNI-remod /home/oriol/WORK/superValidation/RUNS_FROM_SERVER/$dir/remod.met.ghi.nc nc GHI GHI-remod $extrastr
	fi
done
