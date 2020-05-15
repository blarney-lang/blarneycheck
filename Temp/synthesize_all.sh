#!/bin/bash

COMMAND=${1:-}

[ -f "${QUARTUS_ROOTDIR}/adm/qenv.sh" ] && source "${QUARTUS_ROOTDIR}/adm/qenv.sh"
[ -d "${QSYS_ROOTDIR}" ] && export PATH="${QSYS_ROOTDIR}:${PATH}"

cd /home/jonas/Part_II_Project/blarneycheck/work/CheckFPGA_ActoraStack/
printf "ActoraStack:\n"
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

printf "\n\nBRAMStack:\n"
cd ../CheckFPGA_BRAMStack/
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

printf "\n\nCPU:\n"
cd ../CheckFPGA_CPU/
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

printf "\n\nFirstHot\n"
cd ../CheckFPGA_FirstHot/
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

printf "\n\nSorter\n"
cd ../CheckFPGA_Sorter/
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

printf "\n\nSums\n"
cd ../CheckFPGA_Sums/
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

printf "\n\nSums_Parallel:\n"
cd ../CheckFPGA_Sums_Parallel/
make $COMMAND
quartus_dse SoCKitTop.qpf --report fmax_summary --report-format csv --report-file report_fmax.csv
quartus_dse SoCKitTop.qpf --report utilization --report-format csv --report-file report_util.csv

