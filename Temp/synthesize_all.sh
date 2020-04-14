#!/bin/bash

COMMAND=${1:-report}

[ -f "${QUARTUS_ROOTDIR}/adm/qenv.sh" ] && source "${QUARTUS_ROOTDIR}/adm/qenv.sh"
[ -d "${QSYS_ROOTDIR}" ] && export PATH="${QSYS_ROOTDIR}:${PATH}"

cd /home/jonas/Part_II_Project/blackcheck/work/CheckFPGA_MemAddr/
printf "\n\nMem Addr:\n"
make $COMMAND
quartus_dse SoCKitTop.qpf --report exploration_summary --report-format csv --report-file report.csv

printf "\n\nOne Hot:\n"
cd ../CheckFPGA_OneHot/
make $COMMAND
quartus_dse SoCKitTop.qpf --report exploration_summary --report-format csv --report-file report.csv

printf "\n\nSorter:\n"
cd ../CheckFPGA_Sorter/
make $COMMAND
quartus_dse SoCKitTop.qpf --report exploration_summary --report-format csv --report-file report.csv

printf "\n\nSums\n"
cd ../CheckFPGA_Sums/
make $COMMAND
quartus_dse SoCKitTop.qpf --report exploration_summary --report-format csv --report-file report.csv

printf "\n\nStack:\n"
cd ../CheckFPGA_Stack/
make $COMMAND
quartus_dse SoCKitTop.qpf --report exploration_summary --report-format csv --report-file report.csv

