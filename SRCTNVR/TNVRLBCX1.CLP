000100000913 TNVRLBCX1:  PGM
000101000913             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000102000913             DCL        VAR(&CTL) TYPE(*CHAR) LEN(8)
000103000913             DCL        VAR(&CTLAS) TYPE(*CHAR) LEN(8)
000104000913             DCL        VAR(&IND) TYPE(*CHAR) LEN(12)
000105000913             RTVNETA    SYSNAME(&SYSNAME)
000106000913
000107000913             IF         COND(&SYSNAME *NE 'AS001') THEN(DO)
000108000913             CHGVAR     VAR(&CTL) VALUE('AS001F  ')
000109000913             CHGVAR     VAR(&CTLAS) VALUE('AS001   ')
000110000913             CHGVAR     VAR(&IND) VALUE('020010000000')
000200000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000201000913             ENDDO
000202000913             IF         COND(&SYSNAME *NE 'AS005') THEN(DO)
000203000913             CHGVAR     VAR(&CTL) VALUE('AS005F  ')
000204000913             CHGVAR     VAR(&CTLAS) VALUE('AS005   ')
000205000913             CHGVAR     VAR(&IND) VALUE('020050000000')
000206000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000208000913             ENDDO
000209000913             IF         COND(&SYSNAME *NE 'AS028') THEN(DO)
000210000913             CHGVAR     VAR(&CTL) VALUE('AS028F  ')
000211000913             CHGVAR     VAR(&CTLAS) VALUE('AS028   ')
000212000913             CHGVAR     VAR(&IND) VALUE('020280000000')
000213000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000215000913             ENDDO
000216000913             IF         COND(&SYSNAME *NE 'AS043') THEN(DO)
000217000913             CHGVAR     VAR(&CTL) VALUE('AS043F  ')
000218000913             CHGVAR     VAR(&CTLAS) VALUE('AS043   ')
000219000913             CHGVAR     VAR(&IND) VALUE('020430000000')
000220000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000222000913             ENDDO
000223000913             IF         COND(&SYSNAME *NE 'AS049') THEN(DO)
000224000913             CHGVAR     VAR(&CTL) VALUE('AS049F  ')
000225000913             CHGVAR     VAR(&CTLAS) VALUE('AS049   ')
000226000913             CHGVAR     VAR(&IND) VALUE('020490000000')
000227000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000229000913             ENDDO
000230000913             IF         COND(&SYSNAME *NE 'AS050') THEN(DO)
000231000913             CHGVAR     VAR(&CTL) VALUE('AS050F  ')
000232000913             CHGVAR     VAR(&CTLAS) VALUE('AS050   ')
000233000913             CHGVAR     VAR(&IND) VALUE('020500000000')
000234000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000236000913             ENDDO
000237000913             IF         COND(&SYSNAME *NE 'AS059') THEN(DO)
000238000913             CHGVAR     VAR(&CTL) VALUE('AS059F  ')
000239000913             CHGVAR     VAR(&CTLAS) VALUE('AS059   ')
000240000913             CHGVAR     VAR(&IND) VALUE('020590000000')
000241000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000243000913             ENDDO
000244000928             IF         COND(&SYSNAME *NE 'AS089') THEN(DO)
000245000928             CHGVAR     VAR(&CTL) VALUE('AS089F  ')
000246000928             CHGVAR     VAR(&CTLAS) VALUE('AS089   ')
000247000928             CHGVAR     VAR(&IND) VALUE('020890000000')
000248000928             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000249000928             ENDDO
000250000928             IF         COND(&SYSNAME *NE 'AS006') THEN(DO)
000251000928             CHGVAR     VAR(&CTL) VALUE('AS006F  ')
000252000928             CHGVAR     VAR(&CTLAS) VALUE('AS006   ')
000253000928             CHGVAR     VAR(&IND) VALUE('020060000000')
000254000928             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000255000928             ENDDO
000256000928             IF         COND(&SYSNAME *NE 'AS063') THEN(DO)
000257000928             CHGVAR     VAR(&CTL) VALUE('AS063F  ')
000258000928             CHGVAR     VAR(&CTLAS) VALUE('AS063   ')
000259000928             CHGVAR     VAR(&IND) VALUE('020630000000')
000260000928             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000261001110             ENDDO
000262001110             IF         COND(&SYSNAME *NE 'AS067') THEN(DO)
000263001110             CHGVAR     VAR(&CTL) VALUE('AS067F  ')
000264001110             CHGVAR     VAR(&CTLAS) VALUE('AS067   ')
000265001110             CHGVAR     VAR(&IND) VALUE('020670000000')
000266001110             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000267000928             ENDDO
000268001124             IF         COND(&SYSNAME *NE 'AS093') THEN(DO)
000269001124             CHGVAR     VAR(&CTL) VALUE('AS093F  ')
000270001124             CHGVAR     VAR(&CTLAS) VALUE('AS093   ')
000271001124             CHGVAR     VAR(&IND) VALUE('020930000000')
000272001124             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000273001124             ENDDO
000274001124             IF         COND(&SYSNAME *NE 'AS026') THEN(DO)
000275001124             CHGVAR     VAR(&CTL) VALUE('AS026F  ')
000276001124             CHGVAR     VAR(&CTLAS) VALUE('AS026   ')
000277001124             CHGVAR     VAR(&IND) VALUE('020260000000')
000278001124             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000279001124             ENDDO
000280001202             IF         COND(&SYSNAME *NE 'AS002') THEN(DO)
000281001202             CHGVAR     VAR(&CTL) VALUE('AS002F  ')
000282001202             CHGVAR     VAR(&CTLAS) VALUE('AS002   ')
000283001202             CHGVAR     VAR(&IND) VALUE('020020000000')
000284001202             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000285001202             ENDDO
000286001216             IF         COND(&SYSNAME *NE 'AS020') THEN(DO)
000287001216             CHGVAR     VAR(&CTL) VALUE('AS020F  ')
000288001216             CHGVAR     VAR(&CTLAS) VALUE('AS020   ')
000289001216             CHGVAR     VAR(&IND) VALUE('020200000000')
000290001216             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000291001216             ENDDO
000292001216             IF         COND(&SYSNAME *NE 'AS018') THEN(DO)
000293001216             CHGVAR     VAR(&CTL) VALUE('AS018F  ')
000294001216             CHGVAR     VAR(&CTLAS) VALUE('AS018   ')
000295001216             CHGVAR     VAR(&IND) VALUE('020180000000')
000296001216             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000297001216             ENDDO
000298001216             IF         COND(&SYSNAME *NE 'AS053') THEN(DO)
000299001216             CHGVAR     VAR(&CTL) VALUE('AS053F  ')
000300001216             CHGVAR     VAR(&CTLAS) VALUE('AS053   ')
000301001216             CHGVAR     VAR(&IND) VALUE('020530000000')
000302001216             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000303001216             ENDDO
000304001216             IF         COND(&SYSNAME *NE 'AS066') THEN(DO)
000305001216             CHGVAR     VAR(&CTL) VALUE('AS066F  ')
000306001216             CHGVAR     VAR(&CTLAS) VALUE('AS066   ')
000307001216             CHGVAR     VAR(&IND) VALUE('020660000000')
000308001216             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000309001216             ENDDO
000310010223             IF         COND(&SYSNAME *NE 'AS062') THEN(DO)
000311010223             CHGVAR     VAR(&CTL) VALUE('AS062F  ')
000312010223             CHGVAR     VAR(&CTLAS) VALUE('AS062   ')
000313010223             CHGVAR     VAR(&IND) VALUE('020620000000')
000314010223             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000315010223             ENDDO
000316010223             IF         COND(&SYSNAME *NE 'AS032') THEN(DO)
000317010223             CHGVAR     VAR(&CTL) VALUE('AS032F  ')
000318010223             CHGVAR     VAR(&CTLAS) VALUE('AS032   ')
000319010223             CHGVAR     VAR(&IND) VALUE('020320000000')
000320010223             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000321010223             ENDDO
000322010223             IF         COND(&SYSNAME *NE 'AS031') THEN(DO)
000323010223             CHGVAR     VAR(&CTL) VALUE('AS031F  ')
000324010223             CHGVAR     VAR(&CTLAS) VALUE('AS031   ')
000325010223             CHGVAR     VAR(&IND) VALUE('020310000000')
000326010223             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000327010223             ENDDO
000328010309             IF         COND(&SYSNAME *NE 'AS082') THEN(DO)
000329010309             CHGVAR     VAR(&CTL) VALUE('AS082F  ')
000330010309             CHGVAR     VAR(&CTLAS) VALUE('AS082   ')
000331010309             CHGVAR     VAR(&IND) VALUE('020820000000')
000332010309             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000333010309             ENDDO
000334010323             IF         COND(&SYSNAME *NE 'AS007') THEN(DO)
000335010323             CHGVAR     VAR(&CTL) VALUE('AS007F  ')
000336010323             CHGVAR     VAR(&CTLAS) VALUE('AS007   ')
000337010323             CHGVAR     VAR(&IND) VALUE('020070000000')
000338010323             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000339010323             ENDDO
000340011005             IF         COND(&SYSNAME *NE 'AS015') THEN(DO)
000341011005             CHGVAR     VAR(&CTL) VALUE('AS015F  ')
000342011005             CHGVAR     VAR(&CTLAS) VALUE('AS015   ')
000343011005             CHGVAR     VAR(&IND) VALUE('020150000000')
000344011005             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000345011005             ENDDO
000346000913             CHGVAR     VAR(&CTL) VALUE('SETRASF ')
000347000913             CHGVAR     VAR(&CTLAS) VALUE('SETRAS  ')
000348000913             CHGVAR     VAR(&IND) VALUE('020460000000')
000349000913             CALL       PGM(TNVRLBCX2) PARM(&CTL &IND &CTLAS)
000350010309
000351010309             CHGPRTF    FILE(QSYSPROGET/QSYSPRT) PRTQLTY(*DEVD)
000352010309             CHGPRTF    FILE(QSYSPROGET/SYSPRT) PRTQLTY(*DEVD)
001400970607             ENDPGM
