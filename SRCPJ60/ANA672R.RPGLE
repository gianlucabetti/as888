000001160715      // ***********************************************************************
000002160715      //
000003160715      // Questo programma sostituisce quello di Proj.
000004160715      //
000005160715      // Visualizza il documento IVA in DocFlow dal riepilogo registrazione
000006160715      // (opzione 21).
000007160715      //
000008160715      // ***********************************************************************
000009160715
000200160715      /COPY GAITRASRC/SRCCONST,BRTLEHSPEC
000201160715       CTL-OPT ACTGRP(*CALLER) BNDDIR('YCO');
000202160715
000203160715     D kpjba         E DS
000204161214     D xMsgDs        E DS                  QUALIFIED INZ
000205161214     D  msgEmV       E                     INZ('S')
000206161214     D  msgStp       E                     INZ('N')
000207161214     D  msgOpe       E                     INZ('N')
000208161214     D  msgVid       E                     INZ('N')
000209161214     D  msgLck       E                     INZ('N')
000210161214     D  msgCnl       E                     INZ('N')
000211161214     D  msgRsp       E                     INZ('S')
000212161214     D  msgTpR       E                     INZ('123')
000214161214     D  msgMs1       E                     INZ('Documento in DocFlow:')
000219160715     D KeyOld          DS
000220160715     D  KeyOldLgn                    15A   dim(8)
000221160715     D KeyNew          DS
000222160715     D KeyNewLgn                     15A   dim(8)
000224160715     D DesNew          DS
000225160715     D  DesNewLg                     44A   dim(10)
000228160715     D DesOld          DS
000229160715     D  DesOldLgn                    44A   dim(10)
000230161214     D SostOVers       S              1A
000231161214     D WDesOld         S                   like(DesOld)
000232161214     D Key2NewLgn      S             15A   dim(8)
000233161214     D msgTxt          S            135A   INZ('Vedi documento Metadati       -
000234161214     D                                     Uscire         ')
000235161214     D ClasseProJ      S              3A
000236161214     D nrAsReg         S              9P 0
000237161222     D rpyOpCode       S             10I 0
000238161222
000239161222      //************************************************************************
000240161222      //
000241161222      // Costanti.
000242161222      //
000243161222      //************************************************************************
000244161222
000245161222       /COPY GAITRASRC/SRCCONST,YCODF
000246161222
000247160715      //************************************************************************
000248160715      //
000249160715      // Prototipi.
000250160715      //
000251160715      //************************************************************************
000252160715
000253161214       /COPY GAITRASRC/SRCPROTOPR,XMSGR
000254161214       /COPY GAITRASRC/SRCPROTOPR,YCODF010R
000255161214       /COPY GAITRASRC/SRCPROTOPR,YDOCFLOW
000256160715
000257160715      // ***********************************************************************
000258160715      //
000259160715      // Parametri.
000260160715      //
000261160715      // ***********************************************************************
000262160715
000263160715     C     *ENTRY        PLIST
000264160715     C                   PARM                    kpjba
000265160715     C                   PARM                    ClasseProJ
000266160715     C                   PARM                    KeyOld
000267160715     C                   PARM                    KeyNew
000268160715     C                   PARM                    Key2NewLgn
000269160715     C                   PARM                    DesNew
000270160715     C                   PARM                    SostOVers
000271160715     C                   PARM                    WDesOld
000272160715
000273160721       IF key2newLgn(2) = *BLANK;
000274160721         RETURN;
000275160721       ENDIF;
000276160721
000277160721       MONITOR;
000278161214         nrAsReg = %DEC(key2newLgn(2) : 9 : 0);
000279160721         ON-ERROR;
000280160721           RETURN;
000281160721       ENDMON;
044602160715
044603161222       DOU *INLR;
044604161222
044606161222         RESET xMsgDs;
044607161222         xMsgDs.msgJba = kpjba;
044608161222         Proj_GestioneMessaggi(xMsgDs : msgTxt);
044609161214
044610161222         SELECT;
044611161222           WHEN xMsgDs.msgRtn = '1';
044612161222             DocFlow_DspDocumento( 0 : nrAsReg );
044613161222             *INLR = *ON;
044614161222             LEAVE;
044615161222           WHEN xMsgDs.msgRtn = '2';
044616161222             Ycodf010r(*ZERO : nrAsReg : rpyOpCode);
044617161222             IF rpyOpCode <> YCODF_RPYOPCODE_F12;
044618161222               *INLR = *ON;
044619161222               LEAVE;
044620161222             ENDIF;
044621161222           WHEN xMsgDs.msgRtn = '3';
044622161222             *INLR = *ON;
044623161222             LEAVE;
044624161222         ENDSL;
044625161222
044626161222       ENDDO;
044627161214
044628160715       RETURN;
