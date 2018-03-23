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
000204160715     D ClasseProJ      S              3A
000205160715     D KeyOld          DS
000206160715     D  KeyOldLgn                    15A   dim(8)
000207160715     D KeyNew          DS
000208160715     D KeyNewLgn                     15A   dim(8)
000209160715     D Key2NewLgn      S             15A   dim(8)
000210160715     D DesNew          DS
000211160715     D  DesNewLg                     44A   dim(10)
000212160715     D SostOVers       S              1A
000213160715     D WDesOld         S                   like(DesOld)
000214160715     D DesOld          DS
000215160715     D  DesOldLgn                    44A   dim(10)
000216160715
000225160715       DCL-DS this QUALIFIED;
000226160715         registrazioneNumeroAssoluto PACKED(9);
000228160715       END-DS;
000229160715
000230160715      //************************************************************************
000231160715      //
000232160715      // Prototipi.
000233160715      //
000234160715      //************************************************************************
000235160715
000236160715       /COPY GAITRASRC/SRCPROTOPR,YDOCFLOW
000237160715
000238160715      // ***********************************************************************
000239160715      //
000240160715      // Parametri.
000241160715      //
000242160715      // ***********************************************************************
000243160715
000244160715     C     *ENTRY        PLIST
000245160715     C                   PARM                    kpjba
000246160715     C                   PARM                    ClasseProJ
000247160715     C                   PARM                    KeyOld
000248160715     C                   PARM                    KeyNew
000249160715     C                   PARM                    Key2NewLgn
000250160715     C                   PARM                    DesNew
000251160715     C                   PARM                    SostOVers
000252160715     C                   PARM                    WDesOld
000253160715
000254160721       IF key2newLgn(2) = *BLANK;
000255160721         RETURN;
000256160721       ENDIF;
000257160721
000258160721       MONITOR;
000259160721         this.registrazioneNumeroAssoluto = %DEC(key2newLgn(2):9:0);
000260160721         ON-ERROR;
000261160721           RETURN;
000262160721       ENDMON;
044602160715
044603160715       DocFlow_DspDocumento( 0 : this.registrazioneNumeroAssoluto );
044604160715
044605160715       RETURN;
