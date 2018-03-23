000100160922       //==============================================================
000200160922       // Driver per reperimento dati spedizione
000300160922       //==============================================================
000400160922
000500160922       //--------------------------------------------------------------
000600160922       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160922       //--------------------------------------------------------------
000800160922
000900160922     /*PRM dbgview(*source)
001000160922     /*END
001100160922     **
001200160922     ** ISTRUZIONI PER LA COMPILAZIONE
001300160922
001400160922      * per compilare aggiungere la lib. dati di filiale (se si è in un SI di sede) FILTRAPRD
001500160922      * o la lib. dati di sede (se si è in un SI di filiale) GAITRAGRPS
001600160922
001700160922     **
001800160922     ** 1. Creare il modulo UBRTVSPE (Opz 15 PDM)
001900160922     ** 2. Creare/Aggiornare il programma di servizio UBRTVSPE (CRTSRVPGM / UPDSRVPGM)
002000160922     **    con ACTGRP(*caller) e BNDDIR=UBBNDDIR
002100160922     ** 3. Creare/Aggiornare il programma UBRTVSPE (CRTPGM / UPDPGM)
002200160922     **    con ACTGRP(*caller) e BNDDIR=UBBMDDIR
002300160922     ** 4. Cancellare il modulo.
002400160922     **
002500160922
002600160922       //--------------------------------------------------------------
002700160922       // Specifiche di controllo.
002800160922       //--------------------------------------------------------------
002900160922
003000160922     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003100111216
003200111216     FTITAS30C  IF   E           K DISK    usropn
003300111216     F                                     extfile(LibFileTAS30)
003400110929     FFNARB01L  IF   E           K DISK    usropn
003500110929     F                                     extfile(LibFileARB01)
003600110929     FFNBLP01L  IF   E           K DISK    usropn
003700110929     F                                     extfile(LibFileBLP01)
003800031103
003900041015     D*-----------
004000041015     D* VARIABILI D WRK
004100041015     D*-----------
004200110929     D LibFileARB01    s             21A   inz
004300110929     D LibFileBLP01    s             21A   inz
004400111216     D LibFileTAS30    s             21A   inz
004500110929     D currSysNeta     s              8A   inz('*NULL')
004600110929
004700110928
004800110928     D*------------------
004900110928     D* LINKING A DEFINIZIONI ESTERNE
005000110928     D*------------------
005100110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
005200110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
005300160922     D/COPY GAITRASRC/SRCPROTOPR,UBRTVSPE
005400160922     D/COPY GAITRASRC/SRCPROTOPI,UBRTVSPE
005500110928
005600030924
005700030325
005800030924     C* MAIN/
005900041015     C*
006000111216     C* Tenta il reperimento
006100111216     C                   EXSR      TentaReperim
006200110929     C*
006300111216     C                   SETON                                        RT
006400111216     C                   RETURN
006500031103
006600031103
006700031103
006800111216     C     TentaReperim  BEGSR
006900111216     C*
007000160922     C                   CLEAR                   oRTVSPE_TASDS
007100160922     C                   CLEAR                   oRTVSPE_ARBDS
007200160922     C                   CLEAR                   oRTVSPE_BLPDS
007300160922     C                   EVAL      oRTVSPE_Esito = *blank
007400110928     C*
007500111221     C* - richiesta filiale arrivo   (FNARB)
007600160922     C                   if        %subst(iRTVSPE_Opz:1:1) <> *blank
007700111221     C                   if        not %open(FNARB01L)
007800111216     C                   open      FNARB01L
007900111216     C                   endif
008000111216     C     K04TAS30      chain     fnarb01l
008100110929     C                   if        %found(fnarb01l)
008200160922     C                   eval      %subst(oRTVSPE_Esito:1:1) = 'A'
008300110929     C                   endif
008400111221     C                   endif
008500110928     C*
008600111221     C* - richiesta filiale partenza (FNBLP)
008700160922     C                   if        %subst(iRTVSPE_Opz:2:1) <> *blank
008800111216     C                   if        not %open(FNBLP01L)
008900111216     C                   open      FNBLP01L
009000111216     C                   endif
009100111216     C     K04TAS30      chain     fnblp01l
009200110929     C                   if        %found(fnblp01l)
009300160922     C                   eval      %subst(oRTVSPE_Esito:2:1) = 'P'
009400110929     C                   endif
009500111216     C                   endif
009600110928     C*
009700111221     C* - richiesta sede (TITAS)
009800160922     C                   if        %subst(iRTVSPE_Opz:3:1) <> *blank
009900111216     C                   if        not %open(TITAS30C)
010000111216     C                   open      TITAS30C
010100111216     C                   endif
010200111216     C     K04TAS30      chain     titas30c
010300110929     C                   if        %found(titas30c)
010400160922     C                   eval      %subst(oRTVSPE_Esito:3:1) = 'S'
010500110929     C                   endif
010600110929     C                   endif
010700031103     C*
010800031103     C                   ENDSR
010900031103
011000031103
011100991027
011200991027      /TITLE Operazioni iniziali.
011300991027     C     *inzsr        BEGSR
011400991027     C*
011500991027     C     *ENTRY        PLIST
011600160922     C                   PARM                    iRTVSPE_Opz
011700160922     C                   PARM                    iRTVSPE_AAS
011800160922     C                   PARM                    iRTVSPE_LNP
011900160922     C                   PARM                    iRTVSPE_NRS
012000160922     C                   PARM                    iRTVSPE_NSP
012100160922     C                   PARM                    oRTVSPE_TASDS
012200160922     C                   PARM                    oRTVSPE_ARBDS
012300160922     C                   PARM                    oRTVSPE_BLPDS
012400160922     C                   PARM                    oRTVSPE_Esito
012500111216     C*
012600111216     C* Reperisco il sistema AS/400 corrente
012700111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
012800111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
012900111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
013000111216     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
013100111216     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
013200111216     C                   eval      LibFileTAS30 = 'GAITRAGRU/TITAS30C'
013300111216     C                   else
013400111216     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
013500111216     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
013600111216     C                   eval      LibFileTAS30 = 'GAITRAGRPS/TITAS30C'
013700111216     C                   endif
013800030709     C*
013900030709     C* Definizione chiavi
014000110929     C*
014100111216     C* Chiave su FNARB01L / FNBLP01L  Completa / TITAS30C  parziale
014200111216     C     K04TAS30      KLIST
014300160922     C                   KFLD                    iRTVSPE_AAS
014400160922     C                   KFLD                    iRTVSPE_LNP
014500160922     C                   KFLD                    iRTVSPE_NRS
014600160922     C                   KFLD                    iRTVSPE_NSP
014700991027     C*
014800991027     C                   ENDSR
