000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200111216     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000300991027
000400111216      * per compilare aggiungere la lib. dati di filiale (se si è in un SI di sede) FILTRAPRD
000500111216      * o la lib. dati di sede (se si è in un SI di filiale) GAITRAGRPS
000600111216
000700111216
000800111216     FTITAS30C  IF   E           K DISK    usropn
000900111216     F                                     extfile(LibFileTAS30)
001000110929     FFNARB01L  IF   E           K DISK    usropn
001100110929     F                                     extfile(LibFileARB01)
001200110929     FFNBLP01L  IF   E           K DISK    usropn
001300110929     F                                     extfile(LibFileBLP01)
001400031103
001500041015     D*-----------
001600041015     D* VARIABILI D WRK
001700041015     D*-----------
001800110929     D LibFileARB01    s             21A   inz
001900110929     D LibFileBLP01    s             21A   inz
002000111216     D LibFileTAS30    s             21A   inz
002100110929     D currSysNeta     s              8A   inz('*NULL')
002200110929
002300111216
002400111216     D*-----------
002500111216     D* PARAMETRI
002600111216     D*-----------
002700111221     D pInRichiesta    S              3A
002800111221     D pInAnno         S              4s 0
002900111221     D pInLineaPar     S              3s 0
003000111221     D pInSerie        S              2s 0
003100111221     D pInNumSped      S              7s 0
003200111216     D pOutTITAS_DS  E DS                  extname(TITAS30C)
003300111216     D pOutFNARB_DS  E DS                  extname(FNARB00F)
003400111216     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
003500111221     D pOutEsito       S              3A
003600031103
003700110928
003800110928     D*------------------
003900110928     D* LINKING A DEFINIZIONI ESTERNE
004000110928     D*------------------
004100110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004200110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004300110928
004400030924
004500030325
004600030924     C* MAIN/
004700041015     C*
004800111216     C* Tenta il reperimento
004900111216     C                   EXSR      TentaReperim
005000110929     C*
005100111216     C                   SETON                                        RT
005200111216     C                   RETURN
005300031103
005400031103
005500031103
005600111216     C     TentaReperim  BEGSR
005700111216     C*
005800111216     C                   CLEAR                   pOutFNARB_DS
005900111216     C                   CLEAR                   pOutFNBLP_DS
006000111216     C                   CLEAR                   pOutTITAS_DS
006100111221     C                   EVAL      pOutEsito = *blank
006200110928     C*
006300111221     C* - richiesta filiale arrivo   (FNARB)
006400111221     C                   if        %subst(pInRichiesta:1:1) <> *blank
006500111221     C                   if        not %open(FNARB01L)
006600111216     C                   open      FNARB01L
006700111216     C                   endif
006800111216     C     K04TAS30      chain     fnarb01l
006900110929     C                   if        %found(fnarb01l)
007000111221     C                   eval      %subst(pOutEsito:1:1) = 'A'
007100110929     C                   endif
007200111221     C                   endif
007300110928     C*
007400111221     C* - richiesta filiale partenza (FNBLP)
007500111221     C                   if        %subst(pInRichiesta:2:1) <> *blank
007600111216     C                   if        not %open(FNBLP01L)
007700111216     C                   open      FNBLP01L
007800111216     C                   endif
007900111216     C     K04TAS30      chain     fnblp01l
008000110929     C                   if        %found(fnblp01l)
008100111221     C                   eval      %subst(pOutEsito:2:1) = 'P'
008200110929     C                   endif
008300111216     C                   endif
008400110928     C*
008500111221     C* - richiesta sede (TITAS)
008600111221     C                   if        %subst(pInRichiesta:3:1) <> *blank
008700111216     C                   if        not %open(TITAS30C)
008800111216     C                   open      TITAS30C
008900111216     C                   endif
009000111216     C     K04TAS30      chain     titas30c
009100110929     C                   if        %found(titas30c)
009200111221     C                   eval      %subst(pOutEsito:3:1) = 'S'
009300110929     C                   endif
009400110929     C                   endif
009500031103     C*
009600031103     C                   ENDSR
009700031103
009800031103
009900991027
010000991027      /TITLE Operazioni iniziali.
010100991027     C     *inzsr        BEGSR
010200991027     C*
010300991027     C     *ENTRY        PLIST
010400111221     C                   PARM                    pInRichiesta
010500111221     C                   PARM                    pInAnno
010600111216     C                   PARM                    pInLineaPar
010700111216     C                   PARM                    pInSerie
010800111216     C                   PARM                    pInNumSped
010900111216     C                   PARM                    pOutFNARB_DS
011000111216     C                   PARM                    pOutFNBLP_DS
011100111216     C                   PARM                    pOutTITAS_DS
011200111216     C                   PARM                    pOutEsito
011300111216     C*
011400111216     C* Reperisco il sistema AS/400 corrente
011500111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
011600111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
011700111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
011800111216     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
011900111216     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
012000111216     C                   eval      LibFileTAS30 = 'GAITRAGRU/TITAS30C'
012100111216     C                   else
012200111216     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
012300111216     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
012400111216     C                   eval      LibFileTAS30 = 'GAITRAGRPS/TITAS30C'
012500111216     C                   endif
012600030709     C*
012700030709     C* Definizione chiavi
012800110929     C*
012900111216     C* Chiave su FNARB01L / FNBLP01L  Completa / TITAS30C  parziale
013000111216     C     K04TAS30      KLIST
013100111216     C                   KFLD                    pInAnno
013200111216     C                   KFLD                    pInLineaPar
013300111216     C                   KFLD                    pInSerie
013400111216     C                   KFLD                    pInNumSped
013500991027     C*
013600991027     C                   ENDSR
