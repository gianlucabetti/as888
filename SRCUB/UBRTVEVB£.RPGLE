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
002700111216     D pInAnno         S                   like(TASAAS)
002800111216     D pInLineaPar     S                   like(TASLNP)
002900111216     D pInSerie        S                   like(TASNRS)
003000111216     D pInNumSped      S                   like(TASNSP)
003100111216     D pOutTITAS_DS  E DS                  extname(TITAS30C)
003200111216     D pOutFNARB_DS  E DS                  extname(FNARB00F)
003300111216     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
003400111216     D pOutEsito       S              1A
003500031103
003600110928
003700110928     D*------------------
003800110928     D* LINKING A DEFINIZIONI ESTERNE
003900110928     D*------------------
004000110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004100110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004200110928
004300030924
004400030325
004500030924     C* MAIN/
004600041015     C*
004700111216     C* Tenta il reperimento
004800111216     C                   EXSR      TentaReperim
004900110929     C*
005000111216     C                   SETON                                        RT
005100111216     C                   RETURN
005200031103
005300031103
005400031103
005500111216     C     TentaReperim  BEGSR
005600111216     C*
005700110929     C                   setoff                                       60
005800111216     C                   CLEAR                   pOutFNARB_DS
005900111216     C                   CLEAR                   pOutFNBLP_DS
006000111216     C                   CLEAR                   pOutTITAS_DS
006100111216     C                   EVAL      pOutEsito = '0'
006200110928     C*
006300110928     C* - 1° tentativo in filiale arrivo   (FNARB)
006400111216     C                   if        not %open(FNARB01L)
006500111216     C                   open      FNARB01L
006600111216     C                   endif
006700111216     C     K04TAS30      chain     fnarb01l
006800110929     C                   if        %found(fnarb01l)
006900110929     C                   seton                                        60
007000111216     C                   eval      pOutEsito = 'A'
007100110929     C                   endif
007200110928     C*
007300110928     C* - 2° tentativo in filiale partenza (FNBLP)
007400111216     C                   if        not *in60
007500111216     C                   if        not %open(FNBLP01L)
007600111216     C                   open      FNBLP01L
007700111216     C                   endif
007800111216     C     K04TAS30      chain     fnblp01l
007900110929     C                   if        %found(fnblp01l)
008000110929     C                   seton                                        60
008100111216     C                   eval      pOutEsito = 'P'
008200110929     C                   endif
008300111216     C                   endif
008400110928     C*
008500110928     C* - 3° tentativo in sede             (TITAS)
008600110929     C                   if        not *in60
008700111216     C                   if        not %open(TITAS30C)
008800111216     C                   open      TITAS30C
008900111216     C                   endif
009000111216     C     K04TAS30      chain     titas30c
009100110929     C                   if        %found(titas30c)
009200111216     C                   eval      pOutEsito = 'S'
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
010400111216     C                   PARM                    pInAnno
010500111216     C                   PARM                    pInLineaPar
010600111216     C                   PARM                    pInSerie
010700111216     C                   PARM                    pInNumSped
010800111216     C                   PARM                    pOutFNARB_DS
010900111216     C                   PARM                    pOutFNBLP_DS
011000111216     C                   PARM                    pOutTITAS_DS
011100111216     C                   PARM                    pOutEsito
011200111216     C*
011300111216     C* Reperisco il sistema AS/400 corrente
011400111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
011500111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
011600111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
011700111216     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
011800111216     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
011900111216     C                   eval      LibFileTAS30 = 'GAITRAGRU/TITAS30C'
012000111216     C                   else
012100111216     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
012200111216     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
012300111216     C                   eval      LibFileTAS30 = 'GAITRAGRPS/TITAS30C'
012400111216     C                   endif
012500030709     C*
012600030709     C* Definizione chiavi
012700110929     C*
012800111216     C* Chiave su FNARB01L / FNBLP01L  Completa / TITAS30C  parziale
012900111216     C     K04TAS30      KLIST
013000111216     C                   KFLD                    pInAnno
013100111216     C                   KFLD                    pInLineaPar
013200111216     C                   KFLD                    pInSerie
013300111216     C                   KFLD                    pInNumSped
013400991027     C*
013500991027     C                   ENDSR
