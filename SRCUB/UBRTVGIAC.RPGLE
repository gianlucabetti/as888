000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200111216     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000300991027
000400111219      * per compilare aggiungere la lib. dati di filiale (se si è in un SI di sede) UNITRAGRP
000500111219      * o la lib. dati di sede (se si è in un SI di filiale) UNITRAGRU
000600111216
000700111216
000800111219     FTIGCP51L  IF   E           K DISK    usropn
000900111219     F                                     extfile(LibFileGCP51)
001000031103
001100041015     D*-----------
001200041015     D* VARIABILI D WRK
001300041015     D*-----------
001400111219     D LibFileGCP51    s             21A   inz
001500110929     D currSysNeta     s              8A   inz('*NULL')
001600110929
001700111216
001800111216     D*-----------
001900111216     D* PARAMETRI
002000111216     D*-----------
002100111220     D pInAnno         S              4S 0
002200111220     D pInLineaPar     S              3S 0
002300111220     D pInSerie        S              2S 0
002400111220     D pInNumSped      S              7S 0
002500111220     D pInNumProg      S              2S 0
002600111219     D pOutTIGCP_DS  E DS                  extname(TIGCP51L)
002700111216     D pOutEsito       S              1A
002800031103
002900110928
003000110928     D*------------------
003100110928     D* LINKING A DEFINIZIONI ESTERNE
003200110928     D*------------------
003300110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003400110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003500110928
003600030924
003700030325
003800030924     C* MAIN/
003900041015     C*
004000111216     C* Tenta il reperimento
004100111216     C                   EXSR      TentaReperim
004200110929     C*
004300111216     C                   SETON                                        RT
004400111216     C                   RETURN
004500031103
004600031103
004700031103
004800111216     C     TentaReperim  BEGSR
004900111216     C*
005000111219     C                   CLEAR                   pOutTIGCP_DS
005100111216     C                   EVAL      pOutEsito = '0'
005200110928     C*
005300111219     C                   if        not %open(TIGCP51L)
005400111219     C                   open      TIGCP51L
005500111216     C                   endif
005600111219     C     K05GCP51      chain     TIGCP51l
005700111219     C                   if        %found(TIGCP51l)
005800111219     C                   eval      pOutEsito = '1'
005900110929     C                   endif
006000031103     C*
006100031103     C                   ENDSR
006200031103
006300031103
006400991027
006500991027      /TITLE Operazioni iniziali.
006600991027     C     *inzsr        BEGSR
006700991027     C*
006800991027     C     *ENTRY        PLIST
006900111216     C                   PARM                    pInAnno
007000111216     C                   PARM                    pInLineaPar
007100111216     C                   PARM                    pInSerie
007200111216     C                   PARM                    pInNumSped
007300111219     C                   PARM                    pInNumProg
007400111219     C                   PARM                    pOutTIGCP_DS
007500111216     C                   PARM                    pOutEsito
007600111216     C*
007700111216     C* Reperisco il sistema AS/400 corrente
007800111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
007900111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
008000111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
008100111220     C                   eval      LibFileGCP51 = 'UNITRAGRU/TIGCP51L'
008200111216     C                   else
008300111220     C                   eval      LibFileGCP51 = 'UNITRAGRP/TIGCP51L'
008400111216     C                   endif
008500030709     C*
008600030709     C* Definizione chiavi
008700110929     C*
008800111219     C* Chiave su TIGCP51L parziale
008900111219     C     K05GCP51      KLIST
009000111216     C                   KFLD                    pInAnno
009100111216     C                   KFLD                    pInLineaPar
009200111216     C                   KFLD                    pInSerie
009300111216     C                   KFLD                    pInNumSped
009400111219     C                   KFLD                    pInNumProg
009500991027     C*
009600991027     C                   ENDSR
