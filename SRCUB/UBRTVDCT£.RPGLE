000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200111216     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*caller)
000300991027
000400111219      * per compilare aggiungere la lib. dati di filiale (se si è in un SI di sede) UNITRAGRP
000500111219      * o la lib. dati di sede (se si è in un SI di filiale) UNITRAGRU
000600111216
000700111216
000800111222     FFNDCT02L  IF   E           K DISK    usropn
000900111222     F                                     extfile(LibFileDCT02)
001000111222     FTNTBE11L  IF   E           K DISK
001100031103
001200041015     D*-----------
001300111222     D* DS
001400041015     D*-----------
001500111222     D DCCH          e ds
001600111222
001700111222     D*-----------
001800111222     D* VARIABILI D WRK
001900111222     D*-----------
002000111222     D LibFileDCT02    s             21A   inz
002100110929     D currSysNeta     s              8A   inz('*NULL')
002200111222     D Causali         s              2A   dim(50)
002300111222     D $i              s              3s 0
002400111222     D Inz             s              1A   inz
002500110929
002600111216
002700111216     D*-----------
002800111216     D* PARAMETRI
002900111216     D*-----------
003000111220     D pInAnno         S              4S 0
003100111220     D pInLineaPar     S              3S 0
003200111220     D pInSerie        S              2S 0
003300111220     D pInNumSped      S              7S 0
003400111222     D pInTpChiamata   S              2S 0
003500111222     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
003600111216     D pOutEsito       S              1A
003700031103
003800110928
003900110928     D*------------------
004000110928     D* LINKING A DEFINIZIONI ESTERNE
004100110928     D*------------------
004200110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004300110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004400110928
004500030924
004600030325
004700030924     C* MAIN/
004800041015     C*
004900111222     C* Inizializza extra *inzsr
005000111222     C                   IF        Inz <> '1'
005100111222     C                   EXSR      Inizializza
005200111222     C                   EVAL      Inz = '1'
005300111222     C                   ENDIF
005400111222     C*
005500111222     C* Tenta il reperimento
005600111222     C                   EXSR      TentaReperim
005700110929     C*
005800111216     C                   SETON                                        RT
005900111216     C                   RETURN
006000031103
006100031103
006200031103
006300111222     C     Inizializza   BEGSR
006400111222     C*
006500111222     C* Carico tutti i rcd della tab. CCH
006600111222     C                   EVAL      TBELin = *blank
006700111222     C                   EVAL      TBECod = 'CCH'
006800111222     C     1             DO        50            $i
006900111222     C                   CLEAR                   Causali($i)
007000111222     C                   ENDDO
007100111222     C*
007200111222     C                   EVAL      $i = 0
007300111222     C     K02TBE11      SETLL     TNTBE11L
007400111222     C     K02TBE11      READE     TNTBE11L
007500111222     C*
007600111222     C                   DOW       not %eof(TNTBE11L)
007700111222     C                   MOVEL     TBEUni        DCCH
007800111222     C                   IF        §CCHInte = 'S'
007900111222     C                   EVAL      $i = $i + 1
008000111222     C                   EVAL      Causali($i)=%subst(TBEKe1:1:2)
008100111222     C                   ENDIF
008200111222     C     K02TBE11      READE     TNTBE11L
008300111222     C                   ENDDO
008400111222     C*
008500111222     C                   ENDSR
008600111222
008700111222
008800111216     C     TentaReperim  BEGSR
008900111216     C*
009000111222     C                   CLEAR                   pOutFNDCT_DS
009100111216     C                   EVAL      pOutEsito = '0'
009200110928     C*
009300111222     C                   if        not %open(FNDCT02L)
009400111222     C                   open      FNDCT02L
009500111216     C                   endif
009600111222     C*
009700111222     C     K04DCT02      SETLL     FNDCT02L
009800111222     C     K04DCT02      READE     FNDCT02L
009900111222     C*
010000111222     C                   DOW       not %eof(FNDCT02L)
010100111222     C* controllo che la riga non sia annullata
010200111222     C                   IF        DCTATB = *blank
010300111222     C* controllo che la causale sia usabile in internet
010400111222     C                   EVAL      $i = %lookup(DCTCCH : Causali)
010500111222     C                   IF        $i = 0
010600111222     C                   CLEAR                   pOutFNDCT_DS
010700111222     C                   ELSE
010800111222     C                   EVAL      pOutEsito = '1'
010900111222     C                   ENDIF
011000111222     C                   ENDIF
011100111222     C     K04DCT02      READE     FNDCT02L
011200111222     C                   ENDDO
011300031103     C*
011400031103     C                   ENDSR
011500031103
011600031103
011700991027
011800991027      /TITLE Operazioni iniziali.
011900991027     C     *inzsr        BEGSR
012000991027     C*
012100991027     C     *ENTRY        PLIST
012200111216     C                   PARM                    pInAnno
012300111216     C                   PARM                    pInLineaPar
012400111216     C                   PARM                    pInSerie
012500111216     C                   PARM                    pInNumSped
012600111222     C                   PARM                    pInTpChiamata
012700111222     C                   PARM                    pOutFNDCT_DS
012800111216     C                   PARM                    pOutEsito
012900111216     C*
013000111216     C* Reperisco il sistema AS/400 corrente
013100111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
013200111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
013300111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
013400111222     C                   eval      LibFileDCT02 = 'UNITRAGRU/FNDCT02L'
013500111216     C                   else
013600111222     C                   eval      LibFileDCT02 = 'UNITRAGRP/FNDCT02L'
013700111216     C                   endif
013800111222     C*
013900111222     C* Definizione chiavi
014000110929     C*
014100111222     C* Chiave su FNDCT02L
014200111222     C     K04DCT02      KLIST
014300111216     C                   KFLD                    pInAnno
014400111216     C                   KFLD                    pInLineaPar
014500111216     C                   KFLD                    pInSerie
014600111216     C                   KFLD                    pInNumSped
014700991027     C*
014800111222     C* Chiave su TNTBE11L parziale
014900111222     C     K02TBE11      KLIST
015000111222     C                   KFLD                    TBELin
015100111222     C                   KFLD                    TBECod
015200111222     C*
015300991027     C                   ENDSR
