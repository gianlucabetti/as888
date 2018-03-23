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
002500111228     D FNDCT_Save      s            300A   inz
002600110929
002700111216
002800111216     D*-----------
002900111216     D* PARAMETRI
003000111216     D*-----------
003100111220     D pInAnno         S              4S 0
003200111220     D pInLineaPar     S              3S 0
003300111220     D pInSerie        S              2S 0
003400111220     D pInNumSped      S              7S 0
003500111222     D pInTpChiamata   S              2S 0
003600111222     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
003700111216     D pOutEsito       S              1A
003800031103
003900110928
004000110928     D*------------------
004100110928     D* LINKING A DEFINIZIONI ESTERNE
004200110928     D*------------------
004300110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004400110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004500110928
004600030924
004700030325
004800030924     C* MAIN/
004900041015     C*
005000111222     C* Inizializza extra *inzsr
005100111222     C                   IF        Inz <> '1'
005200111222     C                   EXSR      Inizializza
005300111222     C                   EVAL      Inz = '1'
005400111222     C                   ENDIF
005500111222     C*
005600111228     C                   SELECT
005700111228     C* Tenta il reperimento per tipo chiamata 01
005800111228     C                   WHEN      pInTpChiamata = 1
005900111228     C                   EXSR      TentaRep01
006000110929     C*
006100111228     C                   ENDSL
006200111228     C*
006300111216     C                   SETON                                        RT
006400111216     C                   RETURN
006500031103
006600031103
006700031103
006800111222     C     Inizializza   BEGSR
006900111222     C*
007000111222     C* Carico tutti i rcd della tab. CCH
007100111222     C                   EVAL      TBELin = *blank
007200111222     C                   EVAL      TBECod = 'CCH'
007300111222     C     1             DO        50            $i
007400111222     C                   CLEAR                   Causali($i)
007500111222     C                   ENDDO
007600111222     C*
007700111222     C                   EVAL      $i = 0
007800111222     C     K02TBE11      SETLL     TNTBE11L
007900111222     C     K02TBE11      READE     TNTBE11L
008000111222     C*
008100111222     C                   DOW       not %eof(TNTBE11L)
008200111222     C                   MOVEL     TBEUni        DCCH
008300111222     C                   IF        §CCHInte = 'S'
008400111222     C                   EVAL      $i = $i + 1
008500111222     C                   EVAL      Causali($i)=%subst(TBEKe1:1:2)
008600111222     C                   ENDIF
008700111222     C     K02TBE11      READE     TNTBE11L
008800111222     C                   ENDDO
008900111222     C*
009000111222     C                   ENDSR
009100111222
009200111222
009300111228     C     TentaRep01    BEGSR
009400111216     C*
009500111222     C                   CLEAR                   pOutFNDCT_DS
009600111228     C                   EVAL      FNDCT_Save = pOutFNDCT_DS
009700111216     C                   EVAL      pOutEsito = '0'
009800110928     C*
009900111222     C                   if        not %open(FNDCT02L)
010000111222     C                   open      FNDCT02L
010100111216     C                   endif
010200111222     C*
010300111222     C     K04DCT02      SETLL     FNDCT02L
010400111222     C     K04DCT02      READE     FNDCT02L
010500111222     C*
010600111222     C                   DOW       not %eof(FNDCT02L)
010700111222     C* controllo che la riga non sia annullata
010800111228     C                   IF        DCTATB = *blank
010900111222     C* controllo che la causale sia usabile in internet
011000111222     C                   EVAL      $i = %lookup(DCTCCH : Causali)
011100111228     C* se il rcd è corretto, memorizzo i dati
011200111228     C                   IF        $i > 0
011300111222     C                   EVAL      pOutEsito = '1'
011400111228     C                   EVAL      FNDCT_Save = pOutFNDCT_DS
011500111222     C                   ENDIF
011600111222     C                   ENDIF
011700111222     C     K04DCT02      READE     FNDCT02L
011800111222     C                   ENDDO
011900031103     C*
012000111228     C* passo al chiamante i dati memorizzati
012100111228     C                   MOVEL     FNDCT_Save    pOutFNDCT_DS
012200111228     C*
012300031103     C                   ENDSR
012400031103
012500031103
012600991027
012700991027      /TITLE Operazioni iniziali.
012800991027     C     *inzsr        BEGSR
012900991027     C*
013000991027     C     *ENTRY        PLIST
013100111216     C                   PARM                    pInAnno
013200111216     C                   PARM                    pInLineaPar
013300111216     C                   PARM                    pInSerie
013400111216     C                   PARM                    pInNumSped
013500111222     C                   PARM                    pInTpChiamata
013600111222     C                   PARM                    pOutFNDCT_DS
013700111216     C                   PARM                    pOutEsito
013800111216     C*
013900111216     C* Reperisco il sistema AS/400 corrente
014000111216     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
014100111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
014200111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
014300111222     C                   eval      LibFileDCT02 = 'UNITRAGRU/FNDCT02L'
014400111216     C                   else
014500111222     C                   eval      LibFileDCT02 = 'UNITRAGRP/FNDCT02L'
014600111216     C                   endif
014700111222     C*
014800111222     C* Definizione chiavi
014900110929     C*
015000111222     C* Chiave su FNDCT02L
015100111222     C     K04DCT02      KLIST
015200111216     C                   KFLD                    pInAnno
015300111216     C                   KFLD                    pInLineaPar
015400111216     C                   KFLD                    pInSerie
015500111216     C                   KFLD                    pInNumSped
015600991027     C*
015700111222     C* Chiave su TNTBE11L parziale
015800111222     C     K02TBE11      KLIST
015900111222     C                   KFLD                    TBELin
016000111222     C                   KFLD                    TBECod
016100111222     C*
016200991027     C                   ENDSR
