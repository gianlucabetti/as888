000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130301     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090303     Ftitas30c  iF   E           K DISK
000600130417     Fwfdsc00f1 uF A E             DISK    usropn
000700090303     F                                     extfile(LibFile)
000800130417     FTITA430C  IF   E           K DISK
000900130417     FFNORM01L  IF   E           K DISK
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400030325
001500130417     D wfdscds       e ds                  extname(wfdsc00f)
001600130417     D dta4m         e ds
001700090303     D LibFile         s             21    inz
001800130301
001900130301
002000130301     D*------------------
002100130301     D* LINKING A DEFINIZIONI ESTERNE
002200130301     D*------------------
002300130301     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002400130301     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002500090303
002600130219
002700991027
002800090303     C*
002900090303     C* Gestione parametri d esecuzione
003000090303     C*
003100090303     C* Primi 10 byte => libreria destinazione
003200090303     C                   eval      LibFile = %trim(%subst(prmppt:1:10))+'/'+
003300130417     C                                       'WFDSC00F1'
003400090303     C*
003500090303     C* Apertura file
003600130417     C                   if        not %open(wfdsc00f1)
003700130417     C                   open      wfdsc00f1
003800090303     C                   endif
003900090303     C*
004000090303     C* Elaborazione
004100090303     C                   exsr      traduci
004200090303     C*
004300090303     C* Chiusura file
004400130417     C                   if        %open(wfdsc00f1)
004500130417     C                   close     wfdsc00f1
004600090303     C                   endif
004700090303     C*
004800921023     C                   SETON                                        LR
004900060505
005000090303
005100991027
005200991027     C     traduci       BEGSR
005300030325     C*
005400060410     C                   READ      TIVGDTMP
005500060410     C                   DOW       not %eof(TIVGDTMP)
005600060411     C*
005700060411     C* Valorizzo il buffer dal campo flat di inout
005800090303     C                   EVAL      wfdscds = vgdDTA
005900130301     C*
006000130301     C* Verifico se trattasi d evento derivato da bolla figlia
006100130301     C                   movel     *blanks       wEsito1           1
006200130301     C*
006300130301     C* Chiamata metodo GetLblTyp
006400130301     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
006500130301     C                                                dscAAS
006600130301     C                                               :dscLNP
006700130301     C                                               :dscNRS
006800130301     C                                               :dscNSP
006900130301     C                                               :pOutLblTyp
007000130301     C                                               :pOutAnnoBO
007100130301     C                                               :pOutLineaParBO
007200130301     C                                               :pOutSerieBO
007300130301     C                                               :pOutNumSpedBO
007400130301     C                                               :pOutDcmBO
007500130301     C                                               :pOutCcaBO
007600130301     C                                               :pOutRblBO))
007700130301     C*
007800130301     C                   if        wEsito1 = '0'
007900090303     C*
008000090303     C* Verifico se bolla corrente è della "famiglia" (rispetto al cliente pagante)
008100130301     C                   eval      tasAAS = pOutAnnoBO
008200130301     C                   eval      tasLNP = pOutLineaParBO
008300130301     C                   eval      tasNRS = pOutSerieBO
008400130301     C                   eval      tasNSP = pOutNumSpedBO
008500130417     C*
008600130417     C                   clear                   dscNO1
008700130417     C                   clear                   dscNO2
008800130417     C                   clear                   dscNAM
008900130417     C*
009000130417     C* Reperisco le note immesse sul ORM da internet
009100130417     C                   eval      ta4TRC = 'M'
009200130417     C     KEYta430_C    chain     tita430c
009300130417     C                   if        %found(tita430c)
009400130417     C                   eval      dta4m = ta4NOT
009500130417     C*
009600130417     C* Quindi aggancio l'orm legato
009700130417     C     KEYorm01_C    chain     fnorm01l
009800130417     C                   if        %found(fnorm01l)
009900130417     C                   eval      dscNO1 = ormNO1
010000130417     C                   eval      dscNO2 = ormNO2
010100130417     C                   eval      dscNAM = ormNAM
010200130417     C                   endif
010300130417     C                   endif
010400130301     C*
010500090511     C     KEYtas30_P    chain     titas30c
010600090511     C                   if        %found(titas30c)
010700130304     C                   eval      dscPDC = tasKSC
010800090303     C                   WRITE     wfdsc000
010900090303     C                   endif
011000130301     C*
011100130301     C                   endif
011200930409     C*
011300060410     C                   DELETE    TIVGD000
011400991027     C*
011500060410     C                   READ      TIVGDTMP
011600030325     C                   ENDDO
011700991027     C*
011800030325     C                   EVAL      wrkesito = '0'
011900991027     C*
012000910830     C                   ENDSR
012100060505
012200090303
012300090303
012400991027
012500991027      /TITLE Operazioni iniziali.
012600991027     C     *inzsr        BEGSR
012700991027     C*
012800991027     C     *ENTRY        PLIST
012900991027     C                   parm                    prmppt
013000991027     C     wrkesito      parm      wrkesito      prmesito
013100090303     C*
013200090303     C* Definizione chiavi
013300090303     C*
013400090303     C* TITAS30C - Parziale
013500090303     C     KEYtas30_P    KLIST
013600130301     C                   KFLD                    tasAAS
013700130301     C                   KFLD                    tasLNP
013800130301     C                   KFLD                    tasNRS
013900130301     C                   KFLD                    tasNSP
014000130417     C*
014100130417     C* Chiave su TITA430C - Completa
014200130417     C     KEYta430_C    KLIST
014300130417     C                   KFLD                    tasAAS
014400130417     C                   KFLD                    tasLNP
014500130417     C                   KFLD                    tasNRS
014600130417     C                   KFLD                    tasNSP
014700130417     C                   KFLD                    ta4TRC
014800130417     C*
014900130417     C* Chiave su FNORM01L - Completa
015000130417     C     KEYorm01_C    KLIST
015100130417     C                   KFLD                    §TA4MPOE
015200130417     C                   KFLD                    §TA4MNSR
015300130417     C                   KFLD                    §TA4MNOR
015400130417     C                   KFLD                    §TA4MNRV
015500991027     C*
015600991027     C                   ENDSR
