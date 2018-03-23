000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130301     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090303     Ftitas30c  iF   E           K DISK
000600090303     Fwfdsc00f  uF A E             DISK    usropn
000700090303     F                                     extfile(LibFile)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300090303     D wfdscds       e ds                  extname(wfdsc00f)
001400090303     D LibFile         s             21    inz
001500130301
001600130301
001700130301     D*------------------
001800130301     D* LINKING A DEFINIZIONI ESTERNE
001900130301     D*------------------
002000130301     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002100130301     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002200090303
002300130219
002400991027
002500090303     C*
002600090303     C* Gestione parametri d esecuzione
002700090303     C*
002800090303     C* Primi 10 byte => libreria destinazione
002900090303     C                   eval      LibFile = %trim(%subst(prmppt:1:10))+'/'+
003000090303     C                                       'WFDSC00F'
003100090303     C*
003200090303     C* Apertura file
003300090303     C                   if        not %open(wfdsc00f)
003400090303     C                   open      wfdsc00f
003500090303     C                   endif
003600090303     C*
003700090303     C* Elaborazione
003800090303     C                   exsr      traduci
003900090303     C*
004000090303     C* Chiusura file
004100090303     C                   if        %open(wfdsc00f)
004200090303     C                   close     wfdsc00f
004300090303     C                   endif
004400090303     C*
004500921023     C                   SETON                                        LR
004600060505
004700090303
004800991027
004900991027     C     traduci       BEGSR
005000030325     C*
005100060410     C                   READ      TIVGDTMP
005200060410     C                   DOW       not %eof(TIVGDTMP)
005300060411     C*
005400060411     C* Valorizzo il buffer dal campo flat di inout
005500090303     C                   EVAL      wfdscds = vgdDTA
005600130301     C*
005700130301     C* Verifico se trattasi d evento derivato da bolla figlia
005800130301     C                   movel     *blanks       wEsito1           1
005900130301     C*
006000130301     C* Chiamata metodo GetLblTyp
006100130301     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
006200130301     C                                                dscAAS
006300130301     C                                               :dscLNP
006400130301     C                                               :dscNRS
006500130301     C                                               :dscNSP
006600130301     C                                               :pOutLblTyp
006700130301     C                                               :pOutAnnoBO
006800130301     C                                               :pOutLineaParBO
006900130301     C                                               :pOutSerieBO
007000130301     C                                               :pOutNumSpedBO
007100130301     C                                               :pOutDcmBO
007200130301     C                                               :pOutCcaBO
007300130301     C                                               :pOutRblBO))
007400130301     C*
007500130301     C                   if        wEsito1 = '0'
007600090303     C*
007700090303     C* Verifico se bolla corrente è della "famiglia" (rispetto al cliente pagante)
007800130301     C                   eval      tasAAS = pOutAnnoBO
007900130301     C                   eval      tasLNP = pOutLineaParBO
008000130301     C                   eval      tasNRS = pOutSerieBO
008100130301     C                   eval      tasNSP = pOutNumSpedBO
008200130301     C*
008300090511     C     KEYtas30_P    chain     titas30c
008400090511     C                   if        %found(titas30c)
008500130304     C                   eval      dscPDC = tasKSC
008600090303     C                   WRITE     wfdsc000
008700090303     C                   endif
008800130301     C*
008900130301     C                   endif
009000930409     C*
009100060410     C                   DELETE    TIVGD000
009200991027     C*
009300060410     C                   READ      TIVGDTMP
009400030325     C                   ENDDO
009500991027     C*
009600030325     C                   EVAL      wrkesito = '0'
009700991027     C*
009800910830     C                   ENDSR
009900060505
010000090303
010100090303
010200991027
010300991027      /TITLE Operazioni iniziali.
010400991027     C     *inzsr        BEGSR
010500991027     C*
010600991027     C     *ENTRY        PLIST
010700991027     C                   parm                    prmppt
010800991027     C     wrkesito      parm      wrkesito      prmesito
010900090303     C*
011000090303     C* Definizione chiavi
011100090303     C*
011200090303     C* TITAS30C - Parziale
011300090303     C     KEYtas30_P    KLIST
011400130301     C                   KFLD                    tasAAS
011500130301     C                   KFLD                    tasLNP
011600130301     C                   KFLD                    tasNRS
011700130301     C                   KFLD                    tasNSP
011800991027     C*
011900991027     C                   ENDSR
