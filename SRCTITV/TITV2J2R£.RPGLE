000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500111104     FTIVGD00F  UF A E             DISK    commit prefix(O_)
000600111116     F                                     rename(tivgd000:tivgdout)
000700111104     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000800111104     F                                     PREFIX(f_)
000900111104     F                                     USROPN
001000030924
001100111104
001200111104     D*-----------
001300111104     D* DEFINIZIONE DS ESTERNE
001400111104     D*-----------
001500111104     D trul47ds      E DS
001600111104     D fnvac00t      e ds
001700111104
001800111104
001900111104     D*------------------
002000111104     D* RIDEFINIZIONE DS INPUT
002100111104     D*------------------
002200111117     D inISV           s              2    inz('OB')
002300111117     D inKSU           s              8    inz('01990023')
002400111117
002500111117
002600111117     D*------------------
002700111117     D* SCHIERE A PROGRAMMA
002800111117     D*------------------
002900111117     D output          S            100    DIM(11) CTDATA PERRCD(1)
003000071121
003100071121
003200071121     D*------------
003300071121     D* VARIABILI D WRK
003400071121     D*------------
003500030924     D prmppt          S             50
003600030924     D prmesito        S              1
003700030924     D wrkesito        S                   like(prmesito)
003800071211     D*
003900100910
004000100910
004100100910     D*------------------
004200100910     D* LINKING A DEFINIZIONI ESTERNE
004300100910     D*------------------
004400100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
004500100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004600111117     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004700111117     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004800100910
004900030924
005000030325
005100030924     C* MAIN/
005200111104     C*
005300111104     C* Avvio blocco elaborazione TIVGD
005400111104     C                   EXSR      strTRUL47
005500111104     C*
005600111104     C* Se ok a procedere => elaboro
005700111104     C                   if        wProcedi = 'S'
005800071121     C*
005900071121     C* Effettuo elaborazione
006000071121     C                   EXSR      traduci
006100111104     C*
006200111104     C                   endif
006300111104     C*
006400111104     C* Termino il blocco elaborazione TIVGD
006500111104     C                   EXSR      endTRUL47
006600071121     C*
006700921023     C                   SETON                                        LR
006800071121
006900071121
007000030924
007100991027     C     traduci       BEGSR
007200111116     C*
007300111104     C*
007400080924     C                   READ      TIVGDTMP
007500060119     C*
007600111104     C* Scorro tutto il file
007700080924     C                   DOW       not %eof(TIVGDTMP)
007800080924     C*
007900111104     C                   EVAL      fnvac00t = vgdDTA
008000100910     C*
008100100910     C* Solo bolle originali e valide ai fini della spedizione
008200100910     C                   movel     *blanks       wEsito1           1
008300100910     C*
008400100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
008500100910     C                                                vacAAS
008600100910     C                                               :vacLNP
008700100910     C                                               :vacNRS
008800100910     C                                               :vacNSP
008900100910     C                                               :pOutLblTyp
009000100910     C                                               :pOutAnnoBO
009100100910     C                                               :pOutLineaParBO
009200100910     C                                               :pOutSerieBO
009300100910     C                                               :pOutNumSpedBO
009400100910     C                                               :pOutDcmBO
009500100910     C                                               :pOutCcaBO
009600100910     C                                               :pOutRblBO))
009700100910     C*
009800111117     C* Considero solamente le bolle originali valide ai fini della spedizione
009900100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
010000111117     C*
010100111117     C* Ritorno solamente le consegnate OK
010200111117     C                   if        VACDCM > *zeros AND
010300111117     C                             (VACCCA = *blanks OR VACCCA = '1')
010400111117     C*
010500111117     C* Ritorno solamente se l'RMA è numerico lungo 10 byte
010600130208     C                   if        %len(%trim(vacRMA)) = 10
010700130208     C                   eval      PiStr=%trim(vacRMA)
010800130208     C                   exsr      CHKNUM
010900130208     C                   if        PiInt=*on
011000111104     C*
011100111117     C* Imposto i dati significativi
011200111117     C                   move(p)   vacAAS        wAAS              4
011300111117     C                   move(p)   vacLNP        wLNP              3
011400111117     C                   move(p)   vacNRS        wNRS              2
011500111117     C                   move(p)   vacNSP        wNSP              7
011600111117     C                   move(p)   datcor        wDATCRF           8
011700111117     C                   time                    wn14             14 0
011800111117     C                   movel(p)  wn14          wORACRF           6
011900111117     C                   movel     *blanks       wRMA             10
012000111117     C                   eval      wRMA = %trim(vacRMA)
012100111117     C                   move(p)   vacDCM        wDCM              8
012200111117     C                   move(p)   vacHMC        wHMC              4
012300111117     C*
012400111117     C* Infine compongo il record d output
012500111104     C                   EXSR      CALPROG
012600111117     C                   CLEAR                   TIVGDOUT
012700111117     C                   EVAL      O_VGDDTA =
012800111117     C                              %trim(output(01))+
012900120207     C***                           %trim(output(02))+wAAS+wLNP+wNRS+wNSP+
013000120207     C                              %trim(output(02))+
013100111117     C                              %trim(output(03))+
013200111117     C                              %trim(output(04))+
013300111117     C                              %trim(output(05))+
013400120207     C***                           %trim(output(06))+wDATCRF+
013500120207     C                              %trim(output(06))+
013600120207     C***                           %trim(output(07))+wORACRF+
013700120207     C                              %trim(output(07))+
013800111122     C                              %trim(output(08))+%trim(wRMA)+
013900111117     C                              %trim(output(09))+wDCM+
014000111117     C                              %trim(output(10))+wHMC+'00'+
014100111117     C                              %trim(output(11))
014200111104     C                   EXSR      EXEVGD
014300111117     C*
014400130208     C                   endif
014500130208     C                   endif
014600100910     C*
014700111117     C                   endif
014800111117     C*
014900100910     C                   endif
015000040719     C*
015100040719     C* In ogni caso storicizzo e cancello l'input sempre
015200080924     C                   DELETE    TIVGD000
015300040719     C*
015400040719     C* Proseguo con la lettura dell'input
015500080924     C                   READ      TIVGDTMP
015600060119     C*
015700030325     C                   ENDDO
015800991027     C*
015900030325     C                   EVAL      wrkesito = '0'
016000991027     C*
016100910830     C                   ENDSR
016200071121
016300111116
016400111116
016500111116     C     EXEVGD        BEGSR
016600111116     C*
016700111116     C* Preparo la scrittura del file TIVGD
016800111116     C                   eval      o_vgdtip = 'EC'
016900111117     C                   eval      o_vgdksu = inKSU
017000111116     C                   eval      o_vgdtsc = 'WW'
017100111116     C                   eval      o_vgddat = datcor
017200111116     C                   eval      o_vgdprg = f_tis7prgf
017300111117     C                   eval      o_vgdpgm = 'TITV2J2R'
017400111116     C*
017500111116     C                   write     tivgdout
017600111116     C*
017700111116     C                   ENDSR
017800111116
017900111116
018000111104
018100111104
018200111104     C     strTRUL47     BEGSR
018300111104     C*
018400111104     C                   movel     'N'           wProcedi          1
018500111104     C*
018600111122     C* Avvio il blocco elaborazione TIVGD x tipo file: 'EC'
018700111104     C                   clear                   trul47ds
018800111104     C                   eval      d47opz  = 'I'
018900111122     C                   eval      d47tip  = 'EC'
019000111104     C                   eval      d47lck  = 'N'
019100111104     C                   eval      d47chkj = 'S'
019200111104     C                   eval      d47pgm  = 'TITV2J0R'
019300111104     C                   call      'TRUL47R'
019400111104     C                   parm                    trul47ds
019500111104     C*
019600111104     C* Se elaborazione consentita => proseguo
019700111104     C                   if        d47sts <> 'A'
019800111104     C                   movel     'S'           wProcedi
019900111104     C                   endif
020000111104     C*
020100111104     C                   ENDSR
020200111104
020300111104
020400111104
020500111104     C     endTRUL47     BEGSR
020600111104     C*
020700111104     C                   if        wProcedi = 'S'
020800111104     C*
020900111122     C* Elimino il blocco elaborazione TIVGD x tipo file: 'EC'
021000111104     C                   clear                   trul47ds
021100111104     C                   eval      d47opz  = 'F'
021200111122     C                   eval      d47tip  = 'EC'
021300111104     C                   call      'TRUL47R'
021400111104     C                   parm                    trul47ds
021500111104     C*
021600111104     C                   endif
021700111104     C*
021800111104     C                   ENDSR
021900111104
022000111104
022100111104
022200111104      /TITLE Valorizzazione Progressivo Applicazione
022300111104     C     calprog       begsr
022400111104     C*
022500111104     C                   movel     *blanks       dwlisv            2
022600111104     C                   movel     *all'0'       dwlprg           10
022700111104     C                   z-add     *zeros        wrkprg            8 0
022800111104     C*
022900111116     C                   eval      dwlisv = inISV
023000111104     C*
023100111104     C                   open      tis7prgf
023200111104     C*
023300111104     C                   read(e)   tis7prgf
023400111104     C                   if        not %error
023500111104     C                   eval      dwlprg = f_tis7prgf
023600111104     C*
023700111104     C                   move(p)   dwlprg        wrkprg
023800111104     C                   add       1             wrkprg
023900111104     C                   move(p)   wrkprg        dwlprg
024000111104     C                   movel     dwlisv        dwlprg
024100111104     C*
024200111104     C                   eval      f_tis7prgf = dwlprg
024300111104     C                   update    tis7prg0
024400111104     C                   endif
024500111104     C*
024600111104     C                   close     tis7prgf
024700111104     C*
024800111104     C                   endsr
024900111117
025000111117
025100071121
025200111117     C*----------------------------------------------------*
025300111117     C*  CONTROLLO NUMERICITA' CAMPI
025400111117     C*----------------------------------------------------*
025500111117     C     CHKNUM        BEGSR
025600111117     C*
025700111117     C                   IF        PiDecChr = *blanks
025800111117     C                   EVAL      PiDecChr = '.'
025900111117     C                   ENDIF
026000111117     C*
026100111117     C                   callp(e)  UBISNUM_Check(PiStr
026200111117     C                                          :PiDecChr
026300111117     C                                          :PiVal
026400111117     C                                          :PiNum
026500111117     C                                          :PiInt)
026600111117     C*
026700111117     C                   IF        %error
026800111117     C                   EVAL      PiInt=*off
026900111117     C                   ENDIF
027000111117     C*
027100111117     C                   ENDSR
027200111117     C***
027300111117
027400071121
027500991027
027600991027      /TITLE Operazioni iniziali.
027700991027     C     *inzsr        BEGSR
027800991027     C*
027900991027     C     *ENTRY        PLIST
028000991027     C                   parm                    prmppt
028100991027     C     wrkesito      parm      wrkesito      prmesito
028200100910     C*
028300100910     C* CALCOLA LA DATA CORRENTE
028400100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
028500100910     C                   eval      datcor = %dec(%date() : *ISO)
028600991027     C*
028700991027     C                   ENDSR
028800071121     C*
028900111117** OUTPUT
029000120207<?xml version="1.0" encoding="UTF-8"?>
029100120207<DELVRY03><IDOC BEGIN="1"><EDI_DC40 SEGMENT="1">
029200120207<IDOCTYP>DELVRY03</IDOCTYP>
029300120613<MESTYP>STPPOD</MESTYP><SNDPOR>SAPBC</SNDPOR><SNDPRT>LS</SNDPRT>
029400120207<SNDPFC>EL</SNDPFC><SNDPRN>BARTOLISYS</SNDPRN>
029500120613<RCVPOR>SAPTCG</RCVPOR><RCVPRT>LS</RCVPRT><RCVPRN>TCG201</RCVPRN>
029600120207</EDI_DC40><E1EDL20 SEGMENT="1">
029700120207<VBELN>
029800111117</VBELN><PODAT>
029900111117</PODAT><POTIM>
030000111117</POTIM></E1EDL20></IDOC></DELVRY03>
