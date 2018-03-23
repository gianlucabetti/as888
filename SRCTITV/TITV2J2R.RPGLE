000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500111104     FTIVGD00F  UF A E             DISK    commit prefix(O_)
000600111116     F                                     rename(tivgd000:tivgdout)
000700111104     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
000800111104     F                                     PREFIX(f_)
000900030924
001000111104
001100111104     D*-----------
001200111104     D* DEFINIZIONE DS ESTERNE
001300111104     D*-----------
001400111104     D trul47ds      E DS
001500111104     D fnvac00t      e ds
001600111104
001700111104
001800111104     D*------------------
001900111104     D* RIDEFINIZIONE DS INPUT
002000111104     D*------------------
002100111117     D inISV           s              2    inz('OB')
002200111117     D inKSU           s              8    inz('01990023')
002300111117
002400111117
002500111117     D*------------------
002600111117     D* SCHIERE A PROGRAMMA
002700111117     D*------------------
002800111117     D output          S            100    DIM(11) CTDATA PERRCD(1)
002900071121
003000071121
003100071121     D*------------
003200071121     D* VARIABILI D WRK
003300071121     D*------------
003400030924     D prmppt          S             50
003500030924     D prmesito        S              1
003600030924     D wrkesito        S                   like(prmesito)
003700071211     D*
003800100910
003900100910
004000100910     D*------------------
004100100910     D* LINKING A DEFINIZIONI ESTERNE
004200100910     D*------------------
004300100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
004400100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004500111117     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004600111117     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004700100910
004800030924
004900030325
005000030924     C* MAIN/
005100111104     C*
005200111104     C* Avvio blocco elaborazione TIVGD
005300111104     C                   EXSR      strTRUL47
005400111104     C*
005500111104     C* Se ok a procedere => elaboro
005600111104     C                   if        wProcedi = 'S'
005700071121     C*
005800071121     C* Effettuo elaborazione
005900071121     C                   EXSR      traduci
006000111104     C*
006100111104     C                   endif
006200111104     C*
006300111104     C* Termino il blocco elaborazione TIVGD
006400111104     C                   EXSR      endTRUL47
006500071121     C*
006600921023     C                   SETON                                        LR
006700071121
006800071121
006900030924
007000991027     C     traduci       BEGSR
007100111116     C*
007200111104     C*
007300080924     C                   READ      TIVGDTMP
007400060119     C*
007500111104     C* Scorro tutto il file
007600080924     C                   DOW       not %eof(TIVGDTMP)
007700080924     C*
007800111104     C                   EVAL      fnvac00t = vgdDTA
007900100910     C*
008000100910     C* Solo bolle originali e valide ai fini della spedizione
008100100910     C                   movel     *blanks       wEsito1           1
008200100910     C*
008300100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
008400100910     C                                                vacAAS
008500100910     C                                               :vacLNP
008600100910     C                                               :vacNRS
008700100910     C                                               :vacNSP
008800100910     C                                               :pOutLblTyp
008900100910     C                                               :pOutAnnoBO
009000100910     C                                               :pOutLineaParBO
009100100910     C                                               :pOutSerieBO
009200100910     C                                               :pOutNumSpedBO
009300100910     C                                               :pOutDcmBO
009400100910     C                                               :pOutCcaBO
009500100910     C                                               :pOutRblBO))
009600100910     C*
009700111117     C* Considero solamente le bolle originali valide ai fini della spedizione
009800100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
009900111117     C*
010000111117     C* Ritorno solamente le consegnate OK
010100111117     C                   if        VACDCM > *zeros AND
010200111117     C                             (VACCCA = *blanks OR VACCCA = '1')
010300111117     C*
010400111117     C* Ritorno solamente se l'RMA è numerico lungo 10 byte
010500130208     C                   if        %len(%trim(vacRMA)) = 10
010600130208     C                   eval      PiStr=%trim(vacRMA)
010700130208     C                   exsr      CHKNUM
010800130208     C                   if        PiInt=*on
010900111104     C*
011000111117     C* Imposto i dati significativi
011100111117     C                   move(p)   vacAAS        wAAS              4
011200111117     C                   move(p)   vacLNP        wLNP              3
011300111117     C                   move(p)   vacNRS        wNRS              2
011400111117     C                   move(p)   vacNSP        wNSP              7
011500111117     C                   move(p)   datcor        wDATCRF           8
011600111117     C                   time                    wn14             14 0
011700111117     C                   movel(p)  wn14          wORACRF           6
011800111117     C                   movel     *blanks       wRMA             10
011900111117     C                   eval      wRMA = %trim(vacRMA)
012000111117     C                   move(p)   vacDCM        wDCM              8
012100111117     C                   move(p)   vacHMC        wHMC              4
012200111117     C*
012300111117     C* Infine compongo il record d output
012400111104     C                   EXSR      CALPROG
012500111117     C                   CLEAR                   TIVGDOUT
012600111117     C                   EVAL      O_VGDDTA =
012700111117     C                              %trim(output(01))+
012800120207     C***                           %trim(output(02))+wAAS+wLNP+wNRS+wNSP+
012900120207     C                              %trim(output(02))+
013000111117     C                              %trim(output(03))+
013100111117     C                              %trim(output(04))+
013200111117     C                              %trim(output(05))+
013300120207     C***                           %trim(output(06))+wDATCRF+
013400120207     C                              %trim(output(06))+
013500120207     C***                           %trim(output(07))+wORACRF+
013600120207     C                              %trim(output(07))+
013700111122     C                              %trim(output(08))+%trim(wRMA)+
013800111117     C                              %trim(output(09))+wDCM+
013900111117     C                              %trim(output(10))+wHMC+'00'+
014000111117     C                              %trim(output(11))
014100111104     C                   EXSR      EXEVGD
014200111117     C*
014300130208     C                   endif
014400130208     C                   endif
014500100910     C*
014600111117     C                   endif
014700111117     C*
014800100910     C                   endif
014900040719     C*
015000040719     C* In ogni caso storicizzo e cancello l'input sempre
015100080924     C                   DELETE    TIVGD000
015200040719     C*
015300040719     C* Proseguo con la lettura dell'input
015400080924     C                   READ      TIVGDTMP
015500060119     C*
015600030325     C                   ENDDO
015700991027     C*
015800030325     C                   EVAL      wrkesito = '0'
015900991027     C*
016000910830     C                   ENDSR
016100071121
016200111116
016300111116
016400111116     C     EXEVGD        BEGSR
016500111116     C*
016600111116     C* Preparo la scrittura del file TIVGD
016700111116     C                   eval      o_vgdtip = 'EC'
016800111117     C                   eval      o_vgdksu = inKSU
016900111116     C                   eval      o_vgdtsc = 'WW'
017000111116     C                   eval      o_vgddat = datcor
017100111116     C                   eval      o_vgdprg = f_tis7prgf
017200111117     C                   eval      o_vgdpgm = 'TITV2J2R'
017300111116     C*
017400111116     C                   write     tivgdout
017500111116     C*
017600111116     C                   ENDSR
017700111116
017800111116
017900111104
018000111104
018100111104     C     strTRUL47     BEGSR
018200111104     C*
018300111104     C                   movel     'N'           wProcedi          1
018400111104     C*
018500111122     C* Avvio il blocco elaborazione TIVGD x tipo file: 'EC'
018600111104     C                   clear                   trul47ds
018700111104     C                   eval      d47opz  = 'I'
018800111122     C                   eval      d47tip  = 'EC'
018900111104     C                   eval      d47lck  = 'N'
019000111104     C                   eval      d47chkj = 'S'
019100111104     C                   eval      d47pgm  = 'TITV2J0R'
019200111104     C                   call      'TRUL47R'
019300111104     C                   parm                    trul47ds
019400111104     C*
019500111104     C* Se elaborazione consentita => proseguo
019600111104     C                   if        d47sts <> 'A'
019700111104     C                   movel     'S'           wProcedi
019800111104     C                   endif
019900111104     C*
020000111104     C                   ENDSR
020100111104
020200111104
020300111104
020400111104     C     endTRUL47     BEGSR
020500111104     C*
020600111104     C                   if        wProcedi = 'S'
020700111104     C*
020800111122     C* Elimino il blocco elaborazione TIVGD x tipo file: 'EC'
020900111104     C                   clear                   trul47ds
021000111104     C                   eval      d47opz  = 'F'
021100111122     C                   eval      d47tip  = 'EC'
021200111104     C                   call      'TRUL47R'
021300111104     C                   parm                    trul47ds
021400111104     C*
021500111104     C                   endif
021600111104     C*
021700111104     C                   ENDSR
021800111104
021900111104
022000111104
022100111104      /TITLE Valorizzazione Progressivo Applicazione
022200111104     C     calprog       begsr
022300111104     C*
022400111104     C                   movel     *blanks       dwlisv            2
022500111104     C                   movel     *all'0'       dwlprg           10
022600111104     C                   z-add     *zeros        wrkprg            8 0
022700111104     C*
022800111116     C                   eval      dwlisv = inISV
022900111104     C*
023000170324     C     *start        setll     tis7prgf
023100111104     C                   read(e)   tis7prgf
023200111104     C                   if        not %error
023300111104     C                   eval      dwlprg = f_tis7prgf
023400111104     C*
023500111104     C                   move(p)   dwlprg        wrkprg
023600111104     C                   add       1             wrkprg
023700111104     C                   move(p)   wrkprg        dwlprg
023800111104     C                   movel     dwlisv        dwlprg
023900111104     C*
024000111104     C                   eval      f_tis7prgf = dwlprg
024100111104     C                   update    tis7prg0
024200170324     C                   else
024300170324     C                   eval      f_tis7prgf = 'XX00000000'
024400111104     C                   endif
024500111104     C*
024600111104     C                   endsr
024700111117
024800111117
024900071121
025000111117     C*----------------------------------------------------*
025100111117     C*  CONTROLLO NUMERICITA' CAMPI
025200111117     C*----------------------------------------------------*
025300111117     C     CHKNUM        BEGSR
025400111117     C*
025500111117     C                   IF        PiDecChr = *blanks
025600111117     C                   EVAL      PiDecChr = '.'
025700111117     C                   ENDIF
025800111117     C*
025900111117     C                   callp(e)  UBISNUM_Check(PiStr
026000111117     C                                          :PiDecChr
026100111117     C                                          :PiVal
026200111117     C                                          :PiNum
026300111117     C                                          :PiInt)
026400111117     C*
026500111117     C                   IF        %error
026600111117     C                   EVAL      PiInt=*off
026700111117     C                   ENDIF
026800111117     C*
026900111117     C                   ENDSR
027000111117     C***
027100111117
027200071121
027300991027
027400991027      /TITLE Operazioni iniziali.
027500991027     C     *inzsr        BEGSR
027600991027     C*
027700991027     C     *ENTRY        PLIST
027800991027     C                   parm                    prmppt
027900991027     C     wrkesito      parm      wrkesito      prmesito
028000100910     C*
028100100910     C* CALCOLA LA DATA CORRENTE
028200100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
028300100910     C                   eval      datcor = %dec(%date() : *ISO)
028400991027     C*
028500991027     C                   ENDSR
028600071121     C*
028700111117** OUTPUT
028800120207<?xml version="1.0" encoding="UTF-8"?>
028900120207<DELVRY03><IDOC BEGIN="1"><EDI_DC40 SEGMENT="1">
029000120207<IDOCTYP>DELVRY03</IDOCTYP>
029100120613<MESTYP>STPPOD</MESTYP><SNDPOR>SAPBC</SNDPOR><SNDPRT>LS</SNDPRT>
029200120207<SNDPFC>EL</SNDPFC><SNDPRN>BARTOLISYS</SNDPRN>
029300120613<RCVPOR>SAPTCG</RCVPOR><RCVPRT>LS</RCVPRT><RCVPRN>TCG201</RCVPRN>
029400120207</EDI_DC40><E1EDL20 SEGMENT="1">
029500120207<VBELN>
029600111117</VBELN><PODAT>
029700111117</PODAT><POTIM>
029800111117</POTIM></E1EDL20></IDOC></DELVRY03>
