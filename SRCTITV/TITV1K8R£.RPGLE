000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090302     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090302     Ftivgd00f  o    e             disk
000500081210     F                                     infsr(*pssr)
000600100316     Fwas7ifs0f o    e             disk
000700090302     F                                     infsr(*pssr)
000800991027
000900090302     D wrkesito        S              1A   inz
001000081210     D dataIso         S               D   datfmt(*iso)
001100081210     D dataEur         S               D   datfmt(*eur)
001200090302     D currKey         S              6S 0 inz
001300090302
001400090302     D*------------------
001500090302     D* DS REPERIMENTO NUMERATORE
001600090302     D*------------------
001700090302     D trul33ds      e ds                  inz
001800090302     D*------------------
001900090302     D* DS ARCHITETTURA
002000090302     D*------------------
002100090302     D kpjba         e ds                  inz
002200090302
002300090302
002400090302     D* Informazioni sul job
002500090302     D psds           sds
002600090302     D  psjobname            244    253
002700090302     D  psuser               254    263
002800090302     D  psjobnbr             264    269s 0
002900081120
003000081120
003100991027
003200081120     C*
003300081212     C                   if        pInOPZ = '1'
003400090302     C                   exsr      repNUM
003500090302     C                   if        wrkesito <> '2'
003600081210     C                   exsr      chkPAR
003700100316     C                   exsr      wriWAS7IFS
003800090302     C                   exsr      wriVGD
003900090302     C                   endif
004000081212     C                   endif
004100081210     C*
004200921023     C                   SETON                                        LR
004300081212
004400090302
004500090302
004600090302     C     repNUM        BEGSR
004700090302     C*
004800090302     C                   clear                   TRUL33DS
004900090302     C                   eval      I33OPE = *zeros
005000090302     C                   eval      I33CNU = 302
005100090302     C                   eval      I33NUM = 1
005200090302     C                   movel     TRUL33DS      KPJBU
005300090302     C                   call      'TRUL33R'
005400090302     C                   parm                    KPJBA
005500090302     C                   movel     KPJBU         TRUL33DS
005600090302     C                   if        O33ERR = *zeros
005700090302     C                   z-add     O33NRF        currKey
005800090302     C                   else
005900090302     C                   eval      wrkesito = '2'
006000090302     C                   endif
006100090302     C*
006200090302     C                   ENDSR
006300090302
006400090302
006500090302
006600090302     C     chkPAR        BEGSR
006700090302     C*
006800090302     C                   if        %subst(%trim(pInPathFROM):
006900090302     C                                    %len(%trim(pInPathFROM)):1) <> '/'
007000090302     C                   eval      pInPathFROM = %trim(pInPathFROM) + '/'
007100090302     C                   endif
007200090302     C                   if        %subst(%trim(pInPathFROM):1:1) <> '/'
007300090302     C                   eval      pInPathFROM = '/' + %trim(pInPathFROM)
007400090302     C                   endif
007500090302     C*
007600090302     C                   if        %subst(%trim(pInPathIFS):
007700090302     C                                    %len(%trim(pInPathIFS)):1) <> '/'
007800090302     C                   eval      pInPathIFS = %trim(pInPathIFS) + '/'
007900090302     C                   endif
008000090302     C                   if        %subst(%trim(pInPathIFS):1:1) <> '/'
008100090302     C                   eval      pInPathIFS = '/' + %trim(pInPathIFS)
008200090302     C                   endif
008300090302     C*
008400090302     C                   if        %subst(%trim(pInExtIFStmp):1:1) <> '.'
008500090302     C                   eval      pInExtIFStmp = '.' + %trim(pInExtIFStmp)
008600090302     C                   endif
008700090302     C*
008800090302     C                   if        %subst(%trim(pInExtIFSend):1:1) <> '.'
008900090302     C                   eval      pInExtIFSend = '.' + %trim(pInExtIFSend)
009000090302     C                   endif
009100090302     C*
009200090302     C                   ENDSR
009300090302
009400090302
009500090302
009600100316     C     wriWAS7IFS    begsr
009700090302     C*
009800160314     C* Effettuo forzatura su gestione "particolare" nel caso di destinazione su \\CEDINTRA
009900160314     C                   if        %scan('/QNTC/CEDINTRA/':pInPathFROM) > *zeros
010000160314     C*
010100100316     C                   clear                   was7ifs0
010200090306     C                   eval      ifsins    = datcor*1000000+oracor
010300090306     C                   eval      ifskey    = %trim(psjobname)+'/'+
010400090306     C                                         %trim(psuser)  +'/'+
010500090306     C                                         %trim(%editc(psjobnbr:'Z'))+'_'+
010600090306     C                                         %trim(%char(currKey))
010700090306     C                   eval      ifspths   = %trim(pInPathFROM)
010800090311     C                   eval      ifsfils   = %trim(pInFilIFS)+
010900090306     C                                         %trim(pInExtIFStmp)
011000160314     C                   eval      ifspthd   = %trim(pInPathFROM)
011100090311     C                   eval      ifsfild   = %trim(pInFilIFS)+
011200090306     C                                         %trim(pInExtIFSend)
011300160314     C                   eval      ifsopz    = 'RENA'
011400090306     C                   eval      ifstrytot = 1000
011500100316     C                   write     was7ifs0
011600160314     C*
011700160314     C                   clear                   was7ifs0
011800160314     C                   eval      ifsins    = datcor*1000000+oracor
011900160314     C                   eval      ifskey    = %trim(psjobname)+'/'+
012000160314     C                                         %trim(psuser)  +'/'+
012100160314     C                                         %trim(%editc(psjobnbr:'Z'))+'_'+
012200160314     C                                         %trim(%char(currKey))
012300160314     C                   eval      ifspths   = %trim(pInPathFROM)
012400160314     C                   eval      ifsfils   = %trim(pInFilIFS)+
012500160314     C                                         %trim(pInExtIFSend)
012600160314     C                   eval      ifspthd   = %trim(pInPathIFS)
012700160314     C                   eval      ifsopz    = 'MOVE'
012800160314     C                   eval      ifstrytot = 1000
012900160314     C                   write     was7ifs0
013000160314     C*
013100160314     C                   else
013200160314     C                   clear                   was7ifs0
013300160314     C                   eval      ifsins    = datcor*1000000+oracor
013400160314     C                   eval      ifskey    = %trim(psjobname)+'/'+
013500160314     C                                         %trim(psuser)  +'/'+
013600160314     C                                         %trim(%editc(psjobnbr:'Z'))+'_'+
013700160314     C                                         %trim(%char(currKey))
013800160314     C                   eval      ifspths   = %trim(pInPathFROM)
013900160314     C                   eval      ifsfils   = %trim(pInFilIFS)+
014000160314     C                                         %trim(pInExtIFStmp)
014100160314     C                   eval      ifspthd   = %trim(pInPathIFS)
014200160314     C                   eval      ifsfild   = %trim(pInFilIFS)+
014300160314     C                                         %trim(pInExtIFSend)
014400160314     C                   eval      ifsopz    = 'MORE'
014500160314     C                   eval      ifstrytot = 1000
014600160314     C                   write     was7ifs0
014700160314     C                   endif
014800160314     C*
014900090302     C                   endsr
015000081120
015100081210
015200081120
015300081120
015400081212     C     wriVGD        BEGSR
015500081120     C*
015600081210     C* Preparo la scrittura del file TIVGD
015700081210     C                   clear                   tivgd000
015800081210     C                   eval      vgdTIP = pInTIP
015900081210     C                   eval      vgdKSU = pInKSC
016000081210     C                   eval      vgdTSC = 'WW'
016100081210     C                   eval      vgdDAT = datcor
016200081210     C                   eval      vgdPGM = 'TITV1K8R'
016300081210     C*
016400081210     C* Inserimento 1° record con file origine
016500090302     C                   eval      vgdDTA = 'K>'+ifskey
016600081210     C                   write     tivgd000
016700081210     C*
016800081210     C* Inserimento 2° record con file destinazione
016900090302     C                   eval      vgdDTA = 'F>'+
017000090123     C                                      %trim(pInPathIFS)+%trim(pInFilIFS)+
017100081210     C                                      %trim(pInExtIFSend)
017200081210     C                   write     tivgd000
017300081120     C*
017400081120     C                   ENDSR
017500081210
017600081210
017700081210
017800081210     C     *pssr         BEGSR
017900081210     C*
018000081210     C                   eval      wrkesito = '2'
018100081210     C*
018200081210     C                   seton                                        LR
018300081210     C*
018400081210     C                   ENDSR     '*CANCL'
018500081210     C***
018600081210
018700070625
018800991027
018900991027      /TITLE Operazioni iniziali.
019000991027     C     *inzsr        BEGSR
019100991027     C*
019200991027     C     *ENTRY        PLIST
019300081212     C                   parm                    pInOPZ            1
019400081210     C                   parm                    pInKSC            8
019500081210     C                   parm                    pInTIP            2
019600081212     C                   parm                    pInPathFROM     255
019700081210     C                   parm                    pInPathIFS      255
019800081210     C                   parm                    pInFilIFS       255
019900081210     C                   parm                    pInExtIFStmp    255
020000081210     C                   parm                    pInExtIFSend    255
020100081210     C     wrkesito      parm      wrkesito      pOutEsito         1
020200081210     C*
020300081210     C* Data di elaborazione
020400081210     C                   time                    w0140            14 0
020500081210     C                   move      w0140         w0080             8 0          *DATA (8) IN G/M/AA
020600090302     C                   movel     w0140         oracor            6 0          *ORA  (6) IN H/M/SS
020700081210     C     *eur          movel     w0080         dataeur
020800081210     C                   movel     dataeur       dataiso
020900090302     C     *iso          movel     dataiso       datcor            8 0
021000991027     C*
021100991027     C                   ENDSR
