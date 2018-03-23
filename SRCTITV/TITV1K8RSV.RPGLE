000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090123     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('UBRTVNETA')
000300991027
000400081210     Ftivgd00f  o    e             disk    usropn
000500081210     F                                     extfile(LibFileVGD)
000600081210     F                                     infsr(*pssr)
000700991027
000800081210     D TIS781DS      E DS
000900081210     D TIS781DFLO    E DS
001000081210     D DVGDFLO       E DS
001100081210     D LibFileVGD      S             21A   inz
001200081210     D dataIso         S               D   datfmt(*iso)
001300081210     D dataEur         S               D   datfmt(*eur)
001400081120
001500081120
001600081120     D*------------------
001700081120     D* LINKING A DEFINIZIONI ESTERNE
001800081120     D*------------------
001900081120     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
002000081210     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
002100081120
002200081120
002300991027
002400081120     C*
002500081212     C                   if        pInOPZ = '1'
002600081210     C                   exsr      chkPAR
002700081212     C                   endif
002800081212     C*
002900081212     C                   if        pInOPZ = '2'
003000081212     C                   exsr      elabora
003100081212     C                   endif
003200081210     C*
003300921023     C                   SETON                                        LR
003400081212
003500081212
003600081212
003700081212     C     elabora       BEGSR
003800081212     C*
003900081212     C* Reperisco il sistema AS/400 corrente
004000081212     C                   callp     UBRTVNETA_Rtv(PiOutSysNeta)
004100081212     C                   if        %subst(PiOutSysNeta:1:6) = 'SETRAS'
004200081212     C                   eval      LibFileVGD = 'UNITRAGRU/TIVGD00F'
004300081212     C                   else
004400081212     C                   eval      LibFileVGD = 'UNITRAGRP/TIVGD00F'
004500081212     C                   endif
004600081212     C*
004700081212     C                   if        not %open(TIVGD00F)
004800081212     C                   open      TIVGD00F
004900081212     C                   endif
005000081212     C*
005100081212     C                   exsr      chkPAR
005200081212     C                   exsr      wriVGD
005300081212     C*
005400081212     C                   if        %open(TIVGD00F)
005500081212     C                   close     TIVGD00F
005600081212     C                   endif
005700081212     C*
005800081212     C                   ENDSR
005900081120
006000070625
006100070625
006200081210     C     chkPAR        BEGSR
006300030908     C*
006400081212     C                   if        %subst(%trim(pInPathFROM):
006500081212     C                                    %len(%trim(pInPathFROM)):1) <> '/'
006600081212     C                   eval      pInPathFROM = %trim(pInPathFROM) + '/'
006700081210     C                   endif
006800081212     C                   if        %subst(%trim(pInPathFROM):1:1) <> '/'
006900081212     C                   eval      pInPathFROM = '/' + %trim(pInPathFROM)
007000081210     C                   endif
007100081212     C*
007200081212     C                   if        %subst(%trim(pInPathIFS):
007300081212     C                                    %len(%trim(pInPathIFS)):1) <> '/'
007400081212     C                   eval      pInPathIFS = %trim(pInPathIFS) + '/'
007500081212     C                   endif
007600081212     C                   if        %subst(%trim(pInPathIFS):1:1) <> '/'
007700081212     C                   eval      pInPathIFS = '/' + %trim(pInPathIFS)
007800081212     C                   endif
007900081210     C*
008000081210     C                   if        %subst(%trim(pInExtIFStmp):1:1) <> '.'
008100081210     C                   eval      pInExtIFStmp = '.' + %trim(pInExtIFStmp)
008200081210     C                   endif
008300081210     C*
008400081210     C                   if        %subst(%trim(pInExtIFSend):1:1) <> '.'
008500081210     C                   eval      pInExtIFSend = '.' + %trim(pInExtIFSend)
008600081210     C                   endif
008700030908     C*
008800030908     C                   ENDSR
008900081120
009000081210
009100081120
009200081120
009300081212     C     wriVGD        BEGSR
009400081120     C*
009500081210     C* Preparo la scrittura del file TIVGD
009600081210     C                   clear                   tivgd000
009700081210     C                   eval      vgdTIP = pInTIP
009800081210     C                   eval      vgdKSU = pInKSC
009900081210     C                   eval      vgdTSC = 'WW'
010000081210     C                   eval      vgdDAT = datcor
010100081210     C                   eval      vgdPGM = 'TITV1K8R'
010200090116     C                   clear                   DVGDFLO
010300090123     C                   movel     'P'           §VGDFELA
010400090116     C                   movel     DVGDFLO       vgdFLO
010500081210     C*
010600081210     C* Inserimento 1° record con file origine
010700090123     C                   eval      vgdDTA = 'I'+
010800090123     C                                      %trim(pInPathFROM)+%trim(pInFilIFS)+
010900081210     C                                      %trim(pInExtIFStmp)
011000081210     C                   write     tivgd000
011100081210     C*
011200081210     C* Inserimento 2° record con file destinazione
011300090123     C                   eval      vgdDTA = 'O'+
011400090123     C                                      %trim(pInPathIFS)+%trim(pInFilIFS)+
011500081210     C                                      %trim(pInExtIFSend)
011600081210     C                   write     tivgd000
011700081210     C*
011800081210     C                   clear                   tis781ds
011900081210     C                   eval      §781tip = pInTIP
012000081210     C                   eval      §781ksu = pInKSC
012100081210     C                   eval      §781tsc = 'WW'
012200090123     C                   eval      §781dat = datcor
012300081210     C                   eval      §781pgm = 'TITV1K8R'
012400081210     C*
012500081210     C* Imposto la "S" per il cambio del S.I.
012600081210     C                   clear                   TIS781DFLO
012700081210     C                   movel     'S'           §781flocsi
012800090123     C                   movel     'P'           §781floela
012900081210     C                   movel     TIS781DFLO    §781flo
013000081210     C
013100081210     C* Finita l'elaborazione chiamo pgm
013200081210     C                   CALL      'TIS781C1'
013300081210     C                   parm      *blanks       wrkesito          1
013400081210     C                   parm                    tis781ds
013500081210     C                   parm      *blanks       vgdPRG
013600081120     C*
013700081120     C                   ENDSR
013800070625
013900081210
014000081210
014100081210
014200081210     C     *pssr         BEGSR
014300081210     C*
014400081210     C                   eval      wrkesito = '2'
014500081210     C*
014600081210     C                   seton                                        LR
014700081210     C*
014800081210     C                   ENDSR     '*CANCL'
014900081210     C***
015000081210
015100081210
015200070625
015300070625
015400991027
015500991027      /TITLE Operazioni iniziali.
015600991027     C     *inzsr        BEGSR
015700991027     C*
015800991027     C     *ENTRY        PLIST
015900081212     C                   parm                    pInOPZ            1
016000081210     C                   parm                    pInKSC            8
016100081210     C                   parm                    pInTIP            2
016200081212     C                   parm                    pInPathFROM     255
016300081210     C                   parm                    pInPathIFS      255
016400081210     C                   parm                    pInFilIFS       255
016500081210     C                   parm                    pInExtIFStmp    255
016600081210     C                   parm                    pInExtIFSend    255
016700081210     C     wrkesito      parm      wrkesito      pOutEsito         1
016800081210     C*
016900081210     C* Data di elaborazione
017000081210     C                   time                    w0140            14 0
017100081210     C                   move      w0140         w0080             8 0          *DATA (8) IN G/M/AA
017200081210     C     *eur          movel     w0080         dataeur
017300081210     C                   movel     dataeur       dataiso
017400081210     C     *iso          movel     dataiso       datcor            8 0
017500991027     C*
017600991027     C                   ENDSR
