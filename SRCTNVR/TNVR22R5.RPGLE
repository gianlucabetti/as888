000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100408     H* TNVR22R3 *----------------------------------------------------*
000300100408     H* visite con progressivo
000400000000     H*--------------------------------------------------------------*
000500100407     FTNVIS06L  IF   E           K DISK
000600100408     FTNVIS05L  IF   E           K DISK    rename(tnvis000:tnvis005)
000700100408     f                                     prefix(w:1)
000800020111      * Anagrafiche
000900100407     FTFACO00F  iF   E           K DISK
001000100407     FTFIND00F  iF   E           K DISK
001100100407     FTFCLP00F  iF   E           K DISK
001200020111      * Offerte
001300041202     FTNOFM01L  iF   E           K DISK
001400041202     F                                     INFDS(ofm1)
001500041202     FTIOFD01L  iF   E           K DISK
001600041202     F                                     INFDS(ofd1)
001700041202     FTIOPT01L  iF   E           K DISK
001800041202     F                                     INFDS(opt1)
001900041202     FTIOPD01L  iF   E           K DISK
002000041202     F                                     INFDS(opd1)
002100041202     FTIOGC01L  iF   E           K DISK
002200041202     F                                     INFDS(ogc1)
002300041202
002400100407     FTNOFM00f  iF   E             DISK    rename(tntam000:tnofm000)
002500100407     FTIOFD00f  iF   E             DISK    rename(titad000:tiofd000)
002600100407     FTIOPT00f  iF   E             DISK    rename(titpt000:tiopt000)
002700100407     FTIOPD00f  iF   E             DISK    rename(titpd000:tiopd000)
002800100407     FTIOGC00f  iF   E             DISK    rename(titgc000:tiogc000)
002900100407     FTFNTC00f  iF   E             DISK    rename(tfntc: tfntc000)
003000020111      * Note
003100041202     FTFNTC01L  iF   E           K DISK
003200041206     F                                     INFDS(ntc1)
003300020111      * Cat
003400041202     FTEETC01L  iF   E           K DISK
003500041202     FTEETS01L  iF   E           K DISK
003600041202     FTEETT01L  iF   E           K DISK
003700020325      *
003800020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003900020325      *
004000020910     D tibs34ds      E DS                  inz
004100020910     D ddatiute      e ds
004200020910     D azuteds       e ds                  extname(AZUTE00F)
004300020910      *
004400020325     D$Totale          s              5  0 inz
004500020910     D$TotaleA         s              5  0 inz
004600100408     Dknrv             s                   like(visnrv)
004700100408     Dvisita           s                   like(visnrv)
004800041202     D ofm1            DS
004900041202     D  ofmNRR               397    400B 0
005000041202     D ofd1            DS
005100041202     D  ofdNRR               397    400B 0
005200041202     D opt1            DS
005300041202     D  optNRR               397    400B 0
005400041202     D opd1            DS
005500041202     D  opdNRR               397    400B 0
005600041202     D ogc1            DS
005700041202     D  ogcNRR               397    400B 0
005800041206     D ntc1            DS
005900041206     D  ntcNRR               397    400B 0
006000921027     C*****************************************************************
006100020325      *
006200020910     c     *dtaara       define    §azute        azuteds
006300020910     c     *dtaara       define    §datiute      ddatiute
006400020910     C                   in(E)     *dtaara
006500020910     C                   IF        %error  or RSUT = *blanks
006600020910     C                   CLEAR                   tibs34ds
006700020910     C                   CALL      'TIBS34R'
006800020910     C                   PARM                    tibs34ds
006900020910     C                   in        *dtaara
007000020910     c                   endif
007100100407     c
007200100407     C                   Z-ADD     1             CODUT             1 0
007300100408     c                   except    tesvis
007400100407
007500100407     c
007600100407     C     *LOVAL        SETLL     tnvis06l
007700100407     C                   READ(N)   tnvis06l                               30
007800100407      * 30 on - fine lettura
007900100407    1C     *IN30         DOWEQ     '0'
008000100408     c* Leggo solo le visite fittizie prima del 20070101
008100100408     c                   move      visnrv        w0010             1 0
008200100408     c                   clear                   trova             1
008300100408     c
008400100408    2c                   if        w0010>0
008500100408     c* Cerco altre visite con progr.diverso
008600100408     c                   movel     visnrv        knrv
008700100408     c                   move      0             knrv
008800100408     c     knrv          setll     tnvis05l
008900100408     c                   read      tnvis05l
009000100408     c
009100100408    3c                   dow       not %eof(tnvis05l)
009200100408     c                   movel     wisnrv        visita
009300100408     c                   move      0             knrv
009400100408     c                   if        visita =knrv
009500100408     c* se i 2 commerciali non corrispoondono, memorizzo qualte sono
009600100408     c                   if        viscmm<>wiscmm
009700100408     c                   seton                                        01
009800100408     c                   add       1             divcmm            7 0
009900100408     c                   endif
010000100408     c*
010100100408     c                   except    stav
010200100408     c                   eval      trova='S'
010300100408     c                   setoff                                       01
010400100408     c                   else
010500100408     c                   leave
010600100408     c                   endif
010700100408     c
010800100408     c                   read      tnvis05l
010900100408    3c                   enddo
011000100408     c
011100100408     c* e non trovata lo stampo
011200100408     c                   if        trova=' '
011300100408     c                   except    noaltri
011400100408     c                   endif
011500100408    1c                   endif
011600100408     c
011700100408     C                   READ(N)   tnvis06l                               30
011800100408    3c                   enddo
011900020114      *
012000100408     c                   except    fine
012100020114     C                   SETON                                        LR
012200020114      *
012300020325     OQSYSPRT   E            TESVIS         2 02
012400100408     O                                         + 10 'Controllo visite con progr'
012500020325     O                       UDATE             +  5 '  /  /  '
012600020325     O                                         +  5 'PAG.'
012700020325     O                       PAGE1         Z
012800100408     O          E            stav           1
012900100407     O                       visnrv            +  0 '      / '
013000100407     O                       visdvi            +  1 '    /  /  '
013100100408     O                       viscmm            + 04
013200100407     O                       visRAG            +  1
013300100408     O          E            stav           1
013400100408     O                       wisnrv            +  0 '      / '
013500100408     O                       wisdvi            +  1 '    /  /  '
013600100408     o               01                        +  1 '**'
013700100408     O                       wiscmm            + 01
013800100408     O                       wisRAG            +  1
013900100408     o
014000100408     O          E            noaltri        1
014100100408     O                       visnrv            +  0 '      / '
014200100408     O                       visdvi            +  1 '    /  /  '
014300100408     O                       viscmm            + 01
014400100408     O                       visRAG            +  1
014500100408     O                                         +  1 'non trovati altri progr.'
014600020325      *
014700100408     OQSYSPRT   E            fine        2
014800100408     O                                         +  0 'Viste con pgm e diverso -
014900100408     o                                              commerciale'
015000100408     O                       divcmm        2   +  1
