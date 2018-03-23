000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100408     H* TNVR22R3 *----------------------------------------------------*
000300100408     H* visite fittiizie vecchie
000400000000     H*--------------------------------------------------------------*
000500100407     FTNVIS06L  IF   E           K DISK
000600020111      * Anagrafiche
000700100407     FTFACO00F  iF   E           K DISK
000800100407     FTFIND00F  iF   E           K DISK
000900100407     FTFCLP00F  iF   E           K DISK
001000020111      * Offerte
001100041202     FTNOFM01L  iF   E           K DISK
001200041202     F                                     INFDS(ofm1)
001300041202     FTIOFD01L  iF   E           K DISK
001400041202     F                                     INFDS(ofd1)
001500041202     FTIOPT01L  iF   E           K DISK
001600041202     F                                     INFDS(opt1)
001700041202     FTIOPD01L  iF   E           K DISK
001800041202     F                                     INFDS(opd1)
001900041202     FTIOGC01L  iF   E           K DISK
002000041202     F                                     INFDS(ogc1)
002100041202
002200100407     FTNOFM00f  iF   E             DISK    rename(tntam000:tnofm000)
002300100407     FTIOFD00f  iF   E             DISK    rename(titad000:tiofd000)
002400100407     FTIOPT00f  iF   E             DISK    rename(titpt000:tiopt000)
002500100407     FTIOPD00f  iF   E             DISK    rename(titpd000:tiopd000)
002600100407     FTIOGC00f  iF   E             DISK    rename(titgc000:tiogc000)
002700100407     FTFNTC00f  iF   E             DISK    rename(tfntc: tfntc000)
002800020111      * Note
002900041202     FTFNTC01L  iF   E           K DISK
003000041206     F                                     INFDS(ntc1)
003100020111      * Cat
003200041202     FTEETC01L  iF   E           K DISK
003300041202     FTEETS01L  iF   E           K DISK
003400041202     FTEETT01L  iF   E           K DISK
003500020325      *
003600020325     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003700020325      *
003800020910     D tibs34ds      E DS                  inz
003900020910     D ddatiute      e ds
004000020910     D azuteds       e ds                  extname(AZUTE00F)
004100020910      *
004200020325     D$Totale          s              5  0 inz
004300020910     D$TotaleA         s              5  0 inz
004400041202     D ofm1            DS
004500041202     D  ofmNRR               397    400B 0
004600041202     D ofd1            DS
004700041202     D  ofdNRR               397    400B 0
004800041202     D opt1            DS
004900041202     D  optNRR               397    400B 0
005000041202     D opd1            DS
005100041202     D  opdNRR               397    400B 0
005200041202     D ogc1            DS
005300041202     D  ogcNRR               397    400B 0
005400041206     D ntc1            DS
005500041206     D  ntcNRR               397    400B 0
005600921027     C*****************************************************************
005700020325      *
005800020910     c     *dtaara       define    §azute        azuteds
005900020910     c     *dtaara       define    §datiute      ddatiute
006000020910     C                   in(E)     *dtaara
006100020910     C                   IF        %error  or RSUT = *blanks
006200020910     C                   CLEAR                   tibs34ds
006300020910     C                   CALL      'TIBS34R'
006400020910     C                   PARM                    tibs34ds
006500020910     C                   in        *dtaara
006600020910     c                   endif
006700100407     c
006800100407     C                   Z-ADD     1             CODUT             1 0
006900100408     c                   except    tesvis
007000100407
007100100407     c
007200100407     C     *LOVAL        SETLL     tnvis06l
007300100407     C                   READ(N)   tnvis06l                               30
007400100407      * 30 on - fine lettura
007500100407    1C     *IN30         DOWEQ     '0'
007600100408     c* Leggo solo le visite fittizie prima del 20070101
007700100408     c                   if        visffz='F'  and  visdvi<20090101
007800100408     c                   clear                   contaofm
007900100408     c                   clear                   contaofd
008000100408
008100100408     c* Verifico quante offerte presenti
008200100408     c     visnrv        setll     tnofm01l
008300100408     c     visnrv        reade     tnofm01l
008400100408     c
008500100408     c                   dow       not %eof(tnofm01l)
008600100408     c                   add       1             contaofm          5 0
008700100408     c
008800100408     c     visnrv        reade     tnofm01l
008900100408     c                   enddo
009000100408     c
009100100408     c* Verifico quante offerte presenti
009200100408     c     visnrv        setll     tiofd01l
009300100408     c     visnrv        reade     tiofd01l
009400100408     c
009500100408     c                   dow       not %eof(tiofd01l)
009600100408     c                   add       1             contaofd          5 0
009700100408     c
009800100408     c     visnrv        reade     tiofd01l
009900100408     c                   enddo
010000100408     c
010100100408     c                   except    fitt
010200100408     c                   endif
010300100407
010400100408     C                   READ(N)   tnvis06l                               30
010500100408     c                   enddo
010600020114      *
010700020114     C                   SETON                                        LR
010800020114      *
010900020325     OQSYSPRT   E            TESVIS         2 02
011000100408     O                                         + 10 'Controllo visite fittizie'
011100020325     O                       UDATE             +  5 '  /  /  '
011200020325     O                                         +  5 'PAG.'
011300020325     O                       PAGE1         Z
011400100408     O          E            Fitt           1
011500100407     O                       visnrv            +  0 '      / '
011600100407     O                       visdvi            +  1 '    /  /  '
011700100407     O                       viscmm            + 01
011800100407     O                       visRAG            +  1
011900100408     O                                         +  1 'Totale  OFM'
012000100408     O                       contaofm      2   +  1
012100100408     O                                         +  1 'Totale  OFD'
012200100408     O                       contaofd      2   +  1
012300020325      *
