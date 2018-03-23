000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116     H* TNVR22R *----------------------------------------------------*
000300020111     H* PULIZIA ARCHIVI LEGATI A FILE VISITE SENZA RELATIVO RECORD
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
004100100419     D tntad1ds      e ds
004200100419     D kpjba         e ds
004300020910      *
004400020325     D$Totale          s              5  0 inz
004500020910     D$TotaleA         s              5  0 inz
004600041202     D ofm1            DS
004700041202     D  ofmNRR               397    400B 0
004800041202     D ofd1            DS
004900041202     D  ofdNRR               397    400B 0
005000041202     D opt1            DS
005100041202     D  optNRR               397    400B 0
005200041202     D opd1            DS
005300041202     D  opdNRR               397    400B 0
005400041202     D ogc1            DS
005500041202     D  ogcNRR               397    400B 0
005600041206     D ntc1            DS
005700041206     D  ntcNRR               397    400B 0
005800921027     C*****************************************************************
005900020325      *
006000020910     c     *dtaara       define    §azute        azuteds
006100020910     c     *dtaara       define    §datiute      ddatiute
006200020910     C                   in(E)     *dtaara
006300020910     C                   IF        %error  or RSUT = *blanks
006400020910     C                   CLEAR                   tibs34ds
006500020910     C                   CALL      'TIBS34R'
006600020910     C                   PARM                    tibs34ds
006700020910     C                   in        *dtaara
006800020910     c                   endif
006900020910      *
007000100407     C     KACO          KLIST
007100100407     C                   KFLD                    CodUt
007200100407     C                   KFLD                    DutKci
007300100407     C                   KFLD                    VISNRV
007400100408     C     Kaco2         KLIST
007500100408     C                   KFLD                    acokut
007600100408     C                   KFLD                    acokcc
007700100408     C                   KFLD                    acoksc
007800100407     c
007900100407     C                   Z-ADD     1             CODUT             1 0
008000100407
008100100407     c
008200100407     C     *LOVAL        SETLL     tnvis06l
008300100407     C                   READ(N)   tnvis06l                               30
008400100407      * 30 on - fine lettura
008500100407    1C     *IN30         DOWEQ     '0'
008600100420     c* se visita > 20100901 deleto
008700100420   1ac                   if        visdvi>20100901
008800100420     c                   seton                                        04
008900100420     c                   except    visita
009000100420     c                   setoff                                       04
009100100420     c
009200100420     c                   clear                   tntad1ds
009300100420     c                   eval      itad1nrv=visnrv
009400100420     c                   movel     tntad1ds      kpjbu
009500100420     c
009600100420     c                   Call      'TNTAD1R'
009700100420     c                   Parm                    kpjba
009800100420     c                   eval      callTA='S'
009900100420     c
010000100420   1ac                   else
010100100420     c
010200100407     c* Verifico se non ha i file dell'anagrafica
010300100407    2c                   if        visksc=0
010400100419     c                   clear                   callTA            1
010500100407
010600100407     C     KACO          chain     CNACO000
010700100420    3c                   if        not %found(tfaco00f) or acolib=0
010800100420     c                   seton                                        01
010900100407     c                   except    visita
011000100420     c                   setoff                                       01
011100100419     c
011200100419     c                   clear                   tntad1ds
011300100419     c                   eval      itad1nrv=visnrv
011400100419     c                   movel     tntad1ds      kpjbu
011500100419     c
011600100419     c                   Call      'TNTAD1R'
011700100419     c                   Parm                    kpjba
011800100419     c                   eval      callTA='S'
011900100419     c
012000100420    3c                   endif
012100100407
012200100407     C     KACO          chain     CNind000
012300100420    3c                   if        not %found(tfind00f)
012400100407     c                   seton                                        02
012500100407     c                   except    visita
012600100407     c                   setoff                                       02
012700100420    3c                   endif
012800100407
012900100407     C     KACO          chain     CNclp000
013000100420    3c                   if        not %found(tfclp00f)
013100100407     c                   seton                                        03
013200100407     c                   except    visita
013300100407     c                   setoff                                       03
013400100420    3c                   endif
013500100407     c
013600100420    2c                   endif
013700100420   1ac                   endif
013800100420
013900100407     C                   READ(N)   tnvis06l                               30
014000100420    1c                   enddo
014100100408     c*
014200100408     c* se c'e' un ACO ci deve essere sempre il relativo IND CLP
014300100408     C     *LOVAL        SETLL     TFACO00F
014400100408     C                   READ(N)   TFACO00F                               30
014500100408      * 30 on - fine lettura
014600100408     C     *IN30         DOWEQ     '0'
014700100408     c     kaco2         chain     tfind00f
014800100408     c                   if        not %found(tfind00f)
014900100408     c                   except    Mancaind
015000100408     c                   endif
015100100408     c     kaco2         chain     tfclp00f
015200100408     c                   if        not %found(tfind00f)
015300100408     c                   except    Mancaclp
015400100420     c                   endif
015500100408     c
015600100408     C                   READ(N)   TFACO00F                               30
015700100408     c                   enddo
015800020114      *
015900020114     C                   SETON                                        LR
016000020114      *
016100020325     OQSYSPRT   E            TESVIS         2 02
016200020325     O                                         + 10 'CANCELLAZIONE RECORDS '
016300020325     O                                         +  1 'FILES LEGATI ALLE VISITE:'
016400100407     O                                         +  8 'TNVR22R1'
016500020325     O                       UDATE             +  5 '  /  /  '
016600020325     O                                         +  5 'PAG.'
016700020325     O                       PAGE1         Z
016800100407     O          E            visita         1
016900100407     O                       visnrv            +  0 '      / '
017000100407     O                       visdvi            +  1 '    /  /  '
017100100407     O                       viscmm            + 01
017200100407     O                       visRAG            +  1
017300100407     O                                         +  1 'Visita cli nuovo '
017400100420     O               01                             'MANCA ACO o acolib=0'
017500100407     O               02                             'MANCA IND'
017600100407     O               03                             'MANCA CLP'
017700100420     O               04                         120 'visita post datata'
017800020325      *
017900100408     O          E            MancaIND       1
018000100408     O                       acoksc            +  0 '      / '
018100100408     O                       acorag            +  1
018200100408     O                                              'MANCA IND'
018300100408     O          E            MancaCLP       1
018400100408     O                       acoksc            +  0 '      / '
018500100408     O                       acorag            +  1
018600100408     O                                              'MANCA CLP'
