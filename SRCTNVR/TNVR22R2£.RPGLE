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
006700020910      *
006800100407     C     KACO          KLIST
006900100407     C                   KFLD                    CodUt
007000100407     C                   KFLD                    DutKci
007100100407     C                   KFLD                    VISNRV
007200100407     c
007300100407     C                   Z-ADD     1             CODUT             1 0
007400100407
007500100407     c
007600100407     C     *LOVAL        SETLL     tnvis06l
007700100407     C                   READ(N)   tnvis06l                               30
007800100407      * 30 on - fine lettura
007900100407    1C     *IN30         DOWEQ     '0'
008000100407     c* Verifico se non ha i file dell'anagrafica
008100100407    2c                   if        visksc=0
008200100407
008300100407     C     KACO          chain     CNACO000
008400100407     c                   if        not %found(tfaco00f)
008500100407     c                   seton                                        01
008600100407     c                   except    visita
008700100407     c                   setoff                                       01
008800100407     c                   endif
008900100407
009000100407     C     KACO          chain     CNind000
009100100407     c                   if        not %found(tfind00f)
009200100407     c                   seton                                        02
009300100407     c                   except    visita
009400100407     c                   setoff                                       02
009500100407     c                   endif
009600100407
009700100407     C     KACO          chain     CNclp000
009800100407     c                   if        not %found(tfclp00f)
009900100407     c                   seton                                        03
010000100407     c                   except    visita
010100100407     c                   setoff                                       03
010200100407     c                   endif
010300100407     c
010400100407     c                   endif
010500100407     C                   READ(N)   tnvis06l                               30
010600100407     c                   enddo
010700020114      *
010800020114     C                   SETON                                        LR
010900020114      *
011000020325     OQSYSPRT   E            TESVIS         2 02
011100020325     O                                         + 10 'CANCELLAZIONE RECORDS '
011200020325     O                                         +  1 'FILES LEGATI ALLE VISITE:'
011300100407     O                                         +  8 'TNVR22R1'
011400020325     O                       UDATE             +  5 '  /  /  '
011500020325     O                                         +  5 'PAG.'
011600020325     O                       PAGE1         Z
011700100407     O          E            visita         1
011800100407     O                       visnrv            +  0 '      / '
011900100407     O                       visdvi            +  1 '    /  /  '
012000100407     O                       viscmm            + 01
012100100407     O                       visRAG            +  1
012200100407     O                                         +  1 'Visita cli nuovo '
012300100407     O               01                             'MANCA ACO'
012400100407     O               02                             'MANCA IND'
012500100407     O               03                             'MANCA CLP'
012600020325      *
