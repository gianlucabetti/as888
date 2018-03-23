000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300900606?     *--------------------------------------------------------------*
000400060321      * CNC0W6R  Mandato x cassaforte con feritoia                   *
000500991029?     *--------------------------------------------------------------*
000600060321     FAZORG01L  IF   E           K DISK
000700060418     Fyfcas00f  o    E             DISK
000800060323     Ffidci01l  uF   E           K DISK    usropn
000900060516     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) usropn
001000991029?     *--------------------------------------------------------------*
001100060321     D cnc0wa00s     e ds
001200060321     D ddatiute      e ds
001300021121     D azuteds       e ds                  extname(AZUTE00F)
001400021121     D tibs34ds      E DS                  inz
001500900514     D KPJBA         E DS
001600060323     d totale          s             10  2
001700060323     d totgen          s                   like(totale)
001800991102     D WLBDAT          DS
001900991102     D  G02DAT                 1      8  0
002000991102     D  G02INV                 9     16  0
002100991102     D  G02ERR                17     17
002200991102     D  G02TGI                18     22  0
002300941124     D                SDS
002400941124     D  §PGM                   1     10
002500060321     D F6              C                   X'36'
002501060516     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)
002600991029?     *--------------------------------------------------------------*
002700991029      * CICLO PRINCIPALE
002800991029?     *--------------------------------------------------------------*
002900060321     c                   clear                   cnc0wa00s
003000060321     c                   eval      DIVISA ='EUR'
003100060421     c                   eval      MINIMO = 5
003200060321     c                   eval      kpjbu = cnc0wa00s
003300060321     c                   call      'CNC0WA00R'
003400060321     c                   parm                    kpjba
003500060321     c                   eval      cnc0wa00s = kpjbu
003600060321     c* se non ci sono errori aggiorno
003700060321     c                   if        RTNCMD = F6 and
003800060322     c                             RTNCODE ='DONE' and
003900060321     c                             IMPIMMESSO > 0
004000030423     c* aggiorna
004100021115     c                   exsr      aggio
004300021115     c                   end
004400910521     C                   SETON                                        LR
004500021115     C**-------------------------------------------------------------
004600060321     C** aggio
004700021115     C**-------------------------------------------------------------
004800060321     C     aggio         BEGSR
004900060323     c                   clear                   totgen
005600060323     c* stampo
005700060323     c                   if        not %open(fidci01l)
005800060323     c                   open      fidci01l
005900060323     c                   end
005901060420     c                   except    lista
006100060323     c     knraz         setll     fidci01l
006200060323     c                   do        *hival
006300060323     c     knraz         reade     fidci01l
006400060323     c                   if        %eof(fidci01l)
006500060323     c                   leave
006600060323     c                   end
006700060323     c                   eval      totale = dciqta * dcivalore
006800060323     c                   add       totale        totgen
006900060323     c                   except    detta
006901060407     c                   eval      casqta = dciqta
006902060407     c                   eval      casvalore = dcivalore
006903060407     c                   eval      casfle = dutpou
006904060407     c                   eval      cascuc = dutcou
006905060407     c                   eval      casda8 = oggi
006906060407     c                   eval      casora = ora
006907060407     c                   eval      casicon = totale
006908060407     c                   write     yfcas000
007000060323     c                   delete    fidci000
007100060323     c                   enddo
007200060323     c                   except    firma
007300060321     C*
007400060321     C                   ENDSR
007500030318     C*-----------------------------------------------------------*
007600030318     C*  OPERAZIONI INIZIALI
007700030318     C*-----------------------------------------------------------*
007800030318     C     *INZSR        BEGSR
007900991029     C*
008000000000     C     *ENTRY        PLIST
008100000000     C                   PARM                    KPJBA
008200021118     C*
008300021118     c     *dtaara       define    §azute        azuteds
008400021118     c     *dtaara       define    §datiute      ddatiute
008500021118     C                   in(E)     *dtaara
008600021118     C                   IF        %Error  or  RSUT = *blanks
008700021118     C                   call      'TIBS34R'
008800021118     C                   parm                    Tibs34Ds
008900021118     C                   in        *dtaara
009000021118     c                   ENDIF
009100060321     C     dutpou        CHAIN     AZORG01L                           21
009200060321     C                   TIME                    ora               6 0
009300060321     C                   Z-ADD     UDATE         G02DAT
009400060321     C                   MOVE      *ZEROS        G02INV
009500060321     C                   MOVEL     *BLANK        G02ERR
009600060321     C                   CALL      'XSRDA8'
009700060321     C                   PARM                    WLBDAT
009800060321     C                   Z-ADD     G02INV        oggi              8 0
009801060516     C*§ APRO FILE DI STAMPA
009802060516     C*
009803060516     C                   MOVEA     CMDP          CMDPX            60
009804060516     C                   Z-ADD     60            LUNCMD           15 5
009805060516     C                   CALL      'QCMDEXC'                            91
009806060516     C                   PARM                    CMDPX
009807060516     C                   PARM                    LUNCMD
009808060516     C                   OPEN      QSYSPRT
009900991102     C*
010000991102     C                   ENDSR
010100060321     OQSYSPRT   E            lista            02
010200021211     O                       RSUT                20
010300060502     O                                         + 10 'RICEVUTA'
010301060502     O                                         +  0 ' MANDATO DI VERSAMENTO NEL'
010302060424     O                                         +  0 'LA CASSAFORTE CON'
010400060321     O                                         +  0 ' FERITOIA'
010500060502     O                       UDATE             +  5 '  /  /  '
010600060321     O                       ora               +  1 '  :  :  '
010700060502     O                                         +  1 'CNC0W6R'
010800021211     O                       PAGE          Z    132
010900021211     O                                          128 'PAG.'
011000060321     O          E            lista            05
011100060321     O                                         +  0 'P.O.'
011200060420     O                       dutpou            +  1
011300060321     O                       orgdes            +  1
011400060323     O                                         +  5 'Operatore'
011500060420     O                       dutcou        Z   +  1
011600060323     O                                         +  5 'Data'
011700060321     O                       udate             +  1 '  /  /  '
011800060321     O                                         +  1 'Ora'
011900060420     O                       ora               +  1 '  :  :  '
012000060323     O          E            lista            07
012100060323     o                                         +  5 'Dettaglio contante'
012200060323     O          E            lista            09
012300060323     o                                         +  0 '__________________________'
012400060323     o                                         +  0 '____'
012500060323     O          E            lista            10
012600060323     o                                         +  2 'Qta   Importo'
012700060323     o                                         +  9 'Totale'
012800060323     O          E            lista            11
012900060323     o                                         +  0 '__________________________'
013000060323     o                                         +  0 '____'
013100060323     O          E            detta       1
013200060323     o                       dciqta        2   +  0
013300060323     o                       dcivalore     2   +  1
013400060323     o                       totale        2   +  1
013500060323     O          E            firma       1
013600060323     o                                         +  0 '__________________________'
013700060323     o                                         +  0 '____'
013800060323     O          E            firma       1
013900060323     o                                         +  0 'Totale generale'
014000060323     o                       totgen        2   +  2
014100060323     O          E            firma       3
014200060424     o                                         + 35 'Firma operatore'
014300060424     o                                         + 17 'Firma cassiere'
014400060323     O          E            firma       3
014500060323     o                                         + 35 '__________________________'
014600060323     o                                         +  6 '__________________________'
014700060516**  CMDP
014800060516OVRPRTF FILE(QSYSPRT) SAVE(*YES)
