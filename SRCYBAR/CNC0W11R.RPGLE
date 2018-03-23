000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300900606?     *--------------------------------------------------------------*
000400060502      * CNC0W11R Rettifica MANDATI X CASSAFORTE CON FERITOIA         *
000500991029?     *--------------------------------------------------------------*
000600910521      * 03 -FINE LAVORO                                              *
000700910521      * 06 -AGGIORNAMENTO                                            *
000800900606?     *--------------------------------------------------------------*
000900060502     FCNC0W11d  CF   E             WORKSTN
001000030318     Fazorg01l  iF   E           k disk
001001060502     Fazute02l  iF   E           k disk
001100060502     Fyfcas01L  uF a E           K DISK
001101060516     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) usropn
001300991029?     *--------------------------------------------------------------*
001301060502     D WLBDAT          DS
001302060502     D  G02DAT                 1      8  0
001303060502     D  G02INV                 9     16  0
001304060502     D  G02ERR                17     17
001305060502     D  G02TGI                18     22  0
001306060502     D ddatiute      e ds
001307060502     D azuteds       e ds                  extname(AZUTE00F)
001308060502     D tibs34ds      E DS                  inz
001400021126     D kpjba         e ds
001500060502     D msg             S             60    DIM(2) CTDATA PERRCD(1)              SCHIERA ER
001900021126     D dtaiso          S               d   inz                                  DS3A TIPO
002000021126     D dtaeur          S               d   datfmt(*eur) inz                     DS3A TIPO
002001060502     d totale          s             10  2
002002060502     d totgen          s                   like(totale)
002003060516     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)
002100991029?     *--------------------------------------------------------------*
002200991029      * CICLO PRINCIPALE
002300991029?     *--------------------------------------------------------------*
002400030404     c* controllo password
002500030404     c                   do        *hival
002501060502     c                   if        *in20
002502060502     c                   setoff                                       20
002503060502     C                   write     CN0W102
002504060502     c                   seton                                        20
002505060502     c                   end
003100021126     C                   EXFMT     CN0W102
003200021125     C   kc              leave
003300030318     c* controlli
003400030318     c                   exsr      ctr01
003500030318     c   20              iter
003600060502     c                   if        *inkf
003700021115     c                   exsr      aggio
003800021115     c                   leave
003900021115     c                   end
004000991102     C                   ENDDO
004800991029     C*
004900910521     C                   SETON                                        LR
005000021115     C**-------------------------------------------------------------
005100030318     C** controlli
005200021115     C**-------------------------------------------------------------
005300030318     C     ctr01         BEGSR
005400021126     c                   setoff                                       20
005500021126     c                   clear                   $msg
005501060502     c                   do
005900030318     c* controllo po
006000030318     c     pov           chain     azorg01l
006100030318     c                   if        not %found(azorg01l)
006200030318     c                   seton                                        2001
006201060502     c                   leave
006202060502     c                   else
006203060502     c                   eval      despo = ORGDES
006300060502     c                   end
006301060502     c* controllo il cuc
006302060502     c     kute          chain     azute02l
006303060502     c                   if        not %found(azute02l)
006304060502     c                   seton                                        2002
006305060502     c                   leave
006306060502     c                   else
006307060502     c                   eval      descuc= uteute
006308060502     c                   end
006400060502     c* data
006401060502     c                   clear                   dtaeur
006402060502     c                   if        dtav = 0
006403060502     C                   SETON                                        03
006404060502     c                   else
006405060502     c     *eur          test(d)                 dtav                   03
006406060502     c                   if        *in03
006407060502     c     *dmy          test(d)                 dtav                   03
006408060502     c                   if        not *in03
006409060502     c     *dmy          move      dtav          dtaeur
006410060502     c                   end
006411060502     c                   else
006412060502     c                   move      dtav          dtaeur
006413060502     c                   end
006414060502     c                   end
006415060502     c                   if        *in03
006416060502     c                   seton                                        20
006417060502     c                   leave
006418060502     c                   else
006419060502     c                   move      dtaeur        dtaiso
006420060502     c                   move      dtaiso        comda8            8 0
006421060502     c                   end
006600021126     c*
006700060502     c     kcas          chain     yfcas01l
007000060502     c                   if        not %found(yfcas01l)
007200060502     c                   seton                                        20
007300060502     c                   movel     msg(1)        $msg
007301060502     c                   leave
007302060502     c                   else
007303060502     c                   if        casqta < 0
007304060502     c                   seton                                        20
007305060502     c                   movel     msg(1)        $msg
007306060502     c                   leave
007307060502     c                   end
007308060502     c                   if        casvie = 'S'
007309060502     c                   seton                                        20
007310060502     c                   movel     msg(2)        $msg
007311060502     c                   leave
007400060502     c                   end
007401060502     c                   end
007402060502     c                   enddo
011700060502     c                   endsr
011800021120     C**-------------------------------------------------------------
011900021120     C** aggiorna
012000021120     C**-------------------------------------------------------------
012100021120     C     aggio         BEGSR
012101060502     c                   except    lista
012102060502     c                   clear                   totale
012300060502     c     kcas          setll     yfcas01l
012301060502     c                   do        *hival
012302060502     c     kcas          reade     yfcas01l
012400060502     c                   if        %eof(yfcas01l)
012401060502     c                   leave
012402060502     c                   end
012403060502     c                   if        casvie = 'S'
012404060502     c                   iter
012405060502     c                   end
012406060502     c                   eval      casvie = 'S'
012600060502     c                   update    yfcas000
012601060502     c                   clear                   casvie
012602060502     c                   eval      casqta = casqta * -1
012603060502     c                   eval      cascuc = dutcou
012604060502     c                   eval      casda8 = oggi
012605060502     c                   eval      casora = ora
012606060502     c                   write     yfcas000
012607060502     c                   eval      totale = casqta * casvalore
012608060502     c                   add       totale        totgen
012609060502     c                   except    detta
012700021126     c                   end
012701060502     c                   except    firma
012800021120     c                   endsr
012900991102     C*-----------------------------------------------------------*
013000991029     C*  OPERAZIONI INIZIALI
013100991029     C*-----------------------------------------------------------*
013200941124     C     *INZSR        BEGSR
013300991029     C*
013400000000     C     *ENTRY        PLIST
013500000000     C                   PARM                    KPJBA
013501060502     C*
013502060502     c     *dtaara       define    §azute        azuteds
013503060502     c     *dtaara       define    §datiute      ddatiute
013504060502     C                   in(E)     *dtaara
013505060502     C                   IF        %Error  or  RSUT = *blanks
013506060502     C                   call      'TIBS34R'
013507060502     C                   parm                    Tibs34Ds
013508060502     C                   in        *dtaara
013509060502     c                   ENDIF
013510060502     c                   eval      pov = dutpou
013511060502     c                   eval      cucv= dutcou
013512060502     c                   eval      dtav= *date
013513060502     C                   TIME                    ora               6 0
013514060502     C                   Z-ADD     UDATE         G02DAT
013515060502     C                   MOVE      *ZEROS        G02INV
013516060502     C                   MOVEL     *BLANK        G02ERR
013517060502     C                   CALL      'XSRDA8'
013518060502     C                   PARM                    WLBDAT
013519060502     C                   Z-ADD     G02INV        oggi              8 0
013520060516     C*§ APRO FILE DI STAMPA
013521060516     C*
013522060516     C                   MOVEA     CMDP          CMDPX            60
013523060516     C                   Z-ADD     60            LUNCMD           15 5
013524060516     C                   CALL      'QCMDEXC'                            91
013525060516     C                   PARM                    CMDPX
013526060516     C                   PARM                    LUNCMD
013527060516     C                   OPEN      QSYSPRT
014000900515      *---------------------------------------------------------------*
014100900515      * CHIAVI                                                        *
014200900515      *---------------------------------------------------------------*
014300060502     C     Kute          KLIST
014400030318     C                   KFLD                    pov
014500060502     C                   KFLD                    cucv
014600060502     C     Kcas          KLIST
014700030318     C                   KFLD                    pov
014800060502     C                   KFLD                    comda8
014900060502     C                   KFLD                    cucv
014901060502     C                   KFLD                    orav
015400991102     C*
015500991102     C                   ENDSR
015501060502     OQSYSPRT   E            lista            02
015502060502     O                       RSUT                20
015503060504     O                       dutute            +  1
015504060504     O                                         +  1 '('
015505060504     O                       dutcou            +  0
015506060504     O                                         +  0 ')'
015507060504     O                                         +  3 'STORNO '
015508060502     O                                         +  0 'RICEVUTA MANDATO DI '
015509060504     O                                         +  0 'VERSAMENTO'
015510060504     O                       UDATE             + 10 '  /  /  '
015511060502     O                       ora               +  1 '  :  :  '
015512060502     O                                         +  1 'CNC0W11R'
015513060502     O                       PAGE          Z    132
015514060502     O                                          128 'PAG.'
015515060504     O          E            lista            03
015520060504     O                                         + 48 'CASSAFORTE CON'
015521060504     O                                         +  0 ' FERITOIA'
015527060502     O          E            lista            05
015528060502     O                                         +  0 'P.O.'
015529060502     O                       pov               +  1
015530060502     O                       despo             +  1
015531060502     O                                         +  5 'Operatore'
015532060502     O                       cucv          Z   +  1
015533060502     O                                         +  5 'Data'
015534060502     O                       dtav              +  1 '  /  /    '
015535060502     O                                         +  1 'Ora'
015536060502     O                       orav              +  1 '  :  :  '
015537060502     O          E            lista            07
015538060502     o                                         +  5 'Dettaglio contante'
015539060502     O          E            lista            09
015540060502     o                                         +  0 '__________________________'
015541060502     o                                         +  0 '_______'
015542060502     O          E            lista            10
015543060502     o                                         +  2 'Qta      Importo'
015544060502     o                                         +  9 'Totale'
015545060502     O          E            lista            11
015546060502     o                                         +  0 '__________________________'
015547060502     o                                         +  0 '_______'
015548060502     O          E            detta       1
015549060502     o                       casqta        k   +  0
015550060502     o                       casvalore     k   +  1
015551060502     o                       totale        k   +  1
015552060502     O          E            firma       1
015553060502     o                                         +  0 '__________________________'
015554060502     o                                         +  0 '_______'
015555060502     O          E            firma       1
015556060502     o                                         +  0 'Totale generale'
015557060502     o                       totgen        k   +  4
015558060502     O          E            firma       3
015559060502     o                                         + 35 'Firma operatore'
015560060502     o                                         + 17 'Firma cassiere'
015561060502     O          E            firma       3
015562060502     o                                         + 35 '__________________________'
015563060502     o                                         +  6 '__________________________'
015600021126** MSG
015800060502Non esiste nulla da stornare.
015900060502Il mandato è già stato stornato.
016000060516**  CMDP
016100060516OVRPRTF FILE(QSYSPRT) SAVE(*YES)
