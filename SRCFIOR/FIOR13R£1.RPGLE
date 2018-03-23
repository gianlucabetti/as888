000100001019     H DECEDIT('0,') DATEDIT(*DMY.)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500001019      *       CONFERMA ORDINI RITIRI FISSI                            *
000600000927      *                                                               *
000700000927      *****************************************************************
000800000927
000900001019     FAZORG01L  IF   E           K DISK
001000001024     FFNORS01L  UF   E           K DISK
001100001024     FFNACR01L  IF   E           K DISK
001200001023     FFIOR13D   CF   E             WORKSTN SFILE(FIORSF1:NN1)
001300000927
001400000928     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             MSG VIDEO
001500001024     D GIO             S             20    DIM(7) CTDATA PERRCD(1)              Giorni Settim.
001600001024     D VOC             S              3    DIM(7) CTDATA PERRCD(1)              VOCI A VIDEO
001700001024     D DUP             S             10  0 DIM(5000)                            ORM Duplicati
001800001025     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
001900130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
002000130619
002100000927     D KPJBA         E DS
002200001024     D FIOR05DS      E DS
002300010730     D DORS01        E DS                                                       FNORS ORSFLO
002400000927
002500000927     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002600000927     D  LIN                    1     90  0
002700000927     D                                     DIM(30)
002800130619
002900130619     d azuteds       e ds                  extname(azute00f)
003000130619     d ddatiute      e ds
003100130619     d tibs34ds      e ds
003200130619     d TRUL31DS      e ds
003300130619     D TIBS02DS      E DS
003400130619
003500130619     d dLAT          e ds
003600130619     d dUTE01        e ds
003700001023
003800001023      * Parametri per controllo Data
003900001023     D WLBDAT          DS                  INZ
004000001023     D  G02DAT                 1      8  0
004100001023     D  G02INV                 9     16  0
004200001023     D  G02ERR                17     17
004300001023     D  G02TGI                18     22  0
004400010202
004500010202     D Parm01          DS
004600010202     D  ppoe                          3  0
004700010202     D  pnor                          7  0
004800010202     D  pnsr                          2  0
004900010202     D  pnrv                          2  0
005000010202     D  psce                          1
005100010202     D  pfgs                          3  0
005200010202     D  ppor                          3  0
005300010202     D  pdtr                          8  0
005400010202     D  pmdb                         10
005500010202     D  pprb                         10
005600010202     D  pdts                          8  0
005700010202     D  prmp                          1
005800010202     D  pbrc                          1
005900010202     D  pref                          2
006000010202     D  pmio                          1
006100020506
006200020506     d wdatamax        s               d   datfmt(*eur)
006300020506     d wdatarit        s               d   datfmt(*eur)
006400130619
006500130619     d wabi            s                   like(UTEaut)
006600130619     d wEsci           s               n   inz(*off)
006700130619     d wErrGrave       s               n   inz(*off)
006800130619     d w003a           s              3
006900000927
007000000927      *---------------------------------------------------------------*
007100000927      *                                                               *
007200000927      *    UTILIZZO DEGLI INDICATORI                                  *
007300000927      *                                                               *
007400000927      *---------------------------------------------------------------*
007500000927      *                                                               *
007600001019      *    01 - Posizione Cursore su P.O. Gestione      FIOR131       *
007700001019      *    02 - Posizione Cursore su Data Ritiro        FIOR131       *
007800001023      *    03 - Disabilito F18                          FIOR131       *
007900001024      *    04 - Disabilito F13                                        *
008000010123      *    05 - Posizione Cursore su M/P/_              FIOR131       *
008100001025      *    10 - Confermato almeno un Ordine                           *
008200001025      *    11 - Duplicato almeno un Ordine                            *
008300001019      *    18 - Sproteggo P.O. Gestione                 FIOR131       *
008400001019      *    28 - Visualizzo messaggio Errore             FIOR131       *
008500001024      *    30 - ORM senza giorno                                      *
008600001024      *    31 - Creato Orm da F6                                      *
008700001024      *    32 - F12 da F6                                             *
008800131003      *    33 - Filiale utente attiva agli Orari Servizi
008900001024      *    34 - Lookup sk duplicati                                   *
009000001024      *    39 - Comodo                                                *
009100001023      *    90 - Riemetto Videate                                      *
009200000927      *                                                               *
009300000927      *---------------------------------------------------------------*
009400000927
009500000927      *****************************************************************
009600000927      *                                                               *
009700000927      *    OPERAZIONI INIZIALI                                        *
009800000927      *                                                               *
009900000927      *****************************************************************
010000000927
010100001024
010200001024      *    Inizializzo Campi Video
010300001024
010400001024     C                   z-add     0             y
010500000927     C                   EXSR      SR_INIVIDEO
010600000927
010700000927      *****************************************************************
010800000927      *                                                               *
010900000927      *    ROUTINE PRINCIPALE                                         *
011000000927      *                                                               *
011100000927      *****************************************************************
011200001011
011300001019     C                   do        *hival
011400130619      /free
011500130619       //?Se utente non autorizzato alla funzione esco
011600130619         IF  wErrGrave;
011700130619           *in28 = *on;
011800130619           wEsci = *on;
011900130619           VIDmsg = 'Utente non autorizzato alla funzione';
012000130619         ENDIF;
012100130619      /end-free
012200001019
012300001019      *    Emissione Prima Videata
012400001019
012500001019     C                   EXFMT     FIOR131
012600130619
012700130619      /free
012800130619         IF  wEsci;
012900130619           leave;
013000130619         ENDIF;
013100130619      /end-free
013200130619
013300001019     C   KC              leave
013400130619
013500001023     C                   setoff                                       289018
013600001019     C                   setoff                                       01  02
013700010123     C                   setoff                                       05
013800001019
013900001019      *    Controllo
014000001019
014100001019     C                   EXSR      SR_CONTROL
014200001019
014300001019      *    Se Vi sono Errori o Voglio Cambiare P.O. Riemetto
014400001023      *    .. Riemetto normalmente anche se non ho premuto F6
014500001019
014600001019     C     *in28         ifeq      *on
014700001019     C     *in18         oreq      *on
014800001023     C     *in90         oreq      *on
014900001019     C                   iter
015000001019     C                   endif
015100001023
015200001023      *   GESTIONE DEGLI ORM
015300001023
015400001023     C                   EXSR      SR_GESTIONE
015500001023
015600001019     C                   enddo
015700001019
015800000927     C                   eval      *inlr = *on
015900000927
016000000927      *****************************************************************
016100000927      *                                                               *
016200000927      *    SUBROUTINES                                                *
016300000927      *                                                               *
016400000927      *****************************************************************
016500000927
016600000927      *---------------------------------------------------------------*
016700000927      *    Inizializzazione Campi Video                               *
016800000927      *---------------------------------------------------------------*
016900000927
017000000927     C     SR_INIVIDEO   begsr
017100000927
017200001025      *    Reset Progressivo Nr. Ordini Confermati/Duplicati
017300001025
017400001025     C                   setoff                                       10  11
017500001025     C                   clear                   vconfe
017600001025     C                   clear                   vdupli
017700001002
017800001019      *    Inizializzo il P.O. Gestione
017900000927
018000130619     c                   eval      vidpog = dutpou
018100001012
018200000927      *    Decodifico il P.O. di Appartenenza
018300000927
018400001019     C     vidpog        chain     azorg01l
018500000927     C                   if        %found(azorg01l)
018600000927     C                   movel     orgdes        viddep
018700000927     C                   else
018800001019     C                   clear                   viddep
018900000927     C                   endif
019000000927
019100000927      *    Inizializzo i Campi Restanti
019200001012
019300001019     C                   move      *date         viddar
019400010123     C                   move      *blanks       vidrm
019500010123     C                   move      *blanks       vidrp
019600010131     C                   move      'SI'          vidvis
019700001019
019800001019     C                   clear                   wgio              1
019900001019     C                   movel     viddar        wggmm             4
020000001019     C                   move      viddar        waa               2
020100001019     C                   movel     wggmm         wggmmaa           6
020200001019     C                   move      waa           wggmmaa
020300001019     C                   call      'XGIOSE1'
020400001019     C                   parm                    wggmmaa
020500001019     C                   parm                    wgio
020600001019     C                   move      wgio          x                 1 0
020700001019     C                   movel(p)  gio(x)        vidgio
020800000927
020900000927     C                   endsr
021000001019
021100001019      *---------------------------------------------------------------*
021200001019      *    Controllo Prima Videata                                    *
021300001019      *---------------------------------------------------------------*
021400001019
021500001019     C     SR_CONTROL    begsr
021600001019
021700001025     C                   z-add     0             vconfn
021800001025     C                   z-add     0             vdupln
021900001025
022000001023     C     *inKF         ifeq      *on
022100001023     C                   move      '1'           wkf
022200001023     C                   else
022300001023     C                   clear                   wkf               1
022400001023     C                   endif
022500001024
022600001024     C     *inKM         ifeq      *on
022700001024     C                   move      '1'           wkm
022800001024     C                   eval      *in04 = *on
022900001024     C                   else
023000001024     C                   eval      *in04 = *off
023100001024     C                   clear                   wkm               1
023200001024     C                   endif
023300001023
023400001019      *    F18=Cambio P.O. Gestione
023500001019
023600001019     C     *inks         ifeq      *on
023700001019     C                   seton                                        18
023800001019     C                   goto      endctr
023900001019     C                   endif
024000001023
024100001023      *    Controllo P.O. Gestione
024200001023
024300001023     C                   clear                   viddep
024400130619     c                   move      VIDpog        w003a
024500130619     c     w003a         lookup    SKpog                                  39
024600001023     C     *in39         ifeq      *off
024700001023     C                   eval      *in01 = *on
024800001023     C                   eval      *in28 = *on
024900001023     C                   eval      *in18 = *on
025000001023     C                   movel     msg(1)        vidmsg
025100001023     C                   goto      endctr
025200001023     C                   else
025300001023     C     vidpog        chain     azorg01l
025400001023     C                   if        %found(azorg01l)
025500001023     C                   movel     orgdes        viddep
025600001023     C                   endif
025700001025      *        Carico £6
025800001025     C                   CLEAR                   DSUL06
025900001025     C                   MOVE      '£6'          D06COD
026000001025     C                   MOVEL     vidpog        D06KEY
026100001025     C                   MOVEL     DSUL06        KPJBU
026200001025     C                   CALL      'TRUL06R'
026300001025     C                   PARM                    KPJBA
026400001025     C                   MOVEL     KPJBU         DSUL06
026500001025     C                   MOVEA     LIN           L6
026600001023     C                   endif
026700001023
026800001023      *    Controllo Data (Corretta Formalmente e non Inferiore a Oggi)
026900001023
027000001023     C                   clear                   vidgio
027100001023     C                   z-add     viddar        G02DAT
027200001023     C                   clear                   G02ERR
027300001023     C                   call      'XSRDA8'
027400001023     C                   parm                    WLBDAT
027500001023     C                   z-add     g02inv        wdatainv          8 0
027600001023     C                   z-add     g02dat        viddar
027700001023     C                   move      g02err        werror            1
027800001023     C                   z-add     *date         G02DAT
027900001023     C                   clear                   G02ERR
028000001023     C                   call      'XSRDA8'
028100001023     C                   parm                    WLBDAT
028200001023     C                   z-add     g02inv        wdataod           8 0
028300001023
028400001023     C     werror        IFEQ      '1'
028500001023     C     viddar        OREQ      0
028600001023     C     wdatainv      orlt      wdataod
028700001023     C                   eval      *in02 = *on
028800001023     C                   eval      *in28 = *on
028900001023     C                   movel     msg(2)        vidmsg
029000001023     C                   goto      endctr
029100001023     C                   else
029200001023     C                   clear                   wgio              1
029300001023     C                   movel     viddar        wggmm             4
029400001023     C                   move      viddar        waa               2
029500001023     C                   movel     wggmm         wggmmaa           6
029600001023     C                   move      waa           wggmmaa
029700001023     C                   call      'XGIOSE1'
029800001023     C                   parm                    wggmmaa
029900001023     C                   parm                    wgio
030000001023     C     wgio          ifne      ' '
030100001023     C                   move      wgio          x                 1 0
030200001023     C                   movel(p)  gio(x)        vidgio
030300001023     C                   else
030400001023     C                   eval      *in02 = *on
030500001023     C                   eval      *in28 = *on
030600001023     C                   movel     msg(2)        vidmsg
030700001023     C                   goto      endctr
030800001023     C                   endif
030900001023     C                   ENDIF
031000020506
031100020506      *    La data richiesta non deve essere maggiore di 1 settimana da oggi
031200020506     c     *iso          move      wdataod       wdatamax
031300020506     c                   adddur    7:*D          wdatamax
031400020506     c     *iso          move      wdatainv      wdatarit
031500020506     c                   if        wdatarit > wdatamax
031600020506     C                   eval      *in02 = *on
031700020506     C                   eval      *in28 = *on
031800020506     C                   movel     msg(4)        vidmsg
031900020506     C                   goto      endctr
032000020506     c                   endif
032100001019
032200001023      *    Se è Stata Modificata la Data Segnalo SEMPRE
032300001023
032400001023     C     wdatainv      ifne      wdataod
032500001023     C                   move      'NO'          vsicur
032600001024     C                   EXFMT     FIOR13W
032700001023     C     *inkl         ifeq      *on
032800001023     C     vsicur        orne      'SI'
032900001023     C                   eval      *in90 = *on
033000001023     C                   goto      endctr
033100001023     C                   endif
033200001023     C                   endif
033300010123
033400010123      *    Almeno un ritiro deve essere impostato M/P/_
033500010123
033600010123     C                   if        vidrm = *blanks and vidrp = *blanks
033700010123     C                   eval      *in05 = *on
033800010123     C                   eval      *in28 = *on
033900010123     C                   movel     msg(3)        vidmsg
034000010123     C                   goto      endctr
034100010123     C
034200010123     C                   endif
034300001023
034400001019     C     ENDCTR        endsr
034500001023
034600001023      *---------------------------------------------------------------*
034700001023      *    Lettura / Gestione degli ORM                               *
034800001023      *---------------------------------------------------------------*
034900001023
035000001023     C     SR_GESTIONE   begsr
035100001023
035200001023      *    Pulisco Subfile e mi posiziono per Lettura Ordini Fissi
035300001024
035400001024     C                   do        *hival
035500001023     C                   z-add     0             nn1               4 0
035600001023     C                   eval      *in85 = *on
035700001023     C                   write     fiorrc1
035800001023     C                   eval      *in85 = *off
035900001023     C                   z-add     1             recsf
036000001025
036100001025      *    Carico gli ordini della £6
036200001025
036300001025     C     1             do        30            zz                3 0
036400001025
036500001025     C     L6(zz)        ifeq      0
036600001025     C                   iter
036700001025     C                   endif
036800001025
036900001025     C                   z-add     L6(zz)        vs1pog
037000001025
037100001025     C     vs1pog        setll     fnors01l
037200001023
037300001023      *    Leggo Ordini Fissi
037400001023
037500001023     C                   do        *hival
037600001024     C     wkf           ifne      '1'
037700001025     C     vs1pog        reade(n)  fnors01l
037800001024     C                   else
037900001025     C     vs1pog        reade     fnors01l
038000001024     C                   endif
038100001023
038200001023     C                   if        %eof(fnors01l)
038300001023     C                   leave
038400001023     C                   endif
038500001023
038600001024     C     nn1           ifge      4000
038700001024     C                   leave
038800001024     C                   endif
038900001024
039000001024      *    CONTROLLO VALIDITA' RECORD LETTO:
039100001023
039200001024      *    No Annullati
039300001024
039400001023     C     orsatb        ifeq      'A'
039500001023     C                   iter
039600001023     C                   endif
039700010730
039800010730      *    Chiuso per ferie se periodo chiusura presente in anagrafica
039900010730
040000010730     c                   movel     orsflo        dors01
040100010730
040200010730     c                   if        §orsdif > 0  and wdatainv >=  §orsdif and
040300010730     c                             wdatainv <= §orsdff
040400010730     c                   iter
040500010730     c                   endif
040600010123
040700010124      *    Mattina - Pomeriggio
040800010122
040900010123     C                   if        orsrmp = 'P' and vidrp = *blanks
041000010123     C                   iter
041100010123     C                   endif
041200010123     C                   if        orsrmp = 'M' and vidrm = *blanks
041300010123     C                   iter
041400010123     C                   endif
041500001023
041600001024      *    No Già Elaborati se voglio Confermare
041700001024      *    No Non Elaborati se voglio Duplicare
041800001024
041900001024     C     wkm           ifne      '1'
042000001024     C     orsdte        ifge      wdatainv
042100001024     C                   iter
042200001024     C                   endif
042300001024     C                   else
042400001024     C     orsdte        ifne      wdatainv
042500001024     C                   iter
042600001024     C                   endif
042700001024     C                   endif
042800001023
042900001024      *    Giorni
043000001024
043100001024     C                   eval      *in30 = *on
043200001024
043300001024     C     orsrlu        ifeq      'S'
043400001024     C     orsrma        oreq      'S'
043500001024     C     orsrme        oreq      'S'
043600001024     C     orsrgi        oreq      'S'
043700001024     C     orsrve        oreq      'S'
043800001024     C                   eval      *in30 = *off
043900001024     C     x             ifeq      1
044000001024     C     orsrlu        andne     'S'
044100001024     C                   iter
044200001024     C                   endif
044300001024     C     x             ifeq      2
044400001024     C     orsrma        andne     'S'
044500001024     C                   iter
044600001024     C                   endif
044700001024     C     x             ifeq      3
044800001024     C     orsrme        andne     'S'
044900001024     C                   iter
045000001024     C                   endif
045100001024     C     x             ifeq      4
045200001024     C     orsrgi        andne     'S'
045300001024     C                   iter
045400001024     C                   endif
045500001024     C     x             ifeq      5
045600001024     C     orsrve        andne     'S'
045700001024     C                   iter
045800001024     C                   endif
045900001024     C                   endif
046000001024     C     x             ifeq      6
046100001024     C     x             oreq      7
046200001024     C                   iter
046300001024     C                   endif
046400001024
046500001024      *    OK RECORD VALIDO (Scrivo Subfile)
046600001024      *    .. se ho premuto F6 tento prima la conferma automatica
046700001024      *    .. richiamando il pgm di creazione ORM
046800001024
046900001024     C                   setoff                                       31  32
047000001024     C     wkf           ifeq      '1'
047100001025     C     *in30         ifeq      *off
047200001024     C                   EXSR      SR_RICHIAMA
047300001024     C     §rmerr        ifne      ' '
047400001024     C                   unlock    fnors01l
047500001024     C     §rmerr        ifeq      '3'
047600001024     C                   seton                                        32
047700001024     C                   endif
047800001024     C                   else
047900001024     C                   seton                                        31
048000001024     C                   move      wdatainv      orsdte
048100040818     c                   Z-add     wdataod       OrsDtt
048200001024     C                   update    fnors000
048300001025     C                   add       1             vconfe
048400001025     C                   add       1             vconfn
048500001025     C                   seton                                        10
048600001024     C                   endif
048700001025     C                   else
048800001025     C                   unlock    fnors01l
048900001025     C                   endif
049000001024     C                   endif
049100001024
049200001024     C     *in31         ifeq      *off
049300001024     C     wkf           ifne      '1'
049400001024     C  N30              clear                   vs1opz
049500001024     C  N30              clear                   vs1err
049600001024     C  N30              eval      *in05 = *off
049700001024     C   30              move      'E'           vs1opz
049800001024     C   30              move      'Giorno ?'    vs1err
049900001024     C   30              eval      *in05 = *on
050000001024     C                   else
050100001024     C     *in32         ifeq      *on
050200001024     C  N30              clear                   vs1opz
050300001024     C  N30              clear                   vs1err
050400001024     C  N30              eval      *in05 = *off
050500001024     C   30              move      'E'           vs1opz
050600001024     C   30              move      'Giorno ?'    vs1err
050700001024     C   30              eval      *in05 = *on
050800001024     C                   else
050900001025     C                   move      'E'           vs1opz
051000001025     C  N30              clear                   vs1err
051100001025     C   30              move      'Giorno ?'    vs1err
051200001024     C                   eval      *in05 = *on
051300001024     C                   endif
051400001024     C                   endif
051500001024     C                   move      orsnor        vs1nor
051600001024     C                   move      orscra        vs1cro
051700010122     C                   move      orsrmp        vs1rmp
051800001024     C     vs1cro        chain     fnacr01l
051900001024     C                   if        %found(fnacr01l)
052000001024     C                   movel     acrrsr        vs1rsr
052100001024     C                   movel     acrinr        vs1inr
052200001024     C                   movel     acrcar        vs1car
052300001024     C                   movel     acrlor        vs1lor
052400001024     C                   movel     acrprr        vs1prr
052500001024     C                   movel     acrnar        vs1nar
052600001024     C                   else
052700001024     C                   clear                   vs1rsr
052800001024     C                   clear                   vs1inr
052900001024     C                   clear                   vs1car
053000001024     C                   clear                   vs1lor
053100001024     C                   clear                   vs1prr
053200001024     C                   clear                   vs1nar
053300001024     C                   endif
053400001024
053500001024     C                   select
053600001024     C                   when      ORSncl <> *zeros
053700001024     C                   movel     voc(1)        vs1voc
053800001024     C                   z-add     ORSncl        vs1qta
053900001024
054000001024     C                   when      ORSpkg <> *zeros
054100001024     C                   movel     voc(2)        vs1voc
054200001024     C                   z-add     ORSpkg        vs1qta
054300001024
054400001024     C                   when      ORSvlm <> *zeros
054500001024     C                   movel     voc(3)        vs1voc
054600001024     C                   z-add     ORSvlm        vs1qta
054700001024
054800001024     C                   when      ORSbnc <> *zeros
054900001024     C                   movel     voc(4)        vs1voc
055000001024     C                   z-add     ORSbnc        vs1qta
055100001024
055200001024     C                   when      ORSblc <> *zeros
055300001024     C                   movel     voc(5)        vs1voc
055400001024     C                   z-add     ORSblc        vs1qta
055500001024
055600001024     C                   when      ORSatt <> *zeros
055700001024     C                   movel     voc(6)        vs1voc
055800001024     C                   z-add     ORSatt        vs1qta
055900001024
056000001024     C                   when      ORSmtc <> *zeros
056100001024     C                   movel     voc(7)        vs1voc
056200001024     C                   z-add     ORSmtc        vs1qta
056300001024
056400001024     C                   other
056500001024     C                   z-add     *zeros        vs1qta
056600001024     C                   movel     *blanks       vs1voc
056700001024
056800001024     C                   endsl
056900001023
057000001024     C     wkm           ifeq      '1'
057100001024     C                   movel     orspoe        wnum10           10 0
057200001024     C                   move      orsnor        wnum10
057300001024     C     wnum10        lookup    DUP                                    34
057400001024     C   34              move      'D'           vs1opz
057500001024     C   34              eval      *in05 = *off
057600001024     C                   endif
057700001024
057800001024     C                   add       1             nn1
057900001024     C                   write     fiorsf1
058000001024     C                   endif
058100001024
058200001023     C                   enddo
058300001025     C                   enddo
058400001023
058500001024      *    EMISSIONE RECORD DI CONTROLLO
058600001024
058700001025     C     vconfn        ifeq      0
058800001025     C                   eval      *in12 = *off
058900001025     C                   else
059000001025     C                   eval      *in12 = *on
059100001025     C                   endif
059200001025     C     vdupln        ifeq      0
059300001025     C                   eval      *in13 = *off
059400001025     C                   else
059500001025     C                   eval      *in13 = *on
059600001025     C                   endif
059700001025
059800001024     C     nn1           ifeq      0
059900001024     C                   exfmt     fior133
060000001024     C                   clear                   wkf
060100001024     C                   else
060200001024     C                   write     fior132
060300001024     C                   exfmt     fiorrc1
060400001024     C     *inkf         ifeq      *on
060500001024     C                   move      '1'           wkf
060600001024     C                   else
060700001024     C                   clear                   wkf
060800001024     C                   endif
060900001024     C                   endif
061000001024
061100001025     C                   z-add     0             vconfn
061200001025     C                   z-add     0             vdupln
061300001025
061400001024     C  N04*inkm         ifeq      *on
061500001024     C                   move      '1'           wkm
061600001024     C                   eval      *in04 = *on
061700001024     C                   else
061800001024     C                   clear                   wkm
061900001024     C                   eval      *in04 = *off
062000001024     C                   endif
062100001024
062200001024      *    LETTURA RECORDS SUBFILE
062300001024
062400001024     C     *inkl         ifeq      *off
062500001024
062600001024     C                   do        *hival
062700001024
062800001024     C     wkm           ifeq      '1'
062900001024     C     *inkm         andeq     *on
063000001024     C                   leave
063100001024     C                   endif
063200001024     C     *inkf         ifeq      *on
063300001024     C                   leave
063400001024     C                   endif
063500001024
063600001030     C     nn1           ifne      0
063700001024     C                   readc     fiorsf1
063800001024     C                   if        %eof
063900001024     C                   leave
064000001024     C                   endif
064100001030     C                   else
064200001030     C                   leave
064300001030     C                   endif
064400001024
064500001024     C     vs1opz        ifeq      '1'
064600001024     C     vs1opz        oreq      '2'
064700001024     C     KFNORS        chain     fnors01l
064800001024     C                   if        %found (fnors01l)
064900001024     C     wkm           ifne      '1'
065000001024     C     orsdte        iflt      wdatainv
065100001024
065200001024      *    Richiamo Programma di Creazione ORM (Conferma)
065300001024
065400001024     C                   EXSR      SR_RICHIAMA
065500001024
065600001024     C     §rmerr        ifne      ' '
065700001024     C                   unlock    fnors01l
065800001024     C                   else
065900001024     C                   move      wdatainv      orsdte
066000040818     c                   Z-add     wdataod       OrsDtt
066100001024     C                   update    fnors000
066200001025     C                   add       1             vconfe
066300001025     C                   add       1             vconfn
066400001025     C                   seton                                        10
066500001024     C                   endif
066600001024     C                   else
066700001024     C                   unlock    fnors01l
066800001024     C                   endif
066900001024     C                   else
067000001024     C     orsdte        ifeq      wdatainv
067100001024
067200001024      *    Richiamo Programma di Creazione ORM (da Duplicazione)
067300001024
067400001024     C                   EXSR      SR_RICHIAMA
067500001024
067600001024     C     §rmerr        ifne      ' '
067700001024     C                   unlock    fnors01l
067800001024     C                   else
067900001024     C                   move      wdatainv      orsdte
068000040818     c                   Z-add     wdataod       OrsDtt
068100001024     C                   update    fnors000
068200001025     C                   add       1             vdupli
068300001025     C                   add       1             vdupln
068400001025     C                   seton                                        11
068500001024     C                   add       1             y                 5 0
068600001024     C                   movel     orspoe        dup(y)
068700001024     C                   move      orsnor        dup(y)
068800001024     C                   endif
068900001024     C                   else
069000001024     C                   unlock    fnors01l
069100001024     C                   endif
069200001024
069300001024     C                   endif
069400001024     C                   endif
069500001024     C                   endif
069600001024
069700001024     C                   enddo
069800001024
069900001024     C                   else
070000001024     C                   leave
070100001024     C                   endif
070200001024
070300001024     C                   enddo
070400001023     C                   endsr
070500001024
070600001024      *---------------------------------------------------------------*
070700001024      *    Richiama PGM Creazione ORM                                 *
070800001024      *---------------------------------------------------------------*
070900001024
071000001024     C     SR_RICHIAMA   begsr
071100001024
071200001024     C                   clear                   FIOR05DS
071300001024     C     wkf           ifne      '1'
071400001024     C                   move      vs1opz        §rmtla
071500001024     C                   else
071600001024     C     vidvis        ifeq      'NO'
071700001024     C                   move      '0'           §rmtla
071800001024     C                   else
071900001024     C                   move      '2'           §rmtla
072000001024     C                   endif
072100001024     C                   endif
072200010131     C                   move      'S'           §rmfpr
072300001024     C                   move      orspoe        §rmpoe
072400001024     C                   move      orstor        §rmtor
072500001129     C                   move      orstco        §rmtco
072600001221     C                   if        orscor > *zeros
072700001024     C                   move      orscor        §rmcor
072800001024     C     §rmcor        chain     fnacr01l
072900001024     C                   if        %found(fnacr01l)
073000001024     C                   movel     acrrsr        §rmrso
073100001024     C                   movel     acrinr        §rmino
073200001024     C                   movel     acrcar        §rmcao
073300001024     C                   movel     acrlor        §rmloo
073400001024     C                   movel     acrprr        §rmpro
073500001024     C                   movel     acrnar        §rmnao
073600001024     C                   else
073700001024     C                   clear                   §rmrso
073800001024     C                   clear                   §rmino
073900001024     C                   clear                   §rmcao
074000001024     C                   clear                   §rmloo
074100001024     C                   clear                   §rmpro
074200001024     C                   clear                   §rmnao
074300001024     C                   endif
074400001221     C                   endif
074500001024     C                   move      orscra        §rmcra
074600001024     C     §rmcra        chain     fnacr01l
074700001024     C                   if        %found(fnacr01l)
074800001024     C                   movel     acrrsr        §rmrsr
074900001024     C                   movel     acrinr        §rminr
075000001024     C                   movel     acrcar        §rmcar
075100001024     C                   movel     acrlor        §rmlor
075200001024     C                   movel     acrprr        §rmprr
075300001024     C                   movel     acrnar        §rmnar
075400001024     C                   else
075500001024     C                   clear                   §rmrsr
075600001024     C                   clear                   §rminr
075700001024     C                   clear                   §rmcar
075800001024     C                   clear                   §rmlor
075900001024     C                   clear                   §rmprr
076000001024     C                   clear                   §rmnar
076100001024     C                   endif
076200001024     C                   move      orscrc        §rmcrc
076300001024     C                   move      orsrsc        §rmrsc
076400001024     C                   move      orsinc        §rminc
076500001024     C                   move      orscac        §rmcac
076600001024     C                   move      orsloc        §rmloc
076700001024     C                   move      orsprc        §rmprc
076800001024     C                   move      orsnac        §rmnac
076900001024     C                   move      orsffd        §rmffd
077000001024     C                   move      wdatainv      §rmdar
077100001024     C                   move      orsorr        §rmorr
077200001024     C                   move      orsrmp        §rmrmp
077300001024     C                   move      orsnam        §rmnam
077400001024     C                   move      orsncl        §rmncl
077500001024     C                   move      orspkg        §rmpkg
077600001024     C                   move      orsvlm        §rmvlm
077700001024     C                   move      orsbnc        §rmbnc
077800001024     C                   move      orsblc        §rmblc
077900001024     C                   move      orsatt        §rmatt
078000001024     C                   move      orsmtc        §rmmtc
078100001024     C                   move      orspag        §rmpag
078200001024     C                   move      orsksc        §rmksc
078300001024     C                   move      orsctr        §rmctr
078400001024     C                   move      orspor        §rmpor
078500001024     C                   move      orszor        §rmzor
078600001024     C                   move      orspoc        §rmpoc
078700001024     C                   move      orsno1        §rmno1
078800001024     C                   move      orsno2        §rmno2
078900001110     C                   move      orsrer        §rmrer
079000001110     C                   move      orster        §rmter
079100001024     C                   move      orscst        §rmcst
079200001024     C                   move      orsvcs        §rmvcs
079300001024     C                   move      orsfcs        §rmfcs
079400001024     C                   move      orspoe        §rmpos
079500001024     C                   move      orsnor        §rmors
079600010118     C                   move      orsspi        §rmspi
079700001024
079800001024     C                   call      'FIOR05R'
079900001024     C                   parm                    KPJBA
080000001024     C                   parm                    FIOR05DS
080100001024
080200001024     C                   endsr
080300130619      *****************************************************************
080400130619      * ROUTINE INIZIALE
080500130619      *****************************************************************
080600130619     C     *INZSR        BEGSR
080700130619
080800130619     C     *ENTRY        PLIST
080900130619     C                   PARM                    KPJBA
081000130619
081100130619      *    Recupero Ragione Sociale Ditta da Emettere a Video
081200130619      *    e Punti Operativi Gestiti
081300130619
081400130619
081500130619     c     *dtaara       define    §azute        azuteds
081600130619     c     *dtaara       define    §datiute      ddatiute
081700130619     c                   in(e)     *dtaara
081800130619     c                   if        %error or rsut = *blanks
081900130619     c                   clear                   tibs34ds
082000130619     c                   call      'TIBS34R'
082100130619     c                   parm                    tibs34ds
082200130619     c                   in        *dtaara
082300130619     c                   endif
082400130619
082500130619      * controllo abilitazione utente
082600130619     c                   clear                   wabi
082700130619     c                   clear                   dLAT
082800130619     c                   eval      dUTE01 = UTEfaf
082900130619
083000130619      * Verifica errori e autorità profilo
083100130619     c                   SELECT
083200130619      * se ho errori nei dati utente esco dal pgm
083300130619     c                   WHEN      DUTerr = 'E'
083400130619     c                   eval      wErrGrave = *on
083500130619     c                   leavesr
083600130619      * carica abilitazioni del profilo
083700130619     c                   WHEN      §UTEorm <> *blanks
083800130619     c                   eval      wabi = §UTEorm
083900130619     c                   WHEN      UTEaut <> *blanks
084000130619     c                   eval      wabi = UTEaut
084100130619      * se non c'è l'abilitazione
084200130619      * --> se 1° livello, abilitazioni al terminal
084300130619      *     se 2° livello, abilitazioni al punto operativo
084400130619      *     se sede no abilitazioni
084500130619     c                   OTHER
084600130619     c                   IF        DUTlpo = '1'
084700130619     c                   eval      wabi   = 'TP'
084800130619     c                   ENDIF
084900130619     c                   IF        DUTlpo = '2'
085000130619     c                   eval      wabi   = 'PO'
085100130619     c                   ENDIF
085200130619     c                   ENDSL
085300130619
085400130619      * controllo se ok l'abilitazione dell'utente
085500130619     c                   clear                   TIBS02DS
085600130619     c                   eval      T02mod = 'C'
085700130619     c                   eval      T02sif = knsif
085800130619     c                   eval      T02cod = 'LAT'
085900130619     c                   eval      T02ke1 = wabi
086000130619     c                   Call      'TIBS02R'
086100130619     c                   Parm                    KPJBA
086200130619     c                   Parm                    TIBS02DS
086300130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
086400130619     c                   eval      wErrGrave = *on
086500130619     c                   leavesr
086600130619     c                   ENDIF
086700130619     c                   eval      dLAT = T02uni
086800130619
086900130619      * Reperimento delle filiali gestibili dall'utente
087000130619     c                   clear                   TRUL31DS
087100130619     c                   eval      I31abi = wabi
087200130619     c                   eval      I31cdi = DUTdis
087300130619     c                   eval      I31car = DUTare
087400130619     c                   eval      I31cpo = DUTpou
087500130619     c                   call      'TRUL31R'
087600130619     c                   parm                    KPJBA
087700130619     c                   parm                    TRUL31DS
087800130619     c                   IF        O31pog > *zeros
087900130619     c                   movea     O31pog        SKpog
088000130619     c                   ELSE
088100130619     c                   eval      wErrGrave = *on
088200130619     c                   leavesr
088300130619     c                   ENDIF
088400130619
088500130619      *    Accesso Secco a FNORS - 1
088600130619
088700130619     C     KFNORS        klist
088800130619     C                   kfld                    vs1pog
088900130619     C                   kfld                    vs1nor
089000130619
089100130619     C                   endsr
089200001019
089300000928** MSG
089400070418Filiale errata o non Gestibile                                                      1
089500001023Data Formalmente non Corretta o Inferiore a Data Odierna                            2
089600010131Immettere se ritiro Mattina o Pomeriggio                                            3
089700020506Attenzione la data ritiro non deve superare 1 settimana da oggi!!!                  4
089800001023..............  Libero                                                              5
089900001023..............  Libero                                                              6
090000001023..............  Libero                                                              7
090100001023..............  Libero                                                              8
090200001023..............  Libero                                                              9
090300001023..............  Libero                                                             10
090400001023..............  Libero                                                             11
090500001023..............  Libero                                                             12
090600001023..............  Libero                                                             13
090700001023..............  Libero                                                             14
090800001023..............  Libero                                                             15
090900001023..............  Libero                                                             16
091000001023..............  Libero                                                             17
091100001023..............  Libero                                                             18
091200001023..............  Libero                                                             19
091300001023..............  Libero                                                             20
091400001023..............  Libero                                                             21
091500001023..............  Libero                                                             22
091600001023..............  Libero                                                             23
091700001023..............  Libero                                                             24
091800001023..............  Libero                                                             25
091900001023..............  Libero                                                             26
092000001023..............  Libero                                                             27
092100001010..............  Libero                                                             28
092200001010..............  Libero                                                             29
092300001010..............  Libero                                                             30
092400001019** GIO
092500001019Lunedì                                                                              1
092600001019Martedì                                                                             2
092700001019Mercoledì                                                                           3
092800001019Giovedì                                                                             4
092900001019Venerdì                                                                             5
093000001019Sabato                                                                              6
093100001019Domenica                                                                            7
093200001024** VOC
093300001024COL                                                                                 1
093400001024PKG                                                                                 2
093500001024VLM                                                                                 3
093600001024BNC                                                                                 4
093700001024BLC                                                                                 5
093800001024AUT                                                                                 6
093900001024MOT                                                                                 7
