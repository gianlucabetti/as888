000100170530     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200170530     h dftactgrp(*no)
000300000927
000400000927      *****************************************************************
000500000927      *                                                               *
000600001019      *       CONFERMA ORDINI RITIRI FISSI                            *
000700000927      *                                                               *
000800000927      *****************************************************************
000900000927
001000001019     FAZORG01L  IF   E           K DISK
001100001024     FFNORS01L  UF   E           K DISK
001200001024     FFNACR01L  IF   E           K DISK
001300001023     FFIOR13D   CF   E             WORKSTN SFILE(FIORSF1:NN1)
001400000927
001500000928     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             MSG VIDEO
001600001024     D GIO             S             20    DIM(7) CTDATA PERRCD(1)              Giorni Settim.
001700001024     D VOC             S              3    DIM(7) CTDATA PERRCD(1)              VOCI A VIDEO
001800001024     D DUP             S             10  0 DIM(5000)                            ORM Duplicati
001900001025     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
002000130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
002100130619
002200000927     D KPJBA         E DS
002300001024     D FIOR05DS      E DS
002400010730     D DORS01        E DS                                                       FNORS ORSFLO
002500000927
002600000927     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002700000927     D  LIN                    1     90  0
002800000927     D                                     DIM(30)
002900130619
003000130619     d azuteds       e ds                  extname(azute00f)
003100130619     d ddatiute      e ds
003200130619     d tibs34ds      e ds
003300130619     d TRUL31DS      e ds
003400130619     D TIBS02DS      E DS
003500170530
003600170530      // - Immissione ORM
003700170530     d FIOR001I      e ds                  QUALIFIED INZ(*EXTDFT)
003800170530     d FIOR001O      e ds                  QUALIFIED INZ(*EXTDFT)
003900170530     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
004000170530     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
004100170530     d FIOR000A      e ds                  QUALIFIED INZ(*EXTDFT)
004200170530
004300170530      // - Reperimento filiali abilitate alla procedura
004400170530     d TRULVPODS     e ds
004500170530     d  filOkNewOrm           16    765    dim(250)
004600130619
004700130619     d dLAT          e ds
004800130619     d dUTE01        e ds
004900001023
005000001023      * Parametri per controllo Data
005100001023     D WLBDAT          DS                  INZ
005200001023     D  G02DAT                 1      8  0
005300001023     D  G02INV                 9     16  0
005400001023     D  G02ERR                17     17
005500001023     D  G02TGI                18     22  0
005600010202
005700010202     D Parm01          DS
005800010202     D  ppoe                          3  0
005900010202     D  pnor                          7  0
006000010202     D  pnsr                          2  0
006100010202     D  pnrv                          2  0
006200010202     D  psce                          1
006300010202     D  pfgs                          3  0
006400010202     D  ppor                          3  0
006500010202     D  pdtr                          8  0
006600010202     D  pmdb                         10
006700010202     D  pprb                         10
006800010202     D  pdts                          8  0
006900010202     D  prmp                          1
007000010202     D  pbrc                          1
007100010202     D  pref                          2
007200010202     D  pmio                          1
007300020506
007400020506     d wdatamax        s               d   datfmt(*eur)
007500020506     d wdatarit        s               d   datfmt(*eur)
007600130619
007700130619     d wabi            s                   like(UTEaut)
007800130619     d wEsci           s               n   inz(*off)
007900130619     d wErrGrave       s               n   inz(*off)
008000130619     d w003a           s              3
008100170530
008200170530      // - Campi per richiamo nuova immissione ORM
008300170530     d RpyOpCode       s             10i 0
008400170530     d RpyIdMsg        s             10i 0
008500170530     d RpyData         s          32767A   varying
008600170530
008700170530      //---------------------------------------------------------------
008800170530      //?Definizione Procedure e Prototipi.
008900170530      //---------------------------------------------------------------
009000170530       // - Caricamento Filiali abilitate
009100170530     d TRULVPOR        pr                  extpgm('TRULVPOR')
009200170530     d  trulvpods                          likeds(trulvpods)
009300170530
009400170530       // - Immissione ORM
009500170530      /copy gaitrasrc/srcconst,FIOR001R
009600170530      /copy gaitrasrc/srcprotopi,FIOR001R
009700170530      /copy gaitrasrc/srcprotopr,FIOR001R
009800000927
009900000927      *---------------------------------------------------------------*
010000000927      *                                                               *
010100000927      *    UTILIZZO DEGLI INDICATORI                                  *
010200000927      *                                                               *
010300000927      *---------------------------------------------------------------*
010400000927      *                                                               *
010500001019      *    01 - Posizione Cursore su P.O. Gestione      FIOR131       *
010600001019      *    02 - Posizione Cursore su Data Ritiro        FIOR131       *
010700001023      *    03 - Disabilito F18                          FIOR131       *
010800001024      *    04 - Disabilito F13                                        *
010900010123      *    05 - Posizione Cursore su M/P/_              FIOR131       *
011000001025      *    10 - Confermato almeno un Ordine                           *
011100001025      *    11 - Duplicato almeno un Ordine                            *
011200001019      *    18 - Sproteggo P.O. Gestione                 FIOR131       *
011300001019      *    28 - Visualizzo messaggio Errore             FIOR131       *
011400001024      *    30 - ORM senza giorno                                      *
011500001024      *    31 - Creato Orm da F6                                      *
011600001024      *    32 - F12 da F6                                             *
011700131003      *    33 - Filiale utente attiva agli Orari Servizi
011800001024      *    34 - Lookup sk duplicati                                   *
011900001024      *    39 - Comodo                                                *
012000001023      *    90 - Riemetto Videate                                      *
012100000927      *                                                               *
012200000927      *---------------------------------------------------------------*
012300000927
012400000927      *****************************************************************
012500000927      *                                                               *
012600000927      *    OPERAZIONI INIZIALI                                        *
012700000927      *                                                               *
012800000927      *****************************************************************
012900000927
013000001024
013100001024      *    Inizializzo Campi Video
013200001024
013300001024     C                   z-add     0             y
013400000927     C                   EXSR      SR_INIVIDEO
013500000927
013600000927      *****************************************************************
013700000927      *                                                               *
013800000927      *    ROUTINE PRINCIPALE                                         *
013900000927      *                                                               *
014000000927      *****************************************************************
014100001011
014200001019     C                   do        *hival
014300130619      /free
014400130619       //?Se utente non autorizzato alla funzione esco
014500130619         IF  wErrGrave;
014600130619           *in28 = *on;
014700130619           wEsci = *on;
014800130619           VIDmsg = 'Utente non autorizzato alla funzione';
014900130619         ENDIF;
015000130619      /end-free
015100001019
015200001019      *    Emissione Prima Videata
015300001019
015400001019     C                   EXFMT     FIOR131
015500130619
015600130619      /free
015700130619         IF  wEsci;
015800130619           leave;
015900130619         ENDIF;
016000130619      /end-free
016100130619
016200001019     C   KC              leave
016300130619
016400001023     C                   setoff                                       289018
016500001019     C                   setoff                                       01  02
016600010123     C                   setoff                                       05
016700001019
016800001019      *    Controllo
016900001019
017000001019     C                   EXSR      SR_CONTROL
017100001019
017200001019      *    Se Vi sono Errori o Voglio Cambiare P.O. Riemetto
017300001023      *    .. Riemetto normalmente anche se non ho premuto F6
017400001019
017500001019     C     *in28         ifeq      *on
017600001019     C     *in18         oreq      *on
017700001023     C     *in90         oreq      *on
017800001019     C                   iter
017900001019     C                   endif
018000001023
018100001023      *   GESTIONE DEGLI ORM
018200001023
018300001023     C                   EXSR      SR_GESTIONE
018400001023
018500001019     C                   enddo
018600001019
018700000927     C                   eval      *inlr = *on
018800000927
018900000927      *****************************************************************
019000000927      *                                                               *
019100000927      *    SUBROUTINES                                                *
019200000927      *                                                               *
019300000927      *****************************************************************
019400000927
019500000927      *---------------------------------------------------------------*
019600000927      *    Inizializzazione Campi Video                               *
019700000927      *---------------------------------------------------------------*
019800000927
019900000927     C     SR_INIVIDEO   begsr
020000000927
020100001025      *    Reset Progressivo Nr. Ordini Confermati/Duplicati
020200001025
020300001025     C                   setoff                                       10  11
020400001025     C                   clear                   vconfe
020500001025     C                   clear                   vdupli
020600001002
020700001019      *    Inizializzo il P.O. Gestione
020800000927
020900130619     c                   eval      vidpog = dutpou
021000001012
021100000927      *    Decodifico il P.O. di Appartenenza
021200000927
021300001019     C     vidpog        chain     azorg01l
021400000927     C                   if        %found(azorg01l)
021500000927     C                   movel     orgdes        viddep
021600000927     C                   else
021700001019     C                   clear                   viddep
021800000927     C                   endif
021900000927
022000000927      *    Inizializzo i Campi Restanti
022100001012
022200001019     C                   move      *date         viddar
022300010123     C                   move      *blanks       vidrm
022400010123     C                   move      *blanks       vidrp
022500010131     C                   move      'SI'          vidvis
022600001019
022700001019     C                   clear                   wgio              1
022800001019     C                   movel     viddar        wggmm             4
022900001019     C                   move      viddar        waa               2
023000001019     C                   movel     wggmm         wggmmaa           6
023100001019     C                   move      waa           wggmmaa
023200001019     C                   call      'XGIOSE1'
023300001019     C                   parm                    wggmmaa
023400001019     C                   parm                    wgio
023500001019     C                   move      wgio          x                 1 0
023600001019     C                   movel(p)  gio(x)        vidgio
023700000927
023800000927     C                   endsr
023900001019
024000001019      *---------------------------------------------------------------*
024100001019      *    Controllo Prima Videata                                    *
024200001019      *---------------------------------------------------------------*
024300001019
024400001019     C     SR_CONTROL    begsr
024500001019
024600001025     C                   z-add     0             vconfn
024700001025     C                   z-add     0             vdupln
024800001025
024900001023     C     *inKF         ifeq      *on
025000001023     C                   move      '1'           wkf
025100001023     C                   else
025200001023     C                   clear                   wkf               1
025300001023     C                   endif
025400001024
025500001024     C     *inKM         ifeq      *on
025600001024     C                   move      '1'           wkm
025700001024     C                   eval      *in04 = *on
025800001024     C                   else
025900001024     C                   eval      *in04 = *off
026000001024     C                   clear                   wkm               1
026100001024     C                   endif
026200001023
026300001019      *    F18=Cambio P.O. Gestione
026400001019
026500001019     C     *inks         ifeq      *on
026600001019     C                   seton                                        18
026700001019     C                   goto      endctr
026800001019     C                   endif
026900001023
027000001023      *    Controllo P.O. Gestione
027100001023
027200001023     C                   clear                   viddep
027300130619     c                   move      VIDpog        w003a
027400130619     c     w003a         lookup    SKpog                                  39
027500001023     C     *in39         ifeq      *off
027600001023     C                   eval      *in01 = *on
027700001023     C                   eval      *in28 = *on
027800001023     C                   eval      *in18 = *on
027900001023     C                   movel     msg(1)        vidmsg
028000001023     C                   goto      endctr
028100001023     C                   else
028200001023     C     vidpog        chain     azorg01l
028300001023     C                   if        %found(azorg01l)
028400001023     C                   movel     orgdes        viddep
028500001023     C                   endif
028600001025      *        Carico £6
028700001025     C                   CLEAR                   DSUL06
028800001025     C                   MOVE      '£6'          D06COD
028900001025     C                   MOVEL     vidpog        D06KEY
029000001025     C                   MOVEL     DSUL06        KPJBU
029100001025     C                   CALL      'TRUL06R'
029200001025     C                   PARM                    KPJBA
029300001025     C                   MOVEL     KPJBU         DSUL06
029400001025     C                   MOVEA     LIN           L6
029500001023     C                   endif
029600001023
029700001023      *    Controllo Data (Corretta Formalmente e non Inferiore a Oggi)
029800001023
029900001023     C                   clear                   vidgio
030000001023     C                   z-add     viddar        G02DAT
030100001023     C                   clear                   G02ERR
030200001023     C                   call      'XSRDA8'
030300001023     C                   parm                    WLBDAT
030400001023     C                   z-add     g02inv        wdatainv          8 0
030500001023     C                   z-add     g02dat        viddar
030600001023     C                   move      g02err        werror            1
030700001023     C                   z-add     *date         G02DAT
030800001023     C                   clear                   G02ERR
030900001023     C                   call      'XSRDA8'
031000001023     C                   parm                    WLBDAT
031100001023     C                   z-add     g02inv        wdataod           8 0
031200001023
031300001023     C     werror        IFEQ      '1'
031400001023     C     viddar        OREQ      0
031500001023     C     wdatainv      orlt      wdataod
031600001023     C                   eval      *in02 = *on
031700001023     C                   eval      *in28 = *on
031800001023     C                   movel     msg(2)        vidmsg
031900001023     C                   goto      endctr
032000001023     C                   else
032100001023     C                   clear                   wgio              1
032200001023     C                   movel     viddar        wggmm             4
032300001023     C                   move      viddar        waa               2
032400001023     C                   movel     wggmm         wggmmaa           6
032500001023     C                   move      waa           wggmmaa
032600001023     C                   call      'XGIOSE1'
032700001023     C                   parm                    wggmmaa
032800001023     C                   parm                    wgio
032900001023     C     wgio          ifne      ' '
033000001023     C                   move      wgio          x                 1 0
033100001023     C                   movel(p)  gio(x)        vidgio
033200001023     C                   else
033300001023     C                   eval      *in02 = *on
033400001023     C                   eval      *in28 = *on
033500001023     C                   movel     msg(2)        vidmsg
033600001023     C                   goto      endctr
033700001023     C                   endif
033800001023     C                   ENDIF
033900020506
034000020506      *    La data richiesta non deve essere maggiore di 1 settimana da oggi
034100020506     c     *iso          move      wdataod       wdatamax
034200020506     c                   adddur    7:*D          wdatamax
034300020506     c     *iso          move      wdatainv      wdatarit
034400020506     c                   if        wdatarit > wdatamax
034500020506     C                   eval      *in02 = *on
034600020506     C                   eval      *in28 = *on
034700020506     C                   movel     msg(4)        vidmsg
034800020506     C                   goto      endctr
034900020506     c                   endif
035000001019
035100001023      *    Se è Stata Modificata la Data Segnalo SEMPRE
035200001023
035300001023     C     wdatainv      ifne      wdataod
035400001023     C                   move      'NO'          vsicur
035500001024     C                   EXFMT     FIOR13W
035600001023     C     *inkl         ifeq      *on
035700001023     C     vsicur        orne      'SI'
035800001023     C                   eval      *in90 = *on
035900001023     C                   goto      endctr
036000001023     C                   endif
036100001023     C                   endif
036200010123
036300010123      *    Almeno un ritiro deve essere impostato M/P/_
036400010123
036500010123     C                   if        vidrm = *blanks and vidrp = *blanks
036600010123     C                   eval      *in05 = *on
036700010123     C                   eval      *in28 = *on
036800010123     C                   movel     msg(3)        vidmsg
036900010123     C                   goto      endctr
037000010123     C
037100010123     C                   endif
037200001023
037300001019     C     ENDCTR        endsr
037400001023
037500001023      *---------------------------------------------------------------*
037600001023      *    Lettura / Gestione degli ORM                               *
037700001023      *---------------------------------------------------------------*
037800001023
037900001023     C     SR_GESTIONE   begsr
038000001023
038100001023      *    Pulisco Subfile e mi posiziono per Lettura Ordini Fissi
038200001024
038300001024     C                   do        *hival
038400001023     C                   z-add     0             nn1               4 0
038500001023     C                   eval      *in85 = *on
038600001023     C                   write     fiorrc1
038700001023     C                   eval      *in85 = *off
038800001023     C                   z-add     1             recsf
038900001025
039000001025      *    Carico gli ordini della £6
039100001025
039200001025     C     1             do        30            zz                3 0
039300001025
039400001025     C     L6(zz)        ifeq      0
039500001025     C                   iter
039600001025     C                   endif
039700001025
039800001025     C                   z-add     L6(zz)        vs1pog
039900001025
040000001025     C     vs1pog        setll     fnors01l
040100001023
040200001023      *    Leggo Ordini Fissi
040300001023
040400001023     C                   do        *hival
040500001024     C     wkf           ifne      '1'
040600001025     C     vs1pog        reade(n)  fnors01l
040700001024     C                   else
040800001025     C     vs1pog        reade     fnors01l
040900001024     C                   endif
041000001023
041100001023     C                   if        %eof(fnors01l)
041200001023     C                   leave
041300001023     C                   endif
041400001023
041500001024     C     nn1           ifge      4000
041600001024     C                   leave
041700001024     C                   endif
041800001024
041900001024      *    CONTROLLO VALIDITA' RECORD LETTO:
042000001023
042100001024      *    No Annullati
042200001024
042300001023     C     orsatb        ifeq      'A'
042400001023     C                   iter
042500001023     C                   endif
042600010730
042700010730      *    Chiuso per ferie se periodo chiusura presente in anagrafica
042800010730
042900010730     c                   movel     orsflo        dors01
043000010730
043100010730     c                   if        §orsdif > 0  and wdatainv >=  §orsdif and
043200010730     c                             wdatainv <= §orsdff
043300010730     c                   iter
043400010730     c                   endif
043500010123
043600010124      *    Mattina - Pomeriggio
043700010122
043800010123     C                   if        orsrmp = 'P' and vidrp = *blanks
043900010123     C                   iter
044000010123     C                   endif
044100010123     C                   if        orsrmp = 'M' and vidrm = *blanks
044200010123     C                   iter
044300010123     C                   endif
044400001023
044500001024      *    No Già Elaborati se voglio Confermare
044600001024      *    No Non Elaborati se voglio Duplicare
044700001024
044800001024     C     wkm           ifne      '1'
044900001024     C     orsdte        ifge      wdatainv
045000001024     C                   iter
045100001024     C                   endif
045200001024     C                   else
045300001024     C     orsdte        ifne      wdatainv
045400001024     C                   iter
045500001024     C                   endif
045600001024     C                   endif
045700001023
045800001024      *    Giorni
045900001024
046000001024     C                   eval      *in30 = *on
046100001024
046200001024     C     orsrlu        ifeq      'S'
046300001024     C     orsrma        oreq      'S'
046400001024     C     orsrme        oreq      'S'
046500001024     C     orsrgi        oreq      'S'
046600001024     C     orsrve        oreq      'S'
046700001024     C                   eval      *in30 = *off
046800001024     C     x             ifeq      1
046900001024     C     orsrlu        andne     'S'
047000001024     C                   iter
047100001024     C                   endif
047200001024     C     x             ifeq      2
047300001024     C     orsrma        andne     'S'
047400001024     C                   iter
047500001024     C                   endif
047600001024     C     x             ifeq      3
047700001024     C     orsrme        andne     'S'
047800001024     C                   iter
047900001024     C                   endif
048000001024     C     x             ifeq      4
048100001024     C     orsrgi        andne     'S'
048200001024     C                   iter
048300001024     C                   endif
048400001024     C     x             ifeq      5
048500001024     C     orsrve        andne     'S'
048600001024     C                   iter
048700001024     C                   endif
048800001024     C                   endif
048900001024     C     x             ifeq      6
049000001024     C     x             oreq      7
049100001024     C                   iter
049200001024     C                   endif
049300001024
049400001024      *    OK RECORD VALIDO (Scrivo Subfile)
049500001024      *    .. se ho premuto F6 tento prima la conferma automatica
049600001024      *    .. richiamando il pgm di creazione ORM
049700001024
049800001024     C                   setoff                                       31  32
049900001024     C     wkf           ifeq      '1'
050000001025     C     *in30         ifeq      *off
050100001024     C                   EXSR      SR_RICHIAMA
050200170530
050300170530         IF  %lookup(%editc(DUTpou:'X'):filOkNewOrm) > 0 or
050400170530             %lookup('999':filOkNewOrm) > 0;
050500170530           IF  fior001o.tastoF12 <> *blanks or
050600170530               fior001o.errore <> *blanks;
050700170530             unlock FNORS01L;
050800170530             IF  fior001o.errore <> *blanks;
050900170530               *in32 = *on;
051000170530             ENDIF;
051100170530           ELSE;
051200170530             *in31 = *on;
051300170530             ORSdte = wdatainv;
051400170530             ORSdtt = wdataod;
051500170530             update FNORS000;
051600170530             vconfe += 1;
051700170530             vconfn += 1;
051800170530             *in10 = *on;
051900170530           ENDIF;
052000170530         ELSE;
052100001024     C     §rmerr        ifne      ' '
052200001024     C                   unlock    fnors01l
052300001024     C     §rmerr        ifeq      '3'
052400001024     C                   seton                                        32
052500001024     C                   endif
052600001024     C                   else
052700001024     C                   seton                                        31
052800001024     C                   move      wdatainv      orsdte
052900040818     c                   Z-add     wdataod       OrsDtt
053000001024     C                   update    fnors000
053100001025     C                   add       1             vconfe
053200001025     C                   add       1             vconfn
053300001025     C                   seton                                        10
053400001024     C                   endif
053500170530         ENDIF;
053600001025     C                   else
053700001025     C                   unlock    fnors01l
053800001025     C                   endif
053900001024     C                   endif
054000001024
054100001024     C     *in31         ifeq      *off
054200001024     C     wkf           ifne      '1'
054300001024     C  N30              clear                   vs1opz
054400001024     C  N30              clear                   vs1err
054500001024     C  N30              eval      *in05 = *off
054600001024     C   30              move      'E'           vs1opz
054700001024     C   30              move      'Giorno ?'    vs1err
054800001024     C   30              eval      *in05 = *on
054900001024     C                   else
055000001024     C     *in32         ifeq      *on
055100001024     C  N30              clear                   vs1opz
055200001024     C  N30              clear                   vs1err
055300001024     C  N30              eval      *in05 = *off
055400001024     C   30              move      'E'           vs1opz
055500001024     C   30              move      'Giorno ?'    vs1err
055600001024     C   30              eval      *in05 = *on
055700001024     C                   else
055800001025     C                   move      'E'           vs1opz
055900001025     C  N30              clear                   vs1err
056000001025     C   30              move      'Giorno ?'    vs1err
056100001024     C                   eval      *in05 = *on
056200001024     C                   endif
056300001024     C                   endif
056400001024     C                   move      orsnor        vs1nor
056500001024     C                   move      orscra        vs1cro
056600010122     C                   move      orsrmp        vs1rmp
056700001024     C     vs1cro        chain     fnacr01l
056800001024     C                   if        %found(fnacr01l)
056900001024     C                   movel     acrrsr        vs1rsr
057000001024     C                   movel     acrinr        vs1inr
057100001024     C                   movel     acrcar        vs1car
057200001024     C                   movel     acrlor        vs1lor
057300001024     C                   movel     acrprr        vs1prr
057400001024     C                   movel     acrnar        vs1nar
057500001024     C                   else
057600001024     C                   clear                   vs1rsr
057700001024     C                   clear                   vs1inr
057800001024     C                   clear                   vs1car
057900001024     C                   clear                   vs1lor
058000001024     C                   clear                   vs1prr
058100001024     C                   clear                   vs1nar
058200001024     C                   endif
058300001024
058400001024     C                   select
058500001024     C                   when      ORSncl <> *zeros
058600001024     C                   movel     voc(1)        vs1voc
058700001024     C                   z-add     ORSncl        vs1qta
058800001024
058900001024     C                   when      ORSpkg <> *zeros
059000001024     C                   movel     voc(2)        vs1voc
059100001024     C                   z-add     ORSpkg        vs1qta
059200001024
059300001024     C                   when      ORSvlm <> *zeros
059400001024     C                   movel     voc(3)        vs1voc
059500001024     C                   z-add     ORSvlm        vs1qta
059600001024
059700001024     C                   when      ORSbnc <> *zeros
059800001024     C                   movel     voc(4)        vs1voc
059900001024     C                   z-add     ORSbnc        vs1qta
060000001024
060100001024     C                   when      ORSblc <> *zeros
060200001024     C                   movel     voc(5)        vs1voc
060300001024     C                   z-add     ORSblc        vs1qta
060400001024
060500001024     C                   when      ORSatt <> *zeros
060600001024     C                   movel     voc(6)        vs1voc
060700001024     C                   z-add     ORSatt        vs1qta
060800001024
060900001024     C                   when      ORSmtc <> *zeros
061000001024     C                   movel     voc(7)        vs1voc
061100001024     C                   z-add     ORSmtc        vs1qta
061200001024
061300001024     C                   other
061400001024     C                   z-add     *zeros        vs1qta
061500001024     C                   movel     *blanks       vs1voc
061600001024
061700001024     C                   endsl
061800001023
061900001024     C     wkm           ifeq      '1'
062000001024     C                   movel     orspoe        wnum10           10 0
062100001024     C                   move      orsnor        wnum10
062200001024     C     wnum10        lookup    DUP                                    34
062300001024     C   34              move      'D'           vs1opz
062400001024     C   34              eval      *in05 = *off
062500001024     C                   endif
062600001024
062700001024     C                   add       1             nn1
062800001024     C                   write     fiorsf1
062900001024     C                   endif
063000001024
063100001023     C                   enddo
063200001025     C                   enddo
063300001023
063400001024      *    EMISSIONE RECORD DI CONTROLLO
063500001024
063600001025     C     vconfn        ifeq      0
063700001025     C                   eval      *in12 = *off
063800001025     C                   else
063900001025     C                   eval      *in12 = *on
064000001025     C                   endif
064100001025     C     vdupln        ifeq      0
064200001025     C                   eval      *in13 = *off
064300001025     C                   else
064400001025     C                   eval      *in13 = *on
064500001025     C                   endif
064600001025
064700001024     C     nn1           ifeq      0
064800001024     C                   exfmt     fior133
064900001024     C                   clear                   wkf
065000001024     C                   else
065100001024     C                   write     fior132
065200001024     C                   exfmt     fiorrc1
065300001024     C     *inkf         ifeq      *on
065400001024     C                   move      '1'           wkf
065500001024     C                   else
065600001024     C                   clear                   wkf
065700001024     C                   endif
065800001024     C                   endif
065900001024
066000001025     C                   z-add     0             vconfn
066100001025     C                   z-add     0             vdupln
066200001025
066300001024     C  N04*inkm         ifeq      *on
066400001024     C                   move      '1'           wkm
066500001024     C                   eval      *in04 = *on
066600001024     C                   else
066700001024     C                   clear                   wkm
066800001024     C                   eval      *in04 = *off
066900001024     C                   endif
067000001024
067100001024      *    LETTURA RECORDS SUBFILE
067200001024
067300001024     C     *inkl         ifeq      *off
067400001024
067500001024     C                   do        *hival
067600001024
067700001024     C     wkm           ifeq      '1'
067800001024     C     *inkm         andeq     *on
067900001024     C                   leave
068000001024     C                   endif
068100001024     C     *inkf         ifeq      *on
068200001024     C                   leave
068300001024     C                   endif
068400001024
068500001030     C     nn1           ifne      0
068600001024     C                   readc     fiorsf1
068700001024     C                   if        %eof
068800001024     C                   leave
068900001024     C                   endif
069000001030     C                   else
069100001030     C                   leave
069200001030     C                   endif
069300001024
069400001024     C     vs1opz        ifeq      '1'
069500001024     C     vs1opz        oreq      '2'
069600001024     C     KFNORS        chain     fnors01l
069700001024     C                   if        %found (fnors01l)
069800001024     C     wkm           ifne      '1'
069900001024     C     orsdte        iflt      wdatainv
070000001024
070100001024      *    Richiamo Programma di Creazione ORM (Conferma)
070200001024
070300001024     C                   EXSR      SR_RICHIAMA
070400001024
070500170530         IF  %lookup(%editc(DUTpou:'X'):filOkNewOrm) > 0 or
070600170530             %lookup('999':filOkNewOrm) > 0;
070700170530           IF  fior001o.tastoF12 <> *blanks or
070800170530               fior001o.errore <> *blanks;
070900170530             unlock FNORS01L;
071000170530           ELSE;
071100170530             ORSdte = wdatainv;
071200170530             ORSdtt = wdataod;
071300170530             update FNORS000;
071400170530             vconfe += 1;
071500170530             vconfn += 1;
071600170530             *in10 = *on;
071700170530           ENDIF;
071800170530         ELSE;
071900001024     C     §rmerr        ifne      ' '
072000001024     C                   unlock    fnors01l
072100001024     C                   else
072200001024     C                   move      wdatainv      orsdte
072300040818     c                   Z-add     wdataod       OrsDtt
072400001024     C                   update    fnors000
072500001025     C                   add       1             vconfe
072600001025     C                   add       1             vconfn
072700001025     C                   seton                                        10
072800001024     C                   endif
072900170530         ENDIF;
073000001024     C                   else
073100001024     C                   unlock    fnors01l
073200001024     C                   endif
073300001024     C                   else
073400001024     C     orsdte        ifeq      wdatainv
073500001024
073600001024      *    Richiamo Programma di Creazione ORM (da Duplicazione)
073700001024
073800001024     C                   EXSR      SR_RICHIAMA
073900170530
074000170530         IF  %lookup(%editc(DUTpou:'X'):filOkNewOrm) > 0 or
074100170530             %lookup('999':filOkNewOrm) > 0;
074200170530           IF  fior001o.tastoF12 <> *blanks or
074300170530               fior001o.errore <> *blanks;
074400170530             unlock FNORS01L;
074500170530           ELSE;
074600170530             ORSdte = wdatainv;
074700170530             ORSdtt = wdataod;
074800170530             update FNORS000;
074900170530             vdupli += 1;
075000170530             vdupln += 1;
075100170530             *in11 = *on;
075200170530             y += 1;
075300170530             dup(y) = (ORSpoe * 10000000) + ORSnor;
075400170530           ENDIF;
075500170530         ELSE;
075600001024     C     §rmerr        ifne      ' '
075700001024     C                   unlock    fnors01l
075800001024     C                   else
075900001024     C                   move      wdatainv      orsdte
076000040818     c                   Z-add     wdataod       OrsDtt
076100001024     C                   update    fnors000
076200001025     C                   add       1             vdupli
076300001025     C                   add       1             vdupln
076400001025     C                   seton                                        11
076500001024     C                   add       1             y                 5 0
076600001024     C                   movel     orspoe        dup(y)
076700001024     C                   move      orsnor        dup(y)
076800001024     C                   endif
076900170530         ENDIF;
077000001024     C                   else
077100001024     C                   unlock    fnors01l
077200001024     C                   endif
077300001024
077400001024     C                   endif
077500001024     C                   endif
077600001024     C                   endif
077700001024
077800001024     C                   enddo
077900001024
078000001024     C                   else
078100001024     C                   leave
078200001024     C                   endif
078300001024
078400001024     C                   enddo
078500001023     C                   endsr
078600001024
078700001024      *---------------------------------------------------------------*
078800001024      *    Richiama PGM Creazione ORM                                 *
078900001024      *---------------------------------------------------------------*
079000001024
079100001024     C     SR_RICHIAMA   begsr
079200170530
079300170530       //?Filiale utente abilitata alla nuova immissione ORM
079400170530         IF  %lookup(%editc(DUTpou:'X'):filOkNewOrm) > 0 or
079500170530             %lookup('999':filOkNewOrm) > 0;
079600170530           reset FIORA0M0;
079700170530           reset FIORA0F0;
079800170530           reset FIOR000A;
079900170530
080000170530           fior001i.RSUT  = rsut;
080100170530           fior001i.SISINF = knsif;
080200170530           fior001i.UTENTE = dutute;
080300170530           fior001i.FILUTE = dutpou;
080400170530           fior001i.ABIUTE = wabi;
080500170530           fior001i.POEI = vidpog;
080600170530           fior001i.POE = ORSpoe;
080700170530           fior001i.TOR = ORStor;
080800170530           fior001i.CRA1 = %dec(%subst(%editc(ORScra:'X'):1:3):3:0);
080900170530           fior001i.CRA2 = %dec(%subst(%editc(ORScra:'X'):4:4):4:0);
081000170530           fior001i.CRA3 = %dec(%subst(%editc(ORScra:'X'):8:3):3:0);
081100170530           chain (ORScra) FNACR01L;
081200170530           IF  %found(FNACR01L);
081300170530             fior001i.rsr = ACRrsr;
081400170530             fior001i.inr = ACRinr;
081500170530             fior001i.car = ACRcar;
081600170530             fior001i.lor = ACRlor;
081700170530             fior001i.prr = ACRprr;
081800170530             fior001i.nar = ACRnar;
081900170530           ENDIF;
082000170530           fior001i.RER = ORSrer;
082100170530           fior001i.TER = ORSter;
082200170530           fior001i.NAM = ORSnam;
082300170530           fior001i.NOTA1 = ORSno1;
082400170530           fior001i.NOTA2 = ORSno2;
082500170530           fior001i.POR = ORSpor;
082600170530           fior001i.COR1 = %dec(%subst(%editc(ORScor:'X'):1:3):3:0);
082700170530           fior001i.COR2 = %dec(%subst(%editc(ORScor:'X'):4:4):4:0);
082800170530           fior001i.COR3 = %dec(%subst(%editc(ORScor:'X'):8:3):3:0);
082900170530           chain (ORScor) FNACR01L;
083000170530           IF  %found(FNACR01L);
083100170530             fior001i.rso = ACRrsr;
083200170530             fior001i.ino = ACRinr;
083300170530             fior001i.cao = ACRcar;
083400170530             fior001i.loo = ACRlor;
083500170530             fior001i.pro = ACRprr;
083600170530             fior001i.nao = ACRnar;
083700170530           ENDIF;
083800170530           fior001i.CRC1 = %dec(%subst(%editc(ORScrc:'X'):1:3):3:0);
083900170530           fior001i.CRC2 = %dec(%subst(%editc(ORScrc:'X'):4:4):4:0);
084000170530           fior001i.CRC3 = %dec(%subst(%editc(ORScrc:'X'):8:3):3:0);
084100170530           chain (ORScrc) FNACR01L;
084200170530           IF  %found(FNACR01L);
084300170530             fior001i.rsc = ACRrsr;
084400170530             fior001i.inc = ACRinr;
084500170530             fior001i.cac = ACRcar;
084600170530             fior001i.loc = ACRlor;
084700170530             fior001i.prc = ACRprr;
084800170530             fior001i.nac = ACRnar;
084900170530           ENDIF;
085000170530           fior001i.POC = ORSPoc;
085100170530           fior001i.PAG = ORSpag;
085200170530           fior001i.KSC = ORSksc;
085300170530           fior001i.CTR = ORSctr;
085400170530           fior001i.ORR = ORSorr;
085500170530           fior001i.DPM = viddar;
085600170530           fior001i.NCL = ORSncl;
085700170530           fior001i.PKG = ORSpkg;
085800170530           fior001i.VLM = ORSvlm;
085900170530           fior001i.BNC = ORSbnc;
086000170530           fior001i.SPI = ORSspi;
086100170530           fior001i.BLC = ORSblc;
086200170530           fior001i.ATT = ORSatt;
086300170530           fior001i.MTC = ORSmtc;
086400170530           fior001i.FFD = ORSffd;
086500170530           fior001i.POS = ORSpoe;
086600170530           fior001i.ORS = ORSnor;
086700170530
086800170530           // -?Richiamo FIOR001R?
086900170530           fior001_Controller (FIOR001_RQSOPCODE_CONFERMA_FISSI:
087000170530                               RpyOpCode:
087100170530                               RpyIdMsg:
087200170530                               FIOR001I.formato:
087300170530                               FIOR001I:
087400170530                               %size(FIOR001I):
087500170530                               FIOR001O.formato:
087600170530                               FIOR001O:
087700170530                               %size(FIOR001O):
087800170530                               FIORA0M0.formato:
087900170530                               FIORA0M0:
088000170530                               %size(FIORA0M0):
088100170530                               FIORA0F0.formato:
088200170530                               FIORA0F0:
088300170530                               %size(FIORA0F0):
088400170530                               FIOR000A.formato:
088500170530                               FIOR000A:
088600170530                               %size(FIOR000A):
088700170530                               kpjba);
088800170530
088900170530         ELSE;
089000001024
089100001024     C                   clear                   FIOR05DS
089200001024     C     wkf           ifne      '1'
089300001024     C                   move      vs1opz        §rmtla
089400001024     C                   else
089500001024     C     vidvis        ifeq      'NO'
089600001024     C                   move      '0'           §rmtla
089700001024     C                   else
089800001024     C                   move      '2'           §rmtla
089900001024     C                   endif
090000001024     C                   endif
090100010131     C                   move      'S'           §rmfpr
090200001024     C                   move      orspoe        §rmpoe
090300001024     C                   move      orstor        §rmtor
090400001129     C                   move      orstco        §rmtco
090500001221     C                   if        orscor > *zeros
090600001024     C                   move      orscor        §rmcor
090700001024     C     §rmcor        chain     fnacr01l
090800001024     C                   if        %found(fnacr01l)
090900001024     C                   movel     acrrsr        §rmrso
091000001024     C                   movel     acrinr        §rmino
091100001024     C                   movel     acrcar        §rmcao
091200001024     C                   movel     acrlor        §rmloo
091300001024     C                   movel     acrprr        §rmpro
091400001024     C                   movel     acrnar        §rmnao
091500001024     C                   else
091600001024     C                   clear                   §rmrso
091700001024     C                   clear                   §rmino
091800001024     C                   clear                   §rmcao
091900001024     C                   clear                   §rmloo
092000001024     C                   clear                   §rmpro
092100001024     C                   clear                   §rmnao
092200001024     C                   endif
092300001221     C                   endif
092400001024     C                   move      orscra        §rmcra
092500001024     C     §rmcra        chain     fnacr01l
092600001024     C                   if        %found(fnacr01l)
092700001024     C                   movel     acrrsr        §rmrsr
092800001024     C                   movel     acrinr        §rminr
092900001024     C                   movel     acrcar        §rmcar
093000001024     C                   movel     acrlor        §rmlor
093100001024     C                   movel     acrprr        §rmprr
093200001024     C                   movel     acrnar        §rmnar
093300001024     C                   else
093400001024     C                   clear                   §rmrsr
093500001024     C                   clear                   §rminr
093600001024     C                   clear                   §rmcar
093700001024     C                   clear                   §rmlor
093800001024     C                   clear                   §rmprr
093900001024     C                   clear                   §rmnar
094000001024     C                   endif
094100001024     C                   move      orscrc        §rmcrc
094200001024     C                   move      orsrsc        §rmrsc
094300001024     C                   move      orsinc        §rminc
094400001024     C                   move      orscac        §rmcac
094500001024     C                   move      orsloc        §rmloc
094600001024     C                   move      orsprc        §rmprc
094700001024     C                   move      orsnac        §rmnac
094800001024     C                   move      orsffd        §rmffd
094900001024     C                   move      wdatainv      §rmdar
095000001024     C                   move      orsorr        §rmorr
095100001024     C                   move      orsrmp        §rmrmp
095200001024     C                   move      orsnam        §rmnam
095300001024     C                   move      orsncl        §rmncl
095400001024     C                   move      orspkg        §rmpkg
095500001024     C                   move      orsvlm        §rmvlm
095600001024     C                   move      orsbnc        §rmbnc
095700001024     C                   move      orsblc        §rmblc
095800001024     C                   move      orsatt        §rmatt
095900001024     C                   move      orsmtc        §rmmtc
096000001024     C                   move      orspag        §rmpag
096100001024     C                   move      orsksc        §rmksc
096200001024     C                   move      orsctr        §rmctr
096300001024     C                   move      orspor        §rmpor
096400001024     C                   move      orszor        §rmzor
096500001024     C                   move      orspoc        §rmpoc
096600001024     C                   move      orsno1        §rmno1
096700001024     C                   move      orsno2        §rmno2
096800001110     C                   move      orsrer        §rmrer
096900001110     C                   move      orster        §rmter
097000001024     C                   move      orscst        §rmcst
097100001024     C                   move      orsvcs        §rmvcs
097200001024     C                   move      orsfcs        §rmfcs
097300001024     C                   move      orspoe        §rmpos
097400001024     C                   move      orsnor        §rmors
097500010118     C                   move      orsspi        §rmspi
097600150309
097700150309     c                   clear                   kpjbu
097800001024
097900001024     C                   call      'FIOR05R'
098000001024     C                   parm                    KPJBA
098100001024     C                   parm                    FIOR05DS
098200170530
098300170530         ENDIF;
098400001024
098500001024     C                   endsr
098600130619      *****************************************************************
098700130619      * ROUTINE INIZIALE
098800130619      *****************************************************************
098900130619     C     *INZSR        BEGSR
099000130619
099100130619     C     *ENTRY        PLIST
099200130619     C                   PARM                    KPJBA
099300130619
099400130619      *    Recupero Ragione Sociale Ditta da Emettere a Video
099500130619      *    e Punti Operativi Gestiti
099600130619
099700130619
099800130619     c     *dtaara       define    §azute        azuteds
099900130619     c     *dtaara       define    §datiute      ddatiute
100000130619     c                   in(e)     *dtaara
100100130619     c                   if        %error or rsut = *blanks
100200130619     c                   clear                   tibs34ds
100300130619     c                   call      'TIBS34R'
100400130619     c                   parm                    tibs34ds
100500130619     c                   in        *dtaara
100600130619     c                   endif
100700130619
100800130619      * controllo abilitazione utente
100900130619     c                   clear                   wabi
101000130619     c                   clear                   dLAT
101100130619     c                   eval      dUTE01 = UTEfaf
101200130619
101300130619      * Verifica errori e autorità profilo
101400130619     c                   SELECT
101500130619      * se ho errori nei dati utente esco dal pgm
101600130619     c                   WHEN      DUTerr = 'E'
101700130619     c                   eval      wErrGrave = *on
101800130619     c                   leavesr
101900130619      * carica abilitazioni del profilo
102000130619     c                   WHEN      §UTEorm <> *blanks
102100130619     c                   eval      wabi = §UTEorm
102200130619     c                   WHEN      UTEaut <> *blanks
102300130619     c                   eval      wabi = UTEaut
102400130619      * se non c'è l'abilitazione
102500130619      * --> se 1° livello, abilitazioni al terminal
102600130619      *     se 2° livello, abilitazioni al punto operativo
102700130619      *     se sede no abilitazioni
102800130619     c                   OTHER
102900130619     c                   IF        DUTlpo = '1'
103000130619     c                   eval      wabi   = 'TP'
103100130619     c                   ENDIF
103200130619     c                   IF        DUTlpo = '2'
103300130619     c                   eval      wabi   = 'PO'
103400130619     c                   ENDIF
103500130619     c                   ENDSL
103600130619
103700130619      * controllo se ok l'abilitazione dell'utente
103800130619     c                   clear                   TIBS02DS
103900130619     c                   eval      T02mod = 'C'
104000130619     c                   eval      T02sif = knsif
104100130619     c                   eval      T02cod = 'LAT'
104200130619     c                   eval      T02ke1 = wabi
104300130619     c                   Call      'TIBS02R'
104400130619     c                   Parm                    KPJBA
104500130619     c                   Parm                    TIBS02DS
104600130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
104700130619     c                   eval      wErrGrave = *on
104800130619     c                   leavesr
104900130619     c                   ENDIF
105000130619     c                   eval      dLAT = T02uni
105100130619
105200130619      * Reperimento delle filiali gestibili dall'utente
105300130619     c                   clear                   TRUL31DS
105400130619     c                   eval      I31abi = wabi
105500130619     c                   eval      I31cdi = DUTdis
105600130619     c                   eval      I31car = DUTare
105700130619     c                   eval      I31cpo = DUTpou
105800130619     c                   call      'TRUL31R'
105900130619     c                   parm                    KPJBA
106000130619     c                   parm                    TRUL31DS
106100130619     c                   IF        O31pog > *zeros
106200130619     c                   movea     O31pog        SKpog
106300130619     c                   ELSE
106400130619     c                   eval      wErrGrave = *on
106500130619     c                   leavesr
106600130619     c                   ENDIF
106700170530
106800170530       //?Carico le filiali abilitate alla procedura
106900170530         clear TRULVPODS;
107000170530         IVPOke1 = 'DECOFINEWORM';
107100170530         trulvpor (trulvpods);
107200130619
107300130619      *    Accesso Secco a FNORS - 1
107400130619
107500130619     C     KFNORS        klist
107600130619     C                   kfld                    vs1pog
107700130619     C                   kfld                    vs1nor
107800130619
107900130619     C                   endsr
108000001019
108100000928** MSG
108200070418Filiale errata o non Gestibile                                                      1
108300001023Data Formalmente non Corretta o Inferiore a Data Odierna                            2
108400010131Immettere se ritiro Mattina o Pomeriggio                                            3
108500020506Attenzione la data ritiro non deve superare 1 settimana da oggi!!!                  4
108600001023..............  Libero                                                              5
108700001023..............  Libero                                                              6
108800001023..............  Libero                                                              7
108900001023..............  Libero                                                              8
109000001023..............  Libero                                                              9
109100001023..............  Libero                                                             10
109200001023..............  Libero                                                             11
109300001023..............  Libero                                                             12
109400001023..............  Libero                                                             13
109500001023..............  Libero                                                             14
109600001023..............  Libero                                                             15
109700001023..............  Libero                                                             16
109800001023..............  Libero                                                             17
109900001023..............  Libero                                                             18
110000001023..............  Libero                                                             19
110100001023..............  Libero                                                             20
110200001023..............  Libero                                                             21
110300001023..............  Libero                                                             22
110400001023..............  Libero                                                             23
110500001023..............  Libero                                                             24
110600001023..............  Libero                                                             25
110700001023..............  Libero                                                             26
110800001023..............  Libero                                                             27
110900001010..............  Libero                                                             28
111000001010..............  Libero                                                             29
111100001010..............  Libero                                                             30
111200001019** GIO
111300001019Lunedì                                                                              1
111400001019Martedì                                                                             2
111500001019Mercoledì                                                                           3
111600001019Giovedì                                                                             4
111700001019Venerdì                                                                             5
111800001019Sabato                                                                              6
111900001019Domenica                                                                            7
112000001024** VOC
112100001024COL                                                                                 1
112200001024PKG                                                                                 2
112300001024VLM                                                                                 3
112400001024BNC                                                                                 4
112500001024BLC                                                                                 5
112600001024AUT                                                                                 6
112700001024MOT                                                                                 7
