000100040119      *===============================================================*
000200140523      * TICFQ0R    * Confronto fatturazione con tariffe in UNITRAGRU
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000140407     FticfQ0d   cf   e             workstn
001100040119      *
001200140407     D msg             S             78    DIM(6) CTDATA PERRCD(1)
001300050614
001400050526     d kpjba         e ds
001500130123
001600050614     d param           ds
001700050614     D  pardrf                 1      8  0
001701140407     D  pardtc                 9     16  0
001800140407     D  pardal                17     24  0
001900140407     D  paral                 25     32  0
002100140407     d  parmlib               33     42
002200140407     d  parmlibb              43     52
002400130718
002500130718     d wData           ds
002600130718     D  Wgiorno                1      2  0
002700130718     D  Wmese                  3      4  0
002800130718     D  Wanno                  5      8  0
002900130123
003000050613     d Azuteds       e ds                  Extname(Azute00f)
003100050613     d dDatiute      e ds
003200050613     d Tibs34ds      e ds                  Inz
003300050614     D WLBDA8          DS                  INZ
003400050614     D  G08DAT                 1      8  0
003500050614     D  G08INV                 9     16  0
003600050614     D  G08ERR                17     17
003700050614     D  G08TGI                18     22  0
003800130130
003900130718     d w0140           s             14  0
004000130130     d Err_lib         s              1
004100130130     d Libreria        s             10
004200130130     d Testo           s             50
004201140523     d Wmesel          s              2
004202140523       // -?Campi Data
004203140523     d wData_ISO       s               d   datfmt(*iso)
004204140523     d wData_EUR       s               d   datfmt(*eur)
004205140523     d wDate           s              8s 0 inz
004300040119
004400040119      *
004500050613     c     *entry        plist
004600050613     c                   parm                    kpjba
004800130123      *
005800050613     c     *dtaara       define    §azute        azuteds
005900050613     c     *dtaara       define    §datiute      ddatiute
006000050613     c                   in(E)     *dtaara
006100050613     c                   If        %error  or RSUT = *blanks
006200050613     c                   CLEAR                   tibs34ds
006300050613     c                   CALL      'TIBS34R'
006400050613     c                   PARM                    tibs34ds
006500050613     c                   in        *dtaara
006600050613     c                   EndIf
006700130718      *
006800130718      * Reperisco data corrente
006900130718     c                   time                    w0140
007000130718     c                   move      w0140         wData
007001140407
007002140407     C                   move      W0140         G08dat
007003140407     C                   CALL      'XSRDA8'
007004140407     C                   PARM                    WLBDA8
007010140407     C                   Z-ADD     G08INV        dateu             8 0
007200050614     c*
007300050614     c                   clear                   v1cdrf
007301140407     c                   clear                   v1cdtc
007400140407     c                   clear                   v1cdal
007401140407     c                   clear                   v1cal
007500140407      *
007600140407      * imposto come data riferimento il 31 / 12 dell'anno precedente
008500130718     c                   eval      v1cdrf = (3112 * 10000)+(wanno -1)
008501140523      * imposto periodo da elaborare inizio e fine mese precedente
008503140523     c                   eval      Wgiorno = 01
008504140523     C                   move      Wdata         G08dat
008505140523     C                   CALL      'XSRDA8'
008506140523     C                   PARM                    WLBDA8
008507140523     C                   Z-ADD     G08INV        wdate
008509140523      /free
008510140523         wData_ISO = %date(wDate:*iso);
008511140523         wData_EUR = wData_ISO - %months(1);
008512140523         wData_ISO = wData_EUR;
008513140523         V1cdal    = %dec(wData_EUR);
008514140523         wData_ISO = %date(wDate:*iso);
008515140523         wData_EUR = wData_ISO - %days(1);
008516140523         wData_ISO = wData_EUR;
008517140523         V1cal     = %dec(wData_EUR);
008518140523      /end-free
008700130718     c
008800050614     c* emissione video
008900050613     c     emiss         tag
009000140407     C                   exfmt     cfq0d01
009100050614     c                   setoff                                       9028
009200050613     C                   if        *in03
009300050613     C                   goto      fine
009400050613     c                   endif
009500111220      * controlli del video
009600050614     c                   exsr      ctrd01
009700050614     c                   if        *in06 = *off  or *in90 = *on
009800050613     c                   goto      emiss
009900050613     c                   endif
009901140526      * VERIFICO SE ESISTE GIÀ LA LIBRERIA DI SALVATAGGIO
009902140526      * se esiste pulisco i file all'interno
009903140526     c                   eval      wmesel = %subst(%editc(pardal:'X'): 5: 2)
009904140526      * verifico se esiste la libreria di salvataggio se esiste deleto i file presenti
009905140526     c                   eval      libreria = 'EDPCONF_' + wMESEL
009906140528     c                   eval      Testo='Conf. Fatturazione Bolle ' +
009907140528     c                                    %editc(pardal: 'X') + '/' +
009908140528     c                                     %editc(paral: 'X')
009909140526     C                   call      'TICF89C'
009910140526     C                   parm      *blanks       Err_lib
009911140526     C                   parm                    Libreria
009912140526     c                   If        ERR_lib = 'E'
009913140526     c                   movel(p)  libreria      kpjbu
009914140528     c                   eval      %subst(kpjbu:11:50) = testo
009915140526     c                   call      'TICFQ8C'
009916140526     C                   parm                    Kpjba
009917140526     c                   else
010000130130      * se ok i controlli e F6 ci conferma devo creare la libreria di salvataggio
010100140407      * per il confronto Bolle da fatturare
011100130130     c                   movel(p)  libreria      kpjbu
011200130130     c                   eval      %subst(kpjbu:11:50) = testo
011300140526     c                   call      'TICFQ0C'
011400130130     C                   parm                    Kpjba
011401140526     c                   endif
011500130130
011501140523     c                   eval      parmlib = 'EDPCONF_TA'
011502140407     c                   eval      parmlibb= libreria
011503140407     c                   eval      kcoaz = 'CFQ9'
011504140407     c                   movel(p)  param         kpjbu
011505140407     c                   call      'BCH09'
011506140407     c                   parm                    kpjba
011507140407     c                   call      'BCH10'
011508140407     c                   parm                    kpjba
012800050613     c     fine          tag
012900050613     c                   seton                                        lr
013000050614     c**********************************************************************
013100050614     C     ctrd01        begsr
013200050614     c                   setoff                                       4041
013201140407     c                   setoff                                       424344
013300050614     c                   clear                   pardrf
013301140407     c                   clear                   pardtc
013400140407     c                   clear                   pardal
013401140407     c                   clear                   paral
013500140407     c* controllo data di riferimento
013600121217     c* .....obbligatorietà
013700121217     c                   if        v1cdrf = *zeros
013800050614     c                   seton                                        902840
013900050614     c                   movel     msg(1)        v1cmsg
014000050614     c                   goto      endctr
014100050614     c                   endif
014200050614     c* .....correttezza
014300080714     c                   If        v1cdrf > *zeros
014400050614     c                   clear                   wlbda8
014500050614     C                   z-add     v1cdrf        G08dat
014600050614     C                   CALL      'XSRDA8'
014700050614     C                   PARM                    WLBDA8
014800050614IF  2C     G08ERR        IFEQ      '1'
014900050614     C                   seton                                        409028
015000050614     C                   MOVEL     MSG(2)        v1cmsg
015100050614     c                   goto      endctr
015200050614E   2C                   ENDIF
015300050614     C                   Z-ADD     G08DAT        V1cdrf
015400050614     C                   Z-ADD     G08INV        pardrf            8 0
015500080714     c                   endif
015501140407     c* controllo data tariffa corrente
015502140407     c* non .obbligatoria
015508140407     c* .....correttezza
015509140407     c                   If        v1cdtc > *zeros
015510140407     c                   clear                   wlbda8
015511140407     C                   z-add     v1cdtc        G08dat
015512140407     C                   CALL      'XSRDA8'
015513140407     C                   PARM                    WLBDA8
015514140407IF  2C     G08ERR        IFEQ      '1'
015515140407     C                   seton                                        429028
015516140407     C                   MOVEL     MSG(2)        v1cmsg
015517140407     c                   goto      endctr
015518140407E   2C                   ENDIF
015519140407     C                   Z-ADD     G08DAT        V1cdtc
015520140407     C                   Z-ADD     G08INV        pardtc            8 0
015521140407     c                   endif
015600050614     c*
015700140407     c* controllo periodo da elaborare
015800050614     c* .....obbligatorietà
015900140407     c     v1cdal        ifeq      *zeros
016000050614     c                   seton                                        902841
016100050614     c                   movel     msg(3)        v1cmsg
016200050614     c                   goto      endctr
016300050614     c                   endif
016301140407     c* .....obbligatorietà
016302140407     c     v1cal         ifeq      *zeros
016303140407     c                   seton                                        902843
016304140407     c                   movel     msg(3)        v1cmsg
016305140407     c                   goto      endctr
016306140407     c                   endif
016400050614     c* .....correttezza
016401140407     c                   If        v1cdal > *zeros
016402140407     c                   clear                   wlbda8
016403140407     C                   z-add     v1cdal        G08dat
016404140407     C                   CALL      'XSRDA8'
016405140407     C                   PARM                    WLBDA8
016406140407IF  2C     G08ERR        IFEQ      '1'
016407140407     C                   seton                                        419028
016408140407     C                   MOVEL     MSG(2)        v1cmsg
016409140407     c                   goto      endctr
016410140407E   2C                   ENDIF
016411140407     C                   Z-ADD     G08DAT        V1cdal
016412140407     C                   Z-ADD     G08INV        pardal            8 0
016413140407     c                   endif
016414140407     c* .....correttezza
016415140407     c                   If        v1cal > *zeros
016416140407     c                   clear                   wlbda8
016417140407     C                   z-add     v1cal         G08dat
016418140407     C                   CALL      'XSRDA8'
016419140407     C                   PARM                    WLBDA8
016420140407IF  2C     G08ERR        IFEQ      '1'
016421140407     C                   seton                                        439028
016422140407     C                   MOVEL     MSG(2)        v1cmsg
016423140407     c                   goto      endctr
016424140407E   2C                   ENDIF
016425140407     C                   Z-ADD     G08DAT        V1cal
016426140407     C                   Z-ADD     G08INV        paral             8 0
016427140407     c                   endif
016428140407      * verifico sia iniziale che finale maggiore di oggi
016429140407     c                   if        pardal > Dateu
016430140407     C                   seton                                        419028
016431140407     C                   MOVEL     MSG(4)        v1cmsg
016432140407     c                   goto      endctr
016433140407     C                   ENDIF
016434140407     c                   if        paral > Dateu
016435140407     C                   seton                                        439028
016436140407     C                   MOVEL     MSG(4)        v1cmsg
016437140407     c                   goto      endctr
016438140407     C                   ENDIF
016439140407      * verifico la congruenza tra le due date
016440140407     c                   if        pardal > paral
016441140407     C                   seton                                        439028
016442140407     C                   MOVEL     MSG(6)        v1cmsg
016443140407     c                   goto      endctr
016444140407     c                   Endif
019200050614     c     endctr        endsr
019300050614**
019400050614Data obbligatoria
019500050614Data errata
019600140407Periodo da elaborare obbligatorio
019700140407La data non può essere maggiore di oggi
019800130130ATTENZIONE è presente la libreria di salvataggio
019900140407La data dal deve essere minore di data al
