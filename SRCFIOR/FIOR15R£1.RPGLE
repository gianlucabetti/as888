000100080722     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500001115      *       CONFERMA ORDINI RITIRI DA CLIENTE                       *
000600000927      *                                                               *
000700040909      * COMPILARE CON
000800040909      *               OVRDBF FILE(TIORI01L) TOFILE(GAITRAGRPS/TIORI01L)
000900000927      *****************************************************************
001000000927
001100001019     FAZORG01L  IF   E           K DISK
001200001116     FFNVAO01L  UF   E           K DISK    infds(vaods)
001300001116     FFNVAO00F  UF   E             DISK    rename(fnvao000:fnvao0)
001400001115     FFNACR01L  IF   E           K DISK
001500040909     fTiori01l  uf   e           k disk    usropn
001600100218     fFnvaoe1l  uf   e           k disk
001700140528     fTABEL00F  if   e           k disk
001800001026     FFIOR15D   CF   E             WORKSTN SFILE(FIORSF1:NN1)
001900010302
002000010302     D sav_nn1         S                   LIKE(nn1)
002100090402     d wtime           s             14
002200040909     d wlib            s             10
002300040909     d comman          s             80
002400040909     d lenght          s             15  5 inz(80)
002500130619     d wabi            s                   like(UTEaut)
002600130619     d wEsci           s               n   inz(*off)
002700130619     d wErrGrave       s               n   inz(*off)
002800130619     d w003a           s              3
002900150617     d xx              s              4s 0 inz
003000000927
003100000928     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             MSG VIDEO
003200001115     D VOC             S              3    DIM(7) CTDATA PERRCD(1)              VOCI A VIDEO
003300001025     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
003400130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
003500040909     d cmd             s             80    dim(1) ctdata perrcd(1)
003600050915
003700050915     d wtxt            S           2048
003800050915     d TxtInOut        S           2048
003900050915     d ElencoChar      S            256
004000050915     d TipoElenco      S              1
004100050915     d CharSost        S              1
004200050915     d UpperCase       S              1
004300050915     d ChkNull         S              1    Inz('1')
004400050915     d CharNull        S              1
004500150616     d Esito           S              1
004600001116
004700000927     D KPJBA         E DS
004800001024     D FIOR05DS      E DS
004900010301     D TIBS02DS      E DS
005000131108     d FIOR81DS      e ds
005100080722      * - Parametri x Controllo profilo utenti
005200080722     d TIBS34ds      e ds
005300080722      * - Ds di riferimento al file esterno AZUTE00F
005400080722     d AZUTEds       e ds                  extname(AZUTE00F)
005500080722      * - Ds per dati organigramma
005600080722     d DDatiUte      e ds
005700000927
005800080722     D trul06ds      E DS
005900080722     D  LIN                    1     90  0 dim(30)
006000130619
006100130619     d TRUL31DS      e ds
006200130619
006300130619     d dLAT          e ds
006400130619     d dUTE01        e ds
006500070810
006600040714     d dorm01        e ds
006700140528     d dOREGEN       e ds                  inz qualified
006800150616
006900150616     d TBLuni_£7       ds
007000150616     d   L7                    1     87    dim(29)
007100150616     d   vuoto                88     89
007200001023
007300001023      * Parametri per controllo Data
007400001023
007500001023     D WLBDAT          DS                  INZ
007600001023     D  G02DAT                 1      8  0
007700001023     D  G02INV                 9     16  0
007800001023     D  G02ERR                17     17
007900001023     D  G02TGI                18     22  0
008000001116
008100010205     D Parm01          DS
008200010205     D  ppoe                          3  0
008300010205     D  pnor                          7  0
008400010205     D  pnsr                          2  0
008500010205     D  pnrv                          2  0
008600010205     D  psce                          1
008700010205     D  pfgs                          3  0
008800010205     D  ppor                          3  0
008900010205     D  pdtr                          8  0
009000010205     D  pmdb                         10
009100010205     D  pprb                         10
009200010205     D  pdts                          8  0
009300010205     D  prmp                          1
009400010205     D  pbrc                          1
009500010205     D  pref                          2
009600010205     D  pmio                          1
009700010205
009800001116     D vaods           ds
009900001116     D  vaonrr               397    400b 0
010000061207
010100061207     d                 ds
010200061207     d  vidcor1                1      3  0
010300061207     d  vidcor2                4      7  0
010400061207     d  vidcor12               1      7  0
010500061207     d  vidcor3                8     10  0
010600061207     d  vidcor                 1     10  0
010700061207
010800061207     d                 ds
010900061207     d  vaocor12               1      7  0
011000061207     d  vaocor3                8     10  0
011100061207     d  vaocor                 1     10  0
011200061207
011300040909      *----------------
011400040909      * Costanti
011500040909      *----------------
011600040909     d DIGITn          C                   CONST('1234567890')
011700000927
011800000927      *---------------------------------------------------------------*
011900000927      *                                                               *
012000000927      *    UTILIZZO DEGLI INDICATORI                                  *
012100000927      *                                                               *
012200000927      *---------------------------------------------------------------*
012300000927      *                                                               *
012400001026      *    01 - Posizione Cursore su P.O. Gestione      FIOR151       *
012500001026      *    03 - Disabilito F18                          FIOR151       *
012600061207      *    04 - Richiesto un cliente ordinante
012700061207      *    06 - Tutti luoghi
012800150617      *    07 - Utente Internazionale (INT)
012900001025      *    10 - Confermato almeno un Ordine                           *
013000001026      *    18 - Sproteggo P.O. Gestione                 FIOR151       *
013100010301      *    19 - Tipo comunicazione errato
013200061207      *    20 - Cliente ordinante
013300001026      *    28 - Visualizzo messaggio Errore             FIOR151       *
013400010302      *    30 - Comodo                                                *
013500001024      *    31 - Creato Orm da F6                                      *
013600001024      *    32 - F12 da F6                                             *
013700131003      *    33 - Filiale utente attiva agli Orari Servizi
013800001024      *    39 - Comodo                                                *
013900010301      *    90 - riemetto videata
014000000927      *                                                               *
014100000927      *---------------------------------------------------------------*
014200100218
014300100218      //---------------------------------------------------------------
014400100218      //?Definizione key-list.
014500100218      //---------------------------------------------------------------
014600100218       // - File FNVAOE1L
014700100218     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
014800100218     d                                     prefix(k_)
014900100218     d                                     inz
015000140528       // - File TABEL00F
015100140528     d k03tabel      e ds                  extname(TABEL00F:*key)
015200140528     d                                     prefix(k_)
015300140528     d                                     inz
015400100218
015500000927
015600000927      *****************************************************************
015700000927      *                                                               *
015800000927      *    OPERAZIONI INIZIALI                                        *
015900000927      *                                                               *
016000000927      *****************************************************************
016100000927
016200001024
016300001024      *    Inizializzo Campi Video
016400001024
016500000927     C                   EXSR      SR_INIVIDEO
016600000927
016700000927      *****************************************************************
016800000927      *                                                               *
016900000927      *    ROUTINE PRINCIPALE                                         *
017000000927      *                                                               *
017100000927      *****************************************************************
017200001011
017300001019     C                   do        *hival
017400010302
017500010507     C                   if        *in28 = *off and *in90 = *off
017600010302     C                   clear                   vidtco
017700010302     C                   clear                   viddtc
017800061207     c                   clear                   vidcor
017900061207     c                   clear                   vidluo
018000061207     c                   clear                   vidrso
018100010302     C                   endif
018200130619
018300130619      /free
018400130619       //?Se utente non autorizzato alla funzione esco
018500130619         IF  wErrGrave;
018600130619           *in28 = *on;
018700130619           wEsci = *on;
018800130619           VIDmsg = 'Utente non autorizzato alla funzione';
018900130619         ENDIF;
019000130619      /end-free
019100001019
019200001019      *    Emissione Prima Videata
019300001019
019400001026     C                   EXFMT     FIOR151
019500130619
019600130619      /free
019700130619         IF  wEsci;
019800130619           leave;
019900130619         ENDIF;
020000130619      /end-free
020100130619
020200001019     C   KC              leave
020300130619
020400001026     C                   setoff                                       281801
020500061207     C                   setoff                                       192090
020600061207     c                   setoff                                       0406
020700061207
020800061207      * F8=Int.Cod.Ordinante
020900061207     C   kh              exsr      Sr_RicOrdin
021000061207     C   90              iter
021100001019
021200001019      *    Controllo
021300001019
021400001019     C                   EXSR      SR_CONTROL
021500001019
021600001019      *    Se Vi sono Errori o Voglio Cambiare P.O. Riemetto
021700001023      *    .. Riemetto normalmente anche se non ho premuto F6
021800001019
021900001019     C     *in28         ifeq      *on
022000001019     C     *in18         oreq      *on
022100010301     C     *in90         oreq      *on
022200001019     C                   iter
022300001019     C                   endif
022400001023
022500001023      *   GESTIONE DEGLI ORM
022600001023
022700001023     C                   EXSR      SR_GESTIONE
022800001023
022900001019     C                   enddo
023000001019
023100000927     C                   eval      *inlr = *on
023200000927
023300000927      *****************************************************************
023400000927      *                                                               *
023500000927      *    SUBROUTINES                                                *
023600000927      *                                                               *
023700000927      *****************************************************************
023800000927
023900000927      *---------------------------------------------------------------*
024000000927      *    Inizializzazione Campi Video                               *
024100000927      *---------------------------------------------------------------*
024200000927
024300000927     C     SR_INIVIDEO   begsr
024400000927
024500001026      *    Reset Progressivo Nr. Ordini Confermati
024600001025
024700001026     C                   setoff                                       10
024800001025     C                   clear                   vconfe
024900001002
025000001116      *    Inizializzo il P.O. Gestione
025100000927
025200080722     C                   move      dutpou        vidpog
025300001012
025400001116      *    Decodifico il P.O. Gestione
025500000927
025600001116     C     vidpog        chain     azorg01l
025700000927     C                   if        %found(azorg01l)
025800000927     C                   movel     orgdes        viddep
025900000927     C                   else
026000001019     C                   clear                   viddep
026100000927     C                   endif
026200150617
026300150617      /free
026400150617       //?Se Filiale secondo livello o Utente INT F18 non abilitato
026500150617         *in03 = *off;
026600150617         IF  *in07 or DUTlpo = '2' or DUTlpo = *blanks;
026700150617           *in03 = *on;
026800150617         ENDIF;
026900150617      /end-free
027000000927
027100000927      *    Inizializzo i Campi Restanti
027200001012
027300010301     C                   clear                   vidtco
027400010205     C                   move      'SI'          vidvis
027500000927
027600000927     C                   endsr
027700061207      *---------------------------------------------------------------*
027800061207      * F8 - Ricerca cliente ordinante
027900061207      *---------------------------------------------------------------*
028000061207     c     Sr_RicOrdin   BEGSR
028100061207
028200061207     C                   eval      *in90 = *on
028300131108
028400131108     c                   clear                   FIOR81ds
028500131108     c                   IF        VIDcor1 > 0
028600131108     c                   eval      I81fil = VIDcor1
028700131108     c                   ENDIF
028800131108     c                   IF        VIDrso <> *blanks
028900131108     c                   eval      I81rag = VIDrso
029000131108     c                   ENDIF
029100131108     c                   call      'FIOR81R'
029200131108     c                   parm                    kpjba
029300131108     c                   parm                    FIOR81DS
029400131108     c                   IF        O81err <> *blanks
029500131108     c                   eval      VIDmsg = O81msg
029600131108     c                   seton                                        20  28
029700131108     c                   leavesr
029800131108     c                   ENDIF
029900131108     c                   IF        O81ret = '1'
030000131108     c                   leavesr
030100131108     c                   ENDIF
030200131108     c                   movel     O81cro        VIDcor
030300070809      *
030400070809     C                   exsr      sr_ContrCOR
030500061207
030600061207     C                   endsr
030700001019
030800001019      *---------------------------------------------------------------*
030900001019      *    Controllo Prima Videata                                    *
031000001019      *---------------------------------------------------------------*
031100001019
031200001019     C     SR_CONTROL    begsr
031300001019
031400001025     C                   z-add     0             vconfn
031500001025
031600001023     C     *inKF         ifeq      *on
031700001023     C                   move      '1'           wkf
031800001023     C                   else
031900001023     C                   clear                   wkf               1
032000001023     C                   endif
032100001023
032200001116      *    F18=Cambio P.O. Gestione
032300001019
032400001019     C     *inks         ifeq      *on
032500001019     C                   seton                                        18
032600001019     C                   goto      endctr
032700001019     C                   endif
032800001023
032900001116      *    Controllo P.O. Gestione
033000001023
033100001023     C                   clear                   viddep
033200130619     c                   move      VIDpog        w003a
033300130619     c     w003a         lookup    SKpog                                  39
033400001023     C     *in39         ifeq      *off
033500001023     C                   eval      *in01 = *on
033600001023     C                   eval      *in28 = *on
033700001023     C                   eval      *in18 = *on
033800001023     C                   movel     msg(1)        vidmsg
033900001023     C                   goto      endctr
034000001023     C                   else
034100001116     C     vidpog        chain     azorg01l
034200001023     C                   if        %found(azorg01l)
034300001023     C                   movel     orgdes        viddep
034400001023     C                   endif
034500001025      *        Carico £6
034600080722     C                   CLEAR                   trul06ds
034700001025     C                   MOVE      '£6'          D06COD
034800001116     C                   MOVEL     vidpog        D06KEY
034900080722     C                   MOVEL     trul06ds      KPJBU
035000001025     C                   CALL      'TRUL06R'
035100001025     C                   PARM                    KPJBA
035200080722     C                   MOVEL     KPJBU         trul06ds
035300001025     C                   MOVEA     LIN           L6
035400001023     C                   endif
035500150616
035600150616      /free
035700150616       //?Se utente INT devo caricare la £7 nella £6
035800150617         IF  *in07;
035900150617           xx = 1;
036000150617           FOR xx by 1 to %elem(L7);
036100150617             IF  L7(xx) <> *zeros and L7(xx) <> *blanks;
036200150617               L6(xx) = %dec(L7(xx):3:0);
036300150617             ENDIF;
036400150617           ENDFOR;
036500150616         ENDIF;
036600150616      /end-free
036700010302
036800010302     C                   clear                   viddtc
036900010301      * Ricerca Tipo Comunicazione con "?"
037000010301     C     '?'           scan      vidtco                                 30
037100010301     C     *in30         ifeq      *on
037200010301     C                   eval      *in90 = *on
037300010301     C                   eval      *in19 = *on
037400010301     C                   clear                   TIBS02DS
037500010301     C                   movel     'R'           t02mod
037600010301     C                   movel     knsif         t02sif
037700010301     C                   movel     'TCO'         t02cod
037800010301     C                   call      'TIBS02R'
037900010301     C                   parm                    KPJBA
038000010301     C                   parm                    TIBS02DS
038100010301     C                   movel     T02ke1        vidtco
038200010301     C                   movel     T02uni        viddtc
038300010301     C                   goto      endctr
038400010301     C                   endif
038500010301      * Tipo comunicazione
038600010301     C                   if        vidtco <> *blanks
038700010301     C                   clear                   TIBS02DS
038800010301     C                   movel     'C'           T02mod
038900010301     C                   movel     knsif         t02sif
039000010301     C                   movel     'TCO'         t02cod
039100010301     C                   movel(p)  vidtco        T02ke1
039200010301     C                   call      'TIBS02R'
039300010301     C                   parm                    KPJBA
039400010301     C                   parm                    TIBS02DS
039500010301     C                   if        t02err <> *blanks
039600010301     C                   movel     msg(2)        vidmsg
039700010301     C                   seton                                        19  28
039800010301     C                   goto      endctr
039900010301     C                   endif
040000010301     C                   movel     t02ke1        vidtco
040100010301     C                   movel     t02uni        viddtc
040200010301     C                   endif
040300061207
040400061207      * cliente ordinante
040500061207     C                   if        vidcor <> *zeros
040600061207     c                   eval      *in04 = *on
040700061207     C                   exsr      Sr_Contrcor
040800061207     C   28              leavesr
040900061207     C                   endif
041000061207      * tutti luoghi
041100061207     c                   if        vidluo <> *blanks
041200061207     c                   if        vidcor = *zeros
041300061207     c                   eval      vidmsg = msg(4)
041400061207     c                   eval      *in20 = *on
041500061207     c                   eval      *in28 = *on
041600061207     c   28              leavesr
041700061207     c                   endif
041800061207     c                   eval      *in06 = *on
041900061207     c                   endif
042000001023
042100001019     C     ENDCTR        endsr
042200061207
042300061207      *---------------------------------------------------------------*
042400061207      * Controllo  cliente ordinante
042500061207      *---------------------------------------------------------------*
042600061207     C     Sr_Contrcor   BEGSR
042700061207
042800061207     C     vidcor        chain     fnacr01l
042900061207     C                   if        %found(fnacr01l)
043000061207     C                   if        ACRatb <> *blanks
043100061207     C                   movel     msg(3)        vidmsg
043200061207     C                   seton                                        20  28
043300061207     C                   endif
043400061207     C  n28              movel     ACRrsr        vidrso
043500061207     C                   else
043600061207     C                   movel     msg(3)        vidmsg
043700061207     C                   seton                                        20  28
043800061207     C                   endif
043900061207
044000061207     C                   endsr
044100001023
044200001023      *---------------------------------------------------------------*
044300001023      *    Lettura / Gestione degli ORM                               *
044400001023      *---------------------------------------------------------------*
044500001023
044600001023     C     SR_GESTIONE   begsr
044700001023
044800001026      *    Pulisco Subfile e mi posiziono per Lettura Ordini C.Center
044900001024
045000001024     C                   do        *hival
045100001023     C                   z-add     0             nn1               4 0
045200001023     C                   eval      *in85 = *on
045300001023     C                   write     fiorrc1
045400001023     C                   eval      *in85 = *off
045500001023     C                   z-add     1             recsf
045600001025
045700001025      *    Carico gli ordini della £6
045800001025
045900001025     C     1             do        30            zz                3 0
046000001025
046100001025     C     L6(zz)        ifeq      0
046200001025     C                   iter
046300001025     C                   endif
046400001025
046500001116     C                   z-add     L6(zz)        vs1poe
046600001025
046700001116     C     vs1poe        setll     fnvao01l
046800001023
046900001026      *    Leggo Ordini Call Center
047000001023
047100001023     C                   do        *hival
047200001024     C     wkf           ifne      '1'
047300001116     C     vs1poe        reade(n)  fnvao01l
047400001024     C                   else
047500040401     C     vs1poe        reade(e)  fnvao01l
047600040401      * Allocato leggo il record e poi torno subito a leggere quello sucessivo
047700040401     c                   If        %Error
047800040401     c                   Read(n)   Fnvao01l
047900040401     c                   Iter
048000040401     c                   EndIf
048100001024     C                   endif
048200040401      * Fine File
048300001115     C                   if        %eof(fnvao01l)
048400001023     C                   leave
048500001023     C                   endif
048600001023
048700001026     C     nn1           ifge      8000
048800001024     C                   leave
048900001024     C                   endif
049000010302      * Annullati
049100010302     C                   if        vaoatb <> *blanks
049200010302     C                   iter
049300010302     C                   endif
049400010301      * Tipo comunicazione
049500010301     C                   if        vidtco <> *blanks and vaotco <> vidtco
049600010301     C                   iter
049700010301     C                   endif
049800061207      * cliente ordinante
049900061207     c                   if        vidcor <> *zeros
050000061207     c                             and vidluo = *blanks and vidcor <> vaocor
050100061207     c                   iter
050200061207     c                   endif
050300061207     c                   if        vidcor <> *zeros
050400061207     c                             and vidluo <> *blanks and
050500061207     c                                 vidcor12 <> vaocor12
050600061207     c                   iter
050700061207     c                   endif
050800001024
050900001026      *    SCRIVO SUBFILE
051000001024      *    .. se ho premuto F6 tento prima la conferma automatica
051100001024      *    .. richiamando il pgm di creazione ORM
051200001024
051300001024     C                   setoff                                       31  32
051400001024     C     wkf           ifeq      '1'
051500001024     C                   EXSR      SR_RICHIAMA
051600001024     C     §rmerr        ifne      ' '
051700001115     C                   unlock    fnvao01l
051800001024     C     §rmerr        ifeq      '3'
051900001024     C                   seton                                        32
052000001024     C                   endif
052100001024     C                   else
052200001116     C                   seton                                        31
052300100218     c                   exsr      DelVaoE
052400040909     c                   ExSr      xTIORI
052500001116     C                   delete    fnvao000
052600001025     C                   add       1             vconfe
052700001025     C                   add       1             vconfn
052800001025     C                   seton                                        10
052900001024     C                   endif
053000001024     C                   endif
053100001024
053200001024     C     *in31         ifeq      *off
053300001024     C     wkf           ifne      '1'
053400001026     C                   clear                   vs1opz
053500001026     C                   eval      *in05 = *off
053600001024     C                   else
053700001024     C     *in32         ifeq      *on
053800001026     C                   clear                   vs1opz
053900001026     C                   eval      *in05 = *off
054000001024     C                   else
054100001025     C                   move      'E'           vs1opz
054200001024     C                   eval      *in05 = *on
054300001024     C                   endif
054400001024     C                   endif
054500011029
054600001129     C                   movel     vaotco        vs1tco
054700140520
054800140520      * Pulisco i dati del mittente
054900140520     C                   clear                   vs1rsr
055000140520     C                   clear                   vs1inr
055100140520     C                   clear                   vs1car
055200140520     C                   clear                   vs1lor
055300140520     C                   clear                   vs1prr
055400140520     C                   clear                   vs1nar
055500011029
055600011029      * Passo i dati relativi al mittente
055700011029     C                   select
055800011029     C                   when      vaorsr <> *blanks
055900110121     c                   Eval      wtxt = vaorsr
056000110121     c                   ExSr      Sr_Chk
056100110121     c                   Movel     wtxt          vs1rsr
056200110121     c                   Eval      wtxt = vaoinr
056300110121     c                   ExSr      Sr_Chk
056400110121     c                   Movel     wtxt          vs1inr
056500110121     c                   Eval      wtxt = vaocar
056600110121     c                   ExSr      Sr_Chk
056700110121     c                   Movel     wtxt          vs1car
056800110121     c                   Eval      wtxt = vaolor
056900110121     c                   ExSr      Sr_Chk
057000110121     c                   Movel     wtxt          vs1lor
057100110121     c                   Eval      wtxt = vaoprr
057200110121     c                   ExSr      Sr_Chk
057300110121     c                   Movel     wtxt          vs1prr
057400110121     c                   Eval      wtxt = vaonar
057500110121     c                   ExSr      Sr_Chk
057600110121     c                   Movel     wtxt          vs1nar
057700011029
057800011029     C                   when      vaocra <> *zeros
057900011029     C     vaocra        chain     fnacr01l
058000011029     C                   if        %found(fnacr01l)
058100011029     C                   movel     ACRrsr        vs1rsr
058200011029     C                   movel     ACRinr        vs1inr
058300011029     C                   movel     ACRcar        vs1car
058400011029     C                   movel     ACRlor        vs1lor
058500011029     C                   movel     ACRprr        vs1prr
058600011029     C                   movel     ACRnar        vs1nar
058700011029     C                   else
058800011029     C                   clear                   vs1rsr
058900011029     C                   clear                   vs1inr
059000011029     C                   clear                   vs1car
059100011029     C                   clear                   vs1lor
059200011029     C                   clear                   vs1prr
059300011029     C                   clear                   vs1nar
059400011029     C                   endif
059500011029     C                   endsl
059600011112
059700011112      * Se prepagato metto flag nel subfile
059800011112     C                   if        vaotor = 'P'
059900011112     C                   eval      vs1pre = 'S'
060000011112     C                   else
060100011112     C                   clear                   vs1pre
060200011112     C                   endif
060300011112
060400110121     c                   Eval      wtxt = vaorfa
060500110121     c                   ExSr      Sr_Chk
060600110121     c                   Movel     wtxt          vs1rfa
060700001116     C                   movel     vaopor        vs1por
060800001116     C                   z-add     vaonrr        vs1nrr
060900001116      * Decodifica voce e imposta qtà relativa
061000001115     C                   select
061100001115     C                   when      vaoncl <> *zeros
061200001115     C                   movel     voc(1)        vs1voc
061300001115     C                   z-add     vaoncl        vs1qta
061400001115
061500001115     C                   when      vaopkg <> *zeros
061600001115     C                   movel     voc(2)        vs1voc
061700001115     C                   z-add     vaopkg        vs1qta
061800001115
061900001115     C                   when      vaovlm <> *zeros
062000001115     C                   movel     voc(3)        vs1voc
062100001115     C                   z-add     vaovlm        vs1qta
062200001115
062300001115     C                   when      vaobnc <> *zeros
062400001115     C                   movel     voc(4)        vs1voc
062500001115     C                   z-add     vaobnc        vs1qta
062600001115
062700001115     C                   when      vaoblc <> *zeros
062800001115     C                   movel     voc(5)        vs1voc
062900001115     C                   z-add     vaoblc        vs1qta
063000001115
063100001115     C                   when      vaoatt <> *zeros
063200001115     C                   movel     voc(6)        vs1voc
063300001115     C                   z-add     vaoatt        vs1qta
063400001115
063500001115     C                   when      vaomtc <> *zeros
063600001115     C                   movel     voc(7)        vs1voc
063700001115     C                   z-add     vaomtc        vs1qta
063800001115
063900001115     C                   other
064000001115     C                   z-add     *zeros        vs1qta
064100001115     C                   movel     *blanks       vs1voc
064200001115
064300001115     C                   endsl
064400001024
064500001024     C                   add       1             nn1
064600001024     C                   write     fiorsf1
064700001024     C                   endif
064800001024
064900001023     C                   enddo
065000001025     C                   enddo
065100010302
065200010302     C                   clear                   wkq
065300001023
065400001024      *    EMISSIONE RECORD DI CONTROLLO
065500001024
065600001025     C     vconfn        ifeq      0
065700001025     C                   eval      *in12 = *off
065800001025     C                   else
065900001025     C                   eval      *in12 = *on
066000001025     C                   endif
066100001025
066200010316      * Nessun record
066300001024     C     nn1           ifeq      0
066400010316     C                   clear                   wkf
066500010316     C                   do        *hival
066600001026     C                   exfmt     fior153
066700010316     C                   if        *inkl = *on
066800010316     C                   goto      endgest
066900010316     C                   endif
067000010316     C                   enddo
067100010316     C                   endif
067200010302
067300010302     C                   eval      sav_nn1 = nn1
067400001026     C                   write     fior152
067500001024     C                   exfmt     fiorrc1
067600001024     C     *inkf         ifeq      *on
067700001024     C                   move      '1'           wkf
067800001024     C                   else
067900001024     C                   clear                   wkf
068000001024     C                   endif
068100001024
068200001025     C                   z-add     0             vconfn
068300010302     C                   eval      nn1 = *zeros
068400001025
068500001024      *    LETTURA RECORDS SUBFILE
068600001024
068700001024     C     *inkl         ifeq      *off
068800001024
068900001024     C                   do        *hival
069000010302     C                   eval      nn1 = nn1 +1
069100001024
069200001024     C     *inkf         ifeq      *on
069300001024     C                   leave
069400001024     C                   endif
069500010302     C     *inkq         ifeq      *on
069600010302     C                   move      '1'           wkq               1
069700010302     C                   endif
069800001024
069900010302     C     nn1           chain     fiorsf1                            30
070000010302     C                   if        *in30 = *on
070100010302     C                   leave
070200001024     C                   endif
070300040401
070400040401     c                   Eval      *In28 = *Off
070500001024
070600010302     C                   if        *inkq = *off
070700001024     C     vs1opz        ifeq      '1'
070800001024     C     vs1opz        oreq      '2'
070900040401     C     vs1nrr        chain(e)  fnvao00f
071000040401      * Allocato leggo il record sucessivo
071100040401     c                   If        %Error
071200040401     c                   Iter
071300040401     c                   EndIf
071400040401
071500001116     C                   if        %found (fnvao00f)
071600001024
071700001026      *    Richiamo Programma di Creazione ORM
071800001024
071900001024     C                   EXSR      SR_RICHIAMA
072000001024
072100001024     C     §rmerr        ifne      ' '
072200001116     C                   unlock    fnvao00f
072300001024     C                   else
072400100218     c                   exsr      DelVaoE
072500040909     c                   ExSr      xTIORI
072600001116     C                   delete    fnvao0
072700001025     C                   add       1             vconfe
072800001025     C                   add       1             vconfn
072900001025     C                   seton                                        10
073000001024     C                   endif
073100001024
073200001024     C                   endif
073300001024     C                   endif
073400010302     C                   endif
073500001024
073600010302     C     vs1opz        ifeq      '4'
073700011029     C                   eval      vs1atb = 'A'
073800011029     C                   else
073900011029     C                   eval      vs1atb = ' '
074000010302     C                   endif
074100010302
074200010302     C                   update    fiorsf1
074300001024     C                   enddo
074400001024
074500001024     C                   else
074600001024     C                   leave
074700001024     C                   endif
074800010302
074900010302      * Se f16 e sono stati annullati dei record annullare vao
075000001024
075100010302     C                   if        wkq = '1'
075200010302     C                   if        sav_nn1 <> 0
075300010302     C                   eval      nn1 = *zeros
075400010302     C                   do        *hival
075500010302     C                   eval      nn1 = nn1 +1
075600010302     C     nn1           chain     fiorsf1                            30
075700010302     C                   if        *in30 = *on
075800010302     C                   leave
075900010302     C                   endif
076000010302     C                   if        vs1atb = *blanks
076100010302     C                   iter
076200010302     C                   endif
076300040401     C     vs1nrr        chain(e)  fnvao00f
076400040401      * Allocato leggo il record sucessivo
076500040401     c                   If        %Error
076600040401     c                   Iter
076700040401     c                   EndIf
076800080722     c                   eval      dorm01 = vaoflo
076900010302     C                   if        %found (fnvao00f)
077000011029     C                   delete    fnvao0
077100010302     C                   endif
077200100218      * annullo estensione
077300100218     c                   exsr      DelVaoE
077400090402      * imposto la data e ora di annullamento
077500090402     c                   eval      wtime = %char(%timestamp:*iso0)
077600080722      * devo annullare anche TIORI
077700080722     c                   exsr      xtiori
077800010302     C                   enddo
077900010302     C                   endif
078000010302     C                   endif
078100010302
078200001024     C                   enddo
078300010316
078400010316     C     endgest       endsr
078500001024
078600001024      *---------------------------------------------------------------*
078700001024      *    Richiama PGM Creazione ORM                                 *
078800001024      *---------------------------------------------------------------*
078900001024
079000001024     C     SR_RICHIAMA   begsr
079100001024
079200001024     C                   clear                   FIOR05DS
079300001024     C     wkf           ifne      '1'
079400001024     C                   move      vs1opz        §rmtla
079500001024     C                   else
079600001024     C     vidvis        ifeq      'NO'
079700001024     C                   move      '0'           §rmtla
079800001024     C                   else
079900001024     C                   move      '2'           §rmtla
080000001024     C                   endif
080100001024     C                   endif
080200001026     C                   move      'C'           §rmfpr
080300001115     C                   move      vaopoe        §rmpoe
080400010601     C                   z-add     vaonsr        §rmnsr
080500010601     C                   z-add     vaonor        §rmnor
080600010601     C                   z-add     vaonrv        §rmnrv
080700010228     C                   move      vaotor        §rmtor
080800001129     C                   move      vaotco        §rmtco
080900001115     C                   movel     vaodao        §rmdao
081000001115     C                   movel     vaooao        §rmoao
081100050915
081200010601     C                   movel     vaocor        §rmcor
081300050915     c                   Eval      wtxt = vaorso
081400050915     c                   ExSr      Sr_Chk
081500050915     c                   Movel     wtxt          §rmrso
081600050915     c                   Eval      wtxt = vaoino
081700050915     c                   ExSr      Sr_Chk
081800050915     c                   Movel     wtxt          §rmino
081900050915     c                   Eval      wtxt = vaocao
082000050915     c                   ExSr      Sr_Chk
082100050915     c                   Movel     wtxt          §rmcao
082200050915     c                   Eval      wtxt = vaoloo
082300050915     c                   ExSr      Sr_Chk
082400050915     c                   Movel     wtxt          §rmloo
082500010601     C                   movel     vaopro        §rmpro
082600010601     C                   movel     vaonao        §rmnao
082700050915
082800001115     C                   movel     vaocra        §rmcra
082900050915     c                   Eval      wtxt = vaorsr
083000050915     c                   ExSr      Sr_Chk
083100050915     c                   Movel     wtxt          §rmrsr
083200050915     c                   Eval      wtxt = vaoinr
083300050915     c                   ExSr      Sr_Chk
083400050915     c                   Movel     wtxt          §rminr
083500050915     c                   Eval      wtxt = vaocar
083600050915     c                   ExSr      Sr_Chk
083700050915     c                   Movel     wtxt          §rmcar
083800050915     c                   Eval      wtxt = vaolor
083900050915     c                   ExSr      Sr_Chk
084000050915     c                   Movel     wtxt          §rmlor
084100001115     C                   movel     vaoprr        §rmprr
084200001115     C                   movel     vaonar        §rmnar
084300140110     c                   IF        vaocra <> *zeros
084400140110     c     vaocra        chain     fnacr01l
084500140110     c                   if        %found(fnacr01l)
084600140110     c                   movel     ACRrsr        §rmrsr
084700140110     c                   movel     ACRinr        §rminr
084800140110     c                   movel     ACRcar        §rmcar
084900140110     c                   movel     ACRlor        §rmlor
085000140110     c                   movel     ACRprr        §rmprr
085100140110     c                   movel     ACRnar        §Rmnar
085200140110     c                   ENDIF
085300140110     c                   ENDIF
085400070905
085500070905     c                   eval      wtxt = vaorer
085600070905     c                   exsr      sr_chk
085700070905     c                   movel     wtxt          §rmrer
085800001115     C                   movel     vaoter        §rmter
085900050915
086000010601     C                   movel     vaocrc        §rmcrc
086100050915     c                   Eval      wtxt = vaorsc
086200050915     c                   ExSr      Sr_Chk
086300050915     c                   Movel     wtxt          §rmrsc
086400050915     c                   Eval      wtxt = vaoinc
086500050915     c                   ExSr      Sr_Chk
086600050915     c                   Movel     wtxt          §rminc
086700050915     c                   Eval      wtxt = vaocac
086800050915     c                   ExSr      Sr_Chk
086900050915     c                   Movel     wtxt          §rmcac
087000050915     c                   Eval      wtxt = vaoloc
087100050915     c                   ExSr      Sr_Chk
087200050915     c                   Movel     wtxt          §rmloc
087300010601     C                   movel     vaoprc        §rmprc
087400010601     C                   movel     vaonac        §rmnac
087500050915
087600001115     C                   movel     vaoffd        §rmffd
087700131212     c                   IF        VAOffd = 'N'
087800131212     c                   clear                   §RMffd
087900131212     c                   ENDIF
088000010601     C                   movel     vaodar        §rmdar
088100010601     C                   movel     vaoorr        §rmorr
088200010601     C                   movel     vaormp        §rmrmp
088300050915     c                   Eval      wtxt = vaonam
088400050915     c                   ExSr      Sr_Chk
088500050915     c                   Movel     wtxt          §rmnam
088600010601     C                   movel     vaoncl        §rmncl
088700010601     C                   movel     vaopkg        §rmpkg
088800010601     C                   movel     vaovlm        §rmvlm
088900010601     C                   movel     vaobnc        §rmbnc
089000010601     C                   movel     vaoblc        §rmblc
089100010601     C                   movel     vaoatt        §rmatt
089200010601     C                   movel     vaomtc        §rmmtc
089300010601     C                   movel     vaopag        §rmpag
089400010601     C                   movel     vaoksc        §rmksc
089500010601     C                   movel     vaoctr        §rmctr
089600010601     C                   movel     vaopor        §rmpor
089700010601     C                   movel     vaozor        §rmzor
089800010601     C                   movel     vaopoc        §rmpoc
089900070905
090000070905     c                   eval      wtxt = vaono1
090100070905     c                   exsr      sr_chk
090200070905     c                   movel     wtxt          §rmno1
090300070905     c                   eval      wtxt = vaono2
090400070905     c                   exsr      sr_chk
090500070905     c                   movel     wtxt          §rmno2
090600001115     C                   movel     vaoddt        §rmddt
090700050915     c                   Eval      wtxt = vaorfa
090800050915     c                   ExSr      Sr_Chk
090900050915     c                   Movel     wtxt          §rmrfa
091000010601     C                   movel     vaosto        §rmsto
091100010118     C                   movel     vaospi        §rmspi
091200010507     C                   movel     vaoflo        §rmflo
091300040714      * controllo se i dati immessi in §rmflo sono giusti
091400040714     c                   Eval      dorm01 = §rmflo
091500150714      /free
091600150714       //?Alla posizione 11 c'è una 'X' la elimino
091700150714         IF  §ORMauto = 'X';
091800150714           clear §ORMauto;
091900150714         ENDIF;
092000150714      /end-free
092100040714     c                   If        §ormcb <> 'S'
092200040714     c                   Clear                   §ormcb
092300040714     c                   EndIf
092400040714     c                   If        §ormfr <> 'S'
092500040714     c                   Clear                   §ormfr
092600040714     c                   EndIf
092700040714     c                   If        §ormks <> 'S'
092800040714     c                   Clear                   §ormks
092900040714     c                   EndIf
093000040714     c                   If        §ormfd <> 'S'
093100040714     c                   Clear                   §ormfd
093200040714     c                   EndIf
093300040714     c                   If        §orcom <> 'S'
093400040714     c                   Clear                   §orcom
093500040714     c                   EndIf
093600040714     c                   If        §orcomc <> 'S' and §orcomc <> 'N'
093700040714     c                   Clear                   §orcomc
093800040714     c                   EndIf
093900050202     c                   If        §orsrm <> 'S'
094000050202     c                   Clear                   §orsrm
094100050202     c                   EndIf
094200040714     c                   Eval      §rmflo = dorm01
094300010412
094400010412      * Se non inserita prendo l'ora dall'anagrafico
094500010412     C                   if        §rmorr = *zeros
094600010412     C     §rmcra        chain     fnacr01l
094700010412     C                   if        %found(fnacr01l)
094800010412     C                   movel     acrorr        §rmorr
094900010412     C                   endif
095000010412     C                   endif
095100140528      /free
095200140528       //?Verifico se esiste il rcd 'G' su VAOE
095300140528       //?se esite controllo se OK il tipo servizio
095400140528       //?se NON è OK non devo memorizzarlo
095500140528       exsr VaoRcdG;
095600140528      /end-free
095700010205
095800150306     c                   clear                   kpjbu
095900001024     C                   call      'FIOR05R'
096000001024     C                   parm                    KPJBA
096100001024     C                   parm                    FIOR05DS
096200001024
096300001024     C                   endsr
096400050915
096500050915      *---------------------------------------------------------
096600050915      * Controllo i caratteri dei campi alfanumerici
096700050915      *---------------------------------------------------------
096800050915     c     Sr_Chk        BegSr
096900050915
097000050915     c                   Eval      TxtInOut = wtxt
097100070905     c                   Eval      Uppercase = '1'
097200050915     c                   Call      'XCHKCHAR'
097300050915     c                   Parm                    TxtInOut
097400050915     c                   Parm                    ElencoChar
097500050915     c                   Parm                    TipoElenco
097600050915     c                   Parm                    CharSost
097700050915     c                   Parm                    UpperCase
097800050915     c                   Parm                    ChkNull
097900050915     c                   Parm                    CharNull
098000050915     c                   Parm                    Esito
098100050915     c                   If        Esito = '1'
098200050915     c                   Move      TxtInOut      wtxt
098300050915     c                   EndIf
098400050915
098500050915     c                   EndSr
098600100218
098700100218      /free
098800140528       //--------------------------------------------------------------------
098900140528       //?Controllo Tipo Servizio.
099000140528       //--------------------------------------------------------------------
099100140528       BEGSR VaoRcdG;
099200140528
099300140528         clear dOREGEN;
099400140528
099500140528       //?Imposto la chiave per il FNVAOE rcd 'G '
099600140528         k_vaoecor = vaocor;
099700140528         k_vaoepoe = vaopoe;
099800140528         k_vaoensr = vaonsr;
099900140528         k_vaoenor = vaonor;
100000140528         k_vaoenrv = vaonrv;
100100140528         k_vaoetrc = 'G ';
100200140528         chain  %kds(k06fnvaoe) FNVAOE1L;
100300140528         IF  not %found(FNVAOE1L);
100400140528           leavesr;
100500140528         ENDIF;
100600140528         dOREGEN = VAOEdati;
100700140528         IF  doregen.§OREtsp <> *blanks;
100800140528       //?Imposto la chiave per TABELLA '5E'
100900140528           k_TBLkut = 1;
101000140528           k_TBLcod = '5E';
101100140528           k_TBLkey = doregen.§OREtsp;
101200140528           chain %kds(K03tabel) TABEL00F;
101300140528         //?Non esiste o annullato pulisco il campo
101400140528           IF  not %found(TABEL00F) or TBLflg <> *blanks;
101500140528             clear doregen.§OREtsp;
101600140528           ENDIF;
101700140528           VAOEdati = dOREGEN;
101800140528         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
101900140528           IF  VAOEdati = *blanks;
102000140528             delete FNVAOE00;
102100140528           ELSE;
102200140528             update FNVAOE00;
102300140528           ENDIF;
102400140528         ELSE;
102500140528           unlock FNVAOE1L;
102600140528         ENDIF;
102700140528
102800140528       ENDSR;
102900140528
103000100218       //--------------------------------------------------------------------
103100100218       //?Delete l'estensione del VAO.
103200100218       //--------------------------------------------------------------------
103300100218       BEGSR DelVaoE;
103400100218
103500100218       //?Imposto la chiave per il FNVAOE
103600100218         k_vaoecor = vaocor;
103700100218         k_vaoepoe = vaopoe;
103800100218         k_vaoensr = vaonsr;
103900100218         k_vaoenor = vaonor;
104000100218         k_vaoenrv = vaonrv;
104100100218       //?Cancello i rcd
104200100218         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
104300100218         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104400100318         DOW not %eof(FNVAOE1L);
104500100218           delete fnvaoe00;
104600100218           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
104700100218         ENDDO;
104800100218
104900100218       ENDSR;
105000100218      /end-free
105100040909
105200040909      *--------------------------------------------------------------------------------------------*
105300040909      * xTIORI - aggiornamento file TIORI01L
105400040909      *--------------------------------------------------------------------------------------------*
105500040909
105600040909     c     xTIORI        BEGSR
105700040909
105800040909      * reperisce il progressivo
105900040909      * e ne controlla la numericità
106000040909     c     DigitN        check     §ORMpg                                 97
106100040909      *
106200040909      * se effettivamente numerico ...
106300040909if  1c                   if        not *in97
106400040909      *
106500040909      *   aggiorna il file di ordini ritiro merce
106600040909     c                   move      §ORMpg        ORIprg
106700040909     c     ORIprg        chain     tiori01l                           97
106800080722      * aggiorno con numero ORM
106900080722if  2c                   if        not *in97
107000080722     c                   if        wkq <> '1'
107100040909     c                   eval      ORIpoe = §RMpoeo
107200040909     c                   eval      ORInsr = §RMnsro
107300040909     c                   eval      ORInor = §RMnoro
107400040909     c                   eval      ORInrv = §RMnrvo
107500080722     c                   else
107600080722     c                   eval      vaoatb = 'A'
107700090402      * utente e data ora annullamento
107800090402     c
107900090402     c                   eval      oriflo = knmus +
108000090402     c                                      %subst(wtime:3:6) +
108100090402     c                                      %subst(wtime:9:4)
108200080722     c                   endif
108300040909      *
108400040909     c                   update    tiori000
108500040909e   2c                   endif
108600040909      *
108700040909e   1c                   endif
108800040909
108900040909     c                   ENDSR
109000130619
109100130619      *------------------------------------------------------------------------*
109200130619      * ROUTINE INIZIALE
109300130619      *------------------------------------------------------------------------*
109400130619     c     *inzsr        begsr
109500130619
109600130619     c     *entry        plist
109700130619     c                   parm                    kpjba
109800130619
109900130619      *    Recupero Ragione Sociale Ditta da Emettere a Video
110000130619      *    e Punti Operativi Gestiti
110100130619     c     *dtaara       define    §azute        azuteds
110200130619     c     *dtaara       define    §datiute      ddatiute
110300130619      *
110400130619     c                   in(E)     *dtaara
110500130619     c                   IF        %ERROR or RSUT = *blanks
110600130619     c                   clear                   Tibs34Ds
110700130619     c                   call      'TIBS34R'
110800130619     c                   parm                    Tibs34Ds
110900130619     c                   in        *dtaara
111000130619     c                   ENDIF
111100130619
111200130619
111300130619      * controllo abilitazione utente
111400130619     c                   clear                   wabi
111500130619     c                   clear                   dLAT
111600130619     c                   eval      dUTE01 = UTEfaf
111700130619
111800130619      * Verifica errori e autorità profilo
111900130619     c                   SELECT
112000130619      * se ho errori nei dati utente esco dal pgm
112100130619     c                   WHEN      DUTerr = 'E'
112200130619     c                   eval      wErrGrave = *on
112300130619     c                   leavesr
112400130619      * carica abilitazioni del profilo
112500130619     c                   WHEN      §UTEorm <> *blanks
112600130619     c                   eval      wabi = §UTEorm
112700130619     c                   WHEN      UTEaut <> *blanks
112800130619     c                   eval      wabi = UTEaut
112900130619      * se non c'è l'abilitazione
113000130619      * --> se 1° livello, abilitazioni al terminal
113100130619      *     se 2° livello, abilitazioni al punto operativo
113200130619      *     se sede no abilitazioni
113300130619     c                   OTHER
113400130619     c                   IF        DUTlpo = '1'
113500130619     c                   eval      wabi   = 'TP'
113600130619     c                   ENDIF
113700130619     c                   IF        DUTlpo = '2'
113800130619     c                   eval      wabi   = 'PO'
113900130619     c                   ENDIF
114000130619     c                   ENDSL
114100130619
114200130619      * controllo se ok l'abilitazione dell'utente
114300130619     c                   clear                   TIBS02DS
114400130619     c                   eval      T02mod = 'C'
114500130619     c                   eval      T02sif = knsif
114600130619     c                   eval      T02cod = 'LAT'
114700130619     c                   eval      T02ke1 = wabi
114800130619     c                   Call      'TIBS02R'
114900130619     c                   Parm                    KPJBA
115000130619     c                   Parm                    TIBS02DS
115100130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
115200130619     c                   eval      wErrGrave = *on
115300130619     c                   leavesr
115400130619     c                   ENDIF
115500130619     c                   eval      dLAT = T02uni
115600130619
115700130619      * Reperimento delle filiali gestibili dall'utente
115800130619     c                   clear                   TRUL31DS
115900130619     c                   eval      I31abi = wabi
116000130619     c                   eval      I31cdi = DUTdis
116100130619     c                   eval      I31car = DUTare
116200130619     c                   eval      I31cpo = DUTpou
116300130619     c                   call      'TRUL31R'
116400130619     c                   parm                    KPJBA
116500130619     c                   parm                    TRUL31DS
116600130619     c                   IF        O31pog > *zeros
116700130619     c                   movea     O31pog        SKpog
116800130619     c                   ELSE
116900130619     c                   eval      wErrGrave = *on
117000130619     c                   leavesr
117100130619     c                   ENDIF
117200150616      /free
117300150617       //?Se utente INT devo caricare le filiali estere a cui è abilitato
117400150617       *in07 = *off;
117500150623       IF  %subst(KNMUS:1:3) = 'INT' and
117600150623           %subst(knmus:1:8) <> 'INT08908' and
117700150623           %subst(knmus:1:8) <> 'INT08905';
117800150617         *in07 = *on;
117900150616         k_TBLkut = 1;
118000150616         k_TBLcod = '£7';
118100150616         k_TBLkey = %subst(knmus:1:6);
118200150616         chain %kds(K03tabel) TABEL00F;
118300150616       //?Non esiste o annullato pulisco il campo
118400150616         IF  not %found(TABEL00F) or TBLflg <> *blanks;
118500150616           clear TBLuni;
118600150616         ENDIF;
118700150616       //?Imposto ls £7
118800150616         TBLuni_£7 = TBLuni;
118900150616       ENDIF;
119000150616      /end-free
119100130619
119200130619      * Se ambiente prova
119300130619     c                   If        %subst(knsif:7:1) = 'P'
119400130619     c                   Eval      wlib = 'GAITRAGRPS'
119500130619     c                   Else
119600130619      * Se ambiente buono FILTRA201
119700130619     c                   Eval      wlib = 'GAITRAGRU '
119800130619     c                   EndIf
119900130619
120000130619      * Apro il file TIORI in GAITRAGRU
120100130619     c                   If        Not %Open(Tiori01l)
120200130619     c                   Clear                   comman
120300130619     c                   Movel(p)  cmd(1)        comman
120400130619     c                   Eval      %Subst(comman:30:10) = wlib
120500130619     c                   Eval      comman =
120600130619     c                             %trim(comman) + '/TIORI01L)'
120700130619     c                   Call      'QCMDEXC'                            99
120800130619     c                   Parm                    comman
120900130619     c                   Parm                    lenght
121000130619     c  n99              Open      Tiori01l
121100130619     c                   EndIf
121200130619
121300130619     c                   endsr
121400001019
121500000928** MSG
121600070418Filiale Errata o non Gestibile                                                      1
121700010301Tipo comunicazione orm errata                                                       2
121800061207Codice ordinante non valido                                                         3
121900061207Immettere l'ordinante se scelta di tutti i luoghi                                   4
122000001023..............  Libero                                                              5
122100001023..............  Libero                                                              6
122200001023..............  Libero                                                              7
122300001023..............  Libero                                                              8
122400001023..............  Libero                                                              9
122500001023..............  Libero                                                             10
122600001023..............  Libero                                                             11
122700001023..............  Libero                                                             12
122800001023..............  Libero                                                             13
122900001023..............  Libero                                                             14
123000001023..............  Libero                                                             15
123100001023..............  Libero                                                             16
123200001023..............  Libero                                                             17
123300001023..............  Libero                                                             18
123400001023..............  Libero                                                             19
123500001023..............  Libero                                                             20
123600001023..............  Libero                                                             21
123700001023..............  Libero                                                             22
123800001023..............  Libero                                                             23
123900001023..............  Libero                                                             24
124000001023..............  Libero                                                             25
124100001023..............  Libero                                                             26
124200001023..............  Libero                                                             27
124300001010..............  Libero                                                             28
124400001010..............  Libero                                                             29
124500001010..............  Libero                                                             30
124600001115** VOC  Lungh. 3                                                             *
124700001115COL                                                                                 1
124800001115PKG                                                                                 2
124900001115VLM                                                                                 3
125000001115BNC                                                                                 4
125100001115BLC                                                                                 5
125200001115AUT                                                                                 6
125300001115MOT                                                                                 7
125400040909** cmd
125500040909OVRDBF FILE(TIORI01L) TOFILE(
