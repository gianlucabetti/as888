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
006800160107
006900160107     d OG143         e ds                  inz
007000160107     d ds15          e ds                  inz
007100150616
007200150616     d TBLuni_£7       ds
007300150616     d   L7                    1     87    dim(29)
007400150616     d   vuoto                88     89
007500001023
007600001023      * Parametri per controllo Data
007700001023
007800001023     D WLBDAT          DS                  INZ
007900001023     D  G02DAT                 1      8  0
008000001023     D  G02INV                 9     16  0
008100001023     D  G02ERR                17     17
008200001023     D  G02TGI                18     22  0
008300001116
008400010205     D Parm01          DS
008500010205     D  ppoe                          3  0
008600010205     D  pnor                          7  0
008700010205     D  pnsr                          2  0
008800010205     D  pnrv                          2  0
008900010205     D  psce                          1
009000010205     D  pfgs                          3  0
009100010205     D  ppor                          3  0
009200010205     D  pdtr                          8  0
009300010205     D  pmdb                         10
009400010205     D  pprb                         10
009500010205     D  pdts                          8  0
009600010205     D  prmp                          1
009700010205     D  pbrc                          1
009800010205     D  pref                          2
009900010205     D  pmio                          1
010000010205
010100001116     D vaods           ds
010200001116     D  vaonrr               397    400b 0
010300061207
010400061207     d                 ds
010500061207     d  vidcor1                1      3  0
010600061207     d  vidcor2                4      7  0
010700061207     d  vidcor12               1      7  0
010800061207     d  vidcor3                8     10  0
010900061207     d  vidcor                 1     10  0
011000061207
011100061207     d                 ds
011200061207     d  vaocor12               1      7  0
011300061207     d  vaocor3                8     10  0
011400061207     d  vaocor                 1     10  0
011500061207
011600040909      *----------------
011700040909      * Costanti
011800040909      *----------------
011900040909     d DIGITn          C                   CONST('1234567890')
012000000927
012100000927      *---------------------------------------------------------------*
012200000927      *                                                               *
012300000927      *    UTILIZZO DEGLI INDICATORI                                  *
012400000927      *                                                               *
012500000927      *---------------------------------------------------------------*
012600000927      *                                                               *
012700001026      *    01 - Posizione Cursore su P.O. Gestione      FIOR151       *
012800001026      *    03 - Disabilito F18                          FIOR151       *
012900061207      *    04 - Richiesto un cliente ordinante
013000061207      *    06 - Tutti luoghi
013100150617      *    07 - Utente Internazionale (INT)
013200001025      *    10 - Confermato almeno un Ordine                           *
013300001026      *    18 - Sproteggo P.O. Gestione                 FIOR151       *
013400010301      *    19 - Tipo comunicazione errato
013500061207      *    20 - Cliente ordinante
013600001026      *    28 - Visualizzo messaggio Errore             FIOR151       *
013700010302      *    30 - Comodo                                                *
013800001024      *    31 - Creato Orm da F6                                      *
013900001024      *    32 - F12 da F6                                             *
014000131003      *    33 - Filiale utente attiva agli Orari Servizi
014100001024      *    39 - Comodo                                                *
014200010301      *    90 - riemetto videata
014300000927      *                                                               *
014400000927      *---------------------------------------------------------------*
014500100218
014600100218      //---------------------------------------------------------------
014700100218      //?Definizione key-list.
014800100218      //---------------------------------------------------------------
014900100218       // - File FNVAOE1L
015000100218     d k06fnvaoe     e ds                  extname(FNVAOE1L:*key)
015100100218     d                                     prefix(k_)
015200100218     d                                     inz
015300140528       // - File TABEL00F
015400140528     d k03tabel      e ds                  extname(TABEL00F:*key)
015500140528     d                                     prefix(k_)
015600140528     d                                     inz
015700100218
015800000927
015900000927      *****************************************************************
016000000927      *                                                               *
016100000927      *    OPERAZIONI INIZIALI                                        *
016200000927      *                                                               *
016300000927      *****************************************************************
016400000927
016500001024
016600001024      *    Inizializzo Campi Video
016700001024
016800000927     C                   EXSR      SR_INIVIDEO
016900000927
017000000927      *****************************************************************
017100000927      *                                                               *
017200000927      *    ROUTINE PRINCIPALE                                         *
017300000927      *                                                               *
017400000927      *****************************************************************
017500001011
017600001019     C                   do        *hival
017700010302
017800010507     C                   if        *in28 = *off and *in90 = *off
017900010302     C                   clear                   vidtco
018000010302     C                   clear                   viddtc
018100061207     c                   clear                   vidcor
018200061207     c                   clear                   vidluo
018300061207     c                   clear                   vidrso
018400010302     C                   endif
018500130619
018600130619      /free
018700130619       //?Se utente non autorizzato alla funzione esco
018800130619         IF  wErrGrave;
018900130619           *in28 = *on;
019000130619           wEsci = *on;
019100130619           VIDmsg = 'Utente non autorizzato alla funzione';
019200130619         ENDIF;
019300130619      /end-free
019400001019
019500001019      *    Emissione Prima Videata
019600001019
019700001026     C                   EXFMT     FIOR151
019800130619
019900130619      /free
020000130619         IF  wEsci;
020100130619           leave;
020200130619         ENDIF;
020300130619      /end-free
020400130619
020500001019     C   KC              leave
020600130619
020700001026     C                   setoff                                       281801
020800061207     C                   setoff                                       192090
020900061207     c                   setoff                                       0406
021000061207
021100061207      * F8=Int.Cod.Ordinante
021200061207     C   kh              exsr      Sr_RicOrdin
021300061207     C   90              iter
021400001019
021500001019      *    Controllo
021600001019
021700001019     C                   EXSR      SR_CONTROL
021800001019
021900001019      *    Se Vi sono Errori o Voglio Cambiare P.O. Riemetto
022000001023      *    .. Riemetto normalmente anche se non ho premuto F6
022100001019
022200001019     C     *in28         ifeq      *on
022300001019     C     *in18         oreq      *on
022400010301     C     *in90         oreq      *on
022500001019     C                   iter
022600001019     C                   endif
022700001023
022800001023      *   GESTIONE DEGLI ORM
022900001023
023000001023     C                   EXSR      SR_GESTIONE
023100001023
023200001019     C                   enddo
023300001019
023400000927     C                   eval      *inlr = *on
023500000927
023600000927      *****************************************************************
023700000927      *                                                               *
023800000927      *    SUBROUTINES                                                *
023900000927      *                                                               *
024000000927      *****************************************************************
024100000927
024200000927      *---------------------------------------------------------------*
024300000927      *    Inizializzazione Campi Video                               *
024400000927      *---------------------------------------------------------------*
024500000927
024600000927     C     SR_INIVIDEO   begsr
024700000927
024800001026      *    Reset Progressivo Nr. Ordini Confermati
024900001025
025000001026     C                   setoff                                       10
025100001025     C                   clear                   vconfe
025200001002
025300001116      *    Inizializzo il P.O. Gestione
025400000927
025500080722     C                   move      dutpou        vidpog
025600001012
025700001116      *    Decodifico il P.O. Gestione
025800000927
025900001116     C     vidpog        chain     azorg01l
026000000927     C                   if        %found(azorg01l)
026100000927     C                   movel     orgdes        viddep
026200000927     C                   else
026300001019     C                   clear                   viddep
026400000927     C                   endif
026500150617
026600150617      /free
026700150617       //?Se Filiale secondo livello o Utente INT F18 non abilitato
026800150617         *in03 = *off;
026900150617         IF  *in07 or DUTlpo = '2' or DUTlpo = *blanks;
027000150617           *in03 = *on;
027100150617         ENDIF;
027200150617      /end-free
027300000927
027400000927      *    Inizializzo i Campi Restanti
027500001012
027600010301     C                   clear                   vidtco
027700010205     C                   move      'SI'          vidvis
027800000927
027900000927     C                   endsr
028000061207      *---------------------------------------------------------------*
028100061207      * F8 - Ricerca cliente ordinante
028200061207      *---------------------------------------------------------------*
028300061207     c     Sr_RicOrdin   BEGSR
028400061207
028500061207     C                   eval      *in90 = *on
028600131108
028700131108     c                   clear                   FIOR81ds
028800131108     c                   IF        VIDcor1 > 0
028900131108     c                   eval      I81fil = VIDcor1
029000131108     c                   ENDIF
029100131108     c                   IF        VIDrso <> *blanks
029200131108     c                   eval      I81rag = VIDrso
029300131108     c                   ENDIF
029400131108     c                   call      'FIOR81R'
029500131108     c                   parm                    kpjba
029600131108     c                   parm                    FIOR81DS
029700131108     c                   IF        O81err <> *blanks
029800131108     c                   eval      VIDmsg = O81msg
029900131108     c                   seton                                        20  28
030000131108     c                   leavesr
030100131108     c                   ENDIF
030200131108     c                   IF        O81ret = '1'
030300131108     c                   leavesr
030400131108     c                   ENDIF
030500131108     c                   movel     O81cro        VIDcor
030600070809      *
030700070809     C                   exsr      sr_ContrCOR
030800061207
030900061207     C                   endsr
031000001019
031100001019      *---------------------------------------------------------------*
031200001019      *    Controllo Prima Videata                                    *
031300001019      *---------------------------------------------------------------*
031400001019
031500001019     C     SR_CONTROL    begsr
031600001019
031700001025     C                   z-add     0             vconfn
031800001025
031900001023     C     *inKF         ifeq      *on
032000001023     C                   move      '1'           wkf
032100001023     C                   else
032200001023     C                   clear                   wkf               1
032300001023     C                   endif
032400001023
032500001116      *    F18=Cambio P.O. Gestione
032600001019
032700001019     C     *inks         ifeq      *on
032800001019     C                   seton                                        18
032900001019     C                   goto      endctr
033000001019     C                   endif
033100001023
033200001116      *    Controllo P.O. Gestione
033300001023
033400001023     C                   clear                   viddep
033500130619     c                   move      VIDpog        w003a
033600130619     c     w003a         lookup    SKpog                                  39
033700001023     C     *in39         ifeq      *off
033800001023     C                   eval      *in01 = *on
033900001023     C                   eval      *in28 = *on
034000001023     C                   eval      *in18 = *on
034100001023     C                   movel     msg(1)        vidmsg
034200001023     C                   goto      endctr
034300001023     C                   else
034400001116     C     vidpog        chain     azorg01l
034500001023     C                   if        %found(azorg01l)
034600001023     C                   movel     orgdes        viddep
034700001023     C                   endif
034800001025      *        Carico £6
034900080722     C                   CLEAR                   trul06ds
035000001025     C                   MOVE      '£6'          D06COD
035100001116     C                   MOVEL     vidpog        D06KEY
035200080722     C                   MOVEL     trul06ds      KPJBU
035300001025     C                   CALL      'TRUL06R'
035400001025     C                   PARM                    KPJBA
035500080722     C                   MOVEL     KPJBU         trul06ds
035600001025     C                   MOVEA     LIN           L6
035700001023     C                   endif
035800150616
035900150616      /free
036000150616       //?Se utente INT devo caricare la £7 nella £6
036100150617         IF  *in07;
036200150617           xx = 1;
036300150617           FOR xx by 1 to %elem(L7);
036400150617             IF  L7(xx) <> *zeros and L7(xx) <> *blanks;
036500150617               L6(xx) = %dec(L7(xx):3:0);
036600150617             ENDIF;
036700150617           ENDFOR;
036800150616         ENDIF;
036900150616      /end-free
037000010302
037100010302     C                   clear                   viddtc
037200010301      * Ricerca Tipo Comunicazione con "?"
037300010301     C     '?'           scan      vidtco                                 30
037400010301     C     *in30         ifeq      *on
037500010301     C                   eval      *in90 = *on
037600010301     C                   eval      *in19 = *on
037700010301     C                   clear                   TIBS02DS
037800010301     C                   movel     'R'           t02mod
037900010301     C                   movel     knsif         t02sif
038000010301     C                   movel     'TCO'         t02cod
038100010301     C                   call      'TIBS02R'
038200010301     C                   parm                    KPJBA
038300010301     C                   parm                    TIBS02DS
038400010301     C                   movel     T02ke1        vidtco
038500010301     C                   movel     T02uni        viddtc
038600010301     C                   goto      endctr
038700010301     C                   endif
038800010301      * Tipo comunicazione
038900010301     C                   if        vidtco <> *blanks
039000010301     C                   clear                   TIBS02DS
039100010301     C                   movel     'C'           T02mod
039200010301     C                   movel     knsif         t02sif
039300010301     C                   movel     'TCO'         t02cod
039400010301     C                   movel(p)  vidtco        T02ke1
039500010301     C                   call      'TIBS02R'
039600010301     C                   parm                    KPJBA
039700010301     C                   parm                    TIBS02DS
039800010301     C                   if        t02err <> *blanks
039900010301     C                   movel     msg(2)        vidmsg
040000010301     C                   seton                                        19  28
040100010301     C                   goto      endctr
040200010301     C                   endif
040300010301     C                   movel     t02ke1        vidtco
040400010301     C                   movel     t02uni        viddtc
040500010301     C                   endif
040600061207
040700061207      * cliente ordinante
040800061207     C                   if        vidcor <> *zeros
040900061207     c                   eval      *in04 = *on
041000061207     C                   exsr      Sr_Contrcor
041100061207     C   28              leavesr
041200061207     C                   endif
041300061207      * tutti luoghi
041400061207     c                   if        vidluo <> *blanks
041500061207     c                   if        vidcor = *zeros
041600061207     c                   eval      vidmsg = msg(4)
041700061207     c                   eval      *in20 = *on
041800061207     c                   eval      *in28 = *on
041900061207     c   28              leavesr
042000061207     c                   endif
042100061207     c                   eval      *in06 = *on
042200061207     c                   endif
042300001023
042400001019     C     ENDCTR        endsr
042500061207
042600061207      *---------------------------------------------------------------*
042700061207      * Controllo  cliente ordinante
042800061207      *---------------------------------------------------------------*
042900061207     C     Sr_Contrcor   BEGSR
043000061207
043100061207     C     vidcor        chain     fnacr01l
043200061207     C                   if        %found(fnacr01l)
043300061207     C                   if        ACRatb <> *blanks
043400061207     C                   movel     msg(3)        vidmsg
043500061207     C                   seton                                        20  28
043600061207     C                   endif
043700061207     C  n28              movel     ACRrsr        vidrso
043800061207     C                   else
043900061207     C                   movel     msg(3)        vidmsg
044000061207     C                   seton                                        20  28
044100061207     C                   endif
044200061207
044300061207     C                   endsr
044400001023
044500001023      *---------------------------------------------------------------*
044600001023      *    Lettura / Gestione degli ORM                               *
044700001023      *---------------------------------------------------------------*
044800001023
044900001023     C     SR_GESTIONE   begsr
045000001023
045100001026      *    Pulisco Subfile e mi posiziono per Lettura Ordini C.Center
045200001024
045300001024     C                   do        *hival
045400001023     C                   z-add     0             nn1               4 0
045500001023     C                   eval      *in85 = *on
045600001023     C                   write     fiorrc1
045700001023     C                   eval      *in85 = *off
045800001023     C                   z-add     1             recsf
045900001025
046000001025      *    Carico gli ordini della £6
046100001025
046200001025     C     1             do        30            zz                3 0
046300001025
046400001025     C     L6(zz)        ifeq      0
046500001025     C                   iter
046600001025     C                   endif
046700001025
046800001116     C                   z-add     L6(zz)        vs1poe
046900001025
047000001116     C     vs1poe        setll     fnvao01l
047100001023
047200001026      *    Leggo Ordini Call Center
047300001023
047400001023     C                   do        *hival
047500001024     C     wkf           ifne      '1'
047600001116     C     vs1poe        reade(n)  fnvao01l
047700001024     C                   else
047800040401     C     vs1poe        reade(e)  fnvao01l
047900040401      * Allocato leggo il record e poi torno subito a leggere quello sucessivo
048000040401     c                   If        %Error
048100040401     c                   Read(n)   Fnvao01l
048200040401     c                   Iter
048300040401     c                   EndIf
048400001024     C                   endif
048500040401      * Fine File
048600001115     C                   if        %eof(fnvao01l)
048700001023     C                   leave
048800001023     C                   endif
048900001023
049000001026     C     nn1           ifge      8000
049100001024     C                   leave
049200001024     C                   endif
049300010302      * Annullati
049400010302     C                   if        vaoatb <> *blanks
049500010302     C                   iter
049600010302     C                   endif
049700010301      * Tipo comunicazione
049800010301     C                   if        vidtco <> *blanks and vaotco <> vidtco
049900010301     C                   iter
050000010301     C                   endif
050100061207      * cliente ordinante
050200061207     c                   if        vidcor <> *zeros
050300061207     c                             and vidluo = *blanks and vidcor <> vaocor
050400061207     c                   iter
050500061207     c                   endif
050600061207     c                   if        vidcor <> *zeros
050700061207     c                             and vidluo <> *blanks and
050800061207     c                                 vidcor12 <> vaocor12
050900061207     c                   iter
051000061207     c                   endif
051100001024
051200001026      *    SCRIVO SUBFILE
051300001024      *    .. se ho premuto F6 tento prima la conferma automatica
051400001024      *    .. richiamando il pgm di creazione ORM
051500001024
051600001024     C                   setoff                                       31  32
051700001024     C     wkf           ifeq      '1'
051800001024     C                   EXSR      SR_RICHIAMA
051900001024     C     §rmerr        ifne      ' '
052000001115     C                   unlock    fnvao01l
052100001024     C     §rmerr        ifeq      '3'
052200001024     C                   seton                                        32
052300001024     C                   endif
052400001024     C                   else
052500001116     C                   seton                                        31
052600100218     c                   exsr      DelVaoE
052700040909     c                   ExSr      xTIORI
052800001116     C                   delete    fnvao000
052900001025     C                   add       1             vconfe
053000001025     C                   add       1             vconfn
053100001025     C                   seton                                        10
053200001024     C                   endif
053300001024     C                   endif
053400001024
053500001024     C     *in31         ifeq      *off
053600001024     C     wkf           ifne      '1'
053700001026     C                   clear                   vs1opz
053800001026     C                   eval      *in05 = *off
053900001024     C                   else
054000001024     C     *in32         ifeq      *on
054100001026     C                   clear                   vs1opz
054200001026     C                   eval      *in05 = *off
054300001024     C                   else
054400001025     C                   move      'E'           vs1opz
054500001024     C                   eval      *in05 = *on
054600001024     C                   endif
054700001024     C                   endif
054800011029
054900001129     C                   movel     vaotco        vs1tco
055000140520
055100140520      * Pulisco i dati del mittente
055200140520     C                   clear                   vs1rsr
055300140520     C                   clear                   vs1inr
055400140520     C                   clear                   vs1car
055500140520     C                   clear                   vs1lor
055600140520     C                   clear                   vs1prr
055700140520     C                   clear                   vs1nar
055800011029
055900011029      * Passo i dati relativi al mittente
056000011029     C                   select
056100011029     C                   when      vaorsr <> *blanks
056200110121     c                   Eval      wtxt = vaorsr
056300110121     c                   ExSr      Sr_Chk
056400110121     c                   Movel     wtxt          vs1rsr
056500110121     c                   Eval      wtxt = vaoinr
056600110121     c                   ExSr      Sr_Chk
056700110121     c                   Movel     wtxt          vs1inr
056800110121     c                   Eval      wtxt = vaocar
056900110121     c                   ExSr      Sr_Chk
057000110121     c                   Movel     wtxt          vs1car
057100110121     c                   Eval      wtxt = vaolor
057200110121     c                   ExSr      Sr_Chk
057300110121     c                   Movel     wtxt          vs1lor
057400110121     c                   Eval      wtxt = vaoprr
057500110121     c                   ExSr      Sr_Chk
057600110121     c                   Movel     wtxt          vs1prr
057700110121     c                   Eval      wtxt = vaonar
057800110121     c                   ExSr      Sr_Chk
057900110121     c                   Movel     wtxt          vs1nar
058000011029
058100011029     C                   when      vaocra <> *zeros
058200011029     C     vaocra        chain     fnacr01l
058300011029     C                   if        %found(fnacr01l)
058400011029     C                   movel     ACRrsr        vs1rsr
058500011029     C                   movel     ACRinr        vs1inr
058600011029     C                   movel     ACRcar        vs1car
058700011029     C                   movel     ACRlor        vs1lor
058800011029     C                   movel     ACRprr        vs1prr
058900011029     C                   movel     ACRnar        vs1nar
059000011029     C                   else
059100011029     C                   clear                   vs1rsr
059200011029     C                   clear                   vs1inr
059300011029     C                   clear                   vs1car
059400011029     C                   clear                   vs1lor
059500011029     C                   clear                   vs1prr
059600011029     C                   clear                   vs1nar
059700011029     C                   endif
059800011029     C                   endsl
059900011112
060000011112      * Se prepagato metto flag nel subfile
060100011112     C                   if        vaotor = 'P'
060200011112     C                   eval      vs1pre = 'S'
060300011112     C                   else
060400011112     C                   clear                   vs1pre
060500011112     C                   endif
060600011112
060700110121     c                   Eval      wtxt = vaorfa
060800110121     c                   ExSr      Sr_Chk
060900110121     c                   Movel     wtxt          vs1rfa
061000001116     C                   movel     vaopor        vs1por
061100001116     C                   z-add     vaonrr        vs1nrr
061200001116      * Decodifica voce e imposta qtà relativa
061300001115     C                   select
061400001115     C                   when      vaoncl <> *zeros
061500001115     C                   movel     voc(1)        vs1voc
061600001115     C                   z-add     vaoncl        vs1qta
061700001115
061800001115     C                   when      vaopkg <> *zeros
061900001115     C                   movel     voc(2)        vs1voc
062000001115     C                   z-add     vaopkg        vs1qta
062100001115
062200001115     C                   when      vaovlm <> *zeros
062300001115     C                   movel     voc(3)        vs1voc
062400001115     C                   z-add     vaovlm        vs1qta
062500001115
062600001115     C                   when      vaobnc <> *zeros
062700001115     C                   movel     voc(4)        vs1voc
062800001115     C                   z-add     vaobnc        vs1qta
062900001115
063000001115     C                   when      vaoblc <> *zeros
063100001115     C                   movel     voc(5)        vs1voc
063200001115     C                   z-add     vaoblc        vs1qta
063300001115
063400001115     C                   when      vaoatt <> *zeros
063500001115     C                   movel     voc(6)        vs1voc
063600001115     C                   z-add     vaoatt        vs1qta
063700001115
063800001115     C                   when      vaomtc <> *zeros
063900001115     C                   movel     voc(7)        vs1voc
064000001115     C                   z-add     vaomtc        vs1qta
064100001115
064200001115     C                   other
064300001115     C                   z-add     *zeros        vs1qta
064400001115     C                   movel     *blanks       vs1voc
064500001115
064600001115     C                   endsl
064700001024
064800001024     C                   add       1             nn1
064900001024     C                   write     fiorsf1
065000001024     C                   endif
065100001024
065200001023     C                   enddo
065300001025     C                   enddo
065400010302
065500010302     C                   clear                   wkq
065600001023
065700001024      *    EMISSIONE RECORD DI CONTROLLO
065800001024
065900001025     C     vconfn        ifeq      0
066000001025     C                   eval      *in12 = *off
066100001025     C                   else
066200001025     C                   eval      *in12 = *on
066300001025     C                   endif
066400001025
066500010316      * Nessun record
066600001024     C     nn1           ifeq      0
066700010316     C                   clear                   wkf
066800010316     C                   do        *hival
066900001026     C                   exfmt     fior153
067000010316     C                   if        *inkl = *on
067100010316     C                   goto      endgest
067200010316     C                   endif
067300010316     C                   enddo
067400010316     C                   endif
067500010302
067600010302     C                   eval      sav_nn1 = nn1
067700001026     C                   write     fior152
067800001024     C                   exfmt     fiorrc1
067900001024     C     *inkf         ifeq      *on
068000001024     C                   move      '1'           wkf
068100001024     C                   else
068200001024     C                   clear                   wkf
068300001024     C                   endif
068400001024
068500001025     C                   z-add     0             vconfn
068600010302     C                   eval      nn1 = *zeros
068700001025
068800001024      *    LETTURA RECORDS SUBFILE
068900001024
069000001024     C     *inkl         ifeq      *off
069100001024
069200001024     C                   do        *hival
069300010302     C                   eval      nn1 = nn1 +1
069400001024
069500001024     C     *inkf         ifeq      *on
069600001024     C                   leave
069700001024     C                   endif
069800010302     C     *inkq         ifeq      *on
069900010302     C                   move      '1'           wkq               1
070000010302     C                   endif
070100001024
070200010302     C     nn1           chain     fiorsf1                            30
070300010302     C                   if        *in30 = *on
070400010302     C                   leave
070500001024     C                   endif
070600040401
070700040401     c                   Eval      *In28 = *Off
070800001024
070900010302     C                   if        *inkq = *off
071000001024     C     vs1opz        ifeq      '1'
071100001024     C     vs1opz        oreq      '2'
071200040401     C     vs1nrr        chain(e)  fnvao00f
071300040401      * Allocato leggo il record sucessivo
071400040401     c                   If        %Error
071500040401     c                   Iter
071600040401     c                   EndIf
071700040401
071800001116     C                   if        %found (fnvao00f)
071900001024
072000001026      *    Richiamo Programma di Creazione ORM
072100001024
072200001024     C                   EXSR      SR_RICHIAMA
072300001024
072400001024     C     §rmerr        ifne      ' '
072500001116     C                   unlock    fnvao00f
072600001024     C                   else
072700100218     c                   exsr      DelVaoE
072800040909     c                   ExSr      xTIORI
072900001116     C                   delete    fnvao0
073000001025     C                   add       1             vconfe
073100001025     C                   add       1             vconfn
073200001025     C                   seton                                        10
073300001024     C                   endif
073400001024
073500001024     C                   endif
073600001024     C                   endif
073700010302     C                   endif
073800001024
073900010302     C     vs1opz        ifeq      '4'
074000011029     C                   eval      vs1atb = 'A'
074100011029     C                   else
074200011029     C                   eval      vs1atb = ' '
074300010302     C                   endif
074400010302
074500010302     C                   update    fiorsf1
074600001024     C                   enddo
074700001024
074800001024     C                   else
074900001024     C                   leave
075000001024     C                   endif
075100010302
075200010302      * Se f16 e sono stati annullati dei record annullare vao
075300001024
075400010302     C                   if        wkq = '1'
075500010302     C                   if        sav_nn1 <> 0
075600010302     C                   eval      nn1 = *zeros
075700010302     C                   do        *hival
075800010302     C                   eval      nn1 = nn1 +1
075900010302     C     nn1           chain     fiorsf1                            30
076000010302     C                   if        *in30 = *on
076100010302     C                   leave
076200010302     C                   endif
076300010302     C                   if        vs1atb = *blanks
076400010302     C                   iter
076500010302     C                   endif
076600040401     C     vs1nrr        chain(e)  fnvao00f
076700040401      * Allocato leggo il record sucessivo
076800040401     c                   If        %Error
076900040401     c                   Iter
077000040401     c                   EndIf
077100080722     c                   eval      dorm01 = vaoflo
077200010302     C                   if        %found (fnvao00f)
077300011029     C                   delete    fnvao0
077400010302     C                   endif
077500100218      * annullo estensione
077600100218     c                   exsr      DelVaoE
077700090402      * imposto la data e ora di annullamento
077800090402     c                   eval      wtime = %char(%timestamp:*iso0)
077900080722      * devo annullare anche TIORI
078000080722     c                   exsr      xtiori
078100010302     C                   enddo
078200010302     C                   endif
078300010302     C                   endif
078400010302
078500001024     C                   enddo
078600010316
078700010316     C     endgest       endsr
078800001024
078900001024      *---------------------------------------------------------------*
079000001024      *    Richiama PGM Creazione ORM                                 *
079100001024      *---------------------------------------------------------------*
079200001024
079300001024     C     SR_RICHIAMA   begsr
079400001024
079500001024     C                   clear                   FIOR05DS
079600001024     C     wkf           ifne      '1'
079700001024     C                   move      vs1opz        §rmtla
079800001024     C                   else
079900001024     C     vidvis        ifeq      'NO'
080000001024     C                   move      '0'           §rmtla
080100001024     C                   else
080200001024     C                   move      '2'           §rmtla
080300001024     C                   endif
080400001024     C                   endif
080500001026     C                   move      'C'           §rmfpr
080600001115     C                   move      vaopoe        §rmpoe
080700010601     C                   z-add     vaonsr        §rmnsr
080800010601     C                   z-add     vaonor        §rmnor
080900010601     C                   z-add     vaonrv        §rmnrv
081000010228     C                   move      vaotor        §rmtor
081100001129     C                   move      vaotco        §rmtco
081200001115     C                   movel     vaodao        §rmdao
081300001115     C                   movel     vaooao        §rmoao
081400050915
081500010601     C                   movel     vaocor        §rmcor
081600050915     c                   Eval      wtxt = vaorso
081700050915     c                   ExSr      Sr_Chk
081800050915     c                   Movel     wtxt          §rmrso
081900050915     c                   Eval      wtxt = vaoino
082000050915     c                   ExSr      Sr_Chk
082100050915     c                   Movel     wtxt          §rmino
082200050915     c                   Eval      wtxt = vaocao
082300050915     c                   ExSr      Sr_Chk
082400050915     c                   Movel     wtxt          §rmcao
082500050915     c                   Eval      wtxt = vaoloo
082600050915     c                   ExSr      Sr_Chk
082700050915     c                   Movel     wtxt          §rmloo
082800010601     C                   movel     vaopro        §rmpro
082900010601     C                   movel     vaonao        §rmnao
083000050915
083100001115     C                   movel     vaocra        §rmcra
083200050915     c                   Eval      wtxt = vaorsr
083300050915     c                   ExSr      Sr_Chk
083400050915     c                   Movel     wtxt          §rmrsr
083500050915     c                   Eval      wtxt = vaoinr
083600050915     c                   ExSr      Sr_Chk
083700050915     c                   Movel     wtxt          §rminr
083800050915     c                   Eval      wtxt = vaocar
083900050915     c                   ExSr      Sr_Chk
084000050915     c                   Movel     wtxt          §rmcar
084100050915     c                   Eval      wtxt = vaolor
084200050915     c                   ExSr      Sr_Chk
084300050915     c                   Movel     wtxt          §rmlor
084400001115     C                   movel     vaoprr        §rmprr
084500001115     C                   movel     vaonar        §rmnar
084600140110     c                   IF        vaocra <> *zeros
084700140110     c     vaocra        chain     fnacr01l
084800140110     c                   if        %found(fnacr01l)
084900140110     c                   movel     ACRrsr        §rmrsr
085000140110     c                   movel     ACRinr        §rminr
085100140110     c                   movel     ACRcar        §rmcar
085200140110     c                   movel     ACRlor        §rmlor
085300140110     c                   movel     ACRprr        §rmprr
085400140110     c                   movel     ACRnar        §Rmnar
085500140110     c                   ENDIF
085600140110     c                   ENDIF
085700070905
085800070905     c                   eval      wtxt = vaorer
085900070905     c                   exsr      sr_chk
086000070905     c                   movel     wtxt          §rmrer
086100001115     C                   movel     vaoter        §rmter
086200050915
086300010601     C                   movel     vaocrc        §rmcrc
086400050915     c                   Eval      wtxt = vaorsc
086500050915     c                   ExSr      Sr_Chk
086600050915     c                   Movel     wtxt          §rmrsc
086700050915     c                   Eval      wtxt = vaoinc
086800050915     c                   ExSr      Sr_Chk
086900050915     c                   Movel     wtxt          §rminc
087000050915     c                   Eval      wtxt = vaocac
087100050915     c                   ExSr      Sr_Chk
087200050915     c                   Movel     wtxt          §rmcac
087300050915     c                   Eval      wtxt = vaoloc
087400050915     c                   ExSr      Sr_Chk
087500050915     c                   Movel     wtxt          §rmloc
087600010601     C                   movel     vaoprc        §rmprc
087700010601     C                   movel     vaonac        §rmnac
087800050915
087900001115     C                   movel     vaoffd        §rmffd
088000131212     c                   IF        VAOffd = 'N'
088100131212     c                   clear                   §RMffd
088200131212     c                   ENDIF
088300010601     C                   movel     vaodar        §rmdar
088400010601     C                   movel     vaoorr        §rmorr
088500010601     C                   movel     vaormp        §rmrmp
088600050915     c                   Eval      wtxt = vaonam
088700050915     c                   ExSr      Sr_Chk
088800050915     c                   Movel     wtxt          §rmnam
088900010601     C                   movel     vaoncl        §rmncl
089000010601     C                   movel     vaopkg        §rmpkg
089100010601     C                   movel     vaovlm        §rmvlm
089200010601     C                   movel     vaobnc        §rmbnc
089300010601     C                   movel     vaoblc        §rmblc
089400010601     C                   movel     vaoatt        §rmatt
089500010601     C                   movel     vaomtc        §rmmtc
089600010601     C                   movel     vaopag        §rmpag
089700010601     C                   movel     vaoksc        §rmksc
089800010601     C                   movel     vaoctr        §rmctr
089900010601     C                   movel     vaopor        §rmpor
090000010601     C                   movel     vaozor        §rmzor
090100010601     C                   movel     vaopoc        §rmpoc
090200160107      /free
090300160107       //?Se destinatario estero e Filiale Consegna inserita è una fil.DPD
090400160107       //?devo cercare la filiale giusta
090500160107         clear OG143;
090600160107         clear ds15;
090700160107         IF  VAOpoc > 0 and VAOnac <> *blanks;
090800160107           chain (VAOpoc) AZORG01L;
090900160107           IF  %found(AZORG01L);
091000160107             OG143 = ORGde3;
091100160107           ENDIF;
091200160107           IF  §OGntw = 'DPD';
091300160107          //?cerco la lna sulla tabella nazioni
091400160107             k_TBLkut = 1;
091500160107             k_TBLcod = '15';
091600160107             k_TBLkey = VAOnac;
091700160107             chain %kds(K03tabel) TABEL00F;
091800160107             IF  %found(TABEL00F) and TBLflg = *blanks;
091900160107               ds15 = TBLuni;
092000160107               §RMpoc = §15lad;
092100160107             ENDIF;
092200160107           ENDIF;
092300160107         ENDIF;
092400160107      /end-free
092500070905
092600070905     c                   eval      wtxt = vaono1
092700070905     c                   exsr      sr_chk
092800070905     c                   movel     wtxt          §rmno1
092900070905     c                   eval      wtxt = vaono2
093000070905     c                   exsr      sr_chk
093100070905     c                   movel     wtxt          §rmno2
093200001115     C                   movel     vaoddt        §rmddt
093300050915     c                   Eval      wtxt = vaorfa
093400050915     c                   ExSr      Sr_Chk
093500050915     c                   Movel     wtxt          §rmrfa
093600010601     C                   movel     vaosto        §rmsto
093700010118     C                   movel     vaospi        §rmspi
093800010507     C                   movel     vaoflo        §rmflo
093900040714      * controllo se i dati immessi in §rmflo sono giusti
094000040714     c                   Eval      dorm01 = §rmflo
094100150714      /free
094200150714       //?Alla posizione 11 c'è una 'X' la elimino
094300150714         IF  §ORMauto = 'X';
094400150714           clear §ORMauto;
094500150714         ENDIF;
094600150714      /end-free
094700040714     c                   If        §ormcb <> 'S'
094800040714     c                   Clear                   §ormcb
094900040714     c                   EndIf
095000040714     c                   If        §ormfr <> 'S'
095100040714     c                   Clear                   §ormfr
095200040714     c                   EndIf
095300040714     c                   If        §ormks <> 'S'
095400040714     c                   Clear                   §ormks
095500040714     c                   EndIf
095600040714     c                   If        §ormfd <> 'S'
095700040714     c                   Clear                   §ormfd
095800040714     c                   EndIf
095900040714     c                   If        §orcom <> 'S'
096000040714     c                   Clear                   §orcom
096100040714     c                   EndIf
096200040714     c                   If        §orcomc <> 'S' and §orcomc <> 'N'
096300040714     c                   Clear                   §orcomc
096400040714     c                   EndIf
096500050202     c                   If        §orsrm <> 'S'
096600050202     c                   Clear                   §orsrm
096700050202     c                   EndIf
096800040714     c                   Eval      §rmflo = dorm01
096900010412
097000010412      * Se non inserita prendo l'ora dall'anagrafico
097100010412     C                   if        §rmorr = *zeros
097200010412     C     §rmcra        chain     fnacr01l
097300010412     C                   if        %found(fnacr01l)
097400010412     C                   movel     acrorr        §rmorr
097500010412     C                   endif
097600010412     C                   endif
097700140528      /free
097800140528       //?Verifico se esiste il rcd 'G' su VAOE
097900140528       //?se esite controllo se OK il tipo servizio
098000140528       //?se NON è OK non devo memorizzarlo
098100140528       exsr VaoRcdG;
098200140528      /end-free
098300010205
098400150306     c                   clear                   kpjbu
098500001024     C                   call      'FIOR05R'
098600001024     C                   parm                    KPJBA
098700001024     C                   parm                    FIOR05DS
098800001024
098900001024     C                   endsr
099000050915
099100050915      *---------------------------------------------------------
099200050915      * Controllo i caratteri dei campi alfanumerici
099300050915      *---------------------------------------------------------
099400050915     c     Sr_Chk        BegSr
099500050915
099600050915     c                   Eval      TxtInOut = wtxt
099700070905     c                   Eval      Uppercase = '1'
099800050915     c                   Call      'XCHKCHAR'
099900050915     c                   Parm                    TxtInOut
100000050915     c                   Parm                    ElencoChar
100100050915     c                   Parm                    TipoElenco
100200050915     c                   Parm                    CharSost
100300050915     c                   Parm                    UpperCase
100400050915     c                   Parm                    ChkNull
100500050915     c                   Parm                    CharNull
100600050915     c                   Parm                    Esito
100700050915     c                   If        Esito = '1'
100800050915     c                   Move      TxtInOut      wtxt
100900050915     c                   EndIf
101000050915
101100050915     c                   EndSr
101200100218
101300100218      /free
101400140528       //--------------------------------------------------------------------
101500140528       //?Controllo Tipo Servizio.
101600140528       //--------------------------------------------------------------------
101700140528       BEGSR VaoRcdG;
101800140528
101900140528         clear dOREGEN;
102000140528
102100140528       //?Imposto la chiave per il FNVAOE rcd 'G '
102200140528         k_vaoecor = vaocor;
102300140528         k_vaoepoe = vaopoe;
102400140528         k_vaoensr = vaonsr;
102500140528         k_vaoenor = vaonor;
102600140528         k_vaoenrv = vaonrv;
102700140528         k_vaoetrc = 'G ';
102800140528         chain  %kds(k06fnvaoe) FNVAOE1L;
102900140528         IF  not %found(FNVAOE1L);
103000140528           leavesr;
103100140528         ENDIF;
103200140528         dOREGEN = VAOEdati;
103300140528         IF  doregen.§OREtsp <> *blanks;
103400140528       //?Imposto la chiave per TABELLA '5E'
103500140528           k_TBLkut = 1;
103600140528           k_TBLcod = '5E';
103700140528           k_TBLkey = doregen.§OREtsp;
103800140528           chain %kds(K03tabel) TABEL00F;
103900140528         //?Non esiste o annullato pulisco il campo
104000140528           IF  not %found(TABEL00F) or TBLflg <> *blanks;
104100140528             clear doregen.§OREtsp;
104200140528           ENDIF;
104300140528           VAOEdati = dOREGEN;
104400140528         //?Se tutto il rcd è vuoto cancello il rcd se no lo aggiorno
104500140528           IF  VAOEdati = *blanks;
104600140528             delete FNVAOE00;
104700140528           ELSE;
104800140528             update FNVAOE00;
104900140528           ENDIF;
105000140528         ELSE;
105100140528           unlock FNVAOE1L;
105200140528         ENDIF;
105300140528
105400140528       ENDSR;
105500140528
105600100218       //--------------------------------------------------------------------
105700100218       //?Delete l'estensione del VAO.
105800100218       //--------------------------------------------------------------------
105900100218       BEGSR DelVaoE;
106000100218
106100100218       //?Imposto la chiave per il FNVAOE
106200100218         k_vaoecor = vaocor;
106300100218         k_vaoepoe = vaopoe;
106400100218         k_vaoensr = vaonsr;
106500100218         k_vaoenor = vaonor;
106600100218         k_vaoenrv = vaonrv;
106700100218       //?Cancello i rcd
106800100218         setll  %kds(k06fnvaoe : 5) FNVAOE1L;
106900100218         reade  %kds(k06fnvaoe : 5) FNVAOE1L;
107000100318         DOW not %eof(FNVAOE1L);
107100100218           delete fnvaoe00;
107200100218           reade  %kds(k06fnvaoe : 5) FNVAOE1L;
107300100218         ENDDO;
107400100218
107500100218       ENDSR;
107600100218      /end-free
107700040909
107800040909      *--------------------------------------------------------------------------------------------*
107900040909      * xTIORI - aggiornamento file TIORI01L
108000040909      *--------------------------------------------------------------------------------------------*
108100040909
108200040909     c     xTIORI        BEGSR
108300040909
108400040909      * reperisce il progressivo
108500040909      * e ne controlla la numericità
108600040909     c     DigitN        check     §ORMpg                                 97
108700040909      *
108800040909      * se effettivamente numerico ...
108900040909if  1c                   if        not *in97
109000040909      *
109100040909      *   aggiorna il file di ordini ritiro merce
109200040909     c                   move      §ORMpg        ORIprg
109300040909     c     ORIprg        chain     tiori01l                           97
109400080722      * aggiorno con numero ORM
109500080722if  2c                   if        not *in97
109600080722     c                   if        wkq <> '1'
109700040909     c                   eval      ORIpoe = §RMpoeo
109800040909     c                   eval      ORInsr = §RMnsro
109900040909     c                   eval      ORInor = §RMnoro
110000040909     c                   eval      ORInrv = §RMnrvo
110100080722     c                   else
110200080722     c                   eval      vaoatb = 'A'
110300090402      * utente e data ora annullamento
110400090402     c
110500090402     c                   eval      oriflo = knmus +
110600090402     c                                      %subst(wtime:3:6) +
110700090402     c                                      %subst(wtime:9:4)
110800080722     c                   endif
110900040909      *
111000040909     c                   update    tiori000
111100040909e   2c                   endif
111200040909      *
111300040909e   1c                   endif
111400040909
111500040909     c                   ENDSR
111600130619
111700130619      *------------------------------------------------------------------------*
111800130619      * ROUTINE INIZIALE
111900130619      *------------------------------------------------------------------------*
112000130619     c     *inzsr        begsr
112100130619
112200130619     c     *entry        plist
112300130619     c                   parm                    kpjba
112400130619
112500130619      *    Recupero Ragione Sociale Ditta da Emettere a Video
112600130619      *    e Punti Operativi Gestiti
112700130619     c     *dtaara       define    §azute        azuteds
112800130619     c     *dtaara       define    §datiute      ddatiute
112900130619      *
113000130619     c                   in(E)     *dtaara
113100130619     c                   IF        %ERROR or RSUT = *blanks
113200130619     c                   clear                   Tibs34Ds
113300130619     c                   call      'TIBS34R'
113400130619     c                   parm                    Tibs34Ds
113500130619     c                   in        *dtaara
113600130619     c                   ENDIF
113700130619
113800130619
113900130619      * controllo abilitazione utente
114000130619     c                   clear                   wabi
114100130619     c                   clear                   dLAT
114200130619     c                   eval      dUTE01 = UTEfaf
114300130619
114400130619      * Verifica errori e autorità profilo
114500130619     c                   SELECT
114600130619      * se ho errori nei dati utente esco dal pgm
114700130619     c                   WHEN      DUTerr = 'E'
114800130619     c                   eval      wErrGrave = *on
114900130619     c                   leavesr
115000130619      * carica abilitazioni del profilo
115100130619     c                   WHEN      §UTEorm <> *blanks
115200130619     c                   eval      wabi = §UTEorm
115300130619     c                   WHEN      UTEaut <> *blanks
115400130619     c                   eval      wabi = UTEaut
115500130619      * se non c'è l'abilitazione
115600130619      * --> se 1° livello, abilitazioni al terminal
115700130619      *     se 2° livello, abilitazioni al punto operativo
115800130619      *     se sede no abilitazioni
115900130619     c                   OTHER
116000130619     c                   IF        DUTlpo = '1'
116100130619     c                   eval      wabi   = 'TP'
116200130619     c                   ENDIF
116300130619     c                   IF        DUTlpo = '2'
116400130619     c                   eval      wabi   = 'PO'
116500130619     c                   ENDIF
116600130619     c                   ENDSL
116700130619
116800130619      * controllo se ok l'abilitazione dell'utente
116900130619     c                   clear                   TIBS02DS
117000130619     c                   eval      T02mod = 'C'
117100130619     c                   eval      T02sif = knsif
117200130619     c                   eval      T02cod = 'LAT'
117300130619     c                   eval      T02ke1 = wabi
117400130619     c                   Call      'TIBS02R'
117500130619     c                   Parm                    KPJBA
117600130619     c                   Parm                    TIBS02DS
117700130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
117800130619     c                   eval      wErrGrave = *on
117900130619     c                   leavesr
118000130619     c                   ENDIF
118100130619     c                   eval      dLAT = T02uni
118200130619
118300130619      * Reperimento delle filiali gestibili dall'utente
118400130619     c                   clear                   TRUL31DS
118500130619     c                   eval      I31abi = wabi
118600130619     c                   eval      I31cdi = DUTdis
118700130619     c                   eval      I31car = DUTare
118800130619     c                   eval      I31cpo = DUTpou
118900130619     c                   call      'TRUL31R'
119000130619     c                   parm                    KPJBA
119100130619     c                   parm                    TRUL31DS
119200130619     c                   IF        O31pog > *zeros
119300130619     c                   movea     O31pog        SKpog
119400130619     c                   ELSE
119500130619     c                   eval      wErrGrave = *on
119600130619     c                   leavesr
119700130619     c                   ENDIF
119800150616      /free
119900150617       //?Se utente INT devo caricare le filiali estere a cui è abilitato
120000150617       *in07 = *off;
120100150623       IF  %subst(KNMUS:1:3) = 'INT' and
120200150623           %subst(knmus:1:8) <> 'INT08908' and
120300150623           %subst(knmus:1:8) <> 'INT08905';
120400150617         *in07 = *on;
120500150616         k_TBLkut = 1;
120600150616         k_TBLcod = '£7';
120700150616         k_TBLkey = %subst(knmus:1:6);
120800150616         chain %kds(K03tabel) TABEL00F;
120900150616       //?Non esiste o annullato pulisco il campo
121000150616         IF  not %found(TABEL00F) or TBLflg <> *blanks;
121100150616           clear TBLuni;
121200150616         ENDIF;
121300150616       //?Imposto ls £7
121400150616         TBLuni_£7 = TBLuni;
121500150616       ENDIF;
121600150616      /end-free
121700130619
121800130619      * Se ambiente prova
121900130619     c                   If        %subst(knsif:7:1) = 'P'
122000130619     c                   Eval      wlib = 'GAITRAGRPS'
122100130619     c                   Else
122200130619      * Se ambiente buono FILTRA201
122300130619     c                   Eval      wlib = 'GAITRAGRU '
122400130619     c                   EndIf
122500130619
122600130619      * Apro il file TIORI in GAITRAGRU
122700130619     c                   If        Not %Open(Tiori01l)
122800130619     c                   Clear                   comman
122900130619     c                   Movel(p)  cmd(1)        comman
123000130619     c                   Eval      %Subst(comman:30:10) = wlib
123100130619     c                   Eval      comman =
123200130619     c                             %trim(comman) + '/TIORI01L)'
123300130619     c                   Call      'QCMDEXC'                            99
123400130619     c                   Parm                    comman
123500130619     c                   Parm                    lenght
123600130619     c  n99              Open      Tiori01l
123700130619     c                   EndIf
123800130619
123900130619     c                   endsr
124000001019
124100000928** MSG
124200070418Filiale Errata o non Gestibile                                                      1
124300010301Tipo comunicazione orm errata                                                       2
124400061207Codice ordinante non valido                                                         3
124500061207Immettere l'ordinante se scelta di tutti i luoghi                                   4
124600001023..............  Libero                                                              5
124700001023..............  Libero                                                              6
124800001023..............  Libero                                                              7
124900001023..............  Libero                                                              8
125000001023..............  Libero                                                              9
125100001023..............  Libero                                                             10
125200001023..............  Libero                                                             11
125300001023..............  Libero                                                             12
125400001023..............  Libero                                                             13
125500001023..............  Libero                                                             14
125600001023..............  Libero                                                             15
125700001023..............  Libero                                                             16
125800001023..............  Libero                                                             17
125900001023..............  Libero                                                             18
126000001023..............  Libero                                                             19
126100001023..............  Libero                                                             20
126200001023..............  Libero                                                             21
126300001023..............  Libero                                                             22
126400001023..............  Libero                                                             23
126500001023..............  Libero                                                             24
126600001023..............  Libero                                                             25
126700001023..............  Libero                                                             26
126800001023..............  Libero                                                             27
126900001010..............  Libero                                                             28
127000001010..............  Libero                                                             29
127100001010..............  Libero                                                             30
127200001115** VOC  Lungh. 3                                                             *
127300001115COL                                                                                 1
127400001115PKG                                                                                 2
127500001115VLM                                                                                 3
127600001115BNC                                                                                 4
127700001115BLC                                                                                 5
127800001115AUT                                                                                 6
127900001115MOT                                                                                 7
128000040909** cmd
128100040909OVRDBF FILE(TIORI01L) TOFILE(
