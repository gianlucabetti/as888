000100070123     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500001027      *       GESTIONE PROPOSTE VARIAZIONE ORM                        *
000600000927      *                                                               *
000700000927      *****************************************************************
000800000927
000900001019     FAZORG01L  IF   E           K DISK
001000001103     FFNORP02L  UF   E           K DISK
001100131029     fFNORPE1L  if   e           k disk
001200140303     fFNORPT1L  if   e           k disk
001300001102     FFNORM01L  IF   E           K DISK
001400081030     ffnorf01l  if   e           k disk
001500131029     fFNORE01L  if   e           k disk
001600010119     FTABEL00F  IF   E           K DISK
001700120612     Ffidst01l  IF   E           K DISK
001800020702     ffnacr01l  if   e           k disk
001900071108     ffnorg01l  if   e           k disk
002000071109     FFIAPD01L  IF   E           K DISK
002100001030     FFIOR20D   CF   E             WORKSTN SFILE(FIORSF1:NN1)
002200000927
002300000928     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             MSG VIDEO
002400001025     D L6              S              3  0 DIM(30)                              P.O. Gestiti £6
002500130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
002600030307     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
002700030624      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
002800030624     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
002900030624     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
003000131029     d tf09            s              1    dim(25) ctdata perrcd(25)
003100131029     d tf11            s              1    dim(25) ctdata perrcd(25)
003200131029     d tf16            s              1    dim(25) ctdata perrcd(25)
003300131029     d $tf             s              1    dim(25)
003400010119
003500010119     D kcod            S                   LIKE(TBLcod)
003600010119     D kkey            S                   LIKE(TBLkey)
003700010119     D kdes            S             30
003800020524
003900120612     D knpg            S                   LIKE(dstnpg)
004000020524     D knfv            S                   LIKE(dstnfv)
004100020524     D kfgs            S                   LIKE(dstfgs)
004200020524
004300020524     d sav_lpksc       s                   like(orpksc)
004400020524
004500020524     d conta           s              2  0
004600020524     d okbolla         s              1
004700020524     d wbolla          s              1    inz('0')
004800081125     d wnobolla        s              1    inz('0')
004900031204     d $Fior05         s              1a   inz(*off)
005000020524     D W0140           S             14  0
005100020524     D Wdtgio          S              8  0
005200020524     D dateu           S              8  0
005300070920     d wpkb            s             10  1
005400071109     D w_pdr           S                   LIKE(§lppdr)
005500071109     D w_fpp           S                   LIKE(§lpfpp)
005600071109     D kpdpdr          S                   LIKE(APDpdr)
005700071109     D kpdtip          S                   LIKE(APDtip) inz('A')
005800130619
005900130619     d wabi            s                   like(UTEaut)
006000130619     d wEsci           s               n   inz(*off)
006100130619     d wErrGrave       s               n   inz(*off)
006200130619     d w003a           s              3
006300020524
006400020524     D lung            S             15  5
006500020524     D comman          S            110
006600131029
006700131029     d rigatf1         s              1    dim(78)
006800131029     d rigatf2         s              1    dim(62)
006900131029     d rigatf3         s              1    dim(62)
007000131029     d $loop           s              1  0
007100131029     d $rig            s              1  0
007200131029     d $h              s              3  0
007300131029     d $j              s              3  0
007400131029     d $k              s              3  0
007500131029     d $x              s              3  0
007600131029     d $y              s              3  0
007700131029     d $z              s              3  0
007800131029
007900131216     d wReadxOrm       s               n
008000131029     d wEndF04         s               n
008100131029     d wEndF05         s               n
008200131029
008300131029     d oraamda         s                   like(§OREoramda)
008400131029     d oraama          s                   like(§OREorama)
008500131029     d orapmda         s                   like(§OREorapda)
008600131029     d orapma          s                   like(§OREorapa)
008700131029     d mail            s                   like(W03mail)
008800131029     d sms             s                   like(W03sms)
008900161020     d wdpm            s                   like(VI4dpm)
009000020524
009100020524     D                 DS
009200020524     d  orpcra1                1      3  0
009300020524     d  orpcra2                4      7  0
009400020524     d  orpcra3                8     10  0
009500020524     d  ds_orpcra              1     10  0
009600020524
009700020524     D                 DS
009800020524     d  orpcor1                1      3  0
009900020524     d  orpcor2                4      7  0
010000020524     d  orpcor3                8     10  0
010100020524     d  ds_orpcor              1     10  0
010200020524
010300020524     D                 DS
010400020524     d  orpcrc1                1      3  0
010500020524     d  orpcrc2                4      7  0
010600020524     d  orpcrc3                8     10  0
010700020524     d  ds_orpcrc              1     10  0
010800020524
010900020524     D                 DS
011000020524     D  ds_poe                 1      3  0
011100020524     D  ds_nsr                 4      5  0
011200020524     D  ds_nor                 6     12  0
011300020524     D  ds_nrv                13     14  0
011400020524     D  ds_keyorm              1     14  0
011500010119
011600000927     D KPJBA         E DS
011700140303     D §kpjbu          s                   like(KPJBU)
011800020524
011900010301     D DCMR          E DS
012000031204     d DDFT          e ds                  inz
012100010314     D DFAR          E DS
012200001024     D FIOR05DS      E DS
012300071119     d fior20ds      e ds
012400001102     D TIBS02DS      E DS
012500020524     D DTASV         E DS
012600020524     D DBLP          E DS
012700030624     D TRUL90DS      E DS
012800090202     d ds5P          e ds                  inz
012900090202     d   §5Ppgm      e                     inz('FIOR18R   ')
013000030610     d Dorm01        e ds
013100031204      *
013200031204     d FIOR16DS      e ds                  inz
013300071108     d FIDG09DS      e ds                  inz
013400071108     d og143         e ds
013500070920
013600070920     d TIBS34ds      e ds
013700070920     d AZUTEds       e ds                  extname(AZUTE00F)
013800070920     d DDatiUte      e ds
013900130619     d TRUL31DS      e ds
014000130619
014100130619     d dLAT          e ds
014200130619     d dUTE01        e ds
014300001102
014400000927     D DSUL06        E DS                  EXTNAME(TRUL06DS)
014500000927     D  LIN                    1     90  0
014600000927     D                                     DIM(30)
014700131029
014800131029      // ds per rcd 'O' FNORE - orari apertura
014900131029     d dOREorari     e ds
015000161108
015100161108      // ds per rcd 'DT' FNORE - Data Pronta Merce
015200161108     d dOREdt        e ds
015300140214
015400140303
015500140303      // ds per note AUT
015600140303     d FIORT1DS      e ds
015700001023
015800001023      * Parametri per controllo Data
015900001023
016000001023     D WLBDAT          DS                  INZ
016100001023     D  G02DAT                 1      8  0
016200001023     D  G02INV                 9     16  0
016300001023     D  G02ERR                17     17
016400001023     D  G02TGI                18     22  0
016500010205
016600010205     D Parm01          DS
016700010205     D  ppoe                          3  0
016800010205     D  pnor                          7  0
016900010205     D  pnsr                          2  0
017000010205     D  pnrv                          2  0
017100010205     D  psce                          1
017200010205     D  pfgs                          3  0
017300010205     D  ppor                          3  0
017400010205     D  pdtr                          8  0
017500010205     D  pmdb                         10
017600010205     D  pprb                         10
017700010205     D  pdts                          8  0
017800010205     D  prmp                          1
017900010205     D  pbrc                          1
018000010205     D  pref                          2
018100010205     D  pmio                          1
018200061204     d  pndc                          6  0
018300061204     d  pddc                          8  0
018400120612     d  pdstfgs                       3  0
018500131029
018600131029      *   C O S T A N T I
018700131029     d cf2413          c                   const('F24=AlTasti(1/3)')
018800131029     d cf2423          c                   const('F24=AlTasti(2/3)')
018900131029     d cf2433          c                   const('F24=AlTasti(3/3)')
019000131029     d cf2412          c                   const('F24=AlTasti(1/2)')
019100131029     d cf2422          c                   const('F24=AlTasti(2/2)')
019200131029
019300131029      // Dspatr "REVERSE IMAGE"
019400131029     d high            c                   const(X'22')
019500131029     d highrimm        c                   const(X'23')
019600140214
019700140214      //---------------------------------------------------------------
019800140214      //?Definizione prototipi utilizzati.
019900140214      //---------------------------------------------------------------
020000140214      /copy gaitrasrc/srcprotopr,tibs02r
020100161020      /copy gaitrasrc/srcprotopr,XSRDA8
020200140303
020300140303      // - Gestione note AUT
020400140303     d fiort1r         pr                  extpgm('FIORT1R')
020500140303     d  kpjba                              likeds(KPJBA)
020600140303     d  fiort1ds                           likeds(FIORT1DS)
020700131029
020800131029      //---------------------------------------------------------------
020900131029      //?Definizione key-list.
021000131029      //---------------------------------------------------------------
021100131029
021200131029       // - File FNORPE1L
021300131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
021400131029     d                                     prefix(k_)
021500131029     d                                     inz
021600131029
021700131029       // - File FNORE01L
021800131029     d k05fnore      e ds                  extname(FNORE01L:*key)
021900131029     d                                     prefix(k_)
022000131029     d                                     inz
022100000927
022200000927      *---------------------------------------------------------------*
022300000927      *                                                               *
022400000927      *    UTILIZZO DEGLI INDICATORI                                  *
022500000927      *                                                               *
022600000927      *---------------------------------------------------------------*
022700000927      *                                                               *
022800001027      *    01 - Posizione Cursore su P.O. Ritiro        FIOR201       *
022900001030      *    02 - Posizione Cursore su P.O. Emissione     FIOR201       *
023000001027      *    03 - Disabilito F18                          FIOR201       *
023100131216      *    04 - HI F4-Alert                             FIOR204       *
023200001030      *    05 - Posizione Cursore su Numero             FIOR201       *
023300131216      *    06 - HI F5-Altri dati                        FIOR204       *
023400001027      *    07 - Posizione Cursore su Anno               FIOR201       *
023500140303      *    08 - RI F17-Note AUT                                       *
023600001027      *    09 - Posizione Cursore su Data Variaz. dal   FIOR201       *
023700001027      *    10 - Modificato almeno un Ordine             FIOR201       *
023800001027      *    11 - Rifiutata  almeno una Proposta          FIOR201       *
023900001027      *    12 - Modificato almeno un Ordine             FIORRC1       *
024000001027      *    13 - Rifiutata  almeno una Proposta          FIORRC1       *
024100001027      *    14 - Posizione Cursore su Data Variaz. al    FIOR201       *
024200010209      *    15 - Proposta di chiusura                                  *
024300140303      *    16 - Visualizza 'STAMPATO'
024400140303      *    17 - Visualizza 'CON RICEVUTA DI RITIRO'
024500001027      *    18 - Sproteggo P.O. Gestione                 FIOR201       *
024600140214      *    23 - Abilito F4-Alert                                      *
024700140303      *    24 - Abilito F17-Note AUT                                  *
024800131216      *    28 - Visualizzo messaggio Errore                           *
024900131216      *    33 - Visualizzo messaggio Errore                           *
025000001106      *    34 - R.IMMAGE (AG) Opzione                   FIORSF1       *
025100001106      *    35 - Non ho Scelto la prima Proposta         FIORSF1       *
025200010314      *    36 - Ordine non manutenzionabile             FIORSF1       *
025300070124      *    37 - Ordine in fase 420 non manut.           FIORSF1       *
025400001106      *    39 - Comodo                                                *
025500161020      *    40 - R.IMMAGE Data Pronta Merce              FIOR204       *
025600001030      *    41 - ND Codice Ritiro                        FIORSF1       *
025700001031      *    42 - Visualizzo ORM                          FIOR204       *
025800001031      *    43 - R.IMMAGE (AG) Fase                      FIOR204       *
025900001031      *    44 - R.IMMAGE (AG) Cliente Ritiro            FIOR204       *
026000001031      *    45 - R.IMMAGE (AG) Rag.Soc. Cli.Ritiro       FIOR204       *
026100001031      *    46 - R.IMMAGE (AG) Indirizzo Cli.Ritiro      FIOR204       *
026200001031      *    47 - R.IMMAGE (AG) CAP Cli.Ritiro            FIOR204       *
026300001031      *    48 - R.IMMAGE (AG) Località Cli.Ritiro       FIOR204       *
026400001031      *    49 - R.IMMAGE (AG) Provincia Cli.Ritiro      FIOR204       *
026500001031      *    50 - R.IMMAGE (AG) Nazione Cli.Ritiro        FIOR204       *
026600001031      *    51 - R.IMMAGE (AG) Cliente Ordinante         FIOR204       *
026700001031      *    52 - R.IMMAGE (AG) Rag.Soc. Cli.Ordin.       FIOR204       *
026800001031      *    53 - R.IMMAGE (AG) Indirizzo Cli.Ordin.      FIOR204       *
026900001031      *    54 - R.IMMAGE (AG) CAP Cli.Ordin.            FIOR204       *
027000001031      *    55 - R.IMMAGE (AG) Località Cli.Ordin.       FIOR204       *
027100001031      *    56 - R.IMMAGE (AG) Provincia Cli.Ordin.      FIOR204       *
027200001031      *    57 - R.IMMAGE (AG) Nazione Cli.Ordin.        FIOR204       *
027300001031      *    58 - Visualizzo Nr. Ordine Fisso             FIOR204       *
027400001103      *    59 - R.IMMAGE (AG) Colli                     FIOR204       *
027500001031      *    60 - R.IMMAGE (AG) Peso                      FIOR204       *
027600001031      *    61 - R.IMMAGE (AG) Volume                    FIOR204       *
027700001031      *    62 - R.IMMAGE (AG) Bancali                   FIOR204       *
027800001031      *    63 - R.IMMAGE (AG) Bilico                    FIOR204       *
027900001031      *    64 - R.IMMAGE (AG) Autotreno                 FIOR204       *
028000001031      *    65 - R.IMMAGE (AG) Motrice                   FIOR204       *
028100001031      *    66 - R.IMMAGE (AG) Data Ritiro               FIOR204       *
028200001031      *    68 - R.IMMAGE (AG) Ora Ritiro                FIOR204       *
028300131216      *    69 - r.immage  Orari apertura                FIOR204       *
028400131216      *    70 - r.immage  Orari apertura                FIOR204       *
028500131216      *    71 - r.immage  Orari apertura                FIOR204       *
028600001031      *    72 - R.IMMAGE (AG) Referente                 FIOR204       *
028700001031      *    73 - R.IMMAGE (AG) Telefono                  FIOR204       *
028800001031      *    74 - R.IMMAGE (AG) Natura Merce              FIOR204       *
028900001031      *    75 - R.IMMAGE (AG) Riferimento               FIOR204       *
029000001031      *    76 - R.IMMAGE (AG) Tipo Ordine               FIOR204       *
029100001031      *    77 - R.IMMAGE (AG) Pagamento                 FIOR204       *
029200001031      *    78 - R.IMMAGE (AG) Cliente che Paga          FIOR204       *
029300001031      *    79 - R.IMMAGE (AG) Codice Tariffa            FIOR204       *
029400001031      *    80 - R.IMMAGE (AG) DDT                       FIOR204       *
029500001031      *    81 - R.IMMAGE (AG) P.O. Consegna             FIOR204       *
029600001031      *    82 - R.IMMAGE (AG) Fermo Deposito            FIOR204       *
029700001031      *    83 - R.IMMAGE (AG) Nota 1                    FIOR204       *
029800001031      *    84 - R.IMMAGE (AG) Nota 2                    FIOR204       *
029900001031      *    85 - SFL CLR                                               *
030000001031      *    86 - SFL END                                               *
030100001103      *    88 - R.IMMAGE (AG) Stato                     FIOR204       *
030200010119      *    89 - R.IMMAGE (AG) Sponda idraulica          FIOR204       *
030300001031      *    90 - Emetto Videate                                        *
030400001031      *    92 - R.IMMAGE (AG) Consegna                  FIOR204       *
030500001031      *    93 - R.IMMAGE (AG) Rag.Soc. Consegna         FIOR204       *
030600001031      *    94 - R.IMMAGE (AG) Indirizzo Consegna        FIOR204       *
030700001031      *    95 - R.IMMAGE (AG) CAP Consegna              FIOR204       *
030800001031      *    96 - R.IMMAGE (AG) Località Consegna         FIOR204       *
030900001031      *    97 - R.IMMAGE (AG) Provincia Consegna        FIOR204       *
031000001031      *    98 - R.IMMAGE (AG) Nazione Consegna          FIOR204       *
031100131216      *    99 - r.immage  Orari apertura                FIOR204       *
031200000927      *                                                               *
031300001030      * NB. ... La dicitura "NU" a fianco dell'indicatore vuol dire   *
031400001030      *         che questo è stato definito ma Non è Utilizzato       *
031500001030      *         (previsto per usi futuri).     =     =                *
031600001030      *                                                               *
031700000927      *---------------------------------------------------------------*
031800000927
031900000927      *****************************************************************
032000000927      *                                                               *
032100000927      *    OPERAZIONI INIZIALI                                        *
032200000927      *                                                               *
032300000927      *****************************************************************
032400070920
032500001024
032600001024      *    Inizializzo Campi Video
032700001024
032800000927     C                   EXSR      SR_INIVIDEO
032900000927
033000000927      *****************************************************************
033100000927      *                                                               *
033200000927      *    ROUTINE PRINCIPALE                                         *
033300000927      *                                                               *
033400000927      *****************************************************************
033500001011
033600001019     C                   do        *hival
033700001019
033800001019      *    Emissione Prima Videata
033900001019
034000071119     c                   if        %parms > 1
034100071119     c   kl              leave
034200071119     c                   else
034300130620
034400130620      /free
034500130620       //?Se utente non autorizzato alla funzione esco
034600130620         IF  wErrGrave;
034700130620           *in28 = *on;
034800130620           wEsci = *on;
034900130620           VIDmsg = 'Utente non autorizzato alla funzione';
035000130620         ENDIF;
035100130620      /end-free
035200130620
035300001030     C                   EXFMT     FIOR201
035400071119     c                   endif
035500130620
035600130620      /free
035700130620         IF  wEsci;
035800130620           leave;
035900130620         ENDIF;
036000130620      /end-free
036100130620
036200001019     C   KC              leave
036300001030     C                   setoff                                       281890
036400001030     C                   setoff                                       010204
036500001030     C                   setoff                                       050607
036600001030     C                   setoff                                       080914
036700071126     c                   clear                   vidmsg
036800001019
036900001019      *    Controllo
037000001019
037100001019     C                   EXSR      SR_CONTROL
037200001019
037300130412      *    Per Errori/Cambio P.O. - Riemetto
037400001019
037500001019     C     *in28         ifeq      *on
037600001019     C     *in18         oreq      *on
037700001030     C     *in90         oreq      *on
037800001019     C                   iter
037900001019     C                   endif
038000001023
038100001030      *   GESTIONE DELLE PROPOSTE
038200001023
038300001023     C                   EXSR      SR_GESTIONE
038400001023
038500001019     C                   enddo
038600001019
038700000927     C                   eval      *inlr = *on
038800000927
038900000927      *****************************************************************
039000000927      *                                                               *
039100000927      *    SUBROUTINES                                                *
039200000927      *                                                               *
039300000927      *****************************************************************
039400070920
039500070920      *---------------------------------------------------------------*
039600070920      * Reperimento Dati del job (Utente/Operativi)                   *
039700070920      *---------------------------------------------------------------*
039800070920     c     DatiJob       BEGSR
039900070920      *
040000070920     c     *dtaara       define    §azute        azuteds
040100070920     c     *dtaara       define    §datiute      ddatiute
040200070920      *
040300070920     c                   in(E)     *dtaara
040400070920     c                   IF        %ERROR or RSUT = *blanks
040500070920     c                   clear                   Tibs34Ds
040600070920     c                   call      'TIBS34R'
040700070920     c                   parm                    Tibs34Ds
040800070920     c                   in        *dtaara
040900070920     c                   ENDIF
041000070920      *
041100070920     c                   ENDSR
041200000927
041300000927      *---------------------------------------------------------------*
041400000927      *    Inizializzazione Campi Video                               *
041500000927      *---------------------------------------------------------------*
041600000927
041700000927     C     SR_INIVIDEO   begsr
041800000927
041900001030      *    Reset Progressivo Nr. Proposte Confermate/Rifiutate
042000001025
042100001026     C                   setoff                                       10
042200001030     C                   setoff                                       11
042300001025     C                   clear                   vconfe
042400001030     C                   clear                   vrifiu
042500001002
042600001026      *    Inizializzo il P.O. Ritiro
042700071119     c                   if        %parms > 1
042800071119     c                   eval      vidpor = i20fgs
042900071119     c                   else
043000070920     c                   eval      vidpor = dutpou
043100071119     c                   endif
043200001012
043300001026      *    Decodifico il P.O. Ritiro
043400000927
043500001026     C     vidpor        chain     azorg01l
043600000927     C                   if        %found(azorg01l)
043700000927     C                   movel     orgdes        viddep
043800000927     C                   else
043900001019     C                   clear                   viddep
044000000927     C                   endif
044100031204
044200031204      * Recupero i dati di default prima con il mio p.o.
044300031204     C                   clear                   TIBS02DS
044400031204     C                   clear                   dDFT
044500031204     C                   movel     'C'           T02mod
044600031204     C                   movel     KNSIF         T02sif
044700031204     C                   movel     'DFT'         t02cod
044800031204     C                   movel(p)  VIDpor        T02ke1
044900031204     C                   call      'TIBS02R'
045000031204     C                   parm                    KPJBA
045100031204     C                   parm                    TIBS02DS
045200031204if  1C                   if        T02err =  *blanks
045300031204     C                   movel     T02uni        dDFT
045400031204x   1C                   else
045500031204      * poi con 999 generico se non trovo con il mio p.o.
045600031204     C                   clear                   TIBS02DS
045700031204     C                   movel     'C'           T02mod
045800031204     C                   movel     KNSIF         T02sif
045900031204     C                   movel     'DFT'         T02cod
046000031204     C                   movel(p)  '999'         T02ke1
046100031204     C                   call      'TIBS02R'
046200031204     C                   parm                    KPJBA
046300031204     C                   parm                    TIBS02DS
046400031204if  2C                   if        T02err =  *blanks
046500031204     C                   movel     T02uni        dDFT
046600031204e   2C                   endif
046700031204e   1C                   endif
046800000927
046900000927      *    Inizializzo i Campi Restanti
047000001012
047100001030     C                   clear                   vidpoe
047200001030     C                   clear                   vidnsr
047300001030     C                   clear                   vidnor
047400001030     C                   clear                   vidnrv
047500001030     C                   clear                   viddv1
047600071119     c                   if        %parms > 1
047700071119     c                   eval      viddv2 = i20data
047800071119     c                   else
047900001030     C                   move      *date         viddv2
048000071119     c                   endif
048100000927
048200000927     C                   endsr
048300001019
048400001019      *---------------------------------------------------------------*
048500001019      *    Controllo Prima Videata                                    *
048600001019      *---------------------------------------------------------------*
048700001019
048800001019     C     SR_CONTROL    begsr
048900001019
049000001025     C                   z-add     0             vconfn
049100001030     C                   z-add     0             vrifin
049200001023
049300001026      *    F18=Cambio P.O. Ritiro
049400001019
049500001019     C     *inks         ifeq      *on
049600001019     C                   seton                                        18
049700001019     C                   goto      endctr
049800001019     C                   endif
049900001023
050000001026      *    Controllo P.O. Ritiro
050100001023
050200001023     C                   clear                   viddep
050300130620     c                   move      VIDpor        w003a
050400130620     c     w003a         lookup    SKpog                                  39
050500001023     C     *in39         ifeq      *off
050600001023     C                   eval      *in01 = *on
050700001023     C                   eval      *in28 = *on
050800001023     C                   eval      *in18 = *on
050900001023     C                   movel     msg(1)        vidmsg
051000001023     C                   goto      endctr
051100001023     C                   else
051200001026     C     vidpor        chain     azorg01l
051300001023     C                   if        %found(azorg01l)
051400001023     C                   movel     orgdes        viddep
051500001023     C                   endif
051600001025      *        Carico £6
051700001025     C                   CLEAR                   DSUL06
051800001025     C                   MOVE      '£6'          D06COD
051900001026     C                   MOVEL     vidpor        D06KEY
052000001025     C                   MOVEL     DSUL06        KPJBU
052100001025     C                   CALL      'TRUL06R'
052200001025     C                   PARM                    KPJBA
052300001025     C                   MOVEL     KPJBU         DSUL06
052400001025     C                   MOVEA     LIN           L6
052500070920
052600070920      * Recupero i dati di default prima con il mio p.o.
052700070920     C                   clear                   TIBS02DS
052800070920     C                   clear                   dDFT
052900070920     C                   movel     'C'           T02mod
053000070920     C                   movel     KNSIF         T02sif
053100070920     C                   movel     'DFT'         t02cod
053200070920     C                   movel(p)  VIDpor        T02ke1
053300070920     C                   call      'TIBS02R'
053400070920     C                   parm                    KPJBA
053500070920     C                   parm                    TIBS02DS
053600070920if  1C                   if        T02err =  *blanks
053700070920     C                   movel     T02uni        dDFT
053800070920x   1C                   else
053900070920      * poi con 999 generico se non trovo con il mio p.o.
054000070920     C                   clear                   TIBS02DS
054100070920     C                   movel     'C'           T02mod
054200070920     C                   movel     KNSIF         T02sif
054300070920     C                   movel     'DFT'         T02cod
054400070920     C                   movel(p)  '999'         T02ke1
054500070920     C                   call      'TIBS02R'
054600070920     C                   parm                    KPJBA
054700070920     C                   parm                    TIBS02DS
054800070920if  2C                   if        T02err =  *blanks
054900070920     C                   movel     T02uni        dDFT
055000070920e   2C                   endif
055100070920e   1C                   endif
055200001023     C                   endif
055300001023
055400001030      *    Se immesso P.O. immettere Numero
055500001023
055600001030     C     vidpoe        ifne      0
055700001030     C     vidnor        andeq     0
055800001030     C                   seton                                        05  28
055900001030     C                   movel     msg(7)        vidmsg
056000001030     C                   goto      endctr
056100001030     C                   endif
056200001030
056300001030      *    Se immesso Numero immettere P.O.
056400001030
056500001030     C     vidpoe        ifeq      0
056600001030     C     vidnor        andne     0
056700001030     C                   seton                                        02  28
056800001030     C                   movel     msg(7)        vidmsg
056900001030     C                   goto      endctr
057000001030     C                   endif
057100001030
057200001030      *    Se immesso Viaggio o Serie Immettere P.O. e Numero
057300001030
057400001030     C     vidnrv        ifne      0
057500001030     C     vidnsr        orne      0
057600001030     C     vidpoe        ifeq      0
057700001030     C                   seton                                        02  28
057800001030     C                   movel     msg(7)        vidmsg
057900001030     C                   goto      endctr
058000001030     C                   endif
058100001030     C     vidnor        ifeq      0
058200001030     C                   seton                                        05  28
058300001030     C                   movel     msg(7)        vidmsg
058400001030     C                   goto      endctr
058500001030     C                   endif
058600001030     C                   endif
058700001030
058800001114      *    Accesso diretto per Numero ORM
058900001030
059000001030     C     vidpoe        ifne      0
059100001030     C     vidnsr        orne      0
059200001030     C     vidnor        orne      0
059300001030     C     vidnrv        orne      0
059400131216     c                   eval      wReadxOrm = *on
059500001120     C                   else
059600131216     c                   eval      wReadxOrm = *off
059700001114     C                   endif
059800001030
059900001030      *    Controllo / Inversione Date Variazione
060000001030
060100001030     C                   clear                   wdatainv1
060200001030     C     viddv1        ifne      0
060300001030     C                   z-add     viddv1        G02DAT
060400001030     C                   clear                   G02ERR
060500001030     C                   call      'XSRDA8'
060600001030     C                   parm                    WLBDAT
060700001030     C     g02err        ifeq      '1'
060800001030     C                   seton                                        09  28
060900001030     C                   movel     msg(4)        vidmsg
061000001030     C                   goto      endctr
061100001030     C                   else
061200001030     C                   z-add     g02inv        wdatainv1         8 0
061300001030     C                   z-add     g02dat        viddv1
061400001030     C                   endif
061500001030     C                   endif
061600001030
061700001030     C                   clear                   wdatainv2
061800001030     C     viddv2        ifne      0
061900001030     C                   z-add     viddv2        G02DAT
062000001030     C                   clear                   G02ERR
062100001030     C                   call      'XSRDA8'
062200001030     C                   parm                    WLBDAT
062300001030     C     g02err        ifeq      '1'
062400001030     C                   seton                                        14  28
062500001030     C                   movel     msg(4)        vidmsg
062600001030     C                   goto      endctr
062700001030     C                   else
062800001030     C                   z-add     g02inv        wdatainv2         8 0
062900001030     C                   z-add     g02dat        viddv2
063000001030     C                   endif
063100001030     C                   endif
063200001023
063300001030     C     wdatainv1     ifgt      wdatainv2
063400001030     C                   seton                                        14  28
063500001030     C                   movel     msg(5)        vidmsg
063600001030     C                   goto      endctr
063700001030     C                   endif
063800001030
063900001030     C     wdatainv2     ifeq      0
064000001030     C                   seton                                        14  28
064100001030     C                   movel     msg(6)        vidmsg
064200001030     C                   goto      endctr
064300001030     C                   endif
064400001030
064500001019     C     ENDCTR        endsr
064600001023
064700001023      *---------------------------------------------------------------*
064800001023      *    Lettura / Gestione degli ORM                               *
064900001023      *---------------------------------------------------------------*
065000001023
065100001023     C     SR_GESTIONE   begsr
065200001023
065300001030      *    Pulisco Subfile e mi posiziono per Lettura Proposte
065400001024
065500031204do  1C                   do        *hival
065600001023     C                   z-add     0             nn1               4 0
065700001023     C                   eval      *in85 = *on
065800001023     C                   write     fiorrc1
065900001023     C                   eval      *in85 = *off
066000001023     C                   z-add     1             recsf
066100001106     C                   setoff                                       33  34
066200001030
066300001030      *    Carico Proposte L6
066400001030
066500031204do  2C     1             do        30            zz                3 0
066600001030
066700001030     C     L6(zz)        ifeq      0
066800001030     C                   iter
066900001030     C                   endif
067000001030
067100001030     C                   z-add     L6(zz)        vs1por
067200001025
067300131216     c                   IF        wReadxOrm
067400131216     C     KFNORP        setll     fnorp02l
067500131216     c                   else
067600131216     C     vs1por        setll     fnorp02l
067700131216     c                   ENDIF
067800001023
067900001030      *    Leggo Proposte
068000001023
068100031204do  3C                   do        *hival
068200131216     c                   IF        wReadxOrm
068300131216     C     KFNORP        reade(n)  fnorp02l
068400131216     c                   else
068500131216     C     vs1por        reade(n)  fnorp02l
068600131216     c                   ENDIF
068700001023
068800001030     C                   if        %eof(fnorp02l)
068900001023     C                   leave
069000001023     C                   endif
069100001023
069200001026     C     nn1           ifge      8000
069300001024     C                   leave
069400001024     C                   endif
069500001024
069600001030      *    Controllo parametri prima videata
069700130412      *    Date
069800001030
069900001030     C     wdatainv1     ifne      0
070000001030     C     wdatainv1     andgt     orpdtv
070100001030     C                   iter
070200001030     C                   endif
070300001030     C     wdatainv2     iflt      orpdtv
070400001030     C                   iter
070500001030     C                   endif
070600001030
070700001026      *    SCRIVO SUBFILE
070800001024
070900001026     C                   clear                   vs1opz
071000001030     C                   move      orppoe        vs1poe
071100001030     C                   move      orpnsr        vs1nsr
071200001030     C                   move      orpnor        vs1nor
071300001030     C                   move      orpnrv        vs1nrv
071400001030     C                   move      orpcra        vs1cro
071500001030     C     vs1cro        ifeq      0
071600001030     C                   seton                                        41
071700001030     C                   else
071800001030     C                   setoff                                       41
071900001030     C                   endif
072000001030     C                   movel     orprsr        vs1rsr
072100001030     C                   movel     orpinr        vs1inr
072200001030     C                   movel     orplor        vs1lor
072300001030     C                   movel     orpprr        vs1prr
072400001030     C                   movel     orpnar        vs1nar
072500001030     C                   movel     orputv        vs1ute
072600001030     C                   move      orporv        vs1orv
072700001102     C                   move      orpdtv        vs1dtvw
072800001102     C                   movel     orpdtv        wa4               4
072900001030     C                   move      wa4           vs1dtv
073000001030     C                   move      orpdtv        wa4
073100001030     C                   move      wa4           wg2               2
073200001030     C                   movel     wa4           wm2               2
073300001030     C                   move      wm2           wa4
073400001030     C                   movel     wg2           wa4
073500001030     C                   movel     wa4           vs1dtv
073600001024
073700001024     C                   add       1             nn1
073800001024     C                   write     fiorsf1
073900001024
074000031204e   3C                   enddo
074100031204e   2C                   enddo
074200001023
074300001024      *    EMISSIONE RECORD DI CONTROLLO
074400001024
074500001025     C     vconfn        ifeq      0
074600001025     C                   eval      *in12 = *off
074700001025     C                   else
074800001025     C                   eval      *in12 = *on
074900001025     C                   endif
075000001030     C     vrifin        ifeq      0
075100001030     C                   eval      *in13 = *off
075200001030     C                   else
075300001030     C                   eval      *in13 = *on
075400001030     C                   endif
075500001025
075600001024     C     nn1           ifeq      0
075700001030     C                   exfmt     fior203
075800001024     C                   else
075900001030     C                   write     fior202
076000001024     C                   exfmt     fiorrc1
076100001024     C                   endif
076200071126
076300071126     c                   eval      *in28 = *off
076400071126     c                   clear                   vidmsg
076500001024
076600001025     C                   z-add     0             vconfn
076700001030     C                   z-add     0             vrifin
076800001025
076900001024      *    LETTURA RECORDS SUBFILE
077000001024
077100031204if  2C     *inkl         ifeq      *off
077200001024
077300031204do  3C                   do        *hival
077400001024
077500001030     C     nn1           ifne      0
077600001024     C                   readc     fiorsf1
077700001024     C                   if        %eof
077800001024     C                   leave
077900001024     C                   endif
078000001030     C                   else
078100001030     C                   leave
078200001030     C                   endif
078300001106
078400010314      *    Controllo Se Ordine è Manutenzionabile
078500001103
078600070124     C                   setoff                                       353637
078700081125     c                   eval      wnobolla = *off
078800001106     C     KFNORM        chain     fnorm01l
078900001106     C                   if        %found(fnorm01l)
079000010314     C                   exsr      Sr_Fase
079100010314     C     d§farman      ifne      'S'
079200001106     C                   seton                                        35  36
079300001106     C                   endif
079400070123      * Non posso accettare una variazione se ORM in fase 420
079500070124     c                   if        ormfao = 420 and *in35 = *off
079600070124     c                   seton                                        35  37
079700070123     c                   endif
079800081030     c                   eval      dorm01 = ormflo
079900001106     C                   endif
080000081030      * controllo se posso utilizzare la causale inserita per la
080100081030      * chiusura dell'ORM
080200081030if  8c                   if        orpfao = 999  or OrpFao = 910
080300081030     c                             and not *in35
080400081030     c                   exsr      sr_ctrchiu
080500081030     c                   endif
080600001106
080700001106      *    Devo Scegliere la PRIMA Proposta
080800001106
080900001106     C     *in35         ifeq      *off
081000001106     C     KFNORPA       setll     fnorp02l
081100001106     C                   eval      *in35 = *on
081200001106     C     KFNORPC       readpe(n) fnorp02l
081300001106     C                   if        %eof(fnorp02l)
081400001106     C                   eval      *in35 = *off
081500001106     C                   endif
081600001106     C                   endif
081700001106
081800001103      *    Visualizzazione Proposta
081900001103      *    ------------------------
082000001106
082100130412     c                   IF        vs1opz='1'
082200001102     C                   EXSR      SR_VISUALIZZA
082300001102     C                   endif
082400001106
082500031204e   3C                   enddo
082600001024
082700031204x   2C                   else
082800001024     C                   leave
082900031204e   2C                   endif
083000001024
083100031204e   1C                   enddo
083200031204
083300031204      * Chiudo i printer file eventualmente aperti per gli O.R.M.
083400031204if  1c                   if            $Fior05 = *on
083500031204     c                             and DateU  >= D§DFTdsl
083600031204     c                   clear                   FIOR16ds
083700031204     c                   eval      R16tla  = 'C'
083800031204     c                   movel(p)  FIOR16DS      KPJBU
083900090202     c                   call      §5Ppgm
084000031204     c                   parm                    KPJBA
084100031204e   1c                   endif
084200031204
084300001023     C                   endsr
084400010314
084500010314      *---------------------------------------------------------------*
084600010314      *    Flag fase                                                  *
084700010314      *---------------------------------------------------------------*
084800010314
084900010314     C     Sr_Fase       begsr
085000010314
085100010314     C                   clear                   TIBS02DS
085200010314     C                   movel     'C'           T02mod
085300010314     C                   movel     knsif         t02sif
085400010314     C                   movel     'FAR'         t02cod
085500010314     C                   movel(p)  ormfao        T02ke1
085600010314     C                   call      'TIBS02R'
085700010314     C                   parm                    KPJBA
085800010314     C                   parm                    TIBS02DS
085900010314     C                   movel     T02uni        dfar
086000010314
086100020524     C                   endsr
086200081030
086300081030      *---------------------------------------------------------------*
086400081030      * Controllo se posso chiudere l'ORM con la cau immessa          *
086500081030      *---------------------------------------------------------------*
086600081030     c     sr_ctrchiu    begsr
086700081030
086800081030      * controllo la causale immessa in proposta
086900081030     c                   clear                   dcmr
087000081030     c                   clear                   tibs02ds
087100081030     c                   eval      t02mod = 'C'
087200081030     c                   eval      t02sif = knsif
087300081030     c                   eval      t02cod = 'CMR'
087400081030     c                   eval      t02ke1= orpcau
087500081030     c                   call      'TIBS02R'
087600081030     c                   parm                    kpjba
087700081030     c                   parm                    tibs02ds
087800081030     c                   movel     t02uni        dcmr
087900081030
088000081125      * se causale inserita genera bolla di addebito (colpa cliente)
088100081030      * controllo che l'ORM non abbia già avuto una causale che genera addebito
088200081125      * in questo caso faccio solo un addebito
088300081125     c                   if        d§cmradd = 'S'
088400081030     c     kfnorm        setll     fnorf01l
088500081030     c                   do        *hival
088600131029     c     kfnorm        reade     fnorf01l
088700081030     c                   if        %eof (fnorf01l)
088800081030     c                   leave
088900081030     c                   endif
089000081030      * senza causale rileggo
089100081030     c                   if        orfcar = *blanks
089200081030     c                   iter
089300081030     c                   endif
089400081030      * fase da PDA rileggo
089500081030     c                   if        orffar = 410
089600081030     c                   iter
089700081030     c                   endif
089800081030     c                   clear                   dcmr
089900081030     c                   clear                   tibs02ds
090000081030     c                   eval      t02mod = 'C'
090100081030     c                   eval      t02sif = knsif
090200081030     c                   eval      t02cod = 'CMR'
090300081030     c                   eval      t02ke1 = orfcar
090400081030     c                   call      'TIBS02R'
090500081030     c                   parm                    kpjba
090600081030     c                   parm                    tibs02ds
090700081030     c                   eval      dcmr = t02uni
090800081030      * se causale che genera bolla esco ed emetto msg di errore
090900081030     c                   if        d§cmradd = 'S'
091000081125     c                   eval      wnobolla = *on
091100081030     c                   leavesr
091200081030     c                   endif
091300081030     c                   enddo
091400081030     c                   endif
091500081030
091600081030     c                   endsr
091700001103
091800020524      *---------------------------------------------------------------*
091900020524      * Controllo se devo generare la bolla di addebito
092000020524      *---------------------------------------------------------------*
092100020524     c     sr_contradd   begsr
092200020524
092300020524     c                   clear                   sav_lpksc
092400081125
092500081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
092600081125      * come se avessi fatto la bolla
092700081125     c                   if        wnobolla = *on
092800081125     c                   eval      wbolla = *on
092900081125     c                   leavesr
093000081125     c                   endif
093100020524
093200020524      * se cod.ordinante codificato addebito
093300020524 b1  c                   if        orpcor <> *zeros and orpcor2 <> 8888
093400020524     c                             and orpcor2 <> 9999
093500020524     c                   clear                   dblp
093600090505     c                   movel     orpcor        §lpksca
093700020702      * se c'è imposto il codice bollettazione
093800020702     c     orpcor        chain     fnacr01l
093900020702     c                   if        %found(fnacr01l) and acratb = *blanks
094000020702     c                             and acrksc <> *zeros
094100090505     c                   move      acrksc        §lpksca
094200090505     c                   if        acrccc<>999
094300090505     c                   move      acrccc        §lpctra
094400090505     C                   endif
094500020702     c                   endif
094600020524     c                   eval      §lprsd = orprso
094700020524     c                   eval      §lpind = orpino
094800020524     c                   eval      §lpcad = orpcao
094900020524     c                   eval      §lplod = orploo
095000020524     c                   eval      §lpprd = orppro
095100020524     c                   eval      §lpnzd = orpnao
095200020524     c                   exsr      sr_wrtbolla
095300020524 x1  c                   else
095400020524      * se non inserito l'ordinante
095500020524 b2  c                   if        orprso = *blanks
095600020524      * orm non commissionato
095700020524 b3  c                   if        orppoe = orppor
095800020524      * se cod.mittente codificato addebito
095900020524 b4  c                   if        orpcra <> *zeros and orpcra2 <> 8888
096000020524     c                             and orpcra2 <> 9999
096100020524     c                   clear                   dblp
096200090505     c                   movel     orpcra        §lpksca
096300020702      * se c'è imposto il codice bollettazione
096400020702     c     orpcra        chain     fnacr01l
096500020702     c                   if        %found(fnacr01l) and acratb = *blanks
096600020702     c                             and acrksc <> *zeros
096700090505     c                   move      acrksc        §lpksca
096800090505     c                   if        acrccc<>999
096900090505     c                   move      acrccc        §lpctra
097000020702     c                   endif
097100090505     c                   endif
097200020524     c                   eval      §lprsd = orprsr
097300020524     c                   eval      §lpind = orpinr
097400020524     c                   eval      §lpcad = orpcar
097500020524     c                   eval      §lplod = orplor
097600020524     c                   eval      §lpprd = orpprr
097700020524     c                   eval      §lpnzd = orpnar
097800020524     c                   exsr      sr_wrtbolla
097900020524      * se cod.mittente non codificato
098000020524 x4  c                   else
098100020524      * se cod.destinatario codificato addebito
098200020524 b5  c                   if        orpcrc <> *zeros and orpcrc2 <> 8888
098300020524     c                             and orpcrc2 <> 9999
098400020524     c                   clear                   dblp
098500090505     c                   movel     orpcrc        §lpksca
098600020702      * se c'è imposto il codice bollettazione
098700020702     c     orpcrc        chain     fnacr01l
098800020702     c                   if        %found(fnacr01l) and acratb = *blanks
098900020702     c                             and acrksc <> *zeros
099000090505     c                   move      acrksc        §lpksca
099100090505     c                   if        acrccc<>999
099200090505     c                   move      acrccc        §lpctra
099300020702     c                   endif
099400090505     c                   endif
099500020524     c                   eval      §lprsd = orprsc
099600020524     c                   eval      §lpind = orpinc
099700020524     c                   eval      §lpcad = orpcac
099800020524     c                   eval      §lplod = orploc
099900020524     c                   eval      §lpprd = orpprc
100000020524     c                   eval      §lpnzd = orpnac
100100020524     c                   exsr      sr_wrtbolla
100200020524 e5  c                   endif
100300020524 e4  c                   endif
100400020524      * orm commissionato
100500020524 x3  c                   else
100600020524      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
100700020524 b4  c                   if        orpcrc <> *zeros and orpcrc2 <> 8888
100800020524     c                             and orpcrc2 <> 9999  and orpcrc1 = orppoe
100900020524     c                   clear                   dblp
101000090505     c                   movel     orpcrc        §lpksca
101100020702      * se c'è imposto il codice bollettazione
101200020702     c     orpcrc        chain     fnacr01l
101300020702     c                   if        %found(fnacr01l) and acratb = *blanks
101400020702     c                             and acrksc <> *zeros
101500090505     c                   move      acrksc        §lpksca
101600090505     c                   if        acrccc<>999
101700090505     c                   move      acrccc        §lpctra
101800020702     c                   endif
101900090505     c                   endif
102000020524     c                   eval      §lprsd = orprsc
102100020524     c                   eval      §lpind = orpinc
102200020524     c                   eval      §lpcad = orpcac
102300020524     c                   eval      §lplod = orploc
102400020524     c                   eval      §lpprd = orpprc
102500020524     c                   eval      §lpnzd = orpnac
102600020524     c                   exsr      sr_wrtbolla
102700020524      * se non codificato o p.o. differenti
102800020524 x4  c                   else
102900020524      * se cod.mittente codificato addebito
103000020524 b5  c                   if        orpcra <> *zeros and orpcra2 <> 8888
103100020524     c                             and orpcra2 <> 9999
103200020524     c                   clear                   dblp
103300090505     c                   movel     orpcra        §lpksca
103400020702      * se c'è imposto il codice bollettazione
103500020702     c     orpcra        chain     fnacr01l
103600020702     c                   if        %found(fnacr01l) and acratb = *blanks
103700020702     c                             and acrksc <> *zeros
103800090505     c                   move      acrksc        §lpksca
103900090505     c                   if        acrccc<>999
104000090505     c                   move      acrccc        §lpctra
104100020702     c                   endif
104200090505     c                   endif
104300020524     c                   eval      §lprsd = orprsr
104400020524     c                   eval      §lpind = orpinr
104500020524     c                   eval      §lpcad = orpcar
104600020524     c                   eval      §lplod = orplor
104700020524     c                   eval      §lpprd = orpprr
104800020524     c                   eval      §lpnzd = orpnar
104900020524     c                   exsr      sr_wrtbolla
105000020524 e5  c                   endif
105100020524 e4  c                   endif
105200020524 e3  c                   endif
105300020524 e2  c                   endif
105400020524 e1  c                   endif
105500020524
105600020524     c                   endsr
105700020524      *---------------------------------------------------------------*
105800020524      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
105900020524      *---------------------------------------------------------------*
106000020524     c     sr_wrtbolla   begsr
106100020524
106200020524      * richiamo il programma per la scelta delle stampanti
106300020524     c                   exsr      sr_scestp
106400020530     c   91              eval      okbolla = '1'
106500020530     c   91              goto      endwrtbolla
106600020524
106700020524     c                   clear                   kpjbu
106800020524     c                   clear                   dtasv
106900020524
107000020524     c                   eval      §lpfpr = 'T'
107100020524     c                   movel     orppor        §lpccn
107200020524     c                   move      9999          §lpccn
107300020524     c                   movel     dateu         §lpaas
107400020524     c                   move      dateu         §lpmgs
107500020524     c                   eval      §lplnp = orppor
107600020524     c                   eval      §lpcbo = 'FY'
107700020524     c                   eval      §lpsop = '+'
107800020530     c                   eval      §lplna = orppoe
107900020524     c                   eval      §lprsm = orprsr
108000020524     c                   eval      §lpinm = orpinr
108100020524     c                   eval      §lpcam = orpcar
108200020524     c                   eval      §lplom = orplor
108300020524     c                   eval      §lpprm = orpprr
108400020524     c                   eval      §lpnzm = orpnar
108500020524
108600020524 b1  c                   if        orpncl <> 0
108700020524     c                   eval      §lpncl = orpncl
108800020524 x1  c                   else
108900020524     c                   eval      §lpncl = 1
109000020524 e1  c                   endif
109100020524 b1  c                   if        orppkg <> 0
109200020524     c                   eval      §lppkb = orppkg
109300020524 x1  c                   else
109400070920 b3  c                   if        orpbnc <> 0
109500070920     c                   eval      wpkb = orpbnc / d§dftbnc * d§dftpkg
109600070920     c                   if        wpkb > 999999,9
109700070920     c                   eval      wpkb = 999999,9
109800070920     c                   endif
109900070920     c                   eval      §lppkb = wpkb
110000070920 x2  c                   else
110100070920 b2  c                   if        orpvlm <> 0
110200070920     c                   eval      wpkb = orpvlm * d§dftpkg
110300070920     c                   if        wpkb > 999999,9
110400070920     c                   eval      wpkb = 999999,9
110500070920     c                   endif
110600070920     c                   eval      §lppkb = wpkb
110700020524 e3  c                   endif
110800020524 e2  c                   endif
110900020524 e1  c                   endif
111000020524
111100020524     c                   clear                   ds_keyorm
111200020524     c                   eval      ds_poe = ORppoe
111300020524     c                   eval      ds_nsr = ORpnsr
111400020524     c                   eval      ds_nor = ORpnor
111500020524     c                   eval      ds_nrv = ORpnrv
111600041201     c                   move      ds_keyorm     §lprmn
111700020524
111800020524     c                   eval      §lprma = orprfa
111900020524     c                   eval      §lprmo = orpcau + '-' + d§cmrdes1
112000020524     c                   eval      §lpctm = '4F'
112100020524     c                   movel     dateu         §lpdrt
112200020524
112300020524      * Cerca codice autotrasportatore e flag mattino/pomeriggio
112400071109 b1  c                   if        orpndc <> 0 and vs1opz='2'
112500071109     c                   exsr      sr_carpdr
112600071109 e1  c                   endif
112700071109     c                   if        W_pdr=*zeros or orpndc=0
112800020524     c                   movel     §lplnp        §lppdr
112900020524     c                   move      0999          §lppdr
113000020524     c                   eval      §lpfpp = 'M'
113100071113     c                   else
113200071113     c                   eval      §lppdr = w_pdr
113300071113     c                   eval      §lpfpp = w_fpp
113400020524 e1  c                   endif
113500020524
113600020524     C                   call      'FNLS01R'
113700020524     C                   parm                    kpjba
113800020524     C                   parm                    dblp
113900020524     C                   parm                    dtasv
114000030624     c                   Parm                    Trul90ds
114100020524
114200020524     C                   if        §lpfpr = '4' or §lpfpr = '3'
114300020524     C                             or §lpfpr = '5'
114400020524     C                   eval      okbolla = '1'
114500020524     c                   else
114600020524     c                   eval      wbolla = *on
114700020524     C                   endif
114800020524
114900020524     c     endwrtbolla   endsr
115000020524      *---------------------------------------------------------------*
115100020524      * scelta stampante per immissione bolla
115200020524      *---------------------------------------------------------------*
115300020524     c     sr_scestp     begsr
115400020524
115500020524     C                   clear                   kpjbu
115600020530     c                   setoff                                       91
115700020524
115800020524      * Richiesta stampanti
115900020524     C                   if        conta = *zeros
116000030624     C                   clear                   trul90ds
116100030624     C                   movel     'S'           D90rse
116200030624     C                   movel     'S'           D90rsb
116300030624     C                   call      'TRUL90R'
116400020524     C                   parm                    kpjba
116500030624     C                   parm                    trul90ds
116600020524      * F3=Fine
116700030624     C                   if        D90f3 = '1'
116800020530     C                   eval      *in91 = *on
116900020524     C                   goto      endstp
117000020524     C                   endif
117100090202
117200090202      /free
117300090202         reset ds5P;
117400090202         if  %subst(knmus : 1 : 3) = 'EDP';
117500090202           TBLkey = 'O' + 'EDP';
117600090202         else;
117700090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
117800090202         endif;
117900090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
118000090202         if  %found( TABEL00F );
118100090202           ds5P = TBLuni;
118200090202         endif;
118300090202      /end-free
118400030307
118500030307     C                   z-add     103           lung
118600020524
118700020524      * Ovrprtf Segnacolli
118800030624     C                   movea     D90pre        cm3(28)
118900030624     C                   movea     D90mde        cm3(50)
119000020524     C                   clear                   comman
119100020524     C                   movea     cm3           comman
119200020530     C                   CALL      'QCMDEXC'                            91
119300020524     C                   PARM                    comman
119400020524     C                   PARM                    lung
119500020530     C   91              goto      endstp
119600030624
119700030624      * Bolle - Nuovo modulo
119800030624
119900030624     c                   Z-Add     110           lung
120000030624
120100030624      * Ovrprtf Bolle A4
120200030624     c                   Movea     D90prb4       cma4(30)
120300030624     c                   Movea     D90mdb4       cma4(52)
120400030624     c                   Clear                   Comman
120500030624     c                   Movea     cmA4          Comman
120600030624     c                   Call      'QCMDEXC'                            91
120700030624     c                   Parm                    Comman
120800030624     c                   Parm                    Lung
120900030624     c   91              goto      endstp
121000030624
121100030624      * Ovrprtf Bolle A5
121200030624     c                   Movea     D90prb5       cmA5(30)
121300030624     c                   Movea     D90mdb5       cmA5(52)
121400030624     c                   Clear                   Comman
121500030624     c                   Movea     cmA5          Comman
121600030624     c                   Call      'QCMDEXC'                            91
121700030624     c                   Parm                    Comman
121800030624     c                   Parm                    Lung
121900030624     c   91              goto      endstp
122000020524
122100020524     C                   eval      conta = conta +1
122200020524     C                   endif
122300020524
122400020524     c     endstp        endsr
122500001103      *---------------------------------------------------------------*
122600001103      *    Aggiornamento Proposta                                     *
122700001103      *---------------------------------------------------------------*
122800001103
122900001103     C     SR_UPDATEPRO  begsr
123000001103
123100001103     C                   z-add     *date         G02DAT
123200001103     C                   clear                   G02ERR
123300001103     C                   call      'XSRDA8'
123400001103     C                   parm                    WLBDAT
123500001103
123600001103     C                   z-add     g02inv        orpdte
123700001103     C                   time                    orpore
123800001103     C                   movel     knmus         orpute
123900040818     c                   z-add     g02inv        orpdtt
124000001103     C                   UPDATE    fnorp000
124100001103     C     orpfev        ifeq      'R'
124200001103     C                   add       1             vrifin
124300001103     C                   add       1             vrifiu
124400001103     C                   seton                                        11
124500001103     C                   else
124600001103     C                   add       1             vconfn
124700001103     C                   add       1             vconfe
124800001103     C                   seton                                        10
124900001103     C                   endif
125000001103
125100001103     C                   endsr
125200001024
125300001024      *---------------------------------------------------------------*
125400001024      *    Richiama PGM Creazione ORM                                 *
125500001024      *---------------------------------------------------------------*
125600001024
125700001103     C     SR_RICHIAMA   begsr
125800001024
125900001103     C                   clear                   FIOR05DS
126000001103
126100001106     C     orpfao        ifeq      999
126200030801     c     orpfao        oreq      910
126300001106     C                   move      'C'           §rmtla
126400001106     C                   else
126500001103     C                   move      '2'           §rmtla
126600001106     C                   endif
126700001103     C                   move      'P'           §rmfpr
126800001103     C                   move      orppoe        §rmpoe
126900001103     C                   move      orpnsr        §rmnsr
127000001103     C                   move      orpnor        §rmnor
127100001103     C                   move      orpnrv        §rmnrv
127200001103     C                   move      orptor        §rmtor
127300001106     C                   move      orpcor        §rmcor
127400001106     C                   move      orprso        §rmrso
127500001106     C                   move      orpino        §rmino
127600001106     C                   move      orpcao        §rmcao
127700001106     C                   move      orploo        §rmloo
127800001106     C                   move      orppro        §rmpro
127900001106     C                   move      orpnao        §rmnao
128000001106     C                   move      orpcra        §rmcra
128100001106     C                   move      orprsr        §rmrsr
128200001106     C                   move      orpinr        §rminr
128300001106     C                   move      orpcar        §rmcar
128400001106     C                   move      orplor        §rmlor
128500001106     C                   move      orpprr        §rmprr
128600001106     C                   move      orpnar        §rmnar
128700001106     C                   move      orprer        §rmrer
128800001106     C                   move      orpter        §rmter
128900001106     C                   move      orpcrc        §rmcrc
129000001106     C                   move      orprsc        §rmrsc
129100001106     C                   move      orpinc        §rminc
129200001106     C                   move      orpcac        §rmcac
129300001106     C                   move      orploc        §rmloc
129400001106     C                   move      orpprc        §rmprc
129500001106     C                   move      orpnac        §rmnac
129600001106     C                   move      orpffd        §rmffd
129700001106     C                   move      orpdar        §rmdar
129800001106     C                   move      orporr        §rmorr
129900001106     C                   move      orpnam        §rmnam
130000001106     C                   move      orpncl        §rmncl
130100001106     C                   move      orppkg        §rmpkg
130200001106     C                   move      orpvlm        §rmvlm
130300001106     C                   move      orpbnc        §rmbnc
130400001106     C                   move      orpblc        §rmblc
130500001106     C                   move      orpatt        §rmatt
130600001106     C                   move      orpmtc        §rmmtc
130700001106     C                   move      orppag        §rmpag
130800001106     C                   move      orpksc        §rmksc
130900001106     C                   move      orpctr        §rmctr
131000001106     C                   move      orppor        §rmpor
131100001106     C                   move      orpzor        §rmzor
131200001106     C                   move      orppoc        §rmpoc
131300001106     C                   move      orpno1        §rmno1
131400001106     C                   move      orpno2        §rmno2
131500001106     C                   move      orpcst        §rmcst
131600001106     C                   move      orpvcs        §rmvcs
131700001106     C                   move      orpfcs        §rmfcs
131800001106     C                   move      orpddt        §rmddt
131900001221     C                   z-add     orpsto        §rmsto
132000001106     C                   move      orprfa        §rmrfa
132100070706     C                   move      orpeti        §rmeti
132200010119     C                   move      orpspi        §rmspi
132300010209     C                   move      orpcau        §rmcau
132400010507     C                   move      orpflo        §rmflo
132500020524      * se generata bolla addebito passo flag per impedire f12 da conferma
132600020524     c                   if        wbolla = *on
132700020524     c                   movel     'B'           §rmflv
132800020524     c                   endif
132900131106
133000131106     c                   eval      §rmdtv = ORPdtv
133100131106     c                   eval      §rmorv = ORPorv
133200010205
133300010205      * Passo alcuni parametri anche nella parm01
133400010205     C                   clear                   parm01
133500010322     C                   if        orppoe = orppor
133600010205     C                   eval      pmio = 'S'
133700010322     C                   endif
133800061204     c                   eval      pndc = orpndc
133900061204     c                   eval      pddc = orpddc
134000120612     c                   eval      pdstfgs = orppor
134100010205     C                   movel     parm01        kpjbu
134200001106
134300131111     C                   call      'FIOR05R'
134400001103     C                   parm                    KPJBA
134500001103     C                   parm                    FIOR05DS
134600031204
134700031204     c                   eval       $Fior05 = *on
134800001024
134900001103     C                   endsr
135000001019
135100001102      *---------------------------------------------------------------*
135200001102      *    Visualizza Proposta di Variazione Scelta                   *
135300001102      *---------------------------------------------------------------*
135400001102
135500001102     C     SR_VISUALIZZA begsr
135600001102
135700001102      *    Accedo a Proposte
135800001102
135900001102     C     KFNORPA       chain(e)  fnorp02l
136000001102
136100001102      *    Se Errore Emetto Videata
136200001102
136300001102     C                   if        %error
136400001102     C                   move      vs1dtv        vi5dtv
136500001102     C                   move      vs1orv        vi5orv
136600001103     C                   move      vs1ute        vi5utv
136700001102     C                   move      vs1poe        vi5poe
136800001102     C                   move      vs1nsr        vi5nsr
136900001102     C                   move      vs1nor        vi5nor
137000001102     C                   move      vs1nrv        vi5nrv
137100001102     C                   do        *hival
137200001102     C                   EXFMT     FIOR205
137300001102     C   KL              leave
137400001102     C                   enddo
137500001102     C                   else
137600001102
137700001102      *    Se ho Trovato il Record Muovo Campi a Video
137800001102
137900001102     C                   if        %found(fnorp02l)
138000001102     C                   EXSR      SR_MUOVIPRO
138100001102
138200001102      *    Emetto Video
138300001102
138400001102     C                   do        *hival
138500001102     C                   EXFMT     FIOR204
138600071126
138700071126     c                   eval      *in28 = *off
138800071126     c                   clear                   vidmsg
138900001102
139000001102      *    F12=Ritorno
139100001102
139200001102     C     *inkl         ifeq      *on
139300001102     C     *in42         ifeq      *on
139400001102     C                   EXSR      SR_MUOVIPRO
139500001102     C                   else
139600001102     C                   leave
139700001102     C                   endif
139800001102     C                   endif
139900131029
140000131029      /free
140100131029       //?F4=Dati Avviso Ritiro
140200131029       IF  *inkd;
140300131029         exsr F04D01;
140400131029         iter;
140500131029       ENDIF;
140600131029
140700131029       //?F5=Altri dati
140800131029       IF  *inke;
140900131029         exsr F05D01;
141000131029         iter;
141100131029       ENDIF;
141200140303
141300140303       //?F17=Note AUT
141400140303       IF  *inkr;
141500140303         exsr F17D01;
141600140303         iter;
141700140303       ENDIF;
141800131029      /end-free
141900001102
142000131029      *    F9=Visualizza ORM
142100001102
142200131029     c                   if        *inki
142300071108     c                   Clear                   parm01
142400071108     c                   Eval      ppoe = Vi4Poe
142500071108     c                   Eval      pnor = Vi4Nor
142600071108     c                   Eval      pnsr = Vi4Nsr
142700071108     c                   Eval      pnrv = Vi4Nrv
142800071108     c                   Eval      pfgs = Vidpor
142900071108     c                   Eval      kpjbu = parm01
143000071108     c                   call      'FIOR07R'
143100071108     c                   Parm                    kpjba
143200071108     C                   iter
143300071108     c                   endif
143400131029
143500131029      * F24=Altri tasti  ?
143600131029     c                   if        *inky and $loop > 1
143700131029     c                   exsr      sr_f24
143800131029     c                   iter
143900131029     c                   endif
144000001103
144100001103
144200001103      *    F16=Rifiuta
144300001103
144400001103     C     *inKQ         ifeq      *on
144500140303      /free
144600140303       //?Se presente proposta note AUT le devo prima gestire
144700140303         IF  *in08;
144800140303           clear fiort1ds;
144900140303           IORT1mod = 'P';
145000140303           IORT1poe = VS1poe;
145100140303           IORT1nsr = VS1nsr;
145200140303           IORT1nor = VS1nor;
145300140303           IORT1nrv = VS1nrv;
145400140303           IORT1pgm = 'FIOR20R';
145500140303           IORT1dim = VS1dtvw;
145600140303           IORT1oim = VS1orv;
145700140303           §kpjbu = kpjbu;
145800140303
145900140303           fiort1r (kpjba:fiort1ds);
146000140303
146100140303           kpjbu = §kpjbu;
146200140303         ENDIF;
146300140303      /end-free
146400001103     C                   move      'R'           orpfev
146500001103     C                   exsr      SR_UPDATEPRO
146600001103     C                   leave
146700001103     C                   endif
146800001103
146900001103      *    F11=Modifica ORM
147000070123      * non posso accettare una variazione su un ORM in fase 420
147100070123      * il controllo avviene prima non abilita f11
147200001103
147300001103     C     *inKK         ifeq      *on
147400020530
147500020530     c                   eval      ds_orpcra = orpcra
147600020530     c                   eval      ds_orpcor = orpcor
147700020530     c                   eval      ds_orpcrc = orpcrc
147800020530
147900020530      * se sto confermando una proposta di chiusura
148000030801 1b  c                   if        orpfao = 999 or orpfao = 910
148100130412      * emetto msg info se orm in fase 400
148200130412     c                   if        ormfao = 400 or ormfao = 410
148300071126     c                   eval      *in28 = *on
148400071126     c                   eval      vidmsg = msg(08)
148500071126     c                   iter
148600071126     c                   endif
148700071126
148800020530     C                   clear                   dcmr
148900020530     C                   clear                   TIBS02DS
149000020530     C                   movel     'C'           T02mod
149100020530     C                   movel     knsif         t02sif
149200020530     C                   movel     'CMR'         t02cod
149300020530     C                   movel(p)  orpcau        T02ke1
149400020530     C                   call      'TIBS02R'
149500020530     C                   parm                    KPJBA
149600020530     C                   parm                    TIBS02DS
149700020530     C                   movel     t02uni        dcmr
149800020530      * procedo con i controlli per creare l'addebito
149900080226      * solo se l'orm è in fase 400 o 410
150000020530 2b  c                   if        d§cmradd <> *blanks
150100080226     c                             and (ormfao = 400 or ormfao = 410)
150200020530     c                   exsr      sr_contradd
150300020530 e2  c                   endif
150400020530 e1  c                   endif
150500020530      * se f12 o f03 da immissione bolla non devo confermare
150600020530 b1  c                   if        okbolla = ' '
150700001103
150800001103      *    Richiamo Programma di Creazione ORM
150900001103
151000001106     C                   EXSR      SR_RICHIAMA
151100001103
151200001103     C     §rmerr        ifeq      ' '
151300001103     C                   move      'E'           orpfev
151400001103     C                   exsr      SR_UPDATEPRO
151500001103     C                   leave
151600001103     C                   endif
151700020530
151800020530     c                   endif
151900001103
152000001103     C                   endif
152100001103
152200001102     C                   enddo
152300001102
152400001102     C                   endif
152500001102
152600001102     C                   endif
152700001102     C                   endsr
152800001102
152900001102      *---------------------------------------------------------------*
153000001102      *    Muovo Campi Proposta da File a Video                       *
153100001102      *---------------------------------------------------------------*
153200001102
153300001102     C     SR_MUOVIPRO   begsr
153400131029
153500131029     c                   ExSr      Sr_Tastifun
153600131029     c                   exsr      muoviORPE
153700131029
153800010209     C                   setoff                                       4215
153900131216     c                   eval      *in04 = *off
154000131216     c                   eval      *in06 = *off
154100131216
154200001102     C                   clear                   fnorm000
154300001102     C     KFNORM        chain     fnorm01l
154400131029     c                   exsr      leggiFNORE
154500001102
154600001102      *    Key
154700001102
154800001102     C                   move      vs1dtv        vi4dtv
154900001102     C                   move      vs1orv        vi4orv
155000001102     C                   move      vs1ute        vi4utv
155100001102     C                   move      vs1poe        vi4poe
155200001102     C                   move      vs1nsr        vi4nsr
155300001102     C                   move      vs1nor        vi4nor
155400001102     C                   move      vs1nrv        vi4nrv
155500001102     C                   movel     ormdao        wa4               4
155600001102     C                   move      wa4           vi4dao
155700001102     C                   move      ormdao        wa4
155800001102     C                   move      wa4           wg2               2
155900001102     C                   movel     wa4           wm2               2
156000001102     C                   move      wm2           wa4
156100001102     C                   movel     wg2           wa4
156200001102     C                   movel     wa4           vi4dao
156300001215
156400001215     C                   movel     ormoao        vi4oao
156500001102
156600001102      *    Fase
156700001102
156800001102     C                   move      orpfao        vi4fao
156900001102     C                   if        vi4fao <> *zeros
157000001102     C                   clear                   TIBS02DS
157100001102     C                   movel     'C'           T02mod
157200001102     C                   movel     knsif         t02sif
157300001102     C                   movel     'FAR'         t02cod
157400001102     C                   movel(p)  vi4fao        T02ke1
157500001102     C                   call      'TIBS02R'
157600001102     C                   parm                    KPJBA
157700001102     C                   parm                    TIBS02DS
157800010314     C                   movel     t02uni        dfar
157900010314     C                   movel     d§fardes      vi4def
158000001102     C                   else
158100001102     C                   clear                   vi4def
158200001102     C                   endif
158300001102
158400001102      *    Dati Cliente Ritiro
158500001102
158600001102     C     orpcra        ifne      0
158700001102     C                   movel     orpcra        vi4cr1
158800001102     C                   move      orpcra        wsept             7
158900001102     C                   movel     wsept         vi4cr2
159000001102     C                   move      wsept         vi4cr3
159100001102     C                   else
159200001102     C                   clear                   vi4cr1
159300001102     C                   clear                   vi4cr2
159400001102     C                   clear                   vi4cr3
159500001102     C                   endif
159600001102     C                   movel     orprsr        vi4rsr
159700001102     C                   movel     orpinr        vi4inr
159800001102     C                   movel     orpcar        vi4car
159900001102     C                   movel     orplor        vi4lor
160000001102     C                   movel     orpprr        vi4prr
160100001102     C                   movel     orpnar        vi4nar
160200001102
160300001102      *    Dati Cliente Ordinante
160400001102
160500001102     C     orpcor        ifne      0
160600001102     C                   movel     orpcor        vi4co1
160700001102     C                   move      orpcor        wsept             7
160800001102     C                   movel     wsept         vi4co2
160900001102     C                   move      wsept         vi4co3
161000001102     C                   else
161100001102     C                   clear                   vi4co1
161200001102     C                   clear                   vi4co2
161300001102     C                   clear                   vi4co3
161400001102     C                   endif
161500001102     C                   movel     orprso        vi4rso
161600001102     C                   movel     orpino        vi4ino
161700001102     C                   movel     orpcao        vi4cao
161800001102     C                   movel     orploo        vi4loo
161900001102     C                   movel     orppro        vi4pro
162000001102     C                   movel     orpnao        vi4nao
162100001102
162200001129      *    Tipo comunicazione ORM (tco)
162300001102
162400001129     C                   if        ormtco <> *blanks
162500001102     C                   clear                   TIBS02DS
162600001102     C                   movel     'C'           T02mod
162700001102     C                   movel     knsif         t02sif
162800001129     C                   movel     'TCO'         t02cod
162900001129     C                   movel(p)  ormtco        T02ke1
163000001102     C                   call      'TIBS02R'
163100001102     C                   parm                    KPJBA
163200001102     C                   parm                    TIBS02DS
163300001129     C                   movel     t02uni        vi4tco
163400001102     C                   else
163500001129     C                   clear                   vi4tco
163600001102     C                   endif
163700001102
163800001102      *    Numero ORM Fisso
163900001102
164000001102     C                   move      ormpos        vi4pos
164100001102     C                   move      ormors        vi4ors
164200001102     C     ormpos        ifne      0
164300001102     C     ormors        orne      0
164400001102     C                   seton                                        58
164500001102     C                   else
164600001102     C                   setoff                                       58
164700001102     C                   endif
164800001102
164900001102      *    Quantità
165000001102
165100001102     C                   move      orpncl        vi4ncl
165200001102     C                   move      orppkg        vi4pkg
165300001102     C                   move      orpvlm        vi4vlm
165400001102     C                   move      orpbnc        vi4bnc
165500001102     C                   move      orpblc        vi4blc
165600001102     C                   move      orpatt        vi4att
165700001102     C                   move      orpmtc        vi4mtc
165800071107     c                   if        orpspi='S'
165900071107     C                   eval      vi4spi='SI'
166000071107     c                   else
166100071107     C                   eval      vi4spi='NO'
166200071107     c                   endif
166300001102
166400001102      *    Dati di Ritiro
166500001102
166600001102     C                   movel     orpdar        wa4               4
166700001102     C                   move      wa4           vi4dar
166800001102     C                   move      orpdar        wa4
166900001102     C                   move      wa4           wg2               2
167000001102     C                   movel     wa4           wm2               2
167100001102     C                   move      wm2           wa4
167200001102     C                   movel     wg2           wa4
167300001102     C                   movel     wa4           vi4dar
167400001102     C                   move      orporr        vi4orr
167500010205
167600010205      * Priorità
167700010205
167800001221     C                   move      orpsto        vi4sto
167900001129     C     vi4sto        ifne      *zeros
168000001103     C                   clear                   TIBS02DS
168100001103     C                   movel     'C'           T02mod
168200001103     C                   movel     knsif         t02sif
168300001103     C                   movel     'STO'         t02cod
168400131029     c                   eval      T02ke1 = %editc(ORPsto:'X')
168500001103     C                   call      'TIBS02R'
168600001103     C                   parm                    KPJBA
168700001103     C                   parm                    TIBS02DS
168800010205     C                   movel     t02uni        vi4dst
168900001103     C                   else
169000010205     C                   clear                   vi4dst
169100001103     C                   endif
169200001102
169300001102      *    P.O. Ritiro
169400001102
169500071108     c                   clear                   og143
169600001102     C     orppor        ifne      0
169700001102     C                   move      orppor        vi4por
169800001102     C     orppor        chain     azorg01l
169900001102     C                   if        %found(azorg01l)
170000001102     C                   movel     orgdes        vi4der
170100071108     c                   eval      og143 = orgde3
170200001102     C                   else
170300001102     C                   clear                   vi4der
170400001102     C                   endif
170500001102     C                   else
170600001102     C                   clear                   vi4por
170700001102     C                   clear                   vi4der
170800001102     C                   endif
170900071107      * giro
171000071107     c                   clear                   vi4cgi
171100071108     c     kfnorm        chain     fnorg01l
171200071107     c                   if        %found(fnorg01l)
171300071107     c                   eval      vi4cgi = orgcgi
171400071107     c                   endif
171500071108     c                   clear                   vi4dcg
171600071108     c                   clear                   fidg09ds
171700071108     c                   eval      d09iop0 = '001'
171800071108     c                   eval      d09ifgs = vidpor
171900071109     c                   eval      d09idat = dateu
172000071108     c                   eval      d09icgi = vi4cgi
172100071108     c                   eval      d09itug = 'R'
172200071108     c                   eval      kpjbu = fidg09ds
172300071108     c                   call      'FIDG09R'
172400071108     c                   parm                    kpjba
172500071108     c                   eval      fidg09ds = kpjbu
172600071108     c                   if        d09ocgi <> ' '
172700071108     c                   eval      vi4dcg = d09odes
172800071108     c                   endif
172900071108      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
173000071108      * europolitan invece ormai è obsoleta
173100071108     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
173200071108      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
173300071108      * partener se caricata e la visualizzo al posto della zona
173400071108     c                   if        ormfcs <> *blanks
173500071108     c                   clear                   tibs02ds
173600071108     c                   eval      t02mod = 'C'
173700071108     c                   eval      t02sif = knsif
173800071108     c                   eval      t02cod = 'MIP'
173900071108     c                   eval      t02ke1 = ormfcs
174000071108     c                   call      'TIBS02R'
174100071108     c                   parm                    kpjba
174200071108     c                   parm                    tibs02ds
174300071108     c                   eval      vi4fcs = t02uni
174400071108     c                   eval      *in22 = *on
174500071108     c                   endif
174600071108     c                   endif
174700071108
174800071108      *    dati della distinta
174900071109     C                   z-add     ORpndc        vi4ndc
175000071108      * Inversione data distinta
175100071108     C                   clear                   wlbdat
175200071109     C                   z-add     ORpddc        G02inv
175300071108     C                   movel     '3'           G02err
175400071108     C                   call      'XSRDA8'
175500071108     C                   parm                    wlbdat
175600071108     C                   z-add     G02dat        vi4ddc
175700071108      * Cerca codice autotrasportatore
175800071109     c                   clear                   vi4pdc
175900071109     c                   clear                   vi4dpd
176000071109     c                   if        orpndc<>0
176100071109     c                   exsr      sr_carpdr
176200071109     c                   eval      vi4pdc=w_pdr
176300071109      * decodifico l'autotrasportatore
176400071109     c                   if        vi4pdc>0
176500071109     c                   Movel     vi4pdc        kpdpdr
176600071109     c     kfapd         Chain     Fiapd01L
176700071109     c                   If        %Found(Fiapd01l)
176800071109     c                   Eval      vi4dpd = ApdRsc
176900071109     c                   EndIf
177000071109     c                   endif
177100071109     c                   endif
177200071108
177300071108      * se Stampato faccio vedere la scritta
177400071108     c                   eval      *in16 = (ormdst > 0)
177500001102
177600001102      *    Referente/Telefono/Riferimento/Natura Merce
177700001102
177800001102     C                   movel     orprer        vi4rer
177900001102     C                   movel     orpter        vi4ter
178000001102     C                   movel     orprfa        vi4rfa
178100001102     C                   movel     orpnam        vi4nam
178200001102
178300001102      *    Tipo Ordine (tor)
178400001102
178500001102     C     orptor        ifne      *blanks
178600001102     C                   move      orptor        vi4tor
178700001102     C                   clear                   TIBS02DS
178800001102     C                   movel     'C'           T02mod
178900001102     C                   movel     knsif         t02sif
179000001102     C                   movel     'TOR'         t02cod
179100001102     C                   movel(p)  orptor        T02ke1
179200001102     C                   call      'TIBS02R'
179300001102     C                   parm                    KPJBA
179400001102     C                   parm                    TIBS02DS
179500001102     C                   movel     t02uni        vi4det
179600001102     C                   else
179700001102     C                   clear                   vi4tor
179800001102     C                   clear                   vi4det
179900001102     C                   endif
180000001102
180100001102      *    Dati Pagamento
180200001102
180300071108     C                   if        ORppag = 'M'
180400071108     C                   eval      vi4pag = 'Mitt.'
180500071108     C                   endif
180600071108     C                   if        ORppag = 'D'
180700071108     C                   eval      vi4pag = 'Dest.'
180800071108     C                   endif
180900071108     C                   if        ORppag = 'O'
181000071108     C                   eval      vi4pag = 'Ord. '
181100071108     C                   endif
181200001102     C                   move      orpksc        vi4ksc
181300001102     C                   move      orpctr        vi4ctr
181400071108     C                   if        ORpddt = 'S'
181500071108     C                   eval      vi4ddt = 'SI'
181600071108     C                   endif
181700071108     C                   if        ORpddt = 'N'
181800071108     C                   eval      vi4ddt = 'NO'
181900071108     C                   endif
182000071108     C                   if        ORPffd = 'S'
182100071108     C                   eval      vi4ffd = 'SI'
182200071108     C                   else
182300071108     C                   eval      vi4ffd = 'NO'
182400071108     C                   endif
182500001102
182600001102      *    P.O. Consegna
182700001102
182800001102     C     orppoc        ifne      0
182900001102     C                   move      orppoc        vi4poc
183000001102     C     orppoc        chain     azorg01l
183100001102     C                   if        %found(azorg01l)
183200001102     C                   movel     orgdes        vi4dec
183300001102     C                   else
183400001102     C                   clear                   vi4dec
183500001102     C                   endif
183600001102     C                   else
183700001102     C                   clear                   vi4poc
183800001102     C                   clear                   vi4dec
183900001102     C                   endif
184000071108      * ORM con Ricevuta di Ritiro
184100071108     c                   eval      dorm01=ormflo
184200071108     c                   Eval      *In17 = (§orsrm = 'S')
184300001102
184400001102      *    Dati Consegna A
184500001102
184600001102     C     orpcrc        ifne      0
184700001102     C                   movel     orpcrc        vi4cc1
184800001102     C                   move      orpcrc        wsept             7
184900001102     C                   movel     wsept         vi4cc2
185000001102     C                   move      wsept         vi4cc3
185100001102     C                   else
185200001102     C                   clear                   vi4cc1
185300001102     C                   clear                   vi4cc2
185400001102     C                   clear                   vi4cc3
185500001102     C                   endif
185600001102     C                   movel     orprsc        vi4rsc
185700001102     C                   movel     orpinc        vi4inc
185800001102     C                   movel     orpcac        vi4cac
185900001102     C                   movel     orploc        vi4loc
186000001102     C                   movel     orpprc        vi4prc
186100001102     C                   movel     orpnac        vi4nac
186200001102
186300001102      *    Note
186400001102
186500001102     C                   movel     orpno1        vi4no1
186600001102     C                   movel     orpno2        vi4no2
186700010209
186800010209      * ORM chiuso
186900010209
187000030801     C                   if        orpfao = 999 or orpfao = 910
187100010209     C                   eval      *in15 = *on
187200010209     C                   movel     orpcau        vi4cau
187300010301     C                   clear                   dcmr
187400010209     C                   clear                   TIBS02DS
187500010209     C                   movel     'C'           T02mod
187600010209     C                   movel     knsif         t02sif
187700010227     C                   movel     'CMR'         t02cod
187800010209     C                   movel(p)  vi4cau        T02ke1
187900010209     C                   call      'TIBS02R'
188000010209     C                   parm                    KPJBA
188100010209     C                   parm                    TIBS02DS
188200010301     C                   movel     t02uni        dcmr
188300010301     C                   movel     d§cmrdes      vi4dca
188400010209     C                   endif
188500030610
188600030610     c                   eval      dorm01 = ORPflo
188700030610
188800030610      * ORM commissionato
188900030610     c                   If        §orCom = 'S'
189000030610     c                   Eval      Vi4Com = 'COMMISSIONATO'
189100030610     c                   Else
189200030610     c                   Clear                   Vi4Com
189300030610     c                   EndIf
189400001102
189500001102      *    Accendo Indicatori per differenze
189600001102
189700001102     C                   setoff                                       434445
189800001102     C                   setoff                                       464748
189900001102     C                   setoff                                       495051
190000001102     C                   setoff                                       525354
190100001102     C                   setoff                                       555657
190200001102     C                   setoff                                       596061
190300001102     C                   setoff                                       626364
190400001102     C                   setoff                                       656667
190500001102     C                   setoff                                       687071
190600001102     C                   setoff                                       727374
190700001102     C                   setoff                                       757677
190800001102     C                   setoff                                       787980
190900001102     C                   setoff                                       818292
191000001102     C                   setoff                                       939495
191100001102     C                   setoff                                       969798
191200001103     C                   setoff                                       838488
191300131029     C                   setoff                                       896999
191400161020     c                   setoff                                       40
191500131029     c                   clear                   AT3sms
191600131029     c                   clear                   AT3mail
191700001102
191800001102     C     orpfao        ifne      ormfao
191900001102     C                   eval      *in43 = *on
192000001102     C                   endif
192100001102     C     orpcra        ifne      ormcra
192200001102     C                   eval      *in44 = *on
192300001102     C                   endif
192400001102     C     orprsr        ifne      ormrsr
192500001102     C                   eval      *in45 = *on
192600001102     C                   endif
192700001102     C     orpinr        ifne      orminr
192800001102     C                   eval      *in46 = *on
192900001102     C                   endif
193000001102     C     orpcar        ifne      ormcar
193100001102     C                   eval      *in47 = *on
193200001102     C                   endif
193300001102     C     orplor        ifne      ormlor
193400001102     C                   eval      *in48 = *on
193500001102     C                   endif
193600001102     C     orpprr        ifne      ormprr
193700001102     C                   eval      *in49 = *on
193800001102     C                   endif
193900001102     C     orpnar        ifne      ormnar
194000001102     C                   eval      *in50 = *on
194100001102     C                   endif
194200001102     C     orpcor        ifne      ormcor
194300001102     C                   eval      *in51 = *on
194400001102     C                   endif
194500001102     C     orprso        ifne      ormrso
194600001102     C                   eval      *in52 = *on
194700001102     C                   endif
194800001102     C     orpino        ifne      ormino
194900001102     C                   eval      *in53 = *on
195000001102     C                   endif
195100001102     C     orpcao        ifne      ormcao
195200001102     C                   eval      *in54 = *on
195300001102     C                   endif
195400001102     C     orploo        ifne      ormloo
195500001102     C                   eval      *in55 = *on
195600001102     C                   endif
195700001102     C     orppro        ifne      ormpro
195800001102     C                   eval      *in56 = *on
195900001102     C                   endif
196000001102     C     orpnao        ifne      ormnao
196100001102     C                   eval      *in57 = *on
196200001102     C                   endif
196300001102     C     orpncl        ifne      ormncl
196400001102     C                   eval      *in59 = *on
196500001102     C                   endif
196600001102     C     orppkg        ifne      ormpkg
196700001102     C                   eval      *in60 = *on
196800001102     C                   endif
196900001102     C     orpvlm        ifne      ormvlm
197000001102     C                   eval      *in61 = *on
197100001102     C                   endif
197200001102     C     orpbnc        ifne      ormbnc
197300001102     C                   eval      *in62 = *on
197400001102     C                   endif
197500001102     C     orpblc        ifne      ormblc
197600001102     C                   eval      *in63 = *on
197700001102     C                   endif
197800001102     C     orpatt        ifne      ormatt
197900001102     C                   eval      *in64 = *on
198000001102     C                   endif
198100001102     C     orpmtc        ifne      ormmtc
198200001102     C                   eval      *in65 = *on
198300001102     C                   endif
198400001102     C     orpdar        ifne      ormdar
198500001102     C                   eval      *in66 = *on
198600001102     C                   endif
198700001102     C     orporr        ifne      ormorr
198800001102     C                   eval      *in68 = *on
198900001102     C                   endif
199000001103     C     orpsto        ifne      ormsto
199100001103     C                   eval      *in88 = *on
199200001102     C                   endif
199300001102     C     orprer        ifne      ormrer
199400001102     C                   eval      *in72 = *on
199500001102     C                   endif
199600001102     C     orpter        ifne      ormter
199700001102     C                   eval      *in73 = *on
199800001102     C                   endif
199900001102     C     orpnam        ifne      ormnam
200000001102     C                   eval      *in74 = *on
200100001102     C                   endif
200200001102     C     orprfa        ifne      ormrfa
200300001102     C                   eval      *in75 = *on
200400001102     C                   endif
200500001102     C     orptor        ifne      ormtor
200600001102     C                   eval      *in76 = *on
200700001102     C                   endif
200800001102     C     orppag        ifne      ormpag
200900001102     C                   eval      *in77 = *on
201000001102     C                   endif
201100001102     C     orpksc        ifne      ormksc
201200001102     C                   eval      *in78 = *on
201300001102     C                   endif
201400001102     C     orpctr        ifne      ormctr
201500001102     C                   eval      *in79 = *on
201600001102     C                   endif
201700001102     C     orpddt        ifne      ormddt
201800001102     C                   eval      *in80 = *on
201900001102     C                   endif
202000010119     C     orpspi        ifne      ormspi
202100010119     C                   eval      *in89 = *on
202200010119     C                   endif
202300001102     C     orppoc        ifne      ormpoc
202400001102     C                   eval      *in81 = *on
202500001102     C                   endif
202600001102     C     orpffd        ifne      ormffd
202700001102     C                   eval      *in82 = *on
202800001102     C                   endif
202900001102     C     orpcrc        ifne      ormcrc
203000001102     C                   eval      *in92 = *on
203100001102     C                   endif
203200001102     C     orprsc        ifne      ormrsc
203300001102     C                   eval      *in93 = *on
203400001102     C                   endif
203500001102     C     orpinc        ifne      orminc
203600001102     C                   eval      *in94 = *on
203700001102     C                   endif
203800001102     C     orpcac        ifne      ormcac
203900001102     C                   eval      *in95 = *on
204000001102     C                   endif
204100001102     C     orploc        ifne      ormloc
204200001102     C                   eval      *in96 = *on
204300001102     C                   endif
204400001102     C     orpprc        ifne      ormprc
204500001102     C                   eval      *in97 = *on
204600001102     C                   endif
204700001102     C     orpnac        ifne      ormnac
204800001102     C                   eval      *in98 = *on
204900001102     C                   endif
205000001102     C     orpno1        ifne      ormno1
205100001102     C                   eval      *in83 = *on
205200001102     C                   endif
205300001102     C     orpno2        ifne      ormno2
205400001102     C                   eval      *in84 = *on
205500001102     C                   endif
205600131029
205700131029      /free
205800161020         IF  VI4dpm <> wdpm;
205900161020           *in40 = *on;
206000161020         ENDIF;
206100131029         IF  VI4oraamda <> oraamda;
206200131029           *in69 = *on;
206300131029         ENDIF;
206400131029         IF  VI4oraama <> oraama;
206500131029           *in70 = *on;
206600131029         ENDIF;
206700131029         IF  VI4orapmda <> orapmda;
206800131029           *in71 = *on;
206900131029         ENDIF;
207000131029         IF  VI4orapma <> orapma;
207100131029           *in99 = *on;
207200131029         ENDIF;
207300131029         IF  W03sms <> sms;
207400131029           AT3sms = highrimm;
207500131029         ENDIF;
207600131029         IF  W03mail <> mail;
207700131029           AT3mail = highrimm;
207800131029         ENDIF;
207900131216
208000131216       //?Verifico se sono presenti dati nella altre 2 videate.
208100131216         IF  VI4cgi <> *blanks or VI4blc > 0 or VI4att > 0 or
208200131216             VI4mtc > 0 or VI4sto > 0 or VI4ffd <> 'NO';
208300131216           *in06 = *on;
208400131216         ENDIF;
208500131216         IF  W03mail <> *blanks or W03sms <> *blanks;
208600131216           *in04 = *on;
208700131216         ENDIF;
208800140303
208900140303         *in08 = *off;
209000140303       //?Verifico se sono presenti proposte note AUT
209100140303         chain (VS1poe:VS1nsr:VS1nor:VS1nrv:VS1dtvw:VS1orv) FNORPT1L;
209200140303         IF  %found(FNORPT1L);
209300140303           *in08 = *on;
209400140303         ENDIF;
209500140303
209600140303
209700131029      /end-free
209800001102
209900001102     C                   endsr
210000131029
210100131029      /free
210200131029       //--------------------------------------------------------------------
210300131029       //?Imposto campi da ORPE a video.
210400131029       //--------------------------------------------------------------------
210500131029       BEGSR muoviORPE;
210600131029
210700131029       //?Rcd 'O ' --> Orari apertura
210800131029         k_ORPEpoe = ORPpoe;
210900131029         k_ORPEnsr = ORPnsr;
211000131029         k_ORPEnor = ORPnor;
211100131029         k_ORPEnrv = ORPnrv;
211200131029         K_ORPEdtv = ORPdtv;
211300131029         k_ORPEorv = ORPorv;
211400131029         k_ORPEtrc = 'O ';
211500131029         clear dOREorari;
211600131029       //?Aggancio il rcd
211700131029         chain  %kds(k07fnorpe) FNORPE1L;
211800131029         IF  %found(FNORPE1L);
211900131029           dOREorari = ORPEdati;
212000131029         ENDIF;
212100131029         VI4oraamda = §OREoramda;
212200131029         VI4oraama  = §OREorama;
212300131029         VI4orapmda = §OREorapda;
212400131029         VI4orapma  = §OREorapa;
212500161020
212600161020       //?Rcd 'DT' --> Data Pronta Merce
212700161020         k_ORPEtrc = 'DT';
212800161108         clear dOREdt;
212900161020         chain  %kds(k07fnorpe) FNORPE1L;
213000161020         IF  %found(FNORPE1L);
213100161108           dOREdt = ORPEdati;
213200161020         ENDIF;
213300161108         IF  §OREdpm > *zeros;
213400161020           clear wlbdat;
213500161108           G02inv = §OREdpm;
213600161020           G02err = '3';
213700161020           xsrda8 (wlbdat);
213800161020           VI4dpm = G02dat;
213900161020         ENDIF;
214000161108         IF  VI4dpm = 0;
214100161108           VI4dpm = VI4dar;
214200161108         ENDIF;
214300131029
214400131029       //?Rcd 'S ' --> SMS per invio alert
214500140214         clear w03sms;
214600131029         k_ORPEtrc = 'S ';
214700131029       //?Aggancio il rcd
214800131029         chain  %kds(k07fnorpe) FNORPE1L;
214900131029         IF  %found(FNORPE1L);
215000131029           w03sms = ORPEdati;
215100131029         ENDIF;
215200131029
215300131029       //?Rcd 'MA' --> Mail per invio alert
215400140214         clear w03mail;
215500131029         k_ORPEtrc = 'MA';
215600131029       //?Aggancio il rcd
215700131029         chain  %kds(k07fnorpe) FNORPE1L;
215800131029         IF  %found(FNORPE1L);
215900131029           w03mail = ORPEdati;
216000131029         ENDIF;
216100131029
216200131029       ENDSR;
216300131029
216400131029       //--------------------------------------------------------------------
216500131029       //?Leggo FNORE00F.
216600131029       //--------------------------------------------------------------------
216700131029       BEGSR LeggiFNORE;
216800131029
216900131029         clear oraamda;
217000131029         clear oraama;
217100131029         clear orapmda;
217200131029         clear orapma;
217300131029         clear sms;
217400131029         clear mail;
217500131029
217600131029       //?Rcd 'O ' --> Orari apertura
217700131029         k_OREpoe = ORMpoe;
217800131029         k_OREnsr = ORMnsr;
217900131029         k_OREnor = ORMnor;
218000131029         k_OREnrv = ORMnrv;
218100131029         k_OREtrc = 'O ';
218200131029         clear dOREorari;
218300131029       //?Aggancio il rcd
218400131029         chain  %kds(k05fnore) FNORE01L;
218500131029         IF  %found(FNORE01L);
218600161108           dOREorari = OREdati;
218700131029         ENDIF;
218800131029         oraamda = §OREoramda;
218900131029         oraama  = §OREorama;
219000131029         orapmda = §OREorapda;
219100131029         orapma  = §OREorapa;
219200161020
219300161020       //?Rcd 'DT' --> Data Pronta Merce
219400161108         clear dOREdt;
219500161020         k_OREtrc = 'DT';
219600161020         chain  %kds(k05fnore) FNORE01L;
219700161020         IF  %found(FNORE01L);
219800161108           dOREdt = OREdati;
219900161020         ENDIF;
220000161108         IF  §OREdpm > *zeros;
220100161020           clear wlbdat;
220200161108           G02inv = §OREdpm;
220300161020           G02err = '3';
220400161020           xsrda8 (wlbdat);
220500161020           wdpm = G02dat;
220600161020         ENDIF;
220700161108         IF  wdpm = 0;
220800161108           clear wlbdat;
220900161108           G02inv = ORMdar;
221000161108           G02err = '3';
221100161108           xsrda8 (wlbdat);
221200161108           wdpm = G02dat;
221300161108         ENDIF;
221400131029
221500131029       //?Rcd 'S ' --> SMS per invio alert
221600131029         k_OREtrc = 'S ';
221700131029       //?Aggancio il rcd
221800131029         chain  %kds(k05fnore) FNORE01L;
221900131029         IF  %found(FNORE01L);
222000131029           sms = OREdati;
222100131029         ENDIF;
222200131029
222300131029       //?Rcd 'MA' --> Mail per invio alert
222400131029         k_OREtrc = 'MA';
222500131029       //?Aggancio il rcd
222600131029         chain  %kds(k05fnore) FNORE01L;
222700131029         IF  %found(FNORE01L);
222800131029           mail = OREdati;
222900131029         ENDIF;
223000131029       ENDSR;
223100131029
223200131029       //--------------------------------------------------------------------
223300131029       //?F4 - Alert.
223400131029       //--------------------------------------------------------------------
223500131029       BEGSR F04D01;
223600131029
223700131029         wEndF04 = *off;
223800131029
223900131029       //?emetto la videata relativa alla MAIL e SMS
224000131029         write fior204;
224100131029       //?emetto la window
224200131029         DOW not wEndF04;
224300131029           exfmt fior2043;
224400131029       //?F12-Ritorno
224500131029           IF  *inkl;
224600131029             wEndF04 = *on;
224700131029           ENDIF;
224800131029         ENDDO;
224900131029
225000131029       ENDSR;
225100131029
225200131029       //--------------------------------------------------------------------
225300131029       //?F5 - Altri dati.
225400131029       //--------------------------------------------------------------------
225500131029       BEGSR F05D01;
225600131029
225700131029         wEndF05 = *off;
225800131029
225900131029       //?emetto la videata relativa agli altri dati
226000131029         write fior204;
226100131029       //?emetto la window dove richiedo gli altri dati
226200131029         DOW not wEndF05;
226300131029           exfmt fior2042;
226400131029       //?F12-Ritorno
226500131029           IF  *inkl;
226600131029             wEndF05 = *on;
226700131029           ENDIF;
226800131029         ENDDO;
226900131029
227000131029       ENDSR;
227100140303
227200140303       //--------------------------------------------------------------------
227300140303       //?F17 - Note AUT.
227400140303       //--------------------------------------------------------------------
227500140303       BEGSR F17D01;
227600140303
227700140303         clear fiort1ds;
227800140303         IORT1mod = 'I';
227900140303         IORT1poe = VS1poe;
228000140303         IORT1nsr = VS1nsr;
228100140303         IORT1nor = VS1nor;
228200140303         IORT1nrv = VS1nrv;
228300140303         IORT1pgm = 'FIOR20R';
228400140303         IORT1dim = VS1dtvw;
228500140303         IORT1oim = VS1orv;
228600140303         §kpjbu = kpjbu;
228700140303
228800140303         fiort1r (kpjba:fiort1ds);
228900140303
229000140303         kpjbu = §kpjbu;
229100140303
229200140303       ENDSR;
229300131029      /end-free
229400131029
229500131029      *------------------------------------------------------------------------*
229600131029      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
229700131029      *------------------------------------------------------------------------*
229800131029     c     sr_f24        begsr
229900131029
230000131029     c                   Select
230100131029
230200131029     c                   When      $loop = 2 and $rig =2  or
230300131029     c                             $loop = 3 and $rig =3
230400131029     c                   Movea     rigatf1       vzfd01
230500131029     c                   Z-add     1             $rig
230600131029
230700131029     c                   If        $loop = 2
230800131029     c                   Eval      vzfd02= cf2412
230900131029     c                   Else
231000131029     c                   Eval      vzfd02= cf2413
231100131029     c                   EndIf
231200131029
231300131029     c                   When      $loop = 2 and $rig =1 or
231400131029     c                             $loop = 3 and $rig =1
231500131029     c                   Movea     rigatf2       vzfd01
231600131029     c                   Z-add     2             $rig
231700131029
231800131029     c                   If        $loop = 2
231900131029     c                   Eval      vzfd02 = cf2422
232000131029     c                   Else
232100131029     c                   Eval      vzfd02 = cf2423
232200131029     c                   EndIf
232300131029
232400131029     c                   When      $loop = 3 and $rig =2
232500131029     c                   Movea     rigatf3       vzfd01
232600131029     c                   Z-add     3             $rig
232700131029     c                   Eval      vzfd02 = cf2433
232800131029     c                   EndSl
232900131029
233000131029     c                   EndSr
233100001102
233200071109      *---------------------------------------------------------------*
233300071109      *    Reperimento dati autotrasportatore                         *
233400071109      *---------------------------------------------------------------*
233500071109
233600071109     C     SR_CARPDR     begsr
233700071109     c                   clear                   w_fpp
233800071109     c                   clear                   w_pdr
233900071109     c
234000120612     c                   z-add     orpnpg        knpg
234100071109     c                   z-add     orpndc        knfv
234200071109     c                   z-add     orppor        kfgs
234300120612     c     kfidst        chain     fidst01l
234400120612     c                   if        %found(fidst01l)
234500071109     c                   eval      w_pdr=dstpdr
234600120612     c                   eval      w_fpp = dstfpp
234700071109     c                   endif
234800071109     c                   endsr
234900131029
235000131029      *------------------------------------------------------------------------*
235100131029      *  Imposto descrizione tasti funzione
235200131029      *------------------------------------------------------------------------*
235300131029     c     Sr_Tastifun   BegSr
235400131029
235500131029      * Conta i caratteri riempiti dalle RigaTf1
235600131029     c                   Clear                   $z
235700131029      * Conta i caratteri riempiti dalle RigaTf2
235800131029     c                   Clear                   $k
235900131029      * Conta i caratteri riempiti dalle RigaTf3
236000131029     c                   Clear                   $j
236100131029      * Conta la posizione del campone da cui partire per impostare
236200131029      * la descrizione del tasto funzione
236300131029     c                   Clear                   $y
236400131029
236500131029      * Stringhe che contengono le descrizioni dei tasti funzione
236600131029     c                   Reset                   rigatf1
236700131029     c                   Reset                   rigatf2
236800131029     c                   Reset                   rigatf3
236900131029
237000131029      * F9 - Visualizza ORM
237100131029     c                   if        not *in42
237200131029     c                   reset                   $tf
237300131029     c                   movea     tf09          $tf
237400131029     c                   exsr      rie_$tf
237500131029     c                   endif
237600131029      * F11 - Accetta Proposta
237700131029     c                   if        not *in42 and not *in35
237800131029     c                   reset                   $tf
237900131029     c                   movea     tf11          $tf
238000131029     c                   exsr      rie_$tf
238100131029     c                   endif
238200131029      * F16 - Riufiuta Proposta
238300131029     c                   if        not *in42
238400131029     c                   reset                   $tf
238500131029     c                   movea     tf16          $tf
238600131029     c                   exsr      rie_$tf
238700131029     c                   endif
238800131029
238900131029      * Pulisco la stringa
239000131029     c                   Eval      $h = 1
239100131029     c                   For       $h by 1 to 62
239200131029     c                   If        rigatf1($h) = '#'
239300131029     c                   Clear                   rigatf1($h)
239400131029     c                   EndIf
239500131029     c                   EndFor
239600131029     c                   Eval      $h = 1
239700131029     c                   For       $h by 1 to 62
239800131029     c                   If        rigatf2($h) = '#'
239900131029     c                   Clear                   rigatf2($h)
240000131029     c                   EndIf
240100131029     c                   EndFor
240200131029     c                   Eval      $h = 1
240300131029     c                   For       $h by 1 to 62
240400131029     c                   If        rigatf3($h) = '#'
240500131029     c                   Clear                   rigatf3($h)
240600131029     c                   EndIf
240700131029     c                   EndFor
240800131029
240900131029      * Imposto la descrizione del tasto funzione F24 e segnalo
241000131029      * quante righe ho riempito e quale devo emettere
241100131029     c                   Select
241200131029
241300131029     c                   When      $k <> *Zeros and $y <> *Zeros
241400131029     c                   Eval      $loop = 3
241500131029     c                   Eval      vzfd02 = cf2413
241600131029
241700131029     c                   When      $k <> *Zeros and $y = *Zeros
241800131029     c                   Eval      $loop = 2
241900131029     c                   Eval      vzfd02 = cf2412
242000131029
242100131029     c                   Other
242200131029
242300131029     c                   Eval      $loop = 1
242400131029     c                   Clear                   vzfd02
242500131029     c                   EndSl
242600131029
242700131029      * Prima riga di tasti funzione
242800131029     c                   Movea     rigatf1       vzfd01
242900131029     c                   Eval      $rig = 1
243000131029
243100131029     c                   EndSr
243200131029
243300131029      *------------------------------------------------------------------------*
243400131029      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
243500131029      *------------------------------------------------------------------------*
243600131029     c     Rie_$tf       BegSr
243700131029      *
243800131029     c                   Eval      $x = 1
243900131029     c     '#'           Lookup    $tf($x)                                30
244000131029     c                   Add       $x            $z
244100131029     c                   If        $Z <= 63
244200131029     c                   Eval      $j = $Z - $x + 1
244300131029     c                   Movea     $tf           rigatf1($j)
244400131029     c                   Else
244500131029     c                   Add       $x            $k
244600131029     c                   If        $k <= 63
244700131029     c                   Eval      $j = $K - $x + 1
244800131029     c                   Movea     $tf           rigatf2($j)
244900131029     c                   Else
245000131029     c                   Add       $x            $y
245100131029     c                   If        $y <= 63
245200131029     c                   Eval      $j = $y - $x + 1
245300131029     c                   Movea     $tf           rigatf3($j)
245400131029     c                   EndIf
245500131029     c                   EndIf
245600131029     c                   EndIf
245700131029
245800131029     c                   EndSr
245900131029
246000130619      *****************************************************************
246100130619      * ROUTINE INIZIALE
246200130619      *****************************************************************
246300130619     C     *INZSR        BEGSR
246400130619
246500130619     C     *ENTRY        PLIST
246600130619     C                   PARM                    KPJBA
246700130619     c                   parm                    fior20ds
246800130619
246900130619      *    Accesso a FNORP (per Subfile)
247000130619
247100130619     C     KFNORP        KLIST
247200130619     C                   kfld                    vs1por
247300130619     C                   kfld                    vidpoe
247400130619     C                   kfld                    vidnsr
247500130619     C                   kfld                    vidnor
247600130619     C                   kfld                    vidnrv
247700130619
247800130619      *    Accesso a FNORP (per Aggiornamento)
247900130619
248000130619     C     KFNORPA       KLIST
248100130619     C                   kfld                    vs1por
248200130619     C                   kfld                    vs1poe
248300130619     C                   kfld                    vs1nsr
248400130619     C                   kfld                    vs1nor
248500130619     C                   kfld                    vs1nrv
248600130619     C                   kfld                    vs1dtvw
248700130619     C                   kfld                    vs1orv
248800130619
248900130619      *    Accesso a FNORP (per Controllo)
249000130619
249100130619     C     KFNORPC       KLIST
249200130619     C                   kfld                    vs1por
249300130619     C                   kfld                    vs1poe
249400130619     C                   kfld                    vs1nsr
249500130619     C                   kfld                    vs1nor
249600130619     C                   kfld                    vs1nrv
249700130619
249800130619      *    Accesso a FNORM
249900130619
250000130619     C     KFNORM        KLIST
250100130619     C                   kfld                    vs1poe
250200130619     C                   kfld                    vs1nsr
250300130619     C                   kfld                    vs1nor
250400130619     C                   kfld                    vs1nrv
250500130619
250600130619      *    Accesso a fidst
250700130619
250800130619     c     kfidst        klist
250900130619     c                   kfld                    knpg
251000130619     c                   kfld                    knfv
251100130619     c                   kfld                    kfgs
251200130619
251300130619     C     Kfapd         klist
251400130619     C                   kfld                    kpdtip
251500130619     C                   kfld                    kpdpdr
251600130619
251700130619      * reperisco data e ora
251800130619     C                   TIME                    W0140
251900130619      * UDATE IN GGMMAAAA
252000130619     C                   MOVE      W0140         WDTGIO
252100130619      * UDATE IN AAAAMMGG
252200130619     C                   Z-ADD     WDTGIO        G02DAT
252300130619     C                   MOVEL     *BLANK        G02ERR
252400130619     C                   CALL      'XSRDA8'
252500130619     C                   PARM                    WLBDAT
252600130619     C                   MOVEL     G02INV        DATEU
252700130619
252800130619      *    Recupero Ragione Sociale Ditta da Emettere a Video
252900130619      *    e Punti Operativi Gestiti
253000130619
253100130619     C                   Z-ADD     1             CODUT             1 0
253200130619
253300130619      * Reperisco dati job
253400130619     c                   exsr      DatiJob
253500130619
253600130619      * controllo abilitazione utente
253700130619     c                   clear                   wabi
253800130619     c                   clear                   dLAT
253900130619     c                   eval      dUTE01 = UTEfaf
254000130619
254100130619      * Verifica errori e autorità profilo
254200130619     c                   SELECT
254300130619      * se ho errori nei dati utente esco dal pgm
254400130619     c                   WHEN      DUTerr = 'E'
254500130619     c                   eval      wErrGrave = *on
254600130619     c                   leavesr
254700130619      * carica abilitazioni del profilo
254800130619     c                   WHEN      §UTEorm <> *blanks
254900130619     c                   eval      wabi = §UTEorm
255000130619     c                   WHEN      UTEaut <> *blanks
255100130619     c                   eval      wabi = UTEaut
255200130619      * se non c'è l'abilitazione
255300130619      * --> se 1° livello, abilitazioni al terminal
255400130619      *     se 2° livello, abilitazioni al punto operativo
255500130619      *     se sede no abilitazioni
255600130619     c                   OTHER
255700130619     c                   IF        DUTlpo = '1'
255800130619     c                   eval      wabi   = 'TP'
255900130619     c                   ENDIF
256000130619     c                   IF        DUTlpo = '2'
256100130619     c                   eval      wabi   = 'PO'
256200130619     c                   ENDIF
256300130619     c                   ENDSL
256400130619
256500130619      * controllo se ok l'abilitazione dell'utente
256600130619     c                   clear                   TIBS02DS
256700130619     c                   eval      T02mod = 'C'
256800130619     c                   eval      T02sif = knsif
256900130619     c                   eval      T02cod = 'LAT'
257000130619     c                   eval      T02ke1 = wabi
257100130619     c                   Call      'TIBS02R'
257200130619     c                   Parm                    KPJBA
257300130619     c                   Parm                    TIBS02DS
257400130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
257500130619     c                   eval      wErrGrave = *on
257600130619     c                   leavesr
257700130619     c                   ENDIF
257800130619     c                   eval      dLAT = T02uni
257900130619
258000130619      * Reperimento delle filiali gestibili dall'utente
258100130619     c                   clear                   TRUL31DS
258200130619     c                   eval      I31abi = wabi
258300130619     c                   eval      I31cdi = DUTdis
258400130619     c                   eval      I31car = DUTare
258500130619     c                   eval      I31cpo = DUTpou
258600130619     c                   call      'TRUL31R'
258700130619     c                   parm                    KPJBA
258800130619     c                   parm                    TRUL31DS
258900130619     c                   IF        O31pog > *zeros
259000130619     c                   movea     O31pog        SKpog
259100130619     c                   ELSE
259200130619     c                   eval      wErrGrave = *on
259300130619     c                   leavesr
259400130619     c                   ENDIF
259500130619
259600130619     c                   endsr
259700001102
259800000928** MSG
259900070418Filiale Errata o non Gestibile                                                      1
260000070418Scelta zona relativa ad altra filiale                                               2
260100001030Zona di Ritiro Errata                                                               3
260200001030Data Formalmente Scorretta                                                          4
260300001030Data Dal Maggiore di Data Al                                                        5
260400001030Inserire Data/e                                                                     6
260500001030Immettere un Valore                                                                 7
260600071126ATTENZIONE ORM ASSEGNATO! Per poterlo chiudere prima si deve disassegnare.          8
260700081030ORM Commissionato. Non si può usare più di 1 causale con resp. al cliente           9
260800001023..............  Libero                                                             10
260900001023..............  Libero                                                             11
261000001023..............  Libero                                                             12
261100001023..............  Libero                                                             13
261200001023..............  Libero                                                             14
261300001023..............  Libero                                                             15
261400001023..............  Libero                                                             16
261500001023..............  Libero                                                             17
261600001023..............  Libero                                                             18
261700001023..............  Libero                                                             19
261800001023..............  Libero                                                             20
261900001023..............  Libero                                                             21
262000001023..............  Libero                                                             22
262100001023..............  Libero                                                             23
262200001023..............  Libero                                                             24
262300001023..............  Libero                                                             25
262400001023..............  Libero                                                             26
262500001023..............  Libero                                                             27
262600001010..............  Libero                                                             28
262700001010..............  Libero                                                             29
262800001010..............  Libero                                                             30
262900020524** CM3
263000030307OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
263100030307 SHARE(*YES)
263200030624** cmA4
263300030624OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
263400030624 SHARE(*YES)          USRDTA('BolAdd_A4')
263500030624**  cmA5
263600030624OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
263700030624 SHARE(*YES)          USRDTA('BolAdd_A5')
263800131029** TF09
263900131029F9=Visualizza ORM #
264000131029** TF11
264100131029F11=Accetta #
264200131029** TF16
264300131029F16=Rifiuta #
