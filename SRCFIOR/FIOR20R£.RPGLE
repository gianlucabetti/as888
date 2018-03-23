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
008900020524
009000020524     D                 DS
009100020524     d  orpcra1                1      3  0
009200020524     d  orpcra2                4      7  0
009300020524     d  orpcra3                8     10  0
009400020524     d  ds_orpcra              1     10  0
009500020524
009600020524     D                 DS
009700020524     d  orpcor1                1      3  0
009800020524     d  orpcor2                4      7  0
009900020524     d  orpcor3                8     10  0
010000020524     d  ds_orpcor              1     10  0
010100020524
010200020524     D                 DS
010300020524     d  orpcrc1                1      3  0
010400020524     d  orpcrc2                4      7  0
010500020524     d  orpcrc3                8     10  0
010600020524     d  ds_orpcrc              1     10  0
010700020524
010800020524     D                 DS
010900020524     D  ds_poe                 1      3  0
011000020524     D  ds_nsr                 4      5  0
011100020524     D  ds_nor                 6     12  0
011200020524     D  ds_nrv                13     14  0
011300020524     D  ds_keyorm              1     14  0
011400010119
011500000927     D KPJBA         E DS
011600140303     D §kpjbu          s                   like(KPJBU)
011700020524
011800010301     D DCMR          E DS
011900031204     d DDFT          e ds                  inz
012000010314     D DFAR          E DS
012100001024     D FIOR05DS      E DS
012200071119     d fior20ds      e ds
012300001102     D TIBS02DS      E DS
012400020524     D DTASV         E DS
012500020524     D DBLP          E DS
012600030624     D TRUL90DS      E DS
012700090202     d ds5P          e ds                  inz
012800090202     d   §5Ppgm      e                     inz('FIOR18R   ')
012900030610     d Dorm01        e ds
013000031204      *
013100031204     d FIOR16DS      e ds                  inz
013200071108     d FIDG09DS      e ds                  inz
013300071108     d og143         e ds
013400070920
013500070920     d TIBS34ds      e ds
013600070920     d AZUTEds       e ds                  extname(AZUTE00F)
013700070920     d DDatiUte      e ds
013800130619     d TRUL31DS      e ds
013900130619
014000130619     d dLAT          e ds
014100130619     d dUTE01        e ds
014200001102
014300000927     D DSUL06        E DS                  EXTNAME(TRUL06DS)
014400000927     D  LIN                    1     90  0
014500000927     D                                     DIM(30)
014600131029
014700131029      // ds per rcd 'O' FNORE - orari apertura
014800131029     d dOREorari     e ds
014900140214
015000140303
015100140303      // ds per note AUT
015200140303     d FIORT1DS      e ds
015300001023
015400001023      * Parametri per controllo Data
015500001023
015600001023     D WLBDAT          DS                  INZ
015700001023     D  G02DAT                 1      8  0
015800001023     D  G02INV                 9     16  0
015900001023     D  G02ERR                17     17
016000001023     D  G02TGI                18     22  0
016100010205
016200010205     D Parm01          DS
016300010205     D  ppoe                          3  0
016400010205     D  pnor                          7  0
016500010205     D  pnsr                          2  0
016600010205     D  pnrv                          2  0
016700010205     D  psce                          1
016800010205     D  pfgs                          3  0
016900010205     D  ppor                          3  0
017000010205     D  pdtr                          8  0
017100010205     D  pmdb                         10
017200010205     D  pprb                         10
017300010205     D  pdts                          8  0
017400010205     D  prmp                          1
017500010205     D  pbrc                          1
017600010205     D  pref                          2
017700010205     D  pmio                          1
017800061204     d  pndc                          6  0
017900061204     d  pddc                          8  0
018000120612     d  pdstfgs                       3  0
018100131029
018200131029      *   C O S T A N T I
018300131029     d cf2413          c                   const('F24=AlTasti(1/3)')
018400131029     d cf2423          c                   const('F24=AlTasti(2/3)')
018500131029     d cf2433          c                   const('F24=AlTasti(3/3)')
018600131029     d cf2412          c                   const('F24=AlTasti(1/2)')
018700131029     d cf2422          c                   const('F24=AlTasti(2/2)')
018800131029
018900131029      // Dspatr "REVERSE IMAGE"
019000131029     d high            c                   const(X'22')
019100131029     d highrimm        c                   const(X'23')
019200140214
019300140214      //---------------------------------------------------------------
019400140214      //?Definizione prototipi utilizzati.
019500140214      //---------------------------------------------------------------
019600140214      /copy gaitrasrc/srcprotopr,tibs02r
019700140303
019800140303      // - Gestione note AUT
019900140303     d fiort1r         pr                  extpgm('FIORT1R')
020000140303     d  kpjba                              likeds(KPJBA)
020100140303     d  fiort1ds                           likeds(FIORT1DS)
020200131029
020300131029      //---------------------------------------------------------------
020400131029      //?Definizione key-list.
020500131029      //---------------------------------------------------------------
020600131029
020700131029       // - File FNORPE1L
020800131029     d k07fnorpe     e ds                  extname(FNORPE1L:*key)
020900131029     d                                     prefix(k_)
021000131029     d                                     inz
021100131029
021200131029       // - File FNORE01L
021300131029     d k05fnore      e ds                  extname(FNORE01L:*key)
021400131029     d                                     prefix(k_)
021500131029     d                                     inz
021600000927
021700000927      *---------------------------------------------------------------*
021800000927      *                                                               *
021900000927      *    UTILIZZO DEGLI INDICATORI                                  *
022000000927      *                                                               *
022100000927      *---------------------------------------------------------------*
022200000927      *                                                               *
022300001027      *    01 - Posizione Cursore su P.O. Ritiro        FIOR201       *
022400001030      *    02 - Posizione Cursore su P.O. Emissione     FIOR201       *
022500001027      *    03 - Disabilito F18                          FIOR201       *
022600131216      *    04 - HI F4-Alert                             FIOR204       *
022700001030      *    05 - Posizione Cursore su Numero             FIOR201       *
022800131216      *    06 - HI F5-Altri dati                        FIOR204       *
022900001027      *    07 - Posizione Cursore su Anno               FIOR201       *
023000140303      *    08 - RI F17-Note AUT                                       *
023100001027      *    09 - Posizione Cursore su Data Variaz. dal   FIOR201       *
023200001027      *    10 - Modificato almeno un Ordine             FIOR201       *
023300001027      *    11 - Rifiutata  almeno una Proposta          FIOR201       *
023400001027      *    12 - Modificato almeno un Ordine             FIORRC1       *
023500001027      *    13 - Rifiutata  almeno una Proposta          FIORRC1       *
023600001027      *    14 - Posizione Cursore su Data Variaz. al    FIOR201       *
023700010209      *    15 - Proposta di chiusura                                  *
023800140303      *    16 - Visualizza 'STAMPATO'
023900140303      *    17 - Visualizza 'CON RICEVUTA DI RITIRO'
024000001027      *    18 - Sproteggo P.O. Gestione                 FIOR201       *
024100140214      *    23 - Abilito F4-Alert                                      *
024200140303      *    24 - Abilito F17-Note AUT                                  *
024300131216      *    28 - Visualizzo messaggio Errore                           *
024400131216      *    33 - Visualizzo messaggio Errore                           *
024500001106      *    34 - R.IMMAGE (AG) Opzione                   FIORSF1       *
024600001106      *    35 - Non ho Scelto la prima Proposta         FIORSF1       *
024700010314      *    36 - Ordine non manutenzionabile             FIORSF1       *
024800070124      *    37 - Ordine in fase 420 non manut.           FIORSF1       *
024900001106      *    39 - Comodo                                                *
025000131216      *    40 -                                         FIOR204       *
025100001030      *    41 - ND Codice Ritiro                        FIORSF1       *
025200001031      *    42 - Visualizzo ORM                          FIOR204       *
025300001031      *    43 - R.IMMAGE (AG) Fase                      FIOR204       *
025400001031      *    44 - R.IMMAGE (AG) Cliente Ritiro            FIOR204       *
025500001031      *    45 - R.IMMAGE (AG) Rag.Soc. Cli.Ritiro       FIOR204       *
025600001031      *    46 - R.IMMAGE (AG) Indirizzo Cli.Ritiro      FIOR204       *
025700001031      *    47 - R.IMMAGE (AG) CAP Cli.Ritiro            FIOR204       *
025800001031      *    48 - R.IMMAGE (AG) Località Cli.Ritiro       FIOR204       *
025900001031      *    49 - R.IMMAGE (AG) Provincia Cli.Ritiro      FIOR204       *
026000001031      *    50 - R.IMMAGE (AG) Nazione Cli.Ritiro        FIOR204       *
026100001031      *    51 - R.IMMAGE (AG) Cliente Ordinante         FIOR204       *
026200001031      *    52 - R.IMMAGE (AG) Rag.Soc. Cli.Ordin.       FIOR204       *
026300001031      *    53 - R.IMMAGE (AG) Indirizzo Cli.Ordin.      FIOR204       *
026400001031      *    54 - R.IMMAGE (AG) CAP Cli.Ordin.            FIOR204       *
026500001031      *    55 - R.IMMAGE (AG) Località Cli.Ordin.       FIOR204       *
026600001031      *    56 - R.IMMAGE (AG) Provincia Cli.Ordin.      FIOR204       *
026700001031      *    57 - R.IMMAGE (AG) Nazione Cli.Ordin.        FIOR204       *
026800001031      *    58 - Visualizzo Nr. Ordine Fisso             FIOR204       *
026900001103      *    59 - R.IMMAGE (AG) Colli                     FIOR204       *
027000001031      *    60 - R.IMMAGE (AG) Peso                      FIOR204       *
027100001031      *    61 - R.IMMAGE (AG) Volume                    FIOR204       *
027200001031      *    62 - R.IMMAGE (AG) Bancali                   FIOR204       *
027300001031      *    63 - R.IMMAGE (AG) Bilico                    FIOR204       *
027400001031      *    64 - R.IMMAGE (AG) Autotreno                 FIOR204       *
027500001031      *    65 - R.IMMAGE (AG) Motrice                   FIOR204       *
027600001031      *    66 - R.IMMAGE (AG) Data Ritiro               FIOR204       *
027700001031      *    68 - R.IMMAGE (AG) Ora Ritiro                FIOR204       *
027800131216      *    69 - r.immage  Orari apertura                FIOR204       *
027900131216      *    70 - r.immage  Orari apertura                FIOR204       *
028000131216      *    71 - r.immage  Orari apertura                FIOR204       *
028100001031      *    72 - R.IMMAGE (AG) Referente                 FIOR204       *
028200001031      *    73 - R.IMMAGE (AG) Telefono                  FIOR204       *
028300001031      *    74 - R.IMMAGE (AG) Natura Merce              FIOR204       *
028400001031      *    75 - R.IMMAGE (AG) Riferimento               FIOR204       *
028500001031      *    76 - R.IMMAGE (AG) Tipo Ordine               FIOR204       *
028600001031      *    77 - R.IMMAGE (AG) Pagamento                 FIOR204       *
028700001031      *    78 - R.IMMAGE (AG) Cliente che Paga          FIOR204       *
028800001031      *    79 - R.IMMAGE (AG) Codice Tariffa            FIOR204       *
028900001031      *    80 - R.IMMAGE (AG) DDT                       FIOR204       *
029000001031      *    81 - R.IMMAGE (AG) P.O. Consegna             FIOR204       *
029100001031      *    82 - R.IMMAGE (AG) Fermo Deposito            FIOR204       *
029200001031      *    83 - R.IMMAGE (AG) Nota 1                    FIOR204       *
029300001031      *    84 - R.IMMAGE (AG) Nota 2                    FIOR204       *
029400001031      *    85 - SFL CLR                                               *
029500001031      *    86 - SFL END                                               *
029600001103      *    88 - R.IMMAGE (AG) Stato                     FIOR204       *
029700010119      *    89 - R.IMMAGE (AG) Sponda idraulica          FIOR204       *
029800001031      *    90 - Emetto Videate                                        *
029900001031      *    92 - R.IMMAGE (AG) Consegna                  FIOR204       *
030000001031      *    93 - R.IMMAGE (AG) Rag.Soc. Consegna         FIOR204       *
030100001031      *    94 - R.IMMAGE (AG) Indirizzo Consegna        FIOR204       *
030200001031      *    95 - R.IMMAGE (AG) CAP Consegna              FIOR204       *
030300001031      *    96 - R.IMMAGE (AG) Località Consegna         FIOR204       *
030400001031      *    97 - R.IMMAGE (AG) Provincia Consegna        FIOR204       *
030500001031      *    98 - R.IMMAGE (AG) Nazione Consegna          FIOR204       *
030600131216      *    99 - r.immage  Orari apertura                FIOR204       *
030700000927      *                                                               *
030800001030      * NB. ... La dicitura "NU" a fianco dell'indicatore vuol dire   *
030900001030      *         che questo è stato definito ma Non è Utilizzato       *
031000001030      *         (previsto per usi futuri).     =     =                *
031100001030      *                                                               *
031200000927      *---------------------------------------------------------------*
031300000927
031400000927      *****************************************************************
031500000927      *                                                               *
031600000927      *    OPERAZIONI INIZIALI                                        *
031700000927      *                                                               *
031800000927      *****************************************************************
031900070920
032000001024
032100001024      *    Inizializzo Campi Video
032200001024
032300000927     C                   EXSR      SR_INIVIDEO
032400000927
032500000927      *****************************************************************
032600000927      *                                                               *
032700000927      *    ROUTINE PRINCIPALE                                         *
032800000927      *                                                               *
032900000927      *****************************************************************
033000001011
033100001019     C                   do        *hival
033200001019
033300001019      *    Emissione Prima Videata
033400001019
033500071119     c                   if        %parms > 1
033600071119     c   kl              leave
033700071119     c                   else
033800130620
033900130620      /free
034000130620       //?Se utente non autorizzato alla funzione esco
034100130620         IF  wErrGrave;
034200130620           *in28 = *on;
034300130620           wEsci = *on;
034400130620           VIDmsg = 'Utente non autorizzato alla funzione';
034500130620         ENDIF;
034600130620      /end-free
034700130620
034800001030     C                   EXFMT     FIOR201
034900071119     c                   endif
035000130620
035100130620      /free
035200130620         IF  wEsci;
035300130620           leave;
035400130620         ENDIF;
035500130620      /end-free
035600130620
035700001019     C   KC              leave
035800001030     C                   setoff                                       281890
035900001030     C                   setoff                                       010204
036000001030     C                   setoff                                       050607
036100001030     C                   setoff                                       080914
036200071126     c                   clear                   vidmsg
036300001019
036400001019      *    Controllo
036500001019
036600001019     C                   EXSR      SR_CONTROL
036700001019
036800130412      *    Per Errori/Cambio P.O. - Riemetto
036900001019
037000001019     C     *in28         ifeq      *on
037100001019     C     *in18         oreq      *on
037200001030     C     *in90         oreq      *on
037300001019     C                   iter
037400001019     C                   endif
037500001023
037600001030      *   GESTIONE DELLE PROPOSTE
037700001023
037800001023     C                   EXSR      SR_GESTIONE
037900001023
038000001019     C                   enddo
038100001019
038200000927     C                   eval      *inlr = *on
038300000927
038400000927      *****************************************************************
038500000927      *                                                               *
038600000927      *    SUBROUTINES                                                *
038700000927      *                                                               *
038800000927      *****************************************************************
038900070920
039000070920      *---------------------------------------------------------------*
039100070920      * Reperimento Dati del job (Utente/Operativi)                   *
039200070920      *---------------------------------------------------------------*
039300070920     c     DatiJob       BEGSR
039400070920      *
039500070920     c     *dtaara       define    §azute        azuteds
039600070920     c     *dtaara       define    §datiute      ddatiute
039700070920      *
039800070920     c                   in(E)     *dtaara
039900070920     c                   IF        %ERROR or RSUT = *blanks
040000070920     c                   clear                   Tibs34Ds
040100070920     c                   call      'TIBS34R'
040200070920     c                   parm                    Tibs34Ds
040300070920     c                   in        *dtaara
040400070920     c                   ENDIF
040500070920      *
040600070920     c                   ENDSR
040700000927
040800000927      *---------------------------------------------------------------*
040900000927      *    Inizializzazione Campi Video                               *
041000000927      *---------------------------------------------------------------*
041100000927
041200000927     C     SR_INIVIDEO   begsr
041300000927
041400001030      *    Reset Progressivo Nr. Proposte Confermate/Rifiutate
041500001025
041600001026     C                   setoff                                       10
041700001030     C                   setoff                                       11
041800001025     C                   clear                   vconfe
041900001030     C                   clear                   vrifiu
042000001002
042100001026      *    Inizializzo il P.O. Ritiro
042200071119     c                   if        %parms > 1
042300071119     c                   eval      vidpor = i20fgs
042400071119     c                   else
042500070920     c                   eval      vidpor = dutpou
042600071119     c                   endif
042700001012
042800001026      *    Decodifico il P.O. Ritiro
042900000927
043000001026     C     vidpor        chain     azorg01l
043100000927     C                   if        %found(azorg01l)
043200000927     C                   movel     orgdes        viddep
043300000927     C                   else
043400001019     C                   clear                   viddep
043500000927     C                   endif
043600031204
043700031204      * Recupero i dati di default prima con il mio p.o.
043800031204     C                   clear                   TIBS02DS
043900031204     C                   clear                   dDFT
044000031204     C                   movel     'C'           T02mod
044100031204     C                   movel     KNSIF         T02sif
044200031204     C                   movel     'DFT'         t02cod
044300031204     C                   movel(p)  VIDpor        T02ke1
044400031204     C                   call      'TIBS02R'
044500031204     C                   parm                    KPJBA
044600031204     C                   parm                    TIBS02DS
044700031204if  1C                   if        T02err =  *blanks
044800031204     C                   movel     T02uni        dDFT
044900031204x   1C                   else
045000031204      * poi con 999 generico se non trovo con il mio p.o.
045100031204     C                   clear                   TIBS02DS
045200031204     C                   movel     'C'           T02mod
045300031204     C                   movel     KNSIF         T02sif
045400031204     C                   movel     'DFT'         T02cod
045500031204     C                   movel(p)  '999'         T02ke1
045600031204     C                   call      'TIBS02R'
045700031204     C                   parm                    KPJBA
045800031204     C                   parm                    TIBS02DS
045900031204if  2C                   if        T02err =  *blanks
046000031204     C                   movel     T02uni        dDFT
046100031204e   2C                   endif
046200031204e   1C                   endif
046300000927
046400000927      *    Inizializzo i Campi Restanti
046500001012
046600001030     C                   clear                   vidpoe
046700001030     C                   clear                   vidnsr
046800001030     C                   clear                   vidnor
046900001030     C                   clear                   vidnrv
047000001030     C                   clear                   viddv1
047100071119     c                   if        %parms > 1
047200071119     c                   eval      viddv2 = i20data
047300071119     c                   else
047400001030     C                   move      *date         viddv2
047500071119     c                   endif
047600000927
047700000927     C                   endsr
047800001019
047900001019      *---------------------------------------------------------------*
048000001019      *    Controllo Prima Videata                                    *
048100001019      *---------------------------------------------------------------*
048200001019
048300001019     C     SR_CONTROL    begsr
048400001019
048500001025     C                   z-add     0             vconfn
048600001030     C                   z-add     0             vrifin
048700001023
048800001026      *    F18=Cambio P.O. Ritiro
048900001019
049000001019     C     *inks         ifeq      *on
049100001019     C                   seton                                        18
049200001019     C                   goto      endctr
049300001019     C                   endif
049400001023
049500001026      *    Controllo P.O. Ritiro
049600001023
049700001023     C                   clear                   viddep
049800130620     c                   move      VIDpor        w003a
049900130620     c     w003a         lookup    SKpog                                  39
050000001023     C     *in39         ifeq      *off
050100001023     C                   eval      *in01 = *on
050200001023     C                   eval      *in28 = *on
050300001023     C                   eval      *in18 = *on
050400001023     C                   movel     msg(1)        vidmsg
050500001023     C                   goto      endctr
050600001023     C                   else
050700001026     C     vidpor        chain     azorg01l
050800001023     C                   if        %found(azorg01l)
050900001023     C                   movel     orgdes        viddep
051000001023     C                   endif
051100001025      *        Carico £6
051200001025     C                   CLEAR                   DSUL06
051300001025     C                   MOVE      '£6'          D06COD
051400001026     C                   MOVEL     vidpor        D06KEY
051500001025     C                   MOVEL     DSUL06        KPJBU
051600001025     C                   CALL      'TRUL06R'
051700001025     C                   PARM                    KPJBA
051800001025     C                   MOVEL     KPJBU         DSUL06
051900001025     C                   MOVEA     LIN           L6
052000070920
052100070920      * Recupero i dati di default prima con il mio p.o.
052200070920     C                   clear                   TIBS02DS
052300070920     C                   clear                   dDFT
052400070920     C                   movel     'C'           T02mod
052500070920     C                   movel     KNSIF         T02sif
052600070920     C                   movel     'DFT'         t02cod
052700070920     C                   movel(p)  VIDpor        T02ke1
052800070920     C                   call      'TIBS02R'
052900070920     C                   parm                    KPJBA
053000070920     C                   parm                    TIBS02DS
053100070920if  1C                   if        T02err =  *blanks
053200070920     C                   movel     T02uni        dDFT
053300070920x   1C                   else
053400070920      * poi con 999 generico se non trovo con il mio p.o.
053500070920     C                   clear                   TIBS02DS
053600070920     C                   movel     'C'           T02mod
053700070920     C                   movel     KNSIF         T02sif
053800070920     C                   movel     'DFT'         T02cod
053900070920     C                   movel(p)  '999'         T02ke1
054000070920     C                   call      'TIBS02R'
054100070920     C                   parm                    KPJBA
054200070920     C                   parm                    TIBS02DS
054300070920if  2C                   if        T02err =  *blanks
054400070920     C                   movel     T02uni        dDFT
054500070920e   2C                   endif
054600070920e   1C                   endif
054700001023     C                   endif
054800001023
054900001030      *    Se immesso P.O. immettere Numero
055000001023
055100001030     C     vidpoe        ifne      0
055200001030     C     vidnor        andeq     0
055300001030     C                   seton                                        05  28
055400001030     C                   movel     msg(7)        vidmsg
055500001030     C                   goto      endctr
055600001030     C                   endif
055700001030
055800001030      *    Se immesso Numero immettere P.O.
055900001030
056000001030     C     vidpoe        ifeq      0
056100001030     C     vidnor        andne     0
056200001030     C                   seton                                        02  28
056300001030     C                   movel     msg(7)        vidmsg
056400001030     C                   goto      endctr
056500001030     C                   endif
056600001030
056700001030      *    Se immesso Viaggio o Serie Immettere P.O. e Numero
056800001030
056900001030     C     vidnrv        ifne      0
057000001030     C     vidnsr        orne      0
057100001030     C     vidpoe        ifeq      0
057200001030     C                   seton                                        02  28
057300001030     C                   movel     msg(7)        vidmsg
057400001030     C                   goto      endctr
057500001030     C                   endif
057600001030     C     vidnor        ifeq      0
057700001030     C                   seton                                        05  28
057800001030     C                   movel     msg(7)        vidmsg
057900001030     C                   goto      endctr
058000001030     C                   endif
058100001030     C                   endif
058200001030
058300001114      *    Accesso diretto per Numero ORM
058400001030
058500001030     C     vidpoe        ifne      0
058600001030     C     vidnsr        orne      0
058700001030     C     vidnor        orne      0
058800001030     C     vidnrv        orne      0
058900131216     c                   eval      wReadxOrm = *on
059000131216     C********           seton                                        40        x numero orm
059100001120     C                   else
059200131216     c                   eval      wReadxOrm = *off
059300131216     C********           setoff                                       40
059400001114     C                   endif
059500001030
059600001030      *    Controllo / Inversione Date Variazione
059700001030
059800001030     C                   clear                   wdatainv1
059900001030     C     viddv1        ifne      0
060000001030     C                   z-add     viddv1        G02DAT
060100001030     C                   clear                   G02ERR
060200001030     C                   call      'XSRDA8'
060300001030     C                   parm                    WLBDAT
060400001030     C     g02err        ifeq      '1'
060500001030     C                   seton                                        09  28
060600001030     C                   movel     msg(4)        vidmsg
060700001030     C                   goto      endctr
060800001030     C                   else
060900001030     C                   z-add     g02inv        wdatainv1         8 0
061000001030     C                   z-add     g02dat        viddv1
061100001030     C                   endif
061200001030     C                   endif
061300001030
061400001030     C                   clear                   wdatainv2
061500001030     C     viddv2        ifne      0
061600001030     C                   z-add     viddv2        G02DAT
061700001030     C                   clear                   G02ERR
061800001030     C                   call      'XSRDA8'
061900001030     C                   parm                    WLBDAT
062000001030     C     g02err        ifeq      '1'
062100001030     C                   seton                                        14  28
062200001030     C                   movel     msg(4)        vidmsg
062300001030     C                   goto      endctr
062400001030     C                   else
062500001030     C                   z-add     g02inv        wdatainv2         8 0
062600001030     C                   z-add     g02dat        viddv2
062700001030     C                   endif
062800001030     C                   endif
062900001023
063000001030     C     wdatainv1     ifgt      wdatainv2
063100001030     C                   seton                                        14  28
063200001030     C                   movel     msg(5)        vidmsg
063300001030     C                   goto      endctr
063400001030     C                   endif
063500001030
063600001030     C     wdatainv2     ifeq      0
063700001030     C                   seton                                        14  28
063800001030     C                   movel     msg(6)        vidmsg
063900001030     C                   goto      endctr
064000001030     C                   endif
064100001030
064200001019     C     ENDCTR        endsr
064300001023
064400001023      *---------------------------------------------------------------*
064500001023      *    Lettura / Gestione degli ORM                               *
064600001023      *---------------------------------------------------------------*
064700001023
064800001023     C     SR_GESTIONE   begsr
064900001023
065000001030      *    Pulisco Subfile e mi posiziono per Lettura Proposte
065100001024
065200031204do  1C                   do        *hival
065300001023     C                   z-add     0             nn1               4 0
065400001023     C                   eval      *in85 = *on
065500001023     C                   write     fiorrc1
065600001023     C                   eval      *in85 = *off
065700001023     C                   z-add     1             recsf
065800001106     C                   setoff                                       33  34
065900001030
066000001030      *    Carico Proposte L6
066100001030
066200031204do  2C     1             do        30            zz                3 0
066300001030
066400001030     C     L6(zz)        ifeq      0
066500001030     C                   iter
066600001030     C                   endif
066700001030
066800001030     C                   z-add     L6(zz)        vs1por
066900001025
067000131216     c                   IF        wReadxOrm
067100131216     C     KFNORP        setll     fnorp02l
067200131216     c                   else
067300131216     C     vs1por        setll     fnorp02l
067400131216     c                   ENDIF
067500001023
067600001030      *    Leggo Proposte
067700001023
067800031204do  3C                   do        *hival
067900131216     c                   IF        wReadxOrm
068000131216     C     KFNORP        reade(n)  fnorp02l
068100131216     c                   else
068200131216     C     vs1por        reade(n)  fnorp02l
068300131216     c                   ENDIF
068400001023
068500001030     C                   if        %eof(fnorp02l)
068600001023     C                   leave
068700001023     C                   endif
068800001023
068900001026     C     nn1           ifge      8000
069000001024     C                   leave
069100001024     C                   endif
069200001024
069300001030      *    Controllo parametri prima videata
069400130412      *    Date
069500001030
069600001030     C     wdatainv1     ifne      0
069700001030     C     wdatainv1     andgt     orpdtv
069800001030     C                   iter
069900001030     C                   endif
070000001030     C     wdatainv2     iflt      orpdtv
070100001030     C                   iter
070200001030     C                   endif
070300001030
070400001026      *    SCRIVO SUBFILE
070500001024
070600001026     C                   clear                   vs1opz
070700001030     C                   move      orppoe        vs1poe
070800001030     C                   move      orpnsr        vs1nsr
070900001030     C                   move      orpnor        vs1nor
071000001030     C                   move      orpnrv        vs1nrv
071100001030     C                   move      orpcra        vs1cro
071200001030     C     vs1cro        ifeq      0
071300001030     C                   seton                                        41
071400001030     C                   else
071500001030     C                   setoff                                       41
071600001030     C                   endif
071700001030     C                   movel     orprsr        vs1rsr
071800001030     C                   movel     orpinr        vs1inr
071900001030     C                   movel     orplor        vs1lor
072000001030     C                   movel     orpprr        vs1prr
072100001030     C                   movel     orpnar        vs1nar
072200001030     C                   movel     orputv        vs1ute
072300001030     C                   move      orporv        vs1orv
072400001102     C                   move      orpdtv        vs1dtvw
072500001102     C                   movel     orpdtv        wa4               4
072600001030     C                   move      wa4           vs1dtv
072700001030     C                   move      orpdtv        wa4
072800001030     C                   move      wa4           wg2               2
072900001030     C                   movel     wa4           wm2               2
073000001030     C                   move      wm2           wa4
073100001030     C                   movel     wg2           wa4
073200001030     C                   movel     wa4           vs1dtv
073300001024
073400001024     C                   add       1             nn1
073500001024     C                   write     fiorsf1
073600001024
073700031204e   3C                   enddo
073800031204e   2C                   enddo
073900001023
074000001024      *    EMISSIONE RECORD DI CONTROLLO
074100001024
074200001025     C     vconfn        ifeq      0
074300001025     C                   eval      *in12 = *off
074400001025     C                   else
074500001025     C                   eval      *in12 = *on
074600001025     C                   endif
074700001030     C     vrifin        ifeq      0
074800001030     C                   eval      *in13 = *off
074900001030     C                   else
075000001030     C                   eval      *in13 = *on
075100001030     C                   endif
075200001025
075300001024     C     nn1           ifeq      0
075400001030     C                   exfmt     fior203
075500001024     C                   else
075600001030     C                   write     fior202
075700001024     C                   exfmt     fiorrc1
075800001024     C                   endif
075900071126
076000071126     c                   eval      *in28 = *off
076100071126     c                   clear                   vidmsg
076200001024
076300001025     C                   z-add     0             vconfn
076400001030     C                   z-add     0             vrifin
076500001025
076600001024      *    LETTURA RECORDS SUBFILE
076700001024
076800031204if  2C     *inkl         ifeq      *off
076900001024
077000031204do  3C                   do        *hival
077100001024
077200001030     C     nn1           ifne      0
077300001024     C                   readc     fiorsf1
077400001024     C                   if        %eof
077500001024     C                   leave
077600001024     C                   endif
077700001030     C                   else
077800001030     C                   leave
077900001030     C                   endif
078000001106
078100010314      *    Controllo Se Ordine è Manutenzionabile
078200001103
078300070124     C                   setoff                                       353637
078400081125     c                   eval      wnobolla = *off
078500001106     C     KFNORM        chain     fnorm01l
078600001106     C                   if        %found(fnorm01l)
078700010314     C                   exsr      Sr_Fase
078800010314     C     d§farman      ifne      'S'
078900001106     C                   seton                                        35  36
079000001106     C                   endif
079100070123      * Non posso accettare una variazione se ORM in fase 420
079200070124     c                   if        ormfao = 420 and *in35 = *off
079300070124     c                   seton                                        35  37
079400070123     c                   endif
079500081030     c                   eval      dorm01 = ormflo
079600001106     C                   endif
079700081030      * controllo se posso utilizzare la causale inserita per la
079800081030      * chiusura dell'ORM
079900081030if  8c                   if        orpfao = 999  or OrpFao = 910
080000081030     c                             and not *in35
080100081030     c                   exsr      sr_ctrchiu
080200081030     c                   endif
080300001106
080400001106      *    Devo Scegliere la PRIMA Proposta
080500001106
080600001106     C     *in35         ifeq      *off
080700001106     C     KFNORPA       setll     fnorp02l
080800001106     C                   eval      *in35 = *on
080900001106     C     KFNORPC       readpe(n) fnorp02l
081000001106     C                   if        %eof(fnorp02l)
081100001106     C                   eval      *in35 = *off
081200001106     C                   endif
081300001106     C                   endif
081400001106
081500001103      *    Visualizzazione Proposta
081600001103      *    ------------------------
081700001106
081800130412     c                   IF        vs1opz='1'
081900001102     C                   EXSR      SR_VISUALIZZA
082000001102     C                   endif
082100001106
082200031204e   3C                   enddo
082300001024
082400031204x   2C                   else
082500001024     C                   leave
082600031204e   2C                   endif
082700001024
082800031204e   1C                   enddo
082900031204
083000031204      * Chiudo i printer file eventualmente aperti per gli O.R.M.
083100031204if  1c                   if            $Fior05 = *on
083200031204     c                             and DateU  >= D§DFTdsl
083300031204     c                   clear                   FIOR16ds
083400031204     c                   eval      R16tla  = 'C'
083500031204     c                   movel(p)  FIOR16DS      KPJBU
083600090202     c                   call      §5Ppgm
083700031204     c                   parm                    KPJBA
083800031204e   1c                   endif
083900031204
084000001023     C                   endsr
084100010314
084200010314      *---------------------------------------------------------------*
084300010314      *    Flag fase                                                  *
084400010314      *---------------------------------------------------------------*
084500010314
084600010314     C     Sr_Fase       begsr
084700010314
084800010314     C                   clear                   TIBS02DS
084900010314     C                   movel     'C'           T02mod
085000010314     C                   movel     knsif         t02sif
085100010314     C                   movel     'FAR'         t02cod
085200010314     C                   movel(p)  ormfao        T02ke1
085300010314     C                   call      'TIBS02R'
085400010314     C                   parm                    KPJBA
085500010314     C                   parm                    TIBS02DS
085600010314     C                   movel     T02uni        dfar
085700010314
085800020524     C                   endsr
085900081030
086000081030      *---------------------------------------------------------------*
086100081030      * Controllo se posso chiudere l'ORM con la cau immessa          *
086200081030      *---------------------------------------------------------------*
086300081030     c     sr_ctrchiu    begsr
086400081030
086500081030      * controllo la causale immessa in proposta
086600081030     c                   clear                   dcmr
086700081030     c                   clear                   tibs02ds
086800081030     c                   eval      t02mod = 'C'
086900081030     c                   eval      t02sif = knsif
087000081030     c                   eval      t02cod = 'CMR'
087100081030     c                   eval      t02ke1= orpcau
087200081030     c                   call      'TIBS02R'
087300081030     c                   parm                    kpjba
087400081030     c                   parm                    tibs02ds
087500081030     c                   movel     t02uni        dcmr
087600081030
087700081125      * se causale inserita genera bolla di addebito (colpa cliente)
087800081030      * controllo che l'ORM non abbia già avuto una causale che genera addebito
087900081125      * in questo caso faccio solo un addebito
088000081125     c                   if        d§cmradd = 'S'
088100081030     c     kfnorm        setll     fnorf01l
088200081030     c                   do        *hival
088300131029     c     kfnorm        reade     fnorf01l
088400081030     c                   if        %eof (fnorf01l)
088500081030     c                   leave
088600081030     c                   endif
088700081030      * senza causale rileggo
088800081030     c                   if        orfcar = *blanks
088900081030     c                   iter
089000081030     c                   endif
089100081030      * fase da PDA rileggo
089200081030     c                   if        orffar = 410
089300081030     c                   iter
089400081030     c                   endif
089500081030     c                   clear                   dcmr
089600081030     c                   clear                   tibs02ds
089700081030     c                   eval      t02mod = 'C'
089800081030     c                   eval      t02sif = knsif
089900081030     c                   eval      t02cod = 'CMR'
090000081030     c                   eval      t02ke1 = orfcar
090100081030     c                   call      'TIBS02R'
090200081030     c                   parm                    kpjba
090300081030     c                   parm                    tibs02ds
090400081030     c                   eval      dcmr = t02uni
090500081030      * se causale che genera bolla esco ed emetto msg di errore
090600081030     c                   if        d§cmradd = 'S'
090700081125     c                   eval      wnobolla = *on
090800081030     c                   leavesr
090900081030     c                   endif
091000081030     c                   enddo
091100081030     c                   endif
091200081030
091300081030     c                   endsr
091400001103
091500020524      *---------------------------------------------------------------*
091600020524      * Controllo se devo generare la bolla di addebito
091700020524      *---------------------------------------------------------------*
091800020524     c     sr_contradd   begsr
091900020524
092000020524     c                   clear                   sav_lpksc
092100081125
092200081125      * se non devo generare la bolla di addebito vado a fine e imposto il flag
092300081125      * come se avessi fatto la bolla
092400081125     c                   if        wnobolla = *on
092500081125     c                   eval      wbolla = *on
092600081125     c                   leavesr
092700081125     c                   endif
092800020524
092900020524      * se cod.ordinante codificato addebito
093000020524 b1  c                   if        orpcor <> *zeros and orpcor2 <> 8888
093100020524     c                             and orpcor2 <> 9999
093200020524     c                   clear                   dblp
093300090505     c                   movel     orpcor        §lpksca
093400020702      * se c'è imposto il codice bollettazione
093500020702     c     orpcor        chain     fnacr01l
093600020702     c                   if        %found(fnacr01l) and acratb = *blanks
093700020702     c                             and acrksc <> *zeros
093800090505     c                   move      acrksc        §lpksca
093900090505     c                   if        acrccc<>999
094000090505     c                   move      acrccc        §lpctra
094100090505     C                   endif
094200020702     c                   endif
094300020524     c                   eval      §lprsd = orprso
094400020524     c                   eval      §lpind = orpino
094500020524     c                   eval      §lpcad = orpcao
094600020524     c                   eval      §lplod = orploo
094700020524     c                   eval      §lpprd = orppro
094800020524     c                   eval      §lpnzd = orpnao
094900020524     c                   exsr      sr_wrtbolla
095000020524 x1  c                   else
095100020524      * se non inserito l'ordinante
095200020524 b2  c                   if        orprso = *blanks
095300020524      * orm non commissionato
095400020524 b3  c                   if        orppoe = orppor
095500020524      * se cod.mittente codificato addebito
095600020524 b4  c                   if        orpcra <> *zeros and orpcra2 <> 8888
095700020524     c                             and orpcra2 <> 9999
095800020524     c                   clear                   dblp
095900090505     c                   movel     orpcra        §lpksca
096000020702      * se c'è imposto il codice bollettazione
096100020702     c     orpcra        chain     fnacr01l
096200020702     c                   if        %found(fnacr01l) and acratb = *blanks
096300020702     c                             and acrksc <> *zeros
096400090505     c                   move      acrksc        §lpksca
096500090505     c                   if        acrccc<>999
096600090505     c                   move      acrccc        §lpctra
096700020702     c                   endif
096800090505     c                   endif
096900020524     c                   eval      §lprsd = orprsr
097000020524     c                   eval      §lpind = orpinr
097100020524     c                   eval      §lpcad = orpcar
097200020524     c                   eval      §lplod = orplor
097300020524     c                   eval      §lpprd = orpprr
097400020524     c                   eval      §lpnzd = orpnar
097500020524     c                   exsr      sr_wrtbolla
097600020524      * se cod.mittente non codificato
097700020524 x4  c                   else
097800020524      * se cod.destinatario codificato addebito
097900020524 b5  c                   if        orpcrc <> *zeros and orpcrc2 <> 8888
098000020524     c                             and orpcrc2 <> 9999
098100020524     c                   clear                   dblp
098200090505     c                   movel     orpcrc        §lpksca
098300020702      * se c'è imposto il codice bollettazione
098400020702     c     orpcrc        chain     fnacr01l
098500020702     c                   if        %found(fnacr01l) and acratb = *blanks
098600020702     c                             and acrksc <> *zeros
098700090505     c                   move      acrksc        §lpksca
098800090505     c                   if        acrccc<>999
098900090505     c                   move      acrccc        §lpctra
099000020702     c                   endif
099100090505     c                   endif
099200020524     c                   eval      §lprsd = orprsc
099300020524     c                   eval      §lpind = orpinc
099400020524     c                   eval      §lpcad = orpcac
099500020524     c                   eval      §lplod = orploc
099600020524     c                   eval      §lpprd = orpprc
099700020524     c                   eval      §lpnzd = orpnac
099800020524     c                   exsr      sr_wrtbolla
099900020524 e5  c                   endif
100000020524 e4  c                   endif
100100020524      * orm commissionato
100200020524 x3  c                   else
100300020524      * se cod.destinatario codificato e p.o. cod.destinatario = p.o. emissione addebito
100400020524 b4  c                   if        orpcrc <> *zeros and orpcrc2 <> 8888
100500020524     c                             and orpcrc2 <> 9999  and orpcrc1 = orppoe
100600020524     c                   clear                   dblp
100700090505     c                   movel     orpcrc        §lpksca
100800020702      * se c'è imposto il codice bollettazione
100900020702     c     orpcrc        chain     fnacr01l
101000020702     c                   if        %found(fnacr01l) and acratb = *blanks
101100020702     c                             and acrksc <> *zeros
101200090505     c                   move      acrksc        §lpksca
101300090505     c                   if        acrccc<>999
101400090505     c                   move      acrccc        §lpctra
101500020702     c                   endif
101600090505     c                   endif
101700020524     c                   eval      §lprsd = orprsc
101800020524     c                   eval      §lpind = orpinc
101900020524     c                   eval      §lpcad = orpcac
102000020524     c                   eval      §lplod = orploc
102100020524     c                   eval      §lpprd = orpprc
102200020524     c                   eval      §lpnzd = orpnac
102300020524     c                   exsr      sr_wrtbolla
102400020524      * se non codificato o p.o. differenti
102500020524 x4  c                   else
102600020524      * se cod.mittente codificato addebito
102700020524 b5  c                   if        orpcra <> *zeros and orpcra2 <> 8888
102800020524     c                             and orpcra2 <> 9999
102900020524     c                   clear                   dblp
103000090505     c                   movel     orpcra        §lpksca
103100020702      * se c'è imposto il codice bollettazione
103200020702     c     orpcra        chain     fnacr01l
103300020702     c                   if        %found(fnacr01l) and acratb = *blanks
103400020702     c                             and acrksc <> *zeros
103500090505     c                   move      acrksc        §lpksca
103600090505     c                   if        acrccc<>999
103700090505     c                   move      acrccc        §lpctra
103800020702     c                   endif
103900090505     c                   endif
104000020524     c                   eval      §lprsd = orprsr
104100020524     c                   eval      §lpind = orpinr
104200020524     c                   eval      §lpcad = orpcar
104300020524     c                   eval      §lplod = orplor
104400020524     c                   eval      §lpprd = orpprr
104500020524     c                   eval      §lpnzd = orpnar
104600020524     c                   exsr      sr_wrtbolla
104700020524 e5  c                   endif
104800020524 e4  c                   endif
104900020524 e3  c                   endif
105000020524 e2  c                   endif
105100020524 e1  c                   endif
105200020524
105300020524     c                   endsr
105400020524      *---------------------------------------------------------------*
105500020524      * Richiamo il pgm fnls01r per scrivere la bolla di abbebito
105600020524      *---------------------------------------------------------------*
105700020524     c     sr_wrtbolla   begsr
105800020524
105900020524      * richiamo il programma per la scelta delle stampanti
106000020524     c                   exsr      sr_scestp
106100020530     c   91              eval      okbolla = '1'
106200020530     c   91              goto      endwrtbolla
106300020524
106400020524     c                   clear                   kpjbu
106500020524     c                   clear                   dtasv
106600020524
106700020524     c                   eval      §lpfpr = 'T'
106800020524     c                   movel     orppor        §lpccn
106900020524     c                   move      9999          §lpccn
107000020524     c                   movel     dateu         §lpaas
107100020524     c                   move      dateu         §lpmgs
107200020524     c                   eval      §lplnp = orppor
107300020524     c                   eval      §lpcbo = 'FY'
107400020524     c                   eval      §lpsop = '+'
107500020530     c                   eval      §lplna = orppoe
107600020524     c                   eval      §lprsm = orprsr
107700020524     c                   eval      §lpinm = orpinr
107800020524     c                   eval      §lpcam = orpcar
107900020524     c                   eval      §lplom = orplor
108000020524     c                   eval      §lpprm = orpprr
108100020524     c                   eval      §lpnzm = orpnar
108200020524
108300020524 b1  c                   if        orpncl <> 0
108400020524     c                   eval      §lpncl = orpncl
108500020524 x1  c                   else
108600020524     c                   eval      §lpncl = 1
108700020524 e1  c                   endif
108800020524 b1  c                   if        orppkg <> 0
108900020524     c                   eval      §lppkb = orppkg
109000020524 x1  c                   else
109100070920 b3  c                   if        orpbnc <> 0
109200070920     c                   eval      wpkb = orpbnc / d§dftbnc * d§dftpkg
109300070920     c                   if        wpkb > 999999,9
109400070920     c                   eval      wpkb = 999999,9
109500070920     c                   endif
109600070920     c                   eval      §lppkb = wpkb
109700070920 x2  c                   else
109800070920 b2  c                   if        orpvlm <> 0
109900070920     c                   eval      wpkb = orpvlm * d§dftpkg
110000070920     c                   if        wpkb > 999999,9
110100070920     c                   eval      wpkb = 999999,9
110200070920     c                   endif
110300070920     c                   eval      §lppkb = wpkb
110400020524 e3  c                   endif
110500020524 e2  c                   endif
110600020524 e1  c                   endif
110700020524
110800020524     c                   clear                   ds_keyorm
110900020524     c                   eval      ds_poe = ORppoe
111000020524     c                   eval      ds_nsr = ORpnsr
111100020524     c                   eval      ds_nor = ORpnor
111200020524     c                   eval      ds_nrv = ORpnrv
111300041201     c                   move      ds_keyorm     §lprmn
111400020524
111500020524     c                   eval      §lprma = orprfa
111600020524     c                   eval      §lprmo = orpcau + '-' + d§cmrdes1
111700020524     c                   eval      §lpctm = '4F'
111800020524     c                   movel     dateu         §lpdrt
111900020524
112000020524      * Cerca codice autotrasportatore e flag mattino/pomeriggio
112100071109 b1  c                   if        orpndc <> 0 and vs1opz='2'
112200071109     c                   exsr      sr_carpdr
112300071109 e1  c                   endif
112400071109     c                   if        W_pdr=*zeros or orpndc=0
112500020524     c                   movel     §lplnp        §lppdr
112600020524     c                   move      0999          §lppdr
112700020524     c                   eval      §lpfpp = 'M'
112800071113     c                   else
112900071113     c                   eval      §lppdr = w_pdr
113000071113     c                   eval      §lpfpp = w_fpp
113100020524 e1  c                   endif
113200020524
113300020524     C                   call      'FNLS01R'
113400020524     C                   parm                    kpjba
113500020524     C                   parm                    dblp
113600020524     C                   parm                    dtasv
113700030624     c                   Parm                    Trul90ds
113800020524
113900020524     C                   if        §lpfpr = '4' or §lpfpr = '3'
114000020524     C                             or §lpfpr = '5'
114100020524     C                   eval      okbolla = '1'
114200020524     c                   else
114300020524     c                   eval      wbolla = *on
114400020524     C                   endif
114500020524
114600020524     c     endwrtbolla   endsr
114700020524      *---------------------------------------------------------------*
114800020524      * scelta stampante per immissione bolla
114900020524      *---------------------------------------------------------------*
115000020524     c     sr_scestp     begsr
115100020524
115200020524     C                   clear                   kpjbu
115300020530     c                   setoff                                       91
115400020524
115500020524      * Richiesta stampanti
115600020524     C                   if        conta = *zeros
115700030624     C                   clear                   trul90ds
115800030624     C                   movel     'S'           D90rse
115900030624     C                   movel     'S'           D90rsb
116000030624     C                   call      'TRUL90R'
116100020524     C                   parm                    kpjba
116200030624     C                   parm                    trul90ds
116300020524      * F3=Fine
116400030624     C                   if        D90f3 = '1'
116500020530     C                   eval      *in91 = *on
116600020524     C                   goto      endstp
116700020524     C                   endif
116800090202
116900090202      /free
117000090202         reset ds5P;
117100090202         if  %subst(knmus : 1 : 3) = 'EDP';
117200090202           TBLkey = 'O' + 'EDP';
117300090202         else;
117400090202           TBLkey = 'O' + %editc( DUTpou : 'X' );
117500090202         endif;
117600090202         chain  ( CodUt : '5P' : TBLkey )  TABEL;
117700090202         if  %found( TABEL00F );
117800090202           ds5P = TBLuni;
117900090202         endif;
118000090202      /end-free
118100030307
118200030307     C                   z-add     103           lung
118300020524
118400020524      * Ovrprtf Segnacolli
118500030624     C                   movea     D90pre        cm3(28)
118600030624     C                   movea     D90mde        cm3(50)
118700020524     C                   clear                   comman
118800020524     C                   movea     cm3           comman
118900020530     C                   CALL      'QCMDEXC'                            91
119000020524     C                   PARM                    comman
119100020524     C                   PARM                    lung
119200020530     C   91              goto      endstp
119300030624
119400030624      * Bolle - Nuovo modulo
119500030624
119600030624     c                   Z-Add     110           lung
119700030624
119800030624      * Ovrprtf Bolle A4
119900030624     c                   Movea     D90prb4       cma4(30)
120000030624     c                   Movea     D90mdb4       cma4(52)
120100030624     c                   Clear                   Comman
120200030624     c                   Movea     cmA4          Comman
120300030624     c                   Call      'QCMDEXC'                            91
120400030624     c                   Parm                    Comman
120500030624     c                   Parm                    Lung
120600030624     c   91              goto      endstp
120700030624
120800030624      * Ovrprtf Bolle A5
120900030624     c                   Movea     D90prb5       cmA5(30)
121000030624     c                   Movea     D90mdb5       cmA5(52)
121100030624     c                   Clear                   Comman
121200030624     c                   Movea     cmA5          Comman
121300030624     c                   Call      'QCMDEXC'                            91
121400030624     c                   Parm                    Comman
121500030624     c                   Parm                    Lung
121600030624     c   91              goto      endstp
121700020524
121800020524     C                   eval      conta = conta +1
121900020524     C                   endif
122000020524
122100020524     c     endstp        endsr
122200001103      *---------------------------------------------------------------*
122300001103      *    Aggiornamento Proposta                                     *
122400001103      *---------------------------------------------------------------*
122500001103
122600001103     C     SR_UPDATEPRO  begsr
122700001103
122800001103     C                   z-add     *date         G02DAT
122900001103     C                   clear                   G02ERR
123000001103     C                   call      'XSRDA8'
123100001103     C                   parm                    WLBDAT
123200001103
123300001103     C                   z-add     g02inv        orpdte
123400001103     C                   time                    orpore
123500001103     C                   movel     knmus         orpute
123600040818     c                   z-add     g02inv        orpdtt
123700001103     C                   UPDATE    fnorp000
123800001103     C     orpfev        ifeq      'R'
123900001103     C                   add       1             vrifin
124000001103     C                   add       1             vrifiu
124100001103     C                   seton                                        11
124200001103     C                   else
124300001103     C                   add       1             vconfn
124400001103     C                   add       1             vconfe
124500001103     C                   seton                                        10
124600001103     C                   endif
124700001103
124800001103     C                   endsr
124900001024
125000001024      *---------------------------------------------------------------*
125100001024      *    Richiama PGM Creazione ORM                                 *
125200001024      *---------------------------------------------------------------*
125300001024
125400001103     C     SR_RICHIAMA   begsr
125500001024
125600001103     C                   clear                   FIOR05DS
125700001103
125800001106     C     orpfao        ifeq      999
125900030801     c     orpfao        oreq      910
126000001106     C                   move      'C'           §rmtla
126100001106     C                   else
126200001103     C                   move      '2'           §rmtla
126300001106     C                   endif
126400001103     C                   move      'P'           §rmfpr
126500001103     C                   move      orppoe        §rmpoe
126600001103     C                   move      orpnsr        §rmnsr
126700001103     C                   move      orpnor        §rmnor
126800001103     C                   move      orpnrv        §rmnrv
126900001103     C                   move      orptor        §rmtor
127000001106     C                   move      orpcor        §rmcor
127100001106     C                   move      orprso        §rmrso
127200001106     C                   move      orpino        §rmino
127300001106     C                   move      orpcao        §rmcao
127400001106     C                   move      orploo        §rmloo
127500001106     C                   move      orppro        §rmpro
127600001106     C                   move      orpnao        §rmnao
127700001106     C                   move      orpcra        §rmcra
127800001106     C                   move      orprsr        §rmrsr
127900001106     C                   move      orpinr        §rminr
128000001106     C                   move      orpcar        §rmcar
128100001106     C                   move      orplor        §rmlor
128200001106     C                   move      orpprr        §rmprr
128300001106     C                   move      orpnar        §rmnar
128400001106     C                   move      orprer        §rmrer
128500001106     C                   move      orpter        §rmter
128600001106     C                   move      orpcrc        §rmcrc
128700001106     C                   move      orprsc        §rmrsc
128800001106     C                   move      orpinc        §rminc
128900001106     C                   move      orpcac        §rmcac
129000001106     C                   move      orploc        §rmloc
129100001106     C                   move      orpprc        §rmprc
129200001106     C                   move      orpnac        §rmnac
129300001106     C                   move      orpffd        §rmffd
129400001106     C                   move      orpdar        §rmdar
129500001106     C                   move      orporr        §rmorr
129600001106     C                   move      orpnam        §rmnam
129700001106     C                   move      orpncl        §rmncl
129800001106     C                   move      orppkg        §rmpkg
129900001106     C                   move      orpvlm        §rmvlm
130000001106     C                   move      orpbnc        §rmbnc
130100001106     C                   move      orpblc        §rmblc
130200001106     C                   move      orpatt        §rmatt
130300001106     C                   move      orpmtc        §rmmtc
130400001106     C                   move      orppag        §rmpag
130500001106     C                   move      orpksc        §rmksc
130600001106     C                   move      orpctr        §rmctr
130700001106     C                   move      orppor        §rmpor
130800001106     C                   move      orpzor        §rmzor
130900001106     C                   move      orppoc        §rmpoc
131000001106     C                   move      orpno1        §rmno1
131100001106     C                   move      orpno2        §rmno2
131200001106     C                   move      orpcst        §rmcst
131300001106     C                   move      orpvcs        §rmvcs
131400001106     C                   move      orpfcs        §rmfcs
131500001106     C                   move      orpddt        §rmddt
131600001221     C                   z-add     orpsto        §rmsto
131700001106     C                   move      orprfa        §rmrfa
131800070706     C                   move      orpeti        §rmeti
131900010119     C                   move      orpspi        §rmspi
132000010209     C                   move      orpcau        §rmcau
132100010507     C                   move      orpflo        §rmflo
132200020524      * se generata bolla addebito passo flag per impedire f12 da conferma
132300020524     c                   if        wbolla = *on
132400020524     c                   movel     'B'           §rmflv
132500020524     c                   endif
132600131106
132700131106     c                   eval      §rmdtv = ORPdtv
132800131106     c                   eval      §rmorv = ORPorv
132900010205
133000010205      * Passo alcuni parametri anche nella parm01
133100010205     C                   clear                   parm01
133200010322     C                   if        orppoe = orppor
133300010205     C                   eval      pmio = 'S'
133400010322     C                   endif
133500061204     c                   eval      pndc = orpndc
133600061204     c                   eval      pddc = orpddc
133700120612     c                   eval      pdstfgs = orppor
133800010205     C                   movel     parm01        kpjbu
133900001106
134000131111     C                   call      'FIOR05R'
134100001103     C                   parm                    KPJBA
134200001103     C                   parm                    FIOR05DS
134300031204
134400031204     c                   eval       $Fior05 = *on
134500001024
134600001103     C                   endsr
134700001019
134800001102      *---------------------------------------------------------------*
134900001102      *    Visualizza Proposta di Variazione Scelta                   *
135000001102      *---------------------------------------------------------------*
135100001102
135200001102     C     SR_VISUALIZZA begsr
135300001102
135400001102      *    Accedo a Proposte
135500001102
135600001102     C     KFNORPA       chain(e)  fnorp02l
135700001102
135800001102      *    Se Errore Emetto Videata
135900001102
136000001102     C                   if        %error
136100001102     C                   move      vs1dtv        vi5dtv
136200001102     C                   move      vs1orv        vi5orv
136300001103     C                   move      vs1ute        vi5utv
136400001102     C                   move      vs1poe        vi5poe
136500001102     C                   move      vs1nsr        vi5nsr
136600001102     C                   move      vs1nor        vi5nor
136700001102     C                   move      vs1nrv        vi5nrv
136800001102     C                   do        *hival
136900001102     C                   EXFMT     FIOR205
137000001102     C   KL              leave
137100001102     C                   enddo
137200001102     C                   else
137300001102
137400001102      *    Se ho Trovato il Record Muovo Campi a Video
137500001102
137600001102     C                   if        %found(fnorp02l)
137700001102     C                   EXSR      SR_MUOVIPRO
137800001102
137900001102      *    Emetto Video
138000001102
138100001102     C                   do        *hival
138200001102     C                   EXFMT     FIOR204
138300071126
138400071126     c                   eval      *in28 = *off
138500071126     c                   clear                   vidmsg
138600001102
138700001102      *    F12=Ritorno
138800001102
138900001102     C     *inkl         ifeq      *on
139000001102     C     *in42         ifeq      *on
139100001102     C                   EXSR      SR_MUOVIPRO
139200001102     C                   else
139300001102     C                   leave
139400001102     C                   endif
139500001102     C                   endif
139600131029
139700131029      /free
139800131029       //?F4=Dati Avviso Ritiro
139900131029       IF  *inkd;
140000131029         exsr F04D01;
140100131029         iter;
140200131029       ENDIF;
140300131029
140400131029       //?F5=Altri dati
140500131029       IF  *inke;
140600131029         exsr F05D01;
140700131029         iter;
140800131029       ENDIF;
140900140303
141000140303       //?F17=Note AUT
141100140303       IF  *inkr;
141200140303         exsr F17D01;
141300140303         iter;
141400140303       ENDIF;
141500131029      /end-free
141600001102
141700131029      *    F9=Visualizza ORM
141800001102
141900131029     c                   if        *inki
142000071108     c                   Clear                   parm01
142100071108     c                   Eval      ppoe = Vi4Poe
142200071108     c                   Eval      pnor = Vi4Nor
142300071108     c                   Eval      pnsr = Vi4Nsr
142400071108     c                   Eval      pnrv = Vi4Nrv
142500071108     c                   Eval      pfgs = Vidpor
142600071108     c                   Eval      kpjbu = parm01
142700071108     c                   call      'FIOR07R'
142800071108     c                   Parm                    kpjba
142900071108     C                   iter
143000071108     c                   endif
143100131029
143200131029      * F24=Altri tasti  ?
143300131029     c                   if        *inky and $loop > 1
143400131029     c                   exsr      sr_f24
143500131029     c                   iter
143600131029     c                   endif
143700001103
143800001103
143900001103      *    F16=Rifiuta
144000001103
144100001103     C     *inKQ         ifeq      *on
144200140303      /free
144300140303       //?Se presente proposta note AUT le devo prima gestire
144400140303         IF  *in08;
144500140303           clear fiort1ds;
144600140303           IORT1mod = 'P';
144700140303           IORT1poe = VS1poe;
144800140303           IORT1nsr = VS1nsr;
144900140303           IORT1nor = VS1nor;
145000140303           IORT1nrv = VS1nrv;
145100140303           IORT1pgm = 'FIOR20R';
145200140303           IORT1dim = VS1dtvw;
145300140303           IORT1oim = VS1orv;
145400140303           §kpjbu = kpjbu;
145500140303
145600140303           fiort1r (kpjba:fiort1ds);
145700140303
145800140303           kpjbu = §kpjbu;
145900140303         ENDIF;
146000140303      /end-free
146100001103     C                   move      'R'           orpfev
146200001103     C                   exsr      SR_UPDATEPRO
146300001103     C                   leave
146400001103     C                   endif
146500001103
146600001103      *    F11=Modifica ORM
146700070123      * non posso accettare una variazione su un ORM in fase 420
146800070123      * il controllo avviene prima non abilita f11
146900001103
147000001103     C     *inKK         ifeq      *on
147100020530
147200020530     c                   eval      ds_orpcra = orpcra
147300020530     c                   eval      ds_orpcor = orpcor
147400020530     c                   eval      ds_orpcrc = orpcrc
147500020530
147600020530      * se sto confermando una proposta di chiusura
147700030801 1b  c                   if        orpfao = 999 or orpfao = 910
147800130412      * emetto msg info se orm in fase 400
147900130412     c                   if        ormfao = 400 or ormfao = 410
148000071126     c                   eval      *in28 = *on
148100071126     c                   eval      vidmsg = msg(08)
148200071126     c                   iter
148300071126     c                   endif
148400071126
148500020530     C                   clear                   dcmr
148600020530     C                   clear                   TIBS02DS
148700020530     C                   movel     'C'           T02mod
148800020530     C                   movel     knsif         t02sif
148900020530     C                   movel     'CMR'         t02cod
149000020530     C                   movel(p)  orpcau        T02ke1
149100020530     C                   call      'TIBS02R'
149200020530     C                   parm                    KPJBA
149300020530     C                   parm                    TIBS02DS
149400020530     C                   movel     t02uni        dcmr
149500020530      * procedo con i controlli per creare l'addebito
149600080226      * solo se l'orm è in fase 400 o 410
149700020530 2b  c                   if        d§cmradd <> *blanks
149800080226     c                             and (ormfao = 400 or ormfao = 410)
149900020530     c                   exsr      sr_contradd
150000020530 e2  c                   endif
150100020530 e1  c                   endif
150200020530      * se f12 o f03 da immissione bolla non devo confermare
150300020530 b1  c                   if        okbolla = ' '
150400001103
150500001103      *    Richiamo Programma di Creazione ORM
150600001103
150700001106     C                   EXSR      SR_RICHIAMA
150800001103
150900001103     C     §rmerr        ifeq      ' '
151000001103     C                   move      'E'           orpfev
151100001103     C                   exsr      SR_UPDATEPRO
151200001103     C                   leave
151300001103     C                   endif
151400020530
151500020530     c                   endif
151600001103
151700001103     C                   endif
151800001103
151900001102     C                   enddo
152000001102
152100001102     C                   endif
152200001102
152300001102     C                   endif
152400001102     C                   endsr
152500001102
152600001102      *---------------------------------------------------------------*
152700001102      *    Muovo Campi Proposta da File a Video                       *
152800001102      *---------------------------------------------------------------*
152900001102
153000001102     C     SR_MUOVIPRO   begsr
153100131029
153200131029     c                   ExSr      Sr_Tastifun
153300131029     c                   exsr      muoviORPE
153400131029
153500010209     C                   setoff                                       4215
153600131216     c                   eval      *in04 = *off
153700131216     c                   eval      *in06 = *off
153800131216     c                   eval      *in40 = *off
153900131216
154000001102     C                   clear                   fnorm000
154100001102     C     KFNORM        chain     fnorm01l
154200131029     c                   exsr      leggiFNORE
154300001102
154400001102      *    Key
154500001102
154600001102     C                   move      vs1dtv        vi4dtv
154700001102     C                   move      vs1orv        vi4orv
154800001102     C                   move      vs1ute        vi4utv
154900001102     C                   move      vs1poe        vi4poe
155000001102     C                   move      vs1nsr        vi4nsr
155100001102     C                   move      vs1nor        vi4nor
155200001102     C                   move      vs1nrv        vi4nrv
155300001102     C                   movel     ormdao        wa4               4
155400001102     C                   move      wa4           vi4dao
155500001102     C                   move      ormdao        wa4
155600001102     C                   move      wa4           wg2               2
155700001102     C                   movel     wa4           wm2               2
155800001102     C                   move      wm2           wa4
155900001102     C                   movel     wg2           wa4
156000001102     C                   movel     wa4           vi4dao
156100001215
156200001215     C                   movel     ormoao        vi4oao
156300001102
156400001102      *    Fase
156500001102
156600001102     C                   move      orpfao        vi4fao
156700001102     C                   if        vi4fao <> *zeros
156800001102     C                   clear                   TIBS02DS
156900001102     C                   movel     'C'           T02mod
157000001102     C                   movel     knsif         t02sif
157100001102     C                   movel     'FAR'         t02cod
157200001102     C                   movel(p)  vi4fao        T02ke1
157300001102     C                   call      'TIBS02R'
157400001102     C                   parm                    KPJBA
157500001102     C                   parm                    TIBS02DS
157600010314     C                   movel     t02uni        dfar
157700010314     C                   movel     d§fardes      vi4def
157800001102     C                   else
157900001102     C                   clear                   vi4def
158000001102     C                   endif
158100001102
158200001102      *    Dati Cliente Ritiro
158300001102
158400001102     C     orpcra        ifne      0
158500001102     C                   movel     orpcra        vi4cr1
158600001102     C                   move      orpcra        wsept             7
158700001102     C                   movel     wsept         vi4cr2
158800001102     C                   move      wsept         vi4cr3
158900001102     C                   else
159000001102     C                   clear                   vi4cr1
159100001102     C                   clear                   vi4cr2
159200001102     C                   clear                   vi4cr3
159300001102     C                   endif
159400001102     C                   movel     orprsr        vi4rsr
159500001102     C                   movel     orpinr        vi4inr
159600001102     C                   movel     orpcar        vi4car
159700001102     C                   movel     orplor        vi4lor
159800001102     C                   movel     orpprr        vi4prr
159900001102     C                   movel     orpnar        vi4nar
160000001102
160100001102      *    Dati Cliente Ordinante
160200001102
160300001102     C     orpcor        ifne      0
160400001102     C                   movel     orpcor        vi4co1
160500001102     C                   move      orpcor        wsept             7
160600001102     C                   movel     wsept         vi4co2
160700001102     C                   move      wsept         vi4co3
160800001102     C                   else
160900001102     C                   clear                   vi4co1
161000001102     C                   clear                   vi4co2
161100001102     C                   clear                   vi4co3
161200001102     C                   endif
161300001102     C                   movel     orprso        vi4rso
161400001102     C                   movel     orpino        vi4ino
161500001102     C                   movel     orpcao        vi4cao
161600001102     C                   movel     orploo        vi4loo
161700001102     C                   movel     orppro        vi4pro
161800001102     C                   movel     orpnao        vi4nao
161900001102
162000001129      *    Tipo comunicazione ORM (tco)
162100001102
162200001129     C                   if        ormtco <> *blanks
162300001102     C                   clear                   TIBS02DS
162400001102     C                   movel     'C'           T02mod
162500001102     C                   movel     knsif         t02sif
162600001129     C                   movel     'TCO'         t02cod
162700001129     C                   movel(p)  ormtco        T02ke1
162800001102     C                   call      'TIBS02R'
162900001102     C                   parm                    KPJBA
163000001102     C                   parm                    TIBS02DS
163100001129     C                   movel     t02uni        vi4tco
163200001102     C                   else
163300001129     C                   clear                   vi4tco
163400001102     C                   endif
163500001102
163600001102      *    Numero ORM Fisso
163700001102
163800001102     C                   move      ormpos        vi4pos
163900001102     C                   move      ormors        vi4ors
164000001102     C     ormpos        ifne      0
164100001102     C     ormors        orne      0
164200001102     C                   seton                                        58
164300001102     C                   else
164400001102     C                   setoff                                       58
164500001102     C                   endif
164600001102
164700001102      *    Quantità
164800001102
164900001102     C                   move      orpncl        vi4ncl
165000001102     C                   move      orppkg        vi4pkg
165100001102     C                   move      orpvlm        vi4vlm
165200001102     C                   move      orpbnc        vi4bnc
165300001102     C                   move      orpblc        vi4blc
165400001102     C                   move      orpatt        vi4att
165500001102     C                   move      orpmtc        vi4mtc
165600071107     c                   if        orpspi='S'
165700071107     C                   eval      vi4spi='SI'
165800071107     c                   else
165900071107     C                   eval      vi4spi='NO'
166000071107     c                   endif
166100001102
166200001102      *    Dati di Ritiro
166300001102
166400001102     C                   movel     orpdar        wa4               4
166500001102     C                   move      wa4           vi4dar
166600001102     C                   move      orpdar        wa4
166700001102     C                   move      wa4           wg2               2
166800001102     C                   movel     wa4           wm2               2
166900001102     C                   move      wm2           wa4
167000001102     C                   movel     wg2           wa4
167100001102     C                   movel     wa4           vi4dar
167200001102     C                   move      orporr        vi4orr
167300010205
167400010205      * Priorità
167500010205
167600001221     C                   move      orpsto        vi4sto
167700001129     C     vi4sto        ifne      *zeros
167800001103     C                   clear                   TIBS02DS
167900001103     C                   movel     'C'           T02mod
168000001103     C                   movel     knsif         t02sif
168100001103     C                   movel     'STO'         t02cod
168200131029     c                   eval      T02ke1 = %editc(ORPsto:'X')
168300001103     C                   call      'TIBS02R'
168400001103     C                   parm                    KPJBA
168500001103     C                   parm                    TIBS02DS
168600010205     C                   movel     t02uni        vi4dst
168700001103     C                   else
168800010205     C                   clear                   vi4dst
168900001103     C                   endif
169000001102
169100001102      *    P.O. Ritiro
169200001102
169300071108     c                   clear                   og143
169400001102     C     orppor        ifne      0
169500001102     C                   move      orppor        vi4por
169600001102     C     orppor        chain     azorg01l
169700001102     C                   if        %found(azorg01l)
169800001102     C                   movel     orgdes        vi4der
169900071108     c                   eval      og143 = orgde3
170000001102     C                   else
170100001102     C                   clear                   vi4der
170200001102     C                   endif
170300001102     C                   else
170400001102     C                   clear                   vi4por
170500001102     C                   clear                   vi4der
170600001102     C                   endif
170700071107      * giro
170800071107     c                   clear                   vi4cgi
170900071108     c     kfnorm        chain     fnorg01l
171000071107     c                   if        %found(fnorg01l)
171100071107     c                   eval      vi4cgi = orgcgi
171200071107     c                   endif
171300071108     c                   clear                   vi4dcg
171400071108     c                   clear                   fidg09ds
171500071108     c                   eval      d09iop0 = '001'
171600071108     c                   eval      d09ifgs = vidpor
171700071109     c                   eval      d09idat = dateu
171800071108     c                   eval      d09icgi = vi4cgi
171900071108     c                   eval      d09itug = 'R'
172000071108     c                   eval      kpjbu = fidg09ds
172100071108     c                   call      'FIDG09R'
172200071108     c                   parm                    kpjba
172300071108     c                   eval      fidg09ds = kpjbu
172400071108     c                   if        d09ocgi <> ' '
172500071108     c                   eval      vi4dcg = d09odes
172600071108     c                   endif
172700071108      * non controllo fedex xchè per ora non ho orm con ritiro per fedex
172800071108      * europolitan invece ormai è obsoleta
172900071108     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
173000071108      * se p.o. ritiro estero (DPD - EEX) decodifico la modalità invio al
173100071108      * partener se caricata e la visualizzo al posto della zona
173200071108     c                   if        ormfcs <> *blanks
173300071108     c                   clear                   tibs02ds
173400071108     c                   eval      t02mod = 'C'
173500071108     c                   eval      t02sif = knsif
173600071108     c                   eval      t02cod = 'MIP'
173700071108     c                   eval      t02ke1 = ormfcs
173800071108     c                   call      'TIBS02R'
173900071108     c                   parm                    kpjba
174000071108     c                   parm                    tibs02ds
174100071108     c                   eval      vi4fcs = t02uni
174200071108     c                   eval      *in22 = *on
174300071108     c                   endif
174400071108     c                   endif
174500071108
174600071108      *    dati della distinta
174700071109     C                   z-add     ORpndc        vi4ndc
174800071108      * Inversione data distinta
174900071108     C                   clear                   wlbdat
175000071109     C                   z-add     ORpddc        G02inv
175100071108     C                   movel     '3'           G02err
175200071108     C                   call      'XSRDA8'
175300071108     C                   parm                    wlbdat
175400071108     C                   z-add     G02dat        vi4ddc
175500071108      * Cerca codice autotrasportatore
175600071109     c                   clear                   vi4pdc
175700071109     c                   clear                   vi4dpd
175800071109     c                   if        orpndc<>0
175900071109     c                   exsr      sr_carpdr
176000071109     c                   eval      vi4pdc=w_pdr
176100071109      * decodifico l'autotrasportatore
176200071109     c                   if        vi4pdc>0
176300071109     c                   Movel     vi4pdc        kpdpdr
176400071109     c     kfapd         Chain     Fiapd01L
176500071109     c                   If        %Found(Fiapd01l)
176600071109     c                   Eval      vi4dpd = ApdRsc
176700071109     c                   EndIf
176800071109     c                   endif
176900071109     c                   endif
177000071108
177100071108      * se Stampato faccio vedere la scritta
177200071108     c                   eval      *in16 = (ormdst > 0)
177300001102
177400001102      *    Referente/Telefono/Riferimento/Natura Merce
177500001102
177600001102     C                   movel     orprer        vi4rer
177700001102     C                   movel     orpter        vi4ter
177800001102     C                   movel     orprfa        vi4rfa
177900001102     C                   movel     orpnam        vi4nam
178000001102
178100001102      *    Tipo Ordine (tor)
178200001102
178300001102     C     orptor        ifne      *blanks
178400001102     C                   move      orptor        vi4tor
178500001102     C                   clear                   TIBS02DS
178600001102     C                   movel     'C'           T02mod
178700001102     C                   movel     knsif         t02sif
178800001102     C                   movel     'TOR'         t02cod
178900001102     C                   movel(p)  orptor        T02ke1
179000001102     C                   call      'TIBS02R'
179100001102     C                   parm                    KPJBA
179200001102     C                   parm                    TIBS02DS
179300001102     C                   movel     t02uni        vi4det
179400001102     C                   else
179500001102     C                   clear                   vi4tor
179600001102     C                   clear                   vi4det
179700001102     C                   endif
179800001102
179900001102      *    Dati Pagamento
180000001102
180100071108     C                   if        ORppag = 'M'
180200071108     C                   eval      vi4pag = 'Mitt.'
180300071108     C                   endif
180400071108     C                   if        ORppag = 'D'
180500071108     C                   eval      vi4pag = 'Dest.'
180600071108     C                   endif
180700071108     C                   if        ORppag = 'O'
180800071108     C                   eval      vi4pag = 'Ord. '
180900071108     C                   endif
181000001102     C                   move      orpksc        vi4ksc
181100001102     C                   move      orpctr        vi4ctr
181200071108     C                   if        ORpddt = 'S'
181300071108     C                   eval      vi4ddt = 'SI'
181400071108     C                   endif
181500071108     C                   if        ORpddt = 'N'
181600071108     C                   eval      vi4ddt = 'NO'
181700071108     C                   endif
181800071108     C                   if        ORPffd = 'S'
181900071108     C                   eval      vi4ffd = 'SI'
182000071108     C                   else
182100071108     C                   eval      vi4ffd = 'NO'
182200071108     C                   endif
182300001102
182400001102      *    P.O. Consegna
182500001102
182600001102     C     orppoc        ifne      0
182700001102     C                   move      orppoc        vi4poc
182800001102     C     orppoc        chain     azorg01l
182900001102     C                   if        %found(azorg01l)
183000001102     C                   movel     orgdes        vi4dec
183100001102     C                   else
183200001102     C                   clear                   vi4dec
183300001102     C                   endif
183400001102     C                   else
183500001102     C                   clear                   vi4poc
183600001102     C                   clear                   vi4dec
183700001102     C                   endif
183800071108      * ORM con Ricevuta di Ritiro
183900071108     c                   eval      dorm01=ormflo
184000071108     c                   Eval      *In17 = (§orsrm = 'S')
184100001102
184200001102      *    Dati Consegna A
184300001102
184400001102     C     orpcrc        ifne      0
184500001102     C                   movel     orpcrc        vi4cc1
184600001102     C                   move      orpcrc        wsept             7
184700001102     C                   movel     wsept         vi4cc2
184800001102     C                   move      wsept         vi4cc3
184900001102     C                   else
185000001102     C                   clear                   vi4cc1
185100001102     C                   clear                   vi4cc2
185200001102     C                   clear                   vi4cc3
185300001102     C                   endif
185400001102     C                   movel     orprsc        vi4rsc
185500001102     C                   movel     orpinc        vi4inc
185600001102     C                   movel     orpcac        vi4cac
185700001102     C                   movel     orploc        vi4loc
185800001102     C                   movel     orpprc        vi4prc
185900001102     C                   movel     orpnac        vi4nac
186000001102
186100001102      *    Note
186200001102
186300001102     C                   movel     orpno1        vi4no1
186400001102     C                   movel     orpno2        vi4no2
186500010209
186600010209      * ORM chiuso
186700010209
186800030801     C                   if        orpfao = 999 or orpfao = 910
186900010209     C                   eval      *in15 = *on
187000010209     C                   movel     orpcau        vi4cau
187100010301     C                   clear                   dcmr
187200010209     C                   clear                   TIBS02DS
187300010209     C                   movel     'C'           T02mod
187400010209     C                   movel     knsif         t02sif
187500010227     C                   movel     'CMR'         t02cod
187600010209     C                   movel(p)  vi4cau        T02ke1
187700010209     C                   call      'TIBS02R'
187800010209     C                   parm                    KPJBA
187900010209     C                   parm                    TIBS02DS
188000010301     C                   movel     t02uni        dcmr
188100010301     C                   movel     d§cmrdes      vi4dca
188200010209     C                   endif
188300030610
188400030610     c                   eval      dorm01 = ORPflo
188500030610
188600030610      * ORM commissionato
188700030610     c                   If        §orCom = 'S'
188800030610     c                   Eval      Vi4Com = 'COMMISSIONATO'
188900030610     c                   Else
189000030610     c                   Clear                   Vi4Com
189100030610     c                   EndIf
189200001102
189300001102      *    Accendo Indicatori per differenze
189400001102
189500001102     C                   setoff                                       434445
189600001102     C                   setoff                                       464748
189700001102     C                   setoff                                       495051
189800001102     C                   setoff                                       525354
189900001102     C                   setoff                                       555657
190000001102     C                   setoff                                       596061
190100001102     C                   setoff                                       626364
190200001102     C                   setoff                                       656667
190300001102     C                   setoff                                       687071
190400001102     C                   setoff                                       727374
190500001102     C                   setoff                                       757677
190600001102     C                   setoff                                       787980
190700001102     C                   setoff                                       818292
190800001102     C                   setoff                                       939495
190900001102     C                   setoff                                       969798
191000001103     C                   setoff                                       838488
191100131029     C                   setoff                                       896999
191200131029     c                   clear                   AT3sms
191300131029     c                   clear                   AT3mail
191400001102
191500001102     C     orpfao        ifne      ormfao
191600001102     C                   eval      *in43 = *on
191700001102     C                   endif
191800001102     C     orpcra        ifne      ormcra
191900001102     C                   eval      *in44 = *on
192000001102     C                   endif
192100001102     C     orprsr        ifne      ormrsr
192200001102     C                   eval      *in45 = *on
192300001102     C                   endif
192400001102     C     orpinr        ifne      orminr
192500001102     C                   eval      *in46 = *on
192600001102     C                   endif
192700001102     C     orpcar        ifne      ormcar
192800001102     C                   eval      *in47 = *on
192900001102     C                   endif
193000001102     C     orplor        ifne      ormlor
193100001102     C                   eval      *in48 = *on
193200001102     C                   endif
193300001102     C     orpprr        ifne      ormprr
193400001102     C                   eval      *in49 = *on
193500001102     C                   endif
193600001102     C     orpnar        ifne      ormnar
193700001102     C                   eval      *in50 = *on
193800001102     C                   endif
193900001102     C     orpcor        ifne      ormcor
194000001102     C                   eval      *in51 = *on
194100001102     C                   endif
194200001102     C     orprso        ifne      ormrso
194300001102     C                   eval      *in52 = *on
194400001102     C                   endif
194500001102     C     orpino        ifne      ormino
194600001102     C                   eval      *in53 = *on
194700001102     C                   endif
194800001102     C     orpcao        ifne      ormcao
194900001102     C                   eval      *in54 = *on
195000001102     C                   endif
195100001102     C     orploo        ifne      ormloo
195200001102     C                   eval      *in55 = *on
195300001102     C                   endif
195400001102     C     orppro        ifne      ormpro
195500001102     C                   eval      *in56 = *on
195600001102     C                   endif
195700001102     C     orpnao        ifne      ormnao
195800001102     C                   eval      *in57 = *on
195900001102     C                   endif
196000001102     C     orpncl        ifne      ormncl
196100001102     C                   eval      *in59 = *on
196200001102     C                   endif
196300001102     C     orppkg        ifne      ormpkg
196400001102     C                   eval      *in60 = *on
196500001102     C                   endif
196600001102     C     orpvlm        ifne      ormvlm
196700001102     C                   eval      *in61 = *on
196800001102     C                   endif
196900001102     C     orpbnc        ifne      ormbnc
197000001102     C                   eval      *in62 = *on
197100001102     C                   endif
197200001102     C     orpblc        ifne      ormblc
197300001102     C                   eval      *in63 = *on
197400001102     C                   endif
197500001102     C     orpatt        ifne      ormatt
197600001102     C                   eval      *in64 = *on
197700001102     C                   endif
197800001102     C     orpmtc        ifne      ormmtc
197900001102     C                   eval      *in65 = *on
198000001102     C                   endif
198100001102     C     orpdar        ifne      ormdar
198200001102     C                   eval      *in66 = *on
198300001102     C                   endif
198400001102     C     orporr        ifne      ormorr
198500001102     C                   eval      *in68 = *on
198600001102     C                   endif
198700001103     C     orpsto        ifne      ormsto
198800001103     C                   eval      *in88 = *on
198900001102     C                   endif
199000001102     C     orprer        ifne      ormrer
199100001102     C                   eval      *in72 = *on
199200001102     C                   endif
199300001102     C     orpter        ifne      ormter
199400001102     C                   eval      *in73 = *on
199500001102     C                   endif
199600001102     C     orpnam        ifne      ormnam
199700001102     C                   eval      *in74 = *on
199800001102     C                   endif
199900001102     C     orprfa        ifne      ormrfa
200000001102     C                   eval      *in75 = *on
200100001102     C                   endif
200200001102     C     orptor        ifne      ormtor
200300001102     C                   eval      *in76 = *on
200400001102     C                   endif
200500001102     C     orppag        ifne      ormpag
200600001102     C                   eval      *in77 = *on
200700001102     C                   endif
200800001102     C     orpksc        ifne      ormksc
200900001102     C                   eval      *in78 = *on
201000001102     C                   endif
201100001102     C     orpctr        ifne      ormctr
201200001102     C                   eval      *in79 = *on
201300001102     C                   endif
201400001102     C     orpddt        ifne      ormddt
201500001102     C                   eval      *in80 = *on
201600001102     C                   endif
201700010119     C     orpspi        ifne      ormspi
201800010119     C                   eval      *in89 = *on
201900010119     C                   endif
202000001102     C     orppoc        ifne      ormpoc
202100001102     C                   eval      *in81 = *on
202200001102     C                   endif
202300001102     C     orpffd        ifne      ormffd
202400001102     C                   eval      *in82 = *on
202500001102     C                   endif
202600001102     C     orpcrc        ifne      ormcrc
202700001102     C                   eval      *in92 = *on
202800001102     C                   endif
202900001102     C     orprsc        ifne      ormrsc
203000001102     C                   eval      *in93 = *on
203100001102     C                   endif
203200001102     C     orpinc        ifne      orminc
203300001102     C                   eval      *in94 = *on
203400001102     C                   endif
203500001102     C     orpcac        ifne      ormcac
203600001102     C                   eval      *in95 = *on
203700001102     C                   endif
203800001102     C     orploc        ifne      ormloc
203900001102     C                   eval      *in96 = *on
204000001102     C                   endif
204100001102     C     orpprc        ifne      ormprc
204200001102     C                   eval      *in97 = *on
204300001102     C                   endif
204400001102     C     orpnac        ifne      ormnac
204500001102     C                   eval      *in98 = *on
204600001102     C                   endif
204700001102     C     orpno1        ifne      ormno1
204800001102     C                   eval      *in83 = *on
204900001102     C                   endif
205000001102     C     orpno2        ifne      ormno2
205100001102     C                   eval      *in84 = *on
205200001102     C                   endif
205300131029
205400131029      /free
205500131029         IF  VI4oraamda <> oraamda;
205600131029           *in69 = *on;
205700131029         ENDIF;
205800131029         IF  VI4oraama <> oraama;
205900131029           *in70 = *on;
206000131029         ENDIF;
206100131029         IF  VI4orapmda <> orapmda;
206200131029           *in71 = *on;
206300131029         ENDIF;
206400131029         IF  VI4orapma <> orapma;
206500131029           *in99 = *on;
206600131029         ENDIF;
206700131029         IF  W03sms <> sms;
206800131029           AT3sms = highrimm;
206900131029         ENDIF;
207000131029         IF  W03mail <> mail;
207100131029           AT3mail = highrimm;
207200131029         ENDIF;
207300131216
207400131216       //?Verifico se sono presenti dati nella altre 2 videate.
207500131216         IF  VI4cgi <> *blanks or VI4blc > 0 or VI4att > 0 or
207600131216             VI4mtc > 0 or VI4sto > 0 or VI4ffd <> 'NO';
207700131216           *in06 = *on;
207800131216         ENDIF;
207900131216         IF  W03mail <> *blanks or W03sms <> *blanks;
208000131216           *in04 = *on;
208100131216         ENDIF;
208200140303
208300140303         *in08 = *off;
208400140303       //?Verifico se sono presenti proposte note AUT
208500140303         chain (VS1poe:VS1nsr:VS1nor:VS1nrv:VS1dtvw:VS1orv) FNORPT1L;
208600140303         IF  %found(FNORPT1L);
208700140303           *in08 = *on;
208800140303         ENDIF;
208900140303
209000140303
209100131029      /end-free
209200001102
209300001102     C                   endsr
209400131029
209500131029      /free
209600131029       //--------------------------------------------------------------------
209700131029       //?Imposto campi da ORPE a video.
209800131029       //--------------------------------------------------------------------
209900131029       BEGSR muoviORPE;
210000131029
210100131029       //?Rcd 'O ' --> Orari apertura
210200131029         k_ORPEpoe = ORPpoe;
210300131029         k_ORPEnsr = ORPnsr;
210400131029         k_ORPEnor = ORPnor;
210500131029         k_ORPEnrv = ORPnrv;
210600131029         K_ORPEdtv = ORPdtv;
210700131029         k_ORPEorv = ORPorv;
210800131029         k_ORPEtrc = 'O ';
210900131029         clear dOREorari;
211000131029       //?Aggancio il rcd
211100131029         chain  %kds(k07fnorpe) FNORPE1L;
211200131029         IF  %found(FNORPE1L);
211300131029           dOREorari = ORPEdati;
211400131029         ENDIF;
211500131029         VI4oraamda = §OREoramda;
211600131029         VI4oraama  = §OREorama;
211700131029         VI4orapmda = §OREorapda;
211800131029         VI4orapma  = §OREorapa;
211900131029
212000131029       //?Rcd 'S ' --> SMS per invio alert
212100140214         clear w03sms;
212200131029         k_ORPEtrc = 'S ';
212300131029       //?Aggancio il rcd
212400131029         chain  %kds(k07fnorpe) FNORPE1L;
212500131029         IF  %found(FNORPE1L);
212600131029           w03sms = ORPEdati;
212700131029         ENDIF;
212800131029
212900131029       //?Rcd 'MA' --> Mail per invio alert
213000140214         clear w03mail;
213100131029         k_ORPEtrc = 'MA';
213200131029       //?Aggancio il rcd
213300131029         chain  %kds(k07fnorpe) FNORPE1L;
213400131029         IF  %found(FNORPE1L);
213500131029           w03mail = ORPEdati;
213600131029         ENDIF;
213700131029
213800131029       ENDSR;
213900131029
214000131029       //--------------------------------------------------------------------
214100131029       //?Leggo FNORE00F.
214200131029       //--------------------------------------------------------------------
214300131029       BEGSR LeggiFNORE;
214400131029
214500131029         clear oraamda;
214600131029         clear oraama;
214700131029         clear orapmda;
214800131029         clear orapma;
214900131029         clear sms;
215000131029         clear mail;
215100131029
215200131029       //?Rcd 'O ' --> Orari apertura
215300131029         k_OREpoe = ORMpoe;
215400131029         k_OREnsr = ORMnsr;
215500131029         k_OREnor = ORMnor;
215600131029         k_OREnrv = ORMnrv;
215700131029         k_OREtrc = 'O ';
215800131029         clear dOREorari;
215900131029       //?Aggancio il rcd
216000131029         chain  %kds(k05fnore) FNORE01L;
216100131029         IF  %found(FNORE01L);
216200131029           dOREorari = OREdati;
216300131029         ENDIF;
216400131029         oraamda = §OREoramda;
216500131029         oraama  = §OREorama;
216600131029         orapmda = §OREorapda;
216700131029         orapma  = §OREorapa;
216800131029
216900131029       //?Rcd 'S ' --> SMS per invio alert
217000131029         k_OREtrc = 'S ';
217100131029       //?Aggancio il rcd
217200131029         chain  %kds(k05fnore) FNORE01L;
217300131029         IF  %found(FNORE01L);
217400131029           sms = OREdati;
217500131029         ENDIF;
217600131029
217700131029       //?Rcd 'MA' --> Mail per invio alert
217800131029         k_OREtrc = 'MA';
217900131029       //?Aggancio il rcd
218000131029         chain  %kds(k05fnore) FNORE01L;
218100131029         IF  %found(FNORE01L);
218200131029           mail = OREdati;
218300131029         ENDIF;
218400131029       ENDSR;
218500131029
218600131029       //--------------------------------------------------------------------
218700131029       //?F4 - Alert.
218800131029       //--------------------------------------------------------------------
218900131029       BEGSR F04D01;
219000131029
219100131029         wEndF04 = *off;
219200131029
219300131029       //?emetto la videata relativa alla MAIL e SMS
219400131029         write fior204;
219500131029       //?emetto la window
219600131029         DOW not wEndF04;
219700131029           exfmt fior2043;
219800131029       //?F12-Ritorno
219900131029           IF  *inkl;
220000131029             wEndF04 = *on;
220100131029           ENDIF;
220200131029         ENDDO;
220300131029
220400131029       ENDSR;
220500131029
220600131029       //--------------------------------------------------------------------
220700131029       //?F5 - Altri dati.
220800131029       //--------------------------------------------------------------------
220900131029       BEGSR F05D01;
221000131029
221100131029         wEndF05 = *off;
221200131029
221300131029       //?emetto la videata relativa agli altri dati
221400131029         write fior204;
221500131029       //?emetto la window dove richiedo gli altri dati
221600131029         DOW not wEndF05;
221700131029           exfmt fior2042;
221800131029       //?F12-Ritorno
221900131029           IF  *inkl;
222000131029             wEndF05 = *on;
222100131029           ENDIF;
222200131029         ENDDO;
222300131029
222400131029       ENDSR;
222500140303
222600140303       //--------------------------------------------------------------------
222700140303       //?F17 - Note AUT.
222800140303       //--------------------------------------------------------------------
222900140303       BEGSR F17D01;
223000140303
223100140303         clear fiort1ds;
223200140303         IORT1mod = 'I';
223300140303         IORT1poe = VS1poe;
223400140303         IORT1nsr = VS1nsr;
223500140303         IORT1nor = VS1nor;
223600140303         IORT1nrv = VS1nrv;
223700140303         IORT1pgm = 'FIOR20R';
223800140303         IORT1dim = VS1dtvw;
223900140303         IORT1oim = VS1orv;
224000140303         §kpjbu = kpjbu;
224100140303
224200140303         fiort1r (kpjba:fiort1ds);
224300140303
224400140303         kpjbu = §kpjbu;
224500140303
224600140303       ENDSR;
224700131029      /end-free
224800131029
224900131029      *------------------------------------------------------------------------*
225000131029      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
225100131029      *------------------------------------------------------------------------*
225200131029     c     sr_f24        begsr
225300131029
225400131029     c                   Select
225500131029
225600131029     c                   When      $loop = 2 and $rig =2  or
225700131029     c                             $loop = 3 and $rig =3
225800131029     c                   Movea     rigatf1       vzfd01
225900131029     c                   Z-add     1             $rig
226000131029
226100131029     c                   If        $loop = 2
226200131029     c                   Eval      vzfd02= cf2412
226300131029     c                   Else
226400131029     c                   Eval      vzfd02= cf2413
226500131029     c                   EndIf
226600131029
226700131029     c                   When      $loop = 2 and $rig =1 or
226800131029     c                             $loop = 3 and $rig =1
226900131029     c                   Movea     rigatf2       vzfd01
227000131029     c                   Z-add     2             $rig
227100131029
227200131029     c                   If        $loop = 2
227300131029     c                   Eval      vzfd02 = cf2422
227400131029     c                   Else
227500131029     c                   Eval      vzfd02 = cf2423
227600131029     c                   EndIf
227700131029
227800131029     c                   When      $loop = 3 and $rig =2
227900131029     c                   Movea     rigatf3       vzfd01
228000131029     c                   Z-add     3             $rig
228100131029     c                   Eval      vzfd02 = cf2433
228200131029     c                   EndSl
228300131029
228400131029     c                   EndSr
228500001102
228600071109      *---------------------------------------------------------------*
228700071109      *    Reperimento dati autotrasportatore                         *
228800071109      *---------------------------------------------------------------*
228900071109
229000071109     C     SR_CARPDR     begsr
229100071109     c                   clear                   w_fpp
229200071109     c                   clear                   w_pdr
229300071109     c
229400120612     c                   z-add     orpnpg        knpg
229500071109     c                   z-add     orpndc        knfv
229600071109     c                   z-add     orppor        kfgs
229700120612     c     kfidst        chain     fidst01l
229800120612     c                   if        %found(fidst01l)
229900071109     c                   eval      w_pdr=dstpdr
230000120612     c                   eval      w_fpp = dstfpp
230100071109     c                   endif
230200071109     c                   endsr
230300131029
230400131029      *------------------------------------------------------------------------*
230500131029      *  Imposto descrizione tasti funzione
230600131029      *------------------------------------------------------------------------*
230700131029     c     Sr_Tastifun   BegSr
230800131029
230900131029      * Conta i caratteri riempiti dalle RigaTf1
231000131029     c                   Clear                   $z
231100131029      * Conta i caratteri riempiti dalle RigaTf2
231200131029     c                   Clear                   $k
231300131029      * Conta i caratteri riempiti dalle RigaTf3
231400131029     c                   Clear                   $j
231500131029      * Conta la posizione del campone da cui partire per impostare
231600131029      * la descrizione del tasto funzione
231700131029     c                   Clear                   $y
231800131029
231900131029      * Stringhe che contengono le descrizioni dei tasti funzione
232000131029     c                   Reset                   rigatf1
232100131029     c                   Reset                   rigatf2
232200131029     c                   Reset                   rigatf3
232300131029
232400131029      * F9 - Visualizza ORM
232500131029     c                   if        not *in42
232600131029     c                   reset                   $tf
232700131029     c                   movea     tf09          $tf
232800131029     c                   exsr      rie_$tf
232900131029     c                   endif
233000131029      * F11 - Accetta Proposta
233100131029     c                   if        not *in42 and not *in35
233200131029     c                   reset                   $tf
233300131029     c                   movea     tf11          $tf
233400131029     c                   exsr      rie_$tf
233500131029     c                   endif
233600131029      * F16 - Riufiuta Proposta
233700131029     c                   if        not *in42
233800131029     c                   reset                   $tf
233900131029     c                   movea     tf16          $tf
234000131029     c                   exsr      rie_$tf
234100131029     c                   endif
234200131029
234300131029      * Pulisco la stringa
234400131029     c                   Eval      $h = 1
234500131029     c                   For       $h by 1 to 62
234600131029     c                   If        rigatf1($h) = '#'
234700131029     c                   Clear                   rigatf1($h)
234800131029     c                   EndIf
234900131029     c                   EndFor
235000131029     c                   Eval      $h = 1
235100131029     c                   For       $h by 1 to 62
235200131029     c                   If        rigatf2($h) = '#'
235300131029     c                   Clear                   rigatf2($h)
235400131029     c                   EndIf
235500131029     c                   EndFor
235600131029     c                   Eval      $h = 1
235700131029     c                   For       $h by 1 to 62
235800131029     c                   If        rigatf3($h) = '#'
235900131029     c                   Clear                   rigatf3($h)
236000131029     c                   EndIf
236100131029     c                   EndFor
236200131029
236300131029      * Imposto la descrizione del tasto funzione F24 e segnalo
236400131029      * quante righe ho riempito e quale devo emettere
236500131029     c                   Select
236600131029
236700131029     c                   When      $k <> *Zeros and $y <> *Zeros
236800131029     c                   Eval      $loop = 3
236900131029     c                   Eval      vzfd02 = cf2413
237000131029
237100131029     c                   When      $k <> *Zeros and $y = *Zeros
237200131029     c                   Eval      $loop = 2
237300131029     c                   Eval      vzfd02 = cf2412
237400131029
237500131029     c                   Other
237600131029
237700131029     c                   Eval      $loop = 1
237800131029     c                   Clear                   vzfd02
237900131029     c                   EndSl
238000131029
238100131029      * Prima riga di tasti funzione
238200131029     c                   Movea     rigatf1       vzfd01
238300131029     c                   Eval      $rig = 1
238400131029
238500131029     c                   EndSr
238600131029
238700131029      *------------------------------------------------------------------------*
238800131029      *  IMPOSTO LE STRINGHE CON I CAMPI FUNZIONE COMPATTATI
238900131029      *------------------------------------------------------------------------*
239000131029     c     Rie_$tf       BegSr
239100131029      *
239200131029     c                   Eval      $x = 1
239300131029     c     '#'           Lookup    $tf($x)                                30
239400131029     c                   Add       $x            $z
239500131029     c                   If        $Z <= 63
239600131029     c                   Eval      $j = $Z - $x + 1
239700131029     c                   Movea     $tf           rigatf1($j)
239800131029     c                   Else
239900131029     c                   Add       $x            $k
240000131029     c                   If        $k <= 63
240100131029     c                   Eval      $j = $K - $x + 1
240200131029     c                   Movea     $tf           rigatf2($j)
240300131029     c                   Else
240400131029     c                   Add       $x            $y
240500131029     c                   If        $y <= 63
240600131029     c                   Eval      $j = $y - $x + 1
240700131029     c                   Movea     $tf           rigatf3($j)
240800131029     c                   EndIf
240900131029     c                   EndIf
241000131029     c                   EndIf
241100131029
241200131029     c                   EndSr
241300131029
241400130619      *****************************************************************
241500130619      * ROUTINE INIZIALE
241600130619      *****************************************************************
241700130619     C     *INZSR        BEGSR
241800130619
241900130619     C     *ENTRY        PLIST
242000130619     C                   PARM                    KPJBA
242100130619     c                   parm                    fior20ds
242200130619
242300130619      *    Accesso a FNORP (per Subfile)
242400130619
242500130619     C     KFNORP        KLIST
242600130619     C                   kfld                    vs1por
242700130619     C                   kfld                    vidpoe
242800130619     C                   kfld                    vidnsr
242900130619     C                   kfld                    vidnor
243000130619     C                   kfld                    vidnrv
243100130619
243200130619      *    Accesso a FNORP (per Aggiornamento)
243300130619
243400130619     C     KFNORPA       KLIST
243500130619     C                   kfld                    vs1por
243600130619     C                   kfld                    vs1poe
243700130619     C                   kfld                    vs1nsr
243800130619     C                   kfld                    vs1nor
243900130619     C                   kfld                    vs1nrv
244000130619     C                   kfld                    vs1dtvw
244100130619     C                   kfld                    vs1orv
244200130619
244300130619      *    Accesso a FNORP (per Controllo)
244400130619
244500130619     C     KFNORPC       KLIST
244600130619     C                   kfld                    vs1por
244700130619     C                   kfld                    vs1poe
244800130619     C                   kfld                    vs1nsr
244900130619     C                   kfld                    vs1nor
245000130619     C                   kfld                    vs1nrv
245100130619
245200130619      *    Accesso a FNORM
245300130619
245400130619     C     KFNORM        KLIST
245500130619     C                   kfld                    vs1poe
245600130619     C                   kfld                    vs1nsr
245700130619     C                   kfld                    vs1nor
245800130619     C                   kfld                    vs1nrv
245900130619
246000130619      *    Accesso a fidst
246100130619
246200130619     c     kfidst        klist
246300130619     c                   kfld                    knpg
246400130619     c                   kfld                    knfv
246500130619     c                   kfld                    kfgs
246600130619
246700130619     C     Kfapd         klist
246800130619     C                   kfld                    kpdtip
246900130619     C                   kfld                    kpdpdr
247000130619
247100130619      * reperisco data e ora
247200130619     C                   TIME                    W0140
247300130619      * UDATE IN GGMMAAAA
247400130619     C                   MOVE      W0140         WDTGIO
247500130619      * UDATE IN AAAAMMGG
247600130619     C                   Z-ADD     WDTGIO        G02DAT
247700130619     C                   MOVEL     *BLANK        G02ERR
247800130619     C                   CALL      'XSRDA8'
247900130619     C                   PARM                    WLBDAT
248000130619     C                   MOVEL     G02INV        DATEU
248100130619
248200130619      *    Recupero Ragione Sociale Ditta da Emettere a Video
248300130619      *    e Punti Operativi Gestiti
248400130619
248500130619     C                   Z-ADD     1             CODUT             1 0
248600130619
248700130619      * Reperisco dati job
248800130619     c                   exsr      DatiJob
248900130619
249000130619      * controllo abilitazione utente
249100130619     c                   clear                   wabi
249200130619     c                   clear                   dLAT
249300130619     c                   eval      dUTE01 = UTEfaf
249400130619
249500130619      * Verifica errori e autorità profilo
249600130619     c                   SELECT
249700130619      * se ho errori nei dati utente esco dal pgm
249800130619     c                   WHEN      DUTerr = 'E'
249900130619     c                   eval      wErrGrave = *on
250000130619     c                   leavesr
250100130619      * carica abilitazioni del profilo
250200130619     c                   WHEN      §UTEorm <> *blanks
250300130619     c                   eval      wabi = §UTEorm
250400130619     c                   WHEN      UTEaut <> *blanks
250500130619     c                   eval      wabi = UTEaut
250600130619      * se non c'è l'abilitazione
250700130619      * --> se 1° livello, abilitazioni al terminal
250800130619      *     se 2° livello, abilitazioni al punto operativo
250900130619      *     se sede no abilitazioni
251000130619     c                   OTHER
251100130619     c                   IF        DUTlpo = '1'
251200130619     c                   eval      wabi   = 'TP'
251300130619     c                   ENDIF
251400130619     c                   IF        DUTlpo = '2'
251500130619     c                   eval      wabi   = 'PO'
251600130619     c                   ENDIF
251700130619     c                   ENDSL
251800130619
251900130619      * controllo se ok l'abilitazione dell'utente
252000130619     c                   clear                   TIBS02DS
252100130619     c                   eval      T02mod = 'C'
252200130619     c                   eval      T02sif = knsif
252300130619     c                   eval      T02cod = 'LAT'
252400130619     c                   eval      T02ke1 = wabi
252500130619     c                   Call      'TIBS02R'
252600130619     c                   Parm                    KPJBA
252700130619     c                   Parm                    TIBS02DS
252800130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
252900130619     c                   eval      wErrGrave = *on
253000130619     c                   leavesr
253100130619     c                   ENDIF
253200130619     c                   eval      dLAT = T02uni
253300130619
253400130619      * Reperimento delle filiali gestibili dall'utente
253500130619     c                   clear                   TRUL31DS
253600130619     c                   eval      I31abi = wabi
253700130619     c                   eval      I31cdi = DUTdis
253800130619     c                   eval      I31car = DUTare
253900130619     c                   eval      I31cpo = DUTpou
254000130619     c                   call      'TRUL31R'
254100130619     c                   parm                    KPJBA
254200130619     c                   parm                    TRUL31DS
254300130619     c                   IF        O31pog > *zeros
254400130619     c                   movea     O31pog        SKpog
254500130619     c                   ELSE
254600130619     c                   eval      wErrGrave = *on
254700130619     c                   leavesr
254800130619     c                   ENDIF
254900130619
255000130619     c                   endsr
255100001102
255200000928** MSG
255300070418Filiale Errata o non Gestibile                                                      1
255400070418Scelta zona relativa ad altra filiale                                               2
255500001030Zona di Ritiro Errata                                                               3
255600001030Data Formalmente Scorretta                                                          4
255700001030Data Dal Maggiore di Data Al                                                        5
255800001030Inserire Data/e                                                                     6
255900001030Immettere un Valore                                                                 7
256000071126ATTENZIONE ORM ASSEGNATO! Per poterlo chiudere prima si deve disassegnare.          8
256100081030ORM Commissionato. Non si può usare più di 1 causale con resp. al cliente           9
256200001023..............  Libero                                                             10
256300001023..............  Libero                                                             11
256400001023..............  Libero                                                             12
256500001023..............  Libero                                                             13
256600001023..............  Libero                                                             14
256700001023..............  Libero                                                             15
256800001023..............  Libero                                                             16
256900001023..............  Libero                                                             17
257000001023..............  Libero                                                             18
257100001023..............  Libero                                                             19
257200001023..............  Libero                                                             20
257300001023..............  Libero                                                             21
257400001023..............  Libero                                                             22
257500001023..............  Libero                                                             23
257600001023..............  Libero                                                             24
257700001023..............  Libero                                                             25
257800001023..............  Libero                                                             26
257900001023..............  Libero                                                             27
258000001010..............  Libero                                                             28
258100001010..............  Libero                                                             29
258200001010..............  Libero                                                             30
258300020524** CM3
258400030307OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
258500030307 SHARE(*YES)
258600030624** cmA4
258700030624OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
258800030624 SHARE(*YES)          USRDTA('BolAdd_A4')
258900030624**  cmA5
259000030624OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
259100030624 SHARE(*YES)          USRDTA('BolAdd_A5')
259200131029** TF09
259300131029F9=Visualizza ORM #
259400131029** TF11
259500131029F11=Accetta #
259600131029** TF16
259700131029F16=Rifiuta #
