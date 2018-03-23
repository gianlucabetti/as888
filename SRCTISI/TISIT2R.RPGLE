000100060103      *===============================================================*
000200060103      *?TISIT2R * Manutenzione anagrafico destinatari particolari    ?*
000300060103      *===============================================================*
000400060103
000500060103     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060103
000700060103     fAZORG01L  if   e           k disk
000800060103     fTABEL00F  if   e           k disk
000900060103     fAZDST04L  if   e           k disk    rename(AZDST000:AZDST004)
001000060103      *
001100060103     fAZDST03L  Uf A e           k disk
001200060125     fAZDSE01L  Uf A e           k disk
001300060125     fAZDSTP1L  Uf A e           k disk    prefix(A_)
001400060103     fAZDSS03L  Uf   e           k disk
001500060103     fAZDSS04L  Uf A e           k disk    rename(AZDSS000:AZDSS004)
001600060103      *
001700060103     fTISIT2D   cf   e             workstn infds(DSfmt)
001800060103
001900060103      *---------------------------------------------------------------*
002000060103
002100060103      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200060103      *
002300060103      * - Tasti di funzione
002400060103     d Enter           c                   const(x'F1')
002500060103     d RollDwn         c                   const(x'F4')
002600060103     d RollUp          c                   const(x'F5')
002700060103      * - Caratteri numerici
002800060103     d $DigitN         c                   const('0123456789')
002900060103      *
003000060130     d C_POutente      c                   const('P.O. utente:')
003100060126      *
003200060131     d C1_CPF5223      c                   const('*blank +
003300060131     d                                            ''N'' +
003400060126     d                                            ''T'' +
003500060126     d                                            ''F'' +
003600060126     d                                            ''M''')
003700060131     d C2_CPF5223      c                   const('*blank +
003800060131     d                                            ''S'' +
003900060131     d                                            ''R''')
004000060131     d C3_CPF5223      c                   const('*blank +
004100060131     d                                            ''B'' +
004200060126     d                                            ''C''')
004300060131     d C4_CPF5223      c                   const('*blank +
004400060131     d                                            ''S''')
004500060103      *
004600060103      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004700060103      *
004800060103      * - Decodifiche funzioni
004900060103     d $Op1            s              6    dim(12)  ctdata  perrcd(1)
005000060103     d $Fun            s             20    dim(12)  alt($Op1)
005100060103      * - Messaggi
005200090707     d $Msg            s             78    dim(37)  ctdata  perrcd(1)
005300060103      *
005400060103      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
005500060103      *
005600060103     d KPJBA         e ds
005700060103     d Param           ds
005800060103     d  DEPop0                        3
005900060103     d  DEPpro                        1
006000060103     d  DEPlna                        3  0
006100060103      * - Parametri x Controllo profilo utenti
006200060103     d TIBS34ds      e ds
006300060103      * - Ds di riferimento al file esterno AZUTE00F
006400060103     d AZUTEds       e ds                  extname(AZUTE00F)
006500060125     d dUTE01        e ds
006600060103      * - Ds per dati organigramma
006700070904     d dDatiUte      e ds
006800060103      *
006900060103      * - DS di procedura per passaggio parametri
007000060103     d TISIT0ds      e ds
007100060103     d WrkT0ds       e ds                  extname(TISIT0ds)
007200060125     d                                     prefix(W_)
007300060103     d SavT0ds       e ds                  extname(TISIT0ds)
007400060103     d                                     prefix(S_)
007500060103      *
007600070914      * - Parametri x ricerca/controllo tabelle
007700060103     d TIBS02ds      e ds                  inz
007800060103      * - Tab. "TDP" = Tipi Destinatari Particolari
007900051222     d dTDP          e ds                  inz
008000070914      * - Tab. VPO/DECO = Variabili Programmi Operativi: Decorrenze
008100070914     d dVPOdeco      e ds                  inz
008200060103      *
008300060206      * - Tab. "03"  = Zone consegna
008400060206     d ds03          e ds                  inz
008500090707      *
008600090707      * - Tab. "1A"  = Tipo incasso c/assegno
008700090707     d ds1A          e ds                  inz
008800060103      *
008900060103      * - Flag Operativi di AZDST (DSTFLO)
009000060103     d dDSTflo       e ds                  inz
009100060125      * - Flag Operativi (e non solo) di AZDSE (DSEUNI di trk "FLO")
009200060125     d dDSEflo       e ds                  inz
009300070905      * - Flag Operativi (e non solo) di AZDSE (DSEUNI di trk "XT1")
009400070905     d dDSExt1       e ds                  inz
009500060103      *
009600060103      * - Controllo dati di "instradamento"
009700060103     d TISI95ds      e ds                  inz
009800060103      *
009900060103      * - Controllo numeri Telefono / Fax
010000060103     d TRUL42ds      e ds                  inz
010100060125      *
010200060125      * - Controllo indirizzo e-mail
010300060125     d dsEMAIL       e ds                  inz
010400060103      *
010500060103      * - Reperimento numeratore
010600060103     d TRUL33ds      e ds                  inz
010700070914      *
010800070914      * - Reperimento / Controllo giri consegna
010900070914     d FIDG09DS      e ds                  inz
011000090713      *
011100090713      * - Interrogazione tabella "1A"
011200090713     d TRTB13ds      e ds                  inz
011300060103      *
011400060103     d Status         sds           333
011500060103     d   SDSpgm          *proc
011600060103      *
011700060103     d DSFMT           ds           512
011800060103     d  £Tasto               369    369
011900060103      *
012000060103      * - Controllo/Inversione date
012100060103     d WLBdat          ds                  inz
012200060103     d   G08dat                       8  0 inz
012300060103     d   G08inv                       8  0 inz
012400060103     d   G08err                       1    inz('3')
012500060103     d   G08tgi                       5  0 inz
012600060103      *
012700060103      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
012800060103      *
012900060103      * - Flags
013000060103     d $Fine           s              1    inz(*off)
013100060103     d $InzD01         s              1    inz(*on)
013200060103     d $InzD02         s              1    inz(*on)
013300060103     d $Found          s              1    inz(*off)
013400060103     d $Asterisco      s              1    inz(*off)
013500060126     d $in28           s              1    inz(*off)
013600060103     d $Video          s              1    inz('1')
013700060126     d $VideoP         s              1    inz
013800070913     d wForzaLoc       s              1    inz(*off)
013900070913     d wF17            s              1    inz(*off)
014000060103      *
014100060103      * - Indici di schiera
014200060103     d xx              s              3  0 inz
014300060103      *
014400060103      * - Campi di comodo
014500060126     d wrkRICERCA      s             35    varying      inz
014600060126     d wIn4099         s             60    inz(*zeros)
014700060103     d wDate           s              8  0 inz
014800060103     d wOp1            s              6    inz
014900070913      *
015000060104      * - Campi chiave
015100060104     d K_tip_C         s                   like(DSTtip) inz('C')
015200060125     d K_tip_P         s                   like(DSTtip) inz('P')
015300070913      *
015400060125      * - Dati da AZDSE00F
015500060125      *   - trk GSC = Giorni Stabiliti di Consegna
015600060125     d SavDSE_GSC      s                   like(DSEuni) inz
015700060125      *   - trk OSC = Orari di Scarico
015800060125     d SavDSE_OSC      s                   like(DSEuni) inz
015900060125      *   - trk SAC = Servizi Accessori in Consegna
016000060125     d SavDSE_SAC      s                   like(DSEuni) inz
016100060125      *   - trk IEM = Indirizzo @-Mail
016200060125     d SavDSE_IEM      s                   like(DSEuni) inz
016300090707
016400090707       //--------------------------------------------------------------
016500090707       //?Definizione prototipi procedure.                             ?
016600090707       //--------------------------------------------------------------
016700090707
016800090713       // - Interrogazione tabella "1A"
016900090713      /copy gaitrasrc/srcProtoPR,TRTB13R
017000090707
017100060103      *
017200060103      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
017300060103      *
017400060103      * - AZDST03L
017500060103     c     K07DST03P     klist
017600060104     c                   kfld                    K_tip_C                        *TIPO
017700060126     c                   kfld                    IT0naz                         *NAZIONE
017800060103     c                   kfld                    IT0prv                         *PROVINCIA
017900060103     c                   kfld                    IT0cap                         *CAP
018000060103     c                   kfld                    IT0loc                         *LOCALITA
018100060103     c                   kfld                    IT0rag                         *RAGIONE SOCIALE
018200060103     c                   kfld                    IT0kwi                         *KEYWORD INDIRIZZO
018300060103     c     K07DST03V     klist
018400060104     c                   kfld                    K_tip_C                        *TIPO
018500060126     c                   kfld                    V1Cnaz                         *NAZIONE
018600060104     c                   kfld                    V1Cprv                         *PROVINCIA
018700060104     c                   kfld                    V1Ccap                         *CAP
018800060104     c                   kfld                    V1Cloc                         *LOCALITA
018900060104     c                   kfld                    V1Crag                         *RAGIONE SOCIALE
019000060104     c                   kfld                    V1Ckwi                         *KEYWORD INDIRIZZO
019100060103     c     K06DST03V     klist
019200060104     c                   kfld                    K_tip_C                        *TIPO
019300060126     c                   kfld                    V1Cnaz                         *NAZIONE
019400060104     c                   kfld                    V1Cprv                         *PROVINCIA
019500060104     c                   kfld                    V1Ccap                         *CAP
019600060104     c                   kfld                    V1Cloc                         *LOCALITA
019700060104     c                   kfld                    V1Crag                         *RAGIONE SOCIALE
019800060126      * - AZDSE01L
019900060126     c     K02DSE01      klist
020000060126     c                   kfld                    DSTcod                         *COD.DESTINATARIO
020100060126     c                   kfld                    DSEtrk                         *TIPO RECORD
020200060103      * - AZDSTP1L
020300060103     c     K08DSTP1P     klist
020400060104     c                   kfld                    K_tip_P                        *TIPO
020500060103     c                   kfld                    IT0naz
020600060103     c                   kfld                    IT0prv
020700060103     c                   kfld                    IT0loc
020800060103     c                   kfld                    IT0cap
020900060103     c                   kfld                    IT0rag
021000060103     c                   kfld                    IT0ind
021100060103     c                   kfld                    DEPlna
021200060103     c     K08DSTP1V     klist
021300060104     c                   kfld                    K_tip_P                        *TIPO
021400060104     c                   kfld                    V1Cnaz
021500060104     c                   kfld                    V1Cprv
021600060104     c                   kfld                    V1Cloc
021700060104     c                   kfld                    V1Ccap
021800060104     c                   kfld                    V1Crag
021900060104     c                   kfld                    V1Cind
022000060103     c                   kfld                    DUTpou
022100060103      * - AZDSS03L
022200060103     c     K06DSS03      klist
022300060104     c                   kfld                    V1Cnaz                         *NAIONE
022400060104     c                   kfld                    V1Cprv                         *PROVINCIA
022500060104     c                   kfld                    V1Ccap                         *CAP
022600060104     c                   kfld                    V1Cloc                         *LOCALITA
022700060104     c                   kfld                    V1Crag                         *RAGIONE SOCIALE
022800060104     c                   kfld                    V1Ckwi                         *KEYWORD INDIRIZZO
022900060103     c     K06DSS03P     klist
023000060103     c                   kfld                    S_IT0naz                       *NAIONE
023100060103     c                   kfld                    S_IT0prv                       *PROVINCIA
023200060103     c                   kfld                    S_IT0cap                       *CAP
023300060103     c                   kfld                    S_IT0loc                       *LOCALITA
023400060103     c                   kfld                    S_IT0rag                       *RAGIONE SOCIALE
023500060103     c                   kfld                    S_IT0kwi                       *KEYWORD INDIRIZZO
023600060103      * - AZDSS04L
023700060103     c     K07DSS04      klist
023800060104     c                   kfld                    V1Cnaz                         *NAIONE
023900060104     c                   kfld                    V1Cprv                         *PROVINCIA
024000060104     c                   kfld                    V1Cloc                         *LOCALITA
024100060104     c                   kfld                    V1Crag                         *RAGIONE SOCIALE
024200060104     c                   kfld                    V1Ckwi                         *KEYWORD INDIRIZZO
024300060104     c                   kfld                    V1Ccap                         *CAP
024400060103     c                   kfld                    DSSsin                         *SINONIMO
024500060103      * - TABEL00F
024600060103     c     K03tabel      klist
024700060103     c                   kfld                    TBLkut
024800060103     c                   kfld                    TBLcod
024900060103     c                   kfld                    TBLkey
025000060103
025100060103      *---------------------------------------------------------------*
025200060103      *  RIEPILOGO INDICATORI                                         *
025300060103      *---------------------------------------------------------------*
025400070913      * 07    - Abilita F7                                            *
025500070913      * 17    - Abilita F17                                           *
025600070913      * 25    - Comodo                                                *
025700060103      * 28    - Visualizza messaggio di errore                        *
025800060103      * 40    - Solo visualizzazione                                  *
025900070914      * 41    - Consentita modifica di "Referente"                    *
026000070914      * 42    - Richiesta funzione di inserimento                     *
026100070914      * 43    - Protezione Filiale CGI instradamento prestabilito  e  *
026200070914      *         protezione e non visualizzazione giorni chiusura      *
026300090707      * 45    - Protezione Tipo incasso c/assegni forzato             *
026400060125      *       fmt D01:                                                *
026500060103      * 51    - Ragione sociale          errata o mancante            *
026600060103      * 52    - Indirizzo                errato o mancante            *
026700060103      * 53    - Località                 errata o mancante            *
026800060103      * 54    - C.A.P.                   errato o mancante            *
026900060103      * 55    - Provincia                errata o mancante            *
027000060103      * 56    - Chiave ricerca indirizzo errato o mancante            *
027100060103      * 57    - Tipo destinatario        errato o mancante            *
027200060103      * 58    - Nazione                  errata o mancante            *
027300060103      * 59    - Telefono                 errato o mancante            *
027400060103      * 60    - Referente                errato o mancante            *
027500060110      * 61    - Obbligo DDT in consegna  errato o mancante            *
027600070914      * 62    - LNA instradam. prestab.  errata o mancante            *
027700070914      * 63    - ZNC instradam. prestab.  errata o mancante            *
027800060125      * 64    - Fax                      errato o mancante            *
027900070914      * 65    - Indirizzo @-Mail         errato o mancante            *
028000070914      * 66    - CGI instradam. prestab.  errato o mancante            *
028100070914      * 67    - Filiale CGI instr.prest. errata o mancante            *
028200070914      * 68    - 1° giorno chiusura       errato o mancante            *
028300070914      * 69    - 2° giorno chiusura       errato o mancante            *
028400090707      * 70    - Tipo incasso c/assegni forzato    errato              *
028500060126      *       fmt D02:                                                *
028600070914      * 71    - Modalità di Appuntamento errata o mancante            *
028700060126      * 72    - Tipo comunicazione       errato o mancante            *
028800060126      * 73    - Tipo Modulo Comunicaz.   errato o mancante            *
028900070914      * 74    - Richiesta Copia DDT      errata o mancante            *
029000060125      *                                                               *
029100060103      * 90    - Errore generico                                       *
029200060103      *---------------------------------------------------------------*
029300060103
029400060103     c     *Entry        plist
029500060103     c                   parm                    KPJBA
029600060103     c                   parm                    TISIT0ds
029700060103     c                   movel     KPJBU         Param
029800060103      *
029900060103      * Operazioni Iniziali
030000060103     c                   exsr      RoutInz
030100060103      *
030200060103      * Gestione Video
030300060103do  1c                   DOW       $Fine   = *off
030400060103      * - 1ª videata
030500060103cas 2c     $Video        caseq     '1'           GesD01
030600060110      * - 2ª videata
030700060110cas 2c     $Video        caseq     '2'           GesD02
030800060103e   2c                   endcs
030900060103e   1c                   ENDDO
031000060103      *
031100060103      * Fine
031200060103     c                   clear                   TISI95ds
031300060103     c                   eval      I95tla  = 'C'
031400060103     c                   call      'TISI95R'
031500060103     c                   parm                    TISI95ds
031600060103      *
031700060103     c                   clear                   TIBS02ds
031800060103     c                   eval      T02tla = 'C'
031900060103     c                   call      'TIBS02R'
032000060103     c                   parm                    KPJBA
032100060103     c                   parm                    TIBS02ds
032200060103      *
032300060110     c                   movel(p)  Param         KPJBU
032400060103     c                   movel     *on           *inLR
032500060103      *
032600060103      *---------------------------------------------------------------*
032700060103      * Operazioni Iniziali                                           *
032800060103      *---------------------------------------------------------------*
032900060103     c     RoutInz       BEGSR
033000060103      *
033100070914      * Reperimento dati job
033200060103     c                   exsr      DatiJob
033300060103      *
033400060103     c                   movel     SDSpgm        V1Tpgm
033500060103     c                   eval      TBLkut  = 1
033600060103      *
033700060103      * Reperimento data corrente
033800060103     c                   clear                   WLBdat
033900060103     c                   eval      G08dat  = *date
034000060103     c                   call      'XSRDA8'
034100060103     c                   parm                    WLBdat
034200060103     c                   eval      Wdate   = G08inv
034300070914      *
034400070914      * Aggancio tabella "VPO/DECO" per reperimento data attivazione
034500070914      *   dati in trk "XT1": filiale giro  e  giorni chiusura
034600070914     c                   clear                   dVPOdeco
034700070914     c                   clear                   TIBS02ds
034800070914     c                   eval      T02mod  = 'C'
034900070914     c                   eval      T02sif  = KNSIF
035000070914     c                   eval      T02cod  = 'VPO'
035100070914     c                   movel(P)  'DECO'        T02ke1
035200070914     c                   call      'TIBS02R'
035300070914     c                   parm                    KPJBA
035400070914     c                   parm                    TIBS02ds
035500070914    2c                   if        T02err  = *blanks
035600070914     c                   movel     T02uni        dVPOdeco
035700070914    2c                   endif
035800060103      *
035900060103if  1c                   if            IT0op1  = 'O02'
036000060103     c                             or  IT0op1  = 'P06'
036100060103     c                   eval      SavT0ds = TISIT0ds
036200060103e   1c                   endif
036300060103      *
036400070914      * Impostazione funzione di ingresso come "sottotitolo"
036500060103     c                   eval      xx      = 1
036600070913     c                   eval      *in25   = *off
036700060103     c                   movel(p)  IT0op1        wOp1
036800060103     c                   move      DEPop0        wOp1
036900070913     c     wOp1          lookup    $Op1(xx)                               25
037000060103     c                   if        NOT %equal
037100060103     c                   eval      xx      = 1
037200070913     c                   eval      *in25   = *off
037300060103     c                   movel(p)  IT0op1        wOp1
037400070913     c     wOp1          lookup    $Op1(xx)                               25
037500060103     c                   endif
037600070913     c                   if        *in25
037700060104     c                   movel     $Fun(xx)      V1Tfun
037800060103     c                   endif
037900060103      *
038000060103     c                   ENDSR
038100060103      *
038200060103      *---------------------------------------------------------------*
038300060103      * Reperimento Dati del job (Utente/Operativi)                   *
038400060103      *---------------------------------------------------------------*
038500060103     c     DatiJob       BEGSR
038600060103      *
038700060103     c     *dtaara       define    §azute        azuteds
038800060103     c     *dtaara       define    §datiute      ddatiute
038900060103      *
039000060103     c                   in(E)     *dtaara
039100060103     c                   IF        %ERROR or RSUT = *blanks
039200060103     c                   clear                   Tibs34Ds
039300060103     c                   call      'TIBS34R'
039400060103     c                   parm                    Tibs34Ds
039500060103     c                   in        *dtaara
039600060103     c                   ENDIF
039700060125      *
039800060125     c                   movel     UTEfaf        dUTE01
039900060103      *
040000060103     c                   ENDSR
040100060104      *
040200060104      *---------------------------------------------------------------*
040300060104      * Settaggio indicatori video                                    *
040400060104      *---------------------------------------------------------------*
040500060104     c     SettInd       BEGSR
040600060307      *
040700060104      * Test se solo visualizzazione
040800060104     c                   eval      *in40   = (IT0op1 = 'O04'  or
040900060104     c                                        IT0op1 = 'O05'  or
041000060104     c                                        IT0op1 = 'O07'  or
041100060104     c                                        IT0op1 = 'P05'  or
041200060104     c                                        IT0op1 = 'P15')
041300060104      *
041400060130      * Se entrata per PARTICOLARITÀ: modifica solo TEL/REF
041500060104if  1c                   if             DEPop0  = 'P03'
041600060104     c                             and (IT0op1  = 'O02'
041700060104     c                              or  IT0op1  = 'P06')
041800060104     c                   eval      *in40   = *on
041900070914     c                   eval      *in41   = *on
042000060104e   1c                   endif
042100060104      *
042200060104      * Test se richiesta qualche funzione di inserimento
042300060104      *   (protezione "Tipo destinatario")
042400070914     c                   eval      *in42   = (IT0op1 = 'F06'  or
042500060104     c                                        IT0op1 = 'P06')
042600070914      *
042700070914      * Test se iniziata nuova gestione del trk "XT1" di AZDSE00F
042800070914     c                   eval      *in43   = (§VPOxt1 > Wdate)
042900090707      /free
043000090710         // Test se utente con la dovuta autorizzazione
043100090707         *in45 = (§UTEdst < '9');
043200090707      /end-free
043300070913      *
043400070914      * Abilitazione tasti funzionali
043500070914      * - F7  = Selezione Giri Consegna
043600070914     c                   eval      *in07   = (NOT *in40)
043700070914      * - F17 = Forza località
043800070914     c                   eval      *in17   = *off
043900060104      *
044000060104     c                   ENDSR
044100060103      *
044200060103      *---------------------------------------------------------------*
044300060103      * Gestione videata D01                                          *
044400060103      *---------------------------------------------------------------*
044500060103     c     GesD01        BEGSR
044600060103      *
044700070914      * Inizializzazione videata
044800060103if  1c                   if        $InzD01 = *on
044900060103     c                   exsr      InzD01
045000060103     c                   movel     *off          $InzD01
045100060131if  2c                   if            (DepPRO <> 'A'
045200060131     c                             or  (IT0op1 <> 'P02'
045300060131     c                             and  IT0op1 <> 'PF6'))
045400060131     c                             and  IT0op1 <> 'P05'
045500060126     c                   exsr      InzD02
045600060126     c                   movel     *off          $InzD02
045700060126     c                   reset                   $Video
045800060131e   2c                   endif
045900060103e   1c                   endif
046000060103      *
046100070914      * Scrittura testata e riga tasti funzionali abilitati
046200060103     c                   write     SIT2T01
046300060130     c                   if             DepPRO <> 'A'
046400060130     c                             or  (IT0op1 <> 'P02'
046500060130     c                             and  IT0op1 <> 'PF6')
046600060130      * - tasti funzionali in visualizzazione
046700060103     c                   write     SIT2P01
046800060130     c                   else
046900060130      * - tasti funzionali in manutenzione
047000060130     c                   write     SIT2P02
047100060130     c                   endif
047200060126      *
047300060126if  1c                   if        *in90
047400060126     c                   eval      $in28   = (*in28 = *on)
047500060126     c                   eval      *in28   = *off
047600060126     c                   movea     *in(40)       Win4099
047700060126     c                   movea     *zeros        *in(51)
047800060130     c                   if            (DepPRO <> 'A'
047900060130     c                             or  (IT0op1 <> 'P02'
048000060130     c                             and  IT0op1 <> 'PF6'))
048100060130     c                             and  IT0op1 <> 'P05'
048200060126     c                   write     SIT2D01
048300060130     c                   else
048400060130     c                   write     SIT2DP1
048500060130     c                   endif
048600060126     c                   movea     Win4099       *in(40)
048700060126     c                   eval      *in28   = ($in28 = *on)
048800060126     c                   endif
048900060126     c                   eval      $VideoP = $Video
049000060126      *
049100070914      * Emissione videata
049200060130     c                   if            (DepPRO <> 'A'
049300060130     c                             or  (IT0op1 <> 'P02'
049400060130     c                             and  IT0op1 <> 'PF6'))
049500060130     c                             and  IT0op1 <> 'P05'
049600060103     c                   exfmt     SIT2D01
049700060130     c                   else
049800060130     c                   exfmt     SIT2DP1
049900060130     c                   endif
050000060103     c                   setoff                                       28  90
050100060103     c                   clear                   V1Dmsg
050200060103      *
050300060103sel 1c                   select
050400060103      * F3=Fine
050500060103w   1c                   when      *inKC
050600060103     c                   exsr      F03D01
050700060103     c                   leavesr
050800070913      * F7=Selezione Giri Consegna
050900070913w   1c                   when      *inKG
051000070913     c                   exsr      F07D01
051100070913     c                   leavesr
051200060103      * F12=Ritorno
051300060103w   1c                   when      *inKL
051400060103     c                   exsr      F03D01
051500060103     c                   leavesr
051600060104      * RollUp=Videata successiva
051700060125w   1c                   when      £Tasto  = RollUp
051800090709      * - Meglio NON eseguire qui i controlli (per RollUp):
051900090709      *   verranno eseguiti nella videata successiva, rivisualizzanto
052000090709      *   (in caso di errori) questa videata.
052100090707if  2c                   if             IT0op1 <> 'O05'
052200090707     c                             and  IT0op1 <> 'P15'
052300090709     c                             and  '0'    <> '0'
052400070917     c                   exsr      CtrD01
052500090707e   2c                   endif
052600070917if  2c                   if        NOT *in90
052700060104     c                   eval      $Video  = '2'
052800060104     c                   movea     *zeros        *in(51)
052900070917e   2c                   endif
053000060104     c                   leavesr
053100060103      *
053200060103e   1c                   endsl
053300060103      *
053400060110      * Invio o F6
053500060103sel 1c                   select
053600060125     *** *
053700060125     *** * Opzione "Annullamento" o "Ripristino"
053800060125w   1***c                   when           IT0op1  = 'O04'
053900060125     ***c                             or   IT0op1  = 'O07'
054000060125      *
054100060131      * Opzione "Annullamento" => "Cancellazione"
054200060130w   1c                   when      IT0op1  = 'O04'
054300090707     c                   exsr      sr_CtrlDelete
054400090707     c                   if        NOT *in90
054500060125     c                   eval      $Video  = '2'
054600090707     c                   endif
054700060125     c                   leavesr
054800060103      *
054900060130      * Opzione Visualizzazione Proposta
055000060130      * => si esce dal pgm.
055100060130w   1c                   when      IT0op1  = 'P05'
055200060130     c                   eval      $Fine   = *on
055300060130     c                   leavesr
055400060130      * Opzione Visualizzazione anagrafica
055500060125      * => si passa alla 2ª videata
055600060130w   1c                   when           IT0op1  = 'O05'
055700060130     c                             or   IT0op1  = 'P15'
055800060125     c                   eval      $Video  = '2'
055900060110     c                   leavesr
056000060103      *
056100060103      * Altrimenti
056200060103      * => si controllano i dati di questa videata
056300060110x   1c                   other
056400060103     c                   exsr      CtrD01
056500060110if  2c                   if        *in90
056600060103     c                   leavesr
056700060110e   2c                   endif
056800060130      * => SE gestione PROPOSTA: non ci sono altre videate
056900060130if  2c                   if             DepPRO  = 'A'
057000060130     c                             and (IT0op1  = 'P02'
057100060130     c                              or  IT0op1  = 'PF6')
057200060130      *    F6=Conferma Proposta
057300060130if  3c                   if        *inKF
057400060130     c                   exsr      F06D02
057500060130if  4c                   if        *in90   = *off
057600060130     c                   eval      $Fine   = *on
057700060130e   4c                   endif
057800060130e   3c                   endif
057900060130     c                   leavesr
058000060130e   2c                   endif
058100060130      * => ALTRIMENTI:
058200060130      *    => si controllano i dati delle videate successive
058300060125     c                   exsr      CtrD02
058400060130      *    => e si passa alla videata successiva
058500060125     c                   eval      $Video  = '2'
058600060125     c*** già così:      eval      $InzD02 = *on
058700060110      *
058800060110e   1c                   endsl
058900060103      *
059000060103     c                   ENDSR
059100060103      *
059200060103      *---------------------------------------------------------------*
059300060103      * Inizializzazione videata D01                                  *
059400060103      *---------------------------------------------------------------*
059500060103     c     InzD01        BEGSR
059600060103      *
059700060130     c                   clear                   SIT2DP1
059800060130     c                   clear                   SIT2D01
059900060110     c                   clear                   SIT2D02
060000060125      *
060100060125     c                   clear                   dTDP
060200060110      *
060300060110      * Settaggio indicatori
060400060110     c                   exsr      SettInd
060500060110      *
060600070914      * Se entrata da proposte: propone "tipo destinatario = A"
060700060110     c                   if             DEPpro = 'A'
060800060110     c                             or   IT0op1 = 'P05'
060900070914     c                   eval      V1Cdos  = 'A'
061000060110     c                   endif
061100060110      *
061200060125      * Indicazione di videata successiva ("Segue...")
061300060125      *           o di ultima videata     ("Fine")
061400060130      * - se NON propopsta
061500060130     c                   if            (DepPRO <> 'A'
061600060130     c                             or  (IT0op1 <> 'P02'
061700060130     c                             and  IT0op1 <> 'PF6'))
061800060130     c                             and  IT0op1 <> 'P05'
061900060125     c                   eval      V1Dseg  = 'Segue...'
062000060130     c***                else
062100060130     c***                eval      V1Dseg  = 'Fine    '
062200060130     c                   endif
062300060103      *
062400060103      * Se richiesto inserimento non si caricano dati
062500060104     c                   if             IT0op1  = 'F06'
062600060104     c                             or   IT0op1  = 'PF6'
062700060103     c                   leavesr
062800060103     c                   endif
062900060103      *
063000060103      * Reperimento dati da aggiornare
063100060103     c                   exsr      Rep_AZDST
063200060125     c                   exsr      Rep_AZDSE
063300060103      *
063400060130      * Impostazione dei dati in D01/DP1
063500060103     c                   exsr      RieD01
063600060110      *
063700060110      * Decodifiche
063800060110     c                   exsr      CtrD01
063900060110     c                   setoff                                       28  90
064000060110     c                   movea     *zeros        *in(51)
064100060302      * Errore in fase di caricamento dati
064200060125     c                   if        xx      > *zeros
064300060125     c                   seton                                        28  90
064400060125     c                   movel     $Msg(xx)      V1Dmsg
064500060125     c                   endif
064600060103      *
064700060103     c                   ENDSR
064800060103      *
064900060103      *---------------------------------------------------------------*
065000060103      * Impostazione dati nella videata D01                           *
065100060103      *---------------------------------------------------------------*
065200060103     c     RieD01        BEGSR
065300060103      *
065400060103     c                   clear                   dDSTflo
065500060103      *
065600060104     c                   SELECT
065700060103      *
065800060103      * da proposte appuntamenti
065900060104     c                   WHEN           DEPpro = 'A'
066000060104     c                             or   IT0op1 = 'P05'
066100060125     c                   movel     A_DSTrag      V1Crag
066200060125     c                   movel     A_DSTind      V1Cind
066300060125     c                   movel     A_DSTcap      V1Ccap
066400060125     c                   movel     A_DSTloc      V1Cloc
066500060125     c                   movel     A_DSTprv      V1Cprv
066600060125     c                   movel     A_DSTnaz      V1Cnaz
066700060125     c                   movel     A_DSTdos      V1Cdos
066800060125     c                   movel     A_DSTtel      V1Ctel
066900060125     c                   movel     A_DSTref      V1Cref
067000060130     c                   movel     A_DSTute      V1Cute
067100060130     c                   movel     C_POutente    V1Kpou
067200060130     c                   z-add     A_DSTpou      V1Cpou
067300060130     c                   z-add     A_DSTdiv      V1Cdiv
067400060130     c                   movel     A_DSTute      V1Cute
067500060130     c                   z-add     A_DSTaas      V1Cbaas
067600060130     c                   z-add     A_DSTlnp      V1Cblnp
067700060130     c                   z-add     A_DSTnrs      V1Cbnrs
067800060130     c                   z-add     A_DSTnsp      V1Cbnsp
067900060220      * da conferma proposte
068000060220     c                   WHEN      IT0OP1  = 'P06'
068100060220     c                   movel     IT0rag        V1Crag
068200060220     c                   movel     IT0ind        V1Cind
068300060220     c                   movel     IT0cap        V1Ccap
068400060220     c                   movel     IT0loc        V1Cloc
068500060220     c                   movel     IT0prv        V1Cprv
068600060220     c                   movel     IT0naz        V1Cnaz
068700060220     c                   movel     IT0dos        V1Cdos
068800060220     c                   movel     IT0tel        V1Ctel
068900060220     c                   movel     IT0ref        V1Cref
069000060220     c                   movel     IT0kwi        V1Ckwi
069100060220     c                   movel     C_POutente    V1Kpou
069200060131      * da destinatari
069300060104     c                   OTHER
069400100204     c                   if        IT0op1 <> 'O03'
069500060104     c                   z-add     DSTcod        V1Ccod
069600100204     c                   endif
069700060104     c                   movel     DSTkwi        V1Ckwi
069800060104     c                   movel     DSTrag        V1Crag
069900060104     c                   movel     DSTind        V1Cind
070000060104     c                   movel     DSTcap        V1Ccap
070100060104     c                   movel     DSTloc        V1Cloc
070200060104     c                   movel     DSTprv        V1Cprv
070300060104     c                   movel     DSTnaz        V1Cnaz
070400060104     c                   movel     DSTdos        V1Cdos
070500060125     c                   movel     DSTref        V1Cref
070600060104     c                   movel     DSTtel        V1Ctel
070700060104     c                   movel     DSTnot        V1Cnot
070800060103     c                   movel     DSTflo        dDSTflo
070900060125     c                   movel     §DSEfax       V1Cfax
071000060125     c                   movel     SavDSE_IEM    V1Ciem
071100060103      *
071200060104     c                   ENDSL
071300060110      *
071400060110      * Dati da DSTFLO:
071500060110      * - Obbligo DDT in consegna
071600060110     c                   eval      V1Cddt  = §DSTddt
071700060110      * - Linea e zona di instradamento predefinito
071800060110     c                   if        §DSTlna > *zeros
071900060110     c                   move      §DSTlna       V1Clna
072000060110     c                   move      §DSTznc       V1Cznc
072100070905     c*****              move      §DSTcgi       V1Ccgi
072200060110     c                   else
072300060110     c                   clear                   V1Clna
072400060110     c                   clear                   V1Cznc
072500070905     c*****              clear                   V1Ccgi
072600060110     c                   endif
072700070905      *
072800070905      * Dati da DSEFLO:
072900070905     c                   if        §DSEXT1cgi <> *blanks
073000070905      * - Filiale gestione giro consegna
073100070905     c                   eval      V1Cpocgi    = §DSEXT1pog
073200070905      * - Codice giro Tellus
073300070905     c                   eval      V1Ccgi      = §DSEXT1cgi
073400070905     c                   endif
073500070913      * - Giorni di chiusura
073600070913     c                   eval      V1Cgc1  = §DSEXT1gc1
073700070913     c                   eval      V1Cgc2  = §DSEXT1gc2
073800090707      * - Tipo incasso c/assegni forzato
073900090707     c                   eval      V1Cticf = §DSEXT1tic
074000060104      *
074100060104      * Decodifiche:
074200060104      * - Tipo destinatario
074300060104     c                   exsr      Decod_DOS
074400060104      *
074500060104     c                   ENDSR
074600060103      *
074700060103      *---------------------------------------------------------------*
074800060103      * Gestione tasto funzionale F03 da videata D01                  *
074900060103      *---------------------------------------------------------------*
075000060103     c     F03D01        BEGSR
075100060103      *
075200060103      * Chiudo il programma
075300060103     c                   eval      $Fine   = *on
075400060103      *
075500060103     c                   ENDSR
075600070913      *
075700070913      *---------------------------------------------------------------*
075800070913      * Gestione tasto funzionale F07 da videata D01                  *
075900070913      *---------------------------------------------------------------*
076000070913     c     F07D01        BEGSR
076100070917      *
076200070917     c                   movea     *zeros        *in(51)
076300070917if  1c                   if            *in43   = *on
076400070917     c                             and V1Clna <= *zeros
076500070917     c                   seton                                        286290
076600070917     c                   eval      V1Dmsg   = $Msg(35)
076700070917     c                   eval      $Video   = '1'
076800070917     c                   leavesr
076900070917e   1c                   endif
077000070913      *
077100070913      * Ricerca Codice Giro consegna
077200070913     c                   clear                   FIDG09ds
077300070913     c                   eval      D09iop0  = 'P01'
077400070913if  1c                   if        V1Clna   > *zeros
077500070913     c                   move      V1Clna        D09ifgs
077600070913x   1c***                else
077700070913     c***                eval      D09ifgs  = V1Cpocgi
077800070913e   1c                   endif
077900070913     c                   eval      D09idat  = Wdate
078000070913     c                   eval      D09itug  = 'C'
078100070913      *
078200070913     c                   eval      KPJBU    = FIDG09ds
078300070913     c                   call      'FIDG09R'
078400070913     c                   parm                    KPJBA
078500070913     c                   eval      FIDG09ds = KPJBU
078600070913      *
078700070913if  1c                   if        D09oerr  = *on
078800070913     c                   seton                                        286690
078900070913     c                   eval      V1Dmsg   = $Msg(11)
079000070913     c                   eval      $Video   = '1'
079100070913     c                   leavesr
079200070913x   1c                   else
079300070913     c                   eval      V1Cpocgi = D09ofgs
079400070913     c                   eval      V1Ccgi   = D09ocgi
079500070913     c                   eval      V1Dcgi   = D09odes
079600070913e   1c                   endif
079700070913      *
079800070913     c                   ENDSR
079900060103      *
080000060103      *---------------------------------------------------------------*
080100060103      * Controllo dati immessi in videata D01                         *
080200060103      *---------------------------------------------------------------*
080300060103     c     CtrD01        BEGSR
080400060103      *
080500060103     c                   movea     *zeros        *in(51)
080600060110      *
080700060110      * Pulizia decodifiche per campi non immessi
080800060110     c                   if        V1Clna  = *blanks
080900060110     c                   clear                   V1Dlna
081000060110     c                   endif
081100070913     c***                if        V1Cznc  = *blanks
081200070913     c***                clear                   V1Dznc
081300070913     c***                endif
081400070323     c                   if        V1Ccgi  = *blanks
081500070323     c                   clear                   V1Dcgi
081600070323     c                   endif
081700060125      *
081800060125      * Tipo Destinatario
081900060125     c*** meglio di no:  clear                   V1Ddos
082000060125     c     '?'           scan      V1Cdos
082100060125      * - Ricerca
082200060125if  1c                   IF        %found
082300060125     c                   clear                   V1Cdos
082400070914     c                   clear                   TIBS02ds
082500070914     c                   eval      T02mod  = 'R'
082600070914     c                   eval      T02sif  = KNSIF
082700070914     c                   eval      T02cod  = 'TDP'
082800060125     c                   call      'TIBS02R'
082900060125     c                   parm                    KPJBA
083000060125     c                   parm                    TIBS02ds
083100060125if  2c                   if             T02err  = *blanks
083200060125     c                             and  T02ke1 <> *blanks
083300060125     c                   movel     T02ke1        V1Cdos
083400060125     c                   movel     T02uni        dTDP
083500060125     c                   movel     §TDPdes       V1Ddos
083600060125     c                   seton                                          5790
083700070912     c                   eval      $Video  = '1'
083800060125     c                   leavesr
083900060125x   2c                   else
084000060125     c                   seton                                        285790
084100060125     c                   movel     $Msg(12)      V1Dmsg
084200070912     c                   eval      $Video  = '1'
084300060125     c                   leavesr
084400060125e   2c                   endif
084500060125      * - Controllo
084600060125x   1c                   ELSE
084700060125     c                   exsr      Decod_DOS
084800060125if  2c                   if        T02err  = *blanks
084900060125     c                   movel     §TDPdes       V1Ddos
085000060125x   2c                   else
085100060125     c                   seton                                        285790
085200060125     c                   movel     $Msg(22)      V1Dmsg
085300070912     c                   eval      $Video  = '1'
085400060125     c                   leavesr
085500060125e   2c                   endif
085600060125e   1c                   ENDIF
085700060125      *
085800060125      * Controllo tipo destinatario in relazione alla provenienza
085900060125      *   particolare
086000070904if  1c                   if            (DEPpro  = 'A'
086100060130     c                             or   IT0op1  = 'P05')
086200060125     c                             and  §TDPins = 'N'
086300060125     c                   seton                                        285790
086400060125     c                   movel     $Msg(21)      V1Dmsg
086500070912     c                   eval      $Video  = '1'
086600060125     c                   leavesr
086700060125e   1c                   endif
086800060103      *
086900060103      * Controllo dati di instradamento
087000070904      *   (NON se visualizzazione o
087100060302      *   (NON se inzializzazione - prima del 1° Enter)
087200060302if  1c                   if        NOT  *in40
087300060302     c                             and  $Inzd01 = *off
087400060103     c                   exsr      Chk_INSTR
087500060103     c                   if        *in90
087600070912     c                   eval      $Video  = '1'
087700060103     c                   leavesr
087800060103     c                   endif
087900060302e   1c                   endif
088000060103      *
088100060103      * La keyword indirizzo deve essere in esso contenuta
088200060104if  1c                   if        V1Ckwi  <> *blanks
088300060104     c                   eval      %len(wrkRICERCA) = %len(%trim(V1Ckwi))
088400060104     c                   eval      wrkRICERCA = %trim(V1Ckwi)
088500060104     c     wrkRICERCA    scan      V1Cind
088600060103if  2c                   if        NOT %found
088700060103     c                   seton                                        285690
088800060103     c                   movel     $Msg(13)      V1Dmsg
088900070912     c                   eval      $Video  = '1'
089000060103     c                   leavesr
089100060103e   2c                   endif
089200060103e   1c                   endif
089300060110      *
089400060110      * La keyword indirizzo diversa da "*" deve essere lunga almeno
089500060110      *   2 caratteri
089600060110if  1c                   if             DEPpro             <> 'A'
089700060110     c                             and  V1Ckwi             <> '*'
089800060110     c                             and  %len(%trim(V1Ckwi)) < 2
089900060110     c                   seton                                        285690
090000060110     c                   movel     $Msg(19)      V1Dmsg
090100070912     c                   eval      $Video  = '1'
090200060110     c                   leavesr
090300060110e   1c                   endif
090400060103      *
090500060103      * Non è più ammesso l'utilizzo dei caratteri "jolly"
090600060104     c     '*'           scan      V1Crag
090700060103if  1c                   if        %found
090800060103     c                   seton                                        285190
090900060103     c                   movel     $Msg(20)      V1Dmsg
091000070912     c                   eval      $Video  = '1'
091100060103     c                   leavesr
091200060103e   1c                   endif
091300060104     c     '*'           scan      V1Cind
091400060103if  1c                   if        %found
091500060103     c                   seton                                        285290
091600060103     c                   movel     $Msg(20)      V1Dmsg
091700070912     c                   eval      $Video  = '1'
091800060103     c                   leavesr
091900060103e   1c                   endif
092000060104     c     '*'           scan      V1Ckwi
092100060103if  1c                   if        %found
092200060103     c                   seton                                        285690
092300060103     c                   movel     $Msg(20)      V1Dmsg
092400070912     c                   eval      $Video  = '1'
092500060103     c                   leavesr
092600060103e   1c                   endif
092700060103      *
092800060103      * Non è ammesso l'inserimento per medesima NAZ/PRV/CAP/LOC/RAG
092900060103      *   di un record con "*" qualora vi siano già altri record... e viceversa
093000060103      * - Verifico se esistono già records
093100060125     c     K06DST03V     setll     AZDST000
093200060125if  1c                   if        %equal(AZDST03L)
093300060103     c                   eval      $Found     = *on
093400060103     c                   eval      $Asterisco = *off
093500060103     c                   else
093600060103     c                   eval      $Found     = *off
093700060125e   1c                   endif
093800060103      * - Se già ci sono:
093900060103      *   verifico se hanno già "*" sulla keyword indirizzo
094000060103      *   A questo punto NON importa continuare a leggere:
094100060103      *   ho già tutte le discriminanti
094200060125if  1c                   if            $Found        = *on
094300060103     c                             and %trim(DSTkwi) = '*'
094400060103     c                             and DSTkwi       <> IT0kwi
094500060103     c                   eval      $Asterisco = *on
094600060125e   1c                   endif
094700060103      * - Quindi:
094800060103      *   - se sto cercando di inserire un rec. senza "*" sulla keyword
094900060103      *     indirizzo ed ho già dei rec. senza "*" => posso inserire;
095000060103      *   - posso inserire, naturalmente, anche quando NON ho affatto
095100060103      *     alcun rec.
095200060125if  1c                   if            %trim(V1Ckwi) <> '*'
095300060103     c                             and $Found         = *on
095400060103     c                             and $Asterisco     = *on
095500060103     c                   seton                                        28  90
095600060103     c                   seton                                        5256
095700060103     c                   movel     $Msg(10)      V1Dmsg
095800070912     c                   eval      $Video  = '1'
095900060103     c                   leavesr
096000060125e   1c                   endif
096100060103      *
096200060103      * Campi obbligatori
096300060103sel 1c                   select
096400060104w   1c                   when      V1Crag  = *blanks
096500060103     c                   seton                                        285190
096600060104w   1c                   when      V1Cind  = *blanks
096700060103     c                   seton                                        285290
096800060104w   1c                   when      V1Cloc  = *blanks
096900060103     c                   seton                                        285390
097000060104w   1c                   when      V1Ccap  = *blanks
097100060103     c                   seton                                        285490
097200060104w   1c                   when      V1Cprv  = *blanks
097300060103     c                   seton                                        285590
097400060104w   1c                   when      V1Ckwi  = *blanks   and
097500060103     c                             DEPpro <> 'A'
097600060103     c                   seton                                        285690
097700060104w   1c                   when      V1Cdos  = *blanks
097800060103     c                   seton                                        285790
097900060104w   1c                   when      V1Cdos  = 'A'       and
098000060104     c                             V1Ctel  = *blanks
098100060103     c                   seton                                        285990
098200060103e   1c                   endsl
098300060103if  1c                   if        *in28
098400060103     c                   movel     $Msg(12)      V1Dmsg
098500070912     c                   eval      $Video  = '1'
098600060103     c                   leavesr
098700060103e   1c                   endif
098800060103      *
098900060103      * Numero di telefono formalmente corretto
099000060104if  1c                   if        V1Ctel <> *blanks
099100060103     c                   clear                   TRUL42ds
099200060104     c                   eval      D42fax  = V1Ctel
099300060103     c                   call      'TRUL42R'
099400060103     c                   parm                    TRUL42ds
099500060103if  2c                   if        D42err  = *on
099600060103     c                   seton                                        285990
099700060103     c                   movel     D42msg        V1Dmsg
099800070912     c                   eval      $Video  = '1'
099900060103     c                   leavesr
100000060104e   2c                   endif
100100060104e   1c                   endif
100200060125      *
100300060125      * Numero di fax formalmente corretto
100400060125if  1c                   if        V1Cfax <> *blanks
100500060125     c                   clear                   TRUL42ds
100600060125     c                   eval      D42fax  = V1Cfax
100700060125     c                   call      'TRUL42R'
100800060125     c                   parm                    TRUL42ds
100900060125if  2c                   if        D42err  = *on
101000060125     c                   seton                                        286490
101100060125     c                   movel     D42msg        V1Dmsg
101200070912     c                   eval      $Video  = '1'
101300060125     c                   leavesr
101400060125e   2c                   endif
101500060125e   1c                   endif
101600060125      *
101700060125      * Indirizzo e-mail formalmente corretto
101800060302      *   (NON se visualizzazione o annullamento)
101900060302      *   (NON se inzializzazione - prima del 1° Enter)
102000060125if  1c                   if        V1Ciem <> *blanks
102100060302     c                             and  *in40   = *off
102200060302     c                             and  $Inzd01 = *off
102300060125     c                   clear                   dsEMAIL
102400060125     c                   movel     V1Ciem        §EMLindi
102500060125     c                   call      'XEMAIL'
102600060125     c                   parm                    dsEMAIL
102700060126if  2c                   if        §EMLerro = *on
102800060125     c                   seton                                        286590
102900060125     c                   movel     §EMLmsgo      V1Dmsg
103000070912     c                   eval      $Video  = '1'
103100060125     c                   leavesr
103200060125x   2c                   else
103300060125     c                   movel     §EMLindo      V1Ciem
103400060125e   2c                   endif
103500060125e   1c                   endif
103600060110      *
103700060110      *
103800060110      * Obbligo DDT alla consegna
103900060110if  1c                   if             V1Cddt <> *blanks
104000060110     c                             and  V1Cddt <> 'S'
104100060110     c                   seton                                        286190
104200060110     c                   movel     $Msg(23)      V1Dmsg
104300070912     c                   eval      $Video  = '1'
104400060110     c                   leavesr
104500060110e   1c                   endif
104600060110      *
104700060110      * Instradamento predefinito: Linea
104800060110if  1c                   IF        V1Clna <> *blanks
104900060110     c     '?'           scan      V1Clna
105000060110      * - Ricerca
105100060110if  2c                   IF        %found
105200060110     c                   clear                   V1Clna
105300060110     c                   clear                   V1Dlna
105400060110     c                   call      'TNSD24R'
105500060110     c                   parm      *blanks       §Cod              3
105600060110     c                   parm      'F'           §Tip              1
105700060110     c                   parm      *blanks       §Des             25
105800060110     c                   parm      *blanks       §Pra              3
105900060110     c                   movel     §Cod          V1Clna
106000060110     c                   movel     §Des          V1Dlna
106100060110     c                   seton                                          6290
106200070912     c                   eval      $Video  = '1'
106300060110     c                   leavesr
106400060110      * - Controllo
106500060110x   2c                   ELSE
106600060206     c                   clear                   V1Dlna
106700060110     c     $DigitN       check     V1Clna
106800060110if  3c                   if        %error
106900060110     c                   seton                                        286290
107000060110     c                   movel     $Msg(24)      V1Dmsg
107100070912     c                   eval      $Video  = '1'
107200060110     c                   leavesr
107300060110e   3c                   endif
107400060110     c                   move      V1Clna        ORGfil
107500060110     c     ORGfil        chain     AZORG
107600060110if  3c                   if             %found(AZORG01L)
107700060110     c                             and  ORGfva = *blanks
107800060110     c***                          and  ORGfag = 'F'
107900060110     c                   movel     ORGdes        V1Dlna
108000060110x   3c                   else
108100060110     c                   seton                                        286290
108200060110     c                   movel     $Msg(24)      V1Dmsg
108300070912     c                   eval      $Video  = '1'
108400060110     c                   leavesr
108500060110e   3c                   endif
108600060110e   2c                   ENDIF
108700060110e   1c                   ENDIF
108800060110      *
108900060110      * Instradamento predefinito: Zona
109000060110      * (obbligatoria se immessa LNA - ma valida anche "0")
109100060206     c                   clear                   ds03
109200060110if  1c                   if            (V1Clna <> *blanks
109300060110     c                             and  V1Cznc  = *blanks)
109400060110     c                             OR  (V1Clna  = *blanks
109500060110     c                             and  V1Cznc <> *blanks)
109600060110     c                   seton                                        286390
109700060110     c                   movel     $Msg(25)      V1Dmsg
109800070912     c                   eval      $Video  = '1'
109900060110     c                   leavesr
110000060110e   1c                   endif
110100060110     c                   IF        V1Cznc <> *blanks
110200070913     c***                clear                   V1Dznc
110300060110     c     $DigitN       check     V1Cznc
110400060110if  2c                   if        %error
110500060110     c                   seton                                        286390
110600060110     c                   movel     $Msg(25)      V1Dmsg
110700070912     c                   eval      $Video  = '1'
110800060110     c                   leavesr
110900060110e   2c                   endif
111000060110     c                   eval      TBLcod  = '03'
111100060110     c                   eval      TBLkey  = V1Clna
111200060110     c                                     + V1Cznc
111300060110     c*** (fosse numerico)                 + %trim(%editw(V1Cznc:'0  '))
111400060110     c     K03TABEL      chain     TABEL
111500060110if  2c                   if             %found(TABEL00F)
111600060110     c                             and  TBLflg = *blanks
111700060206     c                   movel     TBLuni        ds03
111800070913     c***                movel     §03des        V1Dznc
111900060110x   2c                   else
112000060110     c                   seton                                        286390
112100060110     c                   movel     $Msg(25)      V1Dmsg
112200070912     c                   eval      $Video  = '1'
112300060110     c                   leavesr
112400060110e   2c                   endif
112500060206if  2c                   if        §03nuc = 'S'
112600060206     c                   seton                                        286390
112700060206     c                   movel     $Msg(30)      V1Dmsg
112800070912     c                   eval      $Video  = '1'
112900060206     c                   leavesr
113000060206e   2c                   endif
113100060110e   1c                   ENDIF
113200060110      *
113300060110      * Instradamento predefinito incompleto
113400060110if  1c                   if            (V1Clna =  *blanks
113500060110     c                              or  V1Clna =  *zeros)
113600060110     c                             and  V1Cznc <> *blanks
113700060110     c                             and  V1Cznc <> *zeros
113800060110     c                   seton                                        286290
113900060110     c                   movel     $Msg(24)      V1Dmsg
114000070912     c                   eval      $Video  = '1'
114100060110     c                   leavesr
114200060110e   1c                   endif
114300070323      *
114400070323      * Instradamento predefinito: Giro consegna
114500070918      * - *Temp
114600070918if  1c                   if        *in43   = *on
114700070918if  2c                   if        V1Ccgi  = *blanks
114800070918     c                   clear                   V1Cpocgi
114900070918x   2c                   else
115000070918     c                   move      V1Clna        V1Cpocgi
115100070918e   2c                   endif
115200070918e   1c                   endif
115300070912      * - Filiale del giro consegna mancante
115400070912if  1c                   if             V1Cpocgi = *zeros
115500070912     c                             and  V1Ccgi  <> *blanks
115600070912if  2c                   if        V1Clna  > *zeros
115700070912     c                   move      V1Clna        V1Cpocgi
115800070912x   2c                   else
115900070917if  3c                   if        NOT *in43
116000070912     c                   seton                                        286790
116100070912     c                   eval      V1Dmsg  = $Msg(9)
116200070917x   3c                   else
116300070917     c                   seton                                        286690
116400070917     c                   eval      V1Dmsg  = $Msg(35)
116500070917e   3c                   endif
116600070912     c                   eval      $Video  = '1'
116700070912     c                   leavesr
116800070912e   2c                   endif
116900070323e   1c                   endif
117000070912      * - Filiale del giro consegna errata
117100070912if  1c                   IF             V1Cpocgi> *zeros
117200070912      * - - deve coincidere con la LNA forzata (se inserita)
117300070912if  2c                   if             V1Clna  > *zeros
117400070912     c                             and  V1Clna <> %trim( %editw(
117500070912     c                                            V1Cpocgi : '0   ' ) )
117600070912     c                   seton                                        286790
117700070912     c                   eval      V1Dmsg  = $Msg(31)
117800070912     c                   eval      $Video  = '1'
117900070912     c                   leavesr
118000070912e   2c                   endif
118100070912      * - - deve esistere
118200070912if  2c                   if        V1Clna <> %trim( %editw(
118300070912     c                                       V1Cpocgi : '0   ' ) )
118400070912     c     V1Cpocgi      chain     AZORG
118500070912if  3c                   if        NOT  %found(AZORG01L)
118600070912     c                             or   ORGfva <> *blanks
118700070912     c***                          or   ORGfag <> 'F'
118800070912     c                   seton                                        286790
118900070912     c                   eval      V1Dmsg  = $Msg(24)
119000070912     c                   eval      $Video  = '1'
119100070912     c                   leavesr
119200070912e   3c                   endif
119300070912e   2c                   endif
119400070912e   1c                   endif
119500070912      * - Codice giro consegna mancante
119600070912if  1c                   if            (V1Cpocgi > *zeros
119700070912     c                             and  V1Ccgi   = *blanks)
119800070912     c                   seton                                        286690
119900070912     c                   eval      V1Dmsg  = $Msg(32)
120000070912     c                   eval      $Video  = '1'
120100070912     c                   leavesr
120200070912e   1c                   endif
120300070912      * - Codice giro consegna errato
120400070904if  1c                   IF        V1Ccgi <> *blanks
120500070912      * - - Controllo
120600070904     c                   clear                   FIDG09ds
120700070904     c                   eval      D09iop0 = '001'
120800070905     c                   eval      D09ifgs = V1Cpocgi
120900070904     c                   eval      D09icgi = V1Ccgi
121000070905     c                   eval      D09idat = Wdate
121100070904     c                   eval      D09itug = 'C'
121200070904     c                   eval      KPJBU   = FIDG09ds
121300070323     c                   call      'FIDG09R'
121400070904     c                   parm                    KPJBA
121500070904     c                   eval      FIDG09ds= KPJBU
121600070913if  2c                   If        D09oerr = *on
121700070323     c                   seton                                        286690
121800070912     c                   eval      V1Dmsg  = $Msg(33)
121900070912     c                   eval      $Video  = '1'
122000070323     c                   leavesr
122100070913x   2c                   Else
122200070913if  3c                   if        D09ofgs = V1Cpocgi
122300070905     c                   eval      V1Dcgi  = D09odes
122400070913x   3c                   else
122500070323     c                   seton                                        286690
122600070912     c                   eval      V1Dmsg  = $Msg(33)
122700070912     c                   eval      $Video  = '1'
122800070905     c                   leavesr
122900070913e   3c                   endif
123000070913e   2c                   EndIf
123100070904e   1c                   ENDIF
123200070913      *
123300070913      * Giorni di chiusura
123400070913sel 1c                   select
123500070913      * - 1° giorno di chiusura errato
123600070913w   1c                   when           %subst(V1Cgc1:1:1) <> *blanks
123700070913     c                             and (%subst(V1Cgc1:1:1) <  '1'
123800070913     c                              or  %subst(V1Cgc1:1:1) >  '7')
123900070913     c                   seton                                        286890
124000070913     c                   eval      V1Dmsg  = %trimr($Msg(34))
124100070913     c                                     + ' (1=lu, 2=ma, 3=me, 4=gi, +
124200070913     c                                          5=ve, 6=sa, 7=do)'
124300070913     c                   eval      $Video  = '1'
124400070913     c                   leavesr
124500070913      * - 1° giorno di chiusura mat/pom errato
124600070913w   1c                   when           %subst(V1Cgc1:2:1) <> ' '
124700070913     c                             and  %subst(V1Cgc1:2:1) <> 'M'
124800070913     c                             and  %subst(V1Cgc1:2:1) <> 'P'
124900070913     c                   seton                                        286890
125000070913     c                   eval      V1Dmsg  = %trimr($Msg(34))
125100070913     c                                     + ' (M=mat, P=pom, +
125200070913     c                                          _=tutto il giorno)'
125300070913     c                   eval      $Video  = '1'
125400070913     c                   leavesr
125500070913      * - 2° giorno di chiusura errato
125600070913w   1c                   when           %subst(V1Cgc2:1:1) <> *blanks
125700070913     c                             and (%subst(V1Cgc2:1:1) <  '1'
125800070913     c                              or  %subst(V1Cgc2:1:1) >  '7')
125900070913     c                   seton                                        286990
126000070913     c                   eval      V1Dmsg  = %trimr($Msg(34))
126100070913     c                                     + ' (1=lu, 2=ma, 3=me, 4=gi, +
126200070913     c                                          5=ve, 6=sa, 7=do)'
126300070913     c                   eval      $Video  = '1'
126400070913     c                   leavesr
126500070913      * - 2° giorno di chiusura mat/pom errato
126600070913w   1c                   when           %subst(V1Cgc2:2:1) <> ' '
126700070913     c                             and  %subst(V1Cgc2:2:1) <> 'M'
126800070913     c                             and  %subst(V1Cgc2:2:1) <> 'P'
126900070913     c                   seton                                        286990
127000070913     c                   eval      V1Dmsg  = %trimr($Msg(34))
127100070913     c                                     + ' (M=mat, P=pom, +
127200070913     c                                          _=tutto il giorno)'
127300070913     c                   eval      $Video  = '1'
127400070913     c                   leavesr
127500070913      * - "coincidenze"... (se entrambi immessi)
127600070914w   1c                   when           %subst(V1Cgc1:1:1) =
127700070913     c                                    %subst(V1Cgc2:1:1)
127800070914     c                             and  %subst(V1Cgc1:1:1) <> *blanks
127900070913     c                             and (V1Cgc1             <> *blanks
128000070913     c                             and  V1Cgc2             <> *blanks)
128100070913     c                   seton                                        286990
128200070913     c                   eval      V1Dmsg  = %trimr($Msg(34))
128300070913     c                                     + ': coincide con il primo +
128400070914     c                                        specificato'
128500070913     c                   eval      $Video  = '1'
128600070913     c                   leavesr
128700070914      * - troppo "globali" (se entrambi immessi)
128800070914w   1c                   when           %subst(V1Cgc1:1:1) =
128900070914     c                                    %subst(V1Cgc2:1:1)
129000070914     c                             and  %subst(V1Cgc1:1:1) =  *blanks
129100070914     c                             and (V1Cgc1             <> *blanks
129200070914     c                             and  V1Cgc2             <> *blanks)
129300070914     c                   seton                                        286990
129400070914     c                   eval      V1Dmsg  = %trimr($Msg(34))
129500070914     c                                     + ': "generale" come il +
129600070914     c                                        primo specificato'
129700070914     c                   eval      $Video  = '1'
129800070914     c                   leavesr
129900070913e   1c                   endsl
130000071113      *
130100071113      * Se tipo destinatario "V": obbligatorio DDT o LNA/ZNC
130200071113      *                           o giro consegna o gg. di chiusura
130300090710      *                           o tipo incasso c/assegni forzato
130400071113if  1c                   if             V1Cdos   = 'V'
130500071113     c                             and  V1Cddt   = *blanks
130600071113     c                             and (V1Clna   = *blanks
130700071113     c                              or  V1Clna   = *zeros)
130800071113     c                             and (V1Cpocgi = *zeros
130900071113     c                              or  V1Ccgi   = *blanks)
131000071113     c                             and  V1Cgc1   = *blanks
131100071113     c                             and  V1Cgc2   = *blanks
131200090710     c                             and  V1Cticf  = *blanks
131300071113     c                   seton                                        28  90
131400071113     c                   seton                                        616263
131500071113     c                   seton                                        6667
131600090710     c                   seton                                        686970
131700071113     c                   eval      V1Dmsg  = $Msg(26)
131800071113     c                   eval      $Video  = '1'
131900071113     c                   leavesr
132000071113e   1c                   endif
132100090707      /free
132200090707         // Tipo incasso c/assegni forzato
132300090707         select;
132400090707           // - *blank NON forzabile
132500090707           when  V1Cticf =  *blank;
132600090710             clear  V1Dticf;
132700090710             //V1Dticf = '(?)';
132800090707           // - Ricerca
132900090707           when  %scan('?' : V1Cticf) > *zero;
133000090713             clear  V1Dticf;
133100090707             clear  ds1A;
133200090713             clear  trtb13ds;
133300090713             TB13dos = 'S';
133400090713             kpjbu = TRTB13ds;
133500090713             TRTB13R ( kpjba );
133600090713             TRTB13ds = kpjbu;
133700090713             if  TB13Ocod = *blank;
133800090713               clear  V1Cticf;
133900090713             else;
134000090713               ds1A    = TB13Ouni;
134100090713               V1Cticf = TB13Ocod;
134200090713               V1Dticf = §1Ades;
134300090713             endif;
134400090713             *in90   = *on;
134500090713             *in70   = *on;
134600090707             leavesr;
134700090707           // - Controllo
134800090707           other;
134900090707             TBLcod  = '1A';
135000090707             TBLkey  = V1Cticf;
135100090707             chain  K03TABEL  TABEL;
135200090707             if  %found(TABEL00F)   and   TBLflg = *blank;
135300090707               ds1A = TBLuni;
135400090707               V1Dticf = §1Ades;
135500090707             else;
135600090707               *in28   = *on;
135700090707               *in90   = *on;
135800090707               *in70   = *on;
135900090707               V1Dmsg  = $Msg(36);
136000090707               $Video  = '1';
136100090707               leavesr;
136200090707             endif;
136300090707         endsl;
136400090707      /end-free
136500060103      *
136600060104      * Controllo (in)esistenza record:
136700060104sel 1c                   SELECT
136800060104      *
136900060103      * SOLO se inserimento: controllo tramite TISIT5R =>
137000060103      *   inserimento possibile SOLO se restituito "errore"
137100060104w   1c                   WHEN           IT0op1  = 'F06'
137200060103     c                             or   IT0op1  = 'P06'
137300060103     c                             or   IT0op1  = 'O03'
137400060103     c                             or   IT0op1  = 'O07'
137500060104     c                   clear                   WrkT0ds
137600060125     c                   eval      W_IT0rag = V1Crag
137700060125     c                   eval      W_IT0naz = V1Cnaz
137800060125     c                   eval      W_IT0prv = V1Cprv
137900060125     c                   eval      W_IT0cap = V1Ccap
138000060125     c                   eval      W_IT0loc = V1Cloc
138100060125     c                   eval      W_IT0ind = V1Cind
138200060103     c                   call      'TISIT5R'
138300060103     c                   parm                    WrkT0ds
138400060125if  2c                   if        W_OT0err = *blanks
138500060103     c                   seton                                        28  90
138600060103     c                   eval      V1Dmsg  = %trim($Msg(18))
138700060103     c                                     + '(Codice: '
138800060125     c                                     + %editw(W_OT0cod:'0       ')
138900060103     c                                     + ')'
139000070912     c                   eval      $Video  = '1'
139100060103     c                   leavesr
139200060103e   2c                   endif
139300060103      *
139400060103      * SOLO se modifica: controllo direttamente chiavi duplicate
139500060104w   1c                   WHEN      IT0op1  = 'O02'
139600060103     c     K07DST03V     chain(n)  AZDST000
139700060103if  2c                   if             %found(AZDST03L)
139800060104     c                             and  V1Ccod <> DSTcod
139900060103     c                   seton                                        28  90
140000060103     c                   eval      V1Dmsg  = %trim($Msg(18))
140100060103     c                                     + '(Codice: '
140200060103     c                                     + %editw(DSTcod:'0       ')
140300060103     c                                     + ')'
140400070912     c                   eval      $Video  = '1'
140500060103     c                   leavesr
140600060103e   2c                   endif
140700060103      *
140800060104      * SOLO se inserimento di Proposta Appuntamento: controllo
140900060104      *   se chiavi duplicate
141000060104w   1c                   WHEN           DEPpro  = 'A'
141100060104     c                             and  IT0op1  = 'PF6'
141200060110     c     K08DSTP1V     setll     AZDSTP00
141300060110if  2c                   if        %equal(AZDSTP1L)
141400060104     c                   seton                                        28  90
141500060104     c                   eval      V1Dmsg  = $Msg(8)
141600070912     c                   eval      $Video  = '1'
141700060104     c                   leavesr
141800060104e   2c                   endif
141900060104      *
142000060104      * SOLO se modifica di Proposta Appuntamento: controllo
142100060104      *   se rec. inesistente
142200060104w   1c                   WHEN           DEPpro  = 'A'
142300060104     c                             and  IT0op1  = 'P02'
142400060110     c     K08DSTP1P     setll     AZDSTP00
142500060110if  2c                   if        NOT %equal(AZDSTP1L)
142600060104     c                   seton                                        28  90
142700060104     c                   eval      V1Dmsg  = $Msg(6)
142800070912     c                   eval      $Video  = '1'
142900060104     c                   leavesr
143000060104e   2c                   endif
143100060104      *
143200060104e   1c                   ENDSL
143300060103      *
143400060125      *
143500060125      * Riaggancio record "perduti" in fase di controllo per l'aggiornamento
143600060125     c                   if        *inKF
143700060125     c                   exsr      Rep_AZDST
143800060125     c*** questo no:     exsr      Rep_AZDSE
143900060125     c                   if        *in90
144000070912     c                   eval      $Video  = '1'
144100060125     c                   leavesr
144200060125     c                   endif
144300060125     c                   endif
144400060125      *
144500060125      * Reperimento numeratore (se inserimento)
144600060125      * (aggiornato solo se premuto F6)
144700060125     c                   exsr      CtrRepNum
144800060103      *
144900060103     c                   ENDSR
145000090707      /free
145100090707       //--------------------------------------------------------------
145200090707       //?Controlli da eseguire in fase di cancellazione               ?
145300090707       //--------------------------------------------------------------
145400090707       BEGSR  sr_CtrlDELETE;
145500090707
145600090707         // - Impedimento della cancellazione se:
145700090707         //   · "tipo incasso c/assegni forzato" è valorizzato   &
145800090707         //   · l'utente non possiede l'apposita autorizzazione
145900090707         if  IT0op1  =  'O04'    and
146000090707             V1Cticf <> *blank   and
146100090707             §UTEdst <> '9';
146200090707           *in28   = *on;
146300090707           *in90   = *on;
146400090707           *in70   = *on;
146500090707           V1Dmsg  = $Msg(37);
146600090707           $Video  = '1';
146700090707           leavesr;
146800090707         endif;
146900090707
147000090707       ENDSR;
147100090707      /end-free
147200060125      *
147300060125      *---------------------------------------------------------------*
147400060125      * Controllo / Reperimento nuovo progressivo da numeratore       *
147500060125      *---------------------------------------------------------------*
147600060125     c     CtrRepNum     BEGSR
147700060125      *
147800060125      * Reperimento numeratore (se inserimento)
147900060125      * (aggiornato solo se premuto F6)
148000060125if  1c                   if             IT0op1  = 'F06'
148100060125     c                             or   IT0op1  = 'P06'
148200060125     c                             or   IT0op1  = 'O03'
148300060125     c                   clear                   TRUL33DS
148400060125     c                   eval      I33tla  = 'L'
148500060125if  2c                   if        *inKF   = *on
148600060125     c                   eval      I33ope  = *zeros
148700060125x   2c                   else
148800060125     c                   eval      I33ope  = 1
148900060125e   2c                   endif
149000060125     c                   eval      I33cnu  = 030
149100060125     c                   eval      I33num  = 1
149200060125     c                   movel(p)  TRUL33ds      KPJBU
149300060125     c                   call      'TRUL33R'
149400060125     c                   parm                    KPJBA
149500060125     c                   movel     KPJBU         TRUL33ds
149600060125if  2c                   if        O33err <> *zeros
149700060125     c                   seton                                        28  90
149800060125     c                   movel     O33msg        V1Dmsg
149900060125     c                   leavesr
150000060125e   2c                   endif
150100060125e   1c                   endif
150200060125      *
150300060125     c                   ENDSR
150400060103      *
150500060103      *---------------------------------------------------------------*
150600060103      * Gestione videata D02                                          *
150700060103      *---------------------------------------------------------------*
150800060103     c     GesD02        BEGSR
150900060103      *
151000060103      * Inizializzo la videata
151100060103if  1c                   if        $InzD02 = *on
151200060103     c                   exsr      InzD02
151300060103     c                   movel     *off          $InzD02
151400060130x   1c                   else
151500060130      * Campi "chiave" (ripotati dalla 1ª videata)
151600060130     c                   eval      V2Ccod  = V1Ccod
151700060130     c                   eval      V2Drag  = V1Crag
151800060130     c                   eval      V2Cdos  = V1Cdos
151900060130     c                   eval      V2Ddos  = V1Ddos
152000060103e   1c                   endif
152100060103      *
152200060103      * Scrivo la testata e la riga tasti funzionali abilitati
152300060126if  1c                   if        $Video <> $VideoP
152400060103     c                   write     SIT2T01
152500060104     c                   if            IT0op1  = 'O05'
152600060104     c                             or  IT0op1  = 'P05'
152700060104     c                             or  IT0op1  = 'P15'
152800060125      * - tasti funzionali in visualizzazione
152900060103     c                   write     SIT2P01
153000060104     c                   else
153100060125      * - tasti funzionali in manutenzione
153200060104     c                   write     SIT2P02
153300060104     c                   endif
153400060126e   1c                   endif
153500060126      *
153600060126if  1c                   if        *in90
153700060126     c                   eval      $in28   = (*in28 = *on)
153800060126     c                   eval      *in28   = *off
153900060126     c                   movea     *in(40)       Win4099
154000060126     c                   movea     *zeros        *in(51)
154100060126     c                   write     SIT2D02
154200060126     c                   movea     Win4099       *in(40)
154300060126     c                   eval      *in28   = ($in28 = *on)
154400060126     c                   endif
154500060126     c                   eval      $VideoP = $Video
154600060126      *
154700060103      * Emetto la videata
154800060125      * (solo in visualizzazione se priorità dell'utente < "3")
154900060125if  1c                   if        §UTEdst < '3'
155000060125     c                   write     SIT2D02
155100060125     c                   exfmt     PROTECT
155200060125x   1c                   else
155300060125     c                   exfmt     SIT2D02
155400060125e   1c                   endif
155500060103     c                   setoff                                       28  90
155600060103     c                   clear                   V1Dmsg
155700060103      *
155800060103sel 1c                   select
155900060103      * F3=Fine
156000060103w   1c                   when      *inKC
156100060103     c                   exsr      F03D01
156200060103     c                   leavesr
156300060103      * F12=Ritorno
156400060103w   1c                   when      *inKL
156500060103     c                   exsr      F12D02
156600060103     c                   leavesr
156700060104      * RollDown=Videata precedente
156800060104w   1c                   when      £Tasto  = RollDwn
156900060104     c                   exsr      F12D02
157000060104     c                   leavesr
157100060103      *
157200060103e   1c                   endsl
157300060103      *
157400060103      * Invio o F6
157500060103sel 1c                   select
157600090707     *** * Opzione "Annullamento" o "Ripristino"
157700090707w   1***c                   when          IT0op1  = 'O04'
157800090707     ***c                             or  IT0op1  = 'O07'
157900090707      * Opzione "Annullamento" => "Cancellazione
158000090707w   1c                   when      IT0op1  = 'O04'
158100090707     c                   exsr      sr_CtrlDelete
158200090707     c                   if        *in90
158300090707     c                   leavesr
158400090707     c                   endif
158500060103      *
158600060103      * Opzione "Visualizzazione"
158700060103w   1c                   when          IT0op1  = 'O05'
158800060103     c                             or  IT0op1  = 'P05'
158900060103     c                             or  IT0op1  = 'P15'
159000060104     c                   eval      $Fine   = *on
159100060110     c                   leavesr
159200060103      *
159300060103      * Altrimenti
159400060103      * => si controllano i dati di questa videata
159500060103x   1c                   other
159600060103     c                   exsr      CtrD02
159700060103if  2c                   if        *in90
159800060103     c                   leavesr
159900060103e   2c                   endif
160000060104      * => si controllano i dati delle videate precedenti
160100060104     c                   exsr      CtrD01
160200070904if  2c                   if        *in90
160300070912     c***                eval      $Video  = '1'
160400060104     c                   leavesr
160500070904e   2c                   endif
160600060103      *
160700060103e   1c                   endsl
160800060103      *
160900060103      * F6=Conferma
161000060103     c                   if        *inKF
161100060104     c                   exsr      F06D02
161200060104     c                   if        *in90   = *off
161300060104     c                   eval      $Fine   = *on
161400060104     c                   endif
161500060103     c                   endif
161600060103      *
161700060103     c                   ENDSR
161800060103      *
161900060103      *---------------------------------------------------------------*
162000060103      * Inizializzazione videata D02                                  *
162100060103      *---------------------------------------------------------------*
162200060103     c     InzD02        BEGSR
162300060103      *
162400060110     c*** già fatto:     clear                   SIT2D02
162500060104      *
162600060104      * Settaggio indicatori
162700060110     c*** già fatto:     exsr      SettInd
162800060125      *
162900060125      * Indicazione di videata successiva ("Segue...")
163000060125      *           o di ultima videata     ("Fine")
163100060125     c                   eval      V2Dseg  = 'Fine    '
163200060125      *
163300060125      * Impostazione dei dati in D02
163400060125     c                   exsr      RieD02
163500060104      *
163600060104      * Decodifiche
163700060104     c                   exsr      CtrD02
163800060104     c                   setoff                                       28  90
163900060104     c                   movea     *zeros        *in(51)
164000060103      *
164100060103     c                   ENDSR
164200060125      *
164300060125      *---------------------------------------------------------------*
164400060125      * Impostazione dati nella videata D02                           *
164500060125      *---------------------------------------------------------------*
164600060125     c     RieD02        BEGSR
164700060125      *
164800060125      * Campi "chiave" (ripotati dalla 1ª videata)
164900060125     c                   eval      V2Ccod  = V1Ccod
165000060125     c                   eval      V2Drag  = V1Crag
165100060125     c                   eval      V2Cdos  = V1Cdos
165200060125     c                   eval      V2Ddos  = V1Ddos
165300060125      *
165400060125sel 1c                   SELECT
165500060125      *
165600070912      * Da proposte appuntamenti
165700060125w   1c                   WHEN           DEPpro = 'A'
165800060125     c                             or   IT0op1 = 'P05'
165900060125     c                   movel     A_DSTute      V2Cute
166000060125     c                   movel     C_POutente    V2Kpou
166100060125     c                   z-add     A_DSTpou      V2Cpou
166200060125     c                   z-add     A_DSTdiv      V2Cdiv
166300060125     c                   movel     A_DSTflo      dDSTflo
166400060125      *
166500070912      * Da destinatari
166600060125x   1c                   OTHER
166700070912      * - Giorni stabiliti di consegna
166800060125     c                   eval      V2Cgsc  = SavDSE_GSC
166900070912      * - Orari di scarico
167000060125     c                   eval      V2Cosc  = SavDSE_OSC
167100070912      * - Servizi accessori in consegna
167200060125     c                   eval      V2Csac  = SavDSE_SAC
167300060125      * - Modalità di appuntamento
167400060125     c                   eval      V2Capp  = §DSEapp
167500060125      * - Tipo Comunicazione
167600060125     c                   eval      V2Ccom  = §DSEcom
167700060125      * - Tipo Modulo Comunicazione
167800060125     c                   eval      V2Cmod  = §DSEmod
167900060125      * - Richiesta copia DDT
168000060125     c                   eval      V2Cddt  = §DSEddt
168100070912      * - Se NON proposta: utente e data inserimento
168200060125     c                   if        %subst(IT0op1:1:1) <> 'P'
168300060125     c                   movel     DSTute        V2Cute
168400060125     c                   z-add     DSTdiv        V2Cdiv
168500060125     c                   endif
168600060125      *
168700060125e   1c                   ENDSL
168800060125      *
168900060125     c                   ENDSR
169000060103      *
169100060103      *---------------------------------------------------------------*
169200060103      * Gestione tasto funzionale F12 da videata D02                  *
169300060103      *---------------------------------------------------------------*
169400060103     c     F12D02        BEGSR
169500060103      *
169600060103      * Ritorno alla videata D01
169700060103     c                   eval      $Video  = '1'
169800060104     c                   movea     *zeros        *in(51)
169900060103      *
170000060103     c                   ENDSR
170100060103      *
170200060103      *---------------------------------------------------------------*
170300060103      * Controllo dati immessi in videata D02                         *
170400060103      *---------------------------------------------------------------*
170500060103     c     CtrD02        BEGSR
170600060103      *
170700060103     c                   movea     *zeros        *in(51)
170800060125      *
170900060125      * Modalità di appuntamento
171000060125sel 1c                   select
171100060126      * - Valore errato
171200060130w   1c                   when          V2Capp <> *blanks
171300060130     c                             and V2Capp <> 'N'
171400060126     c                             and V2Capp <> 'T'
171500060126     c                             and V2Capp <> 'F'
171600060126     c                             and V2Capp <> 'M'
171700060126     c                   seton                                        71  90
171800060126     c                   movel(p)  C1_CPF5223    P1Xmsg
171900070912     c                   eval      $Video  = '2'
172000060126     c                   leavesr
172100060130      * - (non immesso)
172200060130w   1c                   when      V2Capp  = *blanks
172300060125      * - No appuntamento
172400060125w   1c                   when      V2Capp  = 'N'
172500060125      * - Telefono
172600060125w   1c                   when          V2Capp  = 'T'
172700060125     c                             and V1Ctel  = *blanks
172800060125     c                   seton                                        285990
172900060125     c                   movel     $Msg(27)      V1Dmsg
173000060125     c                   eval      $Video  = '1'
173100060125     c                   leavesr
173200060125      * - Fax
173300060125w   1c                   when          V2Capp  = 'F'
173400060125     c                             and V1Cfax  = *blanks
173500060125     c                   seton                                        286490
173600060125     c                   movel     $Msg(28)      V1Dmsg
173700060125     c                   eval      $Video  = '1'
173800060125     c                   leavesr
173900060125      * - Indirizzo @-mail
174000060125w   1c                   when          V2Capp  = 'M'
174100060125     c                             and V1Ciem  = *blanks
174200060125     c                   seton                                        286590
174300060125     c                   movel     $Msg(29)      V1Dmsg
174400060125     c                   eval      $Video  = '1'
174500060125     c                   leavesr
174600060125      *
174700060125e   1c                   endsl
174800060126      *
174900060126      *
175000060126sel 1c                   select
175100060126      * Tipo Comunicazione
175200060126      * - Valore errato
175300060130w   1c                   when          V2Ccom <> *blanks
175400060130     c                             and V2Ccom <> 'S'
175500060126     c                             and V2Ccom <> 'R'
175600060126     c                             and V2Ccom <> 'X'
175700060126     c                   seton                                        72  90
175800060126     c                   movel(p)  C2_CPF5223    P1Xmsg
175900070912     c                   eval      $Video  = '2'
176000060126     c                   leavesr
176100060126      *
176200060126      * Tipo Modulo Comunicazione
176300060126      * - Valore errato
176400060130w   1c                   when          V2Cmod <> *blanks
176500060130     c                             and V2Cmod <> 'B'
176600060126     c                             and V2Cmod <> 'C'
176700060126     c                   seton                                        73  90
176800060126     c                   movel(p)  C3_CPF5223    P1Xmsg
176900070912     c                   eval      $Video  = '2'
177000060126     c                   leavesr
177100060126      *
177200060126      * Richiesta Copia DDT
177300060126      * - Valore errato
177400060130w   1c                   when          V2Cddt <> *blanks
177500060130     c                             and V2Cddt <> 'S'
177600060131     c*** *blank = N               and V2Cddt <> 'N'
177700060126     c                   seton                                        74  90
177800060126     c                   movel(p)  C4_CPF5223    P1Xmsg
177900070912     c                   eval      $Video  = '2'
178000060126     c                   leavesr
178100060126      *
178200060126e   1c                   endsl
178300060103      *
178400060103     c                   ENDSR
178500060103      *
178600060103      *---------------------------------------------------------------*
178700060103      * Reperimento record                                            *
178800060103      *---------------------------------------------------------------*
178900060103     c     Rep_AZDST     BEGSR
179000060103      *
179100060125     c                   clear                   xx
179200060125     c                   clear                   DSTcod
179300060125      *
179400060103     c                   select
179500060125      *
179600060125      * Se inserimento di Proposta Appuntamento: controllo
179700060125      * -> non reperisco nulla (controllo già fatto)
179800060125     c                   when           DEPpro  = 'A'
179900060125     c                             and  IT0op1  = 'PF6'
180000060125     c***  K08DSTP1V     chain     AZDSTP00
180100060103      *
180200060125      * Se richiesta sola visualizzazione (di qualunque tipo)
180300060125      *    o modifica di una Proposta di tipo Appuntamento
180400060103      * -> reperisco dati da chiave di accesso passata tra i parametri
180500060103     c                   when           IT0op1  = 'P05'
180600060103     c                             or  (IT0op1  = 'P02'
180700060103     c                              and DEPpro  = 'A')
180800060103     c     K08DSTP1P     chain     AZDSTP00
180900060103      *
181000060125      * Se richieste operazioni sul file destinatari
181100060103      * -> reperisco dati dall'anagrafica destinatari
181200060103     c                   when           DEPpro <> 'A'
181300060103     c                             and  IT0op1  = 'F06'
181400060103     c     K07DST03V     chain     AZDST000
181500060103     c                   if        %found(AZDST03L)                             *ESISTENTE
181600060125     c*** non qui:       seton                                        28  90
181700060125     c*** non qui:       movel     $Msg(7)       V1Dmsg
181800060125     c                   eval      xx      = 7
181900060103     c                   endif
182000060103      *
182100060103     c                   when           DEPpro <> 'A'
182200060103     c                             and (IT0op1  = 'O05'
182300060103     c                              or  IT0op1  = 'P05'
182400060103     c                              or  IT0op1  = 'P15')
182500060103     c     IT0cod        chain     AZDST004
182600060103      *
182700060103      * Altrimenti
182800060103      * -> reperisco dati dalla proposta RICHIESTA
182900060103     c                   other
183000060103     c     K07DST03P     chain     AZDST000
183100060103      *
183200060103     c                   endsl
183300060103      *
183400060103     c                   ENDSR
183500060125      *
183600060125      *---------------------------------------------------------------*
183700060125      * Reperimento record dei dati aggiuntivi per destinatari part.  *
183800060125      *---------------------------------------------------------------*
183900060125     c     Rep_AZDSE     BEGSR
184000060125      *
184100070912     c                   clear                   dDSEflo
184200070912     c                   clear                   dDSExt1
184300060125     c                   clear                   SavDSE_GSC
184400060125     c                   clear                   SavDSE_OSC
184500060125     c                   clear                   SavDSE_SAC
184600060125     c                   clear                   SavDSE_IEM
184700060125      *
184800060125      * Reperimento dati aggiuntivi dei destinatari particolari
184900060125      *   (NON se proposta appuntamento)
185000060125if  1c***                if             DEPpro <> 'A'
185100060125     c***                          or   IT0op1  = 'P05'
185200060125if  1c                   if        DSTcod <> *zeros
185300060125      *
185400060125     c     DSTcod        setll     AZDSE000
185500060125     c     DSTcod        reade     AZDSE000
185600060125do  2c                   DOW       NOT %eof(AZDSE01L)
185700060125      *
185800060125sel 3c                   select
185900060125      * - Flags Operativi (e Numero di Fax)
186000060125w   3c                   when      DSEtrk  = 'FLO'
186100060125     c                   movel     DSEuni        dDSEflo
186200070905      * - Altri flags operativi ("XT1")
186300070905w   3c                   when      DSEtrk  = 'XT1'
186400070905     c                   movel     DSEuni        dDSExt1
186500060125      * - Giorni Stabiliti di Consegna
186600060125w   3c                   when      DSEtrk  = 'GSC'
186700060125     c                   movel     DSEuni        SavDSE_GSC
186800060125      * - Orari di Scarico
186900060125w   3c                   when      DSEtrk  = 'OSC'
187000060125     c                   movel     DSEuni        SavDSE_OSC
187100060125      * - Servizi Accessori in Consegna
187200060125w   3c                   when      DSEtrk  = 'SAC'
187300060125     c                   movel     DSEuni        SavDSE_SAC
187400060125      * - Indirizzo @-Mail
187500060125w   3c                   when      DSEtrk  = 'IEM'
187600060125     c                   movel     DSEuni        SavDSE_IEM
187700060125e   3c                   endsl
187800060125      *
187900060125     c     DSTcod        reade     AZDSE000
188000060125e   2c                   ENDDO
188100060125      *
188200060125e   1c                   endif
188300060125      *
188400060125     c                   ENDSR
188500060103      *
188600060103      *---------------------------------------------------------------*
188700060103      * Aggiornamento record                                          *
188800060103      *---------------------------------------------------------------*
188900060104     c     F06D02        BEGSR
189000060103      *
189100060103      * Si lancia la subroutine di conferma interessata
189200060103      *   in base alla provenienza
189300060103sel 1c                   select
189400060103w   1c                   when           DEPpro  = 'A'
189500060103     c                             and (IT0op1  = 'PF6'
189600060103     c                              or  IT0op1  = 'P02')
189700060104     c                   exsr      WrtUpd_AZDSTp
189800060103w   1c                   when           DEPpro <> 'A'
189900060103     c                             and  IT0op1  = 'F06'
190000060104     c                   exsr      Wrt_AZDST
190100060103w   1c                   when           DEPpro <> 'A'
190200060103     c                             and  IT0op1  = 'O02'
190300060104     c                   exsr      Upd_AZDST
190400060103w   1c                   when           DEPpro <> 'A'
190500060103     c                             and  IT0op1  = 'O03'
190600060104     c                   exsr      Cpy_AZDST
190700060103w   1c                   when           DEPpro <> 'A'
190800060103     c                             and  IT0op1  = 'O04'
190900060104     c                   exsr      Dlt_AZDST
191000060103w   1c                   when           DEPpro <> 'A'
191100060103     c                             and  IT0op1  = 'O07'
191200060104     c                   exsr      Rip_AZDST
191300060103w   1c                   when           DEPpro <> 'A'
191400060103     c                             and  IT0op1  = 'P06'
191500060104     c                   exsr      Wrt_AZDST
191600070913     c                   eval      $Fine     = *on
191700070913     c                   eval      wForzaLoc = *off
191800070913     c                   eval      wF17      = *off
191900070913     c                   eval      *in17     = *off
192000060103e   1c                   endsl
192100060103      *
192200060103     c                   ENDSR
192300060104      *
192400060104      *---------------------------------------------------------------*
192500060104      * INSERIMENTO/MODIFICA RECORD "APPUNTAMENTO" IN FILE PROPOSTE   *
192600060104      *---------------------------------------------------------------*
192700060104     c     WrtUpd_AZDSTp BEGSR
192800060104      *
192900060104     c                   clear                   AZDSTP00
193000060104      *
193100060125     c                   eval      A_DSTtip = 'P'
193200060125     c                   eval      A_DSTnaz = V1Cnaz
193300060125     c                   eval      A_DSTprv = V1Cprv
193400060125     c                   eval      A_DSTcap = V1Ccap
193500060125     c                   eval      A_DSTloc = V1Cloc
193600060125     c                   eval      A_DSTind = V1Cind
193700060125     c                   eval      A_DSTrag = V1Crag
193800060125     c                   eval      A_DSTtel = V1Ctel
193900060125     c                   eval      A_DSTref = V1Cref
194000060125     c                   eval      A_DSTdos = V1Cdos
194100060125     c                   eval      A_DSTnum = 1                                 *RICORRENZA PROP.
194200060104     c***                clear                   DSTdai                         *DATA INIZIALE
194300060104     c***                clear                   DSTdaf                         *DATA FINALE
194400060125     c                   eval      A_DSTute = KNMUS                             *UTENTE INS/VAR
194500060125     c                   eval      A_DSTpou = DUTpou                            *PO UTENTE INS/VAR
194600060125     c                   eval      A_DSTdiv = Wdate                             *DATA INS/VAR
194700060125     c                   eval      A_DSTlna = DUTpou                            *PO UTENTE => IN LNA
194800060125     c***                clear                   A_DSTaas
194900060125     c***                clear                   A_DSTlnp
195000060125     c***                clear                   A_DSTnrs
195100060125     c***                clear                   A_DSTnsp
195200060130     c***                clear                   A_DSTflo
195300060125     c***                clear                   A_DSTatb
195400060104      *
195500060110     c                   select
195600060110      *
195700060110     c                   when      IT0op1   = 'PF6'
195800060104     c                   WRITE     AZDSTP00
195900060104     c                   movel     $Msg(2)       OT0msg                         *MESSAGGIO
196000060104      *
196100060110     c                   when      IT0op1   = 'P02'
196200060104     c                   UPDATE    AZDSTP00
196300060104     c                   movel     $Msg(3)       OT0msg                         *MESSAGGIO
196400060110      *
196500060110     c                   endsl
196600060104      *
196700060104      * Esce dal pgm
196800060104     c                   eval      $Fine    = *on
196900060104      *
197000060104     c                   ENDSR
197100060104      *
197200060104      *---------------------------------------------------------------*
197300060104      * INSERIMENTO RECORD                                            *
197400060104      *---------------------------------------------------------------*
197500060104     c     Wrt_AZDST     BEGSR
197600060104      *
197700060104     c                   clear                   AZDST000
197800060104      *
197900060104      * Impostazione dei campi nel record da scrivere
198000060104     c                   eval      DSTcod  = O33nrf
198100060104     c                   eval      DSTtip  = 'C'
198200060104     c                   exsr      sr_RieFld
198300060104      *
198400060104     c                   WRITE     AZDST000
198500060125      *
198600060125      * "Allineamento" file AZDSE00F
198700060125     c                   exsr      All_AZDSE
198800060104      *
198900060104      * Se proveniente da proposte in automatico
199000060104      * => Cancella la proposta ed esce
199100060104if  1c                   IF        IT0op1  = 'P06'
199200060104      *
199300060104     c     K08DSTP1P     chain     AZDSTP00
199400060104     c                   if        %found(AZDSTP1L)
199500060104     c                   DELETE    AZDSTP00
199600060104     c                   endif
199700060104      *
199800060104     c                   movel     $Msg(2)       OT0msg                         *MESSAGGIO
199900060104     c                   eval      $Fine   = *on
200000060104      *
200100060104      * Altrimenti
200200060104      * => Pulisce le videate per un altro inserimento
200300060104x   1c                   ELSE
200400060104      *
200500060104     c                   eval      $InzD01 = *on
200600060104     c                   eval      $InzD02 = *on
200700060104      *
200800060104e   1c                   ENDIF
200900060104      *
201000060104     c                   ENDSR
201100060125      *
201200060125      *---------------------------------------------------------------*
201300060125      * ALLINEAMENTO DATI IN AZDSE00F                                 *
201400060125      *---------------------------------------------------------------*
201500060125     c     All_AZDSE     BEGSR
201600060125      *
201700060126      * Per ognuno dei trk:
201800060126      * 1) verifico esistenza record in oggetto
201900060126      * 2) aggiorno record in esame:
202000060126      *    a) cancello  se reperito record ma non inseriti dati a video
202100060126      *    b) modifico  se reperito record e modificati dati a video
202200060126      *    c) inserisco se non reperito record ed inseriti dati a video
202300060126      * Non faccio nulla se NON reperito record di dati NON inseriti a
202400060126      * video.
202500060125      *
202600060125      * - Numero Fax /
202700060125      *   Modalità di appuntamento /
202800060125      *   Tipo Comunicazione /
202900060125      *   Tipo Modulo Comunicazione /
203000060125      *   Copia DDT
203100060125     c                   eval      DSEtrk  = 'FLO'
203200060126     c     K02DSE01      chain     AZDSE000
203300060126     c                   movel     DSEuni        dDSEflo
203400060126sel 1c                   select
203500060126w   1c                   when           %found(AZDSE01L)
203600060126     c                             and  V1Cfax  = *blanks
203700060126     c                             and  V2Capp  = *blanks
203800060126     c                             and  V2Ccom  = *blanks
203900060126     c                             and  V2Cmod  = *blanks
204000060126     c                             and  V2Cddt  = *blanks
204100060126     c                   DELETE    AZDSE000
204200060126w   1c                   when           %found(AZDSE01L)
204300060126     c                             and (V1Cfax <> §DSEfax
204400060126     c                              or  V2Capp <> §DSEapp
204500060126     c                              or  V2Ccom <> §DSEcom
204600060126     c                              or  V2Cmod <> §DSEmod
204700060126     c                              or  V2Cddt <> §DSEddt)
204800060126     c                   exsr      Rie_AZDSE
204900060126     c                   UPDATE    AZDSE000
205000060126w   1c                   when      NOT  %found(AZDSE01L)
205100060126     c                             and (V1Cfax <> *blanks
205200060126     c                              or  V2Capp <> *blanks
205300060126     c                              or  V2Ccom <> *blanks
205400060126     c                              or  V2Cmod <> *blanks
205500060126     c                              or  V2Cddt <> *blanks)
205600060126     c                   exsr      Rie_AZDSE
205700060126     c                   WRITE     AZDSE000
205800060126x   1c                   other
205900060126     c                   UNLOCK    AZDSE01L
206000060126e   1c                   endsl
206100070905      *
206200070905      * - Filiale Gestione Giro Consegna /
206300070905      *   Codice Giro Tellus /
206400070905      *   1° Giorno Chiusura Destinatario /
206500090707      *   2° Giorno Chiusura Destinatario /
206600090707      *   Tipo incasso c/assegni FORZATO
206700070905     c                   eval      DSEtrk  = 'XT1'
206800070905     c     K02DSE01      chain     AZDSE000
206900070905     c                   movel     DSEuni        dDSExt1
207000070905sel 1c                   select
207100070905w   1c                   when           %found(AZDSE01L)
207200070905     c                             and  V1Cpocgi  = *zeros
207300070905     c                             and  V1Ccgi    = *blanks
207400070913     c                             and  V1Cgc1    = *blanks
207500070913     c                             and  V1Cgc2    = *blanks
207600090707     c                             and  V1Cticf   = *blanks
207700070905     c                   DELETE    AZDSE000
207800070905w   1c                   when           %found(AZDSE01L)
207900070905     c                             and (V1Cpocgi <> §DSEXT1pog
208000070912     c                              or  V1Ccgi   <> §DSEXT1cgi
208100070913     c                              or  V1Cgc1   <> §DSEXT1gc1
208200090707     c                              or  V1Cgc2   <> §DSEXT1gc2
208300090707     c                              or  V1Cticf  <> §DSEXT1tic)
208400070905     c                   exsr      Rie_AZDSE
208500070905     c                   UPDATE    AZDSE000
208600070905w   1c                   when      NOT  %found(AZDSE01L)
208700070905     c                             and (V1Cpocgi <> *zeros
208800070912     c                              or  V1Ccgi   <> *blanks
208900070913     c                              or  V1Cgc1   <> *blanks
209000090707     c                              or  V1Cgc2   <> *blanks
209100090707     c                              or  V1Cticf  <> *blanks)
209200070905     c                   exsr      Rie_AZDSE
209300070905     c                   WRITE     AZDSE000
209400070905x   1c                   other
209500070905     c                   UNLOCK    AZDSE01L
209600070905e   1c                   endsl
209700060125      *
209800060125      * - Indirizzo @-Mail
209900060126     c                   eval      DSEtrk  = 'IEM'
210000060126     c     K02DSE01      chain     AZDSE000
210100060126sel 1c                   select
210200060126w   1c                   when           %found(AZDSE01L)
210300060126     c                             and  V1Ciem  = *blanks
210400060126     c                   DELETE    AZDSE000
210500060126w   1c                   when           %found(AZDSE01L)
210600060126     c                             and  V1Ciem <> DSEuni
210700060126     c                   exsr      Rie_AZDSE
210800060126     c                   UPDATE    AZDSE000
210900060126w   1c                   when      NOT  %found(AZDSE01L)
211000060126     c                             and  V1Ciem <> *blanks
211100060126     c                   exsr      Rie_AZDSE
211200060126     c                   WRITE     AZDSE000
211300060126x   1c                   other
211400060126     c                   UNLOCK    AZDSE01L
211500060126e   1c                   endsl
211600060125      *
211700060125      * - Giorni Stabiliti di Consegna
211800060125     c                   eval      DSEtrk  = 'GSC'
211900060126     c     K02DSE01      chain     AZDSE000
212000060126sel 1c                   select
212100060126w   1c                   when           %found(AZDSE01L)
212200060126     c                             and  V2Cgsc  = *blanks
212300060126     c                   DELETE    AZDSE000
212400060126w   1c                   when           %found(AZDSE01L)
212500060126     c                             and  V2Cgsc <> DSEuni
212600060126     c                   exsr      Rie_AZDSE
212700060126     c                   UPDATE    AZDSE000
212800060126w   1c                   when      NOT  %found(AZDSE01L)
212900060126     c                             and  V2Cgsc <> *blanks
213000060126     c                   exsr      Rie_AZDSE
213100060126     c                   WRITE     AZDSE000
213200060126x   1c                   other
213300060126     c                   UNLOCK    AZDSE01L
213400060126e   1c                   endsl
213500060125      *
213600060125      * - Orari di Scarico
213700060125     c                   eval      DSEtrk  = 'OSC'
213800060126     c     K02DSE01      chain     AZDSE000
213900060126sel 1c                   select
214000060126w   1c                   when           %found(AZDSE01L)
214100060126     c                             and  V2Cosc  = *blanks
214200060126     c                   DELETE    AZDSE000
214300060126w   1c                   when           %found(AZDSE01L)
214400060126     c                             and  V2Cosc <> DSEuni
214500060126     c                   exsr      Rie_AZDSE
214600060126     c                   UPDATE    AZDSE000
214700060126w   1c                   when      NOT  %found(AZDSE01L)
214800060126     c                             and  V2Cosc <> *blanks
214900060126     c                   exsr      Rie_AZDSE
215000060126     c                   WRITE     AZDSE000
215100060126x   1c                   other
215200060126     c                   UNLOCK    AZDSE01L
215300060126e   1c                   endsl
215400060125      *
215500060125      * - Servizi Accessori in Consegna
215600060125     c                   eval      DSEtrk  = 'SAC'
215700060126     c     K02DSE01      chain     AZDSE000
215800060126sel 1c                   select
215900060126w   1c                   when           %found(AZDSE01L)
216000060126     c                             and  V2Csac  = *blanks
216100060126     c                   DELETE    AZDSE000
216200060126w   1c                   when           %found(AZDSE01L)
216300060126     c                             and  V2Csac <> DSEuni
216400060126     c                   exsr      Rie_AZDSE
216500060126     c                   UPDATE    AZDSE000
216600060126w   1c                   when      NOT  %found(AZDSE01L)
216700060126     c                             and  V2Csac <> *blanks
216800060126     c                   exsr      Rie_AZDSE
216900060126     c                   WRITE     AZDSE000
217000060126x   1c                   other
217100060126     c                   UNLOCK    AZDSE01L
217200060126e   1c                   endsl
217300060125      *
217400060125     c                   ENDSR
217500060126      *
217600060126      *---------------------------------------------------------------*
217700060126      * IMPOSTAZIONE CAMPI DI AZDSE00F (A SECONDA DEL TIPO RECORD)    *
217800060126      *---------------------------------------------------------------*
217900060126     c     Rie_AZDSE     BEGSR
218000060126      *
218100060126     c                   eval      DSEatb  = DSTatb
218200060126     c                   eval      DSEcod  = DSTcod
218300060126     c*** già impostato: eval      DSEtrk  = ...
218400060126      *
218500060126sel 1c                   SELECT
218600060126      * - Numero Fax /
218700060126      *   Modalità di appuntamento /
218800060126      *   Tipo Comunicazione /
218900060126      *   Tipo Modulo Comunicazione /
219000060126      *   Copia DDT
219100060126w   1c                   WHEN      DSEtrk  = 'FLO'
219200060126     c                   clear                   dDSEflo
219300060126     c                   eval      §DSEfax = V1Cfax
219400060126     c                   eval      §DSEapp = V2Capp
219500060126     c                   eval      §DSEcom = V2Ccom
219600060126     c                   eval      §DSEmod = V2Cmod
219700060126     c                   eval      §DSEddt = V2Cddt
219800060126     c                   movel(p)  dDSEflo       DSEuni
219900070905      *
220000070905      * - Filiale Gestione Giro Consegna /
220100070905      *   Codice Giro Tellus /
220200070905      *   1° Giorno Chiusura Destinatario /
220300070905      *   2° Giorno Chiusura Destinatario
220400070905w   1c                   WHEN      DSEtrk  = 'XT1'
220500070905     c                   clear                   dDSExt1
220600070905     c                   eval      §DSEXT1pog = V1Cpocgi
220700070905     c                   eval      §DSEXT1cgi = V1Ccgi
220800070913     c                   eval      §DSEXT1gc1 = V1Cgc1
220900070913     c                   eval      §DSEXT1gc2 = V1Cgc2
221000090707     c                   eval      §DSEXT1tic = V1Cticf
221100070905     c                   movel(p)  dDSExt1       DSEuni
221200060126      * - Indirizzo @-Mail
221300060126w   1c                   WHEN      DSEtrk  = 'IEM'
221400060126     c                   eval      DSEuni  = V1Ciem
221500060126      * - Giorni Stabiliti di Consegna
221600060126w   1c                   WHEN      DSEtrk  = 'GSC'
221700060126     c                   eval      DSEuni  = V2Cgsc
221800060126      * - Orari di Scarico
221900060126w   1c                   WHEN      DSEtrk  = 'OSC'
222000060126     c                   eval      DSEuni  = V2Cosc
222100060126      * - Servizi Accessori in Consegna
222200060126w   1c                   WHEN      DSEtrk  = 'SAC'
222300060126     c                   eval      DSEuni  = V2Csac
222400060126      *
222500060126e   1c                   ENDSL
222600060126      *
222700060126     c                   ENDSR
222800060104      *
222900060104      *---------------------------------------------------------------*
223000060104      * MODIFICA RECORD                                               *
223100060104      *---------------------------------------------------------------*
223200060104     c     Upd_AZDST     BEGSR
223300060104      *
223400060104      * Impostazione dei campi nel record da scrivere
223500060104     c                   exsr      sr_RieFld
223600060104      *
223700060104     c                   UPDATE    AZDST000
223800060125      *
223900060125      * "Allineamento" file AZDSE00F
224000060125     c                   exsr      All_AZDSE
224100060104      *
224200060104      * Esce dal pgm
224300060104     c                   movel     $Msg(3)       OT0msg                         *MESSAGGIO
224400060104     c                   eval      $Fine   = *on
224500060104      *
224600060104      *
224700060104      * Verifica se la modifica interessa campi della chiave esterna
224800060104      *   sui sinonimi (AZDSS)
224900060104if  1c                   if             V1Cnaz  = S_IT0naz
225000060104     c                             and  V1Cprv  = S_IT0prv
225100060104     c                             and  V1Ccap  = S_IT0cap
225200060104     c                             and  V1Cloc  = S_IT0loc
225300060104     c                             and  V1Crag  = S_IT0rag
225400060104     c                             and  V1Ckwi  = S_IT0kwi
225500060104     c                   leavesr
225600060104e   1c                   endif
225700060104      *
225800060104     c     K06DSS03P     setll     AZDSS000
225900060104     c     K06DSS03P     reade     AZDSS000
226000060104      *
226100060104do  1c                   DOW       NOT %eof(AZDSS03L)
226200060104      *
226300060104      * - Annulla i sinonimi con la vecchia chiave
226400060104     c                   DELETE    AZDSS000
226500060104      *
226600060104      * - Verifica eventuale esistenza di sinonimi con la nuova chiave
226700060104     c     K07DSS04      chain     AZDSS004
226800060104      *
226900060104      *   Se trovati: aggiorno i sinonimi con la nuova chiave
227000060104if  2c                   if        %found(AZDSS04L)
227100060104     c                   eval      DSSute  = KNMUS                              *UTENTE INS/VAR
227200060104     c                   eval      DSSdiv  = Wdate                              *DATA INS/VAR
227300060104     c                   clear                   DSSatb                         *FLAG ANNULLAMENTO
227400060104     c***                clear                   DSSftr                         *FLAG TRASMISSIONE
227500060104     c***                clear                   DSSdtr                         *DATA TRASMISSIONE
227600060104     c                   UPDATE    AZDSS004
227700060104      *
227800060104      *   Altrimenti: inserisco i sinonimi con la nuova chiave
227900060104x   2c                   else
228000060104     c                   eval      DSSnaz  = V1Cnaz
228100060104     c                   eval      DSSprv  = V1Cprv
228200060104     c                   eval      DSScap  = V1Ccap
228300060104     c                   eval      DSSloc  = V1Cloc
228400060104     c                   eval      DSSrag  = V1Crag
228500060104     c                   eval      DSSkwi  = V1Ckwi
228600060104     c                   eval      DSSute  = KNMUS                              *UTENTE INS/VAR
228700060104     c                   eval      DSSdiv  = Wdate                              *DATA INS/VAR
228800060104     c                   clear                   DSSatb                         *FLAG ANNULLAMENTO
228900060104     c***                clear                   DSSftr                         *FLAG TRASMISSIONE
229000060104     c***                clear                   DSSdtr                         *DATA TRASMISSIONE
229100060104     c                   WRITE     AZDSS004
229200060104e   2c                   endif
229300060104      *
229400060104     c     K06DSS03P     reade     AZDSS03L
229500060104      *
229600060104e   1c                   ENDDO
229700060104      *
229800060104     c                   ENDSR
229900060104      *
230000060104      *---------------------------------------------------------------*
230100060104      * COPIA RECORD                                                  *
230200060104      *---------------------------------------------------------------*
230300060104     c     Cpy_AZDST     BEGSR
230400060104      *
230500060104      * Impostazione dei campi nel record da scrivere
230600060104     c                   eval      DSTcod  = O33nrf
230700060104     c                   exsr      sr_RieFld
230800060104      *
230900060104     c                   WRITE     AZDST000
231000060125      *
231100060125      * "Allineamento" file AZDSE00F
231200060125     c                   exsr      All_AZDSE
231300060104      *
231400060104      * Esce dal pgm
231500060104     c                   movel     $Msg(15)      OT0msg                         *MESSAGGIO
231600060104     c                   eval      $Fine   = *on
231700060104      *
231800060104     c                   ENDSR
231900060104      *
232000060104      *---------------------------------------------------------------*
232100060131      * CANCELLAZIONE RECORD                                          *
232200060104      *---------------------------------------------------------------*
232300060104     c     Dlt_AZDST     BEGSR
232400060104      *
232500060220     c     K07DST03V     chain     AZDST000
232600060104     c                   DELETE    AZDST000
232700060125      *
232800060125      * "Allineamento" file AZDSE00F
232900060125     c     DSTcod        setll     AZDSE000
233000060125     c                   dou       NOT %found(AZDSE01L)
233100060125     c     DSTcod        DELETE    AZDSE000
233200060125     c                   enddo
233300060104      *
233400060104      * Cancella anche tutti i sinonimi del destinatario
233500060104     c     K06DSS03      setll     AZDSS000
233600060104     c     K06DSS03      reade     AZDSS000
233700060104     c                   dow       NOT %eof(AZDSS03L)
233800060104      *
233900060104     c                   DELETE    AZDSS000
234000060104      *
234100060104     c     K06DSS03      reade     AZDSS000
234200060104     c                   enddo
234300060104      *
234400060104      * Esce dal pgm
234500060104     c                   movel     $Msg(4)       OT0msg                         *MESSAGGIO
234600060104     c                   eval      $Fine   = *on
234700060104      *
234800060104     c                   ENDSR
234900060104      *
235000060104      *---------------------------------------------------------------*
235100060104      * RIRPRISTINO RECORD PRECEDENTEMENTE ANNULLATO                  *
235200060104      *---------------------------------------------------------------*
235300060104     c     Rip_AZDST     BEGSR
235400060104      *
235500060104      * Impostazione dei campi nel record da scrivere
235600060104     c                   eval      DSTute  = KNMUS                              *UTENTE INS/VAR
235700060104     c                   eval      DSTdiv  = Wdate                              *DATA INS/VAR
235800060104     c                   clear                   DSTatb                         *FLAG ANNULLAMENTO
235900060104     c***                clear                   DSTftr                         *FLAG TRASMISSIONE
236000060104     c***                clear                   DSTdtr                         *DATA TRASMISSIONE
236100060104      *
236200060104     c                   UPDATE    AZDST000
236300060125      *
236400060125      * "Allineamento" file AZDSE00F
236500060125     c     DSTcod        setll     AZDSE000
236600060125     c     DSTcod        reade     AZDSE000
236700060125     c                   dow       NOT %eof(AZDSE01L)
236800060125     c                   clear                   DSEatb                         *FLAG ANNULLAMENTO
236900060125     c                   UPDATE    AZDSE000
237000060125     c     DSTcod        reade     AZDSE000
237100060125     c                   enddo
237200060104      *
237300060104      * Esce dal pgm
237400060104     c                   movel     $Msg(5)       OT0msg                         *MESSAGGIO
237500060104     c                   eval      $Fine   = *on
237600060104      *
237700060104     c                   ENDSR
237800060104      *
237900060104      *---------------------------------------------------------------*
238000060104      * IMPOSTAZIONE (PARZIALE) DEI CAMPI DEL RECORD DEL FILE         *
238100060104      *---------------------------------------------------------------*
238200060104     c     sr_RieFld     BEGSR
238300060104      *
238400060104     c                   eval      DSTnaz  = V1Cnaz
238500060104     c                   eval      DSTprv  = V1Cprv
238600060104     c                   eval      DSTcap  = V1Ccap
238700060104     c                   eval      DSTloc  = V1Cloc
238800060104     c                   eval      DSTind  = V1Cind
238900060104     c                   eval      DSTrag  = V1Crag
239000060104     c                   eval      DSTkwi  = V1Ckwi
239100060104     c                   eval      DSTtel  = V1Ctel
239200060104     c                   eval      DSTref  = V1Cref
239300060104     c                   eval      DSTnot  = V1Cnot
239400060104     c                   eval      DSTdos  = V1Cdos
239500060104     c                   eval      DSTute  = KNMUS                              *UTENTE INS/VAR
239600060104     c                   eval      DSTdiv  = Wdate                              *DATA INS/VAR
239700051223      *
239800060102      * - Imposta campi della ds DDSTFLO per campo DSTFLO
239900060104if  1c                   IF        DEPop0  = 'P02'
240000060102     c                   clear                   dDSTflo
240100060110     c                   eval      §DSTddt = V1Cddt
240200060110if  2c                   if        V1Clna  > *zeros
240300060110     c                   move      V1Clna        §DSTlna
240400060110     c                   move      V1Cznc        §DSTznc
240500090317     c*****              movel     V1Ccgi        §DSTcgi
240600060104x   2c                   else
240700060102     c                   clear                   §DSTlna
240800060102     c                   clear                   §DSTznc
240900090317     c*****              clear                   §DSTcgi
241000060104e   2c                   endif
241100060102     c                   movel     dDSTflo       DSTflo
241200060104e   1c                   ENDIF
241300060104      *
241400060104     c***                clear                   DSTatb
241500060104     c***                clear                   DSTftr
241600060104     c***                clear                   DSTdtr
241700060104      *
241800060104     c                   ENDSR
241900060104      *
242000060104      *---------------------------------------------------------------*
242100060104      * Decodifica Tipo Destinatario                                  *
242200060104      *---------------------------------------------------------------*
242300060104     c     Decod_DOS     BEGSR
242400060104      *
242500060104     c                   clear                   dTDP
242600060104     c                   clear                   TIBS02ds
242700060104     c                   eval      T02mod  = 'C'
242800070914     c                   eval      T02sif  = KNSIF
242900060104     c                   eval      T02cod  = 'TDP'
243000060104     c                   movel(p)  V1Cdos        T02ke1
243100060104     c                   call      'TIBS02R'
243200060104     c                   parm                    KPJBA
243300060104     c                   parm                    TIBS02ds
243400060104     c                   if        T02err  = *blanks
243500060104     c                   movel     T02uni        dTDP
243600060104     c                   endif
243700060124     c                   movel     §TDPdes       V1Ddos
243800060104      *
243900060104     c                   ENDSR
244000060104      *
244100060104      *---------------------------------------------------------------*
244200060104      * Controllo di tutti i dati di "INSTRADAMENTO"                  *
244300060104      *---------------------------------------------------------------*
244400060104     c     Chk_INSTR     BEGSR
244500060307      *
244600060307      * Verifico subito se premuto F17 x forzatura località
244700070913     c                   if        wF17 = *off
244800060307     c                   movel     *INKR         wF17
244900060307     c                   endif
245000060104      *
245100060104     c                   clear                   TISI95ds                       *azzera i parametri
245200060104     c***                clear                   I95tla                         *tipo lancio
245300060104     c                   movel     '7'           I95tcn                         *tipo controllo
245400060104     c                   movel     V1Cloc        I95loc                         *località
245500060104     c                   movel     V1Ccap        I95cap                         *cap
245600060104     c                   movel     V1Cprv        I95prv                         *provincia
245700060104     c                   movel     V1Cnaz        I95nar                         *nazione
245800060104     c                   z-add     Wdate         I95dat                         *data di riferimento
245900060104      *
246000060104     c                   call      'TISI95R'
246100060104     c                   parm                    TISI95DS
246200060104      *
246300060104      * Se livello dati 3 o 4: "normalizzo" i dati di "instradamento"
246400060104      *   inseriti a video
246500060104if  1c                   IF            (O95lid  = '3'
246600060104     c                             or   O95lid  = '4')
246700060104     c                             and  O95flg <> 'S'
246800060104      *
246900060104if  2c                   if             V1Cloc <> O95loc
247000060104     c                             or   V1Ccap <> O95cap
247100060104     c                             or   V1Cprv <> O95prv
247200060104     c                             or   V1Cnaz <> O95nar
247300060104     c                   eval      V1Ccap  = O95cap
247400060104     c                   eval      V1Cloc  = O95loc
247500060104     c                   eval      V1Cprv  = O95prv
247600060104     c                   eval      V1Cnaz  = O95nar
247700060104     c                   seton                                        28  90
247800060104     c                   seton                                        535455
247900060104     c                   seton                                        58
248000060104     c                   movel     $Msg(16)      V1Dmsg
248100060104e   2c                   endif
248200060104      *
248300060104x   1c                   ELSE
248400060104      *
248500060104     c                   seton                                        535455
248600060104     c                   seton                                        58
248700070913      *
248800070913      * Consento cmq eventuale forzatura della sola località
248900060307if  2c                   if             V1Cloc <> O95loc
249000060307     c                             and  V1Ccap  = O95cap
249100060307     c                             and  V1Cprv  = O95prv
249200060307     c                             and  V1Cnaz  = O95nar
249300070913     c                   if        wForzaLoc = *off   OR
249400070913     c                             wF17      = *off
249500070913     c                   eval      *in17     = *on
249600070913     c                   eval      wForzaLoc = *on
249700060307     c                   seton                                        28  90
249800060307     c                   movel     $Msg(14)      V1Dmsg
249900060307     c                   endif
250000060307     c                   else
250100160905     c                   if             V1Cloc  = O95loc
250200160905     c                             and  V1Ccap  = O95cap
250300160905     c                             and  V1Cprv  = O95prv
250400160905     c                             and  V1Cnaz  = O95nar
250500160905     c                   else
250600060307     c                   seton                                        28  90
250700060307     c                   movel     $Msg(17)      V1Dmsg
250800160905     c                   endif
250900060307     c                   endif
251000060104      *
251100060104e   1c                   ENDIF
251200060104      *
251300060104     c                   ENDSR
251400060104
251500060103      *---------------------------------------------------------------*
251600060104
251700060103** - $Op1 / $Fun
251800060103F06     INSERIMENTO
251900060103O02       MODIFICA
252000060103O02P03MODIFICA TEL/REF
252100060103O03        COPIA
252200060131O04    CANCELLAZIONE                    ex ANNULLAMENTO
252300060103O05   VISUALIZZAZIONE
252400060103O07      RIPRISTINO
252500060103PF6   INSERIMENTO APP.
252600060103P02   MODIFICA APPUNT.
252700060103P05     VISUAL. PROP.
252800060103P06    CONFERMA PROP.
252900060103P15     VISUAL. DEST.
253000070912** - $MSG -------------------------------------------------------------------*
253100090710Tasto funzionale non gestito                                                    1
253200090710Inserimento effettuato                                                          2
253300090710Modifica effettuata                                                             3
253400090710Annullamento effettuato                                                         4
253500090710Ripristino effettuato                                                           5
253600090710Richiesta funzione per record inesistente                                       6
253700090710Impossibile inserire, record già presente                                       7
253800090710Richiesta funzione per record esistente                                         8
253900090710Giro consegna inseribile solo se indicata la relativa filiale                   9
254000090710Impossibile ins. se già presenti dati x stesso Destinatario con '*'            10
254100090710Non trovati giri consegna per la linea di arrivo indicata                      11
254200090710Campo obbligatorio, valorizzarlo                                               12
254300090710Chiave ricerca indirizzo incompatibile con indirizzo, correggere               13
254400090710NAZ/PRV/CAP/LOC incongruenti, F17 per forzare                                  14
254500090710Copia effettuata                                                               15
254600090710ATTENZIONE... dati a video sostituiti !                                        16
254700090710NAZ/PRV/CAP/LOC incongruenti, verificare e correggere                          17
254800090710Conflitto di destinatari dis/sup.                                              18
254900090710Indicare almeno 2 caratteri.                                                   19
255000090710Utilizzo di '*' non consentito                                                 20
255100090710Per destinatari "su appuntamento" indicare 'A'                                 21
255200090710Tipo destinatario errato                                                       22
255300090710Valore NON ammesso per il campo                                                23
255400090710Filiale errata                                                                 24
255500090710Zona errata                                                                    25
255600090710Destinatario di tipo "V" richiede ALMENO UNO tra i dati evidenziati            26
255700090710Modalità di appuntamento selezionata richiede il numero di telefono            27
255800090710Modalità di appuntamento selezionata richiede il numero di fax                 28
255900090710Modalità di appuntamento selezionata richiede l'indirizzo e-mail               29
256000090710Zona di consegna NON abilitata                                                 30
256100090710Filiale giro diversa dalla "Linea" di instradamento prestabilito               31
256200090710Filiale giro inseribile solo se indicato il relativo giro consegna             32
256300090710Giro consegna indicato non valido o inesistente                                33
256400090710Giorno di chiusura errato                                                      34
256500090710Linea obbligatoria per il giro consegna                                        35
256600090710Tipo incasso c/assegni forzato errato                                          36
256700090710Utente NON autorizzato alla forzatura del tipo incasso c/assegni               37
