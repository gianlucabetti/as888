000100960418     H*BIND
000110951130     H*PARMS OPTION(*NOXREF)
000120950915     H*PARMS CVTOPT(*DATETIME)
000130940223     H DECEDIT('0,') DATEDIT(*DMY.)
000140961219      *
000150961219      *  Questo modulo è riferito al programma NDC004R !!!
000160961219      *  (quindi vanno compilati prima NDC015R e NDC010R poi NDC004R)
000170961219      *
000180001227      * C1456 - ATTENZIONE: il video NDC015D è in "share" con il modulo
000190001228      *         NDC515R (che gestisce il subfile "specchio"), per cui nel
000200001227      *         caso di modifica del video è necessario almeno ricompilare
000210001228      *         anche il modulo NDC515R
000220001227      *
000230940117      *
000240950719      *            Testata prima nota doc. IVA
000250950801      *           -----------------------------
000260940223      *
000270940223      *
000280940223      * DOCUMENTI MICROANALISI :
000290940223      *
000300940223      * NOTE TECNICHE :
000310950616      *  1) ELIMINARE LE SPECIFICHE RELATIVE AL COMMIT/ROLLBACK SE
000320950616      *     QUESTO NON DEVE ESSERE GESTITO
000330941214      *
000340940223      * PERSONALIZZAZIONI
000350990421      *
000360990421      * MODIFICHE:
000370001020D0611 *---------------------------------------------------------------------------------------------
000380001020  "   *          GESTIONE EMISSIONE ERRORI:
000390001020  "   *     ACCENDERE gli INDICATORI di ERRORE
000400001020  "   *      SOLO un ATTIMO PRIMA dell'EXFMT
000410001020  "   *
000420001023  "   * Usare per ogni videata, una schiera apposita per
000430001023  "   * memorizzare quali indicatori di errori devono essere accesi
000440001020  "   *
000450001023  "   * Questo pgm gestisce 3 videate diverse (D1/D2/C1);
000460001023  "   * se esiste uno stesso indicatore usato in 2 videate
000470001023  "   * ma per messaggi diversi, può accadere che il pgm
000480001023  "   * accenda l'indicatore per un errore su un dato non
000490001023  "   * presente nell'ultimo video emesso e ciò causa
000500001023  "   * l'emissione di errori che in realtà non esistono.
000510001023  "   *
000520001020D0611 *
000530991011      *---------------------------------------------------------------------------------------------
000540990421C1176 * C1176: in caso di documento in divisa, il calcolo
000550990505  "   *        dell'imponibile e dell'imposta in m.c. deve essere
000560990505  "   *        il risultato della conversione dai rispettivi
000570990505  "   *        imponibili e imposte in divisa!
000580990505C1176 *        Gestita anche la relativa window di errore.
000590991011      *---------------------------------------------------------------------------------------------
000600990505C1129 * C1129: In fase di revisione di un movimento di fattura,
000610990505  "   *        se si va a modificare il codice del cliente/fornitore,
000620990505  "   *        il pgm aggiornava il movimento Iva nella controp. ut.
000630990505  "   *        con il nuovo codice, ma non lo faceva sulle altre righe
000640990506  "   *        di movimento; quindi si è utilizzata una window in cui
000650990505  "   *        l'utente può decidere se modificare solo le controp. ut.
000660990505  "   *        uguali al conto cambiato oppure tutte le controp. ut.
000670990505  "   *        (anche quelle vuote eccetto la I°riga)
000680990505  "   *        oppure non fare nulla.
000690990505  "   *        ATTENZIONE: la gestione della modifica delle controp.ut.
000700990514C1129 *                    è affidata ud un modulo nuovo (NDC019R)
000710991011      *---------------------------------------------------------------------------------------------
000720990723C1207 * C1207: se la causale non pretende alcun controllo sull'
000730990723  "   *        esistenza della partita (OpeCtPar=' '), è stata
000740990723  "   *        aggiunta una window che avverte dell'esistenza
000750990723  "   *        della partita e fa scegliere all'utente stesso
000760990723  "   *        se continuare o no.
000770990723  "   * C1207: nella videata dell'iva, se l'utente non ha digitato
000780990723  "   *        nulla, ma la causale richiede una quadratura iva
000790990723  "   *        aggiunto l'errore che chiede di inserire almeno un
000800990723  "   *        valore (prima che compaia l'NDC0520 -Es:
000810990723C1207 *        (F1) Differenza Iva in divisa:120,000; in m.c:120,00;
000820991011      *---------------------------------------------------------------------------------------------
000830991011C1212 * C1212  Aggiunto il controllo che l'utente sia abilitato
000840991011  "   *        nell'ambito dell'autorizzazione all'unità,
000850991011C1212 *        al tipo registrazione;
000860991011      *---------------------------------------------------------------------------------------------
000870991012C1251 * C1251 1) Se è stata scelta una condizione di pagamento con
000880991011  "   *          effetti ed il conto principale è un percipiente,
000890991011  "   *          l'utente viene avvertito con una Window:
000900991012  "   *   ....................................................................
000910991012  "   *   . ATTENZIONE: si stanno creando effetti passivi per un percipiente,.
000920991012  "   *   . questa modalità non è gestita per le ritenute d'acconto.         .
000930991012  "   *   .  Vuoi continuare?                                                .
000940991012  "   *   .  ° Si                                                            .
000950991012  "   *   .  ° No                                                            .
000960991012  "   *   ....................................................................
000970991011  "   *       2) In manutenzione di una fattura fornitore, se il fornit.
000980991011  "   *          è un percipiente con ritenuta d'acconto, proteggo il for
000990991011  "   *       3) Eliminato nel Dspf un campo che non serve a nulla
001000991012  "   *          (probabilmente è stato aggiunto per sbaglio con l'SDA)
001010991012  "   *       4) Migliorata gestione XMsgr:
001020991012  "   *          .Dove possibile nella gestione delle risposte
001030991012  "   *          da passare a XMsgr, ho eliminato l'uso di XTxt
001040991012  "   *          (sostituibile sempilemente con delle Eval)
001050991012  "   *          .Dove possibile nella gestione dei msg
001060991012  "   *          da passare a XMsgr, ho specificato il contenuto
001070991012  "   *          del messaggio in una riga di commento
001080991012  "   *          .Dove possibile nella gestione del passaggio parametri
001090991012C1251 *          a XMsgr, ho asteriscato i param. passati inutilmente
001100001011D0732 *  D0732   il controllo del conto eseguito dalla prima videata
001110001011 "    *          non deve tenere conto dell'udate (passando *omit
001120001011 "    *          il modulo ndmvc002 controlla la data annullamento del
001130001011 "    *          conto con udate).
001140001011 "    *          La data di annullamento del conto va testata con la
001150001011 "    *          data registrazione (che è nella seconda videata) e
001160001011 "    *          quindi aggiunto il controllo anche nella seconda vide
001170001011D0732 *          ata passando la data registrazione)
001180001026C1432 *---------------------------------------------------------------------------------------------
001190001031  "   * C1432 : per evitare che venga emesso un errore INCORREGGIBILE
001200001031  "   *         nel caso di imponibile + imposta squadrati rispetto al
001210001031  "   *         totale documento - se il ctrl è richiesto dalla
001220001031  "   *         causale - e registri IVA già stampati (questo caso può
001230001031  "   *         accadere al 99% solo dopo una conversione della moneta
001240001031  "   *         di conto in Euro) è stato aggiunto un flag a NDMVC202
001250001031  "   *         che, se *on, impedisce questo controllo. Tanto una
001260001031  "   *         volta stampati i registri, ciò che c'è in fattura non
001270001031  "   *         può più essere modificato.
001280001031  "   *         Il fatto che uscisse questo errore impediva alla
001290001031  "   *         registrazione di essere modificata nelle parti libere
001300001031C1432 *         (costi e ricavi).
001310010620C1494 *---------------------------------------------------------------------------------------------
001320010620  "   *         aggiunta la gestione della data decorrenza pagamento
001330010621C1494 *         mediante la data libera 2 su NDREG
001340010820C1512 *---------------------------------------------------------------------------------------------
001350011002  "   * C1512   Protetti importi in div ed in m.c. + cambio in testata
001360010820  "   *         qualora la divisa di partita diversa dalla divisa
001370010820  "   *         del movimento (aggiunto a video divisa di partita
001380010820C1512 *         quindi spostati alcuni campi preesistenti..vedi video)
001390020226C1547 *-------------------------------------------------------------------------
001400020226  "   * 20/2/02 modificata la gestione della data decorrenza pagamento
001410020226  "   *         impostata da C1494: eliminato il controllo che data
001420020226  "   *         dec. pagam. debba essere >= dt doc (quindi viene
001430020226  "   *         controllata solo se è corretta formalmente)
001440020226  "   *         P.S:
001450020226C1547 *         se modificata non scatta il ricalcolo rate
001460010620      *---------------------------------------------------------------------------------------------
001470040113C1716 *-------------------------------------------------------------------------
001480040113  "   *         se nel parametro modulo IVA è impostato il controllo
001490040113  "   *         della dichiarazione di intento fornitore, viene
001500040113  "   *         richiamato il driver PJXX011R
001510040113C1716 *-------------------------------------------------------------------------
001520940223      *
001530940223      *               MAPPA INDICATORI STD
001540940223      *
001550940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
001560940223      *  21           GENERICO OPERAZIONI I/O
001570940223      *  22           GENERICO ERRORE OPERAZIONI I/O
001580940223      *  30           SFLDSP
001590940223      * N31           SFLCLR
001600940128      *  31           SFLDSPCTL
001610940128      *  32           SFLNXTCHG
001620940128      *  33           SFLEND
001630940128      *  39           OF PRTF
001640940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001650950616      *  Specificare l'uso dei singoli indicatori
001660940315      *  50 <---> 98  ERRORI SU VIDEO
001670950616      *  Specificare l'uso dei singoli indicatori
001680940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
001690940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001710940128      *  99           INDIC. GENERALE DI ERRORE
001720940128     F*----------------------------------------------------*
001730950621     F*------------
001740950803     FANDFT01L  IF   E           K DISK
001750970520     F*------------
001760970520     FANCDP01L  IF   E           K DISK
001770991012C1251F*------------
001780991011  "  FANCDR01L  IF   E           K DISK
001790991011  "  F*------------
001800991012C1251FNDMRA08L  IF   E           K DISK
001810950803     F*------------
001820950720     FANUNI01L  IF   E           K DISK
001830950719     F*------------
001840950720     FNDPAS01L  IF   E           K DISK
001850950726     F*------------
001860040430D1602FNDPAS04L  IF   E           K DISK
001870040430  "  F                                     RENAME(NDPAS000:NDPAS04)
001880040430D1602 *------------
001890950726     FNDPPA01L  IF   E           K DISK
001900001025C1429 *------------
001910001025C1429FNDPPS01L  IF   E           K DISK
001920020327C1550 *------------
001930020327  "   * slittamenti in regime iva trasportatori iva mista
001940020327C1550FANSLI01L  IF   E           K DISK    usropn
001950021002C1585 *------------
001960021002  "   * Associaz. libri-rif. IVA: Soc/TpReg/Libro/Aliq/RifIva-O Ann='1'
001970021002C1585FANALR02L  IF   E           K DISK
001980060620C1937FNDMOV08L  IF   E           K DISK
001990060619  "  F                                     Rename(NDMov000:NDMov08)
002000060619  "  FNDMOV19L  IF   E           K DISK
002010060619c1937F                                     Rename(NDMov000:NDMov19)
002020090126     F*------------
002030090126C2116F* file elenco documenti
002040090126  "  FNDED202L  IF   E           K DISK
002050090126c2116FNDED101L  IF   E           K DISK
002060950719     F*------------
002070950719     FNDC015D   CF   E             WORKSTN
002080950719     F                                     RENAME(C015T1:FMTT1)
002090950719     F                                     RENAME(C015D1:FMTD1)
002100950719     F                                     RENAME(C015Z1:FMTZ1)
002110950719     F                                     RENAME(C015D2:FMTD2)
002120950719     F                                     RENAME(C015Z2:FMTZ2)
002130950724     F                                     RENAME(C015C1:FMTC1)
002140950724     F                                     RENAME(C015S1:FMTS1)
002150950721     F                                     RENAME(C015Z3:FMTZ3)
002160950724     F                                     SFILE(FMTS1:S1NRR)
002170940201     F                                     INFDS(DSFMT)
002180941215     F*------------
002190980922     D*-------------
002200980922     D* PROTOTIPAZIONE DELLE PROCEDURE
002210981103     D/COPY *LIBL/SRCCPY,PJX002PR
002220950920     D*-------------
002230950922     D Dt0             C                   const('0001-01-01')
002240960122     D Dt9             C                   const('9999-12-31')
002250940128     D*----------------------------------------------------*
002260950404     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002270950404     D $ID             S             27    DIM(5) ALT($OZ)
002280940506     D* Schiere per la gestione dei tasti di funzione
002290940506     D $FM             S             27    DIM(24)
002300940506     D $FX             S             50    DIM(24)
002310940506     D $FC             S              1    DIM(24)
002320940506     D $FR             S              1    DIM(24)
002330960212     D*-------------
002340960212     D* Indice della schiera tasti funzione
002350960212     D PosFun          S              2  0
002360960212     D*-------------
002370960212     D*
002380940506     D* Video D1
002390940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002400940506     D $FX1            S             50    DIM(24) ALT($FM1)
002410940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
002420940506     D $FR1            S              1    DIM(24) ALT($FC1)
002430960212     D* Schiera contente i nomi dei programmi da richiamare con i tasti
002440960212     D* funzione personalizzati
002450960212     D $PGF1           S             10    DIM(25)
002460960212     D*-------------
002470950719     D* Video D2
002480950719     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
002490950719     D $FX2            S             50    DIM(24) ALT($FM2)
002500950719     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
002510950719     D $FR2            S              1    DIM(24) ALT($FC2)
002520960212     D* Schiera contente i nomi dei programmi da richiamare con i tasti
002530960212     D* funzione personalizzati
002540960212     D $PGF2           S             10    DIM(25)
002550960212     D*-------------
002560950724     D* Video S1
002570950721     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
002580950721     D $FX3            S             50    DIM(24) ALT($FM3)
002590950721     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
002600950721     D $FR3            S              1    DIM(24) ALT($FC3)
002610960212     D* Schiera contente i nomi dei programmi da richiamare con i tasti
002620960212     D* funzione personalizzati
002630960212     D $PGF3           S             10    DIM(25)
002640940127     D*-------------
002650940127     D* Passaggio Parametri
002660940127     D KPJBA         E DS
002670950616     D*-------------
002680950616     D* Autorizzazioni alla contabilità
002690950620     D XAutCtbDS     E DS
002700950616     D*-------------
002710950616     D* Autorizzazioni alla unità
002720950620     D XAutUniDS     E DS
002730990809C1212D XAutUniDS1    E DS
002740950620     D*-------------
002750950620     D* Autorizzazioni alla causale
002760950620     D XAutCauDS     E DS
002770050707     D*-------------
002780050707A1125D* Parametri del PGM di gestione traduzioni
002790050707  "  D XtraDs        E DS                  Inz
002800050707A1125D*-------------
002810950126     D* Reperimento nome PGM
002820950126     D STATUS         SDS           333
002830950126     D  DSPGM            *PROC
002840950411     D  PARMS            *PARMS
002850941209     D*-------------
002860941209     D* Dati di ambiente ottenuti da XSOC
002870950616     D SOC001        E DS                  EXTNAME(XSOC001DS)
002880950616     D    FgO1                        1    overlay(XscFgO)
002890000912     D    XscDare                     1    overlay(xscFgo:7)
002900961009     D    XscAvere                    1    overlay(xscFgo:8)
002910020321C1550D    RegimeIva                   1    overlay(XscFgo:9)
002920040524C1763D    XscLegisl                   1    overlay(xscSkp:2)                    Scadenz. retroattivo
002930020304C1530D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
002940081008C2086D    XscGest                     1    overlay(xscSkp:3)
002950090223C2122X*ex C2086  -------------
002960090223  "  X*ex C2086  per verificare l'esistenza di ordini acquisti ancora in
002970090223  "  X*ex C2086  controllo fatture
002980090223  "  X*ex C2086  PJXX044DS     E DS                  Inz
002990090223  "  X*ex C2086   ERRXX044     E                     INZ('0')
003000090223  "  X*ex C2086   RcdFtXX044   E                     INZ('0')
003010090223  "  X*ex C2086  -------------
003020090223  "  X*ex C2086  per verificare l'esistenza di impegni passivi ancora in
003030090223  "  X*ex C2086  controllo fatture
003040090223  "  X*ex C2086  PJCX003DS     E DS                  Inz
003050090223  "  X*ex C2086   ERRCX003     E                     INZ('0')
003060090223  "  X*ex C2086   EsFatCX003   E                     INZ('0')
003070090223  "  X*ex C2086  -----
003080090223  "  X*ex C2086  EsOrdini       S              1    INZ('0')
003090090223C2122X*ex C2086  EsImpegni      S              1    INZ('0')
003100090219C2122D*-------------
003110090223  "  D* Parametri per richiamo controllo fatture
003120090223  "  D NDC711DS      E DS                  Inz
003130090223  "  D  ERR711       E                     INZ('0')
003140090223C2122D  CMT711       E                     INZ('0')
003150941209     D*-------------
003160941209     D* DS Interna per dati di output di XSOC
003170941209     D XSOCDS          DS          1000
003180940203     D*-------------
003190950721     D* Parametri in entrata
003200950719     D NDC015DS      E DS
003210950616     D*-------------
003220960212     D* campo di salvataggio per la DS d'ingresso durante la chiamata
003230960212     D* alla funzione personalizzata
003240040308     D $SaveDS         S                   like(NDC015DS)
003250960212     D*-------------
003260950616     D* Ricerca su tabelle
003270950616     D XTabDS        E DS
003280950928     D* Rcicerca partite
003290960123     D NDC085DS      E DS                   inz
003300950616     D*-------------
003310950616     D* Ricerca divise
003320950616     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
003330950719     D OPZ15         E                     EXTFLD(OPZ015)
003340950719     D ERR15         E                     EXTFLD(ERR015)
003350950719     D OPR15         E                     EXTFLD(OPR015)
003360950719     D RET15         E                     EXTFLD(RET015)
003370950719     D*-------------
003380950719     D* Ricerca sottoconti
003390950719     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
003400950719     D*-------------
003410950719     D* Ricerca condizioni di pagamento
003420950719     D A024DS        E DS                  EXTNAME(ANA024DS) INZ
003430950616     D*-------------
003440950817     D* Ricerca riferimenti e aliquote
003450950817     D ANA068DS      E DS                  INZ
003460951025     D*-------------
003470951025     D*DS per richiamo interrogazione conti
003480951025     D ANA399DS      E DS
003490950817     D*-------------
003500950817     D* Ricerca legende
003510950817     D A964DS        E DS                  EXTNAME(ANA964DS) INZ
003520950719     D*-------------
003530950719     D* Controlli validità legende
003540950719     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
003550950719     D*-------------
003560950719     D* Controllo conti
003570950922     D ANRCODS       E DS                   extname(ANRCO00F)
003580950922     D  RCODTANN     E                     inz('0001-01-01')
003590950922     D  RCODTIMM     E                     inz('0001-01-01')
003600950922     D  RCODTULMO    E                     inz('0001-01-01')
003610950616     D*-------------
003620950616     D* Ricerca unità
003630950616     D ANA009DS      E DS                  INZ
003640950616     D  RET009       E                     INZ('0')
003650950616     D  OPR009       E                     INZ('0')
003660950616     D  ERR009       E                     INZ('0')
003670950616     D*-------------
003680950616     D* Ricerca causale
003690950616     D ANA070DS      E DS                  INZ
003700950616     D  RET070       E                     INZ('0')
003710950616     D  OPR070       E                     INZ('0')
003720950616     D  CMT070       E                     INZ('0')
003730950616     D  ERR070       E                     INZ('0')
003740950616     D  FSC070       E                     INZ('S')
003750950616     D*-------------
003760950621     D* Causali operative
003770950621     D ANOPE         E DS                  EXTNAME(ANOPE00F)
003780950920     D  OPEDTANN     E                      inz(Dt0)
003790950920     D  OPEDTIMM     E                      inz(Dt0)
003800950920     D  OPEDTULMO    E                      inz(Dt0)
003810950621     D*-------------
003820950621     D* Ricerca modello primanota
003830950621     D NDC051DS      E DS
003840950721     D*-------------
003850960517     D* Recupero dati modello PN
003860960517     D NDC011DS      E DS                  inz
003870960517     D  DTREG011     E                      inz(dt0)
003880960517     D  DTDOC011     E                      inz(dt0)
003890960517     D  DTLGIO011    E                      inz(dt0)
003900960517     D*-------------
003910950721     D* Movimenti contabili
003920950721     D NDMOV00F      E DS                  INZ
003930950920     D  MOVDTREG     E                      inz(dt0)
003940950920     D  MOVDTDOC     E                      inz(dt0)
003950950920     D  MOVDTCOCO    E                      inz(dt0)
003960950920     D  MOVDTLGIO    E                      inz(dt0)
003970950920     D  MOVDTLIB     E                      inz(dt0)
003980950721     D*-------------
003990950721     D* Movimenti IVA
004000950721     D NDIVA00F      E DS                  INZ
004010950920     D  IVADTREG     E                      inz(dt0)
004020950920     D  IVADTDOC     E                      inz(dt0)
004030950920     D  IVADTRIF     E                      inz(dt0)
004040950920     D  IVADTCOMP    E                      inz(dt0)
004050940201     D*-------------
004060950816     D* Parametri I/O bottoni registrazione
004070971216     D ND003DS       E DS                  inz
004080950816     D*-------------
004090950616     D* Campi Registrazione da aggiornare
004100950616     D ND004DS       E DS                  INZ
004110950920     D  DTREG004     E                      inz(dt0)
004120950920     D  DTDOC004     E                      inz(dt0)
004130950920     D  DTLGIO004    E                      inz(dt0)
004140950920     D  DTRFCAM004   E                      inz(dt0)
004150950920     D  DTCOCO004    E                      inz(dt0)
004160950920     D  DTCOAN004    E                      inz(dt0)
004170020226C1547 * nd004ds contiene i dati da passare a ndmvc111:
004180020226  "   * le date non inizializzate, sono passate = blank
004190020226  "   * (e ndmvc111 si spacca)
004200020226C1547D   DTDCPAG004  e                     inz('0001-01-01')
004210950616     D*-------------
004220950616     D* Deco Registrazione
004230950616     D ND005DS       E DS                  INZ
004240950816     D*-------------
004250950816     D* Stato movimento
004260950816     D ND006DS       E DS
004270950720     D*-------------
004280950720     D* Controllo movimento
004290950721     D ND007DS       E DS                  INZ
004300950920     D  DTDOC007     E                      inz(dt0)
004310950920     D  DTRFCAM007   E                      inz(dt0)
004320950920     D  DTLIB007     E                      inz(dt0)
004330971202     D*-------------
004340950720     D*output controllo movimento
004350950721     D ND008DS       E DS                  INZ
004360950721     D*-------------
004370950721     D* Controllo registrazione IVA
004380950721     D ND009DS       E DS                  INZ
004390950920     D  DTRIF009     E                      inz(dt0)
004400950920     D  DTCOMP009    E                      inz(dt0)
004410950721     D*-------------
004420950721     D*output controllo registrazione IVA
004430950721     D ND010DS       E DS                  INZ
004440960424     D*-------------
004450960806     D* DS esterna per default di causale
004460960806     D ANDFT         E DS                  EXTNAME(ANDFT00F) inz
004470960806     D*-------------
004480960806     D* DS esterna per dati fiscali
004490960806     D ANCPI         E DS                  EXTNAME(ANCPI00F) INZ
004500950920     D  CPIDTFIVL    E                      inz(dt0)
004510950915     D*-------------
004520950915     D* Driver coordinate bancarie
004530950915     D DvCoBaDS      E DS                  inz
004540950915     D  CobDal       E                     INZ('0001-01-01')
004550950922     D*-------------
004560950922     D* serve per la pulizia dei campi di NDPPA con la sola operazione RESET
004570950922     D NDPPA         E DS                  EXTNAME(NDPPA00F)  inz
004580950922     D  PPADTPAR     E                      inz(Dt0)
004590950922     D  PPADTPARP    E                      inz(Dt0)
004600950922     D  PPADTCHIU    E                      inz(Dt0)
004610950922     D  PPADTULMO    E                      inz(Dt0)
004620950922     D*-------------
004630950922     D* DS esterna per dati clienti e soggetti
004640950922     D DVACLI        E DS                  INZ
004650950922     D  DVCDTENTR    E                      inz(Dt0   )
004660950922     D  DVCDTULMO    E                      inz(Dt0   )
004670950922     D  DVCDTINVL    E                      inz(Dt0   )
004680950922     D  DVCDTFIVL    E                      inz(Dt0   )
004690950922     D  DVCDTNASC    E                      inz(Dt0   )
004700950922     D  DVCDTULMOI   E                      inz(Dt0   )
004710950922     D  DVCDTINVLI   E                      inz(Dt0   )
004720950922     D  DVCDTFIVLI   E                      inz(Dt0   )
004730950922     D  DVCDTINRPC   E                      inz(Dt0   )
004740950922     D  DVCDTULMOC   E                      inz(Dt0   )
004750950922     D  DVCDTFIST    E                      inz(Dt0   )
004760950922     D  DVCDTFIAS    E                      inz(Dt0   )
004770950922     D  DVCDTFIRPC   E                      inz(Dt0   )
004780950922     D  DVCDTINRPL   E                      inz(Dt0   )
004790950922     D  DVCDTFIRPL   E                      inz(Dt0   )
004800950922     D*-------------
004810950922     D* DS esterna per dati fornitori e soggetti
004820950922     D DVAFOR        E DS                  INZ
004830950922     D  DVFDTENTR    E                      inz(Dt0   )
004840950922     D  DVFDTULMO    E                      inz(Dt0   )
004850950922     D  DVFDTINVL    E                      inz(Dt0   )
004860950922     D  DVFDTFIVL    E                      inz(Dt0   )
004870950922     D  DVFDTNASC    E                      inz(Dt0   )
004880950922     D  DVFDTULMOI   E                      inz(Dt0   )
004890950922     D  DVFDTINVLI   E                      inz(Dt0   )
004900950922     D  DVFDTFIVLI   E                      inz(Dt0   )
004910950922     D  DVFDTULMOF   E                      inz(Dt0   )
004920950922     D  DVFDTINRPF   E                      inz(Dt0   )
004930950922     D  DVFDTFIRPF   E                      inz(Dt0   )
004940950922     D  DVFDTCONT    E                      inz(Dt0   )
004950950922     D*--------
004960950922     D ANINDDS       E DS                  extname(ANIND00F)
004970950922     D*--------
004980950719     D* Definizioni parametri driver soggetti
004990950914     DDvaRic           S              1
005000950719     DDvaSocieta       S              3
005010950719     DDvaUnita         S              8
005020950719     DDvaStrutt        S             10
005030950915     DDvaDtRif         S             10A   inz('0001-01-01')
005040950719     DDvaLenOut        S              9B 0
005050950719     DDvaSnatura       S              1
005060950719     DDvaCodice        S              8
005070950719     DDvaLineav        S              3
005080950719     DDvaFiliale       S              3  0
005090950719     DDvaSogg          S              8
005100950719     DDvaTpInd         S              2
005110950719     DDvaCdInd         S              3
005120950719     DDvaErrore        S              1
005130950719     DDvaOutput        S           4000
005140980123     D*-------------
005150980123     D* DS/Variabili per gestire la numerazione SemiAutomatica
005160980123     DXATBDS         E DS
005170980123     DANGG13DS       E DS
005180980123     D SavSerReg       S                   like(D2SerReg)
005190980123     D SavNrReg        S                   like(D2NrReg)
005200980123     D Serie           S                   like(RegSerReg)
005210980123     D*-------------
005220040113C1716D* Parametro modulo IVA
005230040113  "  D ANP045DS      E DS
005240040113  "  D*-------------
005250040113  "  D* DS gestione parametri modulo
005260040113  "  D XPAOUT          DS          2000
005270040113  "  D*-------------
005280040113  "  D* DS Per driver controllo dich. intento
005290040113  "  D PJXX011DS     E DS                  inz
005300040115  "  D $inZx11         S              1A
005310040113C1716D*-------------
005320040607D1617D* DS per controllo ratei
005330040607  "  D NDC326DS      E DS                  INZ
005340040607D1617D*-------------
005341180105R275 D*-------------
005342180105  "  D* DS esterna per recuperare regola detrazione IVA a cerdito
005343180105  "  D SDGSI32DS     E DS                  INZ
005344180105  "  D  ERRI32       E                      inz('0')
005345180105  "  D  EXISTI32     E                      inz('0')
005346180105  "  D  FATTI32      E                      inz('0')
005347180105  "  D  NACCI32      E                      inz('0')
005348180105  "  D  DTANNOTI32   E                      inz(Dt0   )
005349180105  "  D  DTLIQI32     E                      inz(Dt0   )
005350180105  "  D*-------------
005351180105  "  D  MsgW1p2        S              7    inz('SDG0199')
005352180105  "  D  MsgW2p2        S              7    inz('SDG0200')
005353180105  "  D  MsgW3p2        S              7    inz('SDG0201')
005354180105  "  D  MsgW1p3        S              7    inz('SDG0202')
005355180105  "  D  MsgW2p3        S              7    inz('SDG0203')
005356180105  "  D  MsgW3p3        S              7    inz('       ')
005357180105  "  D  MsgWar1        S              7    inz('       ')
005358180105  "  D  MsgWar2        S              7    inz('       ')
005359180105  "  D  MsgWar3        S              7    inz('       ')
005360180105  "  D SavInded        S                   Like(OP_inded)
005361180105  "  D SavIndedP       S                   Like(OP_indedp)
005362180105  "  D ErrRegDet       S              7    inz('       ')
005363180105R275 D*-------------
005364940201     D DSFMT           DS
005365940506     D  $TASTO               369    369
005370940201     D  NRG                  370    370
005380940201     D  NCL                  371    371
005390990803C1211D*-------------
005400990803C1211D* DS per UserExit
005410950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
005420940201     D*-------------
005430940201     D* posizione cursore
005440940201     D CURSOR          DS
005450940223     D  RIRI                   1      2B 0 INZ
005460940201     D  R$$                    2      2
005470940223     D  COCO                   3      4B 0 INZ
005480940201     D  C$$                    4      4
005490940225     D*-------------
005500940616     D* Costanti
005510950809     D*  campo I/O con attributi normali (HI_CS)
005520950809     D Normale         C                   const(X'32')
005530950809     D*  campo protetto con gli attributi normali (HI_CS)
005540950809     D Forzato         C                   const(X'B2')
005550950809     D*  campo da non visualizzare e proteggere
005560950809     D ND_PR           C                   const(X'A7')
005570950816     D*  campo da proteggere
005580950816     D Protetto        C                   const(X'A0')
005590990427C1176 *  campo I/O con inversione di fondo (RI_CS)
005600990506  "  D Illuminato      C                   const(X'31')
005610990514  "  D $D2ImportA      S                   Like(D2ImportoA)
005620990514  "   *-------------
005630990514  "   *Variabili x chiamata a X0201RpDvs
005640990514C1176D $ErrDiv         S              1  0
005650001009D0714 *-------------
005660001009  "   * Variabili per chiamata a X63CtrChar
005670001009  "  D X63Codice       S             15A
005680001009  "  D X63Len          S              2S 0
005690001009  "  D X63Tipo         S              1A
005700001009  "  D X63Blank        S              1A
005710001009D0714D X63Errore       S              1A
005720020304C1530D*-------------
005730020304  "  D* schiera contenente le chiavi ProJ per il documento su Laguna
005740020304  "  D* al momento dell'APERTURA della registrazione, cioè prima di
005750020304  "  D* ogni possibile modifica
005760020304  "  D* (servono solo in modifica per sapere se sono state variate);
005770020304  "  D* uso una schiera grande come le 8 chiavi primarie di Laguna che
005780020304  "  D* sono diverse per ogni tipo registrazione. Elenco:
005790020304  "  D*  Doc.IVA Fornitore : (F)attura o (N)ota/estremi doc./sottoconto
005800020304  "  D*  Doc.IVA Cliente   : estremi doc.
005810020304C1530D KeyLgn          S             15A   dim(8) export
005820030210D1270D DesLgn          S             44A   dim(10) export
005830940225     D*-------------
005840940506     D* Tasti di funzione
005850940506     D F01             C                   CONST(X'31')
005860940506     D F02             C                   CONST(X'32')
005870940506     D F03             C                   CONST(X'33')
005880940506     D F04             C                   CONST(X'34')
005890940506     D F05             C                   CONST(X'35')
005900940506     D F06             C                   CONST(X'36')
005910940506     D F07             C                   CONST(X'37')
005920940506     D F08             C                   CONST(X'38')
005930940506     D F09             C                   CONST(X'39')
005940940506     D F10             C                   CONST(X'3A')
005950940506     D F11             C                   CONST(X'3B')
005960940506     D F12             C                   CONST(X'3C')
005970940506     D F13             C                   CONST(X'B1')
005980940506     D F14             C                   CONST(X'B2')
005990940506     D F15             C                   CONST(X'B3')
006000940506     D F16             C                   CONST(X'B4')
006010950721     D F17             C                   CONST(X'B5')
006020940506     D F18             C                   CONST(X'B6')
006030940506     D F19             C                   CONST(X'B7')
006040940506     D F20             C                   CONST(X'B8')
006050940506     D F21             C                   CONST(X'B9')
006060940506     D F22             C                   CONST(X'BA')
006070940506     D F23             C                   CONST(X'BB')
006080940506     D F24             C                   CONST(X'BC')
006090940506     D ENTER           C                   CONST(X'F1')
006100940506     D ROLDWN          C                   CONST(X'F4')
006110940506     D ROLLUP          C                   CONST(X'F5')
006120950606     D*-------------
006130950606     D* Sottotitolo di testata
006140950616     D SOTTIT          C                   CONST(' ')
006150950606     D*-------------
006160950606     D* Variabili per gestione videate
006170950606     D $Gest           S              2A
006180950606     D $Fine           S              1A
006190950606     D $InzD1          S              1A
006200950719     D $InzD2          S              1A
006210950724     D $InzS1          S              1A
006220950606     D $LastG          S              2A
006230950606     D $LastV          S              2A
006240980119      * Variabile indicatore avvenuta almeno una volta emissione msg errore
006250980119      * (per emetterlo solo una volta e non essere vincolante nel procedere)
006260980119     D $C1InMsg        S              1A
006270980119     D $D2InMsg        S              1A
006280950724     D Resto           S              4  0
006290950724     D Risul           S              4  0
006300950724     D S1Nrr           S                   like(C1Rcd)
006310950724     D* nr.di righe del sfl (SFLPAG)
006320021002     D SFLPAG          C                   CONST(7)
006330950606     D*-------------
006340950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
006350950606     D CurR            S              2S 0
006360950606     D CurC            S              2S 0
006370950606     D Win             S             99A
006380950606     D*-------------
006390950606     D* Variabili appoggio specifiche della singola anagrafica
006400950627     D SoloCtrl        S              1A   inz(*off)
006410950622     D CiSonoDft       S              1A
006420950816     D TesMod          S              1A
006430950816     D FldMod          S              1A
006440950808     D NonAutoriz      S              1A
006450950824     D EffPres         S              1A
006460950914     D Oggi            S             10A
006470950803     D flgImporti      S              1
006480950811     D §CDt            S              2  0
006490950809     D §TDt            S                   like(DftTDtDoc)
006500950725     D Tpcam202        S                   like(TpCam004)
006510950725     D KccCpu202       S                   like($kcc)
006520950725     D KscCpu202       S                   like(C1Ksc)
006530950725     D Cambio202       S                   like(c1Cambio)
006540950725     D Divisa202       S                   like(c1Divisa)
006550950801     D TotImpIVA       S                   like(Imposta015)
006560950801     D TotImpIVAD      S                   like(ImpostD015)
006570950914     D ValData         S             10A
006580950811     D §Data           S              6S 0
006590000127C1320D DataISO         S               D
006600950914     D DtRfCam         S             10A
006610950626     D TpSfl           S              1A
006620950914     D WDate           S             10A
006630040429C1547X**ex m001 svdtareg        S             10A
006640950914     D D2DtDocISO      S             10A
006650950914     D D2DtParISO      S             10A
006660010620C1494D D2DtlibISO      S             10A
006670950808     D Causot          S              1     INZ dim(10)
006680950808     D Indiva          S              3S 0
006690950808     D DataAlfa        DS             6
006700950808     D  Giorno                 1      2S 0
006710950808     D  Mese                   3      4S 0
006720950808     D  Anno                   5      6S 0
006730950809     D Data60          S              6S 0
006740981209     D* appoggio emissione richiesta utilizzo modello collegato
006750981209     D $WindowMod      S              1
006760981209     D $WindowKsc      S                   like (D1Ksc)
006770090223C2122X*ex C2086  ---------------
006780090223  "  X*ex C2086  appoggio emissione per avviso di utilizzo conto per il quale
006790090223  "  X*ex C2086  esistono degli ordini o degli impegni in controllo fatture
006800090223  "  X*ex C2086  $MsgCd1         S                    Like(XMSGCd1)
006810090223  "  X*ex C2086  $MsgCd2         S                    Like(XMSGCd2)
006820090223C2122X*ex C2086  ---------------
006830090223C2122D* flag per forzare il ritorno al video D1                         quale
006840090223  "  D Ritorno_D1      S              1A
006850090223C2122A*---------------
006860950809     D*  campi di memorizzazione per sapere se rigenerare le rate
006870950810     D WPagMan         S                   like (RegPagMan )
006880950810     D WTpSogg         S                   like (MovSNatura)
006890950809     D WKsc            S                   like (MovKsc    )
006900950809     D WImportD        S                   like (MovImportD)
006910950809     D WImporto        S                   like (MovImporto)
006920950809     D WDivisa         S                   like (MovDivisa )
006930950809     D WCondPag        S                   like (RegCondPag)
006940950809     D WDtDoc          S              6S 0
006950060619C1937D WNrDoc          S              9S 0
006960060619  "  D WDtDocIso       S               D
006970060620  "  D CtrNrDoc        S              9S 0
006980060620  "  D CtrDtDoc        S               D
006990060620  "  D $MsgDocEs       S              1
007000060620C1937D $EmWdwDoc       S              1
007010950809     D WDtPar          S              6S 0
007020040429C1547X**ex C1494 WDtLib          S              6S 0
007030030711C1676D WDtLib          S              6S 0
007040040601D1617D WDtReg          S              6S 0
007050040607D1617D DataIso4        S               D
007060950809     D WNrPar          S                   like (PaSNrPar  )
007070950809     D WSerPar         S                   like (PaSSerPar )
007080951004     D WKccFact        S                   like (KccFact015)
007090951004     D WKscFact        S                   like (KscFact015)
007100970203     D xnmDag          S             10
007110950914     D WCopia          S              1
007120950922     D Fattura         S              1
007130950922     D Descr44         S                   like (INDDesFil)
007140950922     D DesAgg44        S                   like (INDDesAgg)
007150951219     D ExiPar          S              1
007160960429     D Decimali        S              1  0
007170960429     D ImportoDec      S                   like(ImportD015)
007180960527     D* Salva la registrazione collegata in manutenzione (capita quando
007190960527     D*  la scrittura automatica va in errore)
007200960527     D WNrAsCol        S                   like(RegNrAsCol)
007210970122     D WFLGREG         S                   like(RegfLGREG  )
007220040429D0611X**************************************************************
007230040429  "  X* Gestione priorità emissione video:                         *
007240040429  "  X* l'emissione diretta sul D2 avviene solo se Modifica        *
007250040429  "  X* e se sul D1 non esistono errori, in tutti gli altri casi   *
007260040429  "  X* (Immissione/Copia/Imm.speciale/Mod con errrori in D1)      *
007270040429  "  X* la prima emissione viene fatta sul D1;                     *
007280040429  "  X* la variabile VideoErr era usata per 'dirigere'             *
007290040429  "  X* la prima emissione in modifica, sul D1 (con errori)        *
007300040429  "  X* piuttosto che sul D2;                                      *
007310040429  "  X* (VideoErr valorizzato (in InzD1)solo in Mod/Copia/Imm.Spec.*
007320040429  "  X* ma usato (in InzVar dopo la chiamata a InzD1) solo in Mod. *
007330040429  "  X* per emettere il D1 nel caso vi siano errori)               *
007340040429  "  X* Ora VideoErr è inutile grazie all'uso delle schiere        *
007350040429  "  X* che memorizzano quali indic. d'errori sono accesi          *
007360040429  "  X**************************************************************
007370040429  "  X***Videata in errore durante caricamento dati
007380040429D0611X*VideoErr        S              2
007390960806     D*  campo di passaggio ANDFT
007400960806     D DFTOutput       S           4000
007410971202     D nrrigam         S              5  0
007420971202     D nrriga          S              5  0
007430971202     D nrriga3         S              3  0
007440971202     D DtRifCau        S               D
007450980922     D* appoggio calcolo
007460980922     D W3015           S             30S15
007470001031C1432D* flag per evitare il ctrl quadratura tra totale documento e
007480001031  "  D* imponibile+imposta, qualora siano già stampati i registri IVA
007490001031C1432D NoCtrlQuad      S              1
007500950606     D*  Indici
007510950606     D X               S              3S 0
007520950606     D Y               S              3S 0
007530950619     D*-------------
007540950619     D* Definizioni parametri standard chiamate moduli
007550950619     D Esito           S              1
007560951004     D Refresh         S              1
007570950619     D GesMsg          S              1
007580950619     D LenOut          S              9B 0
007590950619     D StrutturaO      S             10
007600950724     D Lock            S              1
007610950721     D Codoper         S              1
007620950619     D LenIn           S              9B 0
007630950726     D Operazione      S              2
007640950816     D Fun             S              1     dim(100)
007650950816     D Ctr             S              1
007660950824     D Risposta        S              1
007670010112D0808D Fun8Mvc099      S              1A
007680000330C1350D EffNoCol        S              1
007690000330C1350D $EffNoCol       S              1A
007700951010     D*-------------
007710951010     D* Campi/DS importati
007720951011     D £NDREG        E DS                  extname(NDREG00F)    import
007730980922     D £NrDec          s                   like(DVXNrDec)       import
007740980922     D £NrDecDiv       s                   like(DVXNrDec)       import
007750040308     D £RigaM          S                   like(IvaNrRigaM)   Import
007760040308     D £RigaMInd       S                   like(IvaNrRigaM)   Import
007770990803C1211D*-------------
007780990803  "  D* parametro UEREGCTB per sapere se attivare o no le UserExit
007790990804  "  D £ANP053DS     E DS                  Import EXTNAME(ANP053DS)
007800990803  "  D*-------------
007810990803  "  D* parametri chiamata UserExit
007820990916  "  D UEREG           S            300A
007830990803  "  D UEErr           S              1A
007840990803C1211D UEMsgId         S              7A
007850950616     D*-------------
007860950616     D* Definizione DS per recupero msg generati dai moduli
007870950616     D QLenMsg         S              9B 0
007880950616     D QFormat         S              8
007890950616     D QCallMsg        S             10
007900950616     D QCallStack      S              9B 0
007910950616     D QMsgTyp         S             10
007920950616     D QMsgKey         S              4
007930950616     D QWait           S              9B 0
007940950616     D QAction         S             10
007950950616     D QErrCod         DS
007960950616     D  Q1ErrLen                      9B 0 INZ(16)
007970950616     D  Q1LenRet                      9B 0 INZ(16)
007980950616     D  Q1ErrId                       7
007990950616     D QInfo           DS
008000950616     D  Q2BytRet                      9B 0
008010950616     D  Q2BytAva                      9B 0
008020950616     D  Q2Severity                    9B 0
008030950616     D  Q2MsgId                       7
008040950616     D  Q2MsgTyp                      2
008050950616     D  Q2MsgKey                      4
008060950807     D  Q2Reserve                    15
008070950616     D  Q2MsgLen                      9B 0
008080950616     D  Q2MsgLenAv                    9B 0
008090950616     D  Q2MsgTxt                     80
008100980127     D*-------------
008110980127      * Campi di appoggio per controllo partita
008120980128     D $PPAKcc         S                   like(MovKcc  )
008130980128     D $PPAKsc         S                   like(MovKsc  )
008140980128     D $PPADtPar       S                   like(PasDtPar)
008150980128     D $PPANrPAr       S                   like(PasNrPar)
008160980128     D $PPASerPar      S                   like(PasSerPar)
008170950606     D*-------------
008180040429C1176X*** MsgID           s             27a   Dim(7) ctdata perrcd(1)
008190040429D0154X*** ex C1176 MsgID           s             27a   Dim(10) ctdata perrcd(1)
008200040429C1207X*** ex D0154 MsgID           s             27a   Dim(15) ctdata perrcd(1)
008210040429C1251X*** ex D1207 MsgID           s             27a   Dim(17) ctdata perrcd(1)
008220040429C1350X*** ex C1251 MsgID           s             27a   Dim(19) ctdata perrcd(1)
008230040429D0660X*** ex C1350 MsgID           S             27A   Dim(22) CtData PerRcd(1)
008240000912D0660D MsgID           S             27A   Dim(23) CtData PerRcd(1)
008250991011      *-------------
008260040429D0154X*** MsgT            s             70a   Dim(3)
008270040429C1207X*** ex D0154 MsgT            s             70a   Dim(15)
008280040429C1350X*** ex C1207 MsgT            s             70a   Dim(17)
008290040429D0660X*** ex C1350 MsgT            S             70A   Dim(22)
008300000912D0660D MsgT            S             70A   Dim(23)
008310001020D0611 *-------------
008320001020  "   * Ogni schiera contiene indicatori d'errore delle 3 videate
008330001020  "  D                 DS
008340001020  "  D $InD1                   1     99    Dim(99)  Inz('0')
008350001020  "  D  $InD1All               1     99
008360001020  "  D                 DS
008370001020  "  D $InD2                   1     99    Dim(99)  Inz('0')
008380001020  "  D  $InD2All               1     99
008390001020  "  D                 DS
008400001020  "  D $InC1                   1     99    Dim(99)  Inz('0')
008410001020  "  D  $InC1All               1     99
008420001020D0611 *
008430981020     D*-------------
008440981020     D* DS Esterna gestione messaggi
008450981020     D XMSGDS        E DS
008460981020     D  STS          E                     EXTFLD(MSGSTS)
008470981020     D                                     DIM(333)
008480981020     D  JBA          E                     EXTFLD(MSGJBA)
008490981020     D                                     DIM(502)
008500981020     D*-------------
008510040429C1251X*Usare XTxt per 'distribuire' le risposte nella window,
008520040429  "  X*è superfluo perchè vengono già distribuite accanto
008530040429  "  X*ai relativi bottoni  automaticamente da XMsgR
008540040429  "  X*se in XMsgTxt riceve la successione delle possibili risposte,
008550040429  "  X*distanziate di 15 caratteri
008560040429  "  X*** ex D0154 messaggi opzione per XMsgR
008570040429  "  X*** ex D0154 XTxt            DS
008580040429  "  X*** ex D0154   XTxt1                       15
008590040429  "  X*** ex D0154   XTxt2                       15
008600040429  "  X*** ex D0154   XTxt3                       15
008610040429  "  X*** ex D0154   XTxt4                       15
008620040429  "  X*** ex D0154   XTxt5                       15
008630040429  "  X*** ex D0154   XTxt6                       15
008640040429  "  X*** ex D0154   XTxt7                       15
008650040429  "  X*** ex D0154   XTxt8                       15
008660040429C1251X*-------------
008670040429C1129X* messaggi opzione per XMsgR
008680040429  "  X*XTxt            DS
008690040429  "  X** XTxt1                       15
008700040429  "  X** XTxt2                       15
008710040429  "  X** XTxt3                       15
008720040429  "  X** XTxt4                       15
008730040429  "  X** XTxt5                       15
008740040429  "  X** XTxt6                       15
008750040429  "  X** XTxt7                       15
008760040429  "  X** XTxt8                       15
008770040429C1129X*-------------
008780990204     D $PrimoXMsg      S              1A
008790090720D2530D $Inz711         S              1A
008800090721D2530D SavPNot         S                   like(D1TabPNot)
008810990204      *-------------
008820990426C1176 *-------------
008830990423  "   * Parametri passati per editare un campo numerico in un fld alfa
008840990423  "   * Numero da editare
008850990423  "  D $SrcVar         S             30S 0
008860990423  "   * Lunghezza del campo da editare
008870990423  "  D $SrcVarPr       S              9B 0 Inz(%Size($SrcVar))
008880990423  "   * Numero cifre decimali da editare
008890990423  "  D $SrcVarDe       S              9B 0
008900990423  "   * Codice di editazione
008910990423  "  D $EdtCode        S              1
008920990423  "   * Lunghezza campo video
008930990423  "  D $LenFld         S              3S 0
008940990423  "   * Campo editato
008950990423  "  D $RcvrVar        S             50
008960990423  "   * Flag errore
008970990423  "  D $Err            S              1
008980990426  "   *-------------
008990990426  "   * Variabili di appoggio per gestione errore squadratura iva
009000990426  "  D $DiffDiv        S                   Like(TotTesD003)
009010990426  "  D $DiffMc         S                   Like(TotTest003)
009020990426  "  D*-------------
009030990426  "   * Ds contenente i 3 valori da emettere nella window di gestione
009040990426  "   * errore squadratura iva
009050990426  "  D $PxCOS0496      DS                  inz
009060990426  "  D   $P1TotTest                        Like(C1Importo)
009070990426  "  D   $P2TotRIva                        Like(S1Importo)
009080990426  "  D   $P3Diff                           Like(C1Importo)
009090990427  "   *-------------
009100990427  "  D  $$2TotRIva     S                   Like(D2IMPORTO)
009110990426  "   *-------------
009120990506  "  D                 DS           512
009130990506  "  D  P1NDC0520              1     32
009140990506  "  D  Pack05201              1      8P 3
009150990506  "  D  Pack05202              9     16P 2
009160990506  "  D  Pack05203             17     24P 3
009170990506  "  D  Pack05204             25     32P 2
009180990426  "   *-------------
009190990426C1176 *-------------
009200990506C1129 * Ds utile per pgm che cambia le controp.utente se cambia il cont
009210990506  "  D NDC019DS      E DS                  inz
009220990506  "  D  RET019       E                     INZ('0')
009230990506  "  D  OPR019       E                     INZ('0')
009240990506C1129D  ERR019       E                     INZ('0')
009250080916D2369D NDC019DS1       S                   like(NDC019DS)
009260040429C1350X*-------------
009270040429  "  X*asterisco le ex C1341
009280040429  "  X***  Ds utile per controllo eff. non collegati alla reg.
009290040429  "  X*** NDC275DS      E DS                  inz
009300040429  "  X***  RET275       E                     INZ('0')
009310040429  "  X***  OPR275       E                     INZ('0')
009320040429  "  X***  ERR275       E                     INZ('0')
009330040429  "  X***  $DtIso          S               D
009340040429C1350X*
009350000128D0154 *-------------
009360000128  "  D $MsgWarn        S              1
009370000128  "  D SavDtRif        S                   Like(C1DtRif)
009380000128D0154 *-------------
009390990722C1207D $MsgParEs       S              1
009400990722C1207D $Correggi       S              1
009410991012C1251 * Indica se la condizione di pagamento crea effetti
009420991011  "  D $CreaEff        S              1
009430991011  "   * Indica se ho già emesso il formato D2
009440991012C1251D $ExfmtD2        S              1
009450000912D0660 *----------------------------------------------------------------
009460000912  "   * Plist per  XDATR
009470000912  "  D XDtRic          S              1
009480000912  "  D XDtSoc          S              3
009490000912  "  D XDtUni          S              8
009500000912  "  D XDtCtb          S              2
009510000912  "  D XDtKey          S              8
009520000912  "  D XDtAlc          S              1
009530000912  "  D XDtCmt          S              1
009540000912  "  D XDtDat          S               D
009550000912  "  D XDtTip          S              6
009560000912  "  D XDtErr          S              1
009570000912  "  D XDtTst          S              1
009580000912  "   *----------------------------------------------------------------
009590000912  "   * Memorizza l'ultima data plafond per la quale
009600000912  "   * si è emesso la window per avvertire che non > datario
009610000912  "  D SAVMMPlaf       S              2s 0
009620000912  "  D SAVAAPlaf       S              2s 0
009630000912  "   *----------------------------------------------------------------
009640000912  "   * Memorizza se non emettere più la WINdow di avvertimento
009650000912  "   * sulla data Plafond
009660000912  "  D $StopWinP       S              1
009670000919  "   *----------------------------------------------------------------
009680000919  "   * Ds contenente i 2 valori del msg in window x compet.plafond
009690000919  "  D $P1NDC0602      DS
009700000919  "  D  $C1MMPlaf                     2A
009710000919  "  D  $Barra                        1A
009720000919  "  D  $C1AAPlaf                     2A
009730000919  "  D  $Datario                      6A
009740000912D0660 *----------------------------------------------------------------
009750001025C1429 * Ds reperire la descrizione del blocco partita
009760001025  "  DANG0ABDS       E DS                  Inz
009770001025  "   *----------------------------------------------------------------
009780001025  "   * memorizza se la partita ha un bonifico in corso
009790001025  "  D $BonInC         S                   Like(PPSBonInC)
009800001025C1429 *----------------------------------------------------------------
009810001220D0801D $TpDocIva       S                   Like(OPETpDocI)
009820001220D0801 *-------------
009830010109C1456D Errore515       S              1A
009840010821C1456 *-------------
009850020506C1550 * $ExistSli='1'--> regime iva trasp mista con slittamento
009860020506  "   *                  (Ansli00f legato al libro della registrazione)
009870020506  "   * $ExistSli='0'--> regime iva trasp mista senza slittamento
009880020506  "   *                  oppure regime iva normale o trasp. normale
009890020513  "  D $ExistSli       S              1    inz('0')
009900020513  "  D DataISO1        S               D
009910020513C1550D DataISO2        S               D
009920020807D1182 *----------------------------------------------------*
009930020807  "   * Controllo chiamata ricorsiva
009940020807D1182D X48ChPgmDS    E DS                  INZ
009950021008C1585 *----------------------------------------------------*
009960021008  "   * per chiamata modulo ndmvc003
009970021008  "   * Causali operative
009980021008  "  D SAVAnope        s                   like(ANOPE)
009990021008  "  D $Anope        E DS                  extname(ANOPE00F)
010000021008  "  D                                     prefix(OP_:3)
010010021008  "  D $Uso            S              1
010020021008  "  D $GesMsg         S              1
010030021008  "  D $Esito          S              1
010040021008  "  D $Struttura      S             10
010050021008  "  D $Output         S           4000
010060021008  "  D $LenOut         S              9B 0
010070021008  "  D $Refresh        S              1
010080021008C1585 *
010090040205C1722 *----------------------------------------------------*
010100040205  "   * Parametri driver laguna
010110040205  "  D OpzioneDR       S              2A
010120040205  "  D Errore          S              1A
010130040205  "  D Vista           S              2S 0
010140040205  "  D Dati            S           4000A
010150040205  "  D Lunghezza       S              9B 0
010160040205  "  D Commit          S              1A
010170040205  "  D NrChiavi        S              2S 0
010180040205  "  D Alloca          S              1A
010190040205  "   *-------------
010200040205  "   * dati catalogazione Laguna
010210040205  "  D ANLGN00F      E DS
010220040205  "   *-------------
010230040205  "   * ds paramentro modulo 'LAGUNA' importata da ndc004r
010240040205  "  D £ANP062DS     E DS                  extname(ANP062DS) import
010250040205  "   *-------------
010260040205  "   * indica se la data docum è corretta formalmente
010270040205  "  D $D2DtDocOK      s              1a
010280040205C1722 *----------------------------------------------------*
010290040503D1602 * indica se esiste NDPAS della partita a video
010300040430  "  D $EsistePAS      s              1a   inz('0')
010310040503D1602 *----------------------------------------------------*
010320050215C1817 * indica se esiste NDPAS della partita legata alla reg
010330050215  "  D $PasOk          s              1a   inz('0')
010340050215C1817 *----------------------------------------------------*
010350060803D2031 * Parametri gestione home banking eff.passivi
010360060803  "  D ndck08DS      e ds
010370060803  "  D $RepHBEffPas    s              1a   inz('0')
010380061011  "  D $EffRicHB_I     s              1    inz
010390061011  "  D EffRicHB        s              1
010400060803D2031 *----------------------------------------------------*
010410090126C2116D* Parametri del PGM di dettaglio fatture ELENCO DOCUMENTI
010420090126  " >D NDC853DS      E DS                  inz
010430090126  " >D  RET853       E                     INZ('0')
010440090126  " >D  OPR853       E                     INZ('0')
010450090126  " >D  ERR853       E                     INZ('0')
010460090126C2116D  CMT853       E                     INZ('0')
010470090126     D*-------------
010480060619C1937INDMOV19
010490060619C1937I              MOVDTDOC                    MOVDTC
010500940127     C*----------------------------------------------------*
010510940127     C*                MAIN LINE PROGRAM
010520940127     C*----------------------------------------------------*
010530940127     C*
010540940223     C* inizializzazione variabili
010550950719     C                   IF        Inz015 = *on
010560950706     C                   EXSR      InzVar
010570950706     C                   ELSE
010580950706     C                   EXSR      MiniInzVar
010590950706     C                   ENDIF
010600940223     C*
010610940223     C     $FINE         DOWEQ     *OFF
010620940202     C     $GEST         CASEQ     'D1'          GESD1
010630950719     C     $GEST         CASEQ     'D2'          GESD2
010640950724     C     $GEST         CASEQ     'S1'          GESS1
010650941027     C                   ENDCS
010660941027     C                   ENDDO
010670940201     C*
010680940325     C* fine programma
010690940325     C                   EXSR      ENDPGM
010700940325     C************************************************************
010710940325     C* RICERCHE
010720940325     C************************************************************
010730940325     C     ENDPGM        BEGSR
010740970130     c*
010750970130     c* ripasso la causale
010760970130     c                   eval      CausTes015 = D1CausTes
010770040402D1573c* ripasso il codice mod. pnota
010780040402D1573c                   eval      TabPnot015 = D1TabPNot
010790940325     C*
010800960806     C* se non ho trovato dei default
010810960806     C                   IF        CiSonoDft = *off
010820960806     C* pulisco la DS
010830960806     C                   RESET                   ANDFT
010840960806     C                   ENDIF
010850960806     C                   EVAL      %subst(DFTOutPut:1:%size(ANDFT))
010860960806     C                              = ANDFT
010870960806     C*
010880940325     C                   SETON                                            RT
010890940616     C                   RETURN
010900940325     C*
010910940325     C                   ENDSR
010920940201     C************************************************************
010930940201     C* RICERCHE
010940940201     C************************************************************
010950940127     C     SEARCH        BEGSR
010960940127     C*
010970940127     C* determino Riga/Colonna del cursore
010980940127     C                   MOVE      NRG           R$$
010990940127     C                   MOVE      NCL           C$$
011000940127     C                   Z-ADD     RIRI          CURR
011010940127     C                   Z-ADD     COCO          CURC
011020940127     C*
011030940224     C* I campi che al momento dell'F4 risultano protetti non devono
011040940224     C* essere abilitati alla funzione di ricerca
011050941027     C*
011060941027     C                   SELECT
011070950719     C*
011080950719     C* FMTD1 - Tipo soggetto
011090950719     C     H1NMFL        WHENEQ    'D1TPSOGG'
011100950719     C                   RESET                   A964DS
011110950719     C                   MOVEL     '01'          OPZ964
011120980617     C     D1TpSoggA     IfEq      Protetto
011130980617     C                   Movel     '05'          OPZ964
011140980617     C                   EndIf
011150950719     C                   MOVEL     'ANDIZ000'    TRC964
011160950719     C                   MOVEL     'DSPCLIFOR'   CAM964
011170950719     C                   MOVEL     A964DS        KPJBU
011180950719     C                   CALL      'ANA964R'
011190950719     C                   PARM                    KPJBA
011200950719     C                   MOVEL     KPJBU         A964DS
011210950719     C*
011220950809     C     Val964        IFNE      *BLANK
011230950809     C                   MOVEL     Val964        D1TPSOGG
011240950719     C                   ENDIF
011250950616     C*
011260950719     C* FMTD1  - Sottoconto cliente/fornitore
011270950719     C     H1NMFL        WHENEQ    'D1KSC'
011280950719     C                   RESET                   A201DS
011290950719     C                   MOVE      XSCsoc        SOC201
011300950719     C                   MOVEL     D1tpsogg      KCC201
011310950719     C                   MOVE      D1Ksc         KSC201
011320950719     C                   MOVEL     A201DS        KPJBU
011330950719     C                   CALL      'ANA201R'
011340950719     C                   PARM                    KPJBA
011350950719     C                   MOVEL     KPJBU         A201DS
011360950719     C*
011370950719     C     KSC201        IFNE      *BLANK
011380950719     C                   MOVEL     KSC201        D1KSc
011390950719     C                   ENDIF
011400950719     C*
011410950719     C* FMTD1 - Causale principale
011420950719     C     H1NMFL        WHENEQ    'D1CAUSTES'
011430950719     C                   RESET                   ANA070DS
011440950719     C                   MOVE      '01'          OPZ070
011450980617     C     D1CausTesA    IfEq      Protetto
011460980617     C                   Movel     '05'          OPZ070
011470980617     C                   EndIf
011480950720     C                   MOVE      '1'           tpc070
011490950720     C                   MOVE      D1TPSOGG      STP070
011500950720     C                   MOVE      '0'           trg070
011510950719     C                   CLEAR                   KPJBU
011520950719     C                   MOVEL     ANA070DS      KPJBU
011530950719     C                   CALL      'ANA070R'
011540950719     C                   PARM                    KPJBA
011550950719     C                   MOVEL     KPJBU         ANA070DS
011560950719     C     Cau070        IFNE      *BLANK
011570950719     C                   MOVEL     Cau070        D1CausTes
011580950719     C                   ENDIF
011590950719     C*
011600950719     C* FMTD1 - Unità
011610950719     C     H1NMFL        WHENEQ    'D1UNITA'
011620950719     C                   RESET                   ANA009DS
011630950719     C                   MOVE      '01'          OPZ009
011640950719     C                   MOVE      XscSoc        Soc009
011650950719     C                   CLEAR                   KPJBU
011660950719     C                   MOVEL     ANA009DS      KPJBU
011670950719     C                   CALL      'ANA009R'
011680950719     C                   PARM                    KPJBA
011690950719     C                   MOVEL     KPJBU         ANA009DS
011700950719     C     Uni009        IFNE      *BLANK
011710950719     C                   MOVEL     Uni009        D1Unita
011720950719     C                   ENDIF
011730950719     C*
011740950719     C* FMTD1/D2 - Modello primanota
011750950719     C     H1NMFL        WHENEQ    'D1TABPNOT'
011760950719     C                   RESET                   NDC051DS
011770950719     C                   MOVE      '01'          OPZ051
011780950719     C                   MOVE      '1'           TpPNot051
011790950719     C                   CLEAR                   KPJBU
011800950719     C                   MOVEL     NDC051DS      KPJBU
011810950719     C                   CALL      'NDC051R'
011820950719     C                   PARM                    KPJBA
011830950719     C                   MOVEL     KPJBU         NDC051DS
011840950719     C     TabPNot051    IFNE      *BLANK
011850950719     C                   MOVEL     TabPNot051    D1TabPNot
011860950719     C                   ENDIF
011870950719     c*
011880950719     C* FMTD2 - Condizione di pagamento
011890950719     C     H1NMFL        WHENEQ    'D2CONDPAG'
011900950719     C                   RESET                   A024DS
011910950719     C                   MOVEL     '01'          OPZ024
011920980617     C     D2CondPagA    IfEq      Protetto
011930980617     C                   Movel     '05'          OPZ024
011940980617     C                   EndIf
011950950719     C                   MOVEL     A024DS        KPJBU
011960950719     C                   CALL      'ANA024R'
011970950719     C                   PARM                    KPJBA
011980950719     C                   MOVEL     KPJBU         A024DS
011990950719     c*
012000950719     C     Cod024        IFNE      *BLANK
012010950719     C                   MOVEL     Cod024        D2condpag
012020950719     C                   ENDIF
012030950719     C*
012040950719     C* FMTD2 - Classe
012050950719     C     H1NMFL        WHENEQ    'D2CLASSE'
012060950719     C                   RESET                   XTABDS
012070950719     C                   MOVE      '01'          XTAOPZ
012080950719     C                   MOVE      XSCSOC        XTAAZI
012090950719     C                   MOVE      XSCLIN        XTALIN
012100950719     C                   Z-ADD     CURR          XTARIG
012110950719     C                   Z-ADD     CURC          XTACOL
012120950719     C                   CLEAR                   KPJBU
012130950719     C                   MOVEL     XTABDS        KPJBU
012140950719     C                   CALL      'ANGG81R'
012150950719     C                   PARM                    KPJBA
012160950719     C                   MOVEL     KPJBU         XTABDS
012170950719     C*
012180950719     C     XTAERR        IFNE      '1'
012190950719     C     XTAKEY        ANDNE     *BLANK
012200950719     C                   MOVE      XtaKey        D2Classe
012210950719     C                   ENDIF
012220950719     c*
012230950719     C* FMTD2 - Serie Registrazione
012240950719     C     H1NMFL        WHENEQ    'D2SERREG'
012250950616     C                   RESET                   XTABDS
012260950616     C                   MOVE      '01'          XTAOPZ
012270980617     C     D2SerRegA     IfEq      Protetto
012280980617     C                   Move      '05'          XTaOpz
012290980617     C                   EndIf
012300950616     C                   MOVE      XSCSOC        XTAAZI
012310950616     C                   MOVE      XSCLIN        XTALIN
012320950616     C                   Z-ADD     CURR          XTARIG
012330950616     C                   Z-ADD     CURC          XTACOL
012340950616     C                   CLEAR                   KPJBU
012350950616     C                   MOVEL     XTABDS        KPJBU
012360950616     C                   CALL      'ANGG13R'
012370950616     C                   PARM                    KPJBA
012380950616     C                   MOVEL     KPJBU         XTABDS
012390950616     C*
012400950616     C     XTAERR        IFNE      '1'
012410950616     C     XTAKEY        ANDNE     *BLANK
012420950719     C                   MOVE      XtaKey        D2SerReg
012430950616     C                   ENDIF
012440950616     C*
012450950719     C* FMTD2  - Divisa
012460950719     C     H1NMFL        WHENEQ    'D2DIVISA'
012470950616     C                   RESET                   A015DS
012480950720     C                   MOVEL     '01'          OPZ15
012490980617     C     D2DivisaA     IfEq      Protetto
012500980617     C                   Move      '05'          Opz15
012510980617     C                   EndIf
012520950616     C                   MOVEL     A015DS        KPJBU
012530950616     C                   CALL      'ANA015R'
012540950616     C                   PARM                    KPJBA
012550950616     C                   MOVEL     KPJBU         A015DS
012560950616     C     Div015        IFNE      *BLANK
012570950719     C                   MOVEL     Div015        D2Divisa
012580950616     C                   ENDIF
012590950928     C* FMTD2  -
012600950817     C*
012610950928     C*
012620950928     C* partite
012630950928     C     H1NMFL        WHENEQ    'D2DTPAR'
012640950928     C     H1NMFL        oreq      'D2NRPAR'
012650950928     C     H1NMFL        oreq      'D2SERPAR'
012660020807D1182 *
012670020807  "   * ctr chiamata ricorsiva
012680020807  "  C                   reset                   X48ChPgmds
012690020807  "  C                   eval      X48Pgm      = 'NDC085R'
012700020807  "  C                   eval      Kpjbu       = X48ChPgmDS
012710020807  "  C                   call      'X48CHPGM'
012720020807  "  C                   parm                    Kpjba
012730020807  "  C                   movel     Kpjbu         x48ChPgmDS
012740020807D1182C                   if        x48errore   = *Off
012750950928     C*
012760960123     C                   RESET                   NDC085DS
012770960123     C                   MOVEL     '01'          OPZ085
012780980617     C     D2DtParA      IfEq      Protetto
012790980617     C     D2NrParA      OrEq      Protetto
012800980617     C     D2SerParA     OrEq      Protetto
012810980617     C                   Move      '05'          Opz085
012820980617     C                   EndIf
012830960123     C                   MOVEl     D1TpSogg      KCC085
012840960123     C                   MOVEl     D1Ksc         KSC085
012850960123     C                   MOVEl     Ctb015        Ctb085
012860960123     C                   MOVEl     Unita015      Unita085
012870960123     C                   eval      modalit085 =  '1'
012880960123     c                   eval      movdef085 = *on
012890960123     c                   eval      movprov085 = *on
012900960123     c                   eval      movges085  = *on
012910960123     c                   eval      ord085     = 'D'
012920960123     c                   eval      divvis085  = '1'
012930960123     C                   eval      divsel085  = 'O'
012940960123     C                   eval      sitpar085  = '0'
012950960123     C                   eval      estmul085  = *off
012960960123     C                   eval      ripsald085 = *off
012970960123     C                   MOVEL     NDC085DS      KPJBU
012980950928     C*
012990960123     C                   CALL      'NDC085R'
013000950928     C                   PARM                    KPJBA
013010960123     C                   MOVEL     KPJBU         NDC085DS
013020960123     C     NrPar085      IfNe      *zero
013030960123     C                   MOVEL     NrPar085      D2NrPar
013040960123     C                   MOVEL     SerPar085     D2SerPAr
013050960123     C                   if        DtPar085 > dt0
013060950928     C                   CALLb     'XDT4000'
013070960123     C                   PARM      DtPar085      XDTAMG           10
013080950928     C     D2DtPar       PARM                    XDTGMA            6 0
013090950928     C                   PARM      2             XDTSTA            1 0
013100950928     C                   Else
013110950928     C                   MOVEL     *zero         D2DtPar
013120950928     C                   EndIf
013130010821C1512C                   eval      D2DivisaP = *Blank
013140010821  "  C                   eval      D2BkAmmD  = *Blank
013150010821C1512C                   eval      D2BkAmm   = *Blank
013160950928     C                   EndIf
013170020807D1182C                   endif
013180971211     C*
013190971211     C* FMTC1 - IVA sospesa
013200971211     C     H1NMFL        WHENEQ    'C1SOSP'
013210971211     C                   RESET                   A964DS
013220971211     C                   MOVEL     '01'          OPZ964
013230971211     C                   MOVEL     'NDIVA000'    TRC964
013240971211     C                   MOVEL     'IVASOSP'     CAM964
013250971211     C                   EVAL      All964 = *on
013260971211     C                   MOVEL     A964DS        KPJBU
013270971211     C                   CALL      'ANA964R'
013280971211     C                   PARM                    KPJBA
013290971211     C                   MOVEL     KPJBU         A964DS
013300971211     C*
013310971211     C     Val964        IFNE      *BLANK
013320971211     C                   MOVEL     Val964        C1Sosp
013330971211     C                   ENDIF
013340080125C2072C*
013350080125  "  C* FMTC1 - Trattamento in elenchi
013360080125  "  C     H1NMFL        WHENEQ    'C1ELENCHI'
013370080125  "  C                   RESET                   A964DS
013380080125  "  C                   MOVEL     '01'          OPZ964
013390080125  "  C                   MOVEL     'ANDIZ000'    TRC964
013400080125  "  C                   MOVEL     'DSPELENCHI'  CAM964
013410080125  "  C                   EVAL      All964 = *on
013420080125  "  C                   MOVEL     A964DS        KPJBU
013430080125  "  C                   CALL      'ANA964R'
013440080125  "  C                   PARM                    KPJBA
013450080125  "  C                   MOVEL     KPJBU         A964DS
013460080125  "  C*
013470080125  "  C     Ret964        IFNE      '2'
013480080125  "  C                   MOVEL     Val964        C1Elenchi
013490080125C2072C                   ENDIF
013500950928     C*
013510950817     C* FMTS1  - RIFERIMENTI E ALIQUOTE
013520950817     C     H1NMFL        WHENEQ    'S1RIFIVA'
013530950817     C     H1NMFL        OREQ      'S1ALIQ'
013540950817     C*
013550950817     C     C1NRR         CHAIN     FMTS1                              21
013560950817     C*
013570950817     C                   RESET                   ANA068DS
013580950817     C                   MOVE      '01'          OPZ068
013590980617     C     S1RigaPrA     IfEq      Protetto
013600980617     C                   MOVE      '05'          OPZ068
013610980617     C                   EndIf
013620950817     C                   MOVE      XscSoc        SOC068
013630950817     C                   MOVE      C1TpReg       TPR068
013640950817     C                   MOVE      C1Libro       LIB068
013650950817     C                   MOVE      *ZEROS        ALI068
013660950817     C                   MOVE      *BLANK        RIF068
013670950817     C                   MOVEL     ANA068DS      KPJBU
013680950817     C                   CALL      'ANA068R'
013690950817     C                   PARM                    KPJBA
013700950817     C                   MOVEL     KPJBU         ANA068DS
013710950817     C*
013720950817     C                   IF        Ali068 <> 0        or
013730950817     C                             Rif068 <> *blank
013740950817     C                   MOVE      ALI068        S1Aliq
013750950817     C                   MOVE      RIF068        S1RifIVA
013760950817     C*
013770950817     C                   move      *on           *in32
013780950817     C* imposto gli attributi del sfl
013790950817     C                   EXSR      AtrS1
013800950817     C                   UPDATE    FMTS1
013810950817     C                   move      *off          *in32
013820950817     C*
013830040308     C                   ENDIF
013840960202     C*
013850960202     C* FMTD1 - Causale subfile
013860960202     C     H1NMFL        WHENEQ    'S1CAUSRIG'
013870960202     C*
013880960202     C     C1NRR         CHAIN     FMTS1                              21
013890960202     C                   RESET                   ANA070DS
013900960202     C                   MOVE      '01'          OPZ070
013910980617     C     S1RigaPrA     IfEq      Protetto
013920980617     C                   MOVE      '05'          OPZ070
013930980617     C                   EndIf
013940960202     C                   MOVE      '2'           tpc070
013950960202     C                   CLEAR                   KPJBU
013960960202     C                   MOVEL     ANA070DS      KPJBU
013970960202     C                   CALL      'ANA070R'
013980960202     C                   PARM                    KPJBA
013990960202     C                   MOVEL     KPJBU         ANA070DS
014000960202     C     Cau070        IFNE      *BLANK
014010960202     C                   MOVEL     Cau070        S1CausRig
014020960202     C                   move      *on           *in32
014030960202     C* imposto gli attributi del sfl
014040960202     C                   EXSR      AtrS1
014050960202     C                   UPDATE    FMTS1
014060960202     C                   move      *off          *in32
014070960202     C                   ENDIF
014080940223     C*
014090940223     C* ricerca non abilitata in questa posizione
014100941027     C                   OTHER
014110040429D0611X****************** SETON                                        9899
014120001023D0611 * PRO008-(F1) La funzione di ricerca non è abilitata in questa posizione
014130001023  "  C                   Eval      $InD1(98)   = *On
014140001020  "  C                   Eval      $InD2(98)   = *On
014150001020  "  C                   Eval      $InC1(98)   = *On
014160001020  "  C                   Eval      *In99       = *On
014170001020D0611 *
014180940127     C*
014190941027     C                   ENDSL
014200950612     C*
014210950612     C* imposto pos. del cursore
014220950612     C                   Z-ADD     CURR          H1RIGA
014230950612     C                   Z-ADD     CURC          H1COLO
014240940127     C*
014250940506     C                   MOVE      *HIVAL        $LASTV
014260950612     C*
014270940127     C                   ENDSR
014280940225     C************************************************************
014290940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
014300940225     C************************************************************
014310940225     C     GESPIE        BEGSR
014320940225     C*
014330940225     C                   SELECT
014340940225     C*
014350940506     C     $GEST         WHENEQ    'D1'
014360940506     C     $ULKD1        IFEQ      0
014370940506     C* riempimento opzioni
014380940506     C                   EXSR      F24D1
014390940506     C                   ELSE
014400940506     C                   WRITE     FMTZ1
014410940506     C                   ENDIF
014420940225     C*
014430950719     C     $GEST         WHENEQ    'D2'
014440950719     C     $ULKD2        IFEQ      0
014450950619     C* riempimento opzioni
014460950719     C                   EXSR      F24D2
014470950719     C                   ELSE
014480950719     C                   WRITE     FMTZ2
014490950719     C                   ENDIF
014500950721     C*
014510950724     C     $GEST         WHENEQ    'S1'
014520950724     C     $ULKs1        IFEQ      0
014530950721     C* riempimento opzioni
014540950724     C                   EXSR      F24S1
014550950721     C                   ELSE
014560950721     C                   WRITE     FMTZ3
014570950721     C                   ENDIF
014580940225     C                   ENDSL
014590940225     C*
014600940225     C                   ENDSR
014610960212     C**************************************************************
014620960212     C* Chiamata programma con tasto personalizzato (Video D1 e D2)
014630960212     C**************************************************************
014640960212     C     FPerD1        BEGSR
014650960212     C*
014660960212     C* salvo la DS di ingresso
014670040308     C                   MOVEL     NDC015DS      $SaveDS
014680040308     C*
014690040308     C* passare nei campi appositi della DS STD002DS il valore
014700040308     C* della chiave della VL univoca
014710040308     C*
014720040308     C                   MOVEL     NDC015DS      KPJBU
014730040308     C                   CALL      $PGF1(PosFun)                        22
014740040308     C                   PARM                    KPJBA
014750040308     C                   PARM                    NOMPGM
014760040308     C*
014770040308     C* ripristino la DS di ingresso
014780040308     C                   MOVEL     $saveDS       NDC015DS
014790040308     C*
014800040308     C                   ENDSR
014810940131     C************************************************************
014820940510     C* GESTIONE VIDEO RECORD D1
014830940131     C************************************************************
014840940127     C     GESD1         BEGSR
014850940225     C* inizializzazione videata
014860040308     C     $INZD1        IFEQ      *ON
014870040308     C                   EXSR      INZD1
014880040308     C                   MOVE      *OFF          $INZD1
014890040308     C                   ENDIF
014900040308     C* emissione piede videata se proveniente da altra
014910040308     C* salvataggio valore formato di provenienza
014920040308     C     $LASTV        IFNE      'D1'
014930040308     C                   WRITE     FMTT1
014940040308     C                   EXSR      GESPIE
014950040308     C* se non proviene dalla chiamata ad un altro pgm
014960040308     C     $LASTV        IFNE      *hival
014970040308     C* azzero le variabili di impostazione posizione cursore
014980040308     C                   CLEAR                   H1Riga
014990040308     C                   CLEAR                   H1Colo
015000040308     C                   ENDIF
015010940202     C                   MOVE      $LASTV        $LASTG
015020940202     C                   MOVE      'D1'          $LASTV
015030040308     C                   ENDIF
015040940707     C*
015050940707     C* se esistono errori sulla videata
015060940707     C* emetto la write del formato a indicatori spenti per vedere
015070940707     C* le eventuali decodifiche
015080040308     C     *IN99         IFEQ      *ON
015090940707     C     $GEST         ANDEQ     'D1'
015100940707     C                   MOVEA     *IN           WIN
015110971219     C                   MOVE      *ALL'0'       IN4098           59
015120971219     C                   MOVEA     IN4098        *IN(40)
015130940707     C                   WRITE     FMTD1
015140040429D0611X****************** MOVEA     WIN           *IN
015150001020D0611C                   MOVEA     $InD1(40)     *IN(40)
015160040308     C                   ENDIF
015170940131     C*
015180960212     C*
015190960212     C* inizializzazione indice tasti personalizzati
015200960212     C                   Eval      PosFun = 25
015210960212     C*
015220040308     C                   SELECT
015230940203     C* Annullamento
015240950719     C     OPZ015        WHENEQ    '04'
015250940224     C* Visualizzazione
015260950719     C     OPZ015        OREQ      '05'
015270080125C2015c                   if        VaiRate015 <> *on
015280940203     C                   WRITE     FMTD1
015290940203     C                   EXFMT     PROTECT
015300080125c2015c                   else
015310080125  "  c                   eval      $tasto=ENTER
015320080125C2015c                   endif
015330940224     C* Immissione/Modifica/Copia
015340040308     C                   OTHER
015350070503C2015c* emetto videata se non si chiede di passare direttamente alla
015360070424  "  c* gestione rate
015370070424C2015c                   if        VaiRate015 <> *on
015380040308     C                   EXFMT     FMTD1
015390070503c2015c                   else
015400070503  "  c                   eval      $tasto=ENTER
015410070424C2015c                   endif
015420991012C1251C                   ENDSL
015430990505C1129 * Il prossimo XmsgR deve fare la close + open della window,
015440990505  "   * per 'fissare' come sottofondo, il formato appena emesso.
015450990505C1129C                   Eval      $PrimoXMsg =  *On
015460090720D2530C                   Eval      $Inz711    =  *On
015470040429C1251X****************** ENDSL
015480001020D0611 *
015490001020  "   *Pulisco schiere indicatori errore
015500001020D0611C                   EXSR      Clear$In
015510990204     C*
015520940506     C* controllo abilitazione tasti funzione
015530940506     C*
015540940506     C                   EXSR      CTRKD1
015550940506     C*
015560040308     C     *IN99         IFEQ      *OFF
015570040308     C*
015580040308     C                   SELECT
015590040308     C* ROLLUP
015600040308     C*    $TASTO        WHENEQ    ROLLUP
015610040308     C*                  MOVE      'D2'          $GEST
015620040308     C* F2=Decodifica
015630040308     C     $TASTO        WHENEQ    F02
015640040308     C                   EXSR      F02D1
015650040308     C* F3=Fine
015660040308     C     $TASTO        WHENEQ    F03
015670040308     C                   EXSR      F03D1
015680940131     C* F4=Ricerca
015690940506     C     $TASTO        WHENEQ    F04
015700940127     C                   EXSR      SEARCH
015710940224     C* F5=Refresh
015720940506     C     $TASTO        WHENEQ    F05
015730940309     C                   EXSR      F05D1
015740940223     C* F8=Successivo
015750940506     C     $TASTO        WHENEQ    F08
015760940309     C                   EXSR      F08D1
015770940131     C* F12=Ritorno
015780940506     C     $TASTO        WHENEQ    F12
015790940309     C                   EXSR      F12D1
015800940315     C* F24=Altri tasti
015810940506     C     $TASTO        WHENEQ    F24
015820940315     C                   EXSR      F24D1
015830940131     C*
015840960212     C* Personalizzata
015850960212     C     $PGF1(PosFun) WHENNE    *BLANK
015860960212     C                   EXSR      FperD1
015870960212     C*
015880040308     C                   OTHER
015890040308     C*
015900040308     C* eseguo la sequenza dei controlli per la videata
015910040308     C                   eval      SoloCtrl = *on
015920040308     C                   EXSR      CtrD1
015930040308     C*
015940040308     C* se non vi sono errori passo alla 2a videata
015950040308     C                   IF        *in99 = *off
015960090223C2122C                   if        Ritorno_D1  = *On
015970090223  "  C                   MOVE      *ON           $INZD1
015980090223  "  C                   eval      $GEST = 'D1'
015990090223C2122C                   else
016000040308     C                   MOVE      *ON           $INZD2
016010040308     C                   eval      $GEST = 'D2'
016020090223C2122C                   endif
016030070424c2015c*se vi sono errori e si chiede di andare direttamente alla gestio
016040070424  "  C*ne rate, pulisco il flag gestione rate per emettere gli errori
016050070424  "  c                   else
016060070424c2015c                   eval      VaiRate015=*off
016070040308     C                   ENDIF
016080040308     C*
016090040308     C                   ENDSL
016100040308     C*
016110040308     C                   ENDIF
016120941027     C*
016130940117     C                   ENDSR
016140001020D0611 ************************************************************
016150001020  "   * Pulisco schiere indicatori di errore
016160001020  "   ************************************************************
016170001020  "  C     Clear$In      BEGSR
016180001020  "   *
016190001020  "  C                   MoveA     *All'0'       $InD1
016200001020  "  C                   MoveA     *All'0'       $InD2
016210001020  "  C                   MoveA     *All'0'       $InC1
016220001020  "   *
016230001020D0611C     XClear$In     ENDSR
016240950721     C************************************************************
016250950721     C* GESTIONE VIDEO RECORD  D2
016260950721     C************************************************************
016270950721     C     GESD2         BEGSR
016280950721     C* inizializzazione videata
016290950721     C     $INZD2        IFEQ      *ON
016300950721     C                   EXSR      INZD2
016310950721     C                   MOVE      *OFF          $INZD2
016320040308     C                   ENDIF
016330040308     C* emissione piede videata se proveniente da altra
016340040308     C* salvataggio valore formato di provenienza
016350040308     C     $LASTV        IFNE      'D2'
016360040308     C                   WRITE     FMTT1
016370040308     C                   EXSR      GESPIE
016380040308     C* se non proviene dalla chiamata ad un altro pgm
016390040308     C     $LASTV        IFNE      *hival
016400040308     C* azzero le variabili di impostazione posizione cursore
016410040308     C                   CLEAR                   H1Riga
016420040308     C                   CLEAR                   H1Colo
016430040308     C                   ENDIF
016440040308     C                   MOVE      $LASTV        $LASTG
016450040308     C                   MOVE      'D2'          $LASTV
016460040308     C                   ENDIF
016470040308     C*
016480040308     C* se esistono errori sulla videata
016490950721     C* emetto la write del formato a indicatori spenti per vedere
016500950721     C* le eventuali decodifiche
016510040308     C     *IN99         IFEQ      *ON
016520950721     C     $GEST         ANDEQ     'D2'
016530950721     C                   MOVEA     *IN           WIN
016540040429D0200X***                MOVE      *ALL'0'       IN3898           61
016550040429D0200X***                MOVEA     IN3898        *IN(38)
016560040429D0714X***                MOVE      *ALL'0'       IN3798           62
016570040429D0714X***                MOVEA     IN3798        *IN(37)
016580040429C1494X***                MOVE      *ALL'0'       IN3598           64
016590040429C1494X***                MOVEA     IN3598        *IN(35)
016600010621C1494C                   MOVE      *ALL'0'       IN3498           65
016610010621C1494C                   MOVEA     IN3498        *IN(34)
016620950721     C                   WRITE     FMTD2
016630040429D0611X****************** MOVEA     WIN           *IN
016640040429C1494X****               MOVEA     $InD2(35)     *IN(35)
016650010621C1494C                   MOVEA     $InD2(34)     *IN(34)
016660040308     C                   ENDIF
016670950721     C
016680001023D0611 *PRO0013-Esistono errori su altri schermi
016690001020  "  C                   If        $InD2All = *all'0'  AND
016700001020  "  C                             $InD1All <>*All'0'
016710001020  "  C                   SetOn                                        9699
016720001020  "  C                   EndIf
016730001020D0611 *
016740960212     C*
016750960212     C* inizializzazione indice tasti personalizzati
016760960212     C                   Eval      PosFun = 25
016770960212     C*
016780040308     C                   SELECT
016790950721     C* Annullamento
016800950721     C     OPZ015        WHENEQ    '04'
016810950721     C* Visualizzazione
016820950721     C     OPZ015        OREQ      '05'
016830080125C2015c                   if        VaiRate015 <> *on
016840950721     C                   WRITE     FMTD2
016850950721     C                   EXFMT     PROTECT
016860080125c2015c                   else
016870080125  "  c                   eval      $tasto=ENTER
016880080125C2015C                   endif
016890950721     C* Immissione/Modifica/Copia
016900040308     C                   OTHER
016910070503C2015c* emetto videata se NON si chiede di passare direttamente alla
016920070424  "  c* gestione rate
016930070424C2015c                   if        VaiRate015 <> *on
016940950721     C                   EXFMT     FMTD2
016950070503c2015c                   else
016960070503  "  c                   eval      $tasto=ENTER
016970070424C2015C                   endif
016980991012C1251C                   ENDSL
016990990505C1129 * Il prossimo XmsgR deve fare la close + open della window,
017000990505  "   * per 'fissare' come sottofondo, il formato appena emesso.
017010990505C1129C                   Eval      $PrimoXMsg =  *On
017020090720D2530C                   Eval      $Inz711    =  *On
017030040429C1251X****************** ENDSL
017040991012  "   * Segnalo che ho emesso la videata D2
017050991012  "   * (così nel CTRD2 so se emettere o no le window di avvertimento)
017060991012C1251C                   Eval      $ExfmtD2   =  *On
017070001020D0611 *
017080001020  "   *Pulisco schiere indicatori errore
017090001020D0611C                   EXSR      Clear$In
017100950721     C*
017110950721     C* controllo abilitazione tasti funzione
017120950721     C                   EXSR      CTRKD2
017130950721     C
017140040308     C     *IN99         IFEQ      *OFF
017150950721     C
017160040308     C                   SELECT
017170950721     C* ROLDOWN
017180950721     C*    $TASTO        WHENEQ    ROLDWN
017190950721     C*                  MOVE      'D1'          $GEST
017200950724     C* F2=decodifica
017210950724     C     $TASTO        WHENEQ    F02
017220950724     C                   EXSR      F02D2
017230950721     C* F3=Fine
017240950721     C     $TASTO        WHENEQ    F03
017250950721     C                   EXSR      F03D1
017260950721     C* F4=Ricerca
017270950721     C     $TASTO        WHENEQ    F04
017280950721     C                   EXSR      SEARCH
017290950721     C* F5=Refresh
017300950721     C     $TASTO        WHENEQ    F05
017310950721     C                   EXSR      F05D2
017320950721     C* F8=Successivo
017330950721     C     $TASTO        WHENEQ    F08
017340950721     C                   EXSR      F08D1
017350950721     C* F12=Ritorno
017360950721     C     $TASTO        WHENEQ    F12
017370950721     C                   MOVE      'D1'          $GEST
017380951025     C* F15=Ritorno
017390951025     C     $TASTO        WHENEQ    F15
017400951025     C                   EXSR      F15D2
017410090126C2116C* F16=elenco documenti
017420090126  "  C     $TASTO        WHENEQ    F16
017430090126C2116C                   EXSR      F16D2
017440950721     C* F24=Altri tasti
017450950721     C     $TASTO        WHENEQ    F24
017460950721     C                   EXSR      F24D2
017470960212     C* Personalizzata
017480960212     C     $PGF2(PosFun) WHENNE    *BLANK
017490960212     C                   EXSR      FperD1
017500960212     C*
017510040308     C                   OTHER
017520950725     C* Controlli (no solo controlli perchè se tutto OK scrive NDMOV)
017530950724     C                   eval      SoloCtrl = *off
017540950721     C                   EXSR      CTRD2
017550990803C1211C*
017560990806  "  C* richiamo UserExit se abilitata e se non ci sono stati già errori
017570990806  "  C                   IF        IVA053 = *on and *in99 = *off
017580991008  "  C* e l'opzione NON è l'annullamento
017590991008  "  C                             and Opz015 <> '04'
017600990803  "  C                   EXSR      CallUsrExt
017610990803C1211C                   ENDIF
017620040429C1350X*********************************************************
017630040429  "  X*      (asterisco sigle ex C1341                        *
017640040429  "  X*      CtrEffNoCo sostituita da WinEffNoCo:             *
017650040429  "  X*      CtrEffNoCo chiamava NDC275R che con un SQL       *
017660040429  "  X*      controllava l'esistenza di effetti collegati     *
017670040429  "  X*      alla partita ma non alla fattura, e se ciò       *
017680040429  "  X*      si verificava emetteva window di avvertimento    *
017690040429  "  X*      WinEffNoCo emette solo al window di avvertimento *
017700040429  "  X*      (il controllo dell'esistenza di eff. colleagti   *
017710040429  "  X*      alla partita ma non alla fattura, è già fatto    *
017720040429  "  X*      da NDMVC403)                                     *
017730040429  "  X*********************************************************
017740040429  "  X*** controllo se ci sono effetti collegati
017750040429  "  X*** alla partita ma non alla registrazione
017760040429  "  X***                if        *in99 = *Off
017770040429  "  X***                EXSR      CtrEffNoCo
017780040429C1350X***                endif
017790950721     C*
017800950727     C* se non vi sono errori passo alla 3a videata
017810960202     C* e se non premuto F6 per cancellazione ed è richiesta  IVA
017820040308     C                   IF        *in99 = *off and
017830960202     C                             $Tasto <> F06 and OpeObbIva = *off
017840950724     C                   MOVE      *ON           $INZs1
017850950724     C                   eval      $GEST = 'S1'
017860950721     C                   endif
017870070503c2015c* se chiesto di passare direttamente alla gestione rate
017880070503  "  c                   IF        VaiRate015=*on
017890070503  "  c*se vi sono errori pulisco il flag di passare alle rate
017900070503  "  C*per emettere gli errori
017910070503  "  c                   if        *in99=*on
017920070424  "  c                   eval      VaiRate015=*off
017930070424  "  c                   else
017940070424  "  c* altrimenti simulo F6 per andare avanti
017950070424  "  c                   eval      $tasto=F06
017960070503  "  c                   endif
017970070424c2015c                   endif
017980950727     C*
017990960202     C* Se premuto F6 vado a fine e non vi sono errori oppure è non IVA
018000040308     C                   IF        ($tasto = F06  and *IN99 = *off) or
018010960202     C                              (OpeObbIva = *on and *in99 = *Off)
018020950727     C                   exsr      Gesagg
018030950727     C                   ENDIF
018040950721     C
018050040308     C                   ENDSL
018060950721     C
018070040308     C                   ENDIF
018080950721     C
018090950721     C                   ENDSR
018100950721     C************************************************************
018110950724     C* GESTIONE VIDEO RECORD  S1
018120950721     C************************************************************
018130950724     C     GESS1         BEGSR
018140950721     C* inizializzazione videata
018150950724     C     $INZS1        IFEQ      *ON
018160950724     C                   EXSR      INZS1
018170950724     C                   MOVE      *OFF          $INZS1
018180040308     C                   ENDIF
018190950721     C* emissione piede videata se proveniente da altra
018200040308     C* salvataggio valore formato di provenienza
018210040308     C     $LASTV        IFNE      'S1'
018220950721     C                   WRITE     FMTT1
018230950721     C                   EXSR      GESPIE
018240950904     C* se non proviene dalla chiamata ad un altro pgm
018250040308     C     $LASTV        IFNE      *hival
018260950904     C* azzero le variabili di impostazione posizione cursore
018270950904     C                   CLEAR                   H1Riga
018280950904     C                   CLEAR                   H1Colo
018290040308     C                   ENDIF
018300950721     C                   MOVE      $LASTV        $LASTG
018310950724     C                   MOVE      'S1'          $LASTV
018320040308     C                   ENDIF
018330021001C1585 *
018340021001  "   * mantengo la situazione 'aperta' o 'chiusa' del sfl drop
018350021008C1585C                   eval      *In09 = C1Mode
018360950721     C*
018370950721     C* se esistono errori sulla videata
018380950721     C* emetto la write del formato a indicatori spenti per vedere
018390950721     C* le eventuali decodifiche
018400040308     C     *IN99         IFEQ      *ON
018410950724     C     $GEST         ANDEQ     'S1'
018420950721     C                   MOVEA     *IN           WIN
018430950721     C                   MOVE      *ALL'0'       IN4098           59
018440950721     C                   MOVEA     IN4098        *IN(40)
018450950724     C                   WRITE     FMTC1
018460040429D0611X****************** MOVEA     WIN           *IN
018470001020D0611C                   MOVEA     $InC1(40)     *IN(40)
018480040308     C                   ENDIF
018490950721     C
018500001023D0611 *PRO0013-Esistono errori su altri schermi
018510001020  "  C                   If        $InC1All = *All'0'  AND
018520001020  "  C                             ($InD1All <> *All'0'  OR
018530001020  "  C                              $InD2All <> *All'0')
018540001020  "  C                   SetOn                                        9699
018550001020  "  C                   EndIf
018560001020D0611 *
018570960212     C*
018580960212     C* Inizializzazione indice tasti funzione personalizzati
018590960212     C                   Eval      PosFun = 25
018600960212     C*
018610070503C2015c* Emetto videata se NON si chiede di passare direttamente alla
018620070424  "  c* gestione rate
018630070424C2015c                   if        VaiRate015 <> *on
018640950724     C                   EXFMT     FMTC1
018650070424c2015c                   endif
018660950724     C                   Z-ADD     1             C1RCD
018670990505      *
018680990505C1129 * Il prossimo XmsgR deve fare la close + open della window,
018690990505  "   * per 'fissare' come sottofondo, il formato appena emesso.
018700990505C1129C                   Eval      $PrimoXMsg =  *On
018710040115C1716C                   Eval      $InzX11    =  *On
018720990505     C*
018730001020D0611 *Pulisco schiere indicatori errore
018740001020D0611C                   EXSR      Clear$In
018750950721     C*
018760950721     C* controllo abilitazione tasti funzione
018770950724     C                   EXSR      CTRKS1
018780950721     C
018790040308     C     *IN99         IFEQ      *OFF
018800950721     C
018810040308     C                   SELECT
018820950721     C* ROLDOWN
018830950721     C*    $TASTO        WHENEQ    ROLDWN
018840950721     C*                  MOVE      'D1'          $GEST
018850950904     C* F2=Decodifica
018860950904     C     $TASTO        WHENEQ    F02
018870950904     C                   EXSR      F02S1
018880950721     C* F3=Fine
018890950721     C     $TASTO        WHENEQ    F03
018900950721     C                   EXSR      F03D1
018910950721     C* F4=Ricerca
018920950721     C     $TASTO        WHENEQ    F04
018930950721     C                   EXSR      SEARCH
018940950721     C* F5=Refresh
018950950721     C     $TASTO        WHENEQ    F05
018960950724     C                   EXSR      F05S1
018970950721     C* F8=Successivo
018980950721     C     $TASTO        WHENEQ    F08
018990950721     C                   EXSR      F08D1
019000950721     C* F12=Ritorno
019010950721     C     $TASTO        WHENEQ    F12
019020950721     C                   MOVE      'D2'          $GEST
019030001228C1456C* F20=Ripartizione iva indeducibile
019040001228  "  C     $TASTO        WHENEQ    F20
019050001228C1456C                   EXSR      F20S1
019060950721     C* F24=Altri tasti
019070950721     C     $TASTO        WHENEQ    F24
019080950724     C                   EXSR      F24S1
019090960212     C* Personalizzata
019100960212     C     $PGF3(PosFun) Whenne    *BLANK
019110960212     C                   EXSR      FperS1
019120960212     C*
019130040309     C                   OTHER
019140950904     C* no solo controlli perchè se tutto OK scrive NDIVA
019150950904     C                   eval      SoloCtrl = *off
019160950721     C* Controlli
019170950727     C* CONTROLLO DATI(escluso visualizzazione e annullamento)
019180950727     C                   IF        Opz015 <> '05' and
019190950727     C                             Opz015 <> '04'
019200950724     C                   EXSR      CTRC1
019210950724     C     *IN99         IFEQ      *OFF
019220950724     C                   EXSR      CTRS1
019230950724     C                   ENDIF
019240950727     C*
019250950727     c* Modulo  di controllo quadratura
019260040308     C     *IN99         IFEQ      *OFF
019270950727     C                   EXSR      CALLMVC202
019280990426C1176 *
019290001020C1176 * Errore passato da NDMVC202: "Errata quadratura Iva"
019300040429D0611X**ex C1176******** If        *In66 = *On
019310001020D0611C                   If        $InC1(66)   = *On
019320001020C1176C                   EXSR      IvaSquadra
019330990426  "  C                   EndIf
019340990426C1176 *
019350950727     C                   ENDIF
019360950724     C                   ENDIF
019370950727     C* Annullamento
019380950727     C* e premuto F6
019390950727     C                   IF        Opz015 = '04'  and
019400950727     C                             $TASTO = F06
019410950727     C* richiamo il modulo che cancella la testata
019420950727     C                   EXSR      CallCancel
019430950727     C                   ENDIF
019440950721     C*
019450950808     C* se non ci sono errori
019460040308     C                   IF        *in99 = *off
019470950808     C* per tutte le opzioni tranne l'annullamento
019480950808     C                   IF        Opz015 <> '04' or
019490950808     C* oppure anche per l'annullamento se è stato confermato
019500950808     C                             Opz015 = '04' and $Tasto = F06
019510950808     C* eseguo operazioni del dopo-aggiornamento
019520950721     C                   EXSR      GESAGG
019530040308     C                   ENDIF
019540040308     C                   ENDIF
019550040308     C
019560040308     C                   ENDSL
019570040308     C
019580040308     C                   ENDIF
019590040308     C
019600040308     C                   ENDSR
019610040308     C************************************************************
019620040308     C* Chiamata programma con tasto personalizzato
019630960212     C************************************************************
019640960212     C     FPerS1        BEGSR
019650960212     C*
019660960212     C* salvo la DS di ingresso
019670040308     C                   MOVEL     NDC015DS      $SaveDS
019680040308     C*
019690040308     C                   MOVEL     NDC015DS      KPJBU
019700040308     C                   CALL      $PGF3(PosFun)                        22
019710040308     C                   PARM                    KPJBA
019720040308     C                   PARM                    NOMPGM
019730040308     C*
019740040308     C* ripristino la DS di ingresso
019750040308     C                   MOVEL     $SaveDS       NDC015DS
019760040308     C*
019770040308     C                   ENDSR
019780040308     C************************************************************
019790040308     C* Modulo controllo quadratura
019800040308     C************************************************************
019810040308     C     CALLMVC202    BEGSR
019820040308     C*
019830040308     C                   SELECT
019840950725     C                   WHEN      SoloCtrl = *on
019850950726     C                   MOVE      '01'          Operazione
019860950725     C                   WHEN      Opz015 = '05'
019870950726     C                   MOVE      '01'          Operazione
019880950725     C                   OTHER
019890950726     C                   MOVE      '02'          Operazione
019900950725     C                   ENDSL
019910950725     C                   EVAL      Divisa202  =  C1DIVISA
019920950725     C                   EVAL      Cambio202  =  C1cambio
019930950725     C                   EVAL      KccCpu202  =  $kcc
019940950725     C                   EVAL      KscCpu202  =  c1ksc
019950950725     C                   IF        CiSonoDft = *on
019960950725     C                   EVAL      Tpcam202   =  dfttipcmb
019970950725     C                   ELSE
019980950725     C                   EVAL      TpCam202  = 'C'
019990950725     C                   endif
020000001026C1432C*
020010001026  "  C                   EVAL      NoCtrlQuad = *off
020020001026  "  C* se i movimenti IVA sono già stati stampati (data registrazione
020030001026  "  C* inferiore o uguale data ultimo giornale IVA relativo)
020040001031  "  C                   IF        %subst(FlgMod003:1:1) = *on
020050001026  "  C* e siamo in modifica
020060001031  "  C                             and Opz015 = '02'
020070001026  "  C* non eseguo il controllo quadratura IVA, altrimenti se ci fosse
020080001026  "  C* un errore bisognerebbe variare il totale documento in quanto
020090001026  "  C* le righe IVA sono bloccate.
020100001026  "  C                   EVAL      NoCtrlQuad = *on
020110001026C1432C                   ENDIF
020120950725     c*
020130950725     C* Richiamo controllo registrazione
020140950725     C                   CALLB     'NDMVC202'
020150040308     C                   PARM                    Operazione
020160040308     C                   PARM                    Divisa202
020170040308     C                   PARM                    XscDiv
020180040308     C                   PARM                    Cambio202
020190040308     C                   PARM                    KccCpu202
020200040308     C                   PARM                    KscCpu202
020210040308     C                   PARM                    TpCam202
020220040308     C                   PARM                    DtRfCam
020230040308     C                   PARM                    IvaInd010
020240950725     C                   PARM      *on           GesMsg
020250950725     C                   PARM                    Esito
020260001026C1432C                   PARM                    NoCtrlQuad
020270950725     C*
020280040308     C                   IF        Esito = *On
020290950725     C                   SETON                                        99
020300950807     c                   clear                   TextMsg
020310950725     C*
020320950725     C* gestisco la loro emissione
020330040308     C                   DOU       Q2BytAva = 0
020340950725     C                   EXSR      RestoreMsg
020350040308     C                   IF        Q2BytAva > 0
020360040308     C                   SELECT
020370001020D0611 * MVC0062-Errata quadratura IVA
020380001020  "   * ma il 66 è associato a:
020390001020D0611 * NDC0520-(F1) Differenza Iva in divisa:&1; in m.c:&2;
020400950725     C                   WHEN      Q2MsgId = 'MVC0062'
020410040429D0611X****************** SETON                                        6699
020420001020D0611C                   Eval      $InC1(66)   = *On
020430001020  "  C                   Eval      *In99       = *On
020440001020D0611 * PRO0291-Causale non compatibile con il conto per il segno scelto
020450950802     C                   WHEN      Q2MsgId = 'PRO0291'
020460040429D0611X****************** SETON                                        6799
020470001023D0611C                   Eval      $InC1(67)   = *On
020480001020  "  C                   Eval      *In99       = *On
020490001020D0611 * MVC0063-Errore nella scrittura dei movimenti IVA
020500980105     C                   WHEN      Q2MsgId = 'MVC0063'
020510040429D0611X****************** SETON                                        7099
020520001020D0611C                   Eval      $InC1(70)   = *On
020530001020  "  C                   Eval      *In99       = *On
020540001020D0611 *
020550950725     C                   other
020560950725     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
020570950725     C                   MOVE      Q2MsgId       IdMsg
020580950725     C                   CALLB     'XRTVM'
020590950725     C                   PARM                    IDMSG            27
020600950725     C                   PARM                    TXTMSG          100
020610950725     C                   PARM                    ERRMSG            1
020620950807     C                   PARM                    *omit
020630950807     C                   PARM                    *omit
020640950807     C                   PARM      Q2msgtxt      Msgdta          100
020650040308     C                   IF        ErrMsg <> *on
020660950905     C                   CAT       TxtMsg:1      TextMsg
020670040429D0611X****************** SETON                                        5099
020680001023D0611 * CPF9898-&1
020690001023  "  C                   Eval      $InC1(50)   = *On
020700001020D0611C                   Eval      *In99       = *On
020710040308     C                   ENDIF
020720040308     C                   endsl
020730040308     C                   ENDIF
020740040308     C                   ENDDO
020750040308     C                   endif
020760040308     C*
020770040308     C                   ENDSR
020780040308     C************************************************************
020790040308     C* INIZIALIZZAZIONE VIDEATA DATI
020800040308     C************************************************************
020810040308     C     INZD1         BEGSR
020820940131     C*
020830940131     C                   CLEAR                   FMTD1
020840950719     C                   CLEAR                   FMTD2
020850040429D0151X****** spostato in InzD2
020860040429  "  X******             MOVE      XSCON         D2ON1
020870040429  "  X******             MOVE      XSCOFF        D2OF1
020880040429D0151X******             MOVE      XSCOFF        D2pagman
020890980119     C                   Eval      $D2InMsg  = *Off
020900950816     C                   MOVE      *on           TesMod
020910950921     C                   MOVE      *on           FldMod
020920960527     c                   CLEAR                   WNrAsCol
020930970122     c                   CLEAR                   WFlgReg
020940971216     C                   RESET                   ND003DS
020950980127      * Azzero le variabili che memorizzano la partita originaria
020960980127      * di una registrazione in modifica
020970980128     C                   Eval      $PPAKcc    = *Blank
020980980128     C                   Eval      $PPAKsc    = *Blank
020990980128     C                   Eval      $PPADtPar  = *Loval
021000980128     C                   Eval      $PPANrPAr  = *Zero
021010980128     C                   Eval      $PPASerPar = *Blank
021020001026C1429 *   Inizializzo: non c'è bonifico in corso per la partita
021030001025  "   *   (quindi non proteggo la partita)
021040001025C1429C                   Eval      $BonInC     = '0'
021050950102     C*
021060951012     C* se non sono in immissione
021070950727     C     OPZ015        IFNE      '01'
021080951012     C* oppure sono in una immissione ma con recupero dati
021090951012     C     ImmExp015     ORNE      '0'
021100950727     C*
021110951010     C* reperisco tutti i dati che servono dagli archivi collegati
021120951010     C                   EXSR      AltriDati
021130950816     C*
021140950921     C* se la contabilità risulta essere 'MD' cioè modello prima nota
021150950921     C* non effettuo il reperimento dello stato della registrazione
021160950921     C                   If        Ctb015 <> 'MD'
021170950816     C                   EXSR      RepBottReg
021180950816     C                   MOVE      TesMod003     TesMod
021190950921     C                   endif
021200950816     C*
021210950921     C* se la contabilità risulta essere 'MD' cioè modello prima nota
021220950921     C* non reperisco i bottoni del 1° movimento
021230950921     C                   Z-ADD     1             NrRigaM
021240950921     C                   If        Ctb015 <> 'MD'
021250950816     C                   EXSR      RepBottMov
021260950816     C                   MOVE      FldMod006     FldMod
021270950921     C                   endif
021280950727     C*
021290950727     C* riempio la videata con i campi del rcd (riempio anche D1 anche
021300950727     C* se non utilizzata in modifica, visualizz., solo per effettuare
021310950921     C* i controlli autorizzazioni sugli stessi campi)
021320950727     C                   EXSR      RieD1
021330050215     C                   EXSR      RieD2
021340950809     C*
021350950809     C* memorizzo quei campi che, se variati, scatenano la generazione rate
021360950810     C                   MOVE      D2PagMan      WPagMan
021370950810     C                   MOVE      D1TpSogg      WTpSogg
021380950809     C                   MOVE      D1Ksc         WKsc
021390950809     C                   Z-ADD     D2ImportD     WImportD
021400950809     C                   Z-ADD     D2Importo     WImporto
021410950809     C                   MOVE      D2Divisa      WDivisa
021420950809     C                   MOVE      D2CondPag     WCondPag
021430950809     C                   MOVE      D2DtDoc       WDtDoc
021440120327D2729X***  *Jobrun       MOVE      D2DtDoc       WDtDocISO  ex C1937
021450120327D2729X***                MOVE      D2NRDoc       WNrDoc      "   "
021460040429C1547X**ex C1494         MOVE      D2DtLib       WDtLib
021470030711C1676C                   MOVE      D2DtLib       WDtLib
021480950809     C                   MOVE      D2DtPar       WDtPar
021490950809     C                   MOVE      D2NrPar       WNrPar
021500950809     C                   MOVE      D2SerPar      WSerPar
021510040601D1617C* memorizzo data di registrazione per controllare ratei
021520040601D1617C                   MOVE      D2DtReg       WDtReg
021530120327D2729C* memorizzo i campi per controllo del documento,
021540120327 "   C* ma SOLO SE NON SONO in immissione, perchè in questo caso
021550120327 "   C* se è PN interattiva sarebbe inutile (perchè sono vuoti)
021560120327 "   C* se è PN batch sarebbe dannoso (perchè poi fallisce il ctr sul doc)
021570120327 "   C                   If        Opz015 <> '01'
021580120327 "   C     *Jobrun       MOVE      D2DtDoc       WDtDocISO
021590120327 "   C                   MOVE      D2NRDoc       WNrDoc
021600120327D2729C                   Endif
021610950727     C*
021620950727     C* eseguo la sequenza dei controlli per la videata
021630950727     C* chiedendo di fare solo i controlli (senza aggiornare il rcd)
021640950727     C                   EVAL      SoloCtrl = *on
021650950727     C                   EXSR      CtrD1
021660020304C1530C*
021670020304  "  C* se Laguna è attivo
021680020304  "  C                   IF        xscLaguna = *on
021690020304  "  C* e siamo in modifica
021700020304  "  C                             and Opz015 = '02'
021710030210C1530C                   EXSR      RieKeyLgn
021720030210D1270C* salvo anche i descrittori
021730030210D1270C                   EXSR      RieDesLgn
021740030210C1530C                   ENDIF
021750020304C1530C*
021760950921     C*
021770960528     C* se non ci sono errori in 1a videata
021780040429D0611X****************** IF        *in99 = *off
021790001020D0611C                   If        $InD1All    = *All'0'
021800960528     C* controllo anche la 2a
021810950921     C                   EVAL      SoloCtrl = *on
021820950921     C                   EXSR      CtrD2
021830040429D0611X**se c'è un errore in 2a videata
021840040429  "  X****************** IF        *in99 = *on
021850040429  "  X**memorizzo di aprire il pgm su questa
021860040429  "  X****************** MOVE      'D2'          VideoErr
021870040429D0611X****************** ENDIF
021880960527     C*
021890960528     C* se c'è un errore in 1a videata
021900950921     C                   ELSE
021910040429D0611X**memorizzo di aprire il pgm su questa
021920040429D0611X****************** MOVE      'D1'          VideoErr
021930950808     C* se ci sono degli errori per mancata autorizzazione
021940001019D0745 *
021950001019  "   * P.S:
021960001019  "   * Se NonAutoriz =*Off   --> ho tutte le autorizazioni
021970001019  "   * (sono passata per CtrD1/CtrlValD1/CtrBASE)
021980001019  "   * Se NonAutoriz =*Blank --> non sono riuscito a controllare
021990001019  "   * nemmeno una autorizz. (ho trovato errori in CtrD1/CtrlValD1
022000001019  "   * quindi non sono passata nemmeno da CtrBASE)
022010001019D0745 *
022020040429C1410X****************** IF        NonAutoriz = *on
022030001019C1410C                   If        NonAutoriz <> *Off
022040001019D0745C                             AND  NonAutoriz <> ' '
022050950808     C* se sono in modifica
022060950808     C                   IF        Opz015 = '02'     or
022070990708D0154C* oppure in passaggio da provvisorio a definitivo
022080990708D0154C                             Opz015 = '22'     or
022090990816C1218C* oppure in passaggio da provvisorio a gestionale
022100990816C1218C                             Opz015 = '23'     or
022110950808     C* oppure in annullamento
022120950808     C                             Opz015 = '04'     or
022130950808     C* oppure in visualizzazione
022140950808     C                             Opz015 = '05'
022150950808     C* esco da pgm avvertendo dell'errore
022160040429C1410X****************** EVAL      Err015 = '2'
022170000821C1410 * e 'comunico' al chiamante il tipo di autorizzazione mancante
022180000821C1410C                   Eval      Err015      = NonAutoriz
022190040308     C                   EXSR      EndPgm
022200040308     C                   ENDIF
022210040308     C                   ENDIF
022220950808     C                   ENDIF
022230950727     C*
022240990707     C* se sono in immissione
022250950727     C                   ELSE
022260950727     C*
022270990707     C* riempio con i default del preambolo
022280950727     C                   EVAL      D1CausTes = CausTes015
022290950727     C                   EVAL      D1TabPNot = TabPNot015
022300950727     C                   EVAL      D1Unita = Unita015
022310950727     C                   EVAL      D1Tpsogg = Clifor015
022320950727     C*
022330950727     C* riempio con i default di tabella (se passata)
022340950727     C                   IF        TabPNot015 > *blank
022350960517     C                   EXSR      RepDatiMod
022360950727     C                   ENDIF
022370940223     C*
022380040308     C                   ENDIF
022390950710     C*
022400950710     C* imposto quegli attributi del video che sono modificabili
022410950710     C* a seconda del richiamo al pgm
022420961216     c                   if        ctb015 <> 'MD'
022430950710     C                   EXSR      ATRD1
022440040308     C                   ENDIF
022450950620     C*
022460040429C1429X****************** ENDSR
022470001025C1429C     XInzD1        ENDSR
022480950721     C************************************************************
022490950721     C* INizializzo video d2 da D1 SE immissione
022500950721     C************************************************************
022510950721     C     INZD2         BEGSR
022520950721     C*
022530990707D0151C*
022540990707  "  C                   MOVE      XSCON         D2ON1
022550991005D0151C                   MOVE      XSCOFF        D2OF1
022560991005D0244C                   IF        D2PagMan = *blank
022570000128D0151C                   If        CiSonoDft = *On
022580990707  "  C                   If        DftPagMan = *On
022590990707  "  C                   MOVE      XscOn         D2PagMan
022600990707  "  C                   Else
022610990707  "  C                   MOVE      XscOff        D2PagMan
022620990707  "  C                   EndIf
022630990707  "  C                   Else
022640990707  "  C                   MOVE      XSCOFF        D2pagman
022650000128D0151C                   EndIf
022660991005D0244C                   ENDIF
022670040308D0151C**
022680950721     C* Inizializzo video D2 da D1
022690950721     C                   eval      d2tpsogg =d1tpsogg
022700950721     C                   eval      d2ksc    =d1ksc
022710950922     C                   eval      d2kscd   =Descr44
022720950721     C                   eval      d2caustes=d1caustes
022730950721     C                   eval      d2caustesd=opedescr
022740980123      *
022750980123     C                   If        OPZ015    =  '01'
022760060330D1966X* Se non c'è il modello di pnota, pulisco i campi che altrimenti
022770060330  "  X* succede che passando da una gestione con modello ad una senza
022780060330  "  X* modello, rimarrebbero sporchi
022790060330  "  X* EX D1935         if        D1TabPnot = *blanks
022800060330  "  X* EX D1959                   and ImmExp015='0'
022810060330  "  X* EX D1935         clear                   D2Classe
022820060330  "  X*     "            clear                   D2SerReg
022830060330  "  X*     "            clear                   D2Divisa
022840060330  "  X*     "            clear                   D2Importo
022850060330D1966X* EX D1935         endif
022860980617     C                   If        D2SerReg  =  *Blank
022870980123     C                   Eval      D2SerReg  =  OpeSer
022880980617     C                   EndIf
022890980123     C                   EndIf
022900980123      *
022910980123      * Se primanota fattura fornitore e gestita numeraz. semiautomat.
022920980123     C                   If        Pgm004015 =  'NDC006R' And
022930980123     C                             SavSerReg <> *Blank
022940980123     C                   EXSR      RepSerSA
022950980123     C                   EndIf
022960980123      *
022970980122      * Imposto descriz. della causale se la causale lo richiede
022980980119     C                   If        OPZ015 <>'05'  And
022990980119     C                             OPZ015 <>'04'
023000040429C1492X****               If        OpeDesMov = '2'    And
023010010529C1492C*
023020010529  "  C                   If        (OpeDesMov = '2'   or
023030010529C1492C                              OpeDesMov = '3'     ) And
023040980119     C                             D2DesBrev = *Blank
023050980119     C                   Eval      D2DesBrev = OpeDesBrev
023060980119     C                   EndIf
023070980119     C                   EndIf
023080950721     C*
023090980107     C     XInzD2        ENDSR
023100980123     C************************************************************
023110980123     C* REPerimento serie memorizzata per gestire Numeraz.SemiAuto
023120980123     C************************************************************
023130980123     C     RepSerSA      BEGSR
023140980123     C*
023150980123      * Serie a video impostata
023160980123     C                   If        D2SerReg  <> *Blank
023170980123      *  reperimento dati serie
023180980123     C                   Eval      Serie     =  D2SerReg
023190980123     C                   EXSR      RepDatG13
023200980123      *
023210980123      * Serie a video = serie precedente (per Numeraz. semiautomatica)
023220980123      * e serie a video è Non automatica
023230980123     C                   If        D2SerReg = SavSerReg  And
023240980123     C                             AUTG13   = 'N'
023250980617      *  Se il N° Reg a video non è valorizzato
023260980617      *  (se sono ritornato nel preambolo con un F12, dalla testata)
023270980617     C                   If        D2NrReg  = 0
023280980123      *  n°reg. =  n°reg. precedente + 1
023290980123     C                   Eval      D2NrReg  = SavNrReg + 1
023300980617     C                   EndIf
023310980123     C                   Endif
023320980123      *
023330980123     C                   Endif
023340980123      *
023350980123     C     XRepSerSA     ENDSR
023360950721     C************************************************************
023370950724     C* INizializzo video
023380950721     C************************************************************
023390950724     C     INZS1         BEGSR
023400950724     C*
023410950724     C* pulizia SFL
023420950724     C                   SETOFF                                         3031
023430950724     C                   WRITE     FMTC1
023440950724     C                   SETON                                          3130
023450001221C1456C*
023460001228  "  C* se immissione/manutenzione di una fattura acquisti l'F20
023470001228  "  C*  (ripartizione iva indeducibile) è abilitato, altrimenti no
023480001221  "  C                   Eval      $UlKS1 = 0
023490001221  "  C                   Eval      x = 20
023500001221  "  C                   If        %SubSt(D2SerReg:1:1) = '1' and
023510001221  "  C                             (Opz015 = '01' or Opz015 = '02')
023520001221  "  C                   Eval      $FC3(x) = '1'
023530001221  "  C                   Else
023540001221  "  C                   Eval      $FC3(x) = '0'
023550001221C1456C                   EndIf
023560950724     C*
023570020327C1550 * Stabilisco se esiste slittamento in regime iva trasp. mista
023580020327  "   * (x gestire la dt liquidazione)
023590020327  "  C                   exsr      CtrSlittam
023600020506  "   *
023610020327  "   * Se esiste lo slittamento in regime iva trasportatori mista,
023620020327  "   * la data liquidazione deve essere impostata in base allo
023630020327  "   * slittamento, è protetta e non subisce alcun controllo
023640020327  "  C                   eval      *in06 = *Off
023650020327  "  C                   if        $ExistSli = '1'
023660020327  "  C                   eval      *in06 = *On
023670020327  "  C                   endif
023680020327C1550 *
023690950724     C* CARICAMENTO SFL
023700950724     C*
023710950724     C                   Z-ADD     0             S1NRR
023720950724     C                   Z-ADD     1             C1RCD
023730021001C1585 *
023740021001  "   * visualizzo il sfl chiuso
023750021008C1585C                   eval      C1Mode = *On
023760950721     C*
023770010228D0860C                   Clear                   C1LibroD
023780020321     C                   Z-ADD     0             C1DtRif
023790020321C1550C                   Z-ADD     0             C1DtAnnot
023800950816     C                   Z-ADD     0             C1AAPlaf
023810950816     C                   Z-ADD     0             C1MMPlaf
023820971216     C                   MOVE      *blank        C1Sosp
023830971216     C                   MOVE      *blank        C1SospD
023840950816     C                   eval      C1tpsogg =d2tpsogg
023850950724     C                   eval      C1ksc    =d2ksc
023860950922     C                   eval      C1kscd   =Descr44
023870950724     C                   eval      C1caustes=d2caustes
023880950724     C                   eval      C1caustesd=opedescr
023890950724     C                   eval      C1dtreg   =d2dtreg
023900950724     C                   eval      C1nrreg   =d2nrreg
023910950724     C                   eval      C1serreg  =d2serreg
023920950724     C                   eval      C1dtdoc   =d2dtdoc
023930010621C1494C                   eval      C1dtlib   =d2dtlib
023940950724     C                   eval      C1nrdoc   =d2nrdoc
023950950724     C                   eval      C1serdoc  =d2serdoc
023960950724     C                   eval      C1dtpar   =d2dtpar
023970950724     C                   eval      C1nrpar   =d2nrpar
023980950724     C                   eval      C1serpar  =d2serpar
023990970627     C                   Z-add     D2Importo     C1Importo
024000970627     C                   Z-add     D2Importd     C1Importd
024010950724     C                   eval      C1divisa  =d2divisa
024020950724     C                   eval      C1cambio  =d2cambio
024030950821     C                   MOVE      D2SerReg      C1Libro
024040950817     C                   eval      C1TpReg   =OpeTpReg
024050980119     C                   If        Opz015    = '01'
024060980119     C                   Eval      C1DesBrev = D2DesBrev
024070980119     C                   Endif
024080080125C2072C                   MOVE      *blank        C1Elenchi
024090080125C2072C                   MOVE      *blank        C1ElenchiD
024100980119     C                   Eval      $C1InMsg  = *Off
024110950724     C*
024120950724     C* definizione indicatori su video per solo visualizzazione
024130950724     C                   SETOFF                                       05
024140950724     C                   SELECT
024150950724     C     OPZ015        WHENeq    '05'
024160950727     C     OPZ015        OREQ      '04'
024170950724     C                   SETON                                        05
024180950724     C                   ENDSL
024190950724     C*
024200950724     C* Caricamento da driver IVA
024210950724     C                   EXSR      RIES1
024220950724     c*
024230950724     C* Caricamento ultime righe o tutte le righe se immissione
024240010109C1456C                   If        S1NRR < SFLPAG
024250950724     C                   exsr      rols1
024260010109C1456C                   EndIf
024270950724     C                   Z-ADD     1             C1RCD
024280950721     C*
024290950721     C                   ENDSR
024300020327C1550 ************************************************************
024310020327  "   * Controlli esistenza slittamento in regime iva trasp mista
024320020327  "   ************************************************************
024330020327  "  C     CtrSlittam    Begsr
024340020327  "   *
024350020327  "  C                   eval      $ExistSli = '0'
024360020327  "   *
024370020327  "  C                   if        RegimeIva = '2'
024380020327  "   *
024390020327  "  C                   if        not %open(ANSLI01L)
024400020327  "  C                   open      ANSLI01L
024410020327  "  C                   endif
024420020327  "   *
024430020327  "  C                   eval      SLISocieta = XScSoc
024440020327  "  C                   eval      SLITpReg = OPETpReg
024450020327  "  C                   move      D2SerReg      SLILibro
024460020327  "  C     K03Sli01      SETLL     ANSli000                               21
024470020327  "  C                   if        *in21 =  *on
024480020327  "  C                   eval      $ExistSli = '1'
024490020327  "  C                   endif
024500020327  "   *
024510020327  "  C                   endif
024520020327  "   *
024530020327C1550C     XCtrSlitta    Endsr
024540950724     C************************************************************
024550950724     C* CARICAMENTO PAGINA vuota
024560950724     C************************************************************
024570950724     C     ROLS1         BEGSR
024580971216     C*
024590950724     C                   SETOFF                                       32
024600971216     C*
024610971216     C                   CLEAR                   FmtS1
024620950726     C*
024630961203     c* se i dati iva sono bloccati proteggo tutto
024640961203     c                   if        %subst(Flgmod003:1:1) = *on
024650961203     C                   EVAL      H1RigaPR = *on
024660961203     c                   endif
024670950726     C* definizione indicatori su video per solo visualizzazione
024680950726     C                   SETOFF                                       05
024690950726     C                   SELECT
024700950726     C     OPZ015        WHENeq    '05'
024710950727     C     OPZ015        OREQ      '04'
024720950726     C                   SETON                                        05
024730950726     C                   ENDSL
024740950724     C*
024750950724     C* Carico righe rimanenti o pagina vuota
024760950724     C     S1NRR         DIV       SFLPAG        RISUL
024770950724     C                   MVR                     RESTO
024780950724     C                   ADD       1             RESTO
024790950724     C*
024800950724     C     RESTO         DO        SFLPAG        RESTO
024810950724     C                   ADD       1             S1NRR
024820961203     C* imposto gli attributi del sfl
024830961203     C                   EXSR      AtrS1
024840950724     C                   WRITE     FMTS1
024850950724     C                   ENDDO
024860950724      *
024870021002C1585 *
024880021002  "   * disattivo il Rollup perchè ho già caricato tutte le righe
024890021002  "   * del sfl (sono solo 7) e così compare 'Fine'
024900021002  "  C                   eval      *in33 = *on
024910021002C1585 *
024920950724     C                   ENDSR
024930950724     C************************************************************
024940950724     C* CONTROLLO TESTATA LISTA
024950950724     C************************************************************
024960950724     C     CTRC1         BEGSR
024970950724     C*
024980950724     C                   MOVE      *OFF          *IN99
024990950811     C                   MOVE      *blank        §CDt
025000950811     C                   MOVE      *blank        §TDt
025010950724     C*
025020040429C1550X******************************************
025030040429  "  X* Le specifiche che seguono, sono state  *
025040040429  "  X* spostate in una routine specifica per  *
025050040429  "  X* controllare la data liquidazione iva   *
025060040429  "  X* (CtrDtif)                              *
025070040429C1550X******************************************
025080020327C1550 *data liquidazione
025090020327  "  C                   EXSR      CtrDtRif
025100020327C1550 *
025110040429C1550X**data riferimento registri
025120040429  "  X**
025130040429  "  X**se la data non è valorizzata
025140040429  "  X**   C1DtRif       IFEQ      0
025150040429  "  X**tento di valorizzarla con i suoi dft
025160040429  "  X**                 IF        CiSonoDft = *on
025170040429  "  X**                 MOVE      *zeros        §CDt
025180040429  "  X**                 MOVE      DftTDtIVA     §TDt
025190040429  "  X**                 EXSR      ValDataDft
025200040429  "  X**                 MOVE      §Data         C1DtRif
025210040429  "  X**                 ENDIF
025220040429  "  X**altrimenti ne controllo la validità formale
025230040429  "  X**                 ELSE
025240040429  "  X**                 CALLB     'XDT4000'
025250040429  "  X**                 PARM                    XDTAMG           10
025260040429  "  X**                 PARM      C1DtRif       XDTGMA            6 0
025270040429  "  X**                 PARM      3             XDTSTA            1 0
025280040429  "  X**   XdtSta        IFLT      0
025290040429  "  X**************     SETON                                        5399
025300040429  "  X**PRO0001-Data errata
025310040429  "  X**                 Eval      $InC1(53)   = *On
025320040429  "  X**                 Eval      *In99       = *On
025330040429  "  X**                 ENDIF
025340040429C1550X**                 ENDIF
025350020327     C*
025360040429D0660X******************************************
025370040429  "  X* Le specifiche che seguono, sono state  *
025380040429  "  X* spostate in una routine specifica per  *
025390040429  "  X* controllare la data competenza palfond *
025400040429D0660X******************************************
025410000912D0660 *data competenza PLAFOND IVA
025420000912  "  C                   EXSR      CtrPlafond
025430000912D0660 *
025440040429D0660X**data competenza e plafond IVA
025450040429  "  X**
025460040429  "  X**se la data non è valorizzata
025470040429  "  X**                 IF        C1MMPlaf = 0 and C1AAPlaf = 0
025480040429  "  X**tento di valorizzarla con i suoi dft
025490040429  "  X****************** IF        CiSonoDft = *on
025500040429  "  X****************** MOVE      *zeros        §CDt
025510040429  "  X****************** MOVE      DftTDtIVAP    §TDt
025520040429  "  X****************** EXSR      ValDataDft
025530040429  "  X**********Ex C1320 MOVE      §Data         DataAlfa
025540040429  "  X*Ex C1320********* If        §Data <> 0
025550040429  "  X*Ex***JobRun*C1320 Move      §Data         DataISO
025560040429  "  X*Ex***DMY****C1320 Move      DataISO       Data60
025570040429  "  X*Ex C1320********* Else
025580040429  "  X*Ex C1320********* Z-Add     0             Data60
025590040429  "  X*Ex C1320********* EndIf
025600040429  "  X*Ex C1320********* Move      Data60        DataAlfa
025610040429  "  X****************** Z-ADD     Mese          C1MMPlaf
025620040429  "  X****************** Z-ADD     Anno          C1AAPlaf
025630040429  "  X****************** ENDIF
025640040429  "  X**altrimenti ne controllo la validità formale
025650040429  "  X****************** ELSE
025660040429  "  X**ctrl se non è stato digitato il mese plafond ma l'anno si
025670040429  "  X****************** IF        C1MMPlaf = 0 and C1AAPlaf > 0   or
025680040429  "  X**oppure se il mese è > 12
025690040429  "  X******************           C1MMPlaf >12
025700040429  "  X****************** SETON                                        5199
025710040429  "  X****************** ENDIF
025720040429D0660X****************** ENDIF
025730980119      *
025740980119      * Imposto descriz. della causale se la causale lo richiede
025750980119     C                   If        OPZ015 <>'05'  And
025760980119     C                             OPZ015 <>'04'
025770040429C1492X****               If        OpeDesMov = '2'    And
025780010529C1492C*
025790010529  "  C                   If        (OpeDesMov = '2'   or
025800010529C1492C                              OpeDesMov = '3'     ) And
025810980119     C                             C1DesBrev = *Blank
025820980119     C                   Eval      C1DesBrev = OpeDesBrev
025830980119     C                   EndIf
025840980119      * Gestione errore:"descrizione movim. obbligatoria"non vincolante
025850980119     C                   If        OpeDesMov = '1'    And
025860980119     C                             C1DesBrev = *Blank And
025870980119     C                             $C1InMsg  = *Off
025880040429D0611X****************** SETON                                        7899
025890001023D0611 * NDC0374-(F1) Descrizione movimento obbligatoria
025900001023  "  C                   Eval      $InC1(78)   = *On
025910001020D0611C                   Eval      *In99       = *On
025920980119     C                   Eval      $C1InMsg  = *On
025930980119     C                   Endif
025940980119     C                   Endif
025950040429C1550X******************************************
025960040429  "  X* Le specifiche che seguono, sono state  *
025970040429  "  X* spostate in una routine specifica per  *
025980040429  "  X* controllare la data liquidazione       *
025990040429  "  X* (CtrtRif)                              *
026000040429  "  X******************************************
026010040429  "  X**
026020040429  "  X**se data liquidazione <> da data registrazione
026030040429  "  X**                 If        C1DtRif <> C1DtReg         and
026040040429  "  X**                           C1DtRif <> 0               and
026050040429  "  X**e siamo in una registrazione acquisti
026060040429  "  X**                           %subst(C1SerReg:1:1) = '1' and
026070040429  "  X**e non è una bolla doganale
026080040429  "  X**                           $TpDocIva <> '2' and
026090040429  "  X**e il regime IVA è quello normale
026100040429  "  X**(mi tocca testare anche blank perchè non è detto che i clienti che
026110040429  "  X** avevano ProJ prima dell'IVA trasportatori siano entrati in anag.
026120040429  "  X** società e abbiano impostato correttamente il flag)
026130040429  "  X**                           ( %subst(XscFgO:9:1) = '0' or
026140040429  "  X**                             %subst(XscFgO:9:1) = *blank )
026150040429  "  X**emetto errore bloccante
026160040429  "  X**ex C1277******** SETON                                        7199
026170040429  "  X**NDC0554-(F1) Data liquidazione IVA diversa da registrazione in fattura di acquisto
026180040429  "  X**                 Eval      $InC1(71)   = *On
026190040429  "  X**                 Eval      *In99       = *On
026200040429  "  X**                 ENDIF
026210040429  "  X**
026220040429  "  X**
026230040429  "  X**se data liquidazione <> da data registrazione emetto msg di
026240040429  "  X**avvertimento
026250040429  "  X**                 If        C1DtRif <> C1DtReg and
026260040429  "  X**                           C1DtRif <> 0       and
026270040429  "  X**                           *In99 <> *On       and
026280040429  "  X**                           ( $MsgWarn = *Off or
026290040429  "  X**                             C1DtRif <> SavDtRif and
026300040429  "  X**                             SavDtRif <> 0 )
026310040429  "  X**se sono chiamato con l'F2  non emetto la windowdi errore
026320040429  "  X**                           AND SoloCtrl = *Off
026330040429  "  X**
026340040429  "  X**salvo la data in modo da emettere il msg nel caso che la
026350040429  "  X**modifichi e sia ancora diversa da quella di registrazione
026360040429  "  X**                 Z-Add     C1DtRif       SavDtRif
026370040429  "  X**emissione msg
026380040429  "  X**                 Exsr      Warning
026390040429  "  X**                 EndIf
026400040429C1550X**
026410950724     C*
026420980107     C     XCtrC1        ENDSR
026430020327C1550 ************************************************************
026440020327  "   * Controlli sulla data liquidazione
026450020327  "   * (spostati da CtrC1 e 'riuniti' in un unica routine)
026460020327  "   ************************************************************
026470020327  "  C     CtrDtRif      Begsr
026480020327  "   *
026490020327  "   * Se esiste lo slittamento in regime iva trasportatori mista,
026500020327  "   * la data liquidazione deve essere impostata in base allo
026510020327  "   * slittamento, è protetta e non subisce alcun controllo
026520020327  "  C                   if        $ExistSli = '0'
026530020327C1550 *
026540020327     C*  data riferimento registri
026550020327     C*
026560020327     C* se la data non è valorizzata
026570020327     C     C1DtRif       IFEQ      0
026580020327     C* tento di valorizzarla con i suoi dft
026590020327     C                   IF        CiSonoDft = *on
026600020327     C                   MOVE      *zeros        §CDt
026610020327     C                   MOVE      DftTDtIVA     §TDt
026620020327     C                   EXSR      ValDataDft
026630020327     C                   MOVE      §Data         C1DtRif
026640020327     C                   ENDIF
026650020327     C* altrimenti ne controllo la validità formale
026660020327     C                   ELSE
026670020327     C                   CALLB     'XDT4000'
026680020327     C                   PARM                    XDTAMG           10
026690020327     C                   PARM      C1DtRif       XDTGMA            6 0
026700020327     C                   PARM      3             XDTSTA            1 0
026710020327     C     XdtSta        IFLT      0
026720040429D0611X****************** SETON                                        5399
026730020327D0611 * PRO0001-Data errata
026740020327  "  C                   Eval      $InC1(53)   = *On
026750020327D0611C                   Eval      *In99       = *On
026760020327     C                   ENDIF
026770020327     C                   ENDIF
026771180105R275 C* Verifico se esistono regole per la detrazione dell'Iva a credito
026772180105  "  C                   Exsr      RepRegDetr
026773180105R275 C*
026780020327C1277C*
026790020327  "  C* se data liquidazione <> da data registrazione
026791180105R275 C* e non ho reperito regole
026800020327  "  C                   If        C1DtRif <> C1DtReg         and
026810020327  "  C                             C1DtRif <> 0               and
026820020327  "  C* e siamo in una registrazione acquisti
026830020327C1277C                             %subst(C1SerReg:1:1) = '1' and
026840020327D0801C* e non è una bolla doganale
026850020327D0801C                             $TpDocIva <> '2' and
026860020327C1277C* e il regime IVA è quello normale
026870020327  "  C* (mi tocca testare anche blank perchè non è detto che i clienti che
026880020327  "  C*  avevano ProJ prima dell'IVA trasportatori siano entrati in anag.
026890020327  "  C*  società e abbiano impostato correttamente il flag)
026900020327  "  C                             ( %subst(XscFgO:9:1) = '0' or
026910040524C1277C                               %subst(XscFgO:9:1) = *blank )
026920040524C1763 * e la legislazione della società non è Francese
026930040524C1763C                              and XScLegisl <> '2'
026931180105R275 C                              and (ExistI32 = *off or
026932180105R275 C                              (ExistI32 = *on and DtliqI32=*loval))
026940020327C1277C* emetto errore bloccante
026950040429D0611X**ex C1277******** SETON                                        7199
026960020327D0611 * NDC0554-(F1) Data liquidazione IVA diversa da registrazione in fattura di acquisto
026970020327  "  C                   Eval      $InC1(71)   = *On
026980020327D0611C                   Eval      *In99       = *On
026990020327C1277C                   ENDIF
027000020327C1277C*
027010020327D0154C*
027020020327  "  C* se data liquidazione <> da data registrazione emetto msg di
027030020327  "  C* avvertimento
027040020327  "  C                   If        C1DtRif <> C1DtReg and
027050020327  "  C                             C1DtRif <> 0       and
027060020327  "  C                             *In99 <> *On       and
027070020327  "  C                             ( $MsgWarn = *Off or
027080020327  "  C                               C1DtRif <> SavDtRif and
027090020327D0154C                               SavDtRif <> 0 )
027100020327D0660 * se sono chiamato con l'F2  non emetto la windowdi errore
027110020327  "  C                             AND SoloCtrl = *Off
027120020327D0660 *
027130020327D0154 * salvo la data in modo da emettere il msg nel caso che la
027140020327  "   * modifichi e sia ancora diversa da quella di registrazione
027150020327  "  C                   Z-Add     C1DtRif       SavDtRif
027160020327  "   * emissione msg
027170020327  "  C                   Exsr      Warning
027180020327  "  C                   EndIf
027190020327D0154C*
027200020327C1550C                   endif
027210020506  "   *
027220020327C1550C     XCtrDtRif     Endsr
027221180105R275 C/EJECT
027222180105  "   *----------------------------------------------------------
027223180105  "   * Reperisco se esistono regole per detrazione iva a credito
027224180105  "   *----------------------------------------------------------
027225180105  "  C     RepRegDetr    Begsr
027226180105  "   *
027227180105  "  C                   Reset                   SdgSI32ds
027228180105  "   * Se non iva evito il reperimento
027229180105  "  C                   If        OpeTpc = '1'
027230180105  "  C                   Eval      FattI32     = *On
027231180105  "   * Imposto se si tratta di Nota
027232180105  "  C                   If        OPETpDocI = '1'   or
027233180105  "  C                             OPETpDocI = '4'   or
027234180105  "  C                             OPETpDocI = '7'
027235180105  "  C                   Eval      NaccI32     = *On
027236180105  "  C                   Eval      FattI32     = *Off
027237180105  "  C                   Endif
027238180105  "  C*
027239180105  "  C                   eval      SocietaI32 = XscSoc
027240180105  "  C                   eval      TpRegI32 = C1TpReg
027241180105  "  C                   eval      LibroI32 = C1Libro
027242180105  "  C                   If        C1DtAnnot > 0
027243180105  "  C     *jobrun       move      C1DtAnnot     DataIso1
027244180105  "  C                   move      DataIso1      DtAnnotI32
027245180105  "  C                   Endif
027246180105  "  C                   If        C1DtDoc > 0
027247180105  "  C     *jobrun       move      C1DtDoc       DataIso1
027248180105  "  C                   move      DataIso1      DtDocI32
027249180105  "  C                   endif
027250180105  "  C*
027251180105  "  C                   Call      'SDGSI32R '
027252180105  "  C                   Parm                    SdgSI32DS
027253180105  "   *
027254180105  "  C                   Endif
027255180105  "  C                   Endsr
027256180105  "  C/EJECT
027257180105  "   *----------------------------------------------------------
027258180105  "   * Gestisco i controlli relativi alle regole per l'iva a credito
027259180105  "   * a livello di riga.
027260180105  "   *----------------------------------------------------------
027261180105  "  C     GesRegDetr    Begsr
027262180105  "   *
027263180105  "  C                   Exsr      RepRegDetr
027264180105  "   *
027265180105  "  C                   If        ExistI32 = *on
027266180105  "  C                   Exsr      CtrCausInd
027267180105  "  C                   Endif
027268180105  "   *
027269180105  "  C                   Endsr
027270180105  "  C/EJECT
027271180105  "  C*----------------------------------------------------------
027272180105  "  C* Controlla causale riga Indetraibile (Avvertimento)
027273180105  "  C*----------------------------------------------------------
027274180105  "  C     CtrCausInd    BEGSR
027275180105  "  C* Se trovata la regola di indetraibilità per detrazione iva
027276180105  "  C                   If        TpRicI32  = '3' and
027277180105  "  C                             Msg2I32 = *on   and
027278180105  "  C                             S1Aliq    <> 0
027279180105  "  C                   If        S1CausRig  <> *blanks
027280180105  "  C* Se causale Blank nessun controllo
027281180105  "  C                   if        SAVInded <> *on  or
027282180105  "  C                             SAVIndedP <> 100
027283180105  "  C                   Eval      ErrRegDet = 'PERIOD3'
027284180105  "  C                   EXSR      GestErrRegD
027285180105  "  C                   Endif
027286180105  "  C                   Else
027287180105  "  C                   Eval      ErrRegDet = 'PERIOD3'
027288180105  "  C                   EXSR      GEstErrRegD
027289180105  "  C                   EndIF
027290180105  "  C                   EndIF
027291180105  "  C*
027292180105  "  C                   ENDSR
027293180105  "  C/EJECT
027294180105  "  C*----------------------------------------------------------
027295180105  "  C* Controlla di destata dopo le righe
027296180105  "  C*----------------------------------------------------------
027297180105  "  C     CtrRegDett    BEGSR
027298180105  "  C*
027299180105  "  C                   IF        TpRicI32 = '2'     and
027300180105  "  C                             Msg1I32  = *On
027301180105  "  C                   Eval      ErrRegDet = 'PERIOD2'
027302180105  "  C                   EXSR      GestErrRegD
027303180105  "  C                   EndIf
027304180105  "  C
027305180105  "  C                   EndSr
027306180105  "  C/EJECT
027307180105  "  C************************************************************
027308180105  "  C* Gestione messaggi per il SUBFILE
027309180105  "  C************************************************************
027310180105  "  C     GestErrRegD   BEGSR
027311180105  "  C*
027312180105  "  C                   SELECT
027313180105  "   * (F1) avvertimento , data documento non coerente con regola di detrazione iva
027314180105  "   * legata al libro.
027315180105  "  C                   WHEN      ErrRegDet = 'PERIOD2'
027316180105  "  C                   Eval      MsgWar1=MsgW1p2
027317180105  "  C                   Eval      MsgWar2=MsgW2p2
027318180105  "  C                   Eval      MsgWar3=MsgW3p2
027319180105  "  C                   Exsr      warningI32
027320180105  "   * (F1) avvertimento , causale  non coerente con regola di detrazione iva
027321180105  "   * legata al libro.
027322180105  "  C                   WHEN      ErrRegDet = 'PERIOD3'
027323180105  "  C                   Eval      MsgWar1=MsgW1p3
027324180105  "  C                   Eval      MsgWar2=MsgW2p3
027325180105  "  C                   Eval      MsgWar3=MsgW3p3
027326180105  "  C                   Exsr      warningI32
027327180105  "   * Sola emissione videata per verifiche
027328180105  "   * legata al libro.
027329180105  "  C*                  WHEN      ErrRegDet = 'RIMANI '
027330180105  "  C*
027331180105  "  C                   ENDSL
027332180105  "  C*
027333180105  "  C                   ENDSR
027334180105  "   ************************************************************
027335180105  "   * Window di avvertimento:
027336180105  "   ************************************************************
027337180105  "  C     WarningI32    BEGSR
027338180105  "   *
027339180105  "   *Gestione emissione window
027340180105  "  C                   Clear                   XMsgDS
027341180105  "   *
027342180105  "  C                   If        MsgWar1 <> *blanks
027343180105  "  C                   Eval      IDMSG = 'SDGMSG    *LIBL     XXXXXXX'
027344180105  "  C                   Move      MsgWar1       IDMSG
027345180105  "  C                   CallB     'XRTVM'
027346180105  "  C                   Parm                    IDMsg
027347180105  "  C                   Parm                    TxtMsg
027348180105  "  C                   Parm                    ErrMsg
027349180105  "  C                   If        ErrMsg     <> *On
027350180105  "  C                   MoveL     TxtMsg        MsgMs1
027351180105  "  C                   Else
027352180105  "  C                   MoveL     *All'?'       MsgMs1
027353180105  "  C                   EndIf
027354180105  "  C                   EndIf
027355180105  "  C*
027356180105  "  C                   If        MsgWar2 <> *blanks
027357180105  "  C                   Move      MsgWar2       IDMSG
027358180105  "  C                   CallB     'XRTVM'
027359180105  "  C                   Parm                    IDMsg
027360180105  "  C                   Parm                    TxtMsg
027361180105  "  C                   Parm                    ErrMsg
027362180105  "  C                   If        ErrMsg     <> *On
027363180105  "  C                   MoveL     TxtMsg        MsgMs2
027364180105  "  C                   Else
027365180105  "  C                   MoveL     *All'?'       MsgMs2
027366180105  "  C                   EndIf
027367180105  "  C                   EndIf
027368180105  "  C*
027369180105  "  C                   If        MsgWar3 <> *blanks
027370180105  "  C                   Move      MsgWar3       IDMSG
027371180105  "  C                   CallB     'XRTVM'
027372180105  "  C                   Parm                    IDMsg
027373180105  "  C                   Parm                    TxtMsg
027374180105  "  C                   Parm                    ErrMsg
027375180105  "  C                   If        ErrMsg     <> *On
027376180105  "  C                   MoveL     TxtMsg        MsgMs3
027377180105  "  C                   Else
027378180105  "  C                   MoveL     *All'?'       MsgMs3
027379180105  "  C                   EndIf
027380180105  "  C                   EndIf
027381180105  "   *
027382180105  "   * Chiamo la gestione della finestra di avvertimento
027383180105  "  C                   Z-ADD     0             MSGMSG
027384180105  "  C                   MOVEL     'N'           MSGRSP
027385180105  "  C                   MOVEL     *Blank        MSGTPR
027386180105  "  C                   MOVEL     'S'           MSGEMV
027387180105  "  C                   Z-ADD     10            MSGRGP
027388180105  "  C                   MOVEL     'N'           MSGLCK
027389180105  "  C                   MOVEL     'N'           MSGCNL
027390180105  "  C                   MOVEL     'N'           MSGVID
027391180105  "  C                   MOVEL     'N'           MSGOPE
027392180105  "  C                   MOVEL     'N'           MSGSTP
027393180105  "  C*
027394180105  "  C                   CALL      'XMSGR'
027395180105  "  C                   PARM                    XMSGDS
027396180105  "  C                   PARM                    XMSGTXT         135
027397180105  "  C*
027398180105  "   *
027399180105R275 C                   ENDSR
027400000912D0660 ************************************************************
027401000912  "   * Controlli sulla data competenza PLAFOND Iva
027402000912  "   ************************************************************
027403000912  "  C     CtrPlafond    BEGSR
027404000912  "   *
027405000912  "   *CONTROLLO FORMALE------------------------------------------
027406000912  "   *
027407000912  "   *Se mese e anno Plafond non sono valorizzati
027408000912  "  C                   If        C1MMPlaf = 0  AND  C1AAPlaf = 0
027409000912  "   *
027410000912  "   *  tento di valorizzarla con i suoi dft se esistono
027411000912  "  C                   If        CiSonoDft   = *On
027412000912  "  C                   Move      *zeros        §CDt
027413000912  "  C                   Move      DftTDtIVAP    §TDt
027414000912  "  C                   EXSR      ValDataDft
027415000912  "  C                   If        §Data <> 0
027416000912  "  C     *JobRun       Move      §Data         DataISO
027417000912  "  C     *DMY          Move      DataISO       Data60
027418000912  "  C                   Else
027420000912  "  C                   Z-Add     0             Data60
027430000912  "  C                   EndIf
027440000912  "  C                   Move      Data60        DataAlfa
027450000912  "  C                   Z-Add     Mese          C1MMPlaf
027460000912  "  C                   Z-Add     Anno          C1AAPlaf
027470000912  "  C                   EndIf
027480000912  "   *
027490000912  "   *  se non esistono i dft
027500000912  "  C                   Else
027510000912  "   *
027520000912  "   *     se non è stato digitato il mese plafond ma l'anno si
027530000912  "   *     oppure se il mese non è corretto formalmente
027540000912  "  C                   If        C1MMPlaf = 0   AND
027550000912  "  C                             C1AAPlaf > 0   OR
027560001023  "  C                             C1MMPlaf >12
027570001023D0660 *
027580040429D0611X**ex D0660 ******* SetOn                                        5199
027590001023D0611 * PRO0048-
027600001023D0660 *        'Il valore indicato per il campo non è valido'
027610001023D0611C                   Eval      $InC1(51)   = *On
027620001020D0611C                   Eval      *In99       = *On
027630001020D0660 *
027640000912  "  C                   EndIf
027650000912  "   *
027660000912  "  C                   EndIf
027670000912  "   *
027680000912  "   *CONTROLLO CON DATARIO--------------------------------------
027690000912  "   *
027700000912  "   *Se la societa gestisce il plafond (fisso o mobile)
027710000912  "  C                   If        %SubSt(XScFgo:3:1) = '1'  OR
027720000912  "  C                             %SubSt(XScFgo:3:1) = '2'
027730000912  "   *
027740000912  "   *   Reperisco data ultima estrazione plafond
027750000912  "   *   (dal datario 'PLESTR')
027760000912  "  C                   Move      '1'           XDtRic
027770000912  "  C                   Move      XScSoc        XDtSoc
027780000912  "  C                   Move      *Blank        XDtUni
027790000912  "  C                   Move      'CG'          XDtCtb
027800000912  "  C                   Move      *Blank        XDtKey
027810000912  "  C                   Move      'PLESTR'      XDtTip
027820000912  "  C                   Move      *Off          XDtCmt
027830000912  "  C                   Move      *Off          XDtAlc
027840000912  "  C                   CallB     'XDATR'       PListXDatR
027850000912  "   *
027860000912  "   *   Datario 'PLESTR' inesistente
027870001023  "  C                   If        XDtErr      = *On
027880001023  "   *     NDC0601-Datario &1 inesistente
027890001020D0660C                   Eval      P1NDC0601   = 'PLESTR'
027900001020D0611C                   Eval      $InC1(72)   = *On
027910001020D0611C                   Eval      *In99       = *On
027920040429D0611X**ex D0660 ********SetOn                                        7299
027930001020D0660C                   EndIf
027940000912  "   *
027950000912  "   *
027960000912  "   *Se il Datario 'PLESTR' esiste       e
027970000912  "   *la dt plafond a video è valorizzata e
027980000912  "   *è corretta formalmente              e
027990000912  "   *la registrazione è Definitiva
028000000912  "   *(anche in NDMVC201 il controllo della dt liquidaz.
028010000912  "   *con il suo datario, viene fatto solo se la reg è Def)
028020000912  "  C                   If        XDtErr     <> *On   AND
028030000912  "  C                             C1MMPlaf   <> 0     AND
028040000912  "  C                             *In99       = *Off  AND
028050000912  "  C                             TpRegZ015   = 'D'
028060000912  "   *
028070000912  "   *   preparo la data plafond completa del giorno=1
028080000912  "   *   per confrontarla con quella del datario
028090000912  "  C                   Z-Add     1             Giorno
028100000912  "  C                   Z-Add     C1MMPlaf      Mese
028110000912  "  C                   Z-Add     C1AAPlaf      Anno
028120000912  "  C                   Move      DataAlfa      Data60
028130000912  "  C     *Dmy          Move      Data60        DataISO
028140000912  "   *
028150000912  "   *
028160000912  "   *  Se Dt Plafond non > ultima estrazione (da datario 'PLESTR')
028170000913  "   *  e se sono chiamato con l'F2 non emetto la window
028180000912  "  C                   If        DataISO    <= XDtDat
028190000913  "  C                             AND SoloCtrl = *Off
028200000912  "   *
028210000912  "   *    avverto l'utente che dt palfond non > ultima estrazione.
028220000912  "  C                   EXSR      WinPlafond
028230000912  "   *
028240000912  "  C                   EndIf
028250000912  "   *
028260000912  "  C                   EndIf
028270000912  "  C                   EndIf
028280000912  "   *
028290000912  "  C     XCtrPlafon    ENDSR
028300000912  "   ************************************************************
028310000912  "   * Window di avvertimento:
028320000912  "   * dt competenza palfond non > ultima estrazione.
028330000912  "   ************************************************************
028340000912  "  C     WinPlafond    BEGSR
028350000912  "   *
028360000912  "   *Emetto la window di avvertimento solo se:
028370000912  "   *
028380000912  "   *$StopWinP <> '1' -> . la window di avvertimento sul plafond
028390000912  "   *                      <= datario, non è mai stata emessa
028400000912  "   *                    . l'ultima risposta dell'utente
028410000912  "   *                      alla suddetta window è stata
028420000912  "   *                      di ritornare sullo stesso video iva
028430000912  "   * Oppure
028440000912  "   * $StopWinP  = '1' -> . la window di avvertimento sul plafond
028450000912  "   *                       <= datario, è stata emessa almeno 1 v.
028460000912  "   *                       e la risposta dell'utente è stata di
028470000912  "   *                       non ritornare sullo stesso video iva
028480000912  "   *  E
028490000912  "   *  SAVMMPlaf<>C1MMPlaf
028500000912  "   *  o SAVAAPlaf<>C1AAPlaf ->la window di avvertimento sul plafond
028510000912  "   *                          <= datario, è stata emessa almeno 1 v.
028520000912  "   *                          per una dt plafond <> da quella
028530000912  "   *                          attuale sul video
028540000912  "   *
028550000912  "  C                   If        ($StopWinP  <> *On         OR
028560000912  "  C                             ($StopWinP   = *On AND
028570000912  "  C                             (SAVMMPlaf  <> C1MMPlaf  OR
028580000912  "  C                              SAVAAPlaf  <> C1AAPlaf)))
028590000912  "   *
028600000912  "   * Memorizzo la data per la quale emetto l'avvertimento
028610000912  "   * (se l'utente nonostante l'avvertimento, sceglie 'Continua'
028620000912  "   *  emetterò il prossimo avvertimento solo se la data
028630000912  "   *  sarà diversa da quest'ultima)
028640000912  "  C                   Eval      SAVMMPlaf   = C1MMPlaf
028650000912  "  C                   Eval      SAVAAPlaf   = C1AAPlaf
028660000912  "   *
028670000912  "   *
028680000912  "   *Gestione emissione window
028690000912  "  C                   Clear                   XMsgDS
028700000912  "   *
028710000919  "   *NDC0602-Competenza plafond &1 non superiore al datario &2
028720000919  "   *
028730000919  "   *        Valorizzo le variabili del msg
028740000919  "  C                   Move      C1MMPlaf      $C1MMPlaf
028750000919  "  C                   Move      '/'           $Barra
028760000919  "  C                   Move      C1AAPlaf      $C1AAPlaf
028770000919  "  C                   Move      'PLESTR'      $Datario
028780000919  "   *
028790000912  "  C                   Movel     MsgId(23)     IDMSG
028800000912  "  C                   CallB     'XRTVM'
028810000912  "  C                   Parm                    IDMsg
028820000912  "  C                   Parm                    TxtMsg
028830000912  "  C                   Parm                    ErrMsg
028840000919  "  C                   Parm                    *Omit
028850000919  "  C                   Parm                    *Omit
028860000919  "  C                   Parm      $P1NDC0602    MsgDta
028870000912  "  C                   If        ErrMsg     <> *On
028880000912  "  C                   MoveL     TxtMsg        MsgMs1
028890000912  "  C                   Else
028900000912  "  C                   MoveL     *All'?'       MsgMs1
028910000912  "  C                   EndIf
028920000912  "   *
028930000912  "   *COS1023-Si desidera continuare comunque l'elaborazione (1)
028940000912  "  C                   Movel     MsgId(12)     IDMSG
028950000912  "  C                   CallB     'XRTVM'
028960000912  "  C                   Parm                    IDMsg
028970000912  "  C                   Parm                    TxtMsg
028980000912  "  C                   Parm                    ErrMsg
028990000912  "  C                   If        ErrMsg     <> *On
029000000912  "  C                   MoveL     TxtMsg        MsgMs2
029010000912  "  C                   Else
029020000912  "  C                   MoveL     *All'?'       MsgMs2
029030000912  "  C                   EndIf
029040000912  "   *
029050000912  "   *COS1043-oppure ritornare alla videata (2) ?
029060000912  "  C                   Movel     MsgId(13)     IDMSG
029070000912  "  C                   CallB     'XRTVM'
029080000912  "  C                   Parm                    IDMsg
029090000912  "  C                   Parm                    TxtMsg
029100000912  "  C                   Parm                    ErrMsg
029110000912  "  C                   If        ErrMsg     <> *On
029120000912  "  C                   MoveL     TxtMsg        MsgMs3
029130000912  "  C                   Else
029140000912  "  C                   MoveL     *All'?'       MsgMs3
029150000912  "  C                   EndIf
029160000912  "   *
029170000912  "   *Reperisco i testi delle possibili risposte:
029180000912  "  C                   Clear                   XMsgTxt
029190000912  "   *
029200000912  "   *COS1025-Continua
029210000912  "  C                   Eval      IdMsg       = MsgId(14)
029220000912  "  C                   CallB     'XRTVM'
029230000912  "  C                   Parm                    IdMsg
029240000912  "  C                   Parm      *Blank        TxTMsg
029250000912  "  C                   Parm                    ErrMsg
029260000912  "  C                   If        ErrMsg      = *On
029270000912  "  C                   MoveL     *All'?'       TxtMsg
029280000912  "  C                   EndIf
029290000912  "   *
029300000912  "  C                   Eval      %SubSt(XMsgTxt:1:15)  = TxtMsg
029310000912  "   *
029320000912  "   *COS1026-Ritorno
029330000912  "  C                   Eval      IdMsg       = MsgId(15)
029340000912  "  C                   CallB     'XRTVM'
029350000912  "  C                   Parm                    IdMsg
029360000912  "  C                   Parm      *Blank        TxTMsg
029370000912  "  C                   Parm                    ErrMsg
029380000912  "  C                   If        ErrMsg      = *On
029390000912  "  C                   MoveL     *All'?'       TxtMsg
029400000912  "  C                   EndIf
029410000912  "   *
029420000912  "  C                   Eval      %SubSt(XMsgTxt:16:15)  = TxtMsg
029430000912  "   *
029440000912  "   *Richiamo finestrina
029450000912  "  C                   MoveL     'S'           MsgRsp
029460000912  "  C                   MoveL     '12'          MsgTpR
029470000912  "  C                   MoveL     'S'           MsgEmV
029480000912  "  C                   Z-Add     10            MsgRgp
029490000912  "   *
029500000912  "   *Modo di inizializzazione della window:
029510000912  "   *('1'->Close + Open  -> I° volta dopo l'emissione del video princip.)
029520000912  "   *('0'->rimane aperta -> tutte le emissioni successive se consecutive,
029530000912  "   *                       per mantenere in sottofondo il video princip.)
029540000912  "  C                   Eval      XMsgInz     = $PrimoXMsg
029550000912  "  C                   If        $PrimoXMsg  = *On
029560000912  "  C                   Eval      $PrimoXMsg  = *Off
029570000912  "  C                   EndIf
029580000912  "   *
029590000912  "  C                   Call      'XMSGR'
029600000912  "  C                   Parm                    XMsgDS
029610000912  "  C                   Parm                    XMsgTxt
029620000912  "  C                   Parm                    XMsgInz
029630000912  "   *
029640000912  "   *Pulisco i testi delle possibili risposte,per evitare
029650000912  "   *'sporcizie' nelle eventuali finestre successive
029660000912  "  C                   Clear                   XMsgTxt
029670000912  "   *
029680000912  "   *
029690000912  "   *L'utente ha scelto 'Ritorno'
029700000912  "  C                   If        MsgRtn      = '2'
029710000912  "   *
029720000912  "   *  accendo l'indicatore d'errore generico,
029730000912  "   *  per rimanere sulla stessa videata
029740000912  "  C                   Eval      *In99       = *On
029750000912  "   *
029760000912  "   *  L'utente ha deciso di ritornare sui 'suoi passi' così
029770000912  "   *  continuerò ad avvertirlo se c'è errore
029780000912  "  C                   Eval      $StopWinP   = *Off
029790000912  "   *
029800000912  "   *L'utente ha scelto 'Continua'
029810000912  "  C                   Else
029820000912  "   *
029830000912  "   *  L'utente ha deciso di procedere nonostante l'avvertimento
029840000912  "   *  così non emetterò più la finestrina per avvertirlo
029850000912  "   *  (solo se la data a video rimane la stessa)
029860000912  "  C                   Eval      $StopWinP   = *On
029870000912  "   *
029880000912  "  C                   EndIf
029890000912  "  C                   EndIf
029900000912  "   *
029910000912D0660C     XWinPlafon    ENDSR
029920990709D0154C************************************************************
029930990709  "  C* Emissione avvertimento data liq <> da data reg
029940990709  "  C************************************************************
029950990709  "  C     Warning       BEGSR
029960990709  "   *
029970990712  "  C     11            Do        15            $X                2 0
029980990709  "  C                   Clear                   XMsgDs
029990990709  "  C                   Clear                   XMsgTxt
030000990712  "  C                   Movel     MsgId($X)     IDMSG
030010990709  "  C                   CALLB     'XRTVM'
030020990709  "  C                   PARM                    IDMSG            27
030030990712  "  C                   PARM                    TXTMSG          100
030040990709  "  C                   PARM                    ERRMSG            1
030050990709  "  C     ERRMSG        IFNE      *ON
030060000128D0154C                   MOVEL     TXTMSG        MsgT($X)
030070040429C1321X*******************************************
030080040429  "  X* la routine ValDateMsg è nata con la     *
030090040429  "  X* sigla D0154, ma dovendola ristrutturare *
030100040429  "  X* completamente (x fare la stessa cosa ma *
030110040429  "  X* con poche specifiche), ho cancellato la *
030120040429  "  X* vecchia e scritta una nuova (cancellato *
030130040429  "  X* anche la definizione delle variabili    *
030140040429  "  X* usate solo dalla vecchia 'ValDateMsg'   *
030150040429C1321X*******************************************
030160000128D0154 * valorizzo le date nel testo del messaggio 11 sostituendo le XX
030170990716  "  C                   Exsr      ValDateMsg
030180990709  "  C                   ELSE
030190990712  "  C                   MOVEL     *ALL'?'       MsgT($X)
030200990709  "  C                   ENDIF
030210990712  "  C                   EndDo
030220991012D0154 *
030230040429C1251X*Usare XTxt per 'distribuire' le risposte nella window,
030240040429  "  X*è superfluo perchè vengono già distribuite accanto
030250040429  "  X*ai relativi bottoni  automaticamente da XMsgR
030260040429  "  X*se in XMsgTxt riceve la successione delle possibili risposte,
030270040429  "  X*distanziate di 15 caratteri
030280040429  "  X*** ex D0154****** EVAL      XTXT  = *blank
030290040429  "  X*** ex D0154****** EVAL      XTXT1 = MsgT(14)
030300040429  "  X*** ex D0154****** EVAL      XTXT2 = MsgT(15)
030310040429  "  X*** ex D0154****** EVAL      XMsgTxt = XTxt
030320991012  "  C                   Eval      %SubSt(XMsgTxt:1:15)  = MsgT(14)
030330991012C1251C                   Eval      %SubSt(XMsgTxt:16:15) = MsgT(15)
030340991012D0154C                   MOVEL     MsgT(11)      MSGMS1
030350990712  "  C                   MOVEL     MsgT(12)      MSGMS2
030360990712  "  C                   MOVEL     MsgT(13)      MSGMS3
030370991012D0154 *
030380040429C1251X*** ex D0154****** Z-ADD     0             MSGMSG
030390991012D0154C                   MOVEL     'S'           MSGRSP
030400990712  "  C                   MOVEL     '12'          MSGTPR
030410990709  "  C                   MOVEL     'S'           MSGEMV
030420991012D0154C                   Z-ADD     10            MSGRGP
030430040429C1251X*** ex D0154****** MOVEL     'N'           MSGLCK
030440040429  "  X*** ex D0154****** MOVEA     STATUS        STS
030450040429  "  X*** ex D0154****** MOVEL     'N'           MSGCNL
030460040429  "  X*** ex D0154****** MOVEL     'N'           MSGVID
030470040429  "  X*** ex D0154****** MOVEL     'N'           MSGOPE
030480040429C1251X*** ex D0154****** MOVEL     'N'           MSGSTP
030490991012D0154 *
030500990709  "   *Modo di inizializzazione della window:
030510990709  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
030520990709  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
030530990709  "   *                       per mantenere il sottofondo del video princip.)
030540990709  "  C                   Eval      XMSGINZ     = $PrimoXMsg
030550990709  "  C                   If        $PrimoXMsg  = *On
030560990709  "  C                   Eval      $PrimoXMsg  = *Off
030570990709  "  C                   EndIf
030580990709  "   *
030590990709  "  C                   CALL      'XMSGR'
030600990709  "  C                   PARM                    XMSGDS
030610990709  "  C                   PARM                    XMSGTXT         135
030620990709  "  C                   PARM                    XMSGINZ           1
030630990709  "   *
030640990712  "  C* l'utente ha scelto di correggere la data
030650990712  "  C     MsgRtn        IFEQ      '2'
030660990712  "  C                   Move      *On           *In99
030670990712  "   * se ha deciso di correggere la data l'avverto fino a quando c'è
030680990712  "   * l'errore
030690990712  "  C                   Move      *Off          $MsgWarn
030700990712  "   * altrimenti un'unica volta
030710990712  "  C                   Else
030720990712  "  C                   Move      *On           $MsgWarn
030730990712  "  C                   ENDIF
030740990712  "   *
030750001020D0154C                   EndSr
030760000128C1321C************************************************************
030770000128  "  C* Valorizzo le date nell'undicesimo messaggio
030780000128  "  C************************************************************
030790000128  "  C     ValDateMsg    BEGSR
030800000128  "   *                                                                /MM/AA
030810000128  "  C                   If        $X = 11
030820000128  "   *
030830000128  "  C                   Move      C1DtRif       DataAlfa
030840000128  "   * posiziono i vari dati al loro posto nel msg
030850000128  "  C                   Eval      %SubSt(MsgT($X):19:2)=%SubSt(DataAlfa:1:2)
030860000128  "  C                   Eval      %SubSt(MsgT($X):22:2)=%SubSt(DataAlfa:3:2)
030870000128  "  C                   Eval      %SubSt(MsgT($X):25:2)=%SubSt(DataAlfa:5:2)
030880000128  "  C                   Move      C1DtReg       DataAlfa
030890000128  "  C                   Eval      %SubSt(MsgT($X):61:2)=%SubSt(DataAlfa:1:2)
030900000128  "  C                   Eval      %SubSt(MsgT($X):64:2)=%SubSt(DataAlfa:3:2)
030910000128  "  C                   Eval      %SubSt(MsgT($X):67:2)=%SubSt(DataAlfa:5:2)
030920000128  "   *
030930000128  "  C                   EndIf
030940000128  "   *
030950001023C1321C                   EndSr
030960950721     C************************************************************
030970950721     C* Sequenza dei ctrl per la videata
030980950721     C************************************************************
030990950721     C     CTRD1         BEGSR
031000990325      *
031010040429C1129X**********************************************************************
031020040429  "  X* spostato dopo tutti i controlli possibili di CtrD1             stare
031030040429  "  X* per dare precedenza ai controlli di validità e formali
031040040429  "  X* (esistenza della causale e/o conto, autorizzazioni ..)
031050040429  "  X* Attenzione: le specifiche del cambiamento dei dft
031060040429  "  X*             si snodano in routine diverse
031070040429  "  X**********************************************************************
031080040429  "  X*Se è stata cambiata la causale
031090040429  "  X****               If        D1Caustes <> D2CausTes AND
031100040429  "  X****                         D1CausTes <> *Blank    AND
031110040429  "  X****                         D2CausTes <> *Blank
031120040429  "  X**** ...avverto l'utente che rimangono i dft della vecchia caus.
031130040429  "  X****               EXSR      WinChgCaus
031140040429  "  X****               EndIf
031150040429  "  X****
031160040429  "  X*Se è stata cambiato il conto
031170040429  "  X****               If        D1Ksc     <> D2Ksc     AND
031180040429  "  X****                         D1Ksc     <> *Blank    AND
031190040429  "  X****                         D2Ksc     <> *Blank
031200040429  "  X**** ...chiedo all'utente se vuole cambiare i default
031210040429  "  X****               EXSR      WinChgKsc
031220040429  "  X****               If        MsgRtn      = 'S'
031230040429  "  X****               EXSR      ChgDftKsc
031240040429  "  X****               EndIf
031250040429  "  X****               EndIf
031260040429C1129X**********************************************************************
031270990204     C*
031280950721     C* controlli valori video D1
031290950721     C                   EXSR      CtrlValD1
031300090721D2530C* se chiamato un programma, vado a fine routine, deve fare  inzd1
031310090721  "  C                   if        Ritorno_D1 =*on
031320090721  "  c                   leavesr
031330090721D2530c                   endif
031340950721     C*
031350950807     C* controlli di base (autorizzazioni)
031360950721     C                   if        *in99 = *off
031370950721     C                   EXSR      CtrlBase
031380950721     C                   endif
031390950807     C*
031400950721     C* Controlla coerenza conto/causale/segno
031410950721     C                   if        *in99 = *off
031420950726     C                   EXSR      callmvc001
031430950807     C                   endif
031440990503C1129 *
031450990503  "  C                   If        *In99      = *Off
031460990503  "   *Se è stata cambiata la causale
031470990503  "  C                   If        D1Caustes <> D2CausTes AND
031480990503  "  C                             D1CausTes <> *Blank    AND
031490990503  "  C                             D2CausTes <> *Blank
031500990503  "   *    ...avverto l'utente che rimangono i dft della vecchia caus.
031510990503  "  C                   EXSR      WinChgCaus
031520990503  "  C                   EndIf
031530990503  "   *
031540990503  "   *Se è stata cambiato il conto
031550990504  "  C                   If        (D1Ksc     <> D2Ksc  OR
031560990504  "  C                             D1TpSogg   <> D2TpSogg )   AND
031570990504  "  C                             D1Ksc     <> *Blank        AND
031580990503  "  C                             D2Ksc     <> *Blank
031590990503  "   *    ...chiedo all'utente se vuole cambiare i default
031600990503  "  C                   EXSR      WinChgDftK
031610990505  "   *    ...chiamo il pgm che gestisce la modifica contropartite ut.
031620990505  "  C                   EXSR      CallChgCpu
031630990503  "  C                   EndIf
031640990503C1129C                   EndIf
031650950721     C*
031660950721     C                   ENDSR
031670990325      ************************************************************
031680990325      * Window x cambiamento causale
031690990325      ************************************************************
031700990325     C     WinChgCaus    BEGSR
031710990208      *
031720990208     C                   CLEAR                   XMSGDS
031730990505C1129C                   CLEAR                   XMSGTXT
031740990430C1129 *AVVERTIMENTO: i default di causale impostati a video
031750990208     C                   Movel     MsgId(4)      IDMSG
031760990208     C                   CALLB     'XRTVM'
031770990208     C                   PARM                    IDMSG            27
031780990208     C                   PARM      *Blank        TXTMSG          100
031790990208     C                   PARM                    ERRMSG            1
031800990208     C     ERRMSG        IFNE      *ON
031810990208     C                   MOVEL     TXTMSG        MSGMS1           70
031820990208     C                   ELSE
031830990208     C                   MOVEL     *ALL'?'       MSGMS1
031840990208     C                   ENDIF
031850990208      *
031860990430C1129 *             non cambieranno nonostante sia variata la causale.
031870990208     C                   Movel     MsgId(5)      IDMSG
031880990208     C                   CALLB     'XRTVM'
031890990208     C                   PARM                    IDMSG            27
031900990208     C                   PARM      *Blank        TXTMSG          100
031910990208     C                   PARM                    ERRMSG            1
031920990208     C     ERRMSG        IFNE      *ON
031930990208     C                   MOVEL     TXTMSG        MSGMS2           70
031940990208     C                   ELSE
031950990208     C                   MOVEL     *ALL'?'       MSGMS2
031960990208     C                   ENDIF
031970990208      *
031980990208     C                   MOVEL     'S'           MSGEMV
031990990430     C                   Z-ADD     10            MSGRGP
032000990208     C                   MOVEL     'N'           MSGRSP
032010990208      *
032020990505C1129 *Modo di inizializzazione della window:
032030990505C1129 *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
032040990505C1129 *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
032050990505C1129 *                       per mantenere il sottofondo del video princip.)
032060990208     C                   Eval      XMSGINZ     = $PrimoXMsg
032070990208     C                   If        $PrimoXMsg  = *On
032080990208     C                   Eval      $PrimoXMsg  = *Off
032090990208     C                   EndIf
032100990208      *
032110990208     C                   CALL      'XMSGR'
032120990208     C                   PARM                    XMSGDS
032130990208     C                   PARM                    XMSGTXT         135
032140990208     C                   PARM                    XMSGINZ           1
032150990325      *
032160990325     C     XWinChgCau    ENDSR
032170990325      ************************************************************
032180040429C1129X**Window x cambiamento conto
032190990428C1129 * Window x cambiamento default conto
032200990325      ************************************************************
032210040429C1129X*****WinChgKsc     BEGSR
032220990428C1129C     WinChgDftK    BEGSR
032230990429C1129 *
032240990429C1129C                   CLEAR                   XMSGDS
032250990429C1129C                   Clear                   XMSGTXT
032260990322      *
032270990428      * Vuoi rivalorizzare i campi già impostati a video,                     o,
032280990322     C                   Movel     MsgId(6)      IDMSG
032290990322     C                   CALLB     'XRTVM'
032300990322     C                   PARM                    IDMSG            27
032310990322     C                   PARM      *Blank        TXTMSG          100
032320990322     C                   PARM                    ERRMSG            1
032330990322     C     ERRMSG        IFNE      *ON
032340990322     C                   MOVEL     TXTMSG        MSGMS1           70
032350990322     C                   ELSE
032360990322     C                   MOVEL     *ALL'?'       MSGMS1
032370990322     C                   ENDIF
032380990322      *
032390990428      * con i default del nuovo conto?
032400990322     C                   Movel     MsgId(7)      IDMSG
032410990322     C                   CALLB     'XRTVM'
032420990322     C                   PARM                    IDMSG            27
032430990322     C                   PARM      *Blank        TXTMSG          100
032440990322     C                   PARM                    ERRMSG            1
032450990322     C     ERRMSG        IFNE      *ON
032460990322     C                   MOVEL     TXTMSG        MSGMS2           70
032470990322     C                   ELSE
032480990322     C                   MOVEL     *ALL'?'       MSGMS2
032490990322     C                   ENDIF
032500990322      *
032510990322     C                   MOVEL     'S'           MSGEMV
032520990430     C                   Z-ADD     10            MSGRGP
032530990322     C                   MOVEL     'S'           MSGRSP
032540990322     C                   MOVEL     'SN'          MSGTPR
032550990322      *
032560990505C1129 *Modo di inizializzazione della window:
032570990505C1129 *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
032580990505C1129 *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
032590990505C1129 *                       per mantenere il sottofondo del video princip.)
032600990322     C                   Eval      XMSGINZ     = $PrimoXMsg
032610990322     C                   If        $PrimoXMsg  = *On
032620990322     C                   Eval      $PrimoXMsg  = *Off
032630990322     C                   EndIf
032640990322      *
032650990322     C                   CALL      'XMSGR'
032660990322     C                   PARM                    XMSGDS
032670990322     C                   PARM                    XMSGTXT         135
032680990322     C                   PARM                    XMSGINZ           1
032690990428      *
032700990503     C                   If        MsgRtn      = 'S'
032710990503     C                   EXSR      ChgDftKsc
032720990503     C                   EndIf
032730990325      *
032740040429C1129X*****XWinChgKsc    ENDSR
032750990428C1129C     XWinChgDft    ENDSR
032760990503  "   ************************************************************
032770990503  "   * Chiamo pgm per cambiare le contropartite utente
032780990503  "   * al cambiamento conto principale
032790990503  "   ************************************************************
032800990503  "  C     CallChgCpu    BEGSR
032810990503  "   *
032820990503  "  C                   Reset                   NDC019DS
032830990503  "  C                   Eval      Societa019  = XScSoc
032840990505  "  C                   Eval      PrimoXm019  = $PrimoXMsg
032850990503  "  C                   Eval      Sys019      = REGSys
032860990503  "  C                   Eval      NrAsReg019  = REGNrAsReg
032870990503  "  C                   Eval      OldKsc019   = D2Ksc
032880990503  "  C* reperisco capoconto del vecchio conto
032890990503  "  C                   movel     D2TpSogg      OldKcc019
032900990503  "  C                   CALLB     'XCAPCLIFOR'
032910990503  "  C                   PARM                    XScSoc
032920990503  "  C                   PARM                    OldKcc019
032930990503  "  C                   PARM                    OldKsc019
032940990503  "  C                   Eval      NewKcc019   = $Kcc
032950990503  "  C                   Eval      NewKsc019   = D1Ksc
032960990503  "  C                   CallB     'NDC019R'
032970990503  "  C                   PARM                    KPJBA
032980080916C1129C                   PARM                    NDC019DS
032990080916D2369c* passo la ds ndc019ds al pgm ndc004r per effettuare
033000080916  "  c* aggiornamento della pn collegata
033010080916D2369c                   eval      NDC019DS1 = NDC019DS
033020080916C1129 *
033030001020C1129C     XCallChgCp    ENDSR
033040990429      ************************************************************
033050990325      * Cambio default del veccio conto con il nuovo conto
033060990325      ************************************************************
033070990325     C     ChgDftKsc     BEGSR
033080990325      *
033090040429C1129X****               Eval      D2CondPag   = *Blank
033100990503      *
033110990503C1129C                   Select
033120990503  "  C     D1TpSogg      WhenEq    'C'
033130990503  "  C                   EVAL      D2CondPag = DVCCondPag
033140990503  "  C     D1TpSogg      WhenEq    'F'
033150990503  "  C                   EVAL      D2CondPag = DVFCondPag
033160990503C1129C                   EndSl
033170990325      *
033180990325     C     XChgDftKsc    ENDSR
033190950721     C************************************************************
033200950721     C* Sequenza dei ctrl per la videata D2
033210950721     C************************************************************
033220950721     C     CTRD2         BEGSR
033230950721     C*
033240950726     C* controlli specifici videata D2 (date)
033250950721     C                   EXSR      CtrlValD2
033260040205C1722 *
033270040205  "   * controlli se fattura già catalogata su Proj Doc
033280040205  "  C                   EXSR      CtrlLaguna2
033290040205C1722 *
033300040601D1617 *
033310040601  "   * controlli se modificata data di regitrazione per i ratei
033320040629  "  c                   if        WDtReg <> d2DtReg  and *in99=*off
033330040601  "  C                   EXSR      CtrlRatei
033340040607  "  c                   if        Err326=*on
033350040607  "  c                   seton                                        99
033360040607  "  c                   endif
033370040601D1617c                   endif
033380001009D0714C*
033390040429D0794X*** controllo serie documento: deve contenere solo caratteri
033400040429  "  X*** alfanumerici
033410001218  "  C* controllo serie documento: il primo carattere deve essere
033420001218  "  C* compreso tra blank e '9' e non ci devono essere blank in
033430001218D0794C* in testa o in mezzo
033440001218D0714C                   if        D2SerDoc <> *blank
033450001009  "  C                   eval      X63Codice = D2SerDoc
033460001009  "  C                   exsr      CtrlSerDP
033470001020D0714C                   if        X63Errore = *On
033480040429D0611X**ex D0714 ********seton                                        3599
033490001023D0611 * PRO1277-(F1) Sono presenti caratteri non consentiti
033500001023  "  C                   Eval      $InD2(35)   = *On
033510001020D0611C                   Eval      *In99       = *On
033520001020D0714C                   endif
033530001009D0714C                   endif
033540950807     C*
033550950921     C* eseguo i controlli sugli estremi partita solo se
033560950921     C* non ho già degli errori
033570950921     C                   if        *in99 = *off        and
033580950921     C* non sono in annullamento o visualizzazione
033590950921     C                             opz015 <> '04'      and
033600950921     C                             opz015 <> '05'      and
033610950921     C* e se sono abilitato a scrivere se non ci saranno errori
033620950921     C                             SoloCtrl = *off
033630980129     C                   EXSR      CtrPartita
033640950721     C*
033650960424     C                   ENDIF
033660060619     C*
033670060619C1937C* eseguo i controlli sugli estremi documento solo se
033680060619  "  C* non ho già degli errori
033690060619  "  C                   if        *in99 = *off    and
033700060619  "  C* e se sono abilitato a scrivere se non ci saranno errori
033710060619  "  C                             SoloCtrl = *off and
033720060619  "  c* e solo se sono nella videata D2
033730060619  "  c                             $Gest='D2'      and
033740060619  "  C* non sono in annullamento o visualizzazione
033750060619  "  C                             opz015 <> '04'  and
033760060619  "  C                             opz015 <> '05'  and
033770060619  "  C* e se valorizzata data/numero documento
033780060619  "  c                             D2DtDoc <> 0    and
033790060619  "  c                             D2NrDoc <> 0
033800060619  "  c     *jobrun       move      d2DtDoc       Dataiso
033810060619  "  c* se dati differenti
033820060619  "  C                   if        WDtDocIso<>Dataiso  or
033830060619  "  C                             WNrDoc<>D2NrDoc     or
033840060619  "  c* o se dati uguali ma siamo in copia
033850060703  "  C                             opz015 =  '03'      or
033860060703  "  c* o se dati uguali ma siamo in passaggio a definitivo
033870060703  "  C                             opz015 =  '22'
033880060619  "  C                   EXSR      CtrDocumen
033890060619  "  c                   endif
033900060619  "  c*
033910060619C1937C                   ENDIF
033920060619     C*
033930001026C1429 *
033940001026  "   * Descrizione blocco partita
033950001026C1429C                   EXSR      BloccoPar
033960960424     C*
033970070530D2208 * Se non ci sono errori e non sono in annullam. o visualizzaz.
033980070530  "   * Ctr se l'importo=0 e la cond.pag crea effetti
033990070530  "  C                   if        *in99=*off and
034000070530  "  C                             Opz015<>'04' and Opz015<>'05'
034010070530  "  C                   exsr      CtrEffImpo0
034020070530  "  C                   endif
034030070530D2208 *
034040991012C1251 * Se non ci sono errori e non sono in annullam. o visualizzaz.
034050991011  "   *  e se è già stata emessa almeno una volta il video D2
034060991011  "   *  (non faccio il ctr se CTRD2 è chiamato solo per decodificare)
034070991011  "  C                   If        *In99    =  *Off AND
034080991011  "  C                              Opz015  <> '04' AND
034090991011  "  C                              Opz015  <> '05' AND
034100991011  "  C                              $ExfmtD2 = *On
034110991011  "   *  Ctr se il conto è forn. percipiente e la cond.pag crea effetti
034120991011  "  C                   EXSR      CtrPercEff
034130991011  "  C                   EndIf
034140991012C1251 *
034150950721     C* proseguo solo se non ci sono già degli errori
034160950721     C                   IF        *in99 = *off
034170950721     C*
034180950721     C* se l'opzione è l'annullamento
034190950721     C                   IF        Opz015 = '04'  and
034200950721     C* e non devo controllare i dati
034210950727     C                             SoloCtrl = *off and
034220950727     C* e premuto F6
034230950727     C                             $TASTO = F06
034240950721     C* richiamo il modulo che cancella la testata
034250950721     C                   EXSR      CallCancel
034260950721     C* per tutte le altre opzioni
034270950721     C                   ELSE
034280950721     C*
034290950721     C* riempio la DS del rcd da aggiornare/decodificare
034300950721     C* con i valori a video
034310950721     C                   EXSR      Rie004DS
034320950721     C* richiamo il modulo che aggiorna/decodifica la testata
034330950721     C                   EXSR      CallScrivi
034340950721     C*
034350950721     C                   ENDIF
034360950721     C*
034370950721   E C                   ENDIF
034380950721     C*
034390950721     C                   ENDSR
034400040205C1722 ************************************************************
034410040205  "   * controlli se fattura già catalogata su Proj Doc
034420040205  "   ************************************************************
034430040205  "  C     CtrlLaguna2   Begsr
034440040205  "   *
034450040205  "   * stabilisco se la data documento è corretta
034460040205  "  C                   eval      $D2DtDocOK = *off
034470040205  "  C     *jobrun       test(d)                 D2DtDoc                25
034480040205  "  C                   if        *in25 = *off
034490040205  "  C                   eval      $D2DtDocOK = *on
034500040205  "  C                   endif
034510040205  "   *
034520040205  "   * Se la società gestisce Proj Doc
034530040205  "   * . su parametro modulo 'LAGUNA'
034540040205  "   *     "Contabilità - documenti IVA / Fornitori /
034550040205  "   *      Catalogazione interattiva in immissione primanota : S"
034560040205  "   * . siamo in immissione/copia
034570040205  "   * . documento fornitore
034580040205  "   *     [Sottotipologia causale: 'F'=Doc.Iva Fornitori ]
034590040205  "   * . fattura
034600060303C1722 *     [Tipo documento IVA:
034610060303D1937 *      0=Fatture nazionali/2=Bolle doganali/3=Fatture estere/
034620060303C1722 *      5=Fatture con riten. d'acconto/6=Fatture di sola IVA ]
034630040205  "   *      (escluse 2=Bolle doganali/8=Autofatture/9=Altri documenti)
034640040205  "   * . oppure nota
034650040205  "   *     [Tipo documento IVA:
034660040205  "   *      1=Note accredito nazionali/4=Note accredito estere/
034670040205  "   *      7=Note accredito di sola IVA ]
034680040205  "   *      (escluse 2=Bolle doganali/8=Autofatture/9=Altri documenti)
034690040205  "   * . data documento corretta
034700040205  "   *
034710040205  "  C                   if        XScLaguna = *on
034720040205  "  C                             and CatFatC062 = *on
034730040205  "  C                             and (Opz015 = '01' or Opz015 = '03')
034740060303C1722C                             and OpeStp = 'F'
034750060303D1937x***                          and OPETpDocI<>'2'
034760060303C1722C                             and OPETpDocI<>'8'
034770040205  "  C                             and OPETpDocI<>'9'
034780040205  "  C                             and $D2DtDocOK=*on
034790040205  "   *
034800040205  "  C                   clear                   Dati
034810040205  "  C                   clear                   ANLGN00F
034820040205  "   *
034830040205  "   * kiave fattura/nota fornitore:
034840040205  "   * LGNClassPJ = '020'
034850040205  "   * LGNClassLG = 'DOC'
034860040205  "   * LGNKey101  = Societa
034870040205  "   * LGNKey102  = 'F' se fattura / 'N' se nota
034880040205  "   * LGNKey103  = Data  documento
034890040205  "   * LGNKey104  = Nr    documento
034900040205  "   * LGNKey105  = Serie documento
034910040205  "   * LGNKey106  = Sottoconto fornitore
034920040205  "   *
034930040205  "   * imposto chiavi
034940040205  "  C                   eval      LGNClassPJ = '020'
034950040205  "  C                   eval      LGNClassLg = 'DOC'
034960040205  "  C                   eval      LGNKey101 = XScSoc
034970040205  "   *
034980040205  "  C                   select
034990040205  "   *   fattura
035000060303C1722C                   when      OPETpDocI='0' or OPETpDocI='3' or
035010060303D1937C                             OPETpDocI='2' or
035020060303C1722C                             OPETpDocI='5' or OPETpDocI='6'
035030040205  "  C                   eval      LGNKey102 = 'F'
035040040205  "   *   nota
035050040205  "  C                   when      OPETpDocI='1' or OPETpDocI='4' or
035060040205  "  C                             OPETpDocI='7'
035070040205  "  C                   eval      LGNKey102 = 'N'
035080040205  "  C                   endsl
035090040205  "   *
035100040205  "  C     *jobrun       move      D2DtDoc       DataISO
035110040205  "  C                   movel     DataISO       LGNKey103
035120040205  "  C                   movel     D2NrDoc       LGNKey104
035130040205  "  C                   eval      LGNKey105 = D2SerDoc
035140040205  "  C                   eval      LGNKey106 = D2Ksc
035150040205  "  C                   eval      NrChiavi = 8
035160040205  "   *
035170040205  "  C                   movel     ANLGN00F      Dati
035180040205  "  C                   eval      Lunghezza = %size(ANLGN00F)
035190040205  "  C                   eval      Errore = *off
035200040205  "   *
035210040205  "  C                   callb     'PJDR1701R'
035220040205  "  C                   parm      '01'          OpzioneDR                      1
035230040205  "  C                   parm                    Errore                         2
035240040205  "  C                   parm      2             Vista                          5
035250040205  "  C                   parm                    Dati                           3
035260040205  "  C                   parm                    Lunghezza                      4
035270040205  "  C                   parm      *off          Commit                         6
035280040205  "  C                   parm                    NrChiavi                       7
035290040205  "  C                   parm      *off          Alloca                         8
035300040205  "  C                   parm                    *omit                          9
035310040205  "   *
035320040205  "  C                   if        Errore = *off
035330040205  "  C                   exsr      WinLaguna2
035340040205  "  C                   eval      *in99 = *on
035350040205  "  C                   endif
035360040205  "   *
035370040205  "  C                   endif
035380040205  "   *
035390040205C1722C     XCtrlLaguna2  Endsr
035400040205C1722 ************************************************************
035410040205  "   * blocco l'utente perchè esiste già una fattura catalogata
035420040205  "   * su Proj Doc
035430040205  "   ************************************************************
035440040205  "  C     WinLaguna2    Begsr
035450040205  "   *
035460040205  "  C                   clear                   XMsgDS
035470040205  "  C                   clear                   XMsgTxt
035480040205  "   *
035490040205  "  C                   movel     'S'           MsgEmv
035500040205  "  C                   z-add     10            MsgRgp
035510040205  "  C                   movel     'N'           MsgRsp
035520040205  "   *
035530040205  "   * Modo di inizializzazione della window:
035540040205  "   * ('1'->Close + Open  -> I° volta dopo l'emissione del video princip.)
035550040205  "   * ('0'->rimane aperta -> tutte le emissioni successive se consecutive,
035560040205  "   *                        per mantenere in sottofondo il video princ.)
035570040205  "  C                   eval      XMsgInz = $PrimoXMsg
035580040205  "  C                   if        $PrimoXMsg = *on
035590040205  "  C                   eval      $PrimoXMsg = *off
035600040205  "  C                   endif
035610040205  "  C                   call      'XMSGR'
035620040205  "  C                   parm                    XMsgDS
035630040205  "  C                   parm                    XMsgTxt
035640040205  "  C                   parm                    XMsgInz
035650040205  "   * PRO1972-(F1) Documento già catalogato su Proj Doc
035660040205  "  C                   parm      'PRO1972'     XMsgCd1           7
035670040205  "   *
035680040205  "   * Pulisco i testi delle possibili risposte,per evitare
035690040205  "   * 'sporcizie' nelle eventuali finestre successive
035700040205  "  C                   clear                   XMsgTxt
035710040205  "   *
035720040205C1722C     XWinLaguna2   Endsr
035730001009D0714 ************************************************************
035740040429D0794X*** Controllo che serie Documento/Partita contengano solo
035750040429  "  X*** caratteri alfanumerici (pgm di controllo: X63CtrChar)
035760001218  "   * Controllo che il primo carattere di serie Documento/Partita
035770001218  "   * sia compreso tra blank e '9' e che non ci siano blank posti
035780001218D0794 * in testa o in mezzo alla stringa
035790001218D0714 ************************************************************
035800001009D0714C     CtrlSerDP     begsr
035810001009      *
035820001009     C                   eval      X63Len = 4
035830040429D0794X***                eval      X63Tipo = '5'
035840001218D0794C                   eval      X63Tipo = '6'
035850001218     C                   eval      X63Blank = '0'
035860001009     C                   eval      X63Errore = *Off
035870001009      *
035880001009     C                   call      'X63CTRCHAR'
035890001009     C                   parm                    X63Codice
035900001009     C                   parm                    X63Len
035910001009     C                   parm                    X63Tipo
035920001009     C                   parm                    X63Blank
035930001009     C                   parm                    X63Errore
035940001009      *
035950001009D0714C                   endsr
035960070530d2208 ************************************************************
035970070530  "   *  Ctr se l'importo=0 e la cond.pag crea effetti
035980070530  "   ************************************************************
035990070530  "  C     CtrEffImpo0   begsr
036000070530  "   *
036010070530  "  C                   eval      $CreaEff = *off
036020070531  "  C                   if        d2importo=0 and d2importd=0
036030070530  "  C                             and d2condpag<>*blank
036040070531  "  C                             and (opz015<>'02' or d2pagman=xscoff)
036050070530  "   *  reperisco il tipo rate (Rim.diretta o effetti)
036060070530  "  C                   exsr      RepANCDR
036070070530  "   *  Se la cond. pagam. crea effetti
036080070530  "  C                   if        $CreaEff=*on
036090070531  "   * NDC0988 - Non è possibile gestire effetti se il relativo importo è 0
036100070531  "  C                   eval      $InD2(51) = *on
036110070530  "  C                   eval      *In99 = *on
036120070530  "  C                   endif
036130070530  "  C                   endif
036140070530  "   *
036150070530D2208C     XCtrEffImpo0  endsr
036160991012C1251 ************************************************************
036170991011  "   * Ctr se il conto è forn. percipiente e la cond.pag crea effetti
036180991011  "   * se è così viene emessa una window che avverte l'utente che
036190991011  "   * gli effetti non sono gestiti dalle ritenute d'acconto
036200991011  "   ************************************************************
036210991011  "  C     CtrPercEff    BEGSR
036220991011  "   *
036230991011  "   * Se il soggetto è un fornitore percipiente
036240991011  "  C                   If        D2TpSogg = 'F' AND  RCOPercipi = '1'
036250991011  "   *
036260991011  "   * E' necessario chiedersi se esiste la cond.pagam.
036270991011  "   * xchè il suo ctl viene fatto da NDMVC111 (chiamato dopo)
036280991011  "  C                   Eval      $CreaEff = *Off
036290991011  "  C                   If        D2CondPag <> *Blank
036300991011  "   *  reperisco il tipo rate (Rim.diretta o effetti)
036310991011  "  C                   EXSR      RepANCDR
036320991011  "  C                   EndIf
036330991011  "   *
036340991011  "   *  Se la cond. pagam. crea effetti (per un fornitore percipiente)
036350991011  "   *  emetto window che avverte l'utente che gli effetti
036360991011  "   *  non sono gestiti dalle ritenute d'acconto
036370991011  "  C                   If        $CreaEff = *On
036380991011  "  C                   EXSR      WinPercEff
036390991011  "  C                   EndIf
036400991011  "  C                   EndIf
036410991011  "   *
036420991011  "  C     XCtrPercEf    ENDSR
036430991011  "   ************************************************************
036440991011  "   * Reperisco il tipo rate (Rim.diretta o effetti)
036450991011  "   * (se il conto è forn. percipiente e la cond.pag crea effetti
036460991011  "   *  viene emessa una window che avverte l'utente che
036470991011  "   *  gli effetti non sono gestiti dalle ritenute d'acconto)
036480991011  "   ************************************************************
036490991011  "  C     RepANCDR      BEGSR
036500991011  "   *
036510991011  "  c                   EVAL      CDRSocieta = XScSoc
036520991011  "  C                   EVAL      CDRCondPag = D2CondPag
036530991011  "  C     K02CDR01      CHAIN     ANCDR000                           21
036540991011  "   *
036550991011  "  c                   DoW       *In21 = *Off AND $CreaEff = *Off
036560991011  "  c                   If        CDRMdPag <> ' '
036570991011  "  C                   Eval      $CreaEff = *On
036580991011  "  C                   EndIf
036590991011  "  C     K02CDR01      READE     ANCDR000                               21
036600991011  "  c                   EndDo
036610991011  "   *
036620991011  "  C     XRepANCDR     ENDSR
036630991011  "   ************************************************************
036640991011  "   * Emissione window
036650991011  "   * (quando fornitore percip e cond.pag emette effetti):
036660991011  "   * "ATTENZIONE: si stanno creando effetti passivi per un percipiente,
036670991011  "   *              questa modalità non è gestita per le ritenute d'acconto."
036680991011  "   ************************************************************
036690991011  "  C     WinPercEff    BEGSR
036700991011  "   *
036710991011  "  C                   CLEAR                   XMSGDS
036720991011  "  C                   CLEAR                   XMSGTXT
036730991012  "   *
036740991011  "   *"ATTENZIONE: si stanno creando effetti passivi per un percipiente,"
036750991011  "  C                   Movel     MsgId(18)     IDMSG
036760991011  "  C                   CALLB     'XRTVM'
036770991011  "  C                   PARM                    IDMSG            27
036780991011  "  C                   PARM      *Blank        TXTMSG          100
036790991011  "  C                   PARM                    ERRMSG            1
036800991011  "  C     ERRMSG        IFNE      *ON
036810991011  "  C                   MOVEL     TXTMSG        MSGMS1           70
036820991011  "  C                   ELSE
036830991011  "  C                   MOVEL     *ALL'?'       MSGMS1
036840991011  "  C                   ENDIF
036850991011  "   *
036860991012  "   * "questa modalità non è gestita per le ritenute d'acconto."
036870991011  "  C                   Movel     MsgId(19)     IDMSG
036880991011  "  C                   CALLB     'XRTVM'
036890991011  "  C                   PARM                    IDMSG            27
036900991011  "  C                   PARM      *Blank        TXTMSG          100
036910991011  "  C                   PARM                    ERRMSG            1
036920991011  "  C     ERRMSG        IFNE      *ON
036930991011  "  C                   MOVEL     TXTMSG        MSGMS2           70
036940991011  "  C                   ELSE
036950991011  "  C                   MOVEL     *ALL'?'       MSGMS2
036960991011  "  C                   ENDIF
036970991012  "   *
036980991012  "   *"Vuoi continuare?"
036990991012  "  C                   Movel     MsgId(17)     IDMSG
037000991012  "  C                   CALLB     'XRTVM'
037010991012  "  C                   PARM                    IDMSG            27
037020991012  "  C                   PARM      *Blank        TXTMSG          100
037030991012  "  C                   PARM                    ERRMSG            1
037040991012  "  C     ERRMSG        IFNE      *ON
037050991012  "  C                   MOVEL     TXTMSG        MSGMS3           70
037060991012  "  C                   ELSE
037070991012  "  C                   MOVEL     *ALL'?'       MSGMS3
037080991012  "  C                   ENDIF
037090991012  "   *
037100991011  "  C                   MOVEL     'S'           MSGEMV
037110991011  "  C                   Z-ADD     10            MSGRGP
037120991012  "  C                   MOVEL     'S'           MSGRSP
037130991012  "  C                   MOVEL     'SN'          MSGTPR
037140991011  "   *
037150991011  "   *Modo di inizializzazione della window:
037160991011  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
037170991011  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
037180991011  "   *                       per mantenere il sottofondo del video princip.)
037190991011  "  C                   Eval      XMSGINZ     = $PrimoXMsg
037200991011  "  C                   If        $PrimoXMsg  = *On
037210991011  "  C                   Eval      $PrimoXMsg  = *Off
037220991011  "  C                   EndIf
037230991011  "   *
037240991011  "  C                   CALL      'XMSGR'
037250991011  "  C                   PARM                    XMSGDS
037260991011  "  C                   PARM                    XMSGTXT         135
037270991011  "  C                   PARM                    XMSGINZ           1
037280991012  "   *
037290991012  "   * l'utente ha scelto rimanere nella vodeata D2
037300991012  "  C     MsgRtn        IfEq      'N'
037310991012  "  C                   Move      *On           *In99
037320991012  "  C                   EndIf
037330991011  "   *
037340991012C1251C     XWinPercEf    ENDSR
037350950721     C************************************************************
037360950724     C* Sequenza dei ctrl per la videata s1
037370950721     C************************************************************
037380950724     C     CTRS1         BEGSR
037390950724     C*
037400950724     C                   SETOFF                                       99
037410971209     C                   MOVE      '0'           $FC3(11)
037420950801     C                   Z-ADD     0             TotImpIVA
037430950801     C                   Z-ADD     0             TotImpIVAD
037440950724     C*
037450980528     C* ad ogni ciclo di controllo resetto i bottoni relativi all'IVA
037460980528     C* per evitare che, dopo un F12 dai successivi pgm, i tuberi relativi
037470980528     C* ai bottoni rimangano impostati con l'ultima scrittura
037480980528     C*
037490980528     C* resetto il bottone del totale IVA deducibile
037500980528     C* (tanto il sfl viene riletto tutto e quindi il bottone viene
037510980528     C*  completamente ricalcolato)
037520980528     C                   CLEAR                   Fun
037530980528     C                   EVAL      Fun(8) = '1'
037540980528     C                   EVAL      LenOut = %Size(ND006DS)
037550980528     C                   CALLB     'NDMVC099'
037560980528     C                   PARM                    £RigaM
037570980528     C                   PARM      '0'           Ctr
037580980528     C                   PARM                    Fun
037590980528     C                   PARM                    ND006DS
037600980528     C                   PARM                    LenOut
037610980528     C* resetto il bottone del totale IVA indeducibile
037620980528     C* (tanto il sfl viene riletto tutto e quindi il bottone viene
037630980528     C*  completamente ricalcolato)
037640980528     C                   CLEAR                   Fun
037650980528     C                   EVAL      Fun(8) = '1'
037660980528     C                   EVAL      LenOut = %Size(ND006DS)
037670980528     C                   CALLB     'NDMVC099'
037680980528     C                   PARM                    £RigaMInd
037690980528     C                   PARM      '0'           Ctr
037700980528     C                   PARM                    Fun
037710980528     C                   PARM                    ND006DS
037720980528     C                   PARM                    LenOut
037730980528     C* resetto il bottone del totale imponibile/imposta
037740980528     C* (tanto il sfl viene riletto tutto e quindi il bottone viene
037750980528     C*  completamente ricalcolato)
037760980528     C                   CLEAR                   Fun
037770980528     C                   EVAL      Fun(5) = '1'
037780980528     C                   EVAL      LenOut = %Size(ND003DS)
037790980528     C                   CALLB     'NDMVC199'
037800980528     C                   PARM                    Fun
037810980528     C                   PARM                    ND003DS
037820980528     C                   PARM                    LenOut
037830980528     C* resetto il bottone della quadratura IVA
037840980528     C* (tanto il sfl viene riletto tutto e quindi il bottone viene
037850980528     C*  completamente ricalcolato)
037860980528     C                   CLEAR                   Fun
037870980528     C                   EVAL      Fun(7) = '1'
037880980528     C                   EVAL      LenOut = %Size(ND003DS)
037890980528     C                   CALLB     'NDMVC199'
037900980528     C                   PARM                    Fun
037910980528     C                   PARM                    ND003DS
037920980528     C                   PARM                    LenOut
037930950721     C*
037940980528     C* ciclo per tutta la pagina del sfl (è un sfl speciale che gestisce
037950980528     C*  una sola pagina)
037960980528     C*
037970950724     C                   DO        sflpag        S1NRR
037980950724     C     S1NRR         CHAIN     FMTS1                              21
037990950724     C*
038000950724     C* Richiamo pgm per controllo registrazione IVA
038010950724     C*
038020950724     C                   IF        S1causRig  <> *blanks or
038030950724     C                             S1RifIva   <> *blanks or
038040950724     C                             S1Aliq     <> 0       or
038050950724     C                             S1Importd  <> 0       or
038060950724     C                             S1ImpIvad  <> 0       or
038070950724     C                             S1Importo  <> 0       or
038080950724     C                             S1ImpIva   <> 0       or
038090950803     C                             S1segno    <> *blanks  or
038100950803     C                             H1nrrigam  <> 0
038110950724     C*
038120950803     C                   SELECT
038130950904     C                   WHEN      SoloCtrl = *on
038140950904     C                   MOVE      '01'          Operazione
038150950803     C                   WHEN      Opz015 = '05'
038160950803     C                   MOVE      '01'          Operazione
038170950803     C                   OTHER
038180950803     c
038190950803     c* cancellazione
038200950803     C                   if        S1RifIva   =  *blanks and
038210950803     C                             S1Aliq     =  0       and
038220950803     C                             S1Importd  =  0       and
038230950803     C                             S1ImpIvad  =  0       and
038240950803     C                             S1Importo  =  0       and
038250950803     C                             S1ImpIva   =  0       and
038260950803     C                             H1nrrigam  <> 0
038270950803     C                   MOVE      '05'          Operazione
038280950803     c                   else
038290950803     c* aggiornamento
038300971205     C                   IF        $Tasto = F11
038310980529     C                   MOVE      '06'          Operazione
038320971205     C                   ELSE
038330950803     C                   MOVE      '02'          Operazione
038340971205     C                   ENDIF
038350971205     C*
038360950803     c                   endif
038370950817     C*
038380950803     C                   ENDSL
038390950817     C*
038400950817     C* eseguo i controlli primari al sfl
038410040429C1176X*****************  EXSR      CtrlBaseS1
038420990506  "   * eseguo gli automatismi possibili sugli imponibile e imposte
038430990421C1176C                   EXSR      RieImpS1
038440980529     C*
038450950817     C* riempio la DS di input con i valori a video
038460950817     C                   EXSR      RIE009DS
038470950724     C*
038480950724     C                   EVAL      LenIn = %Size(ND009DS)
038490950724     C                   EVAL      LenOut = %Size(ND010DS)
038500950725     C                   eval      NrrigaM  = H1nrrigam
038510950726     C                   eval      Nrriga3  = H1nrriga
038520950724     C*
038530950724     C                   CALLB     'NDMVC201'
038540040308     C                   PARM                    Operazione
038550040308     C                   PARM                    LenIn
038560040308     C                   PARM                    ND009DS
038570040308     C                   PARM                    NrRigaM
038580040308     C                   PARM                    NrRiga3
038590950724     C                   PARM      *on           GesMsg
038600950725     C                   PARM                    Esito
038610950724     C                   PARM      'ND010DS'     StrutturaO
038620950724     C                   PARM                    LenOut
038630950724     C                   PARM                    ND010DS
038640010112D0808C                   PARM      '5'           Fun8Mvc099
038650950724     C*
038660980529     C                   IF        Esito    = *On
038670950724     C*
038680950724     C* gestisco la loro emissione
038690950925     c                   clear                   TextMsg
038700980529     C*
038710980529     C                   DOU       Q2BytAva = 0
038720980529     C                   EXSR      RestoreMsg
038730980529     C                   IF        Q2BytAva > 0
038740980529     C                   EXSR      GestErr201
038750980529     C                   ENDIF
038760980529     C                   ENDDO
038770980529     C*
038780980529     C* se c'è l'errore data liquidazione IVA risulta < data
038790971205     C* registrazione e > del datario
038800040429D0611X****************** IF        *in63 = *on or
038810001020D0611C                   If        $InC1(63) = *On  OR
038820980529     C* oppure c'è l'errore sulla quadratura imponibile/aliquota/
038830980529     C* imposta
038840040429D0611X******************           *in95 = *on
038850001020D0611C                             $InC1(95) = *On
038860060613C1939C                             or $InC1(94) = *On
038870971205     C* richiedo l'F11 di forzatura
038880971209     C                   MOVE      '1'           $FC3(11)
038890980529     C                   ENDIF
038900950804     C*
038910980529     C                   else
038920950804     C* imposto i campi del sfl con
038930950804     C* i valori calcolati/aggiunti dalla routine di aggiornamento
038940950804     C                   EXSR      Rep009
038950950725     C*
038960980529     c                   if         Operazione = '05'
038970950803     c                   clear                   h1nrrigam
038980950803     c* cancellazione
038990950803     c                   clear                   H1nrriga
039000980529     c                   else
039010950803     c*aggiornamento
039020950725     c                   eval       H1nrrigam = Nrrigam
039030950726     c                   eval       H1nrriga  = Nrriga3
039040980529     C                   EndIf
039050950725     C*
039060980529     C                   ENDIF
039070021002C1585 *
039080021002  "   * riempio descrizioni della II° riga del sfl
039090021008  "  C                   exsr      RieS1Descr
039100021002C1585 *
039101180105R275 C* Reperisco le regole di detrazione IVA a credito (legate al libro)
039102180105  "  C* ed effettuo i controlli a livello di riga del savf
039103180105  "  c                   Exsr      GesRegDetr
039104180105R275 C*
039110980529     C                   ENDIF
039120070116D2109c* DA FARE PER FORNITORI SEMPRE, PER CLIENTI SOLO PER CLASSE
039130070116  "  C* NAZIONALE
039140070116  "  C                   IF        D1TpSogg='F' or
039150070116D2109C                             (D1TpSogg='C' and RcoItaEst ='0')
039160070116C1716c* emissione videata non bloccnte di eventuali dichiarazioni di
039170040113  "  c* intento che non soddisfano la riga di esenzione
039180040113  "  C* se non ci sono errori
039190040416C1716C                   IF        *in99 = *off   and
039200040429C1754X* e se FORNITORE
039210040429C1754X***                          D1TpSogg ='F' and
039220040416C1716c* e se opzione immissione, copia o modifica
039230040113  "  C                             (Opz015 = '01' or opz015='02'or
039240040113  "  c                              Opz015='03')  and
039250041011c1716c* e se richiesto il controllo della dichiarazione di intenti
039260041011D1675c                             (CtrDich045 = D1TpSogg or
039270041011  "  c                             CtrDich045=*on)and
039280041011D1675X**************************** CtrDich045=*on and
039290041011C1716c* se presente aliquota o riferimento iva
039300040122  "  c                             (s1rifiva <> *blanks or s1aliq <> 0)
039310040113  "  c                   exsr      ExeCtrDich
039320040113C1716C                   endif
039330070116D2109C                   endif
039340950724     C*
039350950801     C* calcolo i totali imposta
039360961009     C                   If        S1Segno = XscDare
039370950801     C                   ADD       S1ImpIVA      TotImpIVA
039380950801     C                   ADD       S1ImpIVAD     TotImpIVAD
039390950801     C                   ELSE
039400950801     C                   SUB       S1ImpIVA      TotImpIVA
039410950801     C                   SUB       S1ImpIVAD     TotImpIVAD
039420950801     C                   EndIf
039430950801     C*
039440950726     C                   setoff                                       05
039450950726     C     OPZ015        ifeq      '05'
039460950727     C     OPZ015        oreq      '04'
039470950726     C                   seton                                        05
039480950726     C                   endif
039490981130     C* imposto gli attributi del sfl
039500981130     C                   EXSR      AtrS1
039510950725     C                   UPDATE    FMTS1
039520950725     C*
039530950724     C     *IN99         IFEQ      *on
039540950724     C                   LEAVE
039550950724     C                   ENDIF
039560950724     C*
039570950724     C                   enddo
039571180105R275 C* Effettuo gli ultimi controlli per le regole iva in detrazione
039572180105  "  C* Reperite.
039573180105  "  C                   If        ExistI32 = *on
039574180105  "  C                   Exsr      CtrRegDetT
039575180105R275 C                   EndiF
039580950724     C*
039590950721     C                   ENDSR
039600021002C1585 ************************************************************
039610021002  "   * riempio descrizioni della II° riga del sfl
039620021002  "   ************************************************************
039630021008  "  C     RieS1Descr    Begsr
039640021002  "   *
039650021002  "   * descrizione codice esenzione/Aliquota
039660021002  "  C                   clear                   S1Descr1
039670021002  "  C                   eval      ALRSocieta = XScSoc
039680021002  "  C                   eval      ALRLibro = C1Libro
039690021002  "  C                   eval      ALRTpReg = C1TpReg
039700021002  "  C                   eval      ALRAliq  = S1Aliq
039710021002  "  C                   eval      ALRRifIva = S1RifIva
039720021002  "  C     K05ALR02      chain     ANALR02L                           23
039730021002  "  C                   if        *in23 =  *off
039740021002  "  C                   movel     ALRDescr      S1Descr1
039750021002  "  C                   endif
039760021002  "   *
039770021002  "   * descrizione causale di riga
039771180105C1585C                   clear                   S1Descr2
039772180105R275 C* Salvo anche se indetraibile e la percentuale
039773180105  "  C                   Clear                   savInded
039774180105R275 C                   Clear                   savIndedP
039775180105C1585C                   if        S1CausRig <> *blank
039800021008  "   *
039810021008  "   *   non voglio sporcare il rec di Anope della
039820021008  "   *   causale di testata eventualmente recuperato
039830021008  "   *   perchè verrà utilizzato nel corso del pgm
039840021008  "  C                   clear                   SAVAnope
039850021008  "  C                   eval      SAVAnope = Anope
039860021008  "   *
039870021008  "  C                   eval      $LenOut = %size($Anope)
039880021008  "  C     *jobrun       move      C1DtReg       DataISO1
039890021008  "  C                   callb     'NDMVC003'
039900021008  "  C                   parm                    XScSoc
039910021008  "  C                   parm                    S1CausRig
039920021008  "  C                   parm                    DataISO1
039930021008  "   * solo riga
039940021008  "   * (una causale iva con Tipologia=Dettagli righe IVA -OPETPC='2'-)
039950021008  "   *  ha sempre un Utilizzo=Solo riga -OPETESRIG='1'-)
039960021008  "  C                   parm      '1'           $Uso
039970021008  "  C                   parm      *off          $GesMsg
039980021008  "  C                   parm                    $Esito
039990021008  "  C                   parm      'ANOPE'       $Struttura
040000021008  "  C                   parm                    $Anope
040010021008  "  C                   parm                    $LenOut
040020021008  "  C                   parm      *on           $Refresh
040030021008  "   *
040040021008  "  C                   if        $Esito <> *On
040041180105C1585C                   movel     OP_Descr      S1Descr2
040042180105R275 C                   movel     OP_Inded      SavInded
040043180105R275 C                   movel     OP_IndedP     SavIndedP
040044180105C1585C                   endif
040070021008  "   *
040080021008  "   *   ripristino il rec di Anope della causale di testata
040090021008  "   *   eventualmente recuperato
040100021008  "   *   perchè verrà utilizzato nel corso del pgm
040110021008  "  C                   eval      Anope = SAVAnope
040120021008  "   *
040130021008  "  C                   endif
040140021002  "   *
040150021008C1585C     XRieS1Desc    Endsr
040160040113C1716 ************************************************************
040170040113  "   * Esecuzione chiamata al driver controllo dichiarazioni intento
040180040113C1716 ************************************************************
040190040113     C     ExeCtrDich    Begsr
040200040113     c*
040210040113     c                   reset                   PJXX011DS
040220040113     c                   eval      SocietaX11 = xscsoc
040230040113     c                   eval      CfX11 =D1TpSogg
040240040113     c                   eval      kscX11=c1ksc
040250040113     c* reperisco ultimo giorno del mese della data plafond
040260040113     c                   move      C1AaPlaf      AnnoPl            2
040270040113     c                   move      C1MmPlaf      MesePl            2
040280040113     c                   clear                   WrkdataA          6
040290040113     c                   clear                   WrkdataD          6 0
040300040113     c                   eval      WrkDataA= '01' + Mesepl + Annopl
040310040113     c                   move      WrkDataA      WrkdataD
040320040113     c     *jobrun       move      WrkdataD      DataIso
040330040113     C                   move      Dataiso       Xdat
040340040113     c                   move      4             xcod              2 0
040350040113     C                   CALL      'XDATCD'
040360040113     C                   parm                    xcod
040370040113     C                   PARM                    XDAT             10
040380040113     C                   move      xdat          DtRifX11
040390040113     c*
040400040122     c                   eval      aliqX11  =s1aliq
040410040115     c                   eval      rifivaX11=s1RifIva
040420040115     c                   eval      TpRegx11 =c1TpReg
040430040114     c                   eval      ImportoX11=s1Importo
040440040113     c                   eval      emettiX11=*on
040450040115     c                   eval      inzX11 = $inzX11
040460040113     c*
040470040113     c                   call      'PJXX011R'
040480040113     C                   parm                    PJXX011DS
040490040421D1591c* se emesso il video, imposto il flag ad *off, altrimenti no
040500040421D1591c                   if        ErroreX11=*on
040510040115     C                   Eval      $InzX11    =  *off
040520040421D1591c                   endif
040530040113     C*
040540040113C1716c                   endSR
040550940207     C************************************************************
040560950616     C* Chiamata al modulo di reperimento registrazione + allocaz.
040570940207     C************************************************************
040580951010     C     AltriDati     BEGSR
040590010409D0821 *
040600010409  "   * resetto le chiavi salvate dall'ultima chain di NDDRVMOV
040610010409  "  C                   callb     'NDDRVMOV'
040620010409  "  C                   parm                    *Omit                          Sys
040630010409  "  C                   parm                    *Omit                          NrAsReg
040640010409  "  C                   parm                    *Omit                          NrRigaM
040650010409  "  C                   parm                    *Omit                          StrutturaO
040660010409  "  C                   parm                    *Omit                          LenOut
040670010409  "  C                   parm      '0'           Codoper
040680010409D0821 *
040690010409     C*
040700950719     C* Aggancio NDMOV per altri dati
040710950721     C                   eval      NrRigam = 1
040720950721     C                   EVAL      LenOut = %Size(NDMOV00F)
040730951010     C                   eval      StrutturaO = 'NDMOV'
040740950721     C                   eval      Codoper    = '1'
040750950721     C                   CALLb     'NDDRVMOV'
040760040309     C                   PARM                    RegSys
040770040309     C                   PARM                    RegNrAsReg
040780980210      * Con CodOper='1' il driver esegue una Chain su NDMOV00F
040790980210      * con una chiave che comprende anche il N°Riga
040800980210      * quindi è necessario passare come parametro il N°Riga
040810980210     C                   PARM      1             NrRigaM
040820950721     C                   PARM                    StrutturaO
040830950721     C                   PARM                    LenOut
040840950721     C                   PARM                    Codoper
040850950721     C                   PARM                    NDMOV00F
040860950721     C*
040870950721     C                   IF        Codoper = '9'
040880950719     C                   MOVE      *ON           $FINE
040890950719     C                   MOVE      '1'           ERR015
040900950719     C                   EXSR      EndPgm
040910950719     C                   ENDIF
040920950719     C*
040930950719     C* Aggancio NDPAS per altri dati
040940950721     C                   eval      PASSys = REGSys
040950950721     C                   eval      PASNrasReg = REGNrasReg
040960950721     C                   eval      PASNrRigam = 1
040970950721     C     K03PAS01      CHAIN     NDPAS01L                           21
040980950719     C                   IF        *in21 = *on
040990050215C1817C                   eval      $PasOk = *off
041000050215  "  C                   else
041010050215C1817C                   eval      $PasOk = *on
041020950719     C                   ENDIF
041030950719     C*
041040950616     C                   ENDSR
041050040601D1617 ************************************************************
041060040601      * controlli se modificata data di registrazione per i ratei
041070040601D1617 ************************************************************
041080040601     C     CtrlRatei     Begsr
041090040601     c*
041100040607     c                   reset                   ndc326ds
041110040607     c                   eval      Societa326=xscsoc
041120040607     c                   eval      Ctb326 = Ctb015
041130040607     c     *jobrun       move      d2dtreg       DataIso4
041140040607     c                   move      DataIso4      Dtreg326
041150040607     c                   eval      Sys326=REgsys
041160040607     c                   eval      Nrasreg326=RegNrasreg
041170040607     c                   eval      kpjbu=ndc326ds
041180040607     c                   call      'NDC326R'
041190040607     C                   PARM                    kpjba
041200040607     c                   eval      ndc326ds=kpjbu
041210040607     c*
041220040601D1617c                   endsr
041230950816     C************************************************************
041240950816     C* Reperisce i bottoni della registrazione che interessano al pgm
041250950816     C************************************************************
041260950816     C     RepBottReg    BEGSR
041270950816     C*
041280950816     C                   CLEAR                   Fun
041290950816     C* flag testata modificabile
041300950816     C                   EVAL      Fun(8) = '2'
041310950816     C*
041320950816     C                   EVAL      LenOut = %Size(ND003DS)
041330950816     C                   CALLB     'NDMVC199'
041340950816     C                   PARM                    Fun
041350950816     C                   PARM                    ND003DS
041360950816     C                   PARM                    LenOut
041370950816     C*
041380950816     C                   ENDSR
041390950816     C************************************************************
041400950816     C* Reperisce lo stato dei bottoni del 1° movimento
041410950816     C* che interessano tutto il pgm
041420950816     C************************************************************
041430950816     C     RepBottMov    BEGSR
041440950816     C*
041450950816     C                   CLEAR                   Fun
041460950816     C* flag movimento modificabile
041470950816     C                   EVAL      Fun(6) = '2'
041480950816     C*
041490950816     C                   EVAL      LenOut = %Size(ND006DS)
041500950816     C                   CALLB     'NDMVC099'
041510950816     C                   PARM                    NrRigaM
041520950816     C                   PARM      '0'           Ctr
041530950816     C                   PARM                    Fun
041540950816     C                   PARM                    ND006DS
041550950816     C                   PARM                    LenOut
041560950816     C*
041570950816     C                   ENDSR
041580950719     C************************************************************
041590950719     C* RIEMPIMENTO VIDEATA D1 da file
041600950719     C************************************************************
041610950719     C     RIED1         BEGSR
041620950719     C*
041630950726     C                   eval      D1ksc   = movksc
041640950726     C                   eval      D1tpsogg= movsnatura
041650950719     C                   eval      D1unita = regUnita
041660950719     C                   eval      D1CausTes = RegCausTes
041670960527     C                   EVAL      WNrAsCol = REGNrAsCol
041680970122     C                   EVAL      Wflgreg = REGflgreg
041690950719     C*
041700950719     C                   ENDSR
041710950719     C************************************************************
041720950721     C* RIEMPIMENTO VIDEATA D2 da file
041730950719     C************************************************************
041740950719     C     RIED2         BEGSR
041750950719     C* Da Ndreg
041760950719     C                   EVAL      D2CausTes = RegCausTes
041770950719     C                   EVAL      D2CondPag = RegCondPag
041780050215C1817 *
041790050215  "   * se la condizione di pagamento non esiste oppure è incompleta
041800050215  "   * non esiste nemmeno la partita e ndmvc403 si spaccherebbe
041810050215  "  C                   exsr      CtrCondPag
041820050215C1817 *
041830950720     C                   if        regpagman = *on
041840950720     C                   EVAL      D2PagMan  = xscon
041850950720     C                   else
041860950720     C                   EVAL      D2PagMan  = xscoff
041870950720     C                   endif
041880950719     C                   EVAL      D2Classe  = RegClasse
041890950914     C     RegDtReg      IFGT      Dt0
041900960202     C                   CALLB     'XDT4000'
041910950914     C                   PARM      RegDtReg      XDTAMG           10
041920950914     C     D2DtReg       PARM                    XDTGMA            6 0
041930950914     C                   PARM      2             XDTSTA            1 0
041940950914     C                   ELSE
041950950914     C                   MOVE      *zero         D2DtReg
041960950914     C                   ENDIF
041970950719     C                   EVAL      D2NrReg   = RegNrReg
041980950719     C                   EVAL      D2SerReg  = RegSerReg
041990950914     C     RegDtDoc      IFGT      Dt0
042000960202     C                   CALLB     'XDT4000'
042010950914     C                   PARM      RegDtDoc      XDTAMG           10
042020950914     C     D2DtDoc       PARM                    XDTGMA            6 0
042030950914     C                   PARM      2             XDTSTA            1 0
042040950914     C                   ELSE
042050950914     C                   MOVE      *zero         D2DtDoc
042060950914     C                   ENDIF
042070040429C1547X* la data decorrenza pagamento non viene più memorizzata in
042080040429  "  X* MOVDtLib ma in REGDtLib2
042090040429C1547X**ex C1494 movDtLib      IFGT      Dt0
042100020226C1547C     REGDtLib2     IFGT      Dt0
042110020226C1494C                   CALLB     'XDT4000'
042120040429C1547X**ex C1494         PARM      movdtlib      XDTAMG           10
042130020226C1547C                   PARM      REGDtLib2     XDTAMG           10
042140020226C1494C     D2Dtlib       PARM                    XDTGMA            6 0
042150010621  "  C                   PARM      2             XDTSTA            1 0
042160010621  "  C                   ELSE
042170010621  "  C                   MOVE      *zero         D2Dtlib
042180010621C1494C                   ENDIF
042190950719     C                   EVAL      D2NrDoc   = RegNrDoc
042200950719     C                   EVAL      D2SerDoc  = RegSerDoc
042210950719     C                   EVAL      D2Divisa  = RegDivisa
042220950719     C                   EVAL      D2Cambio  = RegCambio
042230950914     C     RegDtCoCo     IFGT      Dt0
042240960202     C                   CALLB     'XDT4000'
042250950914     C                   PARM      RegDtCoCo     XDTAMG           10
042260950914     C     D2DtCoCo      PARM                    XDTGMA            6 0
042270950914     C                   PARM      2             XDTSTA            1 0
042280950914     C                   ELSE
042290950914     C                   MOVE      *zero         D2DtCoCo
042300950914     C                   ENDIF
042310950914     C     RegDtCoAn     IFGT      Dt0
042320960202     C                   CALLB     'XDT4000'
042330950914     C                   PARM      RegDtCoAn     XDTAMG           10
042340950914     C     D2DtCoAn      PARM                    XDTGMA            6 0
042350950914     C                   PARM      2             XDTSTA            1 0
042360950914     C                   ELSE
042370950914     C                   MOVE      *zero         D2DtCoAn
042380950914     C                   ENDIF
042390981230     C     REGDtLib1     IFGT      Dt0
042400981229     C                   CALLB     'XDT4000'
042410981230     C                   PARM      REGDtLib1     XDTAMG           10
042420981229     C     D2DtRfCam     PARM                    XDTGMA            6 0
042430981229     C                   PARM      2             XDTSTA            1 0
042440981229     C                   ELSE
042450981229     C                   MOVE      *zero         D2DtRfCam
042460981229     C                   ENDIF
042470950719     C* Da Ndmov
042480950719     C                   EVAL      D2tpsogg  = MOVSnatura
042490950719     C                   EVAL      D2ksc     = MOVksc
042500950721     C                   EVAL      D2importD = MOVimportd
042510950721     C                   EVAL      D2imporTO = MOVimporto
042520950719     C                   EVAL      D2divisa  = MOVDivisa
042530950719     C                   EVAL      D2cambio  = MOVcambio
042540980119     C                   EVAL      D2DesBrev = MOVDesBrev
042550950719     C* Da NdPas
042560950914     C     PasDtPar      IFGT      Dt0
042570960202     C                   CALLB     'XDT4000'
042580950914     C                   PARM      PasDtPar      XDTAMG           10
042590950914     C     D2DtPar       PARM                    XDTGMA            6 0
042600950914     C                   PARM      2             XDTSTA            1 0
042610950914     C                   ELSE
042620950914     C                   MOVE      *zero         D2DtPar
042630950914     C                   ENDIF
042640950719     C                   EVAL      D2NrPar   = PasNrPar
042650950719     C                   EVAL      D2SerPar  = PasSerPar
042660010820C1512 *
042670010820  "   * divisa di partita
042680010820  "  C                   eval      D2DivisaP = PASDivisa
042690010820C1512 *
042700001025     C*
042710950921     C* se non sto gestendo un modello
042720950921     C                   IF        Ctb015 <> 'MD'
042730060803D2031 * reperisco par.mod. HBEffPas
042740060803D2031C                   exsr      RepHBEffPas
042750950808     C* testo se esistono effetti presentati
042760950809     C                   CALLB     'NDMVC403'
042770950809     C                   PARM                    MovSocieta
042780950809     C                   PARM                    MovCtb
042790950809     C                   PARM                    MovSNatura
042800950809     C                   PARM                    MovKcc
042810950809     C                   PARM                    MovKsc
042820950809     C                   PARM                    PaSDtPar
042830950809     C                   PARM                    PaSNrPar
042840950809     C                   PARM                    PaSSerPar
042850950809     C                   PARM                    *omit
042860950817     C                   PARM                    *omit
042870950824     C                   PARM                    Risposta
042880000330C1350C                   PARM                    EffNoCol
042890060803D2031C                   PARM                    EffRicHB
042900950822     C                   PARM                    *omit
042910950822     C                   PARM                    *omit
042920950808     C*
042930950809     C* nel qual caso bisogna forzare le rate manuali
042940950809     C                   MOVE      Normale       D2PagManA
042950950824     C                   MOVE      *off          EffPres
042960000330C1350C                   Move      *Off          $EffNoCol
042970000330C1350C* se esistono effetti presentati o accorpati collegati alla reg.
042980951004     C                   IF        Risposta = '2'        or
042990951004     C                             Risposta = '3'
043000000330C1350 * oppure esistono effetti non collegati (di qualsiasi tipo)
043010000330  "   * a questa registraz. ma collegati alla partita indicata
043020000330  "  C                             OR  EffNoCol <> '0'
043030000330C1350 * forzo la gestione manuale delle rate
043040950809     C                   EVAL      D2PagMan  = xscon
043050000330C1350 *
043060000330  "   * se esistono effetti presentati o accorpati collegati alla reg.
043070000330  "  C                   IF        Risposta = '2'        or
043080000330  "  C                             Risposta = '3'
043090000330  "   * lo memorizzo per proteggere il campo Rate manuali a SI e per
043100000330C1350 * evitare una rigenerazione delle rate
043110950824     C                   MOVE      *on           EffPres
043120000330C1350C                   ENDIF
043130000330  "   *
043140000410  "   *
043150000330  "   * memorizzo che esistono degli effetti non collegati alla
043160000330  "   * registrazione ma collegati alla partita
043170000404  "   * per poetr proteggere il flag (in ATRD1)
043180000330  "  C                   If        EffNoCol   <> '0'
043190000410  "   *
043200000330  "  C                   Move      *On           $EffNoCol
043210000410  "   *
043220000410  "  C                   EndIf
043230000330C1350 *
043240991214     C                   ENDIF
043250950906     C*
043260950921     C                   ENDIF
043270950921     C*
043280950906     C* se non sono in immissione
043290950906     C                   IF        Opz015 <> '01'        and
043300950906     C* né in copia
043310960729     C                             Opz015 <> '03'           or
043320951012     C* oppure in questi casi di immissione speciale :
043330951012     C*  autofattura in errore
043340951012     C                             ImmExp015 = '1'
043350950906     C* imposto la chiave di NDPPA ai valori video, in modo da non fare il
043360950906     C* ctrl successivo sulla coerenza riferimenti partita che deve essere
043370950906     C* eseguito in immissione o a cambio di uno di questi valori
043380980128     C                   Eval      $PPAKcc    = MovKcc
043390980128     C                   Eval      $PPAKsc    = MovKsc
043400980128     C                   Eval      $PPADtPar  = PasDtPar
043410980128     C                   Eval      $PPANrPAr  = PasNrPar
043420980128     C                   Eval      $PPASerPar = PasSerPar
043430980128     C                   ENDIF
043440950809     C*
043450040429C1429X****************** ENDSR
043460001025C1429C     XRieD2        ENDSR
043470050215C1817 ************************************************************
043480050215  "   * se la condizione di pagamento non esiste oppure è incompleta
043490050215  "   * emetto window bloccante
043500050215  "   * ( altrimenti ndmvc403 si spaccherebbe)
043510050215  "   ************************************************************
043520050215  "  C     CtrCondPag    Begsr
043530050215  "   *
043540050215  "   * Se la partita legata alla registraz. è inesistente e
043550051110C1817 * cond.pagam. valorizzata blocco l'utente con una window
043560051110D1895 * (blocco utente anche quando cond.pagam. è vuota)
043570051110C1817 * (altrimenti il pgm si rompe)
043580050215  "   * (se la partita esiste ma la cond. pagam. è errata
043590050215  "   *  il controllo della cond. pagam. viene già fatto
043600051110C1817 *  e il pgm non si rompe)
043610051110D1895X**ex C1817 ******* if        $PasOk = *off    and
043620051110D1895X**ex C1817 *******           D2CondPag <> *blank
043630051110D1895C                   if        $PasOk = *off
043640051110C1817 *
043650050215  "  C                   eval      CDRSocieta = XScSoc
043660050215  "  C                   eval      CDRCondPag = D2CondPag
043670050215  "  C     K02CDR01      chain     ANCDR000                           21
043680050215  "   *
043690050215  "   *  Cond. pagam. inesistente in anagrafica righe
043700050215  "  C                   if        *in21 = *on
043710050215  "   *
043720050215  "   *   window d'avevrtimento bloccante
043730050215  "  C                   clear                   XMsgDS
043740050215  "  C                   clear                   XMsgTxt
043750050215  "   *     nr messaggio precodificato
043760050215  "  C                   movel     0             MsgMsg
043770050215  "   *     esistenza risposta
043780050215  "  C                   movel     'N'           MsgRsp
043790050215  "   *     emissione a video
043800050215  "  C                   movel     'S'           MsgEmv
043810050215  "   *     riga da cui parte la finestra
043820050215  "  C                   z-add     10            MsgRgp
043830050215  "   *     gestione analisi del problema (F2)
043840050215  "  C                   movel     'N'           MsgLck
043850050215  "   *     non utilizzato
043860050215  "  C                   movel     'N'           MsgCnl
043870050215  "   *     mandare o meno alla coda del display/utente
043880050215  "  C                   movel     'N'           MsgVid
043890050215  "   *     mandare o meno all'operatore
043900050215  "  C                   movel     'N'           MsgOpe
043910050215  "   *     emissione in stampa
043920050215  "  C                   movel     'N'           MsgStp
043930050215  "   *
043940050215  "  C                   call      'XMSGR'
043950050215  "  C                   parm                    XMsgDS
043960050215  "  C                   parm                    XMsgTxt         135
043970050215  "  C                   parm                    XMsgInz
043980050215  "   *     PRO2054-Condizione di pagamento &1 inesistente o incompleta
043990050215  "  C                   parm      'PRO2054'     XMsgCd1           7
044000050215  "  C                   parm                    XMsgCd2           7
044010050215  "  C                   parm                    XMsgCd3           7
044020050215  "  C                   parm                    XMsgSn1           7
044030050215  "  C                   parm                    XMsgCdTxT        63
044040050215  "  C                   parm                    MsgCdTit         14
044050050215  "  C                   parm                    MsgClPrtf         1
044060050215  "  C                   parm      D2CondPag     VarMsg1         100
044070050215  "  C                   parm                    VarMsg2         100
044080050215  "  C                   parm                    VarMsg3         100
044090050215  "   *
044100050215  "   *   Vado FINE PGM e dico al chiamante di non confermare nulla
044110050215  "  C                   move      '1'           ERR015
044120050215  "  C                   exsr      EndPgm
044130070531C1817 *
044140070531D2208 *
044150070531  "   * partita legata alla registraz., inesistente e
044160070531  "   * Cond. pagam. esistente in anagrafica righe
044170070531  "  C                   else
044180070531  "   *   window d'avevrtimento bloccante
044190070531  "  C                   clear                   XMsgDS
044200070531  "  C                   clear                   XMsgTxt
044210070531  "  C                   movel     0             MsgMsg
044220070531  "  C                   movel     'N'           MsgRsp
044230070531  "  C                   movel     'S'           MsgEmv
044240070531  "  C                   z-add     10            MsgRgp
044250070531  "  C                   movel     'N'           MsgLck
044260070531  "  C                   movel     'N'           MsgCnl
044270070531  "  C                   movel     'N'           MsgVid
044280070531  "  C                   movel     'N'           MsgOpe
044290070531  "  C                   movel     'N'           MsgStp
044300070531  "  C                   call      'XMSGR'
044310070531  "  C                   parm                    XMsgDS
044320070531  "  C                   parm                    XMsgTxt         135
044330070531  "  C                   parm                    XMsgInz
044340070531  "   *     COS2660-Non è possibile generare la partita legata al cli/for
044350070531  "  C                   parm      'COS2660'     XMsgCd1           7
044360070601  "   *     NDC0988-Non è possibile gestire effetti se il relativo importo è 0
044370070601  "  C                   parm      'NDC0988'     XMsgCd2           7
044380070601  "   *     COS2661-Si consiglia di verificare la condizione di pagamento
044390070601  "  C                   parm      'COS2661'     XMsgCd3           7
044400070531  "  C                   parm                    XMsgSn1           7
044410070531  "  C                   parm                    XMsgCdTxT        63
044420070531  "  C                   parm                    MsgCdTit         14
044430070531  "  C                   parm                    MsgClPrtf         1
044440070531  "  C                   parm                    VarMsg1         100
044450070531  "  C                   parm                    VarMsg2         100
044460070531  "  C                   parm                    VarMsg3         100
044470070531  "   *
044480070531  "   *   Vado FINE PGM e dico al chiamante di non confermare nulla
044490070531  "  C                   move      '1'           ERR015
044500070531  "  C                   exsr      EndPgm
044510070531D2208 *
044520070531C1817C                   endif
044530070531  "  C                   endif
044540050215  "   *
044550050215C1817C     XCtrCondPag   Endsr
044560060803D2031 ************************************************************
044570060803  "   * Reperisco par.mod. HBEffPas
044580060803  "   ************************************************************
044590060803  "  C     RepHBEffPas   Begsr
044600060803  "   *
044610060803  "   * reperisco parm.mod solo 1 volta
044620060803  "  C                   if        $RepHBEffPas <> *on
044630060803  "   *
044640060803  "  C                   clear                   ndck08ds
044650061011  "  C                   clear                   $EffRicHB_I
044660060803  "   *
044670060803  "  C                   callb     'XPAR'
044680060803  "  C                   parm                    XScSoc
044690060803  "  C                   parm      'HBEFFPAS'    XPaPar            8
044700060803  "  C                   parm                    XPaOut
044710060803  "  C                   parm      *off          XPaErr            1
044720060803  "  C                   parm      ' '           XPaRic            1
044730060803  "  C                   parm                    *omit
044740060803  "  C                   parm                    *omit
044750060803  "   *          XPar non emette win se non trova parametro
044760060803  "   *          (potrebbe non essere usato l'home banking)
044770060803  "  C                   parm      *off          XPaMsg            1
044780060803  "   *
044790060803  "  C                   if        XPaErr = *off
044800060803  "  C                   movel     XPaOut        ndck08ds
044810061011  "  C                   movel     TpAvvK08      $EffRicHB_I
044820060803  "  C                   endif
044830060803  "   *
044840060803  "  C                   eval      $RepHBEffPas = *on
044850060803  "  C                   endif
044860061011  "   *
044870061011  "   * ad ogni chiamata a ndmvc403, rinnovo EffRicHb=EffRicHb_I
044880061011  "   * ($EffRicHb_I : tipo avviso di ricezione (par. mod HBEFFPAS)
044890061011  "   *  EffRicHb    : parametro passato e ricevuto da ndmvc403
044900061011  "   *  se sono già stato in pn iva, ndmvc403 ha già controllato se
044910061011  "   *  l'effetto è riconciliato e EffRicHb=ultimo valore restituito)
044920061011  "  C                   movel     $EffRicHB_I   EffRicHB
044930060803  "   *
044940060803D2031C     XRepHBEffPas  Endsr
044950020304C1530C************************************************************
044960020304  "  C* memo chiavi dell'associazione tra registrazione e Laguna
044970020304  "  C************************************************************
044980020304C1530C     RieKeyLgn     BEGSR
044990020304     C*
045000020304     C                   SELECT
045010020304     C*  Doc.IVA Fornitore : (F)attura o (N)ota/estremi doc./sottoconto
045020020304     C                   WHEN      D1TpSogg = 'F'
045030020304     C                   MOVEL(P)  XSCSOC        KeyLgn(1)
045040020304     C*    decido se fattura o nota
045050020304     C                   SELECT
045060020304     C*     fattura
045070020304     C                   WHEN      OpeTpDocI = '0' or
045080060303D1937C                             OpeTpDocI = '2' or
045090060303     C                             OpeTpDocI = '3' or
045100020304     C                             OpeTpDocI = '5' or
045110020304     C                             OpeTpDocI = '6'
045120020304     C                   MOVEL(P)  'F'           KeyLgn(2)
045130020304     C*     nota
045140020304     C                   WHEN      OpeTpDocI = '1' or
045150020304     C                             OpeTpDocI = '4' or
045160020304     C                             OpeTpDocI = '7'
045170020304     C                   MOVEL(P)  'N'           KeyLgn(2)
045180020304     C                   ENDSL
045190020304     C                   MOVEL(P)  RegDtDoc      KeyLgn(3)
045200020304     C                   MOVEL(P)  D2NrDoc       KeyLgn(4)
045210020304     C                   MOVEL(P)  D2SerDoc      KeyLgn(5)
045220020304     C                   MOVEL(P)  D1Ksc         KeyLgn(6)
045230020304     C*
045240020304     C*  Doc.IVA Cliente   : estremi doc.
045250020304     C                   WHEN      D1TpSogg = 'C'
045260020304     C                   MOVEL(P)  XSCSOC        KeyLgn(1)
045270020304     C                   MOVEL(P)  RegDtDoc      KeyLgn(2)
045280020304     C                   MOVEL(P)  D2NrDoc       KeyLgn(3)
045290020304     C                   MOVEL(P)  D2SerDoc      KeyLgn(4)
045300020304     C*
045310020304     C                   ENDSL
045320020304     C*
045330020304C1530C                   ENDSR
045340030210D1270C************************************************************
045350030210  "  C* memo descrittori
045360030210  "  C************************************************************
045370030210D1270C     RieDesLgn     BEGSR
045380030210     C*
045390030210     C                   SELECT
045400030210     C*  Doc.IVA Fornitore : (F)attura o (N)ota/estremi doc./sottoconto
045410030210     C                   WHEN      D1TpSogg = 'F'
045420030210     C*   Ragione Sociale Fornitore
045430030210     C                   eval      DesLgn(1) = D1RagSocL
045440030210     C*   Partita Iva Fornitore
045450030210     C                   eval      DesLgn(2) = D1PartIVA
045460030210     C*   Serie registrazione
045470030210     C                   eval      DesLgn(3) = D2SerReg
045480030210     C*   Numero registrazione
045490030210     C                   MOVEL     D2NrReg       DesLgn(4)
045500030210     C*   Data registrazione
045510030210     C     *JobRun       Move      D2DtReg       DataISO
045520030210     C                   MOVEL     DataISO       DesLgn(5)
045530070213C2005C*   Codice fiscale
045540070213C2005C                   MOVEL     D1CdFisc      DesLgn(6)
045550030210     C*
045560030210     C*  Doc.IVA Cliente   : estremi doc.
045570030210     C                   WHEN      D1TpSogg = 'C'
045580030210     C*   Codice Cliente
045590030210     C                   eval      DesLgn(1) = D2Ksc
045600030210     C*   Ragione Sociale Cliente
045610030210     C                   eval      DesLgn(2) = D1RagSocL
045620030210     C*   Partita Iva Cliente
045630030210     C                   eval      DesLgn(3) = D1PartIVA
045640030210     C*   Serie registrazione
045650030210     C                   eval      DesLgn(4) = D2SerReg
045660030210     C*   Numero registrazione
045670030210     C                   MOVEL     D2NrReg       DesLgn(5)
045680030210     C*   Data registrazione
045690030210     C     *JobRun       Move      D2DtReg       DataISO
045700030210     C                   MOVEL     DataISO       DesLgn(6)
045710070213C2005C*   Codice fiscale
045720080424D2379C*   Flag natura ordine, Agente 1 e descrizione agente 1
045730080424  "  C*   non ce li ho in primanota per cui lascio 3 buchi
045740080424  "  C                   eval      DesLgn(10) = D1CdFisc
045750080424D2379C*** ex C2005       MOVEL     D1CdFisc      DesLgn(7)
045760030210     C*
045770030210     C                   ENDSL
045780030210     C*
045790030210D1270C                   ENDSR
045800950721     C************************************************************
045810950725     C* Caricamento subfile
045820950721     C************************************************************
045830950724     C     RieS1         BEGSR
045840971216     C*
045850971216     C                   CLEAR                   FmtS1
045860950721     C*
045870950721     C* Carico da NDIVA fino a fine file
045880950721     C* prima lettura 1, le altre letture '2' per eseguire read
045890950721     C*
045900951003     C                   MOVE      *blank        Volta1            1
045910950721     C                   eval      Codoper    = '1'
045920040309     C     Codoper       DOWNE     '9'
045930950721     C*
045940950721     C                   EVAL      LenOut = %Size(NDIVA00F)
045950951003     C                   eval      StrutturaO = 'NDIVA'
045960950721     C                   CALLb     'NDDRVIVA'
045970040308     C                   PARM                    RegSys
045980040308     C                   PARM                    RegNrAsReg
045990040308     C                   PARM                    *omit
046000040308     C                   PARM                    *omit
046010040308     C                   PARM      '1'           Lock
046020040308     C                   PARM                    StrutturaO
046030040308     C                   PARM                    LenOut
046040040308     C                   PARM                    Codoper
046050040308     C                   PARM                    NDIVA00F
046060040308     C*
046070040308     C                   IF        Codoper <> '9'
046080040308     C                   eval      H1NrRigaM=IvaNrRigaM
046090040308     C                   eval      H1NrRiga=IvaNrRiga
046100040308     C                   eval      S1Causrig=IvaCausRig
046110950724     C                   eval      S1Rifiva=IvaRifIva
046120950724     C                   eval      S1Aliq=IvaAliq
046130970627     C                   Z-add     IvaImponib    S1Importo
046140970627     C                   Z-add     IvaImpond     S1Importd
046150950724     C                   eval      S1Impiva  = IvaImporto
046160950724     C                   eval      S1Impivad = IvaImportd
046170040308     C                   if        IvaDare = 1
046180040308     C                   eval      S1Segno = XscDare
046190040308     C                   else
046200040308     c                   eval      S1Segno = XscAvere
046210040308     C                   endif
046220040308     C*
046230040308     C* riempio le date del sflctl con quelle relative, prese dal 1° rcd
046240040308     C* di NDIVA letto, tanto non cambiano per tutti i successivi
046250040308     C     Volta1        IFEQ      *blank
046260040308     C* a meno che non sia un'immissione tramite modello
046270040308     C     D1TabPNot     ANDEQ     *blank
046280990709D0154C* a meno che non si tratti di un passaggio da prov a def
046290990709D0154C     Opz015        AndNe     '22'
046300990816C1218C* a meno che non si tratti di un passaggio da prov a ges
046310990816C1218C     Opz015        AndNe     '23'
046320040308     C     IVADtRif      IFNE      Dt0
046330960202     C                   CALLB     'XDT4000'
046340951003     C                   PARM      IVADtRif      XDTAMG           10
046350951003     C     C1DtRif       PARM                    XDTGMA            6 0
046360951003     C                   PARM      2             XDTSTA            1 0
046370040308     C                   ELSE
046380951003     C                   Z-ADD     0             C1DtRif
046390040308     C                   ENDIF
046400040308     C     IVADtComp     IFNE      Dt0
046410040429C1320X*****              CALLB     'XDT4000'
046420040429  "  X*****              PARM      IVADtComp     XDTAMG           10
046430040429  "  X*****Data60        PARM                    XDTGMA            6 0
046440040429  "  X*****              PARM      2             XDTSTA            1 0
046450000127  "  C                   Move      IVADtComp     DataISO
046460000127C1320C     *DMY          Move      DataISO       Data60
046470040308     C                   ELSE
046480951003     C                   Z-ADD     0             Data60
046490040308     C                   ENDIF
046500951003     C                   MOVE      Data60        DataAlfa
046510951003     C                   Z-ADD     Mese          C1MMPlaf
046520951003     C                   Z-ADD     Anno          C1AAPlaf
046530971212     C* flag IVA sospesa
046540971212     C                   MOVE      IVASosp       C1Sosp
046550980119      * Descrizione breve movimento
046551121220C2218 * La imposto da ndiva sse non sono in immissione
046552121220C2218C                   If        OPZ015 <>'01'
046560980119     C                   eval      C1DesBrev=IvaDesBrev
046561121220C2218C                   Endif
046570980119     C                   If        OPZ015 <>'05'  And
046580980119     C                             OPZ015 <>'04'
046590980119      * Imposto descriz. della causale se la causale lo richiede
046600010529     C                   If        C1DesBrev = *Blank  And
046610040429C1492X****                         OpeDesMov = '2'
046620010529C1492C*
046630010529  "  C                             (OpeDesMov = '2'   or
046640010529C1492C                              OpeDesMov = '3'     )
046650980119     C                   Eval      C1DesBrev = OpeDesBrev
046660980119     C                   EndIf
046670980119     C                   EndIf
046680951003     C                   MOVE      *on           Volta1
046690040308     C                   ENDIF
046700080125C2072C*
046710080125  "  C* flag trattamento note in elenchi
046720080125C2072C                   Eval      C1Elenchi   = %SubSt(IVAflag5A:1:1)
046730021008C1585 *
046740021008  "   * riempio descrizioni della II° riga del sfl
046750021008  "  C                   exsr      RieS1Descr
046760021008C1585 *
046770950816     C*
046780950816     C* reperisco i bottoni per ogni movimento
046790950816     C                   Z-ADD     H1NrRigaM     NrRigaM
046800950816     C                   EXSR      RepBottMov
046810950816     C*
046820950816     C* se siamo in visualizzazione o annullamento
046830040308     C                   IF        *in05 = *on           or
046840040308     C* oppure la riga non è modificabile
046850040308     C                             FldMod = *off
046860040308     C* memorizzo che la riga deve essere protetta
046870040308     C                   EVAL      H1RigaPR = *on
046880040308     C                   ENDIF
046890950816     C*
046900950816     C* imposto gli attributi della riga
046910950816     C                   EXSR      ATRS1
046920950816     C*
046930950816     C                   ADD       1             S1NRR
046940950724     C                   WRITE     FMTS1
046950950725     C* lettura successiva
046960950725     C                   eval      Codoper    = '2'
046970961127     c*
046980040308     C                   ENDIF
046990950725     c*
047000040308     C                   ENDDO
047010991130C1277C*
047020991130  "  C* se si tratta di un passaggio P a D (a G di una fattura non può
047030991130  "  C*  essere fatto)
047040991130  "  C                   IF        Opz015 = '22'              and
047050991130  "  C* e siamo in una registrazione acquisti
047060991130  "  C* (per qualunque regime IVA)
047070040429C1277C                             %subst(C1SerReg:1:1) = '1'
047080040429D1600 *
047090040429  "   * se esiste slittamento (quindi regime iva trasp. mista)
047100040429  "   * la data liq. deve essere ricalcolata in base allo slittamento
047110040429D1600C                   if        $ExistSli <>'1'
047120040429C1277C* la data liquidazione deve essere = alla data registrazione
047130991130  "  C* e qualla plafond alla data documento (d.l.56 del 23/3/98)
047140991130  "  C                   CALLB     'XDT4000'
047150991130  "  C                   PARM      REGDtReg      XDTAMG           10
047160991130  "  C     C1DtRif       PARM                    XDTGMA            6 0
047170040429C1277C                   PARM      2             XDTSTA            1 0
047180040429D1600C                   endif
047190040429D1600 *
047200000125C1277C                   IF        REGDtDoc <> *loval
047210040429C1320X*****  ex C1277    CALLB     'XDT4000'
047220040429  "  X*****       "      PARM      REGDtDoc      XDTAMG           10
047230040429  "  X*****Data60 "      PARM                    XDTGMA            6 0
047240040429  "  X*****       "      PARM      2             XDTSTA            1 0
047250000127  "  C                   Move      REGDtDoc      DataISO
047260000127C1320C     *DMY          Move      DataISO       Data60
047270000125C1277C                   MOVE      Data60        DataAlfa
047280991130  "  C                   Z-ADD     Mese          C1MMPlaf
047290991130  "  C                   Z-ADD     Anno          C1AAPlaf
047300991130  "  C                   ENDIF
047310991130C1277C                   ENDIF
047320950721     C*
047330961127     c* se non ci sono righe iva e la fattura è con autofattura
047340961127     c                   if        s1nrr = 0 and opeobbiva = *off
047350961127     c                             and opeaftacq = *on
047360970627     C                   Z-add     D2Importo     S1Importo
047370970627     C                   Z-add     D2Importd     S1Importd
047380981130     C* imposto gli attributi della riga
047390981130     C                   EXSR      ATRS1
047400961127     C                   ADD       1             S1NRR
047410961127     C                   WRITE     FMTS1
047420961127     c                   endif
047430020327C1550 *
047440020513  "   * Data annot valorizzata dal file se presente
047450020514  "  C                   if        IVADtAnnot <> Dt0 and
047460020514  "  C                             IVADtAnnot <> *blank
047470020513  "  C                   move      IVADtAnnot    DataISO
047480020513  "  C     *jobrun       move      DataISO       C1DtAnnot
047490020513  "  C                   endif
047500020513  "   *
047510020513  "  C     *jobrun       move      D2DtReg       DataISO1
047520020513  "  C                   move      IVADtReg      DataISO2
047530020513  "   *
047540020513  "   * azzero Dt annnot. se:
047550020513  "   * - siamo in copia o in immissione (qualsiasi regime iva) oppure
047560020514  "   * - cambiata dt reg (qualsiasi regime iva)                oppure
047570020514  "   * - cambiata serie reg  (solo in caso di regime iva mista)
047580020513  "  C                   if        (Opz015='03' or Opz015='01')  or
047590021127C1550C                             (DataISO1 <> DataISO2)  or
047600040429D1249X**ex C1550 ******            (IVASerReg = D2SerReg  and
047610021127D1249C                             (IVASerReg <>D2SerReg  and
047620021127C1550C                             RegimeIva = '2')
047630020513  "  C                   z-add     0             C1DtAnnot
047640020513  "  C                   endif
047650020513  "   *
047660030702C1550 * azzero Dt liq. se:
047670030702C1550 * - siamo in copia o in immissione (qualsiasi regime iva) oppure
047680030102C1550 * - in regime iva mista modificata serie con una che ha slitt
047690030102D1249 *   oppure cambiata dt reg in regime iva mista con serie con slit
047700040429D1390X** ex C1550        if        (Opz015='03' or Opz015='01')  or
047710030702  "  C                   if        (Opz015='03' or Opz015='01' and
047720030702  "  c* ma non provengo da primanota batch
047730030702D1390C                             ImmExp015 ='0') or
047740040429D1249X**ex C1550 ***************** (IVASerReg = D2SerReg  and
047750030102D1249C                             ((IVASerReg <> D2SerReg   or
047760030102D1249C                               DataISO1  <> DataISO2)  and
047770021127C1550C                             RegimeIva = '2'  and
047780030102  "  C                             $ExistSli = '1' )
047790020513  "  C                   z-add     0             C1DtRif
047800020513  "  C                   endif
047810020327C1550 *
047820030702     C*
047830950721     C                   endsr
047840950616     C************************************************************
047850950616     C* RIPRISTINA GLI ATTRIBUTI DEL VIDEO
047860950616     C************************************************************
047870950616     C     ATRD1         BEGSR
047880950616     C*
047890950803     C* azzero gl'attributi relativi ai campi video
047900950616     C                   EVAL      D1UnitaA = x'32'
047910950616     C                   EVAL      D1UnitaKA = x'A0'
047920950803     C                   EVAL      D1TabPNotA = x'32'
047930950803     C                   EVAL      D1TabPNoKA = x'A0'
047940950913     C                   EVAL      D1RAGKA   = x'A0'
047950950913     C                   EVAL      D1INDLKA  = x'A0'
047960950913     C                   EVAL      D1INDAKA  = x'A0'
047970950913     C                   EVAL      D1PIVAKA  = x'A0'
047980950913     C                   EVAL      D1CDFISKA = x'A0'
047990950913     C                   EVAL      D1COORDKA = x'A0'
048000950913     C                   EVAL      D1TpSoggA = x'32'
048010950913     C                   EVAL      D2DtRegA   = x'32'
048020950913     C                   EVAL      D2NrRegA   = x'32'
048030950913     C                   EVAL      D2SerRegA  = x'32'
048040000217C1336C                   EVAL      D2DtCoCoA  = x'32'
048050950913     C                   EVAL      D1CausTesA = x'32'
048060950913     C                   EVAL      D2DivisaA  = x'32'
048070950913     C                   EVAL      D2CambioA  = x'32'
048080950913     C                   EVAL      D2DtDocA   = x'32'
048090010621C1494C                   EVAL      D2DtLibA   = x'32'
048100950913     C                   EVAL      D2NrDocA   = x'32'
048110950913     C                   EVAL      D2SerDocA  = x'32'
048120950913     C                   EVAL      D1KscA     = x'32'
048130950913     C                   EVAL      D2ImportDA = x'32'
048140950913     C                   EVAL      D2ImportoA = x'32'
048150950913     C                   EVAL      D2DtParA   = x'32'
048160950913     C                   EVAL      D2NrParA   = x'32'
048170950913     C                   EVAL      D2SerParA  = x'32'
048180950913     C                   EVAL      D2CondPagA = x'32'
048190950616     C*
048200950616     C* se l'unità non è gestita
048210950616     C* la valorizzo a ND
048220950616     C* e il campo viene protetto
048230950720     C                   IF        FgO1 <> *on
048240950712     C                   EVAL      D1UnitaA = ND_PR
048250950712     C                   EVAL      D1UnitaKA = ND_PR
048260950616     C                   ENDIF
048270950719     C*
048280951005     C* nascondo le descrizioni dei campi di decodifica (solo in immissione)
048290951012     C                   IF        opz015 = '01'          and
048300951012     C                             ImmExp015 = '0'
048310950719     C                   EVAL      D1RAGKA   = ND_PR
048320950719     C                   EVAL      D1INDLKA  = ND_PR
048330950719     C                   EVAL      D1INDAKA  = ND_PR
048340950719     C                   EVAL      D1PIVAKA  = ND_PR
048350950719     C                   EVAL      D1CDFISKA = ND_PR
048360950719     C                   EVAL      D1COORDKA = ND_PR
048370950803     C                   ENDIF
048380950803     C*
048390950803     C* se non sono in immissione
048400951012     C                   IF        Opz015 <> '01'         or
048410951012     C* oppure sono in una delle immissione speciali
048420951012     C                             ImmExp015 <> '0'
048430950825     C* nascondo il modello PN
048440950803     C                   EVAL      D1TabPNotA = ND_PR
048450950803     C                   EVAL      D1TabPNoKA = ND_PR
048460951005     C* proteggo
048470951005     C* - il tipo soggetto
048480950825     C                   EVAL      D1TpSoggA = Protetto
048490951005     C* - la divisa
048500951005     C                   EVAL      D2DivisaA = Protetto
048510950816     C                   ENDIF
048520950816     C*
048530950816     C* se la testata non è modificabile
048540950816     C* valorizzo a Protetto
048550950816     C* rif.registrazione - causale - divisa - cambio - rif.documento
048560950816     C                   IF        TesMod = *off
048570961128     C                   EVAL      D2DtRegA = Protetto
048580961128     C                   EVAL      D2NrRegA = Protetto
048590961128     C                   EVAL      D2SerRegA = Protetto
048600000217C1336C                   EVAL      D2DtCoCoA = Protetto
048610950816     C                   EVAL      D1CausTesA = Protetto
048620950816     C                   EVAL      D2DivisaA = Protetto
048630961128     C                   EVAL      D2CambioA = Protetto
048640961128     C                   EVAL      D2DtDocA = Protetto
048650010621C1494C                   EVAL      D2DtLibA = Protetto
048660961128     C                   EVAL      D2NrDocA = Protetto
048670961128     C                   EVAL      D2SerDocA = Protetto
048680981229     C                   EVAL      D2DtRfCaA = Protetto
048690950816     C                   ENDIF
048700961129     C* se i campi IVA della testata non sono modificabili
048710961129     C* valorizzo a Protetto
048720961129     C                   IF        %subst(Flgmod003:1:1) = *on
048730000706D0659C                   EVAL      D1CausTesA = Protetto
048740961129     C                   EVAL      D2DtRegA = Protetto
048750961129     C                   EVAL      D2NrRegA = Protetto
048760961129     C                   EVAL      D2SerRegA = Protetto
048770000217C1336C                   EVAL      D2DtCoCoA = Protetto
048780961129     C                   EVAL      D2DtDocA = Protetto
048790010621C1494C                   EVAL      D2DtLibA = Protetto
048800961129     C                   EVAL      D2NrDocA = Protetto
048810961129     C                   EVAL      D2SerDocA = Protetto
048820981229     C                   EVAL      D2DtRfCaA = Protetto
048830961129     C                   ENDIF
048840950816     C*
048850950816     C* se il 1° movimento non è modificabile
048860950816     C* valorizzo a Protetto
048870950816     C* conto - importi - rif.partita - condizioni pagamento
048880950816     C                   IF        FldMod = *off
048890950816     C                   EVAL      D1TpSoggA = Protetto
048900950816     C                   EVAL      D1KscA = Protetto
048910950816     C                   EVAL      D2ImportDA = Protetto
048920950816     C                   EVAL      D2ImportoA = Protetto
048930010202C1463C* per permetterne la modifica quando la registrazione è
048940010202  "  C*  "solo" a giornale bollato e/o a bollato iva, in realtà
048950010202  "  C*  proteggo i RIFERIMENTI DI PARTITA solo se:
048960010202  "  C* - esiste BMV...
048970010202  "  C                   If        %SubSt(FlgMod006:1:1) = *On or
048980010202  "  C* - esiste MRA...
048990010202  "  C                             %SubSt(FlgMod006:2:1) = *On or
049000010202  "  C* - esiste PPG...
049010010202C1463C                             %SubSt(FlgMod006:3:1) = *On
049020950816     C                   EVAL      D2DtParA = Protetto
049030950816     C                   EVAL      D2NrParA = Protetto
049040950816     C                   EVAL      D2SerParA = Protetto
049050010202C1463C                   EndIf
049060061221C1991 * se l'unico motivo per il quale il movimento non è modificabile
049070061221  "   * è lo stop di periodo su dt registraz. e/o su dt comp. coge e/o
049080061221  "   * su dt comp. analit., la cond pag deve essere manutenzionabile
049090061221  "   *  (FlgMod003 dettaglia i motivi per cui non modificare la reg
049100061221  "   *   (1° posizione: dt reg <= dt ultimo giornale iva
049110061221  "   *    2° posizione: dt giornale bollato <= dt stampa giornale 'GIOBOL'
049120061221  "   *    3° posizione: registrazione non modificabile da causale
049130061221  "   *   >4° posizione: presenza stop di periodo su data registrazione
049140070109  "   *   >5° posizione: presenza stop di periodo su data competenza coge
049150061221  "   *  (FlgMod006 dettaglia i motivi per cui non modificare il mov
049160061221  "   *   (1° posizione: presenza di tesoreria con BMVStatus <> ' '
049170061221  "   *    2° posizione: presenza di ritenute d'acconto con MRASTA > '0'
049180061221  "   *    3° posizione: presenza di assegni con PPGStatus <> '1'
049190061221  "   *   >4° posizione: presenza di stop di periodo su data competenza analitica
049200061221  "   *    5° posizione: presenza di registrazione intercompany collegata
049210061221  "   *    6° posizione: presenza di tesoreria con blocco riconc con vincolo sui
049220061221  "  C                   if        %subst(FlgMod003:1:3) <> *blank or
049230061221  "  C                             %subst(FlgMod003:6:5) <> *blank or
049240061221  "  C                             %subst(FlgMod006:1:3) <> *blank or
049250061221C1991C                             %subst(FlgMod006:5:6) <> *blank
049260950816     C                   EVAL      D2CondPagA = Protetto
049270061221C1991C                   endif
049280950816     C                   ENDIF
049290951004     C*
049300951004     C* se esiste almeno un effetto presentato
049310060803D2031 * oppure esiste un effetto riconciliato in Home Banking
049320951004     C* proteggo conto e riferimenti partita
049330951004     C* proteggo la richiesta rate a manuale
049340951004     C                   IF        EffPres = *on
049350060803D2031C                             or EffRicHB = *on
049360951004     C                   EVAL      D1KscA = Protetto
049370960429     C                   EVAL      D2DtParA = Protetto
049380951004     C                   EVAL      D2NrParA = Protetto
049390951004     C                   EVAL      D2SerParA = Protetto
049400951004     C                   MOVE      Forzato       D2PagManA
049410951004     C                   ENDIF
049420000330C1350 *
049430000330  "   * proteggo la richiesta rate a manuale se esistono
049440000330  "   * effetti collegati alla partita ma non alla registrazione
049450000330  "  C                   If        $EffNoCol   = *On
049460000330  "  C                   Move      Forzato       D2PagManA
049470000330  "  C                   EndIf
049480000330C1350 *
049490960111     C*
049500960111     C* se comunque il tipo soggetto è valorizzato
049510960111     C                   IF        D1TpSogg <> *blank
049520960111     C* lo proteggo
049530960111     C                   EVAL      D1TpSoggA = Protetto
049540960111     C                   ENDIF
049550991011     C*
049560991012C1251 * Se il soggetto è un fornitore e siamo in manutenzione
049570991011  "  C                   If        D1TpSogg = 'F'  AND  Opz015 = '02'
049580991011  "   *  se esiste riten. d'acc.
049590991011  "  C                   Eval      MRASysF     = REGSys
049600991011  "  C                   Eval      MRANAsRegF  = REGNrAsReg
049610991011  "  C     K02MRA08      SETLL     NDMRA000                               21
049620991011  "  C                   If        *In21     = *On
049630991011  "   *    proteggo il fornitore
049640991011  "  C                   Eval      D1TpSoggA = Protetto
049650991011  "  C                   Eval      D1KscA    = Protetto
049660991011  "  C                   EndIf
049670991012C1251C                   EndIf
049680001025     C*
049690001025C1429 * Se Partita bloccata da estrazione per bonifici
049700001026  "   * in manutenzione
049710001026  "  C                   If        $BonInc     = '1'  AND
049720001026  "  C                             Opz015      = '02'
049730001026  "   *    proteggo la partita
049740001025  "   *    (se non fosse protetta l'utente la potrebbe cambiare
049750001025  "   *      e la fattura verrebbe riestratta nei bonifici)
049760001025  "  C                   Eval      D2DtParA    = Protetto
049770001025  "  C                   Eval      D2NrParA    = Protetto
049780001025  "  C                   Eval      D2SerParA   = Protetto
049790001025C1429C                   EndIf
049800010821C1512 *
049810011002  "   * Proteggo importi e cambio se divisa di par. <> divisa di movim
049820010821  "   *  e gli importi sono valorizzati
049830010821  "   * (utile per la prima emissione della testata)
049840010821  "  C                   if        D2DivisaP  <> D2Divisa  and
049850010821  "  C                             D2DivisaP  <> *Blank    and
049860010821  "  C                             D2Divisa   <> *Blank    and
049870010821  "  C                             D2ImportD  <> *Zero     and
049880010821  "  C                             D2Importo  <> *Zero
049890010821  "  C                   eval      D2ImportDA = Protetto
049900010821  "  C                   eval      D2ImportoA = Protetto
049910011002  "  C                   eval      D2CambioA  = Protetto
049920010821C1512C                   endif
049930950719     C*
049940040429C1429X****************** ENDSR
049950001025C1429C     XAtrD1        ENDSR
049960950816     C************************************************************
049970950816     C* RIPRISTINA GLI ATTRIBUTI DEL SFL
049980950816     C************************************************************
049990950816     C     ATRS1         BEGSR
050000950816     C*
050010981009     C* azzero gl'attributi relativi ai campi video (HI UL)
050020981009     C                   EVAL      S1RigaPRA = x'26'
050030981009     C                   EVAL      S1CausRigA = x'26'
050040981009     C                   EVAL      S1RifIVAA = x'26'
050050981009     C                   EVAL      S1AliqA = x'26'
050060981009     C                   EVAL      S1ImportDA = x'26'
050070950816     C*
050080950816     C* se il movimento non è modificabile
050090950816     C* valorizzo a Protetto
050100950816     C* tutta la riga
050110950816     C                   IF        H1RigaPR = *on
050120950816     C                   EVAL      S1RigaPRA = Protetto
050130981009     C                   EVAL      S1CausRigA = Protetto
050140981009     C                   EVAL      S1RifIVAA = Protetto
050150981009     C                   EVAL      S1AliqA = Protetto
050160981009     C                   EVAL      S1ImportDA = Protetto
050170950816     C                   ENDIF
050180981009     C*
050190981009     C* per ogni errore metto il rispettivo campo a RI UL
050200040429D0611X****************** IF        *in65 = *on
050210001020D0611C                   If        $InC1(65)   = *On
050220981009     C                   EVAL      S1CausRigA = x'25'
050230981009     C                   ENDIF
050240040429D0611X****************** IF        *in61 = *on
050250001020D0611C                   If        $InC1(61)   = *On
050260981009     C                   EVAL      S1RifIVAA = x'25'
050270981214     C                   EVAL      S1AliqA = x'25'
050280981009     C                   ENDIF
050290040429D0611X****************** IF        *in60 = *on
050300001020D0611C                   If        $InC1(60)   = *On
050310981009     C                   EVAL      S1ImportDA = x'25'
050320981009     C                   ENDIF
050330950816     C*
050340950816     C                   ENDSR
050350950720     C*----------------------------------------------------------
050360950720     C* Controlla coerenza conto/causale/segno
050370950720     C*----------------------------------------------------------
050380950726     C     CallMvc001    BEGSR
050390950720     C                   SETOFF                                       99
050400950720     C*
050410950720     C                   Clear                   CausOt
050420950720     C* Costruzione Causale Sottonatura
050430950720     C                   movea     OpeNa0        Causot(1)
050440950720     C                   movea     OpeNa1        Causot(2)
050450950720     C                   movea     OpeNa2        Causot(3)
050460950720     C                   movea     OpeNa3        Causot(4)
050470950720     C                   movea     OpeNa4        Causot(5)
050480950720     C                   movea     OpeNa5        Causot(6)
050490950720     C                   movea     OpeNa6        Causot(7)
050500950721     C                   movea     OpeNa7        Causot(8)
050510950721     C                   movea     OpeNa8        Causot(9)
050520950721     C                   movea     OpeNa9        Causot(10)
050530950720     c* Segno clienti
050540950720     C                   if        d1tpsogg = 'C'
050550950720     C                   if        opena0 = '1'
050560961009     C                   eval      segno007 = 'D'
050570950720     C                   else
050580961009     C                   eval      segno007 = 'A'
050590950720     C                   endif
050600950720     C                   endif
050610950720     c* Segno fornitori
050620950720     C                   if        d1tpsogg = 'F'
050630950720     C                   if        opena1 = '1'
050640961009     C                   eval      segno007 = 'D'
050650950720     C                   else
050660961009     C                   eval      segno007 = 'A'
050670950720     C                   endif
050680950720     C                   endif
050690950720     C*
050700950720     C                   CallB     'NDMVC001'
050710950720     C                   PARM                    xscsoc
050720950720     C                   PARM                    d1unita
050730950720     C                   PARM                    $kcc
050740950720     C                   PARM                    d1ksc
050750950720     C                   PARM                    d1caustes
050760950914     C                   PARM      Dt0           WDate
050770950720     C                   PARM                    d1tpsogg
050780950720     C                   PARM                    Causot
050790950720     C                   PARM                    Segno007
050800950720     C                   PARM      '1'           GesMsg
050810950720     C                   PARM                    Esito
050820950720     C*
050830950720     C                   IF        Esito = *On
050840950720     C*
050850950720     C* gestisco la loro emissione
050860950925     c                   clear                   TextMsg
050870950720     C                   DOU       Q2BytAva = 0
050880950720     C                   EXSR      RestoreMsg
050890950720     C                   IF        Q2BytAva > 0
050900950720     C                   EXSR      GestErr
050910950720     C                   ENDIF
050920950720     C                   ENDDO
050930950720     C                   EndIf
050940950720     C*
050950950720     C                   ENDSR
050960950719     C************************************************************
050970950719     C* Eseguo controlli Valori campi dD1
050980950719     C************************************************************
050990950719     C     CtrlValD1     BEGSR
051000950719     C*
051010950719     C                   SETOFF                                       99
051020950719     C                   EVAL      CiSonoDft = *off
051030950719     C                   MOVE      *blank        TpSfl
051040981020     C*
051050040429     X**********************************************************************
051060040429     X* spostato dopo il reperimento dati del conto, per vedere se impostare
051070040429     X* il modello presente sul conto
051080040429     X**********************************************************************
051090040429     X* Modello prima nota
051100040429     X***                MOVEL     *blank        D1TabPNotD
051110040429     X***                IF        D1TabPNot <> *blank
051120040429     X***                EXSR      RepDatiMod
051130040429     X***                ENDIF
051140040429C1129X**********************************************************************
051150040429  "  X* reimmesso per evitare questo caso:                             stare
051160040429  "  X* in immissione se in D1 veniva impostato il modello             stare
051170040429  "  X* senza digitare il conto, veniva emesso l'errore
051180040429  "  X* sul conto perchè non presente e contemporaneamente impostato
051190040429  "  X* dalla RepDatiMod (valida dopo il controllo su D2Ksc)
051200040429  "  X**********************************************************************
051210990505  "  C* Modello prima nota
051220990505  "  C                   MOVEL     *blank        D1TabPNotD
051230040308  "  C                   IF        D1TabPNot <> *blank
051240110110C1129C                   EXSR      RepDatiMod
051250110110D2659c                   else
051260110110D2659c                   exsr      ClrDatiMod
051270110110C1129C                   ENDIF
051280040429C1129X**********************************************************************
051290950719     C*
051300950719     C* Tipo soggetto
051310950719     C                   MOVEL     *BLANK        D1tpsoggd
051320950719     C                   RESET                   X07DS
051330950719     C                   MOVE      '1'           X07RIC
051340950719     C                   MOVEL     'ANDIZ000'    X07TRC
051350950719     C                   MOVEL     'DSPCLIFOR'   X07CAM
051360950719     C                   MOVEL     D1tpsogg      X07VAL
051370961009     C                   CALLB     'X07VALR'
051380950719     C                   PARM                    X07DS
051390040308     C     X07ERR        IFEQ      '0'
051400950719     C                   MOVEL     X07DES        D1tpsoggd
051410040308     C                   ELSE
051420040429D0611X****************** SETON                                          5899
051430001023D0611 * PRO0048-Il valore indicato per il campo non è valido
051440001023  "  C                   Eval      $InD1(58)   = *On
051450001020D0611C                   Eval      *In99       = *On
051460040308     C                   ENDIF
051470950719     C*
051480950719     C* Sottoconto
051490950719     C                   CLEAR                   D1KSCD
051500950719     C                   if        d1ksc = *blanks
051510040429D0611X****************** seton                                        6099
051520001023D0611 * PRO0014-Campo obbligatorio
051530001023  "  C                   Eval      $InD1(60)   = *On
051540001020D0611C                   Eval      *In99       = *On
051550950719     C                   else
051560950719     C                   CALLb     'XALLINEA'
051570950719     C                   PARM                    D1KSC
051580950719     C                   PARM      8             LEN               5 0
051590950719     C                   PARM                    XSCINP
051600950719     C* xcapclifor
051610950719     C                   movel     d1tpsogg      $kcc
051620950719     C                   CALLB     'XCAPCLIFOR'
051630950719     C                   PARM                    XSCsoc
051640950719     C                   PARM                    $kcc              6
051650950719     C                   PARM                    D1KSC
051660950719     C*
051670950719     C* Richiamo modulo controllo conto
051680950922     C                   reset                   ANRCODS
051690001011d0732c                   eval      DataIso=*loval
051700950922     C                   EVAL      LenOut = %Size(ANRCODS)
051710950719     C                   CallB     'NDMVC002'
051720950719     C                   PARM                    xscsoc
051730950719     C                   PARM                    $kcc
051740950719     C                   PARM                    d1ksc
051750040429D0732X****               PARM                    *omit
051760001011D0732C                   PARM                    DataIso
051770950721     C                   PARM      *OFF          GesMsg
051780950721     C                   PARM      *OFF          Esito
051790950922     C                   PARM      'ANRCO'       StrutturaO
051800950922     C                   PARM                    ANRCODS
051810950721     C                   PARM                    LenOut
051820950719     C* Errore
051830951004     C                   Movel     *Blank        WKccFact
051840951004     C                   Movel     *Blank        WKccFact
051850040308     C     Esito         IFEQ      *ON
051860040429D0611X****************** SETON                                        5799
051870001023D0611 * PRO0046-Codice errato
051880001023  "  C                   Eval      $InD1(57)   = *On
051890001020D0611C                   Eval      *In99       = *On
051900951002     C                   Else
051910951004     C                   Movel     RcoKccFact    WKccFact
051920951004     C                   Movel     RcoKscFact    WKscFact
051930981020     C* se non è stato impostato un modello PN
051940040308     C                   IF        D1TabPNot = *blank  and
051950981020     C* e ne esiste uno associato al conto
051960040308     C                             RCOTabPNot <> *blank and
051970981021     C* e si è in immissione
051980981021     C                             Opz015 = '01'        and
051990981021     C* e l'immissione non è speciale
052000981209     C                             ImmExp015 = '0'      and
052010981209     C* non ho mai chiesto all'utente se intende usare il modello collegato
052020981209     C                             ($WindowMod = *Off or
052030981209     C* oppure ha cambiato il conto di riferimento
052040981209     C                              D1Ksc <> $WindowKsc)
052050981020     C* richiedo all'utente se lo vuole usare
052060981020     C                   EXSR      RichUsoMod
052070981209     C* valorizzo il flag che mi indica l'avvenuta emissione della
052080981209     C* finestra di richiesta utilizzo modello collegato
052090981209     C                   Move      *On           $WindowMod
052100981209     C* mi salvo il sottoconto per l'eventuale richiesta di uso modello
052110981209     C* collegato
052120981209     C                   Movel     D1Ksc         $WindowKsc
052130040308     C                   ENDIF
052140081008C2086C*
052150081008  "  C* SOLO se sono in immissione PURA, cioè senza recupero dati
052160081008  "  C                   if        OPZ015     = '01'    and
052170081008  "  C                             ImmExp015  = '0'
052180081008  "  C* in caso di fornitore controllo se esistono righe ordini o
052190081008  "  C* impegni in controllo fatture, e nel caso avverto dell'esistenza
052200081008  "  C* e chiedo se si vuole continuare ad inserire il documento IVA
052210081008  "  C                   if        D1TpSogg   = 'F'
052220090720C2086C                   EXSR      CtrOrdini
052230090720D2530C* se chiamato un programma, vado a fine routine, deve fare  inzd1
052240090720  "  C                   if        Ritorno_D1 =*on
052250090720  "  c                   leavesr
052260090720D2530c                   endif
052270090720C2086C                   endif
052280081008  "  C                   endif
052290081008C2086C*
052300040308     C                   ENDIF
052310040308     c*
052320040308     C                   ENDIF
052330040308     C*
052340040308     C* Modello prima nota
052350040308     C                   MOVEL     *blank        D1TabPNotD
052360040308     C                   IF        D1TabPNot <> *blank
052370040308     C                   EXSR      RepDatiMod
052380040308     C                   ENDIF
052390040308     C*
052400040308     C* Causale
052410040308     C                   EVAL      LenOut = %Size(Anope)
052420040308     C                   CALLB     'NDMVC103'
052430040308     C                   PARM                    XscSoc
052440040308     C                   PARM                    D1CausTes
052450040308     C                   PARM                    *omit
052460971202     C                   PARM                    *omit
052470971202     C                   PARM                    *omit
052480971202     C                   PARM                    *omit
052490971202     C                   PARM                    Ctb015
052500971202     C                   PARM                    TpRegz015
052510040429D0224X***                PARM      *on           GesMsg
052520990921D0224C                   PARM      *off          GesMsg
052530971202     C                   PARM                    Esito
052540971202     C                   PARM      'ANOPE'       StrutturaO
052550971202     C                   PARM                    Anope
052560971202     C                   PARM                    LenOut
052570971202     C                   PARM      *loval        DtRifCau
052580950719     C* errore
052590040308     C                   IF        Esito = *on
052600040429D0611X****************** seton                                        5999
052610001023D0611 * MVC0001-(F1) Causale operativa &2 non corretta
052620001023  "  C                   Eval      $InD1(59)   = *On
052630001020D0611C                   Eval      *In99       = *On
052640040308     C                   else
052650950725     c* controllo tipologia causale
052660040308     C                   if        opetpc <> '1'
052670040429D0611X****************** seton                                        9399
052680001023D0611 * NDC0029-Tipologia causale incompatibile con la funzione
052690001023  "  C                   Eval      $InD1(93)   = *On
052700001020D0611C                   Eval      *In99       = *On
052710040308     C                   else
052720950725     C*
052730950719     C                   movel     opedesbrev    d1caustesd
052740001220D0801C* memorizzo il tipo documento iva della causale di testata
052750001220D0801C                   Eval      $TpDocIva = OPETpDocI
052760950922     C*
052770950922     C* definisco se sto lavorando su una fattura oppure una nota
052780950922     C                   EVAL      Fattura = *off
052790040308     C                   SELECT
052800040308     C                   WHEN      D1TpSogg = 'C'
052810040308     C                   IF        OpeNa0 = '1'
052820040308     C                   EVAL      Fattura = *on
052830040308     C                   ENDIF
052840040308     C                   WHEN      D1TpSogg = 'F'
052850040308     C                   IF        OpeNa1 = '2'
052860040308     C                   EVAL      Fattura = *on
052870040308     C                   ENDIF
052880040308     C                   ENDSL
052890040308     C*
052900040308     C* reperisco i default di causale
052910040308     C* se non sto gestendo un modello
052920040308     C                   IF        Ctb015 <> 'MD'         and
052930040308     C* e se è stato indicato il codice
052940040308     C                             OpeRepDft <> *blank    And
052950040308     C                             Opz015    =  '01'
052960950719     C                   EVAL      DftSocieta = XscSoc
052970950719     C                   EVAL      DftCdDft = OpeRepDft
052980950719     C                   EVAL      DftUtente = XscCUt
052990950719     C     K03DFT01      CHAIN     ANDFT000                           21
053000950719     C                   IF        *in21 = *off
053010950719     C                   EVAL      CiSonoDft = *on
053020950719     C                   ELSE
053030950719     C     K02DFT01      CHAIN     ANDFT000                           21
053040950719     C                   IF        *in21 = *off
053050950719     C                   EVAL      CiSonoDft = *on
053060950719     C                   ENDIF
053070950719     C                   ENDIF
053080950719     C                   ENDIF
053090950719     C*
053100950719     C                   endif
053110950725     C                   endif
053120960517     C*
053130950719     C* se ci sono default
053140970804     C                   IF        CiSonoDft = *on
053150970804     C* valorizzo il default del tipo sfl da richiamare
053160970804     C                   EVAL      TpSfl = DftTpSfl
053170970804     C* se la divisa non è già stata valorizzata
053180970804     C                   IF        D2Divisa = *blank
053190950719     C* la valorizzo con il suo default
053200950719     C                   EVAL      D2Divisa = DftDivisa
053210970804     C                   ENDIF
053220950719     C                   ENDIF
053230971114     C*
053240971114     C* se la divisa non è già stata valorizzata
053250971114     C                   IF        D2Divisa = *blank
053260971114     C* la valorizzo con la divisa preferenziale del soggetto
053270971114     C                   EVAL      D2Divisa = RcoDivisa
053280971114     C                   ENDIF
053290971114     C*
053300971113     C*
053310950922     C* Unità (viene controllata autorizzazione nella routine successiva)
053320950719     C*
053330950720     C                   clear                   d1unitad
053340950720     C                   IF        FGO1 = *on
053350950720     C                   if        d1unita = *blanks
053360040429D0611X****************** seton                                        5299
053370001023D0611 * PRO0014-Campo obbligatorio
053380001023  "  C                   Eval      $InD1(52)   = *On
053390001020D0611C                   Eval      *In99       = *On
053400950720     C                   else
053410950720     C                   eval      unisocieta = xscsoc
053420950720     C                   eval      uniunita   = d1unita
053430950720     C     K02UNI01      CHAIN     ANUNI01L                           21
053440950720     C                   if        *in21 =*on
053450040429D0611X****************** seton                                        5199
053460001023D0611 * PRO0028-Unità inesistente
053470001023  "  C                   Eval      $InD1(51)   = *On
053480001020D0611C                   Eval      *In99       = *On
053490950720     C                   ELSE
053500950720     C                   eval      d1unitad = unidesbrev
053510950720     C                   endif
053520950720     C                   endif
053530950720     C                   endif
053540950719     C*
053550950719     C* Decodifiche ragione sociale, eccetera....
053560950719     C                   clear                   d1descr
053570950719     C                   clear                   d1desagg
053580950719     C                   clear                   d1ragsoca
053590950719     C                   clear                   D1indriza
053600981209     C                   clear                   d1capa
053610950719     C                   clear                   d1localita
053620950719     C                   clear                   d1prova
053630950719     C                   clear                   d1ragsocl
053640950719     C                   clear                   D1indrizl
053650981209     C                   clear                   d1capl
053660950719     C                   clear                   d1localitl
053670950719     C                   clear                   d1provl
053680950720     C                   clear                   d1partiva
053690950720     C                   clear                   d1cdfisc
053700981209     C                   clear                   d1abi
053710981209     C                   clear                   d1cab
053720950719     C* sproteggo le descrizioni dei campi di decodifica
053730950719     C                   if        *in99 = *off
053740950719     C                   EVAL      D1RAGKA   = x'A0'
053750950719     C                   EVAL      D1INDLKA  = x'A0'
053760950719     C                   EVAL      D1INdaka  = x'A0'
053770950719     C                   EVAL      D1PIVAKA  = x'A0'
053780950719     C                   EVAL      D1CDFISKA = x'A0'
053790950719     C                   EVAL      D1COORDKA = x'A0'
053800950922     C*
053810950922     C* reperisco i dati del soggetto relativo al conto e i dati relativi
053820950922     C* alla sua natura
053830950922     C                   EXSR      RepDatiSog
053840950922     C* imposta la ragione sociale a video
053850950922     C                   eval      D1descr=Descr44
053860950922     C                   eval      D1desagg=DesAgg44
053870950915     c* Richiamo dvcoba
053880950915     C* reperisco coordinate bancarie
053890950915     C                   RESET                   DVCOBADS
053900950915     C                   MOVE      XSCSOC        CobSoc
053910950915     C                   MOVE      D1TpSogg      CobCoF
053920950915     C                   MOVE      D1Ksc         CobKsc
053930950720     C*
053940950915     C                   CALL      'DVCOBA'
053950950915     C                   PARM                    DVCOBADS
053960950720     C*
053970040308     C                   if        CobErr<>*ON
053980950915     C                   eval      D1Abi = CobAbi
053990950915     C                   eval      D1Cab = CobCab
054000950720     C                   endif
054010950719     C                   endif
054020950719     C*
054030950719     C                   ENDSR
054040081008C2086C************************************************************
054050081008  "  C* controllo esistenza ordini/impegni in controllo fattura
054060081008  "  C************************************************************
054070081008C2086C     CtrOrdini     BEGSR
054080081008     C*
054090090223C2122X*ex C2086          Eval      EsOrdini    = *Off
054100090223  "  X*ex C2086          Eval      EsImpegni   = *Off
054110090223  "  X*ex C2086
054120090223  "  X*ex C2086          Select
054130090223  "  X*ex C2086 presente ProJ Distribuzione
054140090223  "  X*ex C2086          When      XScGest     = '1'
054150090223  "  X*ex C2086          EXSR      CtrOAMOA
054160090223  "  X*ex C2086          Eval      EsOrdini    = RcdFtXX044
054170090223  "  X*ex C2086          endSl
054180090223  "  X*ex C2086
054190090223  "  X*ex C2086          EXSR      CtrImpegni
054200090223  "  X*ex C2086          Eval      EsImpegni   = EsFatCX003
054210090223  "  X*ex C2086
054220090223  "  X*ex C2086          if        EsOrdini    = *On    or
054230090223  "  X*ex C2086                    EsImpegni   = *On
054240090223  "  X*ex C2086 avverto l'utente
054250090223  "  X*ex C2086          EXSR      MsgOrd
054260090223C2122X*ex C2086          endif
054270090223C2122C*
054280090223  "  C                   EXSR      CallNDC711
054290090223C2122C*
054300081008     C*
054310081008C2086C                   ENDSR
054320081008C2086C/EJECT
054330090223C2122X*ex C2086  ************************************************************
054340090223  "  X*ex C2086  Verifico esistenza ordini collegati al fornitore
054350090223  "  X*ex C2086  ************************************************************
054360090223  "  X*ex C2086  CtrOAMOA      BEGSR
054370090223  "  X*ex C2086
054380090223  "  X*ex C2086          RESET                   PJXX044DS
054390090223  "  X*ex C2086
054400090223  "  X*ex C2086          MOVE      XScSoc        SocXX044
054410090223  "  X*ex C2086
054420090223  "  X*ex C2086          if        WKscFact    = *Blank
054430090223  "  X*ex C2086          Eval      ForFtXX044  = D1Ksc
054440090223  "  X*ex C2086          else
054450090223  "  X*ex C2086          Eval      ForFtXX044  = WKscFact
054460090223  "  X*ex C2086          endif
054470090223  "  X*ex C2086          Eval      DtDocXX044  = Dt0
054480090223  "  X*ex C2086          MOVEL     *Off          ErrXX044
054490090223  "  X*ex C2086
054500090223  "  X*ex C2086          MOVEL     PJXX044DS     KPJBU
054510090223  "  X*ex C2086          CALL      'PJXX044R'
054520090223  "  X*ex C2086          PARM                    KPJBA
054530090223  "  X*ex C2086
054540090223  "  X*ex C2086          MOVEL     KPJBU         PJXX044DS
054550090223  "  X*ex C2086  ritorno da PGM gestione
054560090223  "  X*ex C2086          EXSR      RTNXX044
054570090223  "  X*ex C2086
054580090223  "  X*ex C2086          ENDSR
054590090223C2122X*ex C2086 /EJECT
054600090223C2122X*ex C2086***************************************************
054610090223  "  X*ex C2086 GESTIONE RITORNO DA PGM DI verifica ordini
054620090223  "  X*ex C2086***************************************************
054630090223  "  X*ex C2086 RTNXX044      BEGSR
054640090223  "  X*ex C2086
054650090223  "  X*ex C2086 funzione non eseguibile per errore :
054660090223  "  X*ex C2086
054670090223  "  X*ex C2086          SELECT
054680090223  "  X*ex C2086 1 = funzione richiamata chiusa in errore
054690090223  "  X*ex C2086 ERRXX044      WHENEQ    '1'
054700090223  "  X*ex C2086          MOVE      *ON           $FINE                          sospese READC
054710090223  "  X*ex C2086          SETON                                        9399      emiss.SFLMSGID
054720090223  "  X*ex C2086 2 = mancano dei parametri di input
054730090223  "  X*ex C2086 ERRXX044      WHENEQ    '2'
054740090223  "  X*ex C2086          MOVE      *ON           $FINE                          sospese READC
054750090223  "  X*ex C2086          SETON                                        9599      emiss.SFLMSGID
054760090223  "  X*ex C2086
054770090223  "  X*ex C2086          ENDSL
054780090223  "  X*ex C2086
054790090223  "  X*ex C2086          ENDSR
054800090223C2122X*ex C2086 /EJECT
054810090223C2122X*ex C2086***************************************************
054820090223  "  X*ex C2086 Verifico esistenza impegni collegati al fornitore
054830090223  "  X*ex C2086***************************************************
054840090223  "  X*ex C2086 CtrImpegni    BEGSR
054850090223  "  X*ex C2086
054860090223  "  X*ex C2086          RESET                   PJCX003DS
054870090223  "  X*ex C2086
054880090223  "  X*ex C2086          MOVE      XScSoc        SocCX003
054890090223  "  X*ex C2086
054900090223  "  X*ex C2086          Eval      APCX003     = 'P'
054910090223  "  X*ex C2086
054920090223  "  X*ex C2086          if        WKscFact    = *Blank
054930090223  "  X*ex C2086          Eval      KccCX003    = $Kcc
054940090223  "  X*ex C2086          Eval      KscCX003    = D1Ksc
054950090223  "  X*ex C2086          else
054960090223  "  X*ex C2086          Eval      KccCX003    = WKccFact
054970090223  "  X*ex C2086          Eval      KscCX003    = WKscFact
054980090223  "  X*ex C2086          endif
054990090223  "  X*ex C2086          Eval      DtFatCX003  = Dt0
055000090223  "  X*ex C2086          MOVEL     *Off          ErrCX003
055010090223  "  X*ex C2086
055020090223  "  X*ex C2086          MOVEL     PJCX003DS     KPJBU
055030090223  "  X*ex C2086          CALL      'PJCX003R'
055040090223  "  X*ex C2086          PARM                    KPJBA
055050090223  "  X*ex C2086
055060090223  "  X*ex C2086          MOVEL     KPJBU         PJCX003DS
055070090223  "  X*ex C2086 ritorno da PGM gestione
055080090223  "  X*ex C2086          EXSR      RTNCX003
055090090223  "  X*ex C2086
055100090223  "  X*ex C2086          ENDSR
055110090223C2122X*ex C2086 /EJECT
055120090223C2122X*ex C2086***************************************************
055130090223  "  X*ex C2086 GESTIONE RITORNO DA PGM DI verifica impegni
055140090223  "  X*ex C2086***************************************************
055150090223  "  X*ex C2086 RTNCX003      BEGSR
055160090223  "  X*ex C2086
055170090223  "  X*ex C2086 funzione non eseguibile per errore :
055180090223  "  X*ex C2086
055190090223  "  X*ex C2086          SELECT
055200090223  "  X*ex C2086 1 = funzione richiamata chiusa in errore
055210090223  "  X*ex C2086 ERRCX003      WHENEQ    '1'
055220090223  "  X*ex C2086          MOVE      *ON           $FINE                          sospese READC
055230090223  "  X*ex C2086          SETON                                        9399      emiss.SFLMSGID
055240090223  "  X*ex C2086 2 = mancano dei parametri di input
055250090223  "  X*ex C2086 ERRCX003      WHENEQ    '2'
055260090223  "  X*ex C2086          MOVE      *ON           $FINE                          sospese READC
055270090223  "  X*ex C2086          SETON                                        9599      emiss.SFLMSGID
055280090223  "  X*ex C2086
055290090223  "  X*ex C2086          ENDSL
055300090223  "  X*ex C2086
055310090223  "  X*ex C2086          ENDSR
055320090223C2122X*ex C2086 /EJECT
055330090223C2122X*ex C2086***************************************************
055340090223  "  X*ex C2086 Avverto che per il fornitore esistono
055350090223  "  X*ex C2086 ordini o impegni ancora in controllo fatture
055360090223  "  X*ex C2086***************************************************
055370090223  "  X*ex C2086 MsgOrd        BEGSR
055380090223  "  X*ex C2086
055390090223  "  X*ex C2086          Eval      $MsgCd1     = *Blank
055400090223  "  X*ex C2086          if        EsOrdini    = *On
055410090223  "  X*ex C2086          Eval      $MsgCd1     = 'NDC1073'
055420090223  "  X*ex C2086          endif
055430090223  "  X*ex C2086
055440090223  "  X*ex C2086          Eval      $MsgCd2     = *Blank
055450090223  "  X*ex C2086          if        EsImpegni   = *On
055460090223  "  X*ex C2086          Eval      $MsgCd2     = 'NDC1074'
055470090223  "  X*ex C2086          endif
055480090223  "  X*ex C2086
055490090223  "  X*ex C2086          CLEAR                   XMSGDS
055500090223  "  X*ex C2086
055510090223  "  X*ex C2086          MOVEL     'N'           MSGRSP
055520090223  "  X*ex C2086          MOVEL     '10'          MSGTPR
055530090223  "  X*ex C2086          MOVEL     'S'           MSGEMV
055540090223  "  X*ex C2086          Z-ADD     10            MSGRGP
055550090223  "  X*ex C2086          MOVEL     'N'           MSGCNL
055560090223  "  X*ex C2086          MOVEL     0             MSGMSG
055570090223  "  X*ex C2086          MOVEA     STATUS        STS
055580090223  "  X*ex C2086          MOVEL     'N'           MSGVID
055590090223  "  X*ex C2086          MOVEL     'N'           MSGOPE
055600090223  "  X*ex C2086          MOVEL     'N'           MSGSTP
055610090223  "  X*ex C2086          MOVEL     'N'           MSGLCK
055620090223  "  X*ex C2086
055630090223  "  X*ex C2086          CALL      'XMSGR'
055640090223  "  X*ex C2086          PARM                    XMSGDS
055650090223  "  X*ex C2086          PARM                    XMSGTXT         135
055660090223  "  X*ex C2086
055670090223  "  X*ex C2086  Modo di inizializzazione della window:
055680090223  "  X*ex C2086 ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
055690090223  "  X*ex C2086 ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
055700090223  "  X*ex C2086               per mantenere il sottofondo del video princip.)
055710090223  "  X*ex C2086          PARM      $PrimoXMsg    XMSGINZ           1
055720090223  "  X*ex C2086
055730090223  "  X*ex C2086          PARM      $MsgCd1       XMSGCd1           7
055740090223  "  X*ex C2086          PARM      $MsgCd2       XMSGCd2           7
055750090223  "  X*ex C2086          PARM      *BLANKS       XMSGCd3           7
055760090223  "  X*ex C2086          PARM      *BLANKS       XMSGSn1           7
055770090223  "  X*ex C2086          PARM                    XMSGCdTxt        63
055780090223  "  X*ex C2086
055790090223  "  X*ex C2086          Eval      $PrimoXMsg  = *Off
055800090223  "  X*ex C2086
055810090223  "  X*ex C2086          ENDSR
055820090223C2122X*ex C2086 /EJECT
055830090219C2122C************************************************************
055840090223  "  C* Richiamo window per andare in controllo fatture
055850090219  "  C************************************************************
055860090223C2122C     CallNDC711    BEGSR
055870090219     C*
055880090223     C                   RESET                   NDC711DS
055890090219     C*
055900090223     C                   MOVE      xscsoc        Societa711
055910090223     C                   MOVEL     '01'          OPZ711
055920090223     C*
055930090223     C                   if        WKscFact    = *Blank
055940090223     C                   Eval      Kcc711      = $Kcc
055950090223     C                   Eval      Ksc711      = D1Ksc
055960090223     C                   else
055970090223     C                   Eval      Kcc711      = WKccFact
055980090223     C                   Eval      Ksc711      = WKscFact
055990090223     C                   endif
056000090225     C*
056010090302     C                   Eval      CausTes711  = D1CausTes
056020090223     C*
056030090223     C* Modo di inizializzazione della window:
056040090223     C* ('1'->Close + Open  -> I° volta dopo l'emissione del video princip.)
056050090223     C* ('0'->rimane aperta -> tutte le emissioni successive se consecutive,
056060090223     C*                       per mantenere in sottofondo il video princip.)
056070090720D2530X****************** Eval      Wdw711      = $PrimoXMsg
056080090720D2530C                   Eval      Wdw711      = $inz711
056090090223     C*
056100090223     C                   MoveL     NDC711DS      KPJBU
056110090223     C*
056120090223     C                   CALL      'NDC711R'
056130090223     C                   PARM                    KPJBA
056140090223     C*
056150090223     C                   MoveL     KPJBU         NDC711DS
056160090223     C*
056170090720D2530C                   EVAL      $inz711     = *Off
056180090720D2530X**************     if        Wdw711      = *On
056190090720  "  X**************     Eval      $PrimoXMsg  = *Off
056200090720D2530X**************     endif
056210090223     C*
056220090223     C* se ho richiamato un controllo fatture specifico
056230090223     C* ritorno alla videata iniziale della primanota
056240090223     C                   Eval      Ritorno_D1  = *Off
056250090225     C                   if        ERR711      = *Off     and
056260090225     C                             OPR711      = '1'
056270090223     C                   Eval      Ritorno_D1  = *On
056280090720D2530c                   else
056290090720  "  C* eseguo write per non perdere lo sfondo!!!!
056300090720D2530c                   WRITE     FMTD1
056310090223     C                   endif
056320090219     C*
056330090219C2122C                   ENDSR
056340090219C2122C/EJECT
056350950719     C************************************************************
056360960517     C* Reperimento dati modello primanota
056370950719     C************************************************************
056380960517     C     RepDatiMod    BEGSR
056390950922     C*
056400090721D2530c* salvo codice modello
056410090721  "  c                   if        SavPnot=*blanks
056420090721  "  c                   eval      SAvPNot=D1TabPnot
056430090721  "  c                   endif
056440090721  "  c* se codice modello digitato differente, chiedo se si vogliono
056450090721  "  C* riimpostare i dati
056460090721  "  C                   If        D1TabPnot <> SavPnot and
056470090721  "  C                             SavPnot   <> *Blank
056480090721  "   *    ...chiedo all'utente se vuole cambiare i default
056490090721  "  C                   EXSR      WinChgDftM
056500090721  "  c                   eval      SAvPNot=D1TabPnot
056510090721D2530c                   endif
056520090721     c*
056530960517     C                   RESET                   NDC011DS
056540960517     C                   EVAL      Societa011 = XscSoc
056550960517     C                   EVAL      TabPNot011 = D1TabPNot
056560960517     C                   EVAL      Utente011 = XscCUt
056570960517     C                   EVAL      DocIVA011 = '1'
056580960517     C                   MOVEL     NDC011DS      KPJBU
056590960517     C*
056600960517     C                   CALL      'NDC011R'
056610960517     C                   PARM                    KPJBA
056620960517     C*
056630960517     C                   MOVEL     KPJBU         NDC011DS
056640960517     C*
056650040308     C                   SELECT
056660960517     C* rcd NDTPN (testata modello) non trovato
056670960517     C                   WHEN      Err011 = '1'
056680040429D0611X****************** SETON                                        8599
056690001023D0611 * NDC0010-Modello prima nota inesistente
056700001023  "  C                   Eval      $InD1(85)   = *On
056710001020D0611C                   Eval      *In99       = *On
056720960517     C* registrazione del modello non trovata
056730960517     C                   WHEN      Err011 = '2'
056740040429D0611X****************** SETON                                        7999
056750001023D0611 * NDC0217-Registrazione modello PN non trovata, non valida
056760001023  "   *         per il tipo desiserato oppure priva di movimenti
056770001023  "  C                   Eval      $InD1(79)   = *On
056780001020D0611C                   Eval      *In99       = *On
056790960517     C* tipo registrazione non congruente con quella desiderata
056800960517     C                   WHEN      Err011 = '3'
056810040429D0611X****************** SETON                                        7999
056820001023D0611 * NDC0217-Registrazione modello PN non trovata, non valida
056830001023  "   *         per il tipo desiserato oppure priva di movimenti
056840001023  "  C                   Eval      $InD1(79)   = *On
056850001020D0611C                   Eval      *In99       = *On
056860960517     C* tutto OK
056870960517     C                   OTHER
056880960517     C                   MOVEL     DesBrev011    D1TabPNotD
056890090721     C                   IF        D2Classe = *blank
056900040308     C                   MOVEL     Classe011     D2Classe
056910090721     C                   ENDIF
056920090721     C                   IF        D2SerReg = *blank
056930040308     C                   MOVEL     SerReg011     D2SerReg
056940090721     C                   ENDIF
056950040308     C                   IF        D1CausTes = *blank
056960040308     C                   MOVEL     CausTes011    D1CausTes
056970090721     c                   ENDIF
056980090721     C                   IF        D2Divisa = *blank
056990040308     C                   MOVEL     Divisa011     D2Divisa
057000090721     C                   ENDIF
057010090721     C                   IF        D2Importo = 0
057020040308     C                   Z-ADD     Importo011    D2Importo
057030090721     C                   ENDIF
057040040308     c*
057050030224D1293C* reperisco conto testata da modello primanota
057060030224  "  c                   if        d1ksc=*blanks and ksc011 <> *blanks
057070040308  "  C                   MOVEL     Ksc011        D1Ksc
057080030224  "  c* reperisco anche se 'C' o 'F'
057090030224  "  c                   eval      DataIso=*loval
057100030224  "  C                   reset                   ANRCODS
057110030224  "  C                   EVAL      LenOut = %Size(ANRCODS)
057120030224  "  C                   CallB     'NDMVC002'
057130030224  "  C                   PARM                    xscsoc
057140030224  "  C                   PARM                    kcc011
057150030224  "  C                   PARM                    ksc011
057160030224  "  C                   PARM                    dataiso
057170030224  "  C                   PARM      *OFF          GesMsg
057180030224  "  C                   PARM      *OFF          Esito
057190030224  "  C                   PARM      'ANRCO'       StrutturaO
057200030224  "  C                   PARM                    ANRCODS
057210030224  "  C                   PARM                    LenOut
057220030224  "  C* Errore
057230030224  "  C     Esito         IFEQ      *Off
057240030224  "  c                   EVAL      D1tpsogg=rcosnatura
057250030224  "  c                   endif
057260030224D1293c                   endif
057270030224     c*
057280040308     C                   IF        D1Ksc = *blank
057290040308     C                   IF        KscCpD011 <> *blank
057300040308     C                   MOVEL     KscCpD011     D1Ksc
057310040308     C                   ELSE
057320040308     C                   MOVEL     KscCpA011     D1Ksc
057330040308     C                   ENDIF
057340040308     C                   ENDIF
057350040308     C*
057360040308     C                   ENDSL
057370040308     C*
057380040308     C                   ENDSR
057390110110D2659C************************************************************
057400110110  "  C* Pulizia campi impostati dal modello di prima nota della
057410110110  "  c* registrazione precedente
057420110110  "  C************************************************************
057430110110D2659C     ClrDatiMod    BEGSR
057440110110     C*
057450110110     c* se modello precedente <> blanks e modello a video = *blanks
057460110110     c                   if        SavPnot<>*blanks and
057470110110     c                             d1TabPnot=*blanks
057480110110     c*
057490110110     C                   clear                   D1TabPNotD
057500110110     C                   clear                   D2Classe
057510110110     C                   clear                   D2SerReg
057520110110     C                   clear                   D2Divisa
057530110110     C                   clear                   D2Importo
057540110110     C                   endif
057550110110     C*
057560110110D2659C                   ENDSR
057570110110      ************************************************************
057580090721D2530 * Window x cambiamento default modello
057590090721      ************************************************************
057600090721     C     WinChgDftM    BEGSR
057610090721      *
057620090721     C                   CLEAR                   XMSGDS
057630090721     C                   Clear                   XMSGTXT
057640090721      *
057650090721      *
057660090721     C                   MOVEL     'S'           MSGEMV
057670090721     C                   Z-ADD     10            MSGRGP
057680090721     C                   MOVEL     'S'           MSGRSP
057690090721     C                   MOVEL     'SN'          MSGTPR
057700090721     c                   eval      MsgCd1='COS2768'
057710090721      *
057720090721      *Modo di inizializzazione della window:
057730090721      *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
057740090721      *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
057750090721      *                       per mantenere il sottofondo del video princip.)
057760090721     C                   Eval      XMSGINZ     = $PrimoXMsg
057770090721     C                   If        $PrimoXMsg  = *On
057780090721     C                   Eval      $PrimoXMsg  = *Off
057790090721     C                   EndIf
057800090721      *
057810090721     C                   CALL      'XMSGR'
057820090721     C                   PARM                    XMSGDS
057830090721     C                   PARM                    XMSGTXT         135
057840090721     C                   PARM                    XMSGINZ           1
057850090721     C                   PARM                    MSGCd1            7
057860090721      *
057870090721     C                   If        MsgRtn      = 'S'
057880090721     C                   clear                   D2SerReg
057890090721     C                   clear                   D2Classe
057900090721     C                   clear                   D2Divisa
057910090721     C                   clear                   D2Importo
057920090721     C                   EndIf
057930090721      *
057940090721D2530C                   ENDSR
057950981020     C************************************************************
057960981020     C* Richiesta uso del modello collegato al conto
057970981020     C************************************************************
057980981020     C     RichUsoMod    BEGSR
057990981020     C*
058000990208     C                   CLEAR                   XMsgDS
058010990505C1129C                   CLEAR                   XMSGTXT
058020991012     C* reperisco i msg da passare a XMSGR:
058030991012C1251 * "Il modello primanota &1 è collegato al conto in esame;"
058040991012C1251 * "lo si vuole utilizzare (1) oppure no (2) ?"
058050981020     C                   DO        2             x
058060981020     C                   MOVEL     MsgId(X)      IDMSG
058070981020     C                   CALLB     'XRTVM'
058080981020     C                   PARM                    IDMSG            27
058090981020     C                   PARM                    TXTMSG          100
058100981020     C                   PARM                    ERRMSG            1
058110981021     C                   PARM                    *omit
058120981021     C                   PARM                    *omit
058130981021     C                   PARM      RCOTabPNot    Msgdta          100
058140981020     C*
058150981020     C     ERRMSG        IFNE      *ON
058160981020     C                   MOVEL     TXTMSG        MsgT(X)
058170981020     C                   ELSE
058180981020     C                   MOVEL     *ALL' '       MsgT(X)
058190981020     C                   ENDIF
058200981020     C                   ENDDO
058210981020     C*
058220981020     C                   MOVEL     MSGT(1)       MSGMS1
058230981020     C                   MOVEL     MSGT(2)       MSGMS2
058240981020     C                   MOVEL     *blank        MSGMS3
058250981020     C*
058260040429C1129X*Usare XTxt per 'distribuire' le risposte nella window,
058270040429C1129X*è superfluo perchè vengono già distribuite accanto
058280040429C1129X*ai relativi bottoni  automaticamente da XMsgR
058290040429C1129X*se in XMsgTxt riceve la successione delle possibili risposte,
058300040429C1129X*distanziate di 15 caratteri
058310040429C1129X****************** EVAL      XTXT  = *blank
058320981020     C* reperisco i bottoni da passare a XMSGR
058330991012C1251 * Risposte possibili: "Utilizzare     Non utilizzare"
058340981020     C                   MOVEL     MsgId(3)      IDMSG
058350981020     C                   CALLB     'XRTVM'
058360981020     C                   PARM                    IDMSG            27
058370981020     C                   PARM                    TXTMSG          100
058380981020     C                   PARM                    ERRMSG            1
058390981020     C*
058400981020     C     ERRMSG        IFNE      *ON
058410040429C1129X****************** EVAL      XTxt1 = %subst(TXTMSG :1 :15)
058420040429C1129X****************** EVAL      XTxt2 = %subst(TXTMSG :16:15)
058430990429C1129C                   MOVEL     TXTMSG        XMSGTXT
058440981020     C                   ENDIF
058450040429C1129X****************** EVAL      XMsgTxt = XTxt
058460981020     C*
058470981020     C                   MOVEL     'S'           MSGRSP
058480981020     C                   MOVEL     '12'          MSGTPR
058490981020     C                   MOVEL     'S'           MSGEMV
058500990503     C                   Z-ADD     10            MSGRGP
058510990204      *
058520990505C1129 *Modo di inizializzazione della window:
058530990505C1129 *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
058540990505C1129 *('0'->rimane aperto -->tutte le emissioni successive se consecutive,
058550990505C1129 *                       per mantenere il sottofondo del video princip.)
058560990204     C                   Eval      XMSGINZ     = $PrimoXMsg
058570990204     C                   If        $PrimoXMsg  = *On
058580990204     C                   Eval      $PrimoXMsg  = *Off
058590990204     C                   EndIf
058600990204      *
058610981020     C                   CALL      'XMSGR'
058620981020     C                   PARM                    XMSGDS
058630981020     C                   PARM                    XMSGTXT         135
058640990204     C                   PARM                    XMSGINZ           1
058650981020     C*
058660981020     C                   SELECT
058670981020     C* Utilizzare il modello
058680981020     C     MsgRtn        WHENEQ    '1'
058690981020     C                   EVAL      D1TabPNot = RCOTabPNot
058700981020     C* Non utilizzarlo
058710981020     C     MsgRtn        WHENEQ    '2'
058720981020     C* non deve fare niente
058730060123     C*
058740981020     C                   ENDSL
058750981020     C*
058760981020     C                   ENDSR
058770960517     C************************************************************
058780960517     C* Reperimento dati del soggetto e della sua natura (C/F)
058790960517     C************************************************************
058800960517     C     RepDatiSog    BEGSR
058810960517     C*
058820950922     C* richiamo il driver dei soggetti per recuperare i dati
058830950922     C* dell'indirizzo amministrativo (tipo/codice indirizzo contabile)
058840950922     C* e i dati della natura del conto
058850950922     C                   MOVE      '1'           DVARic
058860950922     C                   MOVE      XSCSOC        DVASocieta
058870950922     C                   MOVE      D1TpSogg      DVASNatura
058880950922     C                   MOVE      D1Ksc         DVACodice
058890950922     C                   CLEAR                   DVALineaV
058900950922     C                   CLEAR                   DVAFiliale
058910950922     C                   MOVE      RCOTpInd      DVATpInd
058920950922     C                   MOVE      RCOCdInd      DVACdInd
058930950927     C                   MOVEL     *blank        DVAStrutt
058940950922     C                   IF        D1TpSogg = 'C'
058950950927     C                   MOVEL     'DVACLI'      DVAStrutt
058960950922     C                   EVAL      DVALenOut = %Size(DVACLI)
058970950922     C                   ELSE
058980950922     C                   MOVEL     'DVAFOR'      DVAStrutt
058990950922     C                   EVAL      DVALenOut = %Size(DVAFOR)
059000950922     C                   ENDIF
059010950922     C*
059020950922     C                   CALLB     'NDDVASOG'
059030950922     C                   PARM                    DVARIC
059040950922     C                   PARM                    DVASOCIETA
059050960313     C                   PARM      Unita015      DVAUNITA
059060950922     C                   PARM                    DVASTRUTT
059070950922     C                   PARM                    DVADTRIF
059080950922     C                   PARM                    DVALENOUT
059090950922     C                   PARM                    DVASNATURA
059100950922     C                   PARM                    DVACODICE
059110950922     C                   PARM                    DVALINEAV
059120950922     C                   PARM                    DVAFILIALE
059130950922     C                   PARM                    *omit
059140950922     C                   PARM                    DVATPIND
059150950922     C                   PARM                    DVACDIND
059160950922     C                   PARM                    DVAERRORE
059170950922     C                   PARM                    DVAOUTPUT
059180950922     C*
059190951002     C                   EVAL      Agente015 =*Blank
059200040308     C                   IF        DVAErrore<>*ON
059210950922     C                   IF        D1TpSogg = 'C'
059220950922     C                   EVAL      %subst(DVACLI:1:DVALENOUT)
059230950922     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
059240951002     C                   EVAL      Agente015  =DVCAgente1
059250990218     C                   EVAL      D1RagSocL=DVCDesFil
059260950922     C                   EVAL      D1IndrizL=DVCIndriz
059270950922     C                   EVAL      D1LocalitL=DVCLocalit
059280950922     C                   EVAL      D1CAPL=DVCCAP
059290950922     C                   EVAL      D1ProvL=DVCProv
059300950922     C                   EVAL      D1PartIva=DVCPartIVA
059310950922     C                   EVAL      D1CdFisc=DVCCdFisc
059320950922     C* se sto lavorando su di una fattura
059330950922     C                   IF        Fattura = *on
059340950922     C* reperisco la condizione di pagamento di defaut
059350951004     C                   IF        D2CondPag = *blank
059360950922     C                   EVAL      D2CondPag = DVCCondPag
059370950922     C                   ENDIF
059380951004     C                   ENDIF
059390950922     C                   MOVEL     DVCDesBrev    D1KscD
059400950922     C                   MOVEL     DVCDes        Descr44
059410950922     C                   MOVEL     DVCDesAgg     DesAgg44
059420950922     C*
059430950922     C                   ELSE
059440950922     C                   EVAL      %subst(DVAFOR:1:DVALENOUT)
059450950922     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
059460990218     C                   EVAL      D1RagSocL=DVFDesFil
059470950922     C                   EVAL      D1IndrizL=DVFIndriz
059480950922     C                   EVAL      D1LocalitL=DVFLocalit
059490950922     C                   EVAL      D1CAPL=DVFCAP
059500950922     C                   EVAL      D1ProvL=DVFProv
059510950922     C                   EVAL      D1PartIva=DVFPartIVA
059520950922     C                   EVAL      D1CdFisc=DVFCdFisc
059530950922     C* se sto lavorando su di una fattura
059540950922     C                   IF        Fattura = *on
059550950922     C* reperisco la condizione di pagamento di defaut
059560951004     C                   IF        D2CondPag = *blank
059570951004     C                   EVAL      D2CondPag = DVFCondPag
059580951004     C                   ENDIF
059590950922     C                   ENDIF
059600950922     C                   MOVEL     DVFDesBrev    D1KscD
059610950922     C                   MOVEL     DVFDes        Descr44
059620950922     C                   MOVEL     DVFDesAgg     DesAgg44
059630950922     C                   ENDIF
059640950922     C                   ENDIF
059650950922     C*
059660950922     C* richiamo il driver dei soggetti per recuperare i dati
059670950922     C* dell'indirizzo legale
059680950922     C* solo se il tipo/codice indirizzo di quello amministrativo non è ' '
059690950922     C                   IF        RCOTpInd <> *blank         or
059700950922     C                             RCOCdInd <> *blank
059710950922     C                   MOVE      '1'           DVARIC
059720950922     C                   MOVE      XSCSOC        DVASOCIETA
059730950922     C                   MOVE      RCOSogg       DVASogg
059740950922     C                   MOVE      *blank        DVATpInd
059750950922     C                   MOVE      *blank        DVACdInd
059760950927     C                   MOVEL     *blank        DVAStrutt
059770950927     C                   MOVEL     'ANIND'       DVAStrutt
059780950922     C                   EVAL      DVALenOut = %Size(ANINDDS)
059790950922     C*
059800950922     C                   CALLB     'NDDVASOG'
059810950922     C                   PARM                    DVARIC
059820950922     C                   PARM                    DVASOCIETA
059830960313     C                   PARM      Unita015      DVAUNITA
059840950922     C                   PARM                    DVASTRUTT
059850950922     C                   PARM                    DVADTRIF
059860950922     C                   PARM                    DVALENOUT
059870950922     C                   PARM                    *omit
059880950922     C                   PARM                    *omit
059890950922     C                   PARM                    *omit
059900950922     C                   PARM                    *omit
059910950922     C                   PARM                    DVASOGG
059920950922     C                   PARM                    DVATPIND
059930950922     C                   PARM                    DVACDIND
059940950922     C                   PARM                    DVAERRORE
059950950922     C                   PARM                    DVAOUTPUT
059960950922     C*
059970040308     C                   if        DVaerrore<>*ON
059980950922     C                   eval      %subst(ANINDDS:1:DVALENOUT)
059990950922     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
060000950922     C                   EVAL      D1RagSocA=IndDesFil
060010950922     C                   EVAL      D1IndrizA=IndIndriz
060020950922     C                   EVAL      D1LocalitA=IndLocalit
060030950922     C                   EVAL      D1ProvA=IndProv
060040950922     C                   EVAL      D1CAPA=IndCAP
060050950922     C                   ENDIF
060060950922     C* se il tipo/codice indirizzo di quello amministrativo è ' '
060070950922     C* ricopio gli stessi dati
060080950922     C                   ELSE
060090950922     C                   EVAL      D1RagSocA=D1RagSocL
060100950922     C                   EVAL      D1IndrizA=D1IndrizL
060110950922     C                   EVAL      D1LocalitA=D1LocalitL
060120950922     C                   EVAL      D1ProvA=D1ProvL
060130950922     C                   EVAL      D1CAPA=D1CAPL
060140950922     C                   ENDIF
060150950922     C*
060160950922     C                   ENDSR
060170950922     C************************************************************
060180950922     C* Eseguo quei ctrl autorizzazioni
060190950922     C* Routine di controlli comune alle due videate
060200950922     C************************************************************
060210950922     C     CtrlBase      BEGSR
060220950922     C*
060230950719     C                   SETOFF                                       99
060240950808     C                   EVAL      NonAutoriz = *off
060250950719     C*
060260950719     C* utente autorizzato alla contabilità
060270950719     C*
060280950719     C                   RESET                   XAutCtbDS
060290950719     C                   EVAL      XACPrf = KNmUs
060300950719     C                   EVAL      XACSoc = XscSoc
060310950719     C                   EVAL      XACCtb = CTB015
060320950719     C                   CALL      'XAUTCTB'
060330950719     C                   PARM                    XAutCtbDS
060340950719     C                   IF        XACAbl = *off
060350040429C1410X****************** EVAL      NonAutoriz = *on
060360000821C1410 * memorizzo qual'è l'autorizzazione mancante
060370000821  "   * per 'comunicarlo' a NDC001R che emetterà il msg specifico
060380000821C1410C                   Eval      NonAutoriz = 'B'
060390040429D0611X****************** SETON                                        4099
060400001023D0611 * PRO0354-Utente non autorizzato alla contabilità (D1/D2MSG)
060410001023  "  C                   Eval      $InD1(40)   = *On
060420001020  "  C                   Eval      $InD2(40)   = *On
060430001020D0611C                   Eval      *In99       = *On
060440950719     C*
060450950719     C* flag tipo registrazione compatibile con contabilità
060460950719     C*
060470950719     C                   ELSE
060480040308     C                   IF        XACDef = *off  and
060490950719     C                             tpregZ015 = 'D' or
060500040308     C                             XACPro = *off  and
060510950719     C                             tpregz015 = 'P' or
060520040308     C                             XACGes = *off  and
060530950719     C                             tpregz015 = 'G'
060540040429C1410X****************** EVAL      NonAutoriz = *on
060550000821C1410 * memorizzo qual'è l'autorizzazione mancante
060560000821  "   * per 'comunicarlo' a NDC001R che emetterà il msg specifico
060570000821C1410C                   Eval      NonAutoriz = 'C'
060580040429D0611X****************** SETON                                        8699
060590001023D0611 * MVC0036-Utente non abilitato al tipo registrazione per la
060600001023  "   *         la contabilità scelta (D1/D2MSG)
060610001023  "  C                   Eval      $InD1(86)   = *On
060620001020  "  C                   Eval      $InD2(86)   = *On
060630001020D0611C                   Eval      *In99       = *On
060640950719     C                   ENDIF
060650950719     C                   ENDIF
060660950719     C*
060670950719     C* utente autorizzato alla unità
060680950719     C*
060690951002     C* se gestita
060700951002     C     FgO1          IFEQ      *on
060710950719     C                   RESET                   XAutUniDS
060720990809C1212C                   RESET                   XAutUniDS1
060730950719     C                   EVAL      XAUPrf = KNmUs
060740950719     C                   EVAL      XAUSoc = XscSoc
060750950719     C                   EVAL      XAUUni = D1Unita
060760950719     C                   CALL      'XAUTUNI'
060770950719     C                   PARM                    XAutUniDS
060780990809C1212C                   PARM                    XAutUniDS1
060790950719     C                   IF        XAUAbl = *off
060800040429C1410X****************** EVAL      NonAutoriz = *on
060810000821C1410 * memorizzo qual'è l'autorizzazione mancante
060820000821  "   * per 'comunicarlo' a NDC001R che emetterà il msg specifico
060830000821C1410C                   Eval      NonAutoriz = 'D'
060840040429D0611X****************** SETON                                        4199
060850001023D0611 * PRO0359-Utente non autorizzato all'unità (D1/D2MSG)
060860001023  "  C                   Eval      $InD1(41)   = *On
060870001020  "  C                   Eval      $InD2(41)   = *On
060880001020D0611C                   Eval      *In99       = *On
060890990809C1212 *
060900990809  "   * se utente autorizzato alla unità di registrazione
060910990809  "  C                   Else
060920990809  "   *  controllo se tipo registrazione compatibile con lunità
060930990809  "  C                   If        XAUDef = *Off  and
060940990809  "  C                             TpRegZ015 = 'D' or
060950990809  "  C                             XAUProv = *Off  and
060960990809  "  C                             TpRegZ015 = 'P' or
060970990809  "  C                             XAUGest = *Off  and
060980000821C1212C                             TpRegZ015 = 'G'
060990040429C1410X****************** EVAL      NonAutoriz = *on
061000000821C1410 * memorizzo qual'è l'autorizzazione mancante
061010000821  "   * per 'comunicarlo' a NDC001R che emetterà il msg specifico
061020000821C1410C                   Eval      NonAutoriz = 'E'
061030040429D0611X**ex C1212 ******* SETON                                        4399
061040001023D0611 * PRO1023-
061050000821C1212 *    'Utente non abilitato al tipo registrazione per l'unità scelta'
061060001023D0611C                   Eval      $InD1(43)   = *On
061070001020D0611C                   Eval      *In99       = *On
061080001020C1212C                   EndIf
061090990809C1212 *
061100950719     C                   ENDIF
061110951002     C                   ENDIF
061120950719     C*
061130950719     C* utente autorizzato alla causale
061140950719     C*
061150950719     C                   RESET                   XAutCauDS
061160950719     C                   EVAL      XAKSocieta = XscSoc
061170950719     C                   EVAL      XAKUnita = D1Unita
061180950719     C                   EVAL      XAKCtb = Ctb015
061190950719     C                   EVAL      XAKCausale = D1CausTes
061200950719     C                   EVAL      XAKOpeStp = OpeStp
061210950719     C                   EVAL      XAKPrfR = KNmUs
061220950719     C                   MOVEL     XscGrp        XAKGruppoR
061230950719     C                   CALL      'XAUTCAU'
061240950719     C                   PARM                    XAutCauDS
061250950719     C                   IF        XAKAbil = *off
061260040429C1410X****************** EVAL      NonAutoriz = *on
061270000821C1410 * memorizzo qual'è l'autorizzazione mancante
061280000821  "   * per 'comunicarlo' a NDC001R che emetterà il msg specifico
061290000821C1410C                   Eval      NonAutoriz = 'F'
061300040429D0611X****************** SETON                                        4299
061310001023D0611 * PRO0360-Utente non autorizzato alla causale contabile (D1/D2MSG)
061320001023  "  C                   Eval      $InD1(42)   = *On
061330001020  "  C                   Eval      $InD2(42)   = *On
061340001020D0611C                   Eval      *In99       = *On
061350950719     C                   ENDIF
061360950719     C*
061370950719     C                   ENDSR
061380950719     C*
061390001020     C************************************************************
061400950719     C* Controlli/valorizzazioni date
061410950719     C************************************************************
061420950720     C     CtrlValD2     BEGSR
061430950719     C*
061440950719     C                   SETOFF                                       99
061450950719     C*
061460990427C1176 *Ripristino l'attributo che D2Importo aveva prima di essere illuminato
061470990427  "   *(si è illumina se provenendo dal sfl Iva, l'ho impostato
061480990427  "   * con il valore giusto: sommatoria imponibile+imposte)
061490990427  "  C                   If        D2ImportoA  = Illuminato
061500990427  "  C                   Eval      D2ImportoA  = $D2ImportA
061510990427  "  C                   EndIf
061520990427C1176 *
061530990427     C* ERRORI SULLE DATE
061540950817     C*
061550950914     C                   Z-ADD     0             §Data
061560950809     C                   MOVE      *blank        §CDt
061570950809     C                   MOVE      *blank        §TDt
061580001024     C*
061590950719     C* data registrazione formalmente valida
061600950719     C*
061610950719     C                   IF        D2DtReg  > 0
061620960202     C                   CALLB     'XDT4000'
061630950914     C                   PARM                    XDTAMG           10
061640950914     C                   PARM      D2DtReg       XDTGMA            6 0
061650950914     C                   PARM      3             XDTSTA            1 0
061660950914     C     XdtSta        IFLT      0
061670040429D0611X****************** SETON                                        4499
061680001023D0611 * PRO0001-Data errata
061690001023  "  C                   Eval      $InD2(44)   = *On
061700001020D0611C                   Eval      *In99       = *On
061710040429C1547X**ex m001          else
061720040429C1547X**ex m001          movel     xdtamg        svdtareg
061730950914     C                   ENDIF
061740950719     C* se la data non è valorizzata
061750950719     C                   ELSE
061760950719     C* tento di valorizzarla con i suoi dft
061770950719     C                   IF        CiSonoDft = *on
061780950809     C                   MOVE      DftCDtReg     §CDt
061790950809     C                   MOVE      *blank        §TDt
061800950719     C                   EXSR      ValDataDft
061810950811     C                   MOVE      §Data         D2DtReg
061820950719     C                   ENDIF
061830970203     C* se ancora a zero, provo a prendere la data dal numeratore
061840970203     C                   IF        D2DtReg  = 0
061850970203     C                   EXSR      RUltNum
061860970203     c                   endif
061870970203     C* se dopo tutti i tentativi non sono riuscito a valorizzare la data
061880950802     C* dò errore
061890950802     C                   IF        D2DtReg  = 0
061900040429D0611X****************** SETON                                        9944
061910001023D0611 * PRO0001-Data errata
061920001023  "  C                   Eval      $InD2(44)   = *On
061930001020D0611C                   Eval      *In99       = *On
061940950802     C                   ENDIF
061950950719     C                   ENDIF
061960950719     C*
061970950719     C* data documento formalmente valida
061980950719     C*
061990950719     C                   IF        D2DtDoc  > 0
062000960202     C                   CALLB     'XDT4000'
062010950914     C                   PARM                    XDTAMG           10
062020950914     C                   PARM      D2DtDoc       XDTGMA            6 0
062030950914     C                   PARM      3             XDTSTA            1 0
062040950914     C     XdtSta        IFLT      0
062050040429D0611X****************** SETON                                        4599
062060001023D0611 * PRO0001-Data errata
062070001023  "  C                   Eval      $InD2(45)   = *On
062080001020D0611C                   Eval      *In99       = *On
062090950914     C                   ENDIF
062100950719     C* se la data non è valorizzata
062110950719     C                   ELSE
062120950719     C* tento di valorizzarla con i suoi dft
062130950719     C                   IF        CiSonoDft = *on
062140950809     C                   MOVE      DftCDtDoc     §CDt
062150950809     C                   MOVE      DftTDtDoc     §TDt
062160950719     C                   EXSR      ValDataDft
062170950811     C                   MOVE      §Data         D2DtDoc
062180950719     C                   ENDIF
062190950719     C                   ENDIF
062200010621     C*
062210020226C1547C* data libera=data decorrenza pagamento
062220010621C1494C* data libera    formalmente valida
062230010621  "  C*
062240010621  "  C                   IF        D2Dtlib  > 0
062250010621  "  C                   CALLB     'XDT4000'
062260010621  "  C                   PARM                    XDTAMG           10
062270010621  "  C                   PARM      D2DtLib       XDTGMA            6 0
062280010621  "  C                   PARM      3             XDTSTA            1 0
062290010821C1494C     XdtSta        IFLT      0
062300040429C1547X**ex m001     xdtamg        orlt      svdtareg
062310010821C1494 * PRO0001-Data errata
062320010621  "  C                   Eval      $InD2(34)   = *On
062330010621  "  C                   Eval      *In99       = *On
062340010621  "  C                   ENDIF
062350010621  "  C* se la data non è valorizzata
062360010621  "  C                   ELSE
062370010621  "  C* la valorizzo con la data documento
062380010621  "  C                   MOVE      d2dtDoc       d2dtlib
062390010621  "  C                   ENDIF
062400010621C1494C*
062410950719     C* data partita   formalmente valida
062420950719     C*
062430040308     C                   IF        D2DtPar  > 0
062440960202     C                   CALLB     'XDT4000'
062450950914     C                   PARM                    XDTAMG           10
062460950914     C                   PARM      D2DtPar       XDTGMA            6 0
062470950914     C                   PARM      3             XDTSTA            1 0
062480040308     C     XDtSta        IFLT      0
062490040429D0611X****************** SETON                                        8499
062500001023D0611 * PRO0001-Data errata
062510001023  "  C                   Eval      $InD2(84)   = *On
062520001020D0611C                   Eval      *In99       = *On
062530040308     C                   ENDIF
062540040308     C* se la data non è valorizzata
062550040308     C                   ELSE
062560040308     C* tento di valorizzarla con i suoi dft
062570040308     C                   IF        CiSonoDft = *on
062580040308     C                   MOVE      DftCDtPar     §CDt
062590040308     C                   MOVE      DftTDtPar     §TDt
062600950719     C                   EXSR      ValDataDft
062610950811     C                   MOVE      §Data         D2DtPar
062620951002     C* se il dft di partita è di reperire la data da quella del documento
062630040308     C                   IF        DftTDtPar = 'D'
062640951002     C* allora reperisco anche numero e serie
062650951002     C* se non sono già stati valorizzati
062660040308     C                   IF        D2NrPar = 0            and
062670951002     C                             D2SerPar = *blank
062680951002     C                   MOVE      D2NrDoc       D2NrPar
062690951002     C                   MOVE      D2SerDoc      D2SerPar
062700040308     C                   ENDIF
062710040308     C                   ENDIF
062720040308     C                   ENDIF
062730040308     C                   ENDIF
062740040308     C*
062750040308     C* data competenza co.ge. formalmente valida
062760040308     C*
062770040308     C                   IF        D2DtCoCo > 0
062780040308     C                   CALLB     'XDT4000'
062790040308     C                   PARM                    XDTAMG           10
062800040308     C                   PARM      D2DtCoCo      XDTGMA            6 0
062810040308     C                   PARM      3             XDTSTA            1 0
062820040308     C     XdtSta        IFLT      0
062830040429D0611X****************** SETON                                        4699
062840001023D0611 * PRO0001-Data errata
062850001023  "  C                   Eval      $InD2(46)   = *On
062860001020D0611C                   Eval      *In99       = *On
062870950914     C                   ENDIF
062880950719     C* se la data non è valorizzata
062890950719     C                   ELSE
062900950719     C* tento di valorizzarla con i suoi dft
062910950719     C                   IF        CiSonoDft = *on
062920950809     C                   MOVE      DftCDtCoCo    §CDt
062930950809     C                   MOVE      DftTDtCoCo    §TDt
062940950719     C                   EXSR      ValDataDft
062950950811     C                   MOVE      §Data         D2DtCoCo
062960950719     C                   ENDIF
062970950719     C                   ENDIF
062980950719     C*
062990950719     C* data competenza analitica formalmente valida
063000950719     C*
063010950719     C                   IF        D2DtCoAn > 0
063020960202     C                   CALLB     'XDT4000'
063030950914     C                   PARM                    XDTAMG           10
063040950914     C                   PARM      D2DtCoAn      XDTGMA            6 0
063050950914     C                   PARM      3             XDTSTA            1 0
063060950914     C     XdtSta        IFLT      0
063070040429D0611X****************** SETON                                        4799
063080001023D0611 * PRO0001-Data errata
063090001023  "  C                   Eval      $InD2(47)   = *On
063100001020D0611C                   Eval      *In99       = *On
063110950914     C                   ENDIF
063120950719     C* se la data non è valorizzata
063130950719     C                   ELSE
063140950719     C* tento di valorizzarla con i suoi dft
063150950719     C                   IF        CiSonoDft = *on
063160950809     C                   MOVE      DftCDtCoAn    §CDt
063170950809     C                   MOVE      DftTDtCoAn    §TDt
063180950719     C                   EXSR      ValDataDft
063190950811     C                   MOVE      §Data         D2DtCoAn
063200950719     C                   ENDIF
063210950719     C                   ENDIF
063220950720     C*
063230981229     C* data reperimento cambio formalmente valida
063240950720     C*
063250981229     C                   IF        D2DtRfCam > 0
063260981229     C                   CALLb     'XDT4000'
063270981229     C                   PARM                    XDTAMG           10
063280981229     C                   PARM      D2DtRfCam     XDTGMA            6 0
063290981229     C                   PARM      3             XDTSTA            1 0
063300981229     C                   IF        XDTSTA <>0
063310040429D0200X****************** SETON                                        6099
063320040429D0611X** ex D0200 ****** SETON                                        3799
063330001023D0611 * PRO0001-Data errata
063340001023  "  C                   Eval      $InD2(37)   = *On
063350001020D0611C                   Eval      *In99       = *On
063360981229     C                   endif
063370981229     C* se la data non è valorizzata
063380981229     C                   ELSE
063390981229     C                   MOVE      Dt0           DtRfCam
063400950720     C                   IF        CiSonoDft = *on
063410950720     C* tento di valorizzarla con i suoi dft
063420950809     C                   MOVE      DftCDtCmb     §CDt
063430950809     C                   MOVE      DftTDtCmb     §TDt
063440950720     C                   EXSR      ValDataDft
063450981229     C                   MOVE      §Data         D2DtRfCam
063460950720     C                   ENDIF
063470950720     C*
063480981229     C*
063490981229     C* se non sono riuscito a valorizzarla
063500981229     C     D2DtRfCam     IFEQ      0
063510981229     C* e non ci sono errori
063520981229     C     *in99         ANDEQ     *off
063530981229     C                   SELECT
063540981229     C* uso la data documento (se valorizzata)
063550981229     C                   WHEN      D2DtDoc > 0
063560981229     C                   EVAL      D2DtRfCam = D2DtDoc
063570981229     C* altrimenti quella di registrazione (che è sicuramente valorizzata)
063580981229     C                   OTHER
063590981229     C                   EVAL      D2DtRfCam = D2DtReg
063600981229     C                   ENDSL
063610981229     C                   ENDIF
063620981229     C                   ENDIF
063630981229     C*
063640981229     C* siccome il valore in aa/mm/gg mi serve spesso, giro subito la data
063650981229     C* video
063660981230     C     D2DtRfCam     IFGT      0
063670981229     C* e non ci sono errori
063680981229     C     *in99         ANDEQ     *off
063690981229     C                   CALLB     'XDT4000'
063700981229     C     DtRfCam       PARM                    XDTAMG           10
063710981229     C                   PARM      D2DtRfCam     XDTGMA            6 0
063720981229     C                   PARM      4             XDTSTA            1 0
063730981229     C                   ENDIF
063740950810     C*
063750950906     C* se nonostante tutto non sono riuscito a valorizzare i riferimenti
063760950906     C* partita
063770950906     C                   IF        D2NrPar = 0              or
063780950906     C                             D2DtPar = 0
063790950906     C* emetto errore
063800040429D0611X****************** SETON                                        9399
063810001023D0611 * NDC0035-Riferimenti partita non corretti (D2Dt/Nr/SerPar)
063820001023  "  C                   Eval      $InD2(93)   = *On
063830001020D0611C                   Eval      *In99       = *On
063840950906     C                   ENDIF
063850950817     C*
063860950817     C* ERRORI VARI
063870001011d0732C* Richiamo modulo controllo conto passando come data la data
063880001011  "  C* registrazione da controllare con la data anullamento del conto
063890001011  "  c                   if        *in99=*off
063900001011  "  C                   CALLB     'XDT4000'
063910001011  "  C                   PARM                    XDTAMG           10
063920001011  "  C                   PARM      D2DtReg       XDTGMA            6 0
063930001011  "  C                   PARM      4             XDTSTA            1 0
063940001011  "  C                   reset                   ANRCODS
063950001011  "  C                   EVAL      LenOut = %Size(ANRCODS)
063960001011  "  C                   CallB     'NDMVC002'
063970001011  "  C                   PARM                    xscsoc
063980001011  "  C                   PARM                    $kcc
063990001011  "  C                   PARM                    d1ksc
064000001011  "  C                   PARM                    XdtAmg
064010001011  "  C                   PARM      *OFF          GesMsg
064020001011  "  C                   PARM      *OFF          Esito
064030001011  "  C                   PARM      'ANRCO'       StrutturaO
064040001011  "  C                   PARM                    ANRCODS
064050001011  "  C                   PARM                    LenOut
064060001011  "  C* Errore
064070001020D0732C     Esito         IFEQ      *ON
064080040429D0611X**ex D0732 ********SETON                                        9199
064090001023D0611 * MVC0002-Conto &3 &4 non corretto
064100001023  "  C                   Eval      $InD2(91)   = *On
064110001020D0611C                   Eval      *In99       = *On
064120001020D0732c                   endif
064130001011D0732c                   endif
064140950817     C*
064150960429     C* se la divisa non è già stata valorizzata
064160960429     C                   IF        D2Divisa = *blank
064170960429     C* imposto la m.c.
064180960429     C                   MOVE      XscDiv        D2Divisa
064190960429     C                   ENDIF
064200960429     C*
064210950817     C* valore non valido nel campo richiesta rate manuali
064220990707D0151C* se ci sono default provo a reperire il default PagMan poi
064230990707  "  C* ne controllo l'esattezza
064240990707  "  C                   If        D2PagMan = *Blank and CiSonoDft = *On
064250990707  "  C                   If        DftPagMan = *On
064260990707  "  C                   Move      XscOn         D2PagMan
064270990707  "  C                   Else
064280990707  "  C                   Move      XscOff        D2PagMan
064290990707  "  C                   EndIf
064300990707  "  C                   EndIf
064310990707D0151C*
064320950817     C                   IF        D2PagMan <> XscOn and D2PagMan <> XscOff
064330040429D0611X****************** SETON                                        8899
064340001023D0611 * PRO0048-Il valore indicato per il campo non è valido
064350001023  "  C                   Eval      $InD2(88)   = *On
064360001020D0611C                   Eval      *In99       = *On
064370950817     C                   ENDIF
064380950817     C*
064390950817     C* se entrambi gli importi sono vuoti
064400961216     C                   If        Ctb015 <> 'MD'
064410980128      * Controllo asteriscato perchè esiste già nel tubero NDMVC011
064420961216     C                   ENDIF
064430980922     C*
064440980922     C* ctrl congruenze decimali e cambio
064450981008     C                   EXSR      CtrImporti
064460950817     C*
064470980119      * Se si gestisce la descr.breve
064480990903      * (*In39=*On acceso solo in Inzd2 solo se si è in Immiss./copia)
064490980119      * gestisco l'errore di obbligatorietà
064500980119     C                   If        OPZ015 <>'05'  And
064510980119     C                             OPZ015 <>'04'
064520980119      * Imposto descriz. della causale se la causale lo richiede
064530040429C1492X****               If        OpeDesMov = '2'    And
064540010529C1492C*
064550010529  "  C                   If        (OpeDesMov = '2'   or
064560010529C1492C                              OpeDesMov = '3'     )  And
064570980119     C                             D2DesBrev = *Blank
064580980119     C                   Eval      D2DesBrev = OpeDesBrev
064590980119     C                   EndIf
064600980119      * Gestione errore:"descrizione movim. obbligatoria"
064610980119     C                   Select
064620980119      *   Errore vincolante se immissione/copia
064630980119     C     OPZ015        Wheneq    '01'
064640980119     C     OPZ015        Oreq      '03'
064650980119     C                   If        OpeDesMov = '1'    And
064660980119     C                             D2DesBrev = *Blank
064670040429D0611X****************** SETON                                        3999
064680001023D0611 * NDC0374-(F1) Descrizione movimento obbligatoria
064690001023  "  C                   Eval      $InD2(39)   = *On
064700001020D0611C                   Eval      *In99       = *On
064710980119     C                   Endif
064720980119      *   Errore NON vincolante se modifica
064730980119     C     OPZ015        Wheneq    '02'
064740990816D0154 *    se passaggio P a D
064750990708D0154C     Opz015        OrEq      '22'
064760990816C1218C*    se passaggio P a G
064770990816C1218C     Opz015        OrEq      '23'
064780980119     C                   If        OpeDesMov = '1'    And
064790980119     C                             D2DesBrev = *Blank And
064800980119     C                             $D2InMsg  = *Off
064810040429D0611X****************** SETON                                        3999
064820001023D0611 * NDC0374-(F1) Descrizione movimento obbligatoria
064830001023  "  C                   Eval      $InD2(39)   = *On
064840001020D0611C                   Eval      *In99       = *On
064850980119     C                   Eval      $D2InMsg  = *On
064860980119     C                   Endif
064870980119     C                   Endsl
064880980119     C                   Endif
064890971219     C*
064900971222     C     XCtrlValD2    ENDSR
064910970203     C*----------------------------------------------------------
064920970203     C* Preleva l'ultimo numeratore della serie della causale
064930970203     C*----------------------------------------------------------
064940970203     C     RUltNum       BEGSR
064950970203     C*
064960970203     C                   Eval      XNMRIC = '6'
064970970203     C                   Eval      XNMSOC = XscSoc
064980090520D2510X***************    Eval      XNMUNI = unita015
064990090520D2510C                   Eval      XNMUNI = D1unita
065000970203     C                   Eval      XNMSER = OpeSer
065010970203     C                   Eval      XNMCTB = ctb015
065020970203     C                   Eval      XNMKEY = TpRegz015
065030970203     C                   Eval      XNMCMT = *OFF
065040970203     C                   Eval      XNMIVA = *OFF
065050970203     C                   Eval      XNMREG = *OFF
065060970203     c                   move      Dt0           xnmdat
065070970203     C*
065080970203     C                   CALLB     'XNMR'        PNMR
065090970203     C*
065100970203     C                   If        XNMERR  = '0'
065110970203     C                   CALLb     'XDT4000'
065120970204     C                   PARM      XNMDAT        XDTAMG           10
065130970203     C     D2DtREG       PARM                    XDTGMA            6 0
065140970203     C                   PARM      2             XDTSTA            1 0
065150970203     c                   endif
065160970203     C*
065170970203     c                   ENDSR
065180950726     C*----------------------------------------------------------
065190950726     C* Esitenza partita
065200950726     C*----------------------------------------------------------
065210950726     C     CtrPartita    BEGSR
065220950726     C*
065230990726C1207C                   Move      *Off          $Correggi
065240990726C1207C*
065250951219     C                   EVAL      ExiPar = *blank
065260951219     C*
065270040430D1602 *---------------------------------------------------------------
065280040430  "   * DATA PARTITA
065290040430  "  C                   exsr      CtrD2DtPar
065300040430D1602 *
065310040430D1602X**riunite in CtrD2DtPar le specifiche di seguito asteriscate
065320040430  "  X**** D2DtPar       IFNE      0
065330040430  "  X****               CALLB     'XDT4000'
065340040430  "  X**** D2DtParISO    PARM                    XDTAMG           10
065350040430  "  X****               PARM      D2DtPar       XDTGMA            6 0
065360040430  "  X****               PARM      4             XDTSTA            1 0
065370040430  "  X****               ELSE
065380040430  "  X****               move      Dt0           d2DtParIso
065390040430  "  X****               ENDIF
065400040430D1602X****
065410040430D1602 *---------------------------------------------------------------
065420040430  "   * SERIE PARTITA
065430040430  "  C                   exsr      CtrD2SerPar
065440040430D1602 *
065450040430D1602X**riunite in CtrD2SerPar le specifiche di seguito asteriscate
065460040430  "  X*** controllo serie partita: deve contenere solo caratteri
065470040429  "  X*** alfanumerici
065480040430  "  X**controllo serie partita: il primo carattere deve essere
065490040430  "  X**compreso tra blank e '9' e non devono essere presenti blank
065500040430  "  X**in testa o in mezzo alla stringa
065510040430  "  X****               if        D2SerPar <> *blank
065520040430  "  X****               eval      X63Codice = D2SerPar
065530040430  "  X****               exsr      CtrlSerDP
065540040430  "  X****               if        X63Errore = *On
065550040430  "  X**ex D0714 ********seton                                        3699
065560040430  "  X**PRO1277-(F1) Sono presenti caratteri non consentiti
065570040430  "  X****               Eval      $InD2(36)   = *On
065580040430  "  X****               Eval      *In99       = *On
065590040430  "  X****               endif
065600040430  "  X****               endif
065610040430D1602X****
065620040430D1602 *---------------------------------------------------------------
065630040430  "   * ESISTENZA LEGITTIMA PARTITA
065640040430  "  C                   exsr      CtrExistPar
065650040430D1602 *
065660040430D1602X**riunite in CtrExistPar le specifiche di seguito asteriscate
065670040430  "  X**se non sto gestendo un modello
065680040430  "  X****               IF        Ctb015 <> 'MD'
065690040430  "  X**controllo la validità estremi partita
065700040430  "  X**controllo solo se i valori a video <> dai valori sui files.
065710040430  "  X****               if        $PPAKcc   <> $kcc            or
065720040430  "  X****                         $PPAKsc   <> d2ksc           or
065730040430  "  X****                         $PPADtPar <> D2DtParIso      or
065740040430  "  X****                         $PPANrPAr <> D2NrPar         or
065750040430  "  X****                         $PPASerPar<> D2SerPar        or
065760040430  "  X**oppure sono in copia
065770040430  "  X****                         Opz015    = '03'
065780040430  "  X**oppure sono in una immissione ma con recupero dati
065790040430  "  X****                         or  ImmExp015 <> '0'
065800040430  "  X****
065810040430  "  X****               Eval      PPASocieta = XscSoc
065820040430  "  X****               Eval      PPACtb     = Ctb015
065830040430  "  X****               Eval      PPAKcc     = $kcc
065840040430  "  X****               Eval      PPAKsc     = d2ksc
065850040430  "  X****               Eval      PPADtPar   = D2DtParIso
065860040430  "  X****               Eval      PPANrPAr   = D2NrPar
065870040430  "  X****               Eval      PPASerPar  = D2SerPar
065880040430  "  X**** K07PPA01      Chain     NDPPA01l                           21
065890040430  "  X****               EVAL      ExiPAr = *in21
065900040430  "  X****
065910040430  "  X**se la partita deve esistere ma non c'è
065920040430  "  X****               If        OPECtPar = '1' and *in21 = *on
065930040430  "  X****************** SETON                                        9983
065940040430  "  X**MVC0052-Riferimento partita non esistente
065950040430  "  X****               Eval      $InD2(83)   = *On
065960040430  "  X****               Eval      *In99       = *On
065970040430  "  X****               EndIf
065980040430  "  X****
065990040430  "  X**se la partita deve essere nuova ma c'è già
066000040430  "  X****               IF        *In21 = *Off   And
066010040430  "  X****                         PPANrPar <> 0  And
066020040430  "  X****                         (OPECtPar = '2' Or OPECtPar = '3') And
066030040430  "  X**e la sua registrazione generante non è la stessa
066040040430  "  X****                         (PPASys <> RegSys  or
066050040430  "  X****                          PPANrAsGen <> RegNrAsReg)
066060040430  "  X****************** SETON                                        9994
066070040430  "  X**MVC0053-Riferimento partita già esistente
066080040430  "  X****               Eval      $InD2(94)   = *On
066090040430  "  X****               Eval      *In99       = *On
066100040430  "  X****               EndIf
066110040430  "  X****
066120040430  "  X**se la partita esiste e non prevede controlli emetto msg di
066130040430  "  X**avvertimento così da far decidere l'utente se continuare o
066140040430  "  X**se modificare la partita
066150040430  "  X****               If        OpeCtPar <> '1' and OpeCtPar <> '2' and
066160040430  "  X****                         OpeCtPar <> '3' and ExiPar = *Off and
066170040430  "  X****                         $MsgParEs = *Off and
066180040430  "  X**e la sua registrazione generante non è la stessa
066190040430  "  X****                         (PPASys <> RegSys  or
066200040430  "  X****                          PPANrAsGen <> RegNrAsReg)
066210040430  "  X****               Exsr      MsgParEs
066220040430  "  X****               EndIf
066230040430  "  X****
066240040430  "  X****               EndIf
066250040430  "  X****
066260040430  "  X**se la partita esiste
066270040430  "  X****               If        ExiPar = *off   and
066280040430  "  X**e la partita doveva esistere oppure non c'erano controli
066290040430  "  X****                         ( OPECtPar = '1' or OPECtPar = ' ' )
066300040430  "  X**e la divisa di partita non è = a quella di registrazione
066310040430  "  X****                         and PPADivisa <> D2Divisa
066320040430  "  X**errore
066330040430  "  X****************** SETON                                        9978
066340040430  "  X**NDC0111-Divisa di partita non può essere diversa da quella di registrazione
066350040430  "  X****               Eval      $InD2(78)   = *On
066360040430  "  X****               Eval      *In99       = *On
066370040430  "  X****               EndIf
066380040430  "  X****               EndIf
066390040430  "  X****
066400040430  "  X**se non ci sono errori, ma l'utente (all'emissione della mia
066410040430  "  X**finestra di avvertimento dell'esistenza partita) ha deciso
066420040430  "  X**di rimanere in videata per correggere la partita, faccio in
066430040430  "  X**modo che non passi ad altra videata
066440040430  "  X****               If        *In99=*Off
066450040430  "  X****               Move      $Correggi     *In99
066460040430  "  X****               EndIf
066470040503D1602X****
066480040503D1602 *---------------------------------------------------------------
066490040430  "   * EFFETTI COLLEGATI ALLA PARTITA
066500040430  "  C                   exsr      CtrEffColP
066510040430D1602 *
066520040430D1602X**riunite in CtrEffColP  le specifiche di seguito asteriscate
066530040430  "  X****
066540040430  "  X**se non sto gestendo un modello e non ci sono errori
066550040430  "  X**P.S:
066560040430  "  X**Non Controllo che la partita esista (If ExiPar = *Off)
066570040430  "  X**xchè in modifica ExiPar rimane = *Blank
066580040430  "  X****               If        Ctb015 <> 'MD'  AND  *In99 = *Off
066590040430  "  X****
066600040430  "  X**testo se esistono effetti collegati alla partita
066610040430  "  X****               CallB     'NDMVC403'
066620040430  "  X****               Parm                    XScSoc
066630040430  "  X****               Parm                    Ctb015
066640040430  "  X****               Parm                    D1TpSogg
066650040430  "  X****               Parm                    $Kcc
066660040430  "  X****               Parm                    D2Ksc
066670040430  "  X****               Parm                    D2DtParISO
066680040430  "  X****               Parm                    D2NrPar
066690040430  "  X****               Parm                    D2SerPar
066700040430  "  X****               Parm                    *Omit
066710040430  "  X****               Parm                    *Omit
066720040430  "  X****               Parm                    Risposta
066730040430  "  X****               Parm                    EffNoCol
066740040430  "  X****               Parm                    *Omit
066750040430  "  X****               Parm                    *Omit
066760040430  "  X****
066770040430  "  X**se ci sono effetti collegati alla partita ma non alla reg
066780040430  "  X****               Move      Normale       D2PagManA
066790040430  "  X****               If        EffNoCol   <> '0'
066800040430  "  X****
066810040430  "  X**** imposto Rate manuali='S'
066820040430  "  X****               Eval      D2PagMan    = XScOn
066830040430  "  X****
066840040430  "  X**** e lo proteggo
066850040430  "  X****               Move      Forzato       D2PagManA
066860040430  "  X****
066870040430  "  X**** avverto l'utente gli chiedo se continuare o ritornare
066880040430  "  X****               EXSR      WinEffNoCo
066890040430  "  X****
066900040430  "  X****               EndIf
066910040430  "  X****
066920040430  "  X****               EndIf
066930040503D1602X****
066940040429C1429X****************** ENDSR
066950001025C1429C     XCtrPartit    ENDSR
066960040430D1602 ************************************************************
066970040430  "   * Controllo data partita
066980040430  "  X* (specifiche preesistenti in CtrPartita e qui riunite)
066990040430  "   ************************************************************
067000040430  "  C     CtrD2DtPar    Begsr
067010040430D1602 *
067020040430     C     D2DtPar       IFNE      0
067030040430     C                   CALLB     'XDT4000'
067040040430     C     D2DtParISO    PARM                    XDTAMG           10
067050040430     C                   PARM      D2DtPar       XDTGMA            6 0
067060040430     C                   PARM      4             XDTSTA            1 0
067070040430     C                   ELSE
067080040430     C                   move      Dt0           d2DtParIso
067090040430     C                   ENDIF
067100040430D1602 *
067110040430  "  C     XCtrD2DtPar   Endsr
067120040430  "   ************************************************************
067130040430  "   * Controllo serie partita
067140040430  "  X* (specifiche preesistenti in CtrPartita e qui riunite)
067150040430  "   ************************************************************
067160040430  "  C     CtrD2SerPar   Begsr
067170040430D1602 *
067180040430D0794X*** controllo serie partita: deve contenere solo caratteri
067190040430  "  X*** alfanumerici
067200040430  "  C* controllo serie partita: il primo carattere deve essere
067210040430  "  C* compreso tra blank e '9' e non devono essere presenti blank
067220040430D0794C* in testa o in mezzo alla stringa
067230040430D0714C                   if        D2SerPar <> *blank
067240040430  "  C                   eval      X63Codice = D2SerPar
067250040430  "  C                   exsr      CtrlSerDP
067260040430D0714C                   if        X63Errore = *On
067270040430D0611X**ex D0714 ********seton                                        3699
067280040430D0611 * PRO1277-(F1) Sono presenti caratteri non consentiti
067290040430  "  C                   Eval      $InD2(36)   = *On
067300040430D0611C                   Eval      *In99       = *On
067310040430D0714C                   endif
067320040430D0714C                   endif
067330040430D1602 *
067340040430  "  C     XCtrD2SerPar  Endsr
067350040430  "   ************************************************************
067360040430  "   * Controllo esistenza partita
067370040430  "  X* (specifiche preesistenti in CtrPartita e qui riunite)
067380040430  "   ************************************************************
067390040430  "  C     CtrExistPar   Begsr
067400040430D1602 *
067410040430     C* se non sto gestendo un modello
067420040430     C                   IF        Ctb015 <> 'MD'
067430040430     C* controllo la validità estremi partita
067440040430     C* controllo solo se i valori a video <> dai valori sui files.
067450040430     C                   if        $PPAKcc   <> $kcc            or
067460040430     C                             $PPAKsc   <> d2ksc           or
067470040430     C                             $PPADtPar <> D2DtParIso      or
067480040430     C                             $PPANrPAr <> D2NrPar         or
067490040430     C                             $PPASerPar<> D2SerPar        or
067500040430     C* oppure sono in copia
067510040430     C                             Opz015    = '03'
067520040430D0217C* oppure sono in una immissione ma con recupero dati
067530040430D0217C                             or  ImmExp015 <> '0'
067540040430     C
067550040430     C                   Eval      PPASocieta = XscSoc
067560040430     C                   Eval      PPACtb     = Ctb015
067570040430     C                   Eval      PPAKcc     = $kcc
067580040430     C                   Eval      PPAKsc     = d2ksc
067590040430     C                   Eval      PPADtPar   = D2DtParIso
067600040430     C                   Eval      PPANrPAr   = D2NrPar
067610040430     C                   Eval      PPASerPar  = D2SerPar
067620040430     C     K07PPA01      Chain     NDPPA01l                           21
067630040430     C                   EVAL      ExiPAr = *in21
067640040430
067650040430D1602C                   exsr      RepPAS
067660040430  "   *
067670040430  "   *---------------------------------------------------------------
067680040430  "   * PARTITA DEVE ESISTERE
067690040430D1602 *
067700040430     C* se la partita deve esistere ma non c'è
067710040430D1602X****************** If        OPECtPar = '1' and *in21 = *on
067720040430D1602C                   If        OPECtPar = '1' and $EsistePAS=*off
067730040430D0611X****************** SETON                                        9983
067740040430D0611 * MVC0052-Riferimento partita non esistente
067750040430  "  C                   Eval      $InD2(83)   = *On
067760040430D0611C                   Eval      *In99       = *On
067770040430     C                   EndIf
067780040430
067790040503D1602 *---------------------------------------------------------------
067800040430  "   * PARTITA DEVE ESSERE NUOVA
067810040430D1602 *
067820040430     C* se la partita deve essere nuova ma c'è già
067830040430D1602X****************** IF        *In21 = *Off   And
067840040430D1602C                   if        $EsistePAS=*on And
067850040430     c                             PPANrPar <> 0  And
067860040430     C                             (OPECtPar = '2' Or OPECtPar = '3') And
067870040430     C* e la sua registrazione generante non è la stessa
067880040430     C                             (PPASys <> RegSys  or
067890040430     C                              PPANrAsGen <> RegNrAsReg)
067900040430D0611X****************** SETON                                        9994
067910040430D0611 * MVC0053-Riferimento partita già esistente
067920040430  "  C                   Eval      $InD2(94)   = *On
067930040430D0611C                   Eval      *In99       = *On
067940040430     C                   EndIf
067950040430
067960040503D1602 *---------------------------------------------------------------
067970040430  "   * AVVERTO SE PARTITA ESISTE
067980040430D1602 *
067990040430C1207C* se la partita esiste e non prevede controlli emetto msg di
068000040430  "  C* avvertimento così da far decidere l'utente se continuare o
068010040430  "  C* se modificare la partita
068020040430C1207C                   If        OpeCtPar <> '1' and OpeCtPar <> '2' and
068030040430D1602X**************************** OpeCtPar <> '3' and ExiPar = *Off and
068040040430D1602C                             OpeCtPar <> '3' and $EsistePAS=*On  and
068050040430C1207C                             $MsgParEs = *Off and
068060040430  "  C* e la sua registrazione generante non è la stessa
068070040430  "  C                             (PPASys <> RegSys  or
068080040430  "  C                              PPANrAsGen <> RegNrAsReg)
068090040430  "  C                   Exsr      MsgParEs
068100040430  "  C                   EndIf
068110040430C1207
068120040430     C                   EndIf
068130040430
068140040503D1602 *---------------------------------------------------------------
068150040430  "   * DIVISA DI PARTITA
068160040503D1602 *
068170040430     C* se la partita esiste
068180040430D1602X****************** If        ExiPar = *off   and
068190040430D1602C                   If        $EsistePAS=*on  and
068200040430     C* e la partita doveva esistere oppure non c'erano controli
068210040430     C                             ( OPECtPar = '1' or OPECtPar = ' ' )
068220040430     C* e la divisa di partita non è = a quella di registrazione
068230040430     c                             and PPADivisa <> D2Divisa
068240040430     C* errore
068250040430D0611X****************** SETON                                        9978
068260040430D0611 * NDC0111-Divisa di partita non può essere diversa da quella di registrazione
068270040430  "  C                   Eval      $InD2(78)   = *On
068280040430D0611C                   Eval      *In99       = *On
068290040430     C                   EndIf
068300040430     C                   EndIf
068310040430     C*
068320040430C1207C* se non ci sono errori, ma l'utente (all'emissione della mia
068330040430  "  C* finestra di avvertimento dell'esistenza partita) ha deciso
068340040430  "  C* di rimanere in videata per correggere la partita, faccio in
068350040430  "  C* modo che non passi ad altra videata
068360040430  "  C                   If        *In99=*Off
068370040430  "  C                   Move      $Correggi     *In99
068380040430  "  C                   EndIf
068390040430C1207C*
068400040430D1602C     XCtrExistPar  Endsr
068410040430  "   ************************************************************
068420040430  "   * Controllo esistenza partita
068430040430  "  X* (specifiche preesistenti in CtrPartita e qui riunite)
068440040430  "   ************************************************************
068450040430D1602C     CtrEffColP    Begsr
068460040430C1350C*
068470040430  "   * se non sto gestendo un modello e non ci sono errori
068480040430  "   * P.S:
068490040430  "   * Non Controllo che la partita esista (If ExiPar = *Off)
068500040430  "   * xchè in modifica ExiPar rimane = *Blank
068510040430  "  C                   If        Ctb015 <> 'MD'  AND  *In99 = *Off
068520040430  "   *
068530040430  "   * testo se esistono effetti collegati alla partita
068540040430  "  C                   CallB     'NDMVC403'
068550040430  "  C                   Parm                    XScSoc
068560040430  "  C                   Parm                    Ctb015
068570040430  "  C                   Parm                    D1TpSogg
068580040430  "  C                   Parm                    $Kcc
068590040430  "  C                   Parm                    D2Ksc
068600040430  "  C                   Parm                    D2DtParISO
068610040430  "  C                   Parm                    D2NrPar
068620040430  "  C                   Parm                    D2SerPar
068630040430  "  C                   Parm                    *Omit
068640040430  "  C                   Parm                    *Omit
068650040430  "  C                   Parm                    Risposta
068660040430  "  C                   Parm                    EffNoCol
068670040430  "  C                   Parm                    *Omit
068680040430  "  C                   Parm                    *Omit
068690040430  "   *
068700040430  "   * se ci sono effetti collegati alla partita ma non alla reg
068710040430  "  C                   Move      Normale       D2PagManA
068720040430  "  C                   If        EffNoCol   <> '0'
068730040430  "   *
068740040430  "   *    imposto Rate manuali='S'
068750040430  "  C                   Eval      D2PagMan    = XScOn
068760040430  "   *
068770040430  "   *    e lo proteggo
068780040430  "  C                   Move      Forzato       D2PagManA
068790040430  "   *
068800040430  "   *    avverto l'utente gli chiedo se continuare o ritornare
068810040430  "  C                   EXSR      WinEffNoCo
068820040430  "   *
068830040430  "  C                   EndIf
068840040430  "   *
068850040430  "  C                   EndIf
068860040430C1350 *
068870040430D1602C     XCtrEffColP   Endsr
068880040430D1602 ************************************************************
068890040430  "   * Reperisco NDPAS
068900040430  "   ************************************************************
068910040430  "  C     RepPAS        Begsr
068920040430  "   *
068930040430  "   * PAS esiste se esiste anche PPA
068940040430  "  C                   eval      $EsistePAS = *off
068950040430  "   *
068960040430  "   * Se c'è PPA legato alla partita valorizzata a video
068970040503  "   * controllo se esiste almeno un PAS legato alla partita
068980040430  "   *
068990040430  "  C                   if        ExiPar = *off
069000040430  "   *
069010040430  "  C                   eval      PASSocieta = PPASocieta
069020040430  "  C                   eval      PASCtb     = PPACtb
069030040430  "  C                   eval      PASKcc     = PPAKcc
069040040430  "  C                   eval      PASKsc     = PPAKsc
069050040430  "  C                   eval      PASDtPar   = PPADtPar
069060040430  "  C                   eval      PASNrPar   = PPANrPar
069070040430  "  C                   eval      PASSerPar  = PPASerPar
069080040430  "  C     K07PAS04      setll     NDPAS04L                               21
069090040430  "  C                   if        *in21 = *on
069100040430  "  C                   eval      $EsistePAS = *on
069110040430  "  C                   endif
069120040430  "   *
069130040430  "  C                   endif
069140040430  "   *
069150040430D1602C     XRepPAS       Endsr
069160040430C1429 ************************************************************
069170001026  "   * Gestione blocco partita
069180001026  "   ************************************************************
069190001026  "  C     BloccoPar     BEGSR
069200001026  "   *
069210001026  "   *Se l'utente blocca la partita
069220001026  "   *(grazie all'F4 sulla partita, va in E/C)
069230001026  "   *devo sempre tentare di reperirne la descrizione
069240001026  "   *
069250001026  "   *Pulisco descriz. blocco partita
069260010821C1429C                   Clear                   D2BKAmmD
069270010821C1512C                   Clear                   D2BKAmm
069280010821C1512C                   Clear                   D2DivisaP
069290010821C1429 *
069300001026  "   *Inizializzo: non c'è bonifico in corso per la partita
069310001026  "   *(quindi non proteggo la partita)
069320001026  "  C                   Eval      $BonInC     = '0'
069330001026  "   *
069340001026  "   *Se data partita esiste
069350001026  "  C                   If        D2DtPar  > 0
069360001026  "   *
069370001026  "   * Se data partita formalmente valida
069380001026  "  C                   CALLB     'XDT4000'
069390001026  "  C                   PARM                    XDTAMG           10
069400001026  "  C                   PARM      D2DtPar       XDTGMA            6 0
069410001026  "  C                   PARM      3             XDTSTA            1 0
069420001026  "  C                   If        XDtSta      = 0
069430001026  "   *
069440001026  "   *  Reperisco descr. blocco partita
069450001026  "  C                   Eval      PPASocieta  = XscSoc
069460001026  "  C                   Eval      PPACtb      = Ctb015
069470001026  "  C                   Eval      PPAKcc      = $Kcc
069480001026  "  C                   Eval      PPAKsc      = D2Ksc
069490001026  "  C                   Move      XDtAMG        PPADtPar
069500001026  "  C                   Eval      PPANrPAr    = D2NrPar
069510001026  "  C                   Eval      PPASerPar   = D2SerPar
069520001026  "  C                   EXSR      BloccoParD
069530001026  "   *
069540001026  "  C                   EndIf
069550001026  "   *
069560001026  "  C                   EndIf
069570001026  "   *
069580001026  "  C     XBloccoPar    ENDSR
069590001026  "   ************************************************************
069600001026  "   * Descrizione blocco partita
069610001026  "   ************************************************************
069620001026  "  C     BloccoParD    BEGSR
069630001026  "   *
069640010821C1429C     K07PPA01      Chain     NDPPA01l                           21
069650010821C1512 *
069660010821  "   * gestione divisa di partita
069670010918  "  C                   exsr      GesDivPar
069680010821C1512 *
069690010821C1429 *
069700010821  "  C                   If        *In21       = *Off
069710010821  "   *
069720001026  "  C                   Eval      PPSSys      = PPASys
069730001026  "  C                   Eval      PPSNrAsInt  = PPANrAsInt
069740001026  "  C     K02PPS01      Chain     NDPPS01l                           21
069750001026  "  C                   If        *In21       = *Off
069760010821C1429 *
069770010821C1512 *   Codice blocco
069780010821  "  C                   MoveL     PPSBkAmm      D2BkAmm
069790010821C1512 *
069800010821C1429C                   Move      XScCUt        XTBUte
069810001026  "  C                   Move      '1'           XTBRic
069820001026  "  C                   Move      XScSoc        XTBAzi
069830001026  "  C                   Move      *Zero         XTBKey
069840001026  "  C                   Move      '0AB'         XTBCod
069850001026  "  C                   Move      PPSBkAmm      XTBKey
069860001026  "  C                   CALLB     'XATB'
069870001026  "  C                   Parm                    XATBDS
069880010821  "   *
069890010821  "   *   Descrizione blocco
069900001026  "  C                   If        XTBErr      = *Off
069910001026  "  C                   MoveL     XTBUni        ANG0ABDS
069920001026  "  C                   MoveL     De20AB        D2BkAmmD
069930001026  "  C                   EndIf
069940001026  "   *
069950001026  "   *   Partita con bonifico in corso
069960001026  "  C                   If        PPSBonInC   = '1'
069970001026  "   *     lo memorizzo x proteggere la partita
069980001026  "  C                   Eval      $BonInC     = '1'
069990001026  "  C                   EndIf
070000001026  "   *
070010010820  "  C                   EndIf
070020010821  "   *
070030010821  "   *
070040001026  "  C                   EndIf
070050001026  "   *
070060001026C1429C     XBloccoPaD    ENDSR
070070010821C1512 ************************************************************
070080010821  "   * Gestisco la divisa di partita
070090010821  "   ************************************************************
070100010918  "  C     GesDivPar     Begsr
070110010821  "   *
070120010821  "   * Partita esistente (in PPA)
070130010821  "  C                   if        *In21 = *Off
070140010821  "   *
070150010821  "   *  Imposto divisa di partita
070160010821  "  C                   eval      D2DivisaP   = PPADivisa
070170010821  "   *
070180010821  "  C                   endif
070190010821  "   *
070200011002  "   * Proteggo importi/cambio---------------------------------
070210010821  "   * se divisa di partita diversa dalla divisa di movimento
070220010821  "   *  e gli importi sono valorizzati
070230010821  "   * (utile quando l'utente cambia la partita)
070240010821  "   *
070250010821  "  C                   if        D2DivisaP  <> D2Divisa  and
070260010821  "  C                             D2DivisaP  <> *Blank    and
070270010821  "  C                             D2Divisa   <> *Blank    and
070280010821  "  C                             D2ImportD  <> *Zero     and
070290010821  "  C                             D2Importo  <> *Zero
070300010821  "   *
070310010821  "  C                   eval      D2ImportDA = Protetto
070320010821  "  C                   eval      D2ImportoA = Protetto
070330011002  "  C                   eval      D2CambioA  = Protetto
070340010821  "   *
070350010821  "  C                   endif
070360010821  "   *
070370011002  "   * SProteggo importi/cambio----------------------------------
070380010821  "   * se la divisa di partita è uguale alla divisa di movim
070390010821  "   * (in modifica l'utente può cambiare la partita originaria
070400010821  "   * scegliendo una partita che ha stessa divisa del movimento)
070410010821  "   * oppure la partita è nuova
070420010821  "  C                   if        (D2DivisaP  = D2Divisa  or
070430011002  "  C                             D2DivisaP   = *Blank)
070440011002  "   *
070450011002  "   * Se il 1° movim non è modificabile (FldMod=*Off)
070460011002  "   * gli importi devono essere protetti
070470011002  "   * (sproteggo gli importi solo se protetti per la divisa diversa)
070480011002  "  C                   if        FldMod <> *off
070490011002  "  C                   eval      D2ImportDA = x'32'
070500011002  "  C                   eval      D2ImportoA = x'32'
070510011002  "  C                   endif
070520011002  "   *
070530011002  "   * Se la testata non è modificabile (TesMod=*off)
070540011002  "   * il cambio deve essere protetto
070550011002  "   * (sproteggo il cambio solo se protetto per la divisa diversa)
070560011002  "  C                   if        TesMod <> *off
070570011002  "  C                   eval      D2CambioA  = x'32'
070580011002  "  C                   endif
070590010821  "   *
070600010821  "  C                   endif
070610010821  "   *
070620010918C1512C     XGesDivPar    Endsr
070630000410C1350 ************************************************************
070640000410  "   * Esistono effetti collegati alla partita ma non alla reg:
070650000410  "   * avverto l'utente gli chiedo se continuare o ritornare
070660000410  "   * P.S: routine che sostituisce CtrEffNoCo:
070670000410  "   *      CtrEffNoCo chiamava NDC275R che con un SQL
070680000410  "   *      controllava l'esistenza di effetti collegati
070690000410  "   *      alla partita ma non alla fattura, e se ciò
070700000410  "   *      si verificava emetteva window di avvertimento
070710000410  "   *      WinEffNoCo emette solo al window di avvertimento
070720000410  "   *      (il controllo dell'esistenza di eff. colleagti
070730000410  "   *      alla partita ma non alla fattura, è già fatto
070740000410  "   *      da NDMVC403)
070750000410  "   ************************************************************
070760000410  "  C     WinEffNoCo    BEGSR
070770000410  "   *
070780000410  "   *Reperisco i testi dei messaggi da emettere nella window:
070790000410  "   *'Esistono effetti collegati alla partita ma non generati da questa'
070800000410  "   *'registrazione. Si consiglia di controllare gli effetti della partita'
070810000410  "   *'e comunque si andrà in gestione rate manuali.'
070820000410  "  C     20            Do        22            $X
070830000410  "   *
070840000410  "  C                   Movel     MsgId($X)     IdMsg
070850000410  "  C                   CallB     'XRTVM'
070860000410  "  C                   Parm                    IdMsg            27
070870000410  "  C                   Parm      *Blank        TxTMsg          100
070880000410  "  C                   Parm                    ErrMsg            1
070890000410  "   *
070900000410  "  C                   If        ErrMsg     <> *On
070910000410  "  C                   Movel     TxtMsg        MsgT($X)
070920000410  "  C                   Else
070930000410  "  C                   Movel     *ALL'?'       MsgT($X)
070940000410  "  C                   EndIf
070950000410  "   *
070960000410  "  C                   EndDo
070970000410  "   *
070980000410  "   *  Valorizzo i parametri descrizione msg da passare a XMsgR
070990000410  "  C                   Clear                   XMsgDS
071000000410  "  C                   Eval      MsgMs1      = MsgT(20)
071010000410  "  C                   Eval      MsgMs2      = MsgT(21)
071020000410  "  C                   Eval      MsgMs3      = MsgT(22)
071030000410  "   *
071040000410  "   *Reperisco i testi delle possibili risposte:
071050000410  "   *'Continua'
071060000410  "   *'Ritorna'
071070000410  "  C     14            Do        15            $X
071080000410  "   *
071090000410  "  C                   Movel     MsgId($X)     IdMsg
071100000410  "  C                   CallB     'XRTVM'
071110000410  "  C                   Parm                    IdMsg            27
071120000410  "  C                   Parm      *Blank        TxTMsg          100
071130000410  "  C                   Parm                    ErrMsg            1
071140000410  "   *
071150000410  "  C                   If        ErrMsg     <> *On
071160000410  "  C                   Movel     TxtMsg        MsgT($X)
071170000410  "  C                   Else
071180000410  "  C                   Movel     *ALL'?'       MsgT($X)
071190000410  "  C                   EndIf
071200000410  "   *
071210000410  "  C                   EndDo
071220000410  "   *
071230000410  "   *  Valorizzo i parametri da passare a XMsgR
071240000410  "  C                   Clear                   XMsgTxt
071250000410  "   *  Continua
071260000410  "  C                   Eval      %SubSt(XMsgTxt:1:15)  = MsgT(14)
071270000410  "   *  Rotorna
071280000410  "  C                   Eval      %SubSt(XMsgTxt:16:15) = MsgT(15)
071290000410  "   *
071300000410  "  C                   Movel     'S'           MsgEmv
071310000410  "  C                   Z-Add     10            MsgRgp
071320000410  "  C                   Movel     'S'           MsgRsp
071330000410  "  C                   Movel     '12'          MsgTpr
071340000410  "   *
071350000410  "   *Modo di inizializzazione della window:
071360000410  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
071370000410  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
071380000410  "   *                       per mantenere il sottofondo del video princip.)
071390000410  "  C                   Eval      XMSGINZ     = $PrimoXMsg
071400000410  "  C                   If        $PrimoXMsg  = *On
071410000410  "  C                   Eval      $PrimoXMsg  = *Off
071420000410  "  C                   EndIf
071430000410  "   *
071440000410  "  C                   CALL      'XMSGR'
071450000410  "  C                   PARM                    XMSGDS
071460000410  "  C                   PARM                    XMSGTXT         135
071470000410  "  C                   PARM                    XMSGINZ           1
071480000410  "   *
071490000410  "   *
071500000410  "   * se l'utente ha scelto di non continuare
071510000410  "  C                   If        MsgRtn      = '2'
071520000410  "   *  ritorno
071530000410  "  C                   Eval      *In99       = *On
071540000410  "  C                   EndIf
071550000410  "   *
071560000410C1350C     XWinEffNoC    ENDSR
071570990722C1207C************************************************************
071580990722  "  C* Emissione msg di partita già esistente
071590990722  "  C************************************************************
071600990722  "  C     MsgParEs      BEGSR
071610991012C1207C*
071620991012C1251 *"La partita esiste già e la causale non richiede controlli"
071630991012C1251 *"Vuoi continuare?"
071640991012C1207C     16            Do        17            $X
071650990722  "  C                   Clear                   XMsgDs
071660990722  "  C                   Clear                   XMsgTxt
071670990722  "  C                   Movel     MsgId($X)     IDMSG
071680990722  "  C                   CALLB     'XRTVM'
071690990722  "  C                   PARM                    IDMSG
071700990722  "  C                   PARM                    TXTMSG
071710990722  "  C                   PARM                    ERRMSG
071720990722  "  C     ERRMSG        IFNE      *ON
071730990722  "  C                   MOVEL     TXTMSG        MsgT($X)
071740990722  "  C                   ELSE
071750990722  "  C                   MOVEL     *ALL'?'       MsgT($X)
071760990722  "  C                   ENDIF
071770990722  "  C                   EndDo
071780990722  "   *
071790990722  "  C                   MOVEL     MsgT(16)      MSGMS1
071800990722  "  C                   MOVEL     MsgT(17)      MSGMS2
071810991012C1207 *
071820040429C1251X*** ex C1207****** Z-ADD     0             MSGMSG
071830991012C1207C                   MOVEL     'S'           MSGRSP
071840990722  "  C                   MOVEL     'SN'          MSGTPR
071850990722  "  C                   MOVEL     'S'           MSGEMV
071860991012C1207C                   Z-ADD     10            MSGRGP
071870040429C1251X*** ex C1207****** MOVEL     'N'           MSGLCK
071880040429  "  X*** ex C1207****** MOVEA     STATUS        STS
071890040429  "  X*** ex C1207****** MOVEL     'N'           MSGCNL
071900040429  "  X*** ex C1207****** MOVEL     'N'           MSGVID
071910040429  "  X*** ex C1207****** MOVEL     'N'           MSGOPE
071920040429C1251X*** ex C1207****** MOVEL     'N'           MSGSTP
071930991012C1207 *
071940990722  "   *Modo di inizializzazione della window:
071950990722  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
071960990722  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
071970990722  "   *                       per mantenere il sottofondo del video princip.)
071980990722  "  C                   Eval      XMSGINZ     = $PrimoXMsg
071990990722  "  C                   If        $PrimoXMsg  = *On
072000990722  "  C                   Eval      $PrimoXMsg  = *Off
072010990722  "  C                   EndIf
072020990722  "   *
072030990722  "  C                   CALL      'XMSGR'
072040990722  "  C                   PARM                    XMSGDS
072050990722  "  C                   PARM                    XMSGTXT         135
072060990722  "  C                   PARM                    XMSGINZ           1
072070990722  "   *
072080990722  "  C* l'utente ha scelto di rimanere in videata per correggere
072090990722  "  C     MsgRtn        IFEQ      'N'
072100990722  "  C                   Move      *On           $Correggi
072110990722  "   * se ha deciso di correggere emetto il msg fino a quando c'è
072120990722  "   * errore
072130990722  "  C                   Move      *Off          $MsgParEs
072140990722  "   * altrimenti un'unica volta
072150990722  "  C                   Else
072160990722  "  C                   Move      *Off          $Correggi
072170990722  "  C                   Move      *On           $MsgParEs
072180990722  "  C                   ENDIF
072190990722  "  C*
072200001020C1207C                   ENDSR
072210040429C1350X*********************************************************
072220040429  "  X*            (Asterisco le sigle ex C1341)              *
072230040429  "  X*      CtrEffNoCo (Ex C1341) sostituita da WinEffNoCo:  *
072240040429  "  X*      CtrEffNoCo chiamava NDC275R che con un SQL       *
072250040429  "  X*      controllava l'esistenza di effetti collegati     *
072260040429  "  X*      alla partita ma non alla fattura, e se ciò       *
072270040429  "  X*      si verificava emetteva window di avvertimento    *
072280040429  "  X*      WinEffNoCo emette solo al window di avvertimento *
072290040429  "  X*      (il controllo dell'esistenza di eff. colleagti   *
072300040429  "  X*      alla partita ma non alla fattura, è già fatto    *
072310040429  "  X*      da NDMVC403)                                     *
072320040429  "  X*********************************************************
072330040429  "  X************************************************************
072340040429  "  X*** CtrEfNoCol
072350040429  "  X************************************************************
072360040429  "  X***  CtrEffNoCo    BEGSR
072370040429  "  X***
072380040429  "  X***                Reset                   NDC275DS
072390040429  "  X***                Eval      PrimoXm275  = $PrimoXMsg
072400040429  "  X***                Eval      Societa275  = XScSoc
072410040429  "  X***                Eval      Ctb275      = Ctb015
072420040429  "  X***                Eval      Sys275      = REGSys
072430040429  "  X***                Eval      NrAsReg275  = REGNrAsReg
072440040429  "  X***                Eval      SNatura275  = D2TpSogg
072450040429  "  X***                Eval      Ksc275      = D2Ksc
072460040429  "  X***  *JobRun       Move      D2DtPar       $DtIso
072470040429  "  X***                Move      $DtIso        DtPar275
072480040429  "  X***                Eval      NrPar275    = D2NrPar
072490040429  "  X***                Eval      SerPar275   = D2SerPar
072500040429  "  X***                Eval      Opr275      = '1'
072510040429  "  X***                MoveL     NDC275DS      KPJBU
072520040429  "  X***                Call      'NDC275R'
072530040429  "  X***                PARM                    KPJBA
072540040429  "  X***
072550040429  "  X***                Eval      NDC275DS    = KPJBU
072560040429  "  X***
072570040429  "  X***
072580040429  "  X*** Se NDC275R ha emesso la window, ed è stata la prima
072590040429  "  X*** ad essere emessa sopra il video di NDC015D, le pross         ime
072600040429  "  X*** eventuali window, devono mantenere lo stesso sottofo         ndo
072610040429  "  X***                Eval      $PrimoXMsg  = PrimoXM275
072620040429  "  X***
072630040429  "  X*** se esistono effetti collegati alla partita ma non al         la
072640040429  "  X*** registr. e l'utente non vuole proseguire si accende
072650040429  "  X*** l'indicatore di errore
072660040429  "  X***                If        Opr275      = '0'
072670040429  "  X***                SetOn                                        99
072680040429  "  X***                Endif
072690040429  "  X***
072700040429  "  X***           XCtrEffNoC    ENDSR
072710040429C1350X***
072720060619c1937C*----------------------------------------------------------
072730060619     C* 1) Controllo numero /data documento già utilizzati
072740060619     C* 2) Controllo numero  documento già utilizzato nello stesso anno
072750060619     c* solare della data documento
072760060619c1937C*----------------------------------------------------------
072770060619     C     CtrDocumen    BEGSR
072780060619     c*
072790060620     c                   move      *off          $EmWdwDoc
072800060620     c* se modificati i dati dico di riemettere wdw
072810060620     c     *jobrun       move      D2DtDoc       DataIso
072820060620     C                   if        CtrDtDoc <>Dataiso  or
072830060620     C                             CtrNrDoc <>D2NrDoc
072840060620     c                   move      *off          $MsgDocEs
072850060620     c                   move      DataIso       CtrDtDoc
072860060620     c                   move      D2NrDoc       CtrNrDoc
072870060620     c                   endif
072880060620     c*
072890060620     c                   eval      movsocieta=xscsoc
072900060620     c                   eval      movctb    =ctb015
072910060620     c                   eval      movkcc    =$kcc
072920060620     c                   eval      movksc    =d1ksc
072930060619     c                   eval      movnrdoc =D2NrDoc
072940060619     c     *jobrun       move      D2DtDoc       DataIso
072950060619     c                   move      DataIso       MovdtDoc
072960060620     c*
072970060620     c* test con data/nr documento
072980060620     c     K06Mov08      setll     NdMov08L
072990060620 b2  c                   do        *hival
073000060620     c     K06Mov08      reade     NdMov08L
073010060620 b3  c                   if        %eof(ndmov08L)
073020060620     c                   leave
073030060620 e3  c                   endif
073040060627     c* se già emessa(+ righe di mov con stesso documento)
073050060627 b3  c                   if        $EmWdwDoc = *on
073060060627     c                   leave
073070060627 e3  c                   endif
073080061124D2088 *   devo considerare solo documenti di movimenti di tipo iva
073090061124  "  C                   exsr      CtrCausrig
073100061124  "  C                   if        $Esito <> *On and
073110061124  "  c                             OpeTpc <>'1'
073120061124  "  C                   eval      Anope = SavAnope
073130061124  "  c                   iter
073140061124  "  c                   endif
073150061124D2088C                   eval      Anope = SavAnope
073160061124     C*
073170060620     c* se non  è la stessa registrazione
073180060620 b3  C                   if        MovNrasReg <> RegNrasReg
073190060619      *  ndc0942-Numero/data documento già utilizzati
073200060619     C                   move      'NDC0942'     XMsgCd1
073210060619     C                   move      'COS2491'     XMsgCd2
073220060619     c                   exsr      CallWdwDoc
073230060620     c                   move      *on           $EmWdwDoc
073240060620     c                   leave
073250060620 e3  c                   endif
073260060620 e2  c                   enddo
073270060620     c*
073280060620     c* se non emessa la finestra con data/numero,
073290060620     c* test con l'anno della data/nr documento
073300060620 b2  c                   if        $EmWdwDoc = *off
073310060619     c                   extrct    DataIso:*Y    MovAnnoDoc
073320060620     c     K06Mov19      setll     NdMov19L
073330060620 b3  c                   do        *hival
073340060620     c     K06Mov19      reade     NdMov19L
073350060620 b4  c                   if        %eof(ndmov19L)
073360060620     c                   leave
073370060620 e4  c                   endif
073380060627     c* se già emessa(+ righe di mov con stesso documento)
073390060627 b4  c                   if        $EmWdwDoc = *on
073400060627     c                   leave
073410060627 e4  c                   endif
073420061124D2088 *   devo considerare solo documenti di movimenti di tipo iva
073430061124  "  C                   exsr      CtrCausrig
073440061124  "  C                   if        $Esito <> *On and
073450061124  "  c                             OpeTpc <>'1'
073460061124  "  C                   eval      Anope = SavAnope
073470061124  "  c                   iter
073480061124  "  c                   endif
073490061124D2088C                   eval      Anope = SavAnope
073500061124     c*
073510060620 b4  C                   if        MovNrasReg <> RegNrasReg
073520060619      *  ndc0941-Documento già utilizzato nell'anno solare
073530060619     C                   move      'NDC0941'     XMsgCd1
073540060619     C                   move      'COS2491'     XMsgCd2
073550060619     c                   exsr      CallWdwDoc
073560060627     c                   move      *on           $EmWdwDoc
073570060620 e4  c                   endif
073580060620 e3  c                   enddo
073590060620 e2  c                   endif
073600060619     C*
073610060619c1937C                   ENDSR
073620060619c1937C************************************************************
073630060619     C* emissione videata per estremi documento
073640060619c1937C************************************************************
073650060619     C     CallWdwDoc    BEGSR
073660060619     c*
073670060619     c* solo se non ho già emesso nella stessa videata
073680060619     c                   if        $MsgDocEs = *off
073690060619     C                   clear                   XMsgDS
073700060619     C                   clear                   XMsgTxt
073710060619     C                   movel     0             MsgMsg
073720060619     C                   movel     'S'           MsgRsp
073730060619     C                   MoveL     'SN'          MsgTpR
073740060619     C                   movel     'S'           MsgEmv
073750060619     C                   z-add     10            MsgRgp
073760060619     C                   movel     'N'           MsgLck
073770060619     C                   movel     'N'           MsgCnl
073780060619     C                   movel     'N'           MsgVid
073790060619     C                   movel     'N'           MsgOpe
073800060619     C                   movel     'N'           MsgStp
073810060619     C                   Eval      XMsgInz     = $PrimoXMsg
073820070802D2244C                   If        $PrimoXMsg  = *On
073830070802  "  C                   Eval      $PrimoXMsg  = *Off
073840070802D2244C                   EndIf
073850060619      *
073860060619     C                   call      'XMSGR'
073870060619     C                   parm                    XMsgDS
073880060619     C                   parm                    XMsgTxt         135
073890060619     C                   parm                    XMsgInz
073900060619     C                   parm                    XMsgCd1           7
073910060619     C                   parm                    XMsgCd2           7
073920060619     C                   parm                    XMsgCd3           7
073930060619     C                   parm                    XMsgSn1           7
073940060619     C                   parm                    XMsgCdTxT        63
073950060619     C                   parm                    MsgCdTit         14
073960060619     C                   parm                    MsgClPrtf         1
073970060619     C                   parm                    VarMsg1         100
073980060619     C                   parm                    VarMsg2         100
073990060619     C                   parm                    VarMsg3         100
074000060619     c* se sceglie di non continuare
074010060619     c                   if        MsgRtn = 'N'
074020060619     C                   Move      *Off          $MsgDocEs
074030060619     C                   seton                                        99
074040060619     c                   else
074050060619     C                   Move      *On           $MsgDocEs
074060060619     c                   endif
074070060619     c*
074080060619     c                   endif
074090060619     c*
074100060619c1937C                   ENDSR
074110061124D2088C************************************************************
074120061124     C* Controllo causale riga movimento (deve essere di tipo IVA)
074130061124D2088C************************************************************
074140061124     C     CtrCausRig    begsr
074150061124     c*
074160061124      *   non voglio sporcare il rec di Anope della
074170061124      *   causale di testata eventualmente recuperato
074180061124      *   perchè verrà utilizzato nel corso del pgm
074190061124     C                   clear                   SAVAnope
074200061124     C                   eval      SAVAnope = Anope
074210061124      *
074220061124     C                   eval      $LenOut = %size($Anope)
074230061124     C                   move      movdtreg      DataISO1
074240061124     C                   callb     'NDMVC003'
074250061124     C                   parm                    XScSoc
074260061124     C                   parm                    MovCausRig
074270061124     C                   parm                    DataISO1
074280061124     C                   parm      '1'           $Uso
074290061124     C                   parm      *off          $GesMsg
074300061124     C                   parm                    $Esito
074310061124     C                   parm      'ANOPE'       $Struttura
074320061124     C                   parm                    $Anope
074330061124     C                   parm                    $LenOut
074340061124     C                   parm      *on           $Refresh
074350061124     C                   if        $Esito <> *On
074360061124     c                   eval      Anope=$anope
074370061124     c                   endif
074380061124     c*
074390061124D2088C                   ENDSR
074400950616     C************************************************************
074410960424     C* Controlli sugli importi
074420950616     C************************************************************
074430960424     C     CtrImporti    BEGSR
074440960424     C*
074450980922     C* controllo congruenza importi/cambio
074460980922     C                   RESET                   PJX00202DS
074470980922     C                   MOVEL     D2Divisa      Divisa0202
074480980922     C                   MOVEL     XscDiv        DivMC0202
074490980922     C                   Z-ADD     D2ImportD     ImporD0202
074500980922     C                   Z-ADD     D2Importo     ImporM0202
074510980922     C                   Z-ADD     D2Cambio      Cambio0202
074520980922     C                   MOVEL     DtRfCam       DtRif0202
074530980922     C*
074540980922     C                   MOVEL     *on           GesMsg
074550980922     C*
074560980922     C                   CALLP     X0202TestC(PJX00202DS :
074570980922     C                                        %size(PJX00202DS) :
074580980922     C                                        GesMsg : Esito :
074590980922     C                                        *omit  : *omit)
074600980922     C*
074610960424     C* se ci sono errori
074620040308     C                   IF        Esito = *on
074630960424     C* gestisco la loro emissione
074640960424     c                   clear                   TextMsg
074650040308     C                   DOU       Q2BytAva = 0
074660960424     C                   EXSR      RestoreMsg
074670040308     C                   IF        Q2BytAva > 0
074680040308     C                   SELECT
074690040308     C                   WHEN      Q2MsgId = 'MVC0067'
074700040429D0611X****************** SETON                                        5799
074710001020D0611 * MVC0067-(F1) Cambio incongruente rispetto al cambio alla data di riferimento
074720001020  "   * (D2Cambio)
074730001020  "  C                   Eval      $InD2(57)   = *On
074740001020D0611C                   Eval      *In99       = *On
074750960424     C                   WHEN      Q2MsgId = 'MVC0065'
074760040429D0611X****************** SETON                                        5699
074770001020D0611 * MVC0065-(F1) Importo in m.c. incongruente rispetto al cambio indicato o reperito
074780001020  "   * (D2ImportD)
074790001020  "  C                   Eval      $InD2(56)   = *On
074800001020D0611C                   Eval      *In99       = *On
074810040308     C                   OTHER
074820960424     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
074830960424     C                   MOVE      Q2MsgId       IdMsg
074840960424     C                   CALLB     'XRTVM'
074850960424     C                   PARM                    IDMSG            27
074860960424     C                   PARM                    TXTMSG          100
074870960424     C                   PARM                    ERRMSG            1
074880960424     C                   PARM                    *omit
074890960424     C                   PARM                    *omit
074900960424     C                   PARM      Q2msgtxt      Msgdta          100
074910040308     C                   IF        ErrMsg <> *on
074920960424     C                   CAT       TxtMsg:1      TextMsg
074930040429D0611X****************** SETON                                        5099
074940001023D0611 * CPF9898-&1
074950001023  "  C                   Eval      $InD2(50)   = *On
074960001020D0611C                   Eval      *In99       = *On
074970040308     C                   ENDIF
074980040308     C                   ENDSL
074990040308     C                   ENDIF
075000040308     C                   ENDDO
075010040308     C                   else
075020040308     C                   EVAL      D2importd = ImporD0202
075030040308     C                   EVAL      D2importo = ImporM0202
075040040308     C                   EVAL      D2cambio  = Cambio0202
075050040308     C                   ENDIF
075060040308     C*
075070040308     C                   ENDSR
075080040308     C************************************************************
075090040308     C* Valorizzazione data da default di causale
075100040308     C************************************************************
075110040308     C     ValDataDft    BEGSR
075120040308     C*
075130040308     C                   Z-ADD     0             §Data
075140030707     C*
075150030707D1392c* eseguo solo se 99 spento, che significa che non ci sono errori
075160030707  "  c* sulle date, altrimenti si spacca il programma
075170030707D1392c                   if        *in99 = *off
075180950811     C*
075190950811     C                   SELECT
075200950811     C*
075210950811     C* se è valorizzato il codice data per il default
075220950811     C                   WHEN      §CDt <> 0
075230950811     C* chiamo la routine che ricava una data da un codice
075240961009     C                   CALL      'XDATCD'
075250950811     C                   PARM                    §CDt
075260950914     C                   PARM      Dt0           ValData
075270950811     C*
075280950914     C     ValData       IFGT      Dt0
075290960202     C                   CALLB     'XDT4000'
075300950914     C                   PARM      ValData       XDTAMG           10
075310950914     C     §Data         PARM                    XDTGMA            6 0
075320950914     C                   PARM      2             XDTSTA            1 0
075330950811     C                   ENDIF
075340950811     C*
075350950811     C* se è valorizzato il tipo data per il default
075360950811     C                   WHEN      §TDt <> *blank
075370950811     C* chiamo la subroutine che assegna ad una data il valore di un altra
075380950811     C                   EXSR      DataDftTDt
075390950811     C*
075400950811     C                   ENDSL
075410030707     C*
075420030707D1392c                   endif
075430950621     C*
075440950621     C                   ENDSR
075450950621     C************************************************************
075460950621     C* Valorizzazione data da tipo data
075470950621     C************************************************************
075480950621     C     DataDftTDt    BEGSR
075490950811     C*
075500950811     C                   SELECT
075510950811     C*
075520950811     C* da data competenza co.ge.
075530950811     C                   WHEN      §TDt = 'C'
075540950811     C                   MOVE      D2DtCoCo      §Data
075550950811     C*
075560950811     C* da data documento
075570950811     C                   WHEN      §TDt = 'D'
075580950811     C                   MOVE      D2DtDoc       §Data
075590021009C1589 *
075600021009  "   * da giorno precedente data documento
075610021009  "  C                   WHEN      §TDt = 'E'
075620021009  "  C                   move      D2DtDoc       §Data
075630021009  "  C                   if        D2DtDoc <> 0
075640021009  "  C     *jobrun       move      D2DtDoc       DataIso
075650021009  "  C                   subdur    1:*D          DataIso
075660021009  "  C     *jobrun       move      DataIso       §Data
075670021009  "  C                   endif
075680021009C1589 *
075690950811     C*
075700950811     C* da data libro giornale
075710950811     C                   WHEN      §TDt = 'L'
075720990714D0154C*** in questo pgm non gestisco questo tipo data
075730990714  "  C* l'affermazione precedente non è più vera perchè:
075740990714  "  C* in questo pgm la data libro giornale è sempre uguale alla data
075750990714  "  C* di registrazione in quanto non è possibile la sovrapposizione
075760990714  "  C* d'esercizio
075770990714D0154C                   MOVE      D2DtReg       §Data
075780950811     C*
075790950811     C* da data partita
075800950811     C                   WHEN      §TDt = 'P'
075810950811     C                   MOVE      D2DtPar       §Data
075820950811     C*
075830950811     C* da data operazione
075840950811     C                   WHEN      §TDt = 'O'
075850950811     C*** in questo pgm non gestisco questo tipo data
075860950811     C*
075870950811     C* da data registrazione
075880950811     C                   WHEN      §TDt = 'R'
075890950811     C                   MOVE      D2DtReg       §Data
075900950811     C*
075910950811     C* da data valuta
075920950811     C                   WHEN      §TDt = 'V'
075930950811     C*** in questo pgm non gestisco questo tipo data
075940950811     C*
075950950811     C                   ENDSL
075960950621     C*
075970950621     C                   ENDSR
075980950621     C************************************************************
075990950621     C* Riempimento rcd da scrivere
076000950621     C************************************************************
076010950621     C     Rie004DS      BEGSR
076020950621     C*
076030950616     C                   RESET                   ND004DS
076040950616     C*
076050950616     C                   EVAL      KNrAz004  = KNrAz
076060040429D1022X****               EVAL      FunGen004 = *blank
076070011203D1022C                   EVAL      FunGen004 = RegFunGen
076080950616     C                   EVAL      Societa004 = XscSoc
076090950719     C                   EVAL      CausTes004 = D2CausTes
076100950719     C                   EVAL      Classe004  = D2Classe
076110950616     C                   EVAL      Unita004   = D1Unita
076120950719     C                   EVAL      Ctb004   = Ctb015
076130950719     C                   EVAL      TpRegz004   = TpRegz015
076140950719     C                   IF        D2DtReg > 0
076150960202     C                   CALLB     'XDT4000'
076160950914     C     DtReg004      PARM                    XDTAMG           10
076170950914     C                   PARM      D2DtReg       XDTGMA            6 0
076180950914     C                   PARM      4             XDTSTA            1 0
076190950914     C                   MOVE      DtReg004      DtLGio004
076200950623     C                   ENDIF
076210950719     C                   EVAL      NrReg004   = D2NrReg
076220950719     C                   EVAL      SerReg004  = D2SerReg
076230950719     C                   IF        D2DtDoc > 0
076240960202     C                   CALLB     'XDT4000'
076250950914     C     DtDoc004      PARM                    XDTAMG           10
076260950914     C                   PARM      D2DtDoc       XDTGMA            6 0
076270950914     C                   PARM      4             XDTSTA            1 0
076280950623     C                   ENDIF
076290951002     C                   EVAL      NrDoc004   = D2NrDoc
076300950719     C                   EVAL      SerDoc004  = D2SerDoc
076310950719     C                   EVAL      Divisa004  = D2Divisa
076320950719     C                   EVAL      Cambio004  = D2Cambio
076330950720     C                   EVAL      Condpag004 = D2condpag
076340950720     C                   if        d2pagman = xscon
076350950720     C                   EVAL      Pagman004 = *on
076360950720     C                   else
076370950720     C                   eval      pagman004 = *off
076380950720     C                   endif
076390981230     C                   EVAL      DtRfCam004 = DtRfCam
076400950630     C                   IF        CiSonoDft = *on
076410950630     C                   EVAL      TpCam004  = DftTipCmb
076420950630     C                   ELSE
076430950616     C                   EVAL      TpCam004  = 'C'
076440950630     C                   ENDIF
076450950719     C     D2DtCoCo      IFGT      0
076460960202     C                   CALLB     'XDT4000'
076470950914     C     DtCoCo004     PARM                    XDTAMG           10
076480950914     C                   PARM      D2DtCoCo      XDTGMA            6 0
076490950914     C                   PARM      4             XDTSTA            1 0
076500950623     C                   ENDIF
076510950719     C     D2DtCoAn      IFGT      0
076520960202     C                   CALLB     'XDT4000'
076530950914     C     DtCoAn004     PARM                    XDTAMG           10
076540950914     C                   PARM      D2DtCoAn      XDTGMA            6 0
076550950914     C                   PARM      4             XDTSTA            1 0
076560950623     C                   ENDIF
076570960527     C                   EVAL      NrAsCol004 = WNrAsCol
076580970122     C                   EVAL      flgreg004 = Wflgreg
076590020226C1547 *
076600020226  "   * data decorrenza pagamento da memorizzare in REGDtLib2
076610020226  "  C     D2DtLib       ifeq      0
076620020226  "  C                   move      Dt0           D2DtLibISO
076630020226  "  C                   else
076640020226  "  C                   CALLB     'XDT4000'
076650020226  "  C     D2DtLibISO    PARM                    XDTAMG           10
076660020226  "  C                   PARM      D2DtLib       XDTGMA            6 0
076670020226  "  C                   PARM      4             XDTSTA            1 0
076680020226  "  C                   endif
076690020226C1547C                   EVAL      DtDcPag004 = D2DtLibISO
076700950616     C*
076710950616     C                   ENDSR
076720950720     C************************************************************
076730950720     C* Riempimento rcd da controllare movimento
076740950720     C************************************************************
076750950720     C     Rie007DS      BEGSR
076760950720     C*
076770950720     C                   RESET                   ND007DS
076780950720     C*
076790950720     C
076800950720     C     D2DTDOC       ifeq      0
076810950914     C                   move      Dt0           d2dtdociso
076820950720     C                   else
076830960202     C                   CALLB     'XDT4000'
076840950914     C     D2DtDocISO    PARM                    XDTAMG           10
076850950914     C                   PARM      D2DtDoc       XDTGMA            6 0
076860950914     C                   PARM      4             XDTSTA            1 0
076870950720     C                   endif
076880950720     c*
076890950720     C                   EVAL      dtdoc007  = d2dtdociso
076900040429C1547X* la data decorrenza pagamento non viene più memorizzata in
076910040429  "  X* MOVDtLib ma in REGDtLib2
076920040429  "  X**ex C1494 D2DTlib       ifeq      0
076930040429  "  X**ex C1494         move      Dt0           d2dtlibiso
076940040429  "  X**ex C1494         else
076950040429  "  X**ex C1494         CALLB     'XDT4000'
076960040429  "  X**ex C1494 D2DtlibISO    PARM                    XDTAMG           10
076970040429  "  X**ex C1494         PARM      D2Dtlib       XDTGMA            6 0
076980040429  "  X**ex C1494         PARM      4             XDTSTA            1 0
076990040429  "  X**ex C1494         endif
077000040429  "  X**ex C1494         EVAL      dtlib007  = d2dtlibiso
077010040429C1547X**ex C1494         EVAL      regdtlib2 = d2dtlibiso
077020950720     C                   EVAL      nrdoc007 =  d2nrdoc
077030950720     C                   EVAL      serdoc007 = d2serdoc
077040950720     C                   EVAL      kcc007    = $kcc
077050950720     C                   EVAL      ksc007    = d2ksc
077060950720     C                   EVAL      causrig007= d2caustes
077070950721     C                   EVAL      importo007= d2imporTO
077080950721     C                   EVAL      importd007= d2importD
077090950720     C                   EVAL      divisa007= d2divisa
077100950720     C                   EVAL      cambio007= d2cambio
077110950720     C                   EVAL      untval007= d1unita
077120950720     c* Segno clienti
077130950720     C                   if        d2tpsogg = 'C'
077140950720     C                   if        opena0 = '1'
077150961009     C                   eval      segno007 = 'D'
077160950720     C                   else
077170961009     C                   eval      segno007 = 'A'
077180950720     C                   endif
077190950720     C                   endif
077200950720     c* Segno fornitori
077210950720     C                   if        d2tpsogg = 'F'
077220950720     C                   if        opena1 = '1'
077230961009     C                   eval      segno007 = 'D'
077240950720     C                   else
077250961009     C                   eval      segno007 = 'A'
077260950720     C                   endif
077270950720     C                   endif
077280950720     C*
077290950720     C                   eval      moncont007 = XSCDIV
077300981230     C                   eval      DtRfCam007 = DtRfCam
077310971222     C*
077320980119     C                   eval      DesBrev007 = D2DesBrev
077330980219     C                   eval      Entit1007  = MovEntit1
077340980219     C                   eval      Entit2007  = MovEntit2
077350980219     C                   eval      Entit3007  = MovEntit3
077360041117C1761 *
077370041117  "   * se MOVSocVal è vuota
077380041117  "   * (NDMVC011 imposta MOVSocVal = SocVal007 ricevuto)
077390041117  "   * e il conto ha trattamento intercompany di gruppo
077400041117  "   * (RCOTPCNTGR = '1'/'2'),
077410041117  "   * viene emesso l'errore MVC0090
077420041117  "  C                   eval      SocVal007  = MOVSocVal
077430041117C1761 *
077440950720     c*
077450971223     C     XRie007Ds     ENDSR
077460950724     C************************************************************
077470950817     C* Controlli base riga sfl
077480950724     C************************************************************
077490950817     C     CtrlBaseS1    BEGSR
077500950724     C*
077510950817     C* se sono stati digitati riferimenti esenzione o aliquote
077520950817     C                   IF        S1RifIVA <> *blank    or
077530950817     C                             S1Aliq   <> 0
077540950904     C*
077550950904     C* vedo se è possibile eseguire degli automatismi nel calcolo
077560961206     C* imponibile e/o imposta sempre in divisa
077570950904     C                   SELECT
077580950817     C* se non sono stati digitati ne imponibili ne imposte
077590950904     C                   WHEN      S1ImportD = 0         and
077600950817     C                             S1ImpIVAD = 0         and
077610950817     C                             S1Importo = 0         and
077620950817     C                             S1ImpIVA  = 0
077630950817     C* calcolo quelli in divisa dall'importo totale
077640950817     C                   SELECT
077650950904     C* se c'è il riferimento esenzione
077660950904     C                   WHEN      S1RifIVA <> *blank    and
077670950817     C                             S1Aliq   =  0
077680950904     C* uso l'importo come imponibile e non c'è imposta
077690950817     C                   EVAL      S1ImportD = C1ImportD
077700981005     C                   EVAL      S1Importo = C1Importo
077710950904     C* se c'è l'aliquota
077720950904     C                   WHEN      S1Aliq   <> 0         and
077730950817     C                             S1RifIVA =  *blank
077740951003     C* IMPORTI IN DIVISA
077750951003     C* eseguo lo scorporo
077760980922     C                   EVAL      W3015 = (C1ImportD*100)/(S1Aliq+100)
077770951003     C* l'arrotondo per difetto al nr. di decimali giusti per la divisa
077780980922     C                   CALLP     X0201Arrot(W3015 : '-' : £NrDecDiv)
077790980922     C                   EVAL      S1ImportD  = W3015
077800980922     C*
077810951003     C* calcolo l'imposta per differenza
077820950817     C                   EVAL      S1ImpIVAD = C1ImportD - S1ImportD
077830980922     C*
077840951003     C* IMPORTI IN MONETA DI CONTO
077850951003     C* eseguo lo scorporo
077860980922     C                   EVAL      W3015 = (C1Importo*100)/(S1Aliq+100)
077870980922     C* l'arrotondo per difetto al nr. di decimali giusti per la divisa
077880980922     C                   CALLP     X0201Arrot(W3015 : '-' : £NrDec   )
077890980922     C                   EVAL      S1Importo  = W3015
077900980922     C*
077910951003     C* calcolo l'imposta per differenza
077920951003     C                   EVAL      S1ImpIVA = C1Importo - S1Importo
077930950817     C                   ENDSL
077940950904     C* se è stato digitato solo l'imponibile
077950950904     C                   WHEN      S1ImportD > 0         and
077960950904     C                             S1ImpIVAD = 0
077970950904     C* calcolo quelli in divisa dall'importo totale
077980950904     C                   SELECT
077990950904     C* se c'è il riferimento esenzione
078000950904     C                   WHEN      S1RifIVA <> *blank    and
078010950904     C                             S1Aliq   =  0
078020950904     C* non faccio nulla perchè basta questo
078030951003     C
078040950904     C* se c'è l'aliquota
078050950904     C                   WHEN      S1Aliq   <> 0         and
078060950904     C                             S1RifIVA =  *blank
078070961127     C* IMPORTI IN DIVISA
078080950904     C* calcolo l'imposta
078090980922     C                   EVAL      W3015 = (S1ImportD * S1Aliq)/100
078100980922     C* l'arrotondo per eccesso al nr. di decimali giusti per la divisa
078110980922     C                   CALLP     X0201Arrot(W3015 : '+' : £NrDecDiv)
078120980922     C                   EVAL      S1ImpIVAD = W3015
078130980922     C*
078140961127     C* IMPORTI IN MONETA DI CONTO
078150961127     c* calcola l'imponibile in lire
078160980922     C                   RESET                   PJX00202DS
078170980922     C                   MOVEL     D2Divisa      Divisa0202
078180980922     C                   MOVEL     XscDiv        DivMC0202
078190980922     C                   Z-ADD     S1ImportD     ImporD0202
078200980922     C                   Z-ADD     0             ImporM0202
078210980922     C                   Z-ADD     D2Cambio      Cambio0202
078220980922     C                   MOVEL     WDate         DtRif0202
078230980922     C*
078240980922     C                   CALLP     X0202TestC(PJX00202DS :
078250980922     C                                        %size(PJX00202DS) :
078260980922     C                                        '0'    : Esito :
078270980922     C                                        *omit  : *omit)
078280980922     C                   EVAL      S1Importo = ImporM0202
078290980922     C*
078300961127     C* calcolo l'imposta
078310980922     C                   EVAL      W3015 = (S1Importo * S1Aliq)/100
078320980922     C* l'arrotondo per eccesso al nr. di decimali giusti per la divisa
078330980922     C                   CALLP     X0201Arrot(W3015 : '+' : £NrDec   )
078340980922     C                   EVAL      S1ImpIVA = W3015
078350000229     C*
078360950904     C                   ENDSL
078370950904     C                   ENDSL
078380950817     C                   ENDIF
078390950817     C*
078400950817     C                   ENDSR
078410990421C1176 ************************************************************
078420990421  "   * Automatismi possibili sugli imponibile e imposte
078430990421  "   ************************************************************
078440990421  "  C     RieImpS1      BEGSR
078450990421  "   *
078460990421  "  C                   SELECT
078470990421  "   *
078480990421  "   * Se sono stati digitati riferimenti esenzione o aliquote e
078490990421  "   * non sono stati digitati ne imponibili ne imposte
078500990421  "  C                   WHEN      (S1RifIVA <> *Blank OR
078510990421  "  C                             S1Aliq    <> 0)        AND
078520990421  "  C                             S1ImportD  = 0         AND
078530990421  "  C                             S1ImpIVAD  = 0         AND
078540990421  "  C                             S1Importo  = 0         AND
078550990421  "  C                             S1ImpIVA   = 0
078560990421  "   *
078570990421  "  C                   EXSR      RieImpS1A
078580990421  "   *
078590990421  "   * se solo aliquota calcolo imponib. e imposte in mc.
078600990421  "  C                   If        S1Aliq   <> 0         and
078610990421  "  C                             S1RifIva  = *Blank
078620990421  "  C                   EXSR      RieImpS1MC
078630990421  "  C                   EndIf
078640990421  "   *
078650990421  "   * Se sono stati digitati riferimenti esenzione o aliquote e
078660990421  "   * è stato digitato l'imponibile e non l'imposta
078670990421  "  C                   WHEN      (S1RifIVA <> *Blank OR
078680990421  "  C                             S1Aliq    <> 0)       AND
078690990421  "  C                             S1ImportD  > 0        AND
078700990421  "  C                             S1ImpIVAD  = 0
078710990421  "   *
078720990421  "  C                   EXSR      RieImpS1B
078730990421  "   *
078740990421  "   * se solo aliquota calcolo imponib. e imposte in mc.
078750990421  "  C                   If        S1Aliq   <> 0         and
078760990421  "  C                             S1RifIva  = *Blank
078770990421  "  C                   EXSR      RieImpS1MC
078780990421  "  C                   EndIf
078790990421  "   *
078800990421  "  C                   ENDSL
078810990421  "   *
078820990421  "  C     XRieImpS1     ENDSR
078830990506  "   ************************************************************
078840990421  "   * Non sono stati digitati né gli imponibili né le imposte
078850990421  "   * (riferim. iva oppure aliquote) imposto imponibile e
078860990421  "   * imposta in divisa
078870990421  "   ************************************************************
078880990421  "  C     RieImpS1A     BEGSR
078890990421  "   *
078900990421  "  C                   SELECT
078910990421  "   *
078920990421  "   *solo RIFERIMENTO ESENZIONE
078930990421  "  C                   WHEN      S1RifIVA <> *blank    and
078940990421  "  C                             S1Aliq   =  0
078950990421  "   * l'imponibile è l'importo di testata (l'imposta non deve esistere)
078960990421  "  C                   EVAL      S1ImportD = C1ImportD
078970990421  "  C                   EVAL      S1Importo = C1Importo
078980990421  "   *
078990990421  "   *solo ALIQUOTA
079000990421  "  C                   WHEN      S1Aliq   <> 0         and
079010990421  "  C                             S1RifIVA =  *blank
079020990421  "   *
079030990421  "   * calcolo l'imponibile in divisa con scorporo
079040990719C1176C                   EVAL      W3015 = (C1ImportD*100)/(S1Aliq+100)
079050040429D0170X**** ex C1176 arrotondo al nr. di decimali giusti per la divisa
079060040429  "  X**** ex C1176 (da 0 a 4 -> 0  da 5 a 9 -> 1)
079070040429  "  X**** ex C1176      CALLP     X0201Arrot(W3015 : 'H' : £NrDecDiv)
079080040429  "  X**** ex C1176      EVAL      S1ImportD  = W3015
079090990719  "   *
079100990719  "   * arrotondo l'imponibile in divisa
079110990719D0170C                   EXSR      ImpS1AARRO
079120990719C1176 *
079130990421  "   * calcolo l'imposta in divisa per differenza
079140990421  "  C                   EVAL      S1ImpIVAD = C1ImportD - S1ImportD
079150990421  "   *
079160990421  "  C                   ENDSL
079170990421  "   *
079180990421  "  C     XRieImpS1A    ENDSR
079190990506  "   ************************************************************
079200990421  "   * C'è  l'imponibile in divisa e non l'imposta in divisa
079210990421  "   * (riferim. iva oppure aliquote) imposto imposta in divisa
079220990421  "   ************************************************************
079230990421  "  C     RieImpS1B     BEGSR
079240990421  "   *
079250990421  "  C                   SELECT
079260990421  "   *
079270990421  "   *solo RIFERIMENTO ESENZIONE
079280990421  "  C                   WHEN      S1RifIVA <> *blank    and
079290990421  "  C                             S1Aliq   =  0
079300990421  "   * non faccio nulla perchè basta questo
079310990421  "  C
079320990421  "   *solo ALIQUOTA
079330990421  "  C                   WHEN      S1Aliq   <> 0         and
079340990421  "  C                             S1RifIVA =  *blank
079350990421  "   * calcolo l'imposta in divisa
079360990421  "  C                   EVAL      W3015 = (S1ImportD * S1Aliq)/100
079370990514  "   *
079380990514  "   * arrotondo l'imposta in divisa
079390990514  "  C                   EXSR      ImpS1BARRO
079400990421  "   *
079410990421  "  C                   ENDSL
079420990421  "   *
079430990421  "  C     XRieImpS1B    ENDSR
079440990514  "   ************************************************************
079450990514  "   * Arrotondo l'imposta in divisa
079460990514  "   ************************************************************
079470990514  "  C     ImpS1BArro    BEGSR
079480990514  "   *
079490990514  "   *Arrotondo al nr. di decimali giusti per la divisa:
079500990514  "   *per ECCESSO:se cod.internazionale della divisa del doc.='ITL'
079510990514  "   *            Oppure se la divisa NON ha DECIMALI
079520990514  "   *NORMALE: (da 0 a 4 -> 0  da 5 a 9 -> 1) x le altre divise
079530990514  "   *
079540990514  "   * Reperisco il cod. internazionale della divisa del doc.
079550990514  "  C                   Eval      $ErrDiv     = 0
079560990514  "  C                   Eval      $ErrDiv     = X0201RpDVS(D2Divisa :
079570990514  "  C                                                      WDate    :
079580990514  "  C                                                      ANDVS00F:
079590990514  "  C                                                      %size(ANDVS00F):
079600990514  "  C                                                      ANDVX00F:
079610990514  "  C                                                      %size(ANDVX00F) )
079620990514  "  C                   Select
079630990514  "   *
079640990514  "   *Se reperimento dati divisa doc.OK:
079650990514  "  C     $ErrDiv       WhenEq    0
079660990514  "   *  divisa del doc. è la Lira italiana
079670990514  "  C                   If        DVSCdInter = 'ITL'
079680990514  "   *  arrotondo per eccesso
079690990514  "  C                   CALLP     X0201Arrot(W3015 : '+' : £NrDecDiv)
079700990514  "   *  divisa del doc. non è la Lira italiana
079710990514  "  C                   Else
079720990514  "   *  arrotondo normalmente
079730990514  "  C                   CALLP     X0201Arrot(W3015 : 'H' : £NrDecDiv)
079740990514  "  C                   EndIf
079750990514  "   *
079760990514  "   *Se reperimento dati divisa doc. non OK:
079770990514  "  C     $ErrDiv       WhenNE    0
079780990514  "   *  divisa del doc. ha 0 decimali (come la Lira italiana)
079790990514  "  C                   If        £NrDecDiv = 0
079800990514  "   *  arrotondo per eccesso
079810990514  "  C                   CALLP     X0201Arrot(W3015 : '+' : £NrDecDiv)
079820990514  "   *  divisa del doc. ha > 0 decimali (non come la Lira italiana)
079830990514  "  C                   Else
079840990514  "   *  arrotondo normalmente
079850990514  "  C                   CALLP     X0201Arrot(W3015 : 'H' : £NrDecDiv)
079860990514  "  C                   EndIf
079870990514  "   *
079880990514  "  C                   EndSl
079890990518  "   *
079900990518  "   *Valorizzo l'imposta in divisa
079910990518  "  C                   EVAL      S1ImpIVAD = W3015
079920990514  "   *
079930990514  "  C     XImpS1BArr    ENDSR
079940990506  "   ************************************************************
079950990421  "   * Imposto imponibile e imposta in mc convertendoli
079960990421  "   * dagli importi in divisa, nei casi previsti
079970990421  "   * (A=non c'è nessun importo  oppure
079980990421  "   *  B=c'è  l'imponibile in divisa e non l'imposta in divisa e
079990990421  "   *  c'è solo aliquote)
080000990421  "   ************************************************************
080010990421  "  C     RieImpS1MC    BEGSR
080020990421  "   *
080030990421  "   *
080040990421  "   *Divisa = m.c.
080050990421  "  C                   If        D2Divisa = XScDiv
080060990422  "   *
080070990514  "   *  imponibile e imposta in m.c. = imponibile e imposta in divisa
080080990421  "  C                   EVAL      S1Importo  = S1ImportD
080090990421  "  C                   EVAL      S1ImpIVA   = S1ImpIVAD
080100990421  "   *
080110990421  "   *Divisa <> m.c.
080120990421  "  C                   Else
080130990421  "   *  calcola l'imponibile in lire convertendolo da imponib. in div.
080140990421  "  C                   RESET                   PJX00202DS
080150990421  "  C                   MOVEL     D2Divisa      Divisa0202
080160990421  "  C                   MOVEL     XscDiv        DivMC0202
080170990421  "  C                   Z-ADD     S1ImportD     ImporD0202
080180990421  "  C                   Z-ADD     0             ImporM0202
080190990421  "  C                   Z-ADD     D2Cambio      Cambio0202
080200990421  "  C                   MOVEL     WDate         DtRif0202
080210990421  "   *
080220990421  "  C                   CALLP     X0202TestC(PJX00202DS :
080230990421  "  C                                        %size(PJX00202DS) :
080240990421  "  C                                        '0'    : Esito :
080250990421  "  C                                        *omit  : *omit)
080260990421  "  C                   EVAL      S1Importo = ImporM0202
080270990421  "   *
080280990421  "   *  calcola l'imposta in lire convertendolo da imposta in div.
080290990421  "  C                   RESET                   PJX00202DS
080300990421  "  C                   MOVEL     D2Divisa      Divisa0202
080310990421  "  C                   MOVEL     XscDiv        DivMC0202
080320990421  "  C                   Z-ADD     S1ImpIVAD     ImporD0202
080330990421  "  C                   Z-ADD     0             ImporM0202
080340990421  "  C                   Z-ADD     D2Cambio      Cambio0202
080350990421  "  C                   MOVEL     WDate         DtRif0202
080360990421  "   *
080370990421  "  C                   CALLP     X0202TestC(PJX00202DS :
080380990421  "  C                                        %size(PJX00202DS) :
080390990421  "  C                                        '0'    : Esito :
080400990421  "  C                                        *omit  : *omit)
080410990422  "  C                   EVAL      S1ImpIVA = ImporM0202
080420990421  "   *
080430990421  "  C                   EndIf
080440990421  "   *
080450990421  "  C     XRieImpS1M    ENDSR
080460990506  "   ************************************************************
080470990426  "   * Gestione iva squadrata
080480990426  "   *    -emissione window solo se la registraz. non è in m.c  AND
080490990426  "   *     la squadratura si verifica solo sui valori in m.c.
080500990426  "   *    -ErrMsgId in tutti gli altri casi:
080510990426  "   *     squadrature sull'importo in divisa   OR
080520990426  "   *     sull'importo sia in divisa che in m.c. in una reg. in m.c.
080530990422  "   ************************************************************
080540990723  "  C     IvaSquadra    BEGSR
080550990723  "   *
080560990723  "   * Reperisco dati per emettere errori di squadratura iva
080570990426  "   * (tot.imponibile, tot./imposta, tot.testata, differenza)
080580990423  "  C                   EXSR      IvaS_RepD
080590990426  "   *
080600000229C1176C                   SELECT
080610990723C1207 *
080620990723  "   * Non è stato immesso nessun valore nel video dell'iva
080630990723  "   * (Totale imposta e imponibile in m.c. e in divisa = 0)
080640990723  "  C                   When      TotIva003   = 0   AND
080650990723  "  C                             TotImpo003  = 0   AND
080660990723  "  C                             TotIvaD003  = 0   AND
080670001023C1207C                             TotImpD003  = 0
080680040429D0611X**ex C1207******** Seton                                        5999
080690001023D0611 * PRO0505-
080700001020C1207 *   'Inserire almeno un valore'
080710001020D0611C                   Eval      $InC1(59)   = *On
080720001020D0611C                   Eval      *In99       = *On
080730001020C1207 *   spengo l'ErrMsgId acceso al ritorno da NDMVC202
080740001020C1207 *   per iva squadrata
080750040429D0611X**ex C1207 ********Move      *Off          *In66
080760001020D0611C                   Eval      $InC1(66)   = *Off
080770990723C1207 *
080780000229C1176 *
080790990426  "   * I una reg. non in m.c., i valori in mc. sono calcolati
080800990426  "   * in base ai valori in divisa (se non già digitati),
080810990426  "   * quindi se c'è una squadratura iva sui valori in m.c.,
080820990505  "   * prima di emettere il relativo errore con window,
080830990426  "   * segnalo la squadratura sui valori in divisa.
080840990426  "   * Lo stesso errore è usato in caso di registraz. in m.c.
080850990426  "  C                   WHEN      $DiffDiv  <>  0    OR
080860990426  "  C                             D2Divisa  =   XScDiv
080870990506  "   *   Valorizzo le variabili da emettere con il msg
080880990506  "  C                   Z-Add     $DiffDiv      Pack05201
080890990506  "  C                   Z-Add     $DiffMC       Pack05202
080900990506  "  C                   Z-Add     $DiffDiv      Pack05203
080910990506  "  C                   Z-Add     $DiffMC       Pack05204
080920990426  "   *
080930990426  "   * Registrazione NON in m.c. con squadratura sui valori in mc
080940990426  "  C                   WHEN      D2Divisa  <>  XScDiv   AND
080950990426  "  C                             $DiffMC   <>  0
080960990426  "   *   spengo l'indicatore dell'ErrMsgId
080970990426  "   *   (anche perchè in D2, *In66 corrisponde ad un altro ErrMsg)
080980990426  "   *   (non spengo l'indicatore d'errore generico xchè altrimenti
080990001020C1176 *    il pgm procede con gli aggiornamenti)
081000040429D0611X**ex C1176******** Move      *Off          *In66
081010001020D0611C                   Eval      $InC1(66)   = *Off
081020001020D0611 *
081030001020C1176 *   gestione msg in window.
081040990426  "  C                   EXSR      IvaS_Win
081050990426  "   *
081060990426  "  C                   EndSl
081070990423  "   *
081080990423  "  C     XIvaSquadr    ENDSR
081090990506  "   ************************************************************
081100990426  "   * Reperisco tot imponibile/imposta + tot testata + differrenza
081110990423  "   * per la gestione dell'iva squadrata
081120990423  "   ************************************************************
081130990423  "  C     IvaS_RepD     BEGSR
081140990423  "   *
081150990426  "   *Totali imponibile/imposta
081160990423  "  C                   CLEAR                   Fun
081170990423  "  C                   EVAL      Fun(5) = '2'
081180990423  "  C                   EVAL      LenOut = %Size(ND003DS)
081190990423  "  C                   CALLB     'NDMVC199'
081200990423  "  C                   PARM                    Fun
081210990423  "  C                   PARM                    ND003DS
081220990423  "  C                   PARM                    LenOut
081230990426  "   *Totali testata
081240990423  "  C                   CLEAR                   Fun
081250990423  "  C                   EVAL      Fun(6) = '2'
081260990423  "  C                   EVAL      LenOut = %Size(ND003DS)
081270990423  "  C                   CALLB     'NDMVC199'
081280990423  "  C                   PARM                    Fun
081290990423  "  C                   PARM                    ND003DS
081300990423  "  C                   PARM                    LenOut
081310990423  "   *
081320990426  "   *Differenze tra tot. imponibile/imposta e tot testata
081330990426  "  C                   Eval      $DiffMC  = 0
081340990426  "  C                   Eval      $DiffDiv = 0
081350990426  "  C                   Eval      $DiffMC  = TotTest003 +
081360990426  "  C                                        TotIva003  + Totimpo003
081370990426  "  C                   Eval      $DiffDiv = TotTesD003 +
081380990426  "  C                                        TotIvaD003 + TotimpD003
081390990426  "   *
081400990426  "   *Rendo assoluto la differenza in mc
081410990426  "  C                   If        $DiffMC  < 0
081420990426  "  C                   Eval      $DiffMC  = $DiffMC  * -1
081430990426  "  C                   EndIf
081440990426  "   *
081450990426  "   *Rendo assoluto la differenza in divisa
081460990426  "  C                   If        $DiffDiv < 0
081470990426  "  C                   Eval      $DiffDiv = $DiffDiv * -1
081480990426  "  C                   EndIf
081490990426  "   *
081500990423  "  C     XIvaS_RepD    ENDSR
081510990506  "   ************************************************************
081520990426  "   * Gestione squadratura con la finestrina
081530990426  "   * (per le registrazione non in m.c)
081540990426  "   ************************************************************
081550990426  "  C     IvaS_Win      BEGSR
081560990426  "   *
081570990426  "   *Emetto la Window
081580990426  "  C                   EXSR      IvaS_WinEm
081590990426  "   *
081600990426  "   *
081610990426  "   *Se l'utente vuole cambiare l'importo di testata
081620990426  "  C                   If        MsgRtn      = 'S'
081630990426  "   * simulo un F12 per ritornare in testata!
081640990426  "  C                   Eval      $Tasto     =  F12
081650990426  "  C                   MOVE      'D2'          $GEST
081660990427  "   * imposto la sommatoria righe Iva in testata
081670990427  "  C                   Eval      D2Importo  =  $$2TotRIva
081680990427  "   * salvo l'attributo del campo (per ripristinarlo in D2 al 1° invio)
081690990427  "  C                   Eval      $D2ImportA  = D2ImportoA
081700990427  "   * ed illumino il campo
081710990427  "  C                   Eval      D2ImportoA =  Illuminato
081720990427  "  C                   EndIf
081730990426  "   *
081740990426  "  C     XIvaS_Win     ENDSR
081750990506  "   ************************************************************
081760990423  "   * Emetto la window che mostra quali sono gli importi
081770990423  "   * che rendono l'Iva squadrata
081780990423  "   ************************************************************
081790990426  "  C     IvaS_WinEm    BEGSR
081800990423  "   *
081810990423  "  C                   CLEAR                   XMSGDS
081820990505  "  C                   CLEAR                   XMSGTXT
081830990426  "   *
081840990426  "   *"Errata quadratura Iva in m.c."
081850990426  "  C                   Movel     MsgId(8)      IDMSG
081860990423  "  C                   CALLB     'XRTVM'
081870990423  "  C                   PARM                    IDMSG            27
081880990423  "  C                   PARM      *Blank        TXTMSG          100
081890990423  "  C                   PARM                    ERRMSG            1
081900990423  "  C     ERRMSG        IFNE      *ON
081910990423  "  C                   MOVEL     TXTMSG        MSGMS1           70
081920990423  "  C                   ELSE
081930990423  "  C                   MOVEL     *ALL'?'       MSGMS1
081940990423  "  C                   ENDIF
081950000913C1176 *
081960040429D0660X*"Doc.: &1 Righe Iva: &2 Diff.: &3"
081970000913D0660 *"Tot doc.: &1 Righe Iva: &2 Diff.: &3"
081980000913C1176 *   variabili numeriche necessarie al messaggio COS0496
081990990426  "  C                   Clear                   $$2TotRIva
082000990426  "  C                   RESET                   $PxCOS0496
082010990506  "   *           documento..
082020990426  "  C                   Z-ADD     TotTest003    $P1TotTest
082030990426  "   *             ..in valore assoluto
082040990426  "  C                   If        $P1TotTest  < 0
082050990426  "  C                   Eval      $P1TotTest  = $P1TotTest * -1
082060990426  "  C                   EndIf
082070990506  "   *           totale righe iva..
082080990426  "  C                   Eval      $$2TotRIva  = TotImpo003 + TotIva003
082090990427  "   *             ..in valore assoluto
082100990427  "  C                   If        $$2TotRIva  < 0
082110990427  "  C                   Eval      $$2TotRIva  = $$2TotRIva * -1
082120990427  "  C                   EndIf
082130990426  "  C                   Z-ADD     $$2TotRIva    $P2TotRIva
082140990506  "   *           differenza..
082150990426  "  C                   Z-ADD     $DiffMc       $P3Diff
082160990426  "   *             ..in valore assoluto
082170990426  "  C                   If        $P3Diff     < 0
082180990426  "  C                   Eval      $P3Diff     = $P3Diff    * -1
082190990426  "  C                   EndIf
082200990426  "   *
082210990426  "  C                   Movel     MsgId(9)      IDMSG
082220990426  "  C                   CALLB     'XRTVM'
082230990426  "  C                   PARM                    IDMSG            27
082240990426  "  C                   PARM                    TXTMSG          100
082250990426  "  C                   PARM                    ERRMSG            1
082260990426  "  C                   PARM                    *omit
082270990426  "  C                   PARM                    *omit
082280990426  "  C                   PARM      $PxCOS0496    Msgdta          100
082290990426  "  C     ERRMSG        IFNE      *ON
082300990426  "  C                   MOVEL     TXTMSG        MSGMS2
082310990426  "  C                   ELSE
082320990426  "  C                   MOVEL     *ALL'?'       MSGMS2
082330990426  "  C                   ENDIF
082340000913C1176 *
082350040429D0660X*"Vuoi correggere il valore del documento in m.c., in testata?
082360000913D0660 *"Allineare in automatico il tot. documento in m.c. tornando in testata?"
082370000913C1176C                   Movel     MsgId(10)     IDMSG
082380990426  "  C                   CALLB     'XRTVM'
082390990426  "  C                   PARM                    IDMSG            27
082400990426  "  C                   PARM      *Blank        TXTMSG          100
082410990426  "  C                   PARM                    ERRMSG            1
082420990426  "  C     ERRMSG        IFNE      *ON
082430990426  "  C                   MOVEL     TXTMSG        MSGMS3           70
082440990426  "  C                   ELSE
082450990426  "  C                   MOVEL     *ALL'?'       MSGMS3
082460990426  "  C                   ENDIF
082470990426  "   *
082480990426  "   *
082490990423  "  C                   MOVEL     'S'           MSGEMV
082500990423  "  C                   Z-ADD     10            MSGRGP
082510990423  "  C                   MOVEL     'S'           MSGRSP
082520990423  "  C                   MOVEL     'SN'          MSGTPR
082530990423  "   *
082540990505  "   *Modo di inizializzazione della window:
082550990505  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
082560990505  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
082570990505  "   *                       per mantenere il sottofondo del video princip.)
082580990423  "  C                   Eval      XMSGINZ     = $PrimoXMsg
082590990423  "  C                   If        $PrimoXMsg  = *On
082600990423  "  C                   Eval      $PrimoXMsg  = *Off
082610990423  "  C                   EndIf
082620990423  "   *
082630990423  "  C                   CALL      'XMSGR'
082640990423  "  C                   PARM                    XMSGDS
082650990423  "  C                   PARM                    XMSGTXT         135
082660990423  "  C                   PARM                    XMSGINZ           1
082670990423  "   *
082680001020C1176C     XIvaS_WinM    ENDSR
082690990719D0170 ************************************************************
082700990719  "   * Arrotondo l'imponibile in divisa
082710990719  "   ************************************************************
082720990719  "  C     ImpS1AArro    BEGSR
082730990719  "   *
082740990719  "   * Arrotondo al nr. di decimali giusti per la divisa:
082750990719  "   * per DIFETTO (dato che l'imposta deve esserlo per eccesso):
082760990719  "   *            se cod.internazionale della divisa del doc.='ITL'
082770990719  "   *            oppure se la divisa NON ha DECIMALI
082780990719  "   * NORMALE: (da 0 a 4 -> 0  da 5 a 9 -> 1) x le altre divise
082790990719  "   *
082800990719  "   * Reperisco il cod. internazionale della divisa del doc.
082810990719  "  C                   Eval      $ErrDiv     = 0
082820990719  "  C                   Eval      $ErrDiv     = X0201RpDVS(D2Divisa :
082830990719  "  C                                                      WDate    :
082840990719  "  C                                                      ANDVS00F:
082850990719  "  C                                                      %size(ANDVS00F):
082860990719  "  C                                                      ANDVX00F:
082870990719  "  C                                                      %size(ANDVX00F) )
082880990719  "  C                   Select
082890990719  "   *
082900990719  "   *Se reperimento dati divisa doc.OK:
082910990719  "  C     $ErrDiv       WhenEq    0
082920990719  "   *  divisa del doc. è la Lira italiana
082930990719  "  C                   If        DVSCdInter = 'ITL'
082940990719  "   *  arrotondo per difetto
082950990719  "  C                   CALLP     X0201Arrot(W3015 : '-' : £NrDecDiv)
082960990719  "   *  divisa del doc. non è la Lira italiana
082970990719  "  C                   Else
082980990719  "   *  arrotondo normalmente
082990990719  "  C                   CALLP     X0201Arrot(W3015 : 'H' : £NrDecDiv)
083000990719  "  C                   EndIf
083010990719  "   *
083020990719  "   *Se reperimento dati divisa doc. non OK:
083030990719  "  C     $ErrDiv       WhenNE    0
083040990719  "   *  divisa del doc. ha 0 decimali (come la Lira italiana)
083050990719  "  C                   If        £NrDecDiv = 0
083060990719  "   *  arrotondo per difetto
083070990719  "  C                   CALLP     X0201Arrot(W3015 : '-' : £NrDecDiv)
083080990719  "   *  divisa del doc. ha > 0 decimali (non come la Lira italiana)
083090990719  "  C                   Else
083100990719  "   *  arrotondo normalmente
083110990719  "  C                   CALLP     X0201Arrot(W3015 : 'H' : £NrDecDiv)
083120990719  "  C                   EndIf
083130990719  "   *
083140990719  "  C                   EndSl
083150990719  "   *
083160990719  "   *Valorizzo l'imponibile in divisa
083170990719  "  C                   EVAL      S1ImportD = W3015
083180990719  "   *
083190001020D0170C                   ENDSR
083200990421     C************************************************************
083210990421     C* Riempimento rcd iva
083220990421     C************************************************************
083230950817     C     Rie009DS      BEGSR
083240950817     C*
083250950817     C                   RESET                   ND009DS
083260950817     C*
083270950724     C                   eval      causrig009 = s1CausRig
083280960202     C                   CALLB     'XDT4000'
083290020321     C     DtRif009      PARM                    XDTAMG           10
083300950914     C                   PARM      D2DtReg       XDTGMA            6 0
083310950914     C                   PARM      4             XDTSTA            1 0
083320961010     c                   select
083330961010     c                   When      s1segno = XscDare
083340961009     C                   eval      segno009 = 'D'
083350961010     c                   When      s1segno = XscAvere
083360961009     C                   eval      segno009 = 'A'
083370961010     c                   other
083380961010     C                   eval      segno009 = *blanks
083390961010     C                   endsl
083400950725     C*
083410950725     C                   if        S1segno = *blanks
083420950725     c* Segno clienti (contrario)
083430950724     C                   if        d1tpsogg = 'C'
083440950724     C                   if        opena0 = '1'
083450961009     C                   eval      segno009 = 'A'
083460950724     C                   else
083470961009     C                   eval      segno009 = 'D'
083480950724     C                   endif
083490950724     C                   endif
083500950725     c* Segno fornitori (contrario)
083510950724     C                   if        d1tpsogg = 'F'
083520950724     C                   if        opena1 = '1'
083530961009     C                   eval      segno009 = 'A'
083540950724     C                   else
083550961009     C                   eval      segno009 = 'D'
083560950724     C                   endif
083570950724     C                   endif
083580950725     C*
083590950725     C                   endif
083600950725     C*
083610950724     C                   eval      importo009 = s1Impiva
083620950724     C                   eval      importd009 = s1Impivad
083630950724     C                   eval      impon009 = s1Importo
083640950724     C                   eval      impond009 = s1Importd
083650950808     C     C1DtRif       ifne      0
083660960202     C                   CALLB     'XDT4000'
083670950914     C     DtRif009      PARM                    XDTAMG           10
083680950914     C                   PARM      C1DtRif       XDTGMA            6 0
083690950914     C                   PARM      4             XDTSTA            1 0
083700950725     C                   else
083710950914     c                   move      Dt0           DtRif009
083720950725     c                   endif
083730020322C1550 *
083740020321  "   * Data annotazione
083750020321  "  C     C1DtAnnot     ifne      0
083760020321  "  C                   callb     'XDT4000'
083770020321  "  C     DtAnnot009    parm                    XDtAMG           10
083780020321  "  C                   parm      C1DtAnnot     XDtGMA            6 0
083790020321  "  C                   parm      4             XDtSTA            1 0
083800020321  "  C                   else
083810020321  "  C                   move      Dt0           DtAnnot009
083820020321  "  C                   endif
083830020321C1550 * Competenza Plafon
083840950808     C                   IF        C1MMPlaf > 0
083850950808     C                   Z-add     C1MMPlaf      Mese
083860950808     C                   Z-add     C1AAPlaf      Anno
083870950808     C                   Z-add     1             Giorno
083880950809     C                   MOVE      DataAlfa      Data60
083890040429C1320X*****              CALLB     'XDT4000'
083900040429  "  X*****DtComp009     PARM                    XDTAMG           10
083910040429  "  X*****              PARM      Data60        XDTGMA            6 0
083920040429  "  X*****              PARM      4             XDTSTA            1 0
083930000127  "  C     *DMY          Move      Data60        DataISO
083940000127C1320C                   Move      DataISO       DtComp009
083950950808     C                   else
083960950914     c                   move      Dt0           DtComp009
083970950808     c                   endif
083980950725     C                   eval      tpreg009 = opetpreg
083990951009     C                   eval      libro009 = C1libro
084000950724     C                   eval      ali009 = s1aliq
084010950724     C                   eval      rifiva009 = s1rifiva
084020971211     C                   EVAL      Sosp009 = C1Sosp
084030080125C2072C                   EVAL      Elenchi009 = C1Elenchi
084040980119     C*
084050980119     C                   eval      DesBrev009 = C1DesBrev
084060950724     C*
084070971222     C     XRie009Ds     ENDSR
084080950804     C************************************************************
084090950804     C* Imposto i campi del sfl con i valori calcolati/aggiunti
084100950804     C* dalla routine di aggiornamento
084110950804     C************************************************************
084120950804     C     Rep009        BEGSR
084130950804     C*
084140950804     C                   movel     causrig009    s1CausRig
084150950804     C*
084160950804     C                   z-add     importo009    s1Impiva
084170950804     C                   z-add     importd009    s1Impivad
084180950804     C                   z-add     impon009      s1Importo
084190950804     C                   z-add     impond009     s1Importd
084200950804     C                   movel     rifiva009     s1rifiva
084210950809     C     C1DtRif       IFEQ      0
084220950914     C     DtRif009      ANDNE     Dt0
084230960202     C                   CALLB     'XDT4000'
084240950914     C                   PARM      DtRif009      XDTAMG           10
084250950914     C     C1DtRif       PARM                    XDTGMA            6 0
084260950914     C                   PARM      2             XDTSTA            1 0
084270950809     c                   ENDIF
084280020322C1550 * Data annotazione
084290020322  "  C     C1DtAnnot     ifeq      0
084300020322  "  C     DtAnnot009    andne     Dt0
084310020322  "  C                   callb     'XDT4000'
084320020322  "  C                   parm      DtAnnot009    XDtAMG           10
084330020322  "  C     C1DtAnnot     parm                    XDtGMA            6 0
084340020322  "  C                   parm      2             XDtSTA            1 0
084350020322  "  C                   endif
084360020322C1550 * Competenza Plafond
084370950809     C     C1MMPlaf      IFEQ      0
084380950914     C     Dtcomp009     ANDNE     Dt0
084390040429C1320X*****              CALLB     'XDT4000'
084400040429  "  X*****              PARM      DtComp009     XDTAMG           10
084410040429  "  X*****Data60        PARM                    XDTGMA            6 0
084420040429  "  X*****              PARM      2             XDTSTA            1 0
084430000127  "  C                   Move      DtComp009     DataISO
084440000127C1320C     *DMY          Move      DataISO       Data60
084450950809     C                   MOVE      Data60        DataAlfa
084460950808     C                   Z-add     Mese          C1MMPlaf
084470950808     C                   Z-add     Anno          C1AAPlaf
084480950809     c                   ENDIF
084490971212     C                   MOVEL     Sosp009       C1Sosp
084500080125C2072C                   MOVEL     Elenchi009    C1Elenchi
084510950804     C*
084520971211     C* riempio le descrizioni reperite dalla DS ND010DS
084530971211     C                   MOVEL     DLIDes010     C1LibroD
084540971211     C                   MOVEL     SospD010      C1SospD
084550080125C2072C                   MOVEL     EleD010       C1ElenchiD
084560971211     C*
084570950804     C                   ENDSR
084580950403     C************************************************************
084590950809     C* Riempimento decodifiche videata D2
084600950809     C* provenienti dal modulo ctrl/aggiornamento testata
084610950403     C************************************************************
084620950809     C     DecoD2111     BEGSR
084630950403     C*
084640950809     C* campi I/O del modulo testata
084650950809     C                   EVAL      D2Classe = Classe004
084660950809     C                   EVAL      D2SerReg = SerReg004
084670950809     C                   EVAL      D2Divisa = Divisa004
084680950809     C                   if        PagMan004 = *on
084690950809     C                   EVAL      D2PagMan  = xscon
084700950809     C                   else
084710950809     C                   EVAL      D2PagMan  = xscoff
084720950809     C                   endif
084730950809     C                   EVAL      D2Cambio = Cambio004
084740950809     C                   EVAL      D2Condpag = Condpag004
084750960202     C                   CALLB     'XDT4000'
084760960202     C                   PARM      DtCoCo004     XDTAMG           10
084770960202     C     D2DtCoCo      PARM                    XDTGMA            6 0
084780960202     C                   PARM      2             XDTSTA            1 0
084790960202     C                   CALLB     'XDT4000'
084800960202     C                   PARM      DtCoAn004     XDTAMG           10
084810960202     C     D2DtCoAn      PARM                    XDTGMA            6 0
084820960202     C                   PARM      2             XDTSTA            1 0
084830950809     C*
084840950809     C* campi di decodifica del modulo testata
084850950809     C                   EVAL      D2CausTesD = DesCau005
084860980113     C                   EVAL      D2ClasseD  = DesClal005
084870980113     C                   EVAL      D2Condpagd = Descdp005
084880950809     C                   EVAL      D2divisaD  = DesDIVI005
084890970520     c*
084900970520     c* Se decodifica condizioni di pagamento blank (per il fatto che
084910970520     c* nel modulo ndmvc111 non riesegue i controlli e le decodifiche per
084920970520     c* ottimizzare il programma) eseguo direttamente chain su file
084930970520     c                   if        d2CondPagd = *blanks
084940970520     c                   EVAL      CdpSocieta = xscsoc
084950970520     C                   EVAL      CdpCondPag = CondPag004
084960970520     C     K02CDP01      CHAIN     ANCDP000                           21
084970970520     C                   IF        *in21 = *off
084980980113     C                   EVAL      D2CondPagd = CDPDescr
084990080312D2355C                   else
085000080512  "  c                   if        Opz015 <> '04' and
085010080512  "  c                             Opz015 <> '05' and
085020080512  "  c                             D2CondPagA <>  protetto
085030080312  "  C                   Eval      $InD2(58)   = *On
085040080509  "  C                   Eval      *In99       = *On
085050080509D2355c                   endif
085060970520     c                   endif
085070970520     c                   endif
085080050707A1125**
085090050707  "  ** Chiamo il driver per la traduzione del codice pagamento
085100050707  "  C                   Exsr      RepTrad
085110050707  "  C                   If        Xtrtxt <> *blanks
085120050707  "  C                   Eval      D2CondPagD = XtrTxt
085130050707A1125C                   Endif
085140950809     C*
085150950809     C                   ENDSR
085160950809     C************************************************************
085170950809     C* Riempimento decodifiche videata D2
085180950809     C* provenienti dal modulo ctrl/aggiornamento righe
085190950809     C************************************************************
085200950809     C     DecoD2011     BEGSR
085210950809     C*
085220950809     C* campi I/O del modulo righe
085230950807     C                   Z-ADD     ImportD007    D2ImportD
085240950807     C                   Z-ADD     Importo007    D2Importo
085250960424     C                   Z-ADD     Cambio007     D2Cambio
085260950616     C*
085270950616     C                   ENDSR
085280940916     C************************************************************
085290940916     C* GESTIONE F02 VIDEO D1
085300940916     C************************************************************
085310940916     C     F02D1         BEGSR
085320950619     C*
085330950627     C* eseguo la sequenza dei controlli per la videata
085340950627     C* chiedendo di fare solo i controlli (senza aggiornare il rcd)
085350950627     C                   EVAL      SoloCtrl = *on
085360950627     C                   EXSR      CtrD1
085370940916     C*
085380940916     C                   ENDSR
085390940309     C************************************************************
085400940309     C* GESTIONE F03 VIDEO D1
085410940309     C************************************************************
085420940309     C     F03D1         BEGSR
085430940309     C*
085440940309     C                   MOVE      *ON           $FINE
085450950719     C                   MOVE      '1'           RET015
085460940325     C* fine programma
085470940325     C                   EXSR      ENDPGM
085480940325     C*
085490940325     C                   ENDSR
085500940309     C************************************************************
085510940309     C* GESTIONE F05 VIDEO D1
085520940309     C************************************************************
085530940309     C     F05D1         BEGSR
085540940309     C*
085550940309     C                   MOVE      *ON           $INZD1
085560940309     C*
085570940309     C                   ENDSR
085580940309     C************************************************************
085590940309     C* GESTIONE F08 VIDEO D1
085600940309     C************************************************************
085610940309     C     F08D1         BEGSR
085620940309     C*
085630940309     C                   MOVE      *ON           $FINE
085640950719     C                   MOVE      '0'           RET015
085650950808     C                   MOVE      'FINE      '  Pgm015
085660940309     C*
085670940309     C                   ENDSR
085680940309     C************************************************************
085690940309     C* GESTIONE F12 VIDEO D1
085700940309     C************************************************************
085710940309     C     F12D1         BEGSR
085720940309     C*
085730940309     C                   MOVE      *ON           $FINE
085740950719     C                   MOVE      '2'           RET015
085750940309     C*
085760940309     C                   ENDSR
085770951025     C************************************************************
085780951025     C* GESTIONE F15 VIDEO D2 - Interrogazione conti
085790951025     C************************************************************
085800951025     C     F15D2         BEGSR
085810951025     C*
085820951025     C                   if        $kcc > *blanks
085830951025     C                   reset                   ana399ds
085840951025     C                   eval      A39societa = XSCSOC
085850951025     C                   eval      A39unita = D1Unita
085860951025     C                   eval      A39ctb = Ctb015
085870951025     C                   eval      A39kcc = $kcc
085880951025     C                   eval      A39ksc = D1KSC
085890951025     C                   eval      A39SNATURA = D1TpSogg
085900951025     C                   movel     ana399ds      kpjbu
085910951025     C                   CALL      'ANA399R'                            21
085920951025     C                   PARM                    KPJBA
085930040308     C     A39ERRORI     IFEQ      '0'
085940951025     C                   endif
085950951025     C*
085960951025     C                   endif
085970951025     C                   ENDSR
085980090126c2116C************************************************************
085990090126     C* GESTIONE F16 VIDEO D2 - richiamo elenco documenti
086000090126c2116C************************************************************
086010090126     C     F16D2         BEGSR
086020090126     C*
086030090129     C                   reset                   X48ChPgmds
086040090129     C                   eval      X48Pgm      = 'NDC853R'
086050090129     C                   eval      Kpjbu       = X48ChPgmDS
086060090129     C                   call      'X48CHPGM'
086070090129     C                   parm                    Kpjba
086080090129     C                   movel     Kpjbu         x48ChPgmDS
086090090129     C                   if        x48errore   = *Off
086100090126     c*
086110090126     C                   reset                   NDC853DS
086120090126     C                   eval      Societa853 = XSCSOC
086130090126     C                   eval      opz853='05'
086140090126     c                   eval      progre853=ed2progret
086150090126     c                   eval      progrer853=ed2progrer
086160090126     c                   eval      flgcon853='1'
086170090126     C                   eval      kpjbu=NDC853DS
086180090126     C                   CALL      'NDC853R'                            21
086190090126     C                   PARM                    KPJBA
086200090126     c*
086210090126     c                   endif
086220090126     C*
086230090126C2116C                   ENDSR
086240940309     C************************************************************
086250940309     C* GESTIONE F24 VIDEO D1
086260940309     C************************************************************
086270940309     C     F24D1         BEGSR
086280940506     C*
086290940506     C                   MOVEA     $FX1          $FX
086300940506     C                   MOVEA     $FC1          $FC
086310940506     C                   MOVEA     $FR1          $FR
086320940506     C                   Z-ADD     $ULKD1        $ULK
086330940506     C                   EXSR      RIEKEY
086340940506     C                   Z-ADD     $ULK          $ULKD1            3 0
086350940720     C                   MOVEA     $FC           $FC1
086360940506     C                   MOVEL     $KEY1         Z1KE1
086370060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
086380940506     C                   MOVEL     $KEY2         Z1KE2
086390940506     C                   WRITE     FMTZ1
086400940506     C*
086410940309     C                   ENDSR
086420940131     C************************************************************
086430950616     C* Chiamata al modulo di controllo e aggiornamento
086440940131     C************************************************************
086450950616     C     CallScrivi    BEGSR
086460950616     C*
086470950616     C                   EVAL      LenIn = %Size(ND004DS)
086480950616     C                   EVAL      LenOut = %Size(ND005DS)
086490950627     C*
086500950626     C                   SELECT
086510970117     C                   WHEN      SoloCtrl = *on and ImmExp015 = '0'
086520950726     C                   MOVE      '01'          Operazione
086530950719     C                   WHEN      Opz015 = '05'
086540950726     C                   MOVE      '01'          Operazione
086550950626     C                   OTHER
086560950726     C                   MOVE      '02'          Operazione
086570950626     C                   ENDSL
086580950616     C*
086590950616     C                   CALLB     'NDMVC111'
086600950726     C                   PARM                    Operazione
086610950616     C                   PARM                    LenIn
086620950616     C                   PARM                    ND004DS
086630950616     C                   PARM      *on           GesMsg
086640950616     C                   PARM                    Esito
086650950616     C                   PARM      'ND005DS'     StrutturaO
086660950616     C                   PARM                    LenOut
086670950616     C                   PARM                    ND005DS
086680950705     C                   PARM                    *omit
086690950809     C*
086700950809     C* qualunque sia l'esito
086710950809     C* riempio la videata con le decodifiche provenienti da questo modulo
086720950809     C                   EXSR      DecoD2111
086730950620     C*
086740950620     C* se ci sono errori
086750950620     C                   IF        Esito = *on
086760940127     C*
086770950620     C* gestisco la loro emissione
086780950925     c                   clear                   TextMsg
086790950616     C                   DOU       Q2BytAva = 0
086800950616     C                   EXSR      RestoreMsg
086810950616     C                   IF        Q2BytAva > 0
086820950707     C                   EXSR      GestErr
086830950616     C                   ENDIF
086840950616     C                   ENDDO
086850950720     C*
086860950720     C                   else
086870950720     C*
086880950803     C* se inputato un modello PN
086890950803     C                   IF        D1TabPNot <> *blank
086900990923D0228C* e non siamo in solo controlli
086910990923D0228C                             and SoloCtrl = *off
086920950803     C                   Exsr      CopiaDaMod
086930950803     C                   ENDIF
086940960429     C*
086950960429     C* Se la chiamata al modulo di scrittura registr. non è di solo ctrl
086960960429     C* chiamo modulo controllo movimento contabile
086970971222     C                   IF        SoloCtrl = *off
086980950720     C*
086990950720     C                   EXSR      RIE007DS
087000950720     C*
087010950720     C                   EVAL      LenIn = %Size(ND007DS)
087020950720     C                   EVAL      LenOut = %Size(ND008DS)
087030950725     C                   eval      NrRigaM = 1
087040950720     c*
087050950720     C                   CALLB     'NDMVC011'
087060950720     C                   PARM                    NrRigaM
087070040309     C                   PARM                    Operazione
087080950720     C                   PARM                    LenIn
087090950720     C                   PARM                    ND007DS
087100950720     C                   PARM      *On           GesMsg
087110950720     C                   PARM                    Esito
087120950720     C                   PARM      'ND008DS'     StrutturaO
087130950720     C                   PARM                    LenOut
087140950720     C                   PARM                    ND008DS
087150950720     C                   PARM                    *omit
087160950720     C                   PARM                    *omit
087170950809     C*
087180950809     C* qualunque sia l'esito
087190950809     C* riempio la videata con le decodifiche provenienti da questo modulo
087200950809     C                   EXSR      DecoD2011
087210950720     C*
087220950720     C* se ci sono errori
087230950807     C                   IF        Esito = *on    and
087240950807     C* e non siamo in solo ctrl
087250950807     C                             SoloCtrl = *off
087260950720     C*
087270950720     C* gestisco la loro emissione
087280950925     c                   clear                   TextMsg
087290950720     C                   DOU       Q2BytAva = 0
087300950720     C                   EXSR      RestoreMsg
087310950720     C                   IF        Q2BytAva > 0
087320950720     C                   EXSR      GestErr
087330950720     C                   ENDIF
087340950720     C                   ENDDO
087350950720     C                   ENDIF
087360950616     C*
087370950620     C                   ENDIF
087380960429     C*
087390960429     C                   ENDIF
087400950620     C*
087410940117     C                   ENDSR
087420950803     C************************************************************
087430950803     C* Crea riga da modello
087440950803     C************************************************************
087450950803     C     CopiaDaMod    BEGSR
087460950803     C*
087470950803     C* controllo che non esista nessun movimento in NDMOV
087480950803     C                   EVAL      LenOut = %Size(NDMOV00F)
087490950803     C                   eval      Strutturao = 'NDMOV'
087500950803     C                   eval      Codoper    = '1'
087510950803     C                   CALLb     'NDDRVMOV'
087520040309     C                   PARM                    RegSys
087530040309     C                   PARM                    RegNrAsReg
087540040309     C                   PARM                    NrRigaM
087550950803     C                   PARM                    StrutturaO
087560950803     C                   PARM                    LenOut
087570950803     C                   PARM                    Codoper
087580950803     C                   PARM                    NDMOV00F
087590950803     C*
087600950803     C                   IF        Codoper = '9'
087610950803     C                   Eval      LenIn = %Size(ND004DS)
087620950803     C                   CallB     'NDMVC118'
087630960517     C                   PARM                    Sys011
087640960517     C                   PARM                    NrAsReg011
087650951002     C                   PARM      *oN           FlgImporti
087660950803     C                   PARM                    ND004DS
087670950803     C                   PARM                    LenIn
087680950803     C                   PARM                    Esito
087690980722     C                   PARM                    $Kcc
087700980722     C                   PARM                    D1Ksc
087710950803     C                   endif
087720950803     C*
087730950803     C                   ENDSR
087740950711     C************************************************************
087750950711     C* Chiamata al modulo di cancellazione
087760950711     C************************************************************
087770950711     C     CallCancel    BEGSR
087780950711     C*
087790950711     C                   CALLB     'NDMVC116'
087800951010     C                   PARM                    RegSys
087810951010     C                   PARM                    RegNrAsReg
087820950727     C                   PARM      '02'          Operazione
087830950711     C                   PARM      *on           GesMsg
087840950711     C                   PARM                    Esito
087850950711     C*
087860950711     C* se ci sono errori
087870950711     C                   IF        Esito = *on
087880950711     C*
087890950711     C* gestisco la loro emissione
087900950925     c                   clear                   TextMsg
087910950711     C                   DOU       Q2BytAva = 0
087920950711     C                   EXSR      RestoreMsg
087930950711     C                   IF        Q2BytAva > 0
087940950711     C                   EXSR      GestErr
087950950711     C                   ENDIF
087960950711     C                   ENDDO
087970950711     C*
087980990806C1211C* rilasci la registrazione solo se non ci sono stati errori
087990990806  "  C                   ELSE
088000990806C1211C                   CALLB     'NDMVC115'
088010950711     C                   ENDIF
088020950711     C*
088030950711     C                   EVAL      SoloCtrl = *off
088040040429     X* RILASCIO REGISTRAZIONE
088050040429C1211X****************** CALLB     'NDMVC115'
088060950711     C*
088070950711     C                   ENDSR
088080001020C1211C************************************************************
088090990803  "  C* Richiamo UserExit
088100990803  "  C************************************************************
088110990803  "  C     CallUsrExt    BEGSR
088120990803  "  C*
088130990803  "  C     USRERR        IFEQ      *BLANK
088140990803  "  C                   RESET                   XUKDS
088150990803  "  C* passa opzione ricevuta a user exit + nome pgm chiamante
088160990803  "  C                   MOVEL     OPZ015        XUKOPZ
088170990803  "  C                   MOVEL     DSPGM         XUKPGM
088180990803  "  C* gli altri dati non servono perchè desumibili dal rcd di NDREG
088190990803  "  C                   MOVEL     XUKDS         KPJBU
088200990916  "  C* passo anche il rcd di NDREG mediante il campo UEREG lungo 300 A
088210990916  "  C* (dovrebbe essere una lunghezza di sicurezza)
088220990916  "  C                   MOVEL     £NDREG        UEREG
088230990803  "  C     DSPGM         CAT       'U':0         USREXT           10
088240990803  "  C                   CALL      USREXT                               21
088250990803  "  C                   PARM                    KPJBA
088260990916  "  C                   PARM                    UEREG
088270990803  "  C                   PARM                    UEErr
088280990803  "  C                   PARM                    UEMsgId
088290990803  "  C* qualora la chiamata non sia andata a buon fine (pgm
088300990803  "  C*  inesistente) memorizzo un flag per evitare di ripeterla altre
088310990803  "  C* volte
088320990803  "  C     *IN21         IFEQ      *ON
088330990803  "  C                   MOVE      *ON           USRERR            1
088340990803  "  C*
088350990803  "  C* se invece la chiamata c'è stata
088360990803  "  C                   ELSE
088370990803  "  C* ma ho ricevuto un errore
088380990803  "  C                   IF        UEErr = *on
088390990803  "  C* eseguo l'emissione del messaggio avuto in output
088400990803  "  C* (per sfruttare la routine GestErr valorizzo con il msgid in
088410990803  "  C*  output il campo Q2MsgId che normalmente tornerebbe dall'API)
088420990803  "  C                   EVAL      Q2MsgId = UEMsgId
088430990805  "  C                   CLEAR                   TextMsg
088440990803  "  C                   EXSR      GestErr
088450990803  "  C                   ENDIF
088460990803  "  C                   ENDIF
088470990803  "  C                   ENDIF
088480990803  "  C*
088490990803C1211C                   ENDSR
088500050707A1125C************************************************************
088510050707  "  C* REPERIMENTO TRADUZIONE CONDIZIONI PAGAMENTO
088520050707A1125C************************************************************
088530050707     C     RepTrad       BEGSR
088540050707     **
088550050707     ** Lingua della società
088560050707     C                   Eval      XtrLin     =  XscLin
088570050707     **
088580050707     C                   Eval      XtrRic     = '1'
088590050707     C                   Eval      XtrSoc     =  XscSoc
088600050707     C                   Eval      XtrFil     =  'ANCDP'
088610050707     C                   Eval      XtrFld     =  'CDPDESCR'
088620050707     C                   Eval      XtrKey     =  D2CondPag
088630050707     C                   Eval      XtrAlc     = *off
088640050707     C                   Eval      XtrErr     = *off
088650050707     C                   Eval      XtrPar     = 'ANCDP00F'
088660050707     ** Indico al cappello di utilizzare la lingua di xsoc e di reperire le traduzioni
088670050707     C                   Eval      XtrUtiLin  = 'X'
088680050707     **
088690050707     ** Chiamata al Pgm
088700050707     C                   CallB     'XTRA'
088710050707     C                   Parm                    XtraDs
088720050707     **
088730050707     ** Se vi sono errori pulisco traduzione
088740050707     C                   If        XtrErr <> *off
088750050707     C                   Eval      XtrTxt = *blanks
088760050707     C                   Endif
088770050707     **
088780050707A1125C                   ENDSR
088790950616     C************************************************************
088800950616     C* Recupero i msg sono stati generati dal modulo chiamato
088810950616     C************************************************************
088820950616     C     RestoreMsg    BEGSR
088830950616     C*
088840950616     C                   CALL      'QMHRCVPM'
088850950616     C                   PARM                    QInfo
088860950616     C                   PARM      113           QLenMsg
088870950719     C                   PARM      'RCVM0100'    QFormat
088880950616     C                   PARM      '*'           QCallMsg
088890950616     C                   PARM      0             QCallStack
088900950616     C                   PARM      '*DIAG'       QMsgTyp
088910950616     C                   PARM                    QMsgKey
088920950616     C                   PARM      0             QWait
088930950616     C                   PARM      '*REMOVE'     QAction
088940950616     C                   PARM                    QErrCod
088950950616     C*
088960950616     C                   ENDSR
088970950616     C************************************************************
088980950725     C* Gestione messaggi per le prime 2 videate
088990950616     C************************************************************
089000950707     C     GestErr       BEGSR
089010950616     C*
089020950616     C                   SELECT
089030040429D0611X************************************************************
089040040429  "  X*Routine 'ristrutturata' per il seguente motivo:
089050040429  "  X*quando viene acceso un indicatore di errore questo
089060040429  "  X*viene emesso indipendentemente dalla videata in cui
089070040429  "  X*l'utente si trova; se lo stesso indicatore è presente
089080040429  "  X*in più videate, è possibile che venga emesso l'errore
089090040429  "  X*nella videata sbagliata.
089100040429  "  X************************************************************
089110040429  "  X***                WHEN      Q2MsgId = 'MVC0027'
089120040429  "  X***                SETON                                        4399
089130040429  "  X***                WHEN      Q2MsgId = 'MVC0030'
089140040429  "  X***                SETON                                        4899
089150040429  "  X***                WHEN      Q2MsgId = 'MVC0038'
089160040429  "  X***                SETON                                        4999
089170040429  "  X***                WHEN      Q2MsgId = 'PRO0028'
089180040429  "  X***                SETON                                        5199
089190040429  "  X***                WHEN      Q2MsgId = 'PRO0014'
089200040429  "  X***                SETON                                        5299
089210040429  "  X***                WHEN      Q2MsgId = 'MVC0016'
089220040429  "  X***                SETON                                        5399
089230040429  "  X***                WHEN      Q2MsgId = 'MVC0024'
089240040429  "  X***                SETON                                        5499
089250040429  "  X***                WHEN      Q2MsgId = 'PRO0339'
089260040429  "  X***                SETON                                        5599
089270040429  "  X***                WHEN      Q2MsgId = 'PRO0178'
089280040429  "  X***                SETON                                        5899
089290040429  "  X***                WHEN      Q2MsgId = 'MVC0001'
089300040429  "  X***                SETON                                        5999
089310040429  "  X***                WHEN      Q2MsgId = 'MVC0017'
089320040429  "  X***                SETON                                        6199
089330040429  "  X***                WHEN      Q2MsgId = 'PRO0303'
089340040429  "  X***                SETON                                        6299
089350040429  "  X***                WHEN      Q2MsgId = 'PRO0313'
089360040429  "  X***                SETON                                        6399
089370040429  "  X***                WHEN      Q2MsgId = 'PRO0324'
089380040429  "  X***                SETON                                        6499
089390040429  "  X***                WHEN      Q2MsgId = 'MVC0031'
089400040429  "  X***                SETON                                        6599
089410040429  "  X***                WHEN      Q2MsgId = 'MVC0012'
089420040429  "  X***                SETON                                        6699
089430040429  "  X***                WHEN      Q2MsgId = 'PRO0051'
089440040429  "  X***                SETON                                        6799
089450040429  "  X***                WHEN      Q2MsgId = 'MVC0013'
089460040429  "  X***                SETON                                        6899
089470040429  "  X***                WHEN      Q2MsgId = 'PRO0001'
089480040429  "  X***                SETON                                        6999
089490040429  "  X***                WHEN      Q2MsgId = 'MVC0008'
089500040429  "  X***                MOVEL     Q2MsgTxt      TipoData          1
089510040429  "  X***                SELECT
089520040429  "  X* data registrazione
089530040429  "  X***                WHEN      TipoData = 'R'
089540040429  "  X***                MOVEL     TipoData      TipoDataR
089550040429  "  X***                SETON                                        6999
089560040429  "  X* data competenza co.ge.
089570040429  "  X***                WHEN      TipoData = 'C'
089580040429  "  X***                MOVEL     TipoData      TipoDataC
089590040429  "  X***                SETON                                        7399
089600040429  "  X* data competenza analitica
089610040429  "  X***                WHEN      TipoData = 'A'
089620040429  "  X***                MOVEL     TipoData      TipoDataA
089630040429  "  X***                SETON                                        8299
089640040429  "  X***                ENDSL
089650040429  "  X***                WHEN      Q2MsgId = 'MVC0004'
089660040429  "  X***                SETON                                        7099
089670040429  "  X***                WHEN      Q2MsgId = 'MVC0005'
089680040429  "  X***                SETON                                        7199
089690040429  "  X***                WHEN      Q2MsgId = 'MVC0007'
089700040429  "  X***                SETON                                        7299
089710040429  "  X***                WHEN      Q2MsgId = 'MVC0015'
089720040429  "  X***                SETON                                        7499
089730040429  "  X***                WHEN      Q2MsgId = 'MVC0014'
089740040429  "  X***                SETON                                        7599
089750040429  "  X***                WHEN      Q2MsgId = 'MVC0018'
089760040429  "  X***                SETON                                        7699
089770040429  "  X***                WHEN      Q2MsgId = 'PRO0330'
089780040429  "  X***                SETON                                        7799
089790040429  "  X***                WHEN      Q2MsgId = 'MVC0011'
089800040429  "  X***                SETON                                        7899
089810040429  "  X***                WHEN      Q2MsgId = 'MVC0010'
089820040429  "  X***                WHEN      Q2MsgId = 'MVC0082'
089830040429  "  X***                SETON                                        7999
089840040429  "  X***                WHEN      Q2MsgId = 'MVC0006'
089850040429  "  X***                SETON                                        8099
089860040429  "  X***                WHEN      Q2MsgId = 'MVC0009'
089870040429  "  X***                SETON                                        8199
089880040429  "  X***                WHEN      Q2MsgId = 'MVC0050'
089890040429  "  X***                SETON                                        8599
089900040429  "  X***                WHEN      Q2MsgId = 'MVC0026'
089910040429  "  X***                SETON                                        8799
089920040429  "  X***                WHEN      Q2MsgId = 'MVC0025'
089930040429  "  X***                SETON                                        8999
089940040429  "  X***                WHEN      Q2MsgId = 'PRO0291'
089950040429  "  X***                SETON                                        9099
089960040429  "  X***                WHEN      Q2MsgId = 'MVC0002'
089970040429  "  X***                SETON                                        9199
089980040429  "  X***                WHEN      Q2MsgId = 'MVC0066'
089990040429  "  X***                SETON                                        6099
090000040429  "  X***                WHEN      Q2MsgId = 'MVC0064'
090010040429  "  X***                SETON                                        9299
090020040429  "  X***                WHEN      Q2MsgId = 'MVC0074'
090030040429D0611X***                SETON                                        3899
090040001020D0611 *
090050001020  "   *D1--------------------------------------------------------------
090060001020  "   *
090070001020  "   *'Unita inesistente'(D1Unita)
090080001020  "  C                   WHEN      Q2MsgId     = 'PRO0028'
090090001020  "   *       se unita gestita dalla societa
090100001020  "  C                   If        FGO1        = *On
090110001020  "  C                   Eval      $InD1(51)   = *On
090120001020  "  C                   Eval      *In99       = *On
090130001020  "  C                   EndIf
090140001020  "   *
090150001020  "   *'Campo obbligatorio' (D1Unita/D2CondPag/C1DtRif)
090160001020  "  C                   WHEN      Q2MsgId     = 'PRO0014'
090170001020  "   *       se unita gestita dalla societa
090180001020  "  C                   If        FGO1        = *On
090190001020  "  C                   Eval      $InD1(52)   = *On
090200001020  "  C                   EndIf
090210001020  "  C                   Eval      $InD2(52)   = *On
090220001020  "  C                   Eval      $InC1(52)   = *On
090230001020  "  C                   Eval      *In99       = *On
090240001020  "   *
090250001020  "   *'Causale operativa non corretta' (D1CausTes/D2CausTes)
090260001020  "  C                   WHEN      Q2MsgId     = 'MVC0001'
090270001020  "  C                   Eval      $InD1(59)   = *On
090280001020  "  C                   Eval      $InD2(59)   = *On
090290001020  "  C                   Eval      *In99       = *On
090300001020  "   *
090310001020  "   *'Causale non compatibile con il conto' (D1CausTes/D2Msg)
090320001020  "  C                   WHEN      Q2MsgId     = 'PRO0291'
090330001020  "  C                   Eval      $InD1(90)   = *On
090340001020  "  C                   Eval      $InD2(90)   = *On
090350001020  "  C                   Eval      *In99       = *On
090360001020  "   *
090370001020  "   *'Conto non corretto' (D1Ksc/D2Ksc/C1Ksc)
090380001020  "  C                   WHEN      Q2MsgId     = 'MVC0002'
090390001020  "  C                   Eval      $InD1(91)   = *On
090400001020  "  C                   Eval      $InD2(91)   = *On
090410001020  "  C                   Eval      $InC1(91)   = *On
090420001020  "  C                   Eval      *In99       = *On
090430001020  "   *
090440001020  "   *D2--------------------------------------------------------------
090450001020  "   *
090460001020  "   *'Conto non movimentabile' (D2Ksc/C1Ksc)
090470001020  "  C                   WHEN      Q2MsgId     = 'MVC0027'
090480001020  "  C                   Eval      $InD2(43)   = *On
090490001020  "  C                   Eval      $InC1(43)   = *On
090500001020  "  C                   Eval      *In99       = *On
090510001020  "   *
090520001020  "   *'Movimento non annullabile' (D2Msg)
090530001020  "  C                   WHEN      Q2MsgId     = 'MVC0030'
090540001020  "  C                   Eval      $InD2(48)   = *On
090550001020  "  C                   Eval      *In99       = *On
090560001020  "   *
090570001020  "   *'Non è possibile aggiungere o annullare movimenti' (D2Msg)
090580001020  "  C                   WHEN      Q2MsgId     = 'MVC0038'
090590001020  "  C                   Eval      $InD2(49)   = *On
090600001020  "  C                   Eval      *In99       = *On
090610001020  "   *
090620001020  "   *'Ctb del movimento incoerente con quella della causale' (D2Msg)
090630001020  "  C                   WHEN      Q2MsgId     = 'MVC0016'
090640001020  "  C                   Eval      $InD2(53)   = *On
090650001020  "  C                   Eval      *In99       = *On
090660001020  "   *
090670001020  "   *'Condiz. di pag. deve essere valorizzata solo se reg Iva'(D2CondPag)
090680001020  "  C                   WHEN      Q2MsgId     = 'MVC0024'
090690001020  "  C                   Eval      $InD2(54)   = *On
090700001020  "  C                   Eval      *In99       = *On
090710001020  "   *
090720001020  "   *'Classe contabile inesistente in tab.G81' (D2Classe)
090730001020  "  C                   WHEN      Q2MsgId     = 'PRO0339'
090740001020  "  C                   Eval      $InD2(55)   = *On
090750001020  "  C                   Eval      *In99       = *On
090760001020  "   *
090770001020  "   *'Codice condizione di pagamento inesistente' (D2CondPag)
090780001020  "  C                   WHEN      Q2MsgId     = 'PRO0178'
090790001020  "  C                   Eval      $InD2(58)   = *On
090800001020  "  C                   Eval      *In99       = *On
090810001020  "   *
090820001020  "   *'Tp Reg movimento incoerente con quello dellla causale' (D2Msg)
090830001020  "  C                   WHEN      Q2MsgId     = 'MVC0017'
090840001020  "  C                   Eval      $InD2(61)   = *On
090850001020  "  C                   Eval      *In99       = *On
090860001020  "   *
090870001020  "   *'Serie inesistente' (D2SerReg)
090880001020  "  C                   WHEN      Q2MsgId     = 'PRO0303'
090890001020  "  C                   Eval      $InD2(62)   = *On
090900001020  "  C                   Eval      *In99       = *On
090910001020  "   *
090920001020  "   *'Serie non abilitata all'Iva' (D2SerReg)
090930001020  "  C                   WHEN      Q2MsgId     = 'PRO0313'
090940001020  "  C                   Eval      $InD2(63)   = *On
090950001020  "  C                   Eval      *In99       = *On
090960001020  "   *
090970001020  "   *'Serie non ammessa con la causale' (D2SerReg)
090980001020  "  C                   WHEN      Q2MsgId     = 'PRO0324'
090990001020  "  C                   Eval      $InD2(64)   = *On
091000001020  "  C                   Eval      *In99       = *On
091010001020  "   *
091020001020  "   *'Riferimenti documento non corretto' (D2Dt/Nr/SerDoc)
091030001020  "  C                   WHEN      Q2MsgId     = 'MVC0031'
091040001020  "  C                   Eval      $InD2(65)   = *On
091050001020  "  C                   Eval      *In99       = *On
091060001020  "   *
091070001020  "   *'La divisa deve essere diversa dalla m.c.' (D2Divisa)
091080001020  "  C                   WHEN      Q2MsgId     = 'MVC0012'
091090001020  "  C                   Eval      $InD2(66)   = *On
091100001020  "  C                   Eval      *In99       = *On
091110001020  "   *
091120001020  "   *'Divisa inesistente' (D2Divisa)
091130001020  "  C                   WHEN      Q2MsgId     = 'PRO0051'
091140001020  "  C                   Eval      $InD2(67)   = *On
091150001020  "  C                   Eval      *In99       = *On
091160001020  "   *
091170001020  "   *'Cambio non reperibile alla data' (D2Cambio)
091180001020  "  C                   WHEN      Q2MsgId     = 'MVC0013'
091190001020  "  C                   Eval      $InD2(68)   = *On
091200001020  "  C                   Eval      *In99       = *On
091210001020  "   *
091220001020  "   *'(F1) Data < stop di periodo'
091230001020  "  C                   WHEN      Q2MsgId     = 'MVC0008'
091240001020  "  C                   MoveL     Q2MsgTxt      TipoData          1
091250001020  "   *
091260001020  "  C                   Select
091270001020  "   *
091280001020  "   *'(F1) Data < stop di periodo' (D2DtReg)
091290001020  "   * data registrazione
091300001020  "  C                   When      TipoData    = 'R'
091310001020  "  C                   MoveL     TipoData      TipoDataR
091320001020  "  C                   Eval      $InD2(69)   = *On
091330001020  "  C                   Eval      *In99       = *On
091340001020  "   *
091350001020  "   *
091360001020  "   *'(F1) Data < stop di periodo' (D2DtCoCo)
091370001020  "   * data competenza co.ge.
091380001020  "  C                   When      TipoData    = 'C'
091390001020  "  C                   MoveL     TipoData      TipoDataC
091400001020  "  C                   Eval      $InD2(73)   = *On
091410001020  "  C                   Eval      *In99       = *On
091420001020  "   *
091430001020  "   *'(F1) Data < stop di periodo' (D2DtCoAn)
091440001020  "   * data competenza analitica
091450001020  "  C                   When      TipoData    = 'A'
091460001020  "  C                   MoveL     TipoData      TipoDataA
091470001020  "  C                   Eval      $InD2(82)   = *On
091480001020  "  C                   Eval      *In99       = *On
091490001020  "   *
091500001020  "  C                   EndSl
091510001020  "   *
091520001020  "   *'Dt Reg non appartiene ad un esercizio aperto' (D2DtReg)
091530001020  "  C                   WHEN      Q2MsgId     = 'MVC0004'
091540001020  "  C                   Eval      $InD2(70)   = *On
091550001020  "  C                   Eval      *In99       = *On
091560001020  "   *
091570001020  "   *'Dt Reg > data odierna' (D2DtReg)
091580001020  "  C                   WHEN      Q2MsgId     = 'MVC0005'
091590001020  "  C                   Eval      $InD2(71)   = *On
091600001020  "  C                   Eval      *In99       = *On
091610001020  "   *
091620001020  "   *'Dt Reg < dt ultima reg iva' (D2DtReg)
091630001020  "  C                   WHEN      Q2MsgId     = 'MVC0007'
091640001020  "  C                   Eval      $InD2(72)   = *On
091650001020  "  C                   Eval      *In99       = *On
091660001020  "   *
091670001023  "   *'Registrazione doppia' (D2DtReg/D2NrReg/D2SerReg)
091680001020  "  C                   WHEN      Q2MsgId     = 'MVC0015'
091690001020  "  C                   Eval      $InD2(74)   = *On
091700001020  "  C                   Eval      *In99       = *On
091710001020  "   *
091720001020  "   *'Nr reg non corretto' (D2NrReg)
091730001020  "  C                   WHEN      Q2MsgId     = 'MVC0014'
091740001020  "  C                   Eval      $InD2(75)   = *On
091750001020  "  C                   Eval      *In99       = *On
091760001020  "   *
091770001020  "   *'Dt Reg < dt ultimo numereatore' (D2DtReg)
091780001020  "  C                   WHEN      Q2MsgId     = 'MVC0018'
091790001020  "  C                   Eval      $InD2(76)   = *On
091800001020  "  C                   Eval      *In99       = *On
091810001020  "   *
091820001020  "   *'Non è stato reperito il numeratore per la serie desiderata' (D2SerReg)
091830001020  "  C                   WHEN      Q2MsgId     = 'PRO0330'
091840001020  "  C                   Eval      $InD2(77)   = *On
091850001020  "  C                   Eval      *In99       = *On
091860001020  "   *
091870001020  "  C                   WHEN      Q2MsgId     = 'MVC0010'
091880001020  "   *
091890001020  "   *'Conto non congruente con uso della cond. di pagam.' (D2CondPag)
091900001020  "  C                   WHEN      Q2MsgId     = 'MVC0082'
091910001020  "  C                   Eval      $InD2(79)   = *On
091920001020  "  C                   Eval      *In99       = *On
091930001020  "   *
091940001020  "   *'(F1) La dt reg deve essere > della dt ultimo giorn bollato'(D2DtReg)
091950001020  "  C                   WHEN      Q2MsgId     = 'MVC0006'
091960001020  "  C                   Eval      $InD2(80)   = *On
091970001020  "  C                   Eval      *In99       = *On
091980001020  "   *
091990001020  "   *'(F1) Data competenza CoGe non corretta.' (D2DtCoCo)
092000001020  "  C                   WHEN      Q2MsgId     = 'MVC0009'
092010001020  "  C                   Eval      $InD2(81)   = *On
092020001020  "  C                   Eval      *In99       = *On
092030001020  "   *
092040001020  "   *'(F1) Registrazione non annullabile' (D2Msg/C1..)
092050001020  "  C                   WHEN      Q2MsgId     = 'MVC0050'
092060001020  "  C                   Eval      $InD2(85)   = *On
092070001020  "  C                   Eval      $InC1(85)   = *On
092080001020  "  C                   Eval      *In99       = *On
092090001020  "   *
092100001020  "   *'Impossibile modificare il contenuto dei campi' (D2Msg)
092110001020  "  C                   WHEN      Q2MsgId     = 'MVC0026'
092120001020  "  C                   Eval      $InD2(87)   = *On
092130001020  "  C                   Eval      *In99       = *On
092140001020  "   *
092150001020  "   *'Conto non ammesso per ctb e tp reg passata' (D2Ksc/C1Ksc)
092160001020  "  C                   WHEN      Q2MsgId     = 'MVC0025'
092170001020  "  C                   Eval      $InD2(89)   = *On
092180001020  "  C                   Eval      $InC1(89)   = *On
092190001020  "  C                   Eval      *In99       = *On
092200001020  "   *
092210001020  "   *'(F1) Non è possibile generare effetti per questa reg' (D2CondPag)
092220001020  "  C                   WHEN      Q2MsgId     = 'MVC0066'
092230001020  "  C                   Eval      $InD2(60)   = *On
092240001020  "  C                   Eval      *In99       = *On
092250001020  "   *
092260001020  "   *'(F1) Imp. in m.c. incongruente rispetto al cambio indicato o reperito'
092270001020  "   * (D2ImportD)
092280001020  "  C                   WHEN      Q2MsgId     = 'MVC0064'
092290001020  "  C                   Eval      $InD2(92)   = *On
092300001020  "  C                   Eval      *In99       = *On
092310001020  "   *
092320001020  "   *'(F1) La dt comp analitica non è corretta' (D2DtCoAn)
092330001020  "  C                   WHEN      Q2MsgId     = 'MVC0074'
092340001020  "  C                   Eval      $InD2(38)   = *On
092350001020  "  C                   Eval      *In99       = *On
092360001020  "   *
092370001020  "   *C1--------------------------------------------------------------
092380001020  "   *
092390001020D0611 *
092400001020     C                   OTHER
092410001020     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
092420950707     C                   MOVE      Q2MsgId       IdMsg
092430950704     C                   CALLB     'XRTVM'
092440950704     C                   PARM                    IDMSG            27
092450950704     C                   PARM                    TXTMSG          100
092460950704     C                   PARM                    ERRMSG            1
092470950905     C                   PARM                    *omit
092480950905     C                   PARM                    *omit
092490950905     C                   PARM      Q2msgtxt      Msgdta          100
092500950704     C                   IF        ErrMsg <> *on
092510950905     C                   CAT       TxtMsg:1      TextMsg
092520040429D0611X*******************SETON                                        5099
092530001023D0611 * CPF9898-&1
092540001023  "  C                   Eval      $InD1(50)   = *On
092550001020  "  C                   Eval      $InD2(50)   = *On
092560001020  "  C                   Eval      $InC1(50)   = *On
092570001020D0611C                   Eval      *In99       = *On
092580950704     C                   ENDIF
092590950616     C                   ENDSL
092600950616     C*
092610950616     C                   ENDSR
092620950724     C************************************************************
092630950724     C* Gestione messaggi per il SUBFILE
092640950724     C************************************************************
092650950724     C     GestErr201    BEGSR
092660950724     C*
092670950724     C                   SELECT
092680000912D0660 *'(F1) Importo non valido'(S1ImportD)
092690950724     C                   WHEN      Q2MsgId = 'MVC0034'
092700040429D0611X****************** SETON                                        6099
092710001020D0611C                   Eval      $InC1(60)   = *On
092720001020D0611C                   Eval      *In99       = *On
092730000912D0660 *'Aliquota o riferimento esenzione non sono corretti'(S1RifIva/S1Aliq)
092740950724     C                   WHEN      Q2MsgId = 'MVC0033'
092750040429D0611X****************** SETON                                        6199
092760001020D0611C                   Eval      $InC1(61)   = *On
092770001020D0611C                   Eval      *In99       = *On
092780000912D0660 *'Registro e libro IVA inesistenti'( )
092790950724     C                   WHEN      Q2MsgId = 'PRO0306'
092800040429D0611X****************** SETON                                        6299
092810001020D0611C                   Eval      $InC1(62)   = *On
092820001020D0611C                   Eval      *In99       = *On
092830040429D0660X**Data liquidazione IVA non valida
092840000912D0660 *'(F1) Data liquidazione IVA non valida'(C1DtRif)
092850950724     C                   WHEN      Q2MsgId = 'MVC0032'
092860040429D0611X****************** SETON                                        6399
092870001020D0611C                   Eval      $InC1(63)   = *On
092880001020D0611C                   Eval      *In99       = *On
092890000912D0660 *'Segno non corretto'( )
092900950724     C                   WHEN      Q2MsgId = 'MVC0028'
092910040429D0611X****************** SETON                                        6499
092920001020D0611C                   Eval      $InC1(64)   = *On
092930001020D0611C                   Eval      *In99       = *On
092940000912D0660 *'(F1) Causale operativa &2 non corretta'(S1CausRig)
092950950724     C                   WHEN      Q2MsgId = 'MVC0001'
092960040429D0611X****************** SETON                                        6599
092970001020D0611C                   Eval      $InC1(65)   = *On
092980001020D0611C                   Eval      *In99       = *On
092990040429D0660X**Datario inesistente
093000000912D0660 *'(F1) Datario IVA inesistente'(C1DtRif)
093010971210     C                   WHEN      Q2MsgId = 'MVC0085'
093020040429D0611X****************** SETON                                        6899
093030001020D0611C                   Eval      $InC1(68)   = *On
093040001020D0611C                   Eval      *In99       = *On
093050971211     C* Valore errato per l'IVA sospesa
093060971216     C                   WHEN      Q2MsgId = 'MVC0084'
093070040429D0611X****************** SETON                                        6999
093080001020D0611C                   Eval      $InC1(69)   = *On
093090001020D0611C                   Eval      *In99       = *On
093100980515     C* Imposta differisce da (imponibile * aliquota)
093110980515     C                   WHEN      Q2MsgId = 'MVC0088'
093120040429D0611X****************** SETON                                        9599
093130001023D0611 * NDC0418-(F1) Imposta differisce da (imponibile * aliquota):
093140001023  "   *          F11 per forzatura
093150001023  "  C                   Eval      $InC1(95)   = *On
093160001020D0611C                   Eval      *In99       = *On
093170060613C1939 *
093180060613  "   * Imposta differisce da (imponibile * aliquota) + o - tolleranza
093190060613  "  C                   when      Q2MsgId = 'MVC0095'
093200060613  "   * NDC0944-(F1) Imposta oltre i limiti di (imponib.*aliq.
093210060613  "   *         + o - tolleranza): F11 per forzatura
093220060613  "  C                   movel     Q2MsgTxt      P1NDC0944
093230060613  "  C                   eval      $InC1(94) = *on
093240060613  "  C                   eval      *in99 = *on
093250060613C1939 *
093260980515     C* Imponibile obbligatorio su tutte le righe IVA
093270980515     C                   WHEN      Q2MsgId = 'MVC0089'
093280040429D0611X*L'Ind.96 è l'ind comune a tutte e tre le videate (D1/D2/S1)
093290040429  "  X*usato per l'errore PRO0013:Esistono errori sul altri schermi
093300040429D0611X****************** SETON                                        9699
093310001020D0611C                   Eval      $InC1(94)   = *On
093320001020D0611C                   Eval      *In99       = *On
093330000707     C*
093340950724     C                   OTHER
093350950724     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
093360950724     C                   MOVE      Q2MsgId       IdMsg
093370950724     C                   CALLB     'XRTVM'
093380950724     C                   PARM                    IDMSG            27
093390950724     C                   PARM                    TXTMSG          100
093400950724     C                   PARM                    ERRMSG            1
093410950905     C                   PARM                    *omit
093420950905     C                   PARM                    *omit
093430950905     C                   PARM      Q2msgtxt      Msgdta          100
093440950724     C                   IF        ErrMsg <> *on
093450950905     C                   CAT       TxtMsg:1      TextMsg
093460040429D0611X****************** SETON                                        5099
093470001023D0611 * CPF9898-&1
093480001023  "  C                   Eval      $InC1(50)   = *On
093490001020D0611C                   Eval      *In99       = *On
093500950724     C                   ENDIF
093510950724     C                   ENDSL
093520950724     C*
093530950724     C                   ENDSR
093540950616     C************************************************************
093550950616     C* GESTIONE F05 VIDEO D2
093560950616     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
093570950616     C* e il riposizionamento alla prima
093580950616     C************************************************************
093590950719     C     F05D2         BEGSR
093600950616     C*
093610950719     C                   MOVE      *ON           $INZD1
093620950719     C                   MOVE      'D1'          $GEST
093630950616     C*
093640950719     C                   ENDSR
093650950724     C************************************************************
093660950724     C* GESTIONE F02 VIDEO D2
093670950724     C************************************************************
093680950724     C     F02D2         BEGSR
093690950724     C*
093700040429D0228X**********************************************************************
093710040429  "  X* queste specifiche sono un errore concettuale:
093720040429  "  X*  NDMVC118 ha sì bisogno che sia valorizzata la testata
093730040429  "  X*  registrazione, però non aggiunge altri valori ai dati di
093740040429  "  X*  questa; in pratica tutto ciò che il modello poteva dare alla
093750040429  "  X*  testata l'ha già dato con la routine RepDatiMod.
093760040429  "  X* anzi, queste specifiche generano il problema che la registrazione
093770040429  "  X* intera viene scritta con lo schema del modello e i dati di una
093780040429  "  X* testata che non è ancora confermata: se dopo l'F02 cambio dei
093790040429  "  X* dati relativi alla contropartita, questo rcd non viene aggiornato
093800040429  "  X**********************************************************************
093810040429  "  X**se inputato un modello PN
093820040429  "  X**per copiare tutti i dati del modello (è Ndmvc118 a farlo!)
093830040429  "  X**ho bisogno prima di tutto della testata (£NdReg) della registr.
093840040429  "  X***                IF        D1TabPNot <> *blank
093850040429  "  X***                EVAL      SoloCtrl = *off
093860040429  "  X***                Else
093870040429  "  X**eseguo la sequenza dei controlli per la videata
093880040429  "  X**chiedendo di fare solo i controlli (senza aggiornare il rcd)
093890040429  "  X***                EVAL      SoloCtrl = *on
093900040429  "  X***                ENDIF
093910040429D0228X**********************************************************************
093920990923D0228C                   EVAL      SoloCtrl = *on
093930950724     C                   EXSR      CtrD2
093940950724     C*
093950950724     C                   ENDSR
093960950616     C************************************************************
093970950616     C* GESTIONE F24 VIDEO D2
093980950616     C************************************************************
093990950719     C     F24D2         BEGSR
094000950616     C*
094010950719     C                   MOVEA     $FX2          $FX
094020950719     C                   MOVEA     $FC2          $FC
094030950719     C                   MOVEA     $FR2          $FR
094040950719     C                   Z-ADD     $ULKD2        $ULK
094050950719     C                   EXSR      RIEKEY
094060950719     C                   Z-ADD     $ULK          $ULKD2            3 0
094070950719     C                   MOVEA     $FC           $FC2
094080950719     C                   MOVEL     $KEY1         Z2KE1
094090060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
094100950719     C                   MOVEL     $KEY2         Z2KE2
094110950719     C                   WRITE     FMTZ2
094120950719     C*
094130950719     C                   ENDSR
094140950904     C************************************************************
094150950904     C* GESTIONE F02 VIDEO S1
094160950904     C************************************************************
094170950904     C     F02S1         BEGSR
094180970204     C*
094190950904     C* eseguo la sequenza dei controlli per la videata
094200950904     C* chiedendo di fare solo i controlli (senza aggiornare il rcd)
094210970204     C                   EVAL      SoloCtrl = *on
094220970204     C* CONTROLLO DATI(escluso visualizzazione e annullamento)
094230970204     C                   IF        Opz015 <> '05' and
094240970204     C                             Opz015 <> '04'
094250970204     C                   EXSR      CTRC1
094260970204     C     *IN99         IFEQ      *OFF
094270970204     C                   EXSR      CTRS1
094280970204     C                   ENDIF
094290970204     C                   ENDIF
094300970204     C
094310950904     C                   ENDSR
094320950721     C************************************************************
094330950724     C* GESTIONE F05 VIDEO S1
094340950721     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
094350950721     C* e il riposizionamento alla prima
094360950721     C************************************************************
094370950724     C     F05S1         BEGSR
094380950721     C*
094390970204     C                   MOVE      *ON           $INZS1
094400950721     C*
094410950721     C                   ENDSR
094420001221C1456C************************************************************
094430001221  "  C* GESTIONE F20 VIDEO S1
094440001221  "  C************************************************************
094450001221C1456C     F20S1         BEGSR
094460001221     C*
094470001228     C* eseguo controlli subfile per "esplicitare" gli importi non digitati
094480010105     C                   Eval      SoloCtrl = *On
094490010105     C                   ExSr      CtrS1
094500001228     C*
094510001227     C                   CallB     'NDC515R'
094520001227     C                   Parm                    Soc001
094530001228     C                   Parm                    C1NRR
094540010109     C                   Parm                    Errore515
094550001221     C*
094560010109     C                   Select
094570010109     C* non è possibile aggiungere righe
094580010109     C                   When      Errore515 = '1'
094590010109     C                   Eval      $InC1(54) = *On
094600010109     C                   Eval      *In99 = *On
094610010109     C                   EndSl
094620010109     C*
094630001221C1456C                   ENDSR
094640950721     C************************************************************
094650950724     C* GESTIONE F24 VIDEO S1
094660950721     C************************************************************
094670950724     C     F24S1         BEGSR
094680950721     C*
094690950721     C                   MOVEA     $FX3          $FX
094700950721     C                   MOVEA     $FC3          $FC
094710950721     C                   MOVEA     $FR3          $FR
094720950724     C                   Z-ADD     $ULKS1        $ULK
094730950721     C                   EXSR      RIEKEY
094740950724     C                   Z-ADD     $ULK          $ULKS1            3 0
094750950721     C                   MOVEA     $FC           $FC3
094760950721     C                   MOVEL     $KEY1         Z3KE1
094770060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
094780950721     C                   MOVEL     $KEY2         Z3KE2
094790950721     C                   WRITE     FMTZ3
094800950721     C*
094810950721     C                   ENDSR
094820940224     C************************************************************
094830940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
094840940224     C************************************************************
094850940224     C     GESAGG        BEGSR
094860950809     C*
094870040308     C                   EVAL      GesRate015 = '0'
094880950921     C* se non sto gestendo un modello
094890950921     C                   IF        Ctb015 <> 'MD'             and
094900950921     C* e non ho effetti presentati
094910950921     C                             EffPres    =  *off
094920950914     C                   SELECT
094930950908     C* testo se sono variati quei campi che scatenano la generazione rate
094940950914     C                   WHEN      D1TpSogg   <> WTpSogg      or
094950950809     C                             D1Ksc      <> WKsc         or
094960950809     C                             D2ImportD  <> WImportD     or
094970950809     C                             D2Importo  <> WImporto     or
094980950809     C                             D2Divisa   <> WDivisa      or
094990041020D1672X* ex C1676                   D2DtLib    <> WDtLib       or
095000041020D1672 * nel 3.2 non esisteva il concetto di data decorrenza pagam.
095010041020  "   * e le rate erano calcolate autoamticamente dalla data doc,
095020041020  "   * quindi in una reg. nata sul 3.2, REGDtLib2 è vuota (=WDtLib)
095030041020  "   * e se non viene modificata la data stessa a video (D2DtLib),
095040041020  "   * non devono essere ricalcolate le rate
095050041020  "  C                             (D2DtLib <> WDtLib  and
095060041020D1672C                             WDtLib <> 0       )    or
095070960308     C                             D2CondPag  <> WCondPag     and
095080960308     c                             WcondPag   <> *blank       or
095090950914     C* se la gestione rate passa da manuale a automatica
095100950914     C* le rate devono essere rigenerate
095110950914     C                             D2PagMan   <> WPagMan      and
095120960627     C                             D2PagMan   =  XscOff       or
095130960627      * da verificare
095140960627      * caso di immissione con caricamento dati (prima nota batch)
095150960627     C                             Immexp015  =  *on  and
095160970210     C                             opz015     =  '01' and
095170970210     c                             Wpagman    =  XscOff
095180960627      *
095190040308     C                   EVAL      GesRate015 = '1'
095200960429     C* passo anche il conto precedente
095210960429     C                   EVAL      KscOld015 = WKsc
095220960429     C* siccome del capoconto precedente ho solo la sigla C o F, con
095230960429     C* XCapCliFor ne recupero il valore
095240960429     C                   movel     WTpSogg       KccOld015
095250960429     C                   CALLB     'XCAPCLIFOR'
095260960429     C                   PARM                    XSCsoc
095270960429     C                   PARM                    KccOld015
095280960429     C                   PARM                    KscOld015
095290950914     C* testo se sono variati quei campi che scatenano l'aggiornamento rate
095300950914     C                   WHEN      D2DtPar    <> WDtPar       or
095310950809     C                             D2NrPar    <> WNrPar       or
095320950914     C                             D2SerPar   <> WSerPar
095330040308     C                   EVAL      GesRate015 = '2'
095340950914     C* testo se sono in copia ed è la prima volta
095350960729     C                   WHEN      Opz015 = '03'     and
095360950914     C                             WCopia = *on
095370950914     C* richiedo la preparazione della schiera degli effetti da generare
095380950914     C* con tutte le rate presenti
095390040308     C                   EVAL      GesRate015 = '3'
095400950914     C                   ENDSL
095410950908     C                   ENDIF
095420950623     C*
095430950623     C* fine pgm
095440950623     C                   MOVE      *ON           $Fine
095450950824     C*
095460950727     C* segnalo al pgm chiamante
095470950727     C*
095480950727     C* se non siamo in visualizzazione
095490950727     C                   IF        Opz015 <> '05'
095500950727     C* l'avvenuta conferma
095510950727     C                   MOVE      *ON           OPR015
095520950727     C                   ENDIF
095530950802     C*
095540950616     C* il non aver premuto F3/F12
095550950719     C                   MOVE      '0'           RET015
095560950824     C*
095570950712     C* se non siamo in annullamento
095580040308     C                   IF        Opz015 <> '04'
095590950908     C*
095600950911     C* che voglio passare alle righe
095610950908     C                   MOVE      'RIGHE     '  Pgm015
095620950824     C*
095630950808     C* passo al pgm chiamante (e a quelli successivamente chiamati)
095640950808     C*
095650950808     C* alcuni dei default di causale
095660950808     C                   MOVE      TpSfl         TpSfl015
095670950808     C*
095680950808     C* Ripasso al pgm chiamante i campi modificati o scritti
095690960320     C                   eval      ksc015 = D2ksc
095700960320     C                   eval      SoggD015 = Descr44
095710950808     C                   eval      CliFor015 = D2TpSogg
095720950808     C                   eval      CausD015 = D2CausTesD
095730980113     C                   Movel     D2CondPagD    CondPaD015
095740951004     C                   eval      KccFact015 = WKccFact
095750951004     C                   eval      KscFact015 = WKscFact
095760950808     C                   eval      Kcc015 = $Kcc
095770960320     C     d2dtpar       ifne      0
095780960202     C                   CALLB     'XDT4000'
095790950914     C     DtPar015      PARM                    XDTAMG           10
095800960202     C                   PARM      D2DtPar       XDTGMA            6 0
095810950914     C                   PARM      4             XDTSTA            1 0
095820950808     C                   ELSE
095830950914     C                   move      Dt0           dtpar015
095840950808     C                   endif
095850960202     C                   eval      nrpar015 = D2nrpar
095860960202     C                   eval      serpar015 = D2serpar
095870950808     C                   eval      importo015 = Importo007
095880950808     C                   eval      importD015 = ImportD007
095890950808     C                   IF        TotImpIVA > 0
095900950808     C                   eval      imposta015 = TotImpIVA
095910950808     C                   eval      impostD015 = TotImpIVAD
095920950808     C                   ELSE
095930950808     C                   Z-SUB     TotImpIVA     Imposta015
095940950808     C                   Z-SUB     TotImpIVAD    ImpostD015
095950950808     C                   endif
095960950808     C                   EVAL      Segno015 = Segno007
095970950927     C                   MOVE      Fattura       Fattura015
095980951005     C                   MOVE      RCOPercipi    Percipi015
095990951011     C                   EVAL      Intra015 = OpeIntra
096000020415C1530C* la partita IVA serve sempre
096010020415  "  C                   IF        D1TpSogg = 'C'
096020020415  "  C                   EVAL      PartIVA015 = DVCPartIVA
096030020415  "  C                   ELSE
096040020415  "  C                   EVAL      PartIVA015 = DVFPartIVA
096050020415C1530C                   ENDIF
096060951011     C* se la causale prevede la gestione intracomunitaria
096070951011     C                   IF        OpeIntra = *on
096080100531C2162C                             or OpeAftAcq = *on
096090951011     C                   EVAL      TpReg015 = OpeTpReg
096100101103D2632C                   IF        RCOTpInd = *blank and
096110101103D2632C                             RCOCdInd = *blank
096120101103     C                   IF        D1TpSogg = 'C'
096130101103     C                   EVAL      Stato015 = DVCStato
096140040429C1530X***                EVAL      PartIVA015 = DVCPartIVA
096150101103     C                   ELSE
096160101103     C                   EVAL      Stato015 = DVFStato
096170040429C1530X***                EVAL      PartIVA015 = DVFPartIVA
096180101103     C                   ENDIF
096190101103D2632C                   ELSE
096200101103  "  C                   EVAL      Stato015 = IndStato
096210101103D2632C                   ENDIF
096220951011     C                   ENDIF
096230951011     C                   EVAL      AFtAcq015 = OpeAFtAcq
096240951113     C                   EVAL      AFtSta015 = OpeAFtSta
096250951013     C                   MOVE      OPECausAFt    CausAFt015
096260980123      *
096270980123      * Se primanota IVA forn.--> SAlVo serie se Numerazione SemiAutom.
096280980123     c                   If        Pgm004015 = 'NDC006R'
096290980123     c                   Exsr      SavSerSA
096300980123     c                   Endif
096310980123      *
096320951011     C*
096330950801     C* se siamo in annullamento
096340950801     C                   ELSE
096350950712     C* che voglio tornare al chiamante
096360950719     C                   MOVE      'FINE      '  Pgm015
096370040308     C                   ENDIF
096380070424c2015C* se richiesta diretta alla gestione rate, passo 'RATE'
096390070424  "  c                   if        VaiRate015=*on
096400070424  "  C                   MOVE      'RATE      '  Pgm015
096410070424c2015C                   ENDIF
096420950626     C*
096430940224     C                   ENDSR
096440980123     C************************************************************
096450980123     C* Salvo Serie se gestione Numerazione SemiAutomatica
096460980123     C************************************************************
096470980123     C     SavSerSA      BEGSR
096480980123     C*
096490980123     C* Serie registrazione reperita da causale
096500980123     C                   If        RegSerReg = OpeSer
096510980123     C*
096520980123     C*  reperimento dati serie
096530980123     c                   Eval      Serie     = RegSerReg
096540980123     C                   EXSR      RepDatG13
096550980123      *  Serie Non automatica
096560980123     C                   If        AUTG13 = 'N'
096570980123      *    memorizzo n° reg. + serie reg. per numerazione semiautomatica
096580980123     C                   Eval      SavSerReg =  RegSerReg
096590980123     C                   Eval      SavNrReg  =  RegNrReg
096600980123      *
096610980123      *  Serie Automatica --> no numeraz. semiautom.
096620980123     C                   Else
096630980123     C                   Eval      SavSerReg  = *Blank
096640980123     C                   Eval      SavNrReg   = *Zero
096650980123     C                   Endif
096660980123     C*
096670980123     C* Serie registr. non reperita da causale --> no numeraz. semiautom.
096680980123     c                   Else
096690980123     c                   Eval      SavSerReg  = *Blank
096700980123     c                   Eval      SavNrReg   = *Zero
096710980123     c                   Endif
096720980123     C*
096730980123     C     XGesNumSA     ENDSR
096740980123     C**---------------------------------------------------------
096750980123     C* Reperisce dati da tabella ANGG13
096760980123     C*----------------------------------------------------------
096770980123     C     RepDatG13     BEGSR
096780980123     C* Controllo Serie
096790980123     C                   Eval      XTbRic = '1'
096800980123     C                   Eval      XTbAzi = XScSoc
096810980123     C                   Eval      XTbKey = *zero
096820980123     C                   Eval      XTbCod = 'G13'
096830980123     C                   Move      Serie         XTbKey
096840980123     C                   CALLB     'XATB'
096850980123     C                   PARM                    XATbDS
096860980123     C                   If        XTbErr = '0'
096870980123     C                   Eval      ANGG13DS = XTbUni
096880980123     C                   Endif
096890980123     C*
096900980123     C     XRepDatG13    ENDSR
096910950719     C************************************************************
096920950719     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
096930950719     C************************************************************
096940950719     C     CTRKD1        BEGSR
096950950719     C*
096960950719     C                   SETOFF                                       99
096970950719     C                   MOVEA     $FC1          $FC
096980950719     C                   EXSR      CTRKEY
096990950719     C     *IN99         IFEQ      *ON
097000040429D0611X****************** SETON                                        97
097010001023D0611 * CPD6A60-Tasto funzionale non consentito.
097020001023D0611C                   Eval      $InD1(97)   = *On
097030950719     C                   ENDIF
097040950719     C*
097050950719     C                   ENDSR
097060950719     C************************************************************
097070950719     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
097080950719     C************************************************************
097090950719     C     CTRKD2        BEGSR
097100950719     C*
097110950719     C                   SETOFF                                       99
097120950719     C                   MOVEA     $FC2          $FC
097130950719     C                   EXSR      CTRKEY
097140950719     C     *IN99         IFEQ      *ON
097150040429D0611X****************** SETON                                        97
097160001023D0611 * CPD6A60-Tasto funzionale non consentito.
097170001020D0611C                   Eval      $InD2(97)   = *On
097180950719     C                   ENDIF
097190950719     C*
097200950719     C                   ENDSR
097210950721     C************************************************************
097220950724     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
097230950721     C************************************************************
097240950724     C     CTRKS1        BEGSR
097250950721     C*
097260950721     C                   SETOFF                                       99
097270950721     C                   MOVEA     $FC3          $FC
097280950721     C                   EXSR      CTRKEY
097290950721     C     *IN99         IFEQ      *ON
097300040429D0611X****************** SETON                                        97
097310001023D0611 * CPD6A60-Tasto funzionale non consentito.
097320001020D0611C                   Eval      $InC1(97)   = *On
097330950721     C                   ENDIF
097340950721     C*
097350950721     C                   ENDSR
097360941205     C************************************************************
097370941205     C* CONTROLLO ABILITAZIONE TASTI
097380941205     C************************************************************
097390941205     C     CTRKEY        BEGSR
097400941205     C*
097410941205     C                   SELECT
097420941205     C     $TASTO        WHENEQ    F01
097430960212     C                   z-add     01            posfun
097440941205     C     $FC(1)        IFEQ      '0'
097450941205     C                   SETON                                        99
097460941205     C                   ENDIF
097470941205     C     $TASTO        WHENEQ    F02
097480960212     C                   z-add     02            posfun
097490941205     C     $FC(2)        IFEQ      '0'
097500941205     C                   SETON                                        99
097510941205     C                   ENDIF
097520941205     C     $TASTO        WHENEQ    F03
097530960212     C                   z-add     03            posfun
097540941205     C     $FC(3)        IFEQ      '0'
097550941205     C                   SETON                                        99
097560941205     C                   ENDIF
097570941205     C     $TASTO        WHENEQ    F04
097580960212     C                   z-add     04            posfun
097590941205     C     $FC(4)        IFEQ      '0'
097600941205     C                   SETON                                        99
097610941205     C                   ENDIF
097620941205     C     $TASTO        WHENEQ    F05
097630960212     C                   z-add     05            posfun
097640941205     C     $FC(5)        IFEQ      '0'
097650941205     C                   SETON                                        99
097660941205     C                   ENDIF
097670941205     C     $TASTO        WHENEQ    F06
097680960212     C                   z-add     06            posfun
097690941205     C     $FC(6)        IFEQ      '0'
097700941205     C                   SETON                                        99
097710941205     C                   ENDIF
097720941205     C     $TASTO        WHENEQ    F07
097730960212     C                   z-add     07            posfun
097740941205     C     $FC(7)        IFEQ      '0'
097750941205     C                   SETON                                        99
097760941205     C                   ENDIF
097770941205     C     $TASTO        WHENEQ    F08
097780960212     C                   z-add     08            posfun
097790941205     C     $FC(8)        IFEQ      '0'
097800941205     C                   SETON                                        99
097810941205     C                   ENDIF
097820941205     C     $TASTO        WHENEQ    F09
097830960212     C                   z-add     09            posfun
097840941205     C     $FC(09)       IFEQ      '0'
097850941205     C                   SETON                                        99
097860941205     C                   ENDIF
097870941205     C     $TASTO        WHENEQ    F10
097880960212     C                   z-add     10            posfun
097890941205     C     $FC(10)       IFEQ      '0'
097900941205     C                   SETON                                        99
097910941205     C                   ENDIF
097920941205     C     $TASTO        WHENEQ    F11
097930960212     C                   z-add     11            posfun
097940941205     C     $FC(11)       IFEQ      '0'
097950941205     C                   SETON                                        99
097960941205     C                   ENDIF
097970941205     C     $TASTO        WHENEQ    F12
097980960212     C                   z-add     12            posfun
097990941205     C     $FC(12)       IFEQ      '0'
098000941205     C                   SETON                                        99
098010941205     C                   ENDIF
098020941205     C     $TASTO        WHENEQ    F13
098030960212     C                   z-add     13            posfun
098040941205     C     $FC(13)       IFEQ      '0'
098050941205     C                   SETON                                        99
098060941205     C                   ENDIF
098070941205     C     $TASTO        WHENEQ    F14
098080960212     C                   z-add     14            posfun
098090941205     C     $FC(14)       IFEQ      '0'
098100941205     C                   SETON                                        99
098110941205     C                   ENDIF
098120941205     C     $TASTO        WHENEQ    F15
098130960212     C                   z-add     15            posfun
098140941205     C     $FC(15)       IFEQ      '0'
098150941205     C                   SETON                                        99
098160941205     C                   ENDIF
098170941205     C     $TASTO        WHENEQ    F16
098180960212     C                   z-add     16            posfun
098190941205     C     $FC(16)       IFEQ      '0'
098200941205     C                   SETON                                        99
098210941205     C                   ENDIF
098220941205     C     $TASTO        WHENEQ    F17
098230960212     C                   z-add     17            posfun
098240941205     C     $FC(17)       IFEQ      '0'
098250941205     C                   SETON                                        99
098260941205     C                   ENDIF
098270941205     C     $TASTO        WHENEQ    F18
098280960212     C                   z-add     18            posfun
098290941205     C     $FC(18)       IFEQ      '0'
098300941205     C                   SETON                                        99
098310941205     C                   ENDIF
098320941205     C     $TASTO        WHENEQ    F19
098330960212     C                   z-add     19            posfun
098340941205     C     $FC(19)       IFEQ      '0'
098350941205     C                   SETON                                        99
098360941205     C                   ENDIF
098370941205     C     $TASTO        WHENEQ    F20
098380960212     C                   z-add     20            posfun
098390941205     C     $FC(20)       IFEQ      '0'
098400941205     C                   SETON                                        99
098410941205     C                   ENDIF
098420941205     C     $TASTO        WHENEQ    F21
098430960212     C                   z-add     21            posfun
098440941205     C     $FC(21)       IFEQ      '0'
098450941205     C                   SETON                                        99
098460941205     C                   ENDIF
098470941205     C     $TASTO        WHENEQ    F22
098480960212     C                   z-add     22            posfun
098490941205     C     $FC(22)       IFEQ      '0'
098500941205     C                   SETON                                        99
098510941205     C                   ENDIF
098520941205     C     $TASTO        WHENEQ    F23
098530960212     C                   z-add     23            posfun
098540941205     C     $FC(23)       IFEQ      '0'
098550941205     C                   SETON                                        99
098560941205     C                   ENDIF
098570941205     C     $TASTO        WHENEQ    F24
098580960212     C                   z-add     24            posfun
098590941205     C     $FC(24)       IFEQ      '0'
098600941205     C                   SETON                                        99
098610941205     C                   ENDIF
098620941205     C                   ENDSL
098630941205     C*
098640941205     C                   ENDSR
098650940131     C************************************************************
098660940131     C* OPERAZIONI INIZIALI
098670940131     C************************************************************
098680940131     C     *INZSR        BEGSR
098690940131     C*
098700940127     C* Reperimento parametri
098710940127     C*
098720940117     C     *ENTRY        PLIST
098730940117     C                   PARM                    KPJBA
098740950807     C                   PARM                    NDC015DS
098750960806     C                   PARM                    DFTOutPut
098760080916D2369C                   PARM                    Ndc019ds1
098770940506     C*
098780940506     C* Reperimento tasti di funzione
098790940506     C*
098800940506     C                   EXSR      REPKEY
098810950616     C*
098820960212     C* Reperimento dati società
098830960212     C*
098840960212     C                   MOVEL     'SOC001'      TIPXSC
098850960212     C                   MOVEL     *BLANK        SOCXSC
098860960212     C                   EXSR      REPSOC
098870960212     C     RTNXSC        IFNE      '1'
098880960212     C                   MOVEL     XSOCDS        SOC001
098890960212     C                   ELSE
098900960212     C                   RETURN
098910960212     C                   ENDIF
098920040113c1716C*
098930040113  "  C* Reperimento parametro modulo IVA
098940040113  "  C                   MOVEL     *BLANK        XPARIC
098950040113  "  C                   MOVEL     XSCSOC        XPASOC
098960040113  "  C                   MOVEL     'IVA      '   XPAPAR
098970040113  "  C                   MOVEL     *BLANK        XPAOUT
098980040113  "  C                   EXSR      REPPAR
098990040113  "  C     XPAERR        IFEQ      '0'
099000040113  "  C                   MOVEL     XPAOUT        ANP045DS
099010040113C1716C                   ENDIF
099020040113     C*
099030960212     C*
099040960212     C* Chiamata a routine per il reperimento delle opzioni e funzioni
099050960212     C* personalizzate per S1
099060960212     C                   CAllb     'X46USREXT'
099070960212     C                   PARM                    *omit
099080960212     C                   PARM                    *omit
099090960212     C                   PARM                    *omit
099100960212     C                   PARM                    *omit
099110960212     C                   PARM                    *omit
099120960212     C                   PARM                    $FX3
099130960212     C                   PARM                    $FC3
099140960212     C                   PARM                    $PGF3
099150960212     C                   PARM                    DsPgm
099160960212     C                   PARM                    xsccut
099170960212     C                   PARM                    xscgrp
099180960212     C*
099190960212     C* Richiamo routine per aggiunta tasti funzione personalizzati
099200960212     C* Video D1
099210960212     C                   CAllb     'X46USREXT'
099220960212     C                   PARM                    *omit
099230960212     C                   PARM                    *omit
099240960212     C                   PARM                    *omit
099250960212     C                   PARM                    *omit
099260960212     C                   PARM                    *omit
099270960212     C                   PARM                    $FX1
099280960212     C                   PARM                    $FC1
099290960212     C                   PARM                    $PGF1
099300960212     C                   PARM                    DsPgm
099310960212     C                   PARM                    xsccut
099320960212     C                   PARM                    xscgrp
099330960212     C*
099340960212     C* Richiamo routine per aggiunta tasti funzione personalizzati
099350960212     C* Video D2
099360960212     C                   CAllb     'X46USREXT'
099370960212     C                   PARM                    *omit
099380960212     C                   PARM                    *omit
099390960212     C                   PARM                    *omit
099400960212     C                   PARM                    *omit
099410960212     C                   PARM                    *omit
099420960212     C                   PARM                    $FX2
099430960212     C                   PARM                    $FC2
099440960212     C                   PARM                    $PGF2
099450960212     C                   PARM                    DsPgm
099460960212     C                   PARM                    xsccut
099470960212     C                   PARM                    xscgrp
099480960212     C*
099490940223     C                   ENDSR
099500940506     C************************************************************
099510940506     C* REPERIMENTO TASTI DI FUNZIONE
099520940506     C************************************************************
099530940506     C     REPKEY        BEGSR
099540940506     C                   MOVEA     $FM1          $FM
099550950913     C                   MOVEA     $FC1          $FC
099560940506     C                   EXSR      REPKKK
099570940506     C                   MOVEA     $FX           $FX1
099580940614     C*
099590950913     C                   MOVEA     $FC2          $FC
099600950719     C                   MOVEA     $FM2          $FM
099610950719     C                   EXSR      REPKKK
099620950719     C                   MOVEA     $FX           $FX2
099630950721     C*
099640950913     C                   MOVEA     $FC3          $FC
099650950721     C                   MOVEA     $FM3          $FM
099660950721     C                   EXSR      REPKKK
099670950721     C                   MOVEA     $FX           $FX3
099680940614     C                   ENDSR
099690940506     C************************************************************
099700940506     C* REPERIMENTO TASTI DI FUNZIONE
099710940506     C************************************************************
099720940506     C     REPKKK        BEGSR
099730940506     C*
099740940506     C                   CLEAR                   $FX
099750940506     C     1             DO        24            X
099760940506     C*
099770940506     C     $FM(X)        IFNE      *BLANK
099780950913     C     $FC(X)        ANDNE     '0'
099790940506     C*
099800940506     C                   MOVEL     $FM(X)        IDMSG
099810950522     C                   CALLB     'XRTVM'
099820940506     C                   PARM                    IDMSG            27
099830940506     C                   PARM                    TXTMSG          100
099840940506     C                   PARM                    ERRMSG            1
099850940506     C*
099860940506     C     ERRMSG        IFNE      *ON
099870940506     C                   MOVEL     TXTMSG        $FX(X)
099880940506     C                   ELSE
099890940506     C                   MOVEL     *ALL' '       $FX(X)
099900941027     C                   ENDIF
099910940506     C*
099920941027     C                   ENDIF
099930940506     C*
099940941027     C                   ENDDO
099950940506     C*
099960940506     C                   ENDSR
099970940223     C************************************************************
099980940223     C* INIZIALIZZAZIONE VARIABILI
099990940223     C************************************************************
100000940223     C     INZVAR        BEGSR
100010941201     C*
100020941201     C* Reperimento dati società
100030941201     C*
100040950616     C                   MOVEL     'SOC001'      TIPXSC
100050941115     C                   MOVEL     *BLANK        SOCXSC
100060941115     C                   EXSR      REPSOC
100070941115     C     RTNXSC        IFNE      '1'
100080941115     C                   MOVEL     XSOCDS        SOC001
100090941115     C                   ELSE
100100941115     C                   RETURN
100110941115     C                   ENDIF
100120090126     c*
100130090126C2116c* prima della defopz, vedo se fattura presente in elenco document
100140090126  "  c* per emettere tasto visualizzazione elenco (F16)
100150090126  "  c                   eval      Ed2SysCon=Regsys
100160090126  "  c                   eval      Ed2NrAsCon=RegNrAsReg
100170090126  "  c     K02ed202      chain     NdEd202L
100180090126  "  c                   if        %found(NdEd202L)
100190090126  "  c                   eval      Ed1Societa=xscsoc
100200090126  "  c                   eval      Ed1Progre =ed2Progret
100210090126  "  c     K02ed101      chain     Nded101L
100220090126c2116c                   endif
100230940715     C*
100240940715     C* Definizione dei tasti funzione utilizzabili a pgm
100250940715     C*
100260940715     C                   EXSR      DEFOPZ
100270940128     C*
100280940223     C* Variabili per gestione videate
100290940128     C*
100300940223     C                   MOVE      'D1'          $GEST
100310940223     C                   MOVE      *OFF          $FINE
100320940223     C                   MOVE      *ON           $INZD1
100330950719     C                   MOVE      *ON           $INZD2
100340940224     C                   MOVE      *BLANK        $LASTG
100350940224     C                   MOVE      *BLANK        $LASTV
100360940506     C                   Z-ADD     0             $ULKD1            3 0
100370950726     C                   Z-ADD     0             $ULKD2            3 0
100380950726     C                   Z-ADD     0             $ULKs1            3 0
100390940127     C*
100400940223     C* Variabili appoggio
100410940127     C*
100420940224     C                   Z-ADD     0             CURR
100430940224     C                   Z-ADD     0             CURC
100440950626     C                   MOVE      *blank        TpSfl
100450950810     C                   CLEAR                   WPagMan
100460950810     C                   CLEAR                   WTpSogg
100470950809     C                   CLEAR                   WKsc
100480950809     C                   CLEAR                   WImportD
100490950809     C                   CLEAR                   WImporto
100500950809     C                   CLEAR                   WDivisa
100510950809     C                   CLEAR                   WCondPag
100520950920     C                   movel     dt0           WDtDoc
100530950920     C                   movel     dt0           WDtPar
100540950809     C                   CLEAR                   WNrPar
100550950809     C                   CLEAR                   WSerPar
100560950914     C                   EVAL      WCopia = *off
100570950911     C                   MOVE      *off          EffPres
100580981209     C                   Move      *Off          $WindowMod
100590990714D0154C                   Move      *Off          $MsgWarn
100600990722C1207C                   Move      *Off          $MsgParEs
100610991012C1251C                   Move      *Off          $ExfmtD2
100620000330C1350C                   Move      *Off          $EffNoCol
100630000912D0660C                   Move      *Blank        $StopWinP
100640060619C1937C                   Move      *Off          $MsgDocEs
100650940207     C*
100660940207     C* Valorizzazione campi univoci testate
100670940207     C*
100680940207     C                   CLEAR                   FMTT1
100690940207     C                   MOVEL     KNSIF         NOMSIF
100700941110     C                   MOVEL     XSCDSI        NOMDIT
100710941027     C                   MOVEL     DSPGM         NOMPGM
100720950331     C* recupero il sottotitolo (se c'è)
100730950331     C                   MOVEL     *BLANK        T1TIT
100740950331     C     SOTTIT        IFNE      *BLANK
100750950331     C                   MOVEL     SOTTIT        IDMSG
100760950522     C                   CALLB     'XRTVM'
100770950331     C                   PARM                    IDMSG            27
100780950331     C                   PARM                    TXTMSG          100
100790950331     C                   PARM                    ERRMSG            1
100800950331     C                   PARM      *ON           CTRMSG            1
100810950331     C                   PARM      30            LENMSG            3 0
100820950331     C     ERRMSG        IFNE      *ON
100830950331     C                   MOVEL     TXTMSG        T1TIT
100840950331     C                   ELSE
100850950331     C                   MOVEL     *ALL'?'       T1TIT
100860950331     C                   ENDIF
100870950331     C                   ENDIF
100880950331     C* recupero il tipo operazione
100890950331     C                   Z-ADD     1             X
100900950719     C     OPZ015        LOOKUP    $OZ(X)                                 21
100910950331     C     *IN21         IFEQ      *ON
100920950331     C                   MOVEL     $ID(X)        IDMSG
100930950522     C                   CALLB     'XRTVM'
100940950331     C                   PARM                    IDMSG            27
100950950331     C                   PARM                    TXTMSG          100
100960950331     C                   PARM                    ERRMSG            1
100970950331     C                   PARM      *ON           CTRMSG            1
100980950331     C                   PARM      30            LENMSG            3 0
100990950331     C     ERRMSG        IFNE      *ON
101000950331     C                   MOVEL     TXTMSG        T1OPE
101010950331     C                   ELSE
101020950331     C                   MOVEL     *ALL'?'       T1OPE
101030950331     C                   ENDIF
101040950331     C                   ENDIF
101050950921     C*
101060950921     C* pulisco i campi di NDPPA perchè in immissione e copia rimarrebbero
101070950921     C* sporchi dall'ultimo rcd reperito
101080950922     C                   RESET                   NDPPA
101090951010     C*
101100951010     C* se sono in copia
101110960729     C                   IF        Opz015 = '03'
101120951010     C* la prima volta attivo il flag che mi richiederà la generazione rate
101130951010     C                   EVAL      WCopia     =  *on
101140951010     C                   ENDIF
101150950809     C*
101160950809     C* Inizializzazione/Decodifica prima videata
101170950809     C*
101180950809     C                   EXSR      INZD1
101190940224     C                   MOVE      *OFF          $INZD1
101200950719     C*
101210001023D0611 * Gestione priorità emissione video:
101220001023  "   * l'emissione diretta sul D2 avviene solo se Modifica
101230001023  "   * e se non esistono errori sul D1, in tutti gli altri casi
101240001023  "   * (Immissione/Copia/Imm.speciale/Mod con errrori in D1)
101250001023  "   * la prima emissione viene fatta sul D1.
101260001023  "   * P.S:
101270001023  "   * Se immissione speciale (Es. da PNBatch)
101280001023  "   * ImmExp015='1' e Opz015='01'
101290001023D0611 *
101300950807     C* se non immissione o copia
101310960729     C                   if        opz015 <> '01' and opz015 <> '03'
101320960527     C* se non ci sono errori
101330960527     C                   IF        *in99 = *off
101340950807     C* si passa direttamente alla 2a videata
101350950807     C                   EVAL      $Gest = 'D2'
101360950807     C                   MOVE      *ON           $INZD2
101370040429D0611X**se ci sono errori
101380040429  "  X****************** ELSE
101390040429  "  X**vado nella videata errata
101400040429  "  X****************** IF        VideoErr = 'D1'
101410040429  "  X****************** EVAL      $Gest = 'D1'
101420040429  "  X****************** ENDIF
101430040429  "  X****************** IF        VideoErr = 'D2'
101440040429  "  X****************** EVAL      $Gest = 'D2'
101450040429D0611X****************** ENDIF
101460960527     C                   endif
101470960527     C                   ENDIF
101480940127     C*
101490940117     C                   ENDSR
101500950706     C************************************************************
101510950706     C* INIZIALIZZAZIONE VARIABILI
101520950706     C* (effetto mini per ritorno da pgm chiamato)
101530950706     C************************************************************
101540950807     C     MiniInzVar    BEGSR
101550950706     C*
101560950706     C* Variabili per gestione videate
101570950706     C*
101580950706     C                   MOVE      *OFF          $FINE
101590950908     C*
101600950908     C* dato che le rate si generano appera usciti dalla testata,
101610950908     C* memorizzo quei campi che, se variati, scatenano la rigenerazione,
101620950908     C* com'erano all'ultima uscita da pgm, per evitare che venga richiesta
101630950908     C* una operazione già fatta
101640950908     C                   MOVE      D2PagMan      WPagMan
101650950908     C                   MOVE      D1TpSogg      WTpSogg
101660950908     C                   MOVE      D1Ksc         WKsc
101670950908     C                   Z-ADD     D2ImportD     WImportD
101680950908     C                   Z-ADD     D2Importo     WImporto
101690950908     C                   MOVE      D2Divisa      WDivisa
101700950908     C                   MOVE      D2CondPag     WCondPag
101710950908     C                   MOVE      D2DtDoc       WDtDoc
101720040429C1547X**ex C1494         MOVE      D2DtLib       WDtLib
101730030711C1676C                   MOVE      D2DtLib       WDtLib
101740950908     C                   MOVE      D2DtPar       WDtPar
101750950908     C                   MOVE      D2NrPar       WNrPar
101760950908     C                   MOVE      D2SerPar      WSerPar
101770950914     C                   MOVE      *off          WCopia
101780990716D0154C                   Move      *Off          $MsgWarn
101790991012C1251C                   Move      *Off          $ExfmtD2
101800991013D0265C* reimposto il campone dei flag della registrazione col dato
101810991013  "  C* salvato su NDREG, per memorizzare se ci sono stati dei
101820991013  "  C* cambiamenti durante l'emissione della lista movimenti
101830991013D0265C                   EVAL      WFlgReg = REGFlgReg
101840000912D0660C                   Move      *Blank        $StopWinP
101850950719     C*
101860950719     C*  Se non immissione o copia si passa direttamente alla 2a videata
101870950719     C*
101880960729     C                   if        opz015 <> '01' and opz015 <> '03'
101890950719     C                   eval      $Gest = 'D2'
101900950719     C                   endif
101910950706     C*
101920950706     C                   ENDSR
101930941209     C************************************************************
101940941209     C* REPERIMENTO DATI SOCIETA'
101950941209     C************************************************************
101960941209     C     REPSOC        BEGSR
101970941209     C*
101980950522     C                   CALLB     'XSOC'
101990941209     C                   PARM                    TIPXSC            6
102000941209     C                   PARM                    SOCXSC            3
102010941209     C                   PARM                    CDSXSC            9 0
102020941209     C                   PARM                    MODXSC            3
102030941209     C                   PARM      *BLANKS       RTNXSC            1
102040941209     C                   PARM                    XSOCDS
102050941209     C                   PARM                    KPJBA
102060941209     C*
102070941209     C                   ENDSR
102080040113c1716C************************************************************
102090040113     C* REPERIMENTO PARAMETRI GESTIONE
102100040113c1716C************************************************************
102110040113     C     REPPAR        BEGSR
102120040113     C*
102130040113     C                   CALLB     'XPAR'
102140040113     C                   PARM                    XPASOC            3
102150040113     C                   PARM                    XPAPAR            8
102160040113     C                   PARM                    XPAOUT
102170040113     C                   PARM                    XPAERR            1
102180040113     C                   PARM                    XPARIC            1
102190040113     C*
102200040113c1716C                   ENDSR
102210090126     C*
102220940715     C************************************************************
102230940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
102240940715     C************************************************************
102250940715     C     DEFOPZ        BEGSR
102260940715     C*
102270940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
102280940715     C* con S o N a seconda della utilizzabilità o meno
102290940715     C* del tasto funzione correlato
102300940715     C*
102310940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
102320940715     C* di compilazione
102330940715     C                   RESET                   $FC1
102340940715     C                   RESET                   $FR1
102350950726     C                   RESET                   $FC2
102360950726     C                   RESET                   $FR2
102370950726     C                   RESET                   $FC3
102380950726     C                   RESET                   $FR3
102390950727     C*
102400950727     C* Azzerato sempre e ripristinato in annullamento
102410950727     C* F6=Conferma
102420950727     C                   Z-ADD     6             X
102430950727     C                   MOVE      '0'           $FC1(X)
102440950727     C                   MOVE      '0'           $FC2(X)
102450950727     C                   MOVE      '0'           $FC3(X)
102460940715     C*
102470940715     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
102480940715     C* invalido alcune opzioni
102490940715     C                   SELECT
102500950818     C* Se pgm richiamato in immissione disattivo :
102510950818     C     OPZ015        WHENEQ    '01'
102520950818     C* F8=Selezione seguente
102530950818     C                   Z-ADD     8             X
102540950818     C                   MOVE      '0'           $FC1(X)
102550950818     C                   MOVE      '0'           $FC2(X)
102560950818     C                   MOVE      '0'           $FC3(X)
102570940715     C* Se pgm richiamato in interrogazione disattivo :
102580950719     C     OPZ015        WHENEQ    '05'
102590950726     C* F2=Decodifica
102600950726     C                   Z-ADD     2             X
102610950726     C                   MOVE      '0'           $FC2(X)
102620940715     C* F4=Ricerca
102630940715     C                   Z-ADD     4             X
102640940720     C                   MOVE      '0'           $FC1(X)
102650950726     C                   MOVE      '0'           $FC2(X)
102660940715     C* F5=Ripristino
102670940715     C                   Z-ADD     5             X
102680940720     C                   MOVE      '0'           $FC1(X)
102690950726     C                   MOVE      '0'           $FC2(X)
102700950726     C                   MOVE      '0'           $FC3(X)
102710940715     C* Se pgm richiamato in annullamento disattivo :
102720950719     C     OPZ015        WHENEQ    '04'
102730950727     C* F2=Decodifica
102740950727     C                   Z-ADD     2             X
102750950727     C                   MOVE      '0'           $FC2(X)
102760940715     C* F4=Ricerca
102770940715     C                   Z-ADD     4             X
102780940720     C                   MOVE      '0'           $FC1(X)
102790950726     C                   MOVE      '0'           $FC2(X)
102800940715     C* F5=Ripristino
102810940715     C                   Z-ADD     5             X
102820940720     C                   MOVE      '0'           $FC1(X)
102830950726     C                   MOVE      '0'           $FC2(X)
102840950726     C                   MOVE      '0'           $FC3(X)
102850950727     C* F6=Conferma
102860950727     C                   Z-ADD     6             X
102870950727     C                   MOVE      '1'           $FC1(X)
102880950727     C                   MOVE      '1'           $FC2(X)
102890950727     C                   MOVE      '1'           $FC3(X)
102900940715     C*
102910940715     C                   OTHER
102920940715     C*
102930940715     C                   ENDSL
102940090126     c*
102950090126C2116C* Se non presenti elenco documenti o se presenti ma di classe '1'
102960090126  "  c* disattivo tasto F16=elenchi documenti
102970090126  "  C                   Z-ADD     16            X
102980090126  "  C                   if        not %found(nded202L)
102990090126  "  C                   MOVE      '0'           $FC2(X)
103000090126  "  c                   else
103010090126  "  C                   if        not %found(nded101L)
103020090126  "  C                   MOVE      '0'           $FC2(X)
103030090126  "  c                   else
103040090126  "  c                   if        ed1cladoc='1'
103050090126  "  C                   MOVE      '0'           $FC2(X)
103060090126  "  c                   z-add     0             $ulkd2
103070090126  "  c                   endif
103080090126  "  c                   endif
103090090126C2116c                   endif
103100940715     C*
103110940715     C                   ENDSR
103120950403     C************************************************************
103130950403     C* REPERIMENTO CAMPI DI DEFAULT
103140950403     C************************************************************
103150950403     C     REPDEF        BEGSR
103160950403     C*
103170950616     C*
103180950403     C                   ENDSR
103190940128     C************************************************************
103200950606     C* DEFINIZIONE KLIST
103210940223     C************************************************************
103220950606     C     DefKlist      BEGSR
103230940223     C*
103240940223     C* klist
103250960517     C*
103260950621     C* ANDFT01L
103270950621     C     K03DFT01      KLIST
103280950621     C                   KFLD                    DFTSocieta
103290950621     C                   KFLD                    DFTCdDft
103300950621     C                   KFLD                    DFTUtente
103310950621     C     K02DFT01      KLIST
103320950621     C                   KFLD                    DFTSocieta
103330950621     C                   KFLD                    DFTCdDft
103340950719     C* NDPAS01L
103350950719     C     K03PAS01      KLIST
103360950719     C                   KFLD                    PASSys
103370950719     C                   KFLD                    PASNrasReg
103380950719     C                   KFLD                    PASNrRigam
103390040430D1602 * NDPAS04L
103400040430  "  C     K07PAS04      klist
103410040430  "  C                   kfld                    PASSocieta
103420040430  "  C                   kfld                    PASCtb
103430040430  "  C                   kfld                    PASKcc
103440040430  "  C                   kfld                    PASKsc
103450040430  "  C                   kfld                    PASDtPar
103460040430  "  C                   kfld                    PASNrPar
103470040430  "  C                   kfld                    PASSerPar
103480040430D1602 *
103490950720     C* ANUNI01L
103500950720     C     K02UNI01      KLIST
103510950720     C                   KFLD                    UniSocieta
103520950720     C                   KFLD                    Uniunita
103530950726     C* NDPPA01L
103540950905     c     K07PPA01      KLIST
103550950726     C                   KFLD                    PPASocieta
103560950905     C                   KFLD                    PPACtb
103570950726     C                   KFLD                    PPAKcc
103580950726     C                   KFLD                    PPAKsc
103590950726     C                   KFLD                    PPADtPar
103600950726     C                   KFLD                    PPANrPar
103610950726     C                   KFLD                    PPASerPar
103620001025C1429 * NDPPS01L
103630001025  "  c     K02PPS01      KLIST
103640001025  "  C                   KFLD                    PPSSys
103650001025C1429C                   KFLD                    PPSNrAsInt
103660020327C1550C* ANSLI01L
103670020327  "  C     K03SLI01      KLIST
103680020327  "  C                   KFLD                    SLISocieta
103690020327  "  C                   KFLD                    SLITpReg
103700020327C1550C                   KFLD                    SLILibro
103710021002C1585 * ANALR02L
103720021002  "  C     K05ALR02      klist
103730021002  "  C                   kfld                    ALRSocieta
103740021002  "  C                   kfld                    ALRTpReg
103750021002  "  C                   kfld                    ALRLibro
103760021002  "  C                   kfld                    ALRAliq
103770021002C1585C                   kfld                    ALRRifIva
103780060620c1937C* NDMOV08L
103790060619  "  C     K06MOV08      KList
103800060619  "  C                   KFld                    MOVSocieta
103810060619  "  C                   KFld                    MOVCtb
103820060619  "  C                   KFld                    MOVKcc
103830060619  "  C                   KFld                    MOVKsc
103840060619  "  C                   KFld                    MOVDtDoc
103850060619  "  C                   KFld                    MOVNrDoc
103860060619  "  C* NDMOV19L
103870060619  "  C     K06MOV19      KList
103880060619  "  C                   KFld                    MOVSocieta
103890060619  "  C                   KFld                    MOVCtb
103900060619  "  C                   KFld                    MOVKcc
103910060619  "  C                   KFld                    MOVKsc
103920060619  "  C                   KFld                    MOVAnnoDoc
103930060619C1937C                   KFld                    MOVNrDoc
103940970203     C*
103950970203     C     PNMR          PLIST
103960970203     C                   PARM                    XNMRIC            1
103970970203     C                   PARM                    XNMSOC            3
103980970203     C                   PARM                    XNMUNI            8
103990970203     C                   PARM                    XNMCTB            2
104000970203     C                   PARM                    XNMKEY            8
104010970203     C                   PARM                    XNMSER            4
104020970203     C                   PARM                    XNMCMT            1
104030970203     C                   PARM                    XNMDAT           10
104040970203     C                   PARM                    XNMIVA            1
104050970203     C                   PARM                    XNMREG            1
104060970203     C                   PARM                    XNMERR            1
104070970203     C                   PARM                    XNMNUM            9 0
104080970203     C                   PARM                    XNMAUT            1
104090000912D0660 *
104100000912  "   * PListper la chiamata a XDatR
104110000912  "  C     PListXDatR    PList
104120000912  "  C                   Parm                    XDtRic
104130000912  "  C                   Parm                    XDtSoc
104140000912  "  C                   Parm                    XDtUni
104150000912  "  C                   Parm                    XDtCtb
104160000912  "  C                   Parm                    XDtKey
104170000912  "  C                   Parm                    XDtAlc
104180000912  "  C                   Parm                    XDtCmt
104190000912  "  C                   Parm                    XDtDat
104200000912  "  C                   Parm                    XDtTip
104210000912  "  C                   Parm                    XDtErr
104220000912  "  C                   Parm                    XDtTst
104230000912D0660 *
104240970520     C*
104250970520     C* ANCDP01L
104260970520     C     K02CDP01      KLIST
104270970520     C                   KFLD                    CdpSocieta
104280970520     C                   KFLD                    CdpCondPag
104290991012C1251C* ANCDR01L
104300991011  "  C     K02CDR01      KLIST
104310991011  "  C                   KFLD                    CDRSocieta
104320991011  "  C                   KFLD                    CDRCondPag
104330991011  "   * NDMRA08L
104340991011  "  C     K02MRA08      KLIST
104350991011  "  C                   KFLD                    MRASysF
104360991012C1251C                   KFLD                    MRANAsRegF
104370090126     c*
104380090126c2116C* NDed202L
104390090126  "  C     K02Ed202      KLIST
104400090126  "  C                   KFLD                    ed2SysCon
104410090126  "  C                   KFLD                    Ed2NrasCon
104420090126  "  C* NDed101L
104430090126  "  C     K02Ed101      KLIST
104440090126  "  C                   KFLD                    ed1Societa
104450090126C2116C                   KFLD                    Ed1Progre
104460940127     C*
104470940117     C                   ENDSR
104480940509     C************************************************************
104490940509     C* RIEMPIMENTO TASTI FUNZIONE
104500940509     C************************************************************
104510950726     C
104520950726     C     RIEKEY        BEGSR
104530940509     C*
104540940509     C* Input :
104550940509     C* - $FX = schiera con tutti i testi disponibili
104560940509     C* - $FC = schiere con i flag di validità associati ai testi
104570940509     C* - $FR = schiere con i flag di REVERSE IMAGE
104580940509     C* - $ULK = ultimo testo caricato
104590940722     C* - $L01 = Lunghezza prima riga
104600940722     C* - $L02 = Lunghezza seconda riga
104610940509     C* Output :
104620940509     C* - $ULK
104630940609     C* - $KEY1= 1° riga di tasti funzione
104640940609     C* - $KEY2= 2° riga di tasti funzione
104650940509     C*
104660950522     C                   CALLB     'XKEYMSG'
104670940509     C* input
104680940509     C                   PARM                    $FX
104690940509     C                   PARM                    $FC
104700940509     C                   PARM                    $FR
104710940509     C                   PARM                    $ULK              3 0
104720940509     C* output
104730940509     C                   PARM                    $KEY1            79
104740940509     C                   PARM                    $KEY2            79
104750950616     C* input
104760950616     C                   PARM      79            $L01              3 0
104770950616     C                   PARM      79            $L02              3 0
104780940509     C*
104790060224C1913C                   PARM                    $ALLFUNCT       480
104800060224C1913C*
104810940509     C                   ENDSR
104820941214**
10483095040401PROMSG    *LIBL     COS0001
10484095061602PROMSG    *LIBL     COS0002
10485095040403PROMSG    *LIBL     COS0003
10486095040404PROMSG    *LIBL     COS0004
10487095040405PROMSG    *LIBL     COS0005
104880940509** TASTI DI FUNZIONE  D1
104890950721
104900950616PROMSG    *LIBL     KEY0002  02
104910940509PROMSG    *LIBL     KEY0003  03
104920940509PROMSG    *LIBL     KEY0004  04
104930940509PROMSG    *LIBL     KEY0005  05
104940950727PROMSG    *LIBL     KEY0006  06
104950950721
104960940509PROMSG    *LIBL     KEY0008  08
104970950721
104980950721
104990950721
105000940509PROMSG    *LIBL     KEY0012  12
105010950721
105020950721
105030950721
105040950721
105050950721
105060950721
105070950721
105080950721
105090950721
105100950721
105110950721
105120940509PROMSG    *LIBL     KEY0024  24
105130940509**  ABILITAZIONE D1
1051409407200N  01
1051509506231N  02
1051609407201N  03
1051709407201N  04
1051809407201N  05
1051909509131N  06
1052009407200N  07
1052109407201N  08
1052209407200N  09
1052309411160N  10
1052409407200N  11
1052509407201N  12
1052609407200N  13
1052709407200N  14
1052809407200N  15
1052909407200N  16
1053009407200N  17
1053109407200N  18
1053209407200N  19
1053309407200N  20
1053409407200N  21
1053509407200N  22
1053609507260N  23   Lasciare sempre impostato a '0' il valore della riga 23
1053709507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
105380950719** TASTI DI FUNZIONE  D2
105390950721
105400950719PROMSG    *LIBL     KEY0002  02
105410950719PROMSG    *LIBL     KEY0003  03
105420950719PROMSG    *LIBL     KEY0004  04
105430950719PROMSG    *LIBL     KEY0005  05
105440950727PROMSG    *LIBL     KEY0006  06
105450950721
105460950719PROMSG    *LIBL     KEY0008  08
105470950721
105480950721
105490950721
105500950719PROMSG    *LIBL     KEY0012  12
105510951025
105520950721
105530951025PROMSG    *LIBL     KEY0053  15
105540090126PROMSG    *LIBL     KEY0347  16                            C2116
105550950721
105560950721
105570950721
105580950721
105590950721
105600950721
105610950721
105620950719PROMSG    *LIBL     KEY0024  24
105630950721**  ABILITAZIONE D2
1056409507190N  01
1056509507191N  02
1056609507191N  03
1056709507191N  04
1056809507191N  05
1056909509131N  06
1057009507190N  07
1057109507191N  08
1057209507190N  09
1057309507190N  10
1057409507190N  11
1057509507191N  12
1057609510250N  13
1057709507190N  14
1057809510251N  15
1057900901261N  16                                                     C2116
1058009507190N  17
1058109507190N  18
1058209507190N  19
1058309507190N  20
1058409507190N  21
1058509507190N  22
1058609507260N  23   Lasciare sempre impostato a '0' il valore della riga 23
1058709507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
105880950724** TASTI DI FUNZIONE  S1
105890950721
105900950721PROMSG    *LIBL     KEY0002  02
105910950721PROMSG    *LIBL     KEY0003  03
105920950721PROMSG    *LIBL     KEY0004  04
105930950721PROMSG    *LIBL     KEY0005  05
105940950727PROMSG    *LIBL     KEY0006  06
105950950721
105960950721PROMSG    *LIBL     KEY0008  08
105970021002PROMSG    *LIBL     KEY0009  09                                                            **C1585**
105980950721
105990971205
106000950721PROMSG    *LIBL     KEY0012  12
106010950721
106020950721
106030950721
106040950721
106050950721
106060950721
106070950721
106080001221PROMSG    *LIBL     KEY0044  20
106090950721
106100950721
106110950721
106120950721PROMSG    *LIBL     KEY0024  24
106130950724**  ABILITAZIONE S1
1061409507210N  01
1061509509041N  02
1061609507211N  03
1061709508171N  04
1061809507211N  05
1061909509131N  06
1062009507210N  07
1062109507211N  08
1062200210021N  09                                                                                     **C1585**
1062309507210N  10
1062409507210N  11
1062509507211N  12
1062609507210N  13
1062709507210N  14
1062809507210N  15
1062909507210N  16
1063009507210N  17
1063109507210N  18
1063209507210N  19
1063300012211N  20
1063409507210N  21
1063509507210N  22
1063609507260N  23   Lasciare sempre impostato a '0' il valore della riga 23
1063709507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
106380990208** MsgId
106390991011PROMSG    *LIBL     COS1221    ( 1)Il modello primanota &1 è collegato al conto in esame;
106400991011PROMSG    *LIBL     COS1222    ( 2)lo si vuole utilizzare (1) oppure no (2) ?
106410991011PROMSG    *LIBL     COS1223    ( 3)Utilizzare     Non utilizzare
106420991011PROMSG    *LIBL     COS0464    ( 4)AVVERTIMENTO: i default di causale impostati a vi
106430991011PROMSG    *LIBL     COS0465    ( 5)              non cambieranno nonostante sia vari
106440991011PROMSG    *LIBL     COS0490    ( 6)Vuoi rivalorizzare i campi già impostati a video,
106450991011PROMSG    *LIBL     COS0491    ( 7)con i default del nuovo conto?
106460991011PROMSG    *LIBL     COS0495    ( 8)Errata quadratura Iva in m.c.:
106470000913PROMSG    *LIBL     COS0496    ( 9)Tot doc.: &1 Righe Iva: &2 Diff.: &3                    **D0660**
106480000913PROMSG    *LIBL     COS0497    (10)Allineare in automatico il tot. documento in m.c. tornan**D0660**
106490991011PROMSG    *LIBL     NDC0530    (11)Data liquidazione XX/XX/XX differisce data registrazione XX/XX/XX
106500991011PROMSG    *LIBL     COS1023    (12)Si desidera continuare comunque l'elaborazione (1)
106510991011PROMSG    *LIBL     COS1043    (13)oppure ritornare alla videata (2) ?
106520000410PROMSG    *LIBL     COS1025    (14)Continua
106530991011PROMSG    *LIBL     COS1026    (15)Ritorno
106540991012PROMSG    *LIBL     NDC0531    (16)La partita esiste già e la causale non richiede controlli
106550991011PROMSG    *LIBL     COS0434    (17)Vuoi continuare?
106560991012PROMSG    *LIBL     COS0525    (18)ATTENZIONE: si stanno creando effetti passivi per un per**C1251**
106570991012PROMSG    *LIBL     COS0526    (19)questa modalità non à gestita per le ritenute d'acconto **C1251**
106580010621PROMSG    *LIBL     COS0566    (20)Esistono effetti collegati alla partita ma non generati **C1350**
106590000410PROMSG    *LIBL     COS0582    (21)registrazione. Si consiglia di controllare la partita   **C1350**
106600000410PROMSG    *LIBL     COS0581    (22)e comunque si andrà in gestione rate manuali            **C1350**
106610000919PROMSG    *LIBL     NDC0602    (23)Competenza plafond &1 non superiore al datario &2       **D0660**
