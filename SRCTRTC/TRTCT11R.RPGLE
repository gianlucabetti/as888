000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600110502????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFTMIN vers D01B  *
000700110503      *                         cliente     AEG TTI      con  IFTMIN vers D01B  *
000800060612      **?************************************************************************
000900991124      * Il pgm crea:                                                            *
001000020919      *             EDivab3L file spedizioni da confermare per camion           *
001100020919      *             FiVAB01L file spedizioni da confermare                      *
001200060609      **?************************************************************************
001300090213     Ftivin00r  uF   E             DISK    usropn  INFDS(VINRECDS)
001400991124      *
001500991206     FFLNUF01L  UF   E           K DISK
001600090213     FEDRDE01L  UF A E           K DISK    commit
001700050112     FTABEL00F  IF   E           K DISK
001800090209     FCNACO00F  IF   E           K DISK
001900090209     FCNIND00F  IF   E           K DISK
002000090210     FEDTAB01L  IF   E           K DISK
002100090210      *
002200091019???? Fedstbl01l IF   E           K DISK
002300090209      *
002400090213     FEDiVAB1L  UF A E           K DISK    commit
002500090213     FEDiVAD0F  O  A E           K DISK    commit
002600090213     FEDiVAT0F  O  A E           K DISK    commit
002700090209      *
002800090213     FEDSUM00F  O  A E           K DISK    commit   INFDS(SRECDS)
002900940321      *----------------------------------------------------*
003000090209      * numero relativo di record
003100090209     D SRECDS          DS
003200090209     D  NRREC                397    400B 0
003300090209      *
003400090213      * numero relativo di record
003500090213     D VINRECDS        DS
003600090213     D  VNREC                397    400B 0
003700090213      *
003800090205      **  Elenco DS di tutti i Segmenti da tradurre
003900091019???? D edS00BGM      E DS
004000091019???? D edS00CNT      E DS
004100091019???? D edS00COM      E DS
004200091019???? D edS00CTA      E DS
004300091019???? D edS00DTM      E DS
004400091019???? D edS00GID      E DS
004500091019???? D edS00MEA      E DS
004600091019???? D edS00NAD      E DS
004700091019???? D edS00PCI      E DS
004800091019???? D  D30_1                  4    353    DIM(10)
004900091019???? D edS00RFF      E DS
005000091019???? D edS00TDT      E DS
005100091019???? D edS00TOD      E DS
005200091019???? D edS00MOA      E DS
005300091019???? D edS00FTX      E DS
005400091019???? D  D05                   16    365    DIM(5)
005500091019???? D edS00TSR      E DS
005600091019???? D edS00UNA      E DS
005700091019???? D edS00UNB      E DS
005800091019???? D edS00UNH      E DS
005900091019???? D edS00UNT      E DS
006000091019???? D edS00UNZ      E DS
006100090210      **
006200090210     D  X30            S             35    DIM(20)                              generico segnacolli
006300090205      **
006400090209     D Valori_inErr    ds
006500090209     D  SKout_Errori                  1    DIM(50)
006600090209     D Descr_Errore    ds
006700090209     D  SKout_DesErr                 50    DIM(50)
006800090209      *
006900090205      *----------------------------------------------------*
007000091016     D EoFTivin        s              1
007100091016      *----------------------------------------------------*
007200091016      *
007300090205     D Tipo_seg        S              3
007400100720     D Trovato_seg     S              1
007500100716      *
007600100716     d keyUNBCLI       s             35
007700100716     d keyTIPOMSG      s              6
007800100716     d keyVERSION      s              3
007900100716     d keyRELEASE      s              3
008000100716     d keyAGENCY       s              3
008100100716     d keyASSOCIA      s              6
008200100716      *
008300100720     D DsGenerica      s           2048
008400090209     D segmento        s           2048
008500090209     D Errore          s              1
008600000223     D W0140           S             14  0
008700991129     D WORA            S              6  0
008800991129     D XX              S              3  0 INZ
008900110328     d Key_Generica35  s             35
009000991129     D WDTGIO          S              8  0
009100991129     D DATEU           S              8  0
009200991129     D DATA_eur        S               D   DATFMT(*eur)
009300050112     D NUM_Sped        s                   LIKE(vabnsp)
009400090213     D NUM_RECord      s                   LIKE(vnREC)
009500090213     D SAVNUM_RECord   s                   LIKE(vnREC)
009600090213     D Last_RECord     s                   LIKE(vnREC)
009700050112     D svkpjbu         s                   like(kpjbu)
009800091019???? D Dettaglio_NAD_destinatario...
009900091019???? D                 s              1
010000091019???? D NAD_destinatario_in_testata...
010100091019???? D                 s              1
010200091019???? D NAD_mittente_in_testata...
010300091019???? D                 s              1
010400091019???? D Inizio_Dettaglio...
010500091019???? D                 s              1
010600100120???? D seg_imprevisto...
010700100120???? D                 s              1
010800091019???? D Riferimento_Spedizione...
010900110523???? D                 s              3    INZ('AGE')
011000110502???? D Riferimento_Numerico...
011100110502???? D                 s              3    INZ('ON ')
011200091019???? D Segmento_Inizio_Dettaglio...
011300091019???? D                 s              3    INZ('BGM')
011400090209      *---------------------------------------------------------------------*
011500090209     D KPJBA         E DS
011600090209     D FNLV55DS      E DS
011700090209     D TIBS55DS      E DS
011800090209     D TISI95DS      E DS
011900090209     D UTEDSE0F      E DS
012000090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012100090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012200090209      * Ds scomposzione tipo capoconti
012300090209     D TCUDS           DS
012400090209     D  F34                    3      4
012500090209     D  F56                    5      6
012600090209     D DS1B          E DS
012700090209     D DS15          E DS
012800090209     D DSCV          E DS
012900090213     D TRTC86DS      E DS
013000090209     D EDIDSPT       E DS
013100090209     D EDIDSPU       E DS
013200090209     D EDIDSPV       E DS
013300090211     D EDIDSCL       E DS
013400090216     D EDIDSTC       E DS
013500090209      **
013600090209      *  DS x salvataggio dati impostati in EDiVAB
013700090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
013800090209     D SALVA           DS           545
013900090210      **
014000090209     D WLBDA8          DS
014100090209     D  G02DAT                 1      8  0
014200090209     D  G02INV                 9     16  0
014300090209     D  G02ERR                17     17
014400090209     D  G02TGI                18     22  0
014500090209      *  DS x scomposizione numero spedizione
014600090209     D DSSPE           DS
014700090209     D  SPEAAS                 1      4  0
014800090209     D  SPELNP                 5      7  0
014900090209     D  SPENRS                 8      9  0
015000090209     D  SPENSP                10     16  0
015100090209      *  DS x scomposizione segnacollo
015200090209     D DSSGN           DS
015300090209     D  SGNLNP                 1      3  0
015400090209     D  SGNLNA                 4      6  0
015500090209     D  SGNNRS                 7      8  0
015600090209     D  SGNNSC                 9     15  0
015700090209     D  SGNZNC                16     17  0
015800090209      *  DS x stampa segnacollo
015900090209      *---------------                                                      *
016000090211     D  vablnp_PT      s                   LIKE(vablnp)
016100090212     D  vabnrs_PT      s                   LIKE(vabnrs)
016200090211     D  vabctm_PT      s                   LIKE(vabctm)
016300090209     D Wvabtic         s                   LIKE(vabtic)
016400090603     d tes_VABrmo      s                   LIKE(VABrmo)
016500090603     d tes_VABnmo      s                   LIKE(VABnmo)
016600090603     d tes_VABcmo      s                   LIKE(VABcmo)
016700090209     C*
016800090603     d tes_VABrsd      s                   LIKE(VABrsd)
016900090603     d tes_VABrd2      s                   LIKE(VABrd2)
017000090603     d tes_VABind      s                   LIKE(VABind)
017100090603     d tes_VABlod      s                   LIKE(VABlod)
017200090603     d tes_VABnzd      s                   LIKE(VABnzd)
017300090603     C*
017400090603     d tes_Valuta      s                   LIKE(VABvca)
017500090603     C*
017600090209      *---------------------------------------------------------------------*
017700090209      * Schiere
017800090209      *---------------------------------------------------------------------*
017900090209      * Schiera per reperimento nr.seganacollo
018000090209     D SGN             S              7  0 DIM(5000)
018100090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
018200090209      * Stringhe per conversione alfanumerico/numerico N.Documento
018300090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
018400090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
018500090209      * Nr. documento alfanumerico da decodificare
018600090209     D NDA             S              1    DIM(35)
018700090209      * Nr. documento alfanumerico da già decodificato
018800090209     D NDN             S              1    DIM(15)
018900090209      * Messaggi di errore
019000090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
019100090209      * Tabella codici trattamento merce (1B)
019200090209     D CTM             S              2    DIM(200)
019300090209     D DTM             S             67    DIM(200)
019400090209      * Tabella codici valuta
019500090209     D CCV             S              3    DIM(200)
019600090209     D DCV             S             89    DIM(200)
019700090209      * Tabella codici nazioni
019800090209     D C15             S              3    DIM(500)
019900090209     D ISO             S              3    DIM(500)
020000090209     D CPF             S              2  0 DIM(500)
020100090209      * Tabella partner
020200121105     D CPT             S             35    DIM(200)
020300121105     D CPTparz         S             35    DIM(%elem(CPT))
020400121105     D KSC_UNB         S              7    DIM(%elem(CPT))
020500121105     D DPT             S             90    DIM(%elem(CPT))
020600121105     D DPU             S             90    DIM(%elem(CPT))
020700121105     D DPV             S             90    DIM(%elem(CPT))
020800121105     D TRUL0SDS      E DS
020900090209      *
021000121105     d Ptn_parziale    s              1    inz('N')
021100121105     d sav_CPTx        s              3  0 inz(0)
021200090209      * Tabella CL --> codici clienti - tariffa
021300130305     D CCL             S             35    DIM(999)
021400130305     D DCL             S             90    DIM(999)
021500090209      * Schiere x caricamento cod.cliente  <>
021600130305     D KCL             S              7  0 DIM(999)
021700090209      * Termini di consegna
021800110328     D K35_TpBolla     S             35    DIM(100)
021900110328     D Cod_TpBolla     S              3    DIM(100)
022000110328     D Decod_Porto     S              1    DIM(100)
022100110328     d Trovata_Tab_TB  S              1
022200110328      *
022300090216      * Tipo pagamento C/Assegno
022400090216     D CTC35           S             35    DIM(100)
022500090216     D TPI             S              2    DIM(100)
022600090209      * Decodifiche servizi speciali
022700090209     D SS35            S             35    DIM(100)
022800090209     D SS              S              3    DIM(100)
022900090209     D DSS             S             90    DIM(100)
023000090209      * Schiere x routine di conversione CAP
023100090209     D CAP5            S              1    DIM(5)
023200090209     D CAP9            S              1    DIM(9)
023300090209     D CAPM            S              1    DIM(9)
023400090209      * Schiera per memorizzazione FTX ricevuti
023500090209     D QUA             S              3    DIM(100)
023600090209     D FTX             S             70    DIM(100)
023700090209      * Tabelle capiconto
023800090209      * Schiere x località
023900090209     D LOD             S              1    DIM(35)
024000060608      **?------------------------------------------------------------------ */
024100090601     C*? Ds Scrittura del VAT "E"
024200060608      **?------------------------------------------------------------------ */
024300060612     D XTOEBCDDS     E DS
024400060620      *
024500060608      **?------------------------------------------------------------------ */
024600050112      * Numeratore Bolle (302)
024700050112     D trul33ds      E DS
024800050112     D Ds3C          E DS
024900090209     C*
025000090209      **?------------------------------------------------------------------ */
025100090209     C* Definizione campi di work
025200090209     D pVLB            s                   LIKE(VABVLB)
025300090209     D kKUT            s                   LIKE(TBLKUT)
025400090209     D kCOD            s                   LIKE(TBLCOD)
025500090209     D kKEY            s                   LIKE(TBLKEY)
025600090209     D kKCC            s                   LIKE(ACOKCC)
025700090209     D kKSC            s                   LIKE(ACOKSC)
025800090209     D kAAA            s                   LIKE(NUFAAA)
025900090209     D kCNU            s                   LIKE(NUFCNU)
026000090209     D kFIL            s                   LIKE(NUFFIL)
026100090209     D kAAS            s                   LIKE(VABAAS)
026200090209     D kLNP            s                   LIKE(VABLNP)
026300090209     D kNRS            s                   LIKE(VABNRS)
026400090209     D kNSP            s                   LIKE(VABNSP)
026500090209     D kCMR            s                   LIKE(VABCMR)
026600090209     D kCCM            s                   LIKE(VABCCM)
026700090209     D kNCD            s                   LIKE(VADNCD)
026800090210     D kCNT            s                   LIKE(VADCNT)
026900090209     D kNMC            s                   LIKE(RDENMC)
027000090209     D kNRP            s                   LIKE(RDENRP)
027100090209     D kLNP1           s                   LIKE(RDELNP)
027200090209      *
027300090209      *  Invio E-mail
027400090209     D Indirizzi       s            253
027500090209     D Oggetto         s             44
027600090209     D Messaggio       s           5000
027700090209      *
027800090209     d   DSmail        ds
027900090209     D Mail_Ind                      44
028000090209     d   IND_char                     1    dim(44)
028100090209      *
028200090209     D Lun_Ind         s              5u 0
028300090209      *
028400090209     D Dac             s              5u 0
028500090209     D Luc             s              5u 0
028600090209      *
028700090209     C*---------------------------------------------------------------*
028800090209     D MsgDSP          S            256                                         Testo generico
028900090209     C*---------------------------------------------------------------*
029000090209     D SndMg01         S             10                                         Message type
029100090209     D                                     INZ('*INFO')
029200090209     D SndMg02         S             10                                         Deluvery mode
029300090209     D                                     INZ('*BREAK')
029400090209     D SndMg03         S            256                                         Message text
029500090209     D SndMg04         S             10I 0                                      Length of text
029600090209     D                                     INZ(%SIZE(SndMg03))
029700090209     D SndMg05         S             10                                         User profile
029800090209     D                                     DIM(299)
029900090209     D SndMg06         S             10I 0                                      Number of user
030000090209     D SndMg07         S             10I 0                                      Message sent indic.
030100090209     D SndMg08         S             10I 0                                      Function requested
030200090209     D SndMg10         S              1                                         Show display
030300090209     D                                     INZ('N')
030400090209     D SndMg11         S             20                                         Qualified MSGQ name
030500090209     D SndMg12         S              4                                         Name type indicator
030600090209     D                                     INZ('*USR')
030700090209      * indice di scihera
030800090209     D Jx              S              5I 0
030900110527     D Position        S              5I 0
031000110527     D daPos           S              5I 0
031100090209      *
031200090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031300090209      *
031400090209     d bart_it         C                   CONST('@Bartolini.it;')
031500090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031600060612      * ?================================================================== */
031700060612      *
031800060612      * ?   * Campi x decodifica * (INPUT  del Record)
031900090130     D  Dati           s           2048
032000060612      * ?   *--------------------------------------------------------------*
032100060612     D  se_errore      s              1    inz(' ')
032200060612      * ?* ------------------------------------------------------ *
032300060710     D Digits          C                   '0123456789'
032400091019     D*------------------
032500091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
032600091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
032700110527     D*------------------
032800110527     D allChars        c                   const('QWERTYUIOPASDFGHJKLZXCVBNMqwe-  caratteri
032900110527     D                                     rtyuiopasdfghjklzxcvbnm0123456789/*--  accettati
033000110527     D                                     +\|!"£$%&/()=''?ì+*èéòçà°ù§-.:_,;<>[-
033100110527     D                                     ]@ ')
033200060612      * ?================================================================== */
033300060612      *   Ciclo principale
033400090205      * ?================================================================== */
033500060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
033600060612     C*
033700060612     C                   if        not %open(tivin00r)
033800060612     C                   open      tivin00r
033900060612     C                   endif
034000060612      **
034100060612      * ?------------------------------------------------------------------ */
034200060612     C*? Controllo dati arrivati da DPD
034300060612      * ?------------------------------------------------------------------ */
034400060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
034500060612      * ?- controllando che la trasmissione sia completa.
034600090205      **
034700060612      * ?              /*---------------------- */
034800090205     c                   goto      nonFare_adesso
034900060612     c                   exsr      check_Trasm
035000090205     c     nonFare_adessotag
035100060612      * ?              /*---------------------- */
035200090205      **
035300060613      **  Errore di trasmissione x tutti i records
035400060613      **   --> file in errore
035500060613     c                   if        se_errore = 'S'
035600060612
035700060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
035800090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
035900090205     c                   exsr      Msg_errato
036000060612      **
036100060612     c                   else
036200060614      **
036300060612      * ?------------------------------------------------------------------ */
036400060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
036500060612      * ?------------------------------------------------------------------ */
036600090205      **
036700060612      * ?              /*---------------------- */
036800060612     c                   exsr      Importa_Msg
036900060612      * ?              /*---------------------- */
037000060612
037100060614     c                   end
037200060614
037300060612     C                   if        %open(tivin00r)
037400060612     C                   close     tivin00r
037500060612     C                   endif
037600060612      *
037700060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
037800090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
037900090225     c                             esito ='0'
038000060710     C                   eval      esito ='0'
038100090225      *
038200090225     c                   COMMIT
038300090224      *
038400090213     c                   if        almeno_un_due ='S'
038500090213     C                   eval      esito ='1'
038600060614     C                   endif
038700090213     C                   endif
038800060614      *
038900060612     c                   SETON                                        LR
039000060612      * ?================================================================== */
039100060612      *? Importa i records della tramsissione
039200060612      * ?------------------------------------------------------------------ */
039300060612     c     Importa_Msg   Begsr
039400060612
039500090211     C                   EXSR      AZZMSG
039600060614
039700060612     c     *start        setll     Tivin00r
039800060612     c                   read      Tivin00r
039900091016     c                   if        %Eof(tivin00r)
040000091016     c                   move      'S'           EoFTivin
040100091016     c                   end
040200060612
040300060612     c                   dow       not %eof(Tivin00r)
040400060614
040500060614      * solo i record sflaggati da rielaborare
040600060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
040700090211      *  Sempre Record OK
040800090211      ** Controlli formali sui campi
040900090211     C                   eval      vinFLG = '1'
041000060612     c                   clear                   se_errore
041100060612
041200060612      ** Decodifica record a campi variabili
041300060612      * ?              /*---------------------- */
041400090205     c                   exsr      Decod_Segmento
041500090209      * ?              /*---------------------- */
041600090209
041700090213      *** ------------------------------------
041800091019????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT" x IFTMIN
041900090213      *** ------------------------------------
042000090213      * ?              /*---------------------- */
042100090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
042200090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
042300090213      * ?              /*---------------------- */
042400090601      ***
042500091019????  **
042600091019????  *   OGNI fine DETTAGLIO è determinato dall'UNT.
042700091019???? c                   if           tipo_seg = 'UNT'
042800091019????  *
042900090213      *  Deve flaggare il dettaglio con dei problemi
043000090213     C                   if        Err_in_detta = 'S'
043100090213      **?              /*---------------------- */
043200090213     c                   exsr      DETta_Errato
043300090213      **?              /*---------------------- */
043400090213     c                   end
043500090213      *
043600060612      **  Se presente un errore nel record emette una segnalazione msg
043700091019???? c                   if        se_errore = 'S'
043800091019???? c                   else
043900091019????  ******
044000090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
044100090227     c                   if         se_trovato_NAD ='S'
044200060612      * ?              /*---------------------- */
044300090209     c                   exsr      Scrive_VAB
044400060612      * ?              /*---------------------- */
044500090227     c                   else
044600090227     c                   eval      errori_in_Wrt = 'S'
044700090227     c                   end
044800090227      *
044900090209      *  Problemi nella decodifica dei campi VAB/VAT
045000090213     c                   if        errori_in_Wrt = 'S'
045100090213      *
045200090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
045300090213     c                   exsr      Error_DET
045400090213      *
045500090213     c                   ROLBK
045600090213      *
045700090213     c                   else
045800090601      *
045900090213     c                   COMMIT
046000090601      *
046100090227      * Dopo aver confermato, con COMMIT
046200090227      *  si prepara per una nuova spedizione
046300090227      * ?                         --------------
046400090227     c                   exsr      Nuova_spediz
046500090227      * ?                         --------------
046600090213     c                   end
046700090209
046800090209     c                   end
046900090210      **
047000060620     c                   end
047100060612
047200060621      *  x errore bloccante nella composizione del VAB/VAT
047300060621     c                   if        err_bloccante ='S'
047400060621     C                   eval      vinFLG = '2'
047500090211     c                   eval      esito  = '2'
047600110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG '
047700090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047800090603     C                             Bolle import in filiale - messaggio -
047900090603     C                             con UNB non presente in tabella PT controlla-
048000090603     C                             re EDI ricevuto su UPLOAD.'
048100090603     c                   exsr      invio_mail_AB
048200060621     c                   end
048300060612
048400091016     c                   if        EoFTivin = ' '
048500060612     c                   update    Tivin000
048600091016     c                   end
048700090211
048800090211      *** se non è riconosciuto l'UNB deve uscire direttamente
048900090211     c                   if        se_errore ='S'  and
049000090211     c                             tipo_seg = 'UNB'
049100090213     c                   eval      err_bloccante = 'S'
049200090211     c                   exsr      Msg_errato
049300090211     c                   LEAVE
049400090211     c                   endIF
049500090211      ***
049600060614     c                   endIF
049700060612
049800060612     c                   read      Tivin00r
049900091016     c                   if        %Eof(tivin00r)
050000091016     c                   move      'S'           EoFTivin
050100091016     c                   end
050200060612     C                   ENDdo
050300060612      **
050400060612     c                   endSR
050500100120      * ?------------------------------------------------------------------ */
050600100120      *? Controlla la trasmissione   se completa
050700100120      * ?------------------------------------------------------------------ */
050800100120     c     Check_Trasm   Begsr
050900100120
051000100120     C                   clear                   se_errore
051100100120     C                   clear                   Riga_iNIZIo       1
051200100120     C                   clear                   Riga_fINe         1
051300100120      ** primo  record
051400100120     c     *start        setll     tivin00r
051500100120      *
051600100120     c     rileggi       tag
051700100120     c                   read      tivin00r
051800100120     c                   if        %Eof(tivin00r)
051900100120     c                   move      'S'           EoFTivin
052000100120     c                   end
052100100120      *
052200100120     c                   if        not %eof(tivin00r)
052300110527      *  Caratteri non previsti
052400110527     c                   z-add     1             dapos
052500110527     c     cerca         tag
052600110527     c     allChars      checK     VINdta:daPos  position
052700110527     C                   if        %Found
052800110527     c                   eval      %subst(VinDTA:position:1)=' '
052900110527     c                   eval      dapos = Position +1
053000110527     c                   goto      cerca
053100110527     c                   endIf
053200110527      *
053300100120     c                   clear                   dati
053400100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
053500100120     c                   if        dati = *blank
053600100120      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
053700100120      * le leggo e le escludo.
053800100120     C                   eval      vinFLG = '1'
053900100120     c                   if        EoFTivin = ' '
054000100120     c                   update    Tivin000
054100100120     c                   end
054200100120     c                   goto      rileggi
054300100120     c                   end
054400100120      * ?              /*---------------------- */
054500100120     c                   exsr      Decod_Segmento
054600100120      * ?              /*---------------------- */
054700100120     c                   endif
054800100120      **
054900100120      ** ultimo record
055000100120     c     *hival        setll     tivin00r
055100100120     c     leggiAncora   tag
055200100120     c                   readp     tivin00r
055300100120     c                   if        %Eof(tivin00r)
055400100120     c                   move      'S'           EoFTivin
055500100120     c                   end
055600100120     c                   if        not %eof(tivin00r)
055700100120      *
055800100120     c***********        movel(p)  VINDTA        dati
055900100120     c                   clear                   dati
056000100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
056100100120     c                   if        dati = *blank
056200100120      * le ultime righe potrebbero essere vuote prima di finire il messaggio
056300100120      * le leggo e le escludo.
056400100120     C                   eval      vinFLG = '1'
056500100120     c                   if        EoFTivin = ' '
056600100120     c                   update    Tivin000
056700100120     c                   end
056800100120     c                   goto      leggiAncora
056900100120     c                   end
057000100120      * ?              /*---------------------- */
057100100120     c                   exsr      Decod_Segmento
057200100120      * ?              /*---------------------- */
057300100120     c                   endif
057400100120
057500100120      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
057600100120      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
057700100120      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
057800100120     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
057900100120      * ?-----> Errore
058000100120     C                   eval      se_errore = 'S'
058100100120     c                   end
058200100120
058300100120     c                   endSR
058400100120      * ?------------------------------------------------------------------ */
058500100120      *?    Decodifica il segmento ed importa i campi in formato DS
058600100120      * ?------------------------------------------------------------------ */
058700100120     c     Decod_SegmentoBegsr
058800100120      **
058900110527      *  Caratteri non previsti
059000110527     c                   z-add     1             dapos
059100110527     c     $erca         tag
059200110527     c     allChars      checK     VINdta:daPos  position
059300110527     C                   if        %Found
059400110527     c                   eval      %subst(VinDTA:position:1)=' '
059500110527     c                   eval      dapos = Position +1
059600110527     c                   goto      $erca
059700110527     c                   endIf
059800110527      *
059900100120     c                   clear                   dati
060000100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
060100100120     c                   eval      tipo_seg = %subst(dati:1:3)
060200100120     c                   eval      segmento = dati
060300100120      *
060400100120      * Imposta il flag di Inizio Dettaglio
060500100120     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
060600100120     c                   eval      Inizio_Dettaglio ='S'
060700100120     c                   end
060800110503      *
060900100120     c                   clear                   seg_imprevisto
061000100720     c                   clear                   trovato_seg
061100100716      *
061200100716     C                   clear                   keyUNBCLI
061300100716     C                   clear                   keyTIPOMSG
061400100716     C                   clear                   keyVERSION
061500100716     C                   clear                   keyRELEASE
061600100716     C                   clear                   keyAGENCY
061700100716     C                   clear                   keyASSOCIA
061800110527      *
061900110527???? c                   if           tipo_seg = 'UNH'
062000110527     C                   movel     'AEG'         keyUNBCLI
062100110527     c                   end
062200110503      *
062300100720      *  Chiamata generica x decodificare il singolo segmento letto
062400100720     c                   call      'TRTCT00R1'
062500100720      * input
062600100720     c                   parm                    tipo_seg
062700100720     c                   parm                    segmento
062800100720     C                   parm                    keyUNBCLI
062900100720     C                   parm                    keyTIPOMSG
063000100720     C                   parm                    keyVERSION
063100100720     C                   parm                    keyRELEASE
063200100720     C                   parm                    keyAGENCY
063300100720     C                   parm                    keyASSOCIA
063400100720      *output
063500100720     c                   parm                    Errore
063600100720     c                   parm                    DsGenerica
063700100720     c                   parm                    Valori_inErr
063800100720     c                   parm                    Descr_Errore
063900100720     c                   parm                    trovato_seg
064000100720      **--------
064100100720      *  Ritorna la DS GENERICA oppure l'errore
064200100120      **--------
064300100120     c                   select
064400100720      *
064500100720     c                   when      trovato_seg ='N'
064600100720      *
064700100720     c                   move      'S'           seg_imprevisto
064800100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
064900100720     c                   exsr      Error_DET
065000100720     c                   goto      end_Decod
065100100720      **--------
065200100120     c                   when      tipo_seg = 'UNA'
065300100720     c                   movel(p)  DsGenerica    EDS00UNA
065400100120      *
065500100720      **--------
065600100120     c                   when      tipo_seg = 'UNB'
065700100720     c                   movel(p)  DsGenerica    EDS00UNB
065800100120     C                   EXSR      DATI_UNB
065900100120      * --------
066000100120     c                   when      tipo_seg = 'UNH'
066100100720     c                   movel(p)  DsGenerica    EDS00UNH
066200100120     C                   EXSR      DATI_UNH
066300100120      * --------
066400100120     c                   when      tipo_seg = 'BGM'
066500100720     c                   movel(p)  DsGenerica    EDS00BGM
066600100120      **--------
066700100120     c                   when      tipo_seg = 'DTM'
066800100720     c                   movel(p)  DsGenerica    EDS00DTM
066900100120     C                   EXSR      DATI_DTM
067000100120      * --------
067100100120     c                   when      tipo_seg = 'TSR'
067200100720     c                   movel(p)  DsGenerica    EDS00TSR
067300100120      **--------
067400100120     c                   when      tipo_seg = 'CNT'
067500100720     c                   movel(p)  DsGenerica    EDS00CNT
067600100120      **--------
067700100120     c                   when      tipo_seg = 'TOD'
067800100720     c                   movel(p)  DsGenerica    EDS00TOD
067900100120     C                   EXSR      DATI_TOD
068000100120      **--------
068100100120     c                   when      tipo_seg = 'RFF'
068200100720     c                   movel(p)  DsGenerica    EDS00RFF
068300100120     C                   EXSR      DATI_RFF
068400100120      **--------
068500100120     c                   when      tipo_seg = 'TDT'
068600100720     c                   movel(p)  DsGenerica    EDS00TDT
068700100120      **--------
068800100120     c                   when      tipo_seg = 'MOA'
068900100720     c                   movel(p)  DsGenerica    EDS00MOA
069000100120     C                   EXSR      DATI_MOA
069100100120      **--------
069200100120     c                   when      tipo_seg = 'FTX'
069300100720     c                   movel(p)  DsGenerica    EDS00FTX
069400100120     C                   EXSR      DATI_FTX
069500100120      **--------
069600100120     c                   when      tipo_seg = 'NAD'
069700100720     c                   movel(p)  DsGenerica    EDS00NAD
069800100120     C                   EXSR      DATI_NAD
069900100120      **--------
070000100120     c                   when      tipo_seg = 'CTA'
070100100720     c                   movel(p)  DsGenerica    EDS00CTA
070200100120     C                   EXSR      DATI_CTA
070300100120      **--------
070400100120     c                   when      tipo_seg = 'COM'
070500100720     c                   movel(p)  DsGenerica    EDS00COM
070600100120     C                   EXSR      DATI_COM
070700100120      **--------
070800100120     c                   when      tipo_seg = 'GID'
070900100720     c                   movel(p)  DsGenerica    EDS00GID
071000100120     C                   EXSR      DATI_GID
071100100120      **--------
071200100120     c                   when      tipo_seg = 'MEA'
071300100720     c                   movel(p)  DsGenerica    EDS00MEA
071400100120     C                   EXSR      DATI_MEA
071500100120      **--------
071600100120     c                   when      tipo_seg = 'PCI'
071700100720     c                   movel(p)  DsGenerica    EDS00PCI
071800100120     C                   EXSR      DATI_PCI
071900100120      **--------
072000100120     c                   when      tipo_seg = 'UNT'
072100100720     c                   movel(p)  DsGenerica    EDS00UNT
072200100120     C                   EXSR      DATI_UNT
072300100120      **--------
072400100120     c                   when      tipo_seg = 'UNZ'
072500100720     c                   movel(p)  DsGenerica    EDS00UNZ
072600100120     C                   EXSR      DATI_UNZ
072700100120      **--------
072800100120     c                   OTHER
072900100120      **--------
073000100120     c                   move      'S'           seg_imprevisto
073100100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
073200100120     c                   exsr      Error_DET
073300100120     c                   goto      end_Decod
073400100120      **--------
073500100120     c                   endSL
073600100120      **
073700100120     c     end_Decod     endSR
073800090211     C*----------------------------------------------------------------
073900090211     C*? AZZMSG - Azzera dati messaggio
074000090211     C*----------------------------------------------------------------
074100090211     C     AZZMSG        BEGSR
074200090211     C*
074300090211     C* Pulisco dati di work x foglio viaggio
074400090211     C                   MOVEL     *BLANKS       WNUMFV           35
074500090211     C                   MOVEL     *BLANKS       WNRCMR           35
074600090211     C                   Z-ADD     0             WDATPA            8 0
074700090211     C                   Z-ADD     0             WDATFV            8 0
074800090211     C                   Z-ADD     0             WHHNFV            4 0
074900090211     C                   Z-ADD     0             WDTCMR            8 0
075000090211     C                   Z-ADD     0             WHHCMR            4 0
075100090211      *
075200090211      * pulisce gli RFF+FF di testata
075300090211     C                   clear                   $CLKSC            7 0
075400090211     C                   clear                   $CLTAR            3
075500090211     C                   clear                   $CLLNP            3 0
075600090211     C                   clear                   $CLNRS            2 0
075700090211     C                   clear                   $CLCTM            2
075800090211     C                   clear                   $CLBS1            1
075900090211     C                   clear                   $CLfnsp           1            *blk/S
076000090211      *
076100090211     C*  Inizializzo variabile new documento
076200090211     C                   Z-ADD     0             KCL
076300090211     C                   Z-ADD     0             WW                3 0
076400090211     ** Inizializza campo di work
076500090211     c                   move      'N'           WGID99
076600090211      * per controllare se almeno un record è stato importato sul VAB
076700090211     c                   clear                   Almeno_Uno        1
076800090213     c                   clear                   Almeno_Un_Due     1
076900090211     c                   eval      esito  = '0'
077000090211     C*
077100090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
077200090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
077300090603     c                   eval      Dettaglio_NAD_destinatario = *blank
077400090603     c                   eval      NAD_destinatario_in_testata = *blank
077500090603     c                   eval      NAD_mittente_in_testata = *blank
077600090603     c                   eval      Inizio_Dettaglio = *blank
077700090603     C*
077800090603     c                   clear                   tes_VABrmo
077900090603     c                   clear                   tes_VABnmo
078000090603     c                   clear                   tes_VABcmo
078100090603     C*
078200090603     c                   clear                   tes_VABrsd
078300090603     c                   clear                   tes_VABrd2
078400090603     c                   clear                   tes_VABind
078500090603     c                   clear                   tes_VABlod
078600090603     c                   clear                   tes_VABnzd
078700090603     C*
078800090603     c                   clear                   tes_Valuta
078900090603     C*
079000090211     C                   ENDSR
079100090210     C*----------------------------------------------------------------
079200090210      *? DECODIFICA DATI UNB
079300090210     C*----------------------------------------------------------------
079400090210     C     DATI_UNB      BEGSR
079500090210      *
079600090209      *  INIZIALIZZA  dati fondamentali messaggio
079700090209     C                   clear                   EDIDSPT
079800090209     C                   clear                   EDIDSPU
079900090209     C                   clear                   EDIDSPV
080000090209     C                   clear                   wnrDOC           35
080100090209     C                   move      'N'           PT_ok             1
080200090209      ** ---------------------------------------------------------------
080300090209      * quindi 1° cosa:
080400090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
080500090209      *     senza questo il messaggio NON è trascodificabile
080600090209     C                   MOVEL     unb0004       WCOD             35
080700090209     C                   MOVE      unb000720     WCOD
080800090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
080900090209     C                   Z-ADD     1             XX                3 0
081000090209     C     WCOD          LOOKUP    CPT(XX)                                32
081100090209      *
081200090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
081300090209     C                   If        *IN32 = *off
081400090209      * ******  Errore generale sul Messaggio
081500090209      * Msg di allerta Traduzione
081600090209     C                   EVAL      Indirizzi = CED_Bart
081700110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG'
081800090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
081900090209     C                             Bolle import in filiale - messaggio -
082000090209     C                             con UNB non presente in tabella PT controlla-
082100090209     C                             re EDI ricevuto su UPLOAD.'
082200090209      *
082300090209     c                   exsr      invio_mail
082400090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
082500090210     C                   eval      vinFLG = '2'
082600090210     C                   eval      se_errore   = 'S'
082700090210     c                   goto      end_UNB
082800090209      *
082900090209     C                   move      'N'           PT_ok             1
083000090209      *
083100090209      *             *------------------------------*
083200090209     C                   eLSe
083300090209      *             *------------------------------*
083400090209      * Se tutto OK su tab.PT:
083500090209     C                   move      'S'           PT_ok             1
083600090209      *
083700090209     C                   MOVEL     DPT(XX)       EDIDSPT
083800090209     C                   MOVEL     DPU(XX)       EDIDSPU
083900090209     C                   MOVEL     DPV(XX)       EDIDSPV
084000091016     c                   clear                   Mittente         35
084100091016     c                   clear                   Mitt_Qualifier    4
084200091016     c                   clear                   Id_Messaggio     14
084300090226     C                   eval      Mittente       = unb0004
084400090226     C                   eval      Mitt_qualifier = unb000720
084500090226     C                   eval      Id_Messaggio   = UNB0022
084600090209      *
084700090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
084800090209      *  ricevuti:
084900090209     c                   movel     §PVema        mail_ind         44
085000090209      *
085100090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
085200090209     c                   if        mail_ind <> *blank
085300090209     c                   exsr      imp_ADDR_mail
085400090209     c                   end
085500090209      *
085600090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
085700090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
085800090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
085900090209     C                   if        §PULUN = *blank
086000090209     C                   eval      §PULUN = '00'
086100090209     c                   end
086200090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
086300090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
086400090209      *
086500090209     C                   Z-ADD     §PTKSC        WKSC              7 0
086600090211     C                   MOVEL     §PTLNP        VABlnp_PT
086700090212     C                   MOVEL     §PTNRS        VABnrs_PT
086800090211     C                   MOVEL     §PTCTM        VABctm_PT
086900090209      *
087000090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
087100090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
087200090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
087300090209      *  i records trasmessi).
087400090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
087500090209      *
087600090209      *  In base al cod.trattamento merce reperisco tipo tratt.
087700090209     C                   CLEAR                   DS1B
087800090209     C                   Z-ADD     1             Y                 3 0
087900090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
088000090209     C     *IN32         IFEQ      '1'
088100090209     C                   MOVEL     DTM(Y)        DS1B
088200090209     C                   MOVEL     DS1B          WSVTRM           38
088300090209     C                   END
088400090209      *
088500090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
088600090209     C                   clear                   ACORAG
088700090209     C                   clear                   INDCAE
088800090209     C                   clear                   INDCAP
088900090209     C                   clear                   INDSTA
089000090209      * Decodifico partner
089100090209     C                   Z-ADD     1             KKUT
089200090209     C                   Z-ADD     KCI           KKCC
089300090209     C                   MOVE      §PTKSC        KKSC
089400090209     C     KACO          CHAIN     CNACO00F                           31
089500090209     C     KIND          CHAIN     CNIND00F                           31
089600090211      *
089700090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
089800090211      *   questa schiera è utilizzata anche per i clienti.
089900090211     C     §ptKSC        LOOKUP    KCL                                    39
090000090211     c                   If        *in39 = *off
090100090211     C                   add       1             WW
090200090211     C                   z-add     §ptKSC        KCL(WW)
090300090211     C                   Endif
090400090209      *
090500090209     C                   MOVEL     unb0017       WAA4              4 0
090600090209     C                   MOVE      WAA4          WAA2              2 0
090700090209     C                   MOVE      unb0017       WGG               2 0
090800090209     C                   MOVE      unb0017       WDTPRE            8 0
090900090209     C                   MOVE      unb0019       WHMPRE            4 0
091000090209     C                   MOVE      unb0017       WDTMSG            6 0
091100090209     C                   MOVE      WAA2          WDTMSG
091200090209     C                   MOVEL     WGG           WDTMSG
091300090210      * potremmo
091400090210      * Utilizzare l'identificativo della trasmissione come CMR
091500090209     C                   IF        unb0022 <> *blank
091600090209     C                   MOVEL     unb0022       WNRDOC
091700090211     C                   MOVEL     unb0022       WNUMFV
091800090211     C                   MOVEL     unb0022       WNRCMR
091900090209     c                   end
092000090209      *
092100090209     c                   Endif
092200090209      **
092300090210     c     end_UNB       endSR
092400090209      * ?================================================================== */
092500090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
092600090209      * ?================================================================== */
092700090227     C     Nuova_spediz  BEGSR
092800090227      **
092900090227      * Pulisco campi del file
093000090227     c                   z-add     vnrec         SAVNUM_RECord
093100090227     C                   CLEAR                   EDiVAB00
093200090227     C                   CLEAR                   EDiVAT00
093300090227     C                   CLEAR                   EDiVAD00
093400090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
093500090227     C                   MOVEL     'N'           WGID99            1
093600090227     C                   clear                   XY                5 0
093700090227     C                   clear                   §§                5 0
093800090227     C                   clear                   WS1496            5 0
093900090227     C                   clear                   WNSP             35
094000090227     C                   Z-ADD     *ZEROS        PVLB
094100090227     C                   MOVEA     *HIVAL        SGN
094200090227     C*  Azzero codice cliente - tariffa
094300090227     C                   Z-ADD     0             WTOCOC           16 0
094400090227     C                   Z-ADD     0             WTOPLC           16 2          ???
094500090227     C                   Z-ADD     0             WTOPNC           16 2          ???
094600110718     C                   Z-ADD     0             Work_KG          16 2          ???
094700090227     C*  Azzero codice cliente - tariffa
094800090227     C                   clear                   WCLKSC
094900090227     C                   clear                   WCLTAR
095000090227     C                   clear                   WCLLNP
095100090227     C                   clear                   WCLNRS
095200090227     C                   clear                   WCLCTM
095300090227     C                   clear                   WCLBS1
095400090227     C                   clear                   WCLfnsp                        *blk/S
095500090227     C                   clear                   WNOT1            35
095600090227     C                   clear                   WNOT2            35
095700090227     C*  Campi di work
095800090227     c                   clear                   wVabTIC
095900090227     c                   clear                   wri_vabtic        1
096000090227      *
096100090227     C* pulisce campi di work
096200090227     c                   clear                   watrde           35
096300090227     c                   clear                   wattel           25
096400090227     C                   clear                   WRMN              3
096500090227      *
096600090227      * Un errore nel dettaglio
096700090227     C                   clear                   Err_in_detta      1
096800090227     c                   clear                   errori_in_Wrt     1
096900090227     c                   clear                   se_trovato_NAD    1
097000090227      **
097100090227     c                   endSR
097200090227      * ?================================================================== */
097300090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
097400090227      * ?================================================================== */
097500090227     C     DATI_UNH      BEGSR
097600090227      *
097700090227      *  Prepara una nuova spedizione
097800090227      * ?                         --------------
097900090227     c                   exsr      Nuova_spediz
098000090227      * ?                         --------------
098100090211      *
098200090211      ** Riferimento del cliente
098300090211     C                   MOVEL     unh0062       WPRGSP           35
098400090211     C                   MOVEL     unh0062       WNSP             35
098500090211     C                   MOVEL     unh0062       WNUM             35
098600090211      * ?                                       --------
098700090211     C                   MOVEL     unh0062       VABRMA
098800090211      * ?                                       --------
098900090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
099000090211     C                   SUB       1             WLUNGH
099100090211     C                   EXSR      TSTNUM
099200090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
099300090211      * ?                                       --------
099400090211     C                   MOVEL     WNUMOK        VABRMN
099500090211      * ?                                       --------
099600090211     C                   END
099700090210      **
099800090303     C                   MOVEL     VABlnp_PT     VABLNP
099900090303     C                   MOVEL     VABnrs_PT     VABNRS
100000090303     C                   MOVEL     VABctm_PT     VABCTM
100100090303      **
100200090303      **
100300090210     c     end_UNH       endSR
100400090601      * ?================================================================== */
100500090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
100600090601      * ?================================================================== */
100700090601     C     DATI_DTM      BEGSR
100800090601     C*
100900090209     C*  Se data documento  memorizzo numero e data x CMR
101000090603     C     dtm2005       IFEQ      '9'
101100090209     C                   MOVEL     dtm2380       WDATA            35            Data
101200090209     C                   MOVEL     dtm2379       WFORM             3            Formato
101300090209     C                   EXSR      CNVDAT
101400090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
101500090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
101600090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
101700090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
101800090209     C     WERRO         IFEQ      'S'
101900090210     C                   eval      vinMSG = 'DTM data errata'
102000090213     c                   exsr      Error_DET
102100090210     c                   goto      end_DTM
102200090209     C                   END
102300090209     C                   END
102400090209      **
102500090209     C*  Se data consegna richiesta imposto già sui campi del VAB
102600110523     C     dtm2005       IFEQ      '2'
102700090209     C                   MOVEL     dtm2380       WDATA            35            Data
102800090209     C                   MOVEL     dtm2379       WFORM             3            Formato
102900090209     C                   EXSR      CNVDAT
103000090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
103100090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
103200090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
103300090209     C     WERRO         IFEQ      'S'
103400090210     C                   eval      vinMSG = 'DTM data errata'
103500090213     c                   exsr      Error_DET
103600090210     c                   goto      end_DTM
103700090209     C                   END
103800090209     C                   END
103900090211      **
104000090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
104100090603     C     dtm2005       IFEQ      '10'
104200090211     C                   MOVEL     dtm2380       WDATA            35            Data
104300090211     C                   MOVEL     dtm2379       WFORM             3            Formato
104400090211     C                   EXSR      CNVDAT
104500090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
104600090211     C     WERRO         IFEQ      'S'
104700090211     C                   eval      vinMSG = 'DTM data errata'
104800090213     c                   exsr      Error_DET
104900090211     c                   goto      end_DTM
105000090211     C                   END
105100090211     C                   END
105200090209      **
105300090210     c     end_DTM       endSR
105400090209      * ?================================================================== */
105500090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
105600090209      * ?================================================================== */
105700090209     C     DATI_TOD      BEGSR
105800090209      *
105900090210      *  Trascodifico tipo trasporto
106000090210     c                   clear                   TOD_char3         3
106100090210      *
106200090210      *  imposta il default dalla tabella PT se c'è
106300090210     C     §puPFA        ifNE      *blank
106400090210     c                   SELECT
106500090210      *  Porto Franco
106600090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
106700090210     C                   movel     '4'           VABCBO                         + C/Ass
106800090210     c                   WHEN      §puPFA = 'F'
106900090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
107000090210      *  Porto Assegnato
107100090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
107200090210     C                   movel     '6'           VABCBO                         + C/Ass
107300090210     C                   WHEN      §puPFA = 'A'
107400090210     C                   movel     '2'           VABCBO                         Senza C/Ass
107500090210     C                   ENDSL
107600090210     c                   end
107700090210      *
107800090210      *   Imposta o il codice o il metodo
107900090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
108000090210     c                   if        tod4053  <> *blank
108100090210     c                   movel(p)  tod4053       TOD_char3
108200090210     c                   else
108300090210     c                   if        tod4215  <> *blank
108400090210     c                   movel(p)  tod4215       TOD_char3
108500090210     c                   end
108600090210     c                   end
108700110328      ****************
108800110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
108900110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
109000110328      ****************
109100110328     c                   eval      Trovata_Tab_TB ='N'
109200110328      *
109300110328     c                   if              TOD_char3 <> *blank
109400110328     c                   movel(p)  TOD_char3     Key_Generica35
109500110328     c                   move      §puTB         Key_Generica35
109600110328     C                   eval      Y = 1
109700110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
109800110328     c   32              eval      Trovata_Tab_TB ='S'
109900110328      *
110000110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
110100110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
110200110328      *
110300110328     c                   if          Trovata_Tab_TB ='N'
110400110328     c                                 and
110500110328     C                               tod4215 <> *blank
110600110328     c                   movel(p)  tod4215       TOD_char3
110700110328     c                   movel(p)  TOD_char3     Key_Generica35
110800110328     c                   move      §puTB         Key_Generica35
110900110328     C                   eval      Y = 1
111000110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
111100110328     c   32              eval      Trovata_Tab_TB ='S'
111200110328     c                   end
111300110328     c                   end
111400110328      *
111500110328      ****************
111600110328      ******* Se vi sono eccezioni sul cliente imposta quelle
111700110328      ****************
111800110328     c                   If          Trovata_Tab_TB ='S'
111900110328      *
112000110328     c                   SELECT
112100110328     c                   WHEN      Decod_Porto(Y) = 'F'
112200110328     c                               and  VABcas > *zeros
112300110328     C                   movel     '4'           VABCBO                         + C/Ass
112400110328     c                   WHEN      Decod_Porto(Y) = 'F'
112500110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
112600110328      *  Porto Assegnato
112700110328     c                   WHEN      VABCAS > *zeros
112800110328     C                   movel     '6'           VABCBO                         + C/Ass
112900110328     C                   OTHER
113000110328     C                   movel     '2'           VABCBO                         Senza C/Ass
113100110328     C                   ENDSL
113200110328      *
113300110328      ****************  altrimenti
113400110328      ******* Se non vi sono eccezioni per il cliente
113500110328      *   Rileva a questo punto lo Standard per tutti
113600110328      ****************
113700110328     c                   elseIf      Trovata_Tab_TB ='N'
113800090210      **
113900090210     c                   if              TOD_char3 <> *blank
114000090210     c                   movel(p)  TOD_char3     fld35a           35
114100090213     C                   movel     UNB_Mittente  etbUNB
114200090213     C                   movel(p)  'TOD'         etbSGM
114300090213     C                   movel(p)  TOD_char3     etbVALSGM
114400090213     c     K_TAB1L       chain     edstbl01l
114500090210      *
114600090210      * prova quindi senza l'UNB specifico del cliente
114700090213     c                   if        not %Found(edstbl01l)
114800090213     C                   clear                   etbUNB
114900090213     c     K_TAB1L       chain     edstbl01l
115000090210     c                   end
115100090210      *
115200090213     c                   if        %Found(edstbl01l)
115300090210     c                   SELECT
115400090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
115500090210     C                   movel     '4'           VABCBO                         + C/Ass
115600090213     c                   WHEN      ETBDATI = 'F'
115700090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
115800090210      *  Porto Assegnato
115900090210     c                   WHEN      VABCAS > *zeros
116000090210     C                   movel     '6'           VABCBO                         + C/Ass
116100090210     C                   OTHER
116200090210     C                   movel     '2'           VABCBO                         Senza C/Ass
116300090210     C                   ENDSL
116400090210      *
116500090210     c                   end
116600090210     c                   endIF
116700090209      **
116800110328     c                   endIF
116900110328      **
117000090210      **  Il tipo bolla non è presente
117100090210     c                   if        VABCBO = *blank
117200090210     C                   eval      vinMSG = 'TOD non rilevato'
117300090213     c                   exsr      Error_DET
117400090210     c                   goto      end_TOD
117500090210     c                   end
117600090210      **
117700090210     c     end_TOD       endSR
117800090210      * ?================================================================== */
117900090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
118000090210      * ?================================================================== */
118100090210     C     DATI_RFF      BEGSR
118200090210      *
118300090210     C                   clear                   WAGE             35
118400110224     C                   clear                   WFF              35
118500110224     C                   clear                   WCU              35
118600090210      *
118700090210      *  Trascodifico riferimento spedizione
118800091019     c                   if        RFF1153 = Riferimento_Spedizione  and
118900091019     c                             RFF1154 <> *BLANKS
119000090210     C                   movel(p)  RFF1154       WAGE             35
119100090210     c                   end
119200090210      **
119300090210     C                   IF         WAGE <> *BLANKS
119400090210     C                   movel     WAGE          WNSP             35
119500090210     C                   movel     WAGE          WNUM             35
119600090210     C     ' '           scan      WAGE          WLUNGH            3 0
119700090210     C                   sub       1             WLUNGH
119800090210     C                   EXSR      TSTNUM
119900090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
120000090210     C                   movel     WAGE          VABRMA
120100091019     C                   IF         WRMN = Riferimento_Spedizione  or
120200091019     C                              WRMN = *blanks
120300090210     C                   movel     WNUMOK        VABRMN
120400091019     C                   eval       WRMN = Riferimento_Spedizione
120500090210     C                   ENDIF
120600090210     C                   ENDIF
120700090210     C                   ENDIF
120800090210      *
120900090210      * Codice Cliente di riferimento
121000091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
121100090210     C                   movel     RFF1154       WFF
121200090210     c                   END
121300090211      *
121400090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
121500090211     C     WFF           IFNE      *BLANKS
121600090211     C                   eval      XX = 1
121700090211     C                   movel     §PTKSC        WCCL             35
121800090211     C                   movel     WFF           WCOM28           28
121900090211     C                   move      WCOM28        WCCL
122000090211     C     WCCL          lookup    CCL(XX)                                34
122100090211     C     *IN34         IFEQ      '1'
122200090211     C                   movel     DCL(XX)       EDIDSCL
122300090211     C                   z-add     §CLKSC        WCLKSC            7 0
122400090211     C                   movel     §CLTAR        WCLTAR            3
122500090211     C                   z-add     §CLLNP        WCLLNP            3 0
122600090211     C                   z-add     §CLNRS        WCLNRS            2 0
122700090211     C                   movel     §CLCTM        WCLCTM            2
122800090211     C                   movel     §CLBS1        WCLBS1            1
122900090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
123000090211      * imposta la LNP   x il dettaglio specifco
123100090211     c                   eval      VABlnp = §CLlnp
123200090211      * imposta la Serie x il dettaglio specifco
123300090211     c                   eval      VABnrs = §CLnrs
123400090211      *
123500090211      *  In base al cod.trattamento merce reperisco tipo tratt.
123600090211      *    per un dettaglio specifico.
123700090211     C                   CLEAR                   DS1B
123800090211     C                   Z-ADD     1             Y                 3 0
123900090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
124000090211     C     *IN32         IFEQ      '1'
124100090211     C                   MOVEL     DTM(Y)        DS1B
124200090211     C                   END
124300090211      *
124400090211      * Se il cliente non era sulla schiera lo aggiunge
124500090211     C     §CLKSC        lookup    KCL                                    39
124600090211     C                   if        *in39 = *off
124700090211     C                   add       1             WW
124800090211     C                   z-add     §CLKSC        KCL(WW)
124900090211     c                   endif
125000090211      *
125100090211     C                   ENDIF
125200090211     C                   ENDIF
125300090211     C*----------------------------------------------------------------
125400090210      *
125500110502     c                   if        RFF1153 = Riferimento_Numerico    and
125600110502     C                             RFF1154 <> *blanks
125700090210     C                   movel     RFF1154       WCU
125800090210     C                   END
125900090210      *
126000090210      * Numero Ordine di vendita
126100090210     C     WCU           IFNE      *BLANKS
126200090210     C                   movel     WCU           WNUM             35
126300090210     C     ' '           scan      WCU           WLUNGH
126400090210     C                   sub       1             WLUNGH
126500090210     C                   EXSR      TSTNUM
126600090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
126700090210     C                   movel     WNUMOK        VABRMN
126800110502     c                   eval      WRMN = Riferimento_Numerico
126900090210     C                   Endif
127000090210     C                   ENDIF
127100090210      *
127200090210      * NOn c'è l'identificativo bolla del cliente
127300091019     c                   if        (RFF1153 = Riferimento_Spedizione and
127400091019     c                              RFF1154 = *BLANKS)
127500091019     c                             or
127600110502     c                             (RFF1153 = Riferimento_Numerico and
127700110502     c                              RFF1154 = *BLANKS)
127800090210     C                   eval      vinMSG = 'RFF riferimento assente'
127900090213     c                   exsr      Error_DET
128000090210     c                   goto      end_RFF
128100090210     C                   ENDIF
128200090210      *
128300090210     c     end_RFF       endSR
128400090209      * ?================================================================== */
128500090213     C*?  C.O.D. Monetary Amount
128600090209      * ?================================================================== */
128700090213     C     DATI_MOA      BEGSR
128800090210      *
128900090216     C* Controllo se campo importo numerico
129000090216      * con 3 decimali
129100090216     C     moa5004       IFNE      *zeros
129200090216      *
129300090216     C     moa5025       IFEQ      '157'
129400090216     C     moa5004       div       1000          VABIAS                         IMPORTO
129500090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
129600090216     C                   END
129700090216      *
129800090216     C     moa5025       IFEQ      '77 '                                        Valore
129900090216     C     moa5004       div       1000          VABVMD                         MERCE
130000090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
130100090216     C                   END
130200090216      *
130300090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
130400090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
130500090216     C     moa5004       div       1000          VABCAS
130600090216     C                   MOVEL     moa6345       VABVCA
130700090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
130800090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
130900090216     C     §PUTPC        IFEQ      'P'
131000090216     C     moa5004       ANDGT     *ZEROS
131100090216     C                   MOVEL     'OM'          VABTIC
131200090216     C                   MOVEL     'OM'          WVABTIC
131300090216     C                   ENDIF
131400090216     C                   END
131500090216     C                   END
131600090216      *
131700090216     C                   END                                                    Imp.non numer.
131800090213      *
131900090213     c     end_MOA       endSR
132000090213      * ?================================================================== */
132100090213     C*?  FREE TEXT
132200090213      * ?================================================================== */
132300090213     C     DATI_FTX      BEGSR
132400110404      *
132500110523      *
132600110523      * ?Se è la Natura Merce
132700110523     C                   IF        ftx4451 = 'AAA'                              -- 00 --
132800110523      *
132900110523     C                   MOVEL     D05(1)        vabNAS
133000110523      *
133100110523     C                   ELSE
133200110404      *
133300110404      * ?Se non è un testo per il Pagamento
133400110404     C     ftx4451       IFne      'PAI'                                        -- 01 --
133500110404      *
133600090216      * ? solo se si tratta di note SIN ICN o PAI non altro
133700090216     C                   DO        4             X
133800090216     C     D05(X)        IFNE      *BLANKS
133900090216     C     WNOT1         IFEQ      *BLANKS
134000090216     C                   MOVEL     D05(X)        WNOT1
134100090216     C                   MOVE      D05(X)        WNOT2
134200090216     C                   ELSE
134300090216     C     WNOT2         IFEQ      *BLANKS
134400090216     C                   MOVEL     D05(X)        WNOT2
134500090216     C                   END
134600110404     C                   ENDif
134700110404     C                   ENDif
134800110404     C                   ENDdo
134900110404      *
135000110404     C                   ENDif
135100090216     C*------>>
135200090216     C*  Decodifico tipo pagamento C/Assegno
135300090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
135400090216      *      (Problema di pulizia in presenza di + righe di testo)
135500090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
135600090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
135700090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
135800090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
135900090216     C                   if        wri_vabtic = *blank
136000090216     C                   MOVEL     *BLANKS       VABTIC
136100090216     c                   end
136200090216      **
136300090216     c                   clear                   trovato_TPI       1
136400090216      *
136500090216      * ?Se c'è il tipo Incasso
136600090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
136700090216      *
136800090216     C                   DO        4             X                              -- 02 --
136900090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
137000130311      **
137100130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
137200130311      **  per gestire Partner come AZKAR che ha codice "AAA"
137300130311     C*******            MOVEL     D05(X)        WCTC              2
137400130311     C                   MOVEL     D05(X)        WCTC             10
137500090216     C                   Z-ADD     1             Y
137600090216     c                   movel(p)  Wctc          fld35a           35
137700090216     c                   move      §puTC         fld35a
137800090216     C     fld35a        LOOKUP    CTC35(Y)                               33
137900090216     C   33              MOVEL     TPI(Y)        VABTIC
138000090216     c   33              move      'S'           trovato_TPI
138100090216     C                   END                                                    -- 03 --
138200090216     C                   ENDdo                                                  -- 02 --
138300090216      *
138400090216     C                   ELSE                                                   -- 01 --
138500090216      *
138600090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
138700090216      *    Deve essere impostato comunque da testata
138800090216     c                   if        vabtic = *blank
138900090216     C                   MOVEL     wVabTIC       VABTIC
139000090216     c                   end
139100090216      *
139200090216     C                   END                                                    -- 01 --
139300090216      *
139400110523     C                   ENDif                                                  -- 00 --
139500110523      *
139600090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
139700090216      *    essere presenti altri record Flat con altre informazioni
139800090216      *    che potrebbero abblancare il Tipo incasso precedentemente
139900090216      *    decodificato
140000090216     c                   if        Trovato_TPI = 'S' and
140100090216     c                                 wri_vabtic = *blank
140200090216     c                   eval      wri_vabtic = 'S'
140300090216     c                   end
140400090213      *
140500090213     c     end_FTX       endSR
140600090213      * ?================================================================== */
140700090213     C*?  Anagrafica del mittente e del destinatario
140800090213      * ?================================================================== */
140900091016     C     DATI_NAD      BEGSR
141000090213      *
141100090210      * Dati destinatario
141200090210     C                   IF        nad3035 = 'CN' or
141300090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
141400090227      *
141500090227      * Se trovato un indirizzo di destinatario
141600090227     c                   eval       se_trovato_NAD ='S'
141700090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
141800090210      *
141900090210     C* Reperisco nazione corrispondente destinatario
142000090210     C                   Z-ADD     1             YY                3 0
142100090210     c                   eval      *in32 = *off
142200090210     C                   if        nad3207 <> *BLANKS
142300090210     C     nad3207       LOOKUP    ISO(YY)                                32
142400090210     C                   endif
142500090210      *
142600090210     C* Imposto dati destinatario
142700090211     C                   MOVEL     nad31241      VABRSD
142800090211     C                   if        nad31242 <> *LOVAL
142900090211     C                   MOVEL     nad31242      VABRD2
143000090210     C                   endif
143100090211      *
143200091019????  * DAL PARTY NAME
143300091019???? C* Imposto dati destinatario
143400091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
143500091019???? C                   MOVEL     nad30361      VABRSD
143600091019???? C                   if        nad30362 <> *LOVAL
143700091019???? C                   MOVEL     nad30362      VABRD2
143800091019???? C                   endif
143900091019???? C                   end
144000090211      *
144100090210     C                   MOVEL     nad30421      VABIND
144200090210     C                   MOVEL     nad3164       VABLOD
144300090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
144400090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
144500090210      *
144600090210     C* Verifico se c'è dello spazio per nella località
144700090210     C     '    '        SCAN      VABLOD        WI                4 0
144800090210      *
144900090210     C                   MOVEA     VABLOD        LOD
145000090210     C                   ADD       1             WI
145100090210     C                   MOVEA     NAD30422      LOD(WI)
145200090210     C                   MOVEA     LOD           VABLOD
145300090210     C                   ENDif
145400090210      *
145500090210     C                   If        *in32 = *on
145600090210     C                   MOVEL     C15(YY)       VABNZD
145700090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
145800090210     c                   Else
145900090210     C                   MOVEL     *BLANKS       VABNZD
146000090210     C                   Z-ADD     0             WFLCPF
146100090210     c                   Endif
146200090210      * Converto CAP
146300090210     C                   EXSR      CNVCAP
146400090603     C*
146500090603     C* Se in testata
146600090603     c                   if        Inizio_dettaglio = *blank
146700090603     c                   eval       tes_VABrsd = VABrsd
146800090603     c                   eval       tes_VABrd2 = VABrd2
146900090603     c                   eval       tes_VABind = VABind
147000090603     c                   eval       tes_VABlod = VABlod
147100090603     c                   eval       tes_VABnzd = VABnzd
147200090603     c                   eval      NAD_destinatario_in_testata = 'S'
147300090603     c                   end
147400090603     C*
147500090210     C                   ENDIF
147600090210      *
147700090210      * Dati mittente
147800091016     C                   IF        nad3035 = 'CZ'
147900090210     C                   movel     nad30361      VABRMO
148000090210      *
148100090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
148200090210     C                   movel     §PTNAZ        VABNMO
148300090210      *
148400090210     C                   IF        nad3207 <> *blanks
148500090210     C                   eval      YY = 1
148600090210     C     nad3207       LOOKUP    ISO(YY)                                32
148700090210     C                   if         *in32 = *on
148800090210     C                   MOVEL     C15(YY)       VABNMO
148900090210     C                   endif
149000090210     C                   ENDIF
149100090210      *
149200090210      * Normalizzo CAP mittente originale
149300090210     C                   clear                   T
149400090210     C                   clear                   S
149500090210     C                   MOVEA     *BLANKS       CAPM
149600090210     C                   MOVEA     nad3251       CAP9
149700090210     C                   DO        9             T
149800090210     C     CAP9(T)       IFNE      *BLANKS
149900090210     C                   ADD       1             S
150000090210     C                   MOVE      CAP9(T)       CAPM(S)
150100090210     C                   ENDIF
150200090210     C                   ENDDO
150300090210     C*
150400090210      *     Controllo validità CAP
150500090210     C                   MOVEA     CAPM          VABCMO
150600090210      *
150700090210     C                   CLEAR                   TISI95DS
150800090210     C                   MOVEL     VABCMO        I95CAP
150900090210     C                   MOVEL     VABNMO        I95NAR
151000090210     C                   MOVEL     WOGGI         I95DAT
151100090210     C                   MOVEL     '3'           I95TCN
151200090210     C                   CALL      'TISI95R'
151300090210     C                   PARM                    TISI95DS
151400090210      *     Errore
151500090210     C     O95ERR        IFNE      *BLANKS
151600090210     C                   MOVEL     INDCAE        VABCMO
151700090210     C                   MOVEL     §PTNAZ        VABNMO
151800090210     C                   END
151900090603      *
152000090603     C* Se in testata
152100090603     c                   if        Inizio_dettaglio = *blank
152200090603     c                   eval      tes_VABrmo = VABrmo
152300090603     c                   eval      tes_VABnmo = VABnmo
152400090603     c                   eval      tes_VABcmo = VABcmo
152500090603     c                   eval      NAD_mittente_in_testata = 'S'
152600090603     c                   end
152700090210      *
152800090210     C                   END
152900090210      *
153000090210      * NOn c'è l'identificativo bolla del cliente
153100090210     C                   if          vabRSD = *blank or
153200090210     C                               vabIND = *blank or
153300090210     C                               vabLOD = *blank
153400090211     C                   IF        nad3035 = 'CN'
153500090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
153600090213     c                   exsr      Error_DET
153700090210     c                   goto      end_NAD
153800090210     C                   ENDIF
153900090211     C                   ENDIF
154000090210      *
154100090210     c     end_NAD       endSR
154200090210      * ?================================================================== */
154300090210     C*?  Contatto a cui far riferimento
154400090210      * ?================================================================== */
154500090210     C     DATI_CTA      BEGSR
154600090210      *
154700090210      *  memorizza il riferimento di consegna x il destinatario
154800090210      *  memorizza il riferimento di consegna nr.telefonico
154900090210     C     cta3412       ifne      *BLANKS
155000090210     c                   movel     cta3412       watrde
155100090210     c                   end
155200090210      *
155300090210     c                   endSR
155400090210      * ?================================================================== */
155500090210     C*?  Contatto telefonico  a cui far riferimento
155600090210      * ?================================================================== */
155700090210     C     DATI_COM      BEGSR
155800090210      *
155900090210     C     com3148       ifne      *BLANKS
156000090210     c                   movel     com3148       wattel
156100090210     c                   end
156200090210      *
156300090209     C                   ENDSR
156400090210      * ?================================================================== */
156500090210     C*?  Dati di dettaglio della spedizione e dei colli
156600090210      * ?================================================================== */
156700090210     C     DATI_GID      BEGSR
156800090210      *
156900090210     C* Se ho totali per spedizione metto in campi VAB
157000090210     C     gid1496       IFEQ      99999
157100090210     C     gid1496       orEQ      9999
157200090210     C                   MOVEL     'S'           WGID99
157300090210     C                   z-add     gid722420     VABNCL
157400090210     C                   ELSE
157500090210     C     WGID99        IFEQ      'N'
157600090210     C                   z-add     gid722420     WTNCL             5 0
157700090210     C                   ADD       WTNCL         VABNCL
157800090210     C                   END
157900090210     C                   END
158000090212      *
158100090210     C                   ADD       VABNCL        WTOCOC           16 0
158200090210      *
158300090210     c     end_GID       endSR
158400090210      * ?================================================================== */
158500090210     C*?  Dati di dettaglio misure spedizione e colli
158600090210      * ?================================================================== */
158700090210     C     DATI_MEA      BEGSR
158800090210      *
158900090210     C*  Peso
159000090210     C     mea6311       IFEQ      'WT '
159100090210     C     mea6411       ANDEQ     'KGM'
159200090210      *
159300090210     C*  Controllo se ho valore totale o parziale
159400090210      *   sempre il Lordo
159500110523???? C     mea6313       IFEQ      'AAE'
159600090210     C     WS1496        IFEQ      99999
159700090210     C     WS1496        orEQ      9999
159800091019???? C                   move      mea6314       mea6314_2        18 2          Peso
159900091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
160000090210     C                   ELSE
160100091019???? C     WGID99        IFEQ      'N'
160200091019???? C                   move      mea6314       mea6314_2                      Peso
160300110718???? C******             ADD       mea6314_2     VABPKB                         Peso
160400110718???? C                   ADD       mea6314_2     Work_KG                        Peso
160500110718     c                   eval         vabPKB = Work_KG
160600090210     C                   END
160700090210     C                   END
160800090210     C                   END
160900090210      *
161000110523???? C     mea6313       IFEQ      'AAE'
161100091019???? C                   move      mea6314       mea6314_2                      Peso
161200091019???? C                   ADD       mea6314_2     WTOPLC
161300090210     C                   END
161400091019???? C     mea6313       IFEQ      'N  '
161500091019???? C                   move      mea6314       mea6314_2                      Peso
161600091019???? C                   ADD       mea6314_2     WTOPNC
161700090210     C                   END
161800090210     C                   END
161900090210      ***
162000090210     C*  Peso in Grammi  se abilitato a utilizzarlo
162100090210      ***
162200090210     C     §puGR         IFeq      'S'
162300090210     C     mea6311       IFEQ      'WT '
162400090210     C     mea6411       ANDEQ     '163'
162500090210      *
162600090210     C*  Controllo se ho valore totale o parziale
162700110523???? C     mea6313       IFEQ      'AAE'
162800091019???? C     WS1496        IFEQ      99999
162900091019???? C     WS1496        orEQ      9999
163000091019???? c                   move      mea6314       mea6314_2
163100091019???? C     mea6314_2     div       1000          VABPKB                         Peso
163200090210     C                   ELSE
163300091019???? C     WGID99        IFEQ      'N'
163400091019???? c                   move      mea6314       mea6314_2
163500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
163600110718???? C*******            add       Peso_inKG     VABPKB                         Peso
163700110718???? C                   ADD       Peso_inKG     Work_KG                        Peso
163800110718     c                   eval         vabPKB = Work_KG
163900090210     C                   END
164000090210     C                   END
164100090210     C                   END
164200090210      *
164300110523???? C     mea6313       IFEQ      'AAE'
164400091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
164500091019???? C                   ADD       Peso_inKG     WTOPLC
164600090210     C                   END
164700091019???? C     mea6313       IFEQ      'N  '
164800091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
164900091019???? C                   ADD       Peso_inKG     WTOPNC
165000090210     C                   END
165100090210     C                   END
165200090210     C                   END
165300090210      *  Volume
165400091019???? C     mea6311       IFEQ      'VOL'
165500091019???? C     mea6411       ANDEQ     'MTQ'
165600090210      *  Controllo se ho valore totale o parziale
165700090210     C     WS1496        IFEQ      99999
165800090210     C     WS1496        orEQ      9999
165900091019???? C                   move      mea6314       mea6314_2        18 2          Volume
166000091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
166100091019???? C                   Z-ADD     mea6314_2     PVLB
166200090210     C                   ELSE
166300091019     C     WGID99        IFEQ      'N'
166400091019???? C                   move      mea6314       mea6314_2        18 2          Volume
166500091019???? C                   ADD       mea6314_2     VABVLB                         Volume
166600091019???? C                   ADD       mea6314_2     PVLB
166700090210     C                   END
166800090210     C                   END
166900090210     C                   END
167000090210      *
167100090210     C* Ricerco CAP
167200090210     C     WCAP          IFNE      *BLANKS
167300090210     C     mea6311       IFEQ      'WT '
167400090210      *
167500090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
167600090210     C                   CLEAR                   FNLV55DS
167700090210     C                   MOVEL     'P'           D55TPT
167800090210     C                   MOVEL     WOGGI         D55DRF
167900090210     C                   MOVEL     VABLNP        D55LNP
168000090210     C                   MOVEL     VABLNP        D55LIN
168100090210     C                   CALL      'FNLV55R'
168200090210     C                   PARM                    FNLV55DS
168300090210     C                   MOVE      D55TFP        COMTFP            3 0
168400090210      *
168500090210     C*  PASSO IL TERMINAL PARTENZA
168600090210     C                   CLEAR                   TISI95DS
168700090210     C                   Z-ADD     COMTFP        I95TFP
168800090210     C                   MOVEL     VABTSP        I95TSP
168900090210     C                   MOVEL     'S'           I95FRE
169000090210     C                   MOVEL     '3'           I95TCN
169100090210     C                   MOVEL     WCAP          I95CAP
169200090210     C                   MOVEL     VABNZD        I95NAR
169300090210     C                   MOVEL     VABLOD        I95LOC
169400090210     C                   MOVEL     VABIND        I95IND
169500090210      *
169600090210     C* Se gestita seconda linea di arrivo passo peso - volume
169700090210     C* e numero colli effettivo
169800090210     C     §PULNA        IFEQ      'S'
169900090210     C                   Z-ADD     VABPKB        I95LKG
170000090210     C                   Z-ADD     VABVLB        I95LMC
170100090210     C                   ELSE
170200090210     C* Altrmenti passo 1 Kg e 0,001
170300090210     C***                  Z-ADD1         I95LKG
170400090210     C***                  Z-ADD0,001     I95LMC
170500090210     C                   END
170600090210     C*
170700090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
170800090210     C                   SELECT
170900090210     C     VABCBO        WHENEQ    '1 '
171000090210     C                   MOVEL     *BLANKS       I95FCA
171100090210     C                   MOVEL     'F'           I95TPO
171200090210     C     VABCBO        WHENEQ    '2 '
171300090210     C                   MOVEL     *BLANKS       I95FCA
171400090210     C                   MOVEL     'A'           I95TPO
171500090210     C     VABCBO        WHENEQ    '4 '
171600090210     C                   MOVEL     'S'           I95FCA
171700090210     C                   MOVEL     'F'           I95TPO
171800090210     C     VABCBO        WHENEQ    '6 '
171900090210     C                   MOVEL     'S'           I95FCA
172000090210     C                   MOVEL     'A'           I95TPO
172100090210     C                   OTHER
172200090210     C                   MOVEL     'F'           I95TPO
172300090210     C     VABCAS        IFGT      0
172400090210     C                   MOVEL     'S'           I95FCA
172500090210     C                   ELSE
172600090210     C                   MOVEL     *BLANKS       I95FCA
172700090210     C                   END
172800090210     C                   ENDSL
172900090210     C*
173000090210     C                   CALL      'TISI95R'
173100090210     C                   PARM                    TISI95DS
173200090210     C* Reperisco i dati da CAP
173300090210     C                   MOVEL     O95PRV        VABPRD
173400090210     C                   MOVEL     O95LNA        VABLNA
173500090210     C                   MOVEL     O95ZNC        VABZNC
173600090210     C                   MOVEL     WCAP          VABCAD
173700090210     C*
173800090210     C     O95ERR        IFNE      *BLANKS
173900090210     C     O95FIT        ANDEQ     'S'
174000090210     C*  Cap non trovato
174100090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
174200090213     c                   exsr      Error_DET
174300090210     C                   goto      end_MEA
174400090210     C                   END
174500090210     C                   END
174600090210      *
174700090210     C                   ELSE
174800090210      *
174900090210     C                   CLEAR                   VABPRD
175000090210     C                   CLEAR                   VABLNA
175100090210     C                   CLEAR                   VABZNC
175200090210     C                   CLEAR                   VABCAD
175300090210     C*  Cap non trovato
175400090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
175500090213     c                   exsr      Error_DET
175600090210     C                   goto      end_MEA
175700090210     C                   END
175800090210      *
175900090210     c     end_MEA       endSR
176000090210      * ?================================================================== */
176100090210     C*?  Dati di dettaglio Codice a barre Segnacolli
176200090210      * ?================================================================== */
176300090210     C     DATI_PCI      BEGSR
176400090210      *
176500090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
176600090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
176700090210     C     §1BF12        IFEQ      'E'
176800090210     C                   EXSR      SEGNEE
176900090210     C                   ELSE
177000090210      *
177100090210      *  La prima volta controllo congruità numero segnacollo
177200090210     C     §PUBS1        IFNE      *BLANKS
177300090210     C     VABnrs        ANDNE     0
177400090210      *  Se il codice trattamento merce prevede che segnacollino i
177500090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
177600090210     C     §1BFG7        IFEQ      'S'
177700090210     C     §1BFG1        OREQ      'S'
177800090210      *  calcolo segnacollo
177900090210     C                   EXSR      SGNELA
178000090210     C                   END
178100090210     C                   END
178200090210      *
178300090210     C                   END
178400090210      *
178500090210     c     end_PCI       endSR
178600090211     C*----------------------------------------------------------------
178700090211      *? dati finali del dettaglio    UNT
178800090211     C*----------------------------------------------------------------
178900090211     C     DATI_UNT      BEGSR
179000090211      *
179100090213     c                   eval      NUM_RECord = vnREC
179200090213     c                   z-add     vnrec         last_RECord
179300090211      **
179400090211     c                   endSR
179500090211     C*----------------------------------------------------------------
179600090211      *? dati finali     UNT
179700090211     C*----------------------------------------------------------------
179800090211     C     DATI_UNZ      BEGSR
179900090211      *
180000090211      **
180100090211     c                   endSR
180200090210      *----------------------------------------------------------------
180300090210      *? Input segnacolli su schiera di 20 elementi
180400090210      *----------------------------------------------------------------
180500090210     C     INP_Segnacol  BEGSR
180600090210      *
180700090210     c                   clear                   quanti_PCI        3 0
180800090210     c                   if        §puEL1 = 0
180900090210     c                   z-add     10            quanti_PCI
181000090210     c                   else
181100090210     c                   z-add     §puEL1        quanti_PCI
181200090210     c                   end
181300090210      *
181400090210      *  Pulisce la schiera dei segnacolli da elaborare
181500090210     c                   clear                   x30
181600090210     c                   z-add     0             Nr_x30            3 0
181700090210      *
181800090210      *   riporto tutto sulla schiera generica X30
181900090210      *
182000090210     c                   do        10            limite            3 0
182100090210      * Limita quanti elementi passati in ogni PCI
182200090210      *   devono essere considerati
182300090210     c                   if        limite > quanti_PCI
182400090210     c                   Leave
182500090210     c                   end
182600090210      *
182700090210      *  Carica la schiera generale di tutti i segnacolli
182800090210     c                   if        D30_1(limite) <> *blank
182900090210     c                   add       1             Nr_x30
183000090210     c                   eval      X30(Nr_x30) = D30_1(limite)
183100090210     c                   end
183200090210     c                   enddo
183300090210      *
183400090210     C                   ENDSR
183500090210      *----------------------------------------------------------------
183600090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
183700090210      *----------------------------------------------------------------
183800090210     C     SEGNEE        BEGSR
183900090210      *
184000090210     c                   exsr      INP_Segnacol
184100090210      *
184200090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
184300090210     C                   DO        10            x                              --- 01 ---
184400090210      *
184500090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
184600090210     C     x30(X)        ANDNE     *LOVAL
184700090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
184800090210     C                   ADD       1             §§
184900090210     C                   MOVEL     x30(X)        SGE(§§)
185000090210     C                   END                                                    --- 03 ---
185100090210      *
185200090210     C                   END                                                    --- 02 ---
185300090210      *
185400090210     C                   ENDSR
185500090210     C*----------------------------------------------------------------
185600090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
185700090210     C*----------------------------------------------------------------
185800090210     C     SGNELA        BEGSR
185900090210      *
186000090210     c                   exsr      INP_Segnacol
186100090210     C*
186200090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
186300090210     C*  in base al numero elementi validi carico sgn.
186400090210     C                   DO        10            X                              --- 01 ---
186500090210     C*
186600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
186700090210     C     x30(X)        ANDNE     *LOVAL
186800090210     C*
186900090210     C                   CLEAR                   DSSGN
187000090210     C*  Controllo se devo ricevere la linea di partenza
187100090210     C                   MOVEL     'N'           WERRO             1
187200090210     C     §PULP1        IFGT      0                                            --- 03 ---
187300090210     C                   EXSR      REPLNP
187400090210     C                   END                                                    --- 03 ---
187500090210     C*  Controllo se devo ricevere la linea di arrivo
187600090210     C     §PULA1        IFGT      0                                            --- 03 ---
187700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
187800090210     C                   EXSR      REPLNA
187900090210     C                   END                                                    --- 03 ---
188000090210     C*  Controllo se devo ricevere il numero di serie
188100090210     C     §PUNR1        IFGT      0                                            --- 03 ---
188200090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
188300090210     C                   EXSR      REPNRS
188400090210     C                   END                                                    --- 03 ---
188500090210     C*  Controllo se devo ricevere il numero segnacollo
188600090210     C     §PUSG1        IFGT      0                                            --- 03 ---
188700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
188800090210     C                   EXSR      REPSGN
188900090210     C                   END                                                    --- 03 ---
189000090210     C*  Controllo se devo ricevere la zona di consegna
189100090210     C     §PUZN1        IFGT      0                                            --- 03 ---
189200090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189300090210     C                   EXSR      REPZNC
189400090210     C                   END                                                    --- 03 ---
189500090210     C*  Imposto da VAB i dati a zero se mi è stato passato
189600090210     C*  numero segnacollo valido
189700090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
189800090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
189900090210     C                   Z-ADD     VABLNP        SGNLNP
190000090210     C                   END                                                    --- 04 ---
190100090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
190200090210     C                   Z-ADD     VABLNA        SGNLNA
190300090210     C                   END                                                    --- 04 ---
190400090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
190500090210     C                   Z-ADD     VABNRS        SGNNRS
190600090210     C                   END                                                    --- 04 ---
190700090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
190800090210     C                   Z-ADD     VABZNC        SGNZNC
190900090210     C                   END                                                    --- 04 ---
191000090210      *
191100090210     C*  mi reimposto LNA e zona da segnacollo
191200090210     C                   Z-ADD     SGNLNA        VABLNA
191300090210     C                   Z-ADD     SGNZNC        VABZNC
191400090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
191500090210     C                   ADD       1             XY
191600090210     C                   MOVE      SGNNSC        SGN(XY)
191700090210     C                   END                                                    --- 03 ---
191800090210     C*
191900090210     C                   END                                                    --- 02 ---
192000090210     C*
192100090210     C                   END                                                    --- 01 ---
192200090210      *
192300090210     C                   ENDSR
192400090210     C*----------------------------------------------------------------
192500090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
192600090210     C*----------------------------------------------------------------
192700090210     C     REPLNP        BEGSR
192800090210     C*
192900090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
193000090210     C*  (a partire dalla posizione iniziale indicata in tabella)
193100090210     C*  dal numero segnacollo passatomi dal partner
193200090210     C                   Z-ADD     §PULP1        WP                2 0
193300090210     C     3             SUBST     x30(X):WP     WLNP              3
193400090210     C*
193500090210     C*  controllo che la lnp calcolata sia numerica
193600090210     C                   TESTN                   WLNP                 98
193700090210     C*
193800090210     C*  se è numerica la carico
193900090210     C     *IN98         IFEQ      '1'
194000090210     C                   MOVE      WLNP          SGNLNP
194100090210     C                   ELSE
194200090210     C*  se non è numerica stampo errore
194300090210     C                   MOVEL     'S'           WERRO             1
194400090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
194500090213     c                   exsr      Error_DET
194600090210     C                   goto      end_LNP
194700090210     C                   END
194800090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
194900090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
195000090210     C     SGNLNP        IFNE      VABLNP
195100090210     C                   MOVEL     'S'           WERRO             1
195200090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
195300090210     c                             a della Bolla'
195400090213     c                   exsr      Error_DET
195500090210     C                   goto      end_LNP
195600090210     C                   END
195700090210     C*
195800090210     C     end_LNP       ENDSR
195900090210     C*----------------------------------------------------------------
196000090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
196100090210     C*----------------------------------------------------------------
196200090210     C     REPLNA        BEGSR
196300090210     C*
196400090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
196500090210     C*  (a partire dalla posizione iniziale indicata in tabella)
196600090210     C*  dal numero segnacollo passatomi dal partner
196700090210     C                   Z-ADD     §PULA1        WP                2 0
196800090210     C     3             SUBST     x30(X):WP     WLNA              3
196900090210     C*  controllo che la lna calcolata sia numerica
197000090210     C                   TESTN                   WLNA                 98
197100090210     C*  se è numerica la carico
197200090210     C     *IN98         IFEQ      '1'
197300090210     C                   MOVE      WLNA          SGNLNA
197400090210     C                   ELSE
197500090210     C*  se non è numerica stampo errore
197600090210     C                   MOVEL     'S'           WERRO             1
197700090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
197800090213     c                   exsr      Error_DET
197900090210     C                   goto      end_LNA
198000090210     C                   END
198100090210     C*
198200090210     C     end_LNA       ENDSR
198300090210     C*----------------------------------------------------------------
198400090210     C*? REPNRS - Reperisce numero serie
198500090210     C*----------------------------------------------------------------
198600090210     C     REPNRS        BEGSR
198700090210     C*
198800090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
198900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
199000090210     C*  dal numero segnacollo passatomi dal partner
199100090210     C                   Z-ADD     §PUNR1        WP                2 0
199200090210     C     2             SUBST     x30(X):WP     WNRS              2
199300090210     C*  controllo che il nrs calcolata sia numerico
199400090210     C                   TESTN                   WNRS                 98
199500090210     C*  se è numerica la carico
199600090210     C     *IN98         IFEQ      '1'
199700090210     C                   MOVE      WNRS          SGNNRS
199800090210     C                   ELSE
199900090210     C*  se non è numerico stampo errore
200000090210     C                   MOVEL     'S'           WERRO             1
200100090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
200200090213     c                   exsr      Error_DET
200300090210     C                   goto      end_NRS
200400090210     C                   END
200500090210      *
200600090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
200700090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
200800090210     C     SGNNRS        IFNE      VABNRS
200900090210     C                   MOVEL     'S'           WERRO             1
201000090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
201100090210     c                             o della Bolla'
201200090213     c                   exsr      Error_DET
201300090210     C                   goto      end_NRS
201400090210     C                   END
201500090210     C*
201600090210     C     end_NRS       ENDSR
201700090210     C*----------------------------------------------------------------
201800090210     C*? REPZNC - Reperisce zona consegna
201900090210     C*----------------------------------------------------------------
202000090210     C     REPZNC        BEGSR
202100090210     C*
202200090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
202300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
202400090210     C*  dal numero segnacollo passatomi dal partner
202500090210     C                   Z-ADD     §PUZN1        WP                2 0
202600090210     C     2             SUBST     x30(X):WP     WZNC              2
202700090210     C*  controllo che la zona calcolata sia numerica
202800090210     C                   TESTN                   WZNC                 98
202900090210     C*  se è numerica la carico
203000090210     C     *IN98         IFEQ      '1'
203100090210     C                   MOVE      WZNC          SGNZNC
203200090210     C                   ELSE
203300090210     C*  se non è numerica stampo errore
203400090210     C                   MOVEL     'S'           WERRO             1
203500090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
203600090213     c                   exsr      Error_DET
203700090210     C                   goto      end_ZNC
203800090210     C                   END
203900090210     C*
204000090210     C     end_ZNC       ENDSR
204100090210     C*----------------------------------------------------------------
204200090210     C*? REPSGN - Reperisce numero segnacollo
204300090210     C*----------------------------------------------------------------
204400090210     C     REPSGN        BEGSR
204500090210     C*
204600090210     C*  se viene passata nr.segnacollo estraggo sottostringa
204700090210     C*  lunga 7 chr (a partire dalla posizione iniziale
204800090210     C*  indicata in tabella)
204900090210     C*  dal numero segnacollo passatomi dal partner
205000090210     C                   Z-ADD     §PUSG1        WP                2 0
205100090210     C     7             SUBST     x30(X):WP     WSGN              7
205200090210     C*  controllo che il nr.segnacollo calcolato sia numerico
205300090210     C                   TESTN                   WSGN                 98
205400090210     C*  se è numerico lo carico
205500090210     C     *IN98         IFEQ      '1'
205600090210     C                   MOVE      WSGN          SGNNSC
205700090210     C                   ELSE
205800090210     C*  se non è numerica stampo errore
205900090210     C                   MOVEL     'S'           WERRO             1
206000090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
206100090213     c                   exsr      Error_DET
206200090210     C                   goto      end_SGN
206300090210     C                   END
206400090210     C*
206500090210     C     end_SGN       ENDSR
206600051205      *----------------------------------------------------*
206700051205      *   DEFINIZIONE CHIAVI                               *
206800051205      *----------------------------------------------------*
206900051205     C     *INZSR        BEGSR
207000051205      *
207100991129     c     *ENTRY        PLIST
207200060613     C                   parm                    esito             1
207300971215      *
207400050112     C     KTABel        KLIST
207500050112     C                   KFLD                    TBLKUT
207600050112     C                   KFLD                    TBLCOD
207700050112     C                   KFLD                    TBLKEY
207800090210      *
207900090210     C     K_TAB1L       KLIST
208000090213     C                   KFLD                    etbUNB
208100090213     C                   KFLD                    etbSGM
208200090213     C                   KFLD                    etbVALSGM
208300090209     C*
208400090209     C* Definisco chiavi di accesso
208500090209     C     KTAB1         KLIST
208600090209     C                   KFLD                    KKUT
208700090209     C                   KFLD                    KCOD
208800090209     C*
208900090209     C     KNUF          KLIST
209000090209     C                   KFLD                    KAAA
209100090209     C                   KFLD                    KCNU
209200090209     C                   KFLD                    KFIL
209300090209      *
209400090209     C     KVAB1         KLIST
209500090209     C                   KFLD                    KCCM
209600090209     C                   KFLD                    KCMR
209700090209     C                   KFLD                    KCNT
209800090209     C                   KFLD                    KAAS
209900090209     C                   KFLD                    KLNP
210000090209     C                   KFLD                    KNRS
210100090209     C                   KFLD                    KNSP
210200090209      *
210300090209     C     KVAB2         KLIST
210400090209     C                   KFLD                    KCCM
210500090209     C                   KFLD                    KCMR
210600090209      *
210700090209     C     KRDE          KLIST
210800090209     C                   KFLD                    KAAS
210900090209     C                   KFLD                    KLNP1
211000090209     C                   KFLD                    KNRS
211100090209     C                   KFLD                    KNSP
211200090209     C                   KFLD                    KNMC
211300090209     C                   KFLD                    KNRP
211400090209      *
211500090209     C     KACO          KLIST
211600090209     C                   KFLD                    KKUT
211700090209     C                   KFLD                    KKCC
211800090209     C                   KFLD                    KKSC
211900090209      *
212000090209     C     KIND          KLIST
212100090209     C                   KFLD                    KKUT
212200090209     C                   KFLD                    KKCC
212300090209     C                   KFLD                    KKSC
212400090209      *
212500090209      * DETERMINO SE SONO BARTOLINI O SDI
212600090209     C                   CLEAR                   TIBS55DS
212700090209     C                   MOVEL     'L'           I50TLA
212800090209     C                   CALL      'TIBS55R'
212900090209     C                   PARM                    TIBS55DS
213000090209      *
213100090209     C* RECUPERO DATI DELL'UTENTE
213200090209     C                   Z-ADD     1             CODUT             1 0
213300090209     C                   CALL      'XPARUT'
213400090209     C                   PARM                    UTEDSE0F
213500090209     C                   MOVEL     RAGUT         RSUT             20
213600090209     C*
213700090209     C* RICERCA CAPOCONTI
213800090209     C                   DO        50            X
213900090209     C                   MOVE      TCU(X)        TCUDS
214000090209     C     F56           IFEQ      'CG'
214100090209     C     F34           ANDEQ     '01'
214200090209     C                   Z-ADD     KCU(X)        KCI               4 0
214300090209     C                   END
214400090209     C                   END
214500090209     C*
214600090211     c                   movel(p)  'EDPAB'       User_Msg         10
214700090211     C*
214800090209     C* IMPOSTO A ZERO VARIABILI DI PGM
214900090209     c                   move      *blank        Snd_Msg_alert     1
215000090209     C                   MOVEL     CA(1)         WCA              78
215100090209     C                   MOVEL     CN(1)         WCN              78
215200090209     C*
215300090209     C* RECUPERO DATA E ORA
215400090209     C                   TIME                    WHHDAT           14 0
215500090209     C                   MOVE      WHHDAT        G02DAT
215600090209     C                   MOVE      WHHDAT        WDATA8            8 0
215700090209     C                   MOVE      WDATA8        WAA2              2 0
215800090209     C                   MOVEL     WDATA8        DATA              6 0
215900090209     C                   MOVE      WAA2          DATA
216000090209     C                   MOVEL     *BLANK        G02ERR
216100090209     C                   CALL      'XSRDA8'
216200090209     C                   PARM                    WLBDA8
216300090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
216400090209     C                   TIME                    WORA              6 0
216500090209      * Recupero data e ora
216600090209     C                   TIME                    W0140
216700090209      * UDATE IN GGMMAAAA
216800090209     C                   MOVE      W0140         WDTGIO
216900090209      * UDATE IN AAAAMMGG
217000090209     C     *eur          MOVEL     WDTGIO        DATA_eur
217100090209     C     *iso          MOVEL     DATA_eur      dateu
217200090209      *
217300090209      * ?              /*--------------------------- */
217400090209     c                   exsr      CARICA_TABELLE
217500090209      * ?              /*--------------------------- */
217600090209      *
217700090209      *
217800090209     C                   MOVEL     *ALL'-'       CMP132          132
217900090209     C                   CLEAR                   EDiVAB00
218000090211     C                   clear                   vablnp_PT
218100090212     C                   clear                   vabnrs_PT
218200090211     C                   clear                   vabctm_PT
218300090211     C                   clear                   VABfnsp_PT
218400090209     C                   MOVEA     *HIVAL        SGN
218500991124      *
218600050414      *------------------
218700991124     C                   ENDSR
218800060621      * ?------------------------------------------------------------------ */
218900090209      *?    CARICA TABELLE SU SCHIERE
219000060621      * ?------------------------------------------------------------------ */
219100090209     C     CARICA_TABELLEBEGSR
219200090205      *
219300090209     C* Caricamento Tabella 1B
219400090209     C                   Z-ADD     0             X                 4 0
219500090209     C                   Z-ADD     CODUT         KKUT
219600090209     C                   MOVEL     '1B'          KCOD
219700090209     C     KTAB1         CHAIN     TABEL00F                           30
219800090209     C     *IN30         DOWEQ     '0'
219900090209     C     TBLFLG        IFEQ      *BLANKS
220000090209     C                   ADD       1             X
220100090209     C                   MOVEL     TBLKEY        CTM(X)
220200090209     C                   MOVEL     TBLUNI        DTM(X)
220300090209     C                   END
220400090209     C     KTAB1         READE     TABEL00F                               30
220500090209     C                   END
220600090209      *
220700090209     C* Caricamento Tabella 15
220800090209     C                   Z-ADD     0             X                 4 0
220900090209     C                   Z-ADD     CODUT         KKUT
221000090209     C                   MOVEL     '15'          KCOD
221100090209     C     KTAB1         CHAIN     TABEL00F                           30
221200090209     C     *IN30         DOWEQ     '0'
221300090209     C     TBLFLG        IFEQ      *BLANKS
221400090209     C                   ADD       1             X
221500090209     C                   MOVEL     TBLKEY        C15(X)
221600090209     C                   MOVEL     TBLUNI        DS15
221700090209     C                   MOVEL     §15COD        ISO(X)
221800090209     C                   MOVEL     §15ECF        CPF(X)
221900090209     C                   MOVE      §15PCF        CPF(X)
222000090209     C                   END
222100090209     C     KTAB1         READE     TABEL00F                               30
222200090209     C                   END
222300090209      *
222400090209     C* Caricamento Tabella CV
222500090209     C                   Z-ADD     0             X
222600090209     C                   Z-ADD     CODUT         KKUT
222700090209     C                   MOVEL     'CV'          KCOD
222800090209     C     KTAB1         CHAIN     TABEL00F                           30
222900090209     C     *IN30         DOWEQ     '0'
223000090209     C     TBLFLG        IFEQ      *BLANKS
223100090209     C                   ADD       1             X
223200090209     C                   MOVEL     TBLKEY        CCV(X)
223300090209     C                   MOVEL     TBLUNI        DCV(X)
223400090209     C                   END
223500090209     C     KTAB1         READE     TABEL00F                               30
223600090209     C                   END
223700090216      *
223800110328      * Caricamento Tabella termini consegna
223900110328     C                   Z-ADD     0             X
224000110328     C                   MOVEL     'TB'          TABCOD
224100110328     C     TABCOD        CHAIN     EDTAB01L                           30
224200110328     C     *IN30         DOWEQ     '0'
224300110328     C     TABFLG        IFEQ      *BLANKS
224400110328     C                   ADD       1             X
224500110328     C                   eval      K35_TpBolla(X) = TABKEY
224600110328     C                   eval      Cod_TpBolla(X) = TABKEY
224700110328     C                   eval      Decod_Porto(X) = TABUNI
224800110328     C                   END
224900110328     C     TABCOD        READE     EDTAB01L                               30
225000110328     C                   END
225100110328      *
225200090216     C* Caricamento Tabella Tipo Incasso C/Assegno
225300090216     C                   Z-ADD     0             X
225400090216     C                   MOVEL     'TC'          TABCOD
225500090216     C     TABCOD        CHAIN     EDTAB01L                           30
225600090216     C     *IN30         DOWEQ     '0'
225700090216     C     TABFLG        IFEQ      *BLANKS
225800090216     C                   ADD       1             X
225900090216     C                   MOVEL     TABKEY        CTC35(X)
226000090216     C                   MOVEL     TABUNI        TPI(X)
226100090216     C                   END
226200090216     C     TABCOD        READE     EDTAB01L                               30
226300090216     C                   END
226400090209      *
226500090209      * Caricamento Tabella Partner esteri
226600090209     C                   Z-ADD     0             X
226700090209     C                   MOVEL     'PT'          TABCOD
226800090209     C     TABCOD        CHAIN     EDTAB01L                           30
226900090209     C     *IN30         DOWEQ     '0'
227000090209      *
227100090209     C                   SELECT
227200090209     C     TABFLG        WHENNE    *BLANKS
227300090209      *
227400090209     C                   OTHER
227500090209     C                   ADD       1             X
227600090209     C                   MOVEL     TABKEY        CPT(X)
227700090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
227800090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
227900090209     C                   MOVEL     TABUNI        DPT(X)
228000090209     C                   ENDSL
228100090209      *
228200090209     C     TABCOD        READE     EDTAB01L                               30
228300090209     C                   END
228400121105      *
228500121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
228600121105     c                   exsr      ChecK_PT
228700121105      *
228800090209      * salva quanti Codici UNB ha caricato
228900090209     c                   z-add     x             sav_CPTx
229000090209      *
229100090209     C                   MOVEL     'PU'          TABCOD
229200090209     C     TABCOD        CHAIN     EDTAB01L                           30
229300090209     C     *IN30         DOWEQ     '0'
229400090209      *
229500090209     C                   SELECT
229600090209     C     TABFLG        WHENNE    *BLANKS
229700090209      *
229800090209     C                   OTHER
229900090209     C                   MOVEL     TABKEY        CODPU            35
230000090209     C                   Z-ADD     1             X
230100090209     C     CODPU         LOOKUP    CPT(X)                                 32
230200090209     C   32              MOVEL     TABUNI        DPU(X)
230300090209     C                   ENDSL
230400090209      *
230500090209     C     TABCOD        READE     EDTAB01L                               30
230600090209     C                   END
230700090209      *
230800090209      * Prosegue tab.PT e PU
230900090209     C                   MOVEL     'PV'          TABCOD
231000090209     C     TABCOD        CHAIN     EDTAB01L                           30
231100090209     C     *IN30         DOWEQ     '0'
231200090209      *
231300090209     C                   SELECT
231400090209     C     TABFLG        WHENNE    *BLANKS
231500090209      *
231600090209     C                   OTHER
231700090209     C                   MOVEL     TABKEY        CODPV            35
231800090209     C                   Z-ADD     1             X
231900090209     C     CODPV         LOOKUP    CPT(X)                                 32
232000090209     C   32              MOVEL     TABUNI        DPV(X)
232100090209     C                   ENDSL
232200090209      *
232300090209     C     TABCOD        READE     EDTAB01L                               30
232400090209     C                   END
232500090209      *
232600090209     C* Caricamento Tabella codici clienti - tariffe
232700090209     C                   Z-ADD     0             X
232800090209     C                   MOVEL     'CL'          TABCOD
232900090209     C     TABCOD        CHAIN     EDTAB01L                           30
233000090209     C     *IN30         DOWEQ     '0'
233100090209     C     TABFLG        IFEQ      *BLANKS
233200090209     C                   ADD       1             X
233300090209     C                   MOVEL     TABKEY        CCL(X)
233400090209     C                   MOVEL     TABUNI        DCL(X)
233500090209     C                   END
233600090209     C     TABCOD        READE     EDTAB01L                               30
233700090209     C                   END
233800090209      *
233900090209     C* Caricamento Serivizi speciali
234000090209     C                   Z-ADD     0             X
234100090209     C                   MOVEL     'SS'          TABCOD
234200090209     C     TABCOD        CHAIN     EDTAB01L                           30
234300090209     C     *IN30         DOWEQ     '0'
234400090209     C     TABFLG        IFEQ      *BLANKS
234500090209     C                   ADD       1             X
234600090209     C                   MOVEL     TABKEY        SS(X)
234700090209     C                   MOVEL     TABKEY        SS35(X)
234800090209     C                   MOVEL     TABUNI        DSS(X)
234900090209     C                   END
235000090209     C     TABCOD        READE     EDTAB01L                               30
235100090209     C                   END
235200090209      *
235300090209     C                   ENDSR
235400090209      * ?------------------------------------------------------------------ */
235500090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
235600090209      * ?------------------------------------------------------------------ */
235700090209     C     MSG_Errato    BEGSR
235800090209      *
235900090205      * ?  Scrive su tutti i records il tipo di errore
236000090205     c     *start        setll     tivin00r
236100090205     c                   read      tivin00r
236200091016     c                   if        %Eof(tivin00r)
236300091016     c                   move      'S'           EoFTivin
236400091016     c                   end
236500090205     c                   dow       not %eof(tivin00r)
236600090211     c                   if        vinMSG  = *blank
236700090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
236800090210     C                               >> Controllare i DATI su UPLOAD !!'
236900090211     c                   end
237000090205     C                   eval      vinFLG = '2'
237100090213     c                   eval      Almeno_Un_Due ='S'
237200090205     c                   eval      esito  = '2'
237300090209     c                   eval      se_errore ='S'
237400090213     c                   eval      err_bloccante ='S'
237500091016     c                   if        EoFTivin = ' '
237600090205     c                   update    tivin000
237700091016     c                   end
237800090205     c                   read      tivin00r
237900091016     c                   if        %Eof(tivin00r)
238000091016     c                   move      'S'           EoFTivin
238100091016     c                   end
238200090205     c                   endDO
238300090205      *
238400090205     C                   ENDSR
238500090213      * ?------------------------------------------------------------------ */
238600090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
238700090213      * ?------------------------------------------------------------------ */
238800090213     C     DETta_Errato  BEGSR
238900090213      *
239000090213      *  rilascia il record prima di rieseguire il ciclo
239100090213     c                   update    tivin000
239200090213      *
239300090213      * ?  Scrive su tutti i records il tipo di errore
239400090213     c     SavNUM_RECord setll     tivin00r
239500090213     c                   read      tivin00r
239600091016     c                   if        %Eof(tivin00r)
239700091016     c                   move      'S'           EoFTivin
239800091016     c                   end
239900090213     c                   dow       not %eof(tivin00r)
240000090213      *
240100090213      *
240200090213     c                   if        vinMSG  = *blank
240300090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
240400090213     C                             >> Controllare i DATI di ogni Segmento <<'
240500090213     c                   end
240600090213     c                   exsr      Error_DET
240700090213      *
240800090213     c                   if        VNREC = NUM_RECord
240900090213     c                   leave
241000090213     c                   end
241100090213      *
241200091016     c                   if        EoFTivin = ' '
241300090213     c                   update    tivin000
241400091016     c                   end
241500090213     c                   read      tivin00r
241600091016     c                   if        %Eof(tivin00r)
241700091016     c                   move      'S'           EoFTivin
241800091016     c                   end
241900090213      *
242000090213     c                   endDO
242100090603      *
242200110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG'
242300090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
242400110511???? C                             Bolle AEG    non totalmente tradotto. -
242500110704     C                             Controllare UPLOAD del cliente :'
242600110704     C                   EVAL      Messaggio = %trim(Messaggio) +
242700110704     C                                         %editc(§PTksc:'X')
242800090603     c                   exsr      invio_mail_AB
242900090603      *
243000090213      *
243100090213     C                   ENDSR
243200090205      * ?------------------------------------------------------------------ */
243300090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
243400090205      * ?------------------------------------------------------------------ */
243500090205     C     *pssr         BEGSR
243600090205     C
243700060710     C                   eval      esito = '2'
243800110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG'
243900090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
244000100603     C                             Bolle import in filiale - messaggio EDI -
244100110704     C                             ricevuto errato su UPLOAD cliente :'
244200110704     C                   EVAL      Messaggio = %trim(Messaggio) +
244300110704     C                                         %editc(§PTksc:'X')
244400090603     c                   exsr      invio_mail_AB
244500060621     C
244600090213     C                   ENDSR
244700090213      * ?------------------------------------------------------------------ */
244800090213      *?      Segnala Errore su TIVIN
244900090213      * ?------------------------------------------------------------------ */
245000090213     C     Error_DET     BEGSR
245100090213     C
245200090213     C                   eval      vinFLG = '2'
245300090213     c                   eval      Almeno_Un_Due ='S'
245400090213     c                   eval      esito  = '1'
245500090213     c                   eval      se_errore ='S'
245600090213     C                   eval      Err_in_detta = 'S'
245700090213     C
245800090213     C                   ENDSR
245900090209     c*==================================================================*
246000090209     C*? CNVDAT - DECODIFICA LA DATA
246100090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
246200090209     C*         - WDATA  (DATA ALFANUMERICA)
246300090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
246400090209     C*         - WHMS   (ORA NUMERICA)
246500090209     C*----------------------------------------------------------------
246600090209     C     CNVDAT        BEGSR
246700090209     C*
246800090209     C                   MOVEL     ' '           WERRO             1
246900090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
247000090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
247100090209     C                   Z-ADD     *ZEROS        WHMS              6 0
247200090209     C*
247300090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
247400090209     C                   MOVEL     WDATA         WCOMO8            8
247500090209     C                   TESTN                   WCOMO8               99
247600090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
247700090209     C  N99              MOVEL     'S'           WERRO             1
247800090209     C                   END
247900090209     C*
248000090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
248100110523     C                   MOVEL     WDATA         WCOMO12          12
248200110523     C                   TESTN                   WCOMO12              99
248300110523     C                   MOVEl     *all'0'       WCAMGORA                       *DATA
248400110523     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
248500090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
248600090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
248700090209     C  N99              MOVEL     'S'           WERRO             1
248800090209     C                   END
248900110523     C*
249000110523     C     WFORM         IFEQ      '204'                                        CCYMMDD
249100110523     C                   MOVEL     WDATA         WCOMO14          14
249200110523     C                   TESTN                   WCOMO14              99
249300110523     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
249400110523     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
249500110523     C   99              MOVE      WCAMGORA      WHMS                           *DATA
249600110523     C  N99              MOVEL     'S'           WERRO             1
249700110523     C                   END
249800090209     C*
249900090209     C                   ENDSR
250000090210     C*----------------------------------------------------------------
250100090210     C*? TSTNUM - CONTROLLO NUMERICO
250200090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
250300090210     C*         - WLUNGH (Lunghezza campo output)
250400090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
250500090210     C*----------------------------------------------------------------
250600090210     C     TSTNUM        BEGSR
250700090210     C*
250800090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
250900090210     C                   MOVEL     'N'           WOKNUM            1
251000090210     C                   MOVEL     *BLANKS       WNUMOK           15
251100090210     C                   Z-ADD     15            IN                3 0
251200090210     C                   Z-ADD     WLUNGH        IX                3 0
251300090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
251400090210     C                   MOVEA     WNUM          NDA
251500090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
251600090210     C                   MOVEA     *ALL'0'       NDN
251700090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
251800090210     C                   Z-ADD     WLUNGH        I                 3 0
251900090210     C     I             DOWGT     0                                            --- 1 -->
252000090210     C* ESTRAE IL CARATTERE DA ESAMINARE
252100090210     C     1             SUBST     WNUM:I        WTEM01            1
252200090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
252300090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
252400090210     C     WTEM01        SCAN      WCA                                    99
252500090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
252600090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
252700090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
252800090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
252900090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
253000090210     C* DA DESTRA VERSO SINISTRA
253100090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
253200090210     C                   MOVEL     WTEMP1        NDN(IN)
253300090210     C                   SUB       1             IN
253400090210     C                   END                                                    <-- 3 ---
253500090210     C                   END                                                    <-- 2 ---
253600090210     C                   SUB       1             I
253700090210     C                   END                                                    <-- 1 ---
253800090210     C*
253900090210     C                   MOVEA     NDN           WNDN             15
254000090210     C     WNDN          IFNE      *ZEROS
254100090210     C                   MOVEA     NDN           WNUMOK           15
254200090210     C                   MOVEL     'S'           WOKNUM            1
254300090210     C                   END
254400090210     C*
254500090210     C                   ENDSR
254600090210     C*----------------------------------------------------------------
254700090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
254800090210     C*----------------------------------------------------------------
254900090210     C     CNVCAP        BEGSR
255000090210     C*
255100090210     C                   MOVEL     *BLANKS       WCAP              9
255200090210     C     nad3251       IFNE      '0'
255300090210     C     '  '          SCAN      nad3251       LENGHT            2 0
255400090210     C                   SUB       1             LENGHT
255500090210     C     LENGHT        IFLE      5
255600090210     C     LENGHT        ANDGT     0
255700090210     C                   Z-ADD     LENGHT        T                 2 0
255800090210     C                   Z-ADD     5             S                 2 0
255900090210     C                   MOVEA     nad3251       CAP9
256000090210     C                   MOVEL     *ZEROS        CAP5
256100090210     C                   DO        LENGHT
256200090210     C                   MOVE      CAP9(T)       CAP5(S)
256300090210     C                   SUB       1             S
256400090210     C                   SUB       1             T
256500090210     C                   END
256600090210     C                   MOVEA     CAP5          WCAP5             5
256700090210     C     WCAP5         IFEQ      '0'
256800090210     C                   MOVEL     *BLANKS       WCAP
256900090210     C                   ELSE
257000090210     C                   MOVEA     CAP5          WCAP
257100090210     C                   END
257200090210     C*
257300090210     C                   ELSE
257400090210     C                   MOVEL     nad3251       WCAP
257500090210     C                   END
257600090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
257700090210     C     WCAP          IFNE      *BLANKS
257800090210     C     WFLCPF        ANDNE     0
257900090210     C                   MOVEL     WFLCPF        WELE              1 0
258000090210     C                   MOVE      WFLCPF        WPOS              1 0
258100090210     C                   MOVEL     *BLANK        WCPF
258200090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
258300090210     C                   ELSE
258400090210     C                   MOVEL     *BLANKS       WCPF
258500090210     C                   END
258600090210     C                   END
258700090210     C*
258800090210     C                   ENDSR
258900090211      * ?================================================================== */
259000090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
259100090211      * ?================================================================== */
259200090211     C     Scrive_VAB    BEGSR
259300090211      *
259400090211     c                   clear                   err_bloccante     1
259500090211     c                   move      'S'           Almeno_Uno
259600090211      *
259700090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
259800090211     c                   exsr      UPDVAB
259900090211      *
260000090211     C* >>>>>>>>>>>>
260100090211     C*  Richiamo pgm per scrittura immediata VAB
260200090211     C     §PUWRK        IFEQ      ' '
260300090211      *
260400090211     C*  Richiamo pgm per esecuzione stampa
260500090211     C     §PTCMR        IFEQ      'S'
260600090213     C                   MOVEL     WNRDOC        d86CMR
260700090211     C                   ELSE
260800090213     C                   MOVEL     WNRDOC        d86CMR
260900090211     C                   END
261000090211      *
261100090213     C                   MOVEL     ' '           d86RIE
261200090213     C                   MOVEL     §PUDTA        d86DTA
261300090213     C                   Z-ADD     1             d86CNT
261400090213     C                   Z-ADD     §PTKSC        d86CCM
261500090213     C                   MOVEL     *BLANKS       d86STA
261600090213     C                   MOVEL     'E'           d86MOD
261700090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
261800090211      * dove poi imposta la DS dei parametri passati
261900090213     C                   MOVEL     'LR'          d86CHI
262000090213     C                   MOVEL     'N'           d86CTL
262100090211      *
262200090211     c                   move      kpjbu         SvKpjbu
262300090211     c                   clear                   kpjbu
262400090216     C                   movel     *on           Commit_on         1
262500090213     C                   MOVEL     TRTC86ds      KPJBU
262600090213      * ?- Chiamata la TRTC86R2 -*
262700090213     C                   CALL      'TRTC86R2'
262800090211     C                   PARM                    KPJBA
262900090216     C                   PARM                    Commit_on
263000090211     c                   move      Svkpjbu       Kpjbu
263100090211      *
263200090211     C*  Controllo pgm per scrittura immediata VAB
263300090211     C     WW            IFNE      0
263400090211     C                   DO        WW            WX                3 0
263500090213     C                   Z-ADD     KCL(WX)       d86CCM
263600090211      *
263700090211     c                   move      kpjbu         SvKpjbu
263800090211     c                   clear                   kpjbu
263900090216     C                   movel     *on           Commit_on         1
264000090213     C                   MOVEL     TRTC86ds      KPJBU
264100090213      * ?- Chiamata la TRTC86R2 -*
264200090213     C                   CALL      'TRTC86R2'
264300090211     C                   PARM                    KPJBA
264400090216     C                   PARM                    Commit_on
264500090211     c                   move      Svkpjbu       Kpjbu
264600090211     C                   END
264700090211     C                   END
264800090211      *
264900090211     C                   END
265000090211      * >>>>>>>>>>>>
265100090211     C                   if        se_errore  = 'S'
265200090211     c                   move      'S'           err_bloccante
265300090211     c                   goto      Error_VAB
265400090211     c                   end
265500090211     C*
265600090211     C     Error_VAB     Endsr
265700090211     C*----------------------------------------------------------------
265800090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
265900090211     C*----------------------------------------------------------------
266000090211     C     UPDVAB        BEGSR
266100090211      *
266200090211     C*  Se non è stato impostato il codice bolla lo imposto io
266300090211     C     VABCBO        IFEQ      *BLANKS
266400090211     C     VABCAS        IFGT      0
266500090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
266600090211     C                   ELSE
266700090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
266800090211     C                   END
266900090211     C                   END
267000110523      * Imposta le NOte
267100110523     C     VABNOT        IFEQ      *BLANKS
267200110523     C                   MOVEL     Wnot1         VABNOT
267300110523     C                   MOVEL     Wnot2         VABNT2
267400110523     C                   ELSE
267500110523     C                   MOVEL     Wnot1         VABNT2
267600110523     C                   END
267700090211      *
267800090211     C*  Attribuisco anno spedizione
267900090211     C     WDATPA        IFGT      0                                            anno sped.
268000090211     C                   MOVEL     WDATPA        VABAAS
268100090211     C                   ELSE
268200090211     C     WDATFV        IFGT      0
268300090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
268400090211     C                   END
268500090211     C                   END
268600090211      *
268700090211      *  Controllo dettaglio segnacolli
268800090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
268900090211     C                   EXSR      CTRSGE
269000090211     C                   ELSE
269100090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
269200090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
269300090211     C                   EXSR      CTRSGN
269400090211     C                   END
269500090211     C                   END
269600090211      *
269700090212      *  Imposto linea partenza e serie
269800090211     C                   eval      VABLNP = VABLNP_PT
269900090212     C                   eval      VABNRS = VABnrs_PT
270000090211      *
270100090211      *  Controllo se devo variare il codice trattamento merce
270200090211     C                   eval      VABCTM = VABctm_PT
270300090211      *
270400090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
270500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
270600090211     c                   clear                   mantiene_nsp      1            *blk/'S'
270700090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
270800090211      *
270900090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
271000090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
271100090211      *
271200090211      *  Imposto codice cliente
271300090211     c                   Select
271400090211      *
271500090211      *  Specificato codice da qualificatore FF (TD15)
271600090211     C                   When        WCLKSC <> *zeros
271700090211     C                   Z-ADD     WCLKSC        VABCCM
271800090211     C                   MOVEL     WCLTAR        VABCTR
271900090211      * forza LNP/CTM/NRS
272000090211     c                   if        WclLNP <> 0
272100090211     C                   eval      VABLNP = WclLNP
272200090211     c                   end
272300090211     c                   if        WclNRS <> 0
272400090211     C                   eval      VABNRS = WclNRS
272500090211     c                   end
272600090211     c                   if        WclCTM <> *blank
272700090211     C                   eval      VABCTM = WclCTM
272800090211     c                   end
272900090211      *
273000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
273100090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
273200090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
273300090211     c                   end
273400090211      *
273500090211      *  Specificato codice da qualificatore FF (TT15)
273600090211     C                   When        $CLKSC <> *zeros
273700090211     C                   Z-ADD     $CLKSC        VABCCM
273800090211     C                   MOVEL     $CLTAR        VABCTR
273900090211      * forza LNP/CTM/NRS
274000090211     c                   if        $clLNP <> 0
274100090211     C                   eval      VABLNP = $clLNP
274200090211     c                   end
274300090211     c                   if        $clNRS <> 0
274400090211     C                   eval      VABNRS = $clNRS
274500090211     c                   end
274600090211     c                   if        $clCTM <> *blank
274700090211     C                   eval      VABCTM = $clCTM
274800090211     c                   end
274900090211      *
275000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
275100090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
275200090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
275300090211     c                   end
275400090211      *
275500090211      *  Imposto dati da tabella - PT-
275600090211     C                   Other
275700090211      *
275800090211     C                   MOVEL     §PTKSC        VABCCM
275900090211     C                   MOVEL     §PTCTR        VABCTR
276000090211      *
276100090211     C                   Endsl
276200090211      *
276300090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
276400090211      *    o dalla relativa CL
276500090211     C                   MOVEL     VABlnp        VABfgs
276600090211      *
276700090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
276800090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
276900090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
277000090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
277100090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
277200090211      *
277300090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
277400090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
277500090211     c                   else
277600090211      *
277700090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
277800090211     C*  lo attribuisco io
277900090211      **              **------------------**
278000090211     c                   exsr      repnum
278100090211      **              **------------------**
278200090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
278300090211      **              **------------------**
278400090211     c                   end
278500090211      *
278600090211     C*  Altrimenti attribuisco data del giorno
278700090211     C     VABMGS        IFEQ      0
278800090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
278900090211     C                   MOVE      WOGGI         VABMGS                         = oggi
279000090211     C                   END
279100090211     C*  Eseguo posizionamento su EDiVAB
279200090211     C                   Z-ADD     VABAAS        KAAS
279300090211     C                   Z-ADD     VABlnp        KLNP
279400090211     C                   Z-ADD     VABNRS        KNRS
279500090211     C                   Z-ADD     VABNSP        KNSP
279600090211     C                   MOVEL     SAVBOL        SALVA
279700090211     C     KVAB1         CHAIN     EDiVAB1L                           30
279800090211     C                   MOVEL     SALVA         SAVBOL
279900090211     C*  Imposto dati CMR
280000090211     C                   Z-ADD     1             VABCNT
280100090211     **
280200090211      *** Attenzione se si deve trattare il VAX con il CMR
280300090211      *** il campo VABCNT deve essere sempre impostato a (1)
280400090211     C     §puWRK        ifEQ      'S'
280500090211     C     §puNSP        andEQ     'S'
280600090211     C                   Z-ADD     1             VABCNT
280700090211     c                   end
280800090211     **
280900090211     C                   Z-ADD     WDTPRE        VABDTS
281000090212     C                   movel     '20'          VABDTS
281100090212     C     WHMPRE        mult      100           VABHMS
281200090211     C     §PTCMR        IFEQ      'S'
281300090211     C                   Z-ADD     WDTCMR        VABDCM
281400090211     C                   MOVEL     WNRCMR        VABCMR
281500090211     C                   ELSE
281600090211     C                   Z-ADD     WDATFV        VABDCM
281700090211     C                   MOVEL     WNUMFV        VABCMR
281800090211     C                   END
281900090211     C*  Se non è indicato il nome del mittente = Partner mittente
282000090211     C     VABRMO        IFEQ      *BLANKS
282100090211     C                   MOVEL     ACORAG        VABRMO
282200090211     C                   MOVEL     INDCAP        VABCMO
282300090211     C     INDCAE        IFNE      *BLANKS
282400090211     C                   MOVEL     INDCAE        VABCMO
282500090211     C                   END
282600090211     C     INDSTA        IFNE      *BLANKS
282700090211     C                   MOVEL     INDSTA        VABNMO
282800090211     C                   END
282900090211     C                   END
283000090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
283100090211     C     §1BFG1        IFEQ      'N'
283200090211     C     §1BFG7        ANDEQ     'N'
283300090211     C     §1BFG2        ANDEQ     'N'
283400090211     C***                  Z-ADD0         VABNRS
283500090211     C                   END
283600090211     C*  Imposto segancollo da - a
283700090211     C     §1BFG1        IFEQ      'S'
283800090211     C     §1BFG7        ANDEQ     'N'
283900090211     C     §1BF12        ANDNE     'E'
284000090211      *
284100090211     C     XY            IFEQ      *ZEROS
284200090211     C                   CLEAR                   VABNCD
284300090211     C                   CLEAR                   VABNCA
284400090211     C                   ELSE
284500090211     C                   Z-ADD     SGN(1)        VABNCD
284600090211     C                   Z-ADD     SGN(XY)       VABNCA
284700090211     C                   ENDIF
284800090211     C*
284900090211     C                   END
285000090211     C*
285100090211     C*  Se cap mittente originale a blank abblenco nazione
285200090211     C     VABCMO        IFEQ      *BLANKS
285300090211     C                   MOVEL     *BLANKS       VABNMO
285400090211     C                   END
285500090603     C*
285600090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
285700090603     C* Se in testata
285800090603     c                   if        NAD_destinatario_in_testata = 'S'
285900090603     c                   eval         VABrsd =  tes_VABrsd
286000090603     c                   eval         VABrd2 =  tes_VABrd2
286100090603     c                   eval         VABind =  tes_VABind
286200090603     c                   eval         VABlod =  tes_VABlod
286300090603     c                   eval         VABnzd =  tes_VABnzd
286400090603     c                   end
286500090603     C* Se in testata
286600090603     c                   if        NAD_mittente_in_testata = 'S'
286700090603     c                   eval         VABrmo = tes_VABrmo
286800090603     c                   eval         VABnmo = tes_VABnmo
286900090603     c                   eval         VABcmo = tes_VABcmo
287000090603     c                   end
287100110718      *
287200110718      *
287300090211      *? ------------------------------------------------------------------------
287400090213     C   30              WRITE     EDiVAB00                             99
287500090213     C  N30              UPDATE    EDiVAB00                             99
287600090211      *? ------------------------------------------------------------------------
287700090213     c   99              eval      errori_in_Wrt = 'S'
287800090211     C*
287900090211     c                   clear                   wri_vabtic        1
288000090211     C*
288100090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
288200090211     C                   EXSR      WRTVAT_Rif
288300090211     C*
288400090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
288500090211     C     §1BF12        IFEQ      'E'
288600090211     C                   EXSR      WRTVAT
288700090211     C                   ELSE
288800090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
288900090211     C     §1BFG7        IFEQ      'S'
289000090211     C     §1BFG1        OREQ      'S'
289100090211     C                   EXSR      WRTVAD
289200090211     C                   END
289300090211     C                   END
289400090211      *
289500090211     C* Reimposto codice trattamento merce cliente
289600090211     C                   MOVEL     WSVTRM        DS1B
289700090211     C**
289800090211     C* Do errore se previsto CMR ma non c'è
289900090211     C     §PTCMR        IFNE      ' '
290000090211     C                   EXSR      MEMCMR
290100090211     C                   END
290200090211     C* Do errore se previsto AFB ma non c'è
290300090211     C     §PTNFV        IFNE      ' '
290400090211     C                   EXSR      MEMAFB
290500090211     C                   END
290600090211     C*  Se il cliente vuole il ritorno delle date di consegna
290700090211     C*  è c'è una squdratura fra i segnacolli e il numero
290800090211     C*  dei colli che ci ha passato scrivo EDSUM
290900090211     C     §PUDTA        IFEQ      'S'
291000090211     C     WSQUAD        ANDEQ     'S'
291100090211     C                   EXSR      WRTSUM
291200090211     C                   END
291300090211     C*
291400090211     C                   ENDSR
291500090211     C*----------------------------------------------------------------
291600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
291700090211     C*----------------------------------------------------------------
291800090211     C     CTRSGN        BEGSR
291900090211     C*
292000090211     C                   MOVEL     'N'           WNSGER            1
292100090211     C*
292200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
292300090211     C     VABNCL        IFNE      XY
292400090211     C                   MOVEL     'S'           WNSGER
292500090211     C                   MOVEL     'S'           WSQUAD
292600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
292700090211     c                              bollettati'
292800090211     C                   GOTO      FINSGN
292900090211     C                   END
293000090211     C*  Riordino i segnacolli
293100090211     C     §1BFG7        IFEQ      'N'
293200090211     C                   SORTA     SGN
293300090211     C*  Controllo se sono sequenziali
293400090211     C                   MOVEL     'N'           WNOSEQ            1
293500090211     C     SGN(1)        ADD       1             WEXSGN            7 0
293600090211     C     2             DO        XY            X
293700090211     C     SGN(X)        IFNE      WEXSGN
293800090211     C                   MOVEL     'S'           WNOSEQ
293900090211     C                   END
294000090211     C     SGN(X)        ADD       1             WEXSGN
294100090211     C                   END
294200090211     C                   END
294300090211     C*
294400090211     C     FINSGN        ENDSR
294500090211     C*----------------------------------------------------------------
294600090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
294700090211     C*----------------------------------------------------------------
294800090211     C     CTRSGE        BEGSR
294900090211     C*
295000090211     C                   MOVEL     'N'           WNSGER            1
295100090211      *
295200090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
295300090211      *
295400090211     C     VABNCL        IFNE      §§
295500090211     C                   MOVEL     'S'           WNSGER
295600090211     C                   MOVEL     'S'           WSQUAD
295700090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
295800090211     c                              bollettati'
295900090211     C                   END
296000090211      *
296100090211     C                   ENDSR
296200090211     C*----------------------------------------------------------------
296300090211     C*? ESEGUO SCRITTURA EDiVAD
296400090211     C*----------------------------------------------------------------
296500090211     C     WRTVAD        BEGSR
296600090211     C*
296700090211     C* Se non seq. scrivo xy record
296800090211     C     §1BFG1        IFEQ      'S'
296900090211     C     XY            ANDNE     *ZEROS
297000090211     C*
297100090211     C     §1BFG7        IFEQ      'S'
297200090211     C                   DO        XY            X
297300090211     C                   Z-ADD     VABCCM        KCCM
297400090211     C                   Z-ADD     SGN(X)        KNCD
297500090211     C                   Z-ADD     VABCNT        VADCNT
297600090211     C                   MOVEL     VABCMR        VADCMR
297700090211     C                   Z-ADD     VABAAS        VADAAS
297800090211     C                   Z-ADD     VABLNP        VADLNP
297900090211     C                   Z-ADD     VABNRS        VADNRS
298000090211     C                   Z-ADD     VABNSP        VADNSP
298100090211     C                   Z-ADD     1             VADNCL
298200090211     C                   MOVEL     SGN(X)        VADCDP
298300090211     C                   Z-ADD     SGN(X)        VADNCD
298400090211     C                   Z-ADD     *ZEROS        VADNCA
298500090211     C                   Z-ADD     VABCCM        VADCCM
298600090211     C                   movel     VABfgs        VADfgs
298700090213     C                   WRITE     EDiVAD00                             99
298800090213     c   99              eval      errori_in_Wrt = 'S'
298900090211     C                   END
299000090211     C* Se sequenziali scrivo un solo record
299100090211     C                   ELSE
299200090211     C                   Z-ADD     VABAAS        VADAAS
299300090211     C                   Z-ADD     VABLNP        VADLNP
299400090211     C                   Z-ADD     VABNRS        VADNRS
299500090211     C                   Z-ADD     VABNSP        VADNSP
299600090211     C                   Z-ADD     VABNCL        VADNCL
299700090211     C                   Z-ADD     SGN(1)        VADNCD
299800090211     C                   Z-ADD     SGN(XY)       VADNCA
299900090211     C                   Z-ADD     VABCCM        VADCCM
300000090211     C                   movel     VABfgs        VADfgs
300100090213     C                   WRITE     EDiVAD00                             99
300200090213     c   99              eval      errori_in_Wrt = 'S'
300300090211     C                   END
300400090211     C*
300500090211     C                   ELSE
300600090211     C                   DO        XY            X
300700090211     C                   Z-ADD     VABCCM        KCCM
300800090211     C                   Z-ADD     SGN(X)        KNCD
300900090211     C                   Z-ADD     VABAAS        VADAAS
301000090211     C                   Z-ADD     VABLNP        VADLNP
301100090211     C                   Z-ADD     VABNRS        VADNRS
301200090211     C                   Z-ADD     VABNSP        VADNSP
301300090211     C                   MOVEL     SGN(X)        VADCDP
301400090211     C                   Z-ADD     1             VADNCL
301500090211     C                   Z-ADD     *ZEROS        VADNCD
301600090211     C                   Z-ADD     *ZEROS        VADNCA
301700090211     C                   Z-ADD     VABCCM        VADCCM
301800090211     C                   movel     VABfgs        VADfgs
301900090213     C                   WRITE     EDiVAD00                             99
302000090213     c   99              eval      errori_in_Wrt = 'S'
302100090211     C                   END
302200090211     C*
302300090211     C                   END
302400090211     C*
302500090211     C                   ENDSR
302600090211     C*----------------------------------------------------------------
302700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
302800090211     C*----------------------------------------------------------------
302900090211     C     WRTVAT_Rif    BEGSR
303000090211      *
303100090211      *  riferimento di consegna destinatario
303200090211     c                   if        WatRDE <> *blank
303300090211     C                   Z-ADD     VABCNT        VATCNT
303400090211     C                   MOVEL     VABCMR        VATCMR
303500090211     C                   Z-ADD     VABCCM        VATCCM
303600090211     C                   movel     VABfgs        VATfgs
303700090211     C                   Z-ADD     VABLNP        VATLNP
303800090211     C                   Z-ADD     VABAAS        VATAAS
303900090211     C                   Z-ADD     VABNRS        VATNRS
304000090211     C                   Z-ADD     VABNSP        VATNSP
304100090211     C                   MOVEL     'A'           VATTRC
304200090211     C                   MOVEL     WatRDE        VATNOT
304300090213     C                   WRITE     EDiVAT00                             99
304400090213     c   99              eval      errori_in_Wrt = 'S'
304500090211     c                   end
304600090211      *
304700090211      *  riferimento di consegna telefonico
304800090211     c                   if        WatTEL <> *blank
304900090211     C                   Z-ADD     VABCNT        VATCNT
305000090211     C                   MOVEL     VABCMR        VATCMR
305100090211     C                   Z-ADD     VABCCM        VATCCM
305200090211     C                   movel     VABfgs        VATfgs
305300090211     C                   Z-ADD     VABLNP        VATLNP
305400090211     C                   Z-ADD     VABAAS        VATAAS
305500090211     C                   Z-ADD     VABNRS        VATNRS
305600090211     C                   Z-ADD     VABNSP        VATNSP
305700090211     C                   MOVEL     'B'           VATTRC
305800090211     C                   MOVEL     WatTEL        VATNOT
305900090213     C                   WRITE     EDiVAT00                             99
306000090213     c   99              eval      errori_in_Wrt = 'S'
306100090211     c                   end
306200090211      *
306300090211      * riferimento del Partner che una volta veniva riportato sul BL4
306400090211     c                   if        Wnsp   <> *blank
306500090211     C                   Z-ADD     VABCNT        VATCNT
306600090211     C                   MOVEL     VABCMR        VATCMR
306700090211     C                   Z-ADD     VABCCM        VATCCM
306800090211     C                   movel     VABfgs        VATfgs
306900090211     C                   Z-ADD     VABLNP        VATLNP
307000090211     C                   Z-ADD     VABAAS        VATAAS
307100090211     C                   Z-ADD     VABNRS        VATNRS
307200090211     C                   Z-ADD     VABNSP        VATNSP
307300090211     C                   MOVEL     'C'           VATTRC
307400090211     C                   MOVEL     Wnsp          VATNOT
307500090213     C                   WRITE     EDiVAT00                             99
307600090213     c   99              eval      errori_in_Wrt = 'S'
307700090211     c                   end
307800090211      *
307900090211     C                   ENDSR
308000090211     C*----------------------------------------------------------------
308100090211     C*? ESEGUO SCRITTURA EDiVAT
308200090211     C*----------------------------------------------------------------
308300090211     C     WRTVAT        BEGSR
308400090211      *
308500090211     C                   DO        §§            X
308600090211     C                   Z-ADD     VABCNT        VATCNT
308700090211     C                   MOVEL     VABCMR        VATCMR
308800090211     C                   Z-ADD     VABCCM        VATCCM
308900090211     C                   movel     VABfgs        VATfgs
309000090211     C                   Z-ADD     VABLNP        VATLNP
309100090211     C                   Z-ADD     VABAAS        VATAAS
309200090211     C                   Z-ADD     VABNRS        VATNRS
309300090211     C                   Z-ADD     VABNSP        VATNSP
309400090211     C                   MOVEL     'E'           VATTRC
309500090211      *
309600090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
309700090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
309800090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
309900090211     c                   if        WVATlun > 0
310000090211      *
310100090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
310200090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
310300090211      *
310400090211     c                   else
310500090211      *
310600090211      * Se non è impostato nulla prende il segnacollo passato così com'è
310700090211     C                   MOVEL     SGE(X)        VATNOT
310800090211     c                   end
310900090211      *
311000090213     C                   WRITE     EDiVAT00                             99
311100090213     c   99              eval      errori_in_Wrt = 'S'
311200090211     C                   END
311300090211      *
311400090211     C                   ENDSR
311500090211     C*----------------------------------------------------------------
311600090211     C*? ESEGUO SCRITTURA EDSUM
311700090211     C*----------------------------------------------------------------
311800090211     C     WRTSUM        BEGSR
311900090211     C*
312000090211     C* Se non seq. scrivo xy record
312100090211     C                   DO        XY            X
312200090211     C                   Z-ADD     VABAAS        SUMAAS
312300090211     C                   Z-ADD     VABLNP        SUMFLS
312400090211     C                   MOVEL     WKSC          SUMLNP
312500090211     C                   Z-ADD     VABLNA        SUMLNA
312600090211     C                   Z-ADD     VABZNC        SUMZNC
312700090211     C                   Z-ADD     VABNRS        SUMNRS
312800090211     C                   Z-ADD     VABNSP        SUMNSP
312900090211     C                   Z-ADD     SGN(X)        SUMNSC
313000090211     C                   Z-ADD     WKSC          SUMCCM
313100090211     C                   MOVEL     *BLANKS       SUMSTS
313200090211     C                   MOVEL     *BLANKS       SUMFLG
313300090211     C                   MOVEL     WPRGSP        SUMCNI
313400090211     C                   MOVEL     VABCMR        SUMCMR
313500090211     C                   Z-ADD     0             SUMNFE
313600090211     C                   Z-ADD     VABCCM        VADCCM
313700090213     C                   WRITE     EDSUM000                             99
313800090213     c   99              eval      errori_in_Wrt = 'S'
313900090211     C                   END
314000090211     C*
314100090211     C                   ENDSR
314200090211     c*==================================================================*
314300090211      * Manda un Msg in Posta AS Sede a EDP*
314400090211     c*==================================================================*
314500090211     c     SEND_MSG_EDP  begsr
314600090211      *
314700090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
314800090211      *   Lo manda una sola volta per lavoro
314900090211     c                   IF        Snd_Msg_alert  <> 'N' and
315000090211     c                             User_msg <> *blank
315100090211     c                   z-add     1             Jx
315200090211     C                   exsr      CalQEZSNDMG
315300090211     c                   end
315400090211      *
315500090211     c                   endsr
315600090213     c**********************************************************************
315700090213     c* reperimento numero Spedizione
315800090213     c**********************************************************************
315900090213     C     repnum        BEGSR
316000090213      *
316100090213      *    deve controllare sulla tabella "3C" se il numero spedizione
316200090213      *     deve essere mantenuto oppure no
316300090213     C*
316400090213     C                   clear                   Ds3C
316500090213     C                   Z-ADD     CODUT         TBLKUT
316600090213     C                   MOVEL     '3C'          TBLCOD
316700090213     C                   movel(p)  VABccm        TBLKEY
316800090213     C     KTABel        CHAIN     TABEL00F                           30
316900090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
317000090213     C                   MOVEL     TBLUNI        Ds3C
317100090213     C                   END
317200090213      *
317300090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
317400090213     c                   if        §3CFsp <> 'S'
317500090213      *
317600090213     C* NSP => Stacco un numeratore da AZNUM
317700090213     c                   movel     kpjbu         svkpjbu
317800090213     c                   clear                   kpjbu
317900090213     C                   clear                   TRUL33DS
318000090213     C                   eval      I33OPE = *zeros
318100090213     C                   eval      I33CNU = 302
318200090213     C                   eval      I33NUM = 1
318300090213     C                   movel     TRUL33DS      KPJBU
318400090213     C                   call      'TRUL33R'
318500090213     C                   parm                    KPJBA
318600090213     C                   movel     KPJBU         TRUL33DS
318700090213     c                   movel     svkpjbu       kpjbu
318800090213      ****
318900090213     C                   if        O33ERR = *zeros
319000090213     c                   z-add     o33nrf        NUM_SPED
319100090213     C                   else
319200090213     C                   endif
319300090213      ****
319400090213     c                   ELSE
319500090213      *   se si vuole mantenere il numero spedizione
319600090213      ****
319700090213     C                   MOVEL     WOGGI         WANNO             4 0
319800090213     C                   MOVE      WANNO         KAAA
319900090213     C                   Z-ADD     3             KCNU
320000090213     C                   Z-ADD     VABlnp        KFIL
320100090213     C     KNUF          CHAIN     FLNUF                              9999
320200090213     C     *IN99         IFEQ      '0'
320300090213     C                   ADD       1             NUFNUM
320400090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
320500090213     C                   UPDATE    FLNUF
320600090213     C                   ELSE
320700090213     C                   END
320800090213     C*
320900090213     c                   EndIF
321000090213      **
321100090213     C                   ENDSR
321200090213     C*----------------------------------------------------------------
321300090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
321400090213     C*----------------------------------------------------------------
321500090213     C     MEMCMR        BEGSR
321600090213     C*
321700090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
321800090213     C     §PTCM1        IFEQ      'S'
321900090213     C                   CLEAR                   EDRDE000
322000090213     C                   MOVEL     'TD1154'      KNMC
322100090213     C                   Z-ADD     1             KNRP
322200090213     C                   MOVEL     WKSC          KLNP1
322300090213     C     KRDE          CHAIN     EDRDE01L                           31
322400090213     C     *IN31         IFEQ      '1'
322500090213     C                   Z-ADD     VABAAS        RDEAAS
322600090213     C                   Z-ADD     VABLNP        RDELNP
322700090213     C                   Z-ADD     VABNRS        RDENRS
322800090213     C                   Z-ADD     VABNSP        RDENSP
322900090213     C                   MOVEL     'TD1154'      RDENMC
323000090213     C                   Z-ADD     1             RDENRP
323100090213     C                   MOVEL     WNRCMR        RDEVAL
323200090213     C                   WRITE     EDRDE000                             99
323300090213     c   99              eval      errori_in_Wrt = 'S'
323400090213     C                   END
323500090213     C*  Memorizzo qualificatore CMR
323600090213     C                   CLEAR                   EDRDE000
323700090213     C                   MOVEL     'TD1153'      KNMC
323800090213     C                   Z-ADD     1             KNRP
323900090213     C                   MOVEL     WKSC          KLNP1
324000090213     C     KRDE          CHAIN     EDRDE01L                           31
324100090213     C     *IN31         IFEQ      '1'
324200090213     C                   Z-ADD     VABAAS        RDEAAS
324300090213     C                   Z-ADD     VABLNP        RDELNP
324400090213     C                   Z-ADD     VABNRS        RDENRS
324500090213     C                   Z-ADD     VABNSP        RDENSP
324600090213     C                   Z-ADD     1             RDENRP
324700090213     C                   MOVEL     'TD1153'      RDENMC
324800090213     C                   MOVEL     'CMR'         RDEVAL
324900090213     C                   WRITE     EDRDE000                             99
325000090213     c   99              eval      errori_in_Wrt = 'S'
325100090213     C                   END
325200090213     C*  Memorizzo la data
325300090213     C                   CLEAR                   EDRDE000
325400090213     C                   MOVEL     'TD2380'      KNMC
325500090213     C                   Z-ADD     1             KNRP
325600090213     C                   MOVEL     WKSC          KLNP1
325700090213     C     KRDE          CHAIN     EDRDE01L                           31
325800090213     C     *IN31         IFEQ      '1'
325900090213     C                   Z-ADD     VABAAS        RDEAAS
326000090213     C                   Z-ADD     VABLNP        RDELNP
326100090213     C                   Z-ADD     VABNRS        RDENRS
326200090213     C                   Z-ADD     VABNSP        RDENSP
326300090213     C                   Z-ADD     1             RDENRP
326400090213     C                   MOVEL     'TD2380'      RDENMC
326500090213     C                   MOVEL     WDTCMR        RDEVAL
326600090213     C                   WRITE     EDRDE000                             99
326700090213     c   99              eval      errori_in_Wrt = 'S'
326800090213     C                   END
326900090213     C*
327000090213     C                   END                                                    §PTCM1 = 'S'
327100090213     C*
327200090213     C                   ENDSR
327300090213     C*----------------------------------------------------------------
327400090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
327500090213     C*----------------------------------------------------------------
327600090213     C     MEMAFB        BEGSR
327700090213     C*
327800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
327900090213     C     §PTNF1        IFEQ      'S'
328000090213     C                   CLEAR                   EDRDE000
328100090213     C                   MOVEL     WKSC          KLNP1
328200090213     C                   MOVEL     'TD1154'      KNMC
328300090213     C                   Z-ADD     1             KNRP
328400090213     C     KRDE          CHAIN     EDRDE01L                           31
328500090213     C     *IN31         IFEQ      '1'
328600090213     C                   Z-ADD     VABAAS        RDEAAS
328700090213     C                   Z-ADD     VABLNP        RDELNP
328800090213     C                   Z-ADD     VABNRS        RDENRS
328900090213     C                   Z-ADD     VABNSP        RDENSP
329000090213     C                   MOVEL     'TD1154'      RDENMC
329100090213     C     §PTCM1        IFNE      'S'
329200090213     C     WNRCMR        OREQ      *BLANKS
329300090213     C                   Z-ADD     1             RDENRP
329400090213     C                   ELSE
329500090213     C                   Z-ADD     2             RDENRP
329600090213     C                   END
329700090213     C                   MOVEL     WNUMFV        RDEVAL
329800090213     C                   WRITE     EDRDE000                             99
329900090213     c   99              eval      errori_in_Wrt = 'S'
330000090213     C                   END
330100090213     C*  Memorizzo qualificatore CMR
330200090213     C                   CLEAR                   EDRDE000
330300090213     C                   MOVEL     WKSC          KLNP1
330400090213     C                   MOVEL     'TD1153'      KNMC
330500090213     C                   Z-ADD     1             KNRP
330600090213     C     KRDE          CHAIN     EDRDE01L                           31
330700090213     C     *IN31         IFEQ      '1'
330800090213     C                   Z-ADD     VABAAS        RDEAAS
330900090213     C                   Z-ADD     VABLNP        RDELNP
331000090213     C                   Z-ADD     VABNRS        RDENRS
331100090213     C                   Z-ADD     VABNSP        RDENSP
331200090213     C     §PTCM1        IFNE      'S'
331300090213     C     WNRCMR        OREQ      *BLANKS
331400090213     C                   Z-ADD     1             RDENRP
331500090213     C                   ELSE
331600090213     C                   Z-ADD     2             RDENRP
331700090213     C                   END
331800090213     C                   MOVEL     'TD1153'      RDENMC
331900090213     C                   MOVEL     'AFB'         RDEVAL
332000090213     C                   WRITE     EDRDE000                             99
332100090213     c   99              eval      errori_in_Wrt = 'S'
332200090213     C                   END
332300090213     C*  Memorizzo la data
332400090213     C                   CLEAR                   EDRDE000
332500090213     C                   MOVEL     WKSC          KLNP1
332600090213     C                   MOVEL     'TD2380'      KNMC
332700090213     C                   Z-ADD     1             KNRP
332800090213     C     KRDE          CHAIN     EDRDE01L                           31
332900090213     C     *IN31         IFEQ      '1'
333000090213     C                   Z-ADD     VABAAS        RDEAAS
333100090213     C                   Z-ADD     VABLNP        RDELNP
333200090213     C                   Z-ADD     VABNRS        RDENRS
333300090213     C                   Z-ADD     VABNSP        RDENSP
333400090213     C     §PTCM1        IFNE      'S'
333500090213     C     WNRCMR        OREQ      *BLANKS
333600090213     C                   Z-ADD     1             RDENRP
333700090213     C                   ELSE
333800090213     C                   Z-ADD     2             RDENRP
333900090213     C                   END
334000090213     C                   MOVEL     'TD2380'      RDENMC
334100090213     C                   MOVEL     WDATFV        RDEVAL
334200090213     C                   WRITE     EDRDE000                             99
334300090213     c   99              eval      errori_in_Wrt = 'S'
334400090213     C                   END
334500090213     C*
334600090213     C                   END                                                    §PTCM1 = 'S'
334700090213     C*
334800090213     C                   ENDSR
334900090213     c*==================================================================*
335000090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
335100090213     c*==================================================================*
335200090213     c     Imp_ADDR_mail begsr
335300090213      *
335400090213     c                   clear                   Indirizzi
335500090213      *
335600090213      *  Lunghezza indirizzi presenti
335700090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
335800090213     c                   z-add     1             al_char           3 0
335900090213     c                   z-add     1             dal_char          3 0
336000090213     c                   z-add     1             tot_char          3 0
336100090213      *
336200090213     c     successivo    tag
336300090213      *
336400090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
336500090213      *   e aggiunge il dominio Bartolini + il (;) separatore
336600090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
336700090213     c                   eval      tot_char = al_char - dal_char
336800090213     c                   z-add     dal_char      Dac
336900090213     c                   z-add     tot_char      Luc
337000090213      *
337100090213     c                   clear                   Indir_Bartol     40
337200090213     c                   clear                   Indir_Parz       20
337300090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
337400090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
337500090213     c                             %Trim(bart_it)
337600090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
337700090213      *
337800090213     c                   if        al_char = Lun_Ind
337900090213     c                   goto      fine_indmail
338000090213     c                   else
338100090213     c                   eval      dal_char = ( al_char + 1 )
338200090213     c                   goto      successivo
338300090213     c                   end
338400090213      *
338500090213     C     fine_indmail  TAG
338600090213      *
338700090213     C                   ENDSR
338800090213     c*==================================================================*
338900090213      * Manda un Msg x E-mail
339000090213     c*==================================================================*
339100090213     c     Invio_Mail    begsr
339200090213      *
339300090213     c                   goto      non_inviare
339400090213      *
339500090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
339600090213     C* Inizializzo variabili
339700090213     C                   movel     *blanks       wrkEml          253
339800090213     C                   movel     *blanks       wrkMsg         5000
339900090213     C                   movel     *blanks       wrkOgg           44
340000090213      *
340100090213     C* Valorizzo i campi della e-m@ail - indirizzo
340200090213     C                   eval      wrkEml = Indirizzi
340300090213     C                   eval      wrkOgg = Oggetto
340400090213     C                   eval      wrkMsg = Messaggio
340500090213
340600090213     C                   call(e)   'TIS701C'
340700090213     C                   parm                    wrkEml
340800090213     C                   parm                    wrkOgg
340900090213     C                   parm                    wrkMsg
341000090213
341100090213     c     non_inviare   tag
341200090213     C*
341300090213     C                   ENDSR
341400090603     c*==================================================================*
341500090603      * Manda un Msg x E-mail a EDPAB
341600090603     c*==================================================================*
341700090603     c     Invio_Mail_AB begsr
341800090603      *
341900090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
342000090603     C* Inizializzo variabili
342100090603     C                   movel     *blanks       wrkEml          253
342200090603     C                   movel     *blanks       wrkMsg         5000
342300090603     C                   movel     *blanks       wrkOgg           44
342400090603      *
342500090603     C* Valorizzo i campi della e-m@ail - indirizzo
342600110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
342700110203     C                   eval      wrkEml = Indirizzi
342800090603     C                   eval      wrkOgg = Oggetto
342900090603     C                   eval      wrkMsg = Messaggio
343000110203      ********
343100110203     C********           call(e)   'TIS701C'
343200110203     C********           parm                    wrkEml
343300110203     C********           parm                    wrkOgg
343400110203     C********           parm                    wrkMsg
343500110203     C*
343600110203      ** *****
343700110203     C                   call(e)   'TRTCT00R2'
343800110203     C                   parm                    wrkEml
343900110203     C                   parm                    wrkOgg
344000110203     C                   parm                    wrkMsg
344100090603     C*
344200090603     C                   ENDSR
344300090211     ***********************************************************************
344400090211     **
344500090211     ** Send Message (QEZSNDMG) API
344600090211     **
344700090211     ***********************************************************************
344800090211     C     CalQEZSNDMG   BEGSR
344900090211      *
345000090211     ** Invio messaggio all'utente.
345100090211     C                   EVAL      SndMg03 = msgDSP
345200090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
345300090211     C                   EVAL      SndMg05(Jx) = User_msg
345400090211     C                   EVAL      SndMg06 = Jx
345500090211     C                   CLEAR                   QUSEC
345600090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
345700090211
345800090211     C                   CALL      'QEZSNDMG'
345900090211     C                   PARM                    SndMg01
346000090211     C                   PARM                    SndMg02
346100090211     C                   PARM                    SndMg03
346200090211     C                   PARM                    SndMg04
346300090211     C                   PARM                    SndMg05
346400090211     C                   PARM                    SndMg06
346500090211     C                   PARM                    SndMg07
346600090211     C                   PARM                    SndMg08
346700090211     C                   PARM                    Qusec
346800090211     C                   PARM                    SndMg10
346900090211     C                   PARM                    SndMg11
347000090211     C                   PARM                    SndMg12
347100090211      *
347200090211     C                   ENDSR
347300121105      *---------------------------------------------------------------*
347400121105      * CTRL caricamento schiere    PT                               -*
347500121105      *---------------------------------------------------------------*
347600121105     C     Check_PT      begsr
347700121105     C*
347800121105     c* Controllo riempimento schiera
347900121105     c                   clear                   trul0sds
348000121105     c                   eval      i0sski='CPT'
348100121105     c                   eval      i0sele=%elem(CPT)
348200121105     c                   eval      i0spie=x
348300121105     c                   eval      i0sfile='EDTAB00F'
348400121105     c                   eval      i0ssif=knsif
348500121105     c                   eval      i0spgm='TRTCT11R'
348600121113     c                   move      kpjbu         SvKpjbu
348700121113     c                   clear                   kpjbu
348800121105     c                   movel     trul0sds      kpjbu
348900121105     c                   call      'TRUL0SR'
349000121105     c                   parm                    kpjba
349100121113     c                   eval      kpjbu  =  SvKpjbu
349200121105     C*
349300121105     C                   ENDSR
349400090211     O*****************************************************************
349500090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
349600090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
349700090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
349800090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
