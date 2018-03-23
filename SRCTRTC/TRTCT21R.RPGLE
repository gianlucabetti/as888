000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600160511????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFTMIN vers D10A  *
000700160511      *                         cliente     AEG TTI      con  IFTMIN vers D10A  *
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
010900160624???? D                 s              3    INZ('AGE')
011000110502???? D Riferimento_Numerico...
011100160624???? D                 s              3    INZ('CU ')
011200091019???? D Segmento_Inizio_Dettaglio...
011300091019???? D                 s              3    INZ('BGM')
011400160511???? D Totale_Peso_Lordo...
011500160511???? D                 s              3    INZ('7  ')
011600160511???? D Totale_Nr_Spedizioni...
011700160511???? D                 s              3    INZ('10 ')
011800160511???? D Totale_Nr_Colli...
011900160511???? D                 s              3    INZ('11 ')
012000090209      *---------------------------------------------------------------------*
012100090209     D KPJBA         E DS
012200090209     D FNLV55DS      E DS
012300090209     D TIBS55DS      E DS
012400090209     D TISI95DS      E DS
012500090209     D UTEDSE0F      E DS
012600090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012700090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012800090209      * Ds scomposzione tipo capoconti
012900090209     D TCUDS           DS
013000090209     D  F34                    3      4
013100090209     D  F56                    5      6
013200090209     D DS1B          E DS
013300090209     D DS15          E DS
013400090209     D DSCV          E DS
013500090213     D TRTC86DS      E DS
013600090209     D EDIDSPT       E DS
013700090209     D EDIDSPU       E DS
013800090209     D EDIDSPV       E DS
013900090211     D EDIDSCL       E DS
014000090216     D EDIDSTC       E DS
014100090209      **
014200090209      *  DS x salvataggio dati impostati in EDiVAB
014300090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
014400090209     D SALVA           DS           545
014500090210      **
014600090209     D WLBDA8          DS
014700090209     D  G02DAT                 1      8  0
014800090209     D  G02INV                 9     16  0
014900090209     D  G02ERR                17     17
015000090209     D  G02TGI                18     22  0
015100090209      *  DS x scomposizione numero spedizione
015200090209     D DSSPE           DS
015300090209     D  SPEAAS                 1      4  0
015400090209     D  SPELNP                 5      7  0
015500090209     D  SPENRS                 8      9  0
015600090209     D  SPENSP                10     16  0
015700090209      *  DS x scomposizione segnacollo
015800090209     D DSSGN           DS
015900090209     D  SGNLNP                 1      3  0
016000090209     D  SGNLNA                 4      6  0
016100090209     D  SGNNRS                 7      8  0
016200090209     D  SGNNSC                 9     15  0
016300090209     D  SGNZNC                16     17  0
016400090209      *  DS x stampa segnacollo
016500090209      *---------------                                                      *
016600090211     D  vablnp_PT      s                   LIKE(vablnp)
016700090212     D  vabnrs_PT      s                   LIKE(vabnrs)
016800090211     D  vabctm_PT      s                   LIKE(vabctm)
016900090209     D Wvabtic         s                   LIKE(vabtic)
017000090603     d tes_VABrmo      s                   LIKE(VABrmo)
017100090603     d tes_VABnmo      s                   LIKE(VABnmo)
017200090603     d tes_VABcmo      s                   LIKE(VABcmo)
017300090209     C*
017400090603     d tes_VABrsd      s                   LIKE(VABrsd)
017500090603     d tes_VABrd2      s                   LIKE(VABrd2)
017600090603     d tes_VABind      s                   LIKE(VABind)
017700090603     d tes_VABlod      s                   LIKE(VABlod)
017800090603     d tes_VABnzd      s                   LIKE(VABnzd)
017900090603     C*
018000090603     d tes_Valuta      s                   LIKE(VABvca)
018100090603     C*
018200090209      *---------------------------------------------------------------------*
018300090209      * Schiere
018400090209      *---------------------------------------------------------------------*
018500090209      * Schiera per reperimento nr.seganacollo
018600090209     D SGN             S              7  0 DIM(5000)
018700090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
018800090209      * Stringhe per conversione alfanumerico/numerico N.Documento
018900090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
019000090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
019100090209      * Nr. documento alfanumerico da decodificare
019200090209     D NDA             S              1    DIM(35)
019300090209      * Nr. documento alfanumerico da già decodificato
019400090209     D NDN             S              1    DIM(15)
019500090209      * Messaggi di errore
019600090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
019700090209      * Tabella codici trattamento merce (1B)
019800090209     D CTM             S              2    DIM(200)
019900090209     D DTM             S             67    DIM(200)
020000090209      * Tabella codici valuta
020100090209     D CCV             S              3    DIM(200)
020200090209     D DCV             S             89    DIM(200)
020300090209      * Tabella codici nazioni
020400090209     D C15             S              3    DIM(500)
020500090209     D ISO             S              3    DIM(500)
020600090209     D CPF             S              2  0 DIM(500)
020700090209      * Tabella partner
020800121105     D CPT             S             35    DIM(200)
020900121105     D CPTparz         S             35    DIM(%elem(CPT))
021000121105     D KSC_UNB         S              7    DIM(%elem(CPT))
021100121105     D DPT             S             90    DIM(%elem(CPT))
021200121105     D DPU             S             90    DIM(%elem(CPT))
021300121105     D DPV             S             90    DIM(%elem(CPT))
021400121105     D TRUL0SDS      E DS
021500090209      *
021600121105     d Ptn_parziale    s              1    inz('N')
021700121105     d sav_CPTx        s              3  0 inz(0)
021800090209      * Tabella CL --> codici clienti - tariffa
021900130305     D CCL             S             35    DIM(999)
022000130305     D DCL             S             90    DIM(999)
022100090209      * Schiere x caricamento cod.cliente  <>
022200130305     D KCL             S              7  0 DIM(999)
022300090209      * Termini di consegna
022400110328     D K35_TpBolla     S             35    DIM(100)
022500110328     D Cod_TpBolla     S              3    DIM(100)
022600110328     D Decod_Porto     S              1    DIM(100)
022700110328     d Trovata_Tab_TB  S              1
022800110328      *
022900090216      * Tipo pagamento C/Assegno
023000090216     D CTC35           S             35    DIM(100)
023100090216     D TPI             S              2    DIM(100)
023200090209      * Decodifiche servizi speciali
023300090209     D SS35            S             35    DIM(100)
023400090209     D SS              S              3    DIM(100)
023500090209     D DSS             S             90    DIM(100)
023600090209      * Schiere x routine di conversione CAP
023700090209     D CAP5            S              1    DIM(5)
023800090209     D CAP9            S              1    DIM(9)
023900090209     D CAPM            S              1    DIM(9)
024000090209      * Schiera per memorizzazione FTX ricevuti
024100090209     D QUA             S              3    DIM(100)
024200090209     D FTX             S             70    DIM(100)
024300090209      * Tabelle capiconto
024400090209      * Schiere x località
024500090209     D LOD             S              1    DIM(35)
024600060608      **?------------------------------------------------------------------ */
024700090601     C*? Ds Scrittura del VAT "E"
024800060608      **?------------------------------------------------------------------ */
024900060612     D XTOEBCDDS     E DS
025000060620      *
025100060608      **?------------------------------------------------------------------ */
025200050112      * Numeratore Bolle (302)
025300050112     D trul33ds      E DS
025400050112     D Ds3C          E DS
025500090209     C*
025600090209      **?------------------------------------------------------------------ */
025700090209     C* Definizione campi di work
025800090209     D pVLB            s                   LIKE(VABVLB)
025900090209     D kKUT            s                   LIKE(TBLKUT)
026000090209     D kCOD            s                   LIKE(TBLCOD)
026100090209     D kKEY            s                   LIKE(TBLKEY)
026200090209     D kKCC            s                   LIKE(ACOKCC)
026300090209     D kKSC            s                   LIKE(ACOKSC)
026400090209     D kAAA            s                   LIKE(NUFAAA)
026500090209     D kCNU            s                   LIKE(NUFCNU)
026600090209     D kFIL            s                   LIKE(NUFFIL)
026700090209     D kAAS            s                   LIKE(VABAAS)
026800090209     D kLNP            s                   LIKE(VABLNP)
026900090209     D kNRS            s                   LIKE(VABNRS)
027000090209     D kNSP            s                   LIKE(VABNSP)
027100090209     D kCMR            s                   LIKE(VABCMR)
027200090209     D kCCM            s                   LIKE(VABCCM)
027300090209     D kNCD            s                   LIKE(VADNCD)
027400090210     D kCNT            s                   LIKE(VADCNT)
027500090209     D kNMC            s                   LIKE(RDENMC)
027600090209     D kNRP            s                   LIKE(RDENRP)
027700090209     D kLNP1           s                   LIKE(RDELNP)
027800090209      *
027900090209      *  Invio E-mail
028000090209     D Indirizzi       s            253
028100090209     D Oggetto         s             44
028200090209     D Messaggio       s           5000
028300090209      *
028400090209     d   DSmail        ds
028500090209     D Mail_Ind                      44
028600090209     d   IND_char                     1    dim(44)
028700090209      *
028800090209     D Lun_Ind         s              5u 0
028900090209      *
029000090209     D Dac             s              5u 0
029100090209     D Luc             s              5u 0
029200090209      *
029300090209     C*---------------------------------------------------------------*
029400090209     D MsgDSP          S            256                                         Testo generico
029500090209     C*---------------------------------------------------------------*
029600090209     D SndMg01         S             10                                         Message type
029700090209     D                                     INZ('*INFO')
029800090209     D SndMg02         S             10                                         Deluvery mode
029900090209     D                                     INZ('*BREAK')
030000090209     D SndMg03         S            256                                         Message text
030100090209     D SndMg04         S             10I 0                                      Length of text
030200090209     D                                     INZ(%SIZE(SndMg03))
030300090209     D SndMg05         S             10                                         User profile
030400090209     D                                     DIM(299)
030500090209     D SndMg06         S             10I 0                                      Number of user
030600090209     D SndMg07         S             10I 0                                      Message sent indic.
030700090209     D SndMg08         S             10I 0                                      Function requested
030800090209     D SndMg10         S              1                                         Show display
030900090209     D                                     INZ('N')
031000090209     D SndMg11         S             20                                         Qualified MSGQ name
031100090209     D SndMg12         S              4                                         Name type indicator
031200090209     D                                     INZ('*USR')
031300090209      * indice di scihera
031400090209     D Jx              S              5I 0
031500110527     D Position        S              5I 0
031600110527     D daPos           S              5I 0
031700090209      *
031800090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031900090209      *
032000090209     d bart_it         C                   CONST('@Bartolini.it;')
032100090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
032200060612      * ?================================================================== */
032300060612      *
032400060612      * ?   * Campi x decodifica * (INPUT  del Record)
032500090130     D  Dati           s           2048
032600060612      * ?   *--------------------------------------------------------------*
032700060612     D  se_errore      s              1    inz(' ')
032800060612      * ?* ------------------------------------------------------ *
032900060710     D Digits          C                   '0123456789'
033000091019     D*------------------
033100091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
033200091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
033300110527     D*------------------
033400110527     D allChars        c                   const('QWERTYUIOPASDFGHJKLZXCVBNMqwe-  caratteri
033500110527     D                                     rtyuiopasdfghjklzxcvbnm0123456789/*--  accettati
033600110527     D                                     +\|!"£$%&/()=''?ì+*èéòçà°ù§-.:_,;<>[-
033700110527     D                                     ]@ ')
033800060612      * ?================================================================== */
033900060612      *   Ciclo principale
034000090205      * ?================================================================== */
034100060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
034200060612     C*
034300060612     C                   if        not %open(tivin00r)
034400060612     C                   open      tivin00r
034500060612     C                   endif
034600060612      **
034700060612      * ?------------------------------------------------------------------ */
034800060612     C*? Controllo dati arrivati da DPD
034900060612      * ?------------------------------------------------------------------ */
035000060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
035100060612      * ?- controllando che la trasmissione sia completa.
035200090205      **
035300060612      * ?              /*---------------------- */
035400090205     c                   goto      nonFare_adesso
035500060612     c                   exsr      check_Trasm
035600090205     c     nonFare_adessotag
035700060612      * ?              /*---------------------- */
035800090205      **
035900060613      **  Errore di trasmissione x tutti i records
036000060613      **   --> file in errore
036100060613     c                   if        se_errore = 'S'
036200060612
036300060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
036400090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
036500090205     c                   exsr      Msg_errato
036600060612      **
036700060612     c                   else
036800060614      **
036900060612      * ?------------------------------------------------------------------ */
037000060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
037100060612      * ?------------------------------------------------------------------ */
037200090205      **
037300060612      * ?              /*---------------------- */
037400060612     c                   exsr      Importa_Msg
037500060612      * ?              /*---------------------- */
037600060612
037700060614     c                   end
037800060614
037900060612     C                   if        %open(tivin00r)
038000060612     C                   close     tivin00r
038100060612     C                   endif
038200060612      *
038300060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
038400090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
038500090225     c                             esito ='0'
038600060710     C                   eval      esito ='0'
038700090225      *
038800090225     c                   COMMIT
038900090224      *
039000090213     c                   if        almeno_un_due ='S'
039100090213     C                   eval      esito ='1'
039200060614     C                   endif
039300090213     C                   endif
039400060614      *
039500060612     c                   SETON                                        LR
039600060612      * ?================================================================== */
039700060612      *? Importa i records della tramsissione
039800060612      * ?------------------------------------------------------------------ */
039900060612     c     Importa_Msg   Begsr
040000060612
040100090211     C                   EXSR      AZZMSG
040200060614
040300060612     c     *start        setll     Tivin00r
040400060612     c                   read      Tivin00r
040500091016     c                   if        %Eof(tivin00r)
040600091016     c                   move      'S'           EoFTivin
040700091016     c                   end
040800060612
040900060612     c                   dow       not %eof(Tivin00r)
041000060614
041100060614      * solo i record sflaggati da rielaborare
041200060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
041300090211      *  Sempre Record OK
041400090211      ** Controlli formali sui campi
041500090211     C                   eval      vinFLG = '1'
041600060612     c                   clear                   se_errore
041700060612
041800060612      ** Decodifica record a campi variabili
041900060612      * ?              /*---------------------- */
042000090205     c                   exsr      Decod_Segmento
042100090209      * ?              /*---------------------- */
042200090209
042300090213      *** ------------------------------------
042400091019????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT" x IFTMIN
042500090213      *** ------------------------------------
042600090213      * ?              /*---------------------- */
042700090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
042800090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
042900090213      * ?              /*---------------------- */
043000090601      ***
043100091019????  **
043200091019????  *   OGNI fine DETTAGLIO è determinato dall'UNT.
043300091019???? c                   if           tipo_seg = 'UNT'
043400091019????  *
043500090213      *  Deve flaggare il dettaglio con dei problemi
043600090213     C                   if        Err_in_detta = 'S'
043700090213      **?              /*---------------------- */
043800090213     c                   exsr      DETta_Errato
043900090213      **?              /*---------------------- */
044000090213     c                   end
044100090213      *
044200060612      **  Se presente un errore nel record emette una segnalazione msg
044300091019???? c                   if        se_errore = 'S'
044400091019???? c                   else
044500091019????  ******
044600090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
044700090227     c                   if         se_trovato_NAD ='S'
044800060612      * ?              /*---------------------- */
044900090209     c                   exsr      Scrive_VAB
045000060612      * ?              /*---------------------- */
045100090227     c                   else
045200090227     c                   eval      errori_in_Wrt = 'S'
045300090227     c                   end
045400090227      *
045500090209      *  Problemi nella decodifica dei campi VAB/VAT
045600090213     c                   if        errori_in_Wrt = 'S'
045700090213      *
045800090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
045900090213     c                   exsr      Error_DET
046000090213      *
046100090213     c                   ROLBK
046200090213      *
046300090213     c                   else
046400090601      *
046500090213     c                   COMMIT
046600090601      *
046700090227      * Dopo aver confermato, con COMMIT
046800090227      *  si prepara per una nuova spedizione
046900090227      * ?                         --------------
047000090227     c                   exsr      Nuova_spediz
047100090227      * ?                         --------------
047200090213     c                   end
047300090209
047400090209     c                   end
047500090210      **
047600060620     c                   end
047700060612
047800060621      *  x errore bloccante nella composizione del VAB/VAT
047900060621     c                   if        err_bloccante ='S'
048000060621     C                   eval      vinFLG = '2'
048100090211     c                   eval      esito  = '2'
048200110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG '
048300090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
048400090603     C                             Bolle import in filiale - messaggio -
048500090603     C                             con UNB non presente in tabella PT controlla-
048600090603     C                             re EDI ricevuto su UPLOAD.'
048700090603     c                   exsr      invio_mail_AB
048800060621     c                   end
048900060612
049000091016     c                   if        EoFTivin = ' '
049100060612     c                   update    Tivin000
049200091016     c                   end
049300090211
049400090211      *** se non è riconosciuto l'UNB deve uscire direttamente
049500090211     c                   if        se_errore ='S'  and
049600090211     c                             tipo_seg = 'UNB'
049700090213     c                   eval      err_bloccante = 'S'
049800090211     c                   exsr      Msg_errato
049900090211     c                   LEAVE
050000090211     c                   endIF
050100090211      ***
050200060614     c                   endIF
050300060612
050400060612     c                   read      Tivin00r
050500091016     c                   if        %Eof(tivin00r)
050600091016     c                   move      'S'           EoFTivin
050700091016     c                   end
050800060612     C                   ENDdo
050900060612      **
051000060612     c                   endSR
051100100120      * ?------------------------------------------------------------------ */
051200100120      *? Controlla la trasmissione   se completa
051300100120      * ?------------------------------------------------------------------ */
051400100120     c     Check_Trasm   Begsr
051500100120
051600100120     C                   clear                   se_errore
051700100120     C                   clear                   Riga_iNIZIo       1
051800100120     C                   clear                   Riga_fINe         1
051900100120      ** primo  record
052000100120     c     *start        setll     tivin00r
052100100120      *
052200100120     c     rileggi       tag
052300100120     c                   read      tivin00r
052400100120     c                   if        %Eof(tivin00r)
052500100120     c                   move      'S'           EoFTivin
052600100120     c                   end
052700100120      *
052800100120     c                   if        not %eof(tivin00r)
052900110527      *  Caratteri non previsti
053000110527     c                   z-add     1             dapos
053100110527     c     cerca         tag
053200110527     c     allChars      checK     VINdta:daPos  position
053300110527     C                   if        %Found
053400110527     c                   eval      %subst(VinDTA:position:1)=' '
053500110527     c                   eval      dapos = Position +1
053600110527     c                   goto      cerca
053700110527     c                   endIf
053800110527      *
053900100120     c                   clear                   dati
054000100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
054100100120     c                   if        dati = *blank
054200100120      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
054300100120      * le leggo e le escludo.
054400100120     C                   eval      vinFLG = '1'
054500100120     c                   if        EoFTivin = ' '
054600100120     c                   update    Tivin000
054700100120     c                   end
054800100120     c                   goto      rileggi
054900100120     c                   end
055000100120      * ?              /*---------------------- */
055100100120     c                   exsr      Decod_Segmento
055200100120      * ?              /*---------------------- */
055300100120     c                   endif
055400100120      **
055500100120      ** ultimo record
055600100120     c     *hival        setll     tivin00r
055700100120     c     leggiAncora   tag
055800100120     c                   readp     tivin00r
055900100120     c                   if        %Eof(tivin00r)
056000100120     c                   move      'S'           EoFTivin
056100100120     c                   end
056200100120     c                   if        not %eof(tivin00r)
056300100120      *
056400100120     c***********        movel(p)  VINDTA        dati
056500100120     c                   clear                   dati
056600100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
056700100120     c                   if        dati = *blank
056800100120      * le ultime righe potrebbero essere vuote prima di finire il messaggio
056900100120      * le leggo e le escludo.
057000100120     C                   eval      vinFLG = '1'
057100100120     c                   if        EoFTivin = ' '
057200100120     c                   update    Tivin000
057300100120     c                   end
057400100120     c                   goto      leggiAncora
057500100120     c                   end
057600100120      * ?              /*---------------------- */
057700100120     c                   exsr      Decod_Segmento
057800100120      * ?              /*---------------------- */
057900100120     c                   endif
058000100120
058100100120      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
058200100120      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
058300100120      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
058400100120     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
058500100120      * ?-----> Errore
058600100120     C                   eval      se_errore = 'S'
058700100120     c                   end
058800100120
058900100120     c                   endSR
059000100120      * ?------------------------------------------------------------------ */
059100100120      *?    Decodifica il segmento ed importa i campi in formato DS
059200100120      * ?------------------------------------------------------------------ */
059300100120     c     Decod_SegmentoBegsr
059400100120      **
059500110527      *  Caratteri non previsti
059600110527     c                   z-add     1             dapos
059700110527     c     $erca         tag
059800110527     c     allChars      checK     VINdta:daPos  position
059900110527     C                   if        %Found
060000110527     c                   eval      %subst(VinDTA:position:1)=' '
060100110527     c                   eval      dapos = Position +1
060200110527     c                   goto      $erca
060300110527     c                   endIf
060400110527      *
060500100120     c                   clear                   dati
060600100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
060700100120     c                   eval      tipo_seg = %subst(dati:1:3)
060800100120     c                   eval      segmento = dati
060900100120      *
061000100120      * Imposta il flag di Inizio Dettaglio
061100100120     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
061200100120     c                   eval      Inizio_Dettaglio ='S'
061300100120     c                   end
061400110503      *
061500100120     c                   clear                   seg_imprevisto
061600100720     c                   clear                   trovato_seg
061700100716      *
061800100716     C                   clear                   keyUNBCLI
061900100716     C                   clear                   keyTIPOMSG
062000100716     C                   clear                   keyVERSION
062100100716     C                   clear                   keyRELEASE
062200100716     C                   clear                   keyAGENCY
062300100716     C                   clear                   keyASSOCIA
062400110527      *
062500110527???? c                   if           tipo_seg = 'UNH'
062600110527     C                   movel     'AEG'         keyUNBCLI
062700110527     c                   end
062800110503      *
062900100720      *  Chiamata generica x decodificare il singolo segmento letto
063000100720     c                   call      'TRTCT00R1'
063100100720      * input
063200100720     c                   parm                    tipo_seg
063300100720     c                   parm                    segmento
063400100720     C                   parm                    keyUNBCLI
063500100720     C                   parm                    keyTIPOMSG
063600100720     C                   parm                    keyVERSION
063700100720     C                   parm                    keyRELEASE
063800100720     C                   parm                    keyAGENCY
063900100720     C                   parm                    keyASSOCIA
064000100720      *output
064100100720     c                   parm                    Errore
064200100720     c                   parm                    DsGenerica
064300100720     c                   parm                    Valori_inErr
064400100720     c                   parm                    Descr_Errore
064500100720     c                   parm                    trovato_seg
064600100720      **--------
064700100720      *  Ritorna la DS GENERICA oppure l'errore
064800100120      **--------
064900100120     c                   select
065000100720      *
065100100720     c                   when      trovato_seg ='N'
065200100720      *
065300100720     c                   move      'S'           seg_imprevisto
065400100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
065500100720     c                   exsr      Error_DET
065600100720     c                   goto      end_Decod
065700100720      **--------
065800100120     c                   when      tipo_seg = 'UNA'
065900100720     c                   movel(p)  DsGenerica    EDS00UNA
066000100120      *
066100100720      **--------
066200100120     c                   when      tipo_seg = 'UNB'
066300100720     c                   movel(p)  DsGenerica    EDS00UNB
066400100120     C                   EXSR      DATI_UNB
066500100120      * --------
066600100120     c                   when      tipo_seg = 'UNH'
066700100720     c                   movel(p)  DsGenerica    EDS00UNH
066800100120     C                   EXSR      DATI_UNH
066900100120      * --------
067000100120     c                   when      tipo_seg = 'BGM'
067100100720     c                   movel(p)  DsGenerica    EDS00BGM
067200100120      **--------
067300100120     c                   when      tipo_seg = 'DTM'
067400100720     c                   movel(p)  DsGenerica    EDS00DTM
067500100120     C                   EXSR      DATI_DTM
067600100120      * --------
067700100120     c                   when      tipo_seg = 'TSR'
067800100720     c                   movel(p)  DsGenerica    EDS00TSR
067900100120      **--------
068000100120     c                   when      tipo_seg = 'CNT'
068100100720     c                   movel(p)  DsGenerica    EDS00CNT
068200160511     C                   EXSR      DATI_CNT
068300100120      **--------
068400100120     c                   when      tipo_seg = 'TOD'
068500100720     c                   movel(p)  DsGenerica    EDS00TOD
068600100120     C                   EXSR      DATI_TOD
068700100120      **--------
068800100120     c                   when      tipo_seg = 'RFF'
068900100720     c                   movel(p)  DsGenerica    EDS00RFF
069000100120     C                   EXSR      DATI_RFF
069100100120      **--------
069200100120     c                   when      tipo_seg = 'TDT'
069300100720     c                   movel(p)  DsGenerica    EDS00TDT
069400100120      **--------
069500100120     c                   when      tipo_seg = 'MOA'
069600100720     c                   movel(p)  DsGenerica    EDS00MOA
069700100120     C                   EXSR      DATI_MOA
069800100120      **--------
069900100120     c                   when      tipo_seg = 'FTX'
070000100720     c                   movel(p)  DsGenerica    EDS00FTX
070100100120     C                   EXSR      DATI_FTX
070200100120      **--------
070300100120     c                   when      tipo_seg = 'NAD'
070400100720     c                   movel(p)  DsGenerica    EDS00NAD
070500100120     C                   EXSR      DATI_NAD
070600100120      **--------
070700100120     c                   when      tipo_seg = 'CTA'
070800100720     c                   movel(p)  DsGenerica    EDS00CTA
070900100120     C                   EXSR      DATI_CTA
071000100120      **--------
071100100120     c                   when      tipo_seg = 'COM'
071200100720     c                   movel(p)  DsGenerica    EDS00COM
071300100120     C                   EXSR      DATI_COM
071400100120      **--------
071500100120     c                   when      tipo_seg = 'GID'
071600100720     c                   movel(p)  DsGenerica    EDS00GID
071700100120     C                   EXSR      DATI_GID
071800100120      **--------
071900100120     c                   when      tipo_seg = 'MEA'
072000100720     c                   movel(p)  DsGenerica    EDS00MEA
072100100120     C                   EXSR      DATI_MEA
072200100120      **--------
072300100120     c                   when      tipo_seg = 'PCI'
072400100720     c                   movel(p)  DsGenerica    EDS00PCI
072500100120     C                   EXSR      DATI_PCI
072600100120      **--------
072700100120     c                   when      tipo_seg = 'UNT'
072800100720     c                   movel(p)  DsGenerica    EDS00UNT
072900100120     C                   EXSR      DATI_UNT
073000100120      **--------
073100100120     c                   when      tipo_seg = 'UNZ'
073200100720     c                   movel(p)  DsGenerica    EDS00UNZ
073300100120     C                   EXSR      DATI_UNZ
073400100120      **--------
073500100120     c                   OTHER
073600100120      **--------
073700100120     c                   move      'S'           seg_imprevisto
073800100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
073900100120     c                   exsr      Error_DET
074000100120     c                   goto      end_Decod
074100100120      **--------
074200100120     c                   endSL
074300100120      **
074400100120     c     end_Decod     endSR
074500090211     C*----------------------------------------------------------------
074600090211     C*? AZZMSG - Azzera dati messaggio
074700090211     C*----------------------------------------------------------------
074800090211     C     AZZMSG        BEGSR
074900090211     C*
075000090211     C* Pulisco dati di work x foglio viaggio
075100090211     C                   MOVEL     *BLANKS       WNUMFV           35
075200090211     C                   MOVEL     *BLANKS       WNRCMR           35
075300090211     C                   Z-ADD     0             WDATPA            8 0
075400090211     C                   Z-ADD     0             WDATFV            8 0
075500090211     C                   Z-ADD     0             WHHNFV            4 0
075600090211     C                   Z-ADD     0             WDTCMR            8 0
075700090211     C                   Z-ADD     0             WHHCMR            4 0
075800090211      *
075900090211      * pulisce gli RFF+FF di testata
076000090211     C                   clear                   $CLKSC            7 0
076100090211     C                   clear                   $CLTAR            3
076200090211     C                   clear                   $CLLNP            3 0
076300090211     C                   clear                   $CLNRS            2 0
076400090211     C                   clear                   $CLCTM            2
076500090211     C                   clear                   $CLBS1            1
076600090211     C                   clear                   $CLfnsp           1            *blk/S
076700090211      *
076800090211     C*  Inizializzo variabile new documento
076900090211     C                   Z-ADD     0             KCL
077000090211     C                   Z-ADD     0             WW                3 0
077100090211     ** Inizializza campo di work
077200090211     c                   move      'N'           WGID99
077300090211      * per controllare se almeno un record è stato importato sul VAB
077400090211     c                   clear                   Almeno_Uno        1
077500090213     c                   clear                   Almeno_Un_Due     1
077600090211     c                   eval      esito  = '0'
077700090211     C*
077800090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
077900090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
078000090603     c                   eval      Dettaglio_NAD_destinatario = *blank
078100090603     c                   eval      NAD_destinatario_in_testata = *blank
078200090603     c                   eval      NAD_mittente_in_testata = *blank
078300090603     c                   eval      Inizio_Dettaglio = *blank
078400090603     C*
078500090603     c                   clear                   tes_VABrmo
078600090603     c                   clear                   tes_VABnmo
078700090603     c                   clear                   tes_VABcmo
078800090603     C*
078900090603     c                   clear                   tes_VABrsd
079000090603     c                   clear                   tes_VABrd2
079100090603     c                   clear                   tes_VABind
079200090603     c                   clear                   tes_VABlod
079300090603     c                   clear                   tes_VABnzd
079400090603     C*
079500090603     c                   clear                   tes_Valuta
079600160511     C*
079700160511     c                   clear                   Tot_Peso_tes     15 2
079800160511     c                   clear                   Tot_Sped_tes     15 2
079900160511     c                   clear                   Tot_Colli_tes    15 2
080000090603     C*
080100090211     C                   ENDSR
080200090210     C*----------------------------------------------------------------
080300090210      *? DECODIFICA DATI UNB
080400090210     C*----------------------------------------------------------------
080500090210     C     DATI_UNB      BEGSR
080600090210      *
080700090209      *  INIZIALIZZA  dati fondamentali messaggio
080800090209     C                   clear                   EDIDSPT
080900090209     C                   clear                   EDIDSPU
081000090209     C                   clear                   EDIDSPV
081100090209     C                   clear                   wnrDOC           35
081200090209     C                   move      'N'           PT_ok             1
081300090209      ** ---------------------------------------------------------------
081400090209      * quindi 1° cosa:
081500090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
081600090209      *     senza questo il messaggio NON è trascodificabile
081700090209     C                   MOVEL     unb0004       WCOD             35
081800090209     C                   MOVE      unb000720     WCOD
081900090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
082000090209     C                   Z-ADD     1             XX                3 0
082100090209     C     WCOD          LOOKUP    CPT(XX)                                32
082200090209      *
082300090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
082400090209     C                   If        *IN32 = *off
082500090209      * ******  Errore generale sul Messaggio
082600090209      * Msg di allerta Traduzione
082700090209     C                   EVAL      Indirizzi = CED_Bart
082800110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG'
082900090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
083000090209     C                             Bolle import in filiale - messaggio -
083100090209     C                             con UNB non presente in tabella PT controlla-
083200090209     C                             re EDI ricevuto su UPLOAD.'
083300090209      *
083400090209     c                   exsr      invio_mail
083500090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
083600090210     C                   eval      vinFLG = '2'
083700090210     C                   eval      se_errore   = 'S'
083800090210     c                   goto      end_UNB
083900090209      *
084000090209     C                   move      'N'           PT_ok             1
084100090209      *
084200090209      *             *------------------------------*
084300090209     C                   eLSe
084400090209      *             *------------------------------*
084500090209      * Se tutto OK su tab.PT:
084600090209     C                   move      'S'           PT_ok             1
084700090209      *
084800090209     C                   MOVEL     DPT(XX)       EDIDSPT
084900090209     C                   MOVEL     DPU(XX)       EDIDSPU
085000090209     C                   MOVEL     DPV(XX)       EDIDSPV
085100091016     c                   clear                   Mittente         35
085200091016     c                   clear                   Mitt_Qualifier    4
085300091016     c                   clear                   Id_Messaggio     14
085400090226     C                   eval      Mittente       = unb0004
085500090226     C                   eval      Mitt_qualifier = unb000720
085600090226     C                   eval      Id_Messaggio   = UNB0022
085700090209      *
085800090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
085900090209      *  ricevuti:
086000090209     c                   movel     §PVema        mail_ind         44
086100090209      *
086200090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
086300090209     c                   if        mail_ind <> *blank
086400090209     c                   exsr      imp_ADDR_mail
086500090209     c                   end
086600090209      *
086700090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
086800090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
086900090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
087000090209     C                   if        §PULUN = *blank
087100090209     C                   eval      §PULUN = '00'
087200090209     c                   end
087300090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
087400090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
087500090209      *
087600090209     C                   Z-ADD     §PTKSC        WKSC              7 0
087700090211     C                   MOVEL     §PTLNP        VABlnp_PT
087800090212     C                   MOVEL     §PTNRS        VABnrs_PT
087900090211     C                   MOVEL     §PTCTM        VABctm_PT
088000090209      *
088100090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
088200090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
088300090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
088400090209      *  i records trasmessi).
088500090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
088600090209      *
088700090209      *  In base al cod.trattamento merce reperisco tipo tratt.
088800090209     C                   CLEAR                   DS1B
088900090209     C                   Z-ADD     1             Y                 3 0
089000090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
089100090209     C     *IN32         IFEQ      '1'
089200090209     C                   MOVEL     DTM(Y)        DS1B
089300090209     C                   MOVEL     DS1B          WSVTRM           38
089400090209     C                   END
089500090209      *
089600090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
089700090209     C                   clear                   ACORAG
089800090209     C                   clear                   INDCAE
089900090209     C                   clear                   INDCAP
090000090209     C                   clear                   INDSTA
090100090209      * Decodifico partner
090200090209     C                   Z-ADD     1             KKUT
090300090209     C                   Z-ADD     KCI           KKCC
090400090209     C                   MOVE      §PTKSC        KKSC
090500090209     C     KACO          CHAIN     CNACO00F                           31
090600090209     C     KIND          CHAIN     CNIND00F                           31
090700090211      *
090800090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
090900090211      *   questa schiera è utilizzata anche per i clienti.
091000090211     C     §ptKSC        LOOKUP    KCL                                    39
091100090211     c                   If        *in39 = *off
091200090211     C                   add       1             WW
091300090211     C                   z-add     §ptKSC        KCL(WW)
091400090211     C                   Endif
091500090209      *
091600090209     C                   MOVEL     unb0017       WAA4              4 0
091700090209     C                   MOVE      WAA4          WAA2              2 0
091800090209     C                   MOVE      unb0017       WGG               2 0
091900090209     C                   MOVE      unb0017       WDTPRE            8 0
092000090209     C                   MOVE      unb0019       WHMPRE            4 0
092100090209     C                   MOVE      unb0017       WDTMSG            6 0
092200090209     C                   MOVE      WAA2          WDTMSG
092300090209     C                   MOVEL     WGG           WDTMSG
092400090210      * potremmo
092500090210      * Utilizzare l'identificativo della trasmissione come CMR
092600090209     C                   IF        unb0022 <> *blank
092700090209     C                   MOVEL     unb0022       WNRDOC
092800090211     C                   MOVEL     unb0022       WNUMFV
092900090211     C                   MOVEL     unb0022       WNRCMR
093000090209     c                   end
093100090209      *
093200090209     c                   Endif
093300090209      **
093400090210     c     end_UNB       endSR
093500090209      * ?================================================================== */
093600090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
093700090209      * ?================================================================== */
093800090227     C     Nuova_spediz  BEGSR
093900090227      **
094000090227      * Pulisco campi del file
094100090227     c                   z-add     vnrec         SAVNUM_RECord
094200090227     C                   CLEAR                   EDiVAB00
094300090227     C                   CLEAR                   EDiVAT00
094400090227     C                   CLEAR                   EDiVAD00
094500090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
094600090227     C                   MOVEL     'N'           WGID99            1
094700090227     C                   clear                   XY                5 0
094800090227     C                   clear                   §§                5 0
094900090227     C                   clear                   WS1496            5 0
095000090227     C                   clear                   WNSP             35
095100090227     C                   Z-ADD     *ZEROS        PVLB
095200090227     C                   MOVEA     *HIVAL        SGN
095300090227     C*  Azzero codice cliente - tariffa
095400090227     C                   Z-ADD     0             WTOCOC           16 0
095500090227     C                   Z-ADD     0             WTOPLC           16 2          ???
095600090227     C                   Z-ADD     0             WTOPNC           16 2          ???
095700110718     C                   Z-ADD     0             Work_KG          16 2          ???
095800160511     C*
095900160511     c                   clear                   Tot_Peso_tes     15 2
096000160511     c                   clear                   Tot_Sped_tes     15 2
096100160511     c                   clear                   Tot_Colli_tes    15 2
096200160511     C*
096300090227     C*  Azzero codice cliente - tariffa
096400090227     C                   clear                   WCLKSC
096500090227     C                   clear                   WCLTAR
096600090227     C                   clear                   WCLLNP
096700090227     C                   clear                   WCLNRS
096800090227     C                   clear                   WCLCTM
096900090227     C                   clear                   WCLBS1
097000090227     C                   clear                   WCLfnsp                        *blk/S
097100090227     C                   clear                   WNOT1            35
097200090227     C                   clear                   WNOT2            35
097300090227     C*  Campi di work
097400090227     c                   clear                   wVabTIC
097500090227     c                   clear                   wri_vabtic        1
097600090227      *
097700090227     C* pulisce campi di work
097800090227     c                   clear                   watrde           35
097900090227     c                   clear                   wattel           25
098000090227     C                   clear                   WRMN              3
098100090227      *
098200090227      * Un errore nel dettaglio
098300090227     C                   clear                   Err_in_detta      1
098400090227     c                   clear                   errori_in_Wrt     1
098500090227     c                   clear                   se_trovato_NAD    1
098600090227      **
098700090227     c                   endSR
098800090227      * ?================================================================== */
098900090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
099000090227      * ?================================================================== */
099100090227     C     DATI_UNH      BEGSR
099200090227      *
099300090227      *  Prepara una nuova spedizione
099400090227      * ?                         --------------
099500090227     c                   exsr      Nuova_spediz
099600090227      * ?                         --------------
099700090211      *
099800090211      ** Riferimento del cliente
099900090211     C                   MOVEL     unh0062       WPRGSP           35
100000090211     C                   MOVEL     unh0062       WNSP             35
100100090211     C                   MOVEL     unh0062       WNUM             35
100200090211      * ?                                       --------
100300090211     C                   MOVEL     unh0062       VABRMA
100400090211      * ?                                       --------
100500090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
100600090211     C                   SUB       1             WLUNGH
100700090211     C                   EXSR      TSTNUM
100800090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
100900090211      * ?                                       --------
101000090211     C                   MOVEL     WNUMOK        VABRMN
101100090211      * ?                                       --------
101200090211     C                   END
101300090210      **
101400090303     C                   MOVEL     VABlnp_PT     VABLNP
101500090303     C                   MOVEL     VABnrs_PT     VABNRS
101600090303     C                   MOVEL     VABctm_PT     VABCTM
101700090303      **
101800090303      **
101900090210     c     end_UNH       endSR
102000090601      * ?================================================================== */
102100090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
102200090601      * ?================================================================== */
102300090601     C     DATI_DTM      BEGSR
102400090601     C*
102500090209     C*  Se data documento  memorizzo numero e data x CMR
102600090603     C     dtm2005       IFEQ      '9'
102700090209     C                   MOVEL     dtm2380       WDATA            35            Data
102800090209     C                   MOVEL     dtm2379       WFORM             3            Formato
102900090209     C                   EXSR      CNVDAT
103000090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
103100090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
103200090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
103300090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
103400090209     C     WERRO         IFEQ      'S'
103500090210     C                   eval      vinMSG = 'DTM data errata'
103600090213     c                   exsr      Error_DET
103700090210     c                   goto      end_DTM
103800090209     C                   END
103900090209     C                   END
104000090209      **
104100090209     C*  Se data consegna richiesta imposto già sui campi del VAB
104200110523     C     dtm2005       IFEQ      '2'
104300090209     C                   MOVEL     dtm2380       WDATA            35            Data
104400090209     C                   MOVEL     dtm2379       WFORM             3            Formato
104500090209     C                   EXSR      CNVDAT
104600090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
104700090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
104800090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
104900090209     C     WERRO         IFEQ      'S'
105000090210     C                   eval      vinMSG = 'DTM data errata'
105100090213     c                   exsr      Error_DET
105200090210     c                   goto      end_DTM
105300090209     C                   END
105400090209     C                   END
105500090211      **
105600090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
105700090603     C     dtm2005       IFEQ      '10'
105800090211     C                   MOVEL     dtm2380       WDATA            35            Data
105900090211     C                   MOVEL     dtm2379       WFORM             3            Formato
106000090211     C                   EXSR      CNVDAT
106100090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
106200090211     C     WERRO         IFEQ      'S'
106300090211     C                   eval      vinMSG = 'DTM data errata'
106400090213     c                   exsr      Error_DET
106500090211     c                   goto      end_DTM
106600090211     C                   END
106700090211     C                   END
106800090209      **
106900090210     c     end_DTM       endSR
107000160511      * ?================================================================== */
107100160511     C*?  Contatori in testata o in Dettaglio
107200160511      * ?================================================================== */
107300160511     C     DATI_CNT      BEGSR
107400160511      **
107500160511      *  TOTALE colli, peso, spedizioni:
107600160511     c                   if        cnt6069 = Totale_Peso_Lordo
107700160511     c                   z-add     cnt6066       Tot_Peso_tes     15 2
107800160511     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
107900160511     c                   z-add     cnt6066       Tot_Sped_tes     15 2
108000160511     c                   elseIf    cnt6069 = Totale_Nr_Colli
108100160511     c                   z-add     cnt6066       Tot_Colli_tes    15 2
108200160511     c                   end
108300160511      **
108400160511     c     end_CNT       endSR
108500090209      * ?================================================================== */
108600090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
108700090209      * ?================================================================== */
108800090209     C     DATI_TOD      BEGSR
108900090209      *
109000090210      *  Trascodifico tipo trasporto
109100090210     c                   clear                   TOD_char3         3
109200090210      *
109300090210      *  imposta il default dalla tabella PT se c'è
109400090210     C     §puPFA        ifNE      *blank
109500090210     c                   SELECT
109600090210      *  Porto Franco
109700090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
109800090210     C                   movel     '4'           VABCBO                         + C/Ass
109900090210     c                   WHEN      §puPFA = 'F'
110000090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
110100090210      *  Porto Assegnato
110200090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
110300090210     C                   movel     '6'           VABCBO                         + C/Ass
110400090210     C                   WHEN      §puPFA = 'A'
110500090210     C                   movel     '2'           VABCBO                         Senza C/Ass
110600090210     C                   ENDSL
110700090210     c                   end
110800090210      *
110900090210      *   Imposta o il codice o il metodo
111000090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
111100090210     c                   if        tod4053  <> *blank
111200090210     c                   movel(p)  tod4053       TOD_char3
111300090210     c                   else
111400090210     c                   if        tod4215  <> *blank
111500090210     c                   movel(p)  tod4215       TOD_char3
111600090210     c                   end
111700090210     c                   end
111800110328      ****************
111900110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
112000110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
112100110328      ****************
112200110328     c                   eval      Trovata_Tab_TB ='N'
112300110328      *
112400110328     c                   if              TOD_char3 <> *blank
112500110328     c                   movel(p)  TOD_char3     Key_Generica35
112600110328     c                   move      §puTB         Key_Generica35
112700110328     C                   eval      Y = 1
112800110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
112900110328     c   32              eval      Trovata_Tab_TB ='S'
113000110328      *
113100110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
113200110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
113300110328      *
113400110328     c                   if          Trovata_Tab_TB ='N'
113500110328     c                                 and
113600110328     C                               tod4215 <> *blank
113700110328     c                   movel(p)  tod4215       TOD_char3
113800110328     c                   movel(p)  TOD_char3     Key_Generica35
113900110328     c                   move      §puTB         Key_Generica35
114000110328     C                   eval      Y = 1
114100110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
114200110328     c   32              eval      Trovata_Tab_TB ='S'
114300110328     c                   end
114400110328     c                   end
114500110328      *
114600110328      ****************
114700110328      ******* Se vi sono eccezioni sul cliente imposta quelle
114800110328      ****************
114900110328     c                   If          Trovata_Tab_TB ='S'
115000110328      *
115100110328     c                   SELECT
115200110328     c                   WHEN      Decod_Porto(Y) = 'F'
115300110328     c                               and  VABcas > *zeros
115400110328     C                   movel     '4'           VABCBO                         + C/Ass
115500110328     c                   WHEN      Decod_Porto(Y) = 'F'
115600110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
115700110328      *  Porto Assegnato
115800110328     c                   WHEN      VABCAS > *zeros
115900110328     C                   movel     '6'           VABCBO                         + C/Ass
116000110328     C                   OTHER
116100110328     C                   movel     '2'           VABCBO                         Senza C/Ass
116200110328     C                   ENDSL
116300110328      *
116400110328      ****************  altrimenti
116500110328      ******* Se non vi sono eccezioni per il cliente
116600110328      *   Rileva a questo punto lo Standard per tutti
116700110328      ****************
116800110328     c                   elseIf      Trovata_Tab_TB ='N'
116900090210      **
117000090210     c                   if              TOD_char3 <> *blank
117100090210     c                   movel(p)  TOD_char3     fld35a           35
117200090213     C                   movel     UNB_Mittente  etbUNB
117300090213     C                   movel(p)  'TOD'         etbSGM
117400090213     C                   movel(p)  TOD_char3     etbVALSGM
117500090213     c     K_TAB1L       chain     edstbl01l
117600090210      *
117700090210      * prova quindi senza l'UNB specifico del cliente
117800090213     c                   if        not %Found(edstbl01l)
117900090213     C                   clear                   etbUNB
118000090213     c     K_TAB1L       chain     edstbl01l
118100090210     c                   end
118200090210      *
118300090213     c                   if        %Found(edstbl01l)
118400090210     c                   SELECT
118500090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
118600090210     C                   movel     '4'           VABCBO                         + C/Ass
118700090213     c                   WHEN      ETBDATI = 'F'
118800090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
118900090210      *  Porto Assegnato
119000090210     c                   WHEN      VABCAS > *zeros
119100090210     C                   movel     '6'           VABCBO                         + C/Ass
119200090210     C                   OTHER
119300090210     C                   movel     '2'           VABCBO                         Senza C/Ass
119400090210     C                   ENDSL
119500090210      *
119600090210     c                   end
119700090210     c                   endIF
119800090209      **
119900110328     c                   endIF
120000110328      **
120100090210      **  Il tipo bolla non è presente
120200090210     c                   if        VABCBO = *blank
120300090210     C                   eval      vinMSG = 'TOD non rilevato'
120400090213     c                   exsr      Error_DET
120500090210     c                   goto      end_TOD
120600090210     c                   end
120700090210      **
120800090210     c     end_TOD       endSR
120900090210      * ?================================================================== */
121000090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
121100090210      * ?================================================================== */
121200090210     C     DATI_RFF      BEGSR
121300090210      *
121400090210     C                   clear                   WAGE             35
121500110224     C                   clear                   WFF              35
121600110224     C                   clear                   WCU              35
121700090210      *
121800090210      *  Trascodifico riferimento spedizione
121900091019     c                   if        RFF1153 = Riferimento_Spedizione  and
122000091019     c                             RFF1154 <> *BLANKS
122100090210     C                   movel(p)  RFF1154       WAGE             35
122200090210     c                   end
122300090210      **
122400090210     C                   IF         WAGE <> *BLANKS
122500090210     C                   movel     WAGE          WNSP             35
122600090210     C                   movel     WAGE          WNUM             35
122700090210     C     ' '           scan      WAGE          WLUNGH            3 0
122800090210     C                   sub       1             WLUNGH
122900090210     C                   EXSR      TSTNUM
123000090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
123100090210     C                   movel     WAGE          VABRMA
123200091019     C                   IF         WRMN = Riferimento_Spedizione  or
123300091019     C                              WRMN = *blanks
123400090210     C                   movel     WNUMOK        VABRMN
123500091019     C                   eval       WRMN = Riferimento_Spedizione
123600090210     C                   ENDIF
123700090210     C                   ENDIF
123800090210     C                   ENDIF
123900090210      *
124000090210      * Codice Cliente di riferimento
124100091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
124200090210     C                   movel     RFF1154       WFF
124300090210     c                   END
124400090211      *
124500090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
124600090211     C     WFF           IFNE      *BLANKS
124700090211     C                   eval      XX = 1
124800090211     C                   movel     §PTKSC        WCCL             35
124900090211     C                   movel     WFF           WCOM28           28
125000090211     C                   move      WCOM28        WCCL
125100090211     C     WCCL          lookup    CCL(XX)                                34
125200090211     C     *IN34         IFEQ      '1'
125300090211     C                   movel     DCL(XX)       EDIDSCL
125400090211     C                   z-add     §CLKSC        WCLKSC            7 0
125500090211     C                   movel     §CLTAR        WCLTAR            3
125600090211     C                   z-add     §CLLNP        WCLLNP            3 0
125700090211     C                   z-add     §CLNRS        WCLNRS            2 0
125800090211     C                   movel     §CLCTM        WCLCTM            2
125900090211     C                   movel     §CLBS1        WCLBS1            1
126000090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
126100090211      * imposta la LNP   x il dettaglio specifco
126200090211     c                   eval      VABlnp = §CLlnp
126300090211      * imposta la Serie x il dettaglio specifco
126400090211     c                   eval      VABnrs = §CLnrs
126500090211      *
126600090211      *  In base al cod.trattamento merce reperisco tipo tratt.
126700090211      *    per un dettaglio specifico.
126800090211     C                   CLEAR                   DS1B
126900090211     C                   Z-ADD     1             Y                 3 0
127000090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
127100090211     C     *IN32         IFEQ      '1'
127200090211     C                   MOVEL     DTM(Y)        DS1B
127300090211     C                   END
127400090211      *
127500090211      * Se il cliente non era sulla schiera lo aggiunge
127600090211     C     §CLKSC        lookup    KCL                                    39
127700090211     C                   if        *in39 = *off
127800090211     C                   add       1             WW
127900090211     C                   z-add     §CLKSC        KCL(WW)
128000090211     c                   endif
128100090211      *
128200090211     C                   ENDIF
128300090211     C                   ENDIF
128400090211     C*----------------------------------------------------------------
128500090210      *
128600110502     c                   if        RFF1153 = Riferimento_Numerico    and
128700110502     C                             RFF1154 <> *blanks
128800090210     C                   movel     RFF1154       WCU
128900090210     C                   END
129000090210      *
129100090210      * Numero Ordine di vendita
129200090210     C     WCU           IFNE      *BLANKS
129300090210     C                   movel     WCU           WNUM             35
129400090210     C     ' '           scan      WCU           WLUNGH
129500090210     C                   sub       1             WLUNGH
129600090210     C                   EXSR      TSTNUM
129700090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
129800090210     C                   movel     WNUMOK        VABRMN
129900110502     c                   eval      WRMN = Riferimento_Numerico
130000090210     C                   Endif
130100090210     C                   ENDIF
130200090210      *
130300090210      * NOn c'è l'identificativo bolla del cliente
130400091019     c                   if        (RFF1153 = Riferimento_Spedizione and
130500091019     c                              RFF1154 = *BLANKS)
130600091019     c                             or
130700110502     c                             (RFF1153 = Riferimento_Numerico and
130800110502     c                              RFF1154 = *BLANKS)
130900090210     C                   eval      vinMSG = 'RFF riferimento assente'
131000090213     c                   exsr      Error_DET
131100090210     c                   goto      end_RFF
131200090210     C                   ENDIF
131300090210      *
131400090210     c     end_RFF       endSR
131500090209      * ?================================================================== */
131600090213     C*?  C.O.D. Monetary Amount
131700090209      * ?================================================================== */
131800090213     C     DATI_MOA      BEGSR
131900090210      *
132000090216     C* Controllo se campo importo numerico
132100090216      * con 3 decimali
132200090216     C     moa5004       IFNE      *zeros
132300090216      *
132400090216     C     moa5025       IFEQ      '157'
132500090216     C     moa5004       div       1000          VABIAS                         IMPORTO
132600090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
132700090216     C                   END
132800090216      *
132900090216     C     moa5025       IFEQ      '77 '                                        Valore
133000090216     C     moa5004       div       1000          VABVMD                         MERCE
133100090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
133200090216     C                   END
133300090216      *
133400090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
133500090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
133600090216     C     moa5004       div       1000          VABCAS
133700090216     C                   MOVEL     moa6345       VABVCA
133800090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
133900090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
134000090216     C     §PUTPC        IFEQ      'P'
134100090216     C     moa5004       ANDGT     *ZEROS
134200090216     C                   MOVEL     'OM'          VABTIC
134300090216     C                   MOVEL     'OM'          WVABTIC
134400090216     C                   ENDIF
134500090216     C                   END
134600090216     C                   END
134700090216      *
134800090216     C                   END                                                    Imp.non numer.
134900090213      *
135000090213     c     end_MOA       endSR
135100090213      * ?================================================================== */
135200090213     C*?  FREE TEXT
135300090213      * ?================================================================== */
135400090213     C     DATI_FTX      BEGSR
135500110404      *
135600110523      *
135700110523      * ?Se è la Natura Merce
135800110523     C                   IF        ftx4451 = 'AAA'                              -- 00 --
135900110523      *
136000110523     C                   MOVEL     D05(1)        vabNAS
136100110523      *
136200110523     C                   ELSE
136300110404      *
136400110404      * ?Se non è un testo per il Pagamento
136500110404     C     ftx4451       IFne      'PAI'                                        -- 01 --
136600110404      *
136700090216      * ? solo se si tratta di note SIN ICN o PAI non altro
136800090216     C                   DO        4             X
136900090216     C     D05(X)        IFNE      *BLANKS
137000090216     C     WNOT1         IFEQ      *BLANKS
137100090216     C                   MOVEL     D05(X)        WNOT1
137200090216     C                   MOVE      D05(X)        WNOT2
137300090216     C                   ELSE
137400090216     C     WNOT2         IFEQ      *BLANKS
137500090216     C                   MOVEL     D05(X)        WNOT2
137600090216     C                   END
137700110404     C                   ENDif
137800110404     C                   ENDif
137900110404     C                   ENDdo
138000110404      *
138100110404     C                   ENDif
138200090216     C*------>>
138300090216     C*  Decodifico tipo pagamento C/Assegno
138400090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
138500090216      *      (Problema di pulizia in presenza di + righe di testo)
138600090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
138700090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
138800090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
138900090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
139000090216     C                   if        wri_vabtic = *blank
139100090216     C                   MOVEL     *BLANKS       VABTIC
139200090216     c                   end
139300090216      **
139400090216     c                   clear                   trovato_TPI       1
139500090216      *
139600090216      * ?Se c'è il tipo Incasso
139700090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
139800090216      *
139900090216     C                   DO        4             X                              -- 02 --
140000090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
140100130311      **
140200130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
140300130311      **  per gestire Partner come AZKAR che ha codice "AAA"
140400130311     C*******            MOVEL     D05(X)        WCTC              2
140500130311     C                   MOVEL     D05(X)        WCTC             10
140600090216     C                   Z-ADD     1             Y
140700090216     c                   movel(p)  Wctc          fld35a           35
140800090216     c                   move      §puTC         fld35a
140900090216     C     fld35a        LOOKUP    CTC35(Y)                               33
141000090216     C   33              MOVEL     TPI(Y)        VABTIC
141100090216     c   33              move      'S'           trovato_TPI
141200090216     C                   END                                                    -- 03 --
141300090216     C                   ENDdo                                                  -- 02 --
141400090216      *
141500090216     C                   ELSE                                                   -- 01 --
141600090216      *
141700090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
141800090216      *    Deve essere impostato comunque da testata
141900090216     c                   if        vabtic = *blank
142000090216     C                   MOVEL     wVabTIC       VABTIC
142100090216     c                   end
142200090216      *
142300090216     C                   END                                                    -- 01 --
142400090216      *
142500110523     C                   ENDif                                                  -- 00 --
142600110523      *
142700090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
142800090216      *    essere presenti altri record Flat con altre informazioni
142900090216      *    che potrebbero abblancare il Tipo incasso precedentemente
143000090216      *    decodificato
143100090216     c                   if        Trovato_TPI = 'S' and
143200090216     c                                 wri_vabtic = *blank
143300090216     c                   eval      wri_vabtic = 'S'
143400090216     c                   end
143500090213      *
143600090213     c     end_FTX       endSR
143700090213      * ?================================================================== */
143800090213     C*?  Anagrafica del mittente e del destinatario
143900090213      * ?================================================================== */
144000091016     C     DATI_NAD      BEGSR
144100090213      *
144200090210      * Dati destinatario
144300090210     C                   IF        nad3035 = 'CN' or
144400090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
144500090227      *
144600090227      * Se trovato un indirizzo di destinatario
144700090227     c                   eval       se_trovato_NAD ='S'
144800090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
144900090210      *
145000090210     C* Reperisco nazione corrispondente destinatario
145100090210     C                   Z-ADD     1             YY                3 0
145200090210     c                   eval      *in32 = *off
145300090210     C                   if        nad3207 <> *BLANKS
145400090210     C     nad3207       LOOKUP    ISO(YY)                                32
145500090210     C                   endif
145600090210      *
145700090210     C* Imposto dati destinatario
145800090211     C                   MOVEL     nad31241      VABRSD
145900090211     C                   if        nad31242 <> *LOVAL
146000090211     C                   MOVEL     nad31242      VABRD2
146100090210     C                   endif
146200090211      *
146300091019????  * DAL PARTY NAME
146400091019???? C* Imposto dati destinatario
146500091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
146600091019???? C                   MOVEL     nad30361      VABRSD
146700091019???? C                   if        nad30362 <> *LOVAL
146800091019???? C                   MOVEL     nad30362      VABRD2
146900091019???? C                   endif
147000091019???? C                   end
147100090211      *
147200090210     C                   MOVEL     nad30421      VABIND
147300090210     C                   MOVEL     nad3164       VABLOD
147400090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
147500090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
147600090210      *
147700090210     C* Verifico se c'è dello spazio per nella località
147800090210     C     '    '        SCAN      VABLOD        WI                4 0
147900090210      *
148000090210     C                   MOVEA     VABLOD        LOD
148100090210     C                   ADD       1             WI
148200090210     C                   MOVEA     NAD30422      LOD(WI)
148300090210     C                   MOVEA     LOD           VABLOD
148400090210     C                   ENDif
148500090210      *
148600090210     C                   If        *in32 = *on
148700090210     C                   MOVEL     C15(YY)       VABNZD
148800090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
148900090210     c                   Else
149000090210     C                   MOVEL     *BLANKS       VABNZD
149100090210     C                   Z-ADD     0             WFLCPF
149200090210     c                   Endif
149300090210      * Converto CAP
149400090210     C                   EXSR      CNVCAP
149500090603     C*
149600090603     C* Se in testata
149700090603     c                   if        Inizio_dettaglio = *blank
149800090603     c                   eval       tes_VABrsd = VABrsd
149900090603     c                   eval       tes_VABrd2 = VABrd2
150000090603     c                   eval       tes_VABind = VABind
150100090603     c                   eval       tes_VABlod = VABlod
150200090603     c                   eval       tes_VABnzd = VABnzd
150300090603     c                   eval      NAD_destinatario_in_testata = 'S'
150400090603     c                   end
150500090603     C*
150600090210     C                   ENDIF
150700090210      *
150800090210      * Dati mittente
150900091016     C                   IF        nad3035 = 'CZ'
151000090210     C                   movel     nad30361      VABRMO
151100090210      *
151200090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
151300090210     C                   movel     §PTNAZ        VABNMO
151400090210      *
151500090210     C                   IF        nad3207 <> *blanks
151600090210     C                   eval      YY = 1
151700090210     C     nad3207       LOOKUP    ISO(YY)                                32
151800090210     C                   if         *in32 = *on
151900090210     C                   MOVEL     C15(YY)       VABNMO
152000090210     C                   endif
152100090210     C                   ENDIF
152200090210      *
152300090210      * Normalizzo CAP mittente originale
152400090210     C                   clear                   T
152500090210     C                   clear                   S
152600090210     C                   MOVEA     *BLANKS       CAPM
152700090210     C                   MOVEA     nad3251       CAP9
152800090210     C                   DO        9             T
152900090210     C     CAP9(T)       IFNE      *BLANKS
153000090210     C                   ADD       1             S
153100090210     C                   MOVE      CAP9(T)       CAPM(S)
153200090210     C                   ENDIF
153300090210     C                   ENDDO
153400090210     C*
153500090210      *     Controllo validità CAP
153600090210     C                   MOVEA     CAPM          VABCMO
153700090210      *
153800090210     C                   CLEAR                   TISI95DS
153900090210     C                   MOVEL     VABCMO        I95CAP
154000090210     C                   MOVEL     VABNMO        I95NAR
154100090210     C                   MOVEL     WOGGI         I95DAT
154200090210     C                   MOVEL     '3'           I95TCN
154300090210     C                   CALL      'TISI95R'
154400090210     C                   PARM                    TISI95DS
154500090210      *     Errore
154600090210     C     O95ERR        IFNE      *BLANKS
154700090210     C                   MOVEL     INDCAE        VABCMO
154800090210     C                   MOVEL     §PTNAZ        VABNMO
154900090210     C                   END
155000090603      *
155100090603     C* Se in testata
155200090603     c                   if        Inizio_dettaglio = *blank
155300090603     c                   eval      tes_VABrmo = VABrmo
155400090603     c                   eval      tes_VABnmo = VABnmo
155500090603     c                   eval      tes_VABcmo = VABcmo
155600090603     c                   eval      NAD_mittente_in_testata = 'S'
155700090603     c                   end
155800090210      *
155900090210     C                   END
156000090210      *
156100090210      * NOn c'è l'identificativo bolla del cliente
156200090210     C                   if          vabRSD = *blank or
156300090210     C                               vabIND = *blank or
156400090210     C                               vabLOD = *blank
156500090211     C                   IF        nad3035 = 'CN'
156600090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
156700090213     c                   exsr      Error_DET
156800090210     c                   goto      end_NAD
156900090210     C                   ENDIF
157000090211     C                   ENDIF
157100090210      *
157200090210     c     end_NAD       endSR
157300090210      * ?================================================================== */
157400090210     C*?  Contatto a cui far riferimento
157500090210      * ?================================================================== */
157600090210     C     DATI_CTA      BEGSR
157700090210      *
157800090210      *  memorizza il riferimento di consegna x il destinatario
157900090210      *  memorizza il riferimento di consegna nr.telefonico
158000090210     C     cta3412       ifne      *BLANKS
158100090210     c                   movel     cta3412       watrde
158200090210     c                   end
158300090210      *
158400090210     c                   endSR
158500090210      * ?================================================================== */
158600090210     C*?  Contatto telefonico  a cui far riferimento
158700090210      * ?================================================================== */
158800090210     C     DATI_COM      BEGSR
158900090210      *
159000090210     C     com3148       ifne      *BLANKS
159100090210     c                   movel     com3148       wattel
159200090210     c                   end
159300090210      *
159400090209     C                   ENDSR
159500090210      * ?================================================================== */
159600090210     C*?  Dati di dettaglio della spedizione e dei colli
159700090210      * ?================================================================== */
159800090210     C     DATI_GID      BEGSR
159900090210      *
160000090210     C* Se ho totali per spedizione metto in campi VAB
160100090210     C     gid1496       IFEQ      99999
160200090210     C     gid1496       orEQ      9999
160300090210     C                   MOVEL     'S'           WGID99
160400090210     C                   z-add     gid722420     VABNCL
160500090210     C                   ELSE
160600090210     C     WGID99        IFEQ      'N'
160700090210     C                   z-add     gid722420     WTNCL             5 0
160800090210     C                   ADD       WTNCL         VABNCL
160900090210     C                   END
161000090210     C                   END
161100090212      *
161200090210     C                   ADD       VABNCL        WTOCOC           16 0
161300090210      *
161400090210     c     end_GID       endSR
161500090210      * ?================================================================== */
161600090210     C*?  Dati di dettaglio misure spedizione e colli
161700090210      * ?================================================================== */
161800090210     C     DATI_MEA      BEGSR
161900090210      *
162000090210     C*  Peso
162100090210     C     mea6311       IFEQ      'WT '
162200090210     C     mea6411       ANDEQ     'KGM'
162300090210      *
162400090210     C*  Controllo se ho valore totale o parziale
162500090210      *   sempre il Lordo
162600090210     C     WS1496        IFEQ      99999
162700090210     C     WS1496        orEQ      9999
162800091019???? C                   move      mea6314       mea6314_2        18 2          Peso
162900091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
163000090210     C                   ELSE
163100091019???? C     WGID99        IFEQ      'N'
163200091019???? C                   move      mea6314       mea6314_2                      Peso
163300110718???? C******             ADD       mea6314_2     VABPKB                         Peso
163400110718???? C                   ADD       mea6314_2     Work_KG                        Peso
163500110718     c                   eval         vabPKB = Work_KG
163600090210     C                   END
163700090210     C                   END
163800090210      *
163900160511???? C     Work_KG       IFgt      0
164000091019???? C                   move      mea6314       mea6314_2                      Peso
164100091019???? C                   ADD       mea6314_2     WTOPLC
164200090210     C                   END
164300090210     C                   END
164400090210      ***
164500090210     C*  Peso in Grammi  se abilitato a utilizzarlo
164600090210      ***
164700090210     C     §puGR         IFeq      'S'
164800090210     C     mea6311       IFEQ      'WT '
164900090210     C     mea6411       ANDEQ     '163'
165000090210      *
165100090210     C*  Controllo se ho valore totale o parziale
165200091019???? C     WS1496        IFEQ      99999
165300091019???? C     WS1496        orEQ      9999
165400091019???? c                   move      mea6314       mea6314_2
165500091019???? C     mea6314_2     div       1000          VABPKB                         Peso
165600090210     C                   ELSE
165700091019???? C     WGID99        IFEQ      'N'
165800091019???? c                   move      mea6314       mea6314_2
165900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
166000110718???? C*******            add       Peso_inKG     VABPKB                         Peso
166100110718???? C                   ADD       Peso_inKG     Work_KG                        Peso
166200110718     c                   eval         vabPKB = Work_KG
166300090210     C                   END
166400090210     C                   END
166500090210      *
166600160511???? C     Work_KG       IFgt      0
166700091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
166800091019???? C                   ADD       Peso_inKG     WTOPLC
166900090210     C                   END
167000090210     C                   END
167100160511     C                   END
167200090210      *  Volume
167300091019???? C     mea6311       IFEQ      'VOL'
167400091019???? C     mea6411       ANDEQ     'MTQ'
167500090210      *  Controllo se ho valore totale o parziale
167600090210     C     WS1496        IFEQ      99999
167700090210     C     WS1496        orEQ      9999
167800091019???? C                   move      mea6314       mea6314_2        18 2          Volume
167900091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
168000091019???? C                   Z-ADD     mea6314_2     PVLB
168100090210     C                   ELSE
168200091019     C     WGID99        IFEQ      'N'
168300091019???? C                   move      mea6314       mea6314_2        18 2          Volume
168400091019???? C                   ADD       mea6314_2     VABVLB                         Volume
168500091019???? C                   ADD       mea6314_2     PVLB
168600090210     C                   END
168700090210     C                   END
168800090210     C                   END
168900090210      *
169000090210     C* Ricerco CAP
169100090210     C     WCAP          IFNE      *BLANKS
169200090210     C     mea6311       IFEQ      'WT '
169300090210      *
169400090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
169500090210     C                   CLEAR                   FNLV55DS
169600090210     C                   MOVEL     'P'           D55TPT
169700090210     C                   MOVEL     WOGGI         D55DRF
169800090210     C                   MOVEL     VABLNP        D55LNP
169900090210     C                   MOVEL     VABLNP        D55LIN
170000090210     C                   CALL      'FNLV55R'
170100090210     C                   PARM                    FNLV55DS
170200090210     C                   MOVE      D55TFP        COMTFP            3 0
170300090210      *
170400090210     C*  PASSO IL TERMINAL PARTENZA
170500090210     C                   CLEAR                   TISI95DS
170600090210     C                   Z-ADD     COMTFP        I95TFP
170700090210     C                   MOVEL     VABTSP        I95TSP
170800090210     C                   MOVEL     'S'           I95FRE
170900090210     C                   MOVEL     '3'           I95TCN
171000090210     C                   MOVEL     WCAP          I95CAP
171100090210     C                   MOVEL     VABNZD        I95NAR
171200090210     C                   MOVEL     VABLOD        I95LOC
171300090210     C                   MOVEL     VABIND        I95IND
171400090210      *
171500090210     C* Se gestita seconda linea di arrivo passo peso - volume
171600090210     C* e numero colli effettivo
171700090210     C     §PULNA        IFEQ      'S'
171800090210     C                   Z-ADD     VABPKB        I95LKG
171900090210     C                   Z-ADD     VABVLB        I95LMC
172000090210     C                   ELSE
172100090210     C* Altrmenti passo 1 Kg e 0,001
172200090210     C***                  Z-ADD1         I95LKG
172300090210     C***                  Z-ADD0,001     I95LMC
172400090210     C                   END
172500090210     C*
172600090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
172700090210     C                   SELECT
172800090210     C     VABCBO        WHENEQ    '1 '
172900090210     C                   MOVEL     *BLANKS       I95FCA
173000090210     C                   MOVEL     'F'           I95TPO
173100090210     C     VABCBO        WHENEQ    '2 '
173200090210     C                   MOVEL     *BLANKS       I95FCA
173300090210     C                   MOVEL     'A'           I95TPO
173400090210     C     VABCBO        WHENEQ    '4 '
173500090210     C                   MOVEL     'S'           I95FCA
173600090210     C                   MOVEL     'F'           I95TPO
173700090210     C     VABCBO        WHENEQ    '6 '
173800090210     C                   MOVEL     'S'           I95FCA
173900090210     C                   MOVEL     'A'           I95TPO
174000090210     C                   OTHER
174100090210     C                   MOVEL     'F'           I95TPO
174200090210     C     VABCAS        IFGT      0
174300090210     C                   MOVEL     'S'           I95FCA
174400090210     C                   ELSE
174500090210     C                   MOVEL     *BLANKS       I95FCA
174600090210     C                   END
174700090210     C                   ENDSL
174800090210     C*
174900090210     C                   CALL      'TISI95R'
175000090210     C                   PARM                    TISI95DS
175100090210     C* Reperisco i dati da CAP
175200090210     C                   MOVEL     O95PRV        VABPRD
175300090210     C                   MOVEL     O95LNA        VABLNA
175400090210     C                   MOVEL     O95ZNC        VABZNC
175500090210     C                   MOVEL     WCAP          VABCAD
175600090210     C*
175700090210     C     O95ERR        IFNE      *BLANKS
175800090210     C     O95FIT        ANDEQ     'S'
175900090210     C*  Cap non trovato
176000090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
176100090213     c                   exsr      Error_DET
176200090210     C                   goto      end_MEA
176300090210     C                   END
176400090210     C                   END
176500090210      *
176600090210     C                   ELSE
176700090210      *
176800090210     C                   CLEAR                   VABPRD
176900090210     C                   CLEAR                   VABLNA
177000090210     C                   CLEAR                   VABZNC
177100090210     C                   CLEAR                   VABCAD
177200090210     C*  Cap non trovato
177300090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
177400090213     c                   exsr      Error_DET
177500090210     C                   goto      end_MEA
177600090210     C                   END
177700090210      *
177800090210     c     end_MEA       endSR
177900090210      * ?================================================================== */
178000090210     C*?  Dati di dettaglio Codice a barre Segnacolli
178100090210      * ?================================================================== */
178200090210     C     DATI_PCI      BEGSR
178300090210      *
178400090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
178500090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
178600090210     C     §1BF12        IFEQ      'E'
178700090210     C                   EXSR      SEGNEE
178800090210     C                   ELSE
178900090210      *
179000090210      *  La prima volta controllo congruità numero segnacollo
179100090210     C     §PUBS1        IFNE      *BLANKS
179200090210     C     VABnrs        ANDNE     0
179300090210      *  Se il codice trattamento merce prevede che segnacollino i
179400090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
179500090210     C     §1BFG7        IFEQ      'S'
179600090210     C     §1BFG1        OREQ      'S'
179700090210      *  calcolo segnacollo
179800090210     C                   EXSR      SGNELA
179900090210     C                   END
180000090210     C                   END
180100090210      *
180200090210     C                   END
180300090210      *
180400090210     c     end_PCI       endSR
180500090211     C*----------------------------------------------------------------
180600090211      *? dati finali del dettaglio    UNT
180700090211     C*----------------------------------------------------------------
180800090211     C     DATI_UNT      BEGSR
180900090211      *
181000090213     c                   eval      NUM_RECord = vnREC
181100090213     c                   z-add     vnrec         last_RECord
181200090211      **
181300090211     c                   endSR
181400090211     C*----------------------------------------------------------------
181500090211      *? dati finali     UNT
181600090211     C*----------------------------------------------------------------
181700090211     C     DATI_UNZ      BEGSR
181800090211      *
181900090211      **
182000090211     c                   endSR
182100090210      *----------------------------------------------------------------
182200090210      *? Input segnacolli su schiera di 20 elementi
182300090210      *----------------------------------------------------------------
182400090210     C     INP_Segnacol  BEGSR
182500090210      *
182600090210     c                   clear                   quanti_PCI        3 0
182700090210     c                   if        §puEL1 = 0
182800090210     c                   z-add     10            quanti_PCI
182900090210     c                   else
183000090210     c                   z-add     §puEL1        quanti_PCI
183100090210     c                   end
183200090210      *
183300090210      *  Pulisce la schiera dei segnacolli da elaborare
183400090210     c                   clear                   x30
183500090210     c                   z-add     0             Nr_x30            3 0
183600090210      *
183700090210      *   riporto tutto sulla schiera generica X30
183800090210      *
183900090210     c                   do        10            limite            3 0
184000090210      * Limita quanti elementi passati in ogni PCI
184100090210      *   devono essere considerati
184200090210     c                   if        limite > quanti_PCI
184300090210     c                   Leave
184400090210     c                   end
184500090210      *
184600090210      *  Carica la schiera generale di tutti i segnacolli
184700090210     c                   if        D30_1(limite) <> *blank
184800090210     c                   add       1             Nr_x30
184900090210     c                   eval      X30(Nr_x30) = D30_1(limite)
185000090210     c                   end
185100090210     c                   enddo
185200090210      *
185300090210     C                   ENDSR
185400090210      *----------------------------------------------------------------
185500090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
185600090210      *----------------------------------------------------------------
185700090210     C     SEGNEE        BEGSR
185800090210      *
185900090210     c                   exsr      INP_Segnacol
186000090210      *
186100090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
186200090210     C                   DO        10            x                              --- 01 ---
186300090210      *
186400090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
186500090210     C     x30(X)        ANDNE     *LOVAL
186600090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
186700090210     C                   ADD       1             §§
186800090210     C                   MOVEL     x30(X)        SGE(§§)
186900090210     C                   END                                                    --- 03 ---
187000090210      *
187100090210     C                   END                                                    --- 02 ---
187200090210      *
187300090210     C                   ENDSR
187400090210     C*----------------------------------------------------------------
187500090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
187600090210     C*----------------------------------------------------------------
187700090210     C     SGNELA        BEGSR
187800090210      *
187900090210     c                   exsr      INP_Segnacol
188000090210     C*
188100090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
188200090210     C*  in base al numero elementi validi carico sgn.
188300090210     C                   DO        10            X                              --- 01 ---
188400090210     C*
188500090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
188600090210     C     x30(X)        ANDNE     *LOVAL
188700090210     C*
188800090210     C                   CLEAR                   DSSGN
188900090210     C*  Controllo se devo ricevere la linea di partenza
189000090210     C                   MOVEL     'N'           WERRO             1
189100090210     C     §PULP1        IFGT      0                                            --- 03 ---
189200090210     C                   EXSR      REPLNP
189300090210     C                   END                                                    --- 03 ---
189400090210     C*  Controllo se devo ricevere la linea di arrivo
189500090210     C     §PULA1        IFGT      0                                            --- 03 ---
189600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189700090210     C                   EXSR      REPLNA
189800090210     C                   END                                                    --- 03 ---
189900090210     C*  Controllo se devo ricevere il numero di serie
190000090210     C     §PUNR1        IFGT      0                                            --- 03 ---
190100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
190200090210     C                   EXSR      REPNRS
190300090210     C                   END                                                    --- 03 ---
190400090210     C*  Controllo se devo ricevere il numero segnacollo
190500090210     C     §PUSG1        IFGT      0                                            --- 03 ---
190600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
190700090210     C                   EXSR      REPSGN
190800090210     C                   END                                                    --- 03 ---
190900090210     C*  Controllo se devo ricevere la zona di consegna
191000090210     C     §PUZN1        IFGT      0                                            --- 03 ---
191100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
191200090210     C                   EXSR      REPZNC
191300090210     C                   END                                                    --- 03 ---
191400090210     C*  Imposto da VAB i dati a zero se mi è stato passato
191500090210     C*  numero segnacollo valido
191600090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
191700090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
191800090210     C                   Z-ADD     VABLNP        SGNLNP
191900090210     C                   END                                                    --- 04 ---
192000090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
192100090210     C                   Z-ADD     VABLNA        SGNLNA
192200090210     C                   END                                                    --- 04 ---
192300090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
192400090210     C                   Z-ADD     VABNRS        SGNNRS
192500090210     C                   END                                                    --- 04 ---
192600090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
192700090210     C                   Z-ADD     VABZNC        SGNZNC
192800090210     C                   END                                                    --- 04 ---
192900090210      *
193000090210     C*  mi reimposto LNA e zona da segnacollo
193100090210     C                   Z-ADD     SGNLNA        VABLNA
193200090210     C                   Z-ADD     SGNZNC        VABZNC
193300090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
193400090210     C                   ADD       1             XY
193500090210     C                   MOVE      SGNNSC        SGN(XY)
193600090210     C                   END                                                    --- 03 ---
193700090210     C*
193800090210     C                   END                                                    --- 02 ---
193900090210     C*
194000090210     C                   END                                                    --- 01 ---
194100090210      *
194200090210     C                   ENDSR
194300090210     C*----------------------------------------------------------------
194400090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
194500090210     C*----------------------------------------------------------------
194600090210     C     REPLNP        BEGSR
194700090210     C*
194800090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
194900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
195000090210     C*  dal numero segnacollo passatomi dal partner
195100090210     C                   Z-ADD     §PULP1        WP                2 0
195200090210     C     3             SUBST     x30(X):WP     WLNP              3
195300090210     C*
195400090210     C*  controllo che la lnp calcolata sia numerica
195500090210     C                   TESTN                   WLNP                 98
195600090210     C*
195700090210     C*  se è numerica la carico
195800090210     C     *IN98         IFEQ      '1'
195900090210     C                   MOVE      WLNP          SGNLNP
196000090210     C                   ELSE
196100090210     C*  se non è numerica stampo errore
196200090210     C                   MOVEL     'S'           WERRO             1
196300090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
196400090213     c                   exsr      Error_DET
196500090210     C                   goto      end_LNP
196600090210     C                   END
196700090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
196800090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
196900090210     C     SGNLNP        IFNE      VABLNP
197000090210     C                   MOVEL     'S'           WERRO             1
197100090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
197200090210     c                             a della Bolla'
197300090213     c                   exsr      Error_DET
197400090210     C                   goto      end_LNP
197500090210     C                   END
197600090210     C*
197700090210     C     end_LNP       ENDSR
197800090210     C*----------------------------------------------------------------
197900090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
198000090210     C*----------------------------------------------------------------
198100090210     C     REPLNA        BEGSR
198200090210     C*
198300090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
198400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
198500090210     C*  dal numero segnacollo passatomi dal partner
198600090210     C                   Z-ADD     §PULA1        WP                2 0
198700090210     C     3             SUBST     x30(X):WP     WLNA              3
198800090210     C*  controllo che la lna calcolata sia numerica
198900090210     C                   TESTN                   WLNA                 98
199000090210     C*  se è numerica la carico
199100090210     C     *IN98         IFEQ      '1'
199200090210     C                   MOVE      WLNA          SGNLNA
199300090210     C                   ELSE
199400090210     C*  se non è numerica stampo errore
199500090210     C                   MOVEL     'S'           WERRO             1
199600090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
199700090213     c                   exsr      Error_DET
199800090210     C                   goto      end_LNA
199900090210     C                   END
200000090210     C*
200100090210     C     end_LNA       ENDSR
200200090210     C*----------------------------------------------------------------
200300090210     C*? REPNRS - Reperisce numero serie
200400090210     C*----------------------------------------------------------------
200500090210     C     REPNRS        BEGSR
200600090210     C*
200700090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
200800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
200900090210     C*  dal numero segnacollo passatomi dal partner
201000090210     C                   Z-ADD     §PUNR1        WP                2 0
201100090210     C     2             SUBST     x30(X):WP     WNRS              2
201200090210     C*  controllo che il nrs calcolata sia numerico
201300090210     C                   TESTN                   WNRS                 98
201400090210     C*  se è numerica la carico
201500090210     C     *IN98         IFEQ      '1'
201600090210     C                   MOVE      WNRS          SGNNRS
201700090210     C                   ELSE
201800090210     C*  se non è numerico stampo errore
201900090210     C                   MOVEL     'S'           WERRO             1
202000090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
202100090213     c                   exsr      Error_DET
202200090210     C                   goto      end_NRS
202300090210     C                   END
202400090210      *
202500090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
202600090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
202700090210     C     SGNNRS        IFNE      VABNRS
202800090210     C                   MOVEL     'S'           WERRO             1
202900090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
203000090210     c                             o della Bolla'
203100090213     c                   exsr      Error_DET
203200090210     C                   goto      end_NRS
203300090210     C                   END
203400090210     C*
203500090210     C     end_NRS       ENDSR
203600090210     C*----------------------------------------------------------------
203700090210     C*? REPZNC - Reperisce zona consegna
203800090210     C*----------------------------------------------------------------
203900090210     C     REPZNC        BEGSR
204000090210     C*
204100090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
204200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
204300090210     C*  dal numero segnacollo passatomi dal partner
204400090210     C                   Z-ADD     §PUZN1        WP                2 0
204500090210     C     2             SUBST     x30(X):WP     WZNC              2
204600090210     C*  controllo che la zona calcolata sia numerica
204700090210     C                   TESTN                   WZNC                 98
204800090210     C*  se è numerica la carico
204900090210     C     *IN98         IFEQ      '1'
205000090210     C                   MOVE      WZNC          SGNZNC
205100090210     C                   ELSE
205200090210     C*  se non è numerica stampo errore
205300090210     C                   MOVEL     'S'           WERRO             1
205400090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
205500090213     c                   exsr      Error_DET
205600090210     C                   goto      end_ZNC
205700090210     C                   END
205800090210     C*
205900090210     C     end_ZNC       ENDSR
206000090210     C*----------------------------------------------------------------
206100090210     C*? REPSGN - Reperisce numero segnacollo
206200090210     C*----------------------------------------------------------------
206300090210     C     REPSGN        BEGSR
206400090210     C*
206500090210     C*  se viene passata nr.segnacollo estraggo sottostringa
206600090210     C*  lunga 7 chr (a partire dalla posizione iniziale
206700090210     C*  indicata in tabella)
206800090210     C*  dal numero segnacollo passatomi dal partner
206900090210     C                   Z-ADD     §PUSG1        WP                2 0
207000090210     C     7             SUBST     x30(X):WP     WSGN              7
207100090210     C*  controllo che il nr.segnacollo calcolato sia numerico
207200090210     C                   TESTN                   WSGN                 98
207300090210     C*  se è numerico lo carico
207400090210     C     *IN98         IFEQ      '1'
207500090210     C                   MOVE      WSGN          SGNNSC
207600090210     C                   ELSE
207700090210     C*  se non è numerica stampo errore
207800090210     C                   MOVEL     'S'           WERRO             1
207900090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
208000090213     c                   exsr      Error_DET
208100090210     C                   goto      end_SGN
208200090210     C                   END
208300090210     C*
208400090210     C     end_SGN       ENDSR
208500051205      *----------------------------------------------------*
208600051205      *   DEFINIZIONE CHIAVI                               *
208700051205      *----------------------------------------------------*
208800051205     C     *INZSR        BEGSR
208900051205      *
209000991129     c     *ENTRY        PLIST
209100060613     C                   parm                    esito             1
209200971215      *
209300050112     C     KTABel        KLIST
209400050112     C                   KFLD                    TBLKUT
209500050112     C                   KFLD                    TBLCOD
209600050112     C                   KFLD                    TBLKEY
209700090210      *
209800090210     C     K_TAB1L       KLIST
209900090213     C                   KFLD                    etbUNB
210000090213     C                   KFLD                    etbSGM
210100090213     C                   KFLD                    etbVALSGM
210200090209     C*
210300090209     C* Definisco chiavi di accesso
210400090209     C     KTAB1         KLIST
210500090209     C                   KFLD                    KKUT
210600090209     C                   KFLD                    KCOD
210700090209     C*
210800090209     C     KNUF          KLIST
210900090209     C                   KFLD                    KAAA
211000090209     C                   KFLD                    KCNU
211100090209     C                   KFLD                    KFIL
211200090209      *
211300090209     C     KVAB1         KLIST
211400090209     C                   KFLD                    KCCM
211500090209     C                   KFLD                    KCMR
211600090209     C                   KFLD                    KCNT
211700090209     C                   KFLD                    KAAS
211800090209     C                   KFLD                    KLNP
211900090209     C                   KFLD                    KNRS
212000090209     C                   KFLD                    KNSP
212100090209      *
212200090209     C     KVAB2         KLIST
212300090209     C                   KFLD                    KCCM
212400090209     C                   KFLD                    KCMR
212500090209      *
212600090209     C     KRDE          KLIST
212700090209     C                   KFLD                    KAAS
212800090209     C                   KFLD                    KLNP1
212900090209     C                   KFLD                    KNRS
213000090209     C                   KFLD                    KNSP
213100090209     C                   KFLD                    KNMC
213200090209     C                   KFLD                    KNRP
213300090209      *
213400090209     C     KACO          KLIST
213500090209     C                   KFLD                    KKUT
213600090209     C                   KFLD                    KKCC
213700090209     C                   KFLD                    KKSC
213800090209      *
213900090209     C     KIND          KLIST
214000090209     C                   KFLD                    KKUT
214100090209     C                   KFLD                    KKCC
214200090209     C                   KFLD                    KKSC
214300090209      *
214400090209      * DETERMINO SE SONO BARTOLINI O SDI
214500090209     C                   CLEAR                   TIBS55DS
214600090209     C                   MOVEL     'L'           I50TLA
214700090209     C                   CALL      'TIBS55R'
214800090209     C                   PARM                    TIBS55DS
214900090209      *
215000090209     C* RECUPERO DATI DELL'UTENTE
215100090209     C                   Z-ADD     1             CODUT             1 0
215200090209     C                   CALL      'XPARUT'
215300090209     C                   PARM                    UTEDSE0F
215400090209     C                   MOVEL     RAGUT         RSUT             20
215500090209     C*
215600090209     C* RICERCA CAPOCONTI
215700090209     C                   DO        50            X
215800090209     C                   MOVE      TCU(X)        TCUDS
215900090209     C     F56           IFEQ      'CG'
216000090209     C     F34           ANDEQ     '01'
216100090209     C                   Z-ADD     KCU(X)        KCI               4 0
216200090209     C                   END
216300090209     C                   END
216400090209     C*
216500090211     c                   movel(p)  'EDPAB'       User_Msg         10
216600090211     C*
216700090209     C* IMPOSTO A ZERO VARIABILI DI PGM
216800090209     c                   move      *blank        Snd_Msg_alert     1
216900090209     C                   MOVEL     CA(1)         WCA              78
217000090209     C                   MOVEL     CN(1)         WCN              78
217100090209     C*
217200090209     C* RECUPERO DATA E ORA
217300090209     C                   TIME                    WHHDAT           14 0
217400090209     C                   MOVE      WHHDAT        G02DAT
217500090209     C                   MOVE      WHHDAT        WDATA8            8 0
217600090209     C                   MOVE      WDATA8        WAA2              2 0
217700090209     C                   MOVEL     WDATA8        DATA              6 0
217800090209     C                   MOVE      WAA2          DATA
217900090209     C                   MOVEL     *BLANK        G02ERR
218000090209     C                   CALL      'XSRDA8'
218100090209     C                   PARM                    WLBDA8
218200090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
218300090209     C                   TIME                    WORA              6 0
218400090209      * Recupero data e ora
218500090209     C                   TIME                    W0140
218600090209      * UDATE IN GGMMAAAA
218700090209     C                   MOVE      W0140         WDTGIO
218800090209      * UDATE IN AAAAMMGG
218900090209     C     *eur          MOVEL     WDTGIO        DATA_eur
219000090209     C     *iso          MOVEL     DATA_eur      dateu
219100090209      *
219200090209      * ?              /*--------------------------- */
219300090209     c                   exsr      CARICA_TABELLE
219400090209      * ?              /*--------------------------- */
219500090209      *
219600090209      *
219700090209     C                   MOVEL     *ALL'-'       CMP132          132
219800090209     C                   CLEAR                   EDiVAB00
219900090211     C                   clear                   vablnp_PT
220000090212     C                   clear                   vabnrs_PT
220100090211     C                   clear                   vabctm_PT
220200090211     C                   clear                   VABfnsp_PT
220300090209     C                   MOVEA     *HIVAL        SGN
220400991124      *
220500050414      *------------------
220600991124     C                   ENDSR
220700060621      * ?------------------------------------------------------------------ */
220800090209      *?    CARICA TABELLE SU SCHIERE
220900060621      * ?------------------------------------------------------------------ */
221000090209     C     CARICA_TABELLEBEGSR
221100090205      *
221200090209     C* Caricamento Tabella 1B
221300090209     C                   Z-ADD     0             X                 4 0
221400090209     C                   Z-ADD     CODUT         KKUT
221500090209     C                   MOVEL     '1B'          KCOD
221600090209     C     KTAB1         CHAIN     TABEL00F                           30
221700090209     C     *IN30         DOWEQ     '0'
221800090209     C     TBLFLG        IFEQ      *BLANKS
221900090209     C                   ADD       1             X
222000090209     C                   MOVEL     TBLKEY        CTM(X)
222100090209     C                   MOVEL     TBLUNI        DTM(X)
222200090209     C                   END
222300090209     C     KTAB1         READE     TABEL00F                               30
222400090209     C                   END
222500090209      *
222600090209     C* Caricamento Tabella 15
222700090209     C                   Z-ADD     0             X                 4 0
222800090209     C                   Z-ADD     CODUT         KKUT
222900090209     C                   MOVEL     '15'          KCOD
223000090209     C     KTAB1         CHAIN     TABEL00F                           30
223100090209     C     *IN30         DOWEQ     '0'
223200090209     C     TBLFLG        IFEQ      *BLANKS
223300090209     C                   ADD       1             X
223400090209     C                   MOVEL     TBLKEY        C15(X)
223500090209     C                   MOVEL     TBLUNI        DS15
223600090209     C                   MOVEL     §15COD        ISO(X)
223700090209     C                   MOVEL     §15ECF        CPF(X)
223800090209     C                   MOVE      §15PCF        CPF(X)
223900090209     C                   END
224000090209     C     KTAB1         READE     TABEL00F                               30
224100090209     C                   END
224200090209      *
224300090209     C* Caricamento Tabella CV
224400090209     C                   Z-ADD     0             X
224500090209     C                   Z-ADD     CODUT         KKUT
224600090209     C                   MOVEL     'CV'          KCOD
224700090209     C     KTAB1         CHAIN     TABEL00F                           30
224800090209     C     *IN30         DOWEQ     '0'
224900090209     C     TBLFLG        IFEQ      *BLANKS
225000090209     C                   ADD       1             X
225100090209     C                   MOVEL     TBLKEY        CCV(X)
225200090209     C                   MOVEL     TBLUNI        DCV(X)
225300090209     C                   END
225400090209     C     KTAB1         READE     TABEL00F                               30
225500090209     C                   END
225600090216      *
225700110328      * Caricamento Tabella termini consegna
225800110328     C                   Z-ADD     0             X
225900110328     C                   MOVEL     'TB'          TABCOD
226000110328     C     TABCOD        CHAIN     EDTAB01L                           30
226100110328     C     *IN30         DOWEQ     '0'
226200110328     C     TABFLG        IFEQ      *BLANKS
226300110328     C                   ADD       1             X
226400110328     C                   eval      K35_TpBolla(X) = TABKEY
226500110328     C                   eval      Cod_TpBolla(X) = TABKEY
226600110328     C                   eval      Decod_Porto(X) = TABUNI
226700110328     C                   END
226800110328     C     TABCOD        READE     EDTAB01L                               30
226900110328     C                   END
227000110328      *
227100090216     C* Caricamento Tabella Tipo Incasso C/Assegno
227200090216     C                   Z-ADD     0             X
227300090216     C                   MOVEL     'TC'          TABCOD
227400090216     C     TABCOD        CHAIN     EDTAB01L                           30
227500090216     C     *IN30         DOWEQ     '0'
227600090216     C     TABFLG        IFEQ      *BLANKS
227700090216     C                   ADD       1             X
227800090216     C                   MOVEL     TABKEY        CTC35(X)
227900090216     C                   MOVEL     TABUNI        TPI(X)
228000090216     C                   END
228100090216     C     TABCOD        READE     EDTAB01L                               30
228200090216     C                   END
228300090209      *
228400090209      * Caricamento Tabella Partner esteri
228500090209     C                   Z-ADD     0             X
228600090209     C                   MOVEL     'PT'          TABCOD
228700090209     C     TABCOD        CHAIN     EDTAB01L                           30
228800090209     C     *IN30         DOWEQ     '0'
228900090209      *
229000090209     C                   SELECT
229100090209     C     TABFLG        WHENNE    *BLANKS
229200090209      *
229300090209     C                   OTHER
229400090209     C                   ADD       1             X
229500090209     C                   MOVEL     TABKEY        CPT(X)
229600090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
229700090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
229800090209     C                   MOVEL     TABUNI        DPT(X)
229900090209     C                   ENDSL
230000090209      *
230100090209     C     TABCOD        READE     EDTAB01L                               30
230200090209     C                   END
230300121105      *
230400121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
230500121105     c                   exsr      ChecK_PT
230600121105      *
230700090209      * salva quanti Codici UNB ha caricato
230800090209     c                   z-add     x             sav_CPTx
230900090209      *
231000090209     C                   MOVEL     'PU'          TABCOD
231100090209     C     TABCOD        CHAIN     EDTAB01L                           30
231200090209     C     *IN30         DOWEQ     '0'
231300090209      *
231400090209     C                   SELECT
231500090209     C     TABFLG        WHENNE    *BLANKS
231600090209      *
231700090209     C                   OTHER
231800090209     C                   MOVEL     TABKEY        CODPU            35
231900090209     C                   Z-ADD     1             X
232000090209     C     CODPU         LOOKUP    CPT(X)                                 32
232100090209     C   32              MOVEL     TABUNI        DPU(X)
232200090209     C                   ENDSL
232300090209      *
232400090209     C     TABCOD        READE     EDTAB01L                               30
232500090209     C                   END
232600090209      *
232700090209      * Prosegue tab.PT e PU
232800090209     C                   MOVEL     'PV'          TABCOD
232900090209     C     TABCOD        CHAIN     EDTAB01L                           30
233000090209     C     *IN30         DOWEQ     '0'
233100090209      *
233200090209     C                   SELECT
233300090209     C     TABFLG        WHENNE    *BLANKS
233400090209      *
233500090209     C                   OTHER
233600090209     C                   MOVEL     TABKEY        CODPV            35
233700090209     C                   Z-ADD     1             X
233800090209     C     CODPV         LOOKUP    CPT(X)                                 32
233900090209     C   32              MOVEL     TABUNI        DPV(X)
234000090209     C                   ENDSL
234100090209      *
234200090209     C     TABCOD        READE     EDTAB01L                               30
234300090209     C                   END
234400090209      *
234500090209     C* Caricamento Tabella codici clienti - tariffe
234600090209     C                   Z-ADD     0             X
234700090209     C                   MOVEL     'CL'          TABCOD
234800090209     C     TABCOD        CHAIN     EDTAB01L                           30
234900090209     C     *IN30         DOWEQ     '0'
235000090209     C     TABFLG        IFEQ      *BLANKS
235100090209     C                   ADD       1             X
235200090209     C                   MOVEL     TABKEY        CCL(X)
235300090209     C                   MOVEL     TABUNI        DCL(X)
235400090209     C                   END
235500090209     C     TABCOD        READE     EDTAB01L                               30
235600090209     C                   END
235700090209      *
235800090209     C* Caricamento Serivizi speciali
235900090209     C                   Z-ADD     0             X
236000090209     C                   MOVEL     'SS'          TABCOD
236100090209     C     TABCOD        CHAIN     EDTAB01L                           30
236200090209     C     *IN30         DOWEQ     '0'
236300090209     C     TABFLG        IFEQ      *BLANKS
236400090209     C                   ADD       1             X
236500090209     C                   MOVEL     TABKEY        SS(X)
236600090209     C                   MOVEL     TABKEY        SS35(X)
236700090209     C                   MOVEL     TABUNI        DSS(X)
236800090209     C                   END
236900090209     C     TABCOD        READE     EDTAB01L                               30
237000090209     C                   END
237100090209      *
237200090209     C                   ENDSR
237300090209      * ?------------------------------------------------------------------ */
237400090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
237500090209      * ?------------------------------------------------------------------ */
237600090209     C     MSG_Errato    BEGSR
237700090209      *
237800090205      * ?  Scrive su tutti i records il tipo di errore
237900090205     c     *start        setll     tivin00r
238000090205     c                   read      tivin00r
238100091016     c                   if        %Eof(tivin00r)
238200091016     c                   move      'S'           EoFTivin
238300091016     c                   end
238400090205     c                   dow       not %eof(tivin00r)
238500090211     c                   if        vinMSG  = *blank
238600090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
238700090210     C                               >> Controllare i DATI su UPLOAD !!'
238800090211     c                   end
238900090205     C                   eval      vinFLG = '2'
239000090213     c                   eval      Almeno_Un_Due ='S'
239100090205     c                   eval      esito  = '2'
239200090209     c                   eval      se_errore ='S'
239300090213     c                   eval      err_bloccante ='S'
239400091016     c                   if        EoFTivin = ' '
239500090205     c                   update    tivin000
239600091016     c                   end
239700090205     c                   read      tivin00r
239800091016     c                   if        %Eof(tivin00r)
239900091016     c                   move      'S'           EoFTivin
240000091016     c                   end
240100090205     c                   endDO
240200090205      *
240300090205     C                   ENDSR
240400090213      * ?------------------------------------------------------------------ */
240500090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
240600090213      * ?------------------------------------------------------------------ */
240700090213     C     DETta_Errato  BEGSR
240800090213      *
240900090213      *  rilascia il record prima di rieseguire il ciclo
241000090213     c                   update    tivin000
241100090213      *
241200090213      * ?  Scrive su tutti i records il tipo di errore
241300090213     c     SavNUM_RECord setll     tivin00r
241400090213     c                   read      tivin00r
241500091016     c                   if        %Eof(tivin00r)
241600091016     c                   move      'S'           EoFTivin
241700091016     c                   end
241800090213     c                   dow       not %eof(tivin00r)
241900090213      *
242000090213      *
242100090213     c                   if        vinMSG  = *blank
242200090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
242300090213     C                             >> Controllare i DATI di ogni Segmento <<'
242400090213     c                   end
242500090213     c                   exsr      Error_DET
242600090213      *
242700090213     c                   if        VNREC = NUM_RECord
242800090213     c                   leave
242900090213     c                   end
243000090213      *
243100091016     c                   if        EoFTivin = ' '
243200090213     c                   update    tivin000
243300091016     c                   end
243400090213     c                   read      tivin00r
243500091016     c                   if        %Eof(tivin00r)
243600091016     c                   move      'S'           EoFTivin
243700091016     c                   end
243800090213      *
243900090213     c                   endDO
244000090603      *
244100110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG'
244200090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
244300110511???? C                             Bolle AEG    non totalmente tradotto. -
244400110704     C                             Controllare UPLOAD del cliente :'
244500110704     C                   EVAL      Messaggio = %trim(Messaggio) +
244600110704     C                                         %editc(§PTksc:'X')
244700090603     c                   exsr      invio_mail_AB
244800090603      *
244900090213      *
245000090213     C                   ENDSR
245100090205      * ?------------------------------------------------------------------ */
245200090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
245300090205      * ?------------------------------------------------------------------ */
245400090205     C     *pssr         BEGSR
245500090205     C
245600060710     C                   eval      esito = '2'
245700110511???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AEG'
245800090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
245900100603     C                             Bolle import in filiale - messaggio EDI -
246000110704     C                             ricevuto errato su UPLOAD cliente :'
246100110704     C                   EVAL      Messaggio = %trim(Messaggio) +
246200110704     C                                         %editc(§PTksc:'X')
246300090603     c                   exsr      invio_mail_AB
246400060621     C
246500090213     C                   ENDSR
246600090213      * ?------------------------------------------------------------------ */
246700090213      *?      Segnala Errore su TIVIN
246800090213      * ?------------------------------------------------------------------ */
246900090213     C     Error_DET     BEGSR
247000090213     C
247100090213     C                   eval      vinFLG = '2'
247200090213     c                   eval      Almeno_Un_Due ='S'
247300090213     c                   eval      esito  = '1'
247400090213     c                   eval      se_errore ='S'
247500090213     C                   eval      Err_in_detta = 'S'
247600090213     C
247700090213     C                   ENDSR
247800090209     c*==================================================================*
247900090209     C*? CNVDAT - DECODIFICA LA DATA
248000090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
248100090209     C*         - WDATA  (DATA ALFANUMERICA)
248200090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
248300090209     C*         - WHMS   (ORA NUMERICA)
248400090209     C*----------------------------------------------------------------
248500090209     C     CNVDAT        BEGSR
248600090209     C*
248700090209     C                   MOVEL     ' '           WERRO             1
248800090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
248900090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
249000090209     C                   Z-ADD     *ZEROS        WHMS              6 0
249100090209     C*
249200090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
249300090209     C                   MOVEL     WDATA         WCOMO8            8
249400090209     C                   TESTN                   WCOMO8               99
249500090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
249600090209     C  N99              MOVEL     'S'           WERRO             1
249700090209     C                   END
249800090209     C*
249900090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
250000110523     C                   MOVEL     WDATA         WCOMO12          12
250100110523     C                   TESTN                   WCOMO12              99
250200110523     C                   MOVEl     *all'0'       WCAMGORA                       *DATA
250300110523     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
250400090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
250500090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
250600090209     C  N99              MOVEL     'S'           WERRO             1
250700090209     C                   END
250800110523     C*
250900110523     C     WFORM         IFEQ      '204'                                        CCYMMDD
251000110523     C                   MOVEL     WDATA         WCOMO14          14
251100110523     C                   TESTN                   WCOMO14              99
251200110523     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
251300110523     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
251400110523     C   99              MOVE      WCAMGORA      WHMS                           *DATA
251500110523     C  N99              MOVEL     'S'           WERRO             1
251600110523     C                   END
251700090209     C*
251800090209     C                   ENDSR
251900090210     C*----------------------------------------------------------------
252000090210     C*? TSTNUM - CONTROLLO NUMERICO
252100090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
252200090210     C*         - WLUNGH (Lunghezza campo output)
252300090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
252400090210     C*----------------------------------------------------------------
252500090210     C     TSTNUM        BEGSR
252600090210     C*
252700090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
252800090210     C                   MOVEL     'N'           WOKNUM            1
252900090210     C                   MOVEL     *BLANKS       WNUMOK           15
253000090210     C                   Z-ADD     15            IN                3 0
253100090210     C                   Z-ADD     WLUNGH        IX                3 0
253200090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
253300090210     C                   MOVEA     WNUM          NDA
253400090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
253500090210     C                   MOVEA     *ALL'0'       NDN
253600090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
253700090210     C                   Z-ADD     WLUNGH        I                 3 0
253800090210     C     I             DOWGT     0                                            --- 1 -->
253900090210     C* ESTRAE IL CARATTERE DA ESAMINARE
254000090210     C     1             SUBST     WNUM:I        WTEM01            1
254100090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
254200090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
254300090210     C     WTEM01        SCAN      WCA                                    99
254400090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
254500090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
254600090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
254700090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
254800090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
254900090210     C* DA DESTRA VERSO SINISTRA
255000090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
255100090210     C                   MOVEL     WTEMP1        NDN(IN)
255200090210     C                   SUB       1             IN
255300090210     C                   END                                                    <-- 3 ---
255400090210     C                   END                                                    <-- 2 ---
255500090210     C                   SUB       1             I
255600090210     C                   END                                                    <-- 1 ---
255700090210     C*
255800090210     C                   MOVEA     NDN           WNDN             15
255900090210     C     WNDN          IFNE      *ZEROS
256000090210     C                   MOVEA     NDN           WNUMOK           15
256100090210     C                   MOVEL     'S'           WOKNUM            1
256200090210     C                   END
256300090210     C*
256400090210     C                   ENDSR
256500090210     C*----------------------------------------------------------------
256600090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
256700090210     C*----------------------------------------------------------------
256800090210     C     CNVCAP        BEGSR
256900090210     C*
257000090210     C                   MOVEL     *BLANKS       WCAP              9
257100090210     C     nad3251       IFNE      '0'
257200090210     C     '  '          SCAN      nad3251       LENGHT            2 0
257300090210     C                   SUB       1             LENGHT
257400090210     C     LENGHT        IFLE      5
257500090210     C     LENGHT        ANDGT     0
257600090210     C                   Z-ADD     LENGHT        T                 2 0
257700090210     C                   Z-ADD     5             S                 2 0
257800090210     C                   MOVEA     nad3251       CAP9
257900090210     C                   MOVEL     *ZEROS        CAP5
258000090210     C                   DO        LENGHT
258100090210     C                   MOVE      CAP9(T)       CAP5(S)
258200090210     C                   SUB       1             S
258300090210     C                   SUB       1             T
258400090210     C                   END
258500090210     C                   MOVEA     CAP5          WCAP5             5
258600090210     C     WCAP5         IFEQ      '0'
258700090210     C                   MOVEL     *BLANKS       WCAP
258800090210     C                   ELSE
258900090210     C                   MOVEA     CAP5          WCAP
259000090210     C                   END
259100090210     C*
259200090210     C                   ELSE
259300090210     C                   MOVEL     nad3251       WCAP
259400090210     C                   END
259500090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
259600090210     C     WCAP          IFNE      *BLANKS
259700090210     C     WFLCPF        ANDNE     0
259800090210     C                   MOVEL     WFLCPF        WELE              1 0
259900090210     C                   MOVE      WFLCPF        WPOS              1 0
260000090210     C                   MOVEL     *BLANK        WCPF
260100090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
260200090210     C                   ELSE
260300090210     C                   MOVEL     *BLANKS       WCPF
260400090210     C                   END
260500090210     C                   END
260600090210     C*
260700090210     C                   ENDSR
260800090211      * ?================================================================== */
260900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
261000090211      * ?================================================================== */
261100090211     C     Scrive_VAB    BEGSR
261200090211      *
261300090211     c                   clear                   err_bloccante     1
261400090211     c                   move      'S'           Almeno_Uno
261500090211      *
261600090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
261700090211     c                   exsr      UPDVAB
261800090211      *
261900090211     C* >>>>>>>>>>>>
262000090211     C*  Richiamo pgm per scrittura immediata VAB
262100090211     C     §PUWRK        IFEQ      ' '
262200090211      *
262300090211     C*  Richiamo pgm per esecuzione stampa
262400090211     C     §PTCMR        IFEQ      'S'
262500090213     C                   MOVEL     WNRDOC        d86CMR
262600090211     C                   ELSE
262700090213     C                   MOVEL     WNRDOC        d86CMR
262800090211     C                   END
262900090211      *
263000090213     C                   MOVEL     ' '           d86RIE
263100090213     C                   MOVEL     §PUDTA        d86DTA
263200090213     C                   Z-ADD     1             d86CNT
263300090213     C                   Z-ADD     §PTKSC        d86CCM
263400090213     C                   MOVEL     *BLANKS       d86STA
263500090213     C                   MOVEL     'E'           d86MOD
263600090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
263700090211      * dove poi imposta la DS dei parametri passati
263800090213     C                   MOVEL     'LR'          d86CHI
263900090213     C                   MOVEL     'N'           d86CTL
264000090211      *
264100090211     c                   move      kpjbu         SvKpjbu
264200090211     c                   clear                   kpjbu
264300090216     C                   movel     *on           Commit_on         1
264400090213     C                   MOVEL     TRTC86ds      KPJBU
264500090213      * ?- Chiamata la TRTC86R2 -*
264600090213     C                   CALL      'TRTC86R2'
264700090211     C                   PARM                    KPJBA
264800090216     C                   PARM                    Commit_on
264900090211     c                   move      Svkpjbu       Kpjbu
265000090211      *
265100090211     C*  Controllo pgm per scrittura immediata VAB
265200090211     C     WW            IFNE      0
265300090211     C                   DO        WW            WX                3 0
265400090213     C                   Z-ADD     KCL(WX)       d86CCM
265500090211      *
265600090211     c                   move      kpjbu         SvKpjbu
265700090211     c                   clear                   kpjbu
265800090216     C                   movel     *on           Commit_on         1
265900090213     C                   MOVEL     TRTC86ds      KPJBU
266000090213      * ?- Chiamata la TRTC86R2 -*
266100090213     C                   CALL      'TRTC86R2'
266200090211     C                   PARM                    KPJBA
266300090216     C                   PARM                    Commit_on
266400090211     c                   move      Svkpjbu       Kpjbu
266500090211     C                   END
266600090211     C                   END
266700090211      *
266800090211     C                   END
266900090211      * >>>>>>>>>>>>
267000090211     C                   if        se_errore  = 'S'
267100090211     c                   move      'S'           err_bloccante
267200090211     c                   goto      Error_VAB
267300090211     c                   end
267400090211     C*
267500090211     C     Error_VAB     Endsr
267600090211     C*----------------------------------------------------------------
267700090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
267800090211     C*----------------------------------------------------------------
267900090211     C     UPDVAB        BEGSR
268000090211      *
268100090211     C*  Se non è stato impostato il codice bolla lo imposto io
268200090211     C     VABCBO        IFEQ      *BLANKS
268300090211     C     VABCAS        IFGT      0
268400090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
268500090211     C                   ELSE
268600090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
268700090211     C                   END
268800090211     C                   END
268900110523      * Imposta le NOte
269000110523     C     VABNOT        IFEQ      *BLANKS
269100110523     C                   MOVEL     Wnot1         VABNOT
269200110523     C                   MOVEL     Wnot2         VABNT2
269300110523     C                   ELSE
269400110523     C                   MOVEL     Wnot1         VABNT2
269500110523     C                   END
269600090211      *
269700090211     C*  Attribuisco anno spedizione
269800090211     C     WDATPA        IFGT      0                                            anno sped.
269900090211     C                   MOVEL     WDATPA        VABAAS
270000090211     C                   ELSE
270100090211     C     WDATFV        IFGT      0
270200090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
270300090211     C                   END
270400090211     C                   END
270500090211      *
270600090211      *  Controllo dettaglio segnacolli
270700090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
270800090211     C                   EXSR      CTRSGE
270900090211     C                   ELSE
271000090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
271100090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
271200090211     C                   EXSR      CTRSGN
271300090211     C                   END
271400090211     C                   END
271500090211      *
271600090212      *  Imposto linea partenza e serie
271700090211     C                   eval      VABLNP = VABLNP_PT
271800090212     C                   eval      VABNRS = VABnrs_PT
271900090211      *
272000090211      *  Controllo se devo variare il codice trattamento merce
272100090211     C                   eval      VABCTM = VABctm_PT
272200090211      *
272300090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
272400090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
272500090211     c                   clear                   mantiene_nsp      1            *blk/'S'
272600090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
272700090211      *
272800090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
272900090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
273000090211      *
273100090211      *  Imposto codice cliente
273200090211     c                   Select
273300090211      *
273400090211      *  Specificato codice da qualificatore FF (TD15)
273500090211     C                   When        WCLKSC <> *zeros
273600090211     C                   Z-ADD     WCLKSC        VABCCM
273700090211     C                   MOVEL     WCLTAR        VABCTR
273800090211      * forza LNP/CTM/NRS
273900090211     c                   if        WclLNP <> 0
274000090211     C                   eval      VABLNP = WclLNP
274100090211     c                   end
274200090211     c                   if        WclNRS <> 0
274300090211     C                   eval      VABNRS = WclNRS
274400090211     c                   end
274500090211     c                   if        WclCTM <> *blank
274600090211     C                   eval      VABCTM = WclCTM
274700090211     c                   end
274800090211      *
274900090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
275000090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
275100090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
275200090211     c                   end
275300090211      *
275400090211      *  Specificato codice da qualificatore FF (TT15)
275500090211     C                   When        $CLKSC <> *zeros
275600090211     C                   Z-ADD     $CLKSC        VABCCM
275700090211     C                   MOVEL     $CLTAR        VABCTR
275800090211      * forza LNP/CTM/NRS
275900090211     c                   if        $clLNP <> 0
276000090211     C                   eval      VABLNP = $clLNP
276100090211     c                   end
276200090211     c                   if        $clNRS <> 0
276300090211     C                   eval      VABNRS = $clNRS
276400090211     c                   end
276500090211     c                   if        $clCTM <> *blank
276600090211     C                   eval      VABCTM = $clCTM
276700090211     c                   end
276800090211      *
276900090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
277000090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
277100090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
277200090211     c                   end
277300090211      *
277400090211      *  Imposto dati da tabella - PT-
277500090211     C                   Other
277600090211      *
277700090211     C                   MOVEL     §PTKSC        VABCCM
277800090211     C                   MOVEL     §PTCTR        VABCTR
277900090211      *
278000090211     C                   Endsl
278100090211      *
278200090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
278300090211      *    o dalla relativa CL
278400090211     C                   MOVEL     VABlnp        VABfgs
278500090211      *
278600090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
278700090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
278800090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
278900090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
279000090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
279100090211      *
279200090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
279300090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
279400090211     c                   else
279500090211      *
279600090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
279700090211     C*  lo attribuisco io
279800090211      **              **------------------**
279900090211     c                   exsr      repnum
280000090211      **              **------------------**
280100090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
280200090211      **              **------------------**
280300090211     c                   end
280400090211      *
280500090211     C*  Altrimenti attribuisco data del giorno
280600090211     C     VABMGS        IFEQ      0
280700090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
280800090211     C                   MOVE      WOGGI         VABMGS                         = oggi
280900090211     C                   END
281000090211     C*  Eseguo posizionamento su EDiVAB
281100090211     C                   Z-ADD     VABAAS        KAAS
281200090211     C                   Z-ADD     VABlnp        KLNP
281300090211     C                   Z-ADD     VABNRS        KNRS
281400090211     C                   Z-ADD     VABNSP        KNSP
281500090211     C                   MOVEL     SAVBOL        SALVA
281600090211     C     KVAB1         CHAIN     EDiVAB1L                           30
281700090211     C                   MOVEL     SALVA         SAVBOL
281800090211     C*  Imposto dati CMR
281900090211     C                   Z-ADD     1             VABCNT
282000090211     **
282100090211      *** Attenzione se si deve trattare il VAX con il CMR
282200090211      *** il campo VABCNT deve essere sempre impostato a (1)
282300090211     C     §puWRK        ifEQ      'S'
282400090211     C     §puNSP        andEQ     'S'
282500090211     C                   Z-ADD     1             VABCNT
282600090211     c                   end
282700090211     **
282800090211     C                   Z-ADD     WDTPRE        VABDTS
282900090212     C                   movel     '20'          VABDTS
283000090212     C     WHMPRE        mult      100           VABHMS
283100090211     C     §PTCMR        IFEQ      'S'
283200090211     C                   Z-ADD     WDTCMR        VABDCM
283300090211     C                   MOVEL     WNRCMR        VABCMR
283400090211     C                   ELSE
283500090211     C                   Z-ADD     WDATFV        VABDCM
283600090211     C                   MOVEL     WNUMFV        VABCMR
283700090211     C                   END
283800090211     C*  Se non è indicato il nome del mittente = Partner mittente
283900090211     C     VABRMO        IFEQ      *BLANKS
284000090211     C                   MOVEL     ACORAG        VABRMO
284100090211     C                   MOVEL     INDCAP        VABCMO
284200090211     C     INDCAE        IFNE      *BLANKS
284300090211     C                   MOVEL     INDCAE        VABCMO
284400090211     C                   END
284500090211     C     INDSTA        IFNE      *BLANKS
284600090211     C                   MOVEL     INDSTA        VABNMO
284700090211     C                   END
284800090211     C                   END
284900090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
285000090211     C     §1BFG1        IFEQ      'N'
285100090211     C     §1BFG7        ANDEQ     'N'
285200090211     C     §1BFG2        ANDEQ     'N'
285300090211     C***                  Z-ADD0         VABNRS
285400090211     C                   END
285500090211     C*  Imposto segancollo da - a
285600090211     C     §1BFG1        IFEQ      'S'
285700090211     C     §1BFG7        ANDEQ     'N'
285800090211     C     §1BF12        ANDNE     'E'
285900090211      *
286000090211     C     XY            IFEQ      *ZEROS
286100090211     C                   CLEAR                   VABNCD
286200090211     C                   CLEAR                   VABNCA
286300090211     C                   ELSE
286400090211     C                   Z-ADD     SGN(1)        VABNCD
286500090211     C                   Z-ADD     SGN(XY)       VABNCA
286600090211     C                   ENDIF
286700090211     C*
286800090211     C                   END
286900090211     C*
287000090211     C*  Se cap mittente originale a blank abblenco nazione
287100090211     C     VABCMO        IFEQ      *BLANKS
287200090211     C                   MOVEL     *BLANKS       VABNMO
287300090211     C                   END
287400090603     C*
287500090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
287600090603     C* Se in testata
287700090603     c                   if        NAD_destinatario_in_testata = 'S'
287800090603     c                   eval         VABrsd =  tes_VABrsd
287900090603     c                   eval         VABrd2 =  tes_VABrd2
288000090603     c                   eval         VABind =  tes_VABind
288100090603     c                   eval         VABlod =  tes_VABlod
288200090603     c                   eval         VABnzd =  tes_VABnzd
288300090603     c                   end
288400090603     C* Se in testata
288500090603     c                   if        NAD_mittente_in_testata = 'S'
288600090603     c                   eval         VABrmo = tes_VABrmo
288700090603     c                   eval         VABnmo = tes_VABnmo
288800090603     c                   eval         VABcmo = tes_VABcmo
288900090603     c                   end
289000110718      *
289100110718      *
289200090211      *? ------------------------------------------------------------------------
289300090213     C   30              WRITE     EDiVAB00                             99
289400090213     C  N30              UPDATE    EDiVAB00                             99
289500090211      *? ------------------------------------------------------------------------
289600090213     c   99              eval      errori_in_Wrt = 'S'
289700090211     C*
289800090211     c                   clear                   wri_vabtic        1
289900090211     C*
290000090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
290100090211     C                   EXSR      WRTVAT_Rif
290200090211     C*
290300090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
290400090211     C     §1BF12        IFEQ      'E'
290500090211     C                   EXSR      WRTVAT
290600090211     C                   ELSE
290700090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
290800090211     C     §1BFG7        IFEQ      'S'
290900090211     C     §1BFG1        OREQ      'S'
291000090211     C                   EXSR      WRTVAD
291100090211     C                   END
291200090211     C                   END
291300090211      *
291400090211     C* Reimposto codice trattamento merce cliente
291500090211     C                   MOVEL     WSVTRM        DS1B
291600090211     C**
291700090211     C* Do errore se previsto CMR ma non c'è
291800090211     C     §PTCMR        IFNE      ' '
291900090211     C                   EXSR      MEMCMR
292000090211     C                   END
292100090211     C* Do errore se previsto AFB ma non c'è
292200090211     C     §PTNFV        IFNE      ' '
292300090211     C                   EXSR      MEMAFB
292400090211     C                   END
292500090211     C*  Se il cliente vuole il ritorno delle date di consegna
292600090211     C*  è c'è una squdratura fra i segnacolli e il numero
292700090211     C*  dei colli che ci ha passato scrivo EDSUM
292800090211     C     §PUDTA        IFEQ      'S'
292900090211     C     WSQUAD        ANDEQ     'S'
293000090211     C                   EXSR      WRTSUM
293100090211     C                   END
293200090211     C*
293300090211     C                   ENDSR
293400090211     C*----------------------------------------------------------------
293500090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
293600090211     C*----------------------------------------------------------------
293700090211     C     CTRSGN        BEGSR
293800090211     C*
293900090211     C                   MOVEL     'N'           WNSGER            1
294000090211     C*
294100090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
294200090211     C     VABNCL        IFNE      XY
294300090211     C                   MOVEL     'S'           WNSGER
294400090211     C                   MOVEL     'S'           WSQUAD
294500090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
294600090211     c                              bollettati'
294700090211     C                   GOTO      FINSGN
294800090211     C                   END
294900090211     C*  Riordino i segnacolli
295000090211     C     §1BFG7        IFEQ      'N'
295100090211     C                   SORTA     SGN
295200090211     C*  Controllo se sono sequenziali
295300090211     C                   MOVEL     'N'           WNOSEQ            1
295400090211     C     SGN(1)        ADD       1             WEXSGN            7 0
295500090211     C     2             DO        XY            X
295600090211     C     SGN(X)        IFNE      WEXSGN
295700090211     C                   MOVEL     'S'           WNOSEQ
295800090211     C                   END
295900090211     C     SGN(X)        ADD       1             WEXSGN
296000090211     C                   END
296100090211     C                   END
296200090211     C*
296300090211     C     FINSGN        ENDSR
296400090211     C*----------------------------------------------------------------
296500090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
296600090211     C*----------------------------------------------------------------
296700090211     C     CTRSGE        BEGSR
296800090211     C*
296900090211     C                   MOVEL     'N'           WNSGER            1
297000090211      *
297100090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
297200090211      *
297300090211     C     VABNCL        IFNE      §§
297400090211     C                   MOVEL     'S'           WNSGER
297500090211     C                   MOVEL     'S'           WSQUAD
297600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
297700090211     c                              bollettati'
297800090211     C                   END
297900090211      *
298000090211     C                   ENDSR
298100090211     C*----------------------------------------------------------------
298200090211     C*? ESEGUO SCRITTURA EDiVAD
298300090211     C*----------------------------------------------------------------
298400090211     C     WRTVAD        BEGSR
298500090211     C*
298600090211     C* Se non seq. scrivo xy record
298700090211     C     §1BFG1        IFEQ      'S'
298800090211     C     XY            ANDNE     *ZEROS
298900090211     C*
299000090211     C     §1BFG7        IFEQ      'S'
299100090211     C                   DO        XY            X
299200090211     C                   Z-ADD     VABCCM        KCCM
299300090211     C                   Z-ADD     SGN(X)        KNCD
299400090211     C                   Z-ADD     VABCNT        VADCNT
299500090211     C                   MOVEL     VABCMR        VADCMR
299600090211     C                   Z-ADD     VABAAS        VADAAS
299700090211     C                   Z-ADD     VABLNP        VADLNP
299800090211     C                   Z-ADD     VABNRS        VADNRS
299900090211     C                   Z-ADD     VABNSP        VADNSP
300000090211     C                   Z-ADD     1             VADNCL
300100090211     C                   MOVEL     SGN(X)        VADCDP
300200090211     C                   Z-ADD     SGN(X)        VADNCD
300300090211     C                   Z-ADD     *ZEROS        VADNCA
300400090211     C                   Z-ADD     VABCCM        VADCCM
300500090211     C                   movel     VABfgs        VADfgs
300600090213     C                   WRITE     EDiVAD00                             99
300700090213     c   99              eval      errori_in_Wrt = 'S'
300800090211     C                   END
300900090211     C* Se sequenziali scrivo un solo record
301000090211     C                   ELSE
301100090211     C                   Z-ADD     VABAAS        VADAAS
301200090211     C                   Z-ADD     VABLNP        VADLNP
301300090211     C                   Z-ADD     VABNRS        VADNRS
301400090211     C                   Z-ADD     VABNSP        VADNSP
301500090211     C                   Z-ADD     VABNCL        VADNCL
301600090211     C                   Z-ADD     SGN(1)        VADNCD
301700090211     C                   Z-ADD     SGN(XY)       VADNCA
301800090211     C                   Z-ADD     VABCCM        VADCCM
301900090211     C                   movel     VABfgs        VADfgs
302000090213     C                   WRITE     EDiVAD00                             99
302100090213     c   99              eval      errori_in_Wrt = 'S'
302200090211     C                   END
302300090211     C*
302400090211     C                   ELSE
302500090211     C                   DO        XY            X
302600090211     C                   Z-ADD     VABCCM        KCCM
302700090211     C                   Z-ADD     SGN(X)        KNCD
302800090211     C                   Z-ADD     VABAAS        VADAAS
302900090211     C                   Z-ADD     VABLNP        VADLNP
303000090211     C                   Z-ADD     VABNRS        VADNRS
303100090211     C                   Z-ADD     VABNSP        VADNSP
303200090211     C                   MOVEL     SGN(X)        VADCDP
303300090211     C                   Z-ADD     1             VADNCL
303400090211     C                   Z-ADD     *ZEROS        VADNCD
303500090211     C                   Z-ADD     *ZEROS        VADNCA
303600090211     C                   Z-ADD     VABCCM        VADCCM
303700090211     C                   movel     VABfgs        VADfgs
303800090213     C                   WRITE     EDiVAD00                             99
303900090213     c   99              eval      errori_in_Wrt = 'S'
304000090211     C                   END
304100090211     C*
304200090211     C                   END
304300090211     C*
304400090211     C                   ENDSR
304500090211     C*----------------------------------------------------------------
304600090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
304700090211     C*----------------------------------------------------------------
304800090211     C     WRTVAT_Rif    BEGSR
304900090211      *
305000090211      *  riferimento di consegna destinatario
305100090211     c                   if        WatRDE <> *blank
305200090211     C                   Z-ADD     VABCNT        VATCNT
305300090211     C                   MOVEL     VABCMR        VATCMR
305400090211     C                   Z-ADD     VABCCM        VATCCM
305500090211     C                   movel     VABfgs        VATfgs
305600090211     C                   Z-ADD     VABLNP        VATLNP
305700090211     C                   Z-ADD     VABAAS        VATAAS
305800090211     C                   Z-ADD     VABNRS        VATNRS
305900090211     C                   Z-ADD     VABNSP        VATNSP
306000090211     C                   MOVEL     'A'           VATTRC
306100090211     C                   MOVEL     WatRDE        VATNOT
306200090213     C                   WRITE     EDiVAT00                             99
306300090213     c   99              eval      errori_in_Wrt = 'S'
306400090211     c                   end
306500090211      *
306600090211      *  riferimento di consegna telefonico
306700090211     c                   if        WatTEL <> *blank
306800090211     C                   Z-ADD     VABCNT        VATCNT
306900090211     C                   MOVEL     VABCMR        VATCMR
307000090211     C                   Z-ADD     VABCCM        VATCCM
307100090211     C                   movel     VABfgs        VATfgs
307200090211     C                   Z-ADD     VABLNP        VATLNP
307300090211     C                   Z-ADD     VABAAS        VATAAS
307400090211     C                   Z-ADD     VABNRS        VATNRS
307500090211     C                   Z-ADD     VABNSP        VATNSP
307600090211     C                   MOVEL     'B'           VATTRC
307700090211     C                   MOVEL     WatTEL        VATNOT
307800090213     C                   WRITE     EDiVAT00                             99
307900090213     c   99              eval      errori_in_Wrt = 'S'
308000090211     c                   end
308100090211      *
308200090211      * riferimento del Partner che una volta veniva riportato sul BL4
308300090211     c                   if        Wnsp   <> *blank
308400090211     C                   Z-ADD     VABCNT        VATCNT
308500090211     C                   MOVEL     VABCMR        VATCMR
308600090211     C                   Z-ADD     VABCCM        VATCCM
308700090211     C                   movel     VABfgs        VATfgs
308800090211     C                   Z-ADD     VABLNP        VATLNP
308900090211     C                   Z-ADD     VABAAS        VATAAS
309000090211     C                   Z-ADD     VABNRS        VATNRS
309100090211     C                   Z-ADD     VABNSP        VATNSP
309200090211     C                   MOVEL     'C'           VATTRC
309300090211     C                   MOVEL     Wnsp          VATNOT
309400090213     C                   WRITE     EDiVAT00                             99
309500090213     c   99              eval      errori_in_Wrt = 'S'
309600090211     c                   end
309700090211      *
309800090211     C                   ENDSR
309900090211     C*----------------------------------------------------------------
310000090211     C*? ESEGUO SCRITTURA EDiVAT
310100090211     C*----------------------------------------------------------------
310200090211     C     WRTVAT        BEGSR
310300090211      *
310400090211     C                   DO        §§            X
310500090211     C                   Z-ADD     VABCNT        VATCNT
310600090211     C                   MOVEL     VABCMR        VATCMR
310700090211     C                   Z-ADD     VABCCM        VATCCM
310800090211     C                   movel     VABfgs        VATfgs
310900090211     C                   Z-ADD     VABLNP        VATLNP
311000090211     C                   Z-ADD     VABAAS        VATAAS
311100090211     C                   Z-ADD     VABNRS        VATNRS
311200090211     C                   Z-ADD     VABNSP        VATNSP
311300090211     C                   MOVEL     'E'           VATTRC
311400090211      *
311500090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
311600090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
311700090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
311800090211     c                   if        WVATlun > 0
311900090211      *
312000090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
312100090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
312200090211      *
312300090211     c                   else
312400090211      *
312500090211      * Se non è impostato nulla prende il segnacollo passato così com'è
312600090211     C                   MOVEL     SGE(X)        VATNOT
312700090211     c                   end
312800090211      *
312900090213     C                   WRITE     EDiVAT00                             99
313000090213     c   99              eval      errori_in_Wrt = 'S'
313100090211     C                   END
313200090211      *
313300090211     C                   ENDSR
313400090211     C*----------------------------------------------------------------
313500090211     C*? ESEGUO SCRITTURA EDSUM
313600090211     C*----------------------------------------------------------------
313700090211     C     WRTSUM        BEGSR
313800090211     C*
313900090211     C* Se non seq. scrivo xy record
314000090211     C                   DO        XY            X
314100090211     C                   Z-ADD     VABAAS        SUMAAS
314200090211     C                   Z-ADD     VABLNP        SUMFLS
314300090211     C                   MOVEL     WKSC          SUMLNP
314400090211     C                   Z-ADD     VABLNA        SUMLNA
314500090211     C                   Z-ADD     VABZNC        SUMZNC
314600090211     C                   Z-ADD     VABNRS        SUMNRS
314700090211     C                   Z-ADD     VABNSP        SUMNSP
314800090211     C                   Z-ADD     SGN(X)        SUMNSC
314900090211     C                   Z-ADD     WKSC          SUMCCM
315000090211     C                   MOVEL     *BLANKS       SUMSTS
315100090211     C                   MOVEL     *BLANKS       SUMFLG
315200090211     C                   MOVEL     WPRGSP        SUMCNI
315300090211     C                   MOVEL     VABCMR        SUMCMR
315400090211     C                   Z-ADD     0             SUMNFE
315500090211     C                   Z-ADD     VABCCM        VADCCM
315600090213     C                   WRITE     EDSUM000                             99
315700090213     c   99              eval      errori_in_Wrt = 'S'
315800090211     C                   END
315900090211     C*
316000090211     C                   ENDSR
316100090211     c*==================================================================*
316200090211      * Manda un Msg in Posta AS Sede a EDP*
316300090211     c*==================================================================*
316400090211     c     SEND_MSG_EDP  begsr
316500090211      *
316600090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
316700090211      *   Lo manda una sola volta per lavoro
316800090211     c                   IF        Snd_Msg_alert  <> 'N' and
316900090211     c                             User_msg <> *blank
317000090211     c                   z-add     1             Jx
317100090211     C                   exsr      CalQEZSNDMG
317200090211     c                   end
317300090211      *
317400090211     c                   endsr
317500090213     c**********************************************************************
317600090213     c* reperimento numero Spedizione
317700090213     c**********************************************************************
317800090213     C     repnum        BEGSR
317900090213      *
318000090213      *    deve controllare sulla tabella "3C" se il numero spedizione
318100090213      *     deve essere mantenuto oppure no
318200090213     C*
318300090213     C                   clear                   Ds3C
318400090213     C                   Z-ADD     CODUT         TBLKUT
318500090213     C                   MOVEL     '3C'          TBLCOD
318600090213     C                   movel(p)  VABccm        TBLKEY
318700090213     C     KTABel        CHAIN     TABEL00F                           30
318800090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
318900090213     C                   MOVEL     TBLUNI        Ds3C
319000090213     C                   END
319100090213      *
319200090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
319300090213     c                   if        §3CFsp <> 'S'
319400090213      *
319500090213     C* NSP => Stacco un numeratore da AZNUM
319600090213     c                   movel     kpjbu         svkpjbu
319700090213     c                   clear                   kpjbu
319800090213     C                   clear                   TRUL33DS
319900090213     C                   eval      I33OPE = *zeros
320000090213     C                   eval      I33CNU = 302
320100090213     C                   eval      I33NUM = 1
320200090213     C                   movel     TRUL33DS      KPJBU
320300090213     C                   call      'TRUL33R'
320400090213     C                   parm                    KPJBA
320500090213     C                   movel     KPJBU         TRUL33DS
320600090213     c                   movel     svkpjbu       kpjbu
320700090213      ****
320800090213     C                   if        O33ERR = *zeros
320900090213     c                   z-add     o33nrf        NUM_SPED
321000090213     C                   else
321100090213     C                   endif
321200090213      ****
321300090213     c                   ELSE
321400090213      *   se si vuole mantenere il numero spedizione
321500090213      ****
321600090213     C                   MOVEL     WOGGI         WANNO             4 0
321700090213     C                   MOVE      WANNO         KAAA
321800090213     C                   Z-ADD     3             KCNU
321900090213     C                   Z-ADD     VABlnp        KFIL
322000090213     C     KNUF          CHAIN     FLNUF                              9999
322100090213     C     *IN99         IFEQ      '0'
322200090213     C                   ADD       1             NUFNUM
322300090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
322400090213     C                   UPDATE    FLNUF
322500090213     C                   ELSE
322600090213     C                   END
322700090213     C*
322800090213     c                   EndIF
322900090213      **
323000090213     C                   ENDSR
323100090213     C*----------------------------------------------------------------
323200090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
323300090213     C*----------------------------------------------------------------
323400090213     C     MEMCMR        BEGSR
323500090213     C*
323600090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
323700090213     C     §PTCM1        IFEQ      'S'
323800090213     C                   CLEAR                   EDRDE000
323900090213     C                   MOVEL     'TD1154'      KNMC
324000090213     C                   Z-ADD     1             KNRP
324100090213     C                   MOVEL     WKSC          KLNP1
324200090213     C     KRDE          CHAIN     EDRDE01L                           31
324300090213     C     *IN31         IFEQ      '1'
324400090213     C                   Z-ADD     VABAAS        RDEAAS
324500090213     C                   Z-ADD     VABLNP        RDELNP
324600090213     C                   Z-ADD     VABNRS        RDENRS
324700090213     C                   Z-ADD     VABNSP        RDENSP
324800090213     C                   MOVEL     'TD1154'      RDENMC
324900090213     C                   Z-ADD     1             RDENRP
325000090213     C                   MOVEL     WNRCMR        RDEVAL
325100090213     C                   WRITE     EDRDE000                             99
325200090213     c   99              eval      errori_in_Wrt = 'S'
325300090213     C                   END
325400090213     C*  Memorizzo qualificatore CMR
325500090213     C                   CLEAR                   EDRDE000
325600090213     C                   MOVEL     'TD1153'      KNMC
325700090213     C                   Z-ADD     1             KNRP
325800090213     C                   MOVEL     WKSC          KLNP1
325900090213     C     KRDE          CHAIN     EDRDE01L                           31
326000090213     C     *IN31         IFEQ      '1'
326100090213     C                   Z-ADD     VABAAS        RDEAAS
326200090213     C                   Z-ADD     VABLNP        RDELNP
326300090213     C                   Z-ADD     VABNRS        RDENRS
326400090213     C                   Z-ADD     VABNSP        RDENSP
326500090213     C                   Z-ADD     1             RDENRP
326600090213     C                   MOVEL     'TD1153'      RDENMC
326700090213     C                   MOVEL     'CMR'         RDEVAL
326800090213     C                   WRITE     EDRDE000                             99
326900090213     c   99              eval      errori_in_Wrt = 'S'
327000090213     C                   END
327100090213     C*  Memorizzo la data
327200090213     C                   CLEAR                   EDRDE000
327300090213     C                   MOVEL     'TD2380'      KNMC
327400090213     C                   Z-ADD     1             KNRP
327500090213     C                   MOVEL     WKSC          KLNP1
327600090213     C     KRDE          CHAIN     EDRDE01L                           31
327700090213     C     *IN31         IFEQ      '1'
327800090213     C                   Z-ADD     VABAAS        RDEAAS
327900090213     C                   Z-ADD     VABLNP        RDELNP
328000090213     C                   Z-ADD     VABNRS        RDENRS
328100090213     C                   Z-ADD     VABNSP        RDENSP
328200090213     C                   Z-ADD     1             RDENRP
328300090213     C                   MOVEL     'TD2380'      RDENMC
328400090213     C                   MOVEL     WDTCMR        RDEVAL
328500090213     C                   WRITE     EDRDE000                             99
328600090213     c   99              eval      errori_in_Wrt = 'S'
328700090213     C                   END
328800090213     C*
328900090213     C                   END                                                    §PTCM1 = 'S'
329000090213     C*
329100090213     C                   ENDSR
329200090213     C*----------------------------------------------------------------
329300090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
329400090213     C*----------------------------------------------------------------
329500090213     C     MEMAFB        BEGSR
329600090213     C*
329700090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
329800090213     C     §PTNF1        IFEQ      'S'
329900090213     C                   CLEAR                   EDRDE000
330000090213     C                   MOVEL     WKSC          KLNP1
330100090213     C                   MOVEL     'TD1154'      KNMC
330200090213     C                   Z-ADD     1             KNRP
330300090213     C     KRDE          CHAIN     EDRDE01L                           31
330400090213     C     *IN31         IFEQ      '1'
330500090213     C                   Z-ADD     VABAAS        RDEAAS
330600090213     C                   Z-ADD     VABLNP        RDELNP
330700090213     C                   Z-ADD     VABNRS        RDENRS
330800090213     C                   Z-ADD     VABNSP        RDENSP
330900090213     C                   MOVEL     'TD1154'      RDENMC
331000090213     C     §PTCM1        IFNE      'S'
331100090213     C     WNRCMR        OREQ      *BLANKS
331200090213     C                   Z-ADD     1             RDENRP
331300090213     C                   ELSE
331400090213     C                   Z-ADD     2             RDENRP
331500090213     C                   END
331600090213     C                   MOVEL     WNUMFV        RDEVAL
331700090213     C                   WRITE     EDRDE000                             99
331800090213     c   99              eval      errori_in_Wrt = 'S'
331900090213     C                   END
332000090213     C*  Memorizzo qualificatore CMR
332100090213     C                   CLEAR                   EDRDE000
332200090213     C                   MOVEL     WKSC          KLNP1
332300090213     C                   MOVEL     'TD1153'      KNMC
332400090213     C                   Z-ADD     1             KNRP
332500090213     C     KRDE          CHAIN     EDRDE01L                           31
332600090213     C     *IN31         IFEQ      '1'
332700090213     C                   Z-ADD     VABAAS        RDEAAS
332800090213     C                   Z-ADD     VABLNP        RDELNP
332900090213     C                   Z-ADD     VABNRS        RDENRS
333000090213     C                   Z-ADD     VABNSP        RDENSP
333100090213     C     §PTCM1        IFNE      'S'
333200090213     C     WNRCMR        OREQ      *BLANKS
333300090213     C                   Z-ADD     1             RDENRP
333400090213     C                   ELSE
333500090213     C                   Z-ADD     2             RDENRP
333600090213     C                   END
333700090213     C                   MOVEL     'TD1153'      RDENMC
333800090213     C                   MOVEL     'AFB'         RDEVAL
333900090213     C                   WRITE     EDRDE000                             99
334000090213     c   99              eval      errori_in_Wrt = 'S'
334100090213     C                   END
334200090213     C*  Memorizzo la data
334300090213     C                   CLEAR                   EDRDE000
334400090213     C                   MOVEL     WKSC          KLNP1
334500090213     C                   MOVEL     'TD2380'      KNMC
334600090213     C                   Z-ADD     1             KNRP
334700090213     C     KRDE          CHAIN     EDRDE01L                           31
334800090213     C     *IN31         IFEQ      '1'
334900090213     C                   Z-ADD     VABAAS        RDEAAS
335000090213     C                   Z-ADD     VABLNP        RDELNP
335100090213     C                   Z-ADD     VABNRS        RDENRS
335200090213     C                   Z-ADD     VABNSP        RDENSP
335300090213     C     §PTCM1        IFNE      'S'
335400090213     C     WNRCMR        OREQ      *BLANKS
335500090213     C                   Z-ADD     1             RDENRP
335600090213     C                   ELSE
335700090213     C                   Z-ADD     2             RDENRP
335800090213     C                   END
335900090213     C                   MOVEL     'TD2380'      RDENMC
336000090213     C                   MOVEL     WDATFV        RDEVAL
336100090213     C                   WRITE     EDRDE000                             99
336200090213     c   99              eval      errori_in_Wrt = 'S'
336300090213     C                   END
336400090213     C*
336500090213     C                   END                                                    §PTCM1 = 'S'
336600090213     C*
336700090213     C                   ENDSR
336800090213     c*==================================================================*
336900090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
337000090213     c*==================================================================*
337100090213     c     Imp_ADDR_mail begsr
337200090213      *
337300090213     c                   clear                   Indirizzi
337400090213      *
337500090213      *  Lunghezza indirizzi presenti
337600090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
337700090213     c                   z-add     1             al_char           3 0
337800090213     c                   z-add     1             dal_char          3 0
337900090213     c                   z-add     1             tot_char          3 0
338000090213      *
338100090213     c     successivo    tag
338200090213      *
338300090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
338400090213      *   e aggiunge il dominio Bartolini + il (;) separatore
338500090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
338600090213     c                   eval      tot_char = al_char - dal_char
338700090213     c                   z-add     dal_char      Dac
338800090213     c                   z-add     tot_char      Luc
338900090213      *
339000090213     c                   clear                   Indir_Bartol     40
339100090213     c                   clear                   Indir_Parz       20
339200090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
339300090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
339400090213     c                             %Trim(bart_it)
339500090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
339600090213      *
339700090213     c                   if        al_char = Lun_Ind
339800090213     c                   goto      fine_indmail
339900090213     c                   else
340000090213     c                   eval      dal_char = ( al_char + 1 )
340100090213     c                   goto      successivo
340200090213     c                   end
340300090213      *
340400090213     C     fine_indmail  TAG
340500090213      *
340600090213     C                   ENDSR
340700090213     c*==================================================================*
340800090213      * Manda un Msg x E-mail
340900090213     c*==================================================================*
341000090213     c     Invio_Mail    begsr
341100090213      *
341200090213     c                   goto      non_inviare
341300090213      *
341400090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
341500090213     C* Inizializzo variabili
341600090213     C                   movel     *blanks       wrkEml          253
341700090213     C                   movel     *blanks       wrkMsg         5000
341800090213     C                   movel     *blanks       wrkOgg           44
341900090213      *
342000090213     C* Valorizzo i campi della e-m@ail - indirizzo
342100090213     C                   eval      wrkEml = Indirizzi
342200090213     C                   eval      wrkOgg = Oggetto
342300090213     C                   eval      wrkMsg = Messaggio
342400090213
342500090213     C                   call(e)   'TIS701C'
342600090213     C                   parm                    wrkEml
342700090213     C                   parm                    wrkOgg
342800090213     C                   parm                    wrkMsg
342900090213
343000090213     c     non_inviare   tag
343100090213     C*
343200090213     C                   ENDSR
343300090603     c*==================================================================*
343400090603      * Manda un Msg x E-mail a EDPAB
343500090603     c*==================================================================*
343600090603     c     Invio_Mail_AB begsr
343700090603      *
343800090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
343900090603     C* Inizializzo variabili
344000090603     C                   movel     *blanks       wrkEml          253
344100090603     C                   movel     *blanks       wrkMsg         5000
344200090603     C                   movel     *blanks       wrkOgg           44
344300090603      *
344400090603     C* Valorizzo i campi della e-m@ail - indirizzo
344500110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
344600110203     C                   eval      wrkEml = Indirizzi
344700090603     C                   eval      wrkOgg = Oggetto
344800090603     C                   eval      wrkMsg = Messaggio
344900110203      ********
345000110203     C********           call(e)   'TIS701C'
345100110203     C********           parm                    wrkEml
345200110203     C********           parm                    wrkOgg
345300110203     C********           parm                    wrkMsg
345400110203     C*
345500110203      ** *****
345600110203     C                   call(e)   'TRTCT00R2'
345700110203     C                   parm                    wrkEml
345800110203     C                   parm                    wrkOgg
345900110203     C                   parm                    wrkMsg
346000090603     C*
346100090603     C                   ENDSR
346200090211     ***********************************************************************
346300090211     **
346400090211     ** Send Message (QEZSNDMG) API
346500090211     **
346600090211     ***********************************************************************
346700090211     C     CalQEZSNDMG   BEGSR
346800090211      *
346900090211     ** Invio messaggio all'utente.
347000090211     C                   EVAL      SndMg03 = msgDSP
347100090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
347200090211     C                   EVAL      SndMg05(Jx) = User_msg
347300090211     C                   EVAL      SndMg06 = Jx
347400090211     C                   CLEAR                   QUSEC
347500090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
347600090211
347700090211     C                   CALL      'QEZSNDMG'
347800090211     C                   PARM                    SndMg01
347900090211     C                   PARM                    SndMg02
348000090211     C                   PARM                    SndMg03
348100090211     C                   PARM                    SndMg04
348200090211     C                   PARM                    SndMg05
348300090211     C                   PARM                    SndMg06
348400090211     C                   PARM                    SndMg07
348500090211     C                   PARM                    SndMg08
348600090211     C                   PARM                    Qusec
348700090211     C                   PARM                    SndMg10
348800090211     C                   PARM                    SndMg11
348900090211     C                   PARM                    SndMg12
349000090211      *
349100090211     C                   ENDSR
349200121105      *---------------------------------------------------------------*
349300121105      * CTRL caricamento schiere    PT                               -*
349400121105      *---------------------------------------------------------------*
349500121105     C     Check_PT      begsr
349600121105     C*
349700121105     c* Controllo riempimento schiera
349800121105     c                   clear                   trul0sds
349900121105     c                   eval      i0sski='CPT'
350000121105     c                   eval      i0sele=%elem(CPT)
350100121105     c                   eval      i0spie=x
350200121105     c                   eval      i0sfile='EDTAB00F'
350300121105     c                   eval      i0ssif=knsif
350400121105     c                   eval      i0spgm='TRTCT11R'
350500121113     c                   move      kpjbu         SvKpjbu
350600121113     c                   clear                   kpjbu
350700121105     c                   movel     trul0sds      kpjbu
350800121105     c                   call      'TRUL0SR'
350900121105     c                   parm                    kpjba
351000121113     c                   eval      kpjbu  =  SvKpjbu
351100121105     C*
351200121105     C                   ENDSR
351300090211     O*****************************************************************
351400090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
351500090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
351600090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
351700090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
