000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600091019????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFTMIN vers S93A  *
000700091016      *                         cliente   RHENUS ASSETS  con  IFTMIN vers S93A  *
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
004100091019???? D edS00CUX      E DS
004200091019???? D edS00COM      E DS
004300091019???? D edS00CTA      E DS
004400091019???? D edS00DTM      E DS
004500091019???? D edS00GID      E DS
004600091019???? D edS00MEA      E DS
004700091019???? D edS00NAD      E DS
004800091019???? D edS00PCI      E DS
004900091019???? D  D30_1                  4    353    DIM(10)
005000091019???? D edS00RFF      E DS
005100091019???? D edS00TDT      E DS
005200091019???? D edS00TOD      E DS
005300091019???? D edS00MOA      E DS
005400091019???? D edS00FTX      E DS
005500091019???? D  D05                   16    365    DIM(5)
005600091019???? D edS00TSR      E DS
005700091019???? D edS00UNA      E DS
005800091019???? D edS00UNB      E DS
005900091019???? D edS00UNH      E DS
006000091019???? D edS00UNT      E DS
006100091019???? D edS00UNZ      E DS
006200090210      **
006300090210     D  X30            S             35    DIM(20)                              generico segnacolli
006400090205      **
006500090209     D Valori_inErr    ds
006600090209     D  SKout_Errori                  1    DIM(50)
006700090209     D Descr_Errore    ds
006800090209     D  SKout_DesErr                 50    DIM(50)
006900090209      *
007000090205      *----------------------------------------------------*
007100091016     D EoFTivin        s              1
007200091016      *----------------------------------------------------*
007300091016      *
007400090205     D Tipo_seg        S              3
007500100720     D Trovato_seg     S              1
007600100716      *
007700100716     d keyUNBCLI       s             35
007800100716     d keyTIPOMSG      s              6
007900100716     d keyVERSION      s              3
008000100716     d keyRELEASE      s              3
008100100716     d keyAGENCY       s              3
008200100716     d keyASSOCIA      s              6
008300100716      *
008400100720     D DsGenerica      s           2048
008500090209     D segmento        s           2048
008600090209     D Errore          s              1
008700000223     D W0140           S             14  0
008800991129     D WORA            S              6  0
008900991129     D XX              S              3  0 INZ
009000110328     d Key_Generica35  s             35
009100991129     D WDTGIO          S              8  0
009200991129     D DATEU           S              8  0
009300991129     D DATA_eur        S               D   DATFMT(*eur)
009400050112     D NUM_Sped        s                   LIKE(vabnsp)
009500090213     D NUM_RECord      s                   LIKE(vnREC)
009600090213     D SAVNUM_RECord   s                   LIKE(vnREC)
009700090213     D Last_RECord     s                   LIKE(vnREC)
009800050112     D svkpjbu         s                   like(kpjbu)
009900091019???? D Dettaglio_NAD_destinatario...
010000091019???? D                 s              1
010100091019???? D NAD_destinatario_in_testata...
010200091019???? D                 s              1
010300091019???? D NAD_mittente_in_testata...
010400091019???? D                 s              1
010500091019???? D Inizio_Dettaglio...
010600091019???? D                 s              1
010700100120???? D seg_imprevisto...
010800100120???? D                 s              1
010900091019???? D Riferimento_Spedizione...
011000091019???? D                 s              3    INZ('CU ')
011100091019???? D Segmento_Inizio_Dettaglio...
011200091019???? D                 s              3    INZ('BGM')
011300130422     d FTX_ServizioMail_xDestinatario...
011400130422     d                 s             70A
011500140423     d FTX_MAIL_come_Servizio...
011600140423     d                 s              1A
011700140423     d FTX_ServizioSMS_xDestinatario...
011800140423     d                 s             70A
011900140423     d FTX_SMS_come_Servizio...
012000140423     d                 s              1A
012100140423     d FTX_ai_piani    s              1A
012200090209      *---------------------------------------------------------------------*
012300090209     D KPJBA         E DS
012400090209     D FNLV55DS      E DS
012500090209     D TIBS55DS      E DS
012600090209     D TISI95DS      E DS
012700090209     D UTEDSE0F      E DS
012800090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012900090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
013000090209      * Ds scomposzione tipo capoconti
013100090209     D TCUDS           DS
013200090209     D  F34                    3      4
013300090209     D  F56                    5      6
013400090209     D DS1B          E DS
013500090209     D DS15          E DS
013600090209     D DSCV          E DS
013700090213     D TRTC86DS      E DS
013800090209     D EDIDSPT       E DS
013900090209     D EDIDSPU       E DS
014000090209     D EDIDSPV       E DS
014100090211     D EDIDSCL       E DS
014200090216     D EDIDSTC       E DS
014300090209      **
014400090209      *  DS x salvataggio dati impostati in EDiVAB
014500090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
014600090209     D SALVA           DS           545
014700090210      **
014800090209     D WLBDA8          DS
014900090209     D  G02DAT                 1      8  0
015000090209     D  G02INV                 9     16  0
015100090209     D  G02ERR                17     17
015200090209     D  G02TGI                18     22  0
015300090209      *  DS x scomposizione numero spedizione
015400090209     D DSSPE           DS
015500090209     D  SPEAAS                 1      4  0
015600090209     D  SPELNP                 5      7  0
015700090209     D  SPENRS                 8      9  0
015800090209     D  SPENSP                10     16  0
015900090209      *  DS x scomposizione segnacollo
016000090209     D DSSGN           DS
016100090209     D  SGNLNP                 1      3  0
016200090209     D  SGNLNA                 4      6  0
016300090209     D  SGNNRS                 7      8  0
016400090209     D  SGNNSC                 9     15  0
016500090209     D  SGNZNC                16     17  0
016600090209      *  DS x stampa segnacollo
016700090209      *---------------                                                      *
016800090211     D  vablnp_PT      s                   LIKE(vablnp)
016900090212     D  vabnrs_PT      s                   LIKE(vabnrs)
017000090211     D  vabctm_PT      s                   LIKE(vabctm)
017100090209     D Wvabtic         s                   LIKE(vabtic)
017200090603     d tes_VABrmo      s                   LIKE(VABrmo)
017300090603     d tes_VABnmo      s                   LIKE(VABnmo)
017400090603     d tes_VABcmo      s                   LIKE(VABcmo)
017500090209     C*
017600090603     d tes_VABrsd      s                   LIKE(VABrsd)
017700090603     d tes_VABrd2      s                   LIKE(VABrd2)
017800090603     d tes_VABind      s                   LIKE(VABind)
017900090603     d tes_VABlod      s                   LIKE(VABlod)
018000090603     d tes_VABnzd      s                   LIKE(VABnzd)
018100090603     C*
018200090603     d tes_Valuta      s                   LIKE(VABvca)
018300090603     C*
018400090209      *---------------------------------------------------------------------*
018500090209      * Schiere
018600090209      *---------------------------------------------------------------------*
018700090209      * Schiera per reperimento nr.seganacollo
018800090209     D SGN             S              7  0 DIM(5000)
018900090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
019000090209      * Stringhe per conversione alfanumerico/numerico N.Documento
019100090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
019200090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
019300090209      * Nr. documento alfanumerico da decodificare
019400090209     D NDA             S              1    DIM(35)
019500090209      * Nr. documento alfanumerico da già decodificato
019600090209     D NDN             S              1    DIM(15)
019700090209      * Messaggi di errore
019800090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
019900090209      * Tabella codici trattamento merce (1B)
020000090209     D CTM             S              2    DIM(200)
020100090209     D DTM             S             67    DIM(200)
020200090209      * Tabella codici valuta
020300090209     D CCV             S              3    DIM(200)
020400090209     D DCV             S             89    DIM(200)
020500090209      * Tabella codici nazioni
020600090209     D C15             S              3    DIM(500)
020700090209     D ISO             S              3    DIM(500)
020800090209     D CPF             S              2  0 DIM(500)
020900090209      * Tabella partner
021000121105     D CPT             S             35    DIM(200)
021100121105     D CPTparz         S             35    DIM(%elem(CPT))
021200121105     D KSC_UNB         S              7    DIM(%elem(CPT))
021300121105     D DPT             S             90    DIM(%elem(CPT))
021400121105     D DPU             S             90    DIM(%elem(CPT))
021500121105     D DPV             S             90    DIM(%elem(CPT))
021600121105     D TRUL0SDS      E DS
021700121105      **
021800090209     d Ptn_parziale    s              1    inz('N')
021900090209     d sav_CPTx        s              3  0 inz(0)
022000090209      *
022100090209      * Tabella CL --> codici clienti - tariffa
022200130305     D CCL             S             35    DIM(999)
022300130305     D DCL             S             90    DIM(999)
022400090209      * Schiere x caricamento cod.cliente  <>
022500130305     D KCL             S              7  0 DIM(999)
022600090209      * Termini di consegna
022700110328     D K35_TpBolla     S             35    DIM(100)
022800110328     D Cod_TpBolla     S              3    DIM(100)
022900110328     D Decod_Porto     S              1    DIM(100)
023000110328     d Trovata_Tab_TB  S              1
023100110328      *
023200090216      * Tipo pagamento C/Assegno
023300090216     D CTC35           S             35    DIM(100)
023400090216     D CTC             S              2    DIM(100)
023500090216     D TPI             S              2    DIM(100)
023600090209      * Decodifiche servizi speciali
023700090209     D SS35            S             35    DIM(100)
023800090209     D SS              S              3    DIM(100)
023900090209     D DSS             S             90    DIM(100)
024000090209      * Schiere x routine di conversione CAP
024100090209     D CAP5            S              1    DIM(5)
024200090209     D CAP9            S              1    DIM(9)
024300090209     D CAPM            S              1    DIM(9)
024400090209      * Schiera per memorizzazione FTX ricevuti
024500090209     D QUA             S              3    DIM(100)
024600090209     D FTX             S             70    DIM(100)
024700090209      * Tabelle capiconto
024800090209      * Schiere x località
024900090209     D LOD             S              1    DIM(35)
025000060608      **?------------------------------------------------------------------ */
025100090601     C*? Ds Scrittura del VAT "E"
025200060608      **?------------------------------------------------------------------ */
025300060612     D XTOEBCDDS     E DS
025400060620      *
025500060608      **?------------------------------------------------------------------ */
025600050112      * Numeratore Bolle (302)
025700050112     D trul33ds      E DS
025800050112     D Ds3C          E DS
025900090209     C*
026000090209      **?------------------------------------------------------------------ */
026100090209     C* Definizione campi di work
026200090209     D pVLB            s                   LIKE(VABVLB)
026300090209     D kKUT            s                   LIKE(TBLKUT)
026400090209     D kCOD            s                   LIKE(TBLCOD)
026500090209     D kKEY            s                   LIKE(TBLKEY)
026600090209     D kKCC            s                   LIKE(ACOKCC)
026700090209     D kKSC            s                   LIKE(ACOKSC)
026800090209     D kAAA            s                   LIKE(NUFAAA)
026900090209     D kCNU            s                   LIKE(NUFCNU)
027000090209     D kFIL            s                   LIKE(NUFFIL)
027100090209     D kAAS            s                   LIKE(VABAAS)
027200090209     D kLNP            s                   LIKE(VABLNP)
027300090209     D kNRS            s                   LIKE(VABNRS)
027400090209     D kNSP            s                   LIKE(VABNSP)
027500090209     D kCMR            s                   LIKE(VABCMR)
027600090209     D kCCM            s                   LIKE(VABCCM)
027700090209     D kNCD            s                   LIKE(VADNCD)
027800090210     D kCNT            s                   LIKE(VADCNT)
027900090209     D kNMC            s                   LIKE(RDENMC)
028000090209     D kNRP            s                   LIKE(RDENRP)
028100090209     D kLNP1           s                   LIKE(RDELNP)
028200090209      *
028300090209      *  Invio E-mail
028400090209     D Indirizzi       s            253
028500090209     D Oggetto         s             44
028600090209     D Messaggio       s           5000
028700090209      *
028800090209     d   DSmail        ds
028900090209     D Mail_Ind                      44
029000090209     d   IND_char                     1    dim(44)
029100090209      *
029200090209     D Lun_Ind         s              5u 0
029300090209      *
029400090209     D Dac             s              5u 0
029500090209     D Luc             s              5u 0
029600090209      *
029700090209     C*---------------------------------------------------------------*
029800090209     D MsgDSP          S            256                                         Testo generico
029900090209     C*---------------------------------------------------------------*
030000090209     D SndMg01         S             10                                         Message type
030100090209     D                                     INZ('*INFO')
030200090209     D SndMg02         S             10                                         Deluvery mode
030300090209     D                                     INZ('*BREAK')
030400090209     D SndMg03         S            256                                         Message text
030500090209     D SndMg04         S             10I 0                                      Length of text
030600090209     D                                     INZ(%SIZE(SndMg03))
030700090209     D SndMg05         S             10                                         User profile
030800090209     D                                     DIM(299)
030900090209     D SndMg06         S             10I 0                                      Number of user
031000090209     D SndMg07         S             10I 0                                      Message sent indic.
031100090209     D SndMg08         S             10I 0                                      Function requested
031200090209     D SndMg10         S              1                                         Show display
031300090209     D                                     INZ('N')
031400090209     D SndMg11         S             20                                         Qualified MSGQ name
031500090209     D SndMg12         S              4                                         Name type indicator
031600090209     D                                     INZ('*USR')
031700090209      * indice di scihera
031800090209     D Jx              S              5I 0
031900090209      *
032000090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
032100090209      *
032200090209     d bart_it         C                   CONST('@Bartolini.it;')
032300090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
032400060612      * ?================================================================== */
032500060612      *
032600060612      * ?   * Campi x decodifica * (INPUT  del Record)
032700090130     D  Dati           s           2048
032800060612      * ?   *--------------------------------------------------------------*
032900060612     D  se_errore      s              1    inz(' ')
033000060612      * ?* ------------------------------------------------------ *
033100060710     D Digits          C                   '0123456789'
033200091019     D*------------------
033300091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
033400091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
033500060612      * ?================================================================== */
033600060612      *   Ciclo principale
033700090205      * ?================================================================== */
033800130321     c                   clear                   manda_mail        1
033900060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
034000060612     C*
034100060612     C                   if        not %open(tivin00r)
034200060612     C                   open      tivin00r
034300060612     C                   endif
034400060612      **
034500060612      * ?------------------------------------------------------------------ */
034600060612     C*? Controllo dati arrivati da DPD
034700060612      * ?------------------------------------------------------------------ */
034800060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
034900060612      * ?- controllando che la trasmissione sia completa.
035000090205      **
035100060612      * ?              /*---------------------- */
035200090205     c                   goto      nonFare_adesso
035300060612     c                   exsr      check_Trasm
035400090205     c     nonFare_adessotag
035500060612      * ?              /*---------------------- */
035600090205      **
035700060613      **  Errore di trasmissione x tutti i records
035800060613      **   --> file in errore
035900060613     c                   if        se_errore = 'S'
036000060612
036100060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
036200090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
036300090205     c                   exsr      Msg_errato
036400060612      **
036500060612     c                   else
036600060614      **
036700060612      * ?------------------------------------------------------------------ */
036800060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
036900060612      * ?------------------------------------------------------------------ */
037000090205      **
037100060612      * ?              /*---------------------- */
037200060612     c                   exsr      Importa_Msg
037300060612      * ?              /*---------------------- */
037400060612
037500060614     c                   end
037600060614
037700060612     C                   if        %open(tivin00r)
037800060612     C                   close     tivin00r
037900060612     C                   endif
038000060612      *
038100060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
038200090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
038300090225     c                             esito ='0'
038400060710     C                   eval      esito ='0'
038500090225      *
038600090225     c                   COMMIT
038700090224      *
038800090213     c                   if        almeno_un_due ='S'
038900090213     C                   eval      esito ='1'
039000060614     C                   endif
039100090213     C                   endif
039200060614      *
039300060612     c                   SETON                                        LR
039400060612      * ?================================================================== */
039500060612      *? Importa i records della tramsissione
039600060612      * ?------------------------------------------------------------------ */
039700060612     c     Importa_Msg   Begsr
039800060612
039900090211     C                   EXSR      AZZMSG
040000060614
040100060612     c     *start        setll     Tivin00r
040200060612     c                   read      Tivin00r
040300091016     c                   if        %Eof(tivin00r)
040400091016     c                   move      'S'           EoFTivin
040500091016     c                   end
040600060612
040700060612     c                   dow       not %eof(Tivin00r)
040800060614
040900060614      * solo i record sflaggati da rielaborare
041000060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
041100090211      *  Sempre Record OK
041200090211      ** Controlli formali sui campi
041300090211     C                   eval      vinFLG = '1'
041400060612     c                   clear                   se_errore
041500060612
041600060612      ** Decodifica record a campi variabili
041700060612      * ?              /*---------------------- */
041800090205     c                   exsr      Decod_Segmento
041900090209      * ?              /*---------------------- */
042000090209
042100090213      *** ------------------------------------
042200091019????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT" x IFTMIN
042300090213      *** ------------------------------------
042400090213      * ?              /*---------------------- */
042500090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
042600090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
042700090213      * ?              /*---------------------- */
042800090601      ***
042900091019????  **
043000091019????  *   OGNI fine DETTAGLIO è determinato dall'UNT.
043100091019???? c                   if           tipo_seg = 'UNT'
043200091019????  *
043300090213      *  Deve flaggare il dettaglio con dei problemi
043400090213     C                   if        Err_in_detta = 'S'
043500090213      **?              /*---------------------- */
043600090213     c                   exsr      DETta_Errato
043700090213      **?              /*---------------------- */
043800090213     c                   end
043900090213      *
044000060612      **  Se presente un errore nel record emette una segnalazione msg
044100091019???? c                   if        se_errore = 'S'
044200091019???? c                   else
044300091019????  ******
044400090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
044500090227     c                   if         se_trovato_NAD ='S'
044600060612      * ?              /*---------------------- */
044700090209     c                   exsr      Scrive_VAB
044800060612      * ?              /*---------------------- */
044900090227     c                   else
045000090227     c                   eval      errori_in_Wrt = 'S'
045100090227     c                   end
045200090227      *
045300090209      *  Problemi nella decodifica dei campi VAB/VAT
045400090213     c                   if        errori_in_Wrt = 'S'
045500090213      *
045600090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
045700090213     c                   exsr      Error_DET
045800090213      *
045900090213     c                   ROLBK
046000090213      *
046100090213     c                   else
046200090601      *
046300090213     c                   COMMIT
046400090601      *
046500090227      * Dopo aver confermato, con COMMIT
046600090227      *  si prepara per una nuova spedizione
046700090227      * ?                         --------------
046800090227     c                   exsr      Nuova_spediz
046900090227      * ?                         --------------
047000090213     c                   end
047100090209
047200090209     c                   end
047300090210      **
047400060620     c                   end
047500060612
047600060621      *  x errore bloccante nella composizione del VAB/VAT
047700060621     c                   if        err_bloccante ='S'
047800060621     C                   eval      vinFLG = '2'
047900090211     c                   eval      esito  = '2'
048000091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
048100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
048200090603     C                             Bolle import in filiale - messaggio -
048300090603     C                             con UNB non presente in tabella PT controlla-
048400090603     C                             re EDI ricevuto su UPLOAD.'
048500090603     c                   exsr      invio_mail_AB
048600060621     c                   end
048700060612
048800091016     c                   if        EoFTivin = ' '
048900060612     c                   update    Tivin000
049000091016     c                   end
049100090211
049200090211      *** se non è riconosciuto l'UNB deve uscire direttamente
049300090211     c                   if        se_errore ='S'  and
049400090211     c                             tipo_seg = 'UNB'
049500090213     c                   eval      err_bloccante = 'S'
049600090211     c                   exsr      Msg_errato
049700090211     c                   LEAVE
049800090211     c                   endIF
049900090211      ***
050000060614     c                   endIF
050100060612
050200060612     c                   read      Tivin00r
050300091016     c                   if        %Eof(tivin00r)
050400091016     c                   move      'S'           EoFTivin
050500091016     c                   end
050600060612     C                   ENDdo
050700060612      **
050800060612     c                   endSR
050900100120      * ?------------------------------------------------------------------ */
051000100120      *? Controlla la trasmissione   se completa
051100100120      * ?------------------------------------------------------------------ */
051200100120     c     Check_Trasm   Begsr
051300100120
051400100120     C                   clear                   se_errore
051500100120     C                   clear                   Riga_iNIZIo       1
051600100120     C                   clear                   Riga_fINe         1
051700100120      ** primo  record
051800100120     c     *start        setll     tivin00r
051900100120      *
052000100120     c     rileggi       tag
052100100120     c                   read      tivin00r
052200100120     c                   if        %Eof(tivin00r)
052300100120     c                   move      'S'           EoFTivin
052400100120     c                   end
052500100120      *
052600100120     c                   if        not %eof(tivin00r)
052700100120      *
052800100120     c***********        movel(p)  VINDTA        dati
052900100120     c                   clear                   dati
053000100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
053100100120     c                   if        dati = *blank
053200100120      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
053300100120      * le leggo e le escludo.
053400100120     C                   eval      vinFLG = '1'
053500100120     c                   if        EoFTivin = ' '
053600100120     c                   update    Tivin000
053700100120     c                   end
053800100120     c                   goto      rileggi
053900100120     c                   end
054000100120      * ?              /*---------------------- */
054100100120     c                   exsr      Decod_Segmento
054200100120      * ?              /*---------------------- */
054300100120     c                   endif
054400100120      **
054500100120      ** ultimo record
054600100120     c     *hival        setll     tivin00r
054700100120     c     leggiAncora   tag
054800100120     c                   readp     tivin00r
054900100120     c                   if        %Eof(tivin00r)
055000100120     c                   move      'S'           EoFTivin
055100100120     c                   end
055200100120     c                   if        not %eof(tivin00r)
055300100120      *
055400100120     c***********        movel(p)  VINDTA        dati
055500100120     c                   clear                   dati
055600100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
055700100120     c                   if        dati = *blank
055800100120      * le ultime righe potrebbero essere vuote prima di finire il messaggio
055900100120      * le leggo e le escludo.
056000100120     C                   eval      vinFLG = '1'
056100100120     c                   if        EoFTivin = ' '
056200100120     c                   update    Tivin000
056300100120     c                   end
056400100120     c                   goto      leggiAncora
056500100120     c                   end
056600100120      * ?              /*---------------------- */
056700100120     c                   exsr      Decod_Segmento
056800100120      * ?              /*---------------------- */
056900100120     c                   endif
057000100120
057100100120      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
057200100120      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
057300100120      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
057400100120     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
057500100120      * ?-----> Errore
057600100120     C                   eval      se_errore = 'S'
057700100120     c                   end
057800100120
057900100120     c                   endSR
058000100120      * ?------------------------------------------------------------------ */
058100100120      *?    Decodifica il segmento ed importa i campi in formato DS
058200100120      * ?------------------------------------------------------------------ */
058300100120     c     Decod_SegmentoBegsr
058400100120      **
058500100120     c                   clear                   dati
058600100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
058700100120     c                   eval      tipo_seg = %subst(dati:1:3)
058800100120     c                   eval      segmento = dati
058900100120      *
059000100120      * Imposta il flag di Inizio Dettaglio
059100100120     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
059200100120     c                   eval      Inizio_Dettaglio ='S'
059300100120     c                   end
059400100120      *
059500100120     c                   clear                   seg_imprevisto
059600100720     c                   clear                   trovato_seg
059700100716      *
059800100716     C                   clear                   keyUNBCLI
059900100716     C                   clear                   keyTIPOMSG
060000100716     C                   clear                   keyVERSION
060100100716     C                   clear                   keyRELEASE
060200100716     C                   clear                   keyAGENCY
060300100716     C                   clear                   keyASSOCIA
060400100720      *
060500100720      *  Chiamata generica x decodificare il singolo segmento letto
060600100720     c                   call      'TRTCT00R1'
060700100720      * input
060800100720     c                   parm                    tipo_seg
060900100720     c                   parm                    segmento
061000100720     C                   parm                    keyUNBCLI
061100100720     C                   parm                    keyTIPOMSG
061200100720     C                   parm                    keyVERSION
061300100720     C                   parm                    keyRELEASE
061400100720     C                   parm                    keyAGENCY
061500100720     C                   parm                    keyASSOCIA
061600100720      *output
061700100720     c                   parm                    Errore
061800100720     c                   parm                    DsGenerica
061900100720     c                   parm                    Valori_inErr
062000100720     c                   parm                    Descr_Errore
062100100720     c                   parm                    trovato_seg
062200100720      **--------
062300100720      *  Ritorna la DS GENERICA oppure l'errore
062400100120      **--------
062500100120     c                   select
062600100720      *
062700100720     c                   when      trovato_seg ='N'
062800100720      *
062900100720     c                   move      'S'           seg_imprevisto
063000130321     c                   eval           manda_mail='S'
063100100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
063200100720     c                   exsr      Error_DET
063300100720     c                   goto      end_Decod
063400100720      **--------
063500100120     c                   when      tipo_seg = 'UNA'
063600100720     c                   movel(p)  DsGenerica    EDS00UNA
063700100120      *
063800100720      **--------
063900100120     c                   when      tipo_seg = 'UNB'
064000100720     c                   movel(p)  DsGenerica    EDS00UNB
064100100120     C                   EXSR      DATI_UNB
064200100120      * --------
064300100120     c                   when      tipo_seg = 'UNH'
064400100720     c                   movel(p)  DsGenerica    EDS00UNH
064500100120     C                   EXSR      DATI_UNH
064600100120      * --------
064700100120     c                   when      tipo_seg = 'BGM'
064800100720     c                   movel(p)  DsGenerica    EDS00BGM
064900100120      **--------
065000100120     c                   when      tipo_seg = 'DTM'
065100100720     c                   movel(p)  DsGenerica    EDS00DTM
065200100120     C                   EXSR      DATI_DTM
065300100120      * --------
065400100120     c                   when      tipo_seg = 'TSR'
065500100720     c                   movel(p)  DsGenerica    EDS00TSR
065600100120      **--------
065700100120     c                   when      tipo_seg = 'CNT'
065800100720     c                   movel(p)  DsGenerica    EDS00CNT
065900100120      **--------
066000100120     c                   when      tipo_seg = 'CUX'
066100100720     c                   movel(p)  DsGenerica    EDS00CUX
066200100120      * --------
066300100120     c                   when      tipo_seg = 'TOD'
066400100720     c                   movel(p)  DsGenerica    EDS00TOD
066500100120     C                   EXSR      DATI_TOD
066600100120      **--------
066700100120     c                   when      tipo_seg = 'RFF'
066800100720     c                   movel(p)  DsGenerica    EDS00RFF
066900100120     C                   EXSR      DATI_RFF
067000100120      **--------
067100100120     c                   when      tipo_seg = 'TDT'
067200100720     c                   movel(p)  DsGenerica    EDS00TDT
067300100120      **--------
067400100120     c                   when      tipo_seg = 'MOA'
067500100720     c                   movel(p)  DsGenerica    EDS00MOA
067600100120     C                   EXSR      DATI_MOA
067700100120      **--------
067800100120     c                   when      tipo_seg = 'FTX'
067900100720     c                   movel(p)  DsGenerica    EDS00FTX
068000100120     C                   EXSR      DATI_FTX
068100100120      **--------
068200100120     c                   when      tipo_seg = 'NAD'
068300100720     c                   movel(p)  DsGenerica    EDS00NAD
068400100120     C                   EXSR      DATI_NAD
068500100120      **--------
068600100120     c                   when      tipo_seg = 'CTA'
068700100720     c                   movel(p)  DsGenerica    EDS00CTA
068800100120     C                   EXSR      DATI_CTA
068900100120      **--------
069000100120     c                   when      tipo_seg = 'COM'
069100100720     c                   movel(p)  DsGenerica    EDS00COM
069200100120     C                   EXSR      DATI_COM
069300100120      **--------
069400100120     c                   when      tipo_seg = 'GID'
069500100720     c                   movel(p)  DsGenerica    EDS00GID
069600100120     C                   EXSR      DATI_GID
069700100120      **--------
069800100120     c                   when      tipo_seg = 'MEA'
069900100720     c                   movel(p)  DsGenerica    EDS00MEA
070000100120     C                   EXSR      DATI_MEA
070100100120      **--------
070200100120     c                   when      tipo_seg = 'PCI'
070300100720     c                   movel(p)  DsGenerica    EDS00PCI
070400100120     C                   EXSR      DATI_PCI
070500100120      **--------
070600100120     c                   when      tipo_seg = 'UNT'
070700100720     c                   movel(p)  DsGenerica    EDS00UNT
070800100120     C                   EXSR      DATI_UNT
070900100120      **--------
071000100120     c                   when      tipo_seg = 'UNZ'
071100100720     c                   movel(p)  DsGenerica    EDS00UNZ
071200100120     C                   EXSR      DATI_UNZ
071300100120      **--------
071400100120     c                   OTHER
071500100120      **--------
071600100120     c                   move      'S'           seg_imprevisto
071700100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
071800130321     c                   eval           manda_mail='S'
071900100120     c                   exsr      Error_DET
072000100120     c                   goto      end_Decod
072100100120      **--------
072200100120     c                   endSL
072300100120      **
072400100120     c     end_Decod     endSR
072500090211     C*----------------------------------------------------------------
072600090211     C*? AZZMSG - Azzera dati messaggio
072700090211     C*----------------------------------------------------------------
072800090211     C     AZZMSG        BEGSR
072900090211     C*
073000090211     C* Pulisco dati di work x foglio viaggio
073100090211     C                   MOVEL     *BLANKS       WNUMFV           35
073200090211     C                   MOVEL     *BLANKS       WNRCMR           35
073300090211     C                   Z-ADD     0             WDATPA            8 0
073400090211     C                   Z-ADD     0             WDATFV            8 0
073500090211     C                   Z-ADD     0             WHHNFV            4 0
073600090211     C                   Z-ADD     0             WDTCMR            8 0
073700090211     C                   Z-ADD     0             WHHCMR            4 0
073800090211      *
073900090211      * pulisce gli RFF+FF di testata
074000090211     C                   clear                   $CLKSC            7 0
074100090211     C                   clear                   $CLTAR            3
074200090211     C                   clear                   $CLLNP            3 0
074300090211     C                   clear                   $CLNRS            2 0
074400090211     C                   clear                   $CLCTM            2
074500090211     C                   clear                   $CLBS1            1
074600090211     C                   clear                   $CLfnsp           1            *blk/S
074700090211      *
074800090211     C*  Inizializzo variabile new documento
074900090211     C                   Z-ADD     0             KCL
075000090211     C                   Z-ADD     0             WW                3 0
075100090211     ** Inizializza campo di work
075200090211     c                   move      'N'           WGID99
075300090211      * per controllare se almeno un record è stato importato sul VAB
075400090211     c                   clear                   Almeno_Uno        1
075500090213     c                   clear                   Almeno_Un_Due     1
075600090211     c                   eval      esito  = '0'
075700090211     C*
075800090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
075900090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
076000090603     c                   eval      Dettaglio_NAD_destinatario = *blank
076100090603     c                   eval      NAD_destinatario_in_testata = *blank
076200090603     c                   eval      NAD_mittente_in_testata = *blank
076300090603     c                   eval      Inizio_Dettaglio = *blank
076400090603     C*
076500090603     c                   clear                   tes_VABrmo
076600090603     c                   clear                   tes_VABnmo
076700090603     c                   clear                   tes_VABcmo
076800090603     C*
076900090603     c                   clear                   tes_VABrsd
077000090603     c                   clear                   tes_VABrd2
077100090603     c                   clear                   tes_VABind
077200090603     c                   clear                   tes_VABlod
077300090603     c                   clear                   tes_VABnzd
077400090603     C*
077500090603     c                   clear                   tes_Valuta
077600090603     C*
077700090211     C                   ENDSR
077800090210     C*----------------------------------------------------------------
077900090210      *? DECODIFICA DATI UNB
078000090210     C*----------------------------------------------------------------
078100090210     C     DATI_UNB      BEGSR
078200090210      *
078300090209      *  INIZIALIZZA  dati fondamentali messaggio
078400090209     C                   clear                   EDIDSPT
078500090209     C                   clear                   EDIDSPU
078600090209     C                   clear                   EDIDSPV
078700090209     C                   clear                   wnrDOC           35
078800090209     C                   move      'N'           PT_ok             1
078900090209      ** ---------------------------------------------------------------
079000090209      * quindi 1° cosa:
079100090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
079200090209      *     senza questo il messaggio NON è trascodificabile
079300090209     C                   MOVEL     unb0004       WCOD             35
079400090209     C                   MOVE      unb000720     WCOD
079500090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
079600090209     C                   Z-ADD     1             XX                3 0
079700090209     C     WCOD          LOOKUP    CPT(XX)                                32
079800090209      *
079900090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
080000090209     C                   If        *IN32 = *off
080100090209      * ******  Errore generale sul Messaggio
080200090209      * Msg di allerta Traduzione
080300090209     C                   EVAL      Indirizzi = CED_Bart
080400091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
080500090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
080600090209     C                             Bolle import in filiale - messaggio -
080700090209     C                             con UNB non presente in tabella PT controlla-
080800090209     C                             re EDI ricevuto su UPLOAD.'
080900090209      *
081000130321     c                   eval           manda_mail='S'
081100090209     c                   exsr      invio_mail
081200090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
081300090210     C                   eval      vinFLG = '2'
081400090210     C                   eval      se_errore   = 'S'
081500090210     c                   goto      end_UNB
081600090209      *
081700090209     C                   move      'N'           PT_ok             1
081800090209      *
081900090209      *             *------------------------------*
082000090209     C                   eLSe
082100090209      *             *------------------------------*
082200090209      * Se tutto OK su tab.PT:
082300090209     C                   move      'S'           PT_ok             1
082400090209      *
082500090209     C                   MOVEL     DPT(XX)       EDIDSPT
082600090209     C                   MOVEL     DPU(XX)       EDIDSPU
082700090209     C                   MOVEL     DPV(XX)       EDIDSPV
082800091016     c                   clear                   Mittente         35
082900091016     c                   clear                   Mitt_Qualifier    4
083000091016     c                   clear                   Id_Messaggio     14
083100090226     C                   eval      Mittente       = unb0004
083200090226     C                   eval      Mitt_qualifier = unb000720
083300090226     C                   eval      Id_Messaggio   = UNB0022
083400090209      *
083500090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
083600090209      *  ricevuti:
083700090209     c                   movel     §PVema        mail_ind         44
083800090209      *
083900090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
084000090209     c                   if        mail_ind <> *blank
084100090209     c                   exsr      imp_ADDR_mail
084200090209     c                   end
084300090209      *
084400090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
084500090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
084600090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
084700090209     C                   if        §PULUN = *blank
084800090209     C                   eval      §PULUN = '00'
084900090209     c                   end
085000090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
085100090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
085200090209      *
085300090209     C                   Z-ADD     §PTKSC        WKSC              7 0
085400090211     C                   MOVEL     §PTLNP        VABlnp_PT
085500090212     C                   MOVEL     §PTNRS        VABnrs_PT
085600090211     C                   MOVEL     §PTCTM        VABctm_PT
085700090209      *
085800090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
085900090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
086000090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
086100090209      *  i records trasmessi).
086200090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
086300090209      *
086400090209      *  In base al cod.trattamento merce reperisco tipo tratt.
086500090209     C                   CLEAR                   DS1B
086600090209     C                   Z-ADD     1             Y                 3 0
086700090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
086800090209     C     *IN32         IFEQ      '1'
086900090209     C                   MOVEL     DTM(Y)        DS1B
087000090209     C                   MOVEL     DS1B          WSVTRM           38
087100090209     C                   END
087200090209      *
087300090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
087400090209     C                   clear                   ACORAG
087500090209     C                   clear                   INDCAE
087600090209     C                   clear                   INDCAP
087700090209     C                   clear                   INDSTA
087800090209      * Decodifico partner
087900090209     C                   Z-ADD     1             KKUT
088000090209     C                   Z-ADD     KCI           KKCC
088100090209     C                   MOVE      §PTKSC        KKSC
088200090209     C     KACO          CHAIN     CNACO00F                           31
088300090209     C     KIND          CHAIN     CNIND00F                           31
088400090211      *
088500090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
088600090211      *   questa schiera è utilizzata anche per i clienti.
088700090211     C     §ptKSC        LOOKUP    KCL                                    39
088800090211     c                   If        *in39 = *off
088900090211     C                   add       1             WW
089000090211     C                   z-add     §ptKSC        KCL(WW)
089100090211     C                   Endif
089200090209      *
089300090209     C                   MOVEL     unb0017       WAA4              4 0
089400090209     C                   MOVE      WAA4          WAA2              2 0
089500090209     C                   MOVE      unb0017       WGG               2 0
089600090209     C                   MOVE      unb0017       WDTPRE            8 0
089700090209     C                   MOVE      unb0019       WHMPRE            4 0
089800090209     C                   MOVE      unb0017       WDTMSG            6 0
089900090209     C                   MOVE      WAA2          WDTMSG
090000090209     C                   MOVEL     WGG           WDTMSG
090100090210      * potremmo
090200090210      * Utilizzare l'identificativo della trasmissione come CMR
090300090209     C                   IF        unb0022 <> *blank
090400090209     C                   MOVEL     unb0022       WNRDOC
090500090211     C                   MOVEL     unb0022       WNUMFV
090600090211     C                   MOVEL     unb0022       WNRCMR
090700090209     c                   end
090800090209      *
090900090209     c                   Endif
091000090209      **
091100090210     c     end_UNB       endSR
091200090209      * ?================================================================== */
091300090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
091400090209      * ?================================================================== */
091500090227     C     Nuova_spediz  BEGSR
091600090227      **
091700090227      * Pulisco campi del file
091800090227     c                   z-add     vnrec         SAVNUM_RECord
091900090227     C                   CLEAR                   EDiVAB00
092000090227     C                   CLEAR                   EDiVAT00
092100090227     C                   CLEAR                   EDiVAD00
092200090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
092300090227     C                   MOVEL     'N'           WGID99            1
092400090227     C                   clear                   XY                5 0
092500090227     C                   clear                   §§                5 0
092600090227     C                   clear                   WS1496            5 0
092700090227     C                   clear                   WNSP             35
092800090227     C                   Z-ADD     *ZEROS        PVLB
092900090227     C                   MOVEA     *HIVAL        SGN
093000090227     C*  Azzero codice cliente - tariffa
093100090227     C                   Z-ADD     0             WTOCOC           16 0
093200090227     C                   Z-ADD     0             WTOPLC           16 2          ???
093300090227     C                   Z-ADD     0             WTOPNC           16 2          ???
093400090227     C*  Azzero codice cliente - tariffa
093500090227     C                   clear                   WCLKSC
093600090227     C                   clear                   WCLTAR
093700090227     C                   clear                   WCLLNP
093800090227     C                   clear                   WCLNRS
093900090227     C                   clear                   WCLCTM
094000090227     C                   clear                   WCLBS1
094100090227     C                   clear                   WCLfnsp                        *blk/S
094200090227     C                   clear                   WNOT1            35
094300090227     C                   clear                   WNOT2            35
094400130422     C                   clear                   Note_1T          35
094500130422     C                   clear                   Note_2T          35
094600140423     C                   clear                   Note_1sms        35
094700140423     C                   clear                   Note_2sms        35
094800130422     c                   eval      FTX_ServizioMail_xDestinatario = *blank
094900140423     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
095000140423     c                   eval      FTX_MAIL_come_Servizio = *blank
095100140423     c                   eval      FTX_SMS_come_Servizio = *blank
095200140423     c                   eval      FTX_ai_piani = *blank
095300090227     C*  Campi di work
095400090227     c                   clear                   wVabTIC
095500090227     c                   clear                   wri_vabtic        1
095600090227      *
095700090227     C* pulisce campi di work
095800090227     c                   clear                   watrde           35
095900090227     c                   clear                   wattel           25
096000090227     C                   clear                   WRMN              3
096100090227      *
096200090227      * Un errore nel dettaglio
096300090227     C                   clear                   Err_in_detta      1
096400090227     c                   clear                   errori_in_Wrt     1
096500090227     c                   clear                   se_trovato_NAD    1
096600090227      **
096700090227     c                   endSR
096800090227      * ?================================================================== */
096900090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
097000090227      * ?================================================================== */
097100090227     C     DATI_UNH      BEGSR
097200090227      *
097300090227      *  Prepara una nuova spedizione
097400090227      * ?                         --------------
097500090227     c                   exsr      Nuova_spediz
097600090227      * ?                         --------------
097700090211      *
097800090211      ** Riferimento del cliente
097900090211     C                   MOVEL     unh0062       WPRGSP           35
098000090211     C                   MOVEL     unh0062       WNSP             35
098100090211     C                   MOVEL     unh0062       WNUM             35
098200090211      * ?                                       --------
098300090211     C                   MOVEL     unh0062       VABRMA
098400090211      * ?                                       --------
098500090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
098600090211     C                   SUB       1             WLUNGH
098700090211     C                   EXSR      TSTNUM
098800090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
098900090211      * ?                                       --------
099000090211     C                   MOVEL     WNUMOK        VABRMN
099100090211      * ?                                       --------
099200090211     C                   END
099300090210      **
099400090303     C                   MOVEL     VABlnp_PT     VABLNP
099500090303     C                   MOVEL     VABnrs_PT     VABNRS
099600090303     C                   MOVEL     VABctm_PT     VABCTM
099700090303      **
099800090303      **
099900090210     c     end_UNH       endSR
100000090210      * ?================================================================== */
100100090601     C*? Imposta la VALUTA di eventuali C.O.D.
100200090210      * ?================================================================== */
100300090601     C     DATI_CUX      BEGSR
100400090210     C*
100500090603     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
100600090603     c                   if        cux6345 <> *blank
100700090603     C                   MOVEL     cux6345       Tes_Valuta
100800090603     c                   end
100900090601      **
101000090601     c     end_CUX       endSR
101100090601      * ?================================================================== */
101200090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
101300090601      * ?================================================================== */
101400090601     C     DATI_DTM      BEGSR
101500090601     C*
101600090209     C*  Se data documento  memorizzo numero e data x CMR
101700090603     C     dtm2005       IFEQ      '9'
101800090209     C                   MOVEL     dtm2380       WDATA            35            Data
101900090209     C                   MOVEL     dtm2379       WFORM             3            Formato
102000090209     C                   EXSR      CNVDAT
102100090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
102200090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
102300090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
102400090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
102500090209     C     WERRO         IFEQ      'S'
102600090210     C                   eval      vinMSG = 'DTM data errata'
102700090213     c                   exsr      Error_DET
102800090210     c                   goto      end_DTM
102900090209     C                   END
103000090209     C                   END
103100090209      **
103200090209     C*  Se data consegna richiesta imposto già sui campi del VAB
103300090603     C     dtm2005       IFEQ      'X'
103400090209     C                   MOVEL     dtm2380       WDATA            35            Data
103500090209     C                   MOVEL     dtm2379       WFORM             3            Formato
103600090209     C                   EXSR      CNVDAT
103700090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
103800090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
103900090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
104000090209     C     WERRO         IFEQ      'S'
104100090210     C                   eval      vinMSG = 'DTM data errata'
104200090213     c                   exsr      Error_DET
104300090210     c                   goto      end_DTM
104400090209     C                   END
104500090209     C                   END
104600090211      **
104700090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
104800090603     C     dtm2005       IFEQ      '10'
104900090211     C                   MOVEL     dtm2380       WDATA            35            Data
105000090211     C                   MOVEL     dtm2379       WFORM             3            Formato
105100090211     C                   EXSR      CNVDAT
105200090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
105300090211     C     WERRO         IFEQ      'S'
105400090211     C                   eval      vinMSG = 'DTM data errata'
105500090213     c                   exsr      Error_DET
105600090211     c                   goto      end_DTM
105700090211     C                   END
105800090211     C                   END
105900090209      **
106000090210     c     end_DTM       endSR
106100090209      * ?================================================================== */
106200090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
106300090209      * ?================================================================== */
106400090209     C     DATI_TOD      BEGSR
106500090209      *
106600090210      *  Trascodifico tipo trasporto
106700090210     c                   clear                   TOD_char3         3
106800090210      *
106900090210      *  imposta il default dalla tabella PT se c'è
107000090210     C     §puPFA        ifNE      *blank
107100090210     c                   SELECT
107200090210      *  Porto Franco
107300090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
107400090210     C                   movel     '4'           VABCBO                         + C/Ass
107500090210     c                   WHEN      §puPFA = 'F'
107600090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
107700090210      *  Porto Assegnato
107800090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
107900090210     C                   movel     '6'           VABCBO                         + C/Ass
108000090210     C                   WHEN      §puPFA = 'A'
108100090210     C                   movel     '2'           VABCBO                         Senza C/Ass
108200090210     C                   ENDSL
108300090210     c                   end
108400090210      *
108500090210      *   Imposta o il codice o il metodo
108600090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
108700090210     c                   if        tod4053  <> *blank
108800090210     c                   movel(p)  tod4053       TOD_char3
108900090210     c                   else
109000090210     c                   if        tod4215  <> *blank
109100090210     c                   movel(p)  tod4215       TOD_char3
109200090210     c                   end
109300090210     c                   end
109400110328      ****************
109500110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
109600110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
109700110328      ****************
109800110328     c                   eval      Trovata_Tab_TB ='N'
109900110328      *
110000110328     c                   if              TOD_char3 <> *blank
110100110328     c                   movel(p)  TOD_char3     Key_Generica35
110200110328     c                   move      §puTB         Key_Generica35
110300110328     C                   eval      Y = 1
110400110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
110500110328     c   32              eval      Trovata_Tab_TB ='S'
110600110328      *
110700110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
110800110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
110900110328      *
111000110328     c                   if          Trovata_Tab_TB ='N'
111100110328     c                                 and
111200110328     C                               tod4215 <> *blank
111300110328     c                   movel(p)  tod4215       TOD_char3
111400110328     c                   movel(p)  TOD_char3     Key_Generica35
111500110328     c                   move      §puTB         Key_Generica35
111600110328     C                   eval      Y = 1
111700110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
111800110328     c   32              eval      Trovata_Tab_TB ='S'
111900110328     c                   end
112000110328     c                   end
112100110328      *
112200110328      ****************
112300110328      ******* Se vi sono eccezioni sul cliente imposta quelle
112400110328      ****************
112500110328     c                   If          Trovata_Tab_TB ='S'
112600110328      *
112700110328     c                   SELECT
112800110328     c                   WHEN      Decod_Porto(Y) = 'F'
112900110328     c                               and  VABcas > *zeros
113000110328     C                   movel     '4'           VABCBO                         + C/Ass
113100110328     c                   WHEN      Decod_Porto(Y) = 'F'
113200110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
113300110328      *  Porto Assegnato
113400110328     c                   WHEN      VABCAS > *zeros
113500110328     C                   movel     '6'           VABCBO                         + C/Ass
113600110328     C                   OTHER
113700110328     C                   movel     '2'           VABCBO                         Senza C/Ass
113800110328     C                   ENDSL
113900110328      *
114000110328      ****************  altrimenti
114100110328      ******* Se non vi sono eccezioni per il cliente
114200110328      *   Rileva a questo punto lo Standard per tutti
114300110328      ****************
114400110328     c                   elseIf      Trovata_Tab_TB ='N'
114500090210      **
114600090210     c                   if              TOD_char3 <> *blank
114700090210     c                   movel(p)  TOD_char3     fld35a           35
114800090213     C                   movel     UNB_Mittente  etbUNB
114900090213     C                   movel(p)  'TOD'         etbSGM
115000090213     C                   movel(p)  TOD_char3     etbVALSGM
115100090213     c     K_TAB1L       chain     edstbl01l
115200090210      *
115300090210      * prova quindi senza l'UNB specifico del cliente
115400090213     c                   if        not %Found(edstbl01l)
115500090213     C                   clear                   etbUNB
115600090213     c     K_TAB1L       chain     edstbl01l
115700090210     c                   end
115800090210      *
115900090213     c                   if        %Found(edstbl01l)
116000090210     c                   SELECT
116100090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
116200090210     C                   movel     '4'           VABCBO                         + C/Ass
116300090213     c                   WHEN      ETBDATI = 'F'
116400090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
116500090210      *  Porto Assegnato
116600090210     c                   WHEN      VABCAS > *zeros
116700090210     C                   movel     '6'           VABCBO                         + C/Ass
116800090210     C                   OTHER
116900090210     C                   movel     '2'           VABCBO                         Senza C/Ass
117000090210     C                   ENDSL
117100090210      *
117200090210     c                   end
117300090210     c                   endIF
117400090209      **
117500110328     c                   endIF
117600110328      **
117700090210      **  Il tipo bolla non è presente
117800090210     c                   if        VABCBO = *blank
117900090210     C                   eval      vinMSG = 'TOD non rilevato'
118000130321     c**********         exsr      Error_DET
118100130321     c**********         goto      end_TOD
118200090210     c                   end
118300090210      **
118400090210     c     end_TOD       endSR
118500090210      * ?================================================================== */
118600090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
118700090210      * ?================================================================== */
118800090210     C     DATI_RFF      BEGSR
118900090210      *
119000090210     C                   clear                   WAGE             35
119100110224     C                   clear                   WFF              35
119200110224     C                   clear                   WCU              35
119300090210      *
119400090210      *  Trascodifico riferimento spedizione
119500091019     c                   if        RFF1153 = Riferimento_Spedizione  and
119600091019     c                             RFF1154 <> *BLANKS
119700090210     C                   movel(p)  RFF1154       WAGE             35
119800090210     c                   end
119900090210      **
120000090210     C                   IF         WAGE <> *BLANKS
120100090210     C                   movel     WAGE          WNSP             35
120200090210     C                   movel     WAGE          WNUM             35
120300090210     C     ' '           scan      WAGE          WLUNGH            3 0
120400090210     C                   sub       1             WLUNGH
120500090210     C                   EXSR      TSTNUM
120600090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
120700090210     C                   movel     WAGE          VABRMA
120800091019     C                   IF         WRMN = Riferimento_Spedizione  or
120900091019     C                              WRMN = *blanks
121000090210     C                   movel     WNUMOK        VABRMN
121100091019     C                   eval       WRMN = Riferimento_Spedizione
121200090210     C                   ENDIF
121300090210     C                   ENDIF
121400090210     C                   ENDIF
121500090210      *
121600090210      * Codice Cliente di riferimento
121700091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
121800090210     C                   movel     RFF1154       WFF
121900090210     c                   END
122000090211      *
122100090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
122200090211     C     WFF           IFNE      *BLANKS
122300090211     C                   eval      XX = 1
122400090211     C                   movel     §PTKSC        WCCL             35
122500090211     C                   movel     WFF           WCOM28           28
122600090211     C                   move      WCOM28        WCCL
122700090211     C     WCCL          lookup    CCL(XX)                                34
122800090211     C     *IN34         IFEQ      '1'
122900090211     C                   movel     DCL(XX)       EDIDSCL
123000090211     C                   z-add     §CLKSC        WCLKSC            7 0
123100090211     C                   movel     §CLTAR        WCLTAR            3
123200090211     C                   z-add     §CLLNP        WCLLNP            3 0
123300090211     C                   z-add     §CLNRS        WCLNRS            2 0
123400090211     C                   movel     §CLCTM        WCLCTM            2
123500090211     C                   movel     §CLBS1        WCLBS1            1
123600090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
123700090211      * imposta la LNP   x il dettaglio specifco
123800090211     c                   eval      VABlnp = §CLlnp
123900090211      * imposta la Serie x il dettaglio specifco
124000090211     c                   eval      VABnrs = §CLnrs
124100090211      *
124200090211      *  In base al cod.trattamento merce reperisco tipo tratt.
124300090211      *    per un dettaglio specifico.
124400090211     C                   CLEAR                   DS1B
124500090211     C                   Z-ADD     1             Y                 3 0
124600090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
124700090211     C     *IN32         IFEQ      '1'
124800090211     C                   MOVEL     DTM(Y)        DS1B
124900090211     C                   END
125000090211      *
125100090211      * Se il cliente non era sulla schiera lo aggiunge
125200090211     C     §CLKSC        lookup    KCL                                    39
125300090211     C                   if        *in39 = *off
125400090211     C                   add       1             WW
125500090211     C                   z-add     §CLKSC        KCL(WW)
125600090211     c                   endif
125700090211      *
125800090211     C                   ENDIF
125900090211     C                   ENDIF
126000090211     C*----------------------------------------------------------------
126100090210      *
126200090210     C                   if        RFF1153 = 'CU ' and RFF1154 <> *blanks
126300090210     C                   movel     RFF1154       WCU
126400090210     C                   END
126500090210      *
126600090210      * Numero Ordine di vendita
126700090210     C     WCU           IFNE      *BLANKS
126800090210     C                   movel     WCU           WNUM             35
126900090210     C     ' '           scan      WCU           WLUNGH
127000090210     C                   sub       1             WLUNGH
127100090210     C                   EXSR      TSTNUM
127200090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
127300090210     C                   movel     WNUMOK        VABRMN
127400090210     C                   movel     'CU '         WRMN
127500090210     C                   Endif
127600090210     C                   ENDIF
127700090210      *
127800090210      * NOn c'è l'identificativo bolla del cliente
127900091019     c                   if        (RFF1153 = Riferimento_Spedizione and
128000091019     c                              RFF1154 = *BLANKS)
128100091019     c                             or
128200090211     c                             (RFF1153 = 'CU' and RFF1154 = *BLANKS)
128300090210     C                   eval      vinMSG = 'RFF riferimento assente'
128400090213     c                   exsr      Error_DET
128500090210     c                   goto      end_RFF
128600090210     C                   ENDIF
128700090210      *
128800090210     c     end_RFF       endSR
128900090209      * ?================================================================== */
129000090213     C*?  C.O.D. Monetary Amount
129100090209      * ?================================================================== */
129200090213     C     DATI_MOA      BEGSR
129300090210      *
129400140526      *  In MAPPATURA sono previsti 2 decimali per comprendere quei clienti
129500140526      *  che inviano fuori standard il punto o la virgola decimale
129600140526      *  In questo caso occorre quindi dividere x 2 volte 100 l'importo x quelli
129700140526      *  seguono lo standard correttamente inviando un numero intero che ha al
129800140526      *  suo interno già 2 decimali. (2 decimali + 2 decimali di mappatura = 4 decimali)
129900140526      *     Per riportare correttamente sul VAB i valori occorre dividere x 10000
130000140527      *  PER
130100140527      *
130200090216     C* Controllo se campo importo numerico
130300090216      * con 3 decimali
130400090216     C     moa5004       IFNE      *zeros
130500090216      *
130600090216     C     moa5025       IFEQ      '157'
130700140527     C     moa5004       div       100000        VABIAS                         IMPORTO
130800090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
130900090216     C                   END
131000090216      *
131100090216     C     moa5025       IFEQ      '77 '                                        Valore
131200140527     C     moa5004       div       100000        VABVMD                         MERCE
131300090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
131400090216     C                   END
131500090216      *
131600090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
131700090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
131800140527     C     moa5004       div       100000        VABCAS
131900090216     C                   MOVEL     moa6345       VABVCA
132000090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
132100090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
132200090216     C     §PUTPC        IFEQ      'P'
132300090216     C     moa5004       ANDGT     *ZEROS
132400090216     C                   MOVEL     'OM'          VABTIC
132500090216     C                   MOVEL     'OM'          WVABTIC
132600090216     C                   ENDIF
132700090216     C                   END
132800090216     C                   END
132900090216      *
133000090216     C                   END                                                    Imp.non numer.
133100090213      *
133200090213     c     end_MOA       endSR
133300090213      * ?================================================================== */
133400090213     C*?  FREE TEXT
133500090213      * ?================================================================== */
133600090213     C     DATI_FTX      BEGSR
133700110404      *
133800110404      * ?Se non è un testo per il Pagamento
133900110404     C     ftx4451       IFne      'PAI'                                        -- 01 --
134000140423      *** ------
134100140423      * avviso invio MAIL
134200140423     C                   IF        ftx4451  ='EML'
134300140423      ***
134400130422      *  imposta l'indirizzo e-mail
134500140423     C                   DO        4             X
134600140423      ***
134700140423     C     D05(X)        IFNE      *BLANKS
134800140423      ***
134900140423     C     Note_1t       IFEQ      *BLANKS
135000140423     C                   MOVEL     D05(X)        Note_1t
135100140423     C                   MOVE      D05(X)        Note_2t
135200140423     C                   ELSE
135300140423     C     Note_2t       IFEQ      *BLANKS
135400140423     C                   MOVEL     D05(X)        Note_2t
135500140423     C                   END
135600140423     C                   END
135700140423      ***
135800140423     C                   ENDif
135900140423      ***
136000140423     C                   ENDdo
136100140423      ***
136200130422      * carica il campo da riportare poi sul VAT
136300140423     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
136400140423     c                             note_1T
136500140423     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
136600140423     c                             note_2T
136700140423      *
136800140423     c                   if        ftx4453 = 'N'
136900140423     c                   eval      FTX_MAIL_come_servizio = 'N'
137000140423     c                   end
137100140423      *
137200140423     C                   elseIF    ftx4451  ='SMS'
137300140423      *
137400140423     C                   DO        4             X
137500140423      *
137600140423     C     D05(X)        IFNE      *BLANKS
137700140423      *
137800140423     C     Note_1sms     IFEQ      *BLANKS
137900140423     C                   MOVEL     D05(X)        Note_1sms
138000140423     C                   MOVE      D05(X)        Note_2sms
138100140423     C                   ELSE
138200140423     C     Note_2sms     IFEQ      *BLANKS
138300140423     C                   MOVEL     D05(X)        Note_2sms
138400140423     C                   END
138500140423     C                   END
138600140423      *
138700140423     C                   ENDif
138800140423      *
138900140423     C                   ENDdo
139000140423      *
139100140423      * carica il campo da riportare poi sul VAT
139200140423     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
139300140423     c                             note_1sms
139400140423      *
139500140423     c                   if        ftx4453 = 'N'
139600140423     c                   eval      FTX_SMS_come_servizio = 'N'
139700140423     c                   end
139800140423      *** ------
139900140423     c                   else
140000140423      *** ------
140100090216      * ? solo se si tratta di note SIN ICN o PAI non altro
140200140612     C                   IF        ftx4451  ='SIN' or
140300140612     C                             ftx4451  ='DIN' or
140400140612     C                             ftx4451  ='ICN'
140500090216     C                   DO        4             X
140600090216     C     D05(X)        IFNE      *BLANKS
140700090216     C     WNOT1         IFEQ      *BLANKS
140800090216     C                   MOVEL     D05(X)        WNOT1
140900090216     C                   MOVE      D05(X)        WNOT2
141000090216     C                   ELSE
141100090216     C     WNOT2         IFEQ      *BLANKS
141200090216     C                   MOVEL     D05(X)        WNOT2
141300090216     C                   END
141400110404     C                   ENDif
141500110404     C                   ENDif
141600110404     C                   ENDdo
141700140612     C                   end
141800140423      ***
141900140423     C                   ENDif
142000140423      ***
142100130422     C                   ENDif
142200090216     C*------>>
142300090216     C*  Decodifico tipo pagamento C/Assegno
142400090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
142500090216      *      (Problema di pulizia in presenza di + righe di testo)
142600090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
142700090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
142800090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
142900090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
143000090216     C                   if        wri_vabtic = *blank
143100090216     C                   MOVEL     *BLANKS       VABTIC
143200090216     c                   end
143300090216      **
143400090216     c                   clear                   trovato_TPI       1
143500090216      *
143600090216      * ?Se c'è il tipo Incasso
143700090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
143800090216      *
143900090216     C                   DO        4             X                              -- 02 --
144000090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
144100130311      **
144200130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
144300130311      **  per gestire Partner come AZKAR che ha codice "AAA"
144400130311     C*******            MOVEL     D05(X)        WCTC              2
144500130311     C                   MOVEL     D05(X)        WCTC             10
144600090216     C                   Z-ADD     1             Y
144700090216     c                   movel(p)  Wctc          fld35a           35
144800090216     c                   move      §puTC         fld35a
144900090216     C     fld35a        LOOKUP    CTC35(Y)                               33
145000090216     C   33              MOVEL     TPI(Y)        VABTIC
145100090216     c   33              move      'S'           trovato_TPI
145200090216     C                   END                                                    -- 03 --
145300090216     C                   ENDdo                                                  -- 02 --
145400090216      *
145500090216     C                   ELSE                                                   -- 01 --
145600090216      *
145700090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
145800090216      *    Deve essere impostato comunque da testata
145900090216     c                   if        vabtic = *blank
146000090216     C                   MOVEL     wVabTIC       VABTIC
146100090216     c                   end
146200090216      *
146300090216     C                   END                                                    -- 01 --
146400090216      *
146500090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
146600090216      *    essere presenti altri record Flat con altre informazioni
146700090216      *    che potrebbero abblancare il Tipo incasso precedentemente
146800090216      *    decodificato
146900090216     c                   if        Trovato_TPI = 'S' and
147000090216     c                                 wri_vabtic = *blank
147100090216     c                   eval      wri_vabtic = 'S'
147200090216     c                   end
147300090213      *
147400140612      *----------------------------------
147500140612      * ?Consegne Particolari ai piani
147600140612     c                   if         ftx4451 = 'TCX'
147700140612     c                   movel     D05(1)        codtre            3
147800140612      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
147900140612     C                   if        CodTre = 'FLR'
148000140612     c                   eval      FTX_ai_piani = 'P'
148100140612     c                   End
148200140612     c                   End
148300140612      *----------------------------------
148400140612      *
148500090213     c     end_FTX       endSR
148600090213      * ?================================================================== */
148700090213     C*?  Anagrafica del mittente e del destinatario
148800090213      * ?================================================================== */
148900091016     C     DATI_NAD      BEGSR
149000090213      *
149100090210      * Dati destinatario
149200090210     C                   IF        nad3035 = 'CN' or
149300090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
149400090227      *
149500090227      * Se trovato un indirizzo di destinatario
149600090227     c                   eval       se_trovato_NAD ='S'
149700090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
149800090210      *
149900090210     C* Reperisco nazione corrispondente destinatario
150000090210     C                   Z-ADD     1             YY                3 0
150100090210     c                   eval      *in32 = *off
150200090210     C                   if        nad3207 <> *BLANKS
150300090210     C     nad3207       LOOKUP    ISO(YY)                                32
150400090210     C                   endif
150500090210      *
150600090210     C* Imposto dati destinatario
150700090211     C                   MOVEL     nad31241      VABRSD
150800090211     C                   if        nad31242 <> *LOVAL
150900090211     C                   MOVEL     nad31242      VABRD2
151000090210     C                   endif
151100090211      *
151200091019????  * DAL PARTY NAME
151300091019???? C* Imposto dati destinatario
151400091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
151500091019???? C                   MOVEL     nad30361      VABRSD
151600091019???? C                   if        nad30362 <> *LOVAL
151700091019???? C                   MOVEL     nad30362      VABRD2
151800091019???? C                   endif
151900091019???? C                   end
152000090211      *
152100090210     C                   MOVEL     nad30421      VABIND
152200090210     C                   MOVEL     nad3164       VABLOD
152300090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
152400090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
152500090210      *
152600090210     C* Verifico se c'è dello spazio per nella località
152700090210     C     '    '        SCAN      VABLOD        WI                4 0
152800090210      *
152900090210     C                   MOVEA     VABLOD        LOD
153000090210     C                   ADD       1             WI
153100090210     C                   MOVEA     NAD30422      LOD(WI)
153200090210     C                   MOVEA     LOD           VABLOD
153300090210     C                   ENDif
153400090210      *
153500090210     C                   If        *in32 = *on
153600090210     C                   MOVEL     C15(YY)       VABNZD
153700090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
153800090210     c                   Else
153900090210     C                   MOVEL     *BLANKS       VABNZD
154000090210     C                   Z-ADD     0             WFLCPF
154100090210     c                   Endif
154200090210      * Converto CAP
154300090210     C                   EXSR      CNVCAP
154400090603     C*
154500090603     C* Se in testata
154600090603     c                   if        Inizio_dettaglio = *blank
154700090603     c                   eval       tes_VABrsd = VABrsd
154800090603     c                   eval       tes_VABrd2 = VABrd2
154900090603     c                   eval       tes_VABind = VABind
155000090603     c                   eval       tes_VABlod = VABlod
155100090603     c                   eval       tes_VABnzd = VABnzd
155200090603     c                   eval      NAD_destinatario_in_testata = 'S'
155300090603     c                   end
155400090603     C*
155500090210     C                   ENDIF
155600090210      *
155700090210      * Dati mittente
155800091016     C                   IF        nad3035 = 'CZ'
155900090210     C                   movel     nad30361      VABRMO
156000090210      *
156100090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
156200090210     C                   movel     §PTNAZ        VABNMO
156300090210      *
156400090210     C                   IF        nad3207 <> *blanks
156500090210     C                   eval      YY = 1
156600090210     C     nad3207       LOOKUP    ISO(YY)                                32
156700090210     C                   if         *in32 = *on
156800090210     C                   MOVEL     C15(YY)       VABNMO
156900090210     C                   endif
157000090210     C                   ENDIF
157100090210      *
157200090210      * Normalizzo CAP mittente originale
157300090210     C                   clear                   T
157400090210     C                   clear                   S
157500090210     C                   MOVEA     *BLANKS       CAPM
157600090210     C                   MOVEA     nad3251       CAP9
157700090210     C                   DO        9             T
157800090210     C     CAP9(T)       IFNE      *BLANKS
157900090210     C                   ADD       1             S
158000090210     C                   MOVE      CAP9(T)       CAPM(S)
158100090210     C                   ENDIF
158200090210     C                   ENDDO
158300090210     C*
158400090210      *     Controllo validità CAP
158500090210     C                   MOVEA     CAPM          VABCMO
158600090210      *
158700090210     C                   CLEAR                   TISI95DS
158800090210     C                   MOVEL     VABCMO        I95CAP
158900090210     C                   MOVEL     VABNMO        I95NAR
159000090210     C                   MOVEL     WOGGI         I95DAT
159100090210     C                   MOVEL     '3'           I95TCN
159200090210     C                   CALL      'TISI95R'
159300090210     C                   PARM                    TISI95DS
159400090210      *     Errore
159500090210     C     O95ERR        IFNE      *BLANKS
159600090210     C                   MOVEL     INDCAE        VABCMO
159700090210     C                   MOVEL     §PTNAZ        VABNMO
159800090210     C                   END
159900090603      *
160000090603     C* Se in testata
160100090603     c                   if        Inizio_dettaglio = *blank
160200090603     c                   eval      tes_VABrmo = VABrmo
160300090603     c                   eval      tes_VABnmo = VABnmo
160400090603     c                   eval      tes_VABcmo = VABcmo
160500090603     c                   eval      NAD_mittente_in_testata = 'S'
160600090603     c                   end
160700090210      *
160800090210     C                   END
160900090210      *
161000090210      * NOn c'è l'identificativo bolla del cliente
161100090210     C                   if          vabRSD = *blank or
161200090210     C                               vabIND = *blank or
161300090210     C                               vabLOD = *blank
161400090211     C                   IF        nad3035 = 'CN'
161500090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
161600130321     c**********         exsr      Error_DET
161700130321     c**********         goto      end_NAD
161800090210     C                   ENDIF
161900090211     C                   ENDIF
162000090210      *
162100090210     c     end_NAD       endSR
162200090210      * ?================================================================== */
162300090210     C*?  Contatto a cui far riferimento
162400090210      * ?================================================================== */
162500090210     C     DATI_CTA      BEGSR
162600090210      *
162700090210      *  memorizza il riferimento di consegna x il destinatario
162800090210      *  memorizza il riferimento di consegna nr.telefonico
162900090210     C     cta3412       ifne      *BLANKS
163000090210     c                   movel     cta3412       watrde
163100090210     c                   end
163200090210      *
163300090210     c                   endSR
163400090210      * ?================================================================== */
163500090210     C*?  Contatto telefonico  a cui far riferimento
163600090210      * ?================================================================== */
163700090210     C     DATI_COM      BEGSR
163800090210      *
163900090210     C     com3148       ifne      *BLANKS
164000090210     c                   movel     com3148       wattel
164100090210     c                   end
164200090210      *
164300090209     C                   ENDSR
164400090210      * ?================================================================== */
164500090210     C*?  Dati di dettaglio della spedizione e dei colli
164600090210      * ?================================================================== */
164700090210     C     DATI_GID      BEGSR
164800090210      *
164900090210     C* Se ho totali per spedizione metto in campi VAB
165000090210     C     gid1496       IFEQ      99999
165100090210     C     gid1496       orEQ      9999
165200090210     C                   MOVEL     'S'           WGID99
165300090210     C                   z-add     gid722420     VABNCL
165400090210     C                   ELSE
165500090210     C     WGID99        IFEQ      'N'
165600090210     C                   z-add     gid722420     WTNCL             5 0
165700090210     C                   ADD       WTNCL         VABNCL
165800090210     C                   END
165900090210     C                   END
166000090212      *
166100090210     C                   ADD       VABNCL        WTOCOC           16 0
166200090210      *
166300090210     c     end_GID       endSR
166400090210      * ?================================================================== */
166500090210     C*?  Dati di dettaglio misure spedizione e colli
166600090210      * ?================================================================== */
166700090210     C     DATI_MEA      BEGSR
166800090210      *
166900090210     C*  Peso
167000090210     C     mea6311       IFEQ      'WT '
167100090210     C     mea6411       ANDEQ     'KGM'
167200090210      *
167300090210     C*  Controllo se ho valore totale o parziale
167400090210      *   sempre il Lordo
167500091019???? C     mea6313       IFEQ      'G  '
167600090210     C     WS1496        IFEQ      99999
167700090210     C     WS1496        orEQ      9999
167800091019???? C                   move      mea6314       mea6314_2        18 2          Peso
167900091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
168000090210     C                   ELSE
168100091019???? C     WGID99        IFEQ      'N'
168200091019???? C                   move      mea6314       mea6314_2                      Peso
168300091019???? C                   ADD       mea6314_2     VABPKB                         Peso
168400090210     C                   END
168500090210     C                   END
168600090210     C                   END
168700090210      *
168800091019???? C     mea6313       IFEQ      'G  '
168900091019???? C                   move      mea6314       mea6314_2                      Peso
169000091019???? C                   ADD       mea6314_2     WTOPLC
169100090210     C                   END
169200091019???? C     mea6313       IFEQ      'N  '
169300091019???? C                   move      mea6314       mea6314_2                      Peso
169400091019???? C                   ADD       mea6314_2     WTOPNC
169500090210     C                   END
169600090210     C                   END
169700090210      ***
169800090210     C*  Peso in Grammi  se abilitato a utilizzarlo
169900090210      ***
170000090210     C     §puGR         IFeq      'S'
170100090210     C     mea6311       IFEQ      'WT '
170200090210     C     mea6411       ANDEQ     '163'
170300090210      *
170400090210     C*  Controllo se ho valore totale o parziale
170500091019???? C     mea6313       IFEQ      'G  '
170600091019???? C     WS1496        IFEQ      99999
170700091019???? C     WS1496        orEQ      9999
170800091019???? c                   move      mea6314       mea6314_2
170900091019???? C     mea6314_2     div       1000          VABPKB                         Peso
171000090210     C                   ELSE
171100091019???? C     WGID99        IFEQ      'N'
171200091019???? c                   move      mea6314       mea6314_2
171300091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
171400091019???? C                   add       Peso_inKG     VABPKB                         Peso
171500090210     C                   END
171600090210     C                   END
171700090210     C                   END
171800090210      *
171900091019???? C     mea6313       IFEQ      'G  '
172000091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
172100091019???? C                   ADD       Peso_inKG     WTOPLC
172200090210     C                   END
172300091019???? C     mea6313       IFEQ      'N  '
172400091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
172500091019???? C                   ADD       Peso_inKG     WTOPNC
172600090210     C                   END
172700090210     C                   END
172800090210     C                   END
172900090210      *  Volume
173000091019???? C     mea6311       IFEQ      'VOL'
173100091019???? C     mea6411       ANDEQ     'MTQ'
173200090210      *  Controllo se ho valore totale o parziale
173300090210     C     WS1496        IFEQ      99999
173400090210     C     WS1496        orEQ      9999
173500091019???? C                   move      mea6314       mea6314_2        18 2          Volume
173600091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
173700091019???? C                   Z-ADD     mea6314_2     PVLB
173800090210     C                   ELSE
173900091019     C     WGID99        IFEQ      'N'
174000091019???? C                   move      mea6314       mea6314_2        18 2          Volume
174100091019???? C                   ADD       mea6314_2     VABVLB                         Volume
174200091019???? C                   ADD       mea6314_2     PVLB
174300090210     C                   END
174400090210     C                   END
174500090210     C                   END
174600090210      *
174700090210     C* Ricerco CAP
174800090210     C     WCAP          IFNE      *BLANKS
174900090210     C     mea6311       IFEQ      'WT '
175000090210      *
175100090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
175200090210     C                   CLEAR                   FNLV55DS
175300090210     C                   MOVEL     'P'           D55TPT
175400090210     C                   MOVEL     WOGGI         D55DRF
175500090210     C                   MOVEL     VABLNP        D55LNP
175600090210     C                   MOVEL     VABLNP        D55LIN
175700090210     C                   CALL      'FNLV55R'
175800090210     C                   PARM                    FNLV55DS
175900090210     C                   MOVE      D55TFP        COMTFP            3 0
176000090210      *
176100090210     C*  PASSO IL TERMINAL PARTENZA
176200090210     C                   CLEAR                   TISI95DS
176300090210     C                   Z-ADD     COMTFP        I95TFP
176400090210     C                   MOVEL     VABTSP        I95TSP
176500090210     C                   MOVEL     'S'           I95FRE
176600090210     C                   MOVEL     '3'           I95TCN
176700090210     C                   MOVEL     WCAP          I95CAP
176800090210     C                   MOVEL     VABNZD        I95NAR
176900090210     C                   MOVEL     VABLOD        I95LOC
177000090210     C                   MOVEL     VABIND        I95IND
177100090210      *
177200090210     C* Se gestita seconda linea di arrivo passo peso - volume
177300090210     C* e numero colli effettivo
177400090210     C     §PULNA        IFEQ      'S'
177500090210     C                   Z-ADD     VABPKB        I95LKG
177600090210     C                   Z-ADD     VABVLB        I95LMC
177700090210     C                   ELSE
177800090210     C* Altrmenti passo 1 Kg e 0,001
177900090210     C***                  Z-ADD1         I95LKG
178000090210     C***                  Z-ADD0,001     I95LMC
178100090210     C                   END
178200090210     C*
178300090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
178400090210     C                   SELECT
178500090210     C     VABCBO        WHENEQ    '1 '
178600090210     C                   MOVEL     *BLANKS       I95FCA
178700090210     C                   MOVEL     'F'           I95TPO
178800090210     C     VABCBO        WHENEQ    '2 '
178900090210     C                   MOVEL     *BLANKS       I95FCA
179000090210     C                   MOVEL     'A'           I95TPO
179100090210     C     VABCBO        WHENEQ    '4 '
179200090210     C                   MOVEL     'S'           I95FCA
179300090210     C                   MOVEL     'F'           I95TPO
179400090210     C     VABCBO        WHENEQ    '6 '
179500090210     C                   MOVEL     'S'           I95FCA
179600090210     C                   MOVEL     'A'           I95TPO
179700090210     C                   OTHER
179800090210     C                   MOVEL     'F'           I95TPO
179900090210     C     VABCAS        IFGT      0
180000090210     C                   MOVEL     'S'           I95FCA
180100090210     C                   ELSE
180200090210     C                   MOVEL     *BLANKS       I95FCA
180300090210     C                   END
180400090210     C                   ENDSL
180500090210     C*
180600090210     C                   CALL      'TISI95R'
180700090210     C                   PARM                    TISI95DS
180800090210     C* Reperisco i dati da CAP
180900090210     C                   MOVEL     O95PRV        VABPRD
181000090210     C                   MOVEL     O95LNA        VABLNA
181100090210     C                   MOVEL     O95ZNC        VABZNC
181200090210     C                   MOVEL     WCAP          VABCAD
181300090210     C*
181400090210     C     O95ERR        IFNE      *BLANKS
181500090210     C     O95FIT        ANDEQ     'S'
181600090210     C*  Cap non trovato
181700090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
181800130321     c*********          exsr      Error_DET
181900130321     C*********          goto      end_MEA
182000090210     C                   END
182100090210     C                   END
182200090210      *
182300090210     C                   ELSE
182400090210      *
182500090210     C                   CLEAR                   VABPRD
182600090210     C                   CLEAR                   VABLNA
182700090210     C                   CLEAR                   VABZNC
182800090210     C                   CLEAR                   VABCAD
182900090210     C*  Cap non trovato
183000090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
183100130321     c**********         exsr      Error_DET
183200130321     C**********         goto      end_MEA
183300090210     C                   END
183400090210      *
183500090210     c     end_MEA       endSR
183600090210      * ?================================================================== */
183700090210     C*?  Dati di dettaglio Codice a barre Segnacolli
183800090210      * ?================================================================== */
183900090210     C     DATI_PCI      BEGSR
184000090210      *
184100090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
184200090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
184300090210     C     §1BF12        IFEQ      'E'
184400090210     C                   EXSR      SEGNEE
184500090210     C                   ELSE
184600090210      *
184700090210      *  La prima volta controllo congruità numero segnacollo
184800090210     C     §PUBS1        IFNE      *BLANKS
184900090210     C     VABnrs        ANDNE     0
185000090210      *  Se il codice trattamento merce prevede che segnacollino i
185100090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
185200090210     C     §1BFG7        IFEQ      'S'
185300090210     C     §1BFG1        OREQ      'S'
185400090210      *  calcolo segnacollo
185500090210     C                   EXSR      SGNELA
185600090210     C                   END
185700090210     C                   END
185800090210      *
185900090210     C                   END
186000090210      *
186100090210     c     end_PCI       endSR
186200090211     C*----------------------------------------------------------------
186300090211      *? dati finali del dettaglio    UNT
186400090211     C*----------------------------------------------------------------
186500090211     C     DATI_UNT      BEGSR
186600090211      *
186700090213     c                   eval      NUM_RECord = vnREC
186800090213     c                   z-add     vnrec         last_RECord
186900090211      **
187000090211     c                   endSR
187100090211     C*----------------------------------------------------------------
187200090211      *? dati finali     UNT
187300090211     C*----------------------------------------------------------------
187400090211     C     DATI_UNZ      BEGSR
187500090211      *
187600090211      **
187700090211     c                   endSR
187800090210      *----------------------------------------------------------------
187900090210      *? Input segnacolli su schiera di 20 elementi
188000090210      *----------------------------------------------------------------
188100090210     C     INP_Segnacol  BEGSR
188200090210      *
188300090210     c                   clear                   quanti_PCI        3 0
188400090210     c                   if        §puEL1 = 0
188500090210     c                   z-add     10            quanti_PCI
188600090210     c                   else
188700090210     c                   z-add     §puEL1        quanti_PCI
188800090210     c                   end
188900090210      *
189000090210      *  Pulisce la schiera dei segnacolli da elaborare
189100090210     c                   clear                   x30
189200090210     c                   z-add     0             Nr_x30            3 0
189300090210      *
189400090210      *   riporto tutto sulla schiera generica X30
189500090210      *
189600090210     c                   do        10            limite            3 0
189700090210      * Limita quanti elementi passati in ogni PCI
189800090210      *   devono essere considerati
189900090210     c                   if        limite > quanti_PCI
190000090210     c                   Leave
190100090210     c                   end
190200090210      *
190300090210      *  Carica la schiera generale di tutti i segnacolli
190400090210     c                   if        D30_1(limite) <> *blank
190500090210     c                   add       1             Nr_x30
190600090210     c                   eval      X30(Nr_x30) = D30_1(limite)
190700090210     c                   end
190800090210     c                   enddo
190900090210      *
191000090210     C                   ENDSR
191100090210      *----------------------------------------------------------------
191200090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
191300090210      *----------------------------------------------------------------
191400090210     C     SEGNEE        BEGSR
191500090210      *
191600090210     c                   exsr      INP_Segnacol
191700090210      *
191800090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
191900090210     C                   DO        10            x                              --- 01 ---
192000090210      *
192100090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
192200090210     C     x30(X)        ANDNE     *LOVAL
192300090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
192400090210     C                   ADD       1             §§
192500090210     C                   MOVEL     x30(X)        SGE(§§)
192600090210     C                   END                                                    --- 03 ---
192700090210      *
192800090210     C                   END                                                    --- 02 ---
192900090210      *
193000090210     C                   ENDSR
193100090210     C*----------------------------------------------------------------
193200090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
193300090210     C*----------------------------------------------------------------
193400090210     C     SGNELA        BEGSR
193500090210      *
193600090210     c                   exsr      INP_Segnacol
193700090210     C*
193800090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
193900090210     C*  in base al numero elementi validi carico sgn.
194000090210     C                   DO        10            X                              --- 01 ---
194100090210     C*
194200090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
194300090210     C     x30(X)        ANDNE     *LOVAL
194400090210     C*
194500090210     C                   CLEAR                   DSSGN
194600090210     C*  Controllo se devo ricevere la linea di partenza
194700090210     C                   MOVEL     'N'           WERRO             1
194800090210     C     §PULP1        IFGT      0                                            --- 03 ---
194900090210     C                   EXSR      REPLNP
195000090210     C                   END                                                    --- 03 ---
195100090210     C*  Controllo se devo ricevere la linea di arrivo
195200090210     C     §PULA1        IFGT      0                                            --- 03 ---
195300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
195400090210     C                   EXSR      REPLNA
195500090210     C                   END                                                    --- 03 ---
195600090210     C*  Controllo se devo ricevere il numero di serie
195700090210     C     §PUNR1        IFGT      0                                            --- 03 ---
195800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
195900090210     C                   EXSR      REPNRS
196000090210     C                   END                                                    --- 03 ---
196100090210     C*  Controllo se devo ricevere il numero segnacollo
196200090210     C     §PUSG1        IFGT      0                                            --- 03 ---
196300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
196400090210     C                   EXSR      REPSGN
196500090210     C                   END                                                    --- 03 ---
196600090210     C*  Controllo se devo ricevere la zona di consegna
196700090210     C     §PUZN1        IFGT      0                                            --- 03 ---
196800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
196900090210     C                   EXSR      REPZNC
197000090210     C                   END                                                    --- 03 ---
197100090210     C*  Imposto da VAB i dati a zero se mi è stato passato
197200090210     C*  numero segnacollo valido
197300090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
197400090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
197500090210     C                   Z-ADD     VABLNP        SGNLNP
197600090210     C                   END                                                    --- 04 ---
197700090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
197800090210     C                   Z-ADD     VABLNA        SGNLNA
197900090210     C                   END                                                    --- 04 ---
198000090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
198100090210     C                   Z-ADD     VABNRS        SGNNRS
198200090210     C                   END                                                    --- 04 ---
198300090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
198400090210     C                   Z-ADD     VABZNC        SGNZNC
198500090210     C                   END                                                    --- 04 ---
198600090210      *
198700090210     C*  mi reimposto LNA e zona da segnacollo
198800090210     C                   Z-ADD     SGNLNA        VABLNA
198900090210     C                   Z-ADD     SGNZNC        VABZNC
199000090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
199100090210     C                   ADD       1             XY
199200090210     C                   MOVE      SGNNSC        SGN(XY)
199300090210     C                   END                                                    --- 03 ---
199400090210     C*
199500090210     C                   END                                                    --- 02 ---
199600090210     C*
199700090210     C                   END                                                    --- 01 ---
199800090210      *
199900090210     C                   ENDSR
200000090210     C*----------------------------------------------------------------
200100090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
200200090210     C*----------------------------------------------------------------
200300090210     C     REPLNP        BEGSR
200400090210     C*
200500090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
200600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
200700090210     C*  dal numero segnacollo passatomi dal partner
200800090210     C                   Z-ADD     §PULP1        WP                2 0
200900090210     C     3             SUBST     x30(X):WP     WLNP              3
201000090210     C*
201100090210     C*  controllo che la lnp calcolata sia numerica
201200090210     C                   TESTN                   WLNP                 98
201300090210     C*
201400090210     C*  se è numerica la carico
201500090210     C     *IN98         IFEQ      '1'
201600090210     C                   MOVE      WLNP          SGNLNP
201700090210     C                   ELSE
201800090210     C*  se non è numerica stampo errore
201900090210     C                   MOVEL     'S'           WERRO             1
202000090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
202100090213     c                   exsr      Error_DET
202200090210     C                   goto      end_LNP
202300090210     C                   END
202400090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
202500090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
202600090210     C     SGNLNP        IFNE      VABLNP
202700090210     C                   MOVEL     'S'           WERRO             1
202800090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
202900090210     c                             a della Bolla'
203000090213     c                   exsr      Error_DET
203100090210     C                   goto      end_LNP
203200090210     C                   END
203300090210     C*
203400090210     C     end_LNP       ENDSR
203500090210     C*----------------------------------------------------------------
203600090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
203700090210     C*----------------------------------------------------------------
203800090210     C     REPLNA        BEGSR
203900090210     C*
204000090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
204100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
204200090210     C*  dal numero segnacollo passatomi dal partner
204300090210     C                   Z-ADD     §PULA1        WP                2 0
204400090210     C     3             SUBST     x30(X):WP     WLNA              3
204500090210     C*  controllo che la lna calcolata sia numerica
204600090210     C                   TESTN                   WLNA                 98
204700090210     C*  se è numerica la carico
204800090210     C     *IN98         IFEQ      '1'
204900090210     C                   MOVE      WLNA          SGNLNA
205000090210     C                   ELSE
205100090210     C*  se non è numerica stampo errore
205200090210     C                   MOVEL     'S'           WERRO             1
205300090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
205400090213     c                   exsr      Error_DET
205500090210     C                   goto      end_LNA
205600090210     C                   END
205700090210     C*
205800090210     C     end_LNA       ENDSR
205900090210     C*----------------------------------------------------------------
206000090210     C*? REPNRS - Reperisce numero serie
206100090210     C*----------------------------------------------------------------
206200090210     C     REPNRS        BEGSR
206300090210     C*
206400090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
206500090210     C*  (a partire dalla posizione iniziale indicata in tabella)
206600090210     C*  dal numero segnacollo passatomi dal partner
206700090210     C                   Z-ADD     §PUNR1        WP                2 0
206800090210     C     2             SUBST     x30(X):WP     WNRS              2
206900090210     C*  controllo che il nrs calcolata sia numerico
207000090210     C                   TESTN                   WNRS                 98
207100090210     C*  se è numerica la carico
207200090210     C     *IN98         IFEQ      '1'
207300090210     C                   MOVE      WNRS          SGNNRS
207400090210     C                   ELSE
207500090210     C*  se non è numerico stampo errore
207600090210     C                   MOVEL     'S'           WERRO             1
207700090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
207800090213     c                   exsr      Error_DET
207900090210     C                   goto      end_NRS
208000090210     C                   END
208100090210      *
208200090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
208300090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
208400090210     C     SGNNRS        IFNE      VABNRS
208500090210     C                   MOVEL     'S'           WERRO             1
208600090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
208700090210     c                             o della Bolla'
208800090213     c                   exsr      Error_DET
208900090210     C                   goto      end_NRS
209000090210     C                   END
209100090210     C*
209200090210     C     end_NRS       ENDSR
209300090210     C*----------------------------------------------------------------
209400090210     C*? REPZNC - Reperisce zona consegna
209500090210     C*----------------------------------------------------------------
209600090210     C     REPZNC        BEGSR
209700090210     C*
209800090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
209900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
210000090210     C*  dal numero segnacollo passatomi dal partner
210100090210     C                   Z-ADD     §PUZN1        WP                2 0
210200090210     C     2             SUBST     x30(X):WP     WZNC              2
210300090210     C*  controllo che la zona calcolata sia numerica
210400090210     C                   TESTN                   WZNC                 98
210500090210     C*  se è numerica la carico
210600090210     C     *IN98         IFEQ      '1'
210700090210     C                   MOVE      WZNC          SGNZNC
210800090210     C                   ELSE
210900090210     C*  se non è numerica stampo errore
211000090210     C                   MOVEL     'S'           WERRO             1
211100090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
211200090213     c                   exsr      Error_DET
211300090210     C                   goto      end_ZNC
211400090210     C                   END
211500090210     C*
211600090210     C     end_ZNC       ENDSR
211700090210     C*----------------------------------------------------------------
211800090210     C*? REPSGN - Reperisce numero segnacollo
211900090210     C*----------------------------------------------------------------
212000090210     C     REPSGN        BEGSR
212100090210     C*
212200090210     C*  se viene passata nr.segnacollo estraggo sottostringa
212300090210     C*  lunga 7 chr (a partire dalla posizione iniziale
212400090210     C*  indicata in tabella)
212500090210     C*  dal numero segnacollo passatomi dal partner
212600090210     C                   Z-ADD     §PUSG1        WP                2 0
212700090210     C     7             SUBST     x30(X):WP     WSGN              7
212800090210     C*  controllo che il nr.segnacollo calcolato sia numerico
212900090210     C                   TESTN                   WSGN                 98
213000090210     C*  se è numerico lo carico
213100090210     C     *IN98         IFEQ      '1'
213200090210     C                   MOVE      WSGN          SGNNSC
213300090210     C                   ELSE
213400090210     C*  se non è numerica stampo errore
213500090210     C                   MOVEL     'S'           WERRO             1
213600090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
213700090213     c                   exsr      Error_DET
213800090210     C                   goto      end_SGN
213900090210     C                   END
214000090210     C*
214100090210     C     end_SGN       ENDSR
214200051205      *----------------------------------------------------*
214300051205      *   DEFINIZIONE CHIAVI                               *
214400051205      *----------------------------------------------------*
214500051205     C     *INZSR        BEGSR
214600051205      *
214700991129     c     *ENTRY        PLIST
214800060613     C                   parm                    esito             1
214900971215      *
215000050112     C     KTABel        KLIST
215100050112     C                   KFLD                    TBLKUT
215200050112     C                   KFLD                    TBLCOD
215300050112     C                   KFLD                    TBLKEY
215400090210      *
215500090210     C     K_TAB1L       KLIST
215600090213     C                   KFLD                    etbUNB
215700090213     C                   KFLD                    etbSGM
215800090213     C                   KFLD                    etbVALSGM
215900090209     C*
216000090209     C* Definisco chiavi di accesso
216100090209     C     KTAB1         KLIST
216200090209     C                   KFLD                    KKUT
216300090209     C                   KFLD                    KCOD
216400090209     C*
216500090209     C     KNUF          KLIST
216600090209     C                   KFLD                    KAAA
216700090209     C                   KFLD                    KCNU
216800090209     C                   KFLD                    KFIL
216900090209      *
217000090209     C     KVAB1         KLIST
217100090209     C                   KFLD                    KCCM
217200090209     C                   KFLD                    KCMR
217300090209     C                   KFLD                    KCNT
217400090209     C                   KFLD                    KAAS
217500090209     C                   KFLD                    KLNP
217600090209     C                   KFLD                    KNRS
217700090209     C                   KFLD                    KNSP
217800090209      *
217900090209     C     KVAB2         KLIST
218000090209     C                   KFLD                    KCCM
218100090209     C                   KFLD                    KCMR
218200090209      *
218300090209     C     KRDE          KLIST
218400090209     C                   KFLD                    KAAS
218500090209     C                   KFLD                    KLNP1
218600090209     C                   KFLD                    KNRS
218700090209     C                   KFLD                    KNSP
218800090209     C                   KFLD                    KNMC
218900090209     C                   KFLD                    KNRP
219000090209      *
219100090209     C     KACO          KLIST
219200090209     C                   KFLD                    KKUT
219300090209     C                   KFLD                    KKCC
219400090209     C                   KFLD                    KKSC
219500090209      *
219600090209     C     KIND          KLIST
219700090209     C                   KFLD                    KKUT
219800090209     C                   KFLD                    KKCC
219900090209     C                   KFLD                    KKSC
220000090209      *
220100090209      * DETERMINO SE SONO BARTOLINI O SDI
220200090209     C                   CLEAR                   TIBS55DS
220300090209     C                   MOVEL     'L'           I50TLA
220400090209     C                   CALL      'TIBS55R'
220500090209     C                   PARM                    TIBS55DS
220600090209      *
220700090209     C* RECUPERO DATI DELL'UTENTE
220800090209     C                   Z-ADD     1             CODUT             1 0
220900090209     C                   CALL      'XPARUT'
221000090209     C                   PARM                    UTEDSE0F
221100090209     C                   MOVEL     RAGUT         RSUT             20
221200090209     C*
221300090209     C* RICERCA CAPOCONTI
221400090209     C                   DO        50            X
221500090209     C                   MOVE      TCU(X)        TCUDS
221600090209     C     F56           IFEQ      'CG'
221700090209     C     F34           ANDEQ     '01'
221800090209     C                   Z-ADD     KCU(X)        KCI               4 0
221900090209     C                   END
222000090209     C                   END
222100090209     C*
222200090211     c                   movel(p)  'EDPAB'       User_Msg         10
222300090211     C*
222400090209     C* IMPOSTO A ZERO VARIABILI DI PGM
222500090209     c                   move      *blank        Snd_Msg_alert     1
222600090209     C                   MOVEL     CA(1)         WCA              78
222700090209     C                   MOVEL     CN(1)         WCN              78
222800090209     C*
222900090209     C* RECUPERO DATA E ORA
223000090209     C                   TIME                    WHHDAT           14 0
223100090209     C                   MOVE      WHHDAT        G02DAT
223200090209     C                   MOVE      WHHDAT        WDATA8            8 0
223300090209     C                   MOVE      WDATA8        WAA2              2 0
223400090209     C                   MOVEL     WDATA8        DATA              6 0
223500090209     C                   MOVE      WAA2          DATA
223600090209     C                   MOVEL     *BLANK        G02ERR
223700090209     C                   CALL      'XSRDA8'
223800090209     C                   PARM                    WLBDA8
223900090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
224000090209     C                   TIME                    WORA              6 0
224100090209      * Recupero data e ora
224200090209     C                   TIME                    W0140
224300090209      * UDATE IN GGMMAAAA
224400090209     C                   MOVE      W0140         WDTGIO
224500090209      * UDATE IN AAAAMMGG
224600090209     C     *eur          MOVEL     WDTGIO        DATA_eur
224700090209     C     *iso          MOVEL     DATA_eur      dateu
224800090209      *
224900090209      * ?              /*--------------------------- */
225000090209     c                   exsr      CARICA_TABELLE
225100090209      * ?              /*--------------------------- */
225200090209      *
225300090209      *
225400090209     C                   MOVEL     *ALL'-'       CMP132          132
225500090209     C                   CLEAR                   EDiVAB00
225600090211     C                   clear                   vablnp_PT
225700090212     C                   clear                   vabnrs_PT
225800090211     C                   clear                   vabctm_PT
225900090211     C                   clear                   VABfnsp_PT
226000090209     C                   MOVEA     *HIVAL        SGN
226100991124      *
226200050414      *------------------
226300991124     C                   ENDSR
226400060621      * ?------------------------------------------------------------------ */
226500090209      *?    CARICA TABELLE SU SCHIERE
226600060621      * ?------------------------------------------------------------------ */
226700090209     C     CARICA_TABELLEBEGSR
226800090205      *
226900090209     C* Caricamento Tabella 1B
227000090209     C                   Z-ADD     0             X                 4 0
227100090209     C                   Z-ADD     CODUT         KKUT
227200090209     C                   MOVEL     '1B'          KCOD
227300090209     C     KTAB1         CHAIN     TABEL00F                           30
227400090209     C     *IN30         DOWEQ     '0'
227500090209     C     TBLFLG        IFEQ      *BLANKS
227600090209     C                   ADD       1             X
227700090209     C                   MOVEL     TBLKEY        CTM(X)
227800090209     C                   MOVEL     TBLUNI        DTM(X)
227900090209     C                   END
228000090209     C     KTAB1         READE     TABEL00F                               30
228100090209     C                   END
228200090209      *
228300090209     C* Caricamento Tabella 15
228400090209     C                   Z-ADD     0             X                 4 0
228500090209     C                   Z-ADD     CODUT         KKUT
228600090209     C                   MOVEL     '15'          KCOD
228700090209     C     KTAB1         CHAIN     TABEL00F                           30
228800090209     C     *IN30         DOWEQ     '0'
228900090209     C     TBLFLG        IFEQ      *BLANKS
229000090209     C                   ADD       1             X
229100090209     C                   MOVEL     TBLKEY        C15(X)
229200090209     C                   MOVEL     TBLUNI        DS15
229300090209     C                   MOVEL     §15COD        ISO(X)
229400090209     C                   MOVEL     §15ECF        CPF(X)
229500090209     C                   MOVE      §15PCF        CPF(X)
229600090209     C                   END
229700090209     C     KTAB1         READE     TABEL00F                               30
229800090209     C                   END
229900090209      *
230000090209     C* Caricamento Tabella CV
230100090209     C                   Z-ADD     0             X
230200090209     C                   Z-ADD     CODUT         KKUT
230300090209     C                   MOVEL     'CV'          KCOD
230400090209     C     KTAB1         CHAIN     TABEL00F                           30
230500090209     C     *IN30         DOWEQ     '0'
230600090209     C     TBLFLG        IFEQ      *BLANKS
230700090209     C                   ADD       1             X
230800090209     C                   MOVEL     TBLKEY        CCV(X)
230900090209     C                   MOVEL     TBLUNI        DCV(X)
231000090209     C                   END
231100090209     C     KTAB1         READE     TABEL00F                               30
231200090209     C                   END
231300090216      *
231400110328      * Caricamento Tabella termini consegna
231500110328     C                   Z-ADD     0             X
231600110328     C                   MOVEL     'TB'          TABCOD
231700110328     C     TABCOD        CHAIN     EDTAB01L                           30
231800110328     C     *IN30         DOWEQ     '0'
231900110328     C     TABFLG        IFEQ      *BLANKS
232000110328     C                   ADD       1             X
232100110328     C                   eval      K35_TpBolla(X) = TABKEY
232200110328     C                   eval      Cod_TpBolla(X) = TABKEY
232300110328     C                   eval      Decod_Porto(X) = TABUNI
232400110328     C                   END
232500110328     C     TABCOD        READE     EDTAB01L                               30
232600110328     C                   END
232700110328      *
232800090216     C* Caricamento Tabella Tipo Incasso C/Assegno
232900090216     C                   Z-ADD     0             X
233000090216     C                   MOVEL     'TC'          TABCOD
233100090216     C     TABCOD        CHAIN     EDTAB01L                           30
233200090216     C     *IN30         DOWEQ     '0'
233300090216     C     TABFLG        IFEQ      *BLANKS
233400090216     C                   ADD       1             X
233500090216     C                   MOVEL     TABKEY        CTC35(X)
233600090216     C                   MOVEL     TABKEY        CTC(X)
233700090216     C                   MOVEL     TABUNI        TPI(X)
233800090216     C                   END
233900090216     C     TABCOD        READE     EDTAB01L                               30
234000090216     C                   END
234100090209      *
234200090209      * Caricamento Tabella Partner esteri
234300090209     C                   Z-ADD     0             X
234400090209     C                   MOVEL     'PT'          TABCOD
234500090209     C     TABCOD        CHAIN     EDTAB01L                           30
234600090209     C     *IN30         DOWEQ     '0'
234700090209      *
234800090209     C                   SELECT
234900090209     C     TABFLG        WHENNE    *BLANKS
235000090209      *
235100090209     C                   OTHER
235200090209     C                   ADD       1             X
235300090209     C                   MOVEL     TABKEY        CPT(X)
235400090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
235500090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
235600090209     C                   MOVEL     TABUNI        DPT(X)
235700090209     C                   ENDSL
235800090209      *
235900090209     C     TABCOD        READE     EDTAB01L                               30
236000090209     C                   END
236100121105      *
236200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
236300121105     c                   exsr      ChecK_PT
236400121105     C*
236500090209      * salva quanti Codici UNB ha caricato
236600090209     c                   z-add     x             sav_CPTx
236700090209      *
236800090209     C                   MOVEL     'PU'          TABCOD
236900090209     C     TABCOD        CHAIN     EDTAB01L                           30
237000090209     C     *IN30         DOWEQ     '0'
237100090209      *
237200090209     C                   SELECT
237300090209     C     TABFLG        WHENNE    *BLANKS
237400090209      *
237500090209     C                   OTHER
237600090209     C                   MOVEL     TABKEY        CODPU            35
237700090209     C                   Z-ADD     1             X
237800090209     C     CODPU         LOOKUP    CPT(X)                                 32
237900090209     C   32              MOVEL     TABUNI        DPU(X)
238000090209     C                   ENDSL
238100090209      *
238200090209     C     TABCOD        READE     EDTAB01L                               30
238300090209     C                   END
238400090209      *
238500090209      * Prosegue tab.PT e PU
238600090209     C                   MOVEL     'PV'          TABCOD
238700090209     C     TABCOD        CHAIN     EDTAB01L                           30
238800090209     C     *IN30         DOWEQ     '0'
238900090209      *
239000090209     C                   SELECT
239100090209     C     TABFLG        WHENNE    *BLANKS
239200090209      *
239300090209     C                   OTHER
239400090209     C                   MOVEL     TABKEY        CODPV            35
239500090209     C                   Z-ADD     1             X
239600090209     C     CODPV         LOOKUP    CPT(X)                                 32
239700090209     C   32              MOVEL     TABUNI        DPV(X)
239800090209     C                   ENDSL
239900090209      *
240000090209     C     TABCOD        READE     EDTAB01L                               30
240100090209     C                   END
240200090209      *
240300090209     C* Caricamento Tabella codici clienti - tariffe
240400090209     C                   Z-ADD     0             X
240500090209     C                   MOVEL     'CL'          TABCOD
240600090209     C     TABCOD        CHAIN     EDTAB01L                           30
240700090209     C     *IN30         DOWEQ     '0'
240800090209     C     TABFLG        IFEQ      *BLANKS
240900090209     C                   ADD       1             X
241000090209     C                   MOVEL     TABKEY        CCL(X)
241100090209     C                   MOVEL     TABUNI        DCL(X)
241200090209     C                   END
241300090209     C     TABCOD        READE     EDTAB01L                               30
241400090209     C                   END
241500090209      *
241600090209     C* Caricamento Serivizi speciali
241700090209     C                   Z-ADD     0             X
241800090209     C                   MOVEL     'SS'          TABCOD
241900090209     C     TABCOD        CHAIN     EDTAB01L                           30
242000090209     C     *IN30         DOWEQ     '0'
242100090209     C     TABFLG        IFEQ      *BLANKS
242200090209     C                   ADD       1             X
242300090209     C                   MOVEL     TABKEY        SS(X)
242400090209     C                   MOVEL     TABKEY        SS35(X)
242500090209     C                   MOVEL     TABUNI        DSS(X)
242600090209     C                   END
242700090209     C     TABCOD        READE     EDTAB01L                               30
242800090209     C                   END
242900090209      *
243000090209     C                   ENDSR
243100090209      * ?------------------------------------------------------------------ */
243200090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
243300090209      * ?------------------------------------------------------------------ */
243400090209     C     MSG_Errato    BEGSR
243500090209      *
243600090205      * ?  Scrive su tutti i records il tipo di errore
243700090205     c     *start        setll     tivin00r
243800090205     c                   read      tivin00r
243900091016     c                   if        %Eof(tivin00r)
244000091016     c                   move      'S'           EoFTivin
244100091016     c                   end
244200090205     c                   dow       not %eof(tivin00r)
244300090211     c                   if        vinMSG  = *blank
244400090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
244500090210     C                               >> Controllare i DATI su UPLOAD !!'
244600090211     c                   end
244700090205     C                   eval      vinFLG = '2'
244800090213     c                   eval      Almeno_Un_Due ='S'
244900090205     c                   eval      esito  = '2'
245000090209     c                   eval      se_errore ='S'
245100090213     c                   eval      err_bloccante ='S'
245200091016     c                   if        EoFTivin = ' '
245300090205     c                   update    tivin000
245400091016     c                   end
245500090205     c                   read      tivin00r
245600091016     c                   if        %Eof(tivin00r)
245700091016     c                   move      'S'           EoFTivin
245800091016     c                   end
245900090205     c                   endDO
246000090205      *
246100090205     C                   ENDSR
246200090213      * ?------------------------------------------------------------------ */
246300090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
246400090213      * ?------------------------------------------------------------------ */
246500090213     C     DETta_Errato  BEGSR
246600090213      *
246700090213      *  rilascia il record prima di rieseguire il ciclo
246800090213     c                   update    tivin000
246900090213      *
247000090213      * ?  Scrive su tutti i records il tipo di errore
247100090213     c     SavNUM_RECord setll     tivin00r
247200090213     c                   read      tivin00r
247300091016     c                   if        %Eof(tivin00r)
247400091016     c                   move      'S'           EoFTivin
247500091016     c                   end
247600090213     c                   dow       not %eof(tivin00r)
247700090213      *
247800090213      *
247900090213     c                   if        vinMSG  = *blank
248000090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
248100090213     C                             >> Controllare i DATI di ogni Segmento <<'
248200090213     c                   end
248300090213     c                   exsr      Error_DET
248400090213      *
248500090213     c                   if        VNREC = NUM_RECord
248600090213     c                   leave
248700090213     c                   end
248800090213      *
248900091016     c                   if        EoFTivin = ' '
249000090213     c                   update    tivin000
249100091016     c                   end
249200090213     c                   read      tivin00r
249300091016     c                   if        %Eof(tivin00r)
249400091016     c                   move      'S'           EoFTivin
249500091016     c                   end
249600090213      *
249700090213     c                   endDO
249800090603      *
249900091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
250000090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
250100091019???? C                             Bolle RHENUS non totalmente tradotto. -
250200100603     C                             Controllare UPLOAD del cliente 0895358.'
250300130321     c                   if             manda_mail='S'
250400090603     c                   exsr      invio_mail_AB
250500130321     c                   end
250600090603      *
250700090213      *
250800090213     C                   ENDSR
250900090205      * ?------------------------------------------------------------------ */
251000090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
251100090205      * ?------------------------------------------------------------------ */
251200090205     C     *pssr         BEGSR
251300090205     C
251400060710     C                   eval      esito = '2'
251500091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
251600090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
251700100603     C                             Bolle import in filiale - messaggio EDI -
251800100603     C                             ricevuto errato su UPLOAD cliente 0895358.'
251900090603     c                   exsr      invio_mail_AB
252000060621     C
252100090213     C                   ENDSR
252200090213      * ?------------------------------------------------------------------ */
252300090213      *?      Segnala Errore su TIVIN
252400090213      * ?------------------------------------------------------------------ */
252500090213     C     Error_DET     BEGSR
252600090213     C
252700090213     C                   eval      vinFLG = '2'
252800090213     c                   eval      Almeno_Un_Due ='S'
252900090213     c                   eval      esito  = '1'
253000090213     c                   eval      se_errore ='S'
253100090213     C                   eval      Err_in_detta = 'S'
253200090213     C
253300090213     C                   ENDSR
253400090209     c*==================================================================*
253500090209     C*? CNVDAT - DECODIFICA LA DATA
253600090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
253700090209     C*         - WDATA  (DATA ALFANUMERICA)
253800090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
253900090209     C*         - WHMS   (ORA NUMERICA)
254000090209     C*----------------------------------------------------------------
254100090209     C     CNVDAT        BEGSR
254200090209     C*
254300090209     C                   MOVEL     ' '           WERRO             1
254400090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
254500090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
254600090209     C                   Z-ADD     *ZEROS        WHMS              6 0
254700090209     C*
254800090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
254900090209     C                   MOVEL     WDATA         WCOMO8            8
255000090209     C                   TESTN                   WCOMO8               99
255100090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
255200090209     C  N99              MOVEL     'S'           WERRO             1
255300090209     C                   END
255400090209     C*
255500090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
255600090209     C                   MOVEL     WDATA         WCOMO14          14
255700090209     C                   TESTN                   WCOMO14              99
255800090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
255900090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
256000090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
256100090209     C  N99              MOVEL     'S'           WERRO             1
256200090209     C                   END
256300090209     C*
256400090209     C                   ENDSR
256500090210     C*----------------------------------------------------------------
256600090210     C*? TSTNUM - CONTROLLO NUMERICO
256700090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
256800090210     C*         - WLUNGH (Lunghezza campo output)
256900090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
257000090210     C*----------------------------------------------------------------
257100090210     C     TSTNUM        BEGSR
257200090210     C*
257300090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
257400090210     C                   MOVEL     'N'           WOKNUM            1
257500090210     C                   MOVEL     *BLANKS       WNUMOK           15
257600090210     C                   Z-ADD     15            IN                3 0
257700090210     C                   Z-ADD     WLUNGH        IX                3 0
257800090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
257900090210     C                   MOVEA     WNUM          NDA
258000090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
258100090210     C                   MOVEA     *ALL'0'       NDN
258200090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
258300090210     C                   Z-ADD     WLUNGH        I                 3 0
258400090210     C     I             DOWGT     0                                            --- 1 -->
258500090210     C* ESTRAE IL CARATTERE DA ESAMINARE
258600090210     C     1             SUBST     WNUM:I        WTEM01            1
258700090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
258800090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
258900090210     C     WTEM01        SCAN      WCA                                    99
259000090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
259100090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
259200090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
259300090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
259400090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
259500090210     C* DA DESTRA VERSO SINISTRA
259600090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
259700090210     C                   MOVEL     WTEMP1        NDN(IN)
259800090210     C                   SUB       1             IN
259900090210     C                   END                                                    <-- 3 ---
260000090210     C                   END                                                    <-- 2 ---
260100090210     C                   SUB       1             I
260200090210     C                   END                                                    <-- 1 ---
260300090210     C*
260400090210     C                   MOVEA     NDN           WNDN             15
260500090210     C     WNDN          IFNE      *ZEROS
260600090210     C                   MOVEA     NDN           WNUMOK           15
260700090210     C                   MOVEL     'S'           WOKNUM            1
260800090210     C                   END
260900090210     C*
261000090210     C                   ENDSR
261100090210     C*----------------------------------------------------------------
261200090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
261300090210     C*----------------------------------------------------------------
261400090210     C     CNVCAP        BEGSR
261500090210     C*
261600090210     C                   MOVEL     *BLANKS       WCAP              9
261700090210     C     nad3251       IFNE      '0'
261800090210     C     '  '          SCAN      nad3251       LENGHT            2 0
261900090210     C                   SUB       1             LENGHT
262000090210     C     LENGHT        IFLE      5
262100090210     C     LENGHT        ANDGT     0
262200090210     C                   Z-ADD     LENGHT        T                 2 0
262300090210     C                   Z-ADD     5             S                 2 0
262400090210     C                   MOVEA     nad3251       CAP9
262500090210     C                   MOVEL     *ZEROS        CAP5
262600090210     C                   DO        LENGHT
262700090210     C                   MOVE      CAP9(T)       CAP5(S)
262800090210     C                   SUB       1             S
262900090210     C                   SUB       1             T
263000090210     C                   END
263100090210     C                   MOVEA     CAP5          WCAP5             5
263200090210     C     WCAP5         IFEQ      '0'
263300090210     C                   MOVEL     *BLANKS       WCAP
263400090210     C                   ELSE
263500090210     C                   MOVEA     CAP5          WCAP
263600090210     C                   END
263700090210     C*
263800090210     C                   ELSE
263900090210     C                   MOVEL     nad3251       WCAP
264000090210     C                   END
264100090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
264200090210     C     WCAP          IFNE      *BLANKS
264300090210     C     WFLCPF        ANDNE     0
264400090210     C                   MOVEL     WFLCPF        WELE              1 0
264500090210     C                   MOVE      WFLCPF        WPOS              1 0
264600090210     C                   MOVEL     *BLANK        WCPF
264700090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
264800090210     C                   ELSE
264900090210     C                   MOVEL     *BLANKS       WCPF
265000090210     C                   END
265100090210     C                   END
265200090210     C*
265300090210     C                   ENDSR
265400090211      * ?================================================================== */
265500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
265600090211      * ?================================================================== */
265700090211     C     Scrive_VAB    BEGSR
265800090211      *
265900090211     c                   clear                   err_bloccante     1
266000090211     c                   move      'S'           Almeno_Uno
266100090211      *
266200090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
266300090211     c                   exsr      UPDVAB
266400090211      *
266500090211     C* >>>>>>>>>>>>
266600090211     C*  Richiamo pgm per scrittura immediata VAB
266700090211     C     §PUWRK        IFEQ      ' '
266800090211      *
266900090211     C*  Richiamo pgm per esecuzione stampa
267000090211     C     §PTCMR        IFEQ      'S'
267100090213     C                   MOVEL     WNRDOC        d86CMR
267200090211     C                   ELSE
267300090213     C                   MOVEL     WNRDOC        d86CMR
267400090211     C                   END
267500090211      *
267600090213     C                   MOVEL     ' '           d86RIE
267700090213     C                   MOVEL     §PUDTA        d86DTA
267800090213     C                   Z-ADD     1             d86CNT
267900090213     C                   Z-ADD     §PTKSC        d86CCM
268000090213     C                   MOVEL     *BLANKS       d86STA
268100090213     C                   MOVEL     'E'           d86MOD
268200090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
268300090211      * dove poi imposta la DS dei parametri passati
268400090213     C                   MOVEL     'LR'          d86CHI
268500090213     C                   MOVEL     'N'           d86CTL
268600090211      *
268700090211     c                   move      kpjbu         SvKpjbu
268800090211     c                   clear                   kpjbu
268900090216     C                   movel     *on           Commit_on         1
269000090213     C                   MOVEL     TRTC86ds      KPJBU
269100090213      * ?- Chiamata la TRTC86R2 -*
269200090213     C                   CALL      'TRTC86R2'
269300090211     C                   PARM                    KPJBA
269400090216     C                   PARM                    Commit_on
269500090211     c                   move      Svkpjbu       Kpjbu
269600090211      *
269700090211     C*  Controllo pgm per scrittura immediata VAB
269800090211     C     WW            IFNE      0
269900090211     C                   DO        WW            WX                3 0
270000090213     C                   Z-ADD     KCL(WX)       d86CCM
270100090211      *
270200090211     c                   move      kpjbu         SvKpjbu
270300090211     c                   clear                   kpjbu
270400090216     C                   movel     *on           Commit_on         1
270500090213     C                   MOVEL     TRTC86ds      KPJBU
270600090213      * ?- Chiamata la TRTC86R2 -*
270700090213     C                   CALL      'TRTC86R2'
270800090211     C                   PARM                    KPJBA
270900090216     C                   PARM                    Commit_on
271000090211     c                   move      Svkpjbu       Kpjbu
271100090211     C                   END
271200090211     C                   END
271300090211      *
271400090211     C                   END
271500090211      * >>>>>>>>>>>>
271600090211     C                   if        se_errore  = 'S'
271700090211     c                   move      'S'           err_bloccante
271800090211     c                   goto      Error_VAB
271900090211     c                   end
272000090211     C*
272100090211     C     Error_VAB     Endsr
272200090211     C*----------------------------------------------------------------
272300090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
272400090211     C*----------------------------------------------------------------
272500090211     C     UPDVAB        BEGSR
272600090211      *
272700090211     C*  Se non è stato impostato il codice bolla lo imposto io
272800090211     C     VABCBO        IFEQ      *BLANKS
272900090211     C     VABCAS        IFGT      0
273000090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
273100090211     C                   ELSE
273200090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
273300090211     C                   END
273400090211     C                   END
273500110922      *
273600110922      * Imposta le NOte
273700110922     C     VABNOT        IFEQ      *BLANKS
273800110922     C                   MOVEL     wnot1         VABNOT
273900110922     C                   MOVEL     wnot2         VABNT2
274000110922     C                   ELSE
274100110922     C                   MOVEL     wnot1         VABNT2
274200110922     C                   END
274300090211      *
274400090211     C*  Attribuisco anno spedizione
274500090211     C     WDATPA        IFGT      0                                            anno sped.
274600090211     C                   MOVEL     WDATPA        VABAAS
274700090211     C                   ELSE
274800090211     C     WDATFV        IFGT      0
274900090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
275000090211     C                   END
275100090211     C                   END
275200090211      *
275300090211      *  Controllo dettaglio segnacolli
275400090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
275500090211     C                   EXSR      CTRSGE
275600090211     C                   ELSE
275700090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
275800090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
275900090211     C                   EXSR      CTRSGN
276000090211     C                   END
276100090211     C                   END
276200090211      *
276300090212      *  Imposto linea partenza e serie
276400090211     C                   eval      VABLNP = VABLNP_PT
276500090212     C                   eval      VABNRS = VABnrs_PT
276600090211      *
276700090211      *  Controllo se devo variare il codice trattamento merce
276800090211     C                   eval      VABCTM = VABctm_PT
276900090211      *
277000090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
277100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
277200090211     c                   clear                   mantiene_nsp      1            *blk/'S'
277300090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
277400090211      *
277500090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
277600090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
277700090211      *
277800090211      *  Imposto codice cliente
277900090211     c                   Select
278000090211      *
278100090211      *  Specificato codice da qualificatore FF (TD15)
278200090211     C                   When        WCLKSC <> *zeros
278300090211     C                   Z-ADD     WCLKSC        VABCCM
278400090211     C                   MOVEL     WCLTAR        VABCTR
278500090211      * forza LNP/CTM/NRS
278600090211     c                   if        WclLNP <> 0
278700090211     C                   eval      VABLNP = WclLNP
278800090211     c                   end
278900090211     c                   if        WclNRS <> 0
279000090211     C                   eval      VABNRS = WclNRS
279100090211     c                   end
279200090211     c                   if        WclCTM <> *blank
279300090211     C                   eval      VABCTM = WclCTM
279400090211     c                   end
279500090211      *
279600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
279700090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
279800090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
279900090211     c                   end
280000090211      *
280100090211      *  Specificato codice da qualificatore FF (TT15)
280200090211     C                   When        $CLKSC <> *zeros
280300090211     C                   Z-ADD     $CLKSC        VABCCM
280400090211     C                   MOVEL     $CLTAR        VABCTR
280500090211      * forza LNP/CTM/NRS
280600090211     c                   if        $clLNP <> 0
280700090211     C                   eval      VABLNP = $clLNP
280800090211     c                   end
280900090211     c                   if        $clNRS <> 0
281000090211     C                   eval      VABNRS = $clNRS
281100090211     c                   end
281200090211     c                   if        $clCTM <> *blank
281300090211     C                   eval      VABCTM = $clCTM
281400090211     c                   end
281500090211      *
281600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
281700090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
281800090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
281900090211     c                   end
282000090211      *
282100090211      *  Imposto dati da tabella - PT-
282200090211     C                   Other
282300090211      *
282400090211     C                   MOVEL     §PTKSC        VABCCM
282500090211     C                   MOVEL     §PTCTR        VABCTR
282600090211      *
282700090211     C                   Endsl
282800090211      *
282900090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
283000090211      *    o dalla relativa CL
283100090211     C                   MOVEL     VABlnp        VABfgs
283200090211      *
283300090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
283400090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
283500090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
283600090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
283700090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
283800090211      *
283900090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
284000090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
284100090211     c                   else
284200090211      *
284300090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
284400090211     C*  lo attribuisco io
284500090211      **              **------------------**
284600090211     c                   exsr      repnum
284700090211      **              **------------------**
284800090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
284900090211      **              **------------------**
285000090211     c                   end
285100090211      *
285200090211     C*  Altrimenti attribuisco data del giorno
285300090211     C     VABMGS        IFEQ      0
285400090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
285500090211     C                   MOVE      WOGGI         VABMGS                         = oggi
285600090211     C                   END
285700090211     C*  Eseguo posizionamento su EDiVAB
285800090211     C                   Z-ADD     VABAAS        KAAS
285900090211     C                   Z-ADD     VABlnp        KLNP
286000090211     C                   Z-ADD     VABNRS        KNRS
286100090211     C                   Z-ADD     VABNSP        KNSP
286200090211     C                   MOVEL     SAVBOL        SALVA
286300090211     C     KVAB1         CHAIN     EDiVAB1L                           30
286400090211     C                   MOVEL     SALVA         SAVBOL
286500090211     C*  Imposto dati CMR
286600090211     C                   Z-ADD     1             VABCNT
286700090211     **
286800090211      *** Attenzione se si deve trattare il VAX con il CMR
286900090211      *** il campo VABCNT deve essere sempre impostato a (1)
287000090211     C     §puWRK        ifEQ      'S'
287100090211     C     §puNSP        andEQ     'S'
287200090211     C                   Z-ADD     1             VABCNT
287300090211     c                   end
287400090211     **
287500090211     C                   Z-ADD     WDTPRE        VABDTS
287600090212     C                   movel     '20'          VABDTS
287700090212     C     WHMPRE        mult      100           VABHMS
287800090211     C     §PTCMR        IFEQ      'S'
287900090211     C                   Z-ADD     WDTCMR        VABDCM
288000090211     C                   MOVEL     WNRCMR        VABCMR
288100090211     C                   ELSE
288200090211     C                   Z-ADD     WDATFV        VABDCM
288300090211     C                   MOVEL     WNUMFV        VABCMR
288400090211     C                   END
288500090211     C*  Se non è indicato il nome del mittente = Partner mittente
288600090211     C     VABRMO        IFEQ      *BLANKS
288700090211     C                   MOVEL     ACORAG        VABRMO
288800090211     C                   MOVEL     INDCAP        VABCMO
288900090211     C     INDCAE        IFNE      *BLANKS
289000090211     C                   MOVEL     INDCAE        VABCMO
289100090211     C                   END
289200090211     C     INDSTA        IFNE      *BLANKS
289300090211     C                   MOVEL     INDSTA        VABNMO
289400090211     C                   END
289500090211     C                   END
289600090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
289700090211     C     §1BFG1        IFEQ      'N'
289800090211     C     §1BFG7        ANDEQ     'N'
289900090211     C     §1BFG2        ANDEQ     'N'
290000090211     C***                  Z-ADD0         VABNRS
290100090211     C                   END
290200090211     C*  Imposto segancollo da - a
290300090211     C     §1BFG1        IFEQ      'S'
290400090211     C     §1BFG7        ANDEQ     'N'
290500090211     C     §1BF12        ANDNE     'E'
290600090211      *
290700090211     C     XY            IFEQ      *ZEROS
290800090211     C                   CLEAR                   VABNCD
290900090211     C                   CLEAR                   VABNCA
291000090211     C                   ELSE
291100090211     C                   Z-ADD     SGN(1)        VABNCD
291200090211     C                   Z-ADD     SGN(XY)       VABNCA
291300090211     C                   ENDIF
291400090211     C*
291500090211     C                   END
291600090211     C*
291700090211     C*  Se cap mittente originale a blank abblenco nazione
291800090211     C     VABCMO        IFEQ      *BLANKS
291900090211     C                   MOVEL     *BLANKS       VABNMO
292000090211     C                   END
292100090603     C*
292200090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
292300090603     C* Se in testata
292400090603     c                   if        NAD_destinatario_in_testata = 'S'
292500090603     c                   eval         VABrsd =  tes_VABrsd
292600090603     c                   eval         VABrd2 =  tes_VABrd2
292700090603     c                   eval         VABind =  tes_VABind
292800090603     c                   eval         VABlod =  tes_VABlod
292900090603     c                   eval         VABnzd =  tes_VABnzd
293000090603     c                   end
293100090603     C* Se in testata
293200090603     c                   if        NAD_mittente_in_testata = 'S'
293300090603     c                   eval         VABrmo = tes_VABrmo
293400090603     c                   eval         VABnmo = tes_VABnmo
293500090603     c                   eval         VABcmo = tes_VABcmo
293600090603     c                   end
293700140612      *
293800140612      *? ------------------------------------------------------------------------
293900140612      * x Consegna ai PIANI "FLOOR" codice "FLR"
294000140612     c                   if        FTX_ai_piani = 'P'
294100140612     c                   If         VABTC1 = *blank
294200140612     C                   MOVEL     'P'           VABTC1
294300140612     c                   elseIF     VABTC2 = *blank
294400140612     C                   MOVEL     'P'           VABTC2
294500140612     C                   END
294600140612     c                   End
294700090603      *
294800090211      *? ------------------------------------------------------------------------
294900090213     C   30              WRITE     EDiVAB00                             99
295000090213     C  N30              UPDATE    EDiVAB00                             99
295100090211      *? ------------------------------------------------------------------------
295200090213     c   99              eval      errori_in_Wrt = 'S'
295300090211     C*
295400090211     c                   clear                   wri_vabtic        1
295500090211     C*
295600090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
295700090211     C                   EXSR      WRTVAT_Rif
295800090211     C*
295900090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
296000090211     C     §1BF12        IFEQ      'E'
296100090211     C                   EXSR      WRTVAT
296200090211     C                   ELSE
296300090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
296400090211     C     §1BFG7        IFEQ      'S'
296500090211     C     §1BFG1        OREQ      'S'
296600090211     C                   EXSR      WRTVAD
296700090211     C                   END
296800090211     C                   END
296900090211      *
297000090211     C* Reimposto codice trattamento merce cliente
297100090211     C                   MOVEL     WSVTRM        DS1B
297200090211     C**
297300090211     C* Do errore se previsto CMR ma non c'è
297400090211     C     §PTCMR        IFNE      ' '
297500090211     C                   EXSR      MEMCMR
297600090211     C                   END
297700090211     C* Do errore se previsto AFB ma non c'è
297800090211     C     §PTNFV        IFNE      ' '
297900090211     C                   EXSR      MEMAFB
298000090211     C                   END
298100090211     C*  Se il cliente vuole il ritorno delle date di consegna
298200090211     C*  è c'è una squdratura fra i segnacolli e il numero
298300090211     C*  dei colli che ci ha passato scrivo EDSUM
298400090211     C     §PUDTA        IFEQ      'S'
298500090211     C     WSQUAD        ANDEQ     'S'
298600090211     C                   EXSR      WRTSUM
298700090211     C                   END
298800090211     C*
298900090211     C                   ENDSR
299000090211     C*----------------------------------------------------------------
299100090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
299200090211     C*----------------------------------------------------------------
299300090211     C     CTRSGN        BEGSR
299400090211     C*
299500090211     C                   MOVEL     'N'           WNSGER            1
299600090211     C*
299700090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
299800090211     C     VABNCL        IFNE      XY
299900090211     C                   MOVEL     'S'           WNSGER
300000090211     C                   MOVEL     'S'           WSQUAD
300100090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
300200090211     c                              bollettati'
300300090211     C                   GOTO      FINSGN
300400090211     C                   END
300500090211     C*  Riordino i segnacolli
300600090211     C     §1BFG7        IFEQ      'N'
300700090211     C                   SORTA     SGN
300800090211     C*  Controllo se sono sequenziali
300900090211     C                   MOVEL     'N'           WNOSEQ            1
301000090211     C     SGN(1)        ADD       1             WEXSGN            7 0
301100090211     C     2             DO        XY            X
301200090211     C     SGN(X)        IFNE      WEXSGN
301300090211     C                   MOVEL     'S'           WNOSEQ
301400090211     C                   END
301500090211     C     SGN(X)        ADD       1             WEXSGN
301600090211     C                   END
301700090211     C                   END
301800090211     C*
301900090211     C     FINSGN        ENDSR
302000090211     C*----------------------------------------------------------------
302100090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
302200090211     C*----------------------------------------------------------------
302300090211     C     CTRSGE        BEGSR
302400090211     C*
302500090211     C                   MOVEL     'N'           WNSGER            1
302600090211      *
302700090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
302800090211      *
302900090211     C     VABNCL        IFNE      §§
303000090211     C                   MOVEL     'S'           WNSGER
303100090211     C                   MOVEL     'S'           WSQUAD
303200090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
303300090211     c                              bollettati'
303400090211     C                   END
303500090211      *
303600090211     C                   ENDSR
303700090211     C*----------------------------------------------------------------
303800090211     C*? ESEGUO SCRITTURA EDiVAD
303900090211     C*----------------------------------------------------------------
304000090211     C     WRTVAD        BEGSR
304100090211     C*
304200090211     C* Se non seq. scrivo xy record
304300090211     C     §1BFG1        IFEQ      'S'
304400090211     C     XY            ANDNE     *ZEROS
304500090211     C*
304600090211     C     §1BFG7        IFEQ      'S'
304700090211     C                   DO        XY            X
304800090211     C                   Z-ADD     VABCCM        KCCM
304900090211     C                   Z-ADD     SGN(X)        KNCD
305000090211     C                   Z-ADD     VABCNT        VADCNT
305100090211     C                   MOVEL     VABCMR        VADCMR
305200090211     C                   Z-ADD     VABAAS        VADAAS
305300090211     C                   Z-ADD     VABLNP        VADLNP
305400090211     C                   Z-ADD     VABNRS        VADNRS
305500090211     C                   Z-ADD     VABNSP        VADNSP
305600090211     C                   Z-ADD     1             VADNCL
305700090211     C                   MOVEL     SGN(X)        VADCDP
305800090211     C                   Z-ADD     SGN(X)        VADNCD
305900090211     C                   Z-ADD     *ZEROS        VADNCA
306000090211     C                   Z-ADD     VABCCM        VADCCM
306100090211     C                   movel     VABfgs        VADfgs
306200090213     C                   WRITE     EDiVAD00                             99
306300090213     c   99              eval      errori_in_Wrt = 'S'
306400090211     C                   END
306500090211     C* Se sequenziali scrivo un solo record
306600090211     C                   ELSE
306700090211     C                   Z-ADD     VABAAS        VADAAS
306800090211     C                   Z-ADD     VABLNP        VADLNP
306900090211     C                   Z-ADD     VABNRS        VADNRS
307000090211     C                   Z-ADD     VABNSP        VADNSP
307100090211     C                   Z-ADD     VABNCL        VADNCL
307200090211     C                   Z-ADD     SGN(1)        VADNCD
307300090211     C                   Z-ADD     SGN(XY)       VADNCA
307400090211     C                   Z-ADD     VABCCM        VADCCM
307500090211     C                   movel     VABfgs        VADfgs
307600090213     C                   WRITE     EDiVAD00                             99
307700090213     c   99              eval      errori_in_Wrt = 'S'
307800090211     C                   END
307900090211     C*
308000090211     C                   ELSE
308100090211     C                   DO        XY            X
308200090211     C                   Z-ADD     VABCCM        KCCM
308300090211     C                   Z-ADD     SGN(X)        KNCD
308400090211     C                   Z-ADD     VABAAS        VADAAS
308500090211     C                   Z-ADD     VABLNP        VADLNP
308600090211     C                   Z-ADD     VABNRS        VADNRS
308700090211     C                   Z-ADD     VABNSP        VADNSP
308800090211     C                   MOVEL     SGN(X)        VADCDP
308900090211     C                   Z-ADD     1             VADNCL
309000090211     C                   Z-ADD     *ZEROS        VADNCD
309100090211     C                   Z-ADD     *ZEROS        VADNCA
309200090211     C                   Z-ADD     VABCCM        VADCCM
309300090211     C                   movel     VABfgs        VADfgs
309400090213     C                   WRITE     EDiVAD00                             99
309500090213     c   99              eval      errori_in_Wrt = 'S'
309600090211     C                   END
309700090211     C*
309800090211     C                   END
309900090211     C*
310000090211     C                   ENDSR
310100090211     C*----------------------------------------------------------------
310200090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
310300090211     C*----------------------------------------------------------------
310400090211     C     WRTVAT_Rif    BEGSR
310500090211      *
310600090211      *  riferimento di consegna destinatario
310700090211     c                   if        WatRDE <> *blank
310800090211     C                   Z-ADD     VABCNT        VATCNT
310900090211     C                   MOVEL     VABCMR        VATCMR
311000090211     C                   Z-ADD     VABCCM        VATCCM
311100090211     C                   movel     VABfgs        VATfgs
311200090211     C                   Z-ADD     VABLNP        VATLNP
311300090211     C                   Z-ADD     VABAAS        VATAAS
311400090211     C                   Z-ADD     VABNRS        VATNRS
311500090211     C                   Z-ADD     VABNSP        VATNSP
311600090211     C                   MOVEL     'A'           VATTRC
311700090211     C                   MOVEL     WatRDE        VATNOT
311800090213     C                   WRITE     EDiVAT00                             99
311900090213     c   99              eval      errori_in_Wrt = 'S'
312000090211     c                   end
312100090211      *
312200090211      *  riferimento di consegna telefonico
312300090211     c                   if        WatTEL <> *blank
312400090211     C                   Z-ADD     VABCNT        VATCNT
312500090211     C                   MOVEL     VABCMR        VATCMR
312600090211     C                   Z-ADD     VABCCM        VATCCM
312700090211     C                   movel     VABfgs        VATfgs
312800090211     C                   Z-ADD     VABLNP        VATLNP
312900090211     C                   Z-ADD     VABAAS        VATAAS
313000090211     C                   Z-ADD     VABNRS        VATNRS
313100090211     C                   Z-ADD     VABNSP        VATNSP
313200090211     C                   MOVEL     'B'           VATTRC
313300090211     C                   MOVEL     WatTEL        VATNOT
313400090213     C                   WRITE     EDiVAT00                             99
313500090213     c   99              eval      errori_in_Wrt = 'S'
313600090211     c                   end
313700090211      *
313800090211      * riferimento del Partner che una volta veniva riportato sul BL4
313900090211     c                   if        Wnsp   <> *blank
314000090211     C                   Z-ADD     VABCNT        VATCNT
314100090211     C                   MOVEL     VABCMR        VATCMR
314200090211     C                   Z-ADD     VABCCM        VATCCM
314300090211     C                   movel     VABfgs        VATfgs
314400090211     C                   Z-ADD     VABLNP        VATLNP
314500090211     C                   Z-ADD     VABAAS        VATAAS
314600090211     C                   Z-ADD     VABNRS        VATNRS
314700090211     C                   Z-ADD     VABNSP        VATNSP
314800090211     C                   MOVEL     'C'           VATTRC
314900090211     C                   MOVEL     Wnsp          VATNOT
315000090213     C                   WRITE     EDiVAT00                             99
315100090213     c   99              eval      errori_in_Wrt = 'S'
315200090211     c                   end
315300130422      *
315400130422      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
315500130422      *   sulla consegna della sua merce
315600130422      *
315700130422     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
315800130422     c                                  <> *blank
315900130422     C                   Z-ADD     VABCNT        VATCNT
316000130422     C                   MOVEL     VABCMR        VATCMR
316100130422     C                   Z-ADD     VABCCM        VATCCM
316200130422     C                   movel     VABfgs        VATfgs
316300130422     C                   Z-ADD     VABLNP        VATLNP
316400130422     C                   Z-ADD     VABAAS        VATAAS
316500130422     C                   Z-ADD     VABNRS        VATNRS
316600130422     C                   Z-ADD     VABNSP        VATNSP
316700130422     C                   MOVEL     'I'           VATTRC
316800130422     C                   eval       VATNOT =
316900130422     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
317000130422     C                   WRITE     EDiVAT00                             99
317100130422     c   99              eval      errori_in_Wrt = 'S'
317200130422     c                   end
317300130422      *
317400130422     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
317500130422     c                                 <> *blank
317600130422     C                   Z-ADD     VABCNT        VATCNT
317700130422     C                   MOVEL     VABCMR        VATCMR
317800130422     C                   Z-ADD     VABCCM        VATCCM
317900130422     C                   movel     VABfgs        VATfgs
318000130422     C                   Z-ADD     VABLNP        VATLNP
318100130422     C                   Z-ADD     VABAAS        VATAAS
318200130422     C                   Z-ADD     VABNRS        VATNRS
318300130422     C                   Z-ADD     VABNSP        VATNSP
318400130422     C                   MOVEL     'J'           VATTRC
318500130422     C                   eval       VATNOT =
318600130422     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
318700130422     C                   WRITE     EDiVAT00                             99
318800130422     c   99              eval      errori_in_Wrt = 'S'
318900130422     c                   end
319000090211      *
319100140423      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
319200140423      *   sulla consegna della sua merce
319300140423     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
319400140423     c                                  <> *blank
319500140423     c                             or FTX_MAIL_come_servizio = 'N'
319600140423      *
319700140423     C                   Z-ADD     VABCNT        VATCNT
319800140423     C                   MOVEL     VABCMR        VATCMR
319900140423     C                   Z-ADD     VABCCM        VATCCM
320000140423     C                   movel     VABfgs        VATfgs
320100140423     C                   Z-ADD     VABLNP        VATLNP
320200140423     C                   Z-ADD     VABAAS        VATAAS
320300140423     C                   Z-ADD     VABNRS        VATNRS
320400140423     C                   Z-ADD     VABNSP        VATNSP
320500140423     C                   MOVEL     'S'           VATTRC
320600140423      *
320700140423      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
320800140423      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
320900140423     C                   eval       VATNOT =
321000140423     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
321100140423      *
321200140423      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
321300140423      * seguenti attivano o meno il servizio x SMS o MAIL
321400140423      *
321500140423      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
321600140423      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
321700140423     C                   eval      %subst(VATNOT:17:1) = 'S'
321800140423     c                   if        FTX_SMS_come_servizio = 'N'
321900140423     c                              or
322000140423     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
322100140423     c                              = *blank
322200140423     C                   eval      %subst(VATNOT:17:1) = 'N'
322300140423     c                   end
322400140423      *
322500140423     C                   eval      %subst(VATNOT:18:1) = 'S'
322600140423     c                   if        FTX_MAIL_come_servizio = 'N'
322700140423     c                              or
322800140423     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
322900140423     c                              = *blank
323000140423     C                   eval      %subst(VATNOT:18:1) = 'N'
323100140423     c                   end
323200140423      *
323300140423     C                   WRITE     EDiVAT00                             99
323400140423     c   99              eval      errori_in_Wrt = 'S'
323500140423     c                   end
323600140423      *
323700090211     C                   ENDSR
323800090211     C*----------------------------------------------------------------
323900090211     C*? ESEGUO SCRITTURA EDiVAT
324000090211     C*----------------------------------------------------------------
324100090211     C     WRTVAT        BEGSR
324200090211      *
324300090211     C                   DO        §§            X
324400090211     C                   Z-ADD     VABCNT        VATCNT
324500090211     C                   MOVEL     VABCMR        VATCMR
324600090211     C                   Z-ADD     VABCCM        VATCCM
324700090211     C                   movel     VABfgs        VATfgs
324800090211     C                   Z-ADD     VABLNP        VATLNP
324900090211     C                   Z-ADD     VABAAS        VATAAS
325000090211     C                   Z-ADD     VABNRS        VATNRS
325100090211     C                   Z-ADD     VABNSP        VATNSP
325200090211     C                   MOVEL     'E'           VATTRC
325300090211      *
325400090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
325500090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
325600090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
325700090211     c                   if        WVATlun > 0
325800090211      *
325900090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
326000090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
326100090211      *
326200090211     c                   else
326300090211      *
326400090211      * Se non è impostato nulla prende il segnacollo passato così com'è
326500090211     C                   MOVEL     SGE(X)        VATNOT
326600090211     c                   end
326700090211      *
326800090213     C                   WRITE     EDiVAT00                             99
326900090213     c   99              eval      errori_in_Wrt = 'S'
327000090211     C                   END
327100090211      *
327200090211     C                   ENDSR
327300090211     C*----------------------------------------------------------------
327400090211     C*? ESEGUO SCRITTURA EDSUM
327500090211     C*----------------------------------------------------------------
327600090211     C     WRTSUM        BEGSR
327700090211     C*
327800090211     C* Se non seq. scrivo xy record
327900090211     C                   DO        XY            X
328000090211     C                   Z-ADD     VABAAS        SUMAAS
328100090211     C                   Z-ADD     VABLNP        SUMFLS
328200090211     C                   MOVEL     WKSC          SUMLNP
328300090211     C                   Z-ADD     VABLNA        SUMLNA
328400090211     C                   Z-ADD     VABZNC        SUMZNC
328500090211     C                   Z-ADD     VABNRS        SUMNRS
328600090211     C                   Z-ADD     VABNSP        SUMNSP
328700090211     C                   Z-ADD     SGN(X)        SUMNSC
328800090211     C                   Z-ADD     WKSC          SUMCCM
328900090211     C                   MOVEL     *BLANKS       SUMSTS
329000090211     C                   MOVEL     *BLANKS       SUMFLG
329100090211     C                   MOVEL     WPRGSP        SUMCNI
329200090211     C                   MOVEL     VABCMR        SUMCMR
329300090211     C                   Z-ADD     0             SUMNFE
329400090211     C                   Z-ADD     VABCCM        VADCCM
329500090213     C                   WRITE     EDSUM000                             99
329600090213     c   99              eval      errori_in_Wrt = 'S'
329700090211     C                   END
329800090211     C*
329900090211     C                   ENDSR
330000090211     c*==================================================================*
330100090211      * Manda un Msg in Posta AS Sede a EDP*
330200090211     c*==================================================================*
330300090211     c     SEND_MSG_EDP  begsr
330400090211      *
330500090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
330600090211      *   Lo manda una sola volta per lavoro
330700090211     c                   IF        Snd_Msg_alert  <> 'N' and
330800090211     c                             User_msg <> *blank
330900090211     c                   z-add     1             Jx
331000090211     C                   exsr      CalQEZSNDMG
331100090211     c                   end
331200090211      *
331300090211     c                   endsr
331400090213     c**********************************************************************
331500090213     c* reperimento numero Spedizione
331600090213     c**********************************************************************
331700090213     C     repnum        BEGSR
331800090213      *
331900090213      *    deve controllare sulla tabella "3C" se il numero spedizione
332000090213      *     deve essere mantenuto oppure no
332100090213     C*
332200090213     C                   clear                   Ds3C
332300090213     C                   Z-ADD     CODUT         TBLKUT
332400090213     C                   MOVEL     '3C'          TBLCOD
332500090213     C                   movel(p)  VABccm        TBLKEY
332600090213     C     KTABel        CHAIN     TABEL00F                           30
332700090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
332800090213     C                   MOVEL     TBLUNI        Ds3C
332900090213     C                   END
333000090213      *
333100090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
333200090213     c                   if        §3CFsp <> 'S'
333300090213      *
333400090213     C* NSP => Stacco un numeratore da AZNUM
333500090213     c                   movel     kpjbu         svkpjbu
333600090213     c                   clear                   kpjbu
333700090213     C                   clear                   TRUL33DS
333800090213     C                   eval      I33OPE = *zeros
333900090213     C                   eval      I33CNU = 302
334000090213     C                   eval      I33NUM = 1
334100090213     C                   movel     TRUL33DS      KPJBU
334200090213     C                   call      'TRUL33R'
334300090213     C                   parm                    KPJBA
334400090213     C                   movel     KPJBU         TRUL33DS
334500090213     c                   movel     svkpjbu       kpjbu
334600090213      ****
334700090213     C                   if        O33ERR = *zeros
334800090213     c                   z-add     o33nrf        NUM_SPED
334900090213     C                   else
335000090213     C                   endif
335100090213      ****
335200090213     c                   ELSE
335300090213      *   se si vuole mantenere il numero spedizione
335400090213      ****
335500090213     C                   MOVEL     WOGGI         WANNO             4 0
335600090213     C                   MOVE      WANNO         KAAA
335700090213     C                   Z-ADD     3             KCNU
335800090213     C                   Z-ADD     VABlnp        KFIL
335900090213     C     KNUF          CHAIN     FLNUF                              9999
336000090213     C     *IN99         IFEQ      '0'
336100090213     C                   ADD       1             NUFNUM
336200090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
336300090213     C                   UPDATE    FLNUF
336400090213     C                   ELSE
336500090213     C                   END
336600090213     C*
336700090213     c                   EndIF
336800090213      **
336900090213     C                   ENDSR
337000090213     C*----------------------------------------------------------------
337100090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
337200090213     C*----------------------------------------------------------------
337300090213     C     MEMCMR        BEGSR
337400090213     C*
337500090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
337600090213     C     §PTCM1        IFEQ      'S'
337700090213     C                   CLEAR                   EDRDE000
337800090213     C                   MOVEL     'TD1154'      KNMC
337900090213     C                   Z-ADD     1             KNRP
338000090213     C                   MOVEL     WKSC          KLNP1
338100090213     C     KRDE          CHAIN     EDRDE01L                           31
338200090213     C     *IN31         IFEQ      '1'
338300090213     C                   Z-ADD     VABAAS        RDEAAS
338400090213     C                   Z-ADD     VABLNP        RDELNP
338500090213     C                   Z-ADD     VABNRS        RDENRS
338600090213     C                   Z-ADD     VABNSP        RDENSP
338700090213     C                   MOVEL     'TD1154'      RDENMC
338800090213     C                   Z-ADD     1             RDENRP
338900090213     C                   MOVEL     WNRCMR        RDEVAL
339000090213     C                   WRITE     EDRDE000                             99
339100090213     c   99              eval      errori_in_Wrt = 'S'
339200090213     C                   END
339300090213     C*  Memorizzo qualificatore CMR
339400090213     C                   CLEAR                   EDRDE000
339500090213     C                   MOVEL     'TD1153'      KNMC
339600090213     C                   Z-ADD     1             KNRP
339700090213     C                   MOVEL     WKSC          KLNP1
339800090213     C     KRDE          CHAIN     EDRDE01L                           31
339900090213     C     *IN31         IFEQ      '1'
340000090213     C                   Z-ADD     VABAAS        RDEAAS
340100090213     C                   Z-ADD     VABLNP        RDELNP
340200090213     C                   Z-ADD     VABNRS        RDENRS
340300090213     C                   Z-ADD     VABNSP        RDENSP
340400090213     C                   Z-ADD     1             RDENRP
340500090213     C                   MOVEL     'TD1153'      RDENMC
340600090213     C                   MOVEL     'CMR'         RDEVAL
340700090213     C                   WRITE     EDRDE000                             99
340800090213     c   99              eval      errori_in_Wrt = 'S'
340900090213     C                   END
341000090213     C*  Memorizzo la data
341100090213     C                   CLEAR                   EDRDE000
341200090213     C                   MOVEL     'TD2380'      KNMC
341300090213     C                   Z-ADD     1             KNRP
341400090213     C                   MOVEL     WKSC          KLNP1
341500090213     C     KRDE          CHAIN     EDRDE01L                           31
341600090213     C     *IN31         IFEQ      '1'
341700090213     C                   Z-ADD     VABAAS        RDEAAS
341800090213     C                   Z-ADD     VABLNP        RDELNP
341900090213     C                   Z-ADD     VABNRS        RDENRS
342000090213     C                   Z-ADD     VABNSP        RDENSP
342100090213     C                   Z-ADD     1             RDENRP
342200090213     C                   MOVEL     'TD2380'      RDENMC
342300090213     C                   MOVEL     WDTCMR        RDEVAL
342400090213     C                   WRITE     EDRDE000                             99
342500090213     c   99              eval      errori_in_Wrt = 'S'
342600090213     C                   END
342700090213     C*
342800090213     C                   END                                                    §PTCM1 = 'S'
342900090213     C*
343000090213     C                   ENDSR
343100090213     C*----------------------------------------------------------------
343200090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
343300090213     C*----------------------------------------------------------------
343400090213     C     MEMAFB        BEGSR
343500090213     C*
343600090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
343700090213     C     §PTNF1        IFEQ      'S'
343800090213     C                   CLEAR                   EDRDE000
343900090213     C                   MOVEL     WKSC          KLNP1
344000090213     C                   MOVEL     'TD1154'      KNMC
344100090213     C                   Z-ADD     1             KNRP
344200090213     C     KRDE          CHAIN     EDRDE01L                           31
344300090213     C     *IN31         IFEQ      '1'
344400090213     C                   Z-ADD     VABAAS        RDEAAS
344500090213     C                   Z-ADD     VABLNP        RDELNP
344600090213     C                   Z-ADD     VABNRS        RDENRS
344700090213     C                   Z-ADD     VABNSP        RDENSP
344800090213     C                   MOVEL     'TD1154'      RDENMC
344900090213     C     §PTCM1        IFNE      'S'
345000090213     C     WNRCMR        OREQ      *BLANKS
345100090213     C                   Z-ADD     1             RDENRP
345200090213     C                   ELSE
345300090213     C                   Z-ADD     2             RDENRP
345400090213     C                   END
345500090213     C                   MOVEL     WNUMFV        RDEVAL
345600090213     C                   WRITE     EDRDE000                             99
345700090213     c   99              eval      errori_in_Wrt = 'S'
345800090213     C                   END
345900090213     C*  Memorizzo qualificatore CMR
346000090213     C                   CLEAR                   EDRDE000
346100090213     C                   MOVEL     WKSC          KLNP1
346200090213     C                   MOVEL     'TD1153'      KNMC
346300090213     C                   Z-ADD     1             KNRP
346400090213     C     KRDE          CHAIN     EDRDE01L                           31
346500090213     C     *IN31         IFEQ      '1'
346600090213     C                   Z-ADD     VABAAS        RDEAAS
346700090213     C                   Z-ADD     VABLNP        RDELNP
346800090213     C                   Z-ADD     VABNRS        RDENRS
346900090213     C                   Z-ADD     VABNSP        RDENSP
347000090213     C     §PTCM1        IFNE      'S'
347100090213     C     WNRCMR        OREQ      *BLANKS
347200090213     C                   Z-ADD     1             RDENRP
347300090213     C                   ELSE
347400090213     C                   Z-ADD     2             RDENRP
347500090213     C                   END
347600090213     C                   MOVEL     'TD1153'      RDENMC
347700090213     C                   MOVEL     'AFB'         RDEVAL
347800090213     C                   WRITE     EDRDE000                             99
347900090213     c   99              eval      errori_in_Wrt = 'S'
348000090213     C                   END
348100090213     C*  Memorizzo la data
348200090213     C                   CLEAR                   EDRDE000
348300090213     C                   MOVEL     WKSC          KLNP1
348400090213     C                   MOVEL     'TD2380'      KNMC
348500090213     C                   Z-ADD     1             KNRP
348600090213     C     KRDE          CHAIN     EDRDE01L                           31
348700090213     C     *IN31         IFEQ      '1'
348800090213     C                   Z-ADD     VABAAS        RDEAAS
348900090213     C                   Z-ADD     VABLNP        RDELNP
349000090213     C                   Z-ADD     VABNRS        RDENRS
349100090213     C                   Z-ADD     VABNSP        RDENSP
349200090213     C     §PTCM1        IFNE      'S'
349300090213     C     WNRCMR        OREQ      *BLANKS
349400090213     C                   Z-ADD     1             RDENRP
349500090213     C                   ELSE
349600090213     C                   Z-ADD     2             RDENRP
349700090213     C                   END
349800090213     C                   MOVEL     'TD2380'      RDENMC
349900090213     C                   MOVEL     WDATFV        RDEVAL
350000090213     C                   WRITE     EDRDE000                             99
350100090213     c   99              eval      errori_in_Wrt = 'S'
350200090213     C                   END
350300090213     C*
350400090213     C                   END                                                    §PTCM1 = 'S'
350500090213     C*
350600090213     C                   ENDSR
350700090213     c*==================================================================*
350800090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
350900090213     c*==================================================================*
351000090213     c     Imp_ADDR_mail begsr
351100090213      *
351200090213     c                   clear                   Indirizzi
351300090213      *
351400090213      *  Lunghezza indirizzi presenti
351500090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
351600090213     c                   z-add     1             al_char           3 0
351700090213     c                   z-add     1             dal_char          3 0
351800090213     c                   z-add     1             tot_char          3 0
351900090213      *
352000090213     c     successivo    tag
352100090213      *
352200090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
352300090213      *   e aggiunge il dominio Bartolini + il (;) separatore
352400090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
352500090213     c                   eval      tot_char = al_char - dal_char
352600090213     c                   z-add     dal_char      Dac
352700090213     c                   z-add     tot_char      Luc
352800090213      *
352900090213     c                   clear                   Indir_Bartol     40
353000090213     c                   clear                   Indir_Parz       20
353100090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
353200090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
353300090213     c                             %Trim(bart_it)
353400090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
353500090213      *
353600090213     c                   if        al_char = Lun_Ind
353700090213     c                   goto      fine_indmail
353800090213     c                   else
353900090213     c                   eval      dal_char = ( al_char + 1 )
354000090213     c                   goto      successivo
354100090213     c                   end
354200090213      *
354300090213     C     fine_indmail  TAG
354400090213      *
354500090213     C                   ENDSR
354600090213     c*==================================================================*
354700090213      * Manda un Msg x E-mail
354800090213     c*==================================================================*
354900090213     c     Invio_Mail    begsr
355000090213      *
355100090213     c                   goto      non_inviare
355200090213      *
355300090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
355400090213     C* Inizializzo variabili
355500090213     C                   movel     *blanks       wrkEml          253
355600090213     C                   movel     *blanks       wrkMsg         5000
355700090213     C                   movel     *blanks       wrkOgg           44
355800090213      *
355900090213     C* Valorizzo i campi della e-m@ail - indirizzo
356000090213     C                   eval      wrkEml = Indirizzi
356100090213     C                   eval      wrkOgg = Oggetto
356200090213     C                   eval      wrkMsg = Messaggio
356300090213
356400090213     C                   call(e)   'TIS701C'
356500090213     C                   parm                    wrkEml
356600090213     C                   parm                    wrkOgg
356700090213     C                   parm                    wrkMsg
356800090213
356900090213     c     non_inviare   tag
357000090213     C*
357100090213     C                   ENDSR
357200090603     c*==================================================================*
357300090603      * Manda un Msg x E-mail a EDPAB
357400090603     c*==================================================================*
357500090603     c     Invio_Mail_AB begsr
357600090603      *
357700090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
357800090603     C* Inizializzo variabili
357900090603     C                   movel     *blanks       wrkEml          253
358000090603     C                   movel     *blanks       wrkMsg         5000
358100090603     C                   movel     *blanks       wrkOgg           44
358200090603      *
358300090603     C* Valorizzo i campi della e-m@ail - indirizzo
358400110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
358500110203     C                   eval      wrkEml = Indirizzi
358600090603     C                   eval      wrkOgg = Oggetto
358700090603     C                   eval      wrkMsg = Messaggio
358800110203      ********
358900110203     C********           call(e)   'TIS701C'
359000110203     C********           parm                    wrkEml
359100110203     C********           parm                    wrkOgg
359200110203     C********           parm                    wrkMsg
359300110203     C*
359400110203      ** *****
359500110203     C                   call(e)   'TRTCT00R2'
359600110203     C                   parm                    wrkEml
359700110203     C                   parm                    wrkOgg
359800110203     C                   parm                    wrkMsg
359900090603     C*
360000090603     C                   ENDSR
360100090211     ***********************************************************************
360200090211     **
360300090211     ** Send Message (QEZSNDMG) API
360400090211     **
360500090211     ***********************************************************************
360600090211     C     CalQEZSNDMG   BEGSR
360700090211      *
360800090211     ** Invio messaggio all'utente.
360900090211     C                   EVAL      SndMg03 = msgDSP
361000090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
361100090211     C                   EVAL      SndMg05(Jx) = User_msg
361200090211     C                   EVAL      SndMg06 = Jx
361300090211     C                   CLEAR                   QUSEC
361400090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
361500090211
361600090211     C                   CALL      'QEZSNDMG'
361700090211     C                   PARM                    SndMg01
361800090211     C                   PARM                    SndMg02
361900090211     C                   PARM                    SndMg03
362000090211     C                   PARM                    SndMg04
362100090211     C                   PARM                    SndMg05
362200090211     C                   PARM                    SndMg06
362300090211     C                   PARM                    SndMg07
362400090211     C                   PARM                    SndMg08
362500090211     C                   PARM                    Qusec
362600090211     C                   PARM                    SndMg10
362700090211     C                   PARM                    SndMg11
362800090211     C                   PARM                    SndMg12
362900090211      *
363000090211     C                   ENDSR
363100121105      *---------------------------------------------------------------*
363200121105      * CTRL caricamento schiere    PT                               -*
363300121105      *---------------------------------------------------------------*
363400121105     C     Check_PT      begsr
363500121105     C*
363600121105     c* Controllo riempimento schiera
363700121105     c                   clear                   trul0sds
363800121105     c                   eval      i0sski='CPT'
363900121105     c                   eval      i0sele=%elem(CPT)
364000121105     c                   eval      i0spie=x
364100121105     c                   eval      i0sfile='EDTAB00F'
364200121105     c                   eval      i0ssif=knsif
364300121105     c                   eval      i0spgm='TRTCT04R'
364400121113     c                   move      kpjbu         SvKpjbu
364500121113     c                   clear                   kpjbu
364600121105     c                   movel     trul0sds      kpjbu
364700121105     c                   call      'TRUL0SR'
364800121105     c                   parm                    kpjba
364900121113     c                   eval      kpjbu  =  SvKpjbu
365000121105     C*
365100121105     C                   ENDSR
365200090211     O*****************************************************************
365300090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
365400090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
365500090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
365600090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
