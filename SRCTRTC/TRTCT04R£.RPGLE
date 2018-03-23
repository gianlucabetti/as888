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
140200090216     C                   DO        4             X
140300090216     C     D05(X)        IFNE      *BLANKS
140400090216     C     WNOT1         IFEQ      *BLANKS
140500090216     C                   MOVEL     D05(X)        WNOT1
140600090216     C                   MOVE      D05(X)        WNOT2
140700090216     C                   ELSE
140800090216     C     WNOT2         IFEQ      *BLANKS
140900090216     C                   MOVEL     D05(X)        WNOT2
141000090216     C                   END
141100110404     C                   ENDif
141200110404     C                   ENDif
141300110404     C                   ENDdo
141400140423      ***
141500140423     C                   ENDif
141600140423      ***
141700130422     C                   ENDif
141800090216     C*------>>
141900090216     C*  Decodifico tipo pagamento C/Assegno
142000090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
142100090216      *      (Problema di pulizia in presenza di + righe di testo)
142200090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
142300090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
142400090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
142500090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
142600090216     C                   if        wri_vabtic = *blank
142700090216     C                   MOVEL     *BLANKS       VABTIC
142800090216     c                   end
142900090216      **
143000090216     c                   clear                   trovato_TPI       1
143100090216      *
143200090216      * ?Se c'è il tipo Incasso
143300090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
143400090216      *
143500090216     C                   DO        4             X                              -- 02 --
143600090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
143700130311      **
143800130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
143900130311      **  per gestire Partner come AZKAR che ha codice "AAA"
144000130311     C*******            MOVEL     D05(X)        WCTC              2
144100130311     C                   MOVEL     D05(X)        WCTC             10
144200090216     C                   Z-ADD     1             Y
144300090216     c                   movel(p)  Wctc          fld35a           35
144400090216     c                   move      §puTC         fld35a
144500090216     C     fld35a        LOOKUP    CTC35(Y)                               33
144600090216     C   33              MOVEL     TPI(Y)        VABTIC
144700090216     c   33              move      'S'           trovato_TPI
144800090216     C                   END                                                    -- 03 --
144900090216     C                   ENDdo                                                  -- 02 --
145000090216      *
145100090216     C                   ELSE                                                   -- 01 --
145200090216      *
145300090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
145400090216      *    Deve essere impostato comunque da testata
145500090216     c                   if        vabtic = *blank
145600090216     C                   MOVEL     wVabTIC       VABTIC
145700090216     c                   end
145800090216      *
145900090216     C                   END                                                    -- 01 --
146000090216      *
146100090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
146200090216      *    essere presenti altri record Flat con altre informazioni
146300090216      *    che potrebbero abblancare il Tipo incasso precedentemente
146400090216      *    decodificato
146500090216     c                   if        Trovato_TPI = 'S' and
146600090216     c                                 wri_vabtic = *blank
146700090216     c                   eval      wri_vabtic = 'S'
146800090216     c                   end
146900090213      *
147000090213     c     end_FTX       endSR
147100090213      * ?================================================================== */
147200090213     C*?  Anagrafica del mittente e del destinatario
147300090213      * ?================================================================== */
147400091016     C     DATI_NAD      BEGSR
147500090213      *
147600090210      * Dati destinatario
147700090210     C                   IF        nad3035 = 'CN' or
147800090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
147900090227      *
148000090227      * Se trovato un indirizzo di destinatario
148100090227     c                   eval       se_trovato_NAD ='S'
148200090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
148300090210      *
148400090210     C* Reperisco nazione corrispondente destinatario
148500090210     C                   Z-ADD     1             YY                3 0
148600090210     c                   eval      *in32 = *off
148700090210     C                   if        nad3207 <> *BLANKS
148800090210     C     nad3207       LOOKUP    ISO(YY)                                32
148900090210     C                   endif
149000090210      *
149100090210     C* Imposto dati destinatario
149200090211     C                   MOVEL     nad31241      VABRSD
149300090211     C                   if        nad31242 <> *LOVAL
149400090211     C                   MOVEL     nad31242      VABRD2
149500090210     C                   endif
149600090211      *
149700091019????  * DAL PARTY NAME
149800091019???? C* Imposto dati destinatario
149900091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
150000091019???? C                   MOVEL     nad30361      VABRSD
150100091019???? C                   if        nad30362 <> *LOVAL
150200091019???? C                   MOVEL     nad30362      VABRD2
150300091019???? C                   endif
150400091019???? C                   end
150500090211      *
150600090210     C                   MOVEL     nad30421      VABIND
150700090210     C                   MOVEL     nad3164       VABLOD
150800090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
150900090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
151000090210      *
151100090210     C* Verifico se c'è dello spazio per nella località
151200090210     C     '    '        SCAN      VABLOD        WI                4 0
151300090210      *
151400090210     C                   MOVEA     VABLOD        LOD
151500090210     C                   ADD       1             WI
151600090210     C                   MOVEA     NAD30422      LOD(WI)
151700090210     C                   MOVEA     LOD           VABLOD
151800090210     C                   ENDif
151900090210      *
152000090210     C                   If        *in32 = *on
152100090210     C                   MOVEL     C15(YY)       VABNZD
152200090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
152300090210     c                   Else
152400090210     C                   MOVEL     *BLANKS       VABNZD
152500090210     C                   Z-ADD     0             WFLCPF
152600090210     c                   Endif
152700090210      * Converto CAP
152800090210     C                   EXSR      CNVCAP
152900090603     C*
153000090603     C* Se in testata
153100090603     c                   if        Inizio_dettaglio = *blank
153200090603     c                   eval       tes_VABrsd = VABrsd
153300090603     c                   eval       tes_VABrd2 = VABrd2
153400090603     c                   eval       tes_VABind = VABind
153500090603     c                   eval       tes_VABlod = VABlod
153600090603     c                   eval       tes_VABnzd = VABnzd
153700090603     c                   eval      NAD_destinatario_in_testata = 'S'
153800090603     c                   end
153900090603     C*
154000090210     C                   ENDIF
154100090210      *
154200090210      * Dati mittente
154300091016     C                   IF        nad3035 = 'CZ'
154400090210     C                   movel     nad30361      VABRMO
154500090210      *
154600090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
154700090210     C                   movel     §PTNAZ        VABNMO
154800090210      *
154900090210     C                   IF        nad3207 <> *blanks
155000090210     C                   eval      YY = 1
155100090210     C     nad3207       LOOKUP    ISO(YY)                                32
155200090210     C                   if         *in32 = *on
155300090210     C                   MOVEL     C15(YY)       VABNMO
155400090210     C                   endif
155500090210     C                   ENDIF
155600090210      *
155700090210      * Normalizzo CAP mittente originale
155800090210     C                   clear                   T
155900090210     C                   clear                   S
156000090210     C                   MOVEA     *BLANKS       CAPM
156100090210     C                   MOVEA     nad3251       CAP9
156200090210     C                   DO        9             T
156300090210     C     CAP9(T)       IFNE      *BLANKS
156400090210     C                   ADD       1             S
156500090210     C                   MOVE      CAP9(T)       CAPM(S)
156600090210     C                   ENDIF
156700090210     C                   ENDDO
156800090210     C*
156900090210      *     Controllo validità CAP
157000090210     C                   MOVEA     CAPM          VABCMO
157100090210      *
157200090210     C                   CLEAR                   TISI95DS
157300090210     C                   MOVEL     VABCMO        I95CAP
157400090210     C                   MOVEL     VABNMO        I95NAR
157500090210     C                   MOVEL     WOGGI         I95DAT
157600090210     C                   MOVEL     '3'           I95TCN
157700090210     C                   CALL      'TISI95R'
157800090210     C                   PARM                    TISI95DS
157900090210      *     Errore
158000090210     C     O95ERR        IFNE      *BLANKS
158100090210     C                   MOVEL     INDCAE        VABCMO
158200090210     C                   MOVEL     §PTNAZ        VABNMO
158300090210     C                   END
158400090603      *
158500090603     C* Se in testata
158600090603     c                   if        Inizio_dettaglio = *blank
158700090603     c                   eval      tes_VABrmo = VABrmo
158800090603     c                   eval      tes_VABnmo = VABnmo
158900090603     c                   eval      tes_VABcmo = VABcmo
159000090603     c                   eval      NAD_mittente_in_testata = 'S'
159100090603     c                   end
159200090210      *
159300090210     C                   END
159400090210      *
159500090210      * NOn c'è l'identificativo bolla del cliente
159600090210     C                   if          vabRSD = *blank or
159700090210     C                               vabIND = *blank or
159800090210     C                               vabLOD = *blank
159900090211     C                   IF        nad3035 = 'CN'
160000090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
160100130321     c**********         exsr      Error_DET
160200130321     c**********         goto      end_NAD
160300090210     C                   ENDIF
160400090211     C                   ENDIF
160500090210      *
160600090210     c     end_NAD       endSR
160700090210      * ?================================================================== */
160800090210     C*?  Contatto a cui far riferimento
160900090210      * ?================================================================== */
161000090210     C     DATI_CTA      BEGSR
161100090210      *
161200090210      *  memorizza il riferimento di consegna x il destinatario
161300090210      *  memorizza il riferimento di consegna nr.telefonico
161400090210     C     cta3412       ifne      *BLANKS
161500090210     c                   movel     cta3412       watrde
161600090210     c                   end
161700090210      *
161800090210     c                   endSR
161900090210      * ?================================================================== */
162000090210     C*?  Contatto telefonico  a cui far riferimento
162100090210      * ?================================================================== */
162200090210     C     DATI_COM      BEGSR
162300090210      *
162400090210     C     com3148       ifne      *BLANKS
162500090210     c                   movel     com3148       wattel
162600090210     c                   end
162700090210      *
162800090209     C                   ENDSR
162900090210      * ?================================================================== */
163000090210     C*?  Dati di dettaglio della spedizione e dei colli
163100090210      * ?================================================================== */
163200090210     C     DATI_GID      BEGSR
163300090210      *
163400090210     C* Se ho totali per spedizione metto in campi VAB
163500090210     C     gid1496       IFEQ      99999
163600090210     C     gid1496       orEQ      9999
163700090210     C                   MOVEL     'S'           WGID99
163800090210     C                   z-add     gid722420     VABNCL
163900090210     C                   ELSE
164000090210     C     WGID99        IFEQ      'N'
164100090210     C                   z-add     gid722420     WTNCL             5 0
164200090210     C                   ADD       WTNCL         VABNCL
164300090210     C                   END
164400090210     C                   END
164500090212      *
164600090210     C                   ADD       VABNCL        WTOCOC           16 0
164700090210      *
164800090210     c     end_GID       endSR
164900090210      * ?================================================================== */
165000090210     C*?  Dati di dettaglio misure spedizione e colli
165100090210      * ?================================================================== */
165200090210     C     DATI_MEA      BEGSR
165300090210      *
165400090210     C*  Peso
165500090210     C     mea6311       IFEQ      'WT '
165600090210     C     mea6411       ANDEQ     'KGM'
165700090210      *
165800090210     C*  Controllo se ho valore totale o parziale
165900090210      *   sempre il Lordo
166000091019???? C     mea6313       IFEQ      'G  '
166100090210     C     WS1496        IFEQ      99999
166200090210     C     WS1496        orEQ      9999
166300091019???? C                   move      mea6314       mea6314_2        18 2          Peso
166400091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
166500090210     C                   ELSE
166600091019???? C     WGID99        IFEQ      'N'
166700091019???? C                   move      mea6314       mea6314_2                      Peso
166800091019???? C                   ADD       mea6314_2     VABPKB                         Peso
166900090210     C                   END
167000090210     C                   END
167100090210     C                   END
167200090210      *
167300091019???? C     mea6313       IFEQ      'G  '
167400091019???? C                   move      mea6314       mea6314_2                      Peso
167500091019???? C                   ADD       mea6314_2     WTOPLC
167600090210     C                   END
167700091019???? C     mea6313       IFEQ      'N  '
167800091019???? C                   move      mea6314       mea6314_2                      Peso
167900091019???? C                   ADD       mea6314_2     WTOPNC
168000090210     C                   END
168100090210     C                   END
168200090210      ***
168300090210     C*  Peso in Grammi  se abilitato a utilizzarlo
168400090210      ***
168500090210     C     §puGR         IFeq      'S'
168600090210     C     mea6311       IFEQ      'WT '
168700090210     C     mea6411       ANDEQ     '163'
168800090210      *
168900090210     C*  Controllo se ho valore totale o parziale
169000091019???? C     mea6313       IFEQ      'G  '
169100091019???? C     WS1496        IFEQ      99999
169200091019???? C     WS1496        orEQ      9999
169300091019???? c                   move      mea6314       mea6314_2
169400091019???? C     mea6314_2     div       1000          VABPKB                         Peso
169500090210     C                   ELSE
169600091019???? C     WGID99        IFEQ      'N'
169700091019???? c                   move      mea6314       mea6314_2
169800091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
169900091019???? C                   add       Peso_inKG     VABPKB                         Peso
170000090210     C                   END
170100090210     C                   END
170200090210     C                   END
170300090210      *
170400091019???? C     mea6313       IFEQ      'G  '
170500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
170600091019???? C                   ADD       Peso_inKG     WTOPLC
170700090210     C                   END
170800091019???? C     mea6313       IFEQ      'N  '
170900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
171000091019???? C                   ADD       Peso_inKG     WTOPNC
171100090210     C                   END
171200090210     C                   END
171300090210     C                   END
171400090210      *  Volume
171500091019???? C     mea6311       IFEQ      'VOL'
171600091019???? C     mea6411       ANDEQ     'MTQ'
171700090210      *  Controllo se ho valore totale o parziale
171800090210     C     WS1496        IFEQ      99999
171900090210     C     WS1496        orEQ      9999
172000091019???? C                   move      mea6314       mea6314_2        18 2          Volume
172100091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
172200091019???? C                   Z-ADD     mea6314_2     PVLB
172300090210     C                   ELSE
172400091019     C     WGID99        IFEQ      'N'
172500091019???? C                   move      mea6314       mea6314_2        18 2          Volume
172600091019???? C                   ADD       mea6314_2     VABVLB                         Volume
172700091019???? C                   ADD       mea6314_2     PVLB
172800090210     C                   END
172900090210     C                   END
173000090210     C                   END
173100090210      *
173200090210     C* Ricerco CAP
173300090210     C     WCAP          IFNE      *BLANKS
173400090210     C     mea6311       IFEQ      'WT '
173500090210      *
173600090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
173700090210     C                   CLEAR                   FNLV55DS
173800090210     C                   MOVEL     'P'           D55TPT
173900090210     C                   MOVEL     WOGGI         D55DRF
174000090210     C                   MOVEL     VABLNP        D55LNP
174100090210     C                   MOVEL     VABLNP        D55LIN
174200090210     C                   CALL      'FNLV55R'
174300090210     C                   PARM                    FNLV55DS
174400090210     C                   MOVE      D55TFP        COMTFP            3 0
174500090210      *
174600090210     C*  PASSO IL TERMINAL PARTENZA
174700090210     C                   CLEAR                   TISI95DS
174800090210     C                   Z-ADD     COMTFP        I95TFP
174900090210     C                   MOVEL     VABTSP        I95TSP
175000090210     C                   MOVEL     'S'           I95FRE
175100090210     C                   MOVEL     '3'           I95TCN
175200090210     C                   MOVEL     WCAP          I95CAP
175300090210     C                   MOVEL     VABNZD        I95NAR
175400090210     C                   MOVEL     VABLOD        I95LOC
175500090210     C                   MOVEL     VABIND        I95IND
175600090210      *
175700090210     C* Se gestita seconda linea di arrivo passo peso - volume
175800090210     C* e numero colli effettivo
175900090210     C     §PULNA        IFEQ      'S'
176000090210     C                   Z-ADD     VABPKB        I95LKG
176100090210     C                   Z-ADD     VABVLB        I95LMC
176200090210     C                   ELSE
176300090210     C* Altrmenti passo 1 Kg e 0,001
176400090210     C***                  Z-ADD1         I95LKG
176500090210     C***                  Z-ADD0,001     I95LMC
176600090210     C                   END
176700090210     C*
176800090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
176900090210     C                   SELECT
177000090210     C     VABCBO        WHENEQ    '1 '
177100090210     C                   MOVEL     *BLANKS       I95FCA
177200090210     C                   MOVEL     'F'           I95TPO
177300090210     C     VABCBO        WHENEQ    '2 '
177400090210     C                   MOVEL     *BLANKS       I95FCA
177500090210     C                   MOVEL     'A'           I95TPO
177600090210     C     VABCBO        WHENEQ    '4 '
177700090210     C                   MOVEL     'S'           I95FCA
177800090210     C                   MOVEL     'F'           I95TPO
177900090210     C     VABCBO        WHENEQ    '6 '
178000090210     C                   MOVEL     'S'           I95FCA
178100090210     C                   MOVEL     'A'           I95TPO
178200090210     C                   OTHER
178300090210     C                   MOVEL     'F'           I95TPO
178400090210     C     VABCAS        IFGT      0
178500090210     C                   MOVEL     'S'           I95FCA
178600090210     C                   ELSE
178700090210     C                   MOVEL     *BLANKS       I95FCA
178800090210     C                   END
178900090210     C                   ENDSL
179000090210     C*
179100090210     C                   CALL      'TISI95R'
179200090210     C                   PARM                    TISI95DS
179300090210     C* Reperisco i dati da CAP
179400090210     C                   MOVEL     O95PRV        VABPRD
179500090210     C                   MOVEL     O95LNA        VABLNA
179600090210     C                   MOVEL     O95ZNC        VABZNC
179700090210     C                   MOVEL     WCAP          VABCAD
179800090210     C*
179900090210     C     O95ERR        IFNE      *BLANKS
180000090210     C     O95FIT        ANDEQ     'S'
180100090210     C*  Cap non trovato
180200090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
180300130321     c*********          exsr      Error_DET
180400130321     C*********          goto      end_MEA
180500090210     C                   END
180600090210     C                   END
180700090210      *
180800090210     C                   ELSE
180900090210      *
181000090210     C                   CLEAR                   VABPRD
181100090210     C                   CLEAR                   VABLNA
181200090210     C                   CLEAR                   VABZNC
181300090210     C                   CLEAR                   VABCAD
181400090210     C*  Cap non trovato
181500090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
181600130321     c**********         exsr      Error_DET
181700130321     C**********         goto      end_MEA
181800090210     C                   END
181900090210      *
182000090210     c     end_MEA       endSR
182100090210      * ?================================================================== */
182200090210     C*?  Dati di dettaglio Codice a barre Segnacolli
182300090210      * ?================================================================== */
182400090210     C     DATI_PCI      BEGSR
182500090210      *
182600090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
182700090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
182800090210     C     §1BF12        IFEQ      'E'
182900090210     C                   EXSR      SEGNEE
183000090210     C                   ELSE
183100090210      *
183200090210      *  La prima volta controllo congruità numero segnacollo
183300090210     C     §PUBS1        IFNE      *BLANKS
183400090210     C     VABnrs        ANDNE     0
183500090210      *  Se il codice trattamento merce prevede che segnacollino i
183600090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
183700090210     C     §1BFG7        IFEQ      'S'
183800090210     C     §1BFG1        OREQ      'S'
183900090210      *  calcolo segnacollo
184000090210     C                   EXSR      SGNELA
184100090210     C                   END
184200090210     C                   END
184300090210      *
184400090210     C                   END
184500090210      *
184600090210     c     end_PCI       endSR
184700090211     C*----------------------------------------------------------------
184800090211      *? dati finali del dettaglio    UNT
184900090211     C*----------------------------------------------------------------
185000090211     C     DATI_UNT      BEGSR
185100090211      *
185200090213     c                   eval      NUM_RECord = vnREC
185300090213     c                   z-add     vnrec         last_RECord
185400090211      **
185500090211     c                   endSR
185600090211     C*----------------------------------------------------------------
185700090211      *? dati finali     UNT
185800090211     C*----------------------------------------------------------------
185900090211     C     DATI_UNZ      BEGSR
186000090211      *
186100090211      **
186200090211     c                   endSR
186300090210      *----------------------------------------------------------------
186400090210      *? Input segnacolli su schiera di 20 elementi
186500090210      *----------------------------------------------------------------
186600090210     C     INP_Segnacol  BEGSR
186700090210      *
186800090210     c                   clear                   quanti_PCI        3 0
186900090210     c                   if        §puEL1 = 0
187000090210     c                   z-add     10            quanti_PCI
187100090210     c                   else
187200090210     c                   z-add     §puEL1        quanti_PCI
187300090210     c                   end
187400090210      *
187500090210      *  Pulisce la schiera dei segnacolli da elaborare
187600090210     c                   clear                   x30
187700090210     c                   z-add     0             Nr_x30            3 0
187800090210      *
187900090210      *   riporto tutto sulla schiera generica X30
188000090210      *
188100090210     c                   do        10            limite            3 0
188200090210      * Limita quanti elementi passati in ogni PCI
188300090210      *   devono essere considerati
188400090210     c                   if        limite > quanti_PCI
188500090210     c                   Leave
188600090210     c                   end
188700090210      *
188800090210      *  Carica la schiera generale di tutti i segnacolli
188900090210     c                   if        D30_1(limite) <> *blank
189000090210     c                   add       1             Nr_x30
189100090210     c                   eval      X30(Nr_x30) = D30_1(limite)
189200090210     c                   end
189300090210     c                   enddo
189400090210      *
189500090210     C                   ENDSR
189600090210      *----------------------------------------------------------------
189700090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
189800090210      *----------------------------------------------------------------
189900090210     C     SEGNEE        BEGSR
190000090210      *
190100090210     c                   exsr      INP_Segnacol
190200090210      *
190300090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
190400090210     C                   DO        10            x                              --- 01 ---
190500090210      *
190600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
190700090210     C     x30(X)        ANDNE     *LOVAL
190800090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
190900090210     C                   ADD       1             §§
191000090210     C                   MOVEL     x30(X)        SGE(§§)
191100090210     C                   END                                                    --- 03 ---
191200090210      *
191300090210     C                   END                                                    --- 02 ---
191400090210      *
191500090210     C                   ENDSR
191600090210     C*----------------------------------------------------------------
191700090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
191800090210     C*----------------------------------------------------------------
191900090210     C     SGNELA        BEGSR
192000090210      *
192100090210     c                   exsr      INP_Segnacol
192200090210     C*
192300090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
192400090210     C*  in base al numero elementi validi carico sgn.
192500090210     C                   DO        10            X                              --- 01 ---
192600090210     C*
192700090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
192800090210     C     x30(X)        ANDNE     *LOVAL
192900090210     C*
193000090210     C                   CLEAR                   DSSGN
193100090210     C*  Controllo se devo ricevere la linea di partenza
193200090210     C                   MOVEL     'N'           WERRO             1
193300090210     C     §PULP1        IFGT      0                                            --- 03 ---
193400090210     C                   EXSR      REPLNP
193500090210     C                   END                                                    --- 03 ---
193600090210     C*  Controllo se devo ricevere la linea di arrivo
193700090210     C     §PULA1        IFGT      0                                            --- 03 ---
193800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
193900090210     C                   EXSR      REPLNA
194000090210     C                   END                                                    --- 03 ---
194100090210     C*  Controllo se devo ricevere il numero di serie
194200090210     C     §PUNR1        IFGT      0                                            --- 03 ---
194300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
194400090210     C                   EXSR      REPNRS
194500090210     C                   END                                                    --- 03 ---
194600090210     C*  Controllo se devo ricevere il numero segnacollo
194700090210     C     §PUSG1        IFGT      0                                            --- 03 ---
194800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
194900090210     C                   EXSR      REPSGN
195000090210     C                   END                                                    --- 03 ---
195100090210     C*  Controllo se devo ricevere la zona di consegna
195200090210     C     §PUZN1        IFGT      0                                            --- 03 ---
195300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
195400090210     C                   EXSR      REPZNC
195500090210     C                   END                                                    --- 03 ---
195600090210     C*  Imposto da VAB i dati a zero se mi è stato passato
195700090210     C*  numero segnacollo valido
195800090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
195900090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
196000090210     C                   Z-ADD     VABLNP        SGNLNP
196100090210     C                   END                                                    --- 04 ---
196200090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
196300090210     C                   Z-ADD     VABLNA        SGNLNA
196400090210     C                   END                                                    --- 04 ---
196500090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
196600090210     C                   Z-ADD     VABNRS        SGNNRS
196700090210     C                   END                                                    --- 04 ---
196800090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
196900090210     C                   Z-ADD     VABZNC        SGNZNC
197000090210     C                   END                                                    --- 04 ---
197100090210      *
197200090210     C*  mi reimposto LNA e zona da segnacollo
197300090210     C                   Z-ADD     SGNLNA        VABLNA
197400090210     C                   Z-ADD     SGNZNC        VABZNC
197500090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
197600090210     C                   ADD       1             XY
197700090210     C                   MOVE      SGNNSC        SGN(XY)
197800090210     C                   END                                                    --- 03 ---
197900090210     C*
198000090210     C                   END                                                    --- 02 ---
198100090210     C*
198200090210     C                   END                                                    --- 01 ---
198300090210      *
198400090210     C                   ENDSR
198500090210     C*----------------------------------------------------------------
198600090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
198700090210     C*----------------------------------------------------------------
198800090210     C     REPLNP        BEGSR
198900090210     C*
199000090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
199100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
199200090210     C*  dal numero segnacollo passatomi dal partner
199300090210     C                   Z-ADD     §PULP1        WP                2 0
199400090210     C     3             SUBST     x30(X):WP     WLNP              3
199500090210     C*
199600090210     C*  controllo che la lnp calcolata sia numerica
199700090210     C                   TESTN                   WLNP                 98
199800090210     C*
199900090210     C*  se è numerica la carico
200000090210     C     *IN98         IFEQ      '1'
200100090210     C                   MOVE      WLNP          SGNLNP
200200090210     C                   ELSE
200300090210     C*  se non è numerica stampo errore
200400090210     C                   MOVEL     'S'           WERRO             1
200500090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
200600090213     c                   exsr      Error_DET
200700090210     C                   goto      end_LNP
200800090210     C                   END
200900090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
201000090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
201100090210     C     SGNLNP        IFNE      VABLNP
201200090210     C                   MOVEL     'S'           WERRO             1
201300090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
201400090210     c                             a della Bolla'
201500090213     c                   exsr      Error_DET
201600090210     C                   goto      end_LNP
201700090210     C                   END
201800090210     C*
201900090210     C     end_LNP       ENDSR
202000090210     C*----------------------------------------------------------------
202100090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
202200090210     C*----------------------------------------------------------------
202300090210     C     REPLNA        BEGSR
202400090210     C*
202500090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
202600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
202700090210     C*  dal numero segnacollo passatomi dal partner
202800090210     C                   Z-ADD     §PULA1        WP                2 0
202900090210     C     3             SUBST     x30(X):WP     WLNA              3
203000090210     C*  controllo che la lna calcolata sia numerica
203100090210     C                   TESTN                   WLNA                 98
203200090210     C*  se è numerica la carico
203300090210     C     *IN98         IFEQ      '1'
203400090210     C                   MOVE      WLNA          SGNLNA
203500090210     C                   ELSE
203600090210     C*  se non è numerica stampo errore
203700090210     C                   MOVEL     'S'           WERRO             1
203800090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
203900090213     c                   exsr      Error_DET
204000090210     C                   goto      end_LNA
204100090210     C                   END
204200090210     C*
204300090210     C     end_LNA       ENDSR
204400090210     C*----------------------------------------------------------------
204500090210     C*? REPNRS - Reperisce numero serie
204600090210     C*----------------------------------------------------------------
204700090210     C     REPNRS        BEGSR
204800090210     C*
204900090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
205000090210     C*  (a partire dalla posizione iniziale indicata in tabella)
205100090210     C*  dal numero segnacollo passatomi dal partner
205200090210     C                   Z-ADD     §PUNR1        WP                2 0
205300090210     C     2             SUBST     x30(X):WP     WNRS              2
205400090210     C*  controllo che il nrs calcolata sia numerico
205500090210     C                   TESTN                   WNRS                 98
205600090210     C*  se è numerica la carico
205700090210     C     *IN98         IFEQ      '1'
205800090210     C                   MOVE      WNRS          SGNNRS
205900090210     C                   ELSE
206000090210     C*  se non è numerico stampo errore
206100090210     C                   MOVEL     'S'           WERRO             1
206200090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
206300090213     c                   exsr      Error_DET
206400090210     C                   goto      end_NRS
206500090210     C                   END
206600090210      *
206700090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
206800090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
206900090210     C     SGNNRS        IFNE      VABNRS
207000090210     C                   MOVEL     'S'           WERRO             1
207100090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
207200090210     c                             o della Bolla'
207300090213     c                   exsr      Error_DET
207400090210     C                   goto      end_NRS
207500090210     C                   END
207600090210     C*
207700090210     C     end_NRS       ENDSR
207800090210     C*----------------------------------------------------------------
207900090210     C*? REPZNC - Reperisce zona consegna
208000090210     C*----------------------------------------------------------------
208100090210     C     REPZNC        BEGSR
208200090210     C*
208300090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
208400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
208500090210     C*  dal numero segnacollo passatomi dal partner
208600090210     C                   Z-ADD     §PUZN1        WP                2 0
208700090210     C     2             SUBST     x30(X):WP     WZNC              2
208800090210     C*  controllo che la zona calcolata sia numerica
208900090210     C                   TESTN                   WZNC                 98
209000090210     C*  se è numerica la carico
209100090210     C     *IN98         IFEQ      '1'
209200090210     C                   MOVE      WZNC          SGNZNC
209300090210     C                   ELSE
209400090210     C*  se non è numerica stampo errore
209500090210     C                   MOVEL     'S'           WERRO             1
209600090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
209700090213     c                   exsr      Error_DET
209800090210     C                   goto      end_ZNC
209900090210     C                   END
210000090210     C*
210100090210     C     end_ZNC       ENDSR
210200090210     C*----------------------------------------------------------------
210300090210     C*? REPSGN - Reperisce numero segnacollo
210400090210     C*----------------------------------------------------------------
210500090210     C     REPSGN        BEGSR
210600090210     C*
210700090210     C*  se viene passata nr.segnacollo estraggo sottostringa
210800090210     C*  lunga 7 chr (a partire dalla posizione iniziale
210900090210     C*  indicata in tabella)
211000090210     C*  dal numero segnacollo passatomi dal partner
211100090210     C                   Z-ADD     §PUSG1        WP                2 0
211200090210     C     7             SUBST     x30(X):WP     WSGN              7
211300090210     C*  controllo che il nr.segnacollo calcolato sia numerico
211400090210     C                   TESTN                   WSGN                 98
211500090210     C*  se è numerico lo carico
211600090210     C     *IN98         IFEQ      '1'
211700090210     C                   MOVE      WSGN          SGNNSC
211800090210     C                   ELSE
211900090210     C*  se non è numerica stampo errore
212000090210     C                   MOVEL     'S'           WERRO             1
212100090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
212200090213     c                   exsr      Error_DET
212300090210     C                   goto      end_SGN
212400090210     C                   END
212500090210     C*
212600090210     C     end_SGN       ENDSR
212700051205      *----------------------------------------------------*
212800051205      *   DEFINIZIONE CHIAVI                               *
212900051205      *----------------------------------------------------*
213000051205     C     *INZSR        BEGSR
213100051205      *
213200991129     c     *ENTRY        PLIST
213300060613     C                   parm                    esito             1
213400971215      *
213500050112     C     KTABel        KLIST
213600050112     C                   KFLD                    TBLKUT
213700050112     C                   KFLD                    TBLCOD
213800050112     C                   KFLD                    TBLKEY
213900090210      *
214000090210     C     K_TAB1L       KLIST
214100090213     C                   KFLD                    etbUNB
214200090213     C                   KFLD                    etbSGM
214300090213     C                   KFLD                    etbVALSGM
214400090209     C*
214500090209     C* Definisco chiavi di accesso
214600090209     C     KTAB1         KLIST
214700090209     C                   KFLD                    KKUT
214800090209     C                   KFLD                    KCOD
214900090209     C*
215000090209     C     KNUF          KLIST
215100090209     C                   KFLD                    KAAA
215200090209     C                   KFLD                    KCNU
215300090209     C                   KFLD                    KFIL
215400090209      *
215500090209     C     KVAB1         KLIST
215600090209     C                   KFLD                    KCCM
215700090209     C                   KFLD                    KCMR
215800090209     C                   KFLD                    KCNT
215900090209     C                   KFLD                    KAAS
216000090209     C                   KFLD                    KLNP
216100090209     C                   KFLD                    KNRS
216200090209     C                   KFLD                    KNSP
216300090209      *
216400090209     C     KVAB2         KLIST
216500090209     C                   KFLD                    KCCM
216600090209     C                   KFLD                    KCMR
216700090209      *
216800090209     C     KRDE          KLIST
216900090209     C                   KFLD                    KAAS
217000090209     C                   KFLD                    KLNP1
217100090209     C                   KFLD                    KNRS
217200090209     C                   KFLD                    KNSP
217300090209     C                   KFLD                    KNMC
217400090209     C                   KFLD                    KNRP
217500090209      *
217600090209     C     KACO          KLIST
217700090209     C                   KFLD                    KKUT
217800090209     C                   KFLD                    KKCC
217900090209     C                   KFLD                    KKSC
218000090209      *
218100090209     C     KIND          KLIST
218200090209     C                   KFLD                    KKUT
218300090209     C                   KFLD                    KKCC
218400090209     C                   KFLD                    KKSC
218500090209      *
218600090209      * DETERMINO SE SONO BARTOLINI O SDI
218700090209     C                   CLEAR                   TIBS55DS
218800090209     C                   MOVEL     'L'           I50TLA
218900090209     C                   CALL      'TIBS55R'
219000090209     C                   PARM                    TIBS55DS
219100090209      *
219200090209     C* RECUPERO DATI DELL'UTENTE
219300090209     C                   Z-ADD     1             CODUT             1 0
219400090209     C                   CALL      'XPARUT'
219500090209     C                   PARM                    UTEDSE0F
219600090209     C                   MOVEL     RAGUT         RSUT             20
219700090209     C*
219800090209     C* RICERCA CAPOCONTI
219900090209     C                   DO        50            X
220000090209     C                   MOVE      TCU(X)        TCUDS
220100090209     C     F56           IFEQ      'CG'
220200090209     C     F34           ANDEQ     '01'
220300090209     C                   Z-ADD     KCU(X)        KCI               4 0
220400090209     C                   END
220500090209     C                   END
220600090209     C*
220700090211     c                   movel(p)  'EDPAB'       User_Msg         10
220800090211     C*
220900090209     C* IMPOSTO A ZERO VARIABILI DI PGM
221000090209     c                   move      *blank        Snd_Msg_alert     1
221100090209     C                   MOVEL     CA(1)         WCA              78
221200090209     C                   MOVEL     CN(1)         WCN              78
221300090209     C*
221400090209     C* RECUPERO DATA E ORA
221500090209     C                   TIME                    WHHDAT           14 0
221600090209     C                   MOVE      WHHDAT        G02DAT
221700090209     C                   MOVE      WHHDAT        WDATA8            8 0
221800090209     C                   MOVE      WDATA8        WAA2              2 0
221900090209     C                   MOVEL     WDATA8        DATA              6 0
222000090209     C                   MOVE      WAA2          DATA
222100090209     C                   MOVEL     *BLANK        G02ERR
222200090209     C                   CALL      'XSRDA8'
222300090209     C                   PARM                    WLBDA8
222400090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
222500090209     C                   TIME                    WORA              6 0
222600090209      * Recupero data e ora
222700090209     C                   TIME                    W0140
222800090209      * UDATE IN GGMMAAAA
222900090209     C                   MOVE      W0140         WDTGIO
223000090209      * UDATE IN AAAAMMGG
223100090209     C     *eur          MOVEL     WDTGIO        DATA_eur
223200090209     C     *iso          MOVEL     DATA_eur      dateu
223300090209      *
223400090209      * ?              /*--------------------------- */
223500090209     c                   exsr      CARICA_TABELLE
223600090209      * ?              /*--------------------------- */
223700090209      *
223800090209      *
223900090209     C                   MOVEL     *ALL'-'       CMP132          132
224000090209     C                   CLEAR                   EDiVAB00
224100090211     C                   clear                   vablnp_PT
224200090212     C                   clear                   vabnrs_PT
224300090211     C                   clear                   vabctm_PT
224400090211     C                   clear                   VABfnsp_PT
224500090209     C                   MOVEA     *HIVAL        SGN
224600991124      *
224700050414      *------------------
224800991124     C                   ENDSR
224900060621      * ?------------------------------------------------------------------ */
225000090209      *?    CARICA TABELLE SU SCHIERE
225100060621      * ?------------------------------------------------------------------ */
225200090209     C     CARICA_TABELLEBEGSR
225300090205      *
225400090209     C* Caricamento Tabella 1B
225500090209     C                   Z-ADD     0             X                 4 0
225600090209     C                   Z-ADD     CODUT         KKUT
225700090209     C                   MOVEL     '1B'          KCOD
225800090209     C     KTAB1         CHAIN     TABEL00F                           30
225900090209     C     *IN30         DOWEQ     '0'
226000090209     C     TBLFLG        IFEQ      *BLANKS
226100090209     C                   ADD       1             X
226200090209     C                   MOVEL     TBLKEY        CTM(X)
226300090209     C                   MOVEL     TBLUNI        DTM(X)
226400090209     C                   END
226500090209     C     KTAB1         READE     TABEL00F                               30
226600090209     C                   END
226700090209      *
226800090209     C* Caricamento Tabella 15
226900090209     C                   Z-ADD     0             X                 4 0
227000090209     C                   Z-ADD     CODUT         KKUT
227100090209     C                   MOVEL     '15'          KCOD
227200090209     C     KTAB1         CHAIN     TABEL00F                           30
227300090209     C     *IN30         DOWEQ     '0'
227400090209     C     TBLFLG        IFEQ      *BLANKS
227500090209     C                   ADD       1             X
227600090209     C                   MOVEL     TBLKEY        C15(X)
227700090209     C                   MOVEL     TBLUNI        DS15
227800090209     C                   MOVEL     §15COD        ISO(X)
227900090209     C                   MOVEL     §15ECF        CPF(X)
228000090209     C                   MOVE      §15PCF        CPF(X)
228100090209     C                   END
228200090209     C     KTAB1         READE     TABEL00F                               30
228300090209     C                   END
228400090209      *
228500090209     C* Caricamento Tabella CV
228600090209     C                   Z-ADD     0             X
228700090209     C                   Z-ADD     CODUT         KKUT
228800090209     C                   MOVEL     'CV'          KCOD
228900090209     C     KTAB1         CHAIN     TABEL00F                           30
229000090209     C     *IN30         DOWEQ     '0'
229100090209     C     TBLFLG        IFEQ      *BLANKS
229200090209     C                   ADD       1             X
229300090209     C                   MOVEL     TBLKEY        CCV(X)
229400090209     C                   MOVEL     TBLUNI        DCV(X)
229500090209     C                   END
229600090209     C     KTAB1         READE     TABEL00F                               30
229700090209     C                   END
229800090216      *
229900110328      * Caricamento Tabella termini consegna
230000110328     C                   Z-ADD     0             X
230100110328     C                   MOVEL     'TB'          TABCOD
230200110328     C     TABCOD        CHAIN     EDTAB01L                           30
230300110328     C     *IN30         DOWEQ     '0'
230400110328     C     TABFLG        IFEQ      *BLANKS
230500110328     C                   ADD       1             X
230600110328     C                   eval      K35_TpBolla(X) = TABKEY
230700110328     C                   eval      Cod_TpBolla(X) = TABKEY
230800110328     C                   eval      Decod_Porto(X) = TABUNI
230900110328     C                   END
231000110328     C     TABCOD        READE     EDTAB01L                               30
231100110328     C                   END
231200110328      *
231300090216     C* Caricamento Tabella Tipo Incasso C/Assegno
231400090216     C                   Z-ADD     0             X
231500090216     C                   MOVEL     'TC'          TABCOD
231600090216     C     TABCOD        CHAIN     EDTAB01L                           30
231700090216     C     *IN30         DOWEQ     '0'
231800090216     C     TABFLG        IFEQ      *BLANKS
231900090216     C                   ADD       1             X
232000090216     C                   MOVEL     TABKEY        CTC35(X)
232100090216     C                   MOVEL     TABKEY        CTC(X)
232200090216     C                   MOVEL     TABUNI        TPI(X)
232300090216     C                   END
232400090216     C     TABCOD        READE     EDTAB01L                               30
232500090216     C                   END
232600090209      *
232700090209      * Caricamento Tabella Partner esteri
232800090209     C                   Z-ADD     0             X
232900090209     C                   MOVEL     'PT'          TABCOD
233000090209     C     TABCOD        CHAIN     EDTAB01L                           30
233100090209     C     *IN30         DOWEQ     '0'
233200090209      *
233300090209     C                   SELECT
233400090209     C     TABFLG        WHENNE    *BLANKS
233500090209      *
233600090209     C                   OTHER
233700090209     C                   ADD       1             X
233800090209     C                   MOVEL     TABKEY        CPT(X)
233900090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
234000090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
234100090209     C                   MOVEL     TABUNI        DPT(X)
234200090209     C                   ENDSL
234300090209      *
234400090209     C     TABCOD        READE     EDTAB01L                               30
234500090209     C                   END
234600121105      *
234700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
234800121105     c                   exsr      ChecK_PT
234900121105     C*
235000090209      * salva quanti Codici UNB ha caricato
235100090209     c                   z-add     x             sav_CPTx
235200090209      *
235300090209     C                   MOVEL     'PU'          TABCOD
235400090209     C     TABCOD        CHAIN     EDTAB01L                           30
235500090209     C     *IN30         DOWEQ     '0'
235600090209      *
235700090209     C                   SELECT
235800090209     C     TABFLG        WHENNE    *BLANKS
235900090209      *
236000090209     C                   OTHER
236100090209     C                   MOVEL     TABKEY        CODPU            35
236200090209     C                   Z-ADD     1             X
236300090209     C     CODPU         LOOKUP    CPT(X)                                 32
236400090209     C   32              MOVEL     TABUNI        DPU(X)
236500090209     C                   ENDSL
236600090209      *
236700090209     C     TABCOD        READE     EDTAB01L                               30
236800090209     C                   END
236900090209      *
237000090209      * Prosegue tab.PT e PU
237100090209     C                   MOVEL     'PV'          TABCOD
237200090209     C     TABCOD        CHAIN     EDTAB01L                           30
237300090209     C     *IN30         DOWEQ     '0'
237400090209      *
237500090209     C                   SELECT
237600090209     C     TABFLG        WHENNE    *BLANKS
237700090209      *
237800090209     C                   OTHER
237900090209     C                   MOVEL     TABKEY        CODPV            35
238000090209     C                   Z-ADD     1             X
238100090209     C     CODPV         LOOKUP    CPT(X)                                 32
238200090209     C   32              MOVEL     TABUNI        DPV(X)
238300090209     C                   ENDSL
238400090209      *
238500090209     C     TABCOD        READE     EDTAB01L                               30
238600090209     C                   END
238700090209      *
238800090209     C* Caricamento Tabella codici clienti - tariffe
238900090209     C                   Z-ADD     0             X
239000090209     C                   MOVEL     'CL'          TABCOD
239100090209     C     TABCOD        CHAIN     EDTAB01L                           30
239200090209     C     *IN30         DOWEQ     '0'
239300090209     C     TABFLG        IFEQ      *BLANKS
239400090209     C                   ADD       1             X
239500090209     C                   MOVEL     TABKEY        CCL(X)
239600090209     C                   MOVEL     TABUNI        DCL(X)
239700090209     C                   END
239800090209     C     TABCOD        READE     EDTAB01L                               30
239900090209     C                   END
240000090209      *
240100090209     C* Caricamento Serivizi speciali
240200090209     C                   Z-ADD     0             X
240300090209     C                   MOVEL     'SS'          TABCOD
240400090209     C     TABCOD        CHAIN     EDTAB01L                           30
240500090209     C     *IN30         DOWEQ     '0'
240600090209     C     TABFLG        IFEQ      *BLANKS
240700090209     C                   ADD       1             X
240800090209     C                   MOVEL     TABKEY        SS(X)
240900090209     C                   MOVEL     TABKEY        SS35(X)
241000090209     C                   MOVEL     TABUNI        DSS(X)
241100090209     C                   END
241200090209     C     TABCOD        READE     EDTAB01L                               30
241300090209     C                   END
241400090209      *
241500090209     C                   ENDSR
241600090209      * ?------------------------------------------------------------------ */
241700090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
241800090209      * ?------------------------------------------------------------------ */
241900090209     C     MSG_Errato    BEGSR
242000090209      *
242100090205      * ?  Scrive su tutti i records il tipo di errore
242200090205     c     *start        setll     tivin00r
242300090205     c                   read      tivin00r
242400091016     c                   if        %Eof(tivin00r)
242500091016     c                   move      'S'           EoFTivin
242600091016     c                   end
242700090205     c                   dow       not %eof(tivin00r)
242800090211     c                   if        vinMSG  = *blank
242900090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
243000090210     C                               >> Controllare i DATI su UPLOAD !!'
243100090211     c                   end
243200090205     C                   eval      vinFLG = '2'
243300090213     c                   eval      Almeno_Un_Due ='S'
243400090205     c                   eval      esito  = '2'
243500090209     c                   eval      se_errore ='S'
243600090213     c                   eval      err_bloccante ='S'
243700091016     c                   if        EoFTivin = ' '
243800090205     c                   update    tivin000
243900091016     c                   end
244000090205     c                   read      tivin00r
244100091016     c                   if        %Eof(tivin00r)
244200091016     c                   move      'S'           EoFTivin
244300091016     c                   end
244400090205     c                   endDO
244500090205      *
244600090205     C                   ENDSR
244700090213      * ?------------------------------------------------------------------ */
244800090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
244900090213      * ?------------------------------------------------------------------ */
245000090213     C     DETta_Errato  BEGSR
245100090213      *
245200090213      *  rilascia il record prima di rieseguire il ciclo
245300090213     c                   update    tivin000
245400090213      *
245500090213      * ?  Scrive su tutti i records il tipo di errore
245600090213     c     SavNUM_RECord setll     tivin00r
245700090213     c                   read      tivin00r
245800091016     c                   if        %Eof(tivin00r)
245900091016     c                   move      'S'           EoFTivin
246000091016     c                   end
246100090213     c                   dow       not %eof(tivin00r)
246200090213      *
246300090213      *
246400090213     c                   if        vinMSG  = *blank
246500090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
246600090213     C                             >> Controllare i DATI di ogni Segmento <<'
246700090213     c                   end
246800090213     c                   exsr      Error_DET
246900090213      *
247000090213     c                   if        VNREC = NUM_RECord
247100090213     c                   leave
247200090213     c                   end
247300090213      *
247400091016     c                   if        EoFTivin = ' '
247500090213     c                   update    tivin000
247600091016     c                   end
247700090213     c                   read      tivin00r
247800091016     c                   if        %Eof(tivin00r)
247900091016     c                   move      'S'           EoFTivin
248000091016     c                   end
248100090213      *
248200090213     c                   endDO
248300090603      *
248400091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
248500090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
248600091019???? C                             Bolle RHENUS non totalmente tradotto. -
248700100603     C                             Controllare UPLOAD del cliente 0895358.'
248800130321     c                   if             manda_mail='S'
248900090603     c                   exsr      invio_mail_AB
249000130321     c                   end
249100090603      *
249200090213      *
249300090213     C                   ENDSR
249400090205      * ?------------------------------------------------------------------ */
249500090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
249600090205      * ?------------------------------------------------------------------ */
249700090205     C     *pssr         BEGSR
249800090205     C
249900060710     C                   eval      esito = '2'
250000091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
250100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
250200100603     C                             Bolle import in filiale - messaggio EDI -
250300100603     C                             ricevuto errato su UPLOAD cliente 0895358.'
250400090603     c                   exsr      invio_mail_AB
250500060621     C
250600090213     C                   ENDSR
250700090213      * ?------------------------------------------------------------------ */
250800090213      *?      Segnala Errore su TIVIN
250900090213      * ?------------------------------------------------------------------ */
251000090213     C     Error_DET     BEGSR
251100090213     C
251200090213     C                   eval      vinFLG = '2'
251300090213     c                   eval      Almeno_Un_Due ='S'
251400090213     c                   eval      esito  = '1'
251500090213     c                   eval      se_errore ='S'
251600090213     C                   eval      Err_in_detta = 'S'
251700090213     C
251800090213     C                   ENDSR
251900090209     c*==================================================================*
252000090209     C*? CNVDAT - DECODIFICA LA DATA
252100090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
252200090209     C*         - WDATA  (DATA ALFANUMERICA)
252300090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
252400090209     C*         - WHMS   (ORA NUMERICA)
252500090209     C*----------------------------------------------------------------
252600090209     C     CNVDAT        BEGSR
252700090209     C*
252800090209     C                   MOVEL     ' '           WERRO             1
252900090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
253000090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
253100090209     C                   Z-ADD     *ZEROS        WHMS              6 0
253200090209     C*
253300090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
253400090209     C                   MOVEL     WDATA         WCOMO8            8
253500090209     C                   TESTN                   WCOMO8               99
253600090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
253700090209     C  N99              MOVEL     'S'           WERRO             1
253800090209     C                   END
253900090209     C*
254000090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
254100090209     C                   MOVEL     WDATA         WCOMO14          14
254200090209     C                   TESTN                   WCOMO14              99
254300090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
254400090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
254500090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
254600090209     C  N99              MOVEL     'S'           WERRO             1
254700090209     C                   END
254800090209     C*
254900090209     C                   ENDSR
255000090210     C*----------------------------------------------------------------
255100090210     C*? TSTNUM - CONTROLLO NUMERICO
255200090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
255300090210     C*         - WLUNGH (Lunghezza campo output)
255400090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
255500090210     C*----------------------------------------------------------------
255600090210     C     TSTNUM        BEGSR
255700090210     C*
255800090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
255900090210     C                   MOVEL     'N'           WOKNUM            1
256000090210     C                   MOVEL     *BLANKS       WNUMOK           15
256100090210     C                   Z-ADD     15            IN                3 0
256200090210     C                   Z-ADD     WLUNGH        IX                3 0
256300090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
256400090210     C                   MOVEA     WNUM          NDA
256500090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
256600090210     C                   MOVEA     *ALL'0'       NDN
256700090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
256800090210     C                   Z-ADD     WLUNGH        I                 3 0
256900090210     C     I             DOWGT     0                                            --- 1 -->
257000090210     C* ESTRAE IL CARATTERE DA ESAMINARE
257100090210     C     1             SUBST     WNUM:I        WTEM01            1
257200090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
257300090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
257400090210     C     WTEM01        SCAN      WCA                                    99
257500090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
257600090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
257700090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
257800090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
257900090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
258000090210     C* DA DESTRA VERSO SINISTRA
258100090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
258200090210     C                   MOVEL     WTEMP1        NDN(IN)
258300090210     C                   SUB       1             IN
258400090210     C                   END                                                    <-- 3 ---
258500090210     C                   END                                                    <-- 2 ---
258600090210     C                   SUB       1             I
258700090210     C                   END                                                    <-- 1 ---
258800090210     C*
258900090210     C                   MOVEA     NDN           WNDN             15
259000090210     C     WNDN          IFNE      *ZEROS
259100090210     C                   MOVEA     NDN           WNUMOK           15
259200090210     C                   MOVEL     'S'           WOKNUM            1
259300090210     C                   END
259400090210     C*
259500090210     C                   ENDSR
259600090210     C*----------------------------------------------------------------
259700090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
259800090210     C*----------------------------------------------------------------
259900090210     C     CNVCAP        BEGSR
260000090210     C*
260100090210     C                   MOVEL     *BLANKS       WCAP              9
260200090210     C     nad3251       IFNE      '0'
260300090210     C     '  '          SCAN      nad3251       LENGHT            2 0
260400090210     C                   SUB       1             LENGHT
260500090210     C     LENGHT        IFLE      5
260600090210     C     LENGHT        ANDGT     0
260700090210     C                   Z-ADD     LENGHT        T                 2 0
260800090210     C                   Z-ADD     5             S                 2 0
260900090210     C                   MOVEA     nad3251       CAP9
261000090210     C                   MOVEL     *ZEROS        CAP5
261100090210     C                   DO        LENGHT
261200090210     C                   MOVE      CAP9(T)       CAP5(S)
261300090210     C                   SUB       1             S
261400090210     C                   SUB       1             T
261500090210     C                   END
261600090210     C                   MOVEA     CAP5          WCAP5             5
261700090210     C     WCAP5         IFEQ      '0'
261800090210     C                   MOVEL     *BLANKS       WCAP
261900090210     C                   ELSE
262000090210     C                   MOVEA     CAP5          WCAP
262100090210     C                   END
262200090210     C*
262300090210     C                   ELSE
262400090210     C                   MOVEL     nad3251       WCAP
262500090210     C                   END
262600090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
262700090210     C     WCAP          IFNE      *BLANKS
262800090210     C     WFLCPF        ANDNE     0
262900090210     C                   MOVEL     WFLCPF        WELE              1 0
263000090210     C                   MOVE      WFLCPF        WPOS              1 0
263100090210     C                   MOVEL     *BLANK        WCPF
263200090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
263300090210     C                   ELSE
263400090210     C                   MOVEL     *BLANKS       WCPF
263500090210     C                   END
263600090210     C                   END
263700090210     C*
263800090210     C                   ENDSR
263900090211      * ?================================================================== */
264000090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
264100090211      * ?================================================================== */
264200090211     C     Scrive_VAB    BEGSR
264300090211      *
264400090211     c                   clear                   err_bloccante     1
264500090211     c                   move      'S'           Almeno_Uno
264600090211      *
264700090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
264800090211     c                   exsr      UPDVAB
264900090211      *
265000090211     C* >>>>>>>>>>>>
265100090211     C*  Richiamo pgm per scrittura immediata VAB
265200090211     C     §PUWRK        IFEQ      ' '
265300090211      *
265400090211     C*  Richiamo pgm per esecuzione stampa
265500090211     C     §PTCMR        IFEQ      'S'
265600090213     C                   MOVEL     WNRDOC        d86CMR
265700090211     C                   ELSE
265800090213     C                   MOVEL     WNRDOC        d86CMR
265900090211     C                   END
266000090211      *
266100090213     C                   MOVEL     ' '           d86RIE
266200090213     C                   MOVEL     §PUDTA        d86DTA
266300090213     C                   Z-ADD     1             d86CNT
266400090213     C                   Z-ADD     §PTKSC        d86CCM
266500090213     C                   MOVEL     *BLANKS       d86STA
266600090213     C                   MOVEL     'E'           d86MOD
266700090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
266800090211      * dove poi imposta la DS dei parametri passati
266900090213     C                   MOVEL     'LR'          d86CHI
267000090213     C                   MOVEL     'N'           d86CTL
267100090211      *
267200090211     c                   move      kpjbu         SvKpjbu
267300090211     c                   clear                   kpjbu
267400090216     C                   movel     *on           Commit_on         1
267500090213     C                   MOVEL     TRTC86ds      KPJBU
267600090213      * ?- Chiamata la TRTC86R2 -*
267700090213     C                   CALL      'TRTC86R2'
267800090211     C                   PARM                    KPJBA
267900090216     C                   PARM                    Commit_on
268000090211     c                   move      Svkpjbu       Kpjbu
268100090211      *
268200090211     C*  Controllo pgm per scrittura immediata VAB
268300090211     C     WW            IFNE      0
268400090211     C                   DO        WW            WX                3 0
268500090213     C                   Z-ADD     KCL(WX)       d86CCM
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
269600090211     C                   END
269700090211     C                   END
269800090211      *
269900090211     C                   END
270000090211      * >>>>>>>>>>>>
270100090211     C                   if        se_errore  = 'S'
270200090211     c                   move      'S'           err_bloccante
270300090211     c                   goto      Error_VAB
270400090211     c                   end
270500090211     C*
270600090211     C     Error_VAB     Endsr
270700090211     C*----------------------------------------------------------------
270800090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
270900090211     C*----------------------------------------------------------------
271000090211     C     UPDVAB        BEGSR
271100090211      *
271200090211     C*  Se non è stato impostato il codice bolla lo imposto io
271300090211     C     VABCBO        IFEQ      *BLANKS
271400090211     C     VABCAS        IFGT      0
271500090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
271600090211     C                   ELSE
271700090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
271800090211     C                   END
271900090211     C                   END
272000110922      *
272100110922      * Imposta le NOte
272200110922     C     VABNOT        IFEQ      *BLANKS
272300110922     C                   MOVEL     wnot1         VABNOT
272400110922     C                   MOVEL     wnot2         VABNT2
272500110922     C                   ELSE
272600110922     C                   MOVEL     wnot1         VABNT2
272700110922     C                   END
272800090211      *
272900090211     C*  Attribuisco anno spedizione
273000090211     C     WDATPA        IFGT      0                                            anno sped.
273100090211     C                   MOVEL     WDATPA        VABAAS
273200090211     C                   ELSE
273300090211     C     WDATFV        IFGT      0
273400090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
273500090211     C                   END
273600090211     C                   END
273700090211      *
273800090211      *  Controllo dettaglio segnacolli
273900090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
274000090211     C                   EXSR      CTRSGE
274100090211     C                   ELSE
274200090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
274300090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
274400090211     C                   EXSR      CTRSGN
274500090211     C                   END
274600090211     C                   END
274700090211      *
274800090212      *  Imposto linea partenza e serie
274900090211     C                   eval      VABLNP = VABLNP_PT
275000090212     C                   eval      VABNRS = VABnrs_PT
275100090211      *
275200090211      *  Controllo se devo variare il codice trattamento merce
275300090211     C                   eval      VABCTM = VABctm_PT
275400090211      *
275500090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
275600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
275700090211     c                   clear                   mantiene_nsp      1            *blk/'S'
275800090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
275900090211      *
276000090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
276100090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
276200090211      *
276300090211      *  Imposto codice cliente
276400090211     c                   Select
276500090211      *
276600090211      *  Specificato codice da qualificatore FF (TD15)
276700090211     C                   When        WCLKSC <> *zeros
276800090211     C                   Z-ADD     WCLKSC        VABCCM
276900090211     C                   MOVEL     WCLTAR        VABCTR
277000090211      * forza LNP/CTM/NRS
277100090211     c                   if        WclLNP <> 0
277200090211     C                   eval      VABLNP = WclLNP
277300090211     c                   end
277400090211     c                   if        WclNRS <> 0
277500090211     C                   eval      VABNRS = WclNRS
277600090211     c                   end
277700090211     c                   if        WclCTM <> *blank
277800090211     C                   eval      VABCTM = WclCTM
277900090211     c                   end
278000090211      *
278100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
278200090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
278300090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
278400090211     c                   end
278500090211      *
278600090211      *  Specificato codice da qualificatore FF (TT15)
278700090211     C                   When        $CLKSC <> *zeros
278800090211     C                   Z-ADD     $CLKSC        VABCCM
278900090211     C                   MOVEL     $CLTAR        VABCTR
279000090211      * forza LNP/CTM/NRS
279100090211     c                   if        $clLNP <> 0
279200090211     C                   eval      VABLNP = $clLNP
279300090211     c                   end
279400090211     c                   if        $clNRS <> 0
279500090211     C                   eval      VABNRS = $clNRS
279600090211     c                   end
279700090211     c                   if        $clCTM <> *blank
279800090211     C                   eval      VABCTM = $clCTM
279900090211     c                   end
280000090211      *
280100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
280200090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
280300090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
280400090211     c                   end
280500090211      *
280600090211      *  Imposto dati da tabella - PT-
280700090211     C                   Other
280800090211      *
280900090211     C                   MOVEL     §PTKSC        VABCCM
281000090211     C                   MOVEL     §PTCTR        VABCTR
281100090211      *
281200090211     C                   Endsl
281300090211      *
281400090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
281500090211      *    o dalla relativa CL
281600090211     C                   MOVEL     VABlnp        VABfgs
281700090211      *
281800090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
281900090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
282000090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
282100090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
282200090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
282300090211      *
282400090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
282500090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
282600090211     c                   else
282700090211      *
282800090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
282900090211     C*  lo attribuisco io
283000090211      **              **------------------**
283100090211     c                   exsr      repnum
283200090211      **              **------------------**
283300090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
283400090211      **              **------------------**
283500090211     c                   end
283600090211      *
283700090211     C*  Altrimenti attribuisco data del giorno
283800090211     C     VABMGS        IFEQ      0
283900090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
284000090211     C                   MOVE      WOGGI         VABMGS                         = oggi
284100090211     C                   END
284200090211     C*  Eseguo posizionamento su EDiVAB
284300090211     C                   Z-ADD     VABAAS        KAAS
284400090211     C                   Z-ADD     VABlnp        KLNP
284500090211     C                   Z-ADD     VABNRS        KNRS
284600090211     C                   Z-ADD     VABNSP        KNSP
284700090211     C                   MOVEL     SAVBOL        SALVA
284800090211     C     KVAB1         CHAIN     EDiVAB1L                           30
284900090211     C                   MOVEL     SALVA         SAVBOL
285000090211     C*  Imposto dati CMR
285100090211     C                   Z-ADD     1             VABCNT
285200090211     **
285300090211      *** Attenzione se si deve trattare il VAX con il CMR
285400090211      *** il campo VABCNT deve essere sempre impostato a (1)
285500090211     C     §puWRK        ifEQ      'S'
285600090211     C     §puNSP        andEQ     'S'
285700090211     C                   Z-ADD     1             VABCNT
285800090211     c                   end
285900090211     **
286000090211     C                   Z-ADD     WDTPRE        VABDTS
286100090212     C                   movel     '20'          VABDTS
286200090212     C     WHMPRE        mult      100           VABHMS
286300090211     C     §PTCMR        IFEQ      'S'
286400090211     C                   Z-ADD     WDTCMR        VABDCM
286500090211     C                   MOVEL     WNRCMR        VABCMR
286600090211     C                   ELSE
286700090211     C                   Z-ADD     WDATFV        VABDCM
286800090211     C                   MOVEL     WNUMFV        VABCMR
286900090211     C                   END
287000090211     C*  Se non è indicato il nome del mittente = Partner mittente
287100090211     C     VABRMO        IFEQ      *BLANKS
287200090211     C                   MOVEL     ACORAG        VABRMO
287300090211     C                   MOVEL     INDCAP        VABCMO
287400090211     C     INDCAE        IFNE      *BLANKS
287500090211     C                   MOVEL     INDCAE        VABCMO
287600090211     C                   END
287700090211     C     INDSTA        IFNE      *BLANKS
287800090211     C                   MOVEL     INDSTA        VABNMO
287900090211     C                   END
288000090211     C                   END
288100090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
288200090211     C     §1BFG1        IFEQ      'N'
288300090211     C     §1BFG7        ANDEQ     'N'
288400090211     C     §1BFG2        ANDEQ     'N'
288500090211     C***                  Z-ADD0         VABNRS
288600090211     C                   END
288700090211     C*  Imposto segancollo da - a
288800090211     C     §1BFG1        IFEQ      'S'
288900090211     C     §1BFG7        ANDEQ     'N'
289000090211     C     §1BF12        ANDNE     'E'
289100090211      *
289200090211     C     XY            IFEQ      *ZEROS
289300090211     C                   CLEAR                   VABNCD
289400090211     C                   CLEAR                   VABNCA
289500090211     C                   ELSE
289600090211     C                   Z-ADD     SGN(1)        VABNCD
289700090211     C                   Z-ADD     SGN(XY)       VABNCA
289800090211     C                   ENDIF
289900090211     C*
290000090211     C                   END
290100090211     C*
290200090211     C*  Se cap mittente originale a blank abblenco nazione
290300090211     C     VABCMO        IFEQ      *BLANKS
290400090211     C                   MOVEL     *BLANKS       VABNMO
290500090211     C                   END
290600090603     C*
290700090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
290800090603     C* Se in testata
290900090603     c                   if        NAD_destinatario_in_testata = 'S'
291000090603     c                   eval         VABrsd =  tes_VABrsd
291100090603     c                   eval         VABrd2 =  tes_VABrd2
291200090603     c                   eval         VABind =  tes_VABind
291300090603     c                   eval         VABlod =  tes_VABlod
291400090603     c                   eval         VABnzd =  tes_VABnzd
291500090603     c                   end
291600090603     C* Se in testata
291700090603     c                   if        NAD_mittente_in_testata = 'S'
291800090603     c                   eval         VABrmo = tes_VABrmo
291900090603     c                   eval         VABnmo = tes_VABnmo
292000090603     c                   eval         VABcmo = tes_VABcmo
292100090603     c                   end
292200090603      *
292300090211      *? ------------------------------------------------------------------------
292400090213     C   30              WRITE     EDiVAB00                             99
292500090213     C  N30              UPDATE    EDiVAB00                             99
292600090211      *? ------------------------------------------------------------------------
292700090213     c   99              eval      errori_in_Wrt = 'S'
292800090211     C*
292900090211     c                   clear                   wri_vabtic        1
293000090211     C*
293100090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
293200090211     C                   EXSR      WRTVAT_Rif
293300090211     C*
293400090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
293500090211     C     §1BF12        IFEQ      'E'
293600090211     C                   EXSR      WRTVAT
293700090211     C                   ELSE
293800090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
293900090211     C     §1BFG7        IFEQ      'S'
294000090211     C     §1BFG1        OREQ      'S'
294100090211     C                   EXSR      WRTVAD
294200090211     C                   END
294300090211     C                   END
294400090211      *
294500090211     C* Reimposto codice trattamento merce cliente
294600090211     C                   MOVEL     WSVTRM        DS1B
294700090211     C**
294800090211     C* Do errore se previsto CMR ma non c'è
294900090211     C     §PTCMR        IFNE      ' '
295000090211     C                   EXSR      MEMCMR
295100090211     C                   END
295200090211     C* Do errore se previsto AFB ma non c'è
295300090211     C     §PTNFV        IFNE      ' '
295400090211     C                   EXSR      MEMAFB
295500090211     C                   END
295600090211     C*  Se il cliente vuole il ritorno delle date di consegna
295700090211     C*  è c'è una squdratura fra i segnacolli e il numero
295800090211     C*  dei colli che ci ha passato scrivo EDSUM
295900090211     C     §PUDTA        IFEQ      'S'
296000090211     C     WSQUAD        ANDEQ     'S'
296100090211     C                   EXSR      WRTSUM
296200090211     C                   END
296300090211     C*
296400090211     C                   ENDSR
296500090211     C*----------------------------------------------------------------
296600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
296700090211     C*----------------------------------------------------------------
296800090211     C     CTRSGN        BEGSR
296900090211     C*
297000090211     C                   MOVEL     'N'           WNSGER            1
297100090211     C*
297200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
297300090211     C     VABNCL        IFNE      XY
297400090211     C                   MOVEL     'S'           WNSGER
297500090211     C                   MOVEL     'S'           WSQUAD
297600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
297700090211     c                              bollettati'
297800090211     C                   GOTO      FINSGN
297900090211     C                   END
298000090211     C*  Riordino i segnacolli
298100090211     C     §1BFG7        IFEQ      'N'
298200090211     C                   SORTA     SGN
298300090211     C*  Controllo se sono sequenziali
298400090211     C                   MOVEL     'N'           WNOSEQ            1
298500090211     C     SGN(1)        ADD       1             WEXSGN            7 0
298600090211     C     2             DO        XY            X
298700090211     C     SGN(X)        IFNE      WEXSGN
298800090211     C                   MOVEL     'S'           WNOSEQ
298900090211     C                   END
299000090211     C     SGN(X)        ADD       1             WEXSGN
299100090211     C                   END
299200090211     C                   END
299300090211     C*
299400090211     C     FINSGN        ENDSR
299500090211     C*----------------------------------------------------------------
299600090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
299700090211     C*----------------------------------------------------------------
299800090211     C     CTRSGE        BEGSR
299900090211     C*
300000090211     C                   MOVEL     'N'           WNSGER            1
300100090211      *
300200090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
300300090211      *
300400090211     C     VABNCL        IFNE      §§
300500090211     C                   MOVEL     'S'           WNSGER
300600090211     C                   MOVEL     'S'           WSQUAD
300700090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
300800090211     c                              bollettati'
300900090211     C                   END
301000090211      *
301100090211     C                   ENDSR
301200090211     C*----------------------------------------------------------------
301300090211     C*? ESEGUO SCRITTURA EDiVAD
301400090211     C*----------------------------------------------------------------
301500090211     C     WRTVAD        BEGSR
301600090211     C*
301700090211     C* Se non seq. scrivo xy record
301800090211     C     §1BFG1        IFEQ      'S'
301900090211     C     XY            ANDNE     *ZEROS
302000090211     C*
302100090211     C     §1BFG7        IFEQ      'S'
302200090211     C                   DO        XY            X
302300090211     C                   Z-ADD     VABCCM        KCCM
302400090211     C                   Z-ADD     SGN(X)        KNCD
302500090211     C                   Z-ADD     VABCNT        VADCNT
302600090211     C                   MOVEL     VABCMR        VADCMR
302700090211     C                   Z-ADD     VABAAS        VADAAS
302800090211     C                   Z-ADD     VABLNP        VADLNP
302900090211     C                   Z-ADD     VABNRS        VADNRS
303000090211     C                   Z-ADD     VABNSP        VADNSP
303100090211     C                   Z-ADD     1             VADNCL
303200090211     C                   MOVEL     SGN(X)        VADCDP
303300090211     C                   Z-ADD     SGN(X)        VADNCD
303400090211     C                   Z-ADD     *ZEROS        VADNCA
303500090211     C                   Z-ADD     VABCCM        VADCCM
303600090211     C                   movel     VABfgs        VADfgs
303700090213     C                   WRITE     EDiVAD00                             99
303800090213     c   99              eval      errori_in_Wrt = 'S'
303900090211     C                   END
304000090211     C* Se sequenziali scrivo un solo record
304100090211     C                   ELSE
304200090211     C                   Z-ADD     VABAAS        VADAAS
304300090211     C                   Z-ADD     VABLNP        VADLNP
304400090211     C                   Z-ADD     VABNRS        VADNRS
304500090211     C                   Z-ADD     VABNSP        VADNSP
304600090211     C                   Z-ADD     VABNCL        VADNCL
304700090211     C                   Z-ADD     SGN(1)        VADNCD
304800090211     C                   Z-ADD     SGN(XY)       VADNCA
304900090211     C                   Z-ADD     VABCCM        VADCCM
305000090211     C                   movel     VABfgs        VADfgs
305100090213     C                   WRITE     EDiVAD00                             99
305200090213     c   99              eval      errori_in_Wrt = 'S'
305300090211     C                   END
305400090211     C*
305500090211     C                   ELSE
305600090211     C                   DO        XY            X
305700090211     C                   Z-ADD     VABCCM        KCCM
305800090211     C                   Z-ADD     SGN(X)        KNCD
305900090211     C                   Z-ADD     VABAAS        VADAAS
306000090211     C                   Z-ADD     VABLNP        VADLNP
306100090211     C                   Z-ADD     VABNRS        VADNRS
306200090211     C                   Z-ADD     VABNSP        VADNSP
306300090211     C                   MOVEL     SGN(X)        VADCDP
306400090211     C                   Z-ADD     1             VADNCL
306500090211     C                   Z-ADD     *ZEROS        VADNCD
306600090211     C                   Z-ADD     *ZEROS        VADNCA
306700090211     C                   Z-ADD     VABCCM        VADCCM
306800090211     C                   movel     VABfgs        VADfgs
306900090213     C                   WRITE     EDiVAD00                             99
307000090213     c   99              eval      errori_in_Wrt = 'S'
307100090211     C                   END
307200090211     C*
307300090211     C                   END
307400090211     C*
307500090211     C                   ENDSR
307600090211     C*----------------------------------------------------------------
307700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
307800090211     C*----------------------------------------------------------------
307900090211     C     WRTVAT_Rif    BEGSR
308000090211      *
308100090211      *  riferimento di consegna destinatario
308200090211     c                   if        WatRDE <> *blank
308300090211     C                   Z-ADD     VABCNT        VATCNT
308400090211     C                   MOVEL     VABCMR        VATCMR
308500090211     C                   Z-ADD     VABCCM        VATCCM
308600090211     C                   movel     VABfgs        VATfgs
308700090211     C                   Z-ADD     VABLNP        VATLNP
308800090211     C                   Z-ADD     VABAAS        VATAAS
308900090211     C                   Z-ADD     VABNRS        VATNRS
309000090211     C                   Z-ADD     VABNSP        VATNSP
309100090211     C                   MOVEL     'A'           VATTRC
309200090211     C                   MOVEL     WatRDE        VATNOT
309300090213     C                   WRITE     EDiVAT00                             99
309400090213     c   99              eval      errori_in_Wrt = 'S'
309500090211     c                   end
309600090211      *
309700090211      *  riferimento di consegna telefonico
309800090211     c                   if        WatTEL <> *blank
309900090211     C                   Z-ADD     VABCNT        VATCNT
310000090211     C                   MOVEL     VABCMR        VATCMR
310100090211     C                   Z-ADD     VABCCM        VATCCM
310200090211     C                   movel     VABfgs        VATfgs
310300090211     C                   Z-ADD     VABLNP        VATLNP
310400090211     C                   Z-ADD     VABAAS        VATAAS
310500090211     C                   Z-ADD     VABNRS        VATNRS
310600090211     C                   Z-ADD     VABNSP        VATNSP
310700090211     C                   MOVEL     'B'           VATTRC
310800090211     C                   MOVEL     WatTEL        VATNOT
310900090213     C                   WRITE     EDiVAT00                             99
311000090213     c   99              eval      errori_in_Wrt = 'S'
311100090211     c                   end
311200090211      *
311300090211      * riferimento del Partner che una volta veniva riportato sul BL4
311400090211     c                   if        Wnsp   <> *blank
311500090211     C                   Z-ADD     VABCNT        VATCNT
311600090211     C                   MOVEL     VABCMR        VATCMR
311700090211     C                   Z-ADD     VABCCM        VATCCM
311800090211     C                   movel     VABfgs        VATfgs
311900090211     C                   Z-ADD     VABLNP        VATLNP
312000090211     C                   Z-ADD     VABAAS        VATAAS
312100090211     C                   Z-ADD     VABNRS        VATNRS
312200090211     C                   Z-ADD     VABNSP        VATNSP
312300090211     C                   MOVEL     'C'           VATTRC
312400090211     C                   MOVEL     Wnsp          VATNOT
312500090213     C                   WRITE     EDiVAT00                             99
312600090213     c   99              eval      errori_in_Wrt = 'S'
312700090211     c                   end
312800130422      *
312900130422      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
313000130422      *   sulla consegna della sua merce
313100130422      *
313200130422     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
313300130422     c                                  <> *blank
313400130422     C                   Z-ADD     VABCNT        VATCNT
313500130422     C                   MOVEL     VABCMR        VATCMR
313600130422     C                   Z-ADD     VABCCM        VATCCM
313700130422     C                   movel     VABfgs        VATfgs
313800130422     C                   Z-ADD     VABLNP        VATLNP
313900130422     C                   Z-ADD     VABAAS        VATAAS
314000130422     C                   Z-ADD     VABNRS        VATNRS
314100130422     C                   Z-ADD     VABNSP        VATNSP
314200130422     C                   MOVEL     'I'           VATTRC
314300130422     C                   eval       VATNOT =
314400130422     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
314500130422     C                   WRITE     EDiVAT00                             99
314600130422     c   99              eval      errori_in_Wrt = 'S'
314700130422     c                   end
314800130422      *
314900130422     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
315000130422     c                                 <> *blank
315100130422     C                   Z-ADD     VABCNT        VATCNT
315200130422     C                   MOVEL     VABCMR        VATCMR
315300130422     C                   Z-ADD     VABCCM        VATCCM
315400130422     C                   movel     VABfgs        VATfgs
315500130422     C                   Z-ADD     VABLNP        VATLNP
315600130422     C                   Z-ADD     VABAAS        VATAAS
315700130422     C                   Z-ADD     VABNRS        VATNRS
315800130422     C                   Z-ADD     VABNSP        VATNSP
315900130422     C                   MOVEL     'J'           VATTRC
316000130422     C                   eval       VATNOT =
316100130422     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
316200130422     C                   WRITE     EDiVAT00                             99
316300130422     c   99              eval      errori_in_Wrt = 'S'
316400130422     c                   end
316500090211      *
316600140423      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
316700140423      *   sulla consegna della sua merce
316800140423     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
316900140423     c                                  <> *blank
317000140423     c                             or FTX_MAIL_come_servizio = 'N'
317100140423      *
317200140423     C                   Z-ADD     VABCNT        VATCNT
317300140423     C                   MOVEL     VABCMR        VATCMR
317400140423     C                   Z-ADD     VABCCM        VATCCM
317500140423     C                   movel     VABfgs        VATfgs
317600140423     C                   Z-ADD     VABLNP        VATLNP
317700140423     C                   Z-ADD     VABAAS        VATAAS
317800140423     C                   Z-ADD     VABNRS        VATNRS
317900140423     C                   Z-ADD     VABNSP        VATNSP
318000140423     C                   MOVEL     'S'           VATTRC
318100140423      *
318200140423      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
318300140423      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
318400140423     C                   eval       VATNOT =
318500140423     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
318600140423      *
318700140423      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
318800140423      * seguenti attivano o meno il servizio x SMS o MAIL
318900140423      *
319000140423      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
319100140423      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
319200140423     C                   eval      %subst(VATNOT:17:1) = 'S'
319300140423     c                   if        FTX_SMS_come_servizio = 'N'
319400140423     c                              or
319500140423     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
319600140423     c                              = *blank
319700140423     C                   eval      %subst(VATNOT:17:1) = 'N'
319800140423     c                   end
319900140423      *
320000140423     C                   eval      %subst(VATNOT:18:1) = 'S'
320100140423     c                   if        FTX_MAIL_come_servizio = 'N'
320200140423     c                              or
320300140423     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
320400140423     c                              = *blank
320500140423     C                   eval      %subst(VATNOT:18:1) = 'N'
320600140423     c                   end
320700140423      *
320800140423     C                   WRITE     EDiVAT00                             99
320900140423     c   99              eval      errori_in_Wrt = 'S'
321000140423     c                   end
321100140423      *
321200090211     C                   ENDSR
321300090211     C*----------------------------------------------------------------
321400090211     C*? ESEGUO SCRITTURA EDiVAT
321500090211     C*----------------------------------------------------------------
321600090211     C     WRTVAT        BEGSR
321700090211      *
321800090211     C                   DO        §§            X
321900090211     C                   Z-ADD     VABCNT        VATCNT
322000090211     C                   MOVEL     VABCMR        VATCMR
322100090211     C                   Z-ADD     VABCCM        VATCCM
322200090211     C                   movel     VABfgs        VATfgs
322300090211     C                   Z-ADD     VABLNP        VATLNP
322400090211     C                   Z-ADD     VABAAS        VATAAS
322500090211     C                   Z-ADD     VABNRS        VATNRS
322600090211     C                   Z-ADD     VABNSP        VATNSP
322700090211     C                   MOVEL     'E'           VATTRC
322800090211      *
322900090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
323000090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
323100090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
323200090211     c                   if        WVATlun > 0
323300090211      *
323400090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
323500090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
323600090211      *
323700090211     c                   else
323800090211      *
323900090211      * Se non è impostato nulla prende il segnacollo passato così com'è
324000090211     C                   MOVEL     SGE(X)        VATNOT
324100090211     c                   end
324200090211      *
324300090213     C                   WRITE     EDiVAT00                             99
324400090213     c   99              eval      errori_in_Wrt = 'S'
324500090211     C                   END
324600090211      *
324700090211     C                   ENDSR
324800090211     C*----------------------------------------------------------------
324900090211     C*? ESEGUO SCRITTURA EDSUM
325000090211     C*----------------------------------------------------------------
325100090211     C     WRTSUM        BEGSR
325200090211     C*
325300090211     C* Se non seq. scrivo xy record
325400090211     C                   DO        XY            X
325500090211     C                   Z-ADD     VABAAS        SUMAAS
325600090211     C                   Z-ADD     VABLNP        SUMFLS
325700090211     C                   MOVEL     WKSC          SUMLNP
325800090211     C                   Z-ADD     VABLNA        SUMLNA
325900090211     C                   Z-ADD     VABZNC        SUMZNC
326000090211     C                   Z-ADD     VABNRS        SUMNRS
326100090211     C                   Z-ADD     VABNSP        SUMNSP
326200090211     C                   Z-ADD     SGN(X)        SUMNSC
326300090211     C                   Z-ADD     WKSC          SUMCCM
326400090211     C                   MOVEL     *BLANKS       SUMSTS
326500090211     C                   MOVEL     *BLANKS       SUMFLG
326600090211     C                   MOVEL     WPRGSP        SUMCNI
326700090211     C                   MOVEL     VABCMR        SUMCMR
326800090211     C                   Z-ADD     0             SUMNFE
326900090211     C                   Z-ADD     VABCCM        VADCCM
327000090213     C                   WRITE     EDSUM000                             99
327100090213     c   99              eval      errori_in_Wrt = 'S'
327200090211     C                   END
327300090211     C*
327400090211     C                   ENDSR
327500090211     c*==================================================================*
327600090211      * Manda un Msg in Posta AS Sede a EDP*
327700090211     c*==================================================================*
327800090211     c     SEND_MSG_EDP  begsr
327900090211      *
328000090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
328100090211      *   Lo manda una sola volta per lavoro
328200090211     c                   IF        Snd_Msg_alert  <> 'N' and
328300090211     c                             User_msg <> *blank
328400090211     c                   z-add     1             Jx
328500090211     C                   exsr      CalQEZSNDMG
328600090211     c                   end
328700090211      *
328800090211     c                   endsr
328900090213     c**********************************************************************
329000090213     c* reperimento numero Spedizione
329100090213     c**********************************************************************
329200090213     C     repnum        BEGSR
329300090213      *
329400090213      *    deve controllare sulla tabella "3C" se il numero spedizione
329500090213      *     deve essere mantenuto oppure no
329600090213     C*
329700090213     C                   clear                   Ds3C
329800090213     C                   Z-ADD     CODUT         TBLKUT
329900090213     C                   MOVEL     '3C'          TBLCOD
330000090213     C                   movel(p)  VABccm        TBLKEY
330100090213     C     KTABel        CHAIN     TABEL00F                           30
330200090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
330300090213     C                   MOVEL     TBLUNI        Ds3C
330400090213     C                   END
330500090213      *
330600090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
330700090213     c                   if        §3CFsp <> 'S'
330800090213      *
330900090213     C* NSP => Stacco un numeratore da AZNUM
331000090213     c                   movel     kpjbu         svkpjbu
331100090213     c                   clear                   kpjbu
331200090213     C                   clear                   TRUL33DS
331300090213     C                   eval      I33OPE = *zeros
331400090213     C                   eval      I33CNU = 302
331500090213     C                   eval      I33NUM = 1
331600090213     C                   movel     TRUL33DS      KPJBU
331700090213     C                   call      'TRUL33R'
331800090213     C                   parm                    KPJBA
331900090213     C                   movel     KPJBU         TRUL33DS
332000090213     c                   movel     svkpjbu       kpjbu
332100090213      ****
332200090213     C                   if        O33ERR = *zeros
332300090213     c                   z-add     o33nrf        NUM_SPED
332400090213     C                   else
332500090213     C                   endif
332600090213      ****
332700090213     c                   ELSE
332800090213      *   se si vuole mantenere il numero spedizione
332900090213      ****
333000090213     C                   MOVEL     WOGGI         WANNO             4 0
333100090213     C                   MOVE      WANNO         KAAA
333200090213     C                   Z-ADD     3             KCNU
333300090213     C                   Z-ADD     VABlnp        KFIL
333400090213     C     KNUF          CHAIN     FLNUF                              9999
333500090213     C     *IN99         IFEQ      '0'
333600090213     C                   ADD       1             NUFNUM
333700090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
333800090213     C                   UPDATE    FLNUF
333900090213     C                   ELSE
334000090213     C                   END
334100090213     C*
334200090213     c                   EndIF
334300090213      **
334400090213     C                   ENDSR
334500090213     C*----------------------------------------------------------------
334600090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
334700090213     C*----------------------------------------------------------------
334800090213     C     MEMCMR        BEGSR
334900090213     C*
335000090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
335100090213     C     §PTCM1        IFEQ      'S'
335200090213     C                   CLEAR                   EDRDE000
335300090213     C                   MOVEL     'TD1154'      KNMC
335400090213     C                   Z-ADD     1             KNRP
335500090213     C                   MOVEL     WKSC          KLNP1
335600090213     C     KRDE          CHAIN     EDRDE01L                           31
335700090213     C     *IN31         IFEQ      '1'
335800090213     C                   Z-ADD     VABAAS        RDEAAS
335900090213     C                   Z-ADD     VABLNP        RDELNP
336000090213     C                   Z-ADD     VABNRS        RDENRS
336100090213     C                   Z-ADD     VABNSP        RDENSP
336200090213     C                   MOVEL     'TD1154'      RDENMC
336300090213     C                   Z-ADD     1             RDENRP
336400090213     C                   MOVEL     WNRCMR        RDEVAL
336500090213     C                   WRITE     EDRDE000                             99
336600090213     c   99              eval      errori_in_Wrt = 'S'
336700090213     C                   END
336800090213     C*  Memorizzo qualificatore CMR
336900090213     C                   CLEAR                   EDRDE000
337000090213     C                   MOVEL     'TD1153'      KNMC
337100090213     C                   Z-ADD     1             KNRP
337200090213     C                   MOVEL     WKSC          KLNP1
337300090213     C     KRDE          CHAIN     EDRDE01L                           31
337400090213     C     *IN31         IFEQ      '1'
337500090213     C                   Z-ADD     VABAAS        RDEAAS
337600090213     C                   Z-ADD     VABLNP        RDELNP
337700090213     C                   Z-ADD     VABNRS        RDENRS
337800090213     C                   Z-ADD     VABNSP        RDENSP
337900090213     C                   Z-ADD     1             RDENRP
338000090213     C                   MOVEL     'TD1153'      RDENMC
338100090213     C                   MOVEL     'CMR'         RDEVAL
338200090213     C                   WRITE     EDRDE000                             99
338300090213     c   99              eval      errori_in_Wrt = 'S'
338400090213     C                   END
338500090213     C*  Memorizzo la data
338600090213     C                   CLEAR                   EDRDE000
338700090213     C                   MOVEL     'TD2380'      KNMC
338800090213     C                   Z-ADD     1             KNRP
338900090213     C                   MOVEL     WKSC          KLNP1
339000090213     C     KRDE          CHAIN     EDRDE01L                           31
339100090213     C     *IN31         IFEQ      '1'
339200090213     C                   Z-ADD     VABAAS        RDEAAS
339300090213     C                   Z-ADD     VABLNP        RDELNP
339400090213     C                   Z-ADD     VABNRS        RDENRS
339500090213     C                   Z-ADD     VABNSP        RDENSP
339600090213     C                   Z-ADD     1             RDENRP
339700090213     C                   MOVEL     'TD2380'      RDENMC
339800090213     C                   MOVEL     WDTCMR        RDEVAL
339900090213     C                   WRITE     EDRDE000                             99
340000090213     c   99              eval      errori_in_Wrt = 'S'
340100090213     C                   END
340200090213     C*
340300090213     C                   END                                                    §PTCM1 = 'S'
340400090213     C*
340500090213     C                   ENDSR
340600090213     C*----------------------------------------------------------------
340700090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
340800090213     C*----------------------------------------------------------------
340900090213     C     MEMAFB        BEGSR
341000090213     C*
341100090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
341200090213     C     §PTNF1        IFEQ      'S'
341300090213     C                   CLEAR                   EDRDE000
341400090213     C                   MOVEL     WKSC          KLNP1
341500090213     C                   MOVEL     'TD1154'      KNMC
341600090213     C                   Z-ADD     1             KNRP
341700090213     C     KRDE          CHAIN     EDRDE01L                           31
341800090213     C     *IN31         IFEQ      '1'
341900090213     C                   Z-ADD     VABAAS        RDEAAS
342000090213     C                   Z-ADD     VABLNP        RDELNP
342100090213     C                   Z-ADD     VABNRS        RDENRS
342200090213     C                   Z-ADD     VABNSP        RDENSP
342300090213     C                   MOVEL     'TD1154'      RDENMC
342400090213     C     §PTCM1        IFNE      'S'
342500090213     C     WNRCMR        OREQ      *BLANKS
342600090213     C                   Z-ADD     1             RDENRP
342700090213     C                   ELSE
342800090213     C                   Z-ADD     2             RDENRP
342900090213     C                   END
343000090213     C                   MOVEL     WNUMFV        RDEVAL
343100090213     C                   WRITE     EDRDE000                             99
343200090213     c   99              eval      errori_in_Wrt = 'S'
343300090213     C                   END
343400090213     C*  Memorizzo qualificatore CMR
343500090213     C                   CLEAR                   EDRDE000
343600090213     C                   MOVEL     WKSC          KLNP1
343700090213     C                   MOVEL     'TD1153'      KNMC
343800090213     C                   Z-ADD     1             KNRP
343900090213     C     KRDE          CHAIN     EDRDE01L                           31
344000090213     C     *IN31         IFEQ      '1'
344100090213     C                   Z-ADD     VABAAS        RDEAAS
344200090213     C                   Z-ADD     VABLNP        RDELNP
344300090213     C                   Z-ADD     VABNRS        RDENRS
344400090213     C                   Z-ADD     VABNSP        RDENSP
344500090213     C     §PTCM1        IFNE      'S'
344600090213     C     WNRCMR        OREQ      *BLANKS
344700090213     C                   Z-ADD     1             RDENRP
344800090213     C                   ELSE
344900090213     C                   Z-ADD     2             RDENRP
345000090213     C                   END
345100090213     C                   MOVEL     'TD1153'      RDENMC
345200090213     C                   MOVEL     'AFB'         RDEVAL
345300090213     C                   WRITE     EDRDE000                             99
345400090213     c   99              eval      errori_in_Wrt = 'S'
345500090213     C                   END
345600090213     C*  Memorizzo la data
345700090213     C                   CLEAR                   EDRDE000
345800090213     C                   MOVEL     WKSC          KLNP1
345900090213     C                   MOVEL     'TD2380'      KNMC
346000090213     C                   Z-ADD     1             KNRP
346100090213     C     KRDE          CHAIN     EDRDE01L                           31
346200090213     C     *IN31         IFEQ      '1'
346300090213     C                   Z-ADD     VABAAS        RDEAAS
346400090213     C                   Z-ADD     VABLNP        RDELNP
346500090213     C                   Z-ADD     VABNRS        RDENRS
346600090213     C                   Z-ADD     VABNSP        RDENSP
346700090213     C     §PTCM1        IFNE      'S'
346800090213     C     WNRCMR        OREQ      *BLANKS
346900090213     C                   Z-ADD     1             RDENRP
347000090213     C                   ELSE
347100090213     C                   Z-ADD     2             RDENRP
347200090213     C                   END
347300090213     C                   MOVEL     'TD2380'      RDENMC
347400090213     C                   MOVEL     WDATFV        RDEVAL
347500090213     C                   WRITE     EDRDE000                             99
347600090213     c   99              eval      errori_in_Wrt = 'S'
347700090213     C                   END
347800090213     C*
347900090213     C                   END                                                    §PTCM1 = 'S'
348000090213     C*
348100090213     C                   ENDSR
348200090213     c*==================================================================*
348300090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
348400090213     c*==================================================================*
348500090213     c     Imp_ADDR_mail begsr
348600090213      *
348700090213     c                   clear                   Indirizzi
348800090213      *
348900090213      *  Lunghezza indirizzi presenti
349000090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
349100090213     c                   z-add     1             al_char           3 0
349200090213     c                   z-add     1             dal_char          3 0
349300090213     c                   z-add     1             tot_char          3 0
349400090213      *
349500090213     c     successivo    tag
349600090213      *
349700090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
349800090213      *   e aggiunge il dominio Bartolini + il (;) separatore
349900090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
350000090213     c                   eval      tot_char = al_char - dal_char
350100090213     c                   z-add     dal_char      Dac
350200090213     c                   z-add     tot_char      Luc
350300090213      *
350400090213     c                   clear                   Indir_Bartol     40
350500090213     c                   clear                   Indir_Parz       20
350600090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
350700090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
350800090213     c                             %Trim(bart_it)
350900090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
351000090213      *
351100090213     c                   if        al_char = Lun_Ind
351200090213     c                   goto      fine_indmail
351300090213     c                   else
351400090213     c                   eval      dal_char = ( al_char + 1 )
351500090213     c                   goto      successivo
351600090213     c                   end
351700090213      *
351800090213     C     fine_indmail  TAG
351900090213      *
352000090213     C                   ENDSR
352100090213     c*==================================================================*
352200090213      * Manda un Msg x E-mail
352300090213     c*==================================================================*
352400090213     c     Invio_Mail    begsr
352500090213      *
352600090213     c                   goto      non_inviare
352700090213      *
352800090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
352900090213     C* Inizializzo variabili
353000090213     C                   movel     *blanks       wrkEml          253
353100090213     C                   movel     *blanks       wrkMsg         5000
353200090213     C                   movel     *blanks       wrkOgg           44
353300090213      *
353400090213     C* Valorizzo i campi della e-m@ail - indirizzo
353500090213     C                   eval      wrkEml = Indirizzi
353600090213     C                   eval      wrkOgg = Oggetto
353700090213     C                   eval      wrkMsg = Messaggio
353800090213
353900090213     C                   call(e)   'TIS701C'
354000090213     C                   parm                    wrkEml
354100090213     C                   parm                    wrkOgg
354200090213     C                   parm                    wrkMsg
354300090213
354400090213     c     non_inviare   tag
354500090213     C*
354600090213     C                   ENDSR
354700090603     c*==================================================================*
354800090603      * Manda un Msg x E-mail a EDPAB
354900090603     c*==================================================================*
355000090603     c     Invio_Mail_AB begsr
355100090603      *
355200090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
355300090603     C* Inizializzo variabili
355400090603     C                   movel     *blanks       wrkEml          253
355500090603     C                   movel     *blanks       wrkMsg         5000
355600090603     C                   movel     *blanks       wrkOgg           44
355700090603      *
355800090603     C* Valorizzo i campi della e-m@ail - indirizzo
355900110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
356000110203     C                   eval      wrkEml = Indirizzi
356100090603     C                   eval      wrkOgg = Oggetto
356200090603     C                   eval      wrkMsg = Messaggio
356300110203      ********
356400110203     C********           call(e)   'TIS701C'
356500110203     C********           parm                    wrkEml
356600110203     C********           parm                    wrkOgg
356700110203     C********           parm                    wrkMsg
356800110203     C*
356900110203      ** *****
357000110203     C                   call(e)   'TRTCT00R2'
357100110203     C                   parm                    wrkEml
357200110203     C                   parm                    wrkOgg
357300110203     C                   parm                    wrkMsg
357400090603     C*
357500090603     C                   ENDSR
357600090211     ***********************************************************************
357700090211     **
357800090211     ** Send Message (QEZSNDMG) API
357900090211     **
358000090211     ***********************************************************************
358100090211     C     CalQEZSNDMG   BEGSR
358200090211      *
358300090211     ** Invio messaggio all'utente.
358400090211     C                   EVAL      SndMg03 = msgDSP
358500090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
358600090211     C                   EVAL      SndMg05(Jx) = User_msg
358700090211     C                   EVAL      SndMg06 = Jx
358800090211     C                   CLEAR                   QUSEC
358900090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
359000090211
359100090211     C                   CALL      'QEZSNDMG'
359200090211     C                   PARM                    SndMg01
359300090211     C                   PARM                    SndMg02
359400090211     C                   PARM                    SndMg03
359500090211     C                   PARM                    SndMg04
359600090211     C                   PARM                    SndMg05
359700090211     C                   PARM                    SndMg06
359800090211     C                   PARM                    SndMg07
359900090211     C                   PARM                    SndMg08
360000090211     C                   PARM                    Qusec
360100090211     C                   PARM                    SndMg10
360200090211     C                   PARM                    SndMg11
360300090211     C                   PARM                    SndMg12
360400090211      *
360500090211     C                   ENDSR
360600121105      *---------------------------------------------------------------*
360700121105      * CTRL caricamento schiere    PT                               -*
360800121105      *---------------------------------------------------------------*
360900121105     C     Check_PT      begsr
361000121105     C*
361100121105     c* Controllo riempimento schiera
361200121105     c                   clear                   trul0sds
361300121105     c                   eval      i0sski='CPT'
361400121105     c                   eval      i0sele=%elem(CPT)
361500121105     c                   eval      i0spie=x
361600121105     c                   eval      i0sfile='EDTAB00F'
361700121105     c                   eval      i0ssif=knsif
361800121105     c                   eval      i0spgm='TRTCT04R'
361900121113     c                   move      kpjbu         SvKpjbu
362000121113     c                   clear                   kpjbu
362100121105     c                   movel     trul0sds      kpjbu
362200121105     c                   call      'TRUL0SR'
362300121105     c                   parm                    kpjba
362400121113     c                   eval      kpjbu  =  SvKpjbu
362500121105     C*
362600121105     C                   ENDSR
362700090211     O*****************************************************************
362800090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
362900090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
363000090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
363100090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
