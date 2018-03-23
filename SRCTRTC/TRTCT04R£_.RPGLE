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
011500090209      *---------------------------------------------------------------------*
011600090209     D KPJBA         E DS
011700090209     D FNLV55DS      E DS
011800090209     D TIBS55DS      E DS
011900090209     D TISI95DS      E DS
012000090209     D UTEDSE0F      E DS
012100090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012200090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012300090209      * Ds scomposzione tipo capoconti
012400090209     D TCUDS           DS
012500090209     D  F34                    3      4
012600090209     D  F56                    5      6
012700090209     D DS1B          E DS
012800090209     D DS15          E DS
012900090209     D DSCV          E DS
013000090213     D TRTC86DS      E DS
013100090209     D EDIDSPT       E DS
013200090209     D EDIDSPU       E DS
013300090209     D EDIDSPV       E DS
013400090211     D EDIDSCL       E DS
013500090216     D EDIDSTC       E DS
013600090209      **
013700090209      *  DS x salvataggio dati impostati in EDiVAB
013800090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
013900090209     D SALVA           DS           545
014000090210      **
014100090209     D WLBDA8          DS
014200090209     D  G02DAT                 1      8  0
014300090209     D  G02INV                 9     16  0
014400090209     D  G02ERR                17     17
014500090209     D  G02TGI                18     22  0
014600090209      *  DS x scomposizione numero spedizione
014700090209     D DSSPE           DS
014800090209     D  SPEAAS                 1      4  0
014900090209     D  SPELNP                 5      7  0
015000090209     D  SPENRS                 8      9  0
015100090209     D  SPENSP                10     16  0
015200090209      *  DS x scomposizione segnacollo
015300090209     D DSSGN           DS
015400090209     D  SGNLNP                 1      3  0
015500090209     D  SGNLNA                 4      6  0
015600090209     D  SGNNRS                 7      8  0
015700090209     D  SGNNSC                 9     15  0
015800090209     D  SGNZNC                16     17  0
015900090209      *  DS x stampa segnacollo
016000090209      *---------------                                                      *
016100090211     D  vablnp_PT      s                   LIKE(vablnp)
016200090212     D  vabnrs_PT      s                   LIKE(vabnrs)
016300090211     D  vabctm_PT      s                   LIKE(vabctm)
016400090209     D Wvabtic         s                   LIKE(vabtic)
016500090603     d tes_VABrmo      s                   LIKE(VABrmo)
016600090603     d tes_VABnmo      s                   LIKE(VABnmo)
016700090603     d tes_VABcmo      s                   LIKE(VABcmo)
016800090209     C*
016900090603     d tes_VABrsd      s                   LIKE(VABrsd)
017000090603     d tes_VABrd2      s                   LIKE(VABrd2)
017100090603     d tes_VABind      s                   LIKE(VABind)
017200090603     d tes_VABlod      s                   LIKE(VABlod)
017300090603     d tes_VABnzd      s                   LIKE(VABnzd)
017400090603     C*
017500090603     d tes_Valuta      s                   LIKE(VABvca)
017600090603     C*
017700090209      *---------------------------------------------------------------------*
017800090209      * Schiere
017900090209      *---------------------------------------------------------------------*
018000090209      * Schiera per reperimento nr.seganacollo
018100090209     D SGN             S              7  0 DIM(5000)
018200090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
018300090209      * Stringhe per conversione alfanumerico/numerico N.Documento
018400090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
018500090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
018600090209      * Nr. documento alfanumerico da decodificare
018700090209     D NDA             S              1    DIM(35)
018800090209      * Nr. documento alfanumerico da già decodificato
018900090209     D NDN             S              1    DIM(15)
019000090209      * Messaggi di errore
019100090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
019200090209      * Tabella codici trattamento merce (1B)
019300090209     D CTM             S              2    DIM(200)
019400090209     D DTM             S             67    DIM(200)
019500090209      * Tabella codici valuta
019600090209     D CCV             S              3    DIM(200)
019700090209     D DCV             S             89    DIM(200)
019800090209      * Tabella codici nazioni
019900090209     D C15             S              3    DIM(500)
020000090209     D ISO             S              3    DIM(500)
020100090209     D CPF             S              2  0 DIM(500)
020200090209      * Tabella partner
020300121105     D CPT             S             35    DIM(200)
020400121105     D CPTparz         S             35    DIM(%elem(CPT))
020500121105     D KSC_UNB         S              7    DIM(%elem(CPT))
020600121105     D DPT             S             90    DIM(%elem(CPT))
020700121105     D DPU             S             90    DIM(%elem(CPT))
020800121105     D DPV             S             90    DIM(%elem(CPT))
020900121105     D TRUL0SDS      E DS
021000121105      **
021100090209     d Ptn_parziale    s              1    inz('N')
021200090209     d sav_CPTx        s              3  0 inz(0)
021300090209      *
021400090209      * Tabella CL --> codici clienti - tariffa
021500130305     D CCL             S             35    DIM(999)
021600130305     D DCL             S             90    DIM(999)
021700090209      * Schiere x caricamento cod.cliente  <>
021800130305     D KCL             S              7  0 DIM(999)
021900090209      * Termini di consegna
022000110328     D K35_TpBolla     S             35    DIM(100)
022100110328     D Cod_TpBolla     S              3    DIM(100)
022200110328     D Decod_Porto     S              1    DIM(100)
022300110328     d Trovata_Tab_TB  S              1
022400110328      *
022500090216      * Tipo pagamento C/Assegno
022600090216     D CTC35           S             35    DIM(100)
022700090216     D CTC             S              2    DIM(100)
022800090216     D TPI             S              2    DIM(100)
022900090209      * Decodifiche servizi speciali
023000090209     D SS35            S             35    DIM(100)
023100090209     D SS              S              3    DIM(100)
023200090209     D DSS             S             90    DIM(100)
023300090209      * Schiere x routine di conversione CAP
023400090209     D CAP5            S              1    DIM(5)
023500090209     D CAP9            S              1    DIM(9)
023600090209     D CAPM            S              1    DIM(9)
023700090209      * Schiera per memorizzazione FTX ricevuti
023800090209     D QUA             S              3    DIM(100)
023900090209     D FTX             S             70    DIM(100)
024000090209      * Tabelle capiconto
024100090209      * Schiere x località
024200090209     D LOD             S              1    DIM(35)
024300060608      **?------------------------------------------------------------------ */
024400090601     C*? Ds Scrittura del VAT "E"
024500060608      **?------------------------------------------------------------------ */
024600060612     D XTOEBCDDS     E DS
024700060620      *
024800060608      **?------------------------------------------------------------------ */
024900050112      * Numeratore Bolle (302)
025000050112     D trul33ds      E DS
025100050112     D Ds3C          E DS
025200090209     C*
025300090209      **?------------------------------------------------------------------ */
025400090209     C* Definizione campi di work
025500090209     D pVLB            s                   LIKE(VABVLB)
025600090209     D kKUT            s                   LIKE(TBLKUT)
025700090209     D kCOD            s                   LIKE(TBLCOD)
025800090209     D kKEY            s                   LIKE(TBLKEY)
025900090209     D kKCC            s                   LIKE(ACOKCC)
026000090209     D kKSC            s                   LIKE(ACOKSC)
026100090209     D kAAA            s                   LIKE(NUFAAA)
026200090209     D kCNU            s                   LIKE(NUFCNU)
026300090209     D kFIL            s                   LIKE(NUFFIL)
026400090209     D kAAS            s                   LIKE(VABAAS)
026500090209     D kLNP            s                   LIKE(VABLNP)
026600090209     D kNRS            s                   LIKE(VABNRS)
026700090209     D kNSP            s                   LIKE(VABNSP)
026800090209     D kCMR            s                   LIKE(VABCMR)
026900090209     D kCCM            s                   LIKE(VABCCM)
027000090209     D kNCD            s                   LIKE(VADNCD)
027100090210     D kCNT            s                   LIKE(VADCNT)
027200090209     D kNMC            s                   LIKE(RDENMC)
027300090209     D kNRP            s                   LIKE(RDENRP)
027400090209     D kLNP1           s                   LIKE(RDELNP)
027500090209      *
027600090209      *  Invio E-mail
027700090209     D Indirizzi       s            253
027800090209     D Oggetto         s             44
027900090209     D Messaggio       s           5000
028000090209      *
028100090209     d   DSmail        ds
028200090209     D Mail_Ind                      44
028300090209     d   IND_char                     1    dim(44)
028400090209      *
028500090209     D Lun_Ind         s              5u 0
028600090209      *
028700090209     D Dac             s              5u 0
028800090209     D Luc             s              5u 0
028900090209      *
029000090209     C*---------------------------------------------------------------*
029100090209     D MsgDSP          S            256                                         Testo generico
029200090209     C*---------------------------------------------------------------*
029300090209     D SndMg01         S             10                                         Message type
029400090209     D                                     INZ('*INFO')
029500090209     D SndMg02         S             10                                         Deluvery mode
029600090209     D                                     INZ('*BREAK')
029700090209     D SndMg03         S            256                                         Message text
029800090209     D SndMg04         S             10I 0                                      Length of text
029900090209     D                                     INZ(%SIZE(SndMg03))
030000090209     D SndMg05         S             10                                         User profile
030100090209     D                                     DIM(299)
030200090209     D SndMg06         S             10I 0                                      Number of user
030300090209     D SndMg07         S             10I 0                                      Message sent indic.
030400090209     D SndMg08         S             10I 0                                      Function requested
030500090209     D SndMg10         S              1                                         Show display
030600090209     D                                     INZ('N')
030700090209     D SndMg11         S             20                                         Qualified MSGQ name
030800090209     D SndMg12         S              4                                         Name type indicator
030900090209     D                                     INZ('*USR')
031000090209      * indice di scihera
031100090209     D Jx              S              5I 0
031200090209      *
031300090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031400090209      *
031500090209     d bart_it         C                   CONST('@Bartolini.it;')
031600090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031700060612      * ?================================================================== */
031800060612      *
031900060612      * ?   * Campi x decodifica * (INPUT  del Record)
032000090130     D  Dati           s           2048
032100060612      * ?   *--------------------------------------------------------------*
032200060612     D  se_errore      s              1    inz(' ')
032300060612      * ?* ------------------------------------------------------ *
032400060710     D Digits          C                   '0123456789'
032500091019     D*------------------
032600091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
032700091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
032800060612      * ?================================================================== */
032900060612      *   Ciclo principale
033000090205      * ?================================================================== */
033100130321     c                   clear                   manda_mail        1
033200060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
033300060612     C*
033400060612     C                   if        not %open(tivin00r)
033500060612     C                   open      tivin00r
033600060612     C                   endif
033700060612      **
033800060612      * ?------------------------------------------------------------------ */
033900060612     C*? Controllo dati arrivati da DPD
034000060612      * ?------------------------------------------------------------------ */
034100060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
034200060612      * ?- controllando che la trasmissione sia completa.
034300090205      **
034400060612      * ?              /*---------------------- */
034500090205     c                   goto      nonFare_adesso
034600060612     c                   exsr      check_Trasm
034700090205     c     nonFare_adessotag
034800060612      * ?              /*---------------------- */
034900090205      **
035000060613      **  Errore di trasmissione x tutti i records
035100060613      **   --> file in errore
035200060613     c                   if        se_errore = 'S'
035300060612
035400060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
035500090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
035600090205     c                   exsr      Msg_errato
035700060612      **
035800060612     c                   else
035900060614      **
036000060612      * ?------------------------------------------------------------------ */
036100060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
036200060612      * ?------------------------------------------------------------------ */
036300090205      **
036400060612      * ?              /*---------------------- */
036500060612     c                   exsr      Importa_Msg
036600060612      * ?              /*---------------------- */
036700060612
036800060614     c                   end
036900060614
037000060612     C                   if        %open(tivin00r)
037100060612     C                   close     tivin00r
037200060612     C                   endif
037300060612      *
037400060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
037500090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
037600090225     c                             esito ='0'
037700060710     C                   eval      esito ='0'
037800090225      *
037900090225     c                   COMMIT
038000090224      *
038100090213     c                   if        almeno_un_due ='S'
038200090213     C                   eval      esito ='1'
038300060614     C                   endif
038400090213     C                   endif
038500060614      *
038600060612     c                   SETON                                        LR
038700060612      * ?================================================================== */
038800060612      *? Importa i records della tramsissione
038900060612      * ?------------------------------------------------------------------ */
039000060612     c     Importa_Msg   Begsr
039100060612
039200090211     C                   EXSR      AZZMSG
039300060614
039400060612     c     *start        setll     Tivin00r
039500060612     c                   read      Tivin00r
039600091016     c                   if        %Eof(tivin00r)
039700091016     c                   move      'S'           EoFTivin
039800091016     c                   end
039900060612
040000060612     c                   dow       not %eof(Tivin00r)
040100060614
040200060614      * solo i record sflaggati da rielaborare
040300060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
040400090211      *  Sempre Record OK
040500090211      ** Controlli formali sui campi
040600090211     C                   eval      vinFLG = '1'
040700060612     c                   clear                   se_errore
040800060612
040900060612      ** Decodifica record a campi variabili
041000060612      * ?              /*---------------------- */
041100090205     c                   exsr      Decod_Segmento
041200090209      * ?              /*---------------------- */
041300090209
041400090213      *** ------------------------------------
041500091019????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT" x IFTMIN
041600090213      *** ------------------------------------
041700090213      * ?              /*---------------------- */
041800090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
041900090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
042000090213      * ?              /*---------------------- */
042100090601      ***
042200091019????  **
042300091019????  *   OGNI fine DETTAGLIO è determinato dall'UNT.
042400091019???? c                   if           tipo_seg = 'UNT'
042500091019????  *
042600090213      *  Deve flaggare il dettaglio con dei problemi
042700090213     C                   if        Err_in_detta = 'S'
042800090213      **?              /*---------------------- */
042900090213     c                   exsr      DETta_Errato
043000090213      **?              /*---------------------- */
043100090213     c                   end
043200090213      *
043300060612      **  Se presente un errore nel record emette una segnalazione msg
043400091019???? c                   if        se_errore = 'S'
043500091019???? c                   else
043600091019????  ******
043700090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043800090227     c                   if         se_trovato_NAD ='S'
043900060612      * ?              /*---------------------- */
044000090209     c                   exsr      Scrive_VAB
044100060612      * ?              /*---------------------- */
044200090227     c                   else
044300090227     c                   eval      errori_in_Wrt = 'S'
044400090227     c                   end
044500090227      *
044600090209      *  Problemi nella decodifica dei campi VAB/VAT
044700090213     c                   if        errori_in_Wrt = 'S'
044800090213      *
044900090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
045000090213     c                   exsr      Error_DET
045100090213      *
045200090213     c                   ROLBK
045300090213      *
045400090213     c                   else
045500090601      *
045600090213     c                   COMMIT
045700090601      *
045800090227      * Dopo aver confermato, con COMMIT
045900090227      *  si prepara per una nuova spedizione
046000090227      * ?                         --------------
046100090227     c                   exsr      Nuova_spediz
046200090227      * ?                         --------------
046300090213     c                   end
046400090209
046500090209     c                   end
046600090210      **
046700060620     c                   end
046800060612
046900060621      *  x errore bloccante nella composizione del VAB/VAT
047000060621     c                   if        err_bloccante ='S'
047100060621     C                   eval      vinFLG = '2'
047200090211     c                   eval      esito  = '2'
047300091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
047400090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047500090603     C                             Bolle import in filiale - messaggio -
047600090603     C                             con UNB non presente in tabella PT controlla-
047700090603     C                             re EDI ricevuto su UPLOAD.'
047800090603     c                   exsr      invio_mail_AB
047900060621     c                   end
048000060612
048100091016     c                   if        EoFTivin = ' '
048200060612     c                   update    Tivin000
048300091016     c                   end
048400090211
048500090211      *** se non è riconosciuto l'UNB deve uscire direttamente
048600090211     c                   if        se_errore ='S'  and
048700090211     c                             tipo_seg = 'UNB'
048800090213     c                   eval      err_bloccante = 'S'
048900090211     c                   exsr      Msg_errato
049000090211     c                   LEAVE
049100090211     c                   endIF
049200090211      ***
049300060614     c                   endIF
049400060612
049500060612     c                   read      Tivin00r
049600091016     c                   if        %Eof(tivin00r)
049700091016     c                   move      'S'           EoFTivin
049800091016     c                   end
049900060612     C                   ENDdo
050000060612      **
050100060612     c                   endSR
050200100120      * ?------------------------------------------------------------------ */
050300100120      *? Controlla la trasmissione   se completa
050400100120      * ?------------------------------------------------------------------ */
050500100120     c     Check_Trasm   Begsr
050600100120
050700100120     C                   clear                   se_errore
050800100120     C                   clear                   Riga_iNIZIo       1
050900100120     C                   clear                   Riga_fINe         1
051000100120      ** primo  record
051100100120     c     *start        setll     tivin00r
051200100120      *
051300100120     c     rileggi       tag
051400100120     c                   read      tivin00r
051500100120     c                   if        %Eof(tivin00r)
051600100120     c                   move      'S'           EoFTivin
051700100120     c                   end
051800100120      *
051900100120     c                   if        not %eof(tivin00r)
052000100120      *
052100100120     c***********        movel(p)  VINDTA        dati
052200100120     c                   clear                   dati
052300100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
052400100120     c                   if        dati = *blank
052500100120      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
052600100120      * le leggo e le escludo.
052700100120     C                   eval      vinFLG = '1'
052800100120     c                   if        EoFTivin = ' '
052900100120     c                   update    Tivin000
053000100120     c                   end
053100100120     c                   goto      rileggi
053200100120     c                   end
053300100120      * ?              /*---------------------- */
053400100120     c                   exsr      Decod_Segmento
053500100120      * ?              /*---------------------- */
053600100120     c                   endif
053700100120      **
053800100120      ** ultimo record
053900100120     c     *hival        setll     tivin00r
054000100120     c     leggiAncora   tag
054100100120     c                   readp     tivin00r
054200100120     c                   if        %Eof(tivin00r)
054300100120     c                   move      'S'           EoFTivin
054400100120     c                   end
054500100120     c                   if        not %eof(tivin00r)
054600100120      *
054700100120     c***********        movel(p)  VINDTA        dati
054800100120     c                   clear                   dati
054900100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
055000100120     c                   if        dati = *blank
055100100120      * le ultime righe potrebbero essere vuote prima di finire il messaggio
055200100120      * le leggo e le escludo.
055300100120     C                   eval      vinFLG = '1'
055400100120     c                   if        EoFTivin = ' '
055500100120     c                   update    Tivin000
055600100120     c                   end
055700100120     c                   goto      leggiAncora
055800100120     c                   end
055900100120      * ?              /*---------------------- */
056000100120     c                   exsr      Decod_Segmento
056100100120      * ?              /*---------------------- */
056200100120     c                   endif
056300100120
056400100120      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
056500100120      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
056600100120      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
056700100120     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
056800100120      * ?-----> Errore
056900100120     C                   eval      se_errore = 'S'
057000100120     c                   end
057100100120
057200100120     c                   endSR
057300100120      * ?------------------------------------------------------------------ */
057400100120      *?    Decodifica il segmento ed importa i campi in formato DS
057500100120      * ?------------------------------------------------------------------ */
057600100120     c     Decod_SegmentoBegsr
057700100120      **
057800100120     c                   clear                   dati
057900100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
058000100120     c                   eval      tipo_seg = %subst(dati:1:3)
058100100120     c                   eval      segmento = dati
058200100120      *
058300100120      * Imposta il flag di Inizio Dettaglio
058400100120     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
058500100120     c                   eval      Inizio_Dettaglio ='S'
058600100120     c                   end
058700100120      *
058800100120     c                   clear                   seg_imprevisto
058900100720     c                   clear                   trovato_seg
059000100716      *
059100100716     C                   clear                   keyUNBCLI
059200100716     C                   clear                   keyTIPOMSG
059300100716     C                   clear                   keyVERSION
059400100716     C                   clear                   keyRELEASE
059500100716     C                   clear                   keyAGENCY
059600100716     C                   clear                   keyASSOCIA
059700100720      *
059800100720      *  Chiamata generica x decodificare il singolo segmento letto
059900100720     c                   call      'TRTCT00R1'
060000100720      * input
060100100720     c                   parm                    tipo_seg
060200100720     c                   parm                    segmento
060300100720     C                   parm                    keyUNBCLI
060400100720     C                   parm                    keyTIPOMSG
060500100720     C                   parm                    keyVERSION
060600100720     C                   parm                    keyRELEASE
060700100720     C                   parm                    keyAGENCY
060800100720     C                   parm                    keyASSOCIA
060900100720      *output
061000100720     c                   parm                    Errore
061100100720     c                   parm                    DsGenerica
061200100720     c                   parm                    Valori_inErr
061300100720     c                   parm                    Descr_Errore
061400100720     c                   parm                    trovato_seg
061500100720      **--------
061600100720      *  Ritorna la DS GENERICA oppure l'errore
061700100120      **--------
061800100120     c                   select
061900100720      *
062000100720     c                   when      trovato_seg ='N'
062100100720      *
062200100720     c                   move      'S'           seg_imprevisto
062300130321     c                   eval           manda_mail='S'
062400100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
062500100720     c                   exsr      Error_DET
062600100720     c                   goto      end_Decod
062700100720      **--------
062800100120     c                   when      tipo_seg = 'UNA'
062900100720     c                   movel(p)  DsGenerica    EDS00UNA
063000100120      *
063100100720      **--------
063200100120     c                   when      tipo_seg = 'UNB'
063300100720     c                   movel(p)  DsGenerica    EDS00UNB
063400100120     C                   EXSR      DATI_UNB
063500100120      * --------
063600100120     c                   when      tipo_seg = 'UNH'
063700100720     c                   movel(p)  DsGenerica    EDS00UNH
063800100120     C                   EXSR      DATI_UNH
063900100120      * --------
064000100120     c                   when      tipo_seg = 'BGM'
064100100720     c                   movel(p)  DsGenerica    EDS00BGM
064200100120      **--------
064300100120     c                   when      tipo_seg = 'DTM'
064400100720     c                   movel(p)  DsGenerica    EDS00DTM
064500100120     C                   EXSR      DATI_DTM
064600100120      * --------
064700100120     c                   when      tipo_seg = 'TSR'
064800100720     c                   movel(p)  DsGenerica    EDS00TSR
064900100120      **--------
065000100120     c                   when      tipo_seg = 'CNT'
065100100720     c                   movel(p)  DsGenerica    EDS00CNT
065200100120      **--------
065300100120     c                   when      tipo_seg = 'CUX'
065400100720     c                   movel(p)  DsGenerica    EDS00CUX
065500100120      * --------
065600100120     c                   when      tipo_seg = 'TOD'
065700100720     c                   movel(p)  DsGenerica    EDS00TOD
065800100120     C                   EXSR      DATI_TOD
065900100120      **--------
066000100120     c                   when      tipo_seg = 'RFF'
066100100720     c                   movel(p)  DsGenerica    EDS00RFF
066200100120     C                   EXSR      DATI_RFF
066300100120      **--------
066400100120     c                   when      tipo_seg = 'TDT'
066500100720     c                   movel(p)  DsGenerica    EDS00TDT
066600100120      **--------
066700100120     c                   when      tipo_seg = 'MOA'
066800100720     c                   movel(p)  DsGenerica    EDS00MOA
066900100120     C                   EXSR      DATI_MOA
067000100120      **--------
067100100120     c                   when      tipo_seg = 'FTX'
067200100720     c                   movel(p)  DsGenerica    EDS00FTX
067300100120     C                   EXSR      DATI_FTX
067400100120      **--------
067500100120     c                   when      tipo_seg = 'NAD'
067600100720     c                   movel(p)  DsGenerica    EDS00NAD
067700100120     C                   EXSR      DATI_NAD
067800100120      **--------
067900100120     c                   when      tipo_seg = 'CTA'
068000100720     c                   movel(p)  DsGenerica    EDS00CTA
068100100120     C                   EXSR      DATI_CTA
068200100120      **--------
068300100120     c                   when      tipo_seg = 'COM'
068400100720     c                   movel(p)  DsGenerica    EDS00COM
068500100120     C                   EXSR      DATI_COM
068600100120      **--------
068700100120     c                   when      tipo_seg = 'GID'
068800100720     c                   movel(p)  DsGenerica    EDS00GID
068900100120     C                   EXSR      DATI_GID
069000100120      **--------
069100100120     c                   when      tipo_seg = 'MEA'
069200100720     c                   movel(p)  DsGenerica    EDS00MEA
069300100120     C                   EXSR      DATI_MEA
069400100120      **--------
069500100120     c                   when      tipo_seg = 'PCI'
069600100720     c                   movel(p)  DsGenerica    EDS00PCI
069700100120     C                   EXSR      DATI_PCI
069800100120      **--------
069900100120     c                   when      tipo_seg = 'UNT'
070000100720     c                   movel(p)  DsGenerica    EDS00UNT
070100100120     C                   EXSR      DATI_UNT
070200100120      **--------
070300100120     c                   when      tipo_seg = 'UNZ'
070400100720     c                   movel(p)  DsGenerica    EDS00UNZ
070500100120     C                   EXSR      DATI_UNZ
070600100120      **--------
070700100120     c                   OTHER
070800100120      **--------
070900100120     c                   move      'S'           seg_imprevisto
071000100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
071100130321     c                   eval           manda_mail='S'
071200100120     c                   exsr      Error_DET
071300100120     c                   goto      end_Decod
071400100120      **--------
071500100120     c                   endSL
071600100120      **
071700100120     c     end_Decod     endSR
071800090211     C*----------------------------------------------------------------
071900090211     C*? AZZMSG - Azzera dati messaggio
072000090211     C*----------------------------------------------------------------
072100090211     C     AZZMSG        BEGSR
072200090211     C*
072300090211     C* Pulisco dati di work x foglio viaggio
072400090211     C                   MOVEL     *BLANKS       WNUMFV           35
072500090211     C                   MOVEL     *BLANKS       WNRCMR           35
072600090211     C                   Z-ADD     0             WDATPA            8 0
072700090211     C                   Z-ADD     0             WDATFV            8 0
072800090211     C                   Z-ADD     0             WHHNFV            4 0
072900090211     C                   Z-ADD     0             WDTCMR            8 0
073000090211     C                   Z-ADD     0             WHHCMR            4 0
073100090211      *
073200090211      * pulisce gli RFF+FF di testata
073300090211     C                   clear                   $CLKSC            7 0
073400090211     C                   clear                   $CLTAR            3
073500090211     C                   clear                   $CLLNP            3 0
073600090211     C                   clear                   $CLNRS            2 0
073700090211     C                   clear                   $CLCTM            2
073800090211     C                   clear                   $CLBS1            1
073900090211     C                   clear                   $CLfnsp           1            *blk/S
074000090211      *
074100090211     C*  Inizializzo variabile new documento
074200090211     C                   Z-ADD     0             KCL
074300090211     C                   Z-ADD     0             WW                3 0
074400090211     ** Inizializza campo di work
074500090211     c                   move      'N'           WGID99
074600090211      * per controllare se almeno un record è stato importato sul VAB
074700090211     c                   clear                   Almeno_Uno        1
074800090213     c                   clear                   Almeno_Un_Due     1
074900090211     c                   eval      esito  = '0'
075000090211     C*
075100090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
075200090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
075300090603     c                   eval      Dettaglio_NAD_destinatario = *blank
075400090603     c                   eval      NAD_destinatario_in_testata = *blank
075500090603     c                   eval      NAD_mittente_in_testata = *blank
075600090603     c                   eval      Inizio_Dettaglio = *blank
075700090603     C*
075800090603     c                   clear                   tes_VABrmo
075900090603     c                   clear                   tes_VABnmo
076000090603     c                   clear                   tes_VABcmo
076100090603     C*
076200090603     c                   clear                   tes_VABrsd
076300090603     c                   clear                   tes_VABrd2
076400090603     c                   clear                   tes_VABind
076500090603     c                   clear                   tes_VABlod
076600090603     c                   clear                   tes_VABnzd
076700090603     C*
076800090603     c                   clear                   tes_Valuta
076900090603     C*
077000090211     C                   ENDSR
077100090210     C*----------------------------------------------------------------
077200090210      *? DECODIFICA DATI UNB
077300090210     C*----------------------------------------------------------------
077400090210     C     DATI_UNB      BEGSR
077500090210      *
077600090209      *  INIZIALIZZA  dati fondamentali messaggio
077700090209     C                   clear                   EDIDSPT
077800090209     C                   clear                   EDIDSPU
077900090209     C                   clear                   EDIDSPV
078000090209     C                   clear                   wnrDOC           35
078100090209     C                   move      'N'           PT_ok             1
078200090209      ** ---------------------------------------------------------------
078300090209      * quindi 1° cosa:
078400090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
078500090209      *     senza questo il messaggio NON è trascodificabile
078600090209     C                   MOVEL     unb0004       WCOD             35
078700090209     C                   MOVE      unb000720     WCOD
078800090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
078900090209     C                   Z-ADD     1             XX                3 0
079000090209     C     WCOD          LOOKUP    CPT(XX)                                32
079100090209      *
079200090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
079300090209     C                   If        *IN32 = *off
079400090209      * ******  Errore generale sul Messaggio
079500090209      * Msg di allerta Traduzione
079600090209     C                   EVAL      Indirizzi = CED_Bart
079700091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
079800090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
079900090209     C                             Bolle import in filiale - messaggio -
080000090209     C                             con UNB non presente in tabella PT controlla-
080100090209     C                             re EDI ricevuto su UPLOAD.'
080200090209      *
080300130321     c                   eval           manda_mail='S'
080400090209     c                   exsr      invio_mail
080500090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
080600090210     C                   eval      vinFLG = '2'
080700090210     C                   eval      se_errore   = 'S'
080800090210     c                   goto      end_UNB
080900090209      *
081000090209     C                   move      'N'           PT_ok             1
081100090209      *
081200090209      *             *------------------------------*
081300090209     C                   eLSe
081400090209      *             *------------------------------*
081500090209      * Se tutto OK su tab.PT:
081600090209     C                   move      'S'           PT_ok             1
081700090209      *
081800090209     C                   MOVEL     DPT(XX)       EDIDSPT
081900090209     C                   MOVEL     DPU(XX)       EDIDSPU
082000090209     C                   MOVEL     DPV(XX)       EDIDSPV
082100091016     c                   clear                   Mittente         35
082200091016     c                   clear                   Mitt_Qualifier    4
082300091016     c                   clear                   Id_Messaggio     14
082400090226     C                   eval      Mittente       = unb0004
082500090226     C                   eval      Mitt_qualifier = unb000720
082600090226     C                   eval      Id_Messaggio   = UNB0022
082700090209      *
082800090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
082900090209      *  ricevuti:
083000090209     c                   movel     §PVema        mail_ind         44
083100090209      *
083200090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
083300090209     c                   if        mail_ind <> *blank
083400090209     c                   exsr      imp_ADDR_mail
083500090209     c                   end
083600090209      *
083700090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
083800090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
083900090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
084000090209     C                   if        §PULUN = *blank
084100090209     C                   eval      §PULUN = '00'
084200090209     c                   end
084300090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
084400090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
084500090209      *
084600090209     C                   Z-ADD     §PTKSC        WKSC              7 0
084700090211     C                   MOVEL     §PTLNP        VABlnp_PT
084800090212     C                   MOVEL     §PTNRS        VABnrs_PT
084900090211     C                   MOVEL     §PTCTM        VABctm_PT
085000090209      *
085100090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
085200090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
085300090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
085400090209      *  i records trasmessi).
085500090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
085600090209      *
085700090209      *  In base al cod.trattamento merce reperisco tipo tratt.
085800090209     C                   CLEAR                   DS1B
085900090209     C                   Z-ADD     1             Y                 3 0
086000090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
086100090209     C     *IN32         IFEQ      '1'
086200090209     C                   MOVEL     DTM(Y)        DS1B
086300090209     C                   MOVEL     DS1B          WSVTRM           38
086400090209     C                   END
086500090209      *
086600090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
086700090209     C                   clear                   ACORAG
086800090209     C                   clear                   INDCAE
086900090209     C                   clear                   INDCAP
087000090209     C                   clear                   INDSTA
087100090209      * Decodifico partner
087200090209     C                   Z-ADD     1             KKUT
087300090209     C                   Z-ADD     KCI           KKCC
087400090209     C                   MOVE      §PTKSC        KKSC
087500090209     C     KACO          CHAIN     CNACO00F                           31
087600090209     C     KIND          CHAIN     CNIND00F                           31
087700090211      *
087800090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
087900090211      *   questa schiera è utilizzata anche per i clienti.
088000090211     C     §ptKSC        LOOKUP    KCL                                    39
088100090211     c                   If        *in39 = *off
088200090211     C                   add       1             WW
088300090211     C                   z-add     §ptKSC        KCL(WW)
088400090211     C                   Endif
088500090209      *
088600090209     C                   MOVEL     unb0017       WAA4              4 0
088700090209     C                   MOVE      WAA4          WAA2              2 0
088800090209     C                   MOVE      unb0017       WGG               2 0
088900090209     C                   MOVE      unb0017       WDTPRE            8 0
089000090209     C                   MOVE      unb0019       WHMPRE            4 0
089100090209     C                   MOVE      unb0017       WDTMSG            6 0
089200090209     C                   MOVE      WAA2          WDTMSG
089300090209     C                   MOVEL     WGG           WDTMSG
089400090210      * potremmo
089500090210      * Utilizzare l'identificativo della trasmissione come CMR
089600090209     C                   IF        unb0022 <> *blank
089700090209     C                   MOVEL     unb0022       WNRDOC
089800090211     C                   MOVEL     unb0022       WNUMFV
089900090211     C                   MOVEL     unb0022       WNRCMR
090000090209     c                   end
090100090209      *
090200090209     c                   Endif
090300090209      **
090400090210     c     end_UNB       endSR
090500090209      * ?================================================================== */
090600090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
090700090209      * ?================================================================== */
090800090227     C     Nuova_spediz  BEGSR
090900090227      **
091000090227      * Pulisco campi del file
091100090227     c                   z-add     vnrec         SAVNUM_RECord
091200090227     C                   CLEAR                   EDiVAB00
091300090227     C                   CLEAR                   EDiVAT00
091400090227     C                   CLEAR                   EDiVAD00
091500090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
091600090227     C                   MOVEL     'N'           WGID99            1
091700090227     C                   clear                   XY                5 0
091800090227     C                   clear                   §§                5 0
091900090227     C                   clear                   WS1496            5 0
092000090227     C                   clear                   WNSP             35
092100090227     C                   Z-ADD     *ZEROS        PVLB
092200090227     C                   MOVEA     *HIVAL        SGN
092300090227     C*  Azzero codice cliente - tariffa
092400090227     C                   Z-ADD     0             WTOCOC           16 0
092500090227     C                   Z-ADD     0             WTOPLC           16 2          ???
092600090227     C                   Z-ADD     0             WTOPNC           16 2          ???
092700090227     C*  Azzero codice cliente - tariffa
092800090227     C                   clear                   WCLKSC
092900090227     C                   clear                   WCLTAR
093000090227     C                   clear                   WCLLNP
093100090227     C                   clear                   WCLNRS
093200090227     C                   clear                   WCLCTM
093300090227     C                   clear                   WCLBS1
093400090227     C                   clear                   WCLfnsp                        *blk/S
093500090227     C                   clear                   WNOT1            35
093600090227     C                   clear                   WNOT2            35
093700130422     C                   clear                   Note_1T          35
093800130422     C                   clear                   Note_2T          35
093900130422     c                   eval      FTX_ServizioMail_xDestinatario = *blank
094000090227     C*  Campi di work
094100090227     c                   clear                   wVabTIC
094200090227     c                   clear                   wri_vabtic        1
094300090227      *
094400090227     C* pulisce campi di work
094500090227     c                   clear                   watrde           35
094600090227     c                   clear                   wattel           25
094700090227     C                   clear                   WRMN              3
094800090227      *
094900090227      * Un errore nel dettaglio
095000090227     C                   clear                   Err_in_detta      1
095100090227     c                   clear                   errori_in_Wrt     1
095200090227     c                   clear                   se_trovato_NAD    1
095300090227      **
095400090227     c                   endSR
095500090227      * ?================================================================== */
095600090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
095700090227      * ?================================================================== */
095800090227     C     DATI_UNH      BEGSR
095900090227      *
096000090227      *  Prepara una nuova spedizione
096100090227      * ?                         --------------
096200090227     c                   exsr      Nuova_spediz
096300090227      * ?                         --------------
096400090211      *
096500090211      ** Riferimento del cliente
096600090211     C                   MOVEL     unh0062       WPRGSP           35
096700090211     C                   MOVEL     unh0062       WNSP             35
096800090211     C                   MOVEL     unh0062       WNUM             35
096900090211      * ?                                       --------
097000090211     C                   MOVEL     unh0062       VABRMA
097100090211      * ?                                       --------
097200090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
097300090211     C                   SUB       1             WLUNGH
097400090211     C                   EXSR      TSTNUM
097500090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
097600090211      * ?                                       --------
097700090211     C                   MOVEL     WNUMOK        VABRMN
097800090211      * ?                                       --------
097900090211     C                   END
098000090210      **
098100090303     C                   MOVEL     VABlnp_PT     VABLNP
098200090303     C                   MOVEL     VABnrs_PT     VABNRS
098300090303     C                   MOVEL     VABctm_PT     VABCTM
098400090303      **
098500090303      **
098600090210     c     end_UNH       endSR
098700090210      * ?================================================================== */
098800090601     C*? Imposta la VALUTA di eventuali C.O.D.
098900090210      * ?================================================================== */
099000090601     C     DATI_CUX      BEGSR
099100090210     C*
099200090603     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
099300090603     c                   if        cux6345 <> *blank
099400090603     C                   MOVEL     cux6345       Tes_Valuta
099500090603     c                   end
099600090601      **
099700090601     c     end_CUX       endSR
099800090601      * ?================================================================== */
099900090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
100000090601      * ?================================================================== */
100100090601     C     DATI_DTM      BEGSR
100200090601     C*
100300090209     C*  Se data documento  memorizzo numero e data x CMR
100400090603     C     dtm2005       IFEQ      '9'
100500090209     C                   MOVEL     dtm2380       WDATA            35            Data
100600090209     C                   MOVEL     dtm2379       WFORM             3            Formato
100700090209     C                   EXSR      CNVDAT
100800090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
100900090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
101000090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
101100090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
101200090209     C     WERRO         IFEQ      'S'
101300090210     C                   eval      vinMSG = 'DTM data errata'
101400090213     c                   exsr      Error_DET
101500090210     c                   goto      end_DTM
101600090209     C                   END
101700090209     C                   END
101800090209      **
101900090209     C*  Se data consegna richiesta imposto già sui campi del VAB
102000090603     C     dtm2005       IFEQ      'X'
102100090209     C                   MOVEL     dtm2380       WDATA            35            Data
102200090209     C                   MOVEL     dtm2379       WFORM             3            Formato
102300090209     C                   EXSR      CNVDAT
102400090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
102500090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
102600090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
102700090209     C     WERRO         IFEQ      'S'
102800090210     C                   eval      vinMSG = 'DTM data errata'
102900090213     c                   exsr      Error_DET
103000090210     c                   goto      end_DTM
103100090209     C                   END
103200090209     C                   END
103300090211      **
103400090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
103500090603     C     dtm2005       IFEQ      '10'
103600090211     C                   MOVEL     dtm2380       WDATA            35            Data
103700090211     C                   MOVEL     dtm2379       WFORM             3            Formato
103800090211     C                   EXSR      CNVDAT
103900090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
104000090211     C     WERRO         IFEQ      'S'
104100090211     C                   eval      vinMSG = 'DTM data errata'
104200090213     c                   exsr      Error_DET
104300090211     c                   goto      end_DTM
104400090211     C                   END
104500090211     C                   END
104600090209      **
104700090210     c     end_DTM       endSR
104800090209      * ?================================================================== */
104900090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
105000090209      * ?================================================================== */
105100090209     C     DATI_TOD      BEGSR
105200090209      *
105300090210      *  Trascodifico tipo trasporto
105400090210     c                   clear                   TOD_char3         3
105500090210      *
105600090210      *  imposta il default dalla tabella PT se c'è
105700090210     C     §puPFA        ifNE      *blank
105800090210     c                   SELECT
105900090210      *  Porto Franco
106000090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
106100090210     C                   movel     '4'           VABCBO                         + C/Ass
106200090210     c                   WHEN      §puPFA = 'F'
106300090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
106400090210      *  Porto Assegnato
106500090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
106600090210     C                   movel     '6'           VABCBO                         + C/Ass
106700090210     C                   WHEN      §puPFA = 'A'
106800090210     C                   movel     '2'           VABCBO                         Senza C/Ass
106900090210     C                   ENDSL
107000090210     c                   end
107100090210      *
107200090210      *   Imposta o il codice o il metodo
107300090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
107400090210     c                   if        tod4053  <> *blank
107500090210     c                   movel(p)  tod4053       TOD_char3
107600090210     c                   else
107700090210     c                   if        tod4215  <> *blank
107800090210     c                   movel(p)  tod4215       TOD_char3
107900090210     c                   end
108000090210     c                   end
108100110328      ****************
108200110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
108300110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
108400110328      ****************
108500110328     c                   eval      Trovata_Tab_TB ='N'
108600110328      *
108700110328     c                   if              TOD_char3 <> *blank
108800110328     c                   movel(p)  TOD_char3     Key_Generica35
108900110328     c                   move      §puTB         Key_Generica35
109000110328     C                   eval      Y = 1
109100110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
109200110328     c   32              eval      Trovata_Tab_TB ='S'
109300110328      *
109400110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
109500110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
109600110328      *
109700110328     c                   if          Trovata_Tab_TB ='N'
109800110328     c                                 and
109900110328     C                               tod4215 <> *blank
110000110328     c                   movel(p)  tod4215       TOD_char3
110100110328     c                   movel(p)  TOD_char3     Key_Generica35
110200110328     c                   move      §puTB         Key_Generica35
110300110328     C                   eval      Y = 1
110400110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
110500110328     c   32              eval      Trovata_Tab_TB ='S'
110600110328     c                   end
110700110328     c                   end
110800110328      *
110900110328      ****************
111000110328      ******* Se vi sono eccezioni sul cliente imposta quelle
111100110328      ****************
111200110328     c                   If          Trovata_Tab_TB ='S'
111300110328      *
111400110328     c                   SELECT
111500110328     c                   WHEN      Decod_Porto(Y) = 'F'
111600110328     c                               and  VABcas > *zeros
111700110328     C                   movel     '4'           VABCBO                         + C/Ass
111800110328     c                   WHEN      Decod_Porto(Y) = 'F'
111900110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
112000110328      *  Porto Assegnato
112100110328     c                   WHEN      VABCAS > *zeros
112200110328     C                   movel     '6'           VABCBO                         + C/Ass
112300110328     C                   OTHER
112400110328     C                   movel     '2'           VABCBO                         Senza C/Ass
112500110328     C                   ENDSL
112600110328      *
112700110328      ****************  altrimenti
112800110328      ******* Se non vi sono eccezioni per il cliente
112900110328      *   Rileva a questo punto lo Standard per tutti
113000110328      ****************
113100110328     c                   elseIf      Trovata_Tab_TB ='N'
113200090210      **
113300090210     c                   if              TOD_char3 <> *blank
113400090210     c                   movel(p)  TOD_char3     fld35a           35
113500090213     C                   movel     UNB_Mittente  etbUNB
113600090213     C                   movel(p)  'TOD'         etbSGM
113700090213     C                   movel(p)  TOD_char3     etbVALSGM
113800090213     c     K_TAB1L       chain     edstbl01l
113900090210      *
114000090210      * prova quindi senza l'UNB specifico del cliente
114100090213     c                   if        not %Found(edstbl01l)
114200090213     C                   clear                   etbUNB
114300090213     c     K_TAB1L       chain     edstbl01l
114400090210     c                   end
114500090210      *
114600090213     c                   if        %Found(edstbl01l)
114700090210     c                   SELECT
114800090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
114900090210     C                   movel     '4'           VABCBO                         + C/Ass
115000090213     c                   WHEN      ETBDATI = 'F'
115100090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
115200090210      *  Porto Assegnato
115300090210     c                   WHEN      VABCAS > *zeros
115400090210     C                   movel     '6'           VABCBO                         + C/Ass
115500090210     C                   OTHER
115600090210     C                   movel     '2'           VABCBO                         Senza C/Ass
115700090210     C                   ENDSL
115800090210      *
115900090210     c                   end
116000090210     c                   endIF
116100090209      **
116200110328     c                   endIF
116300110328      **
116400090210      **  Il tipo bolla non è presente
116500090210     c                   if        VABCBO = *blank
116600090210     C                   eval      vinMSG = 'TOD non rilevato'
116700130321     c**********         exsr      Error_DET
116800130321     c**********         goto      end_TOD
116900090210     c                   end
117000090210      **
117100090210     c     end_TOD       endSR
117200090210      * ?================================================================== */
117300090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
117400090210      * ?================================================================== */
117500090210     C     DATI_RFF      BEGSR
117600090210      *
117700090210     C                   clear                   WAGE             35
117800110224     C                   clear                   WFF              35
117900110224     C                   clear                   WCU              35
118000090210      *
118100090210      *  Trascodifico riferimento spedizione
118200091019     c                   if        RFF1153 = Riferimento_Spedizione  and
118300091019     c                             RFF1154 <> *BLANKS
118400090210     C                   movel(p)  RFF1154       WAGE             35
118500090210     c                   end
118600090210      **
118700090210     C                   IF         WAGE <> *BLANKS
118800090210     C                   movel     WAGE          WNSP             35
118900090210     C                   movel     WAGE          WNUM             35
119000090210     C     ' '           scan      WAGE          WLUNGH            3 0
119100090210     C                   sub       1             WLUNGH
119200090210     C                   EXSR      TSTNUM
119300090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
119400090210     C                   movel     WAGE          VABRMA
119500091019     C                   IF         WRMN = Riferimento_Spedizione  or
119600091019     C                              WRMN = *blanks
119700090210     C                   movel     WNUMOK        VABRMN
119800091019     C                   eval       WRMN = Riferimento_Spedizione
119900090210     C                   ENDIF
120000090210     C                   ENDIF
120100090210     C                   ENDIF
120200090210      *
120300090210      * Codice Cliente di riferimento
120400091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
120500090210     C                   movel     RFF1154       WFF
120600090210     c                   END
120700090211      *
120800090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
120900090211     C     WFF           IFNE      *BLANKS
121000090211     C                   eval      XX = 1
121100090211     C                   movel     §PTKSC        WCCL             35
121200090211     C                   movel     WFF           WCOM28           28
121300090211     C                   move      WCOM28        WCCL
121400090211     C     WCCL          lookup    CCL(XX)                                34
121500090211     C     *IN34         IFEQ      '1'
121600090211     C                   movel     DCL(XX)       EDIDSCL
121700090211     C                   z-add     §CLKSC        WCLKSC            7 0
121800090211     C                   movel     §CLTAR        WCLTAR            3
121900090211     C                   z-add     §CLLNP        WCLLNP            3 0
122000090211     C                   z-add     §CLNRS        WCLNRS            2 0
122100090211     C                   movel     §CLCTM        WCLCTM            2
122200090211     C                   movel     §CLBS1        WCLBS1            1
122300090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
122400090211      * imposta la LNP   x il dettaglio specifco
122500090211     c                   eval      VABlnp = §CLlnp
122600090211      * imposta la Serie x il dettaglio specifco
122700090211     c                   eval      VABnrs = §CLnrs
122800090211      *
122900090211      *  In base al cod.trattamento merce reperisco tipo tratt.
123000090211      *    per un dettaglio specifico.
123100090211     C                   CLEAR                   DS1B
123200090211     C                   Z-ADD     1             Y                 3 0
123300090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
123400090211     C     *IN32         IFEQ      '1'
123500090211     C                   MOVEL     DTM(Y)        DS1B
123600090211     C                   END
123700090211      *
123800090211      * Se il cliente non era sulla schiera lo aggiunge
123900090211     C     §CLKSC        lookup    KCL                                    39
124000090211     C                   if        *in39 = *off
124100090211     C                   add       1             WW
124200090211     C                   z-add     §CLKSC        KCL(WW)
124300090211     c                   endif
124400090211      *
124500090211     C                   ENDIF
124600090211     C                   ENDIF
124700090211     C*----------------------------------------------------------------
124800090210      *
124900090210     C                   if        RFF1153 = 'CU ' and RFF1154 <> *blanks
125000090210     C                   movel     RFF1154       WCU
125100090210     C                   END
125200090210      *
125300090210      * Numero Ordine di vendita
125400090210     C     WCU           IFNE      *BLANKS
125500090210     C                   movel     WCU           WNUM             35
125600090210     C     ' '           scan      WCU           WLUNGH
125700090210     C                   sub       1             WLUNGH
125800090210     C                   EXSR      TSTNUM
125900090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
126000090210     C                   movel     WNUMOK        VABRMN
126100090210     C                   movel     'CU '         WRMN
126200090210     C                   Endif
126300090210     C                   ENDIF
126400090210      *
126500090210      * NOn c'è l'identificativo bolla del cliente
126600091019     c                   if        (RFF1153 = Riferimento_Spedizione and
126700091019     c                              RFF1154 = *BLANKS)
126800091019     c                             or
126900090211     c                             (RFF1153 = 'CU' and RFF1154 = *BLANKS)
127000090210     C                   eval      vinMSG = 'RFF riferimento assente'
127100090213     c                   exsr      Error_DET
127200090210     c                   goto      end_RFF
127300090210     C                   ENDIF
127400090210      *
127500090210     c     end_RFF       endSR
127600090209      * ?================================================================== */
127700090213     C*?  C.O.D. Monetary Amount
127800090209      * ?================================================================== */
127900090213     C     DATI_MOA      BEGSR
128000090210      *
128100090216     C* Controllo se campo importo numerico
128200090216      * con 3 decimali
128300090216     C     moa5004       IFNE      *zeros
128400090216      *
128500090216     C     moa5025       IFEQ      '157'
128600090216     C     moa5004       div       1000          VABIAS                         IMPORTO
128700090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
128800090216     C                   END
128900090216      *
129000090216     C     moa5025       IFEQ      '77 '                                        Valore
129100090216     C     moa5004       div       1000          VABVMD                         MERCE
129200090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
129300090216     C                   END
129400090216      *
129500090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
129600090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
129700120904     C     moa5004       div       100000        VABCAS
129800090216     C                   MOVEL     moa6345       VABVCA
129900090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
130000090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
130100090216     C     §PUTPC        IFEQ      'P'
130200090216     C     moa5004       ANDGT     *ZEROS
130300090216     C                   MOVEL     'OM'          VABTIC
130400090216     C                   MOVEL     'OM'          WVABTIC
130500090216     C                   ENDIF
130600090216     C                   END
130700090216     C                   END
130800090216      *
130900090216     C                   END                                                    Imp.non numer.
131000090213      *
131100090213     c     end_MOA       endSR
131200090213      * ?================================================================== */
131300090213     C*?  FREE TEXT
131400090213      * ?================================================================== */
131500090213     C     DATI_FTX      BEGSR
131600110404      *
131700110404      *
131800110404      * ?Se non è un testo per il Pagamento
131900110404     C     ftx4451       IFne      'PAI'                                        -- 01 --
132000110404      *
132100130423      ********* sembrava fosse da ATTIVARE invece NO
132200130423      ****** NON lo cancello perchè può sempre servire se il cliente ci ripensa
132300130423     C**** ftx4451       IFeq      'SSR'                                        -- 01 --
132400130423      ****************
132500130422      *  imposta l'indirizzo e-mail
132600130423     C****************   DO        4             X
132700130423      ****************
132800130423     C**** D05(X)        IFNE      *BLANKS
132900130423      ****************
133000130423     C**** Note_1t       IFEQ      *BLANKS
133100130423     C****************   MOVEL     D05(X)        Note_1t
133200130423     C****************   MOVE      D05(X)        Note_2t
133300130423     C****************   ELSE
133400130423     C**** Note_2t       IFEQ      *BLANKS
133500130423     C****************   MOVEL     D05(X)        Note_2t
133600130423     C****************   END
133700130423     C****************   END
133800130423      ****************
133900130423     C****************   ENDif
134000130423      ****************
134100130423     C****************   ENDdo
134200130423      ****************
134300130422      * carica il campo da riportare poi sul VAT
134400130423     c****************   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
134500130423     c****************             note_1T
134600130423     c****************   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
134700130423     c****************             note_2T
134800130423      ****************
134900130423     c****************   else
135000130423      ****************
135100090216      * ? solo se si tratta di note SIN ICN o PAI non altro
135200090216     C                   DO        4             X
135300090216     C     D05(X)        IFNE      *BLANKS
135400090216     C     WNOT1         IFEQ      *BLANKS
135500090216     C                   MOVEL     D05(X)        WNOT1
135600090216     C                   MOVE      D05(X)        WNOT2
135700090216     C                   ELSE
135800090216     C     WNOT2         IFEQ      *BLANKS
135900090216     C                   MOVEL     D05(X)        WNOT2
136000090216     C                   END
136100110404     C                   ENDif
136200110404     C                   ENDif
136300110404     C                   ENDdo
136400130423      ****************
136500130423     C****************   ENDif
136600130423      ****************
136700130422     C                   ENDif
136800090216     C*------>>
136900090216     C*  Decodifico tipo pagamento C/Assegno
137000090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
137100090216      *      (Problema di pulizia in presenza di + righe di testo)
137200090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
137300090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
137400090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
137500090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
137600090216     C                   if        wri_vabtic = *blank
137700090216     C                   MOVEL     *BLANKS       VABTIC
137800090216     c                   end
137900090216      **
138000090216     c                   clear                   trovato_TPI       1
138100090216      *
138200090216      * ?Se c'è il tipo Incasso
138300090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
138400090216      *
138500090216     C                   DO        4             X                              -- 02 --
138600090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
138700130311      **
138800130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
138900130311      **  per gestire Partner come AZKAR che ha codice "AAA"
139000130311     C*******            MOVEL     D05(X)        WCTC              2
139100130311     C                   MOVEL     D05(X)        WCTC             10
139200090216     C                   Z-ADD     1             Y
139300090216     c                   movel(p)  Wctc          fld35a           35
139400090216     c                   move      §puTC         fld35a
139500090216     C     fld35a        LOOKUP    CTC35(Y)                               33
139600090216     C   33              MOVEL     TPI(Y)        VABTIC
139700090216     c   33              move      'S'           trovato_TPI
139800090216     C                   END                                                    -- 03 --
139900090216     C                   ENDdo                                                  -- 02 --
140000090216      *
140100090216     C                   ELSE                                                   -- 01 --
140200090216      *
140300090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
140400090216      *    Deve essere impostato comunque da testata
140500090216     c                   if        vabtic = *blank
140600090216     C                   MOVEL     wVabTIC       VABTIC
140700090216     c                   end
140800090216      *
140900090216     C                   END                                                    -- 01 --
141000090216      *
141100090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
141200090216      *    essere presenti altri record Flat con altre informazioni
141300090216      *    che potrebbero abblancare il Tipo incasso precedentemente
141400090216      *    decodificato
141500090216     c                   if        Trovato_TPI = 'S' and
141600090216     c                                 wri_vabtic = *blank
141700090216     c                   eval      wri_vabtic = 'S'
141800090216     c                   end
141900090213      *
142000090213     c     end_FTX       endSR
142100090213      * ?================================================================== */
142200090213     C*?  Anagrafica del mittente e del destinatario
142300090213      * ?================================================================== */
142400091016     C     DATI_NAD      BEGSR
142500090213      *
142600090210      * Dati destinatario
142700090210     C                   IF        nad3035 = 'CN' or
142800090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
142900090227      *
143000090227      * Se trovato un indirizzo di destinatario
143100090227     c                   eval       se_trovato_NAD ='S'
143200090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
143300090210      *
143400090210     C* Reperisco nazione corrispondente destinatario
143500090210     C                   Z-ADD     1             YY                3 0
143600090210     c                   eval      *in32 = *off
143700090210     C                   if        nad3207 <> *BLANKS
143800090210     C     nad3207       LOOKUP    ISO(YY)                                32
143900090210     C                   endif
144000090210      *
144100090210     C* Imposto dati destinatario
144200090211     C                   MOVEL     nad31241      VABRSD
144300090211     C                   if        nad31242 <> *LOVAL
144400090211     C                   MOVEL     nad31242      VABRD2
144500090210     C                   endif
144600090211      *
144700091019????  * DAL PARTY NAME
144800091019???? C* Imposto dati destinatario
144900091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
145000091019???? C                   MOVEL     nad30361      VABRSD
145100091019???? C                   if        nad30362 <> *LOVAL
145200091019???? C                   MOVEL     nad30362      VABRD2
145300091019???? C                   endif
145400091019???? C                   end
145500090211      *
145600090210     C                   MOVEL     nad30421      VABIND
145700090210     C                   MOVEL     nad3164       VABLOD
145800090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
145900090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
146000090210      *
146100090210     C* Verifico se c'è dello spazio per nella località
146200090210     C     '    '        SCAN      VABLOD        WI                4 0
146300090210      *
146400090210     C                   MOVEA     VABLOD        LOD
146500090210     C                   ADD       1             WI
146600090210     C                   MOVEA     NAD30422      LOD(WI)
146700090210     C                   MOVEA     LOD           VABLOD
146800090210     C                   ENDif
146900090210      *
147000090210     C                   If        *in32 = *on
147100090210     C                   MOVEL     C15(YY)       VABNZD
147200090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
147300090210     c                   Else
147400090210     C                   MOVEL     *BLANKS       VABNZD
147500090210     C                   Z-ADD     0             WFLCPF
147600090210     c                   Endif
147700090210      * Converto CAP
147800090210     C                   EXSR      CNVCAP
147900090603     C*
148000090603     C* Se in testata
148100090603     c                   if        Inizio_dettaglio = *blank
148200090603     c                   eval       tes_VABrsd = VABrsd
148300090603     c                   eval       tes_VABrd2 = VABrd2
148400090603     c                   eval       tes_VABind = VABind
148500090603     c                   eval       tes_VABlod = VABlod
148600090603     c                   eval       tes_VABnzd = VABnzd
148700090603     c                   eval      NAD_destinatario_in_testata = 'S'
148800090603     c                   end
148900090603     C*
149000090210     C                   ENDIF
149100090210      *
149200090210      * Dati mittente
149300091016     C                   IF        nad3035 = 'CZ'
149400090210     C                   movel     nad30361      VABRMO
149500090210      *
149600090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
149700090210     C                   movel     §PTNAZ        VABNMO
149800090210      *
149900090210     C                   IF        nad3207 <> *blanks
150000090210     C                   eval      YY = 1
150100090210     C     nad3207       LOOKUP    ISO(YY)                                32
150200090210     C                   if         *in32 = *on
150300090210     C                   MOVEL     C15(YY)       VABNMO
150400090210     C                   endif
150500090210     C                   ENDIF
150600090210      *
150700090210      * Normalizzo CAP mittente originale
150800090210     C                   clear                   T
150900090210     C                   clear                   S
151000090210     C                   MOVEA     *BLANKS       CAPM
151100090210     C                   MOVEA     nad3251       CAP9
151200090210     C                   DO        9             T
151300090210     C     CAP9(T)       IFNE      *BLANKS
151400090210     C                   ADD       1             S
151500090210     C                   MOVE      CAP9(T)       CAPM(S)
151600090210     C                   ENDIF
151700090210     C                   ENDDO
151800090210     C*
151900090210      *     Controllo validità CAP
152000090210     C                   MOVEA     CAPM          VABCMO
152100090210      *
152200090210     C                   CLEAR                   TISI95DS
152300090210     C                   MOVEL     VABCMO        I95CAP
152400090210     C                   MOVEL     VABNMO        I95NAR
152500090210     C                   MOVEL     WOGGI         I95DAT
152600090210     C                   MOVEL     '3'           I95TCN
152700090210     C                   CALL      'TISI95R'
152800090210     C                   PARM                    TISI95DS
152900090210      *     Errore
153000090210     C     O95ERR        IFNE      *BLANKS
153100090210     C                   MOVEL     INDCAE        VABCMO
153200090210     C                   MOVEL     §PTNAZ        VABNMO
153300090210     C                   END
153400090603      *
153500090603     C* Se in testata
153600090603     c                   if        Inizio_dettaglio = *blank
153700090603     c                   eval      tes_VABrmo = VABrmo
153800090603     c                   eval      tes_VABnmo = VABnmo
153900090603     c                   eval      tes_VABcmo = VABcmo
154000090603     c                   eval      NAD_mittente_in_testata = 'S'
154100090603     c                   end
154200090210      *
154300090210     C                   END
154400090210      *
154500090210      * NOn c'è l'identificativo bolla del cliente
154600090210     C                   if          vabRSD = *blank or
154700090210     C                               vabIND = *blank or
154800090210     C                               vabLOD = *blank
154900090211     C                   IF        nad3035 = 'CN'
155000090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
155100130321     c**********         exsr      Error_DET
155200130321     c**********         goto      end_NAD
155300090210     C                   ENDIF
155400090211     C                   ENDIF
155500090210      *
155600090210     c     end_NAD       endSR
155700090210      * ?================================================================== */
155800090210     C*?  Contatto a cui far riferimento
155900090210      * ?================================================================== */
156000090210     C     DATI_CTA      BEGSR
156100090210      *
156200090210      *  memorizza il riferimento di consegna x il destinatario
156300090210      *  memorizza il riferimento di consegna nr.telefonico
156400090210     C     cta3412       ifne      *BLANKS
156500090210     c                   movel     cta3412       watrde
156600090210     c                   end
156700090210      *
156800090210     c                   endSR
156900090210      * ?================================================================== */
157000090210     C*?  Contatto telefonico  a cui far riferimento
157100090210      * ?================================================================== */
157200090210     C     DATI_COM      BEGSR
157300090210      *
157400090210     C     com3148       ifne      *BLANKS
157500090210     c                   movel     com3148       wattel
157600090210     c                   end
157700090210      *
157800090209     C                   ENDSR
157900090210      * ?================================================================== */
158000090210     C*?  Dati di dettaglio della spedizione e dei colli
158100090210      * ?================================================================== */
158200090210     C     DATI_GID      BEGSR
158300090210      *
158400090210     C* Se ho totali per spedizione metto in campi VAB
158500090210     C     gid1496       IFEQ      99999
158600090210     C     gid1496       orEQ      9999
158700090210     C                   MOVEL     'S'           WGID99
158800090210     C                   z-add     gid722420     VABNCL
158900090210     C                   ELSE
159000090210     C     WGID99        IFEQ      'N'
159100090210     C                   z-add     gid722420     WTNCL             5 0
159200090210     C                   ADD       WTNCL         VABNCL
159300090210     C                   END
159400090210     C                   END
159500090212      *
159600090210     C                   ADD       VABNCL        WTOCOC           16 0
159700090210      *
159800090210     c     end_GID       endSR
159900090210      * ?================================================================== */
160000090210     C*?  Dati di dettaglio misure spedizione e colli
160100090210      * ?================================================================== */
160200090210     C     DATI_MEA      BEGSR
160300090210      *
160400090210     C*  Peso
160500090210     C     mea6311       IFEQ      'WT '
160600090210     C     mea6411       ANDEQ     'KGM'
160700090210      *
160800090210     C*  Controllo se ho valore totale o parziale
160900090210      *   sempre il Lordo
161000091019???? C     mea6313       IFEQ      'G  '
161100090210     C     WS1496        IFEQ      99999
161200090210     C     WS1496        orEQ      9999
161300091019???? C                   move      mea6314       mea6314_2        18 2          Peso
161400091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
161500090210     C                   ELSE
161600091019???? C     WGID99        IFEQ      'N'
161700091019???? C                   move      mea6314       mea6314_2                      Peso
161800091019???? C                   ADD       mea6314_2     VABPKB                         Peso
161900090210     C                   END
162000090210     C                   END
162100090210     C                   END
162200090210      *
162300091019???? C     mea6313       IFEQ      'G  '
162400091019???? C                   move      mea6314       mea6314_2                      Peso
162500091019???? C                   ADD       mea6314_2     WTOPLC
162600090210     C                   END
162700091019???? C     mea6313       IFEQ      'N  '
162800091019???? C                   move      mea6314       mea6314_2                      Peso
162900091019???? C                   ADD       mea6314_2     WTOPNC
163000090210     C                   END
163100090210     C                   END
163200090210      ***
163300090210     C*  Peso in Grammi  se abilitato a utilizzarlo
163400090210      ***
163500090210     C     §puGR         IFeq      'S'
163600090210     C     mea6311       IFEQ      'WT '
163700090210     C     mea6411       ANDEQ     '163'
163800090210      *
163900090210     C*  Controllo se ho valore totale o parziale
164000091019???? C     mea6313       IFEQ      'G  '
164100091019???? C     WS1496        IFEQ      99999
164200091019???? C     WS1496        orEQ      9999
164300091019???? c                   move      mea6314       mea6314_2
164400091019???? C     mea6314_2     div       1000          VABPKB                         Peso
164500090210     C                   ELSE
164600091019???? C     WGID99        IFEQ      'N'
164700091019???? c                   move      mea6314       mea6314_2
164800091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
164900091019???? C                   add       Peso_inKG     VABPKB                         Peso
165000090210     C                   END
165100090210     C                   END
165200090210     C                   END
165300090210      *
165400091019???? C     mea6313       IFEQ      'G  '
165500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
165600091019???? C                   ADD       Peso_inKG     WTOPLC
165700090210     C                   END
165800091019???? C     mea6313       IFEQ      'N  '
165900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
166000091019???? C                   ADD       Peso_inKG     WTOPNC
166100090210     C                   END
166200090210     C                   END
166300090210     C                   END
166400090210      *  Volume
166500091019???? C     mea6311       IFEQ      'VOL'
166600091019???? C     mea6411       ANDEQ     'MTQ'
166700090210      *  Controllo se ho valore totale o parziale
166800090210     C     WS1496        IFEQ      99999
166900090210     C     WS1496        orEQ      9999
167000091019???? C                   move      mea6314       mea6314_2        18 2          Volume
167100091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
167200091019???? C                   Z-ADD     mea6314_2     PVLB
167300090210     C                   ELSE
167400091019     C     WGID99        IFEQ      'N'
167500091019???? C                   move      mea6314       mea6314_2        18 2          Volume
167600091019???? C                   ADD       mea6314_2     VABVLB                         Volume
167700091019???? C                   ADD       mea6314_2     PVLB
167800090210     C                   END
167900090210     C                   END
168000090210     C                   END
168100090210      *
168200090210     C* Ricerco CAP
168300090210     C     WCAP          IFNE      *BLANKS
168400090210     C     mea6311       IFEQ      'WT '
168500090210      *
168600090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
168700090210     C                   CLEAR                   FNLV55DS
168800090210     C                   MOVEL     'P'           D55TPT
168900090210     C                   MOVEL     WOGGI         D55DRF
169000090210     C                   MOVEL     VABLNP        D55LNP
169100090210     C                   MOVEL     VABLNP        D55LIN
169200090210     C                   CALL      'FNLV55R'
169300090210     C                   PARM                    FNLV55DS
169400090210     C                   MOVE      D55TFP        COMTFP            3 0
169500090210      *
169600090210     C*  PASSO IL TERMINAL PARTENZA
169700090210     C                   CLEAR                   TISI95DS
169800090210     C                   Z-ADD     COMTFP        I95TFP
169900090210     C                   MOVEL     VABTSP        I95TSP
170000090210     C                   MOVEL     'S'           I95FRE
170100090210     C                   MOVEL     '3'           I95TCN
170200090210     C                   MOVEL     WCAP          I95CAP
170300090210     C                   MOVEL     VABNZD        I95NAR
170400090210     C                   MOVEL     VABLOD        I95LOC
170500090210     C                   MOVEL     VABIND        I95IND
170600090210      *
170700090210     C* Se gestita seconda linea di arrivo passo peso - volume
170800090210     C* e numero colli effettivo
170900090210     C     §PULNA        IFEQ      'S'
171000090210     C                   Z-ADD     VABPKB        I95LKG
171100090210     C                   Z-ADD     VABVLB        I95LMC
171200090210     C                   ELSE
171300090210     C* Altrmenti passo 1 Kg e 0,001
171400090210     C***                  Z-ADD1         I95LKG
171500090210     C***                  Z-ADD0,001     I95LMC
171600090210     C                   END
171700090210     C*
171800090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
171900090210     C                   SELECT
172000090210     C     VABCBO        WHENEQ    '1 '
172100090210     C                   MOVEL     *BLANKS       I95FCA
172200090210     C                   MOVEL     'F'           I95TPO
172300090210     C     VABCBO        WHENEQ    '2 '
172400090210     C                   MOVEL     *BLANKS       I95FCA
172500090210     C                   MOVEL     'A'           I95TPO
172600090210     C     VABCBO        WHENEQ    '4 '
172700090210     C                   MOVEL     'S'           I95FCA
172800090210     C                   MOVEL     'F'           I95TPO
172900090210     C     VABCBO        WHENEQ    '6 '
173000090210     C                   MOVEL     'S'           I95FCA
173100090210     C                   MOVEL     'A'           I95TPO
173200090210     C                   OTHER
173300090210     C                   MOVEL     'F'           I95TPO
173400090210     C     VABCAS        IFGT      0
173500090210     C                   MOVEL     'S'           I95FCA
173600090210     C                   ELSE
173700090210     C                   MOVEL     *BLANKS       I95FCA
173800090210     C                   END
173900090210     C                   ENDSL
174000090210     C*
174100090210     C                   CALL      'TISI95R'
174200090210     C                   PARM                    TISI95DS
174300090210     C* Reperisco i dati da CAP
174400090210     C                   MOVEL     O95PRV        VABPRD
174500090210     C                   MOVEL     O95LNA        VABLNA
174600090210     C                   MOVEL     O95ZNC        VABZNC
174700090210     C                   MOVEL     WCAP          VABCAD
174800090210     C*
174900090210     C     O95ERR        IFNE      *BLANKS
175000090210     C     O95FIT        ANDEQ     'S'
175100090210     C*  Cap non trovato
175200090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
175300130321     c*********          exsr      Error_DET
175400130321     C*********          goto      end_MEA
175500090210     C                   END
175600090210     C                   END
175700090210      *
175800090210     C                   ELSE
175900090210      *
176000090210     C                   CLEAR                   VABPRD
176100090210     C                   CLEAR                   VABLNA
176200090210     C                   CLEAR                   VABZNC
176300090210     C                   CLEAR                   VABCAD
176400090210     C*  Cap non trovato
176500090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
176600130321     c**********         exsr      Error_DET
176700130321     C**********         goto      end_MEA
176800090210     C                   END
176900090210      *
177000090210     c     end_MEA       endSR
177100090210      * ?================================================================== */
177200090210     C*?  Dati di dettaglio Codice a barre Segnacolli
177300090210      * ?================================================================== */
177400090210     C     DATI_PCI      BEGSR
177500090210      *
177600090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
177700090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
177800090210     C     §1BF12        IFEQ      'E'
177900090210     C                   EXSR      SEGNEE
178000090210     C                   ELSE
178100090210      *
178200090210      *  La prima volta controllo congruità numero segnacollo
178300090210     C     §PUBS1        IFNE      *BLANKS
178400090210     C     VABnrs        ANDNE     0
178500090210      *  Se il codice trattamento merce prevede che segnacollino i
178600090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
178700090210     C     §1BFG7        IFEQ      'S'
178800090210     C     §1BFG1        OREQ      'S'
178900090210      *  calcolo segnacollo
179000090210     C                   EXSR      SGNELA
179100090210     C                   END
179200090210     C                   END
179300090210      *
179400090210     C                   END
179500090210      *
179600090210     c     end_PCI       endSR
179700090211     C*----------------------------------------------------------------
179800090211      *? dati finali del dettaglio    UNT
179900090211     C*----------------------------------------------------------------
180000090211     C     DATI_UNT      BEGSR
180100090211      *
180200090213     c                   eval      NUM_RECord = vnREC
180300090213     c                   z-add     vnrec         last_RECord
180400090211      **
180500090211     c                   endSR
180600090211     C*----------------------------------------------------------------
180700090211      *? dati finali     UNT
180800090211     C*----------------------------------------------------------------
180900090211     C     DATI_UNZ      BEGSR
181000090211      *
181100090211      **
181200090211     c                   endSR
181300090210      *----------------------------------------------------------------
181400090210      *? Input segnacolli su schiera di 20 elementi
181500090210      *----------------------------------------------------------------
181600090210     C     INP_Segnacol  BEGSR
181700090210      *
181800090210     c                   clear                   quanti_PCI        3 0
181900090210     c                   if        §puEL1 = 0
182000090210     c                   z-add     10            quanti_PCI
182100090210     c                   else
182200090210     c                   z-add     §puEL1        quanti_PCI
182300090210     c                   end
182400090210      *
182500090210      *  Pulisce la schiera dei segnacolli da elaborare
182600090210     c                   clear                   x30
182700090210     c                   z-add     0             Nr_x30            3 0
182800090210      *
182900090210      *   riporto tutto sulla schiera generica X30
183000090210      *
183100090210     c                   do        10            limite            3 0
183200090210      * Limita quanti elementi passati in ogni PCI
183300090210      *   devono essere considerati
183400090210     c                   if        limite > quanti_PCI
183500090210     c                   Leave
183600090210     c                   end
183700090210      *
183800090210      *  Carica la schiera generale di tutti i segnacolli
183900090210     c                   if        D30_1(limite) <> *blank
184000090210     c                   add       1             Nr_x30
184100090210     c                   eval      X30(Nr_x30) = D30_1(limite)
184200090210     c                   end
184300090210     c                   enddo
184400090210      *
184500090210     C                   ENDSR
184600090210      *----------------------------------------------------------------
184700090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
184800090210      *----------------------------------------------------------------
184900090210     C     SEGNEE        BEGSR
185000090210      *
185100090210     c                   exsr      INP_Segnacol
185200090210      *
185300090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
185400090210     C                   DO        10            x                              --- 01 ---
185500090210      *
185600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
185700090210     C     x30(X)        ANDNE     *LOVAL
185800090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
185900090210     C                   ADD       1             §§
186000090210     C                   MOVEL     x30(X)        SGE(§§)
186100090210     C                   END                                                    --- 03 ---
186200090210      *
186300090210     C                   END                                                    --- 02 ---
186400090210      *
186500090210     C                   ENDSR
186600090210     C*----------------------------------------------------------------
186700090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
186800090210     C*----------------------------------------------------------------
186900090210     C     SGNELA        BEGSR
187000090210      *
187100090210     c                   exsr      INP_Segnacol
187200090210     C*
187300090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
187400090210     C*  in base al numero elementi validi carico sgn.
187500090210     C                   DO        10            X                              --- 01 ---
187600090210     C*
187700090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
187800090210     C     x30(X)        ANDNE     *LOVAL
187900090210     C*
188000090210     C                   CLEAR                   DSSGN
188100090210     C*  Controllo se devo ricevere la linea di partenza
188200090210     C                   MOVEL     'N'           WERRO             1
188300090210     C     §PULP1        IFGT      0                                            --- 03 ---
188400090210     C                   EXSR      REPLNP
188500090210     C                   END                                                    --- 03 ---
188600090210     C*  Controllo se devo ricevere la linea di arrivo
188700090210     C     §PULA1        IFGT      0                                            --- 03 ---
188800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
188900090210     C                   EXSR      REPLNA
189000090210     C                   END                                                    --- 03 ---
189100090210     C*  Controllo se devo ricevere il numero di serie
189200090210     C     §PUNR1        IFGT      0                                            --- 03 ---
189300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189400090210     C                   EXSR      REPNRS
189500090210     C                   END                                                    --- 03 ---
189600090210     C*  Controllo se devo ricevere il numero segnacollo
189700090210     C     §PUSG1        IFGT      0                                            --- 03 ---
189800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189900090210     C                   EXSR      REPSGN
190000090210     C                   END                                                    --- 03 ---
190100090210     C*  Controllo se devo ricevere la zona di consegna
190200090210     C     §PUZN1        IFGT      0                                            --- 03 ---
190300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
190400090210     C                   EXSR      REPZNC
190500090210     C                   END                                                    --- 03 ---
190600090210     C*  Imposto da VAB i dati a zero se mi è stato passato
190700090210     C*  numero segnacollo valido
190800090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
190900090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
191000090210     C                   Z-ADD     VABLNP        SGNLNP
191100090210     C                   END                                                    --- 04 ---
191200090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
191300090210     C                   Z-ADD     VABLNA        SGNLNA
191400090210     C                   END                                                    --- 04 ---
191500090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
191600090210     C                   Z-ADD     VABNRS        SGNNRS
191700090210     C                   END                                                    --- 04 ---
191800090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
191900090210     C                   Z-ADD     VABZNC        SGNZNC
192000090210     C                   END                                                    --- 04 ---
192100090210      *
192200090210     C*  mi reimposto LNA e zona da segnacollo
192300090210     C                   Z-ADD     SGNLNA        VABLNA
192400090210     C                   Z-ADD     SGNZNC        VABZNC
192500090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
192600090210     C                   ADD       1             XY
192700090210     C                   MOVE      SGNNSC        SGN(XY)
192800090210     C                   END                                                    --- 03 ---
192900090210     C*
193000090210     C                   END                                                    --- 02 ---
193100090210     C*
193200090210     C                   END                                                    --- 01 ---
193300090210      *
193400090210     C                   ENDSR
193500090210     C*----------------------------------------------------------------
193600090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
193700090210     C*----------------------------------------------------------------
193800090210     C     REPLNP        BEGSR
193900090210     C*
194000090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
194100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
194200090210     C*  dal numero segnacollo passatomi dal partner
194300090210     C                   Z-ADD     §PULP1        WP                2 0
194400090210     C     3             SUBST     x30(X):WP     WLNP              3
194500090210     C*
194600090210     C*  controllo che la lnp calcolata sia numerica
194700090210     C                   TESTN                   WLNP                 98
194800090210     C*
194900090210     C*  se è numerica la carico
195000090210     C     *IN98         IFEQ      '1'
195100090210     C                   MOVE      WLNP          SGNLNP
195200090210     C                   ELSE
195300090210     C*  se non è numerica stampo errore
195400090210     C                   MOVEL     'S'           WERRO             1
195500090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
195600090213     c                   exsr      Error_DET
195700090210     C                   goto      end_LNP
195800090210     C                   END
195900090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
196000090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
196100090210     C     SGNLNP        IFNE      VABLNP
196200090210     C                   MOVEL     'S'           WERRO             1
196300090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
196400090210     c                             a della Bolla'
196500090213     c                   exsr      Error_DET
196600090210     C                   goto      end_LNP
196700090210     C                   END
196800090210     C*
196900090210     C     end_LNP       ENDSR
197000090210     C*----------------------------------------------------------------
197100090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
197200090210     C*----------------------------------------------------------------
197300090210     C     REPLNA        BEGSR
197400090210     C*
197500090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
197600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
197700090210     C*  dal numero segnacollo passatomi dal partner
197800090210     C                   Z-ADD     §PULA1        WP                2 0
197900090210     C     3             SUBST     x30(X):WP     WLNA              3
198000090210     C*  controllo che la lna calcolata sia numerica
198100090210     C                   TESTN                   WLNA                 98
198200090210     C*  se è numerica la carico
198300090210     C     *IN98         IFEQ      '1'
198400090210     C                   MOVE      WLNA          SGNLNA
198500090210     C                   ELSE
198600090210     C*  se non è numerica stampo errore
198700090210     C                   MOVEL     'S'           WERRO             1
198800090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
198900090213     c                   exsr      Error_DET
199000090210     C                   goto      end_LNA
199100090210     C                   END
199200090210     C*
199300090210     C     end_LNA       ENDSR
199400090210     C*----------------------------------------------------------------
199500090210     C*? REPNRS - Reperisce numero serie
199600090210     C*----------------------------------------------------------------
199700090210     C     REPNRS        BEGSR
199800090210     C*
199900090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
200000090210     C*  (a partire dalla posizione iniziale indicata in tabella)
200100090210     C*  dal numero segnacollo passatomi dal partner
200200090210     C                   Z-ADD     §PUNR1        WP                2 0
200300090210     C     2             SUBST     x30(X):WP     WNRS              2
200400090210     C*  controllo che il nrs calcolata sia numerico
200500090210     C                   TESTN                   WNRS                 98
200600090210     C*  se è numerica la carico
200700090210     C     *IN98         IFEQ      '1'
200800090210     C                   MOVE      WNRS          SGNNRS
200900090210     C                   ELSE
201000090210     C*  se non è numerico stampo errore
201100090210     C                   MOVEL     'S'           WERRO             1
201200090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
201300090213     c                   exsr      Error_DET
201400090210     C                   goto      end_NRS
201500090210     C                   END
201600090210      *
201700090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
201800090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
201900090210     C     SGNNRS        IFNE      VABNRS
202000090210     C                   MOVEL     'S'           WERRO             1
202100090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
202200090210     c                             o della Bolla'
202300090213     c                   exsr      Error_DET
202400090210     C                   goto      end_NRS
202500090210     C                   END
202600090210     C*
202700090210     C     end_NRS       ENDSR
202800090210     C*----------------------------------------------------------------
202900090210     C*? REPZNC - Reperisce zona consegna
203000090210     C*----------------------------------------------------------------
203100090210     C     REPZNC        BEGSR
203200090210     C*
203300090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
203400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
203500090210     C*  dal numero segnacollo passatomi dal partner
203600090210     C                   Z-ADD     §PUZN1        WP                2 0
203700090210     C     2             SUBST     x30(X):WP     WZNC              2
203800090210     C*  controllo che la zona calcolata sia numerica
203900090210     C                   TESTN                   WZNC                 98
204000090210     C*  se è numerica la carico
204100090210     C     *IN98         IFEQ      '1'
204200090210     C                   MOVE      WZNC          SGNZNC
204300090210     C                   ELSE
204400090210     C*  se non è numerica stampo errore
204500090210     C                   MOVEL     'S'           WERRO             1
204600090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
204700090213     c                   exsr      Error_DET
204800090210     C                   goto      end_ZNC
204900090210     C                   END
205000090210     C*
205100090210     C     end_ZNC       ENDSR
205200090210     C*----------------------------------------------------------------
205300090210     C*? REPSGN - Reperisce numero segnacollo
205400090210     C*----------------------------------------------------------------
205500090210     C     REPSGN        BEGSR
205600090210     C*
205700090210     C*  se viene passata nr.segnacollo estraggo sottostringa
205800090210     C*  lunga 7 chr (a partire dalla posizione iniziale
205900090210     C*  indicata in tabella)
206000090210     C*  dal numero segnacollo passatomi dal partner
206100090210     C                   Z-ADD     §PUSG1        WP                2 0
206200090210     C     7             SUBST     x30(X):WP     WSGN              7
206300090210     C*  controllo che il nr.segnacollo calcolato sia numerico
206400090210     C                   TESTN                   WSGN                 98
206500090210     C*  se è numerico lo carico
206600090210     C     *IN98         IFEQ      '1'
206700090210     C                   MOVE      WSGN          SGNNSC
206800090210     C                   ELSE
206900090210     C*  se non è numerica stampo errore
207000090210     C                   MOVEL     'S'           WERRO             1
207100090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
207200090213     c                   exsr      Error_DET
207300090210     C                   goto      end_SGN
207400090210     C                   END
207500090210     C*
207600090210     C     end_SGN       ENDSR
207700051205      *----------------------------------------------------*
207800051205      *   DEFINIZIONE CHIAVI                               *
207900051205      *----------------------------------------------------*
208000051205     C     *INZSR        BEGSR
208100051205      *
208200991129     c     *ENTRY        PLIST
208300060613     C                   parm                    esito             1
208400971215      *
208500050112     C     KTABel        KLIST
208600050112     C                   KFLD                    TBLKUT
208700050112     C                   KFLD                    TBLCOD
208800050112     C                   KFLD                    TBLKEY
208900090210      *
209000090210     C     K_TAB1L       KLIST
209100090213     C                   KFLD                    etbUNB
209200090213     C                   KFLD                    etbSGM
209300090213     C                   KFLD                    etbVALSGM
209400090209     C*
209500090209     C* Definisco chiavi di accesso
209600090209     C     KTAB1         KLIST
209700090209     C                   KFLD                    KKUT
209800090209     C                   KFLD                    KCOD
209900090209     C*
210000090209     C     KNUF          KLIST
210100090209     C                   KFLD                    KAAA
210200090209     C                   KFLD                    KCNU
210300090209     C                   KFLD                    KFIL
210400090209      *
210500090209     C     KVAB1         KLIST
210600090209     C                   KFLD                    KCCM
210700090209     C                   KFLD                    KCMR
210800090209     C                   KFLD                    KCNT
210900090209     C                   KFLD                    KAAS
211000090209     C                   KFLD                    KLNP
211100090209     C                   KFLD                    KNRS
211200090209     C                   KFLD                    KNSP
211300090209      *
211400090209     C     KVAB2         KLIST
211500090209     C                   KFLD                    KCCM
211600090209     C                   KFLD                    KCMR
211700090209      *
211800090209     C     KRDE          KLIST
211900090209     C                   KFLD                    KAAS
212000090209     C                   KFLD                    KLNP1
212100090209     C                   KFLD                    KNRS
212200090209     C                   KFLD                    KNSP
212300090209     C                   KFLD                    KNMC
212400090209     C                   KFLD                    KNRP
212500090209      *
212600090209     C     KACO          KLIST
212700090209     C                   KFLD                    KKUT
212800090209     C                   KFLD                    KKCC
212900090209     C                   KFLD                    KKSC
213000090209      *
213100090209     C     KIND          KLIST
213200090209     C                   KFLD                    KKUT
213300090209     C                   KFLD                    KKCC
213400090209     C                   KFLD                    KKSC
213500090209      *
213600090209      * DETERMINO SE SONO BARTOLINI O SDI
213700090209     C                   CLEAR                   TIBS55DS
213800090209     C                   MOVEL     'L'           I50TLA
213900090209     C                   CALL      'TIBS55R'
214000090209     C                   PARM                    TIBS55DS
214100090209      *
214200090209     C* RECUPERO DATI DELL'UTENTE
214300090209     C                   Z-ADD     1             CODUT             1 0
214400090209     C                   CALL      'XPARUT'
214500090209     C                   PARM                    UTEDSE0F
214600090209     C                   MOVEL     RAGUT         RSUT             20
214700090209     C*
214800090209     C* RICERCA CAPOCONTI
214900090209     C                   DO        50            X
215000090209     C                   MOVE      TCU(X)        TCUDS
215100090209     C     F56           IFEQ      'CG'
215200090209     C     F34           ANDEQ     '01'
215300090209     C                   Z-ADD     KCU(X)        KCI               4 0
215400090209     C                   END
215500090209     C                   END
215600090209     C*
215700090211     c                   movel(p)  'EDPAB'       User_Msg         10
215800090211     C*
215900090209     C* IMPOSTO A ZERO VARIABILI DI PGM
216000090209     c                   move      *blank        Snd_Msg_alert     1
216100090209     C                   MOVEL     CA(1)         WCA              78
216200090209     C                   MOVEL     CN(1)         WCN              78
216300090209     C*
216400090209     C* RECUPERO DATA E ORA
216500090209     C                   TIME                    WHHDAT           14 0
216600090209     C                   MOVE      WHHDAT        G02DAT
216700090209     C                   MOVE      WHHDAT        WDATA8            8 0
216800090209     C                   MOVE      WDATA8        WAA2              2 0
216900090209     C                   MOVEL     WDATA8        DATA              6 0
217000090209     C                   MOVE      WAA2          DATA
217100090209     C                   MOVEL     *BLANK        G02ERR
217200090209     C                   CALL      'XSRDA8'
217300090209     C                   PARM                    WLBDA8
217400090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
217500090209     C                   TIME                    WORA              6 0
217600090209      * Recupero data e ora
217700090209     C                   TIME                    W0140
217800090209      * UDATE IN GGMMAAAA
217900090209     C                   MOVE      W0140         WDTGIO
218000090209      * UDATE IN AAAAMMGG
218100090209     C     *eur          MOVEL     WDTGIO        DATA_eur
218200090209     C     *iso          MOVEL     DATA_eur      dateu
218300090209      *
218400090209      * ?              /*--------------------------- */
218500090209     c                   exsr      CARICA_TABELLE
218600090209      * ?              /*--------------------------- */
218700090209      *
218800090209      *
218900090209     C                   MOVEL     *ALL'-'       CMP132          132
219000090209     C                   CLEAR                   EDiVAB00
219100090211     C                   clear                   vablnp_PT
219200090212     C                   clear                   vabnrs_PT
219300090211     C                   clear                   vabctm_PT
219400090211     C                   clear                   VABfnsp_PT
219500090209     C                   MOVEA     *HIVAL        SGN
219600991124      *
219700050414      *------------------
219800991124     C                   ENDSR
219900060621      * ?------------------------------------------------------------------ */
220000090209      *?    CARICA TABELLE SU SCHIERE
220100060621      * ?------------------------------------------------------------------ */
220200090209     C     CARICA_TABELLEBEGSR
220300090205      *
220400090209     C* Caricamento Tabella 1B
220500090209     C                   Z-ADD     0             X                 4 0
220600090209     C                   Z-ADD     CODUT         KKUT
220700090209     C                   MOVEL     '1B'          KCOD
220800090209     C     KTAB1         CHAIN     TABEL00F                           30
220900090209     C     *IN30         DOWEQ     '0'
221000090209     C     TBLFLG        IFEQ      *BLANKS
221100090209     C                   ADD       1             X
221200090209     C                   MOVEL     TBLKEY        CTM(X)
221300090209     C                   MOVEL     TBLUNI        DTM(X)
221400090209     C                   END
221500090209     C     KTAB1         READE     TABEL00F                               30
221600090209     C                   END
221700090209      *
221800090209     C* Caricamento Tabella 15
221900090209     C                   Z-ADD     0             X                 4 0
222000090209     C                   Z-ADD     CODUT         KKUT
222100090209     C                   MOVEL     '15'          KCOD
222200090209     C     KTAB1         CHAIN     TABEL00F                           30
222300090209     C     *IN30         DOWEQ     '0'
222400090209     C     TBLFLG        IFEQ      *BLANKS
222500090209     C                   ADD       1             X
222600090209     C                   MOVEL     TBLKEY        C15(X)
222700090209     C                   MOVEL     TBLUNI        DS15
222800090209     C                   MOVEL     §15COD        ISO(X)
222900090209     C                   MOVEL     §15ECF        CPF(X)
223000090209     C                   MOVE      §15PCF        CPF(X)
223100090209     C                   END
223200090209     C     KTAB1         READE     TABEL00F                               30
223300090209     C                   END
223400090209      *
223500090209     C* Caricamento Tabella CV
223600090209     C                   Z-ADD     0             X
223700090209     C                   Z-ADD     CODUT         KKUT
223800090209     C                   MOVEL     'CV'          KCOD
223900090209     C     KTAB1         CHAIN     TABEL00F                           30
224000090209     C     *IN30         DOWEQ     '0'
224100090209     C     TBLFLG        IFEQ      *BLANKS
224200090209     C                   ADD       1             X
224300090209     C                   MOVEL     TBLKEY        CCV(X)
224400090209     C                   MOVEL     TBLUNI        DCV(X)
224500090209     C                   END
224600090209     C     KTAB1         READE     TABEL00F                               30
224700090209     C                   END
224800090216      *
224900110328      * Caricamento Tabella termini consegna
225000110328     C                   Z-ADD     0             X
225100110328     C                   MOVEL     'TB'          TABCOD
225200110328     C     TABCOD        CHAIN     EDTAB01L                           30
225300110328     C     *IN30         DOWEQ     '0'
225400110328     C     TABFLG        IFEQ      *BLANKS
225500110328     C                   ADD       1             X
225600110328     C                   eval      K35_TpBolla(X) = TABKEY
225700110328     C                   eval      Cod_TpBolla(X) = TABKEY
225800110328     C                   eval      Decod_Porto(X) = TABUNI
225900110328     C                   END
226000110328     C     TABCOD        READE     EDTAB01L                               30
226100110328     C                   END
226200110328      *
226300090216     C* Caricamento Tabella Tipo Incasso C/Assegno
226400090216     C                   Z-ADD     0             X
226500090216     C                   MOVEL     'TC'          TABCOD
226600090216     C     TABCOD        CHAIN     EDTAB01L                           30
226700090216     C     *IN30         DOWEQ     '0'
226800090216     C     TABFLG        IFEQ      *BLANKS
226900090216     C                   ADD       1             X
227000090216     C                   MOVEL     TABKEY        CTC35(X)
227100090216     C                   MOVEL     TABKEY        CTC(X)
227200090216     C                   MOVEL     TABUNI        TPI(X)
227300090216     C                   END
227400090216     C     TABCOD        READE     EDTAB01L                               30
227500090216     C                   END
227600090209      *
227700090209      * Caricamento Tabella Partner esteri
227800090209     C                   Z-ADD     0             X
227900090209     C                   MOVEL     'PT'          TABCOD
228000090209     C     TABCOD        CHAIN     EDTAB01L                           30
228100090209     C     *IN30         DOWEQ     '0'
228200090209      *
228300090209     C                   SELECT
228400090209     C     TABFLG        WHENNE    *BLANKS
228500090209      *
228600090209     C                   OTHER
228700090209     C                   ADD       1             X
228800090209     C                   MOVEL     TABKEY        CPT(X)
228900090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
229000090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
229100090209     C                   MOVEL     TABUNI        DPT(X)
229200090209     C                   ENDSL
229300090209      *
229400090209     C     TABCOD        READE     EDTAB01L                               30
229500090209     C                   END
229600121105      *
229700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
229800121105     c                   exsr      ChecK_PT
229900121105     C*
230000090209      * salva quanti Codici UNB ha caricato
230100090209     c                   z-add     x             sav_CPTx
230200090209      *
230300090209     C                   MOVEL     'PU'          TABCOD
230400090209     C     TABCOD        CHAIN     EDTAB01L                           30
230500090209     C     *IN30         DOWEQ     '0'
230600090209      *
230700090209     C                   SELECT
230800090209     C     TABFLG        WHENNE    *BLANKS
230900090209      *
231000090209     C                   OTHER
231100090209     C                   MOVEL     TABKEY        CODPU            35
231200090209     C                   Z-ADD     1             X
231300090209     C     CODPU         LOOKUP    CPT(X)                                 32
231400090209     C   32              MOVEL     TABUNI        DPU(X)
231500090209     C                   ENDSL
231600090209      *
231700090209     C     TABCOD        READE     EDTAB01L                               30
231800090209     C                   END
231900090209      *
232000090209      * Prosegue tab.PT e PU
232100090209     C                   MOVEL     'PV'          TABCOD
232200090209     C     TABCOD        CHAIN     EDTAB01L                           30
232300090209     C     *IN30         DOWEQ     '0'
232400090209      *
232500090209     C                   SELECT
232600090209     C     TABFLG        WHENNE    *BLANKS
232700090209      *
232800090209     C                   OTHER
232900090209     C                   MOVEL     TABKEY        CODPV            35
233000090209     C                   Z-ADD     1             X
233100090209     C     CODPV         LOOKUP    CPT(X)                                 32
233200090209     C   32              MOVEL     TABUNI        DPV(X)
233300090209     C                   ENDSL
233400090209      *
233500090209     C     TABCOD        READE     EDTAB01L                               30
233600090209     C                   END
233700090209      *
233800090209     C* Caricamento Tabella codici clienti - tariffe
233900090209     C                   Z-ADD     0             X
234000090209     C                   MOVEL     'CL'          TABCOD
234100090209     C     TABCOD        CHAIN     EDTAB01L                           30
234200090209     C     *IN30         DOWEQ     '0'
234300090209     C     TABFLG        IFEQ      *BLANKS
234400090209     C                   ADD       1             X
234500090209     C                   MOVEL     TABKEY        CCL(X)
234600090209     C                   MOVEL     TABUNI        DCL(X)
234700090209     C                   END
234800090209     C     TABCOD        READE     EDTAB01L                               30
234900090209     C                   END
235000090209      *
235100090209     C* Caricamento Serivizi speciali
235200090209     C                   Z-ADD     0             X
235300090209     C                   MOVEL     'SS'          TABCOD
235400090209     C     TABCOD        CHAIN     EDTAB01L                           30
235500090209     C     *IN30         DOWEQ     '0'
235600090209     C     TABFLG        IFEQ      *BLANKS
235700090209     C                   ADD       1             X
235800090209     C                   MOVEL     TABKEY        SS(X)
235900090209     C                   MOVEL     TABKEY        SS35(X)
236000090209     C                   MOVEL     TABUNI        DSS(X)
236100090209     C                   END
236200090209     C     TABCOD        READE     EDTAB01L                               30
236300090209     C                   END
236400090209      *
236500090209     C                   ENDSR
236600090209      * ?------------------------------------------------------------------ */
236700090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
236800090209      * ?------------------------------------------------------------------ */
236900090209     C     MSG_Errato    BEGSR
237000090209      *
237100090205      * ?  Scrive su tutti i records il tipo di errore
237200090205     c     *start        setll     tivin00r
237300090205     c                   read      tivin00r
237400091016     c                   if        %Eof(tivin00r)
237500091016     c                   move      'S'           EoFTivin
237600091016     c                   end
237700090205     c                   dow       not %eof(tivin00r)
237800090211     c                   if        vinMSG  = *blank
237900090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
238000090210     C                               >> Controllare i DATI su UPLOAD !!'
238100090211     c                   end
238200090205     C                   eval      vinFLG = '2'
238300090213     c                   eval      Almeno_Un_Due ='S'
238400090205     c                   eval      esito  = '2'
238500090209     c                   eval      se_errore ='S'
238600090213     c                   eval      err_bloccante ='S'
238700091016     c                   if        EoFTivin = ' '
238800090205     c                   update    tivin000
238900091016     c                   end
239000090205     c                   read      tivin00r
239100091016     c                   if        %Eof(tivin00r)
239200091016     c                   move      'S'           EoFTivin
239300091016     c                   end
239400090205     c                   endDO
239500090205      *
239600090205     C                   ENDSR
239700090213      * ?------------------------------------------------------------------ */
239800090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
239900090213      * ?------------------------------------------------------------------ */
240000090213     C     DETta_Errato  BEGSR
240100090213      *
240200090213      *  rilascia il record prima di rieseguire il ciclo
240300090213     c                   update    tivin000
240400090213      *
240500090213      * ?  Scrive su tutti i records il tipo di errore
240600090213     c     SavNUM_RECord setll     tivin00r
240700090213     c                   read      tivin00r
240800091016     c                   if        %Eof(tivin00r)
240900091016     c                   move      'S'           EoFTivin
241000091016     c                   end
241100090213     c                   dow       not %eof(tivin00r)
241200090213      *
241300090213      *
241400090213     c                   if        vinMSG  = *blank
241500090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
241600090213     C                             >> Controllare i DATI di ogni Segmento <<'
241700090213     c                   end
241800090213     c                   exsr      Error_DET
241900090213      *
242000090213     c                   if        VNREC = NUM_RECord
242100090213     c                   leave
242200090213     c                   end
242300090213      *
242400091016     c                   if        EoFTivin = ' '
242500090213     c                   update    tivin000
242600091016     c                   end
242700090213     c                   read      tivin00r
242800091016     c                   if        %Eof(tivin00r)
242900091016     c                   move      'S'           EoFTivin
243000091016     c                   end
243100090213      *
243200090213     c                   endDO
243300090603      *
243400091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
243500090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
243600091019???? C                             Bolle RHENUS non totalmente tradotto. -
243700100603     C                             Controllare UPLOAD del cliente 0895358.'
243800130321     c                   if             manda_mail='S'
243900090603     c                   exsr      invio_mail_AB
244000130321     c                   end
244100090603      *
244200090213      *
244300090213     C                   ENDSR
244400090205      * ?------------------------------------------------------------------ */
244500090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
244600090205      * ?------------------------------------------------------------------ */
244700090205     C     *pssr         BEGSR
244800090205     C
244900060710     C                   eval      esito = '2'
245000091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
245100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
245200100603     C                             Bolle import in filiale - messaggio EDI -
245300100603     C                             ricevuto errato su UPLOAD cliente 0895358.'
245400090603     c                   exsr      invio_mail_AB
245500060621     C
245600090213     C                   ENDSR
245700090213      * ?------------------------------------------------------------------ */
245800090213      *?      Segnala Errore su TIVIN
245900090213      * ?------------------------------------------------------------------ */
246000090213     C     Error_DET     BEGSR
246100090213     C
246200090213     C                   eval      vinFLG = '2'
246300090213     c                   eval      Almeno_Un_Due ='S'
246400090213     c                   eval      esito  = '1'
246500090213     c                   eval      se_errore ='S'
246600090213     C                   eval      Err_in_detta = 'S'
246700090213     C
246800090213     C                   ENDSR
246900090209     c*==================================================================*
247000090209     C*? CNVDAT - DECODIFICA LA DATA
247100090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
247200090209     C*         - WDATA  (DATA ALFANUMERICA)
247300090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
247400090209     C*         - WHMS   (ORA NUMERICA)
247500090209     C*----------------------------------------------------------------
247600090209     C     CNVDAT        BEGSR
247700090209     C*
247800090209     C                   MOVEL     ' '           WERRO             1
247900090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
248000090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
248100090209     C                   Z-ADD     *ZEROS        WHMS              6 0
248200090209     C*
248300090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
248400090209     C                   MOVEL     WDATA         WCOMO8            8
248500090209     C                   TESTN                   WCOMO8               99
248600090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
248700090209     C  N99              MOVEL     'S'           WERRO             1
248800090209     C                   END
248900090209     C*
249000090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
249100090209     C                   MOVEL     WDATA         WCOMO14          14
249200090209     C                   TESTN                   WCOMO14              99
249300090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
249400090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
249500090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
249600090209     C  N99              MOVEL     'S'           WERRO             1
249700090209     C                   END
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
267000110922      *
267100110922      * Imposta le NOte
267200110922     C     VABNOT        IFEQ      *BLANKS
267300110922     C                   MOVEL     wnot1         VABNOT
267400110922     C                   MOVEL     wnot2         VABNT2
267500110922     C                   ELSE
267600110922     C                   MOVEL     wnot1         VABNT2
267700110922     C                   END
267800090211      *
267900090211     C*  Attribuisco anno spedizione
268000090211     C     WDATPA        IFGT      0                                            anno sped.
268100090211     C                   MOVEL     WDATPA        VABAAS
268200090211     C                   ELSE
268300090211     C     WDATFV        IFGT      0
268400090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
268500090211     C                   END
268600090211     C                   END
268700090211      *
268800090211      *  Controllo dettaglio segnacolli
268900090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
269000090211     C                   EXSR      CTRSGE
269100090211     C                   ELSE
269200090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
269300090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
269400090211     C                   EXSR      CTRSGN
269500090211     C                   END
269600090211     C                   END
269700090211      *
269800090212      *  Imposto linea partenza e serie
269900090211     C                   eval      VABLNP = VABLNP_PT
270000090212     C                   eval      VABNRS = VABnrs_PT
270100090211      *
270200090211      *  Controllo se devo variare il codice trattamento merce
270300090211     C                   eval      VABCTM = VABctm_PT
270400090211      *
270500090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
270600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
270700090211     c                   clear                   mantiene_nsp      1            *blk/'S'
270800090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
270900090211      *
271000090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
271100090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
271200090211      *
271300090211      *  Imposto codice cliente
271400090211     c                   Select
271500090211      *
271600090211      *  Specificato codice da qualificatore FF (TD15)
271700090211     C                   When        WCLKSC <> *zeros
271800090211     C                   Z-ADD     WCLKSC        VABCCM
271900090211     C                   MOVEL     WCLTAR        VABCTR
272000090211      * forza LNP/CTM/NRS
272100090211     c                   if        WclLNP <> 0
272200090211     C                   eval      VABLNP = WclLNP
272300090211     c                   end
272400090211     c                   if        WclNRS <> 0
272500090211     C                   eval      VABNRS = WclNRS
272600090211     c                   end
272700090211     c                   if        WclCTM <> *blank
272800090211     C                   eval      VABCTM = WclCTM
272900090211     c                   end
273000090211      *
273100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
273200090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
273300090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
273400090211     c                   end
273500090211      *
273600090211      *  Specificato codice da qualificatore FF (TT15)
273700090211     C                   When        $CLKSC <> *zeros
273800090211     C                   Z-ADD     $CLKSC        VABCCM
273900090211     C                   MOVEL     $CLTAR        VABCTR
274000090211      * forza LNP/CTM/NRS
274100090211     c                   if        $clLNP <> 0
274200090211     C                   eval      VABLNP = $clLNP
274300090211     c                   end
274400090211     c                   if        $clNRS <> 0
274500090211     C                   eval      VABNRS = $clNRS
274600090211     c                   end
274700090211     c                   if        $clCTM <> *blank
274800090211     C                   eval      VABCTM = $clCTM
274900090211     c                   end
275000090211      *
275100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
275200090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
275300090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
275400090211     c                   end
275500090211      *
275600090211      *  Imposto dati da tabella - PT-
275700090211     C                   Other
275800090211      *
275900090211     C                   MOVEL     §PTKSC        VABCCM
276000090211     C                   MOVEL     §PTCTR        VABCTR
276100090211      *
276200090211     C                   Endsl
276300090211      *
276400090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
276500090211      *    o dalla relativa CL
276600090211     C                   MOVEL     VABlnp        VABfgs
276700090211      *
276800090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
276900090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
277000090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
277100090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
277200090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
277300090211      *
277400090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
277500090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
277600090211     c                   else
277700090211      *
277800090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
277900090211     C*  lo attribuisco io
278000090211      **              **------------------**
278100090211     c                   exsr      repnum
278200090211      **              **------------------**
278300090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
278400090211      **              **------------------**
278500090211     c                   end
278600090211      *
278700090211     C*  Altrimenti attribuisco data del giorno
278800090211     C     VABMGS        IFEQ      0
278900090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
279000090211     C                   MOVE      WOGGI         VABMGS                         = oggi
279100090211     C                   END
279200090211     C*  Eseguo posizionamento su EDiVAB
279300090211     C                   Z-ADD     VABAAS        KAAS
279400090211     C                   Z-ADD     VABlnp        KLNP
279500090211     C                   Z-ADD     VABNRS        KNRS
279600090211     C                   Z-ADD     VABNSP        KNSP
279700090211     C                   MOVEL     SAVBOL        SALVA
279800090211     C     KVAB1         CHAIN     EDiVAB1L                           30
279900090211     C                   MOVEL     SALVA         SAVBOL
280000090211     C*  Imposto dati CMR
280100090211     C                   Z-ADD     1             VABCNT
280200090211     **
280300090211      *** Attenzione se si deve trattare il VAX con il CMR
280400090211      *** il campo VABCNT deve essere sempre impostato a (1)
280500090211     C     §puWRK        ifEQ      'S'
280600090211     C     §puNSP        andEQ     'S'
280700090211     C                   Z-ADD     1             VABCNT
280800090211     c                   end
280900090211     **
281000090211     C                   Z-ADD     WDTPRE        VABDTS
281100090212     C                   movel     '20'          VABDTS
281200090212     C     WHMPRE        mult      100           VABHMS
281300090211     C     §PTCMR        IFEQ      'S'
281400090211     C                   Z-ADD     WDTCMR        VABDCM
281500090211     C                   MOVEL     WNRCMR        VABCMR
281600090211     C                   ELSE
281700090211     C                   Z-ADD     WDATFV        VABDCM
281800090211     C                   MOVEL     WNUMFV        VABCMR
281900090211     C                   END
282000090211     C*  Se non è indicato il nome del mittente = Partner mittente
282100090211     C     VABRMO        IFEQ      *BLANKS
282200090211     C                   MOVEL     ACORAG        VABRMO
282300090211     C                   MOVEL     INDCAP        VABCMO
282400090211     C     INDCAE        IFNE      *BLANKS
282500090211     C                   MOVEL     INDCAE        VABCMO
282600090211     C                   END
282700090211     C     INDSTA        IFNE      *BLANKS
282800090211     C                   MOVEL     INDSTA        VABNMO
282900090211     C                   END
283000090211     C                   END
283100090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
283200090211     C     §1BFG1        IFEQ      'N'
283300090211     C     §1BFG7        ANDEQ     'N'
283400090211     C     §1BFG2        ANDEQ     'N'
283500090211     C***                  Z-ADD0         VABNRS
283600090211     C                   END
283700090211     C*  Imposto segancollo da - a
283800090211     C     §1BFG1        IFEQ      'S'
283900090211     C     §1BFG7        ANDEQ     'N'
284000090211     C     §1BF12        ANDNE     'E'
284100090211      *
284200090211     C     XY            IFEQ      *ZEROS
284300090211     C                   CLEAR                   VABNCD
284400090211     C                   CLEAR                   VABNCA
284500090211     C                   ELSE
284600090211     C                   Z-ADD     SGN(1)        VABNCD
284700090211     C                   Z-ADD     SGN(XY)       VABNCA
284800090211     C                   ENDIF
284900090211     C*
285000090211     C                   END
285100090211     C*
285200090211     C*  Se cap mittente originale a blank abblenco nazione
285300090211     C     VABCMO        IFEQ      *BLANKS
285400090211     C                   MOVEL     *BLANKS       VABNMO
285500090211     C                   END
285600090603     C*
285700090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
285800090603     C* Se in testata
285900090603     c                   if        NAD_destinatario_in_testata = 'S'
286000090603     c                   eval         VABrsd =  tes_VABrsd
286100090603     c                   eval         VABrd2 =  tes_VABrd2
286200090603     c                   eval         VABind =  tes_VABind
286300090603     c                   eval         VABlod =  tes_VABlod
286400090603     c                   eval         VABnzd =  tes_VABnzd
286500090603     c                   end
286600090603     C* Se in testata
286700090603     c                   if        NAD_mittente_in_testata = 'S'
286800090603     c                   eval         VABrmo = tes_VABrmo
286900090603     c                   eval         VABnmo = tes_VABnmo
287000090603     c                   eval         VABcmo = tes_VABcmo
287100090603     c                   end
287200090603      *
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
307800130422      *
307900130422      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
308000130422      *   sulla consegna della sua merce
308100130422      *
308200130422     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
308300130422     c                                  <> *blank
308400130422     C                   Z-ADD     VABCNT        VATCNT
308500130422     C                   MOVEL     VABCMR        VATCMR
308600130422     C                   Z-ADD     VABCCM        VATCCM
308700130422     C                   movel     VABfgs        VATfgs
308800130422     C                   Z-ADD     VABLNP        VATLNP
308900130422     C                   Z-ADD     VABAAS        VATAAS
309000130422     C                   Z-ADD     VABNRS        VATNRS
309100130422     C                   Z-ADD     VABNSP        VATNSP
309200130422     C                   MOVEL     'I'           VATTRC
309300130422     C                   eval       VATNOT =
309400130422     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
309500130422     C                   WRITE     EDiVAT00                             99
309600130422     c   99              eval      errori_in_Wrt = 'S'
309700130422     c                   end
309800130422      *
309900130422     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
310000130422     c                                 <> *blank
310100130422     C                   Z-ADD     VABCNT        VATCNT
310200130422     C                   MOVEL     VABCMR        VATCMR
310300130422     C                   Z-ADD     VABCCM        VATCCM
310400130422     C                   movel     VABfgs        VATfgs
310500130422     C                   Z-ADD     VABLNP        VATLNP
310600130422     C                   Z-ADD     VABAAS        VATAAS
310700130422     C                   Z-ADD     VABNRS        VATNRS
310800130422     C                   Z-ADD     VABNSP        VATNSP
310900130422     C                   MOVEL     'J'           VATTRC
311000130422     C                   eval       VATNOT =
311100130422     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
311200130422     C                   WRITE     EDiVAT00                             99
311300130422     c   99              eval      errori_in_Wrt = 'S'
311400130422     c                   end
311500090211      *
311600090211     C                   ENDSR
311700090211     C*----------------------------------------------------------------
311800090211     C*? ESEGUO SCRITTURA EDiVAT
311900090211     C*----------------------------------------------------------------
312000090211     C     WRTVAT        BEGSR
312100090211      *
312200090211     C                   DO        §§            X
312300090211     C                   Z-ADD     VABCNT        VATCNT
312400090211     C                   MOVEL     VABCMR        VATCMR
312500090211     C                   Z-ADD     VABCCM        VATCCM
312600090211     C                   movel     VABfgs        VATfgs
312700090211     C                   Z-ADD     VABLNP        VATLNP
312800090211     C                   Z-ADD     VABAAS        VATAAS
312900090211     C                   Z-ADD     VABNRS        VATNRS
313000090211     C                   Z-ADD     VABNSP        VATNSP
313100090211     C                   MOVEL     'E'           VATTRC
313200090211      *
313300090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
313400090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
313500090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
313600090211     c                   if        WVATlun > 0
313700090211      *
313800090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
313900090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
314000090211      *
314100090211     c                   else
314200090211      *
314300090211      * Se non è impostato nulla prende il segnacollo passato così com'è
314400090211     C                   MOVEL     SGE(X)        VATNOT
314500090211     c                   end
314600090211      *
314700090213     C                   WRITE     EDiVAT00                             99
314800090213     c   99              eval      errori_in_Wrt = 'S'
314900090211     C                   END
315000090211      *
315100090211     C                   ENDSR
315200090211     C*----------------------------------------------------------------
315300090211     C*? ESEGUO SCRITTURA EDSUM
315400090211     C*----------------------------------------------------------------
315500090211     C     WRTSUM        BEGSR
315600090211     C*
315700090211     C* Se non seq. scrivo xy record
315800090211     C                   DO        XY            X
315900090211     C                   Z-ADD     VABAAS        SUMAAS
316000090211     C                   Z-ADD     VABLNP        SUMFLS
316100090211     C                   MOVEL     WKSC          SUMLNP
316200090211     C                   Z-ADD     VABLNA        SUMLNA
316300090211     C                   Z-ADD     VABZNC        SUMZNC
316400090211     C                   Z-ADD     VABNRS        SUMNRS
316500090211     C                   Z-ADD     VABNSP        SUMNSP
316600090211     C                   Z-ADD     SGN(X)        SUMNSC
316700090211     C                   Z-ADD     WKSC          SUMCCM
316800090211     C                   MOVEL     *BLANKS       SUMSTS
316900090211     C                   MOVEL     *BLANKS       SUMFLG
317000090211     C                   MOVEL     WPRGSP        SUMCNI
317100090211     C                   MOVEL     VABCMR        SUMCMR
317200090211     C                   Z-ADD     0             SUMNFE
317300090211     C                   Z-ADD     VABCCM        VADCCM
317400090213     C                   WRITE     EDSUM000                             99
317500090213     c   99              eval      errori_in_Wrt = 'S'
317600090211     C                   END
317700090211     C*
317800090211     C                   ENDSR
317900090211     c*==================================================================*
318000090211      * Manda un Msg in Posta AS Sede a EDP*
318100090211     c*==================================================================*
318200090211     c     SEND_MSG_EDP  begsr
318300090211      *
318400090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
318500090211      *   Lo manda una sola volta per lavoro
318600090211     c                   IF        Snd_Msg_alert  <> 'N' and
318700090211     c                             User_msg <> *blank
318800090211     c                   z-add     1             Jx
318900090211     C                   exsr      CalQEZSNDMG
319000090211     c                   end
319100090211      *
319200090211     c                   endsr
319300090213     c**********************************************************************
319400090213     c* reperimento numero Spedizione
319500090213     c**********************************************************************
319600090213     C     repnum        BEGSR
319700090213      *
319800090213      *    deve controllare sulla tabella "3C" se il numero spedizione
319900090213      *     deve essere mantenuto oppure no
320000090213     C*
320100090213     C                   clear                   Ds3C
320200090213     C                   Z-ADD     CODUT         TBLKUT
320300090213     C                   MOVEL     '3C'          TBLCOD
320400090213     C                   movel(p)  VABccm        TBLKEY
320500090213     C     KTABel        CHAIN     TABEL00F                           30
320600090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
320700090213     C                   MOVEL     TBLUNI        Ds3C
320800090213     C                   END
320900090213      *
321000090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
321100090213     c                   if        §3CFsp <> 'S'
321200090213      *
321300090213     C* NSP => Stacco un numeratore da AZNUM
321400090213     c                   movel     kpjbu         svkpjbu
321500090213     c                   clear                   kpjbu
321600090213     C                   clear                   TRUL33DS
321700090213     C                   eval      I33OPE = *zeros
321800090213     C                   eval      I33CNU = 302
321900090213     C                   eval      I33NUM = 1
322000090213     C                   movel     TRUL33DS      KPJBU
322100090213     C                   call      'TRUL33R'
322200090213     C                   parm                    KPJBA
322300090213     C                   movel     KPJBU         TRUL33DS
322400090213     c                   movel     svkpjbu       kpjbu
322500090213      ****
322600090213     C                   if        O33ERR = *zeros
322700090213     c                   z-add     o33nrf        NUM_SPED
322800090213     C                   else
322900090213     C                   endif
323000090213      ****
323100090213     c                   ELSE
323200090213      *   se si vuole mantenere il numero spedizione
323300090213      ****
323400090213     C                   MOVEL     WOGGI         WANNO             4 0
323500090213     C                   MOVE      WANNO         KAAA
323600090213     C                   Z-ADD     3             KCNU
323700090213     C                   Z-ADD     VABlnp        KFIL
323800090213     C     KNUF          CHAIN     FLNUF                              9999
323900090213     C     *IN99         IFEQ      '0'
324000090213     C                   ADD       1             NUFNUM
324100090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
324200090213     C                   UPDATE    FLNUF
324300090213     C                   ELSE
324400090213     C                   END
324500090213     C*
324600090213     c                   EndIF
324700090213      **
324800090213     C                   ENDSR
324900090213     C*----------------------------------------------------------------
325000090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
325100090213     C*----------------------------------------------------------------
325200090213     C     MEMCMR        BEGSR
325300090213     C*
325400090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
325500090213     C     §PTCM1        IFEQ      'S'
325600090213     C                   CLEAR                   EDRDE000
325700090213     C                   MOVEL     'TD1154'      KNMC
325800090213     C                   Z-ADD     1             KNRP
325900090213     C                   MOVEL     WKSC          KLNP1
326000090213     C     KRDE          CHAIN     EDRDE01L                           31
326100090213     C     *IN31         IFEQ      '1'
326200090213     C                   Z-ADD     VABAAS        RDEAAS
326300090213     C                   Z-ADD     VABLNP        RDELNP
326400090213     C                   Z-ADD     VABNRS        RDENRS
326500090213     C                   Z-ADD     VABNSP        RDENSP
326600090213     C                   MOVEL     'TD1154'      RDENMC
326700090213     C                   Z-ADD     1             RDENRP
326800090213     C                   MOVEL     WNRCMR        RDEVAL
326900090213     C                   WRITE     EDRDE000                             99
327000090213     c   99              eval      errori_in_Wrt = 'S'
327100090213     C                   END
327200090213     C*  Memorizzo qualificatore CMR
327300090213     C                   CLEAR                   EDRDE000
327400090213     C                   MOVEL     'TD1153'      KNMC
327500090213     C                   Z-ADD     1             KNRP
327600090213     C                   MOVEL     WKSC          KLNP1
327700090213     C     KRDE          CHAIN     EDRDE01L                           31
327800090213     C     *IN31         IFEQ      '1'
327900090213     C                   Z-ADD     VABAAS        RDEAAS
328000090213     C                   Z-ADD     VABLNP        RDELNP
328100090213     C                   Z-ADD     VABNRS        RDENRS
328200090213     C                   Z-ADD     VABNSP        RDENSP
328300090213     C                   Z-ADD     1             RDENRP
328400090213     C                   MOVEL     'TD1153'      RDENMC
328500090213     C                   MOVEL     'CMR'         RDEVAL
328600090213     C                   WRITE     EDRDE000                             99
328700090213     c   99              eval      errori_in_Wrt = 'S'
328800090213     C                   END
328900090213     C*  Memorizzo la data
329000090213     C                   CLEAR                   EDRDE000
329100090213     C                   MOVEL     'TD2380'      KNMC
329200090213     C                   Z-ADD     1             KNRP
329300090213     C                   MOVEL     WKSC          KLNP1
329400090213     C     KRDE          CHAIN     EDRDE01L                           31
329500090213     C     *IN31         IFEQ      '1'
329600090213     C                   Z-ADD     VABAAS        RDEAAS
329700090213     C                   Z-ADD     VABLNP        RDELNP
329800090213     C                   Z-ADD     VABNRS        RDENRS
329900090213     C                   Z-ADD     VABNSP        RDENSP
330000090213     C                   Z-ADD     1             RDENRP
330100090213     C                   MOVEL     'TD2380'      RDENMC
330200090213     C                   MOVEL     WDTCMR        RDEVAL
330300090213     C                   WRITE     EDRDE000                             99
330400090213     c   99              eval      errori_in_Wrt = 'S'
330500090213     C                   END
330600090213     C*
330700090213     C                   END                                                    §PTCM1 = 'S'
330800090213     C*
330900090213     C                   ENDSR
331000090213     C*----------------------------------------------------------------
331100090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
331200090213     C*----------------------------------------------------------------
331300090213     C     MEMAFB        BEGSR
331400090213     C*
331500090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
331600090213     C     §PTNF1        IFEQ      'S'
331700090213     C                   CLEAR                   EDRDE000
331800090213     C                   MOVEL     WKSC          KLNP1
331900090213     C                   MOVEL     'TD1154'      KNMC
332000090213     C                   Z-ADD     1             KNRP
332100090213     C     KRDE          CHAIN     EDRDE01L                           31
332200090213     C     *IN31         IFEQ      '1'
332300090213     C                   Z-ADD     VABAAS        RDEAAS
332400090213     C                   Z-ADD     VABLNP        RDELNP
332500090213     C                   Z-ADD     VABNRS        RDENRS
332600090213     C                   Z-ADD     VABNSP        RDENSP
332700090213     C                   MOVEL     'TD1154'      RDENMC
332800090213     C     §PTCM1        IFNE      'S'
332900090213     C     WNRCMR        OREQ      *BLANKS
333000090213     C                   Z-ADD     1             RDENRP
333100090213     C                   ELSE
333200090213     C                   Z-ADD     2             RDENRP
333300090213     C                   END
333400090213     C                   MOVEL     WNUMFV        RDEVAL
333500090213     C                   WRITE     EDRDE000                             99
333600090213     c   99              eval      errori_in_Wrt = 'S'
333700090213     C                   END
333800090213     C*  Memorizzo qualificatore CMR
333900090213     C                   CLEAR                   EDRDE000
334000090213     C                   MOVEL     WKSC          KLNP1
334100090213     C                   MOVEL     'TD1153'      KNMC
334200090213     C                   Z-ADD     1             KNRP
334300090213     C     KRDE          CHAIN     EDRDE01L                           31
334400090213     C     *IN31         IFEQ      '1'
334500090213     C                   Z-ADD     VABAAS        RDEAAS
334600090213     C                   Z-ADD     VABLNP        RDELNP
334700090213     C                   Z-ADD     VABNRS        RDENRS
334800090213     C                   Z-ADD     VABNSP        RDENSP
334900090213     C     §PTCM1        IFNE      'S'
335000090213     C     WNRCMR        OREQ      *BLANKS
335100090213     C                   Z-ADD     1             RDENRP
335200090213     C                   ELSE
335300090213     C                   Z-ADD     2             RDENRP
335400090213     C                   END
335500090213     C                   MOVEL     'TD1153'      RDENMC
335600090213     C                   MOVEL     'AFB'         RDEVAL
335700090213     C                   WRITE     EDRDE000                             99
335800090213     c   99              eval      errori_in_Wrt = 'S'
335900090213     C                   END
336000090213     C*  Memorizzo la data
336100090213     C                   CLEAR                   EDRDE000
336200090213     C                   MOVEL     WKSC          KLNP1
336300090213     C                   MOVEL     'TD2380'      KNMC
336400090213     C                   Z-ADD     1             KNRP
336500090213     C     KRDE          CHAIN     EDRDE01L                           31
336600090213     C     *IN31         IFEQ      '1'
336700090213     C                   Z-ADD     VABAAS        RDEAAS
336800090213     C                   Z-ADD     VABLNP        RDELNP
336900090213     C                   Z-ADD     VABNRS        RDENRS
337000090213     C                   Z-ADD     VABNSP        RDENSP
337100090213     C     §PTCM1        IFNE      'S'
337200090213     C     WNRCMR        OREQ      *BLANKS
337300090213     C                   Z-ADD     1             RDENRP
337400090213     C                   ELSE
337500090213     C                   Z-ADD     2             RDENRP
337600090213     C                   END
337700090213     C                   MOVEL     'TD2380'      RDENMC
337800090213     C                   MOVEL     WDATFV        RDEVAL
337900090213     C                   WRITE     EDRDE000                             99
338000090213     c   99              eval      errori_in_Wrt = 'S'
338100090213     C                   END
338200090213     C*
338300090213     C                   END                                                    §PTCM1 = 'S'
338400090213     C*
338500090213     C                   ENDSR
338600090213     c*==================================================================*
338700090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
338800090213     c*==================================================================*
338900090213     c     Imp_ADDR_mail begsr
339000090213      *
339100090213     c                   clear                   Indirizzi
339200090213      *
339300090213      *  Lunghezza indirizzi presenti
339400090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
339500090213     c                   z-add     1             al_char           3 0
339600090213     c                   z-add     1             dal_char          3 0
339700090213     c                   z-add     1             tot_char          3 0
339800090213      *
339900090213     c     successivo    tag
340000090213      *
340100090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
340200090213      *   e aggiunge il dominio Bartolini + il (;) separatore
340300090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
340400090213     c                   eval      tot_char = al_char - dal_char
340500090213     c                   z-add     dal_char      Dac
340600090213     c                   z-add     tot_char      Luc
340700090213      *
340800090213     c                   clear                   Indir_Bartol     40
340900090213     c                   clear                   Indir_Parz       20
341000090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
341100090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
341200090213     c                             %Trim(bart_it)
341300090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
341400090213      *
341500090213     c                   if        al_char = Lun_Ind
341600090213     c                   goto      fine_indmail
341700090213     c                   else
341800090213     c                   eval      dal_char = ( al_char + 1 )
341900090213     c                   goto      successivo
342000090213     c                   end
342100090213      *
342200090213     C     fine_indmail  TAG
342300090213      *
342400090213     C                   ENDSR
342500090213     c*==================================================================*
342600090213      * Manda un Msg x E-mail
342700090213     c*==================================================================*
342800090213     c     Invio_Mail    begsr
342900090213      *
343000090213     c                   goto      non_inviare
343100090213      *
343200090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
343300090213     C* Inizializzo variabili
343400090213     C                   movel     *blanks       wrkEml          253
343500090213     C                   movel     *blanks       wrkMsg         5000
343600090213     C                   movel     *blanks       wrkOgg           44
343700090213      *
343800090213     C* Valorizzo i campi della e-m@ail - indirizzo
343900090213     C                   eval      wrkEml = Indirizzi
344000090213     C                   eval      wrkOgg = Oggetto
344100090213     C                   eval      wrkMsg = Messaggio
344200090213
344300090213     C                   call(e)   'TIS701C'
344400090213     C                   parm                    wrkEml
344500090213     C                   parm                    wrkOgg
344600090213     C                   parm                    wrkMsg
344700090213
344800090213     c     non_inviare   tag
344900090213     C*
345000090213     C                   ENDSR
345100090603     c*==================================================================*
345200090603      * Manda un Msg x E-mail a EDPAB
345300090603     c*==================================================================*
345400090603     c     Invio_Mail_AB begsr
345500090603      *
345600090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
345700090603     C* Inizializzo variabili
345800090603     C                   movel     *blanks       wrkEml          253
345900090603     C                   movel     *blanks       wrkMsg         5000
346000090603     C                   movel     *blanks       wrkOgg           44
346100090603      *
346200090603     C* Valorizzo i campi della e-m@ail - indirizzo
346300110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
346400110203     C                   eval      wrkEml = Indirizzi
346500090603     C                   eval      wrkOgg = Oggetto
346600090603     C                   eval      wrkMsg = Messaggio
346700110203      ********
346800110203     C********           call(e)   'TIS701C'
346900110203     C********           parm                    wrkEml
347000110203     C********           parm                    wrkOgg
347100110203     C********           parm                    wrkMsg
347200110203     C*
347300110203      ** *****
347400110203     C                   call(e)   'TRTCT00R2'
347500110203     C                   parm                    wrkEml
347600110203     C                   parm                    wrkOgg
347700110203     C                   parm                    wrkMsg
347800090603     C*
347900090603     C                   ENDSR
348000090211     ***********************************************************************
348100090211     **
348200090211     ** Send Message (QEZSNDMG) API
348300090211     **
348400090211     ***********************************************************************
348500090211     C     CalQEZSNDMG   BEGSR
348600090211      *
348700090211     ** Invio messaggio all'utente.
348800090211     C                   EVAL      SndMg03 = msgDSP
348900090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
349000090211     C                   EVAL      SndMg05(Jx) = User_msg
349100090211     C                   EVAL      SndMg06 = Jx
349200090211     C                   CLEAR                   QUSEC
349300090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
349400090211
349500090211     C                   CALL      'QEZSNDMG'
349600090211     C                   PARM                    SndMg01
349700090211     C                   PARM                    SndMg02
349800090211     C                   PARM                    SndMg03
349900090211     C                   PARM                    SndMg04
350000090211     C                   PARM                    SndMg05
350100090211     C                   PARM                    SndMg06
350200090211     C                   PARM                    SndMg07
350300090211     C                   PARM                    SndMg08
350400090211     C                   PARM                    Qusec
350500090211     C                   PARM                    SndMg10
350600090211     C                   PARM                    SndMg11
350700090211     C                   PARM                    SndMg12
350800090211      *
350900090211     C                   ENDSR
351000121105      *---------------------------------------------------------------*
351100121105      * CTRL caricamento schiere    PT                               -*
351200121105      *---------------------------------------------------------------*
351300121105     C     Check_PT      begsr
351400121105     C*
351500121105     c* Controllo riempimento schiera
351600121105     c                   clear                   trul0sds
351700121105     c                   eval      i0sski='CPT'
351800121105     c                   eval      i0sele=%elem(CPT)
351900121105     c                   eval      i0spie=x
352000121105     c                   eval      i0sfile='EDTAB00F'
352100121105     c                   eval      i0ssif=knsif
352200121105     c                   eval      i0spgm='TRTCT04R'
352300121113     c                   move      kpjbu         SvKpjbu
352400121113     c                   clear                   kpjbu
352500121105     c                   movel     trul0sds      kpjbu
352600121105     c                   call      'TRUL0SR'
352700121105     c                   parm                    kpjba
352800121113     c                   eval      kpjbu  =  SvKpjbu
352900121105     C*
353000121105     C                   ENDSR
353100090211     O*****************************************************************
353200090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
353300090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
353400090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
353500090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
