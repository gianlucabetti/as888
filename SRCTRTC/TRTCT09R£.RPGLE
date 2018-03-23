000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600101108      *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT                          *
000700101108      *                             FM LOGISTIC (PERMA)  con  DISPOR            *
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
002200090213     Fedstbl01l IF   E           K DISK
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
003900090609     D EDPORBGM      E DS
004000090609     D EDPORCAG      E DS
004100090609     D EDPORCTA      E DS
004200091104     D EDPORCOM      E DS
004300090609     D EDPORDGS      E DS
004400090609     D EDPORDOC      E DS
004500090609     D EDPORDTM      E DS
004600090609     D EDPORGDS      E DS
004700090609     D EDPORGID      E DS
004800090609     D EDPORMSE      E DS
004900090609     D EDPORNAD      E DS
005000090609     D EDPORPCI      E DS
005100090611     D  D30_1                  4    353    DIM(10)
005200090611     D  X30            S             35    DIM(10)                              generico segnacolli
005300090609     D EDPORRFF      E DS
005400090609     D EDPORTDT      E DS
005500090609     D EDPORTSR      E DS
005600090609     D EDPORTXT      E DS
005700090609     D EDPORUNB      E DS
005800090609     D EDPORUNH      E DS
005900090609     D EDPORUNS      E DS
006000090609     D EDPORUNT      E DS
006100090609     D EDPORUNZ      E DS
006200090609     D EDPORVAL      E DS
006300090210      **
006400090205      **
006500090209     D Valori_inErr    ds
006600090209     D  SKout_Errori                  1    DIM(50)
006700090209     D Descr_Errore    ds
006800090209     D  SKout_DesErr                 50    DIM(50)
006900090209      *
007000090205      *----------------------------------------------------*
007100090205     D Tipo_seg        S              3
007200100716      *
007300100716     d keyUNBCLI       s             35
007400100716     d keyTIPOMSG      s              6
007500100716     d keyVERSION      s              3
007600100716     d keyRELEASE      s              3
007700100716     d keyAGENCY       s              3
007800100716     d keyASSOCIA      s              6
007900100716      *
008000090209     D segmento        s           2048
008100090209     D Errore          s              1
008200000223     D W0140           S             14  0
008300991129     D WORA            S              6  0
008400991129     D XX              S              3  0 INZ
008500991129     D WDTGIO          S              8  0
008600991129     D DATEU           S              8  0
008700991129     D DATA_eur        S               D   DATFMT(*eur)
008800050112     D NUM_Sped        s                   LIKE(vabnsp)
008900090213     D NUM_RECord      s                   LIKE(vnREC)
009000090213     D SAVNUM_RECord   s                   LIKE(vnREC)
009100090213     D Last_RECord     s                   LIKE(vnREC)
009200050112     D svkpjbu         s                   like(kpjbu)
009300090609     D unh_VABRMA      s                   like(vabRMA)
009400090609     D unh_VABRMN      s                   like(vabRMN)
009500090603     D NAD_destinatario_in_testata...
009600090603     D                 s              1
009700090603     D NAD_mittente_in_testata...
009800090603     D                 s              1
009900090611     D CAG_porto_in_testata...
010000090611     D                 s              1
010100090611     D In_Testata      s              1    INZ('S')
010200090611     D
010300090611     D Mittente        s             35
010400090611     D Mitt_Qualifier  s              4
010500090611     D Id_Messaggio    s             14
010600090209      *---------------------------------------------------------------------*
010700090209     D KPJBA         E DS
010800090209     D FNLV55DS      E DS
010900090209     D TIBS55DS      E DS
011000090209     D TISI95DS      E DS
011100090209     D UTEDSE0F      E DS
011200090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
011300090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
011400090209      * Ds scomposzione tipo capoconti
011500090209     D TCUDS           DS
011600090209     D  F34                    3      4
011700090209     D  F56                    5      6
011800090209     D DS1B          E DS
011900090209     D DS15          E DS
012000090209     D DSCV          E DS
012100090213     D TRTC86DS      E DS
012200090209     D EDIDSPT       E DS
012300090209     D EDIDSPU       E DS
012400090209     D EDIDSPV       E DS
012500090211     D EDIDSCL       E DS
012600090216     D EDIDSTC       E DS
012700090209      **
012800090209      *  DS x salvataggio dati impostati in EDiVAB
012900090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
013000090209     D SALVA           DS           545
013100090210      **
013200090209     D WLBDA8          DS
013300090209     D  G02DAT                 1      8  0
013400090209     D  G02INV                 9     16  0
013500090209     D  G02ERR                17     17
013600090209     D  G02TGI                18     22  0
013700090209      *  DS x scomposizione numero spedizione
013800090209     D DSSPE           DS
013900090209     D  SPEAAS                 1      4  0
014000090209     D  SPELNP                 5      7  0
014100090209     D  SPENRS                 8      9  0
014200090209     D  SPENSP                10     16  0
014300090209      *  DS x scomposizione segnacollo
014400090209     D DSSGN           DS
014500090209     D  SGNLNP                 1      3  0
014600090209     D  SGNLNA                 4      6  0
014700090209     D  SGNNRS                 7      8  0
014800090209     D  SGNNSC                 9     15  0
014900090209     D  SGNZNC                16     17  0
015000090209      *  DS x stampa segnacollo
015100090209      *---------------                                                      *
015200090611     D  Tes_VABlnp     s                   LIKE(vablnp)
015300090611     D  Tes_VABnrs     s                   LIKE(vabnrs)
015400090611     D  Tes_VABctm     s                   LIKE(vabctm)
015500090611     C*
015600090603     d tes_VABrmo      s                   LIKE(VABrmo)
015700090603     d tes_VABnmo      s                   LIKE(VABnmo)
015800090603     d tes_VABcmo      s                   LIKE(VABcmo)
015900090209     C*
016000090603     d tes_VABrsd      s                   LIKE(VABrsd)
016100090603     d tes_VABrd2      s                   LIKE(VABrd2)
016200090603     d tes_VABind      s                   LIKE(VABind)
016300090603     d tes_VABlod      s                   LIKE(VABlod)
016400090603     d tes_VABnzd      s                   LIKE(VABnzd)
016500090603     C*
016600090603     d tes_Valuta      s                   LIKE(VABvca)
016700090611     d tes_VABcbo      s                   LIKE(VABcbo)
016800090603     C*
016900090611     d tes_VABnas      s                   LIKE(VABnas)
017000090611     d tes_VABtsp      s                   LIKE(VABtsp)
017100090611     C*
017200090611     d Tes_VABdcr      s                   LIKE(VABdcr)
017300090611     d Tes_VABtcr      s                   LIKE(VABtcr)
017400090611     d Tes_VABhcr      s                   LIKE(VABhcr)
017500090611     C*
017600090611     d Tes_VABdcm      s                   LIKE(VABdcm)
017700090611     C*
017800090209      *---------------------------------------------------------------------*
017900090209      * Schiere
018000090209      *---------------------------------------------------------------------*
018100090209      * Schiera per reperimento nr.seganacollo
018200090209     D SGN             S              7  0 DIM(5000)
018300090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
018400090209      * Stringhe per conversione alfanumerico/numerico N.Documento
018500090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
018600090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
018700090209      * Nr. documento alfanumerico da decodificare
018800090209     D NDA             S              1    DIM(35)
018900090209      * Nr. documento alfanumerico da già decodificato
019000090209     D NDN             S              1    DIM(15)
019100090209      * Messaggi di errore
019200090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
019300090209      * Tabella codici trattamento merce (1B)
019400090209     D CTM             S              2    DIM(200)
019500090209     D DTM             S             67    DIM(200)
019600090209      * Tabella codici valuta
019700090209     D CCV             S              3    DIM(200)
019800090209     D DCV             S             89    DIM(200)
019900090209      * Tabella codici nazioni
020000090209     D C15             S              3    DIM(500)
020100090611     D ISO             S              2    DIM(500)
020200090209     D CPF             S              2  0 DIM(500)
020300090209      * Tabella partner
020400121105     D CPT             S             35    DIM(200)
020500121105     D CPTparz         S             35    DIM(%elem(CPT))
020600121105     D KSC_UNB         S              7    DIM(%elem(CPT))
020700121105     D DPT             S             90    DIM(%elem(CPT))
020800121105     D DPU             S             90    DIM(%elem(CPT))
020900121105     D DPV             S             90    DIM(%elem(CPT))
021000121105     D TRUL0SDS      E DS
021100090209      *
021200121105     d Ptn_parziale    s              1    inz('N')
021300121105     d sav_CPTx        s              3  0 inz(0)
021400090209      * Tabella CL --> codici clienti - tariffa
021500130305     D CCL             S             35    DIM(999)
021600130305     D DCL             S             90    DIM(999)
021700090209      * Schiere x caricamento cod.cliente  <>
021800130305     D KCL             S              7  0 DIM(999)
021900090209      * Termini di consegna
022000090209     D CTB35           S             35    DIM(100)
022100090209     D CTB             S              3    DIM(100)
022200090209     D POR             S              1    DIM(100)
022300090216      * Tipo pagamento C/Assegno
022400090216     D CTC35           S             35    DIM(100)
022500090216     D CTC             S              2    DIM(100)
022600090216     D TPI             S              2    DIM(100)
022700090209      * Decodifiche servizi speciali
022800090209     D SS35            S             35    DIM(100)
022900090209     D SS              S              3    DIM(100)
023000090209     D DSS             S             90    DIM(100)
023100090209      * Schiere x routine di conversione CAP
023200090209     D CAP5            S              1    DIM(5)
023300090209     D CAP9            S              1    DIM(9)
023400090209     D CAPM            S              1    DIM(9)
023500090209      * Schiera per memorizzazione FTX ricevuti
023600090209     D QUA             S              3    DIM(100)
023700090209     D FTX             S             70    DIM(100)
023800090209      * Tabelle capiconto
023900090209      * Schiere x località
024000090209     D LOD             S              1    DIM(35)
024100060608      **?------------------------------------------------------------------ */
024200090601     C*? Ds Scrittura del VAT "E"
024300060608      **?------------------------------------------------------------------ */
024400060612     D XTOEBCDDS     E DS
024500060620      *
024600060608      **?------------------------------------------------------------------ */
024700050112      * Numeratore Bolle (302)
024800050112     D trul33ds      E DS
024900050112     D Ds3C          E DS
025000090209     C*
025100090209      **?------------------------------------------------------------------ */
025200090209     C* Definizione campi di work
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
030900090209      *
031000090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031100090209      *
031200090209     d bart_it         C                   CONST('@Bartolini.it;')
031300090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031400060612      * ?================================================================== */
031500060612      *
031600060612      * ?   * Campi x decodifica * (INPUT  del Record)
031700090130     D  Dati           s           2048
031800060612      * ?   *--------------------------------------------------------------*
031900060612     D  se_errore      s              1    inz(' ')
032000060612      * ?* ------------------------------------------------------ *
032100060710     D Digits          C                   '0123456789'
032200060612      * ?================================================================== */
032300060612      *   Ciclo principale
032400090205      * ?================================================================== */
032500060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
032600060612     C*
032700060612     C                   if        not %open(tivin00r)
032800060612     C                   open      tivin00r
032900060612     C                   endif
033000060612      **
033100060612      * ?------------------------------------------------------------------ */
033200060612     C*? Controllo dati arrivati da DPD
033300060612      * ?------------------------------------------------------------------ */
033400060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
033500060612      * ?- controllando che la trasmissione sia completa.
033600090205      **
033700060612      * ?              /*---------------------- */
033800090205     c                   goto      nonFare_adesso
033900060612     c                   exsr      check_Trasm
034000090205     c     nonFare_adessotag
034100060612      * ?              /*---------------------- */
034200090205      **
034300060613      **  Errore di trasmissione x tutti i records
034400060613      **   --> file in errore
034500060613     c                   if        se_errore = 'S'
034600060612
034700060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
034800090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
034900090205     c                   exsr      Msg_errato
035000060612      **
035100060612     c                   else
035200060614      **
035300060612      * ?------------------------------------------------------------------ */
035400060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
035500060612      * ?------------------------------------------------------------------ */
035600090205      **
035700060612      * ?              /*---------------------- */
035800060612     c                   exsr      Importa_Msg
035900060612      * ?              /*---------------------- */
036000060612
036100060614     c                   end
036200060614
036300060612     C                   if        %open(tivin00r)
036400060612     C                   close     tivin00r
036500060612     C                   endif
036600060612      *
036700060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
036800090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
036900090225     c                             esito ='0'
037000060710     C                   eval      esito ='0'
037100090225      *
037200090225     c                   COMMIT
037300090224      *
037400090213     c                   if        almeno_un_due ='S'
037500090213     C                   eval      esito ='1'
037600060614     C                   endif
037700090213     C                   endif
037800060614      *
037900060612     c                   SETON                                        LR
038000060612      * ?================================================================== */
038100060612      *? Importa i records della tramsissione
038200060612      * ?------------------------------------------------------------------ */
038300060612     c     Importa_Msg   Begsr
038400060612
038500090211     C                   EXSR      AZZMSG
038600060614
038700060612     c     *start        setll     Tivin00r
038800060612     c                   read      Tivin00r
038900060612
039000060612     c                   dow       not %eof(Tivin00r)
039100060614
039200060614      * solo i record sflaggati da rielaborare
039300060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
039400090211      *  Sempre Record OK
039500090211      ** Controlli formali sui campi
039600090211     C                   eval      vinFLG = '1'
039700060612     c                   clear                   se_errore
039800090209
039900090213      *** ------------------------------------
040000101108      *** A FINE CICLO DI UN DETTAGLIO:   -->"RFF" e alla fine "UNT" x DISPOR
040100090213      *** ------------------------------------
040200090213      * ?              /*---------------------- */
040300090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
040400090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
040500090213      * ?              /*---------------------- */
040600090601      **
040700101108      *   OGNI DETTAGLIO è determinato dal RFF e alla fine per ultimo da UNT.
040800101108     c                   eval      tipo_seg = %subst(vinDTA:1:3)
040900101109      *
041000101109      **  ad ogni inizio dettaglio successivo scarica il precedente
041100101108      * ?              /*---------------------- */
041200101109     c                   IF            se_trovato_NAD ='S'          and
041300101109     c                               ( tipo_seg = 'UNT'             or
041400101109     c                                       ( tipo_seg = 'RFF' and
041500101109     c                               %subst(vinDTA:5:3) = 'SEN' ) )
041600090213      *
041700090213      *  Deve flaggare il dettaglio con dei problemi
041800090213     C                   if        Err_in_detta = 'S'
041900090213      **?              /*---------------------- */
042000090213     c                   exsr      DETta_Errato
042100090213      **?              /*---------------------- */
042200090213     c                   end
042300090213      *
042400060612      **  Se presente un errore nel record emette una segnalazione msg
042500090213     c                   if        se_errore = 'S'
042600090611      **
042700090611      **
042800090213     c                   else
042900090209      ******
043000090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043100090227     c                   if         se_trovato_NAD ='S'
043200060612      * ?              /*---------------------- */
043300090209     c                   exsr      Scrive_VAB
043400060612      * ?              /*---------------------- */
043500090227     c                   end
043600090227      *
043700090209      *  Problemi nella decodifica dei campi VAB/VAT
043800090213     c                   if        errori_in_Wrt = 'S'
043900090213      *
044000090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
044100090213     c                   exsr      Error_DET
044200090213      *
044300090213     c                   ROLBK
044400090213      *
044500090213     c                   else
044600090601      *
044700090213     c                   COMMIT
044800090601      *
044900090227      * Dopo aver confermato, con COMMIT
045000090227      *  si prepara per una nuova spedizione
045100090227      * ?                         --------------
045200090227     c                   exsr      Nuova_spediz
045300090227      * ?                         --------------
045400090213     c                   end
045500090209
045600090209     c                   end
045700090210      **
045800060620     c                   end
045900101108      * ?              /*---------------------- */
046000060612
046100101108      ** Decodifica record a campi variabili
046200101108      * ?              /*---------------------- */
046300101108     c                   exsr      Decod_Segmento
046400101108      * ?              /*---------------------- */
046500101108
046600060621      *  x errore bloccante nella composizione del VAB/VAT
046700060621     c                   if        err_bloccante ='S'
046800060621     C                   eval      vinFLG = '2'
046900090211     c                   eval      esito  = '2'
047000101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
047100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047200090603     C                             Bolle import in filiale - messaggio -
047300090603     C                             con UNB non presente in tabella PT controlla-
047400090603     C                             re EDI ricevuto su UPLOAD.'
047500090603     c                   exsr      invio_mail_AB
047600060621     c                   end
047700060612
047800100716     c                   update    Tivin000                             99
047900090211
048000090211      *** se non è riconosciuto l'UNB deve uscire direttamente
048100090211     c                   if        se_errore ='S'  and
048200090211     c                             tipo_seg = 'UNB'
048300090213     c                   eval      err_bloccante = 'S'
048400090211     c                   exsr      Msg_errato
048500090211     c                   LEAVE
048600090211     c                   endIF
048700090211      ***
048800060614     c                   endIF
048900060612
049000060612     c                   read      Tivin00r
049100060612     C                   ENDdo
049200060612      **
049300060612     c                   endSR
049400090211     C*----------------------------------------------------------------
049500090211     C*? AZZMSG - Azzera dati messaggio
049600090211     C*----------------------------------------------------------------
049700090211     C     AZZMSG        BEGSR
049800090211     C*
049900090211     C* Pulisco dati di work x foglio viaggio
050000090211     C                   MOVEL     *BLANKS       WNUMFV           35
050100090211     C                   MOVEL     *BLANKS       WNRCMR           35
050200090211     C                   Z-ADD     0             WDATPA            8 0
050300090211      *
050400090211      * pulisce gli RFF+FF di testata
050500090211     C                   clear                   $CLKSC            7 0
050600090211     C                   clear                   $CLTAR            3
050700090211     C                   clear                   $CLLNP            3 0
050800090211     C                   clear                   $CLNRS            2 0
050900090211     C                   clear                   $CLCTM            2
051000090211     C                   clear                   $CLBS1            1
051100090211     C                   clear                   $CLfnsp           1            *blk/S
051200090211      *
051300090211     C*  Inizializzo variabile new documento
051400090211     C                   Z-ADD     0             KCL
051500090211     C                   Z-ADD     0             WW                3 0
051600090211     ** Inizializza campo di work
051700090211     c                   move      'N'           WGID99
051800090211      * per controllare se almeno un record è stato importato sul VAB
051900090211     c                   clear                   Almeno_Uno        1
052000090213     c                   clear                   Almeno_Un_Due     1
052100090211     c                   eval      esito  = '0'
052200090211     C*
052300090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
052400090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
052500090603     c                   eval      NAD_destinatario_in_testata = *blank
052600090603     c                   eval      NAD_mittente_in_testata = *blank
052700090611     c                   eval      In_Testata = 'S'
052800090611     C*
052900090611     C                   clear                   Tes_VABdcm
053000090611     C*
053100090611     c                   clear                   tes_VABrmo
053200090611     c                   clear                   tes_VABnmo
053300090611     c                   clear                   tes_VABcmo
053400090611     C*
053500090611     c                   clear                   tes_VABrsd
053600090611     c                   clear                   tes_VABrd2
053700090611     c                   clear                   tes_VABind
053800090611     c                   clear                   tes_VABlod
053900090611     c                   clear                   tes_VABnzd
054000090611     C*
054100090611     c                   clear                   tes_Valuta
054200090611     c                   clear                   tes_VABcbo
054300090611     C*
054400090611     c                   clear                   Tes_VABdcr
054500090611     c                   clear                   Tes_VABtcr
054600090611     c                   clear                   Tes_VABhcr
054700090611     C*
054800090611     c                   clear                   tes_VABnas
054900090611     c                   clear                   tes_VABtsp
055000090611     C*
055100090611     c                   clear                   unh_VABRMA
055200090609     c                   clear                   unh_VABRMN
055300090611     C*
055400090611     C                   clear                   Tes_VABlnp
055500090611     C                   clear                   Tes_VABnrs
055600090611     C                   clear                   Tes_VABctm
055700090611     C*
055800090611     C                   clear                   VABfnsp_PT
055900090611     C*
056000090611     C                   MOVEA     *HIVAL        SGN
056100090603     C*
056200090211     C                   ENDSR
056300090210     C*----------------------------------------------------------------
056400090210      *? DECODIFICA DATI UNB
056500090210     C*----------------------------------------------------------------
056600090210     C     DATI_UNB      BEGSR
056700090210      *
056800090209      *  INIZIALIZZA  dati fondamentali messaggio
056900090209     C                   clear                   EDIDSPT
057000090209     C                   clear                   EDIDSPU
057100090209     C                   clear                   EDIDSPV
057200090209     C                   clear                   wnrDOC           35
057300090209     C                   move      'N'           PT_ok             1
057400090209      ** ---------------------------------------------------------------
057500090209      * quindi 1° cosa:
057600090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
057700090209      *     senza questo il messaggio NON è trascodificabile
057800090209     C                   MOVEL     unb0004       WCOD             35
057900090209     C                   MOVE      unb000720     WCOD
058000090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
058100090209     C                   Z-ADD     1             XX                3 0
058200090209     C     WCOD          LOOKUP    CPT(XX)                                32
058300090209      *
058400090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
058500090209     C                   If        *IN32 = *off
058600090209      * ******  Errore generale sul Messaggio
058700090209      * Msg di allerta Traduzione
058800090209     C                   EVAL      Indirizzi = CED_Bart
058900101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
059000090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
059100090209     C                             Bolle import in filiale - messaggio -
059200090209     C                             con UNB non presente in tabella PT controlla-
059300090209     C                             re EDI ricevuto su UPLOAD.'
059400090209      *
059500090209     c                   exsr      invio_mail
059600090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
059700090210     C                   eval      vinFLG = '2'
059800090210     C                   eval      se_errore   = 'S'
059900090210     c                   goto      end_UNB
060000090209      *
060100090209     C                   move      'N'           PT_ok             1
060200090209      *
060300090209      *             *------------------------------*
060400090209     C                   eLSe
060500090209      *             *------------------------------*
060600090209      * Se tutto OK su tab.PT:
060700090209     C                   move      'S'           PT_ok             1
060800090209      *
060900090209     C                   MOVEL     DPT(XX)       EDIDSPT
061000090209     C                   MOVEL     DPU(XX)       EDIDSPU
061100090209     C                   MOVEL     DPV(XX)       EDIDSPV
061200090226     C                   eval      Mittente       = unb0004
061300090226     C                   eval      Mitt_qualifier = unb000720
061400090226     C                   eval      Id_Messaggio   = UNB0022
061500090209      *
061600090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
061700090209      *  ricevuti:
061800090209     c                   movel     §PVema        mail_ind         44
061900090209      *
062000090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
062100090209     c                   if        mail_ind <> *blank
062200090209     c                   exsr      imp_ADDR_mail
062300090209     c                   end
062400090209      *
062500090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
062600090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
062700090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
062800090209     C                   if        §PULUN = *blank
062900090209     C                   eval      §PULUN = '00'
063000090209     c                   end
063100090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
063200090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
063300090209      *
063400090209     C                   Z-ADD     §PTKSC        WKSC              7 0
063500090611      *
063600090611      ** salva i dati principali
063700090611     C                   MOVEL     §PTLNP        Tes_VABlnp
063800090611     C                   MOVEL     §PTNRS        Tes_VABnrs
063900090611     C                   MOVEL     §PTCTM        Tes_VABctm
064000090209      *
064100090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
064200090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
064300090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
064400090209      *  i records trasmessi).
064500090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
064600090209      *
064700090209      *  In base al cod.trattamento merce reperisco tipo tratt.
064800090209     C                   CLEAR                   DS1B
064900090209     C                   Z-ADD     1             Y                 3 0
065000090611     C     Tes_VABctm    LOOKUP    CTM(Y)                                 32
065100090209     C     *IN32         IFEQ      '1'
065200090209     C                   MOVEL     DTM(Y)        DS1B
065300090209     C                   MOVEL     DS1B          WSVTRM           38
065400090209     C                   END
065500090209      *
065600090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
065700090209     C                   clear                   ACORAG
065800090209     C                   clear                   INDCAE
065900090209     C                   clear                   INDCAP
066000090209     C                   clear                   INDSTA
066100090209      * Decodifico partner
066200090209     C                   Z-ADD     1             KKUT
066300090209     C                   Z-ADD     KCI           KKCC
066400090209     C                   MOVE      §PTKSC        KKSC
066500090209     C     KACO          CHAIN     CNACO00F                           31
066600090209     C     KIND          CHAIN     CNIND00F                           31
066700090211      *
066800090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
066900090211      *   questa schiera è utilizzata anche per i clienti.
067000090211     C     §ptKSC        LOOKUP    KCL                                    39
067100090211     c                   If        *in39 = *off
067200090211     C                   add       1             WW
067300090211     C                   z-add     §ptKSC        KCL(WW)
067400090211     C                   Endif
067500090209      *
067600090209     C                   MOVEL     unb0017       WAA4              4 0
067700090209     C                   MOVE      WAA4          WAA2              2 0
067800090209     C                   MOVE      unb0017       WGG               2 0
067900090209     C                   MOVE      unb0017       WDTPRE            8 0
068000090611     C                   MOVEl     '20'          WDTPRE
068100090209     C                   MOVE      unb0019       WHMPRE            4 0
068200090209     C                   MOVE      unb0017       WDTMSG            6 0
068300090209     C                   MOVE      WAA2          WDTMSG
068400090209     C                   MOVEL     WGG           WDTMSG
068500090210      * potremmo
068600090210      * Utilizzare l'identificativo della trasmissione come CMR
068700090209     C                   IF        unb0022 <> *blank
068800090209     C                   MOVEL     unb0022       WNRDOC
068900090211     C                   MOVEL     unb0022       WNUMFV
069000090211     C                   MOVEL     unb0022       WNRCMR
069100090209     c                   end
069200090209      *
069300090209     c                   Endif
069400090209      **
069500090609     c     end_UNB       endSR
069600090609      * ?================================================================== */
069700090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
069800090609      * ?================================================================== */
069900090609     C     DATI_UNH      BEGSR
070000090609      *
070100090609      ** Riferimento del cliente
070200090609     C                   MOVEL     unh0062       WPRGSP           35
070300090609     C                   MOVEL     unh0062       WNSP             35
070400090609     C                   MOVEL     unh0062       WNUM             35
070500090609      * ?                                       --------
070600090609     C                   MOVEL     unh0062       unh_VABRMA
070700090609      * ?                                       --------
070800090609     C     ' '           SCAN      unh0062       WLUNGH            3 0
070900090609     C                   SUB       1             WLUNGH
071000090609     C                   EXSR      TSTNUM
071100090609     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
071200090609      * ?                                       --------
071300090609     C                   MOVEL     WNUMOK        unh_VABRMN
071400090609      * ?                                       --------
071500090609     C                   END
071600090609      **
071700090609     c     end_UNH       endSR
071800090609      * ?================================================================== */
071900090609     C*? INZIO del Messaggio
072000090609      * ?================================================================== */
072100090609     C     DATI_BGM      BEGSR
072200090609      *
072300090609      **
072400090609     c     end_BGM       endSR
072500090209      * ?================================================================== */
072600090609     C*? Tipo Servizio
072700090209      * ?================================================================== */
072800090609     C     Dati_TSR      BEGSR
072900090609      **
073000090609      **    Inviano solo spedizioni Espresso e non Priority
073100090611      **        tutte le spedizioni vanno come "Normali"
073200101108     c**********         if        tsr4751 = 'D'
073300101108     c**********         if        tsr4219 = 1 or tsr4219 = 2
073400101108     c**********         eval      VABtsp = 'E'
073500101108     c**********         end
073600101108     c**********         end
073700090611      **
073800090611      **  se presente in testata se lo memorizza qualora non fosse presente
073900090611      **   nei singoli dettagli
074000090611     c                   if        In_Testata       = 'S'
074100090611     C                   eval       tes_VABtsp = VABtsp
074200090611     c                   end
074300090609      **
074400090609     c     end_TSR       endSR
074500090610      * ?================================================================== */
074600090611     C*? Accordi commerciali
074700090610      * ?================================================================== */
074800090610     C     DATI_CAG      BEGSR
074900090611      ****
075000090611      *  Attenzione questo segmento in alcuni messaggi potrebbe trovarsi solo
075100090611      *  in testata e non in dettaglio.
075200090611      ****
075300090610      *
075400090610      *  imposta il default dalla tabella PT se c'è
075500090610     C     §puPFA        ifNE      *blank
075600090610     c                   SELECT
075700090610      *  Porto Franco
075800090610     c                   WHEN      §puPFA = 'F'
075900090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
076000090611      *  Porto Assegnato
076100090611     C                   WHEN      §puPFA = 'A'
076200090611     C                   movel     '2'           VABCBO                         Senza C/Ass
076300090610     C                   ENDSL
076400090610     c                   end
076500090610      *
076600090610      *   Imposta o il codice o il metodo
076700090610      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
076800090611     c                   if        cag4237  <> *blank
076900090611      *
077000090610     C                   movel     UNB_Mittente  etbUNB
077100090611     C                   movel(p)  'CAG'         etbSGM
077200090611     C                   movel(p)  cag4237       etbVALSGM
077300090610     c     K_TAB1L       chain     edstbl01l
077400090610      * prova quindi senza l'UNB specifico del cliente
077500090610     c                   if        not %Found(edstbl01l)
077600090610     C                   clear                   etbUNB
077700090610     c     K_TAB1L       chain     edstbl01l
077800090610     c                   end
077900090610      *
078000090610     c                   if        %Found(edstbl01l)
078100090610     c                   SELECT
078200090610     c                   WHEN      ETBDATI = 'F'
078300090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
078400090611      *  Porto Assegnato
078500090611     c                   WHEN      VABCAS > *zero
078600090611     C                   movel     '2'           VABCBO                         Senza C/Ass
078700090610     C                   ENDSL
078800090610      *
078900090610     c                   end
079000090610     c                   endIF
079100090610      **
079200090610      **  Il tipo bolla non è presente
079300090611     c                   if         VABCBO = *blank
079400090611     C                   eval      vinMSG = 'CAG non rilevato'
079500090610     c                   exsr      Error_DET
079600090611     c                   goto      end_CAG
079700090610     c                   end
079800090611      **
079900090611      **  se presente in testata se lo memorizza qualora non fosse presente
080000090611      **   nei singoli dettagli
080100090611     c                   if        In_Testata       = 'S'
080200090611     C                   eval       tes_VABcbo = VABcbo                         Senza C/Ass.
080300090611     c                   eval      CAG_porto_in_testata = 'S'
080400090611     c                   end
080500090610      **
080600090610     c     end_CAG       endSR
080700090611      * ?================================================================== */
080800090611     C*?  Anagrafica del mittente e del destinatario
080900090611      * ?================================================================== */
081000090611     C     DATI_NAD      BEGSR
081100090611      *
081200090611      * Dati destinatario
081300090611     C                   IF        nad3035 = 'CN' or
081400090611     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
081500090611      *
081600090611      * Se trovato un indirizzo di destinatario
081700090611      *   abilita la scrittura del VAB e del VAT.
081800090611     c                   eval       se_trovato_NAD ='S'
081900090611      *
082000090611     C* Reperisco nazione corrispondente destinatario
082100090611     C                   Z-ADD     1             YY                3 0
082200090611     c                   eval      *in32 = *off
082300090611     C                   if        nad3207 <> *BLANKS
082400090611     C     nad3207       LOOKUP    ISO(YY)                                32
082500090611     C                   endif
082600090611      *
082700090611     C* Imposto dati destinatario
082800090615     C                   MOVEL     nad30361      VABRSD
082900090615     C                   if        nad30362 <> *LOVAL
083000090615     C                   MOVEL     nad30362      VABRD2
083100090611     C                   endif
083200090611      *
083300090611     C                   MOVEL     nad30421      VABIND
083400090630      *
083500090630     C                   MOVEL     nad3164       VABLOD
083600090630      *
083700090611      *
083800090611     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
083900101108      *      li mettiamo in aggiunta alla 2°ragione sociale  VABRD2
084000090611     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
084100090611      *
084200090611     C* Verifico se c'è dello spazio per nella località
084300090630     C     '    '        SCAN      VABRD2        WI                4 0
084400090611      *
084500090630     C                   MOVEA     VABRD2        LOD
084600090611     C                   ADD       1             WI
084700090611     C                   MOVEA     NAD30422      LOD(WI)
084800090630     C                   MOVEA     LOD           VABRD2
084900090611     C                   ENDif
085000090611      *
085100090611     C                   If        *in32 = *on
085200090611     C                   MOVEL     C15(YY)       VABNZD
085300090611     C                   MOVEL     CPF(YY)       WFLCPF            2 0
085400090611     c                   Else
085500090611     C                   MOVEL     *BLANKS       VABNZD
085600090611     C                   Z-ADD     0             WFLCPF
085700090611     c                   Endif
085800090611      * Converto CAP
085900090611     c                   move      nad3251       cap09a            9
086000090611     C                   EXSR      CNVCAP
086100090611     C* Se in testata
086200090611     c                   if        In_Testata       = 'S'
086300090611     c                   eval       tes_VABrsd = VABrsd
086400090611     c                   eval       tes_VABrd2 = VABrd2
086500090611     c                   eval       tes_VABind = VABind
086600090611     c                   eval       tes_VABlod = VABlod
086700090611     c                   eval       tes_VABnzd = VABnzd
086800090611     c                   eval      NAD_destinatario_in_testata = 'S'
086900090611     c                   end
087000090615      *
087100090615     C* Ricerco CAP
087200090615     C*****WCAP          IFNE      *BLANKS
087300090615      *
087400090615     C     mse6799       IFEQ      'CGW'
087500090615      *
087600090615      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
087700090615     C                   CLEAR                   FNLV55DS
087800090615     C                   MOVEL     'P'           D55TPT
087900090615     C                   MOVEL     WOGGI         D55DRF
088000090615     C                   MOVEL     VABLNP        D55LNP
088100090615     C                   MOVEL     VABLNP        D55LIN
088200090615     C                   CALL      'FNLV55R'
088300090615     C                   PARM                    FNLV55DS
088400090615     C                   MOVE      D55TFP        COMTFP            3 0
088500090615      *
088600090615     C*  PASSO IL TERMINAL PARTENZA
088700090615     C                   CLEAR                   TISI95DS
088800090615     C                   Z-ADD     COMTFP        I95TFP
088900090615     C                   MOVEL     VABTSP        I95TSP
089000090615     C                   MOVEL     'S'           I95FRE
089100090615     C                   MOVEL     '3'           I95TCN
089200090615     C                   MOVEL     WCAP          I95CAP
089300090615     C                   MOVEL     VABNZD        I95NAR
089400090615     C                   MOVEL     VABLOD        I95LOC
089500090615     C                   MOVEL     VABIND        I95IND
089600090615      *
089700090615     C* Se gestita seconda linea di arrivo passo peso - volume
089800090615     C* e numero colli effettivo
089900090615     C     §PULNA        IFEQ      'S'
090000090615     C                   Z-ADD     VABPKB        I95LKG
090100090615     C                   Z-ADD     VABVLB        I95LMC
090200090615     C                   ELSE
090300090615     C                   END
090400090615     C*
090500090615     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
090600090615     C                   SELECT
090700090615     C     VABCBO        WHENEQ    '1 '
090800090615     C                   MOVEL     *BLANKS       I95FCA
090900090615     C                   MOVEL     'F'           I95TPO
091000090615     C     VABCBO        WHENEQ    '2 '
091100090615     C                   MOVEL     *BLANKS       I95FCA
091200090615     C                   MOVEL     'A'           I95TPO
091300090615     C     VABCBO        WHENEQ    '4 '
091400090615     C                   MOVEL     'S'           I95FCA
091500090615     C                   MOVEL     'F'           I95TPO
091600090615     C     VABCBO        WHENEQ    '6 '
091700090615     C                   MOVEL     'S'           I95FCA
091800090615     C                   MOVEL     'A'           I95TPO
091900090615     C                   OTHER
092000090615     C                   MOVEL     'F'           I95TPO
092100090615     C     VABCAS        IFGT      0
092200090615     C                   MOVEL     'S'           I95FCA
092300090615     C                   ELSE
092400090615     C                   MOVEL     *BLANKS       I95FCA
092500090615     C                   END
092600090615     C                   ENDSL
092700090615     C*
092800090615     C                   CALL      'TISI95R'
092900090615     C                   PARM                    TISI95DS
093000090615     C* Reperisco i dati da CAP
093100090615     C                   MOVEL     O95PRV        VABPRD
093200090615     C                   MOVEL     O95LNA        VABLNA
093300090615     C                   MOVEL     O95ZNC        VABZNC
093400090615     C                   MOVEL     WCAP          VABCAD
093500090615     C*
093600090615     C     O95ERR        IFNE      *BLANKS
093700090615     C     O95FIT        ANDEQ     'S'
093800090615     C*  Cap non trovato
093900090615     C                   eval      vinMSG = 'MSE dal peso (CAP) non trovato'
094000090615     c                   exsr      Error_DET
094100090615     C                   goto      end_NAD
094200090615     C                   END
094300090615     C                   END
094400090615      *
094500090615     C*****              END
094600090615      *
094700090615     C     WCAP          IFEQ      *BLANKS
094800090615      *
094900090615     C                   CLEAR                   VABPRD
095000090615     C                   CLEAR                   VABLNA
095100090615     C                   CLEAR                   VABZNC
095200090615     C                   CLEAR                   VABCAD
095300090615     C*  Cap non trovato
095400090615     C*******            eval      vinMSG = 'MSE dal peso (CAP) mancante'
095500090615     c*******            exsr      Error_DET
095600090615     C*******            goto      end_MSE
095700090615     C                   END
095800090611     C*
095900090611     C                   ENDIF
096000090611      *
096100090611      * Dati mittente  ---------------
096200090611     C                   IF        nad3035 = 'CO'
096300090611     C                   movel     nad30361      VABRMO
096400090611      *
096500090611      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
096600090611     C                   movel     §PTNAZ        VABNMO
096700090611      *
096800090611     C                   IF        nad3207 <> *blanks
096900090611     C                   eval      YY = 1
097000090611     C     nad3207       LOOKUP    ISO(YY)                                32
097100090611     C                   if         *in32 = *on
097200090611     C                   MOVEL     C15(YY)       VABNMO
097300090611     C                   endif
097400090611     C                   ENDIF
097500090611      *
097600090611      * Normalizzo CAP mittente originale
097700090611     C                   if        nad3251 <> *blanks
097800090611      *
097900090611     C                   clear                   T
098000090611     C                   clear                   S
098100090611     C                   MOVEA     *BLANKS       CAPM
098200090611     C                   MOVEA     nad3251       CAP9
098300090611     C                   DO        9             T
098400090611     C     CAP9(T)       IFNE      *BLANKS
098500090611     C                   ADD       1             S
098600090611     C                   MOVE      CAP9(T)       CAPM(S)
098700090611     C                   ENDIF
098800090611     C                   ENDDO
098900090611     C*
099000090611      *     Controllo validità CAP
099100090611     C                   MOVEA     CAPM          VABCMO
099200090611      *
099300090611     C                   CLEAR                   TISI95DS
099400090611     C                   MOVEL     VABCMO        I95CAP
099500090611     C                   MOVEL     VABNMO        I95NAR
099600090611     C                   MOVEL     WOGGI         I95DAT
099700090611     C                   MOVEL     '3'           I95TCN
099800090611     C                   CALL      'TISI95R'
099900090611     C                   PARM                    TISI95DS
100000090611      *     Errore
100100090611     C     O95ERR        IFNE      *BLANKS
100200090611     C                   MOVEL     INDCAE        VABCMO
100300090611     C                   MOVEL     §PTNAZ        VABNMO
100400090611     C                   END
100500090611      *
100600090611     c                   endIF
100700090611     C* Se in testata
100800090611     c                   if        In_Testata       = 'S'
100900090611     c                   eval       tes_VABrmo = VABrmo
101000090611     c                   eval       tes_VABnmo = VABnmo
101100090611     c                   eval       tes_VABcmo = VABcmo
101200090611     c                   eval       NAD_mittente_in_testata = 'S'
101300090611     c                   end
101400090611      *
101500090611     C                   END
101600090611      *
101700090611      * NOn c'è l'identificativo bolla del cliente
101800090611     C                   IF        nad3035 = 'CN'
101900090611     C                   if          vabRSD = *blank or
102000090611     C                               vabIND = *blank or
102100090611     C                               vabLOD = *blank
102200090611     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
102300090611     c                   exsr      Error_DET
102400090611     c                   goto      end_NAD
102500090611     C                   ENDIF
102600090611     C                   ENDIF
102700090611      *
102800090611     c     end_NAD       endSR
102900090611      * ?================================================================== */
103000090611     C*? Imposta le date
103100090611      * ?================================================================== */
103200090611     C     DATI_DTM      BEGSR
103300090611     C*
103400090611     C*  Se data documento  memorizzo numero e data x CMR
103500090611     C     dtm2005       IFEQ      'DEP'
103600090611     C                   MOVEL     '20'          W_DATA8           8            Data
103700090615     C                   MOVE      dtm2001       W_DATA8
103800090611     C                   MOVEL     W_DATA8       WDATA            35            Data
103900090611     C                   MOVEL     '102'         WFORM             3            Formato
104000090611     C                   EXSR      CNVDAT
104100090611     C                   MOVEL     WCAMG         Tes_VABdcm                     Data CMR
104200090611     C     WERRO         IFEQ      'S'
104300090611     C                   eval      vinMSG = 'DTM data msg errata'
104400090611     c                   exsr      Error_DET
104500090611     c                   goto      end_DTM
104600090611     C                   END
104700090611     C                   END
104800090611      **
104900090611     C*  Se data consegna richiesta imposto già sui campi del VAB
105000090611     C     dtm2005       IFEQ      'DAD'
105100090611     C                   MOVEL     '20'          W_DATA8           8            Data
105200090615     C                   MOVE      dtm2001       W_DATA8
105300090611     C                   MOVEL     W_DATA8       WDATA            35            Data
105400090611     C                   MOVEL     '102'         WFORM             3            Formato
105500090611     C                   EXSR      CNVDAT
105600090828     C***  In accordo con il cliente NON si prende + la SUA Data cons.rich.
105700090828     C***  poichè il cliente inviava una data uguale a quella della spedizione
105800090828     C***  e non poteva modificare nulla nel suo sistema.
105900090828     C***   autorizzato da Pantalena il  28/08/2009
106000090828     C*******            MOVEL     WCAMG         VABDCR                         Data cons.rich.
106100090828     C*******            MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
106200090828     C*******            MOVEL     WHMS          VABHCR                         Ora  cons.rich.
106300090611     C     WERRO         IFEQ      'S'
106400090611     C                   eval      vinMSG = 'DTM DEP - data errata'
106500090611     c                   exsr      Error_DET
106600090611     c                   goto      end_DTM
106700090615     C                   ELSE
106800090615     C* Se in testata
106900090615     c                   if        In_Testata       = 'S'
107000090828     C*******            eval       Tes_VABdcr = VABDCR                          Data cons.rich.
107100090828     C*******            eval       Tes_VABtcr = VABTCR                          Tipo cons.rich.
107200090828     C*******            eval       Tes_VABhcr = VABHCR                          Ora  cons.rich.
107300090615     c                   endIf
107400090615     C                   END
107500090611     C                   END
107600090611      **
107700090611     c     end_DTM       endSR
107800090611      * ?================================================================== */
107900090611     C*? Descrizione della Merce (NATURA MERCE)
108000090611      * ?================================================================== */
108100090611     C     DATI_GDS      BEGSR
108200090611     C*
108300090611     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
108400090611     c                   if        gds70021 <> *blank
108500090611     C                   MOVEL     gds70021      Tes_VABnas
108600090611     c                   end
108700090611      **
108800090611     c     end_GDS       endSR
108900090609      * ?================================================================== */
109000090611     C*?  Dettagli sul trasporto
109100090609      * ?================================================================== */
109200090611     C     DATI_TDT      BEGSR
109300090609      *
109400090611      **
109500090611     c     end_TDT       endSR
109600090611      * ?================================================================== */
109700090611     C*? Beni Pericolosi
109800090611      * ?================================================================== */
109900090611     C     DATI_DGS      BEGSR
110000090611      **
110100090611      **
110200090611     c     end_DGS       endSR
110300090611      * ?================================================================== */
110400090611     C*? Documentazione
110500090611      * ?================================================================== */
110600090611     C     DATI_DOC      BEGSR
110700090611      **
110800090611      * Numero di ricevuta
110900090611     C                   if        doc1001 = 'WBL' and doc1791 = 'ACG'
111000090611     C                                     and doc1004 <> *blank
111100090611      *
111200090611     C                   movel     doc1004       WCU
111300090611     C                   movel     WCU           WNUM             35
111400090611     C     ' '           scan      WCU           WLUNGH
111500090611     C                   sub       1             WLUNGH
111600090611     C                   EXSR      TSTNUM
111700090611     C                   If          WOKNUM = 'S'                               Ctrl numerico
111800090611     C                   movel     WNUMOK        VABRMN
111900090611     C                   movel     'WBL'         WRMN
112000090611     C                   Endif
112100090611     C                   ENDIF
112200090611      **
112300090611     c     end_DOC       endSR
112400090611      * ?================================================================== */
112500090611     C*? Testata intermedia Inizio e Fine Dettaglio
112600090611      * ?================================================================== */
112700090611     C     DATI_UNS      BEGSR
112800090611      *
112900090615     c                   eval      In_Testata =' '
113000090615      *
113100090611      *   Inizio Dettaglio
113200090611     c                   if         UNS0081  = 'D'
113300090611     c                   end
113400090611      *
113500090611      *   Fine   Dettaglio
113600090611     c                   if         UNS0081  = 'S'
113700090611     c                   end
113800090609      **
113900090609     c     end_UNS       endSR
114000090609      * ?================================================================== */
114100090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
114200090609      * ?================================================================== */
114300090609     C     Nuova_spediz  BEGSR
114400090609      **
114500090227      * Pulisco campi del file
114600090227     c                   z-add     vnrec         SAVNUM_RECord
114700090227     C                   CLEAR                   EDiVAB00
114800090227     C                   CLEAR                   EDiVAT00
114900090227     C                   CLEAR                   EDiVAD00
115000090611      *
115100090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
115200090227     C                   MOVEL     'N'           WGID99            1
115300090611      *
115400090227     C                   clear                   XY                5 0
115500090227     C                   clear                   §§                5 0
115600090611      *
115700090227     C                   clear                   WNSP             35
115800090227     C                   MOVEA     *HIVAL        SGN
115900090611      *
116000090227     C*  Azzero codice cliente - tariffa
116100090227     C                   Z-ADD     0             WTOCOC           16 0
116200090227     C                   Z-ADD     0             WTOPLC           16 2          ???
116300090227     C                   Z-ADD     0             WTOPNC           16 2          ???
116400090611      *
116500090611     C                   clear                   QUA
116600090611     C                   clear                   FTX
116700090611      *
116800090227     C*  Azzero codice cliente - tariffa
116900090227     C                   clear                   WCLKSC
117000090227     C                   clear                   WCLTAR
117100090227     C                   clear                   WCLLNP
117200090227     C                   clear                   WCLNRS
117300090227     C                   clear                   WCLCTM
117400090227     C                   clear                   WCLBS1
117500090227     C                   clear                   WCLfnsp                        *blk/S
117600090227     C                   clear                   WNOT1            35
117700090227     C                   clear                   WNOT2            35
117800090227      *
117900090227     C* pulisce campi di work
118000090227     c                   clear                   watrde           35
118100090227     c                   clear                   wattel           25
118200090227     C                   clear                   WRMN              3
118300090227      *
118400090227      * Un errore nel dettaglio
118500090227     C                   clear                   Err_in_detta      1
118600090227     c                   clear                   errori_in_Wrt     1
118700090227     c                   clear                   se_trovato_NAD    1
118800090227      **
118900090611      **  imposta i dati x la testata
119000090611     c                   exsr      DATI_TESTATA
119100090611      **
119200090227     c                   endSR
119300090609      * ?================================================================== */
119400090609     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
119500090609      * ?================================================================== */
119600090611     C     DATI_TESTATA  BEGSR
119700090609      *
119800090611      **  IMPOSTA i campi dalla testata
119900090611     C                   eval       VABLNP = Tes_VABlnp
120000090611     C                   eval       VABNRS = Tes_VABnrs
120100090611     C                   eval       VABCTM = Tes_VABctm
120200090611      **
120300090611     C                   eval      VABdcm = Tes_VABdcm
120400090611     C*
120500090611     c                   eval      VABvca = tes_Valuta
120600090611     c                   eval      VABcbo = tes_VABcbo
120700090611     C*
120800090828     C*******   Come da Accordi presi con il cliente la Data consegna richiesta
120900090828     C*******    NON verrà presa in considerazione
121000090828     c*******            eval      VABdcr = Tes_VABdcr
121100090828     c*******            eval      VABtcr = Tes_VABtcr
121200090828     c*******            eval      VABhcr = Tes_VABhcr
121300090611     C*
121400090611     c                   eval      VABnas = tes_VABnas
121500090611     c                   eval      VABtsp = tes_VABtsp
121600090611     C*
121700090611      * Se Mittente o Destinatario presi dalla Testata del messaggio:
121800090611     C* Se in testata
121900090611     c                   if        NAD_destinatario_in_testata = 'S'
122000090611     c                   eval         VABrsd =  tes_VABrsd
122100090611     c                   eval         VABrd2 =  tes_VABrd2
122200090611     c                   eval         VABind =  tes_VABind
122300090611     c                   eval         VABlod =  tes_VABlod
122400090611     c                   eval         VABnzd =  tes_VABnzd
122500090611     c                   end
122600090611     C* Se in testata
122700090611     c                   if        NAD_mittente_in_testata = 'S'
122800090611     c                   eval         VABrmo = tes_VABrmo
122900090611     c                   eval         VABnmo = tes_VABnmo
123000090611     c                   eval         VABcmo = tes_VABcmo
123100090611     c                   end
123200090611      **
123300090611     c                   endSR
123400090611      * ?================================================================== */
123500090611     C*?  Riferimento Spedizione del cliente
123600090611      * ?================================================================== */
123700090611     C     DATI_RFF      BEGSR
123800090611      *
123900090609      *  Prepara una nuova spedizione
124000090609      * ?                         --------------
124100090609     c                   exsr      Nuova_spediz
124200090609      * ?                         --------------
124300090609     C                   clear                   WAGE             35
124400110224     C                   clear                   WFF              35
124500110224     C                   clear                   WCU              35
124600090609      *
124700090609      *  Trascodifico riferimento spedizione
124800090609     c                   if        RFF1153 = 'SEN' and RFF1154 <> *BLANKS
124900090609     C                   movel(p)  RFF1154       WAGE             35
125000090609     c                   end
125100090609      **
125200090609     C                   IF         WAGE <> *BLANKS
125300090609     C                   movel     WAGE          WNSP             35
125400090609     C                   movel     WAGE          WNUM             35
125500090609     C     ' '           scan      WAGE          WLUNGH            3 0
125600090609     C                   sub       1             WLUNGH
125700090609     C                   EXSR      TSTNUM
125800090609     C                   IF         WOKNUM = 'S'                                Ctrl numerico
125900090609     C                   movel     WAGE          VABRMA
126000090609     C                   IF         WRMN = 'SEN' or WRMN = *blanks
126100090609     C                   movel     WNUMOK        VABRMN
126200090609     C                   movel     'SEN'         WRMN
126300090609     C                   ENDIF
126400090609     C                   ENDIF
126500090609     C                   ENDIF
126600090609      *
126700090609      * Codice Cliente di riferimento
126800090609     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
126900090609     C                   movel     RFF1154       WFF
127000090609     c                   END
127100090609      *
127200090609      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
127300090609     C     WFF           IFNE      *BLANKS
127400090609     C                   eval      XX = 1
127500090609     C                   movel     §PTKSC        WCCL             35
127600090609     C                   movel     WFF           WCOM28           28
127700090609     C                   move      WCOM28        WCCL
127800090609     C     WCCL          lookup    CCL(XX)                                34
127900090609     C     *IN34         IFEQ      '1'
128000090609     C                   movel     DCL(XX)       EDIDSCL
128100090611     C* Se in testata
128200090611     c                   if        In_Testata       = 'S'
128300090611      *
128400090611     C                   z-add     §CLKSC        $CLKSC
128500090611     C                   movel     §CLTAR        $CLTAR
128600090611     C                   z-add     §CLLNP        $CLLNP
128700090611     C                   z-add     §CLNRS        $CLNRS
128800090611     C                   movel     §CLCTM        $CLCTM
128900090611     C                   movel     §CLBS1        $CLBS1
129000090611     C                   movel     §CLnsp        $CLfnsp
129100090611      *
129200090611     c                   else
129300090611      *
129400090609     C                   z-add     §CLKSC        WCLKSC            7 0
129500090609     C                   movel     §CLTAR        WCLTAR            3
129600090609     C                   z-add     §CLLNP        WCLLNP            3 0
129700090609     C                   z-add     §CLNRS        WCLNRS            2 0
129800090609     C                   movel     §CLCTM        WCLCTM            2
129900090609     C                   movel     §CLBS1        WCLBS1            1
130000090609     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
130100090611      *
130200090611     c                   end
130300090609      *
130400090609      * imposta la LNP   x il dettaglio specifco
130500090611      * imposta la Serie x il dettaglio specifco
130600090609     c                   eval      VABlnp = §CLlnp
130700090609     c                   eval      VABnrs = §CLnrs
130800090609      *
130900090609      *  In base al cod.trattamento merce reperisco tipo tratt.
131000090609      *    per un dettaglio specifico.
131100090609     C                   CLEAR                   DS1B
131200090609     C                   Z-ADD     1             Y                 3 0
131300090609     C     §CLctm        LOOKUP    CTM(Y)                                 32
131400090609     C     *IN32         IFEQ      '1'
131500090609     C                   MOVEL     DTM(Y)        DS1B
131600090609     C                   END
131700090609      *
131800090609      * Se il cliente non era sulla schiera lo aggiunge
131900090609     C     §CLKSC        lookup    KCL                                    39
132000090609     C                   if        *in39 = *off
132100090609     C                   add       1             WW
132200090609     C                   z-add     §CLKSC        KCL(WW)
132300090609     c                   endif
132400090609      *
132500090609     C                   ENDIF
132600090609     C                   ENDIF
132700090609      *
132800090611      * NON c'è l'identificativo bolla del cliente
132900090611     c                   if        (RFF1153 = 'SEN' and RFF1154 = *BLANKS)
133000090611     C                   eval      vinMSG = 'RFF riferimento cliente assente'
133100090609     c                   exsr      Error_DET
133200090609     c                   goto      end_RFF
133300090609     C                   ENDIF
133400090609      *
133500090609     c     end_RFF       endSR
133600090611      * ?================================================================== */
133700090611     C*?  Dati di dettaglio della spedizione e dei colli
133800090611      * ?================================================================== */
133900090611     C     DATI_GID      BEGSR
134000090611      *
134100090611     C* Il numero dei colli presenti nella spedizione
134200101109     C     GID706530     IFEQ      23
134300090611     C                   MOVEL     'S'           WGID99
134400101109     C                   z-add     GID722430     VABNCL
134500090611     C                   ELSE
134600090611     C     WGID99        IFEQ      'N'
134700101109     C                   z-add     gid722430     WTNCL             5 0
134800090611     C                   ADD       WTNCL         VABNCL
134900090611     C                   END
135000090611     C                   END
135100090611      *
135200090611     C                   ADD       VABNCL        WTOCOC           16 0
135300090611      *
135400090611     c     end_GID       endSR
135500090611      * ?================================================================== */
135600090611     C*?  Dati di dettaglio misure spedizione e colli
135700090611      * ?================================================================== */
135800090611     C     DATI_MSE      BEGSR
135900090611      *
136000090611     C*  Peso
136100090611     C     mse6799       IFEQ      'CGW'
136200090611      *
136300090611     C*  Controllo se ho valore totale o parziale
136400090611      *   sempre il Lordo
136500090611     C     mse6410       IFeq      'KG '
136600090611     C     WGID99        IFEQ      'S'
136700101109     C                   move      mse6314       mse6314_3        18 3          Peso
136800101109     C                   Z-ADD     mse6314_3     VABPKB                         Peso
136900090611     C                   Else
137000101109     C                   move      mse6314       mse6314_3                      Peso
137100101109     C                   ADD       mse6314_3     VABPKB                         Peso
137200090611     C                   End
137300090611     C                   ENDif
137400090615      *
137500090615     C                   END
137600090611      ***
137700090611     c     end_MSE       endSR
137800090209      * ?================================================================== */
137900090213     C*?  C.O.D. Monetary Amount
138000090209      * ?================================================================== */
138100090609     C     DATI_VAL      BEGSR
138200090210      *
138300090216     C* Controllo se campo importo numerico
138400090216      * con 3 decimali
138500090611     C     val5004       IFNE      *zeros
138600090216      *
138700090611     C                   If         val5297 = 'DE '
138800090630     C     val5004       div       100           VABIAS                         IMPORTO
138900090611     C                   MOVEL     val6345       VABVAS                         ASSICURATO
139000090216     C                   END
139100090216      *
139200090611     C                   If         §PUCAS = 'S' and val5297 = 'RE '            C/Assegno
139300090630     C     val5004       div       100           VABCAS
139400090611     C                   MOVEL     val6345       VABVCA
139500090611      *
139600090216     C                   END
139700090216      *
139800090611     C                   END
139900090213      *
140000090611     c     end_VAL       endSR
140100091104      * ?================================================================== */
140200091104     C*?  Contatto a cui far riferimento
140300091104      * ?================================================================== */
140400091104     C     DATI_CTA      BEGSR
140500091104      *
140600091104      *  memorizza il riferimento di consegna x il destinatario
140700091104      *  memorizza il riferimento di consegna nr.telefonico
140800091104     C     cta3412       ifne      *BLANKS
140900091104     c                   movel     cta3412       watrde
141000091104     c                   end
141100091104      *
141200091104     c                   endSR
141300091104      * ?================================================================== */
141400091104     C*?  Contatto telefonico  a cui far riferimento
141500091104      * ?================================================================== */
141600091104     C     DATI_COM      BEGSR
141700091104      *
141800091104     C     com3148       ifne      *BLANKS
141900091104     c                   movel     com3148       wattel
142000091104     c                   end
142100091104      *
142200091104     C                   ENDSR
142300090213      * ?================================================================== */
142400090213     C*?  FREE TEXT
142500090213      * ?================================================================== */
142600090611     C     DATI_TXT      BEGSR
142700090213      *
142800090611      * ? solo se si tratta di note di consegna
142900100311     C*******            if        txt0077 ='DEL'  and
143000100311     C*******                      txt0078 <> *blank
143100090629      *
143200100311     C                   if        txt0077 ='DEL'
143300100311      *
143400090702     c                   movel     txt0078       tipo_payment      2
143500090629      *
143600090629     c                   If         tipo_payment = 'TM'
143700100311     C                               and  %subst(txt0078:3:1)=' '
143800090629     C                   MOVEL     'TM'          VABTIC
143900090629      *
144000090629     c                   elseIf     tipo_payment = 'BM'
144100100311     C                               and  %subst(txt0078:3:1)=' '
144200090629     C                   MOVEL     'BM'          VABTIC
144300090629      *
144400090629     c                   elseIf     tipo_payment = 'CM'
144500100311     C                               and  %subst(txt0078:3:1)=' '
144600090629     C                   MOVEL     'CM'          VABTIC
144700090629      *
144800090629     c                   elseIf     tipo_payment = '  '
144900100311     C                               and  txt0078 = *BLANK
145000090629     C                   MOVEL     '  '          VABTIC
145100090629      *
145200100311     c                   elseIF     txt0078 <> *BLANK
145300090629      *
145400090611     C     WNOT1         IFEQ      *BLANKS
145500090702     C                   MOVEL     txt0078       WNOT1
145600090702     C                   MOVE      txt0078       WNOT2
145700090611     C                   ELSE
145800090611     C     WNOT2         IFEQ      *BLANKS
145900090702     C                   MOVEL     txt0078       WNOT2
146000090611     C                   END
146100090611     C                   END
146200090629      *
146300090629     c                   endIf
146400090629      *
146500090629     c                   end
146600090213      *
146700090611     c     end_TXT       endSR
146800090210      * ?================================================================== */
146900090210     C*?  Dati di dettaglio Codice a barre Segnacolli
147000090210      * ?================================================================== */
147100090210     C     DATI_PCI      BEGSR
147200090210      *
147300090611      *  Contiene i singoli segnacolli
147400090611     c                   if        PCI4233 ='23 '
147500090611      *
147600090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
147700090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
147800090210     C     §1BF12        IFEQ      'E'
147900090210     C                   EXSR      SEGNEE
148000090210     C                   ELSE
148100090210      *
148200090210      *  La prima volta controllo congruità numero segnacollo
148300090210     C     §PUBS1        IFNE      *BLANKS
148400090210     C     VABnrs        ANDNE     0
148500090210      *  Se il codice trattamento merce prevede che segnacollino i
148600090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
148700090210     C     §1BFG7        IFEQ      'S'
148800090210     C     §1BFG1        OREQ      'S'
148900090210      *  calcolo segnacollo
149000090210     C                   EXSR      SGNELA
149100090210     C                   END
149200090210     C                   END
149300090210      *
149400090210     C                   END
149500090210      *
149600090611     C                   ENDif
149700090611      *
149800090210     c     end_PCI       endSR
149900090211     C*----------------------------------------------------------------
150000090211      *? dati finali del dettaglio    UNT
150100090211     C*----------------------------------------------------------------
150200090211     C     DATI_UNT      BEGSR
150300090211      *
150400090213     c                   eval      NUM_RECord = vnREC
150500090213     c                   z-add     vnrec         last_RECord
150600090211      **
150700090211     c                   endSR
150800090211     C*----------------------------------------------------------------
150900090211      *? dati finali     UNT
151000090211     C*----------------------------------------------------------------
151100090211     C     DATI_UNZ      BEGSR
151200090211      *
151300090211      **
151400090211     c                   endSR
151500090210      *----------------------------------------------------------------
151600090611      *? Input segnacolli su schiera di 10 elementi
151700090210      *----------------------------------------------------------------
151800090210     C     INP_Segnacol  BEGSR
151900090210      *
152000090210     c                   clear                   quanti_PCI        3 0
152100090210     c                   if        §puEL1 = 0
152200090210     c                   z-add     10            quanti_PCI
152300090210     c                   else
152400090210     c                   z-add     §puEL1        quanti_PCI
152500090210     c                   end
152600090210      *
152700090210      *  Pulisce la schiera dei segnacolli da elaborare
152800090210     c                   clear                   x30
152900090210     c                   z-add     0             Nr_x30            3 0
153000090210      *
153100090210      *   riporto tutto sulla schiera generica X30
153200090210      *
153300090210     c                   do        10            limite            3 0
153400090210      * Limita quanti elementi passati in ogni PCI
153500090210      *   devono essere considerati
153600090210     c                   if        limite > quanti_PCI
153700090210     c                   Leave
153800090210     c                   end
153900090210      *
154000090210      *  Carica la schiera generale di tutti i segnacolli
154100090210     c                   if        D30_1(limite) <> *blank
154200090210     c                   add       1             Nr_x30
154300090210     c                   eval      X30(Nr_x30) = D30_1(limite)
154400090210     c                   end
154500090210     c                   enddo
154600090210      *
154700090210     C                   ENDSR
154800090210      *----------------------------------------------------------------
154900090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
155000090210      *----------------------------------------------------------------
155100090210     C     SEGNEE        BEGSR
155200090210      *
155300090210     c                   exsr      INP_Segnacol
155400090210      *
155500090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
155600090210     C                   DO        10            x                              --- 01 ---
155700090210      *
155800090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
155900090210     C     x30(X)        ANDNE     *LOVAL
156000090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
156100090210     C                   ADD       1             §§
156200090210     C                   MOVEL     x30(X)        SGE(§§)
156300090210     C                   END                                                    --- 03 ---
156400090210      *
156500090210     C                   END                                                    --- 02 ---
156600090210      *
156700090210     C                   ENDSR
156800090210     C*----------------------------------------------------------------
156900090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
157000090210     C*----------------------------------------------------------------
157100090210     C     SGNELA        BEGSR
157200090210      *
157300090210     c                   exsr      INP_Segnacol
157400090210     C*
157500090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
157600090210     C*  in base al numero elementi validi carico sgn.
157700090210     C                   DO        10            X                              --- 01 ---
157800090210     C*
157900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
158000090210     C     x30(X)        ANDNE     *LOVAL
158100090210     C*
158200090210     C                   CLEAR                   DSSGN
158300090210     C*  Controllo se devo ricevere la linea di partenza
158400090210     C                   MOVEL     'N'           WERRO             1
158500090210     C     §PULP1        IFGT      0                                            --- 03 ---
158600090210     C                   EXSR      REPLNP
158700090210     C                   END                                                    --- 03 ---
158800090210     C*  Controllo se devo ricevere la linea di arrivo
158900090210     C     §PULA1        IFGT      0                                            --- 03 ---
159000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
159100090210     C                   EXSR      REPLNA
159200090210     C                   END                                                    --- 03 ---
159300090210     C*  Controllo se devo ricevere il numero di serie
159400090210     C     §PUNR1        IFGT      0                                            --- 03 ---
159500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
159600090210     C                   EXSR      REPNRS
159700090210     C                   END                                                    --- 03 ---
159800090210     C*  Controllo se devo ricevere il numero segnacollo
159900090210     C     §PUSG1        IFGT      0                                            --- 03 ---
160000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
160100090210     C                   EXSR      REPSGN
160200090210     C                   END                                                    --- 03 ---
160300090210     C*  Controllo se devo ricevere la zona di consegna
160400090210     C     §PUZN1        IFGT      0                                            --- 03 ---
160500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
160600090210     C                   EXSR      REPZNC
160700090210     C                   END                                                    --- 03 ---
160800090210     C*  Imposto da VAB i dati a zero se mi è stato passato
160900090210     C*  numero segnacollo valido
161000090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
161100090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
161200090210     C                   Z-ADD     VABLNP        SGNLNP
161300090210     C                   END                                                    --- 04 ---
161400090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
161500090210     C                   Z-ADD     VABLNA        SGNLNA
161600090210     C                   END                                                    --- 04 ---
161700090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
161800090210     C                   Z-ADD     VABNRS        SGNNRS
161900090210     C                   END                                                    --- 04 ---
162000090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
162100090210     C                   Z-ADD     VABZNC        SGNZNC
162200090210     C                   END                                                    --- 04 ---
162300090210      *
162400090210     C*  mi reimposto LNA e zona da segnacollo
162500090210     C                   Z-ADD     SGNLNA        VABLNA
162600090210     C                   Z-ADD     SGNZNC        VABZNC
162700090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
162800090210     C                   ADD       1             XY
162900090210     C                   MOVE      SGNNSC        SGN(XY)
163000090210     C                   END                                                    --- 03 ---
163100090210     C*
163200090210     C                   END                                                    --- 02 ---
163300090210     C*
163400090210     C                   END                                                    --- 01 ---
163500090210      *
163600090210     C                   ENDSR
163700090210     C*----------------------------------------------------------------
163800090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
163900090210     C*----------------------------------------------------------------
164000090210     C     REPLNP        BEGSR
164100090210     C*
164200090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
164300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
164400090210     C*  dal numero segnacollo passatomi dal partner
164500090210     C                   Z-ADD     §PULP1        WP                2 0
164600090210     C     3             SUBST     x30(X):WP     WLNP              3
164700090210     C*
164800090210     C*  controllo che la lnp calcolata sia numerica
164900090210     C                   TESTN                   WLNP                 98
165000090210     C*
165100090210     C*  se è numerica la carico
165200090210     C     *IN98         IFEQ      '1'
165300090210     C                   MOVE      WLNP          SGNLNP
165400090210     C                   ELSE
165500090210     C*  se non è numerica stampo errore
165600090210     C                   MOVEL     'S'           WERRO             1
165700090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
165800090213     c                   exsr      Error_DET
165900090210     C                   goto      end_LNP
166000090210     C                   END
166100090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
166200090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
166300090210     C     SGNLNP        IFNE      VABLNP
166400090210     C                   MOVEL     'S'           WERRO             1
166500090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
166600090210     c                             a della Bolla'
166700090213     c                   exsr      Error_DET
166800090210     C                   goto      end_LNP
166900090210     C                   END
167000090210     C*
167100090210     C     end_LNP       ENDSR
167200090210     C*----------------------------------------------------------------
167300090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
167400090210     C*----------------------------------------------------------------
167500090210     C     REPLNA        BEGSR
167600090210     C*
167700090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
167800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
167900090210     C*  dal numero segnacollo passatomi dal partner
168000090210     C                   Z-ADD     §PULA1        WP                2 0
168100090210     C     3             SUBST     x30(X):WP     WLNA              3
168200090210     C*  controllo che la lna calcolata sia numerica
168300090210     C                   TESTN                   WLNA                 98
168400090210     C*  se è numerica la carico
168500090210     C     *IN98         IFEQ      '1'
168600090210     C                   MOVE      WLNA          SGNLNA
168700090210     C                   ELSE
168800090210     C*  se non è numerica stampo errore
168900090210     C                   MOVEL     'S'           WERRO             1
169000090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
169100090213     c                   exsr      Error_DET
169200090210     C                   goto      end_LNA
169300090210     C                   END
169400090210     C*
169500090210     C     end_LNA       ENDSR
169600090210     C*----------------------------------------------------------------
169700090210     C*? REPNRS - Reperisce numero serie
169800090210     C*----------------------------------------------------------------
169900090210     C     REPNRS        BEGSR
170000090210     C*
170100090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
170200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
170300090210     C*  dal numero segnacollo passatomi dal partner
170400090210     C                   Z-ADD     §PUNR1        WP                2 0
170500090210     C     2             SUBST     x30(X):WP     WNRS              2
170600090210     C*  controllo che il nrs calcolata sia numerico
170700090210     C                   TESTN                   WNRS                 98
170800090210     C*  se è numerica la carico
170900090210     C     *IN98         IFEQ      '1'
171000090210     C                   MOVE      WNRS          SGNNRS
171100090210     C                   ELSE
171200090210     C*  se non è numerico stampo errore
171300090210     C                   MOVEL     'S'           WERRO             1
171400090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
171500090213     c                   exsr      Error_DET
171600090210     C                   goto      end_NRS
171700090210     C                   END
171800090210      *
171900090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
172000090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
172100090210     C     SGNNRS        IFNE      VABNRS
172200090210     C                   MOVEL     'S'           WERRO             1
172300090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
172400090210     c                             o della Bolla'
172500090213     c                   exsr      Error_DET
172600090210     C                   goto      end_NRS
172700090210     C                   END
172800090210     C*
172900090210     C     end_NRS       ENDSR
173000090210     C*----------------------------------------------------------------
173100090210     C*? REPZNC - Reperisce zona consegna
173200090210     C*----------------------------------------------------------------
173300090210     C     REPZNC        BEGSR
173400090210     C*
173500090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
173600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
173700090210     C*  dal numero segnacollo passatomi dal partner
173800090210     C                   Z-ADD     §PUZN1        WP                2 0
173900090210     C     2             SUBST     x30(X):WP     WZNC              2
174000090210     C*  controllo che la zona calcolata sia numerica
174100090210     C                   TESTN                   WZNC                 98
174200090210     C*  se è numerica la carico
174300090210     C     *IN98         IFEQ      '1'
174400090210     C                   MOVE      WZNC          SGNZNC
174500090210     C                   ELSE
174600090210     C*  se non è numerica stampo errore
174700090210     C                   MOVEL     'S'           WERRO             1
174800090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
174900090213     c                   exsr      Error_DET
175000090210     C                   goto      end_ZNC
175100090210     C                   END
175200090210     C*
175300090210     C     end_ZNC       ENDSR
175400090210     C*----------------------------------------------------------------
175500090210     C*? REPSGN - Reperisce numero segnacollo
175600090210     C*----------------------------------------------------------------
175700090210     C     REPSGN        BEGSR
175800090210     C*
175900090210     C*  se viene passata nr.segnacollo estraggo sottostringa
176000090210     C*  lunga 7 chr (a partire dalla posizione iniziale
176100090210     C*  indicata in tabella)
176200090210     C*  dal numero segnacollo passatomi dal partner
176300090210     C                   Z-ADD     §PUSG1        WP                2 0
176400090210     C     7             SUBST     x30(X):WP     WSGN              7
176500090210     C*  controllo che il nr.segnacollo calcolato sia numerico
176600090210     C                   TESTN                   WSGN                 98
176700090210     C*  se è numerico lo carico
176800090210     C     *IN98         IFEQ      '1'
176900090210     C                   MOVE      WSGN          SGNNSC
177000090210     C                   ELSE
177100090210     C*  se non è numerica stampo errore
177200090210     C                   MOVEL     'S'           WERRO             1
177300090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
177400090213     c                   exsr      Error_DET
177500090210     C                   goto      end_SGN
177600090210     C                   END
177700090210     C*
177800090210     C     end_SGN       ENDSR
177900090611     C*----------------------------------------------------------------
178000090611     C*?   Inizio Messaggio
178100090611     C*----------------------------------------------------------------
178200051205     C     *INZSR        BEGSR
178300051205      *
178400991129     c     *ENTRY        PLIST
178500060613     C                   parm                    esito             1
178600971215      *
178700050112     C     KTABel        KLIST
178800050112     C                   KFLD                    TBLKUT
178900050112     C                   KFLD                    TBLCOD
179000050112     C                   KFLD                    TBLKEY
179100090210      *
179200090210     C     K_TAB1L       KLIST
179300090213     C                   KFLD                    etbUNB
179400090213     C                   KFLD                    etbSGM
179500090213     C                   KFLD                    etbVALSGM
179600090209     C*
179700090209     C* Definisco chiavi di accesso
179800090209     C     KTAB1         KLIST
179900090209     C                   KFLD                    KKUT
180000090209     C                   KFLD                    KCOD
180100090209     C*
180200090209     C     KNUF          KLIST
180300090209     C                   KFLD                    KAAA
180400090209     C                   KFLD                    KCNU
180500090209     C                   KFLD                    KFIL
180600090209      *
180700090209     C     KVAB1         KLIST
180800090209     C                   KFLD                    KCCM
180900090209     C                   KFLD                    KCMR
181000090209     C                   KFLD                    KCNT
181100090209     C                   KFLD                    KAAS
181200090209     C                   KFLD                    KLNP
181300090209     C                   KFLD                    KNRS
181400090209     C                   KFLD                    KNSP
181500090209      *
181600090209     C     KVAB2         KLIST
181700090209     C                   KFLD                    KCCM
181800090209     C                   KFLD                    KCMR
181900090209      *
182000090209     C     KRDE          KLIST
182100090209     C                   KFLD                    KAAS
182200090209     C                   KFLD                    KLNP1
182300090209     C                   KFLD                    KNRS
182400090209     C                   KFLD                    KNSP
182500090209     C                   KFLD                    KNMC
182600090209     C                   KFLD                    KNRP
182700090209      *
182800090209     C     KACO          KLIST
182900090209     C                   KFLD                    KKUT
183000090209     C                   KFLD                    KKCC
183100090209     C                   KFLD                    KKSC
183200090209      *
183300090209     C     KIND          KLIST
183400090209     C                   KFLD                    KKUT
183500090209     C                   KFLD                    KKCC
183600090209     C                   KFLD                    KKSC
183700090209      *
183800090209      * DETERMINO SE SONO BARTOLINI O SDI
183900090209     C                   CLEAR                   TIBS55DS
184000090209     C                   MOVEL     'L'           I50TLA
184100090209     C                   CALL      'TIBS55R'
184200090209     C                   PARM                    TIBS55DS
184300090209      *
184400090209     C* RECUPERO DATI DELL'UTENTE
184500090209     C                   Z-ADD     1             CODUT             1 0
184600090209     C                   CALL      'XPARUT'
184700090209     C                   PARM                    UTEDSE0F
184800090209     C                   MOVEL     RAGUT         RSUT             20
184900090209     C*
185000090209     C* RICERCA CAPOCONTI
185100090209     C                   DO        50            X
185200090209     C                   MOVE      TCU(X)        TCUDS
185300090209     C     F56           IFEQ      'CG'
185400090209     C     F34           ANDEQ     '01'
185500090209     C                   Z-ADD     KCU(X)        KCI               4 0
185600090209     C                   END
185700090209     C                   END
185800090209     C*
185900090211     c                   movel(p)  'EDPAB'       User_Msg         10
186000090211     C*
186100090209     C* IMPOSTO A ZERO VARIABILI DI PGM
186200090209     c                   move      *blank        Snd_Msg_alert     1
186300090209     C                   MOVEL     CA(1)         WCA              78
186400090209     C                   MOVEL     CN(1)         WCN              78
186500090209     C*
186600090209     C* RECUPERO DATA E ORA
186700090209     C                   TIME                    WHHDAT           14 0
186800090209     C                   MOVE      WHHDAT        G02DAT
186900090209     C                   MOVE      WHHDAT        WDATA8            8 0
187000090209     C                   MOVE      WDATA8        WAA2              2 0
187100090209     C                   MOVEL     WDATA8        DATA              6 0
187200090209     C                   MOVE      WAA2          DATA
187300090209     C                   MOVEL     *BLANK        G02ERR
187400090209     C                   CALL      'XSRDA8'
187500090209     C                   PARM                    WLBDA8
187600090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
187700090209     C                   TIME                    WORA              6 0
187800090209      * Recupero data e ora
187900090209     C                   TIME                    W0140
188000090209      * UDATE IN GGMMAAAA
188100090209     C                   MOVE      W0140         WDTGIO
188200090209      * UDATE IN AAAAMMGG
188300090209     C     *eur          MOVEL     WDTGIO        DATA_eur
188400090209     C     *iso          MOVEL     DATA_eur      dateu
188500090209      *
188600090209      * ?              /*--------------------------- */
188700090209     c                   exsr      CARICA_TABELLE
188800090209      * ?              /*--------------------------- */
188900090209      *
189000090209     C                   MOVEL     *ALL'-'       CMP132          132
189100991124      *
189200991124     C                   ENDSR
189300060621      * ?------------------------------------------------------------------ */
189400090209      *?    CARICA TABELLE SU SCHIERE
189500060621      * ?------------------------------------------------------------------ */
189600090209     C     CARICA_TABELLEBEGSR
189700090205      *
189800090209     C* Caricamento Tabella 1B
189900090209     C                   Z-ADD     0             X                 4 0
190000090209     C                   Z-ADD     CODUT         KKUT
190100090209     C                   MOVEL     '1B'          KCOD
190200090209     C     KTAB1         CHAIN     TABEL00F                           30
190300090209     C     *IN30         DOWEQ     '0'
190400090209     C     TBLFLG        IFEQ      *BLANKS
190500090209     C                   ADD       1             X
190600090209     C                   MOVEL     TBLKEY        CTM(X)
190700090209     C                   MOVEL     TBLUNI        DTM(X)
190800090209     C                   END
190900090209     C     KTAB1         READE     TABEL00F                               30
191000090209     C                   END
191100090209      *
191200090209     C* Caricamento Tabella 15
191300090209     C                   Z-ADD     0             X                 4 0
191400090209     C                   Z-ADD     CODUT         KKUT
191500090209     C                   MOVEL     '15'          KCOD
191600090209     C     KTAB1         CHAIN     TABEL00F                           30
191700090209     C     *IN30         DOWEQ     '0'
191800090209     C     TBLFLG        IFEQ      *BLANKS
191900090209     C                   ADD       1             X
192000090209     C                   MOVEL     TBLKEY        C15(X)
192100090209     C                   MOVEL     TBLUNI        DS15
192200090611     C                   MOVEL     §15ISO        ISO(X)
192300090209     C                   MOVEL     §15ECF        CPF(X)
192400090209     C                   MOVE      §15PCF        CPF(X)
192500090209     C                   END
192600090209     C     KTAB1         READE     TABEL00F                               30
192700090209     C                   END
192800090209      *
192900090209     C* Caricamento Tabella CV
193000090209     C                   Z-ADD     0             X
193100090209     C                   Z-ADD     CODUT         KKUT
193200090209     C                   MOVEL     'CV'          KCOD
193300090209     C     KTAB1         CHAIN     TABEL00F                           30
193400090209     C     *IN30         DOWEQ     '0'
193500090209     C     TBLFLG        IFEQ      *BLANKS
193600090209     C                   ADD       1             X
193700090209     C                   MOVEL     TBLKEY        CCV(X)
193800090209     C                   MOVEL     TBLUNI        DCV(X)
193900090209     C                   END
194000090209     C     KTAB1         READE     TABEL00F                               30
194100090209     C                   END
194200090216      *
194300090216     C* Caricamento Tabella Tipo Incasso C/Assegno
194400090216     C                   Z-ADD     0             X
194500090216     C                   MOVEL     'TC'          TABCOD
194600090216     C     TABCOD        CHAIN     EDTAB01L                           30
194700090216     C     *IN30         DOWEQ     '0'
194800090216     C     TABFLG        IFEQ      *BLANKS
194900090216     C                   ADD       1             X
195000090216     C                   MOVEL     TABKEY        CTC35(X)
195100090216     C                   MOVEL     TABKEY        CTC(X)
195200090216     C                   MOVEL     TABUNI        TPI(X)
195300090216     C                   END
195400090216     C     TABCOD        READE     EDTAB01L                               30
195500090216     C                   END
195600090209      *
195700090209      * Caricamento Tabella Partner esteri
195800090209     C                   Z-ADD     0             X
195900090209     C                   MOVEL     'PT'          TABCOD
196000090209     C     TABCOD        CHAIN     EDTAB01L                           30
196100090209     C     *IN30         DOWEQ     '0'
196200090209      *
196300090209     C                   SELECT
196400090209     C     TABFLG        WHENNE    *BLANKS
196500090209      *
196600090209     C                   OTHER
196700090209     C                   ADD       1             X
196800090209     C                   MOVEL     TABKEY        CPT(X)
196900090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
197000090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
197100090209     C                   MOVEL     TABUNI        DPT(X)
197200090209     C                   ENDSL
197300090209      *
197400090209     C     TABCOD        READE     EDTAB01L                               30
197500090209     C                   END
197600121105      *
197700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
197800121105     c                   exsr      ChecK_PT
197900121105      *
198000090209      * salva quanti Codici UNB ha caricato
198100090209     c                   z-add     x             sav_CPTx
198200090209      *
198300090209     C                   MOVEL     'PU'          TABCOD
198400090209     C     TABCOD        CHAIN     EDTAB01L                           30
198500090209     C     *IN30         DOWEQ     '0'
198600090209      *
198700090209     C                   SELECT
198800090209     C     TABFLG        WHENNE    *BLANKS
198900090209      *
199000090209     C                   OTHER
199100090209     C                   MOVEL     TABKEY        CODPU            35
199200090209     C                   Z-ADD     1             X
199300090209     C     CODPU         LOOKUP    CPT(X)                                 32
199400090209     C   32              MOVEL     TABUNI        DPU(X)
199500090209     C                   ENDSL
199600090209      *
199700090209     C     TABCOD        READE     EDTAB01L                               30
199800090209     C                   END
199900090209      *
200000090209      * Prosegue tab.PT e PU
200100090209     C                   MOVEL     'PV'          TABCOD
200200090209     C     TABCOD        CHAIN     EDTAB01L                           30
200300090209     C     *IN30         DOWEQ     '0'
200400090209      *
200500090209     C                   SELECT
200600090209     C     TABFLG        WHENNE    *BLANKS
200700090209      *
200800090209     C                   OTHER
200900090209     C                   MOVEL     TABKEY        CODPV            35
201000090209     C                   Z-ADD     1             X
201100090209     C     CODPV         LOOKUP    CPT(X)                                 32
201200090209     C   32              MOVEL     TABUNI        DPV(X)
201300090209     C                   ENDSL
201400090209      *
201500090209     C     TABCOD        READE     EDTAB01L                               30
201600090209     C                   END
201700090209      *
201800090209     C* Caricamento Tabella codici clienti - tariffe
201900090209     C                   Z-ADD     0             X
202000090209     C                   MOVEL     'CL'          TABCOD
202100090209     C     TABCOD        CHAIN     EDTAB01L                           30
202200090209     C     *IN30         DOWEQ     '0'
202300090209     C     TABFLG        IFEQ      *BLANKS
202400090209     C                   ADD       1             X
202500090209     C                   MOVEL     TABKEY        CCL(X)
202600090209     C                   MOVEL     TABUNI        DCL(X)
202700090209     C                   END
202800090209     C     TABCOD        READE     EDTAB01L                               30
202900090209     C                   END
203000090209      *
203100090209     C* Caricamento Serivizi speciali
203200090209     C                   Z-ADD     0             X
203300090209     C                   MOVEL     'SS'          TABCOD
203400090209     C     TABCOD        CHAIN     EDTAB01L                           30
203500090209     C     *IN30         DOWEQ     '0'
203600090209     C     TABFLG        IFEQ      *BLANKS
203700090209     C                   ADD       1             X
203800090209     C                   MOVEL     TABKEY        SS(X)
203900090209     C                   MOVEL     TABKEY        SS35(X)
204000090209     C                   MOVEL     TABUNI        DSS(X)
204100090209     C                   END
204200090209     C     TABCOD        READE     EDTAB01L                               30
204300090209     C                   END
204400090209      *
204500090209     C                   ENDSR
204600090209      * ?------------------------------------------------------------------ */
204700090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
204800090209      * ?------------------------------------------------------------------ */
204900090209     C     MSG_Errato    BEGSR
205000090209      *
205100090205      * ?  Scrive su tutti i records il tipo di errore
205200090205     c     *start        setll     tivin00r
205300090205     c                   read      tivin00r
205400090205     c                   dow       not %eof(tivin00r)
205500090211     c                   if        vinMSG  = *blank
205600090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
205700090210     C                               >> Controllare i DATI su UPLOAD !!'
205800090211     c                   end
205900090205     C                   eval      vinFLG = '2'
206000090213     c                   eval      Almeno_Un_Due ='S'
206100090205     c                   eval      esito  = '2'
206200090209     c                   eval      se_errore ='S'
206300090213     c                   eval      err_bloccante ='S'
206400090205     c                   update    tivin000
206500090205     c                   read      tivin00r
206600090205     c                   endDO
206700090205      *
206800090205     C                   ENDSR
206900090213      * ?------------------------------------------------------------------ */
207000090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
207100090213      * ?------------------------------------------------------------------ */
207200090213     C     DETta_Errato  BEGSR
207300090213      *
207400090213      *  rilascia il record prima di rieseguire il ciclo
207500090213     c                   update    tivin000
207600090213      *
207700090213      * ?  Scrive su tutti i records il tipo di errore
207800090213     c     SavNUM_RECord setll     tivin00r
207900090213     c                   read      tivin00r
208000090213     c                   dow       not %eof(tivin00r)
208100090213      *
208200090213      *
208300090213     c                   if        vinMSG  = *blank
208400090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
208500090213     C                             >> Controllare i DATI di ogni Segmento <<'
208600090213     c                   end
208700090213     c                   exsr      Error_DET
208800090213      *
208900090213     c                   if        VNREC = NUM_RECord
209000090213     c                   leave
209100090213     c                   end
209200090213      *
209300090213     c                   update    tivin000
209400090213     c                   read      tivin00r
209500090213      *
209600090213     c                   endDO
209700090603      *
209800101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
209900090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
210000101108     C                             Bolle PERMA  non totalmente tradotto. -
210100101109     C                             Controllare UPLOAD del cliente '  +
210200101109     c                             %editc(wksc:'Z')
210300090603     c                   exsr      invio_mail_AB
210400090603      *
210500090213      *
210600090213     C                   ENDSR
210700090205      * ?------------------------------------------------------------------ */
210800090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
210900090205      * ?------------------------------------------------------------------ */
211000090205     C     *pssr         BEGSR
211100090205     C
211200060710     C                   eval      esito = '2'
211300101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
211400090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
211500090603     C                             Bolle import in filiale - messaggio -
211600090603     C                             EDI ricevuto errato su UPLOAD.'
211700090603     c                   exsr      invio_mail_AB
211800060621     C
211900090213     C                   ENDSR
212000090213      * ?------------------------------------------------------------------ */
212100090213      *?      Segnala Errore su TIVIN
212200090213      * ?------------------------------------------------------------------ */
212300090213     C     Error_DET     BEGSR
212400090213     C
212500090213     C                   eval      vinFLG = '2'
212600090213     c                   eval      Almeno_Un_Due ='S'
212700090213     c                   eval      esito  = '1'
212800090213     c                   eval      se_errore ='S'
212900090213     C                   eval      Err_in_detta = 'S'
213000090213     C
213100090213     C                   ENDSR
213200090209      * ?------------------------------------------------------------------ */
213300090209      *? Controlla la trasmissione   se completa
213400090209      * ?------------------------------------------------------------------ */
213500090209     c     Check_Trasm   Begsr
213600090209
213700090209     C                   clear                   se_errore
213800090209     C                   clear                   Riga_iNIZIo       1
213900090209     C                   clear                   Riga_fINe         1
214000090209      ** primo  record
214100090209     c     *start        setll     tivin00r
214200090209      *
214300090209     c     rileggi       tag
214400090209     c                   read      tivin00r
214500090209      *
214600090209     c                   if        not %eof(tivin00r)
214700090209      *
214800090209     c                   movel(p)  VINDTA        dati
214900090209     c                   if        dati = *blank
215000090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
215100090209      * le leggo e le escludo.
215200090209     C                   eval      vinFLG = '1'
215300090209     c                   update    Tivin000
215400090209     c                   goto      rileggi
215500090209     c                   end
215600090209      * ?              /*---------------------- */
215700090210     c                   exsr      Decod_Segmento
215800090209      * ?              /*---------------------- */
215900090209     c                   endif
216000090209      **
216100090209      ** ultimo record
216200090209     c     *hival        setll     tivin00r
216300090209     c     leggiAncora   tag
216400090209     c                   readp     tivin00r
216500090209     c                   if        not %eof(tivin00r)
216600090209      *
216700090209     c                   movel(p)  VINDTA        dati
216800090209     c                   if        dati = *blank
216900090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
217000090209      * le leggo e le escludo.
217100090209     C                   eval      vinFLG = '1'
217200090209     c                   update    Tivin000
217300090209     c                   goto      leggiAncora
217400090209     c                   end
217500090209      * ?              /*---------------------- */
217600090210     c                   exsr      Decod_Segmento
217700090209      * ?              /*---------------------- */
217800090209     c                   endif
217900090209
218000090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
218100090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
218200090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
218300090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
218400090209      * ?-----> Errore
218500090209     C                   eval      se_errore = 'S'
218600090209     c                   end
218700090209
218800090209     c                   endSR
218900090209      * ?------------------------------------------------------------------ */
219000090209      *?    Decodifica il segmento ed importa i campi in formato DS
219100090209      * ?------------------------------------------------------------------ */
219200090209     c     Decod_SegmentoBegsr
219300090209      **
219400090209     c                   eval      segmento = vinDTA
219500090210      **--------
219600100716     C                   eval       keyUNBCLI    = *blank
219700100716     C                   eval       keyTIPOMSG   = 'DISPOR'
219800100716     C                   eval       keyVERSION   = '2'
219900100716     C                   eval       keyRELEASE   = '3'
220000100716     C                   eval       keyAGENCY    = 'GT'
220100100716     C                   eval       keyASSOCIA   = 'GTF'
220200100716      **--------
220300090211     c                   select
220400090609      * --------
220500090609     c                   when      tipo_seg = 'BGM'
220600090609      *
220700090609     c                   call      'TRTCT05BGM'
220800090609     c                   parm                    tipo_seg
220900090609     c                   parm                    segmento
221000100716     C                   parm                    keyUNBCLI
221100100716     C                   parm                    keyTIPOMSG
221200100716     C                   parm                    keyVERSION
221300100716     C                   parm                    keyRELEASE
221400100716     C                   parm                    keyAGENCY
221500100716     C                   parm                    keyASSOCIA
221600090609     c                   parm                    Errore
221700090609     c                   parm                    EDPORBGM
221800090609     c                   parm                    Valori_inErr
221900090609     c                   parm                    Descr_Errore
222000090609      *
222100090609     C                   EXSR      DATI_BGM
222200090609      **--------
222300090609     c                   when      tipo_seg = 'CAG'
222400090609      *
222500090609     c                   call      'TRTCT05CAG'
222600090609     c                   parm                    tipo_seg
222700090609     c                   parm                    segmento
222800100716     C                   parm                    keyUNBCLI
222900100716     C                   parm                    keyTIPOMSG
223000100716     C                   parm                    keyVERSION
223100100716     C                   parm                    keyRELEASE
223200100716     C                   parm                    keyAGENCY
223300100716     C                   parm                    keyASSOCIA
223400090609     c                   parm                    Errore
223500090609     c                   parm                    EDPORCAG
223600090609     c                   parm                    Valori_inErr
223700090609     c                   parm                    Descr_Errore
223800090609      *
223900090609     C                   EXSR      DATI_CAG
224000090609      **--------
224100090609     c                   when      tipo_seg = 'CTA'
224200090609      *
224300090609     c                   call      'TRTCT05CTA'
224400090609     c                   parm                    tipo_seg
224500090609     c                   parm                    segmento
224600100716     C                   parm                    keyUNBCLI
224700100716     C                   parm                    keyTIPOMSG
224800100716     C                   parm                    keyVERSION
224900100716     C                   parm                    keyRELEASE
225000100716     C                   parm                    keyAGENCY
225100100716     C                   parm                    keyASSOCIA
225200090609     c                   parm                    Errore
225300090609     c                   parm                    EDPORCTA
225400090609     c                   parm                    Valori_inErr
225500090609     c                   parm                    Descr_Errore
225600090609      *
225700090609     C                   EXSR      DATI_CTA
225800091104      **--------
225900091104     c                   when      tipo_seg = 'COM'
226000091104      *
226100091104     c                   call      'TRTCT05COM'
226200091104     c                   parm                    tipo_seg
226300091104     c                   parm                    segmento
226400100716     C                   parm                    keyUNBCLI
226500100716     C                   parm                    keyTIPOMSG
226600100716     C                   parm                    keyVERSION
226700100716     C                   parm                    keyRELEASE
226800100716     C                   parm                    keyAGENCY
226900100716     C                   parm                    keyASSOCIA
227000091104     c                   parm                    Errore
227100091104     c                   parm                    EDPORCOM
227200091104     c                   parm                    Valori_inErr
227300091104     c                   parm                    Descr_Errore
227400091104      *
227500091104     C                   EXSR      DATI_COM
227600090609      **--------
227700090609     c                   when      tipo_seg = 'DGS'
227800090609      *
227900090609     c                   call      'TRTCT05DGS'
228000090609     c                   parm                    tipo_seg
228100090609     c                   parm                    segmento
228200100716     C                   parm                    keyUNBCLI
228300100716     C                   parm                    keyTIPOMSG
228400100716     C                   parm                    keyVERSION
228500100716     C                   parm                    keyRELEASE
228600100716     C                   parm                    keyAGENCY
228700100716     C                   parm                    keyASSOCIA
228800090609     c                   parm                    Errore
228900090609     c                   parm                    EDPORDGS
229000090609     c                   parm                    Valori_inErr
229100090609     c                   parm                    Descr_Errore
229200090609      *
229300090609     C                   EXSR      DATI_DGS
229400090609      * --------
229500090609     c                   when      tipo_seg = 'DOC'
229600090609      *
229700090609     c                   call      'TRTCT05DOC'
229800090609     c                   parm                    tipo_seg
229900090609     c                   parm                    segmento
230000100716     C                   parm                    keyUNBCLI
230100100716     C                   parm                    keyTIPOMSG
230200100716     C                   parm                    keyVERSION
230300100716     C                   parm                    keyRELEASE
230400100716     C                   parm                    keyAGENCY
230500100716     C                   parm                    keyASSOCIA
230600090609     c                   parm                    Errore
230700090609     c                   parm                    EDPORDOC
230800090609     c                   parm                    Valori_inErr
230900090609     c                   parm                    Descr_Errore
231000090609      *
231100090609     C                   EXSR      DATI_DOC
231200090609      **--------
231300090609     c                   when      tipo_seg = 'DTM'
231400090609      *
231500090609     c                   call      'TRTCT05DTM'
231600090609     c                   parm                    tipo_seg
231700090609     c                   parm                    segmento
231800100716     C                   parm                    keyUNBCLI
231900100716     C                   parm                    keyTIPOMSG
232000100716     C                   parm                    keyVERSION
232100100716     C                   parm                    keyRELEASE
232200100716     C                   parm                    keyAGENCY
232300100716     C                   parm                    keyASSOCIA
232400090609     c                   parm                    Errore
232500090609     c                   parm                    EDPORDTM
232600090609     c                   parm                    Valori_inErr
232700090609     c                   parm                    Descr_Errore
232800090609      *
232900090609     C                   EXSR      DATI_DTM
233000090609      **--------
233100090609     c                   when      tipo_seg = 'GDS'
233200090609      *
233300090609     c                   call      'TRTCT05GDS'
233400090609     c                   parm                    tipo_seg
233500090609     c                   parm                    segmento
233600100716     C                   parm                    keyUNBCLI
233700100716     C                   parm                    keyTIPOMSG
233800100716     C                   parm                    keyVERSION
233900100716     C                   parm                    keyRELEASE
234000100716     C                   parm                    keyAGENCY
234100100716     C                   parm                    keyASSOCIA
234200090609     c                   parm                    Errore
234300090609     c                   parm                    EDPORGDS
234400090609     c                   parm                    Valori_inErr
234500090609     c                   parm                    Descr_Errore
234600090609      *
234700090609     C                   EXSR      DATI_GDS
234800090609      **--------
234900090609     c                   when      tipo_seg = 'GID'
235000090609      *
235100090609     c                   call      'TRTCT05GID'
235200090609     c                   parm                    tipo_seg
235300090609     c                   parm                    segmento
235400100716     C                   parm                    keyUNBCLI
235500100716     C                   parm                    keyTIPOMSG
235600100716     C                   parm                    keyVERSION
235700100716     C                   parm                    keyRELEASE
235800100716     C                   parm                    keyAGENCY
235900100716     C                   parm                    keyASSOCIA
236000090609     c                   parm                    Errore
236100090609     c                   parm                    EDPORGID
236200090609     c                   parm                    Valori_inErr
236300090609     c                   parm                    Descr_Errore
236400090609      *
236500090609     C                   EXSR      DATI_GID
236600090609      **--------
236700090609     c                   when      tipo_seg = 'MSE'
236800090609      *
236900090609     c                   call      'TRTCT05MSE'
237000090609     c                   parm                    tipo_seg
237100090609     c                   parm                    segmento
237200100716     C                   parm                    keyUNBCLI
237300100716     C                   parm                    keyTIPOMSG
237400100716     C                   parm                    keyVERSION
237500100716     C                   parm                    keyRELEASE
237600100716     C                   parm                    keyAGENCY
237700100716     C                   parm                    keyASSOCIA
237800090609     c                   parm                    Errore
237900090609     c                   parm                    EDPORMSE
238000090609     c                   parm                    Valori_inErr
238100090609     c                   parm                    Descr_Errore
238200090609      *
238300090609     C                   EXSR      DATI_MSE
238400090609      **--------
238500090609     c                   when      tipo_seg = 'NAD'
238600090609      *
238700090609     c                   call      'TRTCT05NAD'
238800090609     c                   parm                    tipo_seg
238900090609     c                   parm                    segmento
239000100716     C                   parm                    keyUNBCLI
239100100716     C                   parm                    keyTIPOMSG
239200100716     C                   parm                    keyVERSION
239300100716     C                   parm                    keyRELEASE
239400100716     C                   parm                    keyAGENCY
239500100716     C                   parm                    keyASSOCIA
239600090609     c                   parm                    Errore
239700090609     c                   parm                    EDPORNAD
239800090609     c                   parm                    Valori_inErr
239900090609     c                   parm                    Descr_Errore
240000090609      *
240100090609     C                   EXSR      DATI_NAD
240200090609      **--------
240300090609     c                   when      tipo_seg = 'PCI'
240400090609      *
240500090609     c                   call      'TRTCT05PCI'
240600090609     c                   parm                    tipo_seg
240700090609     c                   parm                    segmento
240800100716     C                   parm                    keyUNBCLI
240900100716     C                   parm                    keyTIPOMSG
241000100716     C                   parm                    keyVERSION
241100100716     C                   parm                    keyRELEASE
241200100716     C                   parm                    keyAGENCY
241300100716     C                   parm                    keyASSOCIA
241400090609     c                   parm                    Errore
241500090609     c                   parm                    EDPORPCI
241600090609     c                   parm                    Valori_inErr
241700090609     c                   parm                    Descr_Errore
241800090609      *
241900090609     C                   EXSR      DATI_PCI
242000090609      **--------
242100090609     c                   when      tipo_seg = 'RFF'
242200090609      *
242300090609     c                   call      'TRTCT05RFF'
242400090609     c                   parm                    tipo_seg
242500090609     c                   parm                    segmento
242600100716     C                   parm                    keyUNBCLI
242700100716     C                   parm                    keyTIPOMSG
242800100716     C                   parm                    keyVERSION
242900100716     C                   parm                    keyRELEASE
243000100716     C                   parm                    keyAGENCY
243100100716     C                   parm                    keyASSOCIA
243200090609     c                   parm                    Errore
243300090609     c                   parm                    EDPORRFF
243400090609     c                   parm                    Valori_inErr
243500090609     c                   parm                    Descr_Errore
243600090609      *
243700090609     C                   EXSR      DATI_RFF
243800090609      **--------
243900090609     c                   when      tipo_seg = 'TDT'
244000090609      *
244100090609     c                   call      'TRTCT05TDT'
244200090609     c                   parm                    tipo_seg
244300090609     c                   parm                    segmento
244400100716     C                   parm                    keyUNBCLI
244500100716     C                   parm                    keyTIPOMSG
244600100716     C                   parm                    keyVERSION
244700100716     C                   parm                    keyRELEASE
244800100716     C                   parm                    keyAGENCY
244900100716     C                   parm                    keyASSOCIA
245000090609     c                   parm                    Errore
245100090609     c                   parm                    EDPORTDT
245200090609     c                   parm                    Valori_inErr
245300090609     c                   parm                    Descr_Errore
245400090609      *
245500090609     C                   EXSR      DATI_TDT
245600090609      * --------
245700090609     c                   when      tipo_seg = 'TSR'
245800090609      *
245900090609     c                   call      'TRTCT05TSR'
246000090609     c                   parm                    tipo_seg
246100090609     c                   parm                    segmento
246200100716     C                   parm                    keyUNBCLI
246300100716     C                   parm                    keyTIPOMSG
246400100716     C                   parm                    keyVERSION
246500100716     C                   parm                    keyRELEASE
246600100716     C                   parm                    keyAGENCY
246700100716     C                   parm                    keyASSOCIA
246800090609     c                   parm                    Errore
246900090609     c                   parm                    EDPORTSR
247000090609     c                   parm                    Valori_inErr
247100090609     c                   parm                    Descr_Errore
247200090609      *
247300090609     C                   EXSR      DATI_TSR
247400090609      **--------
247500090609     c                   when      tipo_seg = 'TXT'
247600090609      *
247700090609     c                   call      'TRTCT05TXT'
247800090609     c                   parm                    tipo_seg
247900090609     c                   parm                    segmento
248000100716     C                   parm                    keyUNBCLI
248100100716     C                   parm                    keyTIPOMSG
248200100716     C                   parm                    keyVERSION
248300100716     C                   parm                    keyRELEASE
248400100716     C                   parm                    keyAGENCY
248500100716     C                   parm                    keyASSOCIA
248600090609     c                   parm                    Errore
248700090609     c                   parm                    EDPORTXT
248800090609     c                   parm                    Valori_inErr
248900090609     c                   parm                    Descr_Errore
249000090609      *
249100090609     C                   EXSR      DATI_TXT
249200090609      * --------
249300090209     c                   when      tipo_seg = 'UNB'
249400090209      *
249500090609     c                   call      'TRTCT05UNB'
249600090209     c                   parm                    tipo_seg
249700090209     c                   parm                    segmento
249800100716     C                   parm                    keyUNBCLI
249900100716     C                   parm                    keyTIPOMSG
250000100716     C                   parm                    keyVERSION
250100100716     C                   parm                    keyRELEASE
250200100716     C                   parm                    keyAGENCY
250300100716     C                   parm                    keyASSOCIA
250400090209     c                   parm                    Errore
250500090609     c                   parm                    EDPORUNB
250600090209     c                   parm                    Valori_inErr
250700090209     c                   parm                    Descr_Errore
250800090210      *
250900090210     C                   EXSR      DATI_UNB
251000090210      * --------
251100090209     c                   when      tipo_seg = 'UNH'
251200090209      *
251300090609     c                   call      'TRTCT05UNH'
251400090209     c                   parm                    tipo_seg
251500090209     c                   parm                    segmento
251600100716     C                   parm                    keyUNBCLI
251700100716     C                   parm                    keyTIPOMSG
251800100716     C                   parm                    keyVERSION
251900100716     C                   parm                    keyRELEASE
252000100716     C                   parm                    keyAGENCY
252100100716     C                   parm                    keyASSOCIA
252200090209     c                   parm                    Errore
252300090609     c                   parm                    EDPORUNH
252400090209     c                   parm                    Valori_inErr
252500090209     c                   parm                    Descr_Errore
252600090210      *
252700090210     C                   EXSR      DATI_UNH
252800090609      * --------
252900090609     c                   when      tipo_seg = 'UNS'
253000090609      *
253100090609     c                   call      'TRTCT05UNS'
253200090609     c                   parm                    tipo_seg
253300090609     c                   parm                    segmento
253400100716     C                   parm                    keyUNBCLI
253500100716     C                   parm                    keyTIPOMSG
253600100716     C                   parm                    keyVERSION
253700100716     C                   parm                    keyRELEASE
253800100716     C                   parm                    keyAGENCY
253900100716     C                   parm                    keyASSOCIA
254000090609     c                   parm                    Errore
254100090609     c                   parm                    EDPORUNS
254200090609     c                   parm                    Valori_inErr
254300090609     c                   parm                    Descr_Errore
254400090609      *
254500090609     C                   EXSR      DATI_UNS
254600090609      **--------
254700090609     c                   when      tipo_seg = 'UNT'
254800090609      *
254900090616     c                   call      'TRTCT05UNT'
255000090609     c                   parm                    tipo_seg
255100090609     c                   parm                    segmento
255200100716     C                   parm                    keyUNBCLI
255300100716     C                   parm                    keyTIPOMSG
255400100716     C                   parm                    keyVERSION
255500100716     C                   parm                    keyRELEASE
255600100716     C                   parm                    keyAGENCY
255700100716     C                   parm                    keyASSOCIA
255800090609     c                   parm                    Errore
255900090609     c                   parm                    EDPORUNT
256000090609     c                   parm                    Valori_inErr
256100090609     c                   parm                    Descr_Errore
256200090609      *
256300090609     C                   EXSR      DATI_UNT
256400090609      **--------
256500090609     c                   when      tipo_seg = 'UNZ'
256600090609      *
256700090616     c                   call      'TRTCT05UNZ'
256800090609     c                   parm                    tipo_seg
256900090609     c                   parm                    segmento
257000100716     C                   parm                    keyUNBCLI
257100100716     C                   parm                    keyTIPOMSG
257200100716     C                   parm                    keyVERSION
257300100716     C                   parm                    keyRELEASE
257400100716     C                   parm                    keyAGENCY
257500100716     C                   parm                    keyASSOCIA
257600090609     c                   parm                    Errore
257700090609     c                   parm                    EDPORUNZ
257800090609     c                   parm                    Valori_inErr
257900090609     c                   parm                    Descr_Errore
258000090609      *
258100090609     C                   EXSR      DATI_UNZ
258200090210      **--------
258300090609     c                   when      tipo_seg = 'VAL'
258400090213      *
258500090616     c                   call      'TRTCT05VAL'
258600090213     c                   parm                    tipo_seg
258700090213     c                   parm                    segmento
258800100716     C                   parm                    keyUNBCLI
258900100716     C                   parm                    keyTIPOMSG
259000100716     C                   parm                    keyVERSION
259100100716     C                   parm                    keyRELEASE
259200100716     C                   parm                    keyAGENCY
259300100716     C                   parm                    keyASSOCIA
259400090213     c                   parm                    Errore
259500090609     c                   parm                    EDPORVAL
259600090213     c                   parm                    Valori_inErr
259700090213     c                   parm                    Descr_Errore
259800090213      *
259900090609     C                   EXSR      DATI_VAL
260000090210      **--------
260100090209     c                   endSL
260200090209      **
260300090209     c                   endSR
260400090209     c*==================================================================*
260500090209     C*? CNVDAT - DECODIFICA LA DATA
260600090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
260700090209     C*         - WDATA  (DATA ALFANUMERICA)
260800090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
260900090209     C*         - WHMS   (ORA NUMERICA)
261000090209     C*----------------------------------------------------------------
261100090209     C     CNVDAT        BEGSR
261200090209     C*
261300090209     C                   MOVEL     ' '           WERRO             1
261400090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
261500090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
261600090209     C                   Z-ADD     *ZEROS        WHMS              6 0
261700090209     C*
261800090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
261900090209     C                   MOVEL     WDATA         WCOMO8            8
262000090209     C                   TESTN                   WCOMO8               99
262100090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
262200090209     C  N99              MOVEL     'S'           WERRO             1
262300090209     C                   END
262400090209     C*
262500090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
262600090209     C                   MOVEL     WDATA         WCOMO14          14
262700090209     C                   TESTN                   WCOMO14              99
262800090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
262900090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
263000090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
263100090209     C  N99              MOVEL     'S'           WERRO             1
263200090209     C                   END
263300090209     C*
263400090209     C                   ENDSR
263500090210     C*----------------------------------------------------------------
263600090210     C*? TSTNUM - CONTROLLO NUMERICO
263700090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
263800090210     C*         - WLUNGH (Lunghezza campo output)
263900090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
264000090210     C*----------------------------------------------------------------
264100090210     C     TSTNUM        BEGSR
264200090210     C*
264300090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
264400090210     C                   MOVEL     'N'           WOKNUM            1
264500090210     C                   MOVEL     *BLANKS       WNUMOK           15
264600090210     C                   Z-ADD     15            IN                3 0
264700090210     C                   Z-ADD     WLUNGH        IX                3 0
264800090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
264900090210     C                   MOVEA     WNUM          NDA
265000090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
265100090210     C                   MOVEA     *ALL'0'       NDN
265200090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
265300090210     C                   Z-ADD     WLUNGH        I                 3 0
265400090210     C     I             DOWGT     0                                            --- 1 -->
265500090210     C* ESTRAE IL CARATTERE DA ESAMINARE
265600090210     C     1             SUBST     WNUM:I        WTEM01            1
265700090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
265800090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
265900090210     C     WTEM01        SCAN      WCA                                    99
266000090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
266100090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
266200090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
266300090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
266400090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
266500090210     C* DA DESTRA VERSO SINISTRA
266600090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
266700090210     C                   MOVEL     WTEMP1        NDN(IN)
266800090210     C                   SUB       1             IN
266900090210     C                   END                                                    <-- 3 ---
267000090210     C                   END                                                    <-- 2 ---
267100090210     C                   SUB       1             I
267200090210     C                   END                                                    <-- 1 ---
267300090210     C*
267400090210     C                   MOVEA     NDN           WNDN             15
267500090210     C     WNDN          IFNE      *ZEROS
267600090210     C                   MOVEA     NDN           WNUMOK           15
267700090210     C                   MOVEL     'S'           WOKNUM            1
267800090210     C                   END
267900090210     C*
268000090210     C                   ENDSR
268100090210     C*----------------------------------------------------------------
268200090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
268300090210     C*----------------------------------------------------------------
268400090210     C     CNVCAP        BEGSR
268500090210     C*
268600090210     C                   MOVEL     *BLANKS       WCAP              9
268700090609     C     cap09a        IFNE      '0'
268800090609     C     '  '          SCAN      cap09a        LENGHT            2 0
268900090210     C                   SUB       1             LENGHT
269000090210     C     LENGHT        IFLE      5
269100090210     C     LENGHT        ANDGT     0
269200090210     C                   Z-ADD     LENGHT        T                 2 0
269300090210     C                   Z-ADD     5             S                 2 0
269400090609     C                   MOVEA     cap09a        CAP9
269500090210     C                   MOVEL     *ZEROS        CAP5
269600090210     C                   DO        LENGHT
269700090210     C                   MOVE      CAP9(T)       CAP5(S)
269800090210     C                   SUB       1             S
269900090210     C                   SUB       1             T
270000090210     C                   END
270100090210     C                   MOVEA     CAP5          WCAP5             5
270200090210     C     WCAP5         IFEQ      '0'
270300090210     C                   MOVEL     *BLANKS       WCAP
270400090210     C                   ELSE
270500090210     C                   MOVEA     CAP5          WCAP
270600090210     C                   END
270700090210     C*
270800090210     C                   ELSE
270900090609     C                   MOVEL     cap09a        WCAP
271000090210     C                   END
271100090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
271200090210     C     WCAP          IFNE      *BLANKS
271300090210     C     WFLCPF        ANDNE     0
271400090210     C                   MOVEL     WFLCPF        WELE              1 0
271500090210     C                   MOVE      WFLCPF        WPOS              1 0
271600090210     C                   MOVEL     *BLANK        WCPF
271700090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
271800090210     C                   ELSE
271900090210     C                   MOVEL     *BLANKS       WCPF
272000090210     C                   END
272100090210     C                   END
272200090210     C*
272300090210     C                   ENDSR
272400090211      * ?================================================================== */
272500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
272600090211      * ?================================================================== */
272700090211     C     Scrive_VAB    BEGSR
272800090211      *
272900090211     c                   clear                   err_bloccante     1
273000090211     c                   move      'S'           Almeno_Uno
273100090616     c                   eval       se_trovato_NAD = *blank
273200090211      *
273300090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
273400090211     c                   exsr      UPDVAB
273500090211      *
273600090211     C* >>>>>>>>>>>>
273700090211     C*  Richiamo pgm per scrittura immediata VAB
273800090211     C     §PUWRK        IFEQ      ' '
273900090211      *
274000090211     C*  Richiamo pgm per esecuzione stampa
274100090213     C                   MOVEL     WNRDOC        d86CMR
274200090213     C                   MOVEL     ' '           d86RIE
274300090213     C                   MOVEL     §PUDTA        d86DTA
274400090213     C                   Z-ADD     1             d86CNT
274500090213     C                   Z-ADD     §PTKSC        d86CCM
274600090213     C                   MOVEL     *BLANKS       d86STA
274700090213     C                   MOVEL     'E'           d86MOD
274800090611      *
274900090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
275000090211      * dove poi imposta la DS dei parametri passati
275100090213     C                   MOVEL     'LR'          d86CHI
275200090213     C                   MOVEL     'N'           d86CTL
275300090211      *
275400090211     c                   move      kpjbu         SvKpjbu
275500090211     c                   clear                   kpjbu
275600090216     C                   movel     *on           Commit_on         1
275700090213     C                   MOVEL     TRTC86ds      KPJBU
275800090213      * ?- Chiamata la TRTC86R2 -*
275900090213     C                   CALL      'TRTC86R2'
276000090211     C                   PARM                    KPJBA
276100090216     C                   PARM                    Commit_on
276200090211     c                   move      Svkpjbu       Kpjbu
276300090211      *
276400090211     C*  Controllo pgm per scrittura immediata VAB
276500090211     C     WW            IFNE      0
276600090211     C                   DO        WW            WX                3 0
276700090213     C                   Z-ADD     KCL(WX)       d86CCM
276800090211      *
276900090211     c                   move      kpjbu         SvKpjbu
277000090211     c                   clear                   kpjbu
277100090216     C                   movel     *on           Commit_on         1
277200090213     C                   MOVEL     TRTC86ds      KPJBU
277300090213      * ?- Chiamata la TRTC86R2 -*
277400090213     C                   CALL      'TRTC86R2'
277500090211     C                   PARM                    KPJBA
277600090216     C                   PARM                    Commit_on
277700090211     c                   move      Svkpjbu       Kpjbu
277800090211     C                   END
277900090211     C                   END
278000090211      *
278100090211     C                   END
278200090211      * >>>>>>>>>>>>
278300090211     C                   if        se_errore  = 'S'
278400090211     c                   move      'S'           err_bloccante
278500090211     c                   goto      Error_VAB
278600090211     c                   end
278700090211     C*
278800090211     C     Error_VAB     Endsr
278900090211     C*----------------------------------------------------------------
279000090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
279100090211     C*----------------------------------------------------------------
279200090211     C     UPDVAB        BEGSR
279300090211      *
279400090211     C*  Se non è stato impostato il codice bolla lo imposto io
279500090211     C     VABCBO        IFEQ      *BLANKS
279600090630     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
279700090211     C                   END
279800090630      *
279900090630      *  Se c'è il contrassegno
280000090630     C     VABCAS        IFGT      0
280100090630     C     VABCBO        ifeq      '1'
280200090630     C                   MOVEL     '4'           VABCBO                         + C/Ass
280300090630     C                   END
280400090630     C     VABCBO        ifeq      '2'
280500090630     C                   MOVEL     '6'           VABCBO                         + C/Ass
280600090630     C                   END
280700090630     C                   END
280800110922      *
280900110922      * Imposta le NOte
281000110922     C     VABNOT        IFEQ      *BLANKS
281100110922     C                   MOVEL     wnot1         VABNOT
281200110922     C                   MOVEL     wnot2         VABNT2
281300110922     C                   ELSE
281400110922     C                   MOVEL     wnot1         VABNT2
281500110922     C                   END
281600090211      *
281700090211     C*  Attribuisco anno spedizione
281800090611     C     Tes_VABdcm    IFGT      0
281900090611     C                   MOVEL     Tes_VABdcm    VABAAS                         anno sped.
282000090211     C                   END
282100090211      *
282200090211      *  Controllo dettaglio segnacolli
282300090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
282400090211     C                   EXSR      CTRSGE
282500090211     C                   ELSE
282600090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
282700090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
282800090211     C                   EXSR      CTRSGN
282900090211     C                   END
283000090211     C                   END
283100090211      *
283200090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
283300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
283400090211     c                   clear                   mantiene_nsp      1            *blk/'S'
283500090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
283600090211      *
283700090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
283800090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
283900090211      *
284000090211      *  Imposto codice cliente
284100090211     c                   Select
284200090211      *
284300090611      *  Specificato codice da qualificatore FF
284400090211     C                   When        WCLKSC <> *zeros
284500090211     C                   Z-ADD     WCLKSC        VABCCM
284600090211     C                   MOVEL     WCLTAR        VABCTR
284700090211      * forza LNP/CTM/NRS
284800090211     c                   if        WclLNP <> 0
284900090211     C                   eval      VABLNP = WclLNP
285000090211     c                   end
285100090211     c                   if        WclNRS <> 0
285200090211     C                   eval      VABNRS = WclNRS
285300090211     c                   end
285400090211     c                   if        WclCTM <> *blank
285500090211     C                   eval      VABCTM = WclCTM
285600090211     c                   end
285700090211      *
285800090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
285900090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
286000090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
286100090211     c                   end
286200090211      *
286300090611      *  Specificato codice da qualificatore FF
286400090211     C                   When        $CLKSC <> *zeros
286500090211     C                   Z-ADD     $CLKSC        VABCCM
286600090211     C                   MOVEL     $CLTAR        VABCTR
286700090211      * forza LNP/CTM/NRS
286800090211     c                   if        $clLNP <> 0
286900090211     C                   eval      VABLNP = $clLNP
287000090211     c                   end
287100090211     c                   if        $clNRS <> 0
287200090211     C                   eval      VABNRS = $clNRS
287300090211     c                   end
287400090211     c                   if        $clCTM <> *blank
287500090211     C                   eval      VABCTM = $clCTM
287600090211     c                   end
287700090211      *
287800090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
287900090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
288000090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
288100090211     c                   end
288200090211      *
288300090211      *  Imposto dati da tabella - PT-
288400090211     C                   Other
288500090211      *
288600090211     C                   MOVEL     §PTKSC        VABCCM
288700090211     C                   MOVEL     §PTCTR        VABCTR
288800090211      *
288900090211     C                   Endsl
289000090211      *
289100090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
289200090211      *    o dalla relativa CL
289300090211     C                   MOVEL     VABlnp        VABfgs
289400090211      *
289500090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
289600090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
289700090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
289800090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
289900090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
290000090211      *
290100100511      * Avendo attivato il VAX si deve recepire lo stesso numero passato nel rif.Alfabetico
290200100511      *
290300100511     c                   if        mantiene_nsp = 'S' and VABRMA <> *blank      *blk/'S' - 1234567
290400100511     c                   movel(p)  vabrma        campo9a           9
290500100511     c                   move      campo9a       VABNSP                         *NUM.SPED.da Cliente
290600100511     c                   else
290700090211      *
290800090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
290900090211     C*  lo attribuisco io
291000090211      **              **------------------**
291100090211     c                   exsr      repnum
291200090211      **              **------------------**
291300090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
291400090211      **              **------------------**
291500090211     c                   end
291600090211      *
291700090211     C*  Altrimenti attribuisco data del giorno
291800090211     C     VABMGS        IFEQ      0
291900090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
292000090211     C                   MOVE      WOGGI         VABMGS                         = oggi
292100090211     C                   END
292200090611      *
292300090211     C*  Eseguo posizionamento su EDiVAB
292400090211     C                   Z-ADD     VABAAS        KAAS
292500090211     C                   Z-ADD     VABlnp        KLNP
292600090211     C                   Z-ADD     VABNRS        KNRS
292700090211     C                   Z-ADD     VABNSP        KNSP
292800090211     C                   MOVEL     SAVBOL        SALVA
292900090211     C     KVAB1         CHAIN     EDiVAB1L                           30
293000090211     C                   MOVEL     SALVA         SAVBOL
293100090611      *
293200090211     C*  Imposto dati CMR
293300090211     C                   Z-ADD     1             VABCNT
293400090211     **
293500090211      *** Attenzione se si deve trattare il VAX con il CMR
293600090211      *** il campo VABCNT deve essere sempre impostato a (1)
293700090211     C     §puWRK        ifEQ      'S'
293800090211     C     §puNSP        andEQ     'S'
293900090211     C                   Z-ADD     1             VABCNT
294000090211     c                   end
294100090211     **
294200090211     C                   Z-ADD     WDTPRE        VABDTS
294300090212     C     WHMPRE        mult      100           VABHMS
294400090611      *
294500090611     C                   Z-ADD     Tes_VABdcm    VABDCM
294600090211     C                   MOVEL     WNRCMR        VABCMR
294700090611      *
294800090211     C*  Se non è indicato il nome del mittente = Partner mittente
294900090211     C     VABRMO        IFEQ      *BLANKS
295000090211     C                   MOVEL     ACORAG        VABRMO
295100090211     C                   MOVEL     INDCAP        VABCMO
295200090211     C     INDCAE        IFNE      *BLANKS
295300090211     C                   MOVEL     INDCAE        VABCMO
295400090211     C                   END
295500090211     C     INDSTA        IFNE      *BLANKS
295600090211     C                   MOVEL     INDSTA        VABNMO
295700090211     C                   END
295800090211     C                   END
295900090611      *
296000090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
296100090211     C     §1BFG1        IFEQ      'N'
296200090211     C     §1BFG7        ANDEQ     'N'
296300090211     C     §1BFG2        ANDEQ     'N'
296400090211     C***                  Z-ADD0         VABNRS
296500090211     C                   END
296600090611      *
296700090211     C*  Imposto segancollo da - a
296800090211     C     §1BFG1        IFEQ      'S'
296900090211     C     §1BFG7        ANDEQ     'N'
297000090211     C     §1BF12        ANDNE     'E'
297100090211      *
297200090211     C     XY            IFEQ      *ZEROS
297300090211     C                   CLEAR                   VABNCD
297400090211     C                   CLEAR                   VABNCA
297500090211     C                   ELSE
297600090211     C                   Z-ADD     SGN(1)        VABNCD
297700090211     C                   Z-ADD     SGN(XY)       VABNCA
297800090211     C                   ENDIF
297900090211     C*
298000090211     C                   END
298100090211     C*
298200090211     C*  Se cap mittente originale a blank abblenco nazione
298300090211     C     VABCMO        IFEQ      *BLANKS
298400090211     C                   MOVEL     *BLANKS       VABNMO
298500090211     C                   END
298600100311     C*
298700100311     C*  aggiungo le NOTE se ci sono
298800100311     C     Wnot1         IFne      *BLANKS
298900100311     c                   movel(p)  Wnot1         vabNOT
299000100311     c                   end
299100100311     C     Wnot2         IFne      *BLANKS
299200100311     c                   movel(p)  Wnot2         vabNT2
299300100311     c                   end
299400100311     C*
299500090603      *
299600090211      *? ------------------------------------------------------------------------
299700090213     C   30              WRITE     EDiVAB00                             99
299800090213     C  N30              UPDATE    EDiVAB00                             99
299900090211      *? ------------------------------------------------------------------------
300000090611     C*
300100090213     c   99              eval      errori_in_Wrt = 'S'
300200090211     C*
300300090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
300400090211     C                   EXSR      WRTVAT_Rif
300500090211     C*
300600090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
300700090211     C     §1BF12        IFEQ      'E'
300800090211     C                   EXSR      WRTVAT
300900090211     C                   ELSE
301000090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
301100090211     C     §1BFG7        IFEQ      'S'
301200090211     C     §1BFG1        OREQ      'S'
301300090211     C                   EXSR      WRTVAD
301400090211     C                   END
301500090211     C                   END
301600090211      *
301700090211     C* Reimposto codice trattamento merce cliente
301800090211     C                   MOVEL     WSVTRM        DS1B
301900090211     C**
302000090211     C* Do errore se previsto CMR ma non c'è
302100090211     C     §PTCMR        IFNE      ' '
302200090211     C                   EXSR      MEMCMR
302300090211     C                   END
302400090611      *
302500090211     C* Do errore se previsto AFB ma non c'è
302600090211     C     §PTNFV        IFNE      ' '
302700090211     C                   EXSR      MEMAFB
302800090211     C                   END
302900090611      *
303000090211     C*  Se il cliente vuole il ritorno delle date di consegna
303100090211     C*  è c'è una squdratura fra i segnacolli e il numero
303200090211     C*  dei colli che ci ha passato scrivo EDSUM
303300090211     C     §PUDTA        IFEQ      'S'
303400090211     C     WSQUAD        ANDEQ     'S'
303500090211     C                   EXSR      WRTSUM
303600090211     C                   END
303700090211     C*
303800090211     C                   ENDSR
303900090211     C*----------------------------------------------------------------
304000090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
304100090211     C*----------------------------------------------------------------
304200090211     C     CTRSGN        BEGSR
304300090211     C*
304400090211     C                   MOVEL     'N'           WNSGER            1
304500090211     C*
304600090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
304700090211     C     VABNCL        IFNE      XY
304800090211     C                   MOVEL     'S'           WNSGER
304900090211     C                   MOVEL     'S'           WSQUAD
305000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
305100090211     c                              bollettati'
305200090211     C                   GOTO      FINSGN
305300090211     C                   END
305400090211     C*  Riordino i segnacolli
305500090211     C     §1BFG7        IFEQ      'N'
305600090211     C                   SORTA     SGN
305700090211     C*  Controllo se sono sequenziali
305800090211     C                   MOVEL     'N'           WNOSEQ            1
305900090211     C     SGN(1)        ADD       1             WEXSGN            7 0
306000090211     C     2             DO        XY            X
306100090211     C     SGN(X)        IFNE      WEXSGN
306200090211     C                   MOVEL     'S'           WNOSEQ
306300090211     C                   END
306400090211     C     SGN(X)        ADD       1             WEXSGN
306500090211     C                   END
306600090211     C                   END
306700090211     C*
306800090211     C     FINSGN        ENDSR
306900090211     C*----------------------------------------------------------------
307000090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
307100090211     C*----------------------------------------------------------------
307200090211     C     CTRSGE        BEGSR
307300090211     C*
307400090211     C                   MOVEL     'N'           WNSGER            1
307500090211      *
307600090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
307700090211      *
307800090211     C     VABNCL        IFNE      §§
307900090211     C                   MOVEL     'S'           WNSGER
308000090211     C                   MOVEL     'S'           WSQUAD
308100090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
308200090211     c                              bollettati'
308300090211     C                   END
308400090211      *
308500090211     C                   ENDSR
308600090211     C*----------------------------------------------------------------
308700090211     C*? ESEGUO SCRITTURA EDiVAD
308800090211     C*----------------------------------------------------------------
308900090211     C     WRTVAD        BEGSR
309000090211     C*
309100090211     C* Se non seq. scrivo xy record
309200090211     C     §1BFG1        IFEQ      'S'
309300090211     C     XY            ANDNE     *ZEROS
309400090211     C*
309500090211     C     §1BFG7        IFEQ      'S'
309600090211     C                   DO        XY            X
309700090211     C                   Z-ADD     VABCCM        KCCM
309800090211     C                   Z-ADD     SGN(X)        KNCD
309900090211     C                   Z-ADD     VABCNT        VADCNT
310000090211     C                   MOVEL     VABCMR        VADCMR
310100090211     C                   Z-ADD     VABAAS        VADAAS
310200090211     C                   Z-ADD     VABLNP        VADLNP
310300090211     C                   Z-ADD     VABNRS        VADNRS
310400090211     C                   Z-ADD     VABNSP        VADNSP
310500090211     C                   Z-ADD     1             VADNCL
310600090211     C                   MOVEL     SGN(X)        VADCDP
310700090211     C                   Z-ADD     SGN(X)        VADNCD
310800090211     C                   Z-ADD     *ZEROS        VADNCA
310900090211     C                   Z-ADD     VABCCM        VADCCM
311000090211     C                   movel     VABfgs        VADfgs
311100090213     C                   WRITE     EDiVAD00                             99
311200090213     c   99              eval      errori_in_Wrt = 'S'
311300090211     C                   END
311400090211     C* Se sequenziali scrivo un solo record
311500090211     C                   ELSE
311600090211     C                   Z-ADD     VABAAS        VADAAS
311700090211     C                   Z-ADD     VABLNP        VADLNP
311800090211     C                   Z-ADD     VABNRS        VADNRS
311900090211     C                   Z-ADD     VABNSP        VADNSP
312000090211     C                   Z-ADD     VABNCL        VADNCL
312100090211     C                   Z-ADD     SGN(1)        VADNCD
312200090211     C                   Z-ADD     SGN(XY)       VADNCA
312300090211     C                   Z-ADD     VABCCM        VADCCM
312400090211     C                   movel     VABfgs        VADfgs
312500090213     C                   WRITE     EDiVAD00                             99
312600090213     c   99              eval      errori_in_Wrt = 'S'
312700090211     C                   END
312800090211     C*
312900090211     C                   ELSE
313000090211     C                   DO        XY            X
313100090211     C                   Z-ADD     VABCCM        KCCM
313200090211     C                   Z-ADD     SGN(X)        KNCD
313300090211     C                   Z-ADD     VABAAS        VADAAS
313400090211     C                   Z-ADD     VABLNP        VADLNP
313500090211     C                   Z-ADD     VABNRS        VADNRS
313600090211     C                   Z-ADD     VABNSP        VADNSP
313700090211     C                   MOVEL     SGN(X)        VADCDP
313800090211     C                   Z-ADD     1             VADNCL
313900090211     C                   Z-ADD     *ZEROS        VADNCD
314000090211     C                   Z-ADD     *ZEROS        VADNCA
314100090211     C                   Z-ADD     VABCCM        VADCCM
314200090211     C                   movel     VABfgs        VADfgs
314300090213     C                   WRITE     EDiVAD00                             99
314400090213     c   99              eval      errori_in_Wrt = 'S'
314500090211     C                   END
314600090211     C*
314700090211     C                   END
314800090211     C*
314900090211     C                   ENDSR
315000090211     C*----------------------------------------------------------------
315100090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
315200090211     C*----------------------------------------------------------------
315300090211     C     WRTVAT_Rif    BEGSR
315400090211      *
315500090211      *  riferimento di consegna destinatario
315600090211     c                   if        WatRDE <> *blank
315700090211     C                   Z-ADD     VABCNT        VATCNT
315800090211     C                   MOVEL     VABCMR        VATCMR
315900090211     C                   Z-ADD     VABCCM        VATCCM
316000090211     C                   movel     VABfgs        VATfgs
316100090211     C                   Z-ADD     VABLNP        VATLNP
316200090211     C                   Z-ADD     VABAAS        VATAAS
316300090211     C                   Z-ADD     VABNRS        VATNRS
316400090211     C                   Z-ADD     VABNSP        VATNSP
316500090211     C                   MOVEL     'A'           VATTRC
316600090211     C                   MOVEL     WatRDE        VATNOT
316700090213     C                   WRITE     EDiVAT00                             99
316800090213     c   99              eval      errori_in_Wrt = 'S'
316900090211     c                   end
317000090211      *
317100090211      *  riferimento di consegna telefonico
317200090211     c                   if        WatTEL <> *blank
317300090211     C                   Z-ADD     VABCNT        VATCNT
317400090211     C                   MOVEL     VABCMR        VATCMR
317500090211     C                   Z-ADD     VABCCM        VATCCM
317600090211     C                   movel     VABfgs        VATfgs
317700090211     C                   Z-ADD     VABLNP        VATLNP
317800090211     C                   Z-ADD     VABAAS        VATAAS
317900090211     C                   Z-ADD     VABNRS        VATNRS
318000090211     C                   Z-ADD     VABNSP        VATNSP
318100090211     C                   MOVEL     'B'           VATTRC
318200090211     C                   MOVEL     WatTEL        VATNOT
318300090213     C                   WRITE     EDiVAT00                             99
318400090213     c   99              eval      errori_in_Wrt = 'S'
318500090211     c                   end
318600090211      *
318700090211      * riferimento del Partner che una volta veniva riportato sul BL4
318800090211     c                   if        Wnsp   <> *blank
318900090211     C                   Z-ADD     VABCNT        VATCNT
319000090211     C                   MOVEL     VABCMR        VATCMR
319100090211     C                   Z-ADD     VABCCM        VATCCM
319200090211     C                   movel     VABfgs        VATfgs
319300090211     C                   Z-ADD     VABLNP        VATLNP
319400090211     C                   Z-ADD     VABAAS        VATAAS
319500090211     C                   Z-ADD     VABNRS        VATNRS
319600090211     C                   Z-ADD     VABNSP        VATNSP
319700090211     C                   MOVEL     'C'           VATTRC
319800090211     C                   MOVEL     Wnsp          VATNOT
319900090213     C                   WRITE     EDiVAT00                             99
320000090213     c   99              eval      errori_in_Wrt = 'S'
320100090211     c                   end
320200090211      *
320300090211     C                   ENDSR
320400090211     C*----------------------------------------------------------------
320500090211     C*? ESEGUO SCRITTURA EDiVAT
320600090211     C*----------------------------------------------------------------
320700090211     C     WRTVAT        BEGSR
320800090211      *
320900090211     C                   DO        §§            X
321000090211     C                   Z-ADD     VABCNT        VATCNT
321100090211     C                   MOVEL     VABCMR        VATCMR
321200090211     C                   Z-ADD     VABCCM        VATCCM
321300090211     C                   movel     VABfgs        VATfgs
321400090211     C                   Z-ADD     VABLNP        VATLNP
321500090211     C                   Z-ADD     VABAAS        VATAAS
321600090211     C                   Z-ADD     VABNRS        VATNRS
321700090211     C                   Z-ADD     VABNSP        VATNSP
321800090211     C                   MOVEL     'E'           VATTRC
321900090211      *
322000090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
322100090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
322200090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
322300090211     c                   if        WVATlun > 0
322400090211      *
322500090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
322600090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
322700090211      *
322800090211     c                   else
322900090211      *
323000090211      * Se non è impostato nulla prende il segnacollo passato così com'è
323100090211     C                   MOVEL     SGE(X)        VATNOT
323200090211     c                   end
323300090211      *
323400090213     C                   WRITE     EDiVAT00                             99
323500090213     c   99              eval      errori_in_Wrt = 'S'
323600090211     C                   END
323700090211      *
323800090211     C                   ENDSR
323900090211     C*----------------------------------------------------------------
324000090211     C*? ESEGUO SCRITTURA EDSUM
324100090211     C*----------------------------------------------------------------
324200090211     C     WRTSUM        BEGSR
324300090211     C*
324400090211     C* Se non seq. scrivo xy record
324500090211     C                   DO        XY            X
324600090211     C                   Z-ADD     VABAAS        SUMAAS
324700090211     C                   Z-ADD     VABLNP        SUMFLS
324800090211     C                   MOVEL     WKSC          SUMLNP
324900090211     C                   Z-ADD     VABLNA        SUMLNA
325000090211     C                   Z-ADD     VABZNC        SUMZNC
325100090211     C                   Z-ADD     VABNRS        SUMNRS
325200090211     C                   Z-ADD     VABNSP        SUMNSP
325300090211     C                   Z-ADD     SGN(X)        SUMNSC
325400090211     C                   Z-ADD     WKSC          SUMCCM
325500090211     C                   MOVEL     *BLANKS       SUMSTS
325600090211     C                   MOVEL     *BLANKS       SUMFLG
325700090211     C                   MOVEL     WPRGSP        SUMCNI
325800090211     C                   MOVEL     VABCMR        SUMCMR
325900090211     C                   Z-ADD     0             SUMNFE
326000090211     C                   Z-ADD     VABCCM        VADCCM
326100090213     C                   WRITE     EDSUM000                             99
326200090213     c   99              eval      errori_in_Wrt = 'S'
326300090211     C                   END
326400090211     C*
326500090211     C                   ENDSR
326600090211     c*==================================================================*
326700090211      * Manda un Msg in Posta AS Sede a EDP*
326800090211     c*==================================================================*
326900090211     c     SEND_MSG_EDP  begsr
327000090211      *
327100090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
327200090211      *   Lo manda una sola volta per lavoro
327300090211     c                   IF        Snd_Msg_alert  <> 'N' and
327400090211     c                             User_msg <> *blank
327500090211     c                   z-add     1             Jx
327600090211     C                   exsr      CalQEZSNDMG
327700090211     c                   end
327800090211      *
327900090211     c                   endsr
328000090213     c**********************************************************************
328100090213     c* reperimento numero Spedizione
328200090213     c**********************************************************************
328300090213     C     repnum        BEGSR
328400090213      *
328500090213      *    deve controllare sulla tabella "3C" se il numero spedizione
328600090213      *     deve essere mantenuto oppure no
328700090213     C*
328800090213     C                   clear                   Ds3C
328900090213     C                   Z-ADD     CODUT         TBLKUT
329000090213     C                   MOVEL     '3C'          TBLCOD
329100090213     C                   movel(p)  VABccm        TBLKEY
329200090213     C     KTABel        CHAIN     TABEL00F                           30
329300090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
329400090213     C                   MOVEL     TBLUNI        Ds3C
329500090213     C                   END
329600090213      *
329700090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
329800090213     c                   if        §3CFsp <> 'S'
329900090213      *
330000090213     C* NSP => Stacco un numeratore da AZNUM
330100090213     c                   movel     kpjbu         svkpjbu
330200090213     c                   clear                   kpjbu
330300090213     C                   clear                   TRUL33DS
330400090213     C                   eval      I33OPE = *zeros
330500090213     C                   eval      I33CNU = 302
330600090213     C                   eval      I33NUM = 1
330700090213     C                   movel     TRUL33DS      KPJBU
330800090213     C                   call      'TRUL33R'
330900090213     C                   parm                    KPJBA
331000090213     C                   movel     KPJBU         TRUL33DS
331100090213     c                   movel     svkpjbu       kpjbu
331200090213      ****
331300090213     C                   if        O33ERR = *zeros
331400090213     c                   z-add     o33nrf        NUM_SPED
331500090213     C                   else
331600090213     C                   endif
331700090213      ****
331800090213     c                   ELSE
331900090213      *   se si vuole mantenere il numero spedizione
332000090213      ****
332100090213     C                   MOVEL     WOGGI         WANNO             4 0
332200090213     C                   MOVE      WANNO         KAAA
332300090213     C                   Z-ADD     3             KCNU
332400090213     C                   Z-ADD     VABlnp        KFIL
332500090213     C     KNUF          CHAIN     FLNUF                              9999
332600090213     C     *IN99         IFEQ      '0'
332700090213     C                   ADD       1             NUFNUM
332800090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
332900090213     C                   UPDATE    FLNUF
333000090213     C                   ELSE
333100090213     C                   END
333200090213     C*
333300090213     c                   EndIF
333400090213      **
333500090213     C                   ENDSR
333600090213     C*----------------------------------------------------------------
333700090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
333800090213     C*----------------------------------------------------------------
333900090213     C     MEMCMR        BEGSR
334000090213     C*
334100090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
334200090213     C     §PTCM1        IFEQ      'S'
334300090213     C                   CLEAR                   EDRDE000
334400090213     C                   MOVEL     'TD1154'      KNMC
334500090213     C                   Z-ADD     1             KNRP
334600090213     C                   MOVEL     WKSC          KLNP1
334700090213     C     KRDE          CHAIN     EDRDE01L                           31
334800090213     C     *IN31         IFEQ      '1'
334900090213     C                   Z-ADD     VABAAS        RDEAAS
335000090213     C                   Z-ADD     VABLNP        RDELNP
335100090213     C                   Z-ADD     VABNRS        RDENRS
335200090213     C                   Z-ADD     VABNSP        RDENSP
335300090213     C                   MOVEL     'TD1154'      RDENMC
335400090213     C                   Z-ADD     1             RDENRP
335500090213     C                   MOVEL     WNRCMR        RDEVAL
335600090213     C                   WRITE     EDRDE000                             99
335700090213     c   99              eval      errori_in_Wrt = 'S'
335800090213     C                   END
335900090213     C*  Memorizzo qualificatore CMR
336000090213     C                   CLEAR                   EDRDE000
336100090213     C                   MOVEL     'TD1153'      KNMC
336200090213     C                   Z-ADD     1             KNRP
336300090213     C                   MOVEL     WKSC          KLNP1
336400090213     C     KRDE          CHAIN     EDRDE01L                           31
336500090213     C     *IN31         IFEQ      '1'
336600090213     C                   Z-ADD     VABAAS        RDEAAS
336700090213     C                   Z-ADD     VABLNP        RDELNP
336800090213     C                   Z-ADD     VABNRS        RDENRS
336900090213     C                   Z-ADD     VABNSP        RDENSP
337000090213     C                   Z-ADD     1             RDENRP
337100090213     C                   MOVEL     'TD1153'      RDENMC
337200090213     C                   MOVEL     'CMR'         RDEVAL
337300090213     C                   WRITE     EDRDE000                             99
337400090213     c   99              eval      errori_in_Wrt = 'S'
337500090213     C                   END
337600090213     C*  Memorizzo la data
337700090213     C                   CLEAR                   EDRDE000
337800090213     C                   MOVEL     'TD2380'      KNMC
337900090213     C                   Z-ADD     1             KNRP
338000090213     C                   MOVEL     WKSC          KLNP1
338100090213     C     KRDE          CHAIN     EDRDE01L                           31
338200090213     C     *IN31         IFEQ      '1'
338300090213     C                   Z-ADD     VABAAS        RDEAAS
338400090213     C                   Z-ADD     VABLNP        RDELNP
338500090213     C                   Z-ADD     VABNRS        RDENRS
338600090213     C                   Z-ADD     VABNSP        RDENSP
338700090213     C                   Z-ADD     1             RDENRP
338800090213     C                   MOVEL     'TD2380'      RDENMC
338900090611     C                   MOVEL     Tes_VABdcm    RDEVAL
339000090213     C                   WRITE     EDRDE000                             99
339100090213     c   99              eval      errori_in_Wrt = 'S'
339200090213     C                   END
339300090213     C*
339400090213     C                   END                                                    §PTCM1 = 'S'
339500090213     C*
339600090213     C                   ENDSR
339700090213     C*----------------------------------------------------------------
339800090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
339900090213     C*----------------------------------------------------------------
340000090213     C     MEMAFB        BEGSR
340100090213     C*
340200090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
340300090213     C     §PTNF1        IFEQ      'S'
340400090213     C                   CLEAR                   EDRDE000
340500090213     C                   MOVEL     WKSC          KLNP1
340600090213     C                   MOVEL     'TD1154'      KNMC
340700090213     C                   Z-ADD     1             KNRP
340800090213     C     KRDE          CHAIN     EDRDE01L                           31
340900090213     C     *IN31         IFEQ      '1'
341000090213     C                   Z-ADD     VABAAS        RDEAAS
341100090213     C                   Z-ADD     VABLNP        RDELNP
341200090213     C                   Z-ADD     VABNRS        RDENRS
341300090213     C                   Z-ADD     VABNSP        RDENSP
341400090213     C                   MOVEL     'TD1154'      RDENMC
341500090213     C     §PTCM1        IFNE      'S'
341600090213     C     WNRCMR        OREQ      *BLANKS
341700090213     C                   Z-ADD     1             RDENRP
341800090213     C                   ELSE
341900090213     C                   Z-ADD     2             RDENRP
342000090213     C                   END
342100090213     C                   MOVEL     WNUMFV        RDEVAL
342200090213     C                   WRITE     EDRDE000                             99
342300090213     c   99              eval      errori_in_Wrt = 'S'
342400090213     C                   END
342500090213     C*  Memorizzo qualificatore CMR
342600090213     C                   CLEAR                   EDRDE000
342700090213     C                   MOVEL     WKSC          KLNP1
342800090213     C                   MOVEL     'TD1153'      KNMC
342900090213     C                   Z-ADD     1             KNRP
343000090213     C     KRDE          CHAIN     EDRDE01L                           31
343100090213     C     *IN31         IFEQ      '1'
343200090213     C                   Z-ADD     VABAAS        RDEAAS
343300090213     C                   Z-ADD     VABLNP        RDELNP
343400090213     C                   Z-ADD     VABNRS        RDENRS
343500090213     C                   Z-ADD     VABNSP        RDENSP
343600090213     C     §PTCM1        IFNE      'S'
343700090213     C     WNRCMR        OREQ      *BLANKS
343800090213     C                   Z-ADD     1             RDENRP
343900090213     C                   ELSE
344000090213     C                   Z-ADD     2             RDENRP
344100090213     C                   END
344200090213     C                   MOVEL     'TD1153'      RDENMC
344300090213     C                   MOVEL     'AFB'         RDEVAL
344400090213     C                   WRITE     EDRDE000                             99
344500090213     c   99              eval      errori_in_Wrt = 'S'
344600090213     C                   END
344700090213     C*  Memorizzo la data
344800090213     C                   CLEAR                   EDRDE000
344900090213     C                   MOVEL     WKSC          KLNP1
345000090213     C                   MOVEL     'TD2380'      KNMC
345100090213     C                   Z-ADD     1             KNRP
345200090213     C     KRDE          CHAIN     EDRDE01L                           31
345300090213     C     *IN31         IFEQ      '1'
345400090213     C                   Z-ADD     VABAAS        RDEAAS
345500090213     C                   Z-ADD     VABLNP        RDELNP
345600090213     C                   Z-ADD     VABNRS        RDENRS
345700090213     C                   Z-ADD     VABNSP        RDENSP
345800090213     C     §PTCM1        IFNE      'S'
345900090213     C     WNRCMR        OREQ      *BLANKS
346000090213     C                   Z-ADD     1             RDENRP
346100090213     C                   ELSE
346200090213     C                   Z-ADD     2             RDENRP
346300090213     C                   END
346400090213     C                   MOVEL     'TD2380'      RDENMC
346500090611     C                   MOVEL     Tes_VABdcm    RDEVAL
346600090213     C                   WRITE     EDRDE000                             99
346700090213     c   99              eval      errori_in_Wrt = 'S'
346800090213     C                   END
346900090213     C*
347000090213     C                   END                                                    §PTCM1 = 'S'
347100090213     C*
347200090213     C                   ENDSR
347300090213     c*==================================================================*
347400090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
347500090213     c*==================================================================*
347600090213     c     Imp_ADDR_mail begsr
347700090213      *
347800090213     c                   clear                   Indirizzi
347900090213      *
348000090213      *  Lunghezza indirizzi presenti
348100090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
348200090213     c                   z-add     1             al_char           3 0
348300090213     c                   z-add     1             dal_char          3 0
348400090213     c                   z-add     1             tot_char          3 0
348500090213      *
348600090213     c     successivo    tag
348700090213      *
348800090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
348900090213      *   e aggiunge il dominio Bartolini + il (;) separatore
349000090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
349100090213     c                   eval      tot_char = al_char - dal_char
349200090213     c                   z-add     dal_char      Dac
349300090213     c                   z-add     tot_char      Luc
349400090213      *
349500090213     c                   clear                   Indir_Bartol     40
349600090213     c                   clear                   Indir_Parz       20
349700090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
349800090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
349900090213     c                             %Trim(bart_it)
350000090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
350100090213      *
350200090213     c                   if        al_char = Lun_Ind
350300090213     c                   goto      fine_indmail
350400090213     c                   else
350500090213     c                   eval      dal_char = ( al_char + 1 )
350600090213     c                   goto      successivo
350700090213     c                   end
350800090213      *
350900090213     C     fine_indmail  TAG
351000090213      *
351100090213     C                   ENDSR
351200090213     c*==================================================================*
351300090213      * Manda un Msg x E-mail
351400090213     c*==================================================================*
351500090213     c     Invio_Mail    begsr
351600090213      *
351700090213     c                   goto      non_inviare
351800090213      *
351900090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
352000090213     C* Inizializzo variabili
352100090213     C                   movel     *blanks       wrkEml          253
352200090213     C                   movel     *blanks       wrkMsg         5000
352300090213     C                   movel     *blanks       wrkOgg           44
352400090213      *
352500090213     C* Valorizzo i campi della e-m@ail - indirizzo
352600090213     C                   eval      wrkEml = Indirizzi
352700090213     C                   eval      wrkOgg = Oggetto
352800090213     C                   eval      wrkMsg = Messaggio
352900090213
353000090213     C                   call(e)   'TIS701C'
353100090213     C                   parm                    wrkEml
353200090213     C                   parm                    wrkOgg
353300090213     C                   parm                    wrkMsg
353400090213
353500090213     c     non_inviare   tag
353600090213     C*
353700090213     C                   ENDSR
353800090603     c*==================================================================*
353900090603      * Manda un Msg x E-mail a EDPAB
354000090603     c*==================================================================*
354100090603     c     Invio_Mail_AB begsr
354200090603      *
354300090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
354400090603     C* Inizializzo variabili
354500090603     C                   movel     *blanks       wrkEml          253
354600090603     C                   movel     *blanks       wrkMsg         5000
354700090603     C                   movel     *blanks       wrkOgg           44
354800090603      *
354900090603     C* Valorizzo i campi della e-m@ail - indirizzo
355000110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
355100110203     C                   eval      wrkEml = Indirizzi
355200090603     C                   eval      wrkOgg = Oggetto
355300090603     C                   eval      wrkMsg = Messaggio
355400090603
355500110203      ********
355600110203     C********           call(e)   'TIS701C'
355700110203     C********           parm                    wrkEml
355800110203     C********           parm                    wrkOgg
355900110203     C********           parm                    wrkMsg
356000110203     C*
356100110203      ** *****
356200110203     C                   call(e)   'TRTCT00R2'
356300110203     C                   parm                    wrkEml
356400110203     C                   parm                    wrkOgg
356500110203     C                   parm                    wrkMsg
356600110203     C*
356700090603     C*
356800090616     c                   if        %error
356900090616     c                   end
357000090616     C*
357100090603     C                   ENDSR
357200090211     ***********************************************************************
357300090211     **
357400090211     ** Send Message (QEZSNDMG) API
357500090211     **
357600090211     ***********************************************************************
357700090211     C     CalQEZSNDMG   BEGSR
357800090211      *
357900090211     ** Invio messaggio all'utente.
358000090211     C                   EVAL      SndMg03 = msgDSP
358100090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
358200090211     C                   EVAL      SndMg05(Jx) = User_msg
358300090211     C                   EVAL      SndMg06 = Jx
358400090211     C                   CLEAR                   QUSEC
358500090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
358600090211
358700090211     C                   CALL      'QEZSNDMG'
358800090211     C                   PARM                    SndMg01
358900090211     C                   PARM                    SndMg02
359000090211     C                   PARM                    SndMg03
359100090211     C                   PARM                    SndMg04
359200090211     C                   PARM                    SndMg05
359300090211     C                   PARM                    SndMg06
359400090211     C                   PARM                    SndMg07
359500090211     C                   PARM                    SndMg08
359600090211     C                   PARM                    Qusec
359700090211     C                   PARM                    SndMg10
359800090211     C                   PARM                    SndMg11
359900121105     c                   PARM                    SndMg12
360000090211      *
360100090211     C                   ENDSR
360200121105      *---------------------------------------------------------------*
360300121105      * CTRL caricamento schiere    PT                               -*
360400121105      *---------------------------------------------------------------*
360500121105     C     Check_PT      begsr
360600121105     C*
360700121105     c* Controllo riempimento schiera
360800121105     c                   clear                   trul0sds
360900121105     c                   eval      i0sski='CPT'
361000121105     c                   eval      i0sele=%elem(CPT)
361100121105     c                   eval      i0spie=x
361200121105     c                   eval      i0sfile='EDTAB00F'
361300121105     c                   eval      i0ssif=knsif
361400121105     c                   eval      i0spgm='TRTCT09R'
361500121113     c                   move      kpjbu         SvKpjbu
361600121113     c                   clear                   kpjbu
361700121105     c                   movel     trul0sds      kpjbu
361800121105     c                   call      'TRUL0SR'
361900121105     c                   parm                    kpjba
362000121113     c                   eval      kpjbu  =  SvKpjbu
362100121105     C*
362200121105     C                   ENDSR
362300090211     O*****************************************************************
362400090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
362500090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
362600090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
362700090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
