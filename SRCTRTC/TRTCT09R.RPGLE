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
025200090209     D kKUT            s                   LIKE(TBLKUT)
025300090209     D kCOD            s                   LIKE(TBLCOD)
025400090209     D kKEY            s                   LIKE(TBLKEY)
025500090209     D kKCC            s                   LIKE(ACOKCC)
025600090209     D kKSC            s                   LIKE(ACOKSC)
025700090209     D kAAA            s                   LIKE(NUFAAA)
025800090209     D kCNU            s                   LIKE(NUFCNU)
025900090209     D kFIL            s                   LIKE(NUFFIL)
026000090209     D kAAS            s                   LIKE(VABAAS)
026100090209     D kLNP            s                   LIKE(VABLNP)
026200090209     D kNRS            s                   LIKE(VABNRS)
026300090209     D kNSP            s                   LIKE(VABNSP)
026400090209     D kCMR            s                   LIKE(VABCMR)
026500090209     D kCCM            s                   LIKE(VABCCM)
026600090209     D kNCD            s                   LIKE(VADNCD)
026700090210     D kCNT            s                   LIKE(VADCNT)
026800090209     D kNMC            s                   LIKE(RDENMC)
026900090209     D kNRP            s                   LIKE(RDENRP)
027000090209     D kLNP1           s                   LIKE(RDELNP)
027100090209      *
027200090209      *  Invio E-mail
027300090209     D Indirizzi       s            253
027400090209     D Oggetto         s             44
027500090209     D Messaggio       s           5000
027600090209      *
027700090209     d   DSmail        ds
027800090209     D Mail_Ind                      44
027900090209     d   IND_char                     1    dim(44)
028000090209      *
028100090209     D Lun_Ind         s              5u 0
028200090209      *
028300090209     D Dac             s              5u 0
028400090209     D Luc             s              5u 0
028500090209      *
028600090209     C*---------------------------------------------------------------*
028700090209     D MsgDSP          S            256                                         Testo generico
028800090209     C*---------------------------------------------------------------*
028900090209     D SndMg01         S             10                                         Message type
029000090209     D                                     INZ('*INFO')
029100090209     D SndMg02         S             10                                         Deluvery mode
029200090209     D                                     INZ('*BREAK')
029300090209     D SndMg03         S            256                                         Message text
029400090209     D SndMg04         S             10I 0                                      Length of text
029500090209     D                                     INZ(%SIZE(SndMg03))
029600090209     D SndMg05         S             10                                         User profile
029700090209     D                                     DIM(299)
029800090209     D SndMg06         S             10I 0                                      Number of user
029900090209     D SndMg07         S             10I 0                                      Message sent indic.
030000090209     D SndMg08         S             10I 0                                      Function requested
030100090209     D SndMg10         S              1                                         Show display
030200090209     D                                     INZ('N')
030300090209     D SndMg11         S             20                                         Qualified MSGQ name
030400090209     D SndMg12         S              4                                         Name type indicator
030500090209     D                                     INZ('*USR')
030600090209      * indice di scihera
030700090209     D Jx              S              5I 0
030800090209      *
030900090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031000090209      *
031100090209     d bart_it         C                   CONST('@Bartolini.it;')
031200090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031300060612      * ?================================================================== */
031400060612      *
031500060612      * ?   * Campi x decodifica * (INPUT  del Record)
031600090130     D  Dati           s           2048
031700060612      * ?   *--------------------------------------------------------------*
031800060612     D  se_errore      s              1    inz(' ')
031900060612      * ?* ------------------------------------------------------ *
032000060710     D Digits          C                   '0123456789'
032100060612      * ?================================================================== */
032200060612      *   Ciclo principale
032300090205      * ?================================================================== */
032400060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
032500060612     C*
032600060612     C                   if        not %open(tivin00r)
032700060612     C                   open      tivin00r
032800060612     C                   endif
032900060612      **
033000060612      * ?------------------------------------------------------------------ */
033100060612     C*? Controllo dati arrivati da DPD
033200060612      * ?------------------------------------------------------------------ */
033300060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
033400060612      * ?- controllando che la trasmissione sia completa.
033500090205      **
033600060612      * ?              /*---------------------- */
033700090205     c                   goto      nonFare_adesso
033800060612     c                   exsr      check_Trasm
033900090205     c     nonFare_adessotag
034000060612      * ?              /*---------------------- */
034100090205      **
034200060613      **  Errore di trasmissione x tutti i records
034300060613      **   --> file in errore
034400060613     c                   if        se_errore = 'S'
034500060612
034600060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
034700090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
034800090205     c                   exsr      Msg_errato
034900060612      **
035000060612     c                   else
035100060614      **
035200060612      * ?------------------------------------------------------------------ */
035300060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
035400060612      * ?------------------------------------------------------------------ */
035500090205      **
035600060612      * ?              /*---------------------- */
035700060612     c                   exsr      Importa_Msg
035800060612      * ?              /*---------------------- */
035900060612
036000060614     c                   end
036100060614
036200060612     C                   if        %open(tivin00r)
036300060612     C                   close     tivin00r
036400060612     C                   endif
036500060612      *
036600060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
036700090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
036800090225     c                             esito ='0'
036900060710     C                   eval      esito ='0'
037000090225      *
037100090225     c                   COMMIT
037200090224      *
037300090213     c                   if        almeno_un_due ='S'
037400090213     C                   eval      esito ='1'
037500060614     C                   endif
037600090213     C                   endif
037700060614      *
037800060612     c                   SETON                                        LR
037900060612      * ?================================================================== */
038000060612      *? Importa i records della tramsissione
038100060612      * ?------------------------------------------------------------------ */
038200060612     c     Importa_Msg   Begsr
038300060612
038400090211     C                   EXSR      AZZMSG
038500060614
038600060612     c     *start        setll     Tivin00r
038700060612     c                   read      Tivin00r
038800060612
038900060612     c                   dow       not %eof(Tivin00r)
039000060614
039100060614      * solo i record sflaggati da rielaborare
039200060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
039300090211      *  Sempre Record OK
039400090211      ** Controlli formali sui campi
039500090211     C                   eval      vinFLG = '1'
039600060612     c                   clear                   se_errore
039700090209
039800090213      *** ------------------------------------
039900101108      *** A FINE CICLO DI UN DETTAGLIO:   -->"RFF" e alla fine "UNT" x DISPOR
040000090213      *** ------------------------------------
040100090213      * ?              /*---------------------- */
040200090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
040300090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
040400090213      * ?              /*---------------------- */
040500090601      **
040600101108      *   OGNI DETTAGLIO è determinato dal RFF e alla fine per ultimo da UNT.
040700101108     c                   eval      tipo_seg = %subst(vinDTA:1:3)
040800101109      *
040900101109      **  ad ogni inizio dettaglio successivo scarica il precedente
041000101108      * ?              /*---------------------- */
041100101109     c                   IF            se_trovato_NAD ='S'          and
041200101109     c                               ( tipo_seg = 'UNT'             or
041300101109     c                                       ( tipo_seg = 'RFF' and
041400101109     c                               %subst(vinDTA:5:3) = 'SEN' ) )
041500090213      *
041600090213      *  Deve flaggare il dettaglio con dei problemi
041700090213     C                   if        Err_in_detta = 'S'
041800090213      **?              /*---------------------- */
041900090213     c                   exsr      DETta_Errato
042000090213      **?              /*---------------------- */
042100090213     c                   end
042200090213      *
042300060612      **  Se presente un errore nel record emette una segnalazione msg
042400090213     c                   if        se_errore = 'S'
042500090611      **
042600090611      **
042700090213     c                   else
042800090209      ******
042900090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043000090227     c                   if         se_trovato_NAD ='S'
043100060612      * ?              /*---------------------- */
043200090209     c                   exsr      Scrive_VAB
043300060612      * ?              /*---------------------- */
043400090227     c                   end
043500090227      *
043600090209      *  Problemi nella decodifica dei campi VAB/VAT
043700090213     c                   if        errori_in_Wrt = 'S'
043800090213      *
043900090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
044000090213     c                   exsr      Error_DET
044100090213      *
044200090213     c                   ROLBK
044300090213      *
044400090213     c                   else
044500090601      *
044600090213     c                   COMMIT
044700090601      *
044800090227      * Dopo aver confermato, con COMMIT
044900090227      *  si prepara per una nuova spedizione
045000090227      * ?                         --------------
045100090227     c                   exsr      Nuova_spediz
045200090227      * ?                         --------------
045300090213     c                   end
045400090209
045500090209     c                   end
045600090210      **
045700060620     c                   end
045800101108      * ?              /*---------------------- */
045900060612
046000101108      ** Decodifica record a campi variabili
046100101108      * ?              /*---------------------- */
046200101108     c                   exsr      Decod_Segmento
046300101108      * ?              /*---------------------- */
046400101108
046500060621      *  x errore bloccante nella composizione del VAB/VAT
046600060621     c                   if        err_bloccante ='S'
046700060621     C                   eval      vinFLG = '2'
046800090211     c                   eval      esito  = '2'
046900101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
047000090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047100090603     C                             Bolle import in filiale - messaggio -
047200090603     C                             con UNB non presente in tabella PT controlla-
047300090603     C                             re EDI ricevuto su UPLOAD.'
047400090603     c                   exsr      invio_mail_AB
047500060621     c                   end
047600060612
047700100716     c                   update    Tivin000                             99
047800090211
047900090211      *** se non è riconosciuto l'UNB deve uscire direttamente
048000090211     c                   if        se_errore ='S'  and
048100090211     c                             tipo_seg = 'UNB'
048200090213     c                   eval      err_bloccante = 'S'
048300090211     c                   exsr      Msg_errato
048400090211     c                   LEAVE
048500090211     c                   endIF
048600090211      ***
048700060614     c                   endIF
048800060612
048900060612     c                   read      Tivin00r
049000060612     C                   ENDdo
049100060612      **
049200060612     c                   endSR
049300090211     C*----------------------------------------------------------------
049400090211     C*? AZZMSG - Azzera dati messaggio
049500090211     C*----------------------------------------------------------------
049600090211     C     AZZMSG        BEGSR
049700090211     C*
049800090211     C* Pulisco dati di work x foglio viaggio
049900090211     C                   MOVEL     *BLANKS       WNUMFV           35
050000090211     C                   MOVEL     *BLANKS       WNRCMR           35
050100090211     C                   Z-ADD     0             WDATPA            8 0
050200090211      *
050300090211      * pulisce gli RFF+FF di testata
050400090211     C                   clear                   $CLKSC            7 0
050500090211     C                   clear                   $CLTAR            3
050600090211     C                   clear                   $CLLNP            3 0
050700090211     C                   clear                   $CLNRS            2 0
050800090211     C                   clear                   $CLCTM            2
050900090211     C                   clear                   $CLBS1            1
051000090211     C                   clear                   $CLfnsp           1            *blk/S
051100090211      *
051200090211     C*  Inizializzo variabile new documento
051300090211     C                   Z-ADD     0             KCL
051400090211     C                   Z-ADD     0             WW                3 0
051500090211     ** Inizializza campo di work
051600090211     c                   move      'N'           WGID99
051700090211      * per controllare se almeno un record è stato importato sul VAB
051800090211     c                   clear                   Almeno_Uno        1
051900090213     c                   clear                   Almeno_Un_Due     1
052000090211     c                   eval      esito  = '0'
052100090211     C*
052200090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
052300090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
052400090603     c                   eval      NAD_destinatario_in_testata = *blank
052500090603     c                   eval      NAD_mittente_in_testata = *blank
052600090611     c                   eval      In_Testata = 'S'
052700090611     C*
052800090611     C                   clear                   Tes_VABdcm
052900090611     C*
053000090611     c                   clear                   tes_VABrmo
053100090611     c                   clear                   tes_VABnmo
053200090611     c                   clear                   tes_VABcmo
053300090611     C*
053400090611     c                   clear                   tes_VABrsd
053500090611     c                   clear                   tes_VABrd2
053600090611     c                   clear                   tes_VABind
053700090611     c                   clear                   tes_VABlod
053800090611     c                   clear                   tes_VABnzd
053900090611     C*
054000090611     c                   clear                   tes_Valuta
054100090611     c                   clear                   tes_VABcbo
054200090611     C*
054300090611     c                   clear                   Tes_VABdcr
054400090611     c                   clear                   Tes_VABtcr
054500090611     c                   clear                   Tes_VABhcr
054600090611     C*
054700090611     c                   clear                   tes_VABnas
054800090611     c                   clear                   tes_VABtsp
054900090611     C*
055000090611     c                   clear                   unh_VABRMA
055100090609     c                   clear                   unh_VABRMN
055200090611     C*
055300090611     C                   clear                   Tes_VABlnp
055400090611     C                   clear                   Tes_VABnrs
055500090611     C                   clear                   Tes_VABctm
055600090611     C*
055700090611     C                   clear                   VABfnsp_PT
055800090611     C*
055900090611     C                   MOVEA     *HIVAL        SGN
056000090603     C*
056100090211     C                   ENDSR
056200090210     C*----------------------------------------------------------------
056300090210      *? DECODIFICA DATI UNB
056400090210     C*----------------------------------------------------------------
056500090210     C     DATI_UNB      BEGSR
056600090210      *
056700090209      *  INIZIALIZZA  dati fondamentali messaggio
056800090209     C                   clear                   EDIDSPT
056900090209     C                   clear                   EDIDSPU
057000090209     C                   clear                   EDIDSPV
057100090209     C                   clear                   wnrDOC           35
057200090209     C                   move      'N'           PT_ok             1
057300090209      ** ---------------------------------------------------------------
057400090209      * quindi 1° cosa:
057500090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
057600090209      *     senza questo il messaggio NON è trascodificabile
057700090209     C                   MOVEL     unb0004       WCOD             35
057800090209     C                   MOVE      unb000720     WCOD
057900090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
058000090209     C                   Z-ADD     1             XX                3 0
058100090209     C     WCOD          LOOKUP    CPT(XX)                                32
058200090209      *
058300090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
058400090209     C                   If        *IN32 = *off
058500090209      * ******  Errore generale sul Messaggio
058600090209      * Msg di allerta Traduzione
058700090209     C                   EVAL      Indirizzi = CED_Bart
058800101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
058900090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
059000090209     C                             Bolle import in filiale - messaggio -
059100090209     C                             con UNB non presente in tabella PT controlla-
059200090209     C                             re EDI ricevuto su UPLOAD.'
059300090209      *
059400090209     c                   exsr      invio_mail
059500090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
059600090210     C                   eval      vinFLG = '2'
059700090210     C                   eval      se_errore   = 'S'
059800090210     c                   goto      end_UNB
059900090209      *
060000090209     C                   move      'N'           PT_ok             1
060100090209      *
060200090209      *             *------------------------------*
060300090209     C                   eLSe
060400090209      *             *------------------------------*
060500090209      * Se tutto OK su tab.PT:
060600090209     C                   move      'S'           PT_ok             1
060700090209      *
060800090209     C                   MOVEL     DPT(XX)       EDIDSPT
060900090209     C                   MOVEL     DPU(XX)       EDIDSPU
061000090209     C                   MOVEL     DPV(XX)       EDIDSPV
061100090226     C                   eval      Mittente       = unb0004
061200090226     C                   eval      Mitt_qualifier = unb000720
061300090226     C                   eval      Id_Messaggio   = UNB0022
061400090209      *
061500090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
061600090209      *  ricevuti:
061700090209     c                   movel     §PVema        mail_ind         44
061800090209      *
061900090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
062000090209     c                   if        mail_ind <> *blank
062100090209     c                   exsr      imp_ADDR_mail
062200090209     c                   end
062300090209      *
062400090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
062500090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
062600090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
062700090209     C                   if        §PULUN = *blank
062800090209     C                   eval      §PULUN = '00'
062900090209     c                   end
063000090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
063100090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
063200090209      *
063300090209     C                   Z-ADD     §PTKSC        WKSC              7 0
063400090611      *
063500090611      ** salva i dati principali
063600090611     C                   MOVEL     §PTLNP        Tes_VABlnp
063700090611     C                   MOVEL     §PTNRS        Tes_VABnrs
063800090611     C                   MOVEL     §PTCTM        Tes_VABctm
063900090209      *
064000090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
064100090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
064200090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
064300090209      *  i records trasmessi).
064400090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
064500090209      *
064600090209      *  In base al cod.trattamento merce reperisco tipo tratt.
064700090209     C                   CLEAR                   DS1B
064800090209     C                   Z-ADD     1             Y                 3 0
064900090611     C     Tes_VABctm    LOOKUP    CTM(Y)                                 32
065000090209     C     *IN32         IFEQ      '1'
065100090209     C                   MOVEL     DTM(Y)        DS1B
065200090209     C                   MOVEL     DS1B          WSVTRM           38
065300090209     C                   END
065400090209      *
065500090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
065600090209     C                   clear                   ACORAG
065700090209     C                   clear                   INDCAE
065800090209     C                   clear                   INDCAP
065900090209     C                   clear                   INDSTA
066000090209      * Decodifico partner
066100090209     C                   Z-ADD     1             KKUT
066200090209     C                   Z-ADD     KCI           KKCC
066300090209     C                   MOVE      §PTKSC        KKSC
066400090209     C     KACO          CHAIN     CNACO00F                           31
066500090209     C     KIND          CHAIN     CNIND00F                           31
066600090211      *
066700090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
066800090211      *   questa schiera è utilizzata anche per i clienti.
066900090211     C     §ptKSC        LOOKUP    KCL                                    39
067000090211     c                   If        *in39 = *off
067100090211     C                   add       1             WW
067200090211     C                   z-add     §ptKSC        KCL(WW)
067300090211     C                   Endif
067400090209      *
067500090209     C                   MOVEL     unb0017       WAA4              4 0
067600090209     C                   MOVE      WAA4          WAA2              2 0
067700090209     C                   MOVE      unb0017       WGG               2 0
067800090209     C                   MOVE      unb0017       WDTPRE            8 0
067900090611     C                   MOVEl     '20'          WDTPRE
068000090209     C                   MOVE      unb0019       WHMPRE            4 0
068100090209     C                   MOVE      unb0017       WDTMSG            6 0
068200090209     C                   MOVE      WAA2          WDTMSG
068300090209     C                   MOVEL     WGG           WDTMSG
068400090210      * potremmo
068500090210      * Utilizzare l'identificativo della trasmissione come CMR
068600090209     C                   IF        unb0022 <> *blank
068700090209     C                   MOVEL     unb0022       WNRDOC
068800090211     C                   MOVEL     unb0022       WNUMFV
068900090211     C                   MOVEL     unb0022       WNRCMR
069000090209     c                   end
069100090209      *
069200090209     c                   Endif
069300090209      **
069400090609     c     end_UNB       endSR
069500090609      * ?================================================================== */
069600090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
069700090609      * ?================================================================== */
069800090609     C     DATI_UNH      BEGSR
069900090609      *
070000090609      ** Riferimento del cliente
070100090609     C                   MOVEL     unh0062       WPRGSP           35
070200090609     C                   MOVEL     unh0062       WNSP             35
070300090609     C                   MOVEL     unh0062       WNUM             35
070400090609      * ?                                       --------
070500090609     C                   MOVEL     unh0062       unh_VABRMA
070600090609      * ?                                       --------
070700090609     C     ' '           SCAN      unh0062       WLUNGH            3 0
070800090609     C                   SUB       1             WLUNGH
070900090609     C                   EXSR      TSTNUM
071000090609     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
071100090609      * ?                                       --------
071200090609     C                   MOVEL     WNUMOK        unh_VABRMN
071300090609      * ?                                       --------
071400090609     C                   END
071500090609      **
071600090609     c     end_UNH       endSR
071700090609      * ?================================================================== */
071800090609     C*? INZIO del Messaggio
071900090609      * ?================================================================== */
072000090609     C     DATI_BGM      BEGSR
072100090609      *
072200090609      **
072300090609     c     end_BGM       endSR
072400090209      * ?================================================================== */
072500090609     C*? Tipo Servizio
072600090209      * ?================================================================== */
072700090609     C     Dati_TSR      BEGSR
072800090609      **
072900090609      **    Inviano solo spedizioni Espresso e non Priority
073000090611      **        tutte le spedizioni vanno come "Normali"
073100101108     c**********         if        tsr4751 = 'D'
073200101108     c**********         if        tsr4219 = 1 or tsr4219 = 2
073300101108     c**********         eval      VABtsp = 'E'
073400101108     c**********         end
073500101108     c**********         end
073600090611      **
073700090611      **  se presente in testata se lo memorizza qualora non fosse presente
073800090611      **   nei singoli dettagli
073900090611     c                   if        In_Testata       = 'S'
074000090611     C                   eval       tes_VABtsp = VABtsp
074100090611     c                   end
074200090609      **
074300090609     c     end_TSR       endSR
074400090610      * ?================================================================== */
074500090611     C*? Accordi commerciali
074600090610      * ?================================================================== */
074700090610     C     DATI_CAG      BEGSR
074800090611      ****
074900090611      *  Attenzione questo segmento in alcuni messaggi potrebbe trovarsi solo
075000090611      *  in testata e non in dettaglio.
075100090611      ****
075200090610      *
075300090610      *  imposta il default dalla tabella PT se c'è
075400090610     C     §puPFA        ifNE      *blank
075500090610     c                   SELECT
075600090610      *  Porto Franco
075700090610     c                   WHEN      §puPFA = 'F'
075800090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
075900090611      *  Porto Assegnato
076000090611     C                   WHEN      §puPFA = 'A'
076100090611     C                   movel     '2'           VABCBO                         Senza C/Ass
076200090610     C                   ENDSL
076300090610     c                   end
076400090610      *
076500090610      *   Imposta o il codice o il metodo
076600090610      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
076700090611     c                   if        cag4237  <> *blank
076800090611      *
076900090610     C                   movel     UNB_Mittente  etbUNB
077000090611     C                   movel(p)  'CAG'         etbSGM
077100090611     C                   movel(p)  cag4237       etbVALSGM
077200090610     c     K_TAB1L       chain     edstbl01l
077300090610      * prova quindi senza l'UNB specifico del cliente
077400090610     c                   if        not %Found(edstbl01l)
077500090610     C                   clear                   etbUNB
077600090610     c     K_TAB1L       chain     edstbl01l
077700090610     c                   end
077800090610      *
077900090610     c                   if        %Found(edstbl01l)
078000090610     c                   SELECT
078100090610     c                   WHEN      ETBDATI = 'F'
078200090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
078300090611      *  Porto Assegnato
078400090611     c                   WHEN      VABCAS > *zero
078500090611     C                   movel     '2'           VABCBO                         Senza C/Ass
078600090610     C                   ENDSL
078700090610      *
078800090610     c                   end
078900090610     c                   endIF
079000090610      **
079100090610      **  Il tipo bolla non è presente
079200090611     c                   if         VABCBO = *blank
079300090611     C                   eval      vinMSG = 'CAG non rilevato'
079400090610     c                   exsr      Error_DET
079500090611     c                   goto      end_CAG
079600090610     c                   end
079700090611      **
079800090611      **  se presente in testata se lo memorizza qualora non fosse presente
079900090611      **   nei singoli dettagli
080000090611     c                   if        In_Testata       = 'S'
080100090611     C                   eval       tes_VABcbo = VABcbo                         Senza C/Ass.
080200090611     c                   eval      CAG_porto_in_testata = 'S'
080300090611     c                   end
080400090610      **
080500090610     c     end_CAG       endSR
080600090611      * ?================================================================== */
080700090611     C*?  Anagrafica del mittente e del destinatario
080800090611      * ?================================================================== */
080900090611     C     DATI_NAD      BEGSR
081000090611      *
081100090611      * Dati destinatario
081200090611     C                   IF        nad3035 = 'CN' or
081300090611     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
081400090611      *
081500090611      * Se trovato un indirizzo di destinatario
081600090611      *   abilita la scrittura del VAB e del VAT.
081700090611     c                   eval       se_trovato_NAD ='S'
081800090611      *
081900090611     C* Reperisco nazione corrispondente destinatario
082000090611     C                   Z-ADD     1             YY                3 0
082100090611     c                   eval      *in32 = *off
082200090611     C                   if        nad3207 <> *BLANKS
082300090611     C     nad3207       LOOKUP    ISO(YY)                                32
082400090611     C                   endif
082500090611      *
082600090611     C* Imposto dati destinatario
082700090615     C                   MOVEL     nad30361      VABRSD
082800090615     C                   if        nad30362 <> *LOVAL
082900090615     C                   MOVEL     nad30362      VABRD2
083000090611     C                   endif
083100090611      *
083200090611     C                   MOVEL     nad30421      VABIND
083300090630      *
083400090630     C                   MOVEL     nad3164       VABLOD
083500090630      *
083600090611      *
083700090611     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
083800101108      *      li mettiamo in aggiunta alla 2°ragione sociale  VABRD2
083900090611     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
084000090611      *
084100090611     C* Verifico se c'è dello spazio per nella località
084200090630     C     '    '        SCAN      VABRD2        WI                4 0
084300090611      *
084400090630     C                   MOVEA     VABRD2        LOD
084500090611     C                   ADD       1             WI
084600090611     C                   MOVEA     NAD30422      LOD(WI)
084700090630     C                   MOVEA     LOD           VABRD2
084800090611     C                   ENDif
084900090611      *
085000090611     C                   If        *in32 = *on
085100090611     C                   MOVEL     C15(YY)       VABNZD
085200090611     C                   MOVEL     CPF(YY)       WFLCPF            2 0
085300090611     c                   Else
085400090611     C                   MOVEL     *BLANKS       VABNZD
085500090611     C                   Z-ADD     0             WFLCPF
085600090611     c                   Endif
085700090611      * Converto CAP
085800090611     c                   move      nad3251       cap09a            9
085900090611     C                   EXSR      CNVCAP
086000090611     C* Se in testata
086100090611     c                   if        In_Testata       = 'S'
086200090611     c                   eval       tes_VABrsd = VABrsd
086300090611     c                   eval       tes_VABrd2 = VABrd2
086400090611     c                   eval       tes_VABind = VABind
086500090611     c                   eval       tes_VABlod = VABlod
086600090611     c                   eval       tes_VABnzd = VABnzd
086700090611     c                   eval      NAD_destinatario_in_testata = 'S'
086800090611     c                   end
086900090615      *
087000090615     C* Ricerco CAP
087100090615     C*****WCAP          IFNE      *BLANKS
087200090615      *
087300090615     C     mse6799       IFEQ      'CGW'
087400090615      *
087500090615      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
087600090615     C                   CLEAR                   FNLV55DS
087700090615     C                   MOVEL     'P'           D55TPT
087800090615     C                   MOVEL     WOGGI         D55DRF
087900090615     C                   MOVEL     VABLNP        D55LNP
088000090615     C                   MOVEL     VABLNP        D55LIN
088100090615     C                   CALL      'FNLV55R'
088200090615     C                   PARM                    FNLV55DS
088300090615     C                   MOVE      D55TFP        COMTFP            3 0
088400090615      *
088500090615     C*  PASSO IL TERMINAL PARTENZA
088600090615     C                   CLEAR                   TISI95DS
088700090615     C                   Z-ADD     COMTFP        I95TFP
088800090615     C                   MOVEL     VABTSP        I95TSP
088900090615     C                   MOVEL     'S'           I95FRE
089000090615     C                   MOVEL     '3'           I95TCN
089100090615     C                   MOVEL     WCAP          I95CAP
089200090615     C                   MOVEL     VABNZD        I95NAR
089300090615     C                   MOVEL     VABLOD        I95LOC
089400090615     C                   MOVEL     VABIND        I95IND
089500090615      *
089600090615     C* Se gestita seconda linea di arrivo passo peso - volume
089700090615     C* e numero colli effettivo
089800090615     C     §PULNA        IFEQ      'S'
089900090615     C                   Z-ADD     VABPKB        I95LKG
090000090615     C                   Z-ADD     VABVLB        I95LMC
090100090615     C                   ELSE
090200090615     C                   END
090300090615     C*
090400090615     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
090500090615     C                   SELECT
090600090615     C     VABCBO        WHENEQ    '1 '
090700090615     C                   MOVEL     *BLANKS       I95FCA
090800090615     C                   MOVEL     'F'           I95TPO
090900090615     C     VABCBO        WHENEQ    '2 '
091000090615     C                   MOVEL     *BLANKS       I95FCA
091100090615     C                   MOVEL     'A'           I95TPO
091200090615     C     VABCBO        WHENEQ    '4 '
091300090615     C                   MOVEL     'S'           I95FCA
091400090615     C                   MOVEL     'F'           I95TPO
091500090615     C     VABCBO        WHENEQ    '6 '
091600090615     C                   MOVEL     'S'           I95FCA
091700090615     C                   MOVEL     'A'           I95TPO
091800090615     C                   OTHER
091900090615     C                   MOVEL     'F'           I95TPO
092000090615     C     VABCAS        IFGT      0
092100090615     C                   MOVEL     'S'           I95FCA
092200090615     C                   ELSE
092300090615     C                   MOVEL     *BLANKS       I95FCA
092400090615     C                   END
092500090615     C                   ENDSL
092600090615     C*
092700090615     C                   CALL      'TISI95R'
092800090615     C                   PARM                    TISI95DS
092900090615     C* Reperisco i dati da CAP
093000090615     C                   MOVEL     O95PRV        VABPRD
093100090615     C                   MOVEL     O95LNA        VABLNA
093200090615     C                   MOVEL     O95ZNC        VABZNC
093300090615     C                   MOVEL     WCAP          VABCAD
093400090615     C*
093500090615     C     O95ERR        IFNE      *BLANKS
093600090615     C     O95FIT        ANDEQ     'S'
093700090615     C*  Cap non trovato
093800090615     C                   eval      vinMSG = 'MSE dal peso (CAP) non trovato'
093900090615     c                   exsr      Error_DET
094000090615     C                   goto      end_NAD
094100090615     C                   END
094200090615     C                   END
094300090615      *
094400090615     C*****              END
094500090615      *
094600090615     C     WCAP          IFEQ      *BLANKS
094700090615      *
094800090615     C                   CLEAR                   VABPRD
094900090615     C                   CLEAR                   VABLNA
095000090615     C                   CLEAR                   VABZNC
095100090615     C                   CLEAR                   VABCAD
095200090615     C*  Cap non trovato
095300090615     C*******            eval      vinMSG = 'MSE dal peso (CAP) mancante'
095400090615     c*******            exsr      Error_DET
095500090615     C*******            goto      end_MSE
095600090615     C                   END
095700090611     C*
095800090611     C                   ENDIF
095900090611      *
096000090611      * Dati mittente  ---------------
096100090611     C                   IF        nad3035 = 'CO'
096200090611     C                   movel     nad30361      VABRMO
096300090611      *
096400090611      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
096500090611     C                   movel     §PTNAZ        VABNMO
096600090611      *
096700090611     C                   IF        nad3207 <> *blanks
096800090611     C                   eval      YY = 1
096900090611     C     nad3207       LOOKUP    ISO(YY)                                32
097000090611     C                   if         *in32 = *on
097100090611     C                   MOVEL     C15(YY)       VABNMO
097200090611     C                   endif
097300090611     C                   ENDIF
097400090611      *
097500090611      * Normalizzo CAP mittente originale
097600090611     C                   if        nad3251 <> *blanks
097700090611      *
097800090611     C                   clear                   T
097900090611     C                   clear                   S
098000090611     C                   MOVEA     *BLANKS       CAPM
098100090611     C                   MOVEA     nad3251       CAP9
098200090611     C                   DO        9             T
098300090611     C     CAP9(T)       IFNE      *BLANKS
098400090611     C                   ADD       1             S
098500090611     C                   MOVE      CAP9(T)       CAPM(S)
098600090611     C                   ENDIF
098700090611     C                   ENDDO
098800090611     C*
098900090611      *     Controllo validità CAP
099000090611     C                   MOVEA     CAPM          VABCMO
099100090611      *
099200090611     C                   CLEAR                   TISI95DS
099300090611     C                   MOVEL     VABCMO        I95CAP
099400090611     C                   MOVEL     VABNMO        I95NAR
099500090611     C                   MOVEL     WOGGI         I95DAT
099600090611     C                   MOVEL     '3'           I95TCN
099700090611     C                   CALL      'TISI95R'
099800090611     C                   PARM                    TISI95DS
099900090611      *     Errore
100000090611     C     O95ERR        IFNE      *BLANKS
100100090611     C                   MOVEL     INDCAE        VABCMO
100200090611     C                   MOVEL     §PTNAZ        VABNMO
100300090611     C                   END
100400090611      *
100500090611     c                   endIF
100600090611     C* Se in testata
100700090611     c                   if        In_Testata       = 'S'
100800090611     c                   eval       tes_VABrmo = VABrmo
100900090611     c                   eval       tes_VABnmo = VABnmo
101000090611     c                   eval       tes_VABcmo = VABcmo
101100090611     c                   eval       NAD_mittente_in_testata = 'S'
101200090611     c                   end
101300090611      *
101400090611     C                   END
101500090611      *
101600090611      * NOn c'è l'identificativo bolla del cliente
101700090611     C                   IF        nad3035 = 'CN'
101800090611     C                   if          vabRSD = *blank or
101900090611     C                               vabIND = *blank or
102000090611     C                               vabLOD = *blank
102100090611     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
102200090611     c                   exsr      Error_DET
102300090611     c                   goto      end_NAD
102400090611     C                   ENDIF
102500090611     C                   ENDIF
102600090611      *
102700090611     c     end_NAD       endSR
102800090611      * ?================================================================== */
102900090611     C*? Imposta le date
103000090611      * ?================================================================== */
103100090611     C     DATI_DTM      BEGSR
103200090611     C*
103300090611     C*  Se data documento  memorizzo numero e data x CMR
103400090611     C     dtm2005       IFEQ      'DEP'
103500090611     C                   MOVEL     '20'          W_DATA8           8            Data
103600090615     C                   MOVE      dtm2001       W_DATA8
103700090611     C                   MOVEL     W_DATA8       WDATA            35            Data
103800090611     C                   MOVEL     '102'         WFORM             3            Formato
103900090611     C                   EXSR      CNVDAT
104000090611     C                   MOVEL     WCAMG         Tes_VABdcm                     Data CMR
104100090611     C     WERRO         IFEQ      'S'
104200090611     C                   eval      vinMSG = 'DTM data msg errata'
104300090611     c                   exsr      Error_DET
104400090611     c                   goto      end_DTM
104500090611     C                   END
104600090611     C                   END
104700090611      **
104800090611     C*  Se data consegna richiesta imposto già sui campi del VAB
104900090611     C     dtm2005       IFEQ      'DAD'
105000090611     C                   MOVEL     '20'          W_DATA8           8            Data
105100090615     C                   MOVE      dtm2001       W_DATA8
105200090611     C                   MOVEL     W_DATA8       WDATA            35            Data
105300090611     C                   MOVEL     '102'         WFORM             3            Formato
105400090611     C                   EXSR      CNVDAT
105500090828     C***  In accordo con il cliente NON si prende + la SUA Data cons.rich.
105600090828     C***  poichè il cliente inviava una data uguale a quella della spedizione
105700090828     C***  e non poteva modificare nulla nel suo sistema.
105800090828     C***   autorizzato da Pantalena il  28/08/2009
105900090828     C*******            MOVEL     WCAMG         VABDCR                         Data cons.rich.
106000090828     C*******            MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
106100090828     C*******            MOVEL     WHMS          VABHCR                         Ora  cons.rich.
106200090611     C     WERRO         IFEQ      'S'
106300090611     C                   eval      vinMSG = 'DTM DEP - data errata'
106400090611     c                   exsr      Error_DET
106500090611     c                   goto      end_DTM
106600090615     C                   ELSE
106700090615     C* Se in testata
106800090615     c                   if        In_Testata       = 'S'
106900090828     C*******            eval       Tes_VABdcr = VABDCR                          Data cons.rich.
107000090828     C*******            eval       Tes_VABtcr = VABTCR                          Tipo cons.rich.
107100090828     C*******            eval       Tes_VABhcr = VABHCR                          Ora  cons.rich.
107200090615     c                   endIf
107300090615     C                   END
107400090611     C                   END
107500090611      **
107600090611     c     end_DTM       endSR
107700090611      * ?================================================================== */
107800090611     C*? Descrizione della Merce (NATURA MERCE)
107900090611      * ?================================================================== */
108000090611     C     DATI_GDS      BEGSR
108100090611     C*
108200090611     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
108300090611     c                   if        gds70021 <> *blank
108400090611     C                   MOVEL     gds70021      Tes_VABnas
108500090611     c                   end
108600090611      **
108700090611     c     end_GDS       endSR
108800090609      * ?================================================================== */
108900090611     C*?  Dettagli sul trasporto
109000090609      * ?================================================================== */
109100090611     C     DATI_TDT      BEGSR
109200090609      *
109300090611      **
109400090611     c     end_TDT       endSR
109500090611      * ?================================================================== */
109600090611     C*? Beni Pericolosi
109700090611      * ?================================================================== */
109800090611     C     DATI_DGS      BEGSR
109900090611      **
110000090611      **
110100090611     c     end_DGS       endSR
110200090611      * ?================================================================== */
110300090611     C*? Documentazione
110400090611      * ?================================================================== */
110500090611     C     DATI_DOC      BEGSR
110600090611      **
110700090611      * Numero di ricevuta
110800090611     C                   if        doc1001 = 'WBL' and doc1791 = 'ACG'
110900090611     C                                     and doc1004 <> *blank
111000090611      *
111100090611     C                   movel     doc1004       WCU
111200090611     C                   movel     WCU           WNUM             35
111300090611     C     ' '           scan      WCU           WLUNGH
111400090611     C                   sub       1             WLUNGH
111500090611     C                   EXSR      TSTNUM
111600090611     C                   If          WOKNUM = 'S'                               Ctrl numerico
111700090611     C                   movel     WNUMOK        VABRMN
111800090611     C                   movel     'WBL'         WRMN
111900090611     C                   Endif
112000090611     C                   ENDIF
112100090611      **
112200090611     c     end_DOC       endSR
112300090611      * ?================================================================== */
112400090611     C*? Testata intermedia Inizio e Fine Dettaglio
112500090611      * ?================================================================== */
112600090611     C     DATI_UNS      BEGSR
112700090611      *
112800090615     c                   eval      In_Testata =' '
112900090615      *
113000090611      *   Inizio Dettaglio
113100090611     c                   if         UNS0081  = 'D'
113200090611     c                   end
113300090611      *
113400090611      *   Fine   Dettaglio
113500090611     c                   if         UNS0081  = 'S'
113600090611     c                   end
113700090609      **
113800090609     c     end_UNS       endSR
113900090609      * ?================================================================== */
114000090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
114100090609      * ?================================================================== */
114200090609     C     Nuova_spediz  BEGSR
114300090609      **
114400090227      * Pulisco campi del file
114500090227     c                   z-add     vnrec         SAVNUM_RECord
114600090227     C                   CLEAR                   EDiVAB00
114700090227     C                   CLEAR                   EDiVAT00
114800090227     C                   CLEAR                   EDiVAD00
114900090611      *
115000090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
115100090227     C                   MOVEL     'N'           WGID99            1
115200090611      *
115300090227     C                   clear                   XY                5 0
115400090227     C                   clear                   §§                5 0
115500090611      *
115600090227     C                   clear                   WNSP             35
115700090227     C                   MOVEA     *HIVAL        SGN
115800090611      *
115900090227     C*  Azzero codice cliente - tariffa
116000090227     C                   Z-ADD     0             WTOCOC           16 0
116100090227     C                   Z-ADD     0             WTOPLC           16 2          ???
116200090227     C                   Z-ADD     0             WTOPNC           16 2          ???
116300090611      *
116400090611     C                   clear                   QUA
116500090611     C                   clear                   FTX
116600090611      *
116700090227     C*  Azzero codice cliente - tariffa
116800090227     C                   clear                   WCLKSC
116900090227     C                   clear                   WCLTAR
117000090227     C                   clear                   WCLLNP
117100090227     C                   clear                   WCLNRS
117200090227     C                   clear                   WCLCTM
117300090227     C                   clear                   WCLBS1
117400090227     C                   clear                   WCLfnsp                        *blk/S
117500090227     C                   clear                   WNOT1            35
117600090227     C                   clear                   WNOT2            35
117700090227      *
117800090227     C* pulisce campi di work
117900090227     c                   clear                   watrde           35
118000090227     c                   clear                   wattel           25
118100090227     C                   clear                   WRMN              3
118200090227      *
118300090227      * Un errore nel dettaglio
118400090227     C                   clear                   Err_in_detta      1
118500090227     c                   clear                   errori_in_Wrt     1
118600090227     c                   clear                   se_trovato_NAD    1
118700090227      **
118800090611      **  imposta i dati x la testata
118900090611     c                   exsr      DATI_TESTATA
119000090611      **
119100090227     c                   endSR
119200090609      * ?================================================================== */
119300090609     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
119400090609      * ?================================================================== */
119500090611     C     DATI_TESTATA  BEGSR
119600090609      *
119700090611      **  IMPOSTA i campi dalla testata
119800090611     C                   eval       VABLNP = Tes_VABlnp
119900090611     C                   eval       VABNRS = Tes_VABnrs
120000090611     C                   eval       VABCTM = Tes_VABctm
120100090611      **
120200090611     C                   eval      VABdcm = Tes_VABdcm
120300090611     C*
120400090611     c                   eval      VABvca = tes_Valuta
120500090611     c                   eval      VABcbo = tes_VABcbo
120600090611     C*
120700090828     C*******   Come da Accordi presi con il cliente la Data consegna richiesta
120800090828     C*******    NON verrà presa in considerazione
120900090828     c*******            eval      VABdcr = Tes_VABdcr
121000090828     c*******            eval      VABtcr = Tes_VABtcr
121100090828     c*******            eval      VABhcr = Tes_VABhcr
121200090611     C*
121300090611     c                   eval      VABnas = tes_VABnas
121400090611     c                   eval      VABtsp = tes_VABtsp
121500090611     C*
121600090611      * Se Mittente o Destinatario presi dalla Testata del messaggio:
121700090611     C* Se in testata
121800090611     c                   if        NAD_destinatario_in_testata = 'S'
121900090611     c                   eval         VABrsd =  tes_VABrsd
122000090611     c                   eval         VABrd2 =  tes_VABrd2
122100090611     c                   eval         VABind =  tes_VABind
122200090611     c                   eval         VABlod =  tes_VABlod
122300090611     c                   eval         VABnzd =  tes_VABnzd
122400090611     c                   end
122500090611     C* Se in testata
122600090611     c                   if        NAD_mittente_in_testata = 'S'
122700090611     c                   eval         VABrmo = tes_VABrmo
122800090611     c                   eval         VABnmo = tes_VABnmo
122900090611     c                   eval         VABcmo = tes_VABcmo
123000090611     c                   end
123100090611      **
123200090611     c                   endSR
123300090611      * ?================================================================== */
123400090611     C*?  Riferimento Spedizione del cliente
123500090611      * ?================================================================== */
123600090611     C     DATI_RFF      BEGSR
123700090611      *
123800090609      *  Prepara una nuova spedizione
123900090609      * ?                         --------------
124000090609     c                   exsr      Nuova_spediz
124100090609      * ?                         --------------
124200090609     C                   clear                   WAGE             35
124300110224     C                   clear                   WFF              35
124400110224     C                   clear                   WCU              35
124500090609      *
124600090609      *  Trascodifico riferimento spedizione
124700090609     c                   if        RFF1153 = 'SEN' and RFF1154 <> *BLANKS
124800090609     C                   movel(p)  RFF1154       WAGE             35
124900090609     c                   end
125000090609      **
125100090609     C                   IF         WAGE <> *BLANKS
125200090609     C                   movel     WAGE          WNSP             35
125300090609     C                   movel     WAGE          WNUM             35
125400090609     C     ' '           scan      WAGE          WLUNGH            3 0
125500090609     C                   sub       1             WLUNGH
125600090609     C                   EXSR      TSTNUM
125700090609     C                   IF         WOKNUM = 'S'                                Ctrl numerico
125800090609     C                   movel     WAGE          VABRMA
125900090609     C                   IF         WRMN = 'SEN' or WRMN = *blanks
126000090609     C                   movel     WNUMOK        VABRMN
126100090609     C                   movel     'SEN'         WRMN
126200090609     C                   ENDIF
126300090609     C                   ENDIF
126400090609     C                   ENDIF
126500090609      *
126600090609      * Codice Cliente di riferimento
126700090609     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
126800090609     C                   movel     RFF1154       WFF
126900090609     c                   END
127000090609      *
127100090609      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
127200090609     C     WFF           IFNE      *BLANKS
127300090609     C                   eval      XX = 1
127400090609     C                   movel     §PTKSC        WCCL             35
127500090609     C                   movel     WFF           WCOM28           28
127600090609     C                   move      WCOM28        WCCL
127700090609     C     WCCL          lookup    CCL(XX)                                34
127800090609     C     *IN34         IFEQ      '1'
127900090609     C                   movel     DCL(XX)       EDIDSCL
128000090611     C* Se in testata
128100090611     c                   if        In_Testata       = 'S'
128200090611      *
128300090611     C                   z-add     §CLKSC        $CLKSC
128400090611     C                   movel     §CLTAR        $CLTAR
128500090611     C                   z-add     §CLLNP        $CLLNP
128600090611     C                   z-add     §CLNRS        $CLNRS
128700090611     C                   movel     §CLCTM        $CLCTM
128800090611     C                   movel     §CLBS1        $CLBS1
128900090611     C                   movel     §CLnsp        $CLfnsp
129000090611      *
129100090611     c                   else
129200090611      *
129300090609     C                   z-add     §CLKSC        WCLKSC            7 0
129400090609     C                   movel     §CLTAR        WCLTAR            3
129500090609     C                   z-add     §CLLNP        WCLLNP            3 0
129600090609     C                   z-add     §CLNRS        WCLNRS            2 0
129700090609     C                   movel     §CLCTM        WCLCTM            2
129800090609     C                   movel     §CLBS1        WCLBS1            1
129900090609     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
130000090611      *
130100090611     c                   end
130200090609      *
130300090609      * imposta la LNP   x il dettaglio specifco
130400090611      * imposta la Serie x il dettaglio specifco
130500090609     c                   eval      VABlnp = §CLlnp
130600090609     c                   eval      VABnrs = §CLnrs
130700090609      *
130800090609      *  In base al cod.trattamento merce reperisco tipo tratt.
130900090609      *    per un dettaglio specifico.
131000090609     C                   CLEAR                   DS1B
131100090609     C                   Z-ADD     1             Y                 3 0
131200090609     C     §CLctm        LOOKUP    CTM(Y)                                 32
131300090609     C     *IN32         IFEQ      '1'
131400090609     C                   MOVEL     DTM(Y)        DS1B
131500090609     C                   END
131600090609      *
131700090609      * Se il cliente non era sulla schiera lo aggiunge
131800090609     C     §CLKSC        lookup    KCL                                    39
131900090609     C                   if        *in39 = *off
132000090609     C                   add       1             WW
132100090609     C                   z-add     §CLKSC        KCL(WW)
132200090609     c                   endif
132300090609      *
132400090609     C                   ENDIF
132500090609     C                   ENDIF
132600090609      *
132700090611      * NON c'è l'identificativo bolla del cliente
132800090611     c                   if        (RFF1153 = 'SEN' and RFF1154 = *BLANKS)
132900090611     C                   eval      vinMSG = 'RFF riferimento cliente assente'
133000090609     c                   exsr      Error_DET
133100090609     c                   goto      end_RFF
133200090609     C                   ENDIF
133300090609      *
133400090609     c     end_RFF       endSR
133500090611      * ?================================================================== */
133600090611     C*?  Dati di dettaglio della spedizione e dei colli
133700090611      * ?================================================================== */
133800090611     C     DATI_GID      BEGSR
133900090611      *
134000090611     C* Il numero dei colli presenti nella spedizione
134100101109     C     GID706530     IFEQ      23
134200090611     C                   MOVEL     'S'           WGID99
134300101109     C                   z-add     GID722430     VABNCL
134400090611     C                   ELSE
134500090611     C     WGID99        IFEQ      'N'
134600101109     C                   z-add     gid722430     WTNCL             5 0
134700090611     C                   ADD       WTNCL         VABNCL
134800090611     C                   END
134900090611     C                   END
135000090611      *
135100090611     C                   ADD       VABNCL        WTOCOC           16 0
135200090611      *
135300090611     c     end_GID       endSR
135400090611      * ?================================================================== */
135500090611     C*?  Dati di dettaglio misure spedizione e colli
135600090611      * ?================================================================== */
135700090611     C     DATI_MSE      BEGSR
135800090611      *
135900090611     C*  Peso
136000090611     C     mse6799       IFEQ      'CGW'
136100090611      *
136200090611     C*  Controllo se ho valore totale o parziale
136300090611      *   sempre il Lordo
136400090611     C     mse6410       IFeq      'KG '
136500090611     C     WGID99        IFEQ      'S'
136600101109     C                   move      mse6314       mse6314_3        18 3          Peso
136700101109     C                   Z-ADD     mse6314_3     VABPKB                         Peso
136800090611     C                   Else
136900101109     C                   move      mse6314       mse6314_3                      Peso
137000101109     C                   ADD       mse6314_3     VABPKB                         Peso
137100090611     C                   End
137200090611     C                   ENDif
137300090615      *
137400090615     C                   END
137500090611      ***
137600090611     c     end_MSE       endSR
137700090209      * ?================================================================== */
137800090213     C*?  C.O.D. Monetary Amount
137900090209      * ?================================================================== */
138000090609     C     DATI_VAL      BEGSR
138100090210      *
138200090216     C* Controllo se campo importo numerico
138300090216      * con 3 decimali
138400090611     C     val5004       IFNE      *zeros
138500090216      *
138600090611     C                   If         val5297 = 'DE '
138700090630     C     val5004       div       100           VABIAS                         IMPORTO
138800090611     C                   MOVEL     val6345       VABVAS                         ASSICURATO
138900090216     C                   END
139000090216      *
139100090611     C                   If         §PUCAS = 'S' and val5297 = 'RE '            C/Assegno
139200090630     C     val5004       div       100           VABCAS
139300090611     C                   MOVEL     val6345       VABVCA
139400090611      *
139500090216     C                   END
139600090216      *
139700090611     C                   END
139800090213      *
139900090611     c     end_VAL       endSR
140000091104      * ?================================================================== */
140100091104     C*?  Contatto a cui far riferimento
140200091104      * ?================================================================== */
140300091104     C     DATI_CTA      BEGSR
140400091104      *
140500091104      *  memorizza il riferimento di consegna x il destinatario
140600091104      *  memorizza il riferimento di consegna nr.telefonico
140700091104     C     cta3412       ifne      *BLANKS
140800091104     c                   movel     cta3412       watrde
140900091104     c                   end
141000091104      *
141100091104     c                   endSR
141200091104      * ?================================================================== */
141300091104     C*?  Contatto telefonico  a cui far riferimento
141400091104      * ?================================================================== */
141500091104     C     DATI_COM      BEGSR
141600091104      *
141700091104     C     com3148       ifne      *BLANKS
141800091104     c                   movel     com3148       wattel
141900091104     c                   end
142000091104      *
142100091104     C                   ENDSR
142200090213      * ?================================================================== */
142300090213     C*?  FREE TEXT
142400090213      * ?================================================================== */
142500090611     C     DATI_TXT      BEGSR
142600090213      *
142700090611      * ? solo se si tratta di note di consegna
142800100311     C*******            if        txt0077 ='DEL'  and
142900100311     C*******                      txt0078 <> *blank
143000090629      *
143100100311     C                   if        txt0077 ='DEL'
143200100311      *
143300090702     c                   movel     txt0078       tipo_payment      2
143400090629      *
143500090629     c                   If         tipo_payment = 'TM'
143600100311     C                               and  %subst(txt0078:3:1)=' '
143700090629     C                   MOVEL     'TM'          VABTIC
143800090629      *
143900090629     c                   elseIf     tipo_payment = 'BM'
144000100311     C                               and  %subst(txt0078:3:1)=' '
144100090629     C                   MOVEL     'BM'          VABTIC
144200090629      *
144300090629     c                   elseIf     tipo_payment = 'CM'
144400100311     C                               and  %subst(txt0078:3:1)=' '
144500090629     C                   MOVEL     'CM'          VABTIC
144600090629      *
144700090629     c                   elseIf     tipo_payment = '  '
144800100311     C                               and  txt0078 = *BLANK
144900090629     C                   MOVEL     '  '          VABTIC
145000090629      *
145100100311     c                   elseIF     txt0078 <> *BLANK
145200090629      *
145300090611     C     WNOT1         IFEQ      *BLANKS
145400090702     C                   MOVEL     txt0078       WNOT1
145500090702     C                   MOVE      txt0078       WNOT2
145600090611     C                   ELSE
145700090611     C     WNOT2         IFEQ      *BLANKS
145800090702     C                   MOVEL     txt0078       WNOT2
145900090611     C                   END
146000090611     C                   END
146100090629      *
146200090629     c                   endIf
146300090629      *
146400090629     c                   end
146500090213      *
146600090611     c     end_TXT       endSR
146700090210      * ?================================================================== */
146800090210     C*?  Dati di dettaglio Codice a barre Segnacolli
146900090210      * ?================================================================== */
147000090210     C     DATI_PCI      BEGSR
147100090210      *
147200090611      *  Contiene i singoli segnacolli
147300090611     c                   if        PCI4233 ='23 '
147400090611      *
147500090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
147600090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
147700090210     C     §1BF12        IFEQ      'E'
147800090210     C                   EXSR      SEGNEE
147900090210     C                   ELSE
148000090210      *
148100090210      *  La prima volta controllo congruità numero segnacollo
148200090210     C     §PUBS1        IFNE      *BLANKS
148300090210     C     VABnrs        ANDNE     0
148400090210      *  Se il codice trattamento merce prevede che segnacollino i
148500090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
148600090210     C     §1BFG7        IFEQ      'S'
148700090210     C     §1BFG1        OREQ      'S'
148800090210      *  calcolo segnacollo
148900090210     C                   EXSR      SGNELA
149000090210     C                   END
149100090210     C                   END
149200090210      *
149300090210     C                   END
149400090210      *
149500090611     C                   ENDif
149600090611      *
149700090210     c     end_PCI       endSR
149800090211     C*----------------------------------------------------------------
149900090211      *? dati finali del dettaglio    UNT
150000090211     C*----------------------------------------------------------------
150100090211     C     DATI_UNT      BEGSR
150200090211      *
150300090213     c                   eval      NUM_RECord = vnREC
150400090213     c                   z-add     vnrec         last_RECord
150500090211      **
150600090211     c                   endSR
150700090211     C*----------------------------------------------------------------
150800090211      *? dati finali     UNT
150900090211     C*----------------------------------------------------------------
151000090211     C     DATI_UNZ      BEGSR
151100090211      *
151200090211      **
151300090211     c                   endSR
151400090210      *----------------------------------------------------------------
151500090611      *? Input segnacolli su schiera di 10 elementi
151600090210      *----------------------------------------------------------------
151700090210     C     INP_Segnacol  BEGSR
151800090210      *
151900090210     c                   clear                   quanti_PCI        3 0
152000090210     c                   if        §puEL1 = 0
152100090210     c                   z-add     10            quanti_PCI
152200090210     c                   else
152300090210     c                   z-add     §puEL1        quanti_PCI
152400090210     c                   end
152500090210      *
152600090210      *  Pulisce la schiera dei segnacolli da elaborare
152700090210     c                   clear                   x30
152800090210     c                   z-add     0             Nr_x30            3 0
152900090210      *
153000090210      *   riporto tutto sulla schiera generica X30
153100090210      *
153200090210     c                   do        10            limite            3 0
153300090210      * Limita quanti elementi passati in ogni PCI
153400090210      *   devono essere considerati
153500090210     c                   if        limite > quanti_PCI
153600090210     c                   Leave
153700090210     c                   end
153800090210      *
153900090210      *  Carica la schiera generale di tutti i segnacolli
154000090210     c                   if        D30_1(limite) <> *blank
154100090210     c                   add       1             Nr_x30
154200090210     c                   eval      X30(Nr_x30) = D30_1(limite)
154300090210     c                   end
154400090210     c                   enddo
154500090210      *
154600090210     C                   ENDSR
154700090210      *----------------------------------------------------------------
154800090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
154900090210      *----------------------------------------------------------------
155000090210     C     SEGNEE        BEGSR
155100090210      *
155200090210     c                   exsr      INP_Segnacol
155300090210      *
155400090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
155500090210     C                   DO        10            x                              --- 01 ---
155600090210      *
155700090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
155800090210     C     x30(X)        ANDNE     *LOVAL
155900090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
156000090210     C                   ADD       1             §§
156100090210     C                   MOVEL     x30(X)        SGE(§§)
156200090210     C                   END                                                    --- 03 ---
156300090210      *
156400090210     C                   END                                                    --- 02 ---
156500090210      *
156600090210     C                   ENDSR
156700090210     C*----------------------------------------------------------------
156800090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
156900090210     C*----------------------------------------------------------------
157000090210     C     SGNELA        BEGSR
157100090210      *
157200090210     c                   exsr      INP_Segnacol
157300090210     C*
157400090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
157500090210     C*  in base al numero elementi validi carico sgn.
157600090210     C                   DO        10            X                              --- 01 ---
157700090210     C*
157800090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
157900090210     C     x30(X)        ANDNE     *LOVAL
158000090210     C*
158100090210     C                   CLEAR                   DSSGN
158200090210     C*  Controllo se devo ricevere la linea di partenza
158300090210     C                   MOVEL     'N'           WERRO             1
158400090210     C     §PULP1        IFGT      0                                            --- 03 ---
158500090210     C                   EXSR      REPLNP
158600090210     C                   END                                                    --- 03 ---
158700090210     C*  Controllo se devo ricevere la linea di arrivo
158800090210     C     §PULA1        IFGT      0                                            --- 03 ---
158900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
159000090210     C                   EXSR      REPLNA
159100090210     C                   END                                                    --- 03 ---
159200090210     C*  Controllo se devo ricevere il numero di serie
159300090210     C     §PUNR1        IFGT      0                                            --- 03 ---
159400090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
159500090210     C                   EXSR      REPNRS
159600090210     C                   END                                                    --- 03 ---
159700090210     C*  Controllo se devo ricevere il numero segnacollo
159800090210     C     §PUSG1        IFGT      0                                            --- 03 ---
159900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
160000090210     C                   EXSR      REPSGN
160100090210     C                   END                                                    --- 03 ---
160200090210     C*  Controllo se devo ricevere la zona di consegna
160300090210     C     §PUZN1        IFGT      0                                            --- 03 ---
160400090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
160500090210     C                   EXSR      REPZNC
160600090210     C                   END                                                    --- 03 ---
160700090210     C*  Imposto da VAB i dati a zero se mi è stato passato
160800090210     C*  numero segnacollo valido
160900090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
161000090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
161100090210     C                   Z-ADD     VABLNP        SGNLNP
161200090210     C                   END                                                    --- 04 ---
161300090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
161400090210     C                   Z-ADD     VABLNA        SGNLNA
161500090210     C                   END                                                    --- 04 ---
161600090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
161700090210     C                   Z-ADD     VABNRS        SGNNRS
161800090210     C                   END                                                    --- 04 ---
161900090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
162000090210     C                   Z-ADD     VABZNC        SGNZNC
162100090210     C                   END                                                    --- 04 ---
162200090210      *
162300090210     C*  mi reimposto LNA e zona da segnacollo
162400090210     C                   Z-ADD     SGNLNA        VABLNA
162500090210     C                   Z-ADD     SGNZNC        VABZNC
162600090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
162700090210     C                   ADD       1             XY
162800090210     C                   MOVE      SGNNSC        SGN(XY)
162900090210     C                   END                                                    --- 03 ---
163000090210     C*
163100090210     C                   END                                                    --- 02 ---
163200090210     C*
163300090210     C                   END                                                    --- 01 ---
163400090210      *
163500090210     C                   ENDSR
163600090210     C*----------------------------------------------------------------
163700090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
163800090210     C*----------------------------------------------------------------
163900090210     C     REPLNP        BEGSR
164000090210     C*
164100090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
164200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
164300090210     C*  dal numero segnacollo passatomi dal partner
164400090210     C                   Z-ADD     §PULP1        WP                2 0
164500090210     C     3             SUBST     x30(X):WP     WLNP              3
164600090210     C*
164700090210     C*  controllo che la lnp calcolata sia numerica
164800090210     C                   TESTN                   WLNP                 98
164900090210     C*
165000090210     C*  se è numerica la carico
165100090210     C     *IN98         IFEQ      '1'
165200090210     C                   MOVE      WLNP          SGNLNP
165300090210     C                   ELSE
165400090210     C*  se non è numerica stampo errore
165500090210     C                   MOVEL     'S'           WERRO             1
165600090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
165700090213     c                   exsr      Error_DET
165800090210     C                   goto      end_LNP
165900090210     C                   END
166000090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
166100090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
166200090210     C     SGNLNP        IFNE      VABLNP
166300090210     C                   MOVEL     'S'           WERRO             1
166400090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
166500090210     c                             a della Bolla'
166600090213     c                   exsr      Error_DET
166700090210     C                   goto      end_LNP
166800090210     C                   END
166900090210     C*
167000090210     C     end_LNP       ENDSR
167100090210     C*----------------------------------------------------------------
167200090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
167300090210     C*----------------------------------------------------------------
167400090210     C     REPLNA        BEGSR
167500090210     C*
167600090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
167700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
167800090210     C*  dal numero segnacollo passatomi dal partner
167900090210     C                   Z-ADD     §PULA1        WP                2 0
168000090210     C     3             SUBST     x30(X):WP     WLNA              3
168100090210     C*  controllo che la lna calcolata sia numerica
168200090210     C                   TESTN                   WLNA                 98
168300090210     C*  se è numerica la carico
168400090210     C     *IN98         IFEQ      '1'
168500090210     C                   MOVE      WLNA          SGNLNA
168600090210     C                   ELSE
168700090210     C*  se non è numerica stampo errore
168800090210     C                   MOVEL     'S'           WERRO             1
168900090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
169000090213     c                   exsr      Error_DET
169100090210     C                   goto      end_LNA
169200090210     C                   END
169300090210     C*
169400090210     C     end_LNA       ENDSR
169500090210     C*----------------------------------------------------------------
169600090210     C*? REPNRS - Reperisce numero serie
169700090210     C*----------------------------------------------------------------
169800090210     C     REPNRS        BEGSR
169900090210     C*
170000090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
170100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
170200090210     C*  dal numero segnacollo passatomi dal partner
170300090210     C                   Z-ADD     §PUNR1        WP                2 0
170400090210     C     2             SUBST     x30(X):WP     WNRS              2
170500090210     C*  controllo che il nrs calcolata sia numerico
170600090210     C                   TESTN                   WNRS                 98
170700090210     C*  se è numerica la carico
170800090210     C     *IN98         IFEQ      '1'
170900090210     C                   MOVE      WNRS          SGNNRS
171000090210     C                   ELSE
171100090210     C*  se non è numerico stampo errore
171200090210     C                   MOVEL     'S'           WERRO             1
171300090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
171400090213     c                   exsr      Error_DET
171500090210     C                   goto      end_NRS
171600090210     C                   END
171700090210      *
171800090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
171900090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
172000090210     C     SGNNRS        IFNE      VABNRS
172100090210     C                   MOVEL     'S'           WERRO             1
172200090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
172300090210     c                             o della Bolla'
172400090213     c                   exsr      Error_DET
172500090210     C                   goto      end_NRS
172600090210     C                   END
172700090210     C*
172800090210     C     end_NRS       ENDSR
172900090210     C*----------------------------------------------------------------
173000090210     C*? REPZNC - Reperisce zona consegna
173100090210     C*----------------------------------------------------------------
173200090210     C     REPZNC        BEGSR
173300090210     C*
173400090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
173500090210     C*  (a partire dalla posizione iniziale indicata in tabella)
173600090210     C*  dal numero segnacollo passatomi dal partner
173700090210     C                   Z-ADD     §PUZN1        WP                2 0
173800090210     C     2             SUBST     x30(X):WP     WZNC              2
173900090210     C*  controllo che la zona calcolata sia numerica
174000090210     C                   TESTN                   WZNC                 98
174100090210     C*  se è numerica la carico
174200090210     C     *IN98         IFEQ      '1'
174300090210     C                   MOVE      WZNC          SGNZNC
174400090210     C                   ELSE
174500090210     C*  se non è numerica stampo errore
174600090210     C                   MOVEL     'S'           WERRO             1
174700090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
174800090213     c                   exsr      Error_DET
174900090210     C                   goto      end_ZNC
175000090210     C                   END
175100090210     C*
175200090210     C     end_ZNC       ENDSR
175300090210     C*----------------------------------------------------------------
175400090210     C*? REPSGN - Reperisce numero segnacollo
175500090210     C*----------------------------------------------------------------
175600090210     C     REPSGN        BEGSR
175700090210     C*
175800090210     C*  se viene passata nr.segnacollo estraggo sottostringa
175900090210     C*  lunga 7 chr (a partire dalla posizione iniziale
176000090210     C*  indicata in tabella)
176100090210     C*  dal numero segnacollo passatomi dal partner
176200090210     C                   Z-ADD     §PUSG1        WP                2 0
176300090210     C     7             SUBST     x30(X):WP     WSGN              7
176400090210     C*  controllo che il nr.segnacollo calcolato sia numerico
176500090210     C                   TESTN                   WSGN                 98
176600090210     C*  se è numerico lo carico
176700090210     C     *IN98         IFEQ      '1'
176800090210     C                   MOVE      WSGN          SGNNSC
176900090210     C                   ELSE
177000090210     C*  se non è numerica stampo errore
177100090210     C                   MOVEL     'S'           WERRO             1
177200090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
177300090213     c                   exsr      Error_DET
177400090210     C                   goto      end_SGN
177500090210     C                   END
177600090210     C*
177700090210     C     end_SGN       ENDSR
177800090611     C*----------------------------------------------------------------
177900090611     C*?   Inizio Messaggio
178000090611     C*----------------------------------------------------------------
178100051205     C     *INZSR        BEGSR
178200051205      *
178300991129     c     *ENTRY        PLIST
178400060613     C                   parm                    esito             1
178500971215      *
178600050112     C     KTABel        KLIST
178700050112     C                   KFLD                    TBLKUT
178800050112     C                   KFLD                    TBLCOD
178900050112     C                   KFLD                    TBLKEY
179000090210      *
179100090210     C     K_TAB1L       KLIST
179200090213     C                   KFLD                    etbUNB
179300090213     C                   KFLD                    etbSGM
179400090213     C                   KFLD                    etbVALSGM
179500090209     C*
179600090209     C* Definisco chiavi di accesso
179700090209     C     KTAB1         KLIST
179800090209     C                   KFLD                    KKUT
179900090209     C                   KFLD                    KCOD
180000090209     C*
180100090209     C     KNUF          KLIST
180200090209     C                   KFLD                    KAAA
180300090209     C                   KFLD                    KCNU
180400090209     C                   KFLD                    KFIL
180500090209      *
180600090209     C     KVAB1         KLIST
180700090209     C                   KFLD                    KCCM
180800090209     C                   KFLD                    KCMR
180900090209     C                   KFLD                    KCNT
181000090209     C                   KFLD                    KAAS
181100090209     C                   KFLD                    KLNP
181200090209     C                   KFLD                    KNRS
181300090209     C                   KFLD                    KNSP
181400090209      *
181500090209     C     KVAB2         KLIST
181600090209     C                   KFLD                    KCCM
181700090209     C                   KFLD                    KCMR
181800090209      *
181900090209     C     KRDE          KLIST
182000090209     C                   KFLD                    KAAS
182100090209     C                   KFLD                    KLNP1
182200090209     C                   KFLD                    KNRS
182300090209     C                   KFLD                    KNSP
182400090209     C                   KFLD                    KNMC
182500090209     C                   KFLD                    KNRP
182600090209      *
182700090209     C     KACO          KLIST
182800090209     C                   KFLD                    KKUT
182900090209     C                   KFLD                    KKCC
183000090209     C                   KFLD                    KKSC
183100090209      *
183200090209     C     KIND          KLIST
183300090209     C                   KFLD                    KKUT
183400090209     C                   KFLD                    KKCC
183500090209     C                   KFLD                    KKSC
183600090209      *
183700090209      * DETERMINO SE SONO BARTOLINI O SDI
183800090209     C                   CLEAR                   TIBS55DS
183900090209     C                   MOVEL     'L'           I50TLA
184000090209     C                   CALL      'TIBS55R'
184100090209     C                   PARM                    TIBS55DS
184200090209      *
184300090209     C* RECUPERO DATI DELL'UTENTE
184400090209     C                   Z-ADD     1             CODUT             1 0
184500090209     C                   CALL      'XPARUT'
184600090209     C                   PARM                    UTEDSE0F
184700090209     C                   MOVEL     RAGUT         RSUT             20
184800090209     C*
184900090209     C* RICERCA CAPOCONTI
185000090209     C                   DO        50            X
185100090209     C                   MOVE      TCU(X)        TCUDS
185200090209     C     F56           IFEQ      'CG'
185300090209     C     F34           ANDEQ     '01'
185400090209     C                   Z-ADD     KCU(X)        KCI               4 0
185500090209     C                   END
185600090209     C                   END
185700090209     C*
185800090211     c                   movel(p)  'EDPAB'       User_Msg         10
185900090211     C*
186000090209     C* IMPOSTO A ZERO VARIABILI DI PGM
186100090209     c                   move      *blank        Snd_Msg_alert     1
186200090209     C                   MOVEL     CA(1)         WCA              78
186300090209     C                   MOVEL     CN(1)         WCN              78
186400090209     C*
186500090209     C* RECUPERO DATA E ORA
186600090209     C                   TIME                    WHHDAT           14 0
186700090209     C                   MOVE      WHHDAT        G02DAT
186800090209     C                   MOVE      WHHDAT        WDATA8            8 0
186900090209     C                   MOVE      WDATA8        WAA2              2 0
187000090209     C                   MOVEL     WDATA8        DATA              6 0
187100090209     C                   MOVE      WAA2          DATA
187200090209     C                   MOVEL     *BLANK        G02ERR
187300090209     C                   CALL      'XSRDA8'
187400090209     C                   PARM                    WLBDA8
187500090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
187600090209     C                   TIME                    WORA              6 0
187700090209      * Recupero data e ora
187800090209     C                   TIME                    W0140
187900090209      * UDATE IN GGMMAAAA
188000090209     C                   MOVE      W0140         WDTGIO
188100090209      * UDATE IN AAAAMMGG
188200090209     C     *eur          MOVEL     WDTGIO        DATA_eur
188300090209     C     *iso          MOVEL     DATA_eur      dateu
188400090209      *
188500090209      * ?              /*--------------------------- */
188600090209     c                   exsr      CARICA_TABELLE
188700090209      * ?              /*--------------------------- */
188800090209      *
188900090209     C                   MOVEL     *ALL'-'       CMP132          132
189000991124      *
189100991124     C                   ENDSR
189200060621      * ?------------------------------------------------------------------ */
189300090209      *?    CARICA TABELLE SU SCHIERE
189400060621      * ?------------------------------------------------------------------ */
189500090209     C     CARICA_TABELLEBEGSR
189600090205      *
189700090209     C* Caricamento Tabella 1B
189800090209     C                   Z-ADD     0             X                 4 0
189900090209     C                   Z-ADD     CODUT         KKUT
190000090209     C                   MOVEL     '1B'          KCOD
190100090209     C     KTAB1         CHAIN     TABEL00F                           30
190200090209     C     *IN30         DOWEQ     '0'
190300090209     C     TBLFLG        IFEQ      *BLANKS
190400090209     C                   ADD       1             X
190500090209     C                   MOVEL     TBLKEY        CTM(X)
190600090209     C                   MOVEL     TBLUNI        DTM(X)
190700090209     C                   END
190800090209     C     KTAB1         READE     TABEL00F                               30
190900090209     C                   END
191000090209      *
191100090209     C* Caricamento Tabella 15
191200090209     C                   Z-ADD     0             X                 4 0
191300090209     C                   Z-ADD     CODUT         KKUT
191400090209     C                   MOVEL     '15'          KCOD
191500090209     C     KTAB1         CHAIN     TABEL00F                           30
191600090209     C     *IN30         DOWEQ     '0'
191700090209     C     TBLFLG        IFEQ      *BLANKS
191800090209     C                   ADD       1             X
191900090209     C                   MOVEL     TBLKEY        C15(X)
192000090209     C                   MOVEL     TBLUNI        DS15
192100090611     C                   MOVEL     §15ISO        ISO(X)
192200090209     C                   MOVEL     §15ECF        CPF(X)
192300090209     C                   MOVE      §15PCF        CPF(X)
192400090209     C                   END
192500090209     C     KTAB1         READE     TABEL00F                               30
192600090209     C                   END
192700090209      *
192800090209     C* Caricamento Tabella CV
192900090209     C                   Z-ADD     0             X
193000090209     C                   Z-ADD     CODUT         KKUT
193100090209     C                   MOVEL     'CV'          KCOD
193200090209     C     KTAB1         CHAIN     TABEL00F                           30
193300090209     C     *IN30         DOWEQ     '0'
193400090209     C     TBLFLG        IFEQ      *BLANKS
193500090209     C                   ADD       1             X
193600090209     C                   MOVEL     TBLKEY        CCV(X)
193700090209     C                   MOVEL     TBLUNI        DCV(X)
193800090209     C                   END
193900090209     C     KTAB1         READE     TABEL00F                               30
194000090209     C                   END
194100090216      *
194200090216     C* Caricamento Tabella Tipo Incasso C/Assegno
194300090216     C                   Z-ADD     0             X
194400090216     C                   MOVEL     'TC'          TABCOD
194500090216     C     TABCOD        CHAIN     EDTAB01L                           30
194600090216     C     *IN30         DOWEQ     '0'
194700090216     C     TABFLG        IFEQ      *BLANKS
194800090216     C                   ADD       1             X
194900090216     C                   MOVEL     TABKEY        CTC35(X)
195000090216     C                   MOVEL     TABUNI        TPI(X)
195100090216     C                   END
195200090216     C     TABCOD        READE     EDTAB01L                               30
195300090216     C                   END
195400090209      *
195500090209      * Caricamento Tabella Partner esteri
195600090209     C                   Z-ADD     0             X
195700090209     C                   MOVEL     'PT'          TABCOD
195800090209     C     TABCOD        CHAIN     EDTAB01L                           30
195900090209     C     *IN30         DOWEQ     '0'
196000090209      *
196100090209     C                   SELECT
196200090209     C     TABFLG        WHENNE    *BLANKS
196300090209      *
196400090209     C                   OTHER
196500090209     C                   ADD       1             X
196600090209     C                   MOVEL     TABKEY        CPT(X)
196700090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
196800090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
196900090209     C                   MOVEL     TABUNI        DPT(X)
197000090209     C                   ENDSL
197100090209      *
197200090209     C     TABCOD        READE     EDTAB01L                               30
197300090209     C                   END
197400121105      *
197500121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
197600121105     c                   exsr      ChecK_PT
197700121105      *
197800090209      * salva quanti Codici UNB ha caricato
197900090209     c                   z-add     x             sav_CPTx
198000090209      *
198100090209     C                   MOVEL     'PU'          TABCOD
198200090209     C     TABCOD        CHAIN     EDTAB01L                           30
198300090209     C     *IN30         DOWEQ     '0'
198400090209      *
198500090209     C                   SELECT
198600090209     C     TABFLG        WHENNE    *BLANKS
198700090209      *
198800090209     C                   OTHER
198900090209     C                   MOVEL     TABKEY        CODPU            35
199000090209     C                   Z-ADD     1             X
199100090209     C     CODPU         LOOKUP    CPT(X)                                 32
199200090209     C   32              MOVEL     TABUNI        DPU(X)
199300090209     C                   ENDSL
199400090209      *
199500090209     C     TABCOD        READE     EDTAB01L                               30
199600090209     C                   END
199700090209      *
199800090209      * Prosegue tab.PT e PU
199900090209     C                   MOVEL     'PV'          TABCOD
200000090209     C     TABCOD        CHAIN     EDTAB01L                           30
200100090209     C     *IN30         DOWEQ     '0'
200200090209      *
200300090209     C                   SELECT
200400090209     C     TABFLG        WHENNE    *BLANKS
200500090209      *
200600090209     C                   OTHER
200700090209     C                   MOVEL     TABKEY        CODPV            35
200800090209     C                   Z-ADD     1             X
200900090209     C     CODPV         LOOKUP    CPT(X)                                 32
201000090209     C   32              MOVEL     TABUNI        DPV(X)
201100090209     C                   ENDSL
201200090209      *
201300090209     C     TABCOD        READE     EDTAB01L                               30
201400090209     C                   END
201500090209      *
201600090209     C* Caricamento Tabella codici clienti - tariffe
201700090209     C                   Z-ADD     0             X
201800090209     C                   MOVEL     'CL'          TABCOD
201900090209     C     TABCOD        CHAIN     EDTAB01L                           30
202000090209     C     *IN30         DOWEQ     '0'
202100090209     C     TABFLG        IFEQ      *BLANKS
202200090209     C                   ADD       1             X
202300090209     C                   MOVEL     TABKEY        CCL(X)
202400090209     C                   MOVEL     TABUNI        DCL(X)
202500090209     C                   END
202600090209     C     TABCOD        READE     EDTAB01L                               30
202700090209     C                   END
202800090209      *
202900090209     C* Caricamento Serivizi speciali
203000090209     C                   Z-ADD     0             X
203100090209     C                   MOVEL     'SS'          TABCOD
203200090209     C     TABCOD        CHAIN     EDTAB01L                           30
203300090209     C     *IN30         DOWEQ     '0'
203400090209     C     TABFLG        IFEQ      *BLANKS
203500090209     C                   ADD       1             X
203600090209     C                   MOVEL     TABKEY        SS(X)
203700090209     C                   MOVEL     TABKEY        SS35(X)
203800090209     C                   MOVEL     TABUNI        DSS(X)
203900090209     C                   END
204000090209     C     TABCOD        READE     EDTAB01L                               30
204100090209     C                   END
204200090209      *
204300090209     C                   ENDSR
204400090209      * ?------------------------------------------------------------------ */
204500090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
204600090209      * ?------------------------------------------------------------------ */
204700090209     C     MSG_Errato    BEGSR
204800090209      *
204900090205      * ?  Scrive su tutti i records il tipo di errore
205000090205     c     *start        setll     tivin00r
205100090205     c                   read      tivin00r
205200090205     c                   dow       not %eof(tivin00r)
205300090211     c                   if        vinMSG  = *blank
205400090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
205500090210     C                               >> Controllare i DATI su UPLOAD !!'
205600090211     c                   end
205700090205     C                   eval      vinFLG = '2'
205800090213     c                   eval      Almeno_Un_Due ='S'
205900090205     c                   eval      esito  = '2'
206000090209     c                   eval      se_errore ='S'
206100090213     c                   eval      err_bloccante ='S'
206200090205     c                   update    tivin000
206300090205     c                   read      tivin00r
206400090205     c                   endDO
206500090205      *
206600090205     C                   ENDSR
206700090213      * ?------------------------------------------------------------------ */
206800090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
206900090213      * ?------------------------------------------------------------------ */
207000090213     C     DETta_Errato  BEGSR
207100090213      *
207200090213      *  rilascia il record prima di rieseguire il ciclo
207300090213     c                   update    tivin000
207400090213      *
207500090213      * ?  Scrive su tutti i records il tipo di errore
207600090213     c     SavNUM_RECord setll     tivin00r
207700090213     c                   read      tivin00r
207800090213     c                   dow       not %eof(tivin00r)
207900090213      *
208000090213      *
208100090213     c                   if        vinMSG  = *blank
208200090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
208300090213     C                             >> Controllare i DATI di ogni Segmento <<'
208400090213     c                   end
208500090213     c                   exsr      Error_DET
208600090213      *
208700090213     c                   if        VNREC = NUM_RECord
208800090213     c                   leave
208900090213     c                   end
209000090213      *
209100090213     c                   update    tivin000
209200090213     c                   read      tivin00r
209300090213      *
209400090213     c                   endDO
209500090603      *
209600101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
209700090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
209800101108     C                             Bolle PERMA  non totalmente tradotto. -
209900101109     C                             Controllare UPLOAD del cliente '  +
210000101109     c                             %editc(wksc:'Z')
210100090603     c                   exsr      invio_mail_AB
210200090603      *
210300090213      *
210400090213     C                   ENDSR
210500090205      * ?------------------------------------------------------------------ */
210600090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
210700090205      * ?------------------------------------------------------------------ */
210800090205     C     *pssr         BEGSR
210900090205     C
211000060710     C                   eval      esito = '2'
211100101108     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import PERMA '
211200090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
211300090603     C                             Bolle import in filiale - messaggio -
211400090603     C                             EDI ricevuto errato su UPLOAD.'
211500090603     c                   exsr      invio_mail_AB
211600060621     C
211700090213     C                   ENDSR
211800090213      * ?------------------------------------------------------------------ */
211900090213      *?      Segnala Errore su TIVIN
212000090213      * ?------------------------------------------------------------------ */
212100090213     C     Error_DET     BEGSR
212200090213     C
212300090213     C                   eval      vinFLG = '2'
212400090213     c                   eval      Almeno_Un_Due ='S'
212500090213     c                   eval      esito  = '1'
212600090213     c                   eval      se_errore ='S'
212700090213     C                   eval      Err_in_detta = 'S'
212800090213     C
212900090213     C                   ENDSR
213000090209      * ?------------------------------------------------------------------ */
213100090209      *? Controlla la trasmissione   se completa
213200090209      * ?------------------------------------------------------------------ */
213300090209     c     Check_Trasm   Begsr
213400090209
213500090209     C                   clear                   se_errore
213600090209     C                   clear                   Riga_iNIZIo       1
213700090209     C                   clear                   Riga_fINe         1
213800090209      ** primo  record
213900090209     c     *start        setll     tivin00r
214000090209      *
214100090209     c     rileggi       tag
214200090209     c                   read      tivin00r
214300090209      *
214400090209     c                   if        not %eof(tivin00r)
214500090209      *
214600090209     c                   movel(p)  VINDTA        dati
214700090209     c                   if        dati = *blank
214800090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
214900090209      * le leggo e le escludo.
215000090209     C                   eval      vinFLG = '1'
215100090209     c                   update    Tivin000
215200090209     c                   goto      rileggi
215300090209     c                   end
215400090209      * ?              /*---------------------- */
215500090210     c                   exsr      Decod_Segmento
215600090209      * ?              /*---------------------- */
215700090209     c                   endif
215800090209      **
215900090209      ** ultimo record
216000090209     c     *hival        setll     tivin00r
216100090209     c     leggiAncora   tag
216200090209     c                   readp     tivin00r
216300090209     c                   if        not %eof(tivin00r)
216400090209      *
216500090209     c                   movel(p)  VINDTA        dati
216600090209     c                   if        dati = *blank
216700090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
216800090209      * le leggo e le escludo.
216900090209     C                   eval      vinFLG = '1'
217000090209     c                   update    Tivin000
217100090209     c                   goto      leggiAncora
217200090209     c                   end
217300090209      * ?              /*---------------------- */
217400090210     c                   exsr      Decod_Segmento
217500090209      * ?              /*---------------------- */
217600090209     c                   endif
217700090209
217800090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
217900090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
218000090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
218100090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
218200090209      * ?-----> Errore
218300090209     C                   eval      se_errore = 'S'
218400090209     c                   end
218500090209
218600090209     c                   endSR
218700090209      * ?------------------------------------------------------------------ */
218800090209      *?    Decodifica il segmento ed importa i campi in formato DS
218900090209      * ?------------------------------------------------------------------ */
219000090209     c     Decod_SegmentoBegsr
219100090209      **
219200090209     c                   eval      segmento = vinDTA
219300090210      **--------
219400100716     C                   eval       keyUNBCLI    = *blank
219500100716     C                   eval       keyTIPOMSG   = 'DISPOR'
219600100716     C                   eval       keyVERSION   = '2'
219700100716     C                   eval       keyRELEASE   = '3'
219800100716     C                   eval       keyAGENCY    = 'GT'
219900100716     C                   eval       keyASSOCIA   = 'GTF'
220000100716      **--------
220100090211     c                   select
220200090609      * --------
220300090609     c                   when      tipo_seg = 'BGM'
220400090609      *
220500090609     c                   call      'TRTCT05BGM'
220600090609     c                   parm                    tipo_seg
220700090609     c                   parm                    segmento
220800100716     C                   parm                    keyUNBCLI
220900100716     C                   parm                    keyTIPOMSG
221000100716     C                   parm                    keyVERSION
221100100716     C                   parm                    keyRELEASE
221200100716     C                   parm                    keyAGENCY
221300100716     C                   parm                    keyASSOCIA
221400090609     c                   parm                    Errore
221500090609     c                   parm                    EDPORBGM
221600090609     c                   parm                    Valori_inErr
221700090609     c                   parm                    Descr_Errore
221800090609      *
221900090609     C                   EXSR      DATI_BGM
222000090609      **--------
222100090609     c                   when      tipo_seg = 'CAG'
222200090609      *
222300090609     c                   call      'TRTCT05CAG'
222400090609     c                   parm                    tipo_seg
222500090609     c                   parm                    segmento
222600100716     C                   parm                    keyUNBCLI
222700100716     C                   parm                    keyTIPOMSG
222800100716     C                   parm                    keyVERSION
222900100716     C                   parm                    keyRELEASE
223000100716     C                   parm                    keyAGENCY
223100100716     C                   parm                    keyASSOCIA
223200090609     c                   parm                    Errore
223300090609     c                   parm                    EDPORCAG
223400090609     c                   parm                    Valori_inErr
223500090609     c                   parm                    Descr_Errore
223600090609      *
223700090609     C                   EXSR      DATI_CAG
223800090609      **--------
223900090609     c                   when      tipo_seg = 'CTA'
224000090609      *
224100090609     c                   call      'TRTCT05CTA'
224200090609     c                   parm                    tipo_seg
224300090609     c                   parm                    segmento
224400100716     C                   parm                    keyUNBCLI
224500100716     C                   parm                    keyTIPOMSG
224600100716     C                   parm                    keyVERSION
224700100716     C                   parm                    keyRELEASE
224800100716     C                   parm                    keyAGENCY
224900100716     C                   parm                    keyASSOCIA
225000090609     c                   parm                    Errore
225100090609     c                   parm                    EDPORCTA
225200090609     c                   parm                    Valori_inErr
225300090609     c                   parm                    Descr_Errore
225400090609      *
225500090609     C                   EXSR      DATI_CTA
225600091104      **--------
225700091104     c                   when      tipo_seg = 'COM'
225800091104      *
225900091104     c                   call      'TRTCT05COM'
226000091104     c                   parm                    tipo_seg
226100091104     c                   parm                    segmento
226200100716     C                   parm                    keyUNBCLI
226300100716     C                   parm                    keyTIPOMSG
226400100716     C                   parm                    keyVERSION
226500100716     C                   parm                    keyRELEASE
226600100716     C                   parm                    keyAGENCY
226700100716     C                   parm                    keyASSOCIA
226800091104     c                   parm                    Errore
226900091104     c                   parm                    EDPORCOM
227000091104     c                   parm                    Valori_inErr
227100091104     c                   parm                    Descr_Errore
227200091104      *
227300091104     C                   EXSR      DATI_COM
227400090609      **--------
227500090609     c                   when      tipo_seg = 'DGS'
227600090609      *
227700090609     c                   call      'TRTCT05DGS'
227800090609     c                   parm                    tipo_seg
227900090609     c                   parm                    segmento
228000100716     C                   parm                    keyUNBCLI
228100100716     C                   parm                    keyTIPOMSG
228200100716     C                   parm                    keyVERSION
228300100716     C                   parm                    keyRELEASE
228400100716     C                   parm                    keyAGENCY
228500100716     C                   parm                    keyASSOCIA
228600090609     c                   parm                    Errore
228700090609     c                   parm                    EDPORDGS
228800090609     c                   parm                    Valori_inErr
228900090609     c                   parm                    Descr_Errore
229000090609      *
229100090609     C                   EXSR      DATI_DGS
229200090609      * --------
229300090609     c                   when      tipo_seg = 'DOC'
229400090609      *
229500090609     c                   call      'TRTCT05DOC'
229600090609     c                   parm                    tipo_seg
229700090609     c                   parm                    segmento
229800100716     C                   parm                    keyUNBCLI
229900100716     C                   parm                    keyTIPOMSG
230000100716     C                   parm                    keyVERSION
230100100716     C                   parm                    keyRELEASE
230200100716     C                   parm                    keyAGENCY
230300100716     C                   parm                    keyASSOCIA
230400090609     c                   parm                    Errore
230500090609     c                   parm                    EDPORDOC
230600090609     c                   parm                    Valori_inErr
230700090609     c                   parm                    Descr_Errore
230800090609      *
230900090609     C                   EXSR      DATI_DOC
231000090609      **--------
231100090609     c                   when      tipo_seg = 'DTM'
231200090609      *
231300090609     c                   call      'TRTCT05DTM'
231400090609     c                   parm                    tipo_seg
231500090609     c                   parm                    segmento
231600100716     C                   parm                    keyUNBCLI
231700100716     C                   parm                    keyTIPOMSG
231800100716     C                   parm                    keyVERSION
231900100716     C                   parm                    keyRELEASE
232000100716     C                   parm                    keyAGENCY
232100100716     C                   parm                    keyASSOCIA
232200090609     c                   parm                    Errore
232300090609     c                   parm                    EDPORDTM
232400090609     c                   parm                    Valori_inErr
232500090609     c                   parm                    Descr_Errore
232600090609      *
232700090609     C                   EXSR      DATI_DTM
232800090609      **--------
232900090609     c                   when      tipo_seg = 'GDS'
233000090609      *
233100090609     c                   call      'TRTCT05GDS'
233200090609     c                   parm                    tipo_seg
233300090609     c                   parm                    segmento
233400100716     C                   parm                    keyUNBCLI
233500100716     C                   parm                    keyTIPOMSG
233600100716     C                   parm                    keyVERSION
233700100716     C                   parm                    keyRELEASE
233800100716     C                   parm                    keyAGENCY
233900100716     C                   parm                    keyASSOCIA
234000090609     c                   parm                    Errore
234100090609     c                   parm                    EDPORGDS
234200090609     c                   parm                    Valori_inErr
234300090609     c                   parm                    Descr_Errore
234400090609      *
234500090609     C                   EXSR      DATI_GDS
234600090609      **--------
234700090609     c                   when      tipo_seg = 'GID'
234800090609      *
234900090609     c                   call      'TRTCT05GID'
235000090609     c                   parm                    tipo_seg
235100090609     c                   parm                    segmento
235200100716     C                   parm                    keyUNBCLI
235300100716     C                   parm                    keyTIPOMSG
235400100716     C                   parm                    keyVERSION
235500100716     C                   parm                    keyRELEASE
235600100716     C                   parm                    keyAGENCY
235700100716     C                   parm                    keyASSOCIA
235800090609     c                   parm                    Errore
235900090609     c                   parm                    EDPORGID
236000090609     c                   parm                    Valori_inErr
236100090609     c                   parm                    Descr_Errore
236200090609      *
236300090609     C                   EXSR      DATI_GID
236400090609      **--------
236500090609     c                   when      tipo_seg = 'MSE'
236600090609      *
236700090609     c                   call      'TRTCT05MSE'
236800090609     c                   parm                    tipo_seg
236900090609     c                   parm                    segmento
237000100716     C                   parm                    keyUNBCLI
237100100716     C                   parm                    keyTIPOMSG
237200100716     C                   parm                    keyVERSION
237300100716     C                   parm                    keyRELEASE
237400100716     C                   parm                    keyAGENCY
237500100716     C                   parm                    keyASSOCIA
237600090609     c                   parm                    Errore
237700090609     c                   parm                    EDPORMSE
237800090609     c                   parm                    Valori_inErr
237900090609     c                   parm                    Descr_Errore
238000090609      *
238100090609     C                   EXSR      DATI_MSE
238200090609      **--------
238300090609     c                   when      tipo_seg = 'NAD'
238400090609      *
238500090609     c                   call      'TRTCT05NAD'
238600090609     c                   parm                    tipo_seg
238700090609     c                   parm                    segmento
238800100716     C                   parm                    keyUNBCLI
238900100716     C                   parm                    keyTIPOMSG
239000100716     C                   parm                    keyVERSION
239100100716     C                   parm                    keyRELEASE
239200100716     C                   parm                    keyAGENCY
239300100716     C                   parm                    keyASSOCIA
239400090609     c                   parm                    Errore
239500090609     c                   parm                    EDPORNAD
239600090609     c                   parm                    Valori_inErr
239700090609     c                   parm                    Descr_Errore
239800090609      *
239900090609     C                   EXSR      DATI_NAD
240000090609      **--------
240100090609     c                   when      tipo_seg = 'PCI'
240200090609      *
240300090609     c                   call      'TRTCT05PCI'
240400090609     c                   parm                    tipo_seg
240500090609     c                   parm                    segmento
240600100716     C                   parm                    keyUNBCLI
240700100716     C                   parm                    keyTIPOMSG
240800100716     C                   parm                    keyVERSION
240900100716     C                   parm                    keyRELEASE
241000100716     C                   parm                    keyAGENCY
241100100716     C                   parm                    keyASSOCIA
241200090609     c                   parm                    Errore
241300090609     c                   parm                    EDPORPCI
241400090609     c                   parm                    Valori_inErr
241500090609     c                   parm                    Descr_Errore
241600090609      *
241700090609     C                   EXSR      DATI_PCI
241800090609      **--------
241900090609     c                   when      tipo_seg = 'RFF'
242000090609      *
242100090609     c                   call      'TRTCT05RFF'
242200090609     c                   parm                    tipo_seg
242300090609     c                   parm                    segmento
242400100716     C                   parm                    keyUNBCLI
242500100716     C                   parm                    keyTIPOMSG
242600100716     C                   parm                    keyVERSION
242700100716     C                   parm                    keyRELEASE
242800100716     C                   parm                    keyAGENCY
242900100716     C                   parm                    keyASSOCIA
243000090609     c                   parm                    Errore
243100090609     c                   parm                    EDPORRFF
243200090609     c                   parm                    Valori_inErr
243300090609     c                   parm                    Descr_Errore
243400090609      *
243500090609     C                   EXSR      DATI_RFF
243600090609      **--------
243700090609     c                   when      tipo_seg = 'TDT'
243800090609      *
243900090609     c                   call      'TRTCT05TDT'
244000090609     c                   parm                    tipo_seg
244100090609     c                   parm                    segmento
244200100716     C                   parm                    keyUNBCLI
244300100716     C                   parm                    keyTIPOMSG
244400100716     C                   parm                    keyVERSION
244500100716     C                   parm                    keyRELEASE
244600100716     C                   parm                    keyAGENCY
244700100716     C                   parm                    keyASSOCIA
244800090609     c                   parm                    Errore
244900090609     c                   parm                    EDPORTDT
245000090609     c                   parm                    Valori_inErr
245100090609     c                   parm                    Descr_Errore
245200090609      *
245300090609     C                   EXSR      DATI_TDT
245400090609      * --------
245500090609     c                   when      tipo_seg = 'TSR'
245600090609      *
245700090609     c                   call      'TRTCT05TSR'
245800090609     c                   parm                    tipo_seg
245900090609     c                   parm                    segmento
246000100716     C                   parm                    keyUNBCLI
246100100716     C                   parm                    keyTIPOMSG
246200100716     C                   parm                    keyVERSION
246300100716     C                   parm                    keyRELEASE
246400100716     C                   parm                    keyAGENCY
246500100716     C                   parm                    keyASSOCIA
246600090609     c                   parm                    Errore
246700090609     c                   parm                    EDPORTSR
246800090609     c                   parm                    Valori_inErr
246900090609     c                   parm                    Descr_Errore
247000090609      *
247100090609     C                   EXSR      DATI_TSR
247200090609      **--------
247300090609     c                   when      tipo_seg = 'TXT'
247400090609      *
247500090609     c                   call      'TRTCT05TXT'
247600090609     c                   parm                    tipo_seg
247700090609     c                   parm                    segmento
247800100716     C                   parm                    keyUNBCLI
247900100716     C                   parm                    keyTIPOMSG
248000100716     C                   parm                    keyVERSION
248100100716     C                   parm                    keyRELEASE
248200100716     C                   parm                    keyAGENCY
248300100716     C                   parm                    keyASSOCIA
248400090609     c                   parm                    Errore
248500090609     c                   parm                    EDPORTXT
248600090609     c                   parm                    Valori_inErr
248700090609     c                   parm                    Descr_Errore
248800090609      *
248900090609     C                   EXSR      DATI_TXT
249000090609      * --------
249100090209     c                   when      tipo_seg = 'UNB'
249200090209      *
249300090609     c                   call      'TRTCT05UNB'
249400090209     c                   parm                    tipo_seg
249500090209     c                   parm                    segmento
249600100716     C                   parm                    keyUNBCLI
249700100716     C                   parm                    keyTIPOMSG
249800100716     C                   parm                    keyVERSION
249900100716     C                   parm                    keyRELEASE
250000100716     C                   parm                    keyAGENCY
250100100716     C                   parm                    keyASSOCIA
250200090209     c                   parm                    Errore
250300090609     c                   parm                    EDPORUNB
250400090209     c                   parm                    Valori_inErr
250500090209     c                   parm                    Descr_Errore
250600090210      *
250700090210     C                   EXSR      DATI_UNB
250800090210      * --------
250900090209     c                   when      tipo_seg = 'UNH'
251000090209      *
251100090609     c                   call      'TRTCT05UNH'
251200090209     c                   parm                    tipo_seg
251300090209     c                   parm                    segmento
251400100716     C                   parm                    keyUNBCLI
251500100716     C                   parm                    keyTIPOMSG
251600100716     C                   parm                    keyVERSION
251700100716     C                   parm                    keyRELEASE
251800100716     C                   parm                    keyAGENCY
251900100716     C                   parm                    keyASSOCIA
252000090209     c                   parm                    Errore
252100090609     c                   parm                    EDPORUNH
252200090209     c                   parm                    Valori_inErr
252300090209     c                   parm                    Descr_Errore
252400090210      *
252500090210     C                   EXSR      DATI_UNH
252600090609      * --------
252700090609     c                   when      tipo_seg = 'UNS'
252800090609      *
252900090609     c                   call      'TRTCT05UNS'
253000090609     c                   parm                    tipo_seg
253100090609     c                   parm                    segmento
253200100716     C                   parm                    keyUNBCLI
253300100716     C                   parm                    keyTIPOMSG
253400100716     C                   parm                    keyVERSION
253500100716     C                   parm                    keyRELEASE
253600100716     C                   parm                    keyAGENCY
253700100716     C                   parm                    keyASSOCIA
253800090609     c                   parm                    Errore
253900090609     c                   parm                    EDPORUNS
254000090609     c                   parm                    Valori_inErr
254100090609     c                   parm                    Descr_Errore
254200090609      *
254300090609     C                   EXSR      DATI_UNS
254400090609      **--------
254500090609     c                   when      tipo_seg = 'UNT'
254600090609      *
254700090616     c                   call      'TRTCT05UNT'
254800090609     c                   parm                    tipo_seg
254900090609     c                   parm                    segmento
255000100716     C                   parm                    keyUNBCLI
255100100716     C                   parm                    keyTIPOMSG
255200100716     C                   parm                    keyVERSION
255300100716     C                   parm                    keyRELEASE
255400100716     C                   parm                    keyAGENCY
255500100716     C                   parm                    keyASSOCIA
255600090609     c                   parm                    Errore
255700090609     c                   parm                    EDPORUNT
255800090609     c                   parm                    Valori_inErr
255900090609     c                   parm                    Descr_Errore
256000090609      *
256100090609     C                   EXSR      DATI_UNT
256200090609      **--------
256300090609     c                   when      tipo_seg = 'UNZ'
256400090609      *
256500090616     c                   call      'TRTCT05UNZ'
256600090609     c                   parm                    tipo_seg
256700090609     c                   parm                    segmento
256800100716     C                   parm                    keyUNBCLI
256900100716     C                   parm                    keyTIPOMSG
257000100716     C                   parm                    keyVERSION
257100100716     C                   parm                    keyRELEASE
257200100716     C                   parm                    keyAGENCY
257300100716     C                   parm                    keyASSOCIA
257400090609     c                   parm                    Errore
257500090609     c                   parm                    EDPORUNZ
257600090609     c                   parm                    Valori_inErr
257700090609     c                   parm                    Descr_Errore
257800090609      *
257900090609     C                   EXSR      DATI_UNZ
258000090210      **--------
258100090609     c                   when      tipo_seg = 'VAL'
258200090213      *
258300090616     c                   call      'TRTCT05VAL'
258400090213     c                   parm                    tipo_seg
258500090213     c                   parm                    segmento
258600100716     C                   parm                    keyUNBCLI
258700100716     C                   parm                    keyTIPOMSG
258800100716     C                   parm                    keyVERSION
258900100716     C                   parm                    keyRELEASE
259000100716     C                   parm                    keyAGENCY
259100100716     C                   parm                    keyASSOCIA
259200090213     c                   parm                    Errore
259300090609     c                   parm                    EDPORVAL
259400090213     c                   parm                    Valori_inErr
259500090213     c                   parm                    Descr_Errore
259600090213      *
259700090609     C                   EXSR      DATI_VAL
259800090210      **--------
259900090209     c                   endSL
260000090209      **
260100090209     c                   endSR
260200090209     c*==================================================================*
260300090209     C*? CNVDAT - DECODIFICA LA DATA
260400090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
260500090209     C*         - WDATA  (DATA ALFANUMERICA)
260600090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
260700090209     C*         - WHMS   (ORA NUMERICA)
260800090209     C*----------------------------------------------------------------
260900090209     C     CNVDAT        BEGSR
261000090209     C*
261100090209     C                   MOVEL     ' '           WERRO             1
261200090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
261300090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
261400090209     C                   Z-ADD     *ZEROS        WHMS              6 0
261500090209     C*
261600090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
261700090209     C                   MOVEL     WDATA         WCOMO8            8
261800090209     C                   TESTN                   WCOMO8               99
261900090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
262000090209     C  N99              MOVEL     'S'           WERRO             1
262100090209     C                   END
262200090209     C*
262300090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
262400090209     C                   MOVEL     WDATA         WCOMO14          14
262500090209     C                   TESTN                   WCOMO14              99
262600090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
262700090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
262800090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
262900090209     C  N99              MOVEL     'S'           WERRO             1
263000090209     C                   END
263100090209     C*
263200090209     C                   ENDSR
263300090210     C*----------------------------------------------------------------
263400090210     C*? TSTNUM - CONTROLLO NUMERICO
263500090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
263600090210     C*         - WLUNGH (Lunghezza campo output)
263700090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
263800090210     C*----------------------------------------------------------------
263900090210     C     TSTNUM        BEGSR
264000090210     C*
264100090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
264200090210     C                   MOVEL     'N'           WOKNUM            1
264300090210     C                   MOVEL     *BLANKS       WNUMOK           15
264400090210     C                   Z-ADD     15            IN                3 0
264500090210     C                   Z-ADD     WLUNGH        IX                3 0
264600090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
264700090210     C                   MOVEA     WNUM          NDA
264800090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
264900090210     C                   MOVEA     *ALL'0'       NDN
265000090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
265100090210     C                   Z-ADD     WLUNGH        I                 3 0
265200090210     C     I             DOWGT     0                                            --- 1 -->
265300090210     C* ESTRAE IL CARATTERE DA ESAMINARE
265400090210     C     1             SUBST     WNUM:I        WTEM01            1
265500090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
265600090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
265700090210     C     WTEM01        SCAN      WCA                                    99
265800090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
265900090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
266000090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
266100090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
266200090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
266300090210     C* DA DESTRA VERSO SINISTRA
266400090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
266500090210     C                   MOVEL     WTEMP1        NDN(IN)
266600090210     C                   SUB       1             IN
266700090210     C                   END                                                    <-- 3 ---
266800090210     C                   END                                                    <-- 2 ---
266900090210     C                   SUB       1             I
267000090210     C                   END                                                    <-- 1 ---
267100090210     C*
267200090210     C                   MOVEA     NDN           WNDN             15
267300090210     C     WNDN          IFNE      *ZEROS
267400090210     C                   MOVEA     NDN           WNUMOK           15
267500090210     C                   MOVEL     'S'           WOKNUM            1
267600090210     C                   END
267700090210     C*
267800090210     C                   ENDSR
267900090210     C*----------------------------------------------------------------
268000090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
268100090210     C*----------------------------------------------------------------
268200090210     C     CNVCAP        BEGSR
268300090210     C*
268400090210     C                   MOVEL     *BLANKS       WCAP              9
268500090609     C     cap09a        IFNE      '0'
268600090609     C     '  '          SCAN      cap09a        LENGHT            2 0
268700090210     C                   SUB       1             LENGHT
268800090210     C     LENGHT        IFLE      5
268900090210     C     LENGHT        ANDGT     0
269000090210     C                   Z-ADD     LENGHT        T                 2 0
269100090210     C                   Z-ADD     5             S                 2 0
269200090609     C                   MOVEA     cap09a        CAP9
269300090210     C                   MOVEL     *ZEROS        CAP5
269400090210     C                   DO        LENGHT
269500090210     C                   MOVE      CAP9(T)       CAP5(S)
269600090210     C                   SUB       1             S
269700090210     C                   SUB       1             T
269800090210     C                   END
269900090210     C                   MOVEA     CAP5          WCAP5             5
270000090210     C     WCAP5         IFEQ      '0'
270100090210     C                   MOVEL     *BLANKS       WCAP
270200090210     C                   ELSE
270300090210     C                   MOVEA     CAP5          WCAP
270400090210     C                   END
270500090210     C*
270600090210     C                   ELSE
270700090609     C                   MOVEL     cap09a        WCAP
270800090210     C                   END
270900090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
271000090210     C     WCAP          IFNE      *BLANKS
271100090210     C     WFLCPF        ANDNE     0
271200090210     C                   MOVEL     WFLCPF        WELE              1 0
271300090210     C                   MOVE      WFLCPF        WPOS              1 0
271400090210     C                   MOVEL     *BLANK        WCPF
271500090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
271600090210     C                   ELSE
271700090210     C                   MOVEL     *BLANKS       WCPF
271800090210     C                   END
271900090210     C                   END
272000090210     C*
272100090210     C                   ENDSR
272200090211      * ?================================================================== */
272300090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
272400090211      * ?================================================================== */
272500090211     C     Scrive_VAB    BEGSR
272600090211      *
272700090211     c                   clear                   err_bloccante     1
272800090211     c                   move      'S'           Almeno_Uno
272900090616     c                   eval       se_trovato_NAD = *blank
273000090211      *
273100090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
273200090211     c                   exsr      UPDVAB
273300090211      *
273400090211     C* >>>>>>>>>>>>
273500090211     C*  Richiamo pgm per scrittura immediata VAB
273600090211     C     §PUWRK        IFEQ      ' '
273700090211      *
273800090211     C*  Richiamo pgm per esecuzione stampa
273900090213     C                   MOVEL     WNRDOC        d86CMR
274000090213     C                   MOVEL     ' '           d86RIE
274100090213     C                   MOVEL     §PUDTA        d86DTA
274200090213     C                   Z-ADD     1             d86CNT
274300090213     C                   Z-ADD     §PTKSC        d86CCM
274400090213     C                   MOVEL     *BLANKS       d86STA
274500090213     C                   MOVEL     'E'           d86MOD
274600090611      *
274700090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
274800090211      * dove poi imposta la DS dei parametri passati
274900090213     C                   MOVEL     'LR'          d86CHI
275000090213     C                   MOVEL     'N'           d86CTL
275100090211      *
275200090211     c                   move      kpjbu         SvKpjbu
275300090211     c                   clear                   kpjbu
275400090216     C                   movel     *on           Commit_on         1
275500090213     C                   MOVEL     TRTC86ds      KPJBU
275600090213      * ?- Chiamata la TRTC86R2 -*
275700090213     C                   CALL      'TRTC86R2'
275800090211     C                   PARM                    KPJBA
275900090216     C                   PARM                    Commit_on
276000090211     c                   move      Svkpjbu       Kpjbu
276100090211      *
276200090211     C*  Controllo pgm per scrittura immediata VAB
276300090211     C     WW            IFNE      0
276400090211     C                   DO        WW            WX                3 0
276500090213     C                   Z-ADD     KCL(WX)       d86CCM
276600090211      *
276700090211     c                   move      kpjbu         SvKpjbu
276800090211     c                   clear                   kpjbu
276900090216     C                   movel     *on           Commit_on         1
277000090213     C                   MOVEL     TRTC86ds      KPJBU
277100090213      * ?- Chiamata la TRTC86R2 -*
277200090213     C                   CALL      'TRTC86R2'
277300090211     C                   PARM                    KPJBA
277400090216     C                   PARM                    Commit_on
277500090211     c                   move      Svkpjbu       Kpjbu
277600090211     C                   END
277700090211     C                   END
277800090211      *
277900090211     C                   END
278000090211      * >>>>>>>>>>>>
278100090211     C                   if        se_errore  = 'S'
278200090211     c                   move      'S'           err_bloccante
278300090211     c                   goto      Error_VAB
278400090211     c                   end
278500090211     C*
278600090211     C     Error_VAB     Endsr
278700090211     C*----------------------------------------------------------------
278800090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
278900090211     C*----------------------------------------------------------------
279000090211     C     UPDVAB        BEGSR
279100090211      *
279200090211     C*  Se non è stato impostato il codice bolla lo imposto io
279300090211     C     VABCBO        IFEQ      *BLANKS
279400090630     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
279500090211     C                   END
279600090630      *
279700090630      *  Se c'è il contrassegno
279800090630     C     VABCAS        IFGT      0
279900090630     C     VABCBO        ifeq      '1'
280000090630     C                   MOVEL     '4'           VABCBO                         + C/Ass
280100090630     C                   END
280200090630     C     VABCBO        ifeq      '2'
280300090630     C                   MOVEL     '6'           VABCBO                         + C/Ass
280400090630     C                   END
280500090630     C                   END
280600110922      *
280700110922      * Imposta le NOte
280800110922     C     VABNOT        IFEQ      *BLANKS
280900110922     C                   MOVEL     wnot1         VABNOT
281000110922     C                   MOVEL     wnot2         VABNT2
281100110922     C                   ELSE
281200110922     C                   MOVEL     wnot1         VABNT2
281300110922     C                   END
281400090211      *
281500090211     C*  Attribuisco anno spedizione
281600090611     C     Tes_VABdcm    IFGT      0
281700090611     C                   MOVEL     Tes_VABdcm    VABAAS                         anno sped.
281800090211     C                   END
281900090211      *
282000090211      *  Controllo dettaglio segnacolli
282100090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
282200090211     C                   EXSR      CTRSGE
282300090211     C                   ELSE
282400090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
282500090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
282600090211     C                   EXSR      CTRSGN
282700090211     C                   END
282800090211     C                   END
282900090211      *
283000090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
283100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
283200090211     c                   clear                   mantiene_nsp      1            *blk/'S'
283300090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
283400090211      *
283500090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
283600090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
283700090211      *
283800090211      *  Imposto codice cliente
283900090211     c                   Select
284000090211      *
284100090611      *  Specificato codice da qualificatore FF
284200090211     C                   When        WCLKSC <> *zeros
284300090211     C                   Z-ADD     WCLKSC        VABCCM
284400090211     C                   MOVEL     WCLTAR        VABCTR
284500090211      * forza LNP/CTM/NRS
284600090211     c                   if        WclLNP <> 0
284700090211     C                   eval      VABLNP = WclLNP
284800090211     c                   end
284900090211     c                   if        WclNRS <> 0
285000090211     C                   eval      VABNRS = WclNRS
285100090211     c                   end
285200090211     c                   if        WclCTM <> *blank
285300090211     C                   eval      VABCTM = WclCTM
285400090211     c                   end
285500090211      *
285600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
285700090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
285800090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
285900090211     c                   end
286000090211      *
286100090611      *  Specificato codice da qualificatore FF
286200090211     C                   When        $CLKSC <> *zeros
286300090211     C                   Z-ADD     $CLKSC        VABCCM
286400090211     C                   MOVEL     $CLTAR        VABCTR
286500090211      * forza LNP/CTM/NRS
286600090211     c                   if        $clLNP <> 0
286700090211     C                   eval      VABLNP = $clLNP
286800090211     c                   end
286900090211     c                   if        $clNRS <> 0
287000090211     C                   eval      VABNRS = $clNRS
287100090211     c                   end
287200090211     c                   if        $clCTM <> *blank
287300090211     C                   eval      VABCTM = $clCTM
287400090211     c                   end
287500090211      *
287600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
287700090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
287800090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
287900090211     c                   end
288000090211      *
288100090211      *  Imposto dati da tabella - PT-
288200090211     C                   Other
288300090211      *
288400090211     C                   MOVEL     §PTKSC        VABCCM
288500090211     C                   MOVEL     §PTCTR        VABCTR
288600090211      *
288700090211     C                   Endsl
288800090211      *
288900090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
289000090211      *    o dalla relativa CL
289100090211     C                   MOVEL     VABlnp        VABfgs
289200090211      *
289300090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
289400090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
289500090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
289600090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
289700090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
289800090211      *
289900100511      * Avendo attivato il VAX si deve recepire lo stesso numero passato nel rif.Alfabetico
290000100511      *
290100100511     c                   if        mantiene_nsp = 'S' and VABRMA <> *blank      *blk/'S' - 1234567
290200100511     c                   movel(p)  vabrma        campo9a           9
290300100511     c                   move      campo9a       VABNSP                         *NUM.SPED.da Cliente
290400100511     c                   else
290500090211      *
290600090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
290700090211     C*  lo attribuisco io
290800090211      **              **------------------**
290900090211     c                   exsr      repnum
291000090211      **              **------------------**
291100090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
291200090211      **              **------------------**
291300090211     c                   end
291400090211      *
291500090211     C*  Altrimenti attribuisco data del giorno
291600090211     C     VABMGS        IFEQ      0
291700090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
291800090211     C                   MOVE      WOGGI         VABMGS                         = oggi
291900090211     C                   END
292000090611      *
292100090211     C*  Eseguo posizionamento su EDiVAB
292200090211     C                   Z-ADD     VABAAS        KAAS
292300090211     C                   Z-ADD     VABlnp        KLNP
292400090211     C                   Z-ADD     VABNRS        KNRS
292500090211     C                   Z-ADD     VABNSP        KNSP
292600090211     C                   MOVEL     SAVBOL        SALVA
292700090211     C     KVAB1         CHAIN     EDiVAB1L                           30
292800090211     C                   MOVEL     SALVA         SAVBOL
292900090611      *
293000090211     C*  Imposto dati CMR
293100090211     C                   Z-ADD     1             VABCNT
293200090211     **
293300090211      *** Attenzione se si deve trattare il VAX con il CMR
293400090211      *** il campo VABCNT deve essere sempre impostato a (1)
293500090211     C     §puWRK        ifEQ      'S'
293600090211     C     §puNSP        andEQ     'S'
293700090211     C                   Z-ADD     1             VABCNT
293800090211     c                   end
293900090211     **
294000090211     C                   Z-ADD     WDTPRE        VABDTS
294100090212     C     WHMPRE        mult      100           VABHMS
294200090611      *
294300090611     C                   Z-ADD     Tes_VABdcm    VABDCM
294400090211     C                   MOVEL     WNRCMR        VABCMR
294500090611      *
294600090211     C*  Se non è indicato il nome del mittente = Partner mittente
294700090211     C     VABRMO        IFEQ      *BLANKS
294800090211     C                   MOVEL     ACORAG        VABRMO
294900090211     C                   MOVEL     INDCAP        VABCMO
295000090211     C     INDCAE        IFNE      *BLANKS
295100090211     C                   MOVEL     INDCAE        VABCMO
295200090211     C                   END
295300090211     C     INDSTA        IFNE      *BLANKS
295400090211     C                   MOVEL     INDSTA        VABNMO
295500090211     C                   END
295600090211     C                   END
295700090611      *
295800090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
295900090211     C     §1BFG1        IFEQ      'N'
296000090211     C     §1BFG7        ANDEQ     'N'
296100090211     C     §1BFG2        ANDEQ     'N'
296200090211     C***                  Z-ADD0         VABNRS
296300090211     C                   END
296400090611      *
296500090211     C*  Imposto segancollo da - a
296600090211     C     §1BFG1        IFEQ      'S'
296700090211     C     §1BFG7        ANDEQ     'N'
296800090211     C     §1BF12        ANDNE     'E'
296900090211      *
297000090211     C     XY            IFEQ      *ZEROS
297100090211     C                   CLEAR                   VABNCD
297200090211     C                   CLEAR                   VABNCA
297300090211     C                   ELSE
297400090211     C                   Z-ADD     SGN(1)        VABNCD
297500090211     C                   Z-ADD     SGN(XY)       VABNCA
297600090211     C                   ENDIF
297700090211     C*
297800090211     C                   END
297900090211     C*
298000090211     C*  Se cap mittente originale a blank abblenco nazione
298100090211     C     VABCMO        IFEQ      *BLANKS
298200090211     C                   MOVEL     *BLANKS       VABNMO
298300090211     C                   END
298400100311     C*
298500100311     C*  aggiungo le NOTE se ci sono
298600100311     C     Wnot1         IFne      *BLANKS
298700100311     c                   movel(p)  Wnot1         vabNOT
298800100311     c                   end
298900100311     C     Wnot2         IFne      *BLANKS
299000100311     c                   movel(p)  Wnot2         vabNT2
299100100311     c                   end
299200100311     C*
299300090603      *
299400090211      *? ------------------------------------------------------------------------
299500090213     C   30              WRITE     EDiVAB00                             99
299600090213     C  N30              UPDATE    EDiVAB00                             99
299700090211      *? ------------------------------------------------------------------------
299800090611     C*
299900090213     c   99              eval      errori_in_Wrt = 'S'
300000090211     C*
300100090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
300200090211     C                   EXSR      WRTVAT_Rif
300300090211     C*
300400090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
300500090211     C     §1BF12        IFEQ      'E'
300600090211     C                   EXSR      WRTVAT
300700090211     C                   ELSE
300800090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
300900090211     C     §1BFG7        IFEQ      'S'
301000090211     C     §1BFG1        OREQ      'S'
301100090211     C                   EXSR      WRTVAD
301200090211     C                   END
301300090211     C                   END
301400090211      *
301500090211     C* Reimposto codice trattamento merce cliente
301600090211     C                   MOVEL     WSVTRM        DS1B
301700090211     C**
301800090211     C* Do errore se previsto CMR ma non c'è
301900090211     C     §PTCMR        IFNE      ' '
302000090211     C                   EXSR      MEMCMR
302100090211     C                   END
302200090611      *
302300090211     C* Do errore se previsto AFB ma non c'è
302400090211     C     §PTNFV        IFNE      ' '
302500090211     C                   EXSR      MEMAFB
302600090211     C                   END
302700090611      *
302800090211     C*  Se il cliente vuole il ritorno delle date di consegna
302900090211     C*  è c'è una squdratura fra i segnacolli e il numero
303000090211     C*  dei colli che ci ha passato scrivo EDSUM
303100090211     C     §PUDTA        IFEQ      'S'
303200090211     C     WSQUAD        ANDEQ     'S'
303300090211     C                   EXSR      WRTSUM
303400090211     C                   END
303500090211     C*
303600090211     C                   ENDSR
303700090211     C*----------------------------------------------------------------
303800090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
303900090211     C*----------------------------------------------------------------
304000090211     C     CTRSGN        BEGSR
304100090211     C*
304200090211     C                   MOVEL     'N'           WNSGER            1
304300090211     C*
304400090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
304500090211     C     VABNCL        IFNE      XY
304600090211     C                   MOVEL     'S'           WNSGER
304700090211     C                   MOVEL     'S'           WSQUAD
304800090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
304900090211     c                              bollettati'
305000090211     C                   GOTO      FINSGN
305100090211     C                   END
305200090211     C*  Riordino i segnacolli
305300090211     C     §1BFG7        IFEQ      'N'
305400090211     C                   SORTA     SGN
305500090211     C*  Controllo se sono sequenziali
305600090211     C                   MOVEL     'N'           WNOSEQ            1
305700090211     C     SGN(1)        ADD       1             WEXSGN            7 0
305800090211     C     2             DO        XY            X
305900090211     C     SGN(X)        IFNE      WEXSGN
306000090211     C                   MOVEL     'S'           WNOSEQ
306100090211     C                   END
306200090211     C     SGN(X)        ADD       1             WEXSGN
306300090211     C                   END
306400090211     C                   END
306500090211     C*
306600090211     C     FINSGN        ENDSR
306700090211     C*----------------------------------------------------------------
306800090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
306900090211     C*----------------------------------------------------------------
307000090211     C     CTRSGE        BEGSR
307100090211     C*
307200090211     C                   MOVEL     'N'           WNSGER            1
307300090211      *
307400090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
307500090211      *
307600090211     C     VABNCL        IFNE      §§
307700090211     C                   MOVEL     'S'           WNSGER
307800090211     C                   MOVEL     'S'           WSQUAD
307900090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
308000090211     c                              bollettati'
308100090211     C                   END
308200090211      *
308300090211     C                   ENDSR
308400090211     C*----------------------------------------------------------------
308500090211     C*? ESEGUO SCRITTURA EDiVAD
308600090211     C*----------------------------------------------------------------
308700090211     C     WRTVAD        BEGSR
308800090211     C*
308900090211     C* Se non seq. scrivo xy record
309000090211     C     §1BFG1        IFEQ      'S'
309100090211     C     XY            ANDNE     *ZEROS
309200090211     C*
309300090211     C     §1BFG7        IFEQ      'S'
309400090211     C                   DO        XY            X
309500090211     C                   Z-ADD     VABCCM        KCCM
309600090211     C                   Z-ADD     SGN(X)        KNCD
309700090211     C                   Z-ADD     VABCNT        VADCNT
309800090211     C                   MOVEL     VABCMR        VADCMR
309900090211     C                   Z-ADD     VABAAS        VADAAS
310000090211     C                   Z-ADD     VABLNP        VADLNP
310100090211     C                   Z-ADD     VABNRS        VADNRS
310200090211     C                   Z-ADD     VABNSP        VADNSP
310300090211     C                   Z-ADD     1             VADNCL
310400090211     C                   MOVEL     SGN(X)        VADCDP
310500090211     C                   Z-ADD     SGN(X)        VADNCD
310600090211     C                   Z-ADD     *ZEROS        VADNCA
310700090211     C                   Z-ADD     VABCCM        VADCCM
310800090211     C                   movel     VABfgs        VADfgs
310900090213     C                   WRITE     EDiVAD00                             99
311000090213     c   99              eval      errori_in_Wrt = 'S'
311100090211     C                   END
311200090211     C* Se sequenziali scrivo un solo record
311300090211     C                   ELSE
311400090211     C                   Z-ADD     VABAAS        VADAAS
311500090211     C                   Z-ADD     VABLNP        VADLNP
311600090211     C                   Z-ADD     VABNRS        VADNRS
311700090211     C                   Z-ADD     VABNSP        VADNSP
311800090211     C                   Z-ADD     VABNCL        VADNCL
311900090211     C                   Z-ADD     SGN(1)        VADNCD
312000090211     C                   Z-ADD     SGN(XY)       VADNCA
312100090211     C                   Z-ADD     VABCCM        VADCCM
312200090211     C                   movel     VABfgs        VADfgs
312300090213     C                   WRITE     EDiVAD00                             99
312400090213     c   99              eval      errori_in_Wrt = 'S'
312500090211     C                   END
312600090211     C*
312700090211     C                   ELSE
312800090211     C                   DO        XY            X
312900090211     C                   Z-ADD     VABCCM        KCCM
313000090211     C                   Z-ADD     SGN(X)        KNCD
313100090211     C                   Z-ADD     VABAAS        VADAAS
313200090211     C                   Z-ADD     VABLNP        VADLNP
313300090211     C                   Z-ADD     VABNRS        VADNRS
313400090211     C                   Z-ADD     VABNSP        VADNSP
313500090211     C                   MOVEL     SGN(X)        VADCDP
313600090211     C                   Z-ADD     1             VADNCL
313700090211     C                   Z-ADD     *ZEROS        VADNCD
313800090211     C                   Z-ADD     *ZEROS        VADNCA
313900090211     C                   Z-ADD     VABCCM        VADCCM
314000090211     C                   movel     VABfgs        VADfgs
314100090213     C                   WRITE     EDiVAD00                             99
314200090213     c   99              eval      errori_in_Wrt = 'S'
314300090211     C                   END
314400090211     C*
314500090211     C                   END
314600090211     C*
314700090211     C                   ENDSR
314800090211     C*----------------------------------------------------------------
314900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
315000090211     C*----------------------------------------------------------------
315100090211     C     WRTVAT_Rif    BEGSR
315200090211      *
315300090211      *  riferimento di consegna destinatario
315400090211     c                   if        WatRDE <> *blank
315500090211     C                   Z-ADD     VABCNT        VATCNT
315600090211     C                   MOVEL     VABCMR        VATCMR
315700090211     C                   Z-ADD     VABCCM        VATCCM
315800090211     C                   movel     VABfgs        VATfgs
315900090211     C                   Z-ADD     VABLNP        VATLNP
316000090211     C                   Z-ADD     VABAAS        VATAAS
316100090211     C                   Z-ADD     VABNRS        VATNRS
316200090211     C                   Z-ADD     VABNSP        VATNSP
316300090211     C                   MOVEL     'A'           VATTRC
316400090211     C                   MOVEL     WatRDE        VATNOT
316500090213     C                   WRITE     EDiVAT00                             99
316600090213     c   99              eval      errori_in_Wrt = 'S'
316700090211     c                   end
316800090211      *
316900090211      *  riferimento di consegna telefonico
317000090211     c                   if        WatTEL <> *blank
317100090211     C                   Z-ADD     VABCNT        VATCNT
317200090211     C                   MOVEL     VABCMR        VATCMR
317300090211     C                   Z-ADD     VABCCM        VATCCM
317400090211     C                   movel     VABfgs        VATfgs
317500090211     C                   Z-ADD     VABLNP        VATLNP
317600090211     C                   Z-ADD     VABAAS        VATAAS
317700090211     C                   Z-ADD     VABNRS        VATNRS
317800090211     C                   Z-ADD     VABNSP        VATNSP
317900090211     C                   MOVEL     'B'           VATTRC
318000090211     C                   MOVEL     WatTEL        VATNOT
318100090213     C                   WRITE     EDiVAT00                             99
318200090213     c   99              eval      errori_in_Wrt = 'S'
318300090211     c                   end
318400090211      *
318500090211      * riferimento del Partner che una volta veniva riportato sul BL4
318600090211     c                   if        Wnsp   <> *blank
318700090211     C                   Z-ADD     VABCNT        VATCNT
318800090211     C                   MOVEL     VABCMR        VATCMR
318900090211     C                   Z-ADD     VABCCM        VATCCM
319000090211     C                   movel     VABfgs        VATfgs
319100090211     C                   Z-ADD     VABLNP        VATLNP
319200090211     C                   Z-ADD     VABAAS        VATAAS
319300090211     C                   Z-ADD     VABNRS        VATNRS
319400090211     C                   Z-ADD     VABNSP        VATNSP
319500090211     C                   MOVEL     'C'           VATTRC
319600090211     C                   MOVEL     Wnsp          VATNOT
319700090213     C                   WRITE     EDiVAT00                             99
319800090213     c   99              eval      errori_in_Wrt = 'S'
319900090211     c                   end
320000090211      *
320100090211     C                   ENDSR
320200090211     C*----------------------------------------------------------------
320300090211     C*? ESEGUO SCRITTURA EDiVAT
320400090211     C*----------------------------------------------------------------
320500090211     C     WRTVAT        BEGSR
320600090211      *
320700090211     C                   DO        §§            X
320800090211     C                   Z-ADD     VABCNT        VATCNT
320900090211     C                   MOVEL     VABCMR        VATCMR
321000090211     C                   Z-ADD     VABCCM        VATCCM
321100090211     C                   movel     VABfgs        VATfgs
321200090211     C                   Z-ADD     VABLNP        VATLNP
321300090211     C                   Z-ADD     VABAAS        VATAAS
321400090211     C                   Z-ADD     VABNRS        VATNRS
321500090211     C                   Z-ADD     VABNSP        VATNSP
321600090211     C                   MOVEL     'E'           VATTRC
321700090211      *
321800090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
321900090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
322000090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
322100090211     c                   if        WVATlun > 0
322200090211      *
322300090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
322400090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
322500090211      *
322600090211     c                   else
322700090211      *
322800090211      * Se non è impostato nulla prende il segnacollo passato così com'è
322900090211     C                   MOVEL     SGE(X)        VATNOT
323000090211     c                   end
323100090211      *
323200090213     C                   WRITE     EDiVAT00                             99
323300090213     c   99              eval      errori_in_Wrt = 'S'
323400090211     C                   END
323500090211      *
323600090211     C                   ENDSR
323700090211     C*----------------------------------------------------------------
323800090211     C*? ESEGUO SCRITTURA EDSUM
323900090211     C*----------------------------------------------------------------
324000090211     C     WRTSUM        BEGSR
324100090211     C*
324200090211     C* Se non seq. scrivo xy record
324300090211     C                   DO        XY            X
324400090211     C                   Z-ADD     VABAAS        SUMAAS
324500090211     C                   Z-ADD     VABLNP        SUMFLS
324600090211     C                   MOVEL     WKSC          SUMLNP
324700090211     C                   Z-ADD     VABLNA        SUMLNA
324800090211     C                   Z-ADD     VABZNC        SUMZNC
324900090211     C                   Z-ADD     VABNRS        SUMNRS
325000090211     C                   Z-ADD     VABNSP        SUMNSP
325100090211     C                   Z-ADD     SGN(X)        SUMNSC
325200090211     C                   Z-ADD     WKSC          SUMCCM
325300090211     C                   MOVEL     *BLANKS       SUMSTS
325400090211     C                   MOVEL     *BLANKS       SUMFLG
325500090211     C                   MOVEL     WPRGSP        SUMCNI
325600090211     C                   MOVEL     VABCMR        SUMCMR
325700090211     C                   Z-ADD     0             SUMNFE
325800090211     C                   Z-ADD     VABCCM        VADCCM
325900090213     C                   WRITE     EDSUM000                             99
326000090213     c   99              eval      errori_in_Wrt = 'S'
326100090211     C                   END
326200090211     C*
326300090211     C                   ENDSR
326400090211     c*==================================================================*
326500090211      * Manda un Msg in Posta AS Sede a EDP*
326600090211     c*==================================================================*
326700090211     c     SEND_MSG_EDP  begsr
326800090211      *
326900090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
327000090211      *   Lo manda una sola volta per lavoro
327100090211     c                   IF        Snd_Msg_alert  <> 'N' and
327200090211     c                             User_msg <> *blank
327300090211     c                   z-add     1             Jx
327400090211     C                   exsr      CalQEZSNDMG
327500090211     c                   end
327600090211      *
327700090211     c                   endsr
327800090213     c**********************************************************************
327900090213     c* reperimento numero Spedizione
328000090213     c**********************************************************************
328100090213     C     repnum        BEGSR
328200090213      *
328300090213      *    deve controllare sulla tabella "3C" se il numero spedizione
328400090213      *     deve essere mantenuto oppure no
328500090213     C*
328600090213     C                   clear                   Ds3C
328700090213     C                   Z-ADD     CODUT         TBLKUT
328800090213     C                   MOVEL     '3C'          TBLCOD
328900090213     C                   movel(p)  VABccm        TBLKEY
329000090213     C     KTABel        CHAIN     TABEL00F                           30
329100090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
329200090213     C                   MOVEL     TBLUNI        Ds3C
329300090213     C                   END
329400090213      *
329500090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
329600090213     c                   if        §3CFsp <> 'S'
329700090213      *
329800090213     C* NSP => Stacco un numeratore da AZNUM
329900090213     c                   movel     kpjbu         svkpjbu
330000090213     c                   clear                   kpjbu
330100090213     C                   clear                   TRUL33DS
330200090213     C                   eval      I33OPE = *zeros
330300090213     C                   eval      I33CNU = 302
330400090213     C                   eval      I33NUM = 1
330500090213     C                   movel     TRUL33DS      KPJBU
330600090213     C                   call      'TRUL33R'
330700090213     C                   parm                    KPJBA
330800090213     C                   movel     KPJBU         TRUL33DS
330900090213     c                   movel     svkpjbu       kpjbu
331000090213      ****
331100090213     C                   if        O33ERR = *zeros
331200090213     c                   z-add     o33nrf        NUM_SPED
331300090213     C                   else
331400090213     C                   endif
331500090213      ****
331600090213     c                   ELSE
331700090213      *   se si vuole mantenere il numero spedizione
331800090213      ****
331900090213     C                   MOVEL     WOGGI         WANNO             4 0
332000090213     C                   MOVE      WANNO         KAAA
332100090213     C                   Z-ADD     3             KCNU
332200090213     C                   Z-ADD     VABlnp        KFIL
332300090213     C     KNUF          CHAIN     FLNUF                              9999
332400090213     C     *IN99         IFEQ      '0'
332500090213     C                   ADD       1             NUFNUM
332600090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
332700090213     C                   UPDATE    FLNUF
332800090213     C                   ELSE
332900090213     C                   END
333000090213     C*
333100090213     c                   EndIF
333200090213      **
333300090213     C                   ENDSR
333400090213     C*----------------------------------------------------------------
333500090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
333600090213     C*----------------------------------------------------------------
333700090213     C     MEMCMR        BEGSR
333800090213     C*
333900090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
334000090213     C     §PTCM1        IFEQ      'S'
334100090213     C                   CLEAR                   EDRDE000
334200090213     C                   MOVEL     'TD1154'      KNMC
334300090213     C                   Z-ADD     1             KNRP
334400090213     C                   MOVEL     WKSC          KLNP1
334500090213     C     KRDE          CHAIN     EDRDE01L                           31
334600090213     C     *IN31         IFEQ      '1'
334700090213     C                   Z-ADD     VABAAS        RDEAAS
334800090213     C                   Z-ADD     VABLNP        RDELNP
334900090213     C                   Z-ADD     VABNRS        RDENRS
335000090213     C                   Z-ADD     VABNSP        RDENSP
335100090213     C                   MOVEL     'TD1154'      RDENMC
335200090213     C                   Z-ADD     1             RDENRP
335300090213     C                   MOVEL     WNRCMR        RDEVAL
335400090213     C                   WRITE     EDRDE000                             99
335500090213     c   99              eval      errori_in_Wrt = 'S'
335600090213     C                   END
335700090213     C*  Memorizzo qualificatore CMR
335800090213     C                   CLEAR                   EDRDE000
335900090213     C                   MOVEL     'TD1153'      KNMC
336000090213     C                   Z-ADD     1             KNRP
336100090213     C                   MOVEL     WKSC          KLNP1
336200090213     C     KRDE          CHAIN     EDRDE01L                           31
336300090213     C     *IN31         IFEQ      '1'
336400090213     C                   Z-ADD     VABAAS        RDEAAS
336500090213     C                   Z-ADD     VABLNP        RDELNP
336600090213     C                   Z-ADD     VABNRS        RDENRS
336700090213     C                   Z-ADD     VABNSP        RDENSP
336800090213     C                   Z-ADD     1             RDENRP
336900090213     C                   MOVEL     'TD1153'      RDENMC
337000090213     C                   MOVEL     'CMR'         RDEVAL
337100090213     C                   WRITE     EDRDE000                             99
337200090213     c   99              eval      errori_in_Wrt = 'S'
337300090213     C                   END
337400090213     C*  Memorizzo la data
337500090213     C                   CLEAR                   EDRDE000
337600090213     C                   MOVEL     'TD2380'      KNMC
337700090213     C                   Z-ADD     1             KNRP
337800090213     C                   MOVEL     WKSC          KLNP1
337900090213     C     KRDE          CHAIN     EDRDE01L                           31
338000090213     C     *IN31         IFEQ      '1'
338100090213     C                   Z-ADD     VABAAS        RDEAAS
338200090213     C                   Z-ADD     VABLNP        RDELNP
338300090213     C                   Z-ADD     VABNRS        RDENRS
338400090213     C                   Z-ADD     VABNSP        RDENSP
338500090213     C                   Z-ADD     1             RDENRP
338600090213     C                   MOVEL     'TD2380'      RDENMC
338700090611     C                   MOVEL     Tes_VABdcm    RDEVAL
338800090213     C                   WRITE     EDRDE000                             99
338900090213     c   99              eval      errori_in_Wrt = 'S'
339000090213     C                   END
339100090213     C*
339200090213     C                   END                                                    §PTCM1 = 'S'
339300090213     C*
339400090213     C                   ENDSR
339500090213     C*----------------------------------------------------------------
339600090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
339700090213     C*----------------------------------------------------------------
339800090213     C     MEMAFB        BEGSR
339900090213     C*
340000090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
340100090213     C     §PTNF1        IFEQ      'S'
340200090213     C                   CLEAR                   EDRDE000
340300090213     C                   MOVEL     WKSC          KLNP1
340400090213     C                   MOVEL     'TD1154'      KNMC
340500090213     C                   Z-ADD     1             KNRP
340600090213     C     KRDE          CHAIN     EDRDE01L                           31
340700090213     C     *IN31         IFEQ      '1'
340800090213     C                   Z-ADD     VABAAS        RDEAAS
340900090213     C                   Z-ADD     VABLNP        RDELNP
341000090213     C                   Z-ADD     VABNRS        RDENRS
341100090213     C                   Z-ADD     VABNSP        RDENSP
341200090213     C                   MOVEL     'TD1154'      RDENMC
341300090213     C     §PTCM1        IFNE      'S'
341400090213     C     WNRCMR        OREQ      *BLANKS
341500090213     C                   Z-ADD     1             RDENRP
341600090213     C                   ELSE
341700090213     C                   Z-ADD     2             RDENRP
341800090213     C                   END
341900090213     C                   MOVEL     WNUMFV        RDEVAL
342000090213     C                   WRITE     EDRDE000                             99
342100090213     c   99              eval      errori_in_Wrt = 'S'
342200090213     C                   END
342300090213     C*  Memorizzo qualificatore CMR
342400090213     C                   CLEAR                   EDRDE000
342500090213     C                   MOVEL     WKSC          KLNP1
342600090213     C                   MOVEL     'TD1153'      KNMC
342700090213     C                   Z-ADD     1             KNRP
342800090213     C     KRDE          CHAIN     EDRDE01L                           31
342900090213     C     *IN31         IFEQ      '1'
343000090213     C                   Z-ADD     VABAAS        RDEAAS
343100090213     C                   Z-ADD     VABLNP        RDELNP
343200090213     C                   Z-ADD     VABNRS        RDENRS
343300090213     C                   Z-ADD     VABNSP        RDENSP
343400090213     C     §PTCM1        IFNE      'S'
343500090213     C     WNRCMR        OREQ      *BLANKS
343600090213     C                   Z-ADD     1             RDENRP
343700090213     C                   ELSE
343800090213     C                   Z-ADD     2             RDENRP
343900090213     C                   END
344000090213     C                   MOVEL     'TD1153'      RDENMC
344100090213     C                   MOVEL     'AFB'         RDEVAL
344200090213     C                   WRITE     EDRDE000                             99
344300090213     c   99              eval      errori_in_Wrt = 'S'
344400090213     C                   END
344500090213     C*  Memorizzo la data
344600090213     C                   CLEAR                   EDRDE000
344700090213     C                   MOVEL     WKSC          KLNP1
344800090213     C                   MOVEL     'TD2380'      KNMC
344900090213     C                   Z-ADD     1             KNRP
345000090213     C     KRDE          CHAIN     EDRDE01L                           31
345100090213     C     *IN31         IFEQ      '1'
345200090213     C                   Z-ADD     VABAAS        RDEAAS
345300090213     C                   Z-ADD     VABLNP        RDELNP
345400090213     C                   Z-ADD     VABNRS        RDENRS
345500090213     C                   Z-ADD     VABNSP        RDENSP
345600090213     C     §PTCM1        IFNE      'S'
345700090213     C     WNRCMR        OREQ      *BLANKS
345800090213     C                   Z-ADD     1             RDENRP
345900090213     C                   ELSE
346000090213     C                   Z-ADD     2             RDENRP
346100090213     C                   END
346200090213     C                   MOVEL     'TD2380'      RDENMC
346300090611     C                   MOVEL     Tes_VABdcm    RDEVAL
346400090213     C                   WRITE     EDRDE000                             99
346500090213     c   99              eval      errori_in_Wrt = 'S'
346600090213     C                   END
346700090213     C*
346800090213     C                   END                                                    §PTCM1 = 'S'
346900090213     C*
347000090213     C                   ENDSR
347100090213     c*==================================================================*
347200090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
347300090213     c*==================================================================*
347400090213     c     Imp_ADDR_mail begsr
347500090213      *
347600090213     c                   clear                   Indirizzi
347700090213      *
347800090213      *  Lunghezza indirizzi presenti
347900090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
348000090213     c                   z-add     1             al_char           3 0
348100090213     c                   z-add     1             dal_char          3 0
348200090213     c                   z-add     1             tot_char          3 0
348300090213      *
348400090213     c     successivo    tag
348500090213      *
348600090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
348700090213      *   e aggiunge il dominio Bartolini + il (;) separatore
348800090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
348900090213     c                   eval      tot_char = al_char - dal_char
349000090213     c                   z-add     dal_char      Dac
349100090213     c                   z-add     tot_char      Luc
349200090213      *
349300090213     c                   clear                   Indir_Bartol     40
349400090213     c                   clear                   Indir_Parz       20
349500090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
349600090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
349700090213     c                             %Trim(bart_it)
349800090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
349900090213      *
350000090213     c                   if        al_char = Lun_Ind
350100090213     c                   goto      fine_indmail
350200090213     c                   else
350300090213     c                   eval      dal_char = ( al_char + 1 )
350400090213     c                   goto      successivo
350500090213     c                   end
350600090213      *
350700090213     C     fine_indmail  TAG
350800090213      *
350900090213     C                   ENDSR
351000090213     c*==================================================================*
351100090213      * Manda un Msg x E-mail
351200090213     c*==================================================================*
351300090213     c     Invio_Mail    begsr
351400090213      *
351500090213     c                   goto      non_inviare
351600090213      *
351700090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
351800090213     C* Inizializzo variabili
351900090213     C                   movel     *blanks       wrkEml          253
352000090213     C                   movel     *blanks       wrkMsg         5000
352100090213     C                   movel     *blanks       wrkOgg           44
352200090213      *
352300090213     C* Valorizzo i campi della e-m@ail - indirizzo
352400090213     C                   eval      wrkEml = Indirizzi
352500090213     C                   eval      wrkOgg = Oggetto
352600090213     C                   eval      wrkMsg = Messaggio
352700090213
352800090213     C                   call(e)   'TIS701C'
352900090213     C                   parm                    wrkEml
353000090213     C                   parm                    wrkOgg
353100090213     C                   parm                    wrkMsg
353200090213
353300090213     c     non_inviare   tag
353400090213     C*
353500090213     C                   ENDSR
353600090603     c*==================================================================*
353700090603      * Manda un Msg x E-mail a EDPAB
353800090603     c*==================================================================*
353900090603     c     Invio_Mail_AB begsr
354000090603      *
354100090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
354200090603     C* Inizializzo variabili
354300090603     C                   movel     *blanks       wrkEml          253
354400090603     C                   movel     *blanks       wrkMsg         5000
354500090603     C                   movel     *blanks       wrkOgg           44
354600090603      *
354700090603     C* Valorizzo i campi della e-m@ail - indirizzo
354800110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
354900110203     C                   eval      wrkEml = Indirizzi
355000090603     C                   eval      wrkOgg = Oggetto
355100090603     C                   eval      wrkMsg = Messaggio
355200090603
355300110203      ********
355400110203     C********           call(e)   'TIS701C'
355500110203     C********           parm                    wrkEml
355600110203     C********           parm                    wrkOgg
355700110203     C********           parm                    wrkMsg
355800110203     C*
355900110203      ** *****
356000110203     C                   call(e)   'TRTCT00R2'
356100110203     C                   parm                    wrkEml
356200110203     C                   parm                    wrkOgg
356300110203     C                   parm                    wrkMsg
356400110203     C*
356500090603     C*
356600090616     c                   if        %error
356700090616     c                   end
356800090616     C*
356900090603     C                   ENDSR
357000090211     ***********************************************************************
357100090211     **
357200090211     ** Send Message (QEZSNDMG) API
357300090211     **
357400090211     ***********************************************************************
357500090211     C     CalQEZSNDMG   BEGSR
357600090211      *
357700090211     ** Invio messaggio all'utente.
357800090211     C                   EVAL      SndMg03 = msgDSP
357900090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
358000090211     C                   EVAL      SndMg05(Jx) = User_msg
358100090211     C                   EVAL      SndMg06 = Jx
358200090211     C                   CLEAR                   QUSEC
358300090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
358400090211
358500090211     C                   CALL      'QEZSNDMG'
358600090211     C                   PARM                    SndMg01
358700090211     C                   PARM                    SndMg02
358800090211     C                   PARM                    SndMg03
358900090211     C                   PARM                    SndMg04
359000090211     C                   PARM                    SndMg05
359100090211     C                   PARM                    SndMg06
359200090211     C                   PARM                    SndMg07
359300090211     C                   PARM                    SndMg08
359400090211     C                   PARM                    Qusec
359500090211     C                   PARM                    SndMg10
359600090211     C                   PARM                    SndMg11
359700121105     c                   PARM                    SndMg12
359800090211      *
359900090211     C                   ENDSR
360000121105      *---------------------------------------------------------------*
360100121105      * CTRL caricamento schiere    PT                               -*
360200121105      *---------------------------------------------------------------*
360300121105     C     Check_PT      begsr
360400121105     C*
360500121105     c* Controllo riempimento schiera
360600121105     c                   clear                   trul0sds
360700121105     c                   eval      i0sski='CPT'
360800121105     c                   eval      i0sele=%elem(CPT)
360900121105     c                   eval      i0spie=x
361000121105     c                   eval      i0sfile='EDTAB00F'
361100121105     c                   eval      i0ssif=knsif
361200121105     c                   eval      i0spgm='TRTCT09R'
361300121113     c                   move      kpjbu         SvKpjbu
361400121113     c                   clear                   kpjbu
361500121105     c                   movel     trul0sds      kpjbu
361600121105     c                   call      'TRUL0SR'
361700121105     c                   parm                    kpjba
361800121113     c                   eval      kpjbu  =  SvKpjbu
361900121105     C*
362000121105     C                   ENDSR
362100090211     O*****************************************************************
362200090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
362300090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
362400090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
362500090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
