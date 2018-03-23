000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600090605      *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  DISPOR            *
000700090605      *                         cliente  DELTAPLUS J&J   con  DISPOR            *
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
039700060612
039800060612      ** Decodifica record a campi variabili
039900060612      * ?              /*---------------------- */
040000090205     c                   exsr      Decod_Segmento
040100090209      * ?              /*---------------------- */
040200090209
040300090213      *** ------------------------------------
040400090609      *** A FINE CICLO DI UN DETTAGLIO:   -->"RFF" e alla fine "UNT" x DISPOR DELTAPLUS
040500090213      *** ------------------------------------
040600090213      * ?              /*---------------------- */
040700090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
040800090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
040900090213      * ?              /*---------------------- */
041000090601      ***
041100090601      **
041200090616      *   OGNI DETTAGLIO è determinato dal RFF e alla fine da DOC e per ultimo da UNT.
041300090601      **
041400090603     c                   if           tipo_seg = 'UNT'                 or
041500090616     c                                tipo_seg = 'DOC'                 and
041600090611     c                                  se_trovato_NAD ='S'
041700090213      *
041800090213      *  Deve flaggare il dettaglio con dei problemi
041900090213     C                   if        Err_in_detta = 'S'
042000090213      **?              /*---------------------- */
042100090213     c                   exsr      DETta_Errato
042200090213      **?              /*---------------------- */
042300090213     c                   end
042400090213      *
042500060612      **  Se presente un errore nel record emette una segnalazione msg
042600090213     c                   if        se_errore = 'S'
042700090611      **
042800090611      **
042900090213     c                   else
043000090209      ******
043100090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043200090227     c                   if         se_trovato_NAD ='S'
043300060612      * ?              /*---------------------- */
043400090209     c                   exsr      Scrive_VAB
043500060612      * ?              /*---------------------- */
043600090227     c                   end
043700090227      *
043800090209      *  Problemi nella decodifica dei campi VAB/VAT
043900090213     c                   if        errori_in_Wrt = 'S'
044000090213      *
044100090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
044200090213     c                   exsr      Error_DET
044300090213      *
044400090213     c                   ROLBK
044500090213      *
044600090213     c                   else
044700090601      *
044800090213     c                   COMMIT
044900090601      *
045000090227      * Dopo aver confermato, con COMMIT
045100090227      *  si prepara per una nuova spedizione
045200090227      * ?                         --------------
045300090227     c                   exsr      Nuova_spediz
045400090227      * ?                         --------------
045500090213     c                   end
045600090209
045700090209     c                   end
045800090210      **
045900060620     c                   end
046000060612
046100060621      *  x errore bloccante nella composizione del VAB/VAT
046200060621     c                   if        err_bloccante ='S'
046300060621     C                   eval      vinFLG = '2'
046400090211     c                   eval      esito  = '2'
046500090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
046600090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
046700090603     C                             Bolle import in filiale - messaggio -
046800090603     C                             con UNB non presente in tabella PT controlla-
046900090603     C                             re EDI ricevuto su UPLOAD.'
047000090603     c                   exsr      invio_mail_AB
047100060621     c                   end
047200060612
047300100716     c                   update    Tivin000                             99
047400090211
047500090211      *** se non è riconosciuto l'UNB deve uscire direttamente
047600090211     c                   if        se_errore ='S'  and
047700090211     c                             tipo_seg = 'UNB'
047800090213     c                   eval      err_bloccante = 'S'
047900090211     c                   exsr      Msg_errato
048000090211     c                   LEAVE
048100090211     c                   endIF
048200090211      ***
048300060614     c                   endIF
048400060612
048500060612     c                   read      Tivin00r
048600060612     C                   ENDdo
048700060612      **
048800060612     c                   endSR
048900090211     C*----------------------------------------------------------------
049000090211     C*? AZZMSG - Azzera dati messaggio
049100090211     C*----------------------------------------------------------------
049200090211     C     AZZMSG        BEGSR
049300090211     C*
049400090211     C* Pulisco dati di work x foglio viaggio
049500090211     C                   MOVEL     *BLANKS       WNUMFV           35
049600090211     C                   MOVEL     *BLANKS       WNRCMR           35
049700090211     C                   Z-ADD     0             WDATPA            8 0
049800090211      *
049900090211      * pulisce gli RFF+FF di testata
050000090211     C                   clear                   $CLKSC            7 0
050100090211     C                   clear                   $CLTAR            3
050200090211     C                   clear                   $CLLNP            3 0
050300090211     C                   clear                   $CLNRS            2 0
050400090211     C                   clear                   $CLCTM            2
050500090211     C                   clear                   $CLBS1            1
050600090211     C                   clear                   $CLfnsp           1            *blk/S
050700090211      *
050800090211     C*  Inizializzo variabile new documento
050900090211     C                   Z-ADD     0             KCL
051000090211     C                   Z-ADD     0             WW                3 0
051100090211     ** Inizializza campo di work
051200090211     c                   move      'N'           WGID99
051300090211      * per controllare se almeno un record è stato importato sul VAB
051400090211     c                   clear                   Almeno_Uno        1
051500090213     c                   clear                   Almeno_Un_Due     1
051600090211     c                   eval      esito  = '0'
051700090211     C*
051800090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
051900090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
052000090603     c                   eval      NAD_destinatario_in_testata = *blank
052100090603     c                   eval      NAD_mittente_in_testata = *blank
052200090611     c                   eval      In_Testata = 'S'
052300090611     C*
052400090611     C                   clear                   Tes_VABdcm
052500090611     C*
052600090611     c                   clear                   tes_VABrmo
052700090611     c                   clear                   tes_VABnmo
052800090611     c                   clear                   tes_VABcmo
052900090611     C*
053000090611     c                   clear                   tes_VABrsd
053100090611     c                   clear                   tes_VABrd2
053200090611     c                   clear                   tes_VABind
053300090611     c                   clear                   tes_VABlod
053400090611     c                   clear                   tes_VABnzd
053500090611     C*
053600090611     c                   clear                   tes_Valuta
053700090611     c                   clear                   tes_VABcbo
053800090611     C*
053900090611     c                   clear                   Tes_VABdcr
054000090611     c                   clear                   Tes_VABtcr
054100090611     c                   clear                   Tes_VABhcr
054200090611     C*
054300090611     c                   clear                   tes_VABnas
054400090611     c                   clear                   tes_VABtsp
054500090611     C*
054600090611     c                   clear                   unh_VABRMA
054700090609     c                   clear                   unh_VABRMN
054800090611     C*
054900090611     C                   clear                   Tes_VABlnp
055000090611     C                   clear                   Tes_VABnrs
055100090611     C                   clear                   Tes_VABctm
055200090611     C*
055300090611     C                   clear                   VABfnsp_PT
055400090611     C*
055500090611     C                   MOVEA     *HIVAL        SGN
055600090603     C*
055700090211     C                   ENDSR
055800090210     C*----------------------------------------------------------------
055900090210      *? DECODIFICA DATI UNB
056000090210     C*----------------------------------------------------------------
056100090210     C     DATI_UNB      BEGSR
056200090210      *
056300090209      *  INIZIALIZZA  dati fondamentali messaggio
056400090209     C                   clear                   EDIDSPT
056500090209     C                   clear                   EDIDSPU
056600090209     C                   clear                   EDIDSPV
056700090209     C                   clear                   wnrDOC           35
056800090209     C                   move      'N'           PT_ok             1
056900090209      ** ---------------------------------------------------------------
057000090209      * quindi 1° cosa:
057100090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
057200090209      *     senza questo il messaggio NON è trascodificabile
057300090209     C                   MOVEL     unb0004       WCOD             35
057400090209     C                   MOVE      unb000720     WCOD
057500090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
057600090209     C                   Z-ADD     1             XX                3 0
057700090209     C     WCOD          LOOKUP    CPT(XX)                                32
057800090209      *
057900090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
058000090209     C                   If        *IN32 = *off
058100090209      * ******  Errore generale sul Messaggio
058200090209      * Msg di allerta Traduzione
058300090209     C                   EVAL      Indirizzi = CED_Bart
058400090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
058500090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
058600090209     C                             Bolle import in filiale - messaggio -
058700090209     C                             con UNB non presente in tabella PT controlla-
058800090209     C                             re EDI ricevuto su UPLOAD.'
058900090209      *
059000090209     c                   exsr      invio_mail
059100090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
059200090210     C                   eval      vinFLG = '2'
059300090210     C                   eval      se_errore   = 'S'
059400090210     c                   goto      end_UNB
059500090209      *
059600090209     C                   move      'N'           PT_ok             1
059700090209      *
059800090209      *             *------------------------------*
059900090209     C                   eLSe
060000090209      *             *------------------------------*
060100090209      * Se tutto OK su tab.PT:
060200090209     C                   move      'S'           PT_ok             1
060300090209      *
060400090209     C                   MOVEL     DPT(XX)       EDIDSPT
060500090209     C                   MOVEL     DPU(XX)       EDIDSPU
060600090209     C                   MOVEL     DPV(XX)       EDIDSPV
060700090226     C                   eval      Mittente       = unb0004
060800090226     C                   eval      Mitt_qualifier = unb000720
060900090226     C                   eval      Id_Messaggio   = UNB0022
061000090209      *
061100090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
061200090209      *  ricevuti:
061300090209     c                   movel     §PVema        mail_ind         44
061400090209      *
061500090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
061600090209     c                   if        mail_ind <> *blank
061700090209     c                   exsr      imp_ADDR_mail
061800090209     c                   end
061900090209      *
062000090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
062100090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
062200090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
062300090209     C                   if        §PULUN = *blank
062400090209     C                   eval      §PULUN = '00'
062500090209     c                   end
062600090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
062700090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
062800090209      *
062900090209     C                   Z-ADD     §PTKSC        WKSC              7 0
063000090611      *
063100090611      ** salva i dati principali
063200090611     C                   MOVEL     §PTLNP        Tes_VABlnp
063300090611     C                   MOVEL     §PTNRS        Tes_VABnrs
063400090611     C                   MOVEL     §PTCTM        Tes_VABctm
063500090209      *
063600090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
063700090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
063800090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
063900090209      *  i records trasmessi).
064000090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
064100090209      *
064200090209      *  In base al cod.trattamento merce reperisco tipo tratt.
064300090209     C                   CLEAR                   DS1B
064400090209     C                   Z-ADD     1             Y                 3 0
064500090611     C     Tes_VABctm    LOOKUP    CTM(Y)                                 32
064600090209     C     *IN32         IFEQ      '1'
064700090209     C                   MOVEL     DTM(Y)        DS1B
064800090209     C                   MOVEL     DS1B          WSVTRM           38
064900090209     C                   END
065000090209      *
065100090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
065200090209     C                   clear                   ACORAG
065300090209     C                   clear                   INDCAE
065400090209     C                   clear                   INDCAP
065500090209     C                   clear                   INDSTA
065600090209      * Decodifico partner
065700090209     C                   Z-ADD     1             KKUT
065800090209     C                   Z-ADD     KCI           KKCC
065900090209     C                   MOVE      §PTKSC        KKSC
066000090209     C     KACO          CHAIN     CNACO00F                           31
066100090209     C     KIND          CHAIN     CNIND00F                           31
066200090211      *
066300090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
066400090211      *   questa schiera è utilizzata anche per i clienti.
066500090211     C     §ptKSC        LOOKUP    KCL                                    39
066600090211     c                   If        *in39 = *off
066700090211     C                   add       1             WW
066800090211     C                   z-add     §ptKSC        KCL(WW)
066900090211     C                   Endif
067000090209      *
067100090209     C                   MOVEL     unb0017       WAA4              4 0
067200090209     C                   MOVE      WAA4          WAA2              2 0
067300090209     C                   MOVE      unb0017       WGG               2 0
067400090209     C                   MOVE      unb0017       WDTPRE            8 0
067500090611     C                   MOVEl     '20'          WDTPRE
067600090209     C                   MOVE      unb0019       WHMPRE            4 0
067700090209     C                   MOVE      unb0017       WDTMSG            6 0
067800090209     C                   MOVE      WAA2          WDTMSG
067900090209     C                   MOVEL     WGG           WDTMSG
068000090210      * potremmo
068100090210      * Utilizzare l'identificativo della trasmissione come CMR
068200090209     C                   IF        unb0022 <> *blank
068300090209     C                   MOVEL     unb0022       WNRDOC
068400090211     C                   MOVEL     unb0022       WNUMFV
068500090211     C                   MOVEL     unb0022       WNRCMR
068600090209     c                   end
068700090209      *
068800090209     c                   Endif
068900090209      **
069000090609     c     end_UNB       endSR
069100090609      * ?================================================================== */
069200090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
069300090609      * ?================================================================== */
069400090609     C     DATI_UNH      BEGSR
069500090609      *
069600090609      ** Riferimento del cliente
069700090609     C                   MOVEL     unh0062       WPRGSP           35
069800090609     C                   MOVEL     unh0062       WNSP             35
069900090609     C                   MOVEL     unh0062       WNUM             35
070000090609      * ?                                       --------
070100090609     C                   MOVEL     unh0062       unh_VABRMA
070200090609      * ?                                       --------
070300090609     C     ' '           SCAN      unh0062       WLUNGH            3 0
070400090609     C                   SUB       1             WLUNGH
070500090609     C                   EXSR      TSTNUM
070600090609     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
070700090609      * ?                                       --------
070800090609     C                   MOVEL     WNUMOK        unh_VABRMN
070900090609      * ?                                       --------
071000090609     C                   END
071100090609      **
071200090609     c     end_UNH       endSR
071300090609      * ?================================================================== */
071400090609     C*? INZIO del Messaggio
071500090609      * ?================================================================== */
071600090609     C     DATI_BGM      BEGSR
071700090609      *
071800090609      **
071900090609     c     end_BGM       endSR
072000090209      * ?================================================================== */
072100090609     C*? Tipo Servizio
072200090209      * ?================================================================== */
072300090609     C     Dati_TSR      BEGSR
072400090609      **
072500090609      **    Inviano solo spedizioni Espresso e non Priority
072600090611      **        tutte le spedizioni vanno come "Normali"
072700090611     c                   if        tsr4751 = 'D'
072800090611     c                   if        tsr4219 = 1 or tsr4219 = 2
072900090611     c                   eval      VABtsp = 'E'
073000090611     c                   end
073100090611     c                   end
073200090611      **
073300090611      **  se presente in testata se lo memorizza qualora non fosse presente
073400090611      **   nei singoli dettagli
073500090611     c                   if        In_Testata       = 'S'
073600090611     C                   eval       tes_VABtsp = VABtsp
073700090611     c                   end
073800090609      **
073900090609     c     end_TSR       endSR
074000090610      * ?================================================================== */
074100090611     C*? Accordi commerciali
074200090610      * ?================================================================== */
074300090610     C     DATI_CAG      BEGSR
074400090611      ****
074500090611      *  Attenzione questo segmento in alcuni messaggi potrebbe trovarsi solo
074600090611      *  in testata e non in dettaglio.
074700090611      ****
074800090610      *
074900090610      *  imposta il default dalla tabella PT se c'è
075000090610     C     §puPFA        ifNE      *blank
075100090610     c                   SELECT
075200090610      *  Porto Franco
075300090610     c                   WHEN      §puPFA = 'F'
075400090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
075500090611      *  Porto Assegnato
075600090611     C                   WHEN      §puPFA = 'A'
075700090611     C                   movel     '2'           VABCBO                         Senza C/Ass
075800090610     C                   ENDSL
075900090610     c                   end
076000090610      *
076100090610      *   Imposta o il codice o il metodo
076200090610      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
076300090611     c                   if        cag4237  <> *blank
076400090611      *
076500090610     C                   movel     UNB_Mittente  etbUNB
076600090611     C                   movel(p)  'CAG'         etbSGM
076700090611     C                   movel(p)  cag4237       etbVALSGM
076800090610     c     K_TAB1L       chain     edstbl01l
076900090610      * prova quindi senza l'UNB specifico del cliente
077000090610     c                   if        not %Found(edstbl01l)
077100090610     C                   clear                   etbUNB
077200090610     c     K_TAB1L       chain     edstbl01l
077300090610     c                   end
077400090610      *
077500090610     c                   if        %Found(edstbl01l)
077600090610     c                   SELECT
077700090610     c                   WHEN      ETBDATI = 'F'
077800090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
077900090611      *  Porto Assegnato
078000090611     c                   WHEN      VABCAS > *zero
078100090611     C                   movel     '2'           VABCBO                         Senza C/Ass
078200090610     C                   ENDSL
078300090610      *
078400090610     c                   end
078500090610     c                   endIF
078600090610      **
078700090610      **  Il tipo bolla non è presente
078800090611     c                   if         VABCBO = *blank
078900090611     C                   eval      vinMSG = 'CAG non rilevato'
079000090610     c                   exsr      Error_DET
079100090611     c                   goto      end_CAG
079200090610     c                   end
079300090611      **
079400090611      **  se presente in testata se lo memorizza qualora non fosse presente
079500090611      **   nei singoli dettagli
079600090611     c                   if        In_Testata       = 'S'
079700090611     C                   eval       tes_VABcbo = VABcbo                         Senza C/Ass.
079800090611     c                   eval      CAG_porto_in_testata = 'S'
079900090611     c                   end
080000090610      **
080100090610     c     end_CAG       endSR
080200090611      * ?================================================================== */
080300090611     C*?  Anagrafica del mittente e del destinatario
080400090611      * ?================================================================== */
080500090611     C     DATI_NAD      BEGSR
080600090611      *
080700090611      * Dati destinatario
080800090611     C                   IF        nad3035 = 'CN' or
080900090611     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
081000090611      *
081100090611      * Se trovato un indirizzo di destinatario
081200090611      *   abilita la scrittura del VAB e del VAT.
081300090611     c                   eval       se_trovato_NAD ='S'
081400090611      *
081500090611     C* Reperisco nazione corrispondente destinatario
081600090611     C                   Z-ADD     1             YY                3 0
081700090611     c                   eval      *in32 = *off
081800090611     C                   if        nad3207 <> *BLANKS
081900090611     C     nad3207       LOOKUP    ISO(YY)                                32
082000090611     C                   endif
082100090611      *
082200090611     C* Imposto dati destinatario
082300090615     C                   MOVEL     nad30361      VABRSD
082400090615     C                   if        nad30362 <> *LOVAL
082500090615     C                   MOVEL     nad30362      VABRD2
082600090611     C                   endif
082700090611      *
082800090611     C                   MOVEL     nad30421      VABIND
082900090630      *
083000090630     C                   MOVEL     nad3164       VABLOD
083100090630      *
083200090611      *
083300090611     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
083400090630      *   x DELTAPLUS li mettiamo in aggiunta alla 2°ragione sociale  VABRD2
083500090611     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
083600090611      *
083700090611     C* Verifico se c'è dello spazio per nella località
083800090630     C     '    '        SCAN      VABRD2        WI                4 0
083900090611      *
084000090630     C                   MOVEA     VABRD2        LOD
084100090611     C                   ADD       1             WI
084200090611     C                   MOVEA     NAD30422      LOD(WI)
084300090630     C                   MOVEA     LOD           VABRD2
084400090611     C                   ENDif
084500090611      *
084600090611     C                   If        *in32 = *on
084700090611     C                   MOVEL     C15(YY)       VABNZD
084800090611     C                   MOVEL     CPF(YY)       WFLCPF            2 0
084900090611     c                   Else
085000090611     C                   MOVEL     *BLANKS       VABNZD
085100090611     C                   Z-ADD     0             WFLCPF
085200090611     c                   Endif
085300090611      * Converto CAP
085400090611     c                   move      nad3251       cap09a            9
085500090611     C                   EXSR      CNVCAP
085600090611     C* Se in testata
085700090611     c                   if        In_Testata       = 'S'
085800090611     c                   eval       tes_VABrsd = VABrsd
085900090611     c                   eval       tes_VABrd2 = VABrd2
086000090611     c                   eval       tes_VABind = VABind
086100090611     c                   eval       tes_VABlod = VABlod
086200090611     c                   eval       tes_VABnzd = VABnzd
086300090611     c                   eval      NAD_destinatario_in_testata = 'S'
086400090611     c                   end
086500090615      *
086600090615     C* Ricerco CAP
086700090615     C*****WCAP          IFNE      *BLANKS
086800090615      *
086900090615     C     mse6799       IFEQ      'CGW'
087000090615      *
087100090615      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
087200090615     C                   CLEAR                   FNLV55DS
087300090615     C                   MOVEL     'P'           D55TPT
087400090615     C                   MOVEL     WOGGI         D55DRF
087500090615     C                   MOVEL     VABLNP        D55LNP
087600090615     C                   MOVEL     VABLNP        D55LIN
087700090615     C                   CALL      'FNLV55R'
087800090615     C                   PARM                    FNLV55DS
087900090615     C                   MOVE      D55TFP        COMTFP            3 0
088000090615      *
088100090615     C*  PASSO IL TERMINAL PARTENZA
088200090615     C                   CLEAR                   TISI95DS
088300090615     C                   Z-ADD     COMTFP        I95TFP
088400090615     C                   MOVEL     VABTSP        I95TSP
088500090615     C                   MOVEL     'S'           I95FRE
088600090615     C                   MOVEL     '3'           I95TCN
088700090615     C                   MOVEL     WCAP          I95CAP
088800090615     C                   MOVEL     VABNZD        I95NAR
088900090615     C                   MOVEL     VABLOD        I95LOC
089000090615     C                   MOVEL     VABIND        I95IND
089100090615      *
089200090615     C* Se gestita seconda linea di arrivo passo peso - volume
089300090615     C* e numero colli effettivo
089400090615     C     §PULNA        IFEQ      'S'
089500090615     C                   Z-ADD     VABPKB        I95LKG
089600090615     C                   Z-ADD     VABVLB        I95LMC
089700090615     C                   ELSE
089800090615     C                   END
089900090615     C*
090000090615     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
090100090615     C                   SELECT
090200090615     C     VABCBO        WHENEQ    '1 '
090300090615     C                   MOVEL     *BLANKS       I95FCA
090400090615     C                   MOVEL     'F'           I95TPO
090500090615     C     VABCBO        WHENEQ    '2 '
090600090615     C                   MOVEL     *BLANKS       I95FCA
090700090615     C                   MOVEL     'A'           I95TPO
090800090615     C     VABCBO        WHENEQ    '4 '
090900090615     C                   MOVEL     'S'           I95FCA
091000090615     C                   MOVEL     'F'           I95TPO
091100090615     C     VABCBO        WHENEQ    '6 '
091200090615     C                   MOVEL     'S'           I95FCA
091300090615     C                   MOVEL     'A'           I95TPO
091400090615     C                   OTHER
091500090615     C                   MOVEL     'F'           I95TPO
091600090615     C     VABCAS        IFGT      0
091700090615     C                   MOVEL     'S'           I95FCA
091800090615     C                   ELSE
091900090615     C                   MOVEL     *BLANKS       I95FCA
092000090615     C                   END
092100090615     C                   ENDSL
092200090615     C*
092300090615     C                   CALL      'TISI95R'
092400090615     C                   PARM                    TISI95DS
092500090615     C* Reperisco i dati da CAP
092600090615     C                   MOVEL     O95PRV        VABPRD
092700090615     C                   MOVEL     O95LNA        VABLNA
092800090615     C                   MOVEL     O95ZNC        VABZNC
092900090615     C                   MOVEL     WCAP          VABCAD
093000090615     C*
093100090615     C     O95ERR        IFNE      *BLANKS
093200090615     C     O95FIT        ANDEQ     'S'
093300090615     C*  Cap non trovato
093400090615     C                   eval      vinMSG = 'MSE dal peso (CAP) non trovato'
093500090615     c                   exsr      Error_DET
093600090615     C                   goto      end_NAD
093700090615     C                   END
093800090615     C                   END
093900090615      *
094000090615     C*****              END
094100090615      *
094200090615     C     WCAP          IFEQ      *BLANKS
094300090615      *
094400090615     C                   CLEAR                   VABPRD
094500090615     C                   CLEAR                   VABLNA
094600090615     C                   CLEAR                   VABZNC
094700090615     C                   CLEAR                   VABCAD
094800090615     C*  Cap non trovato
094900090615     C*******            eval      vinMSG = 'MSE dal peso (CAP) mancante'
095000090615     c*******            exsr      Error_DET
095100090615     C*******            goto      end_MSE
095200090615     C                   END
095300090611     C*
095400090611     C                   ENDIF
095500090611      *
095600090611      * Dati mittente  ---------------
095700090611     C                   IF        nad3035 = 'CO'
095800090611     C                   movel     nad30361      VABRMO
095900090611      *
096000090611      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
096100090611     C                   movel     §PTNAZ        VABNMO
096200090611      *
096300090611     C                   IF        nad3207 <> *blanks
096400090611     C                   eval      YY = 1
096500090611     C     nad3207       LOOKUP    ISO(YY)                                32
096600090611     C                   if         *in32 = *on
096700090611     C                   MOVEL     C15(YY)       VABNMO
096800090611     C                   endif
096900090611     C                   ENDIF
097000090611      *
097100090611      * Normalizzo CAP mittente originale
097200090611     C                   if        nad3251 <> *blanks
097300090611      *
097400090611     C                   clear                   T
097500090611     C                   clear                   S
097600090611     C                   MOVEA     *BLANKS       CAPM
097700090611     C                   MOVEA     nad3251       CAP9
097800090611     C                   DO        9             T
097900090611     C     CAP9(T)       IFNE      *BLANKS
098000090611     C                   ADD       1             S
098100090611     C                   MOVE      CAP9(T)       CAPM(S)
098200090611     C                   ENDIF
098300090611     C                   ENDDO
098400090611     C*
098500090611      *     Controllo validità CAP
098600090611     C                   MOVEA     CAPM          VABCMO
098700090611      *
098800090611     C                   CLEAR                   TISI95DS
098900090611     C                   MOVEL     VABCMO        I95CAP
099000090611     C                   MOVEL     VABNMO        I95NAR
099100090611     C                   MOVEL     WOGGI         I95DAT
099200090611     C                   MOVEL     '3'           I95TCN
099300090611     C                   CALL      'TISI95R'
099400090611     C                   PARM                    TISI95DS
099500090611      *     Errore
099600090611     C     O95ERR        IFNE      *BLANKS
099700090611     C                   MOVEL     INDCAE        VABCMO
099800090611     C                   MOVEL     §PTNAZ        VABNMO
099900090611     C                   END
100000090611      *
100100090611     c                   endIF
100200090611     C* Se in testata
100300090611     c                   if        In_Testata       = 'S'
100400090611     c                   eval       tes_VABrmo = VABrmo
100500090611     c                   eval       tes_VABnmo = VABnmo
100600090611     c                   eval       tes_VABcmo = VABcmo
100700090611     c                   eval       NAD_mittente_in_testata = 'S'
100800090611     c                   end
100900090611      *
101000090611     C                   END
101100090611      *
101200090611      * NOn c'è l'identificativo bolla del cliente
101300090611     C                   IF        nad3035 = 'CN'
101400090611     C                   if          vabRSD = *blank or
101500090611     C                               vabIND = *blank or
101600090611     C                               vabLOD = *blank
101700090611     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
101800090611     c                   exsr      Error_DET
101900090611     c                   goto      end_NAD
102000090611     C                   ENDIF
102100090611     C                   ENDIF
102200090611      *
102300090611     c     end_NAD       endSR
102400090611      * ?================================================================== */
102500090611     C*? Imposta le date
102600090611      * ?================================================================== */
102700090611     C     DATI_DTM      BEGSR
102800090611     C*
102900090611     C*  Se data documento  memorizzo numero e data x CMR
103000090611     C     dtm2005       IFEQ      'DEP'
103100090611     C                   MOVEL     '20'          W_DATA8           8            Data
103200090615     C                   MOVE      dtm2001       W_DATA8
103300090611     C                   MOVEL     W_DATA8       WDATA            35            Data
103400090611     C                   MOVEL     '102'         WFORM             3            Formato
103500090611     C                   EXSR      CNVDAT
103600090611     C                   MOVEL     WCAMG         Tes_VABdcm                     Data CMR
103700090611     C     WERRO         IFEQ      'S'
103800090611     C                   eval      vinMSG = 'DTM data msg errata'
103900090611     c                   exsr      Error_DET
104000090611     c                   goto      end_DTM
104100090611     C                   END
104200090611     C                   END
104300090611      **
104400090611     C*  Se data consegna richiesta imposto già sui campi del VAB
104500090611     C     dtm2005       IFEQ      'DAD'
104600090611     C                   MOVEL     '20'          W_DATA8           8            Data
104700090615     C                   MOVE      dtm2001       W_DATA8
104800090611     C                   MOVEL     W_DATA8       WDATA            35            Data
104900090611     C                   MOVEL     '102'         WFORM             3            Formato
105000090611     C                   EXSR      CNVDAT
105100090828     C***  In accordo con il cliente NON si prende + la SUA Data cons.rich.
105200090828     C***  poichè il cliente inviava una data uguale a quella della spedizione
105300090828     C***  e non poteva modificare nulla nel suo sistema.
105400090828     C***   autorizzato da Pantalena il  28/08/2009
105500090828     C*******            MOVEL     WCAMG         VABDCR                         Data cons.rich.
105600090828     C*******            MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
105700090828     C*******            MOVEL     WHMS          VABHCR                         Ora  cons.rich.
105800090611     C     WERRO         IFEQ      'S'
105900090611     C                   eval      vinMSG = 'DTM DEP - data errata'
106000090611     c                   exsr      Error_DET
106100090611     c                   goto      end_DTM
106200090615     C                   ELSE
106300090615     C* Se in testata
106400090615     c                   if        In_Testata       = 'S'
106500090828     C*******            eval       Tes_VABdcr = VABDCR                          Data cons.rich.
106600090828     C*******            eval       Tes_VABtcr = VABTCR                          Tipo cons.rich.
106700090828     C*******            eval       Tes_VABhcr = VABHCR                          Ora  cons.rich.
106800090615     c                   endIf
106900090615     C                   END
107000090611     C                   END
107100090611      **
107200090611     c     end_DTM       endSR
107300090611      * ?================================================================== */
107400090611     C*? Descrizione della Merce (NATURA MERCE)
107500090611      * ?================================================================== */
107600090611     C     DATI_GDS      BEGSR
107700090611     C*
107800090611     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
107900090611     c                   if        gds70021 <> *blank
108000090611     C                   MOVEL     gds70021      Tes_VABnas
108100090611     c                   end
108200090611      **
108300090611     c     end_GDS       endSR
108400090609      * ?================================================================== */
108500090611     C*?  Dettagli sul trasporto
108600090609      * ?================================================================== */
108700090611     C     DATI_TDT      BEGSR
108800090609      *
108900090611      **
109000090611     c     end_TDT       endSR
109100090611      * ?================================================================== */
109200090611     C*? Beni Pericolosi
109300090611      * ?================================================================== */
109400090611     C     DATI_DGS      BEGSR
109500090611      **
109600090611      **
109700090611     c     end_DGS       endSR
109800090611      * ?================================================================== */
109900090611     C*? Documentazione
110000090611      * ?================================================================== */
110100090611     C     DATI_DOC      BEGSR
110200090611      **
110300090611      * Numero di ricevuta
110400090611     C                   if        doc1001 = 'WBL' and doc1791 = 'ACG'
110500090611     C                                     and doc1004 <> *blank
110600090611      *
110700090611     C                   movel     doc1004       WCU
110800090611     C                   movel     WCU           WNUM             35
110900090611     C     ' '           scan      WCU           WLUNGH
111000090611     C                   sub       1             WLUNGH
111100090611     C                   EXSR      TSTNUM
111200090611     C                   If          WOKNUM = 'S'                               Ctrl numerico
111300090611     C                   movel     WNUMOK        VABRMN
111400090611     C                   movel     'WBL'         WRMN
111500090611     C                   Endif
111600090611     C                   ENDIF
111700090611      **
111800090611     c     end_DOC       endSR
111900090611      * ?================================================================== */
112000090611     C*? Testata intermedia Inizio e Fine Dettaglio
112100090611      * ?================================================================== */
112200090611     C     DATI_UNS      BEGSR
112300090611      *
112400090615     c                   eval      In_Testata =' '
112500090615      *
112600090611      *   Inizio Dettaglio
112700090611     c                   if         UNS0081  = 'D'
112800090611     c                   end
112900090611      *
113000090611      *   Fine   Dettaglio
113100090611     c                   if         UNS0081  = 'S'
113200090611     c                   end
113300090609      **
113400090609     c     end_UNS       endSR
113500090609      * ?================================================================== */
113600090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
113700090609      * ?================================================================== */
113800090609     C     Nuova_spediz  BEGSR
113900090609      **
114000090227      * Pulisco campi del file
114100090227     c                   z-add     vnrec         SAVNUM_RECord
114200090227     C                   CLEAR                   EDiVAB00
114300090227     C                   CLEAR                   EDiVAT00
114400090227     C                   CLEAR                   EDiVAD00
114500090611      *
114600090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
114700090227     C                   MOVEL     'N'           WGID99            1
114800090611      *
114900090227     C                   clear                   XY                5 0
115000090227     C                   clear                   §§                5 0
115100090611      *
115200090227     C                   clear                   WNSP             35
115300090227     C                   MOVEA     *HIVAL        SGN
115400090611      *
115500090227     C*  Azzero codice cliente - tariffa
115600090227     C                   Z-ADD     0             WTOCOC           16 0
115700090227     C                   Z-ADD     0             WTOPLC           16 2          ???
115800090227     C                   Z-ADD     0             WTOPNC           16 2          ???
115900090611      *
116000090611     C                   clear                   QUA
116100090611     C                   clear                   FTX
116200090611      *
116300090227     C*  Azzero codice cliente - tariffa
116400090227     C                   clear                   WCLKSC
116500090227     C                   clear                   WCLTAR
116600090227     C                   clear                   WCLLNP
116700090227     C                   clear                   WCLNRS
116800090227     C                   clear                   WCLCTM
116900090227     C                   clear                   WCLBS1
117000090227     C                   clear                   WCLfnsp                        *blk/S
117100090227     C                   clear                   WNOT1            35
117200090227     C                   clear                   WNOT2            35
117300090227      *
117400090227     C* pulisce campi di work
117500090227     c                   clear                   watrde           35
117600090227     c                   clear                   wattel           25
117700090227     C                   clear                   WRMN              3
117800090227      *
117900090227      * Un errore nel dettaglio
118000090227     C                   clear                   Err_in_detta      1
118100090227     c                   clear                   errori_in_Wrt     1
118200090227     c                   clear                   se_trovato_NAD    1
118300090227      **
118400090611      **  imposta i dati x la testata
118500090611     c                   exsr      DATI_TESTATA
118600090611      **
118700090227     c                   endSR
118800090609      * ?================================================================== */
118900090609     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
119000090609      * ?================================================================== */
119100090611     C     DATI_TESTATA  BEGSR
119200090609      *
119300090611      **  IMPOSTA i campi dalla testata
119400090611     C                   eval       VABLNP = Tes_VABlnp
119500090611     C                   eval       VABNRS = Tes_VABnrs
119600090611     C                   eval       VABCTM = Tes_VABctm
119700090611      **
119800090611     C                   eval      VABdcm = Tes_VABdcm
119900090611     C*
120000090611     c                   eval      VABvca = tes_Valuta
120100090611     c                   eval      VABcbo = tes_VABcbo
120200090611     C*
120300090828     C*******   Come da Accordi presi con il cliente la Data consegna richiesta
120400090828     C*******    NON verrà presa in considerazione
120500090828     c*******            eval      VABdcr = Tes_VABdcr
120600090828     c*******            eval      VABtcr = Tes_VABtcr
120700090828     c*******            eval      VABhcr = Tes_VABhcr
120800090611     C*
120900090611     c                   eval      VABnas = tes_VABnas
121000090611     c                   eval      VABtsp = tes_VABtsp
121100090611     C*
121200090611      * Se Mittente o Destinatario presi dalla Testata del messaggio:
121300090611     C* Se in testata
121400090611     c                   if        NAD_destinatario_in_testata = 'S'
121500090611     c                   eval         VABrsd =  tes_VABrsd
121600090611     c                   eval         VABrd2 =  tes_VABrd2
121700090611     c                   eval         VABind =  tes_VABind
121800090611     c                   eval         VABlod =  tes_VABlod
121900090611     c                   eval         VABnzd =  tes_VABnzd
122000090611     c                   end
122100090611     C* Se in testata
122200090611     c                   if        NAD_mittente_in_testata = 'S'
122300090611     c                   eval         VABrmo = tes_VABrmo
122400090611     c                   eval         VABnmo = tes_VABnmo
122500090611     c                   eval         VABcmo = tes_VABcmo
122600090611     c                   end
122700090611      **
122800090611     c                   endSR
122900090611      * ?================================================================== */
123000090611     C*?  Riferimento Spedizione del cliente
123100090611      * ?================================================================== */
123200090611     C     DATI_RFF      BEGSR
123300090611      *
123400090609      *  Prepara una nuova spedizione
123500090609      * ?                         --------------
123600090609     c                   exsr      Nuova_spediz
123700090609      * ?                         --------------
123800090609     C                   clear                   WAGE             35
123900110224     C                   clear                   WFF              35
124000110224     C                   clear                   WCU              35
124100090609      *
124200090609      *  Trascodifico riferimento spedizione
124300090609     c                   if        RFF1153 = 'SEN' and RFF1154 <> *BLANKS
124400090609     C                   movel(p)  RFF1154       WAGE             35
124500090609     c                   end
124600090609      **
124700090609     C                   IF         WAGE <> *BLANKS
124800090609     C                   movel     WAGE          WNSP             35
124900090609     C                   movel     WAGE          WNUM             35
125000090609     C     ' '           scan      WAGE          WLUNGH            3 0
125100090609     C                   sub       1             WLUNGH
125200090609     C                   EXSR      TSTNUM
125300090609     C                   IF         WOKNUM = 'S'                                Ctrl numerico
125400090609     C                   movel     WAGE          VABRMA
125500090609     C                   IF         WRMN = 'SEN' or WRMN = *blanks
125600090609     C                   movel     WNUMOK        VABRMN
125700090609     C                   movel     'SEN'         WRMN
125800090609     C                   ENDIF
125900090609     C                   ENDIF
126000090609     C                   ENDIF
126100090609      *
126200090609      * Codice Cliente di riferimento
126300090609     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
126400090609     C                   movel     RFF1154       WFF
126500090609     c                   END
126600090609      *
126700090609      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
126800090609     C     WFF           IFNE      *BLANKS
126900090609     C                   eval      XX = 1
127000090609     C                   movel     §PTKSC        WCCL             35
127100090609     C                   movel     WFF           WCOM28           28
127200090609     C                   move      WCOM28        WCCL
127300090609     C     WCCL          lookup    CCL(XX)                                34
127400090609     C     *IN34         IFEQ      '1'
127500090609     C                   movel     DCL(XX)       EDIDSCL
127600090611     C* Se in testata
127700090611     c                   if        In_Testata       = 'S'
127800090611      *
127900090611     C                   z-add     §CLKSC        $CLKSC
128000090611     C                   movel     §CLTAR        $CLTAR
128100090611     C                   z-add     §CLLNP        $CLLNP
128200090611     C                   z-add     §CLNRS        $CLNRS
128300090611     C                   movel     §CLCTM        $CLCTM
128400090611     C                   movel     §CLBS1        $CLBS1
128500090611     C                   movel     §CLnsp        $CLfnsp
128600090611      *
128700090611     c                   else
128800090611      *
128900090609     C                   z-add     §CLKSC        WCLKSC            7 0
129000090609     C                   movel     §CLTAR        WCLTAR            3
129100090609     C                   z-add     §CLLNP        WCLLNP            3 0
129200090609     C                   z-add     §CLNRS        WCLNRS            2 0
129300090609     C                   movel     §CLCTM        WCLCTM            2
129400090609     C                   movel     §CLBS1        WCLBS1            1
129500090609     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
129600090611      *
129700090611     c                   end
129800090609      *
129900090609      * imposta la LNP   x il dettaglio specifco
130000090611      * imposta la Serie x il dettaglio specifco
130100090609     c                   eval      VABlnp = §CLlnp
130200090609     c                   eval      VABnrs = §CLnrs
130300090609      *
130400090609      *  In base al cod.trattamento merce reperisco tipo tratt.
130500090609      *    per un dettaglio specifico.
130600090609     C                   CLEAR                   DS1B
130700090609     C                   Z-ADD     1             Y                 3 0
130800090609     C     §CLctm        LOOKUP    CTM(Y)                                 32
130900090609     C     *IN32         IFEQ      '1'
131000090609     C                   MOVEL     DTM(Y)        DS1B
131100090609     C                   END
131200090609      *
131300090609      * Se il cliente non era sulla schiera lo aggiunge
131400090609     C     §CLKSC        lookup    KCL                                    39
131500090609     C                   if        *in39 = *off
131600090609     C                   add       1             WW
131700090609     C                   z-add     §CLKSC        KCL(WW)
131800090609     c                   endif
131900090609      *
132000090609     C                   ENDIF
132100090609     C                   ENDIF
132200090609      *
132300090611      * NON c'è l'identificativo bolla del cliente
132400090611     c                   if        (RFF1153 = 'SEN' and RFF1154 = *BLANKS)
132500090611     C                   eval      vinMSG = 'RFF riferimento cliente assente'
132600090609     c                   exsr      Error_DET
132700090609     c                   goto      end_RFF
132800090609     C                   ENDIF
132900090609      *
133000090609     c     end_RFF       endSR
133100090611      * ?================================================================== */
133200090611     C*?  Dati di dettaglio della spedizione e dei colli
133300090611      * ?================================================================== */
133400090611     C     DATI_GID      BEGSR
133500090611      *
133600090611     C* Il numero dei colli presenti nella spedizione
133700090611     C     GID706520     IFEQ      21
133800090611     C                   MOVEL     'S'           WGID99
133900090611     C                   z-add     GID722420     VABNCL
134000090611     C                   ELSE
134100090611     C     WGID99        IFEQ      'N'
134200090611     C                   z-add     gid722420     WTNCL             5 0
134300090611     C                   ADD       WTNCL         VABNCL
134400090611     C                   END
134500090611     C                   END
134600090611      *
134700090611     C                   ADD       VABNCL        WTOCOC           16 0
134800090611      *
134900090611     c     end_GID       endSR
135000090611      * ?================================================================== */
135100090611     C*?  Dati di dettaglio misure spedizione e colli
135200090611      * ?================================================================== */
135300090611     C     DATI_MSE      BEGSR
135400090611      *
135500090611     C*  Peso
135600090611     C     mse6799       IFEQ      'CGW'
135700090611      *
135800090611     C*  Controllo se ho valore totale o parziale
135900090611      *   sempre il Lordo
136000090611     C     mse6410       IFeq      'KG '
136100090611     C     WGID99        IFEQ      'S'
136200090611     C                   move      mse6314       mse6314_2        18 2          Peso
136300090611     C                   Z-ADD     mse6314_2     VABPKB                         Peso
136400090611     C                   Else
136500090611     C                   move      mse6314       mse6314_2                      Peso
136600090611     C                   ADD       mse6314_2     VABPKB                         Peso
136700090611     C                   End
136800090611     C                   ENDif
136900090615      *
137000090615     C                   END
137100090611      ***
137200090611     c     end_MSE       endSR
137300090209      * ?================================================================== */
137400090213     C*?  C.O.D. Monetary Amount
137500090209      * ?================================================================== */
137600090609     C     DATI_VAL      BEGSR
137700090210      *
137800090216     C* Controllo se campo importo numerico
137900090216      * con 3 decimali
138000090611     C     val5004       IFNE      *zeros
138100090216      *
138200090611     C                   If         val5297 = 'DE '
138300090630     C     val5004       div       100           VABIAS                         IMPORTO
138400090611     C                   MOVEL     val6345       VABVAS                         ASSICURATO
138500090216     C                   END
138600090216      *
138700090611     C                   If         §PUCAS = 'S' and val5297 = 'RE '            C/Assegno
138800090630     C     val5004       div       100           VABCAS
138900090611     C                   MOVEL     val6345       VABVCA
139000090611      *
139100090216     C                   END
139200090216      *
139300090611     C                   END
139400090213      *
139500090611     c     end_VAL       endSR
139600091104      * ?================================================================== */
139700091104     C*?  Contatto a cui far riferimento
139800091104      * ?================================================================== */
139900091104     C     DATI_CTA      BEGSR
140000091104      *
140100091104      *  memorizza il riferimento di consegna x il destinatario
140200091104      *  memorizza il riferimento di consegna nr.telefonico
140300091104     C     cta3412       ifne      *BLANKS
140400091104     c                   movel     cta3412       watrde
140500091104     c                   end
140600091104      *
140700091104     c                   endSR
140800091104      * ?================================================================== */
140900091104     C*?  Contatto telefonico  a cui far riferimento
141000091104      * ?================================================================== */
141100091104     C     DATI_COM      BEGSR
141200091104      *
141300091104     C     com3148       ifne      *BLANKS
141400091104     c                   movel     com3148       wattel
141500091104     c                   end
141600091104      *
141700091104     C                   ENDSR
141800090213      * ?================================================================== */
141900090213     C*?  FREE TEXT
142000090213      * ?================================================================== */
142100090611     C     DATI_TXT      BEGSR
142200090213      *
142300090611      * ? solo se si tratta di note di consegna
142400100311     C*******            if        txt0077 ='DEL'  and
142500100311     C*******                      txt0078 <> *blank
142600090629      *
142700100311     C                   if        txt0077 ='DEL'
142800100311      *
142900090702     c                   movel     txt0078       tipo_payment      2
143000090629      *
143100090629     c                   If         tipo_payment = 'TM'
143200100311     C                               and  %subst(txt0078:3:1)=' '
143300090629     C                   MOVEL     'TM'          VABTIC
143400090629      *
143500090629     c                   elseIf     tipo_payment = 'BM'
143600100311     C                               and  %subst(txt0078:3:1)=' '
143700090629     C                   MOVEL     'BM'          VABTIC
143800090629      *
143900090629     c                   elseIf     tipo_payment = 'CM'
144000100311     C                               and  %subst(txt0078:3:1)=' '
144100090629     C                   MOVEL     'CM'          VABTIC
144200090629      *
144300090629     c                   elseIf     tipo_payment = '  '
144400100311     C                               and  txt0078 = *BLANK
144500090629     C                   MOVEL     '  '          VABTIC
144600090629      *
144700100311     c                   elseIF     txt0078 <> *BLANK
144800090629      *
144900090611     C     WNOT1         IFEQ      *BLANKS
145000090702     C                   MOVEL     txt0078       WNOT1
145100090702     C                   MOVE      txt0078       WNOT2
145200090611     C                   ELSE
145300090611     C     WNOT2         IFEQ      *BLANKS
145400090702     C                   MOVEL     txt0078       WNOT2
145500090611     C                   END
145600090611     C                   END
145700090629      *
145800090629     c                   endIf
145900090629      *
146000090629     c                   end
146100090213      *
146200090611     c     end_TXT       endSR
146300090210      * ?================================================================== */
146400090210     C*?  Dati di dettaglio Codice a barre Segnacolli
146500090210      * ?================================================================== */
146600090210     C     DATI_PCI      BEGSR
146700090210      *
146800090611      *  Contiene i singoli segnacolli
146900090611     c                   if        PCI4233 ='23 '
147000090611      *
147100090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
147200090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
147300090210     C     §1BF12        IFEQ      'E'
147400090210     C                   EXSR      SEGNEE
147500090210     C                   ELSE
147600090210      *
147700090210      *  La prima volta controllo congruità numero segnacollo
147800090210     C     §PUBS1        IFNE      *BLANKS
147900090210     C     VABnrs        ANDNE     0
148000090210      *  Se il codice trattamento merce prevede che segnacollino i
148100090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
148200090210     C     §1BFG7        IFEQ      'S'
148300090210     C     §1BFG1        OREQ      'S'
148400090210      *  calcolo segnacollo
148500090210     C                   EXSR      SGNELA
148600090210     C                   END
148700090210     C                   END
148800090210      *
148900090210     C                   END
149000090210      *
149100090611     C                   ENDif
149200090611      *
149300090210     c     end_PCI       endSR
149400090211     C*----------------------------------------------------------------
149500090211      *? dati finali del dettaglio    UNT
149600090211     C*----------------------------------------------------------------
149700090211     C     DATI_UNT      BEGSR
149800090211      *
149900090213     c                   eval      NUM_RECord = vnREC
150000090213     c                   z-add     vnrec         last_RECord
150100090211      **
150200090211     c                   endSR
150300090211     C*----------------------------------------------------------------
150400090211      *? dati finali     UNT
150500090211     C*----------------------------------------------------------------
150600090211     C     DATI_UNZ      BEGSR
150700090211      *
150800090211      **
150900090211     c                   endSR
151000090210      *----------------------------------------------------------------
151100090611      *? Input segnacolli su schiera di 10 elementi
151200090210      *----------------------------------------------------------------
151300090210     C     INP_Segnacol  BEGSR
151400090210      *
151500090210     c                   clear                   quanti_PCI        3 0
151600090210     c                   if        §puEL1 = 0
151700090210     c                   z-add     10            quanti_PCI
151800090210     c                   else
151900090210     c                   z-add     §puEL1        quanti_PCI
152000090210     c                   end
152100090210      *
152200090210      *  Pulisce la schiera dei segnacolli da elaborare
152300090210     c                   clear                   x30
152400090210     c                   z-add     0             Nr_x30            3 0
152500090210      *
152600090210      *   riporto tutto sulla schiera generica X30
152700090210      *
152800090210     c                   do        10            limite            3 0
152900090210      * Limita quanti elementi passati in ogni PCI
153000090210      *   devono essere considerati
153100090210     c                   if        limite > quanti_PCI
153200090210     c                   Leave
153300090210     c                   end
153400090210      *
153500090210      *  Carica la schiera generale di tutti i segnacolli
153600090210     c                   if        D30_1(limite) <> *blank
153700090210     c                   add       1             Nr_x30
153800090210     c                   eval      X30(Nr_x30) = D30_1(limite)
153900090210     c                   end
154000090210     c                   enddo
154100090210      *
154200090210     C                   ENDSR
154300090210      *----------------------------------------------------------------
154400090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
154500090210      *----------------------------------------------------------------
154600090210     C     SEGNEE        BEGSR
154700090210      *
154800090210     c                   exsr      INP_Segnacol
154900090210      *
155000090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
155100090210     C                   DO        10            x                              --- 01 ---
155200090210      *
155300090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
155400090210     C     x30(X)        ANDNE     *LOVAL
155500090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
155600090210     C                   ADD       1             §§
155700090210     C                   MOVEL     x30(X)        SGE(§§)
155800090210     C                   END                                                    --- 03 ---
155900090210      *
156000090210     C                   END                                                    --- 02 ---
156100090210      *
156200090210     C                   ENDSR
156300090210     C*----------------------------------------------------------------
156400090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
156500090210     C*----------------------------------------------------------------
156600090210     C     SGNELA        BEGSR
156700090210      *
156800090210     c                   exsr      INP_Segnacol
156900090210     C*
157000090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
157100090210     C*  in base al numero elementi validi carico sgn.
157200090210     C                   DO        10            X                              --- 01 ---
157300090210     C*
157400090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
157500090210     C     x30(X)        ANDNE     *LOVAL
157600090210     C*
157700090210     C                   CLEAR                   DSSGN
157800090210     C*  Controllo se devo ricevere la linea di partenza
157900090210     C                   MOVEL     'N'           WERRO             1
158000090210     C     §PULP1        IFGT      0                                            --- 03 ---
158100090210     C                   EXSR      REPLNP
158200090210     C                   END                                                    --- 03 ---
158300090210     C*  Controllo se devo ricevere la linea di arrivo
158400090210     C     §PULA1        IFGT      0                                            --- 03 ---
158500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
158600090210     C                   EXSR      REPLNA
158700090210     C                   END                                                    --- 03 ---
158800090210     C*  Controllo se devo ricevere il numero di serie
158900090210     C     §PUNR1        IFGT      0                                            --- 03 ---
159000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
159100090210     C                   EXSR      REPNRS
159200090210     C                   END                                                    --- 03 ---
159300090210     C*  Controllo se devo ricevere il numero segnacollo
159400090210     C     §PUSG1        IFGT      0                                            --- 03 ---
159500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
159600090210     C                   EXSR      REPSGN
159700090210     C                   END                                                    --- 03 ---
159800090210     C*  Controllo se devo ricevere la zona di consegna
159900090210     C     §PUZN1        IFGT      0                                            --- 03 ---
160000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
160100090210     C                   EXSR      REPZNC
160200090210     C                   END                                                    --- 03 ---
160300090210     C*  Imposto da VAB i dati a zero se mi è stato passato
160400090210     C*  numero segnacollo valido
160500090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
160600090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
160700090210     C                   Z-ADD     VABLNP        SGNLNP
160800090210     C                   END                                                    --- 04 ---
160900090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
161000090210     C                   Z-ADD     VABLNA        SGNLNA
161100090210     C                   END                                                    --- 04 ---
161200090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
161300090210     C                   Z-ADD     VABNRS        SGNNRS
161400090210     C                   END                                                    --- 04 ---
161500090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
161600090210     C                   Z-ADD     VABZNC        SGNZNC
161700090210     C                   END                                                    --- 04 ---
161800090210      *
161900090210     C*  mi reimposto LNA e zona da segnacollo
162000090210     C                   Z-ADD     SGNLNA        VABLNA
162100090210     C                   Z-ADD     SGNZNC        VABZNC
162200090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
162300090210     C                   ADD       1             XY
162400090210     C                   MOVE      SGNNSC        SGN(XY)
162500090210     C                   END                                                    --- 03 ---
162600090210     C*
162700090210     C                   END                                                    --- 02 ---
162800090210     C*
162900090210     C                   END                                                    --- 01 ---
163000090210      *
163100090210     C                   ENDSR
163200090210     C*----------------------------------------------------------------
163300090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
163400090210     C*----------------------------------------------------------------
163500090210     C     REPLNP        BEGSR
163600090210     C*
163700090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
163800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
163900090210     C*  dal numero segnacollo passatomi dal partner
164000090210     C                   Z-ADD     §PULP1        WP                2 0
164100090210     C     3             SUBST     x30(X):WP     WLNP              3
164200090210     C*
164300090210     C*  controllo che la lnp calcolata sia numerica
164400090210     C                   TESTN                   WLNP                 98
164500090210     C*
164600090210     C*  se è numerica la carico
164700090210     C     *IN98         IFEQ      '1'
164800090210     C                   MOVE      WLNP          SGNLNP
164900090210     C                   ELSE
165000090210     C*  se non è numerica stampo errore
165100090210     C                   MOVEL     'S'           WERRO             1
165200090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
165300090213     c                   exsr      Error_DET
165400090210     C                   goto      end_LNP
165500090210     C                   END
165600090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
165700090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
165800090210     C     SGNLNP        IFNE      VABLNP
165900090210     C                   MOVEL     'S'           WERRO             1
166000090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
166100090210     c                             a della Bolla'
166200090213     c                   exsr      Error_DET
166300090210     C                   goto      end_LNP
166400090210     C                   END
166500090210     C*
166600090210     C     end_LNP       ENDSR
166700090210     C*----------------------------------------------------------------
166800090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
166900090210     C*----------------------------------------------------------------
167000090210     C     REPLNA        BEGSR
167100090210     C*
167200090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
167300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
167400090210     C*  dal numero segnacollo passatomi dal partner
167500090210     C                   Z-ADD     §PULA1        WP                2 0
167600090210     C     3             SUBST     x30(X):WP     WLNA              3
167700090210     C*  controllo che la lna calcolata sia numerica
167800090210     C                   TESTN                   WLNA                 98
167900090210     C*  se è numerica la carico
168000090210     C     *IN98         IFEQ      '1'
168100090210     C                   MOVE      WLNA          SGNLNA
168200090210     C                   ELSE
168300090210     C*  se non è numerica stampo errore
168400090210     C                   MOVEL     'S'           WERRO             1
168500090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
168600090213     c                   exsr      Error_DET
168700090210     C                   goto      end_LNA
168800090210     C                   END
168900090210     C*
169000090210     C     end_LNA       ENDSR
169100090210     C*----------------------------------------------------------------
169200090210     C*? REPNRS - Reperisce numero serie
169300090210     C*----------------------------------------------------------------
169400090210     C     REPNRS        BEGSR
169500090210     C*
169600090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
169700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
169800090210     C*  dal numero segnacollo passatomi dal partner
169900090210     C                   Z-ADD     §PUNR1        WP                2 0
170000090210     C     2             SUBST     x30(X):WP     WNRS              2
170100090210     C*  controllo che il nrs calcolata sia numerico
170200090210     C                   TESTN                   WNRS                 98
170300090210     C*  se è numerica la carico
170400090210     C     *IN98         IFEQ      '1'
170500090210     C                   MOVE      WNRS          SGNNRS
170600090210     C                   ELSE
170700090210     C*  se non è numerico stampo errore
170800090210     C                   MOVEL     'S'           WERRO             1
170900090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
171000090213     c                   exsr      Error_DET
171100090210     C                   goto      end_NRS
171200090210     C                   END
171300090210      *
171400090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
171500090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
171600090210     C     SGNNRS        IFNE      VABNRS
171700090210     C                   MOVEL     'S'           WERRO             1
171800090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
171900090210     c                             o della Bolla'
172000090213     c                   exsr      Error_DET
172100090210     C                   goto      end_NRS
172200090210     C                   END
172300090210     C*
172400090210     C     end_NRS       ENDSR
172500090210     C*----------------------------------------------------------------
172600090210     C*? REPZNC - Reperisce zona consegna
172700090210     C*----------------------------------------------------------------
172800090210     C     REPZNC        BEGSR
172900090210     C*
173000090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
173100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
173200090210     C*  dal numero segnacollo passatomi dal partner
173300090210     C                   Z-ADD     §PUZN1        WP                2 0
173400090210     C     2             SUBST     x30(X):WP     WZNC              2
173500090210     C*  controllo che la zona calcolata sia numerica
173600090210     C                   TESTN                   WZNC                 98
173700090210     C*  se è numerica la carico
173800090210     C     *IN98         IFEQ      '1'
173900090210     C                   MOVE      WZNC          SGNZNC
174000090210     C                   ELSE
174100090210     C*  se non è numerica stampo errore
174200090210     C                   MOVEL     'S'           WERRO             1
174300090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
174400090213     c                   exsr      Error_DET
174500090210     C                   goto      end_ZNC
174600090210     C                   END
174700090210     C*
174800090210     C     end_ZNC       ENDSR
174900090210     C*----------------------------------------------------------------
175000090210     C*? REPSGN - Reperisce numero segnacollo
175100090210     C*----------------------------------------------------------------
175200090210     C     REPSGN        BEGSR
175300090210     C*
175400090210     C*  se viene passata nr.segnacollo estraggo sottostringa
175500090210     C*  lunga 7 chr (a partire dalla posizione iniziale
175600090210     C*  indicata in tabella)
175700090210     C*  dal numero segnacollo passatomi dal partner
175800090210     C                   Z-ADD     §PUSG1        WP                2 0
175900090210     C     7             SUBST     x30(X):WP     WSGN              7
176000090210     C*  controllo che il nr.segnacollo calcolato sia numerico
176100090210     C                   TESTN                   WSGN                 98
176200090210     C*  se è numerico lo carico
176300090210     C     *IN98         IFEQ      '1'
176400090210     C                   MOVE      WSGN          SGNNSC
176500090210     C                   ELSE
176600090210     C*  se non è numerica stampo errore
176700090210     C                   MOVEL     'S'           WERRO             1
176800090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
176900090213     c                   exsr      Error_DET
177000090210     C                   goto      end_SGN
177100090210     C                   END
177200090210     C*
177300090210     C     end_SGN       ENDSR
177400090611     C*----------------------------------------------------------------
177500090611     C*?   Inizio Messaggio
177600090611     C*----------------------------------------------------------------
177700051205     C     *INZSR        BEGSR
177800051205      *
177900991129     c     *ENTRY        PLIST
178000060613     C                   parm                    esito             1
178100971215      *
178200050112     C     KTABel        KLIST
178300050112     C                   KFLD                    TBLKUT
178400050112     C                   KFLD                    TBLCOD
178500050112     C                   KFLD                    TBLKEY
178600090210      *
178700090210     C     K_TAB1L       KLIST
178800090213     C                   KFLD                    etbUNB
178900090213     C                   KFLD                    etbSGM
179000090213     C                   KFLD                    etbVALSGM
179100090209     C*
179200090209     C* Definisco chiavi di accesso
179300090209     C     KTAB1         KLIST
179400090209     C                   KFLD                    KKUT
179500090209     C                   KFLD                    KCOD
179600090209     C*
179700090209     C     KNUF          KLIST
179800090209     C                   KFLD                    KAAA
179900090209     C                   KFLD                    KCNU
180000090209     C                   KFLD                    KFIL
180100090209      *
180200090209     C     KVAB1         KLIST
180300090209     C                   KFLD                    KCCM
180400090209     C                   KFLD                    KCMR
180500090209     C                   KFLD                    KCNT
180600090209     C                   KFLD                    KAAS
180700090209     C                   KFLD                    KLNP
180800090209     C                   KFLD                    KNRS
180900090209     C                   KFLD                    KNSP
181000090209      *
181100090209     C     KVAB2         KLIST
181200090209     C                   KFLD                    KCCM
181300090209     C                   KFLD                    KCMR
181400090209      *
181500090209     C     KRDE          KLIST
181600090209     C                   KFLD                    KAAS
181700090209     C                   KFLD                    KLNP1
181800090209     C                   KFLD                    KNRS
181900090209     C                   KFLD                    KNSP
182000090209     C                   KFLD                    KNMC
182100090209     C                   KFLD                    KNRP
182200090209      *
182300090209     C     KACO          KLIST
182400090209     C                   KFLD                    KKUT
182500090209     C                   KFLD                    KKCC
182600090209     C                   KFLD                    KKSC
182700090209      *
182800090209     C     KIND          KLIST
182900090209     C                   KFLD                    KKUT
183000090209     C                   KFLD                    KKCC
183100090209     C                   KFLD                    KKSC
183200090209      *
183300090209      * DETERMINO SE SONO BARTOLINI O SDI
183400090209     C                   CLEAR                   TIBS55DS
183500090209     C                   MOVEL     'L'           I50TLA
183600090209     C                   CALL      'TIBS55R'
183700090209     C                   PARM                    TIBS55DS
183800090209      *
183900090209     C* RECUPERO DATI DELL'UTENTE
184000090209     C                   Z-ADD     1             CODUT             1 0
184100090209     C                   CALL      'XPARUT'
184200090209     C                   PARM                    UTEDSE0F
184300090209     C                   MOVEL     RAGUT         RSUT             20
184400090209     C*
184500090209     C* RICERCA CAPOCONTI
184600090209     C                   DO        50            X
184700090209     C                   MOVE      TCU(X)        TCUDS
184800090209     C     F56           IFEQ      'CG'
184900090209     C     F34           ANDEQ     '01'
185000090209     C                   Z-ADD     KCU(X)        KCI               4 0
185100090209     C                   END
185200090209     C                   END
185300090209     C*
185400090211     c                   movel(p)  'EDPAB'       User_Msg         10
185500090211     C*
185600090209     C* IMPOSTO A ZERO VARIABILI DI PGM
185700090209     c                   move      *blank        Snd_Msg_alert     1
185800090209     C                   MOVEL     CA(1)         WCA              78
185900090209     C                   MOVEL     CN(1)         WCN              78
186000090209     C*
186100090209     C* RECUPERO DATA E ORA
186200090209     C                   TIME                    WHHDAT           14 0
186300090209     C                   MOVE      WHHDAT        G02DAT
186400090209     C                   MOVE      WHHDAT        WDATA8            8 0
186500090209     C                   MOVE      WDATA8        WAA2              2 0
186600090209     C                   MOVEL     WDATA8        DATA              6 0
186700090209     C                   MOVE      WAA2          DATA
186800090209     C                   MOVEL     *BLANK        G02ERR
186900090209     C                   CALL      'XSRDA8'
187000090209     C                   PARM                    WLBDA8
187100090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
187200090209     C                   TIME                    WORA              6 0
187300090209      * Recupero data e ora
187400090209     C                   TIME                    W0140
187500090209      * UDATE IN GGMMAAAA
187600090209     C                   MOVE      W0140         WDTGIO
187700090209      * UDATE IN AAAAMMGG
187800090209     C     *eur          MOVEL     WDTGIO        DATA_eur
187900090209     C     *iso          MOVEL     DATA_eur      dateu
188000090209      *
188100090209      * ?              /*--------------------------- */
188200090209     c                   exsr      CARICA_TABELLE
188300090209      * ?              /*--------------------------- */
188400090209      *
188500090209     C                   MOVEL     *ALL'-'       CMP132          132
188600991124      *
188700991124     C                   ENDSR
188800060621      * ?------------------------------------------------------------------ */
188900090209      *?    CARICA TABELLE SU SCHIERE
189000060621      * ?------------------------------------------------------------------ */
189100090209     C     CARICA_TABELLEBEGSR
189200090205      *
189300090209     C* Caricamento Tabella 1B
189400090209     C                   Z-ADD     0             X                 4 0
189500090209     C                   Z-ADD     CODUT         KKUT
189600090209     C                   MOVEL     '1B'          KCOD
189700090209     C     KTAB1         CHAIN     TABEL00F                           30
189800090209     C     *IN30         DOWEQ     '0'
189900090209     C     TBLFLG        IFEQ      *BLANKS
190000090209     C                   ADD       1             X
190100090209     C                   MOVEL     TBLKEY        CTM(X)
190200090209     C                   MOVEL     TBLUNI        DTM(X)
190300090209     C                   END
190400090209     C     KTAB1         READE     TABEL00F                               30
190500090209     C                   END
190600090209      *
190700090209     C* Caricamento Tabella 15
190800090209     C                   Z-ADD     0             X                 4 0
190900090209     C                   Z-ADD     CODUT         KKUT
191000090209     C                   MOVEL     '15'          KCOD
191100090209     C     KTAB1         CHAIN     TABEL00F                           30
191200090209     C     *IN30         DOWEQ     '0'
191300090209     C     TBLFLG        IFEQ      *BLANKS
191400090209     C                   ADD       1             X
191500090209     C                   MOVEL     TBLKEY        C15(X)
191600090209     C                   MOVEL     TBLUNI        DS15
191700090611     C                   MOVEL     §15ISO        ISO(X)
191800090209     C                   MOVEL     §15ECF        CPF(X)
191900090209     C                   MOVE      §15PCF        CPF(X)
192000090209     C                   END
192100090209     C     KTAB1         READE     TABEL00F                               30
192200090209     C                   END
192300090209      *
192400090209     C* Caricamento Tabella CV
192500090209     C                   Z-ADD     0             X
192600090209     C                   Z-ADD     CODUT         KKUT
192700090209     C                   MOVEL     'CV'          KCOD
192800090209     C     KTAB1         CHAIN     TABEL00F                           30
192900090209     C     *IN30         DOWEQ     '0'
193000090209     C     TBLFLG        IFEQ      *BLANKS
193100090209     C                   ADD       1             X
193200090209     C                   MOVEL     TBLKEY        CCV(X)
193300090209     C                   MOVEL     TBLUNI        DCV(X)
193400090209     C                   END
193500090209     C     KTAB1         READE     TABEL00F                               30
193600090209     C                   END
193700090216      *
193800090216     C* Caricamento Tabella Tipo Incasso C/Assegno
193900090216     C                   Z-ADD     0             X
194000090216     C                   MOVEL     'TC'          TABCOD
194100090216     C     TABCOD        CHAIN     EDTAB01L                           30
194200090216     C     *IN30         DOWEQ     '0'
194300090216     C     TABFLG        IFEQ      *BLANKS
194400090216     C                   ADD       1             X
194500090216     C                   MOVEL     TABKEY        CTC35(X)
194600090216     C                   MOVEL     TABUNI        TPI(X)
194700090216     C                   END
194800090216     C     TABCOD        READE     EDTAB01L                               30
194900090216     C                   END
195000090209      *
195100090209      * Caricamento Tabella Partner esteri
195200090209     C                   Z-ADD     0             X
195300090209     C                   MOVEL     'PT'          TABCOD
195400090209     C     TABCOD        CHAIN     EDTAB01L                           30
195500090209     C     *IN30         DOWEQ     '0'
195600090209      *
195700090209     C                   SELECT
195800090209     C     TABFLG        WHENNE    *BLANKS
195900090209      *
196000090209     C                   OTHER
196100090209     C                   ADD       1             X
196200090209     C                   MOVEL     TABKEY        CPT(X)
196300090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
196400090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
196500090209     C                   MOVEL     TABUNI        DPT(X)
196600090209     C                   ENDSL
196700090209      *
196800090209     C     TABCOD        READE     EDTAB01L                               30
196900090209     C                   END
197000121105      *
197100121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
197200121105     c                   exsr      ChecK_PT
197300121105      *
197400090209      * salva quanti Codici UNB ha caricato
197500090209     c                   z-add     x             sav_CPTx
197600090209      *
197700090209     C                   MOVEL     'PU'          TABCOD
197800090209     C     TABCOD        CHAIN     EDTAB01L                           30
197900090209     C     *IN30         DOWEQ     '0'
198000090209      *
198100090209     C                   SELECT
198200090209     C     TABFLG        WHENNE    *BLANKS
198300090209      *
198400090209     C                   OTHER
198500090209     C                   MOVEL     TABKEY        CODPU            35
198600090209     C                   Z-ADD     1             X
198700090209     C     CODPU         LOOKUP    CPT(X)                                 32
198800090209     C   32              MOVEL     TABUNI        DPU(X)
198900090209     C                   ENDSL
199000090209      *
199100090209     C     TABCOD        READE     EDTAB01L                               30
199200090209     C                   END
199300090209      *
199400090209      * Prosegue tab.PT e PU
199500090209     C                   MOVEL     'PV'          TABCOD
199600090209     C     TABCOD        CHAIN     EDTAB01L                           30
199700090209     C     *IN30         DOWEQ     '0'
199800090209      *
199900090209     C                   SELECT
200000090209     C     TABFLG        WHENNE    *BLANKS
200100090209      *
200200090209     C                   OTHER
200300090209     C                   MOVEL     TABKEY        CODPV            35
200400090209     C                   Z-ADD     1             X
200500090209     C     CODPV         LOOKUP    CPT(X)                                 32
200600090209     C   32              MOVEL     TABUNI        DPV(X)
200700090209     C                   ENDSL
200800090209      *
200900090209     C     TABCOD        READE     EDTAB01L                               30
201000090209     C                   END
201100090209      *
201200090209     C* Caricamento Tabella codici clienti - tariffe
201300090209     C                   Z-ADD     0             X
201400090209     C                   MOVEL     'CL'          TABCOD
201500090209     C     TABCOD        CHAIN     EDTAB01L                           30
201600090209     C     *IN30         DOWEQ     '0'
201700090209     C     TABFLG        IFEQ      *BLANKS
201800090209     C                   ADD       1             X
201900090209     C                   MOVEL     TABKEY        CCL(X)
202000090209     C                   MOVEL     TABUNI        DCL(X)
202100090209     C                   END
202200090209     C     TABCOD        READE     EDTAB01L                               30
202300090209     C                   END
202400090209      *
202500090209     C* Caricamento Serivizi speciali
202600090209     C                   Z-ADD     0             X
202700090209     C                   MOVEL     'SS'          TABCOD
202800090209     C     TABCOD        CHAIN     EDTAB01L                           30
202900090209     C     *IN30         DOWEQ     '0'
203000090209     C     TABFLG        IFEQ      *BLANKS
203100090209     C                   ADD       1             X
203200090209     C                   MOVEL     TABKEY        SS(X)
203300090209     C                   MOVEL     TABKEY        SS35(X)
203400090209     C                   MOVEL     TABUNI        DSS(X)
203500090209     C                   END
203600090209     C     TABCOD        READE     EDTAB01L                               30
203700090209     C                   END
203800090209      *
203900090209     C                   ENDSR
204000090209      * ?------------------------------------------------------------------ */
204100090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
204200090209      * ?------------------------------------------------------------------ */
204300090209     C     MSG_Errato    BEGSR
204400090209      *
204500090205      * ?  Scrive su tutti i records il tipo di errore
204600090205     c     *start        setll     tivin00r
204700090205     c                   read      tivin00r
204800090205     c                   dow       not %eof(tivin00r)
204900090211     c                   if        vinMSG  = *blank
205000090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
205100090210     C                               >> Controllare i DATI su UPLOAD !!'
205200090211     c                   end
205300090205     C                   eval      vinFLG = '2'
205400090213     c                   eval      Almeno_Un_Due ='S'
205500090205     c                   eval      esito  = '2'
205600090209     c                   eval      se_errore ='S'
205700090213     c                   eval      err_bloccante ='S'
205800090205     c                   update    tivin000
205900090205     c                   read      tivin00r
206000090205     c                   endDO
206100090205      *
206200090205     C                   ENDSR
206300090213      * ?------------------------------------------------------------------ */
206400090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
206500090213      * ?------------------------------------------------------------------ */
206600090213     C     DETta_Errato  BEGSR
206700090213      *
206800090213      *  rilascia il record prima di rieseguire il ciclo
206900090213     c                   update    tivin000
207000090213      *
207100090213      * ?  Scrive su tutti i records il tipo di errore
207200090213     c     SavNUM_RECord setll     tivin00r
207300090213     c                   read      tivin00r
207400090213     c                   dow       not %eof(tivin00r)
207500090213      *
207600090213      *
207700090213     c                   if        vinMSG  = *blank
207800090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
207900090213     C                             >> Controllare i DATI di ogni Segmento <<'
208000090213     c                   end
208100090213     c                   exsr      Error_DET
208200090213      *
208300090213     c                   if        VNREC = NUM_RECord
208400090213     c                   leave
208500090213     c                   end
208600090213      *
208700090213     c                   update    tivin000
208800090213     c                   read      tivin00r
208900090213      *
209000090213     c                   endDO
209100090603      *
209200090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
209300090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
209400090609     C                             Bolle DELTA+ non totalmente tradotto. -
209500100716     C                             Controllare UPLOAD del cliente 0055110.'
209600090603     c                   exsr      invio_mail_AB
209700090603      *
209800090213      *
209900090213     C                   ENDSR
210000090205      * ?------------------------------------------------------------------ */
210100090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
210200090205      * ?------------------------------------------------------------------ */
210300090205     C     *pssr         BEGSR
210400090205     C
210500060710     C                   eval      esito = '2'
210600090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
210700090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
210800090603     C                             Bolle import in filiale - messaggio -
210900090603     C                             EDI ricevuto errato su UPLOAD.'
211000090603     c                   exsr      invio_mail_AB
211100060621     C
211200090213     C                   ENDSR
211300090213      * ?------------------------------------------------------------------ */
211400090213      *?      Segnala Errore su TIVIN
211500090213      * ?------------------------------------------------------------------ */
211600090213     C     Error_DET     BEGSR
211700090213     C
211800090213     C                   eval      vinFLG = '2'
211900090213     c                   eval      Almeno_Un_Due ='S'
212000090213     c                   eval      esito  = '1'
212100090213     c                   eval      se_errore ='S'
212200090213     C                   eval      Err_in_detta = 'S'
212300090213     C
212400090213     C                   ENDSR
212500090209      * ?------------------------------------------------------------------ */
212600090209      *? Controlla la trasmissione   se completa
212700090209      * ?------------------------------------------------------------------ */
212800090209     c     Check_Trasm   Begsr
212900090209
213000090209     C                   clear                   se_errore
213100090209     C                   clear                   Riga_iNIZIo       1
213200090209     C                   clear                   Riga_fINe         1
213300090209      ** primo  record
213400090209     c     *start        setll     tivin00r
213500090209      *
213600090209     c     rileggi       tag
213700090209     c                   read      tivin00r
213800090209      *
213900090209     c                   if        not %eof(tivin00r)
214000090209      *
214100090209     c                   movel(p)  VINDTA        dati
214200090209     c                   if        dati = *blank
214300090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
214400090209      * le leggo e le escludo.
214500090209     C                   eval      vinFLG = '1'
214600090209     c                   update    Tivin000
214700090209     c                   goto      rileggi
214800090209     c                   end
214900090209      * ?              /*---------------------- */
215000090210     c                   exsr      Decod_Segmento
215100090209      * ?              /*---------------------- */
215200090209     c                   endif
215300090209      **
215400090209      ** ultimo record
215500090209     c     *hival        setll     tivin00r
215600090209     c     leggiAncora   tag
215700090209     c                   readp     tivin00r
215800090209     c                   if        not %eof(tivin00r)
215900090209      *
216000090209     c                   movel(p)  VINDTA        dati
216100090209     c                   if        dati = *blank
216200090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
216300090209      * le leggo e le escludo.
216400090209     C                   eval      vinFLG = '1'
216500090209     c                   update    Tivin000
216600090209     c                   goto      leggiAncora
216700090209     c                   end
216800090209      * ?              /*---------------------- */
216900090210     c                   exsr      Decod_Segmento
217000090209      * ?              /*---------------------- */
217100090209     c                   endif
217200090209
217300090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
217400090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
217500090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
217600090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
217700090209      * ?-----> Errore
217800090209     C                   eval      se_errore = 'S'
217900090209     c                   end
218000090209
218100090209     c                   endSR
218200090209      * ?------------------------------------------------------------------ */
218300090209      *?    Decodifica il segmento ed importa i campi in formato DS
218400090209      * ?------------------------------------------------------------------ */
218500090209     c     Decod_SegmentoBegsr
218600090209      **
218700090209     c                   eval      tipo_seg = %subst(vinDTA:1:3)
218800090209     c                   eval      segmento = vinDTA
218900090210      **--------
219000100716     C                   eval       keyUNBCLI    = *blank
219100100716     C                   eval       keyTIPOMSG   = 'DISPOR'
219200100716     C                   eval       keyVERSION   = '2'
219300100716     C                   eval       keyRELEASE   = '3'
219400100716     C                   eval       keyAGENCY    = 'GT'
219500100716     C                   eval       keyASSOCIA   = 'GTF'
219600100716      **--------
219700090211     c                   select
219800090609      * --------
219900090609     c                   when      tipo_seg = 'BGM'
220000090609      *
220100090609     c                   call      'TRTCT05BGM'
220200090609     c                   parm                    tipo_seg
220300090609     c                   parm                    segmento
220400100716     C                   parm                    keyUNBCLI
220500100716     C                   parm                    keyTIPOMSG
220600100716     C                   parm                    keyVERSION
220700100716     C                   parm                    keyRELEASE
220800100716     C                   parm                    keyAGENCY
220900100716     C                   parm                    keyASSOCIA
221000090609     c                   parm                    Errore
221100090609     c                   parm                    EDPORBGM
221200090609     c                   parm                    Valori_inErr
221300090609     c                   parm                    Descr_Errore
221400090609      *
221500090609     C                   EXSR      DATI_BGM
221600090609      **--------
221700090609     c                   when      tipo_seg = 'CAG'
221800090609      *
221900090609     c                   call      'TRTCT05CAG'
222000090609     c                   parm                    tipo_seg
222100090609     c                   parm                    segmento
222200100716     C                   parm                    keyUNBCLI
222300100716     C                   parm                    keyTIPOMSG
222400100716     C                   parm                    keyVERSION
222500100716     C                   parm                    keyRELEASE
222600100716     C                   parm                    keyAGENCY
222700100716     C                   parm                    keyASSOCIA
222800090609     c                   parm                    Errore
222900090609     c                   parm                    EDPORCAG
223000090609     c                   parm                    Valori_inErr
223100090609     c                   parm                    Descr_Errore
223200090609      *
223300090609     C                   EXSR      DATI_CAG
223400090609      **--------
223500090609     c                   when      tipo_seg = 'CTA'
223600090609      *
223700090609     c                   call      'TRTCT05CTA'
223800090609     c                   parm                    tipo_seg
223900090609     c                   parm                    segmento
224000100716     C                   parm                    keyUNBCLI
224100100716     C                   parm                    keyTIPOMSG
224200100716     C                   parm                    keyVERSION
224300100716     C                   parm                    keyRELEASE
224400100716     C                   parm                    keyAGENCY
224500100716     C                   parm                    keyASSOCIA
224600090609     c                   parm                    Errore
224700090609     c                   parm                    EDPORCTA
224800090609     c                   parm                    Valori_inErr
224900090609     c                   parm                    Descr_Errore
225000090609      *
225100090609     C                   EXSR      DATI_CTA
225200091104      **--------
225300091104     c                   when      tipo_seg = 'COM'
225400091104      *
225500091104     c                   call      'TRTCT05COM'
225600091104     c                   parm                    tipo_seg
225700091104     c                   parm                    segmento
225800100716     C                   parm                    keyUNBCLI
225900100716     C                   parm                    keyTIPOMSG
226000100716     C                   parm                    keyVERSION
226100100716     C                   parm                    keyRELEASE
226200100716     C                   parm                    keyAGENCY
226300100716     C                   parm                    keyASSOCIA
226400091104     c                   parm                    Errore
226500091104     c                   parm                    EDPORCOM
226600091104     c                   parm                    Valori_inErr
226700091104     c                   parm                    Descr_Errore
226800091104      *
226900091104     C                   EXSR      DATI_COM
227000090609      **--------
227100090609     c                   when      tipo_seg = 'DGS'
227200090609      *
227300090609     c                   call      'TRTCT05DGS'
227400090609     c                   parm                    tipo_seg
227500090609     c                   parm                    segmento
227600100716     C                   parm                    keyUNBCLI
227700100716     C                   parm                    keyTIPOMSG
227800100716     C                   parm                    keyVERSION
227900100716     C                   parm                    keyRELEASE
228000100716     C                   parm                    keyAGENCY
228100100716     C                   parm                    keyASSOCIA
228200090609     c                   parm                    Errore
228300090609     c                   parm                    EDPORDGS
228400090609     c                   parm                    Valori_inErr
228500090609     c                   parm                    Descr_Errore
228600090609      *
228700090609     C                   EXSR      DATI_DGS
228800090609      * --------
228900090609     c                   when      tipo_seg = 'DOC'
229000090609      *
229100090609     c                   call      'TRTCT05DOC'
229200090609     c                   parm                    tipo_seg
229300090609     c                   parm                    segmento
229400100716     C                   parm                    keyUNBCLI
229500100716     C                   parm                    keyTIPOMSG
229600100716     C                   parm                    keyVERSION
229700100716     C                   parm                    keyRELEASE
229800100716     C                   parm                    keyAGENCY
229900100716     C                   parm                    keyASSOCIA
230000090609     c                   parm                    Errore
230100090609     c                   parm                    EDPORDOC
230200090609     c                   parm                    Valori_inErr
230300090609     c                   parm                    Descr_Errore
230400090609      *
230500090609     C                   EXSR      DATI_DOC
230600090609      **--------
230700090609     c                   when      tipo_seg = 'DTM'
230800090609      *
230900090609     c                   call      'TRTCT05DTM'
231000090609     c                   parm                    tipo_seg
231100090609     c                   parm                    segmento
231200100716     C                   parm                    keyUNBCLI
231300100716     C                   parm                    keyTIPOMSG
231400100716     C                   parm                    keyVERSION
231500100716     C                   parm                    keyRELEASE
231600100716     C                   parm                    keyAGENCY
231700100716     C                   parm                    keyASSOCIA
231800090609     c                   parm                    Errore
231900090609     c                   parm                    EDPORDTM
232000090609     c                   parm                    Valori_inErr
232100090609     c                   parm                    Descr_Errore
232200090609      *
232300090609     C                   EXSR      DATI_DTM
232400090609      **--------
232500090609     c                   when      tipo_seg = 'GDS'
232600090609      *
232700090609     c                   call      'TRTCT05GDS'
232800090609     c                   parm                    tipo_seg
232900090609     c                   parm                    segmento
233000100716     C                   parm                    keyUNBCLI
233100100716     C                   parm                    keyTIPOMSG
233200100716     C                   parm                    keyVERSION
233300100716     C                   parm                    keyRELEASE
233400100716     C                   parm                    keyAGENCY
233500100716     C                   parm                    keyASSOCIA
233600090609     c                   parm                    Errore
233700090609     c                   parm                    EDPORGDS
233800090609     c                   parm                    Valori_inErr
233900090609     c                   parm                    Descr_Errore
234000090609      *
234100090609     C                   EXSR      DATI_GDS
234200090609      **--------
234300090609     c                   when      tipo_seg = 'GID'
234400090609      *
234500090609     c                   call      'TRTCT05GID'
234600090609     c                   parm                    tipo_seg
234700090609     c                   parm                    segmento
234800100716     C                   parm                    keyUNBCLI
234900100716     C                   parm                    keyTIPOMSG
235000100716     C                   parm                    keyVERSION
235100100716     C                   parm                    keyRELEASE
235200100716     C                   parm                    keyAGENCY
235300100716     C                   parm                    keyASSOCIA
235400090609     c                   parm                    Errore
235500090609     c                   parm                    EDPORGID
235600090609     c                   parm                    Valori_inErr
235700090609     c                   parm                    Descr_Errore
235800090609      *
235900090609     C                   EXSR      DATI_GID
236000090609      **--------
236100090609     c                   when      tipo_seg = 'MSE'
236200090609      *
236300090609     c                   call      'TRTCT05MSE'
236400090609     c                   parm                    tipo_seg
236500090609     c                   parm                    segmento
236600100716     C                   parm                    keyUNBCLI
236700100716     C                   parm                    keyTIPOMSG
236800100716     C                   parm                    keyVERSION
236900100716     C                   parm                    keyRELEASE
237000100716     C                   parm                    keyAGENCY
237100100716     C                   parm                    keyASSOCIA
237200090609     c                   parm                    Errore
237300090609     c                   parm                    EDPORMSE
237400090609     c                   parm                    Valori_inErr
237500090609     c                   parm                    Descr_Errore
237600090609      *
237700090609     C                   EXSR      DATI_MSE
237800090609      **--------
237900090609     c                   when      tipo_seg = 'NAD'
238000090609      *
238100090609     c                   call      'TRTCT05NAD'
238200090609     c                   parm                    tipo_seg
238300090609     c                   parm                    segmento
238400100716     C                   parm                    keyUNBCLI
238500100716     C                   parm                    keyTIPOMSG
238600100716     C                   parm                    keyVERSION
238700100716     C                   parm                    keyRELEASE
238800100716     C                   parm                    keyAGENCY
238900100716     C                   parm                    keyASSOCIA
239000090609     c                   parm                    Errore
239100090609     c                   parm                    EDPORNAD
239200090609     c                   parm                    Valori_inErr
239300090609     c                   parm                    Descr_Errore
239400090609      *
239500090609     C                   EXSR      DATI_NAD
239600090609      **--------
239700090609     c                   when      tipo_seg = 'PCI'
239800090609      *
239900090609     c                   call      'TRTCT05PCI'
240000090609     c                   parm                    tipo_seg
240100090609     c                   parm                    segmento
240200100716     C                   parm                    keyUNBCLI
240300100716     C                   parm                    keyTIPOMSG
240400100716     C                   parm                    keyVERSION
240500100716     C                   parm                    keyRELEASE
240600100716     C                   parm                    keyAGENCY
240700100716     C                   parm                    keyASSOCIA
240800090609     c                   parm                    Errore
240900090609     c                   parm                    EDPORPCI
241000090609     c                   parm                    Valori_inErr
241100090609     c                   parm                    Descr_Errore
241200090609      *
241300090609     C                   EXSR      DATI_PCI
241400090609      **--------
241500090609     c                   when      tipo_seg = 'RFF'
241600090609      *
241700090609     c                   call      'TRTCT05RFF'
241800090609     c                   parm                    tipo_seg
241900090609     c                   parm                    segmento
242000100716     C                   parm                    keyUNBCLI
242100100716     C                   parm                    keyTIPOMSG
242200100716     C                   parm                    keyVERSION
242300100716     C                   parm                    keyRELEASE
242400100716     C                   parm                    keyAGENCY
242500100716     C                   parm                    keyASSOCIA
242600090609     c                   parm                    Errore
242700090609     c                   parm                    EDPORRFF
242800090609     c                   parm                    Valori_inErr
242900090609     c                   parm                    Descr_Errore
243000090609      *
243100090609     C                   EXSR      DATI_RFF
243200090609      **--------
243300090609     c                   when      tipo_seg = 'TDT'
243400090609      *
243500090609     c                   call      'TRTCT05TDT'
243600090609     c                   parm                    tipo_seg
243700090609     c                   parm                    segmento
243800100716     C                   parm                    keyUNBCLI
243900100716     C                   parm                    keyTIPOMSG
244000100716     C                   parm                    keyVERSION
244100100716     C                   parm                    keyRELEASE
244200100716     C                   parm                    keyAGENCY
244300100716     C                   parm                    keyASSOCIA
244400090609     c                   parm                    Errore
244500090609     c                   parm                    EDPORTDT
244600090609     c                   parm                    Valori_inErr
244700090609     c                   parm                    Descr_Errore
244800090609      *
244900090609     C                   EXSR      DATI_TDT
245000090609      * --------
245100090609     c                   when      tipo_seg = 'TSR'
245200090609      *
245300090609     c                   call      'TRTCT05TSR'
245400090609     c                   parm                    tipo_seg
245500090609     c                   parm                    segmento
245600100716     C                   parm                    keyUNBCLI
245700100716     C                   parm                    keyTIPOMSG
245800100716     C                   parm                    keyVERSION
245900100716     C                   parm                    keyRELEASE
246000100716     C                   parm                    keyAGENCY
246100100716     C                   parm                    keyASSOCIA
246200090609     c                   parm                    Errore
246300090609     c                   parm                    EDPORTSR
246400090609     c                   parm                    Valori_inErr
246500090609     c                   parm                    Descr_Errore
246600090609      *
246700090609     C                   EXSR      DATI_TSR
246800090609      **--------
246900090609     c                   when      tipo_seg = 'TXT'
247000090609      *
247100090609     c                   call      'TRTCT05TXT'
247200090609     c                   parm                    tipo_seg
247300090609     c                   parm                    segmento
247400100716     C                   parm                    keyUNBCLI
247500100716     C                   parm                    keyTIPOMSG
247600100716     C                   parm                    keyVERSION
247700100716     C                   parm                    keyRELEASE
247800100716     C                   parm                    keyAGENCY
247900100716     C                   parm                    keyASSOCIA
248000090609     c                   parm                    Errore
248100090609     c                   parm                    EDPORTXT
248200090609     c                   parm                    Valori_inErr
248300090609     c                   parm                    Descr_Errore
248400090609      *
248500090609     C                   EXSR      DATI_TXT
248600090609      * --------
248700090209     c                   when      tipo_seg = 'UNB'
248800090209      *
248900090609     c                   call      'TRTCT05UNB'
249000090209     c                   parm                    tipo_seg
249100090209     c                   parm                    segmento
249200100716     C                   parm                    keyUNBCLI
249300100716     C                   parm                    keyTIPOMSG
249400100716     C                   parm                    keyVERSION
249500100716     C                   parm                    keyRELEASE
249600100716     C                   parm                    keyAGENCY
249700100716     C                   parm                    keyASSOCIA
249800090209     c                   parm                    Errore
249900090609     c                   parm                    EDPORUNB
250000090209     c                   parm                    Valori_inErr
250100090209     c                   parm                    Descr_Errore
250200090210      *
250300090210     C                   EXSR      DATI_UNB
250400090210      * --------
250500090209     c                   when      tipo_seg = 'UNH'
250600090209      *
250700090609     c                   call      'TRTCT05UNH'
250800090209     c                   parm                    tipo_seg
250900090209     c                   parm                    segmento
251000100716     C                   parm                    keyUNBCLI
251100100716     C                   parm                    keyTIPOMSG
251200100716     C                   parm                    keyVERSION
251300100716     C                   parm                    keyRELEASE
251400100716     C                   parm                    keyAGENCY
251500100716     C                   parm                    keyASSOCIA
251600090209     c                   parm                    Errore
251700090609     c                   parm                    EDPORUNH
251800090209     c                   parm                    Valori_inErr
251900090209     c                   parm                    Descr_Errore
252000090210      *
252100090210     C                   EXSR      DATI_UNH
252200090609      * --------
252300090609     c                   when      tipo_seg = 'UNS'
252400090609      *
252500090609     c                   call      'TRTCT05UNS'
252600090609     c                   parm                    tipo_seg
252700090609     c                   parm                    segmento
252800100716     C                   parm                    keyUNBCLI
252900100716     C                   parm                    keyTIPOMSG
253000100716     C                   parm                    keyVERSION
253100100716     C                   parm                    keyRELEASE
253200100716     C                   parm                    keyAGENCY
253300100716     C                   parm                    keyASSOCIA
253400090609     c                   parm                    Errore
253500090609     c                   parm                    EDPORUNS
253600090609     c                   parm                    Valori_inErr
253700090609     c                   parm                    Descr_Errore
253800090609      *
253900090609     C                   EXSR      DATI_UNS
254000090609      **--------
254100090609     c                   when      tipo_seg = 'UNT'
254200090609      *
254300090616     c                   call      'TRTCT05UNT'
254400090609     c                   parm                    tipo_seg
254500090609     c                   parm                    segmento
254600100716     C                   parm                    keyUNBCLI
254700100716     C                   parm                    keyTIPOMSG
254800100716     C                   parm                    keyVERSION
254900100716     C                   parm                    keyRELEASE
255000100716     C                   parm                    keyAGENCY
255100100716     C                   parm                    keyASSOCIA
255200090609     c                   parm                    Errore
255300090609     c                   parm                    EDPORUNT
255400090609     c                   parm                    Valori_inErr
255500090609     c                   parm                    Descr_Errore
255600090609      *
255700090609     C                   EXSR      DATI_UNT
255800090609      **--------
255900090609     c                   when      tipo_seg = 'UNZ'
256000090609      *
256100090616     c                   call      'TRTCT05UNZ'
256200090609     c                   parm                    tipo_seg
256300090609     c                   parm                    segmento
256400100716     C                   parm                    keyUNBCLI
256500100716     C                   parm                    keyTIPOMSG
256600100716     C                   parm                    keyVERSION
256700100716     C                   parm                    keyRELEASE
256800100716     C                   parm                    keyAGENCY
256900100716     C                   parm                    keyASSOCIA
257000090609     c                   parm                    Errore
257100090609     c                   parm                    EDPORUNZ
257200090609     c                   parm                    Valori_inErr
257300090609     c                   parm                    Descr_Errore
257400090609      *
257500090609     C                   EXSR      DATI_UNZ
257600090210      **--------
257700090609     c                   when      tipo_seg = 'VAL'
257800090213      *
257900090616     c                   call      'TRTCT05VAL'
258000090213     c                   parm                    tipo_seg
258100090213     c                   parm                    segmento
258200100716     C                   parm                    keyUNBCLI
258300100716     C                   parm                    keyTIPOMSG
258400100716     C                   parm                    keyVERSION
258500100716     C                   parm                    keyRELEASE
258600100716     C                   parm                    keyAGENCY
258700100716     C                   parm                    keyASSOCIA
258800090213     c                   parm                    Errore
258900090609     c                   parm                    EDPORVAL
259000090213     c                   parm                    Valori_inErr
259100090213     c                   parm                    Descr_Errore
259200090213      *
259300090609     C                   EXSR      DATI_VAL
259400090210      **--------
259500090209     c                   endSL
259600090209      **
259700090209     c                   endSR
259800090209     c*==================================================================*
259900090209     C*? CNVDAT - DECODIFICA LA DATA
260000090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
260100090209     C*         - WDATA  (DATA ALFANUMERICA)
260200090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
260300090209     C*         - WHMS   (ORA NUMERICA)
260400090209     C*----------------------------------------------------------------
260500090209     C     CNVDAT        BEGSR
260600090209     C*
260700090209     C                   MOVEL     ' '           WERRO             1
260800090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
260900090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
261000090209     C                   Z-ADD     *ZEROS        WHMS              6 0
261100090209     C*
261200090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
261300090209     C                   MOVEL     WDATA         WCOMO8            8
261400090209     C                   TESTN                   WCOMO8               99
261500090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
261600090209     C  N99              MOVEL     'S'           WERRO             1
261700090209     C                   END
261800090209     C*
261900090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
262000090209     C                   MOVEL     WDATA         WCOMO14          14
262100090209     C                   TESTN                   WCOMO14              99
262200090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
262300090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
262400090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
262500090209     C  N99              MOVEL     'S'           WERRO             1
262600090209     C                   END
262700090209     C*
262800090209     C                   ENDSR
262900090210     C*----------------------------------------------------------------
263000090210     C*? TSTNUM - CONTROLLO NUMERICO
263100090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
263200090210     C*         - WLUNGH (Lunghezza campo output)
263300090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
263400090210     C*----------------------------------------------------------------
263500090210     C     TSTNUM        BEGSR
263600090210     C*
263700090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
263800090210     C                   MOVEL     'N'           WOKNUM            1
263900090210     C                   MOVEL     *BLANKS       WNUMOK           15
264000090210     C                   Z-ADD     15            IN                3 0
264100090210     C                   Z-ADD     WLUNGH        IX                3 0
264200090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
264300090210     C                   MOVEA     WNUM          NDA
264400090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
264500090210     C                   MOVEA     *ALL'0'       NDN
264600090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
264700090210     C                   Z-ADD     WLUNGH        I                 3 0
264800090210     C     I             DOWGT     0                                            --- 1 -->
264900090210     C* ESTRAE IL CARATTERE DA ESAMINARE
265000090210     C     1             SUBST     WNUM:I        WTEM01            1
265100090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
265200090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
265300090210     C     WTEM01        SCAN      WCA                                    99
265400090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
265500090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
265600090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
265700090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
265800090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
265900090210     C* DA DESTRA VERSO SINISTRA
266000090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
266100090210     C                   MOVEL     WTEMP1        NDN(IN)
266200090210     C                   SUB       1             IN
266300090210     C                   END                                                    <-- 3 ---
266400090210     C                   END                                                    <-- 2 ---
266500090210     C                   SUB       1             I
266600090210     C                   END                                                    <-- 1 ---
266700090210     C*
266800090210     C                   MOVEA     NDN           WNDN             15
266900090210     C     WNDN          IFNE      *ZEROS
267000090210     C                   MOVEA     NDN           WNUMOK           15
267100090210     C                   MOVEL     'S'           WOKNUM            1
267200090210     C                   END
267300090210     C*
267400090210     C                   ENDSR
267500090210     C*----------------------------------------------------------------
267600090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
267700090210     C*----------------------------------------------------------------
267800090210     C     CNVCAP        BEGSR
267900090210     C*
268000090210     C                   MOVEL     *BLANKS       WCAP              9
268100090609     C     cap09a        IFNE      '0'
268200090609     C     '  '          SCAN      cap09a        LENGHT            2 0
268300090210     C                   SUB       1             LENGHT
268400090210     C     LENGHT        IFLE      5
268500090210     C     LENGHT        ANDGT     0
268600090210     C                   Z-ADD     LENGHT        T                 2 0
268700090210     C                   Z-ADD     5             S                 2 0
268800090609     C                   MOVEA     cap09a        CAP9
268900090210     C                   MOVEL     *ZEROS        CAP5
269000090210     C                   DO        LENGHT
269100090210     C                   MOVE      CAP9(T)       CAP5(S)
269200090210     C                   SUB       1             S
269300090210     C                   SUB       1             T
269400090210     C                   END
269500090210     C                   MOVEA     CAP5          WCAP5             5
269600090210     C     WCAP5         IFEQ      '0'
269700090210     C                   MOVEL     *BLANKS       WCAP
269800090210     C                   ELSE
269900090210     C                   MOVEA     CAP5          WCAP
270000090210     C                   END
270100090210     C*
270200090210     C                   ELSE
270300090609     C                   MOVEL     cap09a        WCAP
270400090210     C                   END
270500090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
270600090210     C     WCAP          IFNE      *BLANKS
270700090210     C     WFLCPF        ANDNE     0
270800090210     C                   MOVEL     WFLCPF        WELE              1 0
270900090210     C                   MOVE      WFLCPF        WPOS              1 0
271000090210     C                   MOVEL     *BLANK        WCPF
271100090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
271200090210     C                   ELSE
271300090210     C                   MOVEL     *BLANKS       WCPF
271400090210     C                   END
271500090210     C                   END
271600090210     C*
271700090210     C                   ENDSR
271800090211      * ?================================================================== */
271900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
272000090211      * ?================================================================== */
272100090211     C     Scrive_VAB    BEGSR
272200090211      *
272300090211     c                   clear                   err_bloccante     1
272400090211     c                   move      'S'           Almeno_Uno
272500090616     c                   eval       se_trovato_NAD = *blank
272600090211      *
272700090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
272800090211     c                   exsr      UPDVAB
272900090211      *
273000090211     C* >>>>>>>>>>>>
273100090211     C*  Richiamo pgm per scrittura immediata VAB
273200090211     C     §PUWRK        IFEQ      ' '
273300090211      *
273400090211     C*  Richiamo pgm per esecuzione stampa
273500090213     C                   MOVEL     WNRDOC        d86CMR
273600090213     C                   MOVEL     ' '           d86RIE
273700090213     C                   MOVEL     §PUDTA        d86DTA
273800090213     C                   Z-ADD     1             d86CNT
273900090213     C                   Z-ADD     §PTKSC        d86CCM
274000090213     C                   MOVEL     *BLANKS       d86STA
274100090213     C                   MOVEL     'E'           d86MOD
274200090611      *
274300090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
274400090211      * dove poi imposta la DS dei parametri passati
274500090213     C                   MOVEL     'LR'          d86CHI
274600090213     C                   MOVEL     'N'           d86CTL
274700090211      *
274800090211     c                   move      kpjbu         SvKpjbu
274900090211     c                   clear                   kpjbu
275000090216     C                   movel     *on           Commit_on         1
275100090213     C                   MOVEL     TRTC86ds      KPJBU
275200090213      * ?- Chiamata la TRTC86R2 -*
275300090213     C                   CALL      'TRTC86R2'
275400090211     C                   PARM                    KPJBA
275500090216     C                   PARM                    Commit_on
275600090211     c                   move      Svkpjbu       Kpjbu
275700090211      *
275800090211     C*  Controllo pgm per scrittura immediata VAB
275900090211     C     WW            IFNE      0
276000090211     C                   DO        WW            WX                3 0
276100090213     C                   Z-ADD     KCL(WX)       d86CCM
276200090211      *
276300090211     c                   move      kpjbu         SvKpjbu
276400090211     c                   clear                   kpjbu
276500090216     C                   movel     *on           Commit_on         1
276600090213     C                   MOVEL     TRTC86ds      KPJBU
276700090213      * ?- Chiamata la TRTC86R2 -*
276800090213     C                   CALL      'TRTC86R2'
276900090211     C                   PARM                    KPJBA
277000090216     C                   PARM                    Commit_on
277100090211     c                   move      Svkpjbu       Kpjbu
277200090211     C                   END
277300090211     C                   END
277400090211      *
277500090211     C                   END
277600090211      * >>>>>>>>>>>>
277700090211     C                   if        se_errore  = 'S'
277800090211     c                   move      'S'           err_bloccante
277900090211     c                   goto      Error_VAB
278000090211     c                   end
278100090211     C*
278200090211     C     Error_VAB     Endsr
278300090211     C*----------------------------------------------------------------
278400090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
278500090211     C*----------------------------------------------------------------
278600090211     C     UPDVAB        BEGSR
278700090211      *
278800090211     C*  Se non è stato impostato il codice bolla lo imposto io
278900090211     C     VABCBO        IFEQ      *BLANKS
279000090630     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
279100090211     C                   END
279200090630      *
279300090630      *  Se c'è il contrassegno
279400090630     C     VABCAS        IFGT      0
279500090630     C     VABCBO        ifeq      '1'
279600090630     C                   MOVEL     '4'           VABCBO                         + C/Ass
279700090630     C                   END
279800090630     C     VABCBO        ifeq      '2'
279900090630     C                   MOVEL     '6'           VABCBO                         + C/Ass
280000090630     C                   END
280100090630     C                   END
280200110922      *
280300110922      * Imposta le NOte
280400110922     C     VABNOT        IFEQ      *BLANKS
280500110922     C                   MOVEL     wnot1         VABNOT
280600110922     C                   MOVEL     wnot2         VABNT2
280700110922     C                   ELSE
280800110922     C                   MOVEL     wnot1         VABNT2
280900110922     C                   END
281000090211      *
281100090211     C*  Attribuisco anno spedizione
281200090611     C     Tes_VABdcm    IFGT      0
281300090611     C                   MOVEL     Tes_VABdcm    VABAAS                         anno sped.
281400090211     C                   END
281500090211      *
281600090211      *  Controllo dettaglio segnacolli
281700090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
281800090211     C                   EXSR      CTRSGE
281900090211     C                   ELSE
282000090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
282100090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
282200090211     C                   EXSR      CTRSGN
282300090211     C                   END
282400090211     C                   END
282500090211      *
282600090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
282700090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
282800090211     c                   clear                   mantiene_nsp      1            *blk/'S'
282900090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
283000090211      *
283100090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
283200090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
283300090211      *
283400090211      *  Imposto codice cliente
283500090211     c                   Select
283600090211      *
283700090611      *  Specificato codice da qualificatore FF
283800090211     C                   When        WCLKSC <> *zeros
283900090211     C                   Z-ADD     WCLKSC        VABCCM
284000090211     C                   MOVEL     WCLTAR        VABCTR
284100090211      * forza LNP/CTM/NRS
284200090211     c                   if        WclLNP <> 0
284300090211     C                   eval      VABLNP = WclLNP
284400090211     c                   end
284500090211     c                   if        WclNRS <> 0
284600090211     C                   eval      VABNRS = WclNRS
284700090211     c                   end
284800090211     c                   if        WclCTM <> *blank
284900090211     C                   eval      VABCTM = WclCTM
285000090211     c                   end
285100090211      *
285200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
285300090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
285400090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
285500090211     c                   end
285600090211      *
285700090611      *  Specificato codice da qualificatore FF
285800090211     C                   When        $CLKSC <> *zeros
285900090211     C                   Z-ADD     $CLKSC        VABCCM
286000090211     C                   MOVEL     $CLTAR        VABCTR
286100090211      * forza LNP/CTM/NRS
286200090211     c                   if        $clLNP <> 0
286300090211     C                   eval      VABLNP = $clLNP
286400090211     c                   end
286500090211     c                   if        $clNRS <> 0
286600090211     C                   eval      VABNRS = $clNRS
286700090211     c                   end
286800090211     c                   if        $clCTM <> *blank
286900090211     C                   eval      VABCTM = $clCTM
287000090211     c                   end
287100090211      *
287200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
287300090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
287400090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
287500090211     c                   end
287600090211      *
287700090211      *  Imposto dati da tabella - PT-
287800090211     C                   Other
287900090211      *
288000090211     C                   MOVEL     §PTKSC        VABCCM
288100090211     C                   MOVEL     §PTCTR        VABCTR
288200090211      *
288300090211     C                   Endsl
288400090211      *
288500090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
288600090211      *    o dalla relativa CL
288700090211     C                   MOVEL     VABlnp        VABfgs
288800090211      *
288900090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
289000090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
289100090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
289200090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
289300090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
289400090211      *
289500100511      * Avendo attivato il VAX si deve recepire lo stesso numero passato nel rif.Alfabetico
289600100511      *
289700100511     c                   if        mantiene_nsp = 'S' and VABRMA <> *blank      *blk/'S' - 1234567
289800100511     c                   movel(p)  vabrma        campo9a           9
289900100511     c                   move      campo9a       VABNSP                         *NUM.SPED.da Cliente
290000100511     c                   else
290100090211      *
290200090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
290300090211     C*  lo attribuisco io
290400090211      **              **------------------**
290500090211     c                   exsr      repnum
290600090211      **              **------------------**
290700090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
290800090211      **              **------------------**
290900090211     c                   end
291000090211      *
291100090211     C*  Altrimenti attribuisco data del giorno
291200090211     C     VABMGS        IFEQ      0
291300090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
291400090211     C                   MOVE      WOGGI         VABMGS                         = oggi
291500090211     C                   END
291600090611      *
291700090211     C*  Eseguo posizionamento su EDiVAB
291800090211     C                   Z-ADD     VABAAS        KAAS
291900090211     C                   Z-ADD     VABlnp        KLNP
292000090211     C                   Z-ADD     VABNRS        KNRS
292100090211     C                   Z-ADD     VABNSP        KNSP
292200090211     C                   MOVEL     SAVBOL        SALVA
292300090211     C     KVAB1         CHAIN     EDiVAB1L                           30
292400090211     C                   MOVEL     SALVA         SAVBOL
292500090611      *
292600090211     C*  Imposto dati CMR
292700090211     C                   Z-ADD     1             VABCNT
292800090211     **
292900090211      *** Attenzione se si deve trattare il VAX con il CMR
293000090211      *** il campo VABCNT deve essere sempre impostato a (1)
293100090211     C     §puWRK        ifEQ      'S'
293200090211     C     §puNSP        andEQ     'S'
293300090211     C                   Z-ADD     1             VABCNT
293400090211     c                   end
293500090211     **
293600090211     C                   Z-ADD     WDTPRE        VABDTS
293700090212     C     WHMPRE        mult      100           VABHMS
293800090611      *
293900090611     C                   Z-ADD     Tes_VABdcm    VABDCM
294000090211     C                   MOVEL     WNRCMR        VABCMR
294100090611      *
294200090211     C*  Se non è indicato il nome del mittente = Partner mittente
294300090211     C     VABRMO        IFEQ      *BLANKS
294400090211     C                   MOVEL     ACORAG        VABRMO
294500090211     C                   MOVEL     INDCAP        VABCMO
294600090211     C     INDCAE        IFNE      *BLANKS
294700090211     C                   MOVEL     INDCAE        VABCMO
294800090211     C                   END
294900090211     C     INDSTA        IFNE      *BLANKS
295000090211     C                   MOVEL     INDSTA        VABNMO
295100090211     C                   END
295200090211     C                   END
295300090611      *
295400090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
295500090211     C     §1BFG1        IFEQ      'N'
295600090211     C     §1BFG7        ANDEQ     'N'
295700090211     C     §1BFG2        ANDEQ     'N'
295800090211     C***                  Z-ADD0         VABNRS
295900090211     C                   END
296000090611      *
296100090211     C*  Imposto segancollo da - a
296200090211     C     §1BFG1        IFEQ      'S'
296300090211     C     §1BFG7        ANDEQ     'N'
296400090211     C     §1BF12        ANDNE     'E'
296500090211      *
296600090211     C     XY            IFEQ      *ZEROS
296700090211     C                   CLEAR                   VABNCD
296800090211     C                   CLEAR                   VABNCA
296900090211     C                   ELSE
297000090211     C                   Z-ADD     SGN(1)        VABNCD
297100090211     C                   Z-ADD     SGN(XY)       VABNCA
297200090211     C                   ENDIF
297300090211     C*
297400090211     C                   END
297500090211     C*
297600090211     C*  Se cap mittente originale a blank abblenco nazione
297700090211     C     VABCMO        IFEQ      *BLANKS
297800090211     C                   MOVEL     *BLANKS       VABNMO
297900090211     C                   END
298000100311     C*
298100100311     C*  aggiungo le NOTE se ci sono
298200100311     C     Wnot1         IFne      *BLANKS
298300100311     c                   movel(p)  Wnot1         vabNOT
298400100311     c                   end
298500100311     C     Wnot2         IFne      *BLANKS
298600100311     c                   movel(p)  Wnot2         vabNT2
298700100311     c                   end
298800100311     C*
298900090603      *
299000090211      *? ------------------------------------------------------------------------
299100090213     C   30              WRITE     EDiVAB00                             99
299200090213     C  N30              UPDATE    EDiVAB00                             99
299300090211      *? ------------------------------------------------------------------------
299400090611     C*
299500090213     c   99              eval      errori_in_Wrt = 'S'
299600090211     C*
299700090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
299800090211     C                   EXSR      WRTVAT_Rif
299900090211     C*
300000090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
300100090211     C     §1BF12        IFEQ      'E'
300200090211     C                   EXSR      WRTVAT
300300090211     C                   ELSE
300400090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
300500090211     C     §1BFG7        IFEQ      'S'
300600090211     C     §1BFG1        OREQ      'S'
300700090211     C                   EXSR      WRTVAD
300800090211     C                   END
300900090211     C                   END
301000090211      *
301100090211     C* Reimposto codice trattamento merce cliente
301200090211     C                   MOVEL     WSVTRM        DS1B
301300090211     C**
301400090211     C* Do errore se previsto CMR ma non c'è
301500090211     C     §PTCMR        IFNE      ' '
301600090211     C                   EXSR      MEMCMR
301700090211     C                   END
301800090611      *
301900090211     C* Do errore se previsto AFB ma non c'è
302000090211     C     §PTNFV        IFNE      ' '
302100090211     C                   EXSR      MEMAFB
302200090211     C                   END
302300090611      *
302400090211     C*  Se il cliente vuole il ritorno delle date di consegna
302500090211     C*  è c'è una squdratura fra i segnacolli e il numero
302600090211     C*  dei colli che ci ha passato scrivo EDSUM
302700090211     C     §PUDTA        IFEQ      'S'
302800090211     C     WSQUAD        ANDEQ     'S'
302900090211     C                   EXSR      WRTSUM
303000090211     C                   END
303100090211     C*
303200090211     C                   ENDSR
303300090211     C*----------------------------------------------------------------
303400090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
303500090211     C*----------------------------------------------------------------
303600090211     C     CTRSGN        BEGSR
303700090211     C*
303800090211     C                   MOVEL     'N'           WNSGER            1
303900090211     C*
304000090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
304100090211     C     VABNCL        IFNE      XY
304200090211     C                   MOVEL     'S'           WNSGER
304300090211     C                   MOVEL     'S'           WSQUAD
304400090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
304500090211     c                              bollettati'
304600090211     C                   GOTO      FINSGN
304700090211     C                   END
304800090211     C*  Riordino i segnacolli
304900090211     C     §1BFG7        IFEQ      'N'
305000090211     C                   SORTA     SGN
305100090211     C*  Controllo se sono sequenziali
305200090211     C                   MOVEL     'N'           WNOSEQ            1
305300090211     C     SGN(1)        ADD       1             WEXSGN            7 0
305400090211     C     2             DO        XY            X
305500090211     C     SGN(X)        IFNE      WEXSGN
305600090211     C                   MOVEL     'S'           WNOSEQ
305700090211     C                   END
305800090211     C     SGN(X)        ADD       1             WEXSGN
305900090211     C                   END
306000090211     C                   END
306100090211     C*
306200090211     C     FINSGN        ENDSR
306300090211     C*----------------------------------------------------------------
306400090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
306500090211     C*----------------------------------------------------------------
306600090211     C     CTRSGE        BEGSR
306700090211     C*
306800090211     C                   MOVEL     'N'           WNSGER            1
306900090211      *
307000090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
307100090211      *
307200090211     C     VABNCL        IFNE      §§
307300090211     C                   MOVEL     'S'           WNSGER
307400090211     C                   MOVEL     'S'           WSQUAD
307500090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
307600090211     c                              bollettati'
307700090211     C                   END
307800090211      *
307900090211     C                   ENDSR
308000090211     C*----------------------------------------------------------------
308100090211     C*? ESEGUO SCRITTURA EDiVAD
308200090211     C*----------------------------------------------------------------
308300090211     C     WRTVAD        BEGSR
308400090211     C*
308500090211     C* Se non seq. scrivo xy record
308600090211     C     §1BFG1        IFEQ      'S'
308700090211     C     XY            ANDNE     *ZEROS
308800090211     C*
308900090211     C     §1BFG7        IFEQ      'S'
309000090211     C                   DO        XY            X
309100090211     C                   Z-ADD     VABCCM        KCCM
309200090211     C                   Z-ADD     SGN(X)        KNCD
309300090211     C                   Z-ADD     VABCNT        VADCNT
309400090211     C                   MOVEL     VABCMR        VADCMR
309500090211     C                   Z-ADD     VABAAS        VADAAS
309600090211     C                   Z-ADD     VABLNP        VADLNP
309700090211     C                   Z-ADD     VABNRS        VADNRS
309800090211     C                   Z-ADD     VABNSP        VADNSP
309900090211     C                   Z-ADD     1             VADNCL
310000090211     C                   MOVEL     SGN(X)        VADCDP
310100090211     C                   Z-ADD     SGN(X)        VADNCD
310200090211     C                   Z-ADD     *ZEROS        VADNCA
310300090211     C                   Z-ADD     VABCCM        VADCCM
310400090211     C                   movel     VABfgs        VADfgs
310500090213     C                   WRITE     EDiVAD00                             99
310600090213     c   99              eval      errori_in_Wrt = 'S'
310700090211     C                   END
310800090211     C* Se sequenziali scrivo un solo record
310900090211     C                   ELSE
311000090211     C                   Z-ADD     VABAAS        VADAAS
311100090211     C                   Z-ADD     VABLNP        VADLNP
311200090211     C                   Z-ADD     VABNRS        VADNRS
311300090211     C                   Z-ADD     VABNSP        VADNSP
311400090211     C                   Z-ADD     VABNCL        VADNCL
311500090211     C                   Z-ADD     SGN(1)        VADNCD
311600090211     C                   Z-ADD     SGN(XY)       VADNCA
311700090211     C                   Z-ADD     VABCCM        VADCCM
311800090211     C                   movel     VABfgs        VADfgs
311900090213     C                   WRITE     EDiVAD00                             99
312000090213     c   99              eval      errori_in_Wrt = 'S'
312100090211     C                   END
312200090211     C*
312300090211     C                   ELSE
312400090211     C                   DO        XY            X
312500090211     C                   Z-ADD     VABCCM        KCCM
312600090211     C                   Z-ADD     SGN(X)        KNCD
312700090211     C                   Z-ADD     VABAAS        VADAAS
312800090211     C                   Z-ADD     VABLNP        VADLNP
312900090211     C                   Z-ADD     VABNRS        VADNRS
313000090211     C                   Z-ADD     VABNSP        VADNSP
313100090211     C                   MOVEL     SGN(X)        VADCDP
313200090211     C                   Z-ADD     1             VADNCL
313300090211     C                   Z-ADD     *ZEROS        VADNCD
313400090211     C                   Z-ADD     *ZEROS        VADNCA
313500090211     C                   Z-ADD     VABCCM        VADCCM
313600090211     C                   movel     VABfgs        VADfgs
313700090213     C                   WRITE     EDiVAD00                             99
313800090213     c   99              eval      errori_in_Wrt = 'S'
313900090211     C                   END
314000090211     C*
314100090211     C                   END
314200090211     C*
314300090211     C                   ENDSR
314400090211     C*----------------------------------------------------------------
314500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
314600090211     C*----------------------------------------------------------------
314700090211     C     WRTVAT_Rif    BEGSR
314800090211      *
314900090211      *  riferimento di consegna destinatario
315000090211     c                   if        WatRDE <> *blank
315100090211     C                   Z-ADD     VABCNT        VATCNT
315200090211     C                   MOVEL     VABCMR        VATCMR
315300090211     C                   Z-ADD     VABCCM        VATCCM
315400090211     C                   movel     VABfgs        VATfgs
315500090211     C                   Z-ADD     VABLNP        VATLNP
315600090211     C                   Z-ADD     VABAAS        VATAAS
315700090211     C                   Z-ADD     VABNRS        VATNRS
315800090211     C                   Z-ADD     VABNSP        VATNSP
315900090211     C                   MOVEL     'A'           VATTRC
316000090211     C                   MOVEL     WatRDE        VATNOT
316100090213     C                   WRITE     EDiVAT00                             99
316200090213     c   99              eval      errori_in_Wrt = 'S'
316300090211     c                   end
316400090211      *
316500090211      *  riferimento di consegna telefonico
316600090211     c                   if        WatTEL <> *blank
316700090211     C                   Z-ADD     VABCNT        VATCNT
316800090211     C                   MOVEL     VABCMR        VATCMR
316900090211     C                   Z-ADD     VABCCM        VATCCM
317000090211     C                   movel     VABfgs        VATfgs
317100090211     C                   Z-ADD     VABLNP        VATLNP
317200090211     C                   Z-ADD     VABAAS        VATAAS
317300090211     C                   Z-ADD     VABNRS        VATNRS
317400090211     C                   Z-ADD     VABNSP        VATNSP
317500090211     C                   MOVEL     'B'           VATTRC
317600090211     C                   MOVEL     WatTEL        VATNOT
317700090213     C                   WRITE     EDiVAT00                             99
317800090213     c   99              eval      errori_in_Wrt = 'S'
317900090211     c                   end
318000090211      *
318100090211      * riferimento del Partner che una volta veniva riportato sul BL4
318200090211     c                   if        Wnsp   <> *blank
318300090211     C                   Z-ADD     VABCNT        VATCNT
318400090211     C                   MOVEL     VABCMR        VATCMR
318500090211     C                   Z-ADD     VABCCM        VATCCM
318600090211     C                   movel     VABfgs        VATfgs
318700090211     C                   Z-ADD     VABLNP        VATLNP
318800090211     C                   Z-ADD     VABAAS        VATAAS
318900090211     C                   Z-ADD     VABNRS        VATNRS
319000090211     C                   Z-ADD     VABNSP        VATNSP
319100090211     C                   MOVEL     'C'           VATTRC
319200090211     C                   MOVEL     Wnsp          VATNOT
319300090213     C                   WRITE     EDiVAT00                             99
319400090213     c   99              eval      errori_in_Wrt = 'S'
319500090211     c                   end
319600090211      *
319700090211     C                   ENDSR
319800090211     C*----------------------------------------------------------------
319900090211     C*? ESEGUO SCRITTURA EDiVAT
320000090211     C*----------------------------------------------------------------
320100090211     C     WRTVAT        BEGSR
320200090211      *
320300090211     C                   DO        §§            X
320400090211     C                   Z-ADD     VABCNT        VATCNT
320500090211     C                   MOVEL     VABCMR        VATCMR
320600090211     C                   Z-ADD     VABCCM        VATCCM
320700090211     C                   movel     VABfgs        VATfgs
320800090211     C                   Z-ADD     VABLNP        VATLNP
320900090211     C                   Z-ADD     VABAAS        VATAAS
321000090211     C                   Z-ADD     VABNRS        VATNRS
321100090211     C                   Z-ADD     VABNSP        VATNSP
321200090211     C                   MOVEL     'E'           VATTRC
321300090211      *
321400090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
321500090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
321600090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
321700090211     c                   if        WVATlun > 0
321800090211      *
321900090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
322000090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
322100090211      *
322200090211     c                   else
322300090211      *
322400090211      * Se non è impostato nulla prende il segnacollo passato così com'è
322500090211     C                   MOVEL     SGE(X)        VATNOT
322600090211     c                   end
322700090211      *
322800090213     C                   WRITE     EDiVAT00                             99
322900090213     c   99              eval      errori_in_Wrt = 'S'
323000090211     C                   END
323100090211      *
323200090211     C                   ENDSR
323300090211     C*----------------------------------------------------------------
323400090211     C*? ESEGUO SCRITTURA EDSUM
323500090211     C*----------------------------------------------------------------
323600090211     C     WRTSUM        BEGSR
323700090211     C*
323800090211     C* Se non seq. scrivo xy record
323900090211     C                   DO        XY            X
324000090211     C                   Z-ADD     VABAAS        SUMAAS
324100090211     C                   Z-ADD     VABLNP        SUMFLS
324200090211     C                   MOVEL     WKSC          SUMLNP
324300090211     C                   Z-ADD     VABLNA        SUMLNA
324400090211     C                   Z-ADD     VABZNC        SUMZNC
324500090211     C                   Z-ADD     VABNRS        SUMNRS
324600090211     C                   Z-ADD     VABNSP        SUMNSP
324700090211     C                   Z-ADD     SGN(X)        SUMNSC
324800090211     C                   Z-ADD     WKSC          SUMCCM
324900090211     C                   MOVEL     *BLANKS       SUMSTS
325000090211     C                   MOVEL     *BLANKS       SUMFLG
325100090211     C                   MOVEL     WPRGSP        SUMCNI
325200090211     C                   MOVEL     VABCMR        SUMCMR
325300090211     C                   Z-ADD     0             SUMNFE
325400090211     C                   Z-ADD     VABCCM        VADCCM
325500090213     C                   WRITE     EDSUM000                             99
325600090213     c   99              eval      errori_in_Wrt = 'S'
325700090211     C                   END
325800090211     C*
325900090211     C                   ENDSR
326000090211     c*==================================================================*
326100090211      * Manda un Msg in Posta AS Sede a EDP*
326200090211     c*==================================================================*
326300090211     c     SEND_MSG_EDP  begsr
326400090211      *
326500090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
326600090211      *   Lo manda una sola volta per lavoro
326700090211     c                   IF        Snd_Msg_alert  <> 'N' and
326800090211     c                             User_msg <> *blank
326900090211     c                   z-add     1             Jx
327000090211     C                   exsr      CalQEZSNDMG
327100090211     c                   end
327200090211      *
327300090211     c                   endsr
327400090213     c**********************************************************************
327500090213     c* reperimento numero Spedizione
327600090213     c**********************************************************************
327700090213     C     repnum        BEGSR
327800090213      *
327900090213      *    deve controllare sulla tabella "3C" se il numero spedizione
328000090213      *     deve essere mantenuto oppure no
328100090213     C*
328200090213     C                   clear                   Ds3C
328300090213     C                   Z-ADD     CODUT         TBLKUT
328400090213     C                   MOVEL     '3C'          TBLCOD
328500090213     C                   movel(p)  VABccm        TBLKEY
328600090213     C     KTABel        CHAIN     TABEL00F                           30
328700090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
328800090213     C                   MOVEL     TBLUNI        Ds3C
328900090213     C                   END
329000090213      *
329100090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
329200090213     c                   if        §3CFsp <> 'S'
329300090213      *
329400090213     C* NSP => Stacco un numeratore da AZNUM
329500090213     c                   movel     kpjbu         svkpjbu
329600090213     c                   clear                   kpjbu
329700090213     C                   clear                   TRUL33DS
329800090213     C                   eval      I33OPE = *zeros
329900090213     C                   eval      I33CNU = 302
330000090213     C                   eval      I33NUM = 1
330100090213     C                   movel     TRUL33DS      KPJBU
330200090213     C                   call      'TRUL33R'
330300090213     C                   parm                    KPJBA
330400090213     C                   movel     KPJBU         TRUL33DS
330500090213     c                   movel     svkpjbu       kpjbu
330600090213      ****
330700090213     C                   if        O33ERR = *zeros
330800090213     c                   z-add     o33nrf        NUM_SPED
330900090213     C                   else
331000090213     C                   endif
331100090213      ****
331200090213     c                   ELSE
331300090213      *   se si vuole mantenere il numero spedizione
331400090213      ****
331500090213     C                   MOVEL     WOGGI         WANNO             4 0
331600090213     C                   MOVE      WANNO         KAAA
331700090213     C                   Z-ADD     3             KCNU
331800090213     C                   Z-ADD     VABlnp        KFIL
331900090213     C     KNUF          CHAIN     FLNUF                              9999
332000090213     C     *IN99         IFEQ      '0'
332100090213     C                   ADD       1             NUFNUM
332200090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
332300090213     C                   UPDATE    FLNUF
332400090213     C                   ELSE
332500090213     C                   END
332600090213     C*
332700090213     c                   EndIF
332800090213      **
332900090213     C                   ENDSR
333000090213     C*----------------------------------------------------------------
333100090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
333200090213     C*----------------------------------------------------------------
333300090213     C     MEMCMR        BEGSR
333400090213     C*
333500090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
333600090213     C     §PTCM1        IFEQ      'S'
333700090213     C                   CLEAR                   EDRDE000
333800090213     C                   MOVEL     'TD1154'      KNMC
333900090213     C                   Z-ADD     1             KNRP
334000090213     C                   MOVEL     WKSC          KLNP1
334100090213     C     KRDE          CHAIN     EDRDE01L                           31
334200090213     C     *IN31         IFEQ      '1'
334300090213     C                   Z-ADD     VABAAS        RDEAAS
334400090213     C                   Z-ADD     VABLNP        RDELNP
334500090213     C                   Z-ADD     VABNRS        RDENRS
334600090213     C                   Z-ADD     VABNSP        RDENSP
334700090213     C                   MOVEL     'TD1154'      RDENMC
334800090213     C                   Z-ADD     1             RDENRP
334900090213     C                   MOVEL     WNRCMR        RDEVAL
335000090213     C                   WRITE     EDRDE000                             99
335100090213     c   99              eval      errori_in_Wrt = 'S'
335200090213     C                   END
335300090213     C*  Memorizzo qualificatore CMR
335400090213     C                   CLEAR                   EDRDE000
335500090213     C                   MOVEL     'TD1153'      KNMC
335600090213     C                   Z-ADD     1             KNRP
335700090213     C                   MOVEL     WKSC          KLNP1
335800090213     C     KRDE          CHAIN     EDRDE01L                           31
335900090213     C     *IN31         IFEQ      '1'
336000090213     C                   Z-ADD     VABAAS        RDEAAS
336100090213     C                   Z-ADD     VABLNP        RDELNP
336200090213     C                   Z-ADD     VABNRS        RDENRS
336300090213     C                   Z-ADD     VABNSP        RDENSP
336400090213     C                   Z-ADD     1             RDENRP
336500090213     C                   MOVEL     'TD1153'      RDENMC
336600090213     C                   MOVEL     'CMR'         RDEVAL
336700090213     C                   WRITE     EDRDE000                             99
336800090213     c   99              eval      errori_in_Wrt = 'S'
336900090213     C                   END
337000090213     C*  Memorizzo la data
337100090213     C                   CLEAR                   EDRDE000
337200090213     C                   MOVEL     'TD2380'      KNMC
337300090213     C                   Z-ADD     1             KNRP
337400090213     C                   MOVEL     WKSC          KLNP1
337500090213     C     KRDE          CHAIN     EDRDE01L                           31
337600090213     C     *IN31         IFEQ      '1'
337700090213     C                   Z-ADD     VABAAS        RDEAAS
337800090213     C                   Z-ADD     VABLNP        RDELNP
337900090213     C                   Z-ADD     VABNRS        RDENRS
338000090213     C                   Z-ADD     VABNSP        RDENSP
338100090213     C                   Z-ADD     1             RDENRP
338200090213     C                   MOVEL     'TD2380'      RDENMC
338300090611     C                   MOVEL     Tes_VABdcm    RDEVAL
338400090213     C                   WRITE     EDRDE000                             99
338500090213     c   99              eval      errori_in_Wrt = 'S'
338600090213     C                   END
338700090213     C*
338800090213     C                   END                                                    §PTCM1 = 'S'
338900090213     C*
339000090213     C                   ENDSR
339100090213     C*----------------------------------------------------------------
339200090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
339300090213     C*----------------------------------------------------------------
339400090213     C     MEMAFB        BEGSR
339500090213     C*
339600090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
339700090213     C     §PTNF1        IFEQ      'S'
339800090213     C                   CLEAR                   EDRDE000
339900090213     C                   MOVEL     WKSC          KLNP1
340000090213     C                   MOVEL     'TD1154'      KNMC
340100090213     C                   Z-ADD     1             KNRP
340200090213     C     KRDE          CHAIN     EDRDE01L                           31
340300090213     C     *IN31         IFEQ      '1'
340400090213     C                   Z-ADD     VABAAS        RDEAAS
340500090213     C                   Z-ADD     VABLNP        RDELNP
340600090213     C                   Z-ADD     VABNRS        RDENRS
340700090213     C                   Z-ADD     VABNSP        RDENSP
340800090213     C                   MOVEL     'TD1154'      RDENMC
340900090213     C     §PTCM1        IFNE      'S'
341000090213     C     WNRCMR        OREQ      *BLANKS
341100090213     C                   Z-ADD     1             RDENRP
341200090213     C                   ELSE
341300090213     C                   Z-ADD     2             RDENRP
341400090213     C                   END
341500090213     C                   MOVEL     WNUMFV        RDEVAL
341600090213     C                   WRITE     EDRDE000                             99
341700090213     c   99              eval      errori_in_Wrt = 'S'
341800090213     C                   END
341900090213     C*  Memorizzo qualificatore CMR
342000090213     C                   CLEAR                   EDRDE000
342100090213     C                   MOVEL     WKSC          KLNP1
342200090213     C                   MOVEL     'TD1153'      KNMC
342300090213     C                   Z-ADD     1             KNRP
342400090213     C     KRDE          CHAIN     EDRDE01L                           31
342500090213     C     *IN31         IFEQ      '1'
342600090213     C                   Z-ADD     VABAAS        RDEAAS
342700090213     C                   Z-ADD     VABLNP        RDELNP
342800090213     C                   Z-ADD     VABNRS        RDENRS
342900090213     C                   Z-ADD     VABNSP        RDENSP
343000090213     C     §PTCM1        IFNE      'S'
343100090213     C     WNRCMR        OREQ      *BLANKS
343200090213     C                   Z-ADD     1             RDENRP
343300090213     C                   ELSE
343400090213     C                   Z-ADD     2             RDENRP
343500090213     C                   END
343600090213     C                   MOVEL     'TD1153'      RDENMC
343700090213     C                   MOVEL     'AFB'         RDEVAL
343800090213     C                   WRITE     EDRDE000                             99
343900090213     c   99              eval      errori_in_Wrt = 'S'
344000090213     C                   END
344100090213     C*  Memorizzo la data
344200090213     C                   CLEAR                   EDRDE000
344300090213     C                   MOVEL     WKSC          KLNP1
344400090213     C                   MOVEL     'TD2380'      KNMC
344500090213     C                   Z-ADD     1             KNRP
344600090213     C     KRDE          CHAIN     EDRDE01L                           31
344700090213     C     *IN31         IFEQ      '1'
344800090213     C                   Z-ADD     VABAAS        RDEAAS
344900090213     C                   Z-ADD     VABLNP        RDELNP
345000090213     C                   Z-ADD     VABNRS        RDENRS
345100090213     C                   Z-ADD     VABNSP        RDENSP
345200090213     C     §PTCM1        IFNE      'S'
345300090213     C     WNRCMR        OREQ      *BLANKS
345400090213     C                   Z-ADD     1             RDENRP
345500090213     C                   ELSE
345600090213     C                   Z-ADD     2             RDENRP
345700090213     C                   END
345800090213     C                   MOVEL     'TD2380'      RDENMC
345900090611     C                   MOVEL     Tes_VABdcm    RDEVAL
346000090213     C                   WRITE     EDRDE000                             99
346100090213     c   99              eval      errori_in_Wrt = 'S'
346200090213     C                   END
346300090213     C*
346400090213     C                   END                                                    §PTCM1 = 'S'
346500090213     C*
346600090213     C                   ENDSR
346700090213     c*==================================================================*
346800090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
346900090213     c*==================================================================*
347000090213     c     Imp_ADDR_mail begsr
347100090213      *
347200090213     c                   clear                   Indirizzi
347300090213      *
347400090213      *  Lunghezza indirizzi presenti
347500090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
347600090213     c                   z-add     1             al_char           3 0
347700090213     c                   z-add     1             dal_char          3 0
347800090213     c                   z-add     1             tot_char          3 0
347900090213      *
348000090213     c     successivo    tag
348100090213      *
348200090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
348300090213      *   e aggiunge il dominio Bartolini + il (;) separatore
348400090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
348500090213     c                   eval      tot_char = al_char - dal_char
348600090213     c                   z-add     dal_char      Dac
348700090213     c                   z-add     tot_char      Luc
348800090213      *
348900090213     c                   clear                   Indir_Bartol     40
349000090213     c                   clear                   Indir_Parz       20
349100090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
349200090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
349300090213     c                             %Trim(bart_it)
349400090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
349500090213      *
349600090213     c                   if        al_char = Lun_Ind
349700090213     c                   goto      fine_indmail
349800090213     c                   else
349900090213     c                   eval      dal_char = ( al_char + 1 )
350000090213     c                   goto      successivo
350100090213     c                   end
350200090213      *
350300090213     C     fine_indmail  TAG
350400090213      *
350500090213     C                   ENDSR
350600090213     c*==================================================================*
350700090213      * Manda un Msg x E-mail
350800090213     c*==================================================================*
350900090213     c     Invio_Mail    begsr
351000090213      *
351100090213     c                   goto      non_inviare
351200090213      *
351300090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
351400090213     C* Inizializzo variabili
351500090213     C                   movel     *blanks       wrkEml          253
351600090213     C                   movel     *blanks       wrkMsg         5000
351700090213     C                   movel     *blanks       wrkOgg           44
351800090213      *
351900090213     C* Valorizzo i campi della e-m@ail - indirizzo
352000090213     C                   eval      wrkEml = Indirizzi
352100090213     C                   eval      wrkOgg = Oggetto
352200090213     C                   eval      wrkMsg = Messaggio
352300090213
352400090213     C                   call(e)   'TIS701C'
352500090213     C                   parm                    wrkEml
352600090213     C                   parm                    wrkOgg
352700090213     C                   parm                    wrkMsg
352800090213
352900090213     c     non_inviare   tag
353000090213     C*
353100090213     C                   ENDSR
353200090603     c*==================================================================*
353300090603      * Manda un Msg x E-mail a EDPAB
353400090603     c*==================================================================*
353500090603     c     Invio_Mail_AB begsr
353600090603      *
353700090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
353800090603     C* Inizializzo variabili
353900090603     C                   movel     *blanks       wrkEml          253
354000090603     C                   movel     *blanks       wrkMsg         5000
354100090603     C                   movel     *blanks       wrkOgg           44
354200090603      *
354300090603     C* Valorizzo i campi della e-m@ail - indirizzo
354400110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
354500110203     C                   eval      wrkEml = Indirizzi
354600090603     C                   eval      wrkOgg = Oggetto
354700090603     C                   eval      wrkMsg = Messaggio
354800110203      ********
354900110203     C********           call(e)   'TIS701C'
355000110203     C********           parm                    wrkEml
355100110203     C********           parm                    wrkOgg
355200110203     C********           parm                    wrkMsg
355300110203     C*
355400110203      ** *****
355500110203     C                   call(e)   'TRTCT00R2'
355600110203     C                   parm                    wrkEml
355700110203     C                   parm                    wrkOgg
355800110203     C                   parm                    wrkMsg
355900090603     C*
356000090616     c                   if        %error
356100090616     c                   end
356200090616     C*
356300090603     C                   ENDSR
356400090211     ***********************************************************************
356500090211     **
356600090211     ** Send Message (QEZSNDMG) API
356700090211     **
356800090211     ***********************************************************************
356900090211     C     CalQEZSNDMG   BEGSR
357000090211      *
357100090211     ** Invio messaggio all'utente.
357200090211     C                   EVAL      SndMg03 = msgDSP
357300090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
357400090211     C                   EVAL      SndMg05(Jx) = User_msg
357500090211     C                   EVAL      SndMg06 = Jx
357600090211     C                   CLEAR                   QUSEC
357700090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
357800090211
357900090211     C                   CALL      'QEZSNDMG'
358000090211     C                   PARM                    SndMg01
358100090211     C                   PARM                    SndMg02
358200090211     C                   PARM                    SndMg03
358300090211     C                   PARM                    SndMg04
358400090211     C                   PARM                    SndMg05
358500090211     C                   PARM                    SndMg06
358600090211     C                   PARM                    SndMg07
358700090211     C                   PARM                    SndMg08
358800090211     C                   PARM                    Qusec
358900090211     C                   PARM                    SndMg10
359000090211     C                   PARM                    SndMg11
359100090211     C                   PARM                    SndMg12
359200090211      *
359300090211     C                   ENDSR
359400121105      *---------------------------------------------------------------*
359500121105      * CTRL caricamento schiere    PT                               -*
359600121105      *---------------------------------------------------------------*
359700121105     C     Check_PT      begsr
359800121105     C*
359900121105     c* Controllo riempimento schiera
360000121105     c                   clear                   trul0sds
360100121105     c                   eval      i0sski='CPT'
360200121105     c                   eval      i0sele=%elem(CPT)
360300121105     c                   eval      i0spie=x
360400121105     c                   eval      i0sfile='EDTAB00F'
360500121105     c                   eval      i0ssif=knsif
360600121105     c                   eval      i0spgm='TRTCT05R'
360700121113     c                   move      kpjbu         SvKpjbu
360800121113     c                   clear                   kpjbu
360900121105     c                   movel     trul0sds      kpjbu
361000121105     c                   call      'TRUL0SR'
361100121105     c                   parm                    kpjba
361200121113     c                   eval      kpjbu  =  SvKpjbu
361300121105     C*
361400121105     C                   ENDSR
361500090211     O*****************************************************************
361600090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
361700090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
361800090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
361900090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
